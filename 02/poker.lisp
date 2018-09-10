;(declaim (optimize (speed 3) (safety 0)))

;(defpackage :poker
;  (:use :cl :moje))

;(in-package :poker)
;karty to liczby od 1 do 52, 1=As Pik,2=Krol Pik,52=2 Trefl
;kolory: 0=Pik, 1=Kier, 2=Karo, 3=Trefl
;figury: 0=As, 1=Krol, 12=Dwojka


(defvar *my-pos* nil "My position at the table")
(defvar *history* nil "History of actions")
(defvar *table-size* 10 "Initial number of players")
(defvar *table* nil)
(defvar *hand* nil)
(defvar *pot* 0)

(defvar *sblind* 0.5)
(defvar *blind* 1)
(defvar *stake* *blind*)
(defvar *active-player* 2) ;gracz obecnie grajacy
(defvar *akcje* (make-list *table-size* :initial-element t)) ; lista akcji wykonanych przez graczy w tej rundzie!
(defvar *money* (make-list *table-size* :initial-element 0)) ; lista pieniedzy wplaconych przez graczy w tej rundzie!
(defvar *ocena* nil) ;lista
(defvar *vhand* nil) ;wektor figur na rece
(defvar *vtable* nil) ;wektor figur na stole

(defvar *still-playing* nil)
(defvar *hs* 1000) ;Hand Strength!!!

(defvar H1 0)
(defvar H2 0)
(defvar T1 0)
(defvar T2 0)
(defvar T3 0)
(defvar T4 0)
(defvar T5 0)

(defvar *logging* t)


(defvar *facts* nil)
(defvar *memory* nil)
(defvar +poker-rules+ nil) ;punkt wyjscia funkcji action
(defvar +defs-list+ nil) ;definicje pojec dolaczana do wszystkich innych list
(defvar +pre-flop-rules+ nil)
(defvar +post-flop-rules+ nil)
(defvar +flop-rules+ nil)
(defvar +turn-rules+ nil)
(defvar +river-rules+ nil)

;;--------------------------------------------------------------------
;; funkcje oceny zestawow kart:

(defun lepsza(ocena1 ocena2)
 (cond 
  ((null ocena1) 0)
  ((< (car ocena1) (car ocena2))  1)
  ((> (car ocena1) (car ocena2))  -1)  
  (t (lepsza (cdr ocena1)(cdr ocena2)))  ))

(defun zwyciezca(oceny &optional (best '(10)) (zwyciezcy nil) (biezacy 0))
"oceny to lista ocen, zwraca liste zwyciezcow"
 (cond 
   ((null oceny)  zwyciezcy)
   ((= 1 (lepsza (car oceny)  best)) 
              (zwyciezca (cdr oceny) (car oceny) (list biezacy) (1+ biezacy)))
   ((= 0 (lepsza (car oceny)  best)) 
              (zwyciezca (cdr oceny) best (cons biezacy zwyciezcy) (1+ biezacy)))      
   (t (zwyciezca (cdr oceny) best zwyciezcy (1+ biezacy) ))  ))


(defun wybierz-ile(ile wektor-figur)
 "Uzywana do opisu trojki, z listy figur wybiera <ile> najmocniejszych kart,
  zakladamy ze opisywana trojka juz zostala usunieta"
  (let (wynik)
    (dotimes (figura (length wektor-figur))
        (when (= (aref wektor-figur figura) 1) (push figura wynik)))
  (subseq (reverse wynik) 0 ile)   ))

(defun wektor-figur(liczby)
 (let ((wektor-figur (make-array 13 :initial-element 0))) 
   (dolist (l1 liczby) 
               (incf (aref wektor-figur   (l->figura l1))))  
  wektor-figur))

(defun wektor-kolorow(liczby)
 (let ((wektor-kolorow (make-array 4 :initial-element 0))) 
   (dolist (l1 liczby) 
               (incf (aref wektor-kolorow (l->kolor l1))))  
  wektor-kolorow))

; !!! zakladamy ze liczby sa lista najwyzej siedmioelementowa
; tzn. np. nie ma czterech par !!!
(defun ocena(liczby)
 (let ((wektor-figur (make-array 13 :initial-element 0)) 
       (wektor-kolorow (make-array 4 :initial-element 0))
        wartosc jest-kolor jest-street jest-poker jest-kareta 
        jest-trojka jest-para)
   (dolist (l1 liczby) 
               (incf (aref wektor-figur   (l->figura l1)))  
               (incf (aref wektor-kolorow (l->kolor l1))) )
   ; zakladamy ze jak jest kolor to tylko jeden
   (setq jest-kolor (kolor wektor-kolorow))
   (setq jest-street (street wektor-figur))
   (when (and jest-kolor jest-street) (setq jest-poker (poker liczby jest-kolor)))
   (when jest-poker (return-from ocena (list 0 jest-poker)))

   (setq jest-kareta (szukaj-wartosci wektor-figur 4))
   (when jest-kareta
        (return-from ocena 
               (cons 1 (list jest-kareta 
                                   (szukaj-roznej wektor-figur 4)))))
 
   (setq jest-trojka (szukaj-wartosci wektor-figur 3))
   (when jest-trojka
     ;szukamy drugiej trojki
     (dotimes (indeks (- 12 jest-trojka)) 
        (when (= 3 (aref wektor-figur (+ indeks jest-trojka 1) )) ;mamy druga pare
	     (return-from ocena (list 2 jest-trojka (+ 1 jest-trojka indeks)  ))
	       )))
   (setq jest-para (szukaj-wartosci wektor-figur 2))
   (when (and jest-trojka jest-para) (return-from ocena (cons 2 (list jest-trojka jest-para))))

   (when jest-kolor
       (dolist (l liczby)
           (when (= jest-kolor (l->kolor l))
               (push (l->figura l) wartosc)))
       (return-from ocena (cons 3 (subseq (sort  wartosc #'<) 0 5))))

   (when jest-street (return-from ocena (list 4 jest-street)))
   
   ;jesli zostala trojka to ja opisujemy
   (when jest-trojka
      (return-from ocena (cons 5 (cons jest-trojka (wybierz-ile 2 wektor-figur))))
     )

   (when jest-para
     ;szukamy drugiej pary
     (aif (dwie-pary wektor-figur)
          (return-from ocena it))
     ;jesli jej nie znaleziono to mamy tylko jedna pare:
     (return-from ocena (cons 7 (cons jest-para (wybierz-ile 3 wektor-figur))))  )
  (cons 8 (wybierz-ile 5 wektor-figur ) )  ))

(defun dwie-pary(wektor-figur)
  (let ((pary nil))
    (dotimes (i 13)
        (when (= 2 (aref wektor-figur i))
              (push i pary)))
   (cond
     ((= 2 (length pary)) `(6 ,@(reverse pary) ,(szukaj-wartosci wektor-figur 1)) )
     ((= 3 (length pary)) `(6 ,@(reverse pary)))
     (t nil) )     ))


(defun kolor(wektor-kolorow)
 (dotimes (i 4) (when (> (aref wektor-kolorow i) 4) (return-from kolor i))))

 
(defun szukaj-wartosci(wektor-figur wartosc)
;uzywane do szukania karety,trojki,pary itd.
 (dotimes (i 13) (when (= (aref wektor-figur i) wartosc) (return-from szukaj-wartosci i) )))

(defun szukaj-roznej(wektor-figur wartosc)
 (let (figura)
    (dotimes (i 13)
       (setq figura (aref wektor-figur i))
       (when (and (> figura 0)(not (= wartosc figura)))
          (return-from szukaj-roznej i)))))

(defun najwyzsza(wektor-figur)
 (dotimes (i 13) (when (> (aref wektor-figur i) 0) (return-from najwyzsza i) )))

(defun najnizsza(wektor-figur)
 (dotimes (i 13) (when (> (aref wektor-figur (- 12 i)) 0) (return-from najnizsza (- 12 i)) )))


(defun kolejne(wektor indeks)
 (and (> (aref wektor indeks) 0) 
      (> (aref wektor (+ 1 indeks)) 0)
      (> (aref wektor (+ 2 indeks)) 0)
      (> (aref wektor (+ 3 indeks)) 0)
      (> (aref wektor (+ 4 indeks)) 0)))
       
(defun street(wektor-figur)
 ;najpierw testujemy streeta as,2,3,4,5
 (when (and (> (aref wektor-figur 0) 0)
            (> (aref wektor-figur 9) 0)
            (> (aref wektor-figur 10) 0)
            (> (aref wektor-figur 11) 0)
            (> (aref wektor-figur 12) 0))
  (return-from street 9))
 ;nastepnie dowolnego innego
 (do ((i 0 (+ i 1)))
     ((> i 8))
     (when (kolejne wektor-figur i) (return-from street i) ))  )



(defun poker(liczby kolor)
 "Musimy sprawdzac czy street jest w tym samym kolorze. Argumentem jest
 lista liczb <liczby>. Zwraca figure lub nil"
 (labels ((porownaj (liczba1 liczba2)
              (let ((f1 (l->figura liczba1))(k1 (l->kolor liczba1))
                    (f2 (l->figura liczba2))(k2 (l->kolor liczba2)))
                (and (= k1 k2) (= f2 (1+ f1)))   ))
          (przechodz (lista ilosc-kolejnych poprzedni poczatek)
            (cond
               ((= 5 ilosc-kolejnych) (l->figura poczatek))
               ((null lista) nil)
               ((porownaj poprzedni (car lista))  (przechodz (cdr lista) 
                                                             (1+ ilosc-kolejnych)
                                                             (car lista)
                                                             poczatek)  )
               (t (przechodz (cdr lista) 1 (car lista) (car lista)))   )))
  
  ;ad-hoc testowanie pokera ze streetem As,2,3,4,5
  (when (subsetp
             (mapcar (lambda (x) (+ x (* kolor 13)))  '(1 10 11 12 13))
             liczby)
       (return-from poker 9))

   (let ((ciag (sort (copy-list liczby) #'<)))
       (przechodz ciag 1 (car ciag) (car ciag)))   ))

;;-------------------------------------------------------------------
;; Funkcje pomocnicze



(defun talia()
 (let (wynik)
  (dotimes (i 52 (nreverse wynik))
   (push (1+ i) wynik))))

(defun tasuj( &optional (co (talia)))
 (flet ( (los(a b) (+ a (random (- b a)))) )
 (let ((dlugosc (length co)))
  (dotimes (i dlugosc co)
   (rotatef (nth i co) 
            (nth (los i dlugosc) co))))))


(defun wyjmij(skad &rest listy)
 "Usuwa z listy <skad> elementy z list <listy>"
  (dolist (l listy)
    (dolist (el l)
      (setf skad (remove el skad)))) 
  skad)
     
;;-------------------------------------------------------------------
;; OPIS kart
;;-------------------------------------------------------------------
(defvar kolory '(S H D C))
;(defvar kolory-opis '("&spades;" "&hearts;" "&diams;" "&clubs;"))
;(defvar kolory-opis '("<td style=\"color: red\">~A&#9824;" "&#9829;" "&#9830;" "&#9827;"))
(defvar kolory-opis '("<td style=\"color: black\">~A&#9824;" "<td style=\"color: red\">~A&#9829;" 
		      "<td style=\"color: blue\">~A&#9830;" "<td style=\"color: green\">~A&#9827;" ))
(defvar figury '(A K Q J 10 9 8 7 6 5 4 3 2))

(defun l->kolor(liczba)
  (floor (1- liczba) 13))

(defun l->figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a ¿eby nie bylo ostrzezen
   b))


;(defun l->opis (liczba)
 ;  (format nil "~A~A" (nth (l->figura liczba) figury) 
  ;                    (nth (l->kolor liczba) kolory)))

(defun l->opis (liczba)
   (format nil (nth (l->kolor liczba) kolory-opis)  (nth (l->figura liczba) figury)   ))

(defun liczby->opis(lista)
 (mapcar #'l->opis lista))
      
(defun opis->liczba( opis )
 "Zamienia opis karty na liczbe. AC -> liczba(Asa Trefl)
  (opis->liczba 'as) -> 1"
 (let ((o1 (char (symbol-name opis) 0))
       (o2 (char (symbol-name opis) 1)))
   (+
     (coerce    ;zeby uniknac ostrzezen kompilatora  
       (case (char-code o1)
        (65 1) (75 2) (81 3) (74 4) (84 5) (57 6) (56 7) (55 8) (54 9)
        (53 10) (52 11) (51 12) (50 13)) 'number) 
     (* 13
       (coerce ;zeby uniknac ostrzezen kompilatora  
         (case (char-code o2)
           (83 0)(72 1)(68 2)(67 3))  'number)  )) ))

	   
(defun karty (&rest opisy)
"(karty 'as 'qh)"
  (mapcar #'opis->liczba opisy))

(defvar opisy-ocen '("Poker" "Kareta" "Full" "Kolor" "Street" "Trojka" "Dwie Pary" "Para" "Najwyzsza"))

(defun opisz-ocene(ocena)
  (format nil "~A, ~A"
  (nth (car ocena) opisy-ocen)
  (mapcar  (lambda (o) (nth o figury)) (cdr ocena)) ))

(defun wynik->odds(lista)
  (if (zerop (first lista))   
       1000
       (/ (third lista) (first lista) 1.0 ) ))
				

;; ---------------- SERWER ------------------------------------------------

#+clisp
(defun start-serwer (port)
  (let ( (gniazdko (socket:socket-server port)) (polaczenie nil) (wiersz nil) (wynik nil) )
    (loop
      (handler-case
       (progn
       (setq polaczenie (socket:socket-accept gniazdko))
           (loop
              (setq forma (read polaczenie))
              (format t "Got: ~A~%" forma)
              (if (and (consp forma) (eq (car forma) 'action))
                    (progn (setq wynik (eval forma))
		           (format polaczenie "~A~&" wynik)
                           (force-output polaczenie)
			   (format t "----->~A~%" wynik))
                    (eval forma))  ))

      (error (e) (progn (close polaczenie)(format t "Blad: ~A~&" e)))))
   (socket:socket-server-close gniazdko)))


#+cmu
(defun start-serwer (port)
  (let (p s1 s2 wynik forma)
    (setq s1 (ext:create-inet-listener port :stream :reuse-address t))
 (handler-case
   (loop
    (setq s2 (ext:accept-tcp-connection s1))
    (setq p  (system:make-fd-stream s2 :input t :output t))
       (handler-case
          (loop	   
             (setq forma (read p))
             (dbg 'dbg-servers "Got: ~A~%" forma)
             (if (and (consp forma) (eq (car forma) 'action))
                    (progn (setq wynik (eval forma))
		           (format p "~A~&" wynik)
                           (force-output p)
			   (dbg 'dbg-servers "pot-odds=~A |----->~A~%" (pot-odds) wynik))
                    (eval forma)) )
  
       (error (e2) (progn (close p)(format t  "Blad: ~A~&" e2)))) )
  (error (e) (format t "Status=~A~&" e)))) )

(let ((fd  nil)
      (bufor (make-string 100))
      (dane nil)
      (wynik nil)
      (dlugosc 0)
      (zdalny  nil))

(defun sluchaj-udp(&optional (zdalny-komp "127.0.0.1") (port-lokalny 9000))
  (setq zdalny  (host-entry-addr
		 (lookup-host-entry zdalny-komp)))
  (setq fd (ext:create-inet-listener port-lokalny :datagram)))


(defun pobieraj-udp()
    (loop
     (setq dlugosc (ext:inet-recvfrom fd bufor 100))
     (setq dane (read-from-string (subseq bufor 0 dlugosc)))
     (dbg 'dbg-servers "Got: ~A~%" dane)
     (setq wynik (eval dane))
     (when (and (consp dane) (eq (car dane) 'action))
	 (wyslij-udp wynik 10000))  ))

(defun wyslij-udp(co port)
     (ext:inet-sendto fd co (length co)
		      zdalny
		      port))              );koniec let

;;----------------------- ---------------------------

(defun podzbiory-2(lista)
"Zwraca liste podzbiorow dwu-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)) wynik)))
  wynik )) 

(defun podzbiory-3(lista)
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)  (nth k lista))
             wynik))))
   wynik ))

;;---------------------- LOGIC SUBSYSTEM  ------------------------

(let ((memo nil) (memorize? nil))
  (defun make-reasoner(rules)
   (if (and memorize? (assoc rules memo))
	(cdr (assoc rules memo))
	(let ((rlz (make-hash-table)))
	  ;(princ "tworzymy hashtable")
	  (dolist (r (append +defs-list+ (symbol-value rules)))
	    (setf (gethash (first r) rlz)
		  (nconc (gethash (first r) rlz) (list (rest r)))))
	  (push (cons rules rlz) memo)
	  rlz)))
  (defun memorize-reasoner()
    (setq memorize? (not memorize?)))
  (defun show-reasoners()
   memo))

(defmacro reason-check-with(smb)
  `(prove (make-reasoner ',smb) 'check))

(defmacro reason-bet-with(smb)
  `(prove (make-reasoner ',smb) 'bet))

(defmacro reason-fold-with(smb)
  `(prove (make-reasoner ',smb) 'fold))

(defmacro reason-dont-bet-with(smb)
  `(prove (make-reasoner ',smb) 'dont-bet))


(defun show-rules(table)
  (maphash #'(lambda (k v)
	       (format t "~A <= ~A~%" k v))
	   table))


(defun fact?(f)
 (or (member f *memory*) (member f *facts*)))

(defvar *chosen-rule* nil)

(defun prove(table goal)
 (let (ch-rule)
 (if (consp goal)
     (eval goal)
     (or (fact?  goal)
         (when (some
		#'(lambda(rule)
		    (setq ch-rule rule)
		    (prove-all table rule))
		(gethash goal table))
	   (when (or (eq goal 'bet) (eq goal 'check)
		     (eq goal 'dont-bet)(eq goal 'fold))
	     (push (cons goal ch-rule) *chosen-rule*))
	   (push goal *facts*))))  ))

(defun prove-all(table goals)
  (if (null goals) t (and (prove table (first goals))
			  (prove-all table (rest goals)))))

(defun insert-rule(table rule)
  (setf (gethash (first rule) table)
	(nconc (gethash (first rule) table) (list (rest rule)))))


;-------------------  LOGOWANIE WYNIKOW ------------------------------------------
(defun show-stats()
  (format t "~A ~A ~A ~%" *hand* *table* *history*))

(defun show-html-stats()
  (format nil "~A ~A ~A ~%" (pot) *table* *history*))




(defun czas()
 (let ((c (multiple-value-list (get-decoded-time))))
     (format nil "~A:~A.~A" (nth 2 c) (nth 1 c) (nth 0 c) )))

(let ((log-messages nil))

 (defun short-logs(m)
  (and *logging*
       (push
	(format nil "<td>~A<td colspan=2> <td><table><tr>~{~A~}</table><td colspan=6>~A"
		(czas) (liczby->opis (reverse *table*))  m)
	log-messages)))

 (defun log-message(m)
  (format nil "~A:[~A/~A] pot:~A pot-odds:~A ~A ~A ~A ~A [~A]" (czas)  *my-pos* *table-size* 
		       (pot) (pot-odds)
                       (liczby->opis *hand*)  (liczby->opis *table*)
		       *history* m *chosen-rule*)) 

 (defun log-message2(m)
   (format nil "<td>~A<td>~A/~A <td><table><tr>~{~A~}</table> <td><table><tr>~{~A~}</table>  <td>~A <td>~A <td>~A <td>~A <td>~A <td> ~A" 
	               (czas)  *my-pos* *table-size* 
                       (liczby->opis *hand*)  (liczby->opis (reverse *table*)) 
		       (pot) (pot-odds) *hs*

		       *history* *chosen-rule* m))

 (defun logs(m)
   (and *logging* 
    (let ((msg (log-message2 m)))
     ; (format t "-------------------~%~%~A~%" msg)
     ; (force-output)
      (push msg log-messages))
   'ok))

 (defun clear-logs()
   (setq log-messages nil))

 (defun get-logs()
   log-messages)

 (defun write-logs()
  (with-open-file (f "logs.html" :direction :output 
                                :if-does-not-exist :create :if-exists :supersede)
  (format f "<table border=1 style=\"font-size: 10pt; font-family: sans-serif; text-align: center\">")
  (format f "<tr style=\"background-color: gray\"><td>godz.<td>poz.<td>hand<td>table<td>pot<td>pot-odds<td>hs<td>history<td>rule<td>decision")
  (format f "~{<tr>~A~%~}" log-messages)
  (format f "</table>") ))) 
;--------------------------------------------------------------------
;; ------------------------  EVENTS -------------------------------------

(defun new-hand(&optional (number 10))
  #+cmu(when (<= number 3) (saytext "Grabola, help me!") )
  (setq *table* nil *hand* nil  *history* nil 
        *table-size* number *my-pos* nil
	*akcje* (make-list *table-size* :initial-element t)
        *money* (make-list *table-size* :initial-element 0)
	*pot* 0 *stake* *blind* *memory* nil)
  (setq *still-playing* t)
  (setq *hs* 1000)
  (setq *active-player* (next-player 1))
  (setf (nth 0 *money*)  *sblind*)
  (setf (nth 1 *money*)  *blind*)  )


(defun hand(&rest h)
  (setq *hand* h))


(defun flop(&rest a)
    (setq *table* a)
    (incf *pot* (reduce #'+ *money*))
    (setq *stake* 0)
    (setq *money* (make-list *table-size* :initial-element 0))
    (setq *active-player* (next-player -1))
    (when *still-playing* (setq *hs* (ocena-flop *hand* *table*)))
    (push 'flop *history*))


(defun turn(a)
  (push a *table*)
  (incf *pot* (reduce #'+ *money*))
  (setq *money* (make-list *table-size* :initial-element 0))
  (setq *stake* 0)
  (setq *active-player* (next-player -1))
  (when *still-playing* (setq *hs* (ocena-post-flop *hand* *table*)))
  (push 'turn *history*))


(defun river(a) 
  (push a *table*)
  (incf *pot* (reduce #'+ *money*))
  (setq *money* (make-list *table-size* :initial-element 0))
  (setq *stake* 0)
  (when *still-playing* (setq *hs* (ocena-post-flop *hand* *table*))) 
  (setq *active-player* (next-player -1))
  (push 'river *history*))


(defun check(&optional (stake *stake*))
    (when stake
      (setq *stake* stake))
    (setf (nth *active-player* *money*) stake)
    (push 'c (nth *active-player* *akcje*))
    (push 'c *history*)
    (next-player!))


(defun bet(&optional stake)
    (if stake
       (setq *stake* stake)
       (incf *stake* (if (>= (length *table*) 4) (* 2 *blind*) *blind*  )))
    (setf (nth *active-player* *money*) *stake*)
    (push 'b (nth *active-player* *akcje*))
    (push 'b *history*)
    (next-player!))


(defun fold() 
   (setf (nth *active-player* *akcje*) nil)
   (push 'f *history*)
   (next-player!))




;;--------------------------------------------------------------------------
(defun sklansky-group(karta1 karta2)
 "karta1, karta2 to  karty na rece"
  (macrolet
     ((oba(&body wartosci)
       `(or ,@(mapcar 
              (lambda (w)  `(and (eq ,w figura1) (eq ,w figura2)) )
               wartosci) ))
       (oba2(wart1 wart2)
        `(or(and (eq ,wart2 figura1) (eq ,wart1 figura2))
            (and (eq ,wart1 figura1) (eq ,wart2 figura2))) )
      (same-color(warunek)
          `(and (= kolor1 kolor2) ,warunek) ))
 (let ((kolor1 (l->kolor karta1)) 
       (figura1 (nth (l->figura karta1) figury))
       (kolor2 (l->kolor karta2))
       (figura2 (nth (l->figura karta2) figury)))
 (cond
  ((oba 'A 'K 'Q 'J)  1)
  ((same-color (oba2 'A 'K)) 1)
  ((oba '10) 2)
  ((same-color (or (oba2 'A 'Q) (oba2 'A 'J) (oba2 'K 'Q) ) ) 2)
  ((oba2 'A 'K) 2)  
  ((oba '9) 3)
  ((same-color (or (oba2 'J '10) (oba2 'Q 'J) (oba2 'K 'J) (oba2 'A '10))) 3)
  ((oba2 'A 'Q) 3)
  ((oba '8) 4)
  ((or (oba2 'K 'Q) (oba2 'A 'J)) 4)
  ((same-color (or (oba2 '10 '9) (oba2 'Q '10) (oba2 '9 '8) (oba2 'J '9))) 4)
  ((oba '7) 5)
  ((or (oba2 'K 'J) (oba2 'Q 'J) (oba2 'J '10)) 5)
  ((same-color (or (oba2 '8 '7) (oba2 'Q '9) (oba2 '10 '8) (oba2 '7 '6) (oba2 '9 '7)(oba2 '6 '5))) 5)
  ((same-color (or (eq figura1 'A)(eq figura2 'A))) 5)
  ((oba '6 '5) 6)
  ((or (oba2 'A '10) (oba2 'K '10) (oba2 'Q '10)) 6)
  ((same-color (or (oba2 '8 '6) (oba2 '5 '4) (oba2 'K '9) (oba2 'J '8) (oba2 '7 '5))) 6)
  ((oba '4 '3 '2) 7)
  ((or (oba2 'J '9) (oba2 '10 '9) (oba2 '9 '8)) 7)
  ((same-color (or (oba2 '6 '4) (oba2 '5 '3) (oba2 '4 '3) (oba2 '10 '7) (oba2 'Q '8))) 7)
  ((same-color (or (eq figura1 'K)(eq figura2 'K))) 7)

  ((or (oba2 '8 '7) (oba2 'A '9) (oba2 'Q '9) (oba2 '7 '6) (oba2 'J '8) (oba2 '6 '5)
       (oba2 '5'4) (oba2 'K '9) (oba2 '10 '8)) 8)
  ((same-color (or (oba2 '4 '2) (oba2 '3 '2) (oba2 '9 '6) (oba2 '8 '5) (oba2 'J '7) (oba2 '7 '4))) 8) ))))



(defun sklansky(&optional (h *hand*) )
  (sklansky-group (first h) (second h)))


;;---------- FUNKCJE STANU GRY -------------------------------------
(defun szansa(a b)
  (< (random b) a) )

(defun first-to-act?()
 (loop for i from 0 to (1- *my-pos*) do
       (when (nth i *akcje*) (return-from first-to-act? nil)))
 t)

(defun last-to-act?()
 (loop for i from (1+ *my-pos*) to *table-size* do
       (when (nth i *akcje*) (return-from last-to-act? nil)))
 t)

;zwraca pozycje osoby podbijajcej w ostatniej rundzie:
(defun last-round-raiser()
  (let ((pv (reverse 
		(round-history
		 (previous-round
		  (game-round)))) ))
    (if (eq 'flop (game-round))
	(or
	 (and (eq 'b (nth (- *table-size* 2) pv)) 0)
	 (and (eq 'b (nth (- *table-size* 1) pv)) 1)
	 (aif (position 'b pv) (+ it 2)) )
       (position 'b pv)  	) ))

(defun before-raiser?()
  (aif (last-round-raiser) (<= *my-pos* it)))

(defun after-raiser?()
  (aif (last-round-raiser) (> *my-pos* it)))

(defun no-raiser?()
  (null (last-round-raiser)))


(defun players-left()
  (count-if #'(lambda (x) (not (null x))) *akcje*))

(defun pot()
  (+ *pot* (reduce #'+ *money*)))

(defun pot-odds()
 (let ((to-pay (- *stake* (nth *my-pos* *money*))))
  (if (plusp to-pay) (/ (pot) to-pay) 1000)))


(defun implied-odds()
 (let ((to-pay (- *stake* (nth *my-pos* *money*))))
  (if (plusp to-pay) (/ (+  (* 2 *stake*) (pot)) to-pay) 1000)))


(defun odds(ilosc-dojsc)
 (if (zerop ilosc-dojsc) 100 (/ (- 52 (length *hand*) (length *table*) ilosc-dojsc )  ilosc-dojsc)))


(defun game-round()
  (case (length *table*)
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )

(defun previous-round(smb)
  (case smb
    ('flop 'pre-flop)
    ('turn 'flop)
    ('river 'turn)))

(defun next-round(smb)
  (case smb
    ('pre-flop 'flop)
    ('flop 'turn)
    ('turn 'river)))

(defun round-history(&optional (r (game-round)) (h *history*))
  (cond
    ((eq r 'pre-flop)  (if (zerop (length *table*)) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 



(defun count-bets(&optional (r (game-round)))
 (count 'b (round-history r)))


(defun v->l(vect)
  (coerce vect 'list))

(defun s->val(symb)
 (case symb
  ('hand *vhand*)
  ('table *vtable*)
  ('preflop 0)
  ('flop 3)
  ('turn 4)
  ('river 5)

  ('para 7)
  ('dwie-pary 6)
  ('trojka 5)
  ('street 4)
  ('kolor  3)
  ('full  2)

  ('sblind 0)
  ('bblind 1)))


(defmacro rank(smb what oper)
 `(let ((ocena (or (and (null ,what) *ocena*) 
                  (or (and (eq ,what 'hand) (ocena *hand*))
                      (ocena *table*)))))
   (funcall ,oper (car ocena) (s->val ,smb) )))

(defun rank=(smb &optional what)
  (rank smb what #'=))

(defun rank>=(smb &optional what)
  (rank smb what #'<=))

(defun rank<(smb &optional what)
  (rank smb what #'>))

(defun rank>(smb &optional what)
  (rank smb what #'<))

(defun rank<=(smb &optional what)
  (rank smb what #'>=))


(defun next-player(biezacy)
  (when (= biezacy (1- *table-size*))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr *akcje*)
      nr
      (next-player nr)) ))

(defun next-player!()
  (setq *active-player* (next-player *active-player*)))

(defun my-last-action()
 (first (nth *my-pos* *akcje*))  )

(defun my-pos()
  (cond
   ((= 0 *my-pos*)  'small-blind)
   ((= 1 *my-pos*)  'big-blind)
   ((or (= (1- *table-size*) *my-pos*)
        (= (- *table-size* 2) *my-pos*)) 'late-pos)
   ((<= *table-size* 5) 'late-pos)
   ((and (>= *table-size* 9)(< *my-pos* 5)) 'early-pos)
   ((and (>= *table-size* 7)(< *my-pos* 4)) 'early-pos)
   ((and (= *table-size* 6)(= *my-pos* 2)) 'early-pos)
   (t 'middle-pos)    ))


(defun ustaw-baze-faktow()
 (setq *facts* nil *memory* nil)
 (let ((stol  (sort (mapcar #'l->figura *table*) #'<) )
       (reka  (sort (mapcar #'l->figura *hand*) #'<) ))
  (setq H1 (nth 0 reka) H2 (nth 1 reka))
  (setq T1 (nth 0 stol) T2 (nth 1 stol) T3 (nth 2 stol))
  (setq T4 (nth 3 stol) T5 (nth 4 stol)))

 (when (plusp (length *table*)) 
     (setq *ocena* (ocena (append *hand* *table*)))
     (setq *vhand* (wektor-figur *hand*)) 
     (setq *vtable* (wektor-figur *table*)) ))

(defun get-action-from-rules(table)
  (let (temp)
  (flet ((usun (lista) (if (> (length lista) 1) (last lista) lista  ) ))
  (or
   (and (prove table 'fold) (setq *chosen-rule* (usun *chosen-rule*)) "fold")
   (and (prove table 'bet)
	(progn (setq temp (usun *chosen-rule*) *chosen-rule* nil ) t)
	(if (prove table 'dont-bet)
	    (progn (setq *chosen-rule* (usun *chosen-rule*)) "check")
	    (progn (setq *chosen-rule* temp)"bet"))) 
   (and (prove table 'check)  (setq *chosen-rule* (usun *chosen-rule*)) "check")
   (and (progn  (setq *still-playing* nil) (setq *chosen-rule* 'no-rule)) "fold"))) ))


(defun action()
 (let (wynik)
  (when (null *hand*) (return-from action "repeat"))
  (when (null *my-pos*) (setq *my-pos* *active-player*))
  (ustaw-baze-faktow)
  (setq *chosen-rule* nil)
  (setq wynik   (get-action-from-rules (make-reasoner '+poker-rules+)))
  (logs wynik)
  wynik))



;;------------------------- TEXAS Defs. -----------------------------
;;-------------------------------------------------------------------

(defun max-of-colors(&optional (stol *table*))
 (let ((w-kolorow (wektor-kolorow stol)))
  (apply #'max (coerce w-kolorow 'list) )));ilosc na stole!

(defun color-outs?(&optional (reka *hand*)(stol *table*))
 (let ((colhand (wektor-kolorow reka))
       (coltable (wektor-kolorow stol)))
 (flet ((silna-karta (color) (or
			      (member (+ 1 (* 13 color)) *hand*)
			      (member (+ 2 (* 13 color)) *hand*)
			      (member (+ 3 (* 13 color)) *hand*)) ))
  (or (dotimes (i 4) (when (and (= 2 (aref colhand i))
                            (= 2 (aref coltable i)))
                        (return t)))
     (dotimes (i 4) (when (and (= 1 (aref colhand i))
                            (= 3 (aref coltable i)))
                         (return (silna-karta i))))
   ))))

(defun color-nuts?(&optional (reka *hand*)(stol *table*))
  (flet ((get-color ()
	   (loop for c from 0 to 3 do
		 (when (>= (aref (wektor-kolorow (append reka stol)) c) 5)
		   (return-from get-color c)))))
    (let ((col (get-color)))
      (when col
      (or  (member (+ 1 (* 13 col)) reka)
	   (and (member (+ 1 (* 13 col)) stol)
		(member (+ 2 (* 13 col)) reka))
	   (and (member (+ 1 (* 13 col)) stol)
		(member (+ 2 (* 13 col)) stol)
		(member (+ 3 (* 13 col)) reka))   )))))

(defun num-of-1-street-outs(&optional (karty (append *hand* *table*)))
 "Ile kart mozna dolozyc do listy karty aby dostac strita"
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (when  (street (wektor-figur (cons i karty))) (incf ilosc)))))

;j.w.
(defun num-of-2-street-outs(karty)
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (dotimes (j 13 ilosc)
      (when  (street (wektor-figur (cons j (cons i karty)))) (incf ilosc))))))

(defun street-outs?()
   (let ((o1 (num-of-1-street-outs)) (o2 (num-of-1-street-outs *table*)))
     (and (plusp o1) (> o1 o2))))

(defun my-outs()
 (let ((ilosc 0) (nc (max-of-colors)))
   (when (color-outs?) (incf ilosc 9))
   (when (and (< nc 3) (street-outs?))
     (if (= 2 nc)
	 (incf ilosc (* 3 (num-of-1-street-outs)))
	 (incf ilosc (* 4 (num-of-1-street-outs)))) )
   ilosc))

(defun pary-sklanskyego(&optional (karty (talia)))
 (mappend #'(lambda(p) (when (sklansky p) (list p))) (podzbiory-2 karty)))

(defun pojedynek( reka stol ilosc-przeciwnikow ilosc-testow mozliwe-pary)
  (let ( (przeciwnicy nil)
	 (moja-ocena (ocena (append reka stol))) 
	 (zwyciestwa 0)
	 (porazki 0) 
	 (wygralem nil) )
 (dotimes (i ilosc-testow)
    (setq przeciwnicy (subseq (tasuj mozliwe-pary) 0 ilosc-przeciwnikow));nie dokladne!
    (setq wygralem t)
    (dolist (p przeciwnicy)
      (when (= 1 (lepsza (ocena (append p stol)) moja-ocena)) (setq wygralem nil) (return)))
    (if wygralem (incf zwyciestwa) (incf porazki))  )

  (if (zerop zwyciestwa) 1000 (/ (* 1.0 porazki) zwyciestwa)) ))

(defun ocena-flop(reka flop &optional (ilosc-testow 400))
  (pojedynek reka flop 1 ilosc-testow (pary-sklanskyego 
			      (wyjmij (tasuj (talia)) (append reka flop)))))

(defun pary-do-trojki(reka flop)
  (mappend #'(lambda (p)
	       (when (or 
		      (color-outs? p flop)
		      (plusp (num-of-1-street-outs (append p flop)))
		      (< (car (ocena (append p flop))) 8)  )
		       (list p)))  
	   (pary-sklanskyego (wyjmij (talia) (append reka flop)))))

(defun ocena-post-flop(reka stol)
  (pojedynek reka stol 1 400 (pary-do-trojki reka (subseq (reverse stol) 0 3))))



;;------------------------- GAME RULES ------------------------------
;;-------------------------------------------------------------------

(setq +defs-list+
  '((no-bets          (zerop (count-bets)))
    (with-bets        (plusp (count-bets)))
    (one-bet          (= 1 (count-bets)))
    (raise            (= 2 (count-bets)))
    (with-raise       (> (count-bets) 1))
    (reraise          (= 3 (count-bets)))
    (no-raise         (< (count-bets) 2))
    (no-reraise       (< (count-bets) 3))

    (heads-up         (= 2 *table-size*))
    (shorthand        (and (> *table-size* 2) (< *table-size* 6)))
    (fulltable        (> *table-size* 5))

    (early-pos        (eq (my-pos) 'early-pos))
    (middle-pos       (eq (my-pos) 'middle-pos))
    (late-pos         (eq (my-pos) 'late-pos))
    (big-blind        (eq (my-pos) 'big-blind))
    (small-blind      (eq (my-pos) 'small-blind))

    (pre-flop          (zerop (length *table*)))
    (post-flop         (plusp (length *table*)))
    (after-flop        (> (length *table*) 3))
    (flop              (= 3 (length *table*)))
    (turn              (= 4 (length *table*)))
    (river             (= 5 (length *table*)))
    (before-river      (> 5 (length *table*)))

    (sklansky         (sklansky))
    (top-pair         para (= H1 T1))
    (top-pair-kicker  para (= H2 T1))
    (top-pair         top-pair-kicker)
    (overpair         (= H1 H2) para (< H1 T1))
    (second-pair      (or (= H1 T2) (= H2 T2)))
    (second-pair      (= H1 H2) (< H2 T2))
    (no-pair-table    (rank< 'para 'table))
    (pair-table       (rank= 'para 'table))
    (rainbow          (< (max-of-colors) 3))
    (no-rainbow       (>= (max-of-colors) 3))
    (no-street-table  (not (plusp (num-of-1-street-outs *table*))))
    (street-table     (plusp (num-of-1-street-outs *table*)))
    (safe-table       no-pair-table rainbow no-street-table)
    (para             (rank= 'para))
    (dwie-pary        (rank= 'dwie-pary))
    (trojka           (rank= 'trojka))
    (set              trojka no-pair-table)
    (street           (rank= 'street))
    (kolor            (rank= 'kolor))
    (full             (rank= 'full))
    (mocna-para       top-pair)
    (mocna-para       overpair)

    (stol-blotek      (> T1 4))
    (doszla-blotka    (> (l-> figura (first *table*))  4))
    (i-checked        (eq (my-last-action) 'check))  ))



(setq +poker-rules+
 '((check    post-flop no-bets)
   (check    pre-flop  (reason-check-with +pre-flop-rules+))
   (bet      pre-flop  (reason-bet-with +pre-flop-rules+))

   (check    post-flop (reason-check-with +post-flop-rules+))
   (dont-bet post-flop (reason-dont-bet-with +post-flop-rules+))
   (fold     post-flop (reason-fold-with +post-flop-rules+))
   (bet      post-flop (reason-bet-with +post-flop-rules+))

))