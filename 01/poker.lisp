;(declaim (optimize (speed 3) (safety 0)))

(defpackage :poker
  (:use :cl)
  (:export #:l->karta #:l->figura #:l->kolor #:talia #:ocena-flop2 #:ocena-turn #:ocena-river           #:sklansky-group))

(in-package :poker)

(defvar kolory '(S H D C))
(defvar kolory-long '(Pik Kier Karo Trefl))
(defvar figury '(A K Q J T 9 8 7 6 5 4 3 2))
(defvar figury-long '(As Krol Dama Walet 10 9 8 7 6 5 4 3 2))

(defun pobierz( wyrazy)
 "wyrazy to string zawierajacy wyrazy oddzielone spacjami
  zwracamy liste symboli"
  (if (= 0  (length wyrazy)) 
        nil
        (multiple-value-bind (wyraz indeks) (read-from-string wyrazy)
          (cons wyraz (pobierz (subseq wyrazy indeks ))) ) ))
   
(defmacro linia()
  '(format t "~&------------------------------------~&"))

(defun o->liczba( opis )
 "Zamienia opis karty na liczbe. AC -> liczba(Asa Trefl)"
 (let ((o1 (char (symbol-name opis) 0))
       (o2 (char (symbol-name opis) 1)))
   (+
     (case (char-code o1)
        (65 1) (75 2) (81 3) (74 4) (84 5) (57 6) (56 7) (55 8) (54 9)
        (53 10) (52 11) (51 12) (50 13))   
     (* 13
        (case (char-code o2)
           (83 0)(72 1)(68 2)(67 3))  )) ))

;; --------- SYMULATOR ----------------
;; ------------------------------------
(let ((stol nil) (reka nil) (sklansky 0) (pot 0) (ilosc-graczy 10)
       (zwyciestwa 0)(remisy 0)(porazki 0))

   (defun reka(r1 r2)
        (setq stol nil pot 0) 
        (setq reka (list (o->liczba r1) (o->liczba r2)))
        (setq sklansky (sklansky-group (l->karta (first reka))
                                       (l->karta (second reka)))) )
   (defun gracze(ilosc) (setq ilosc-graczy ilosc))
   (defun stol(&rest karty )
        (setq stol (nconc stol (mapcar #'o->liczba karty)))
        (ocena-stolu))
   (defun pot(ile) (incf pot ile))
   (defun ocena-stolu()
     (destructuring-bind (z r p)
        (case (length stol)
           (3 (ocena-flop2 reka stol))
           (4 (ocena-turn reka stol))
           (5 (ocena-river reka stol)))
        (setq zwyciestwa z remisy r porazki p)   ))
   (defun prawdopodobienstwo() 
           (if (plusp zwyciestwa) 
                     (coerce (/ zwyciestwa (+ zwyciestwa porazki)) 'float   )
                     0)) 
   (defun opis()
             (format t "REKA: ~{~A~}     STOL: ~{~A~} ~&" 
                       (mapcar #'l->karta  reka)
                       (mapcar #'l->karta stol))
             (when (>= (length stol) 3)
                          (format t "ocena stolu: [~A;~A;~A] , p-stwo wygranej: ~A , p-stwo/ilosc-graczy: ~A~&" 
                                     zwyciestwa remisy porazki (prawdopodobienstwo) (/ (prawdopodobienstwo) ilosc-graczy )  )) 
             (format t "ilosc-graczy: ~A,  sklansky: ~A  , " ilosc-graczy sklansky)
             (format t "pot: ~A , pot-odds: ~A, pot-odds/ilosc-graczy: ~A" pot (* pot (prawdopodobienstwo)) (/ (* pot (prawdopodobienstwo)) ilosc-graczy) ) )
   (defun simula()
     (let (wiersz)
      (loop
       (linia)(princ #\Newline)
       (opis)
       (princ #\Newline)(linia)
       (format t "...>  ")
       (setq wiersz (pobierz (read-line)))
       (when (eq 'bye (car wiersz)) (return-from simula 'Bye.))
       (eval wiersz))))   )

;; --------------------------------------

(defstruct (karta (:print-function (lambda (p s k ) 
		    (format s "<~A ~A>"(nth (karta-figura p) figury-long)
		                       (nth (karta-kolor p) kolory-long)) )))
   kolor
   figura)

(defmacro karta(figura kolor)
"Tworzy karte w oparciu o symbole kolorow i figur (karta A S) == As Pik"
 (let ((i1 (position figura figury))
       (i2 (position kolor kolory)))
   `(make-karta :kolor ,i2 :figura ,i1)))

;; talia bedzie lista liczb od 1 do 52 reprezentujaca karty
(defun talia()
 (let (wynik)
  (dotimes (i 52 (nreverse wynik))
   (push (1+ i) wynik))))

(defun karta->liczba(karta)
 (+ 1 (* 13 (karta-kolor karta)) (karta-figura karta)))

(defun l->kolor(liczba)
  (floor (/ (1- liczba) 13)))

(defun l->figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) b))

(defun l->karta(liczba)
  (make-karta :figura (l->figura liczba) 
              :kolor (l->kolor liczba)))

(defun wyjmij(skad &rest listy)
 "Usuwa z listy <skad> elementy z list <listy>"
  (dolist (l listy)
    (dolist (el l)
      (setf skad (remove el skad)))) 
  skad)

(defun podzbiory(lista)
"Zwraca liste podzbiorow dwu-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)) wynik)))
  wynik ))

(defun podzbiory-3(lista)
"Zwraca liste podzbiorow piecio-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)  (nth k lista) ) wynik))))
  (nreverse wynik) ))

(defun podzbiory-5(lista)
"Zwraca liste podzbiorow piecio-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
      (loop for l from (1+ k) to (1- ilosc) do
       (loop for m from (1+ l) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)  (nth k lista)
                   (nth l lista) (nth m lista) ) wynik))))))
  (nreverse wynik) ))

(defun ocena-pre-flop(reka &optional (dokladnosc 900))
  (let ( (reszta (wyjmij (talia) reka )) (x 0) (y 0) (z 0) )  
    (dolist (stol (podzbiory-5 reszta)) 
     (progn
       (when (> (random 1000)  dokladnosc) 
       (dolist (przeciwnik (podzbiory (wyjmij reszta stol))) ;<przeciwnik> to karty przeciwnika
         ;(format t "~A ~A ~&" (append stol p reka)(append stol p przeciwnik))
         (case (lepsza (append stol reka) (append stol przeciwnik))
         (1 (incf x))
         (0  (incf y))
         (-1 (incf z)))
        )) ))   
    (list x  y z)))

(defun ocena-flop(reka stol)
  "Liczy p-stwo wygranej. <Reka> i <stol> to listy liczb.
   <reka> ma dlugosc 2, <stol> ma dlugosc 3"
  (let ( (reszta (wyjmij (talia) reka stol)) (x 0) (y 0) (z 0) )  
    (dolist (p (podzbiory reszta))   ;<p> bedzie dodane do stolu
        (dolist (przeciwnik (podzbiory (wyjmij reszta p))) ;<przeciwnik> to karty przeciwnika
         ;(format t "~A ~A ~&" (append stol p reka)(append stol p przeciwnik))
         (case (lepsza (append stol p reka) (append stol p przeciwnik))
         (1 (incf x))
         (0  (incf y))
         (-1 (incf z)))
        ))   
    (list x  y z)))

(defun sumuj-oceny(o1 o2) (vector (+ (svref o1 0)(svref o2 0))  (+ (svref o1 1)(svref o2 1)) (+ (svref o1 2)(svref o2 2)) ))
(defun ocena3(&rest liczby) (ocena-5 liczby))
(defun lepsza3(wektor1 wektor2) 'ok)
(defun ocena-flop3(r1 r2 s1 s2 s3)
  "Liczy p-stwo wygranej. <Reka> i <stol> to listy liczb.
   <reka> ma dlugosc 2, <stol> ma dlugosc 3"
  (let ( (reszta (coerce (wyjmij (talia) (list r1 r2 s1 s2 s3)) 'vector)) (x 0) (y 0) (z 0) )
;wybieramy wszystkie podzbiory piecio-elementowe na stol i dla przeciwnika:
 (loop for i from 0 to 47 do
    (loop for j from (1+ i) to 46 do
     (loop for k from (1+ j) to 46 do
      (loop for l from (1+ k) to 46 do
       (loop for m from (1+ l) to 46 do
            (lepsza3 (ocena3 r1 r2 s1 s2 s3 (svref reszta i)(svref reszta j) )
                     (ocena3 s1 s2 s3 (svref reszta i)(svref reszta j)(svref reszta k)(svref reszta l) (svref reszta m))   ) )))))      
   
   reszta))

#|
    (dolist (p (podzbiory reszta))   ;<p> bedzie dodane do stolu
        (dolist (przeciwnik (podzbiory (wyjmij reszta p))) ;<przeciwnik> to karty przeciwnika
         ;(format t "~A ~A ~&" (append stol p reka)(append stol p przeciwnik))
         (case (lepsza (append stol p reka) (append stol p przeciwnik))
         (1 (incf x))
         (0  (incf y))
         (-1 (incf z)))
        ))   
    (list x  y z)))
|#
(defun ocena-flop2(reka stol &optional (dokladnosc 900))
  "Liczy p-stwo wygranej. <Reka> i <stol> to listy liczb.
   <reka> ma dlugosc 2, <stol> ma dlugosc 3"
  (let ( (reszta (wyjmij (talia) reka stol)) (x 0) (y 0) (z 0) )  
    (dolist (p (podzbiory reszta))   ;<p> bedzie dodane do stolu
        (dolist (przeciwnik (podzbiory (wyjmij reszta p))) ;<przeciwnik> to karty przeciwnika
          (when (> (random 1000) dokladnosc)
            (case (lepsza2 (append stol p reka) (append stol p przeciwnik))
              (1 (incf x))
              (0  (incf y))
              (-1 (incf z))))
        ))   
   (format nil "Zwyciestw: ~A Remisow: ~A Porazek: ~A;  P-stwo zwyciestwa ~A~&"x y z (p-stwo (list x  y z))) ))

(defun ocena-turn(reka stol)
  "<reka> ma dlugosc 2, <stol> ma dlugosc 4"
  (let ( (reszta (wyjmij (talia) reka stol)) (x 0) (y 0) (z 0) )  
    (dolist (p reszta)   ;<p> bedzie dodane do stolu
     (progn
       (setq p (list p)) 
       (dolist (przeciwnik (podzbiory (wyjmij reszta p))) ;<przeciwnik> to karty przeciwnika
         ;(format t "~A ~A ~&" (append stol p reka)(append stol p przeciwnik))
         (case (lepsza (append stol p reka) (append stol p przeciwnik))
         (1 (incf x))
         (0  (incf y))
         (-1 (incf z)))
        )))
       (format nil "Zwyciestw: ~A Remisow: ~A Porazek: ~A;  P-stwo zwyciestwa ~A~&"x y z (p-stwo (list x  y z)))      ))

(defun ocena-river(reka stol)
  "<reka> ma dlugosc 2, <stol> ma dlugosc 5"
  (let ( (reszta (wyjmij (talia) reka stol)) (x 0) (y 0) (z 0) 
         (moje (append stol reka)))  
     (dolist (przeciwnik (podzbiory reszta )) ;<przeciwnik> to karty przeciwnika
         ;(format t "~A ~A ~&" (append stol p reka)(append stol p przeciwnik))
         (case (lepsza moje (append stol przeciwnik))
         (1 (incf x))
         (0  (incf y))
         (-1 (incf z)))
        )   
       (format nil "Zwyciestw: ~A Remisow: ~A Porazek: ~A;  P-stwo zwyciestwa ~A~&"x y z (p-stwo (list x  y z)))  ))

;;--------------------------------------------------------------
;; funkcje sluzace do porownania i oceny zestawow kart
;; zestaw to lista kart

(defmacro lepsza-wartosc(w1 w2)
 "Uzywane przez <lepsza> do zwracania wyniku"
 `(let ((it (cond
             ((and ,w1 ,w2) (if (= ,w1 ,w2) 0 (if (< ,w1 ,w2) 1 -1 ) ))
             (,w2 -1)
             (,w1 1))))
      (when it  (return-from lepsza it))))

(defmacro lepsza-wartosc2(w1 w2)
 "Uzywane przez <lepsza> do zwracania wyniku"
 `(let ((it (cond
             ((and ,w1 ,w2) (if (= ,w1 ,w2) 0 (if (< ,w1 ,w2) 1 -1 ) ))
             (,w2 -1)
             (,w1 1))))
      (when it  (return-from lepsza2 it))))

(defun ocena-5 (liczby)
 "zwraca ocene listy pieciu liczb"
  (let ((figury (make-array 13 :initial-element 0))
        (kolory (make-array 4 :initial-element 0))
        (ocena  (make-array 9 :initial-element nil))
         kolor street trojka para)
 
    (dolist (l liczby)
         (incf (aref figury (l->figura l)))
         (incf (aref kolory (l->kolor l))))
   (setq kolor (kolor kolory))
   (setq street (street figury))
   (when (and street kolor) (setf (aref ocena 0) (poker (copy-list liczby))))  ;poker
   (setf (aref ocena 1) (szukaj-wartosci figury 4)) ;kareta
   (setq trojka (szukaj-wartosci figury 3))
   (setq para (szukaj-wartosci figury 2))
   (when (and trojka para) (setf (aref ocena 2) (+ (* 100 trojka) para))) ;full
   (when kolor (setf (aref ocena 3) kolor)) ; kolor
   (when street (setf (aref ocena 4) street))  ;street
   (when trojka (setf (aref ocena 5) trojka))  ;trojka
   (when para 
         (setf (aref ocena 7) para) ;para
         (setf (aref ocena 6) (dwie-pary figury para))) ;dwie-pary
   
   (setf (aref ocena 8) (najwyzsza figury))
   ocena))


(let ((table (make-hash-table :test #'equal)))
 (defun ile-memoize() (hash-table-count table))
 (defun clear-memoize() (clrhash table))
 (defun ocena-memoize(liczby)
   (let ((args (sort (copy-list liczby) #'<)))
           (multiple-value-bind (val found)
                             (gethash args table)
              (if found val
                    (setf (gethash args table) (ocena-5 args))) )   )))

(defun lepsza2(liczby1 liczby2)
  (let ((o1 (ocena-memoize liczby1))
        (o2 (ocena-memoize liczby2)))
  (dotimes (i 13)
      (lepsza-wartosc2 (aref o1 i) (aref o2 i)))))

   
(defun lepsza( liczby1 liczby2 )
 "Zwraca 1 jesli karty opisane przez liste <liczby1> sa lepsze niz
  karty <liczby2> , -1 w przeciwnym przypadku, 0 to remis"
 ;(format t "~A , ~A~&" liczby1 liczby2)
 (let ((f1 (make-array 13 :initial-element 0)) ; te wektory opisuja liczby figur i kolorow
       (k1 (make-array 4 :initial-element 0)) ; w poszczegolnych zestawach
       (f2 (make-array 13 :initial-element 0))
       (k2 (make-array 4 :initial-element 0))
        kolor1 kolor2 street1 street2 poker1 poker2 kareta1 kareta2
        trojka1 trojka2 para1 para2 full1 full2 dwie-pary1 dwie-pary2
        najwyzsza1 najwyzsza2)
    (dolist (l1 liczby1) 
               (incf (aref f1 (l->figura l1)))  
               (incf (aref k1 (l->kolor l1))) )
    (dolist (l2 liczby2) 
               (incf (aref f2 (l->figura l2)))  
               (incf (aref k2 (l->kolor l2))) )
   ;(opisz-temp)
   (setq kolor1 (kolor k1))    (setq kolor2 (kolor k2))
   (setq street1 (street f1))  (setq street2 (street f2))
   (setq poker1 (when (and kolor1 street1) (poker (copy-list liczby1))))   
   (setq poker2 (when (and kolor2 street2) (poker (copy-list liczby2))))
   ;(princ "sprawdzamy pokera ")
   (lepsza-wartosc poker1 poker2)
   (setq kareta1 (szukaj-wartosci f1 4))  (setq kareta2 (szukaj-wartosci f2 4))
   ;(princ "karete ")
   (lepsza-wartosc kareta1 kareta2)
   (setq trojka1 (szukaj-wartosci f1 3))  (setq trojka2 (szukaj-wartosci f2 3))
   (setq para1 (szukaj-wartosci f1 2))  (setq para2 (szukaj-wartosci f2 2))
   (when (and trojka1 para1) (setq full1 (cons trojka1 para1)))
   (when (and trojka2 para2) (setq full2 (cons trojka2 para2)))
   ;(princ "fulla ")
   (if (and full1 full2) 
          (cond
           ((= (car full1)(car full2)) (lepsza-wartosc (cdr full1)(cdr full2)) )
           (t (lepsza-wartosc (car full1)(car full2)))  ))
   (when full2 (return-from lepsza -1))
   (when full1 (return-from lepsza 1))
   ;(princ "kolor ")
   (lepsza-wartosc kolor1 kolor2)
   ;(princ "streeta ")
   (lepsza-wartosc street1 street2)
   ;(princ "trojke ")
   (lepsza-wartosc trojka1 trojka2)
   (setq para1 (szukaj-wartosci f1 2))  (setq para2 (szukaj-wartosci f2 2))
   (when para1 (setq dwie-pary1  (dotimes (i (- 12 para1)) 
                                      (when (= 2 (aref f1 (+ 1 i para1))) 
                                         (return (cons para1 (+ i 1 para1) ))   )    ) ))

   (when para2 (setq dwie-pary2  (dotimes (i (- 12 para2)) 
                                      (when (= 2 (aref f1 (+ 1 i para2))) 
                                         (return (cons para2 (+ i 1 para2) ))   )    ) ))
   ;(princ "dwie-pary ")
   (if (and dwie-pary1 dwie-pary2) 
          (cond
           ((= (car dwie-pary1)(car dwie-pary2)) (lepsza-wartosc (cdr dwie-pary1)(cdr dwie-pary2)) )
           (t (lepsza-wartosc (car dwie-pary1)(car dwie-pary2)))  ))
   (when dwie-pary2 (return-from lepsza -1))
   (when dwie-pary1 (return-from lepsza 1))
   ;(princ "para ")
   (lepsza-wartosc para1 para2)
   ;(princ "najwysza karta")
   (setq najwyzsza1 (najwyzsza f1))   (setq najwyzsza2 (najwyzsza f2))
   (lepsza-wartosc najwyzsza1 najwyzsza2)
   0 ))



(defmacro opisz-temp()
 `(progn
   (format t "zestaw1:~A ~&zestaw2:~A~&" (mapcar #'l->karta liczby1) (mapcar #'l->karta liczby2))
   (format t "kolory1:~A  kolory2:~A~&" k1 k2)
   (format t "figury1:~A  figury2:~A~&" f1 f2)))


(defun kolor(wektor-kolorow)
 (dotimes (i 4) (when (> (aref wektor-kolorow i) 4) (return-from kolor i))))


(defun szukaj-wartosci(wektor-figur wartosc)
;uzywane do szukania karety,trojki,pary itd.
 (dotimes (i 13) (when (= (aref wektor-figur i) wartosc) (return-from szukaj-wartosci i) )))

(defun dwie-pary(wektor-figur para)
   (dotimes (i (- 12 para))
             (when (= 2 (aref wektor-figur (+ i para)))
                   (return-from dwie-pary (+ (* 100 para) i)))))

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
 (do ((i 0 (+ i 1)))
     ((> i 8))
     (when (kolejne wektor-figur i) (return-from street i) ))  )


(defun poker(liczby)
 "Musimy sprawdzac czy street jest w tym samym kolorze. Argumentem jest
 lista liczb <liczby>"
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
   (let ((ciag (sort liczby #'<)))
     (przechodz ciag 1 (car ciag) (car ciag)))   ))

(defun sklansky-group(karta1 karta2)
 "karta1, karta2 to struktury typu karta na rece"
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
 (let ((kolor1 (karta-kolor karta1)) 
       (figura1 (nth (karta-figura karta1) figury))
       (kolor2 (karta-kolor karta2))
       (figura2 (nth (karta-figura karta2) figury)))
 (cond
  ((oba 'A 'K 'Q 'J)  1)
  ((same-color (oba2 'A 'K)) 1)
  ((oba 'T) 2)
  ((same-color (or (oba2 'A 'Q) (oba2 'A 'J) (oba2 'K 'Q) ) ) 2)
  ((oba2 'A 'K) 2)  
  ((oba '9) 3)
  ((same-color (or (oba2 'J 'T) (oba2 'Q 'J) (oba2 'K 'J) (oba2 'A 'T))) 3)
  ((oba2 'A 'Q) 3)
  ((oba '8) 4)
  ((or (oba2 'K 'Q) (oba2 'A 'J)) 4)
  ((same-color (or (oba2 'T '9) (oba2 'Q 'T) (oba2 '9 '8) (oba2 'J '9))) 4)
  ((oba '7) 5)
  ((or (oba2 'K 'J) (oba2 'Q 'J) (oba2 'J 'T)) 5)
  ((same-color (or (oba2 '8 '7) (oba2 'Q '9) (oba2 'T '8) (oba2 '7 '6) (oba2 '9 '7)(oba2 '6 '5))) 5)
  ((same-color (or (eq figura1 'A)(eq figura2 'A))) 5)
  ((oba '6 '5) 6)
  ((or (oba2 'A 'T) (oba2 'K 'T) (oba2 'Q 'T)) 6)
  ((same-color (or (oba2 '8 '6) (oba2 '5 '4) (oba2 'K '9) (oba2 'J '8) (oba2 '7 '5))) 6)
  ((oba '4 '3 '2) 7)
  ((or (oba2 'J '9) (oba2 'T '9) (oba2 '9 '8)) 7)
  ((same-color (or (oba2 '6 '4) (oba2 '5 '3) (oba2 '4 '3) (oba2 'T '7) (oba2 'Q '8))) 7)
  ((same-color (or (eq figura1 'K)(eq figura2 'K))) 7)

  ((or (oba2 '8 '7) (oba2 'A '9) (oba2 'Q '9) (oba2 '7 '6) (oba2 'J '8) (oba2 '6 '5)
       (oba2 '5'4) (oba2 'K '9) (oba2 'T '8)) 8)
  ((same-color (or (oba2 '4 '2) (oba2 '3 '2) (oba2 '9 '6) (oba2 '8 '5) (oba2 'J '7) (oba2 '7 '4))) 8) ))))

(defun ocena-sklansky(r1 r2)
 (format nil "Grupa sklanskyego: ~A~&" (sklansky-group (l->karta r1) (l->karta r2))))


(defun p-stwo(lista)
 (/ (* 1.0 (first lista)) (+ (first lista)(third lista))))


(defun sluchaj (port)
 (let ((got nil))
 (trivial-sockets:with-server (s (:port port :reuse-address t))
   (loop
     (handler-case
      (with-open-stream (c (trivial-sockets:accept-connection s))
         (format t "Polaczenie ~A" c) 
         (setq got (read c))
	 (format t "Got: ~A~&" got)
	 (format c "~A" (eval got)) 
         (force-output c)
         (close c))
     (error (e) (print "Blad polaczenia")))))))


;; ---------------- OBSLUGA BAZY DANYCH
#||
(defmacro with-baza-moja(&body code)
  (let ((polaczenie (gensym)))
   `(let ((,polaczenie (pg-connect "moja" "postgres" :host "192.168.0.1")))   
       (labels ((wyslij (co) (pg-exec ,polaczenie co) ))
        ,@code
        (pg-disconnect ,polaczenie) ))))

(defun baza-dodaj-reke(r1 r2)
  (let* ((reszta (wyjmij (talia) (list r1 r2))) (stoly (podzbiory-3 reszta))
         ocena wynik (polaczenie  (pg-connect "moja" "postgres" :host "192.168.0.1") ))
    (dolist (s stoly)
      (setq ocena (ocena-flop2 (list r1 r2) s 200))
      (setq wynik (format nil "INSERT INTO flop VALUES(~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A)" r1 r2  (first s)(second s)(third s) 200 (first ocena)(second ocena)(third ocena) ))
      (print wynik)
      (pg-exec polaczenie wynik))

      (pg-disconnect polaczenie)    ))

||#