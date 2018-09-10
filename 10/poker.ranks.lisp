;(declaim (optimize (speed 3) (safety 0)))

(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena))

(in-package :poker.ranks)

;karty to liczby od 1 do 52, 1=As Pik,2=Krol Pik,52=2 Trefl
;kolory: 0=Pik, 1=Kier, 2=Karo, 3=Trefl
;figury: 0=As, 1=Krol, 12=Dwojka


;;--------------------------------------------------------------------
;; funkcje oceny zestawow kart:

(defun l->kolor(liczba)
  (floor (1- liczba) 13))

(defun l->figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a ¿eby nie bylo ostrzezen
   b))

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

;------------------------------------------------------
;------------------------------------------------------
(defun omaha-zestawy(reka stol)
           (let (wynik)
             (dolist (r (podzbiory-2 reka))
               (dolist (s (podzbiory-3 stol))
                   (push (append r s) wynik)))
          wynik))

(defun omaha-ocena(reka stol)
 (labels ((najlepszy(lista f-oceny &optional (best '(10)))
           (if (null lista) 
            best
             (let ((nowy (ocena (car lista))))
              (if (= 1 (lepsza nowy best))
                (najlepszy (cdr lista) f-oceny nowy)
                (najlepszy (cdr lista) f-oceny best))))))
 (najlepszy (omaha-zestawy reka stol) #'ocena)))


