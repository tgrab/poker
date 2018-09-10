;(in-package :poker)
;;--------------------------------------------------------------------
;;                         OMAHA
;;--------------------------------------------------------------------


(defun l->html (liczba)
 (format nil "~A ~A" (nth (l->figura liczba) figury) 
                                (nth (l->kolor liczba) kolory-opis) ))
(defun liczby->html(lista)
 (with-output-to-string(s)
  (format s "<table><tr>")
  (dolist (karta lista)
     (case (l->kolor karta)
       (0 (format s "<td color=black>"))   (1 (format s "<td color=red>"))
       (2 (format s "<td color=red>"))    (3 (format s "<td color=black>")))
     (format s "~A" (l->html karta)))
  (format s "</table>")))

;; Nowy kod potrzebny do policzenia dojsc do nutsa:
(defun wektor-figur(lista)
  (let ((wektor-figur (make-array 13 :initial-element 0))) 
   (dolist (l lista) 
            (incf (aref wektor-figur   (l->figura l))))    
   wektor-figur))

(defun wektor-kolorow(lista)
  (let ((wektor-kolorow (make-array 4 :initial-element 0))) 
   (dolist (l lista) 
            (incf (aref wektor-kolorow   (l->kolor l))))    
   wektor-kolorow))

(defun nuts?(reka stol ocena)
 "Sprawdza czy uklad jest nuts dla koloru,strita lub fulla,
  nie uwzgledniamy wyzszego pokera badz karety"

 (let ((reszta (wyjmij (talia) reka stol)) ocena-przeciwnik
       (wektor-figur (make-array 13 :initial-element 0)) 
       (wektor-kolorow (make-array 4 :initial-element 0)) )

  (labels ((kolor-na-stole? () (dotimes (i 4) 
                                (when (> (aref wektor-kolorow i) 2)
                                      (return-from kolor-na-stole? t) ))) 
           (para-na-stole? () (dotimes (i 13) 
                                (when (> (aref wektor-figur i) 1)
                                      (return-from para-na-stole? t) )))
           (kolor? ()  (= 3 (car ocena)))
           (street? () (>= 4 (car ocena))))
   
   (dolist (l1 stol) 
               (incf (aref wektor-figur   (l->figura l1)))  
               (incf (aref wektor-kolorow (l->kolor l1))) )
   (when (and (street?) (kolor-na-stole?) )  (return-from nuts?  nil))
   (when (and (kolor?) (para-na-stole?) )  (return-from nuts? nil))
    (dolist (przeciwnik (podzbiory-2 reszta))
      (setq ocena-przeciwnik (omaha-ocena przeciwnik stol))
      (when (and (= 1 (lepsza ocena-przeciwnik  ocena ))
                 (> 1 (car ocena-przeciwnik))) ;porazka z pokerem i kareta nas nie interesuje!
          (return-from nuts? nil)   )    )
  t)))



(defun najlepszy(lista f-oceny &optional (best '(10)))
  (if   (null lista) 
       best
       (let ((nowy (ocena (car lista))))
         (if (= 1 (lepsza nowy best))
          (najlepszy (cdr lista) f-oceny nowy)
          (najlepszy (cdr lista) f-oceny best))  )  ) )

(defun omaha-ocena(reka stol)
 (najlepszy (omaha-zestawy reka stol) #'ocena))

(defun omaha-zestawy(reka stol)
"Zwraca uklady pieciokartowe po dwie z reki i trzy ze stolu"
  (let (wynik)
     (dolist (r (podzbiory-2 reka))
       (dolist (s (podzbiory-3 stol))
         (push (append r s) wynik)))
    wynik))


  
;;----------------------------------------------------------------
;;---------------------- INTERFEJS -------------------------------
;;----------------------------------------------------------------

(defvar *reka* nil "Moje karty")
(defvar *stol* nil "Rece na stole")



(defmacro java( &body polecenia )
 `(with-output-to-string(*standard-output*)
    (progn ,@polecenia)
    (format t "~%.~%")    
    ))


(defun reset()
 (setq *reka* nil *stol* nil)
   (java (princ "<hr><center><b>Reset</center></b><hr>")))



(defun omaha-pre-flop(karty)
      (let (wynik)  
          (setq wynik (gethash karty *h2*))
	  (if (null wynik)
	  (format t "Brak ukladu w bazie!")
          (progn
             (format t "<hr><b><center>PRE FLOP</b></center>~A<hr>" (liczby->opis karty))
             (dolist (w wynik)
               (format t "pozycja:<b> ~A</b>  ocena<b>   ~A</b><hr>~&" 
                       (car w) (cdr w)   ) )))))

(defun omaha-flop(reka stol)
   (let ( (reszta  (wyjmij (talia) reka stol)) 
          (dojscia (make-array 9 :initial-element 0))
          ocena ocena2 (ilosc-nuts 0))
    (setq ocena (omaha-ocena reka stol))
    (format t "<center><table border=1><tr><td><b>FLOP</b><td>~A<td>~A<td><b>~A</b></table></center>~&"           (liczby->opis reka) (liczby->opis stol) (opisz-ocene ocena))
    (dolist (karta reszta)
       (setq ocena2 (omaha-ocena reka (cons karta stol)))
       (when (= 1 (lepsza ocena2 ocena))
             (incf (aref dojscia (car ocena2)))))
    (opisz-dojscia (car ocena) 45 dojscia)
    (setq dojscia (make-array 9 :initial-element 0))
    (dolist (karty (podzbiory-2 reszta))
       (setq ocena2 (omaha-ocena  reka  (append karty stol)))

       (when (= 1 (lepsza ocena2 ocena))
          ;   (when (and  (< (car ocena2) 4)
          ;               (nuts? reka (append karty stol) ocena2))
          ;          (incf ilosc-nuts))
             (incf (aref dojscia (car ocena2)))))
    (opisz-dojscia (car ocena) 1035 dojscia)
    ;(format t "Szanse nutsa co najmniej streetowego: ~A do ~A = <b>~A</b>"
     ;         (- 1035 ilosc-nuts) ilosc-nuts
      ;        (/ (- 1035.0 ilosc-nuts) ilosc-nuts))
    ilosc-nuts)  )

(defun omaha-turn(reka stol)
   (let ( (reszta  (wyjmij (talia) reka stol)) 
          (dojscia (make-array 9 :initial-element 0))
          ocena ocena2 )
    (setq ocena (omaha-ocena reka stol))
    (format t "<center><table border=1><tr><td><b>FLOP</b><td>~A<td>~A<td><b>~A</b></table></center>~&"           (liczby->opis reka) (liczby->opis stol) (opisz-ocene ocena))
    (dolist (karta reszta)
       (setq ocena2 (omaha-ocena reka (cons karta stol)))
       (when (= 1 (lepsza ocena2 ocena))
             (incf (aref dojscia (car ocena2)))))
    (opisz-dojscia (car ocena) 45 dojscia)
    dojscia)  )

(defun omaha-river(reka stol)
   (let (  ocena )
    (setq ocena (omaha-ocena  reka stol))
    (format t "<center><table border=1><tr><td><b>RIVER</b><td>~A<td>~A<td><b>~A</b></table></center>~&"           (liczby->opis reka) (liczby->opis stol) (opisz-ocene ocena))
     ))

(defun wybrano-karte(karta)
 (let ((l1 (length *reka*)) (l2 (length *stol*)) )
   (cond
    ((<= l1 2) (progn (push karta *reka*) (java(princ (liczby->opis *reka*)))))
    ((= l1 3)  (progn (push karta *reka*)
                      (setq *reka* (sort *reka* #'<))
                      (java (omaha-pre-flop *reka*))))
    ((<= l2 1) (progn (push karta *stol*) (java (princ (liczby->opis *reka*))
                                                (princ (liczby->opis *stol*))) ) )   
    ((= l2 2)  (progn (push karta *stol*)(java (omaha-flop *reka* *stol*))) )
    ((= l2 3)  (progn (push karta *stol*)(java (omaha-turn *reka* *stol*))) )
    ((= l2 4)  (progn (push karta *stol*)(java (omaha-river *reka* *stol*))) )
    (t (reset))  )))
    
  
