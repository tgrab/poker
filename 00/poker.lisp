(defun los(a b)
 (+ a (random (- b a))))
;---------------------------------------------------------------------
(setq kolory '(Pik Kier Karo Trefl))
(setq figury '(As K D W 10 9 8 7 6 5 4 3 2))

(defstruct (karta (:print-function (lambda (p s k ) 
		    (format s "<~A ~A>"(nth (karta-figura p) figury)
		                       (nth (karta-kolor p) kolory)) )))
   kolor
   figura)
   
;talia ma 52 karty
(defun talia()
 (let (temp) 
  (dotimes (i 4)
   (dotimes (j 13)
    (push (make-karta :kolor i :figura j) temp))) 
  (reverse temp)))
  
(defun tasuj(zestaw)
 (let ((ile (length zestaw)))
  (dotimes (i ile)
   (rotatef (nth i zestaw) 
            (nth (los i ile) zestaw)))))

(defmacro rozdaj(ile skad dokad)
  (let ((i (gensym)))
     `(dotimes (,i ,ile) (push (pop ,skad)  ,dokad))))	    

(defun wylosuj(ile)
  (let ((z (talia))  (reka nil))
     (dotimes (i 100) (tasuj z))
     (rozdaj ile z reka) reka ))

(defun figury(zestaw)
 (let ((l (list 0 0 0 0 0 0 0 0 0 0 0 0 0)))
   (dolist (el zestaw)  (incf (nth (karta-figura el) l))) l ))
   
(defun kolory(zestaw)
 (let ((l (list 0 0 0 0)))
   (dolist (el zestaw)  (incf (nth (karta-kolor el) l))) l )) 
   

 
(defun najwyzsza(zestaw &optional (szukane 0) (rodzaj #'>))
 (let ((f (figury zestaw)))
  (dotimes (i 13)  (if (funcall rodzaj (nth i f) szukane)  (return-from najwyzsza i) ))))  
  
(defun para(zestaw) (najwyzsza zestaw 2 #'= ))  

(defun dwie(zestaw)
 (let ((f (figury zestaw)))
  (dotimes (i 13)   
   (if (= 2 (nth i f))
    (dotimes (j (- 12 i))
      (if (= 2 (nth (+ 1 i j) f))(return-from dwie `(,i ,(+ 1 i j)))))))))
      
(defun trojka(zestaw) (najwyzsza zestaw 3 #'= )) 

(defun pomnoz5(lista) 
  (let ((il 1))
    (dotimes (i 5) 
     (setq il (* il (nth i lista))) ) il))
(defun street(zestaw)
  (let ( (f (figury zestaw)) )
     (dotimes (i 8)
          (if (> (pomnoz5 (nthcdr i f)) 0) (return i))))) 

(defun kolor(zestaw)
 (dotimes (i 4) 
   (if (>= (nth i (kolory zestaw)) 5) (return-from kolor (najwyzsza zestaw)))))

(defun fulik(reka)
 (let ((i (trojka reka)) (j (para reka)))
  (if (and i j) `(,i ,j))))
	  
(defun kareta(zestaw) (najwyzsza zestaw 4 #'=)) 

;wybieramy z zestawu karty okreslonego koloru i sprawdzamy czy jest tam street
(defun wybierz(zestaw kolor)
 (cond ((null zestaw) nil)
       ((= kolor (karta-kolor (car zestaw))) (cons (car zestaw) (wybierz (cdr zestaw) kolor)) )
       (t (wybierz (cdr zestaw ) kolor))))
(defun poker(zestaw)
 (dotimes (i 4) (let ((s (street (wybierz zestaw i)))) (if s (return-from poker s)))))	

(defun oceny(zestaw)
  (list (poker zestaw)(kareta zestaw)(fulik zestaw)(kolor zestaw)(street zestaw)(trojka zestaw)
        (dwie zestaw)(para zestaw)(najwyzsza zestaw)))

(defun lepsza(l1 l2)
 (labels ((l(l1 l2)
   (cond
     ((and (null l1)(null l2))  0)
     ((null l1) -1)
     ((null l2) 1 )
     ((and (numberp l1)(numberp l2))   
       (cond
        ((> l1 l2)  -1)
        ((< l1 l2)  1)
	(t 0)))
     ((> (car l1) (car l2)) -1 )  
     ((< (car l1)(car l2)) 1 )  
     (t (l (cdr l1)(cdr l2)))  )))
     
 (dotimes (i (length l1) 0) 
     (let ((w (l (nth i l1)(nth i l2))))
            (unless (= w 0) (return-from lepsza w) )))))

(defun policz(reka &optional (ile 1000))
  (let (talia stol r1 r2 (win 0) (lose 0))
     (setq talia (talia))
     (dotimes (i 100) (tasuj talia))
         (dotimes (i ile)
           (setq stol (subseq talia 0 3))
	   (rozdaj 6 talia r1)
           (setq r1 (append r stol))
           (setq r2 (subseq talia 0 8))
	   (if (> (lepsza(oceny r1)(oceny r2)) 0)  (incf win) (incf lose) )
	   (tasuj talia))))

;-----------------------------------------------------------------------------------------
(defun pokaz_figure(n) (if (null n) 'brak (nth n figury)))  
(defun pokaz_kolor(n) (if (null n) 'brak (nth n kolory)))

(defun pokaz_oceny (zestaw)
  (destructuring-bind (a b c d e f g h i) (oceny zestaw)
   (format t "~&Najwyzsza karta: ~A~&"  (pokaz_figure i) )
   (format t "Para: ~A~&"  (pokaz_figure h) )
   (format t "Dwie pary: ~A ~A~&"  (pokaz_figure (car g)) (pokaz_figure (cadr g)) )
   (format t "Trojka: ~A~&"  (pokaz_figure f) )   
   (format t "Street: ~A~&"  (pokaz_figure e) )
   (format t "Kolor: ~A ~&"  (pokaz_figure  d)  )
   (format t "Full: ~A ~A ~&"  (pokaz_figure (car c)) (pokaz_figure (cadr c)) )
   (format t "Kareta: ~A~&"  (pokaz_figure b) )
   (format t "Poker: ~A ~&"  (pokaz_figure a)  )))	
   
(defun sortuj_zestaw(zestaw)
 (sort zestaw #'(lambda (k1 k2) (if (< (karta-kolor k1)(karta-kolor k2)) 
                                       1 ))))

(defun pokaz(zestaw)
 (print zestaw) (print (figury zestaw))(print (kolory zestaw))(pokaz_oceny zestaw))
 
 
;---------------------------------------------------------------------------------


(defun policz (reka)
  (mapcar #'(lambda(x)(if (null x) 0 1)) (oceny reka)))
 
(defun test1(funkcja)
(loop
  (let ((z (talia))  (reka nil))
     (dotimes (i 100) (tasuj z))
     (dotimes (j 10) 
       (setq reka nil)
       (rozdaj 5 z reka)
       (if (funcall funkcja reka) (return-from test1 (pokaz reka)) )))))

(defun test2(ile)
 (let ((suma '(0 0 0 0 0 0 0 0 0)))
  (dotimes (i ile)
   (let ((z (talia))  (reka nil))
     (dotimes (i 100) (tasuj z))
     (dotimes (j 10) 
       (setq reka nil)
       (rozdaj 5 z reka)
       (setq suma  (mapcar #'+ suma (policz reka)) )))) 
 (destructuring-bind (a b c d e f g h i) suma
   (format t "~&Ilosc testow: ~A~&"  i)
   (format t "par: ~A~&"   h )
   (format t "dwojek: ~A~&" g )
   (format t "trojek: ~A~&"  f )   
   (format t "street-ow: ~A~&"  e )
   (format t "kolorow: ~A ~&"  d )
   (format t "fuli: ~A ~&" c )
   (format t "karet: ~A~&"  b )
   (format t "poker-ow: ~A~&" a ))))

(defun test3(&optional (ile 5))
 (let ((r1 (wylosuj ile))(r2 (wylosuj ile)))
  (pokaz r1)(pokaz r2)
  (print (lepsza (oceny r1)(oceny  r2)))))
  
(defun test4(ile)
  (let (talia r stol r1 r2 (win 0) (lose 0))
     (setq talia (talia))
     (dotimes (i 100) (tasuj talia))
     (rozdaj 2 talia r) (print r)
         (dotimes (i ile)
           (setq stol (subseq talia 0 3))
           (setq r1 (append r stol))
           (setq r2 (subseq talia 0 5))
           ;(pokaz r1)(pokaz r2)(print (lepsza (oceny r1)(oceny  r2)))
	   (if (> (lepsza(oceny r1)(oceny r2)) 0)  (incf win) (incf lose) )
	   (tasuj talia))
        (format t "Ilosc testow:~A Zwyciestwa:~A Porazki:~A~&" ile win lose ))'koniec)