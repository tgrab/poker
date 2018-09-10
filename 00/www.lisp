(defvar *talia* (talia))
(defvar *stol* nil)
(defvar *ilosc-graczy* 0)
(defvar *gracz* 0)
(defvar *rece* nil)
;(setq *talia* (talia))


(defun obraz-karty(karta)
 (labels ((kolory (x) 
            (case x
	      (0 "s")
	      (1 "h")
	      (2 "d")
	      (3 "c")))  
          (figury (x)
	    (cond
	      ((> x 4) (- 14 x) )
	      ((= x 4) "t")
	      ((= x 3) "j" )
	      ((= x 2) "q" )
	      ((= x 1) "k" )
	      ((= x 0) "a" ))))
  (format nil "<img src=/decks/~A~A.gif width=50 height=70 >" 
         (figury (karta-figura karta))
	 (kolory (karta-kolor karta)))))

(defun pokaz-oceny (zestaw &optional tytul winner)
 (destructuring-bind (a b c d e f g h i) (oceny zestaw)
  (with-output-to-string (s)
   (if winner (format s "<table border=1 style='background-color:red'>") (format s "<table border=1>"))
   (when tytul (format s "<tr><td colspan=2 align=center>~A" tytul))
   (format s "<tr><td>Najwy¿sza karta<td> ~A"  (pokaz_figure i)) 
   (when  h (format s "<tr><td>Para<td> ~A"  (pokaz_figure h) ))
   (when g (format s "<tr><td>Dwie pary <td> ~A ~A"  (pokaz_figure (car g)) (pokaz_figure (cadr g)) ))
   (when f (format s "<tr><td>Trójka <td>~A"  (pokaz_figure f) ))   
   (when e (format s "<tr><td>Street <td>~A"  (pokaz_figure e) ))
   (when d (format s "<tr><td>Kolor <td>~A"  (pokaz_figure  d)  ))
   (when c (format s "<tr><td>Full <td>~A ~A"  (pokaz_figure (car c)) (pokaz_figure (cadr c)) ))
   (when b (format s "<tr><td>Kareta <td>~A"  (pokaz_figure b) ))
   (when a (format s "<tr><td>Poker <td>~A"  (pokaz_figure a)) )
   (format s "</table>")   )))	

(defun winner()
 (let ((winner 0))
   (dotimes (i *ilosc-graczy* winner) 
      (when (= 1 (lepsza (oceny (aref *rece* i))(oceny (aref *rece* winner))))
               (setq winner i))  )))

(defun ocena-texas(reka stol talia ilosc-graczy)
 "reka,stol,talia to listy kart. reka to karty gracza0"

 (let ((wyniki (make-array ilosc-graczy :initial-element 0)))
  ;test
 (dotimes (j 100)
  (let*  ((winner 0) (talia2 (copy-list talia)) (stol2 (copy-list stol)) 
         (rece (make-array ilosc-graczy :initial-element nil)))
   (tasuj talia2)
   (rozdaj (- 5 (length stol)) talia2 stol2)
   (setf (aref rece 0) (append reka stol2))
   (dotimes (i (1- ilosc-graczy)) 
      (rozdaj 2 talia2 (aref rece (1+ i)) )
      (setf (aref rece (1+ i)) (nconc (aref rece (1+ i)) stol2) ))
  ;winner
   (dotimes (i ilosc-graczy)
      ;(print (pokaz-oceny (aref rece i)))
      (when (= 1 (lepsza (oceny (aref rece i))(oceny (aref rece winner))))
              (setq winner i))     )
     (incf (aref wyniki winner))  )) 
   wyniki))

;-------------------------------------------------------------------------------

(defmacro karty-main()
  `(html
     ((:a :href "karty.main") "[MAIN]") ((:a :href "karty.reset") "[RESET]")
     ((:a :href "karty.tasuj") "[TASUJ]")  ((:a :href "karty.opcje") "[OPCJE]")

     :hr
    ;(:h2 "Talia kart:")
    (dolist (k *talia* ) (html  
                           ((:a :href 
                              (format nil "/karty.rozdaj?figura=~A&kolor=~A" 
			                    (karta-figura k)(karta-kolor k)))
                               (:princ (obraz-karty k)))) )
     :hr :newline
     "Wybór gracza:" ((:a :href "karty.opcje?gracz=0") "[STÓ£]")
     (dotimes (i *ilosc-graczy*)  
          (html (:princ (format nil "[<a href=karty.opcje?gracz=~A>GRACZ ~A</a>] " (1+ i) (1+ i)))))
     :hr
    ((:table :cellpadding "5")
    ;Karty na stole:
    :tr ((:td :if* (= 0 *gracz*) :style "background-color:yellow")
          ((:table :style "border: solid")
            :tr :td    (:princ (pokaz-oceny *stol* "STÓ£"))
                :td (dolist (k *stol* )  (html  (:princ (obraz-karty k)))))) 
    ;Karty u graczy:
    (let ((w (winner)))
     (dotimes (i *ilosc-graczy*)
      (html
       (when (or (= i 3)(= i 7)) (html :tr))
       ((:td :if* (= (1+ i) *gracz*) :style "background-color:yellow")
         ((:table :style "border: solid")
           :tr :td    (:princ (pokaz-oceny (aref *rece* i) (format nil "GRACZ ~A" (1+ i)) (= i w) ))
               :td    (dolist (k (aref *rece* i) )  (html  (:princ (obraz-karty k))))       )))   )))))

(defun karty-reset()
  (setq *talia* (talia) *stol* nil *gracz* 0 *rece* (make-array *ilosc-graczy* :initial-element nil)))

;------------------------------------------------------------------------------
;(publish-directory :prefix "/decks/" :destination "/raid/code/lisp/karty/decks/")
(publish :path "/karty.main" :function (wyslij ((karty-main))))
(publish :path "/karty.reset" :function 
     (wyslij
      ( (karty-reset)
        (karty-main))))
(publish :path "/karty.tasuj" :function 
     (wyslij
      ( (tasuj *talia*)
        (karty-main))))	


(publish :path "/karty.opcje" :function 
     (wyslij
      ((let ((gracz (parameter "gracz"))
            (ilosc (parameter "ilosc")))  
         (cond
            (gracz  (progn (setq *gracz* (parse-integer gracz)) (karty-main)))
            (ilosc  (progn  (setq *ilosc-graczy* (parse-integer ilosc))
                            (karty-reset) (karty-main  )))
            (t (html 
                ((:form :action "karty.opcje")
                  "Podaj ilo¶æ graczy "
                  ((:input :name "ilosc")) :br
                  ((:input :type "submit" :value "Wy¶lij")))))	 )))))


   
(publish :path "/karty.rozdaj" :function 
     (wyslij
      ( (let ((figura (parse-integer(parameter "figura")))
              (kolor  (parse-integer(parameter "kolor"))))
           (setq *talia* (delete-if  (lambda (k) 
	                  (and (= figura (karta-figura k))(= kolor (karta-kolor k)))) 
               	             *talia*))
	  (if (= 0 *gracz* ) (push (make-karta :kolor kolor :figura figura) *stol*)
                             (push (make-karta :kolor kolor :figura figura) (aref *rece* (1- *gracz*))) ))
        (karty-main))))	   
