(defparameter *wielkosc-stolu* 8)
(defvar *ja*) ;moje miejsce na stole
(defvar *gracze* nil) ;lista graczy
(defvar *karty*) ; lista nie rozdanych jeszcze kart

(defun reset()
 (setq *karty* (talia))
 (mapc #'(lambda (g) 
           (setf (gracz-gra? g) t
                 (gracz-karty g) nil ))
       *gracze*))

(defun sit-down(miejsce alias)
  (push (make-gracz :miejsce miejsce :alias alias) *gracze*))

(defstruct gracz
  miejsce ;numer pozycji przy stole(indeksowane od 1)
  alias
  karty
  gra?)

(defun get-gracz(alias)
 (find alias *gracze* :key #'gracz-alias))


(defun rozdaj-karte(alias karta)
  (push karta (gracz-karty (get-gracz alias)))
  (setq *karty* (remove karta *karty*)))

(defun odds(alias &optional (ilosc-rozdan 100))
 "Zwraca wynik pojedynku dla gracza <gracz>. Wynik to trojka liczb #(zw,remisy,por)"
 (flet ( (rozdaj (l1 l2) (let (start (end 0))
                                     (mapcan #'(lambda (g)
						 ;(princ g)
						 (setq start end)
						 (incf end (- 7 (length g)))
                                                 (list (append g (subseq l2 start end )) ) )					  
                                             l1) )) )
 (let*  ( (karty *karty*) numer-gracza
          (karty-graczy (mapcan (let ((indx -1)) 
				     #'(lambda (g) (when (gracz-gra? g)
						(incf indx)
                                                (when (eq alias (gracz-alias g)) (setq numer-gracza indx)) 
						(list (gracz-karty g))))) 
                                *gracze*)) 
	  (wynik (make-array (length karty-graczy)))  )
  (dotimes (i ilosc-rozdan)
    (tasuj karty)
    (dolist (z (zwyciezca (mapcar #'ocena (rozdaj karty-graczy karty))))
        (incf (aref wynik z)) ) )
 (/ (-  (apply #'+ (coerce wynik 'list)) 
        (aref wynik numer-gracza)) 
    (aref wynik numer-gracza) 1.0) )))

