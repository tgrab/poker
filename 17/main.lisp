(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :create-system :dekoduj-czas :permutacje :aif :awhen :it))

(in-package :moje)

(defun permutacje (lista)
  (if (null lista)
      '(())
      (mapcan #'(lambda (e)
		  (mapcar #'(lambda (p) (cons e p))
			  (permutacje
			   (remove e lista :count 1 ))
		  )) 
        lista)))


(defun mappend(fn lista)
 (apply #'append (mapcar fn lista)))


(defun show-hashtable(table)
  (maphash
   #'(lambda (k v) (format t "~A => ~A~%" k v))
   table))

(defun dekoduj-czas(czas)
    (multiple-value-bind (s m h d mn y) (decode-universal-time czas)
      (format nil "~A:~A:~A at ~A/~A/~A" h m s d mn y )))
    
;; ---- MOJE MAKRA -----------------------------------

(defmacro mac( makro )
  `(pprint (macroexpand-1 ',makro)))


;; ---- MAKRA ANAFORYCZNE  ---------------------------
(defmacro aif (warunek then-form &optional else-form)
  `(let ((it ,warunek))
     (if it ,then-form ,else-form)))


(defmacro awhen(test &body exprs)
  `(let ((it ,test))
    (when it ,@exprs)))
    
    