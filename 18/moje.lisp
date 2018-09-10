(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :dekoduj-czas :permutacje :aif :awhen :it :uruchom-program :lispdoc))

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

(defun uruchom-program(nazwa &optional arg)
  #+clisp
  (if arg
    (ext:run-program nazwa :arguments (list arg))
    (ext:run-program nazwa)))


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


(defun lispdoc(source)
  (with-open-file (fin (format nil "~A.lisp" source))
    (with-open-file (fout (format nil "~A.html" source) 
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create)
      (format fout "<h2>File: ~A.lisp</h2>" source)
      (format fout "<table border='1'>")
      (do ((sexpr (read fin nil 'end) (read fin nil 'end)))
	   ((eq sexpr 'end))
	   (if (eq 'defun (car sexpr))
	       (format fout "<tr><td>~A<td>~A<td>~A"
		       (second sexpr) 
		       (third sexpr)
		       (if (stringp (fourth sexpr))
			   (fourth sexpr)
			   ""))) )
      (format fout "</table'>"))))
    