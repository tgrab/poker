;(declaim (optimize (speed 3) (safety 0)))

(defpackage :poker.logs
  (:use :cl :anaphor :poker.defs) )

(in-package :poker.logs)

(defun tokens (str &optional (delimiters (list #\Space))  (start 0))
  (let ((p1 (position-if #'(lambda (c) (not (find c delimiters)))
			 str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda(c) (find c delimiters))
			       str :start  (1+ p1))))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str delimiters (1+ p2))
		    nil)))
	nil)))

;(defmacro tokens->string(tokens)
;  `(concatenate 'string ,@tokens))

(defun variable-p(var)
  (and (symbolp var)
       (equal (char (symbol-name var) 0) #\?)))

(defun extend-binding(var val binding)
  (if (equal '(t . t) (car(last binding)))
      (cons (cons var val) nil)
      (cons (cons var val) binding)))

(defun match-variable(var input bindings)
  (let ((b (assoc var bindings)))
    (cond
      ((not b) (extend-binding var input bindings))
      ((string= input (cdr b)) bindings)
      (t nil))))

;(pat-match '("la" ?x "la") (tokens "la la la"))
(defun pat-match(pattern input &optional (bindings '((t . t))) )
  ;(format t "Wywolanie: ~A , ~A z podst.~A~%" pattern input bindings)
  (cond
    ((null bindings) nil)
    ((and (null pattern) input) nil)

    ((variable-p  pattern)
     (if (null input)
	 (cons (cons pattern "") bindings)
	 (match-variable  pattern input bindings) ))
    ((and (stringp pattern) (string= pattern input)) bindings)
    ((and (symbolp pattern) (search (symbol-name pattern) (string-upcase input)))
     bindings  )


    ((and (consp pattern) (listp input))

     (if (and (variable-p (car pattern))
	      (equal (char (symbol-name (car pattern)) 1) #\?))
	 (cons (cons '?? (if input input "")) bindings)
	 (pat-match (cdr pattern) (cdr input)
		    (pat-match (car pattern) (car input) bindings))) )  
    (t nil)    ))


(defun match-string(pattern input)
  (if (stringp input)
      (pat-match pattern (tokens input))
      (pat-match pattern input)))

(defmacro get-var(var)
  `(cdr (assoc ',var it)))

(defmacro assoc->let(asc &rest body)
  `(let (  ,@(mapcar #'(lambda (p)
			 (if (consp (cdr p))
			     (list (car p) (list 'quote (cdr p)))
			     (list (car p) (cdr p))))
		     asc) )  
    ,@body))

;TODO wewnatrz tego makra zmienne maja byc zdef.:
; it=((?x . 4) (?y . 5)) => (let ((x 4)(y 5)) ...)
(defmacro with-pattern(pattern &rest body)
  `(let ((it (match-string ,pattern line)))
    (when it ,@body)))
;TODO   (assoc->let it ,@body))))

(defun process-log(file-name)
  (with-open-file (file file-name)
  (do ((line (read-line file nil 'end) 
	     (read-line file nil 'end)))
      ((eq line 'end))

    (with-pattern '(** Game ID ?id starting - ?date ?hour) 
     (format t "~A ~A ~A~%" (get-var ?hour) (get-var ?date) (get-var ?id)))

;    (format t "~A~%" line)
  )))