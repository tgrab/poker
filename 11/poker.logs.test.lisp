;(declaim (optimize (speed 3) (safety 0)))

(defpackage :poker.logs
  (:use :cl :anaphor :poker.defs) )

(in-package :poker.logs)

(defvar line "")

;; ---------- PATTERN MATCHING ----------------------------------

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
 ; (format t "match-string ~A ~A~&" pattern input)
  (if (stringp input)
      (pat-match pattern (tokens input))
      (pat-match pattern input)))

(defun a->list(asc)
  (mapcar #'(lambda (p)
			 (if (consp (cdr p))
			     (list (car p) (list 'quote (cdr p)))
			     (list (car p) (cdr p))))
		     asc))


(defmacro match-line(pattern)
  `(pat-match ',pattern (tokens (symbol-value 'line))))

(defmacro with-line (pattern &body body)
  (let* ((m (pat-match pattern (tokens (symbol-value 'line))))
	 (l (a->list m)))
    (if m
	`(let   ,l  ,@body )
	nil)    ))

;(defun with-p(pattern l &rest body)
;  (with-pattern pattern l body))

;; -------- PRIMA Specific -------------------

(defun texts->card(rank color)
  (+
   (cond 
     ((string= "2" rank) 13)
     ((string= "3" rank) 12)
     ((string= "4" rank) 11)
     ((string= "5" rank) 10)
     ((string= "6" rank) 9)
     ((string= "7" rank) 8)
     ((string= "8" rank) 7)
     ((string= "9" rank) 6)
     ((string= "10" rank) 5)
     ((string= "Jack" rank) 4)
     ((string= "Queen" rank) 3)
     ((string= "King" rank) 2)
     ((string= "Ace" rank) 1)
     (t 0)    )

  (* 13
     (cond 
       ((string= "Clubs" color) 3)
       ((string= "Diamonds" color) 2)
       ((string= "Hearts" color) 1)
       ((string= "Spades" color) 0)
       (t 0)))     ))


; -------- Utilities --------------------

(defun remove-return(line)
  (let ((dl (length line)))
    (if (eq #\Return (char line (1- dl)))
	(subseq line 0 (1- dl))
	line)))


; ------- Logging --------------------------


(defun process-log(file-name)
  (let ( (games-list nil) (game nil) )

    (with-open-file (file file-name)
      (do ((line (read-line file nil 'end) 
		 (read-line file nil 'end)))
	  ((eq line 'end))

	(setq line (remove-return line))

       
	(with-line ("End" "of" "game" ?r1) 
	  (princ ?r1)
	  )






	) ;end of do
	  ;(when game (push game games-list))
	  ) ;end of file, game is saved
    games-list))