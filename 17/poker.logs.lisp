;; ----------- PATTERN MATCHING -------------------------------------

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

(defun match-variable(var input bindings)
  (let ((b (assoc var bindings)))
    (cond
      ((not b) (extend-bindings var input bindings))
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


(defmacro with-pattern(pattern &rest body)
  `(let ((it (match-string ,pattern +line+)))
    (when (and it (plusp (kind +game+)))
      ,@body)))


(defun remove-return(line)
  (let ((dl (length line)))
    (if (and (plusp dl) (eq #\Return (char line (1- dl))))
	(subseq line 0 (1- dl))
	line)))

(defmacro get-money(var)
  `(read-from-string  (remove-return (string-left-trim '(#\$) (get-var ,var)))))


(defun is-omaha?(line)
  (search "[Omaha]" line :test #'string=))

(defun is-5draw?(line)
  (search "[5 Card Draw]" line :test #'string=))

(defun is-7draw?(line)
  (search "[7 Card Draw]" line :test #'string=))

(defun is-holdem?(line)
  (search "[Hold 'em]" line :test #'string=))

(defun is-pot-limit?(line)
  (search "Pot Limit - Cash Game" line :test #'string=))

(defun is-fixed-limit?(line)
  (search "Fixed Limit - Cash Game" line :test #'string=))

(defun is-no-limit?(line)
  (search "No Limit - Cash Game" line :test #'string=))


(defun analyze-description-line(line)
  (let ((p1 (is-omaha? line))
	(p2 (is-holdem? line))
	(p3 (is-5draw? line))
	(p4 (is-7draw? line)))
   (cond
     ((and p1 (is-pot-limit? line)) (cons 3 (subseq line 3 (1- p1))))
     ((and p2 (is-no-limit? line))  (cons 1 (subseq line 3 (1- p2))))
     ((and p2 (is-fixed-limit? line))  (cons 2 (subseq line 3 (1- p2))))
     (p2 (cons 0 "xxx")) ; np. holdem pot limit lub turnieje
     (p3 (cons 0 "xxx"))
     (p4 (cons 0 "xxx"))
     (t nil))))

