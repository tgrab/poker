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

#|
(defmacro with-pattern (pattern line &body body)
  (format t "Line:     ~A~&" line)
  (let ((m (match-string pattern line)))
    (if m
	`(let   ,(a->list m)  ,@body )
	nil)    ))
|#

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

#|
(defun process-log-old(file-name)
  (let ( (games-list nil) (game nil) )

    (with-open-file (file file-name)
      (do ((line (read-line file nil 'end) 
		 (read-line file nil 'end)))
	  ((eq line 'end))

	(setq line (remove-return line))
	;to musi przeslac istniejaca gre do logow i utworzyc nowy obiekt.
	(with-pattern '(** Game ID ?id starting - ?date ?time) 
	  (when game (push game games-list)) ;old is saved
	  (setq game (make-instance 'game 
				    :players (make-list 10) ;assume 10 players table
				    :id (get-var ?id) 
				    :date (get-var ?date) 
				    :time (get-var ?time)))  )    ;new game is created
	  ;(format t "~%~A ~A ~A~%" (get-var ?time) (get-var ?date) (get-var ?id)))

	(with-pattern '(- ?player sitting in seat ?nr with ?amount ?info)
	  (when (string= (get-var ?info) "[Dealer]")  (setf (button game) 
							    (1- (parse-integer (get-var ?nr))) ) )
	  (unless (string= (get-var ?info) "[Sitting out]")
	    (setf (nth (1- (parse-integer (get-var ?nr))) (players game)) 
		  (make-instance 'player :name (get-var ?player))))  )

	(with-pattern '("**" "Dealing" "card" "to" grabola ?r1 of ?c1 ?r2 of ?c2  )
	  (setf (cards (get-player "grabola" game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (get-var ?c2)))) )


	(with-pattern '("**" "Dealing" "the" "flop:" ?r1 of ?c1 ?r2 of ?c2 ?r3 of ?c3 )
	  (push 'flop (history game))
	  (setf (table-cards game)
		(list
		 (texts->card 
		  (get-var ?r1) 
		  (string-right-trim '(#\,) (get-var ?c1))) 
		 (texts->card 
		  (get-var ?r2) 
		  (string-right-trim '(#\,) (get-var ?c2))) 
		 (texts->card 
		  (get-var ?r3) 
		  (get-var ?c3)) 	 )))

	(with-pattern '("**" "Dealing" "the" "turn:" ?r1 of ?c1 )
	  (push 'turn (history game))
	  (push 
		 (texts->card 
		  (get-var ?r1) 
		  (get-var ?c1))
		 (table-cards game)  ))

	(with-pattern '("**" "Dealing" "the" "river:" ?r1 of ?c1 )
	  (push 'river (history game))
	  (push 
		 (texts->card 
		  (get-var ?r1) 
		  (get-var ?c1))
		 (table-cards game)  ))


	(with-pattern '(?player "folded")
	  (push 'f (history game)))

	(with-pattern '(?player "called" "-" ?amount)
	  (push (cons 'c 
		      (string-left-trim '(#\$) (get-var ?amount)))
		(history game)))

	(with-pattern '(?player "checked")
	  (push 'c (history game)))

	(with-pattern '(?player "bet" "-" ?amount)
	  (push (cons 'b 
		      (string-left-trim '(#\$) (get-var ?amount)))
		(history game)))

	(with-pattern '(?player "raised" "-" ?amount)
	  (push (cons 'b 
		      (string-left-trim '(#\$) (get-var ?amount)))
		(history game)))

	(with-pattern '(?player "went" "all-in" "-" ?amount)
	  (push (cons 'b 
		      (string-left-trim '(#\$) (get-var ?amount)))
		(history game)))

	(with-pattern '(?player "shows:" ?r1 "of" ?c1 ?r2 "of" ?c2)
	  (setf (cards (get-player (get-var ?player) game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (get-var ?c2)))) )


	) ;end of do
	  (when game (push game games-list))) ;end of file, game is saved
    games-list))

|#

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