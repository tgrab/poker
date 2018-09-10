;(declaim (optimize (speed 3) (safety 0)))

(in-package :poker.logs)


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


(defmacro get-var(var)
  `(cdr (assoc ',var it)))

(defun valid-game?(game)
  (or (= 0 (game-kind game))
      (= 1 (game-kind game))
      (= 2 (game-kind game))))

(defmacro with-pattern(pattern &rest body)
  `(let ((it (match-string ,pattern line)))
    (when (and it  (valid-game? game))
      ,@body)))


(defun remove-return(line)
  (let ((dl (length line)))
    (if (eq #\Return (char line (1- dl)))
	(subseq line 0 (1- dl))
	line)))

(defmacro get-money(var)
  `(read-from-string  (string-left-trim '(#\$) (get-var ,var))))


(defun is-omaha?(line)
  (search "[Omaha]" line :test #'string=))

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
	(p2 (is-holdem? line)))
   (cond
     ((and p1 (is-pot-limit? line)) (cons 0 (subseq line 3 (1- p1))))
     ((and p2 (is-no-limit? line))  (cons 1 (subseq line 3 (1- p2))))
     ((and p2 (is-fixed-limit? line))  (cons 2 (subseq line 3 (1- p2))))
     (t nil))))

(defun check-description(l game)
  (awhen (analyze-description-line l)
    (setf (game-kind game) (car it))
    (setf (table-name game) (cdr it))))


(defun analyze-log(file-name)
  (let ( (games-list nil) (game nil) )

    (with-open-file (file file-name)
      (do ((line (read-line file nil 'end) 
		 (read-line file nil 'end)))
	  ((eq line 'end))

	(setq line (remove-return line))
	;to musi przeslac istniejaca gre do logow i utworzyc nowy obiekt.
	(let ((it (match-string '("**" "Game" "ID" ?id "starting" "-" ?date ?time) line)))
	  (when it
	    (when game
					;a moze stol byl 6-cio osobowy!
	      (let ((plrs (players game)))
		(when (and (null (nth 6 plrs)) (null (nth 7 plrs))(null (nth 8 plrs))(null (nth 9 plrs)) )
		  (setf (table-size game) 6)
		  (setf (players game) (subseq plrs 0 6 )))
					;lub dwu-osobowy
		(when (and (null (nth 2 plrs)) (null (nth 3 plrs))(null (nth 4 plrs))(null (nth 5 plrs)) )
		  (setf (table-size game) 2)
		  (setf (players game) (subseq plrs 0 2 ))))
	      (push game games-list)) ;old is saved TODO: update players list
	    (setq game (make-instance 'game 
				      :players (make-list 10) ;assume 10 players table
				      :id (get-var ?id)
				      :balance (make-list 10 :initial-element 0)
				      :time  (format nil "~A ~A" (get-var ?date) (get-var ?time))))   )) ;new game is created
 
	(check-description line game) ;"** Dolly in chains [Omaha] (0.5|1) Pot-Limit ..."
	(with-pattern '(?player "posted" "the" "small" "blind" "-" ?amount)
	  (setf (small-blind game) (get-money ?amount)) )

	(with-pattern '(?player "posted" "the" "big" "blind" "-" ?amount)
	  (setf (big-blind game) (get-money ?amount)) )

	(with-pattern '(- ?player sitting in seat ?nr with ?amount ?info)
	  (unless (string= (get-var ?info) "[Sitting out]")
	    (let* ((seat (1- (parse-integer (get-var ?nr))))
		   (plr  (make-instance 'player 
				       :name (get-var ?player) 
				       :money (get-money ?amount))))	  
	       (when (string= (get-var ?info) "[Dealer]")  
		 (setf (button game) seat))
	       (setf (nth seat (players game)) plr) 	    )))

	(with-pattern '("**" "Dealing" "card" "to" "grabola:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 ?r4 "of" ?c4 )
	  (setf (cards (get-player "grabola" game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (string-right-trim '(#\,) (get-var ?c2)))
		      (texts->card 
		       (get-var ?r3) 
		       (string-right-trim '(#\,) (get-var ?c3)))
		      (texts->card 
		       (get-var ?r4) 
		       (get-var ?c4)))) )

	(with-pattern '("**" "Dealing" "card" "to" "grabola:" ?r1 "of" ?c1 ?r2 "of" ?c2  )
	  (setf (cards (get-player "grabola" game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (get-var ?c2)))) )


	(with-pattern '("**" "Dealing" "the" "flop:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 )
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

	(with-pattern '("**" "Dealing" "the" "turn:" ?r1 "of" ?c1 )
	  (push 'turn (history game))
	  (push 
		 (texts->card 
		  (get-var ?r1) 
		  (get-var ?c1))
		 (table-cards game)  ))

	(with-pattern '("**" "Dealing" "the" "river:" ?r1 "of" ?c1 )
	  (push 'river (history game))
	  (push 
		 (texts->card 
		  (get-var ?r1) 
		  (get-var ?c1))
		 (table-cards game)  ))


	(with-pattern '(?player "folded")
	  (push 'f (history game)))

	(with-pattern '(?player "called" "-" ?amount)
	  (push 'c (history game)))

	(with-pattern '(?player "checked")
	  (push 'c (history game)))

	(with-pattern '(?player "bet" "-" ?amount)
	  (push (cons 'b 
		      (get-money ?amount))
		(history game)))

	(with-pattern '(?player "raised" "-" ?amount)
	  (push (cons 'b 
		      (get-money ?amount))
		(history game)))

	(with-pattern '(?player "went" "all-in" "-" ?amount)
	  (push (cons 'b 
		      (get-money ?amount))
		(history game)))

	(with-pattern '(?player "shows:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 ?r4 "of" ?c4)
	  (setf (cards (get-player (get-var ?player) game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (string-right-trim '(#\,) (get-var ?c2)))
		      (texts->card 
		       (get-var ?r3) 
		       (string-right-trim '(#\,) (get-var ?c3)))
		      (texts->card 
		       (get-var ?r4) 
		       (get-var ?c4)))))

	(with-pattern '(?player "shows:" ?r1 "of" ?c1 ?r2 "of" ?c2)
	  (setf (cards (get-player (get-var ?player) game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (get-var ?c2)))) )

	(with-pattern '(?player "mucks:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 ?r4 "of" ?c4)
	  (setf (cards (get-player (get-var ?player) game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (string-right-trim '(#\,) (get-var ?c2)))
		      (texts->card 
		       (get-var ?r3) 
		       (string-right-trim '(#\,) (get-var ?c3)))
		      (texts->card 
		       (get-var ?r4) 
		       (get-var ?c4)))))

	(with-pattern '(?player "mucks:" ?r1 "of" ?c1 ?r2 "of" ?c2)
	  (setf (cards (get-player (get-var ?player) game))
		(list (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (get-var ?c2)))) )

	(with-pattern'(?player "wins" ?amount "from" ??)
	  (incf (nth (get-player-pos (get-var ?player) game) (balance game)) (get-money ?amount))  ) 

	) ;end of do
	  (when game 
	    ;a moze stol byl 6-cio osobowy!
	    (let ((plrs (players game)))
	      (when (and (null (nth 6 plrs)) (null (nth 7 plrs))(null (nth 8 plrs))(null (nth 9 plrs)) )
		(setf (table-size game) 6)
		(setf (players game) (subseq plrs 0 6 )))
    		;lub dwu-osobowy
	      (when (and (null (nth 2 plrs)) (null (nth 3 plrs))(null (nth 4 plrs))(null (nth 5 plrs)) )
		(setf (table-size game) 2)
		(setf (players game) (subseq plrs 0 2 ))))
	    (push game games-list))) ;end of file, game is saved
    games-list))

;; -------- Mechanics --------------------
(defun money-analysis(game-log agent)
  (let ((pocz (mapcar #'(lambda(p) (if p (money p) 0)) (players game-log)))
	(wnrs nil)
	(how-much 0)
	(konc nil))
    (game-analysis game-log agent 0)
    (setq wnrs (game-winner agent))
    (setq how-much (/ (pot agent) (length wnrs)))
    (dolist (w wnrs)
      (incf (money (nth w (players (game agent)))) how-much))
    (setq konc (mapcar #'(lambda(p) (if p (money p) 0)) (players (game agent))))
    ;(princ (list wnrs how-much pocz konc))
    (mapcar #'- konc pocz)  ))
