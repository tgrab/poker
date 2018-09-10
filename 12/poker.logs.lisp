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


(defmacro with-pattern(pattern &rest body)
  `(let ((it (match-string ,pattern line)))
    (when it ,@body)))


(defun remove-return(line)
  (let ((dl (length line)))
    (if (eq #\Return (char line (1- dl)))
	(subseq line 0 (1- dl))
	line)))

(defmacro get-money(var)
  `(read-from-string  (string-left-trim '(#\$) (get-var ,var))))

(defun check-description(l game)
  (let ((p1 (or (search "[Omaha]" l :test #'string=)
		(search "[Hold 'em]" l :test #'string=))))
    (when p1
      (let ((tname (subseq l 3 (1- p1)))
	    (reszta (tokens (subseq l p1))))
	(setf (table-name game) tname)
	(if (string= (first reszta) "[Omaha]")
	    ; 0 - Omaha Pot Limit
	    ; 1 - Hold'em No Limit
	    (setf (game-kind game) 0)
	    (setf (game-kind game) 1))
      game))))

(defun process-log(file-name)
  (let ( (games-list nil) (game nil) )

    (with-open-file (file file-name)
      (do ((line (read-line file nil 'end) 
		 (read-line file nil 'end)))
	  ((eq line 'end))

	(setq line (remove-return line))
	;to musi przeslac istniejaca gre do logow i utworzyc nowy obiekt.
	(with-pattern '("**" "Game" "ID" ?id "starting" "-" ?date ?time) 
	  (when game
	    ;a moze stol byl 6-cio osobowy!
	    (let ((plrs (players game)))
	      (when (and (null (nth 6 plrs)) (null (nth 7 plrs))(null (nth 8 plrs))(null (nth 9 plrs)) )
		(setf (table-size game) 6)
		(setf (players game) (subseq plrs 0 6 ))))
	    (push game games-list)) ;old is saved TODO: update players list
	  (setq game (make-instance 'game-log 
				    :players (make-list 10) ;assume 10 players table
				    :id (get-var ?id) 
				    :date (get-var ?date) 
				    :time (get-var ?time)))   ) ;new game is created
 
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

	) ;end of do
	  (when game 
	    ;a moze stol byl 6-cio osobowy!
 (let ((plrs (players game)))
	      (when (and (null (nth 6 plrs)) (null (nth 7 plrs))(null (nth 8 plrs))(null (nth 9 plrs)) )
		(setf (table-size game) 6)
		(setf (players game) (subseq plrs 0 6 ))))
	    (push game games-list))) ;end of file, game is saved
    games-list))

;; -------- Mechanics --------------------

(defvar *logger* (make-instance 'agent))

(defun render-action(a &optional (show-fold t))
  (when (null a) (if show-fold (return-from render-action "Folded")
		     (return-from render-action "")))
  (if (consp a)
      (cond
	((eq (car a) 'c) (if (zerop (cdr a)) "Checked" (format nil "Called ~A" (cdr a))  ))
	((eq (car a) 'b)  (format nil "Bet ~A" (cdr a))  ))
      (cond
	((eq a 'c) "checked")
	((eq a 'sb) "small blind")
	(t "big blind"))))

(defun render-player(agent seat)
  (let ((p (nth seat (players (game agent)))))
    (if p
	(with-output-to-string(str)
	  (if (null (nth seat (actions-list agent)))
	      (princ "<td style='background-color: grey'><ul class='gracz'>" str)
	      (princ "<td><ul class='gracz'>" str))

	  (let ((a (nth seat (actions-list agent))))
	    (when  (not (eq a t))
	      (if (= seat (last-active-player agent))
	      (format str "<li style='background-color: red; font-weight: bold;' >~A" (render-action a))
	      (format str "<li style='color: red;' >~A" (render-action a nil)))))
	  (format str "<li style='font-weight: bold'>~A " (name p))
	  (format str "<li>~6,2F $" (money p))
	  (if (= seat (button (game agent))) 
	      (format str "<li style='color: orange'>(Dealer)"))

	  (awhen (cards p)  
	    (format str "<li>~A" (karty->html it ))
	    (when (table-cards agent) (format str "<li>~A" (render-rank (game-rank (game agent) it (table-cards agent) )))  ))
	  (princ "</ul>" str))
	"<td style='background-color: green'>" )))

(defun render-table(agent)
  (with-output-to-string(str)

  (if (= 6 (table-size agent))
      (progn
	(format str "<table class='stol'>")
	(format str "<tr>~A~A~A" (render-player agent 5)(render-player agent 4)
		(render-player agent 3))
	(format str "<tr>~A~A~A" (render-player agent 0)(render-player agent 1)
		(render-player agent 2))
	(format str "</table>")    )  
      (progn
	(format str "<table class='stol'>")
	(format str "<tr>~A~A~A~A~A" (render-player agent 9)(render-player agent 8)
		(render-player agent 7)(render-player agent 6)(render-player agent 5))
	(format str "<tr>~A~A~A~A~A" (render-player agent 0)(render-player agent 1)
		(render-player agent 2)(render-player agent 3)(render-player agent 4))
	(format str "</table>")      ))))

(defun render-stats(agent)
  (with-output-to-string(str)
    (format str "<ul>")
    (format str "<li>Pot: ~A" (pot agent))
    (aif (table-cards (game agent))
       (format str "<li>Table: ~A"(karty->html it))
       "")
    (format str "</ul>")))

(defun render-state(agent)
  (with-output-to-string(str)
	(format str "<table style='border: 2px solid black'>")
	(format str "<tr><td>~A" (render-table agent))
	(format str "<tr><td style='padding-left: 50px;'>~A" (render-stats agent) )
	(format str "</table>")      ))

(defmacro get-active-player-name(agent)
  `(name (nth (active-player ,agent) (players (game ,agent)))))

;log-level: 0 - tylko gramy bez logowania
;           1 - normalne do wyswietlania na stronie
(defun game-analysis(game &optional (agent *logger*) (log-level 1))
  (let ((page-list nil)
	(c (reverse (table-cards game)))
	(h (reverse (history game))))

    (percept-new-game-log agent game)
    (dolist (a h)
      (cond
	((eq a 'F) 
	 (percept-fold agent)
	 (when (plusp log-level) (push (render-state agent) page-list)))

	((eq a 'C) 
	 (percept-check agent)
	 (when (plusp log-level) (push (render-state agent) page-list)))


	((consp a)
	 (percept-bet agent (cdr a))
	 (when (plusp log-level)(push (render-state agent) page-list)))

	((eq a 'FLOP)	
	 (when (plusp log-level)
	   (push (format  nil "<center>Dealing flop: ~A</center>" (karty->html (subseq c 0 3))) 
		 page-list))
	 (percept-flop agent (nth 0 c) (nth 1 c) (nth 2 c) ))

	((eq a 'TURN)        
	 (when (plusp log-level) 
	   (push (format nil "<center>Dealing turn: ~A</center>" (karty->html (list (nth 3 c)))) 
		 page-list))
	 (percept-turn agent (nth 3 c)  ))

	((eq a 'RIVER)      
	 (when (plusp log-level)
	   (push (format nil "<center>Dealing river: ~A</center>" (karty->html (list (nth 4 c)))) 
		 page-list))
	 (percept-river agent (nth 4 c) )) ) )  
    (when (plusp log-level)
      (let ((w (game-winner agent)))
	(when (= 1 (length w))
	  (push 
	   (format nil "Winner: ~A" (name (nth   (first w)  (players (game agent)) )))
	    page-list))))
    (nreverse page-list)      ))


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
