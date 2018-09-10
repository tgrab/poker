;(load (compile-file "poker.db.lisp" :output-file "/dane/lisp/poker.db.x86f"))

(defstruct game
  (id 0)
  (kind 0)
  time 
  table-name 
  blind 
  table-cards 
  (players  nil)
  (money  (make-array 10 :initial-element 0)) ;initial amount of money
  (cards  (make-array 10 :initial-element nil))
  (history "")) ; history ma inna strukture - zawiera numer gracza i akcje!


(defvar +file+ nil)
(defvar +line+ nil)
(defvar +agent-action+ nil)
(defvar +game-log+ (make-game))
(defvar +games-list+ nil)

;;--------------------------------------------------------------
(defmacro with-prima-connection(&body body)
  `(let ((con (pg-connect "prima" "postgres" :port 5433))
	 (result nil))
    (unwind-protect
	 (setq result (progn ,@body))
      (when con (pg-disconnect con)))
    result))

(defmacro prima-query( query )
  `(with-prima-connection 
    (pg-result (pg-exec con ,query) :tuples)     ))

(let ((names (make-hash-table)))
  (defun get-player-name-table() names)
  (defun get-player-name( nr )
    (or (gethash nr names)
	(progn
	  (let ((val (caar (prima-query (format nil "SELECT name FROM players WHERE nr=~A" nr)))))
	    (setf (gethash nr names)  val)  	  )))))

(let ((names (make-hash-table :test #'equal)))
  (defun get-player-nr-table() names)
  (defun get-player-nr( name &optional (read-only nil))
    (or  (gethash name names)
	 (with-prima-connection
	   (aif (caar (pg-result (pg-exec con (format nil "SELECT nr FROM players WHERE name='~A'" name)) :tuples))
		(setf (gethash name names) it)		  
		(progn
		  (when read-only (return-from get-player-nr 0))
		  (pg-exec con (format nil "INSERT INTO players VALUES(DEFAULT,'~A')" name))
		  (setf (gethash name names)
			(caar (pg-result (pg-exec con (format nil "SELECT nr FROM players WHERE name='~A'" name)) :tuples)))))))))

(defun prepare-players()
  (let ((res ""))
    (dolist (p (sort (copy-list (game-players +game-log+)) #'< :key #'cdr) res)
      (setq res (format nil "~A(~A ~A ~A" res (cdr p) (get-player-nr (car p))
			(aref (game-money +game-log+) (cdr p))  )) 
      (aif (aref (game-cards +game-log+) (cdr p))
	   (setq res (format nil "~A (~A))" res it)) 
	   (setq res (format nil "~A)" res ))))))

(defun insert-game()
; (format t "~A " (game-id +game-log+))
; (force-output)
; (print +game-log+)
; (return-from insert-game)
  (when (plusp (game-kind +game-log+))
   (with-prima-connection
     (let ((sql (format nil "INSERT INTO games VALUES(~A,'~A',~A,'~A','~A',~A,'~A','~A')"
		    (game-id +game-log+)
		    (game-time +game-log+)
		    (game-kind +game-log+)
		    (game-table-name +game-log+)
		    (game-table-cards +game-log+)
		    (game-blind +game-log+)
		    (prepare-players)
		    (game-history +game-log+))))

       (if (caar (pg-result (pg-exec con (format nil "SELECT id FROM games WHERE id=~A" (game-id +game-log+))) :tuples))
	   (progn (format t "-")(force-output))
	   (progn
	     (pg-exec con sql)
	     (dolist (p (game-players +game-log+))
	       (pg-exec con (format nil "INSERT INTO player_log VALUES (~A,~A,~A,~A,'~A')" 
				    (get-player-nr (first p))
				    (game-id +game-log+)
				    (game-kind +game-log+)
				    (game-blind +game-log+)
				    (game-time +game-log+)))       ) )	   )     ))))

(defun str->list(str)
  (read-from-string (format nil "(~A)" str)))

(defun read-game(id)
  (let ((res (car (prima-query (format nil "SELECT * FROM GAMES WHERE id=~A" id)))))
    (when res
      (setq +game-log+ (make-game
			:id (nth 0 res)
			:time (nth 1 res)
			:kind (nth 2 res)
			:table-name (nth 3 res)
			:table-cards (str->list (nth 4 res))
			:history (str->list (nth 7 res))))
      (dolist (e (str->list (nth 6 res))) 
	(push (cons (first e) (get-player-name (second e)) ) (game-players +game-log+))
	(setf (aref (game-money +game-log+) (first e)) (third e)  )
	(when (fourth e) 
	  	(setf (aref (game-cards +game-log+) (first e)) (cadddr e)  ) ) 	) )))

(defun get-log-p-name(nr)
  (cdr (assoc nr (game-players +game-log+))))

(defun prima-call(name amount)
  (call name (min amount (player-stake name))))

(defun get-percept()
  (let ((p (pop (game-history +game-log+))))
    (when p
      (if (and (consp p) 
	       (equal +agent-name+  (get-log-p-name (first p))) 
	       +cards+ 
	       (not (eq 'sb (second p)))
	       (not (eq 'bb (second p)))
	       (= +game-kind+ (game-kind +game-log+))) 
	    (setq +agent-action+ (action))
	    (setq +agent-action+ nil))
      (if (consp p)
	  (cond
	    ((eq (second p) 'SB) (progn
				   (small-blind (get-log-p-name (first p)) (third p) )
				   (setq +agent-action+ nil)
				   (let ((agent (find +agent-name+ (game-players +game-log+) :test #'string= :key #'cdr)))
				     (when agent
				       (awhen (aref (game-cards +game-log+) (car agent) )
					 (if (= 4 (length it))  
					     (holecards (nth 0 it)  (nth 1 it)  (nth 2 it)  (nth 3 it))
					     (holecards  (nth 0 it)  (nth 1 it))    ))  ))))
	    ((eq (second p) 'BB)  (big-blind (get-log-p-name (first p)) (third p) )  )
	    ((eq (second p) 'F)  (fold (get-log-p-name (first p)))  )
	    ((eq (second p) 'C)  (prima-call (get-log-p-name (first p)) (third p) )  )
	    ((eq (second p) 'B)  (bet (get-log-p-name (first p)) (third p) )  )
	    ((eq (second p) 'R)  (bet (get-log-p-name (first p)) (third p) 'R )  )
	    ((eq (second p) 'A)  (all-in (get-log-p-name (first p)) (third p) )  )    )
	  (cond
	    ((eq p 'FLOP)  (flop (nth 0 (game-table-cards +game-log+)) 
				 (nth 1 (game-table-cards +game-log+))
				 (nth 2 (game-table-cards +game-log+))))
	    ((eq p 'TURN)  (turn (nth 3 (game-table-cards +game-log+)))) 
	    ((eq p 'RIVER)  (river (nth 4 (game-table-cards +game-log+))))    )))))
    


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
  `(let ((it (match-string ,pattern +line+)))
    (when (and it (plusp (game-kind +game-log+)))
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

(defun check-description(l)
  (awhen (analyze-description-line l)
    (setf (game-kind +game-log+) (car it))
    (setf (game-table-name +game-log+) (cdr it))))

 




;; ---------- File manipulation -----------------------------

(defun open-log(name)
  (setq +game-log+ nil 
	+file+ (open name)))


(defun next-line()

  (setq +line+ (read-line +file+ nil 'end))
  (when (eq +line+ 'end) (return-from next-line))
  ;(setq +line+ (remove-return +line+))
  (check-description +line+) ;"** Dolly in chains [Omaha] (0.5|1) Pot-Limit ..."

  (or

   (let ((it (match-string '("**" "Game" "ID" ?id "starting" "-" ?date ?time) +line+)))
     (when it
       (when +game-log+ (insert-game))
       (setq +game-log+ (make-game  :id (get-var ?id) :time  (format nil "~A ~A" 
								     (get-var ?date)
								     (remove-return (get-var ?time)))))   ))

   (with-pattern '(- ?player "sitting" "in" "seat" ?nr "with" ?amount ?info)
     (unless (string= (remove-return (get-var  ?info)) "[Sitting out]")
       (let ((seat (1- (parse-integer (get-var ?nr))))
	     (name (get-var ?player)))
	 (push (cons name seat) (game-players +game-log+))
	 (setf (aref (game-money +game-log+) seat) (get-money ?amount))	    )))

   (with-pattern '(?player "posted" "the" "small" "blind" "-" ?amount)
     (let ((nr (player-pos (get-var ?player) (game-players +game-log+)))
	   (money  (get-money  ?amount)))
       (setf  (game-history +game-log+) (format nil "(~A SB ~A)" nr money) )
       (setf (game-blind +game-log+) money) ))

   (with-pattern '(?player "posted" "the" "big" "blind" "-" ?amount)
     (let ((nr (player-pos (get-var ?player) (game-players +game-log+)))
	   (money  (get-money  ?amount)))
       (setf  (game-history +game-log+) (format nil "~A (~A BB ~A)" (game-history +game-log+)  nr money) )
       (setf (game-blind +game-log+) money) ))

   (with-pattern '("**" "Dealing" "card" "to" "grabola:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 ?r4 "of" ?c4 )
	  (setf (aref (game-cards +game-log+) (player-pos "grabola" (game-players +game-log+)))
		(format nil "~A ~A ~A ~A"
			(texts->card 
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
		       (remove-return (get-var  ?c4))))) )

   (with-pattern '("**" "Dealing" "card" "to" "grabola:" ?r1 "of" ?c1 ?r2 "of" ?c2  )
	  (setf  (aref (game-cards +game-log+) (player-pos "grabola" (game-players +game-log+)))
		(format nil "~A ~A"
			(texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (remove-return (get-var ?c2)))) ))

	(with-pattern '("**" "Dealing" "the" "flop:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 )
	  (setf  (game-history +game-log+) (format nil "~A FLOP " (game-history +game-log+)) )
	  (setf (game-table-cards +game-log+)
		(format nil "~A ~A ~A"
		 (texts->card 
		  (get-var ?r1) 
		  (string-right-trim '(#\,) (get-var ?c1))) 
		 (texts->card 
		  (get-var ?r2) 
		  (string-right-trim '(#\,) (get-var ?c2))) 
		 (texts->card 
		  (get-var ?r3) 
		  (remove-return (get-var ?c3))) 	 )))

	(with-pattern '("**" "Dealing" "the" "turn:" ?r1 "of" ?c1 )
	  (setf  (game-history +game-log+) (format nil "~A TURN " (game-history +game-log+)) )
	  (setf  (game-table-cards +game-log+)
		 (format nil "~A ~A"
		  (game-table-cards +game-log+)
		  (texts->card 
		   (get-var ?r1) 
		   (remove-return (get-var ?c1))) )))

	(with-pattern '("**" "Dealing" "the" "river:" ?r1 "of" ?c1 )
	  (setf  (game-history +game-log+) (format nil "~A RIVER " (game-history +game-log+)) )
	  (setf  (game-table-cards +game-log+)
		 (format nil "~A ~A"
		  (game-table-cards +game-log+)
		  (texts->card 
		   (get-var ?r1) 
		   (remove-return (get-var ?c1))) )))

	(with-pattern '(?player folded)
	  (setf  (game-history +game-log+) (format nil "~A (~A F)"
						   (game-history +game-log+)
						   (player-pos (get-var ?player) (game-players +game-log+))) ))


	(with-pattern '(?player "called" "-" ?amount)
	  (let ((nr (player-pos (get-var ?player) (game-players +game-log+)))
		(money  (get-money ?amount)))
	    (setf  (game-history +game-log+) (format nil "~A (~A C ~A)" (game-history +game-log+)  nr money) )))


	(with-pattern '(?player checked)
	  (let ((nr (player-pos (get-var ?player) (game-players +game-log+))))
	    (setf  (game-history +game-log+) (format nil "~A (~A C 0)" (game-history +game-log+)  nr) )))


	(with-pattern '(?player "bet" "-" ?amount)
	  (let ((nr (player-pos (get-var ?player) (game-players +game-log+)))
		(money  (get-money ?amount)))
	    (setf  (game-history +game-log+) (format nil "~A (~A B ~A)" (game-history +game-log+)  nr money) )))


	(with-pattern '(?player "raised" "-" ?amount)
	  (let ((nr (player-pos (get-var ?player) (game-players +game-log+)))
		(money  (get-money ?amount)))
	    (setf  (game-history +game-log+) (format nil "~A (~A R ~A)" (game-history +game-log+)  nr money) )))


	(with-pattern '(?player "went" "all-in" "-" ?amount)
	  (let ((nr (player-pos (get-var ?player) (game-players +game-log+)))
		(money  (get-money ?amount)))
	    (setf  (game-history +game-log+) (format nil "~A (~A A ~A)" (game-history +game-log+)  nr money) )))


	(with-pattern '(?player "shows:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 ?r4 "of" ?c4)
	  (setf 
	   (aref (game-cards +game-log+) (player-pos (get-var ?player) (game-players +game-log+)))
		(format nil "~A ~A ~A ~A"
			(texts->card 
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
		       (remove-return (get-var ?c4))))))

	(with-pattern '(?player "shows:" ?r1 "of" ?c1 ?r2 "of" ?c2)
	  (setf 
	   (aref (game-cards +game-log+) (player-pos (get-var ?player) (game-players +game-log+)))
		(format nil "~A ~A" (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		       (remove-return (get-var ?c2))))) )

	(with-pattern '(?player "mucks:" ?r1 "of" ?c1 ?r2 "of" ?c2 ?r3 "of" ?c3 ?r4 "of" ?c4)
	  (setf 
	   (aref (game-cards +game-log+) (player-pos (get-var ?player) (game-players +game-log+)))
		(format nil "~A ~A ~A ~A" (texts->card 
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
		       (remove-return (get-var ?c4))))))

	(with-pattern '(?player "mucks:" ?r1 "of" ?c1 ?r2 "of" ?c2)
	  (setf 
	   (aref (game-cards +game-log+) (player-pos (get-var ?player) (game-players +game-log+)))
		(format nil "~A ~A" (texts->card 
		       (get-var ?r1) 
		       (string-right-trim '(#\,) (get-var ?c1))) 
		      (texts->card 
		       (get-var ?r2) 
		        (remove-return (get-var ?c2))))) )



   (next-line)))

(defun process-lines()
  (when (next-line) (process-lines)))

(defun analyze-log(name)
  (open-log name)
  (process-lines)
  (when +game-log+ (insert-game))
  (close +file+))


(defun time->napis(i)
  (multiple-value-bind (a b c d e f) (decode-universal-time i)
    (progn (format nil "~A-~A-~A ~A:~A:~A" f e d c b a)   )))

(defun get-games-list(query)
  (setq +games-list+ (prima-query (format nil "SELECT id,game_kind,blind,czas FROM games WHERE  ~A" query))))
;; ----------------------- View -----------------------------

(defmacro main-page(&body body)
  `(html
    (:html
     ;(:head "<link rel='stylesheet' href='styl.css' type='text/css'>" )
     (:head (:style
	     "body { background-color: silver;}"
	     ".karty { list-style : none; margin: 0; padding: 0; font-size: 12pt;}"
	     ".spades { display: inline; color: black; padding-left: 5px;}"
	     ".hearts { display: inline; color: red; padding-left: 5px;}"
	     ".diamonds { display: inline; color: blue; padding-left: 5px;}"
	     ".clubs { display: inline; color: green; padding-left: 5px;}"      ))
     (:body
      ((:a href "/search") " [Select games list] ")
      ((:a href "/gameslist") " [Show games list] ")
      ((:a href "/game") " [Show current game] ")
      :hr
      
      ,@body        ))))

(defun date-chooser(name)
  (html
    ((:select name (format nil "~A_day" name)) 
     (dotimes (d 31) (html ((:option value (1+ d)) (:princ (1+ d))))))

    ((:select name (format nil "~A_month" name))  
     ((:option value "1")"January")
     ((:option value "2")"February")
     ((:option value "3")"March")
     ((:option value "4")"April")
     ((:option value "5")"May")
     ((:option value "6")"June")
     ((:option value "7")"July")
     ((:option value "8")"August")
     ((:option value "9")"September")
     ((:option value "10")"October")
     ((:option value "11")"November")
     ((:option value "12")"December"))

    ((:select name (format nil "~A_year" name))  
     ((:option value "2007")"2007")
     ((:option value "2006")"2006"))   ))
     

(def-url-0 "/search"
   (main-page
    (let ((player (get-parameter "player"))
	  (fromy   (get-parameter "from_year"))
	  (fromm   (get-parameter "from_month"))
	  (fromd   (get-parameter "from_day"))
	  (toy   (get-parameter "to_year"))
	  (tom   (get-parameter "to_month"))
	  (tod   (get-parameter "to_day"))
	  (kind (get-parameter "kind"))
	  (sql ""))
	  
      (html
       ((:form action "/search" method "post" )
	(:table
	 (:tr (:td "Player:")  (:td ((:input name "player" value (if player player "grabola")))((:a href "/players")" [choose players]")))
	 (:tr (:td "   From:")     (:td (date-chooser "from") ))
	 (:tr (:td "   To:")     (:td (date-chooser "to") ))
	 (:tr (:td " Game:")     (:td ((:select name "kind")  
				       ((:option value "1")"Holdem No Limit")
				       ((:option value "2")"Holdem Fixed Limit")
				       ((:option value "3")"Omaha Pot Limit")     )))
	 (:tr ((:td colspan "2") ((:input type "submit" value "Select games")))   ))))
	
      (when (and player fromy toy kind)
	(setq sql (format nil "SELECT game_id,game_kind,blind,czas FROM player_log WHERE player_nr=~A AND  czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59' AND game_kind=~A  ORDER BY czas ASC" (get-player-nr player t) fromy fromm fromd toy tom tod kind))
	(setq +games-list+ (prima-query sql))
	(html "Found " (:b (:princ (length +games-list+))) " games from " (:princ (format nil "~A/~A/~A" fromd fromm fromy )) 
	      " to " (:princ (format nil "~A/~A/~A" tod tom toy )) 
	      :br ((:a href "/gameslist") "See them!"))  ))))

(def-url-0 "/players"
   (main-page
     (dolist (p (mapcar #'car (prima-query "SELECT name FROM players ORDER BY name ASC")))
       (html ((:a href (format nil "/search?player=~A" p)) (:princ p)) :br)        )))

(def-url-0 "/reloadgame"
    (read-game (game-id +game-log+))
    (awhen (get-parameter "agent") (setq +agent-name+ it))
    (main-page
      ((:a href "/game") "Game reloaded")))

(def-url-0 "/gamehistory"
    (main-page

      (let ((id (get-number-parameter "id")))
	(when id (read-game id))
	(html
	 ((:table border 1)
	 ; :tr (dolist (plr (game-players +game-log+))  (html :td (:princ plr)) )
	  (dolist (p (copy-list (game-history +game-log+)))
	    
	    (if (consp p)
		(let ((nr (first p))
		      (action (second p)))
		  (get-percept)
		  (html :tr
			(dotimes (i (length (game-players +game-log+))) 
			  (html :td (:princ (if (eq i nr) action "")  )))  ))
	    (progn (get-percept)
		   (html :tr ((:td colspan "9") (:princ p)))))
	    ) )))))


(def-url-0 "/game"
    (main-page

      (let ((id (get-number-parameter "id")))
	(when id
	  (when (/= id (game-id +game-log+))
	    (read-game id)

	    ;testowanie akcji systemu:
	    ;szukamy gracza ktorego karty sa widoczne
	    ;jesli nie ma graboli:

		 (unless (find "grabola" (game-players +game-log+) :test #'string= :key #'cdr)
		   (dotimes (i 10)
		     (when (aref (game-cards +game-log+) i) ;mamy go
					;ustawiamy pod niego gre:
		       (setq +agent-name+  (get-log-p-name i))))) 

	    (setq +agent-action+ nil)
	    (setq +game-kind+ (game-kind +game-log+))))


	(get-percept)

	(let* ((idx (position (game-id +game-log+) +games-list+ :key #'car))
	       (prev (1- idx))
	       (nxt (1+ idx)))
	  (when (plusp (1+ prev)) 
	    (html ((:a href (format nil "/game?id=~A" (car (nth prev +games-list+)))) "[<<< Prev Game <<<]" )))
	  (html ((:a href "/reloadgame") "[  Reload game  ]"))
	  (when (< nxt (length +games-list+))
	    (html ((:a href (format nil "/game?id=~A" (car (nth nxt +games-list+)))) "[>>> Next Game >>>]" )))
	  (html :hr))


	(case (game-kind +game-log+)
	    (3 (html "<b>Omaha Pot Limit</b><br>"))
	    (1 (html "<b>Holdem No Limit</b><br>"))
	    (0 (html "<b>Holdem Fixed Limit</b><br>")  ))
	(html
	   "Game " ((:a href "/game") (:princ (game-id +game-log+))) " at "  (:princ (time->napis (game-time +game-log+))) " " (:princ (length (game-players +game-log+))) " players table"  :hr)

	(dotimes (i 10)
	    (awhen (aref  (game-cards +game-log+) i)
	      (html (:b ((:a href (format nil "/reloadgame?agent=~A" (get-log-p-name i))) (:princ (get-log-p-name i))))
		    (:princ (karty->html it)) 
		    (when +table-cards+
		      (if (= 4 (length it))
			  (html (:princ (render-rank (omaha-ocena it +table-cards+))) " , ")
			  (html (:princ (render-rank (ocena (append it +table-cards+)))) " , ")) )
		    (:princ (HS it)) 
		    :hr )))

	(when +table-cards+ (html (:princ (karty->html +table-cards+)) :hr))
	(when +agent-action+ (html (:b "Action for " (:princ +agent-name+) ": " ) (:princ +agent-action+) :br :br))
	(html (:b "+history+ ") (:princ (reverse +history+)) :br (:b "+pot+ ") (:princ +pot+) :hr)

	(html "<table border='1'>")
	(dotimes (i (length +name->seat+))
	  (html "<tr><td>"(:b (:princ (first (nth i (reverse +name->seat+))))) 
		"<td>" (:princ (aref +active-players+ i))
		"<td>" (:princ (aref +balance+ i)) 
		"<td>" (:princ (aref +total-balance+ i))
		"<td>" (:princ (reverse (aref +player-history+ i))) ))
	(html "</table>")
	(when (null (game-history +game-log+)) (html (:h2 "Finished!")))
	;(html :hr)
	;(let ((*standard-output* *html-stream*))
	 ;   (show-game))
	)))

(def-url-0 "/gameslist"
    (main-page
	(dolist (g +games-list+)
	  (html (:princ (third g)) "$ " )
	  (case (second g)
	    (3 (html "<b>Omaha</b> Pot Limit     "))
	    (1 (html "<b>Holdem</b> No Limit     "))
	    (0 (html "<b>Holdem</b> Fixed Limit  ")))  
	  (html ((:a href (format nil "/game?id=~A" (first g))) "[" (:princ (first g)) "]") " at " (:princ (time->napis (fourth g)))   :br)
	  )
      ))


(def-url-0 "/insertcomment"
    (main-page
      (let ((plr (get-parameter "player"))
	    (com (get-parameter "comment"))
	    query)
	(when (and plr com)
	  (setq query  (format nil "INSERT INTO comments VALUES(~A,DEFAULT,'~A')"
			       (get-player-nr plr)
			       com))
	  ;(html (:princ query) :hr)
	  (prima-query query)) 	  
	(html ((:form method "post" action "/insertcomment")
	       "Player's name:" ((:input name "player" value (if plr plr ""))) :br
	       "Comment:" ((:input name "comment")) :br
	       ((:input type "submit" value "Insert comment")) ))   )))

(def-url-0 "/showcomments"
    (main-page
      (dolist (c (prima-query "SELECT * FROM comments"))
	(html (:princ c)))))