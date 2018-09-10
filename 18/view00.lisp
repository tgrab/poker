(let ((historia nil))
  (defun oblicz(&optional sexp)
    (when sexp
      (push sexp historia)
      (format t "<hr>> ~A<hr> " sexp)
      (format t "> ~A<hr>" (eval (read-from-string sexp))))
    (dolist (e (subseq historia 0 (min 30 (length historia))))
      (format t "<a href='/eval?sexp=~A'>~A</a><br>" e e))))  


(defmacro show-game-stats()
  '(progn
    (awhen (table-cards +game+) (format *log-stream* "Table: ~A   " (karty->napisy it)))
    (awhen (cards +game+) (format *log-stream* "Hand: ~A   ~&" (karty->napisy it)))
    (dotimes (i (length (name->seat +game+)))
      (when (aref (active-players +game+) i)
	(format *log-stream* "   seat ~A:   <~A> ~A~&" i
		(aref (balance +game+) i)
		(first (nth i (reverse (name->seat +game+))))
		;(reverse (player-history i))
		)))
    (format *log-stream* "GET-STAKE:~$ STAKE:~$ POT:~$~%~%" (get-stake) (stake +game+) (pot +game+))
    (force-output *log-stream*)))

(defmacro view-register-client()
  '(progn
    (awhen (get-parameter "agent") 
     (setq *agent-name*   it))
    (awhen (get-parameter "kind")
     (set-game-kind (parse-integer it)))
    (awhen (get-parameter "casino")
     (setf (casino +game+) it))
    (princ "registerclient")))

(def-url "/registerclient" (view-register-client))

(defmacro view-new-game()
    '(progn
      (handler-case
	  (when (and *logging* (history +game+))
	    (push +game+ *games-list*)
	    (create-infos))
	(error (e)
	  (format *log-stream* "!!!!!! BLAD !!!!!!!!!~&~A ~A~&" e +game+)))
      (new-game :id (get-parameter "id"))
      (format *log-stream* "--------------------------~&newgame~&")
      (princ "newgame")))

(def-url "/newgame" 
	(view-new-game))

(def-url "/gameinfo" 
	(progn
		(awhen (get-parameter "id") (setf (id +game+) it))
		(princ "thanks")))

(defmacro view-small-blind()
    '(progn
      (let ((n  (get-parameter "name"))
	    (a (read-from-string (get-parameter "amount")))  )
	(small-blind n a )
	(format *log-stream* "[ SMALL BLIND ~A ~A ]~&" n a)
	(show-game-stats))
	(princ "smallblind")))

(def-url "/smallblind" (view-small-blind))

(defmacro view-big-blind()
    '(progn
      (let ((n  (get-parameter "name"))
	    (a (read-from-string (get-parameter "amount")))  )
	(big-blind n a )
	(format *log-stream* "[ BIG BLIND ~A ~A ]~&" n a)
	(show-game-stats))
	(princ "bigblind")))

(def-url "/bigblind" (view-big-blind))

(defmacro view-call()
    '(progn
      (let ((n  (get-parameter "name"))
	    (a (read-from-string (get-parameter "amount")))  )
	(call n a )
	(format *log-stream* "[ CALL ~A ~A ]~&" n a)
	(show-game-stats))
	(princ "call")))

(def-url "/call" (view-call))

(defmacro view-fold()
    '(progn
      (let ((n  (get-parameter "name")))
	(fold n )
	(format *log-stream* "[ FOLD ~A ]~&" n)
	(show-game-stats))
	(princ "fold")))


(def-url "/fold" (view-fold))

(defmacro view-bet()
    '(progn
      (let ((n  (get-parameter "name"))
	    (a (read-from-string (get-parameter "amount")))  )
	(bet n a )
	(format *log-stream* "[ BET ~A ~A ]~&" n a)
	(show-game-stats))
	(princ "bet")))


(def-url "/bet" (view-bet))

(defmacro view-raise()
    '(progn
      (let ((n  (get-parameter "name"))
	    (a (read-from-string (get-parameter "amount")))  )
	(bet n a 'r)
	(format *log-stream* "[ RAISE ~A ~A ]~&" n a)
	(show-game-stats))
	(princ "raise")))



(def-url "/raise" (view-raise))

(defmacro view-allin()
    '(progn
      (let ((n  (get-parameter "name"))
	    (a (get-parameter "amount"))  )
	(if a 
	    (allin n (read-from-string a))
	    (allin n 0))
	(format *log-stream* "[ ALL-IN ~A ]~&" n)
	(show-game-stats))
	(princ "allin")))


(def-url "/allin" (view-allin))

(defmacro view-flop()
    '(progn
	(flop (parse-integer (get-parameter "c1"))
	      (parse-integer (get-parameter "c2"))
	      (parse-integer (get-parameter "c3")))
	(format *log-stream* "[ FLOP ~A]~&" (karty->napisy (table-cards +game+)))
	(format t "flop")))

(def-url "/flop" (view-flop))

(defmacro view-turn()
    '(progn
	(turn (parse-integer (get-parameter "c1")))
	(format *log-stream* "[ TURN ~A]~&" (karty->napisy (table-cards +game+)))
	(format t "turn")))

(def-url "/turn" (view-turn))

(defmacro view-river()
    '(progn
	(river (parse-integer (get-parameter "c1")))
	(format *log-stream* "[ RIVER ~A]~&" (karty->napisy (table-cards +game+)))
	(format t "river")))


(def-url "/river" (view-river))

(defmacro view-holecards()
  '(progn
    (let ((c1 (awhen  (get-parameter "c1") (parse-integer it)))
	(c2 (awhen  (get-parameter "c2") (parse-integer it)))
	(c3 (awhen  (get-parameter "c3") (parse-integer it)))
	(c4 (awhen  (get-parameter "c4") (parse-integer it))) )
      (holecards c1 c2 c3 c4))
    (show-game-stats)
    (format t "hand" )))

(def-url "/holecards" (view-holecards))


(def-url "/action" 
    (let ((a (action)))
      (format *log-stream* "[[ ACTION: ]] ~A~%~%" a)
      (princ (car a))))
