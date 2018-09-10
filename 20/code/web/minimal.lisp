(in-package :poker)

(def-url "/registerclient"
    (progn
	(awhen (param "agent") 
         (setq *agent-name*   it))
	(awhen (param "kind")
         (setq *kind* (parse-integer it)))
	(awhen (param "casino")
         (setq *casino* it))
    (setf (games *gameslist*) nil)
    (setq *game* nil)
    (princ "registerclient"))  )

(def-url "/newgame" 
    (progn
      (when *game* 
	(push *game* (games *gameslist*)))
      (setq *game* (new-game :id (read-from-string (param "id")) :prev-game *game*) )
      (format *log-stream* "--------------------------~&newgame~&")
      (princ "newgame")) )

(def-url "/gameinfo" 
	(progn
        	(awhen (param "id") (setf (id *game*) it))
	        	(princ "thanks")))

(defmacro view-small-blind()
    '(progn
      (let ((n  (param "name"))
	    (a (read-from-string (param "amount")))  )
	(small-blind n a )
	(format *log-stream* "[ SMALL BLIND ~A ~A ]~&" n a)
	(princ "smallblind"))))

(def-url "/smallblind" (view-small-blind))

(defmacro view-big-blind()
    '(progn
      (let ((n  (param "name"))
	    (a (read-from-string (param "amount")))  )
	(big-blind n a )
	(format *log-stream* "[ BIG BLIND ~A ~A ]~&" n a)
	(princ "bigblind"))))

(def-url "/bigblind" (view-big-blind))

(defmacro view-call()
    '(progn
      (let ((n (param "name"))
	    (a (nparam "amount"))  )
	(call n a )
	(format *log-stream* "[ CALL ~A ~A ]~&" n a)
	(princ "call"))))

(def-url "/call" (view-call))

(defmacro view-fold()
    '(progn
      (let ((n  (param "name")))
	(fold n )
	(format *log-stream* "[ FOLD ~A ]~&" n)
	(princ "fold"))))


(def-url "/fold" (view-fold))

(defmacro view-bet()
    '(progn
      (let ((n  (param "name"))
	    (a (read-from-string (param "amount")))  )
	(bet n a )
	(format *log-stream* "[ BET ~A ~A ]~&" n a)
	(princ "bet"))))


(def-url "/bet" (view-bet))

(defmacro view-raise()
    '(progn
      (let ((n  (param "name"))
	    (a (read-from-string (param "amount")))  )
	(raise n a)
	(format *log-stream* "[ RAISE ~A ~A ]~&" n a)
	(princ "raise"))))



(def-url "/raise" (view-raise))

(defmacro view-allin()
    '(progn
      (let ((n  (param "name"))
	    (a (param "amount"))  )
	(if a 
	    (allin n (read-from-string a))
	    (allin n 0))
	(format *log-stream* "[ ALL-IN ~A ]~&" n)
	(princ "allin"))))


(def-url "/allin" (view-allin))

(defmacro view-flop()
    '(progn
	(round-flop (parse-integer (param "c1"))
	      (parse-integer (param "c2"))
	      (parse-integer (param "c3")))
	(format *log-stream* "[ FLOP ~A]~&" (karty->napisy (table-cards *game*)))
	(format t "flop")))

(def-url "/flop" (view-flop))

(defmacro view-turn()
    '(progn
	(round-turn (parse-integer (param "c1")))
	(format *log-stream* "[ TURN ~A]~&" (karty->napisy (table-cards *game*)))
	(format t "turn")))

(def-url "/turn" (view-turn))

(defmacro view-river()
    '(progn
	(round-river (parse-integer (param "c1")))
	(format *log-stream* "[ RIVER ~A]~&" (karty->napisy (table-cards *game*)))
	(format t "river")))


(def-url "/river" (view-river))

(defmacro view-holecards()
  '(progn
    (let ((c1 (awhen  (param "c1") (parse-integer it)))
	(c2 (awhen  (param "c2") (parse-integer it)))
	(c3 (awhen  (param "c3") (parse-integer it)))
	(c4 (awhen  (param "c4") (parse-integer it))) )
      (holecards *game* c1 c2 c3 c4))
    (format t "hand" )))

(def-url "/holecards" (view-holecards))


(def-url "/action" 
    (let ((a (action)))
      (format *log-stream* "[[ ACTION: ]] ~A~%~%" a)
      (princ (car a))))
