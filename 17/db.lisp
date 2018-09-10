(clsql:connect '("127.0.0.1" "prima" "tgrab" "" "5433") :database-type :postgresql)

;(clsql:query "select * from players")

(let ((names (make-hash-table)))
  (defun get-player-name-table() names)
  (defun get-player-name( nr )
    (or (gethash nr names)
	  (let ((val (caar (clsql:query (format nil "SELECT name FROM players WHERE nr=~A" nr)))))
	    (setf (gethash nr names)  val)  	  ))))

(defun str->list(str)
  (read-from-string (format nil "(~A)" str)))

(defun get-log-player-name(nr)
  (cdr (assoc nr (players-log +game+))))

;side-effects:
(defun read-game(id)
  (let ((res (car (clsql:query (format nil "SELECT * FROM GAMES WHERE id=~A" id)))))
    (when res
      (setq +game+ (make-instance 'game-log
			:id (nth 0 res)
			:czas (nth 1 res)
			:kind (nth 2 res)
			:table-name (nth 3 res)
			:table-cards-log (str->list (nth 4 res))
			:history-log (str->list (nth 7 res))))        
      (dolist (e (str->list (nth 6 res))) 
	(push (cons (first e) (get-player-name (second e)) ) (players-log +game+))
	(setf (aref (money-log +game+) (first e)) (third e)  )
	(when (fourth e) 
	  	(setf (aref (cards-log +game+) (first e)) (cadddr e)  ) ) 	) )))

(defun prima-call(name amount)
  (call name (min amount (player-stake name))))

;side-effects:
(defun next-percept()
  (incf (percept-nr +game+))
  (let ((p (nth (percept-nr +game+) (history-log +game+))))
    (when p
       (if (consp p)
	  (cond
	    ((eq (second p) 'SB)  (small-blind (get-log-player-name (first p)) (third p) ))
	    ((eq (second p) 'BB)  (big-blind (get-log-player-name (first p)) (third p) )  )
	    ((eq (second p) 'F)  (fold (get-log-player-name (first p)))  )
	    ((eq (second p) 'C)  (prima-call (get-log-player-name (first p)) (third p) )  )
	    ((eq (second p) 'B)  (bet (get-log-player-name (first p)) (third p) )  )
	    ((eq (second p) 'R)  (bet (get-log-player-name (first p)) (third p) 'R )  )
	    ((eq (second p) 'A)  (allin (get-log-player-name (first p)) (third p) )  )    
)
	  (cond
	    ((eq p 'FLOP)  (flop (nth 0 (table-cards-log +game+)) 
				 (nth 1 (table-cards-log +game+))
				 (nth 2 (table-cards-log +game+))))
	    ((eq p 'TURN)  (turn (nth 3 (table-cards-log +game+)))) 
	    ((eq p 'RIVER)  (river (nth 4 (table-cards-log +game+))))    )))

    )  )