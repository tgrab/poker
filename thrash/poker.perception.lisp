(defun new-game()
  ;zerujemy wszystko:
  (setf +game+ (make-instance 'game))
  'ok)

(defun insert-player(name)
  (assert (< (free-seat +game+) 10))
  (when (string= name (agent-name +game+))
    (setf (agent-pos +game+) (free-seat +game+)))
  (push (cons name (free-seat +game+)) (name->seat +game+))
  (setf (aref (taken-seats +game+) (free-seat +game+)) t)
  (setf (aref (active-players +game+) (free-seat +game+)) t)
  (incf (free-seat +game+)))

(defun insert-player?(name)
  (and (null (table-cards +game+)) ;preflop 
       (not (assoc name (name->seat +game+) :test #'string=))
       (insert-player name)))

(defun player-pos(name &optional (g +game+))
  (cdr (assoc name (name->seat g) :test #'string=)))

;;-- PERCEPTS-- game-kind,...


;url: registerclient
;params: kind
;TODO: agentname
(defun set-game-kind(kind &optional (game +game+))
 (case kind
   (3 (<=into (memo game) (game-kind omaha)))
   (1 (<=into (memo game) (game-kind nolimit)) )
   (t (<=into (memo game) (game-kind fixed))) ))

;url: smallblind
;params: name,amount
(defun small-blind(name amount)
  (new-game)
  (incf (pot +game+) amount)
  (setf (stake +game+) amount (blind +game+) amount)
  (push (cons 'sb amount) (history +game+))
  (push (cons 'sb amount) (aref (player-history +game+) 0))
  (setf (aref (balance +game+) 0) (- amount))
  (insert-player name))

;url: bigblind
;params: name,amount
(defun big-blind(name amount)
  (incf (pot +game+) amount)
  (setf (stake +game+) amount (blind +game+) amount)
  (push (cons 'bb amount) (history +game+))
  (push (cons 'bb amount) (aref (player-history +game+) 0))
  (setf (aref (balance +game+) 1) (- amount))
  (insert-player name))

;url: call
;params: name,amount
(defun call(name amount)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (when nr
	  (push (cons 'c amount) (history +game+))
	  (push (cons 'c amount) (aref (player-history +game+) nr))
	  (decf (aref (balance +game+) nr) amount)
	  (incf (pot +game+) amount))))


;url: fold
;params: name
(defun fold(name)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (when nr
	  (push 'f (history +game+))
	  (push 'f (aref (player-history +game+) nr))
	  (setf (aref (active-players +game+) nr) nil) )))

;url: bet
;params: name,amount
(defun bet(name amount &optional (smb 'b))
  (insert-player? name)
  (let ((nr (player-pos name)))
    (when nr
	  (push (cons smb amount) (history +game+))
	  (push (cons smb amount) (aref (player-history +game+) nr))
	  (decf (aref (balance +game+) nr) amount)
	  (setf (stake +game+) amount)
	  (incf (pot +game+) amount))))

;url: allin
;params: name,amount
(defun allin(name amount)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (when nr
	  (push (cons 'a amount) (history +game+))
	  (push (cons 'a amount) (aref (player-history +game+) nr))
	  (decf (aref (balance +game+) nr) amount)
	  (when (> amount (stake +game+)) (setf (stake +game+) amount))
	  (setf (aref (active-players +game+) nr) nil)
	  (incf (pot +game+) amount))))

;url: flop
;params: c1,c2,c3
(defun flop(c1 c2 c3)
  (clear-round-memory)
  (setf (table-cards +game+) (list c1 c2 c3)
	(stake +game+) 0)
  (push 'flop (history +game+))
 (dotimes (i 10)
   (when (aref (active-players +game+) i) (push 'flop (aref (player-history +game+) i)))
   (incf (aref (total-balance +game+) i) (aref (balance +game+) i))
   (setf (aref (balance +game+) i) 0))
;;TODO
 )

;url: turn
;params: c1
(defun turn(c1)
  (clear-round-memory)
  (setf (stake +game+) 0)
  (push c1 (table-cards +game+))
  (push 'turn (history +game+))
 (dotimes (i 10)
   (when (aref (active-players +game+) i) (push 'turn (aref (player-history +game+) i)))
   (incf (aref (total-balance +game+) i) (aref (balance +game+) i))
   (setf (aref (balance +game+) i) 0))   )

;url: river
;params: c1
(defun river(c1)
  (clear-round-memory)
  (setf (stake +game+) 0)
  (push c1 (table-cards +game+))
  (push 'river (history +game+))
 (dotimes (i 10)
   (when (aref (active-players +game+) i) (push 'river (aref (player-history +game+) i)))
   (incf (aref (total-balance +game+) i) (aref (balance +game+) i))
   (setf (aref (balance +game+) i) 0))   )


(defun holecards(c1 c2 &optional c3 c4)
  (if (and c3 c4)
      (progn 
	(setf (cards +game+) (list c1 c2 c3 c4)) )
      (progn
	(setf (cards +game+) (list c1 c2))  )))

;;----------------------- ACTIONS -----------------------------------------------

(defun action-prove()
  (if (cards +game+)
      (progn
      (when (null (agent-pos +game+)) (setf (agent-pos +game+) (free-seat +game+)))
      (or
       (awhen (?- (decision fold ?rule)) (format nil "fold ~A" (subst-bindings it '?rule )))
       (awhen (?- (decision dont-bet ?rule)) (format nil "call ~A" (subst-bindings  it '?rule)))
       (awhen (?- (decision potbet1 ?rule)) (format nil "potbet1 ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision potbet2 ?rule)) (format nil "potbet2 ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision bet ?rule)) (format nil "bet ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision fuzzy-bet ?level ?rule)) (format nil "fuzzy-bet ~A ~A" (cdr (assoc '?level it)) (subst-bindings it '?rule)))
       (awhen (?- (decision call ?rule)) (format nil "call ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision fuzzy-call ?level ?rule)) (format nil "fuzzy-call ~A ~A" (cdr (assoc '?level it)) (subst-bindings it '?rule)))
       "fold no decision"))
    "fold not enough data"))

(defun action()
  (let ((a (tokens (action-prove))))
    (cond
      ((string= (car a) "fuzzy-bet") (if (< (/ (random 1000) 1000.0) 
					     (read-from-string (second a)))
					  "bet"
					  "call"))
      ((string= (car a) "fuzzy-call") (if (< (/ (random 1000) 1000.0) 
					     (read-from-string (second a)))
					  "call"
					  "fold"))
      (t (car a)))   ))
