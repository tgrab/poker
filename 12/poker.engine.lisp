;(declaim (optimize (speed 3) (safety 0)))

(in-package :poker.engine)


(defun next-player(agent biezacy)
  (when (= biezacy (1- (table-size agent)))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr (actions-list agent))
      nr
      (next-player agent nr)) ))

(defmacro set-next-player(a)
  `(progn
    (setf (last-active-player ,a) (active-player ,a))
    (setf (active-player ,a)
     (next-player ,a (active-player ,a)))))

(defmacro percept-round(agent round)
  `(progn 
    (setf (money-list ,agent) (make-list (table-size ,agent) :initial-element 0) )
    (setf (stake ,agent) 0)
    (push ,round (history (game ,agent)))
    (setf (actions-list agent) (mapcar #'(lambda(p) (when p t)) (actions-list agent)))
    (setf (active-player ,agent) (next-player ,agent (button (game agent))))))


;------- percepts from environment ----------------------------------
(defun percept-new-game-log( agent  game-log )
  (setf (game agent) (make-instance 'game-log 
				    :size (length (players game-log))
				    :button (button game-log)
				    :game-kind (game-kind game-log)
				    :players (mapcar
					      #'(lambda(p)
						  (when p
						    (make-instance 'player
								 :name (name p)
								 :money (money p)
								 :cards (cards p))))
					      (players game-log))))


  (setf (actions-list agent) (mapcar #'(lambda(p) (when p t)) (players game-log)))
  (setf (money-list agent) (make-list (length (players game-log)) :initial-element 0))
  (setf (stake agent) (big-blind game-log))
  ;wyznaczamy small blinda:
  (setf (active-player agent) (next-player agent (button game-log)))
  (setf (nth (active-player agent) (actions-list agent)) 'sb)
  (setf (nth (active-player agent) (money-list agent)) (small-blind game-log))
  (decf (money (nth (active-player agent) (players (game agent)))) 
	  (small-blind game-log))
  ;wyznaczamy big blinda:
  (setf (active-player agent) (next-player agent (active-player agent)))
  (setf (nth (active-player agent) (actions-list agent)) 'bb)
  (setf (nth (active-player agent) (money-list agent)) (big-blind game-log))
  (decf (money (nth (active-player agent) (players (game agent)))) 
	  (big-blind game-log))
  ;wyznaczamy gracza aktywnego:
    (setf (active-player agent) (next-player agent (active-player agent)))
   (setf (pot agent) (+ (small-blind game-log) (big-blind game-log)))
  )

(defun percept-new-game( agent &key (table-size 10) (sblind 0.5) (bblind 1.0))
  (setf (game agent) 
	(make-instance 'game :size table-size :sblind sblind :bblind bblind)

	(actions-list agent) (make-list table-size :initial-element t)
	(money-list agent)   (make-list table-size :initial-element 0)
	(stake agent) bblind)
    (setf (active-player agent) (next-player agent 1))
    (setf (nth 0 (money-list agent)) sblind)
    (setf (nth 1 (money-list agent)) bblind)
    (setf (pot agent) (+ sblind bblind)))


(defun percept-hand(agent &rest cards)
  (setf (cards agent) cards
	(still-playing? agent) t))

(defun percept-fold(agent)
  (setf (nth (active-player agent) (actions-list agent)) nil)
    (push 'f (history (game agent)))
    (set-next-player agent))
 
(defun percept-check(agent &optional amount)
  (let* ((how-many (or amount (stake agent)))
	(ile-zostalo (- how-many (nth (active-player agent) (money-list agent)))))
    (setf (nth (active-player agent) (money-list agent)) how-many)
    (setf (nth (active-player agent) (actions-list agent))'c)
    (push 'c (history (game agent)))
    (decf (money (nth (active-player agent) (players (game agent)))) 
	  ile-zostalo)
    (incf (pot agent)  ile-zostalo)
    (set-next-player agent)))

(defun percept-bet(agent &optional amount)
  (let ((how-many (or amount (if (< (length (table-cards agent)) 4)
				 (small-blind (game agent))
				 (big-blind (game agent))))))
    (setf (stake agent) how-many)
    (setf (nth (active-player agent) (money-list agent)) how-many)
    (setf (nth (active-player agent) (actions-list agent)) (cons 'b how-many))
    (push (cons 'b how-many) (history (game agent)))
    (decf (money (nth (active-player agent) (players (game agent)))) how-many)
    (incf (pot agent) 	  how-many)
    (set-next-player agent)))
 
(defun percept-flop(agent &rest cards)
  (setf (table-cards (game agent)) cards)
  (percept-round agent 'flop))

(defun percept-turn(agent card)
  (push card (table-cards (game agent)))
  (percept-round agent 'turn))   

(defun percept-river(agent card)
  (push card (table-cards (game agent)))
  (percept-round agent 'river))

(defgeneric decision(agent))

(defmethod decision((a agent))
  'check)

(defmethod decision((a rule-based-agent))
  (rule-based-decision a))

(defmethod percept-get-action((a agent))
 (when (null (agent-position a))
   (setf (agent-position a) (active-player a)))
 (decision a))


;;---------------------------------------------------------
(defun simulate-game(game-log)
  (let ((c (reverse (table-cards game-log)))
	(hist (reverse (history game-log)))
	(a (make-instance 'agent)))

    (percept-new-game-log a game-log)
    (dolist (h hist)
       (cond
	 ((eq h 'F) (percept-fold a))
	 ((eq h 'C) (percept-check a))
	 ((consp h) (percept-bet a (cdr h)))
	 ((eq h 'FLOP)
	  (percept-flop a (nth 0 c) (nth 1 c) (nth 2 c) )  )
	 ((eq h 'TURN)
	  (percept-turn a (nth 3 c) )  )
	 ((eq h 'RIVER)
	  (percept-turn a (nth 4 c)) )))
    a))