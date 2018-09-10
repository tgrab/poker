;(declaim (optimize (speed 3) (safety 0)))

(in-package :poker.engine)

(defun is-active?(agent seat)
  (and (nth seat (actions-list agent))
       (plusp (money (nth seat (players (game agent)))))))

(defun game-end(agent)
  (= 1 (count-if
	#'(lambda(x)
	    (is-active? agent x) )
	'(0 1 2 3 4 5 6 7 8 9))))

(defun next-player(agent biezacy)
  (when (game-end agent) (return-from next-player biezacy))
  (when (= biezacy (1- (table-size agent)))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (and (nth nr (actions-list agent))
	    ;nie jest all-in:
	    (plusp (money (nth nr (players (game agent))))))
	    
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
    (setf (last-actions agent) (mapcar #'(lambda(p) (when p t)) (last-actions agent)))
    (setf (active-player ,agent) (next-player ,agent (button (game agent))))))


;------- percepts from environment ----------------------------------
(defun percept-new-game-log( agent  game-log )
  (setf (game agent) (make-instance 'game 
				    :size (length (players game-log))
				    :button (button game-log)
				    :game-kind (game-kind game-log)
				    :balance (aif (balance game-log)
						  (copy-list it)
						  (make-list 10 :initial-element 0))
				    :players (mapcar
					      #'(lambda(p)
						  (when p
						    (make-instance 'player
								 :name (name p)
								 :money (money p)
								 :cards (cards p))))
					      (players game-log))))


  (setf (actions-list agent) (mapcar #'(lambda(p) (when p t)) (players game-log)))
  (setf (last-actions agent) (mapcar #'(lambda(p) (when p t)) (players game-log)))
  (setf (money-list agent) (make-list (length (players game-log)) :initial-element 0))
  (setf (all-actions agent) (make-list (length (players game-log)) :initial-element nil))
  (setf (stake agent) (big-blind game-log))
  ;wyznaczamy small blinda:
  (setf (active-player agent) (next-player agent (button game-log)))
  (setf (nth (active-player agent) (last-actions agent)) (cons 'small 'blind))
  (push 'sb (nth (active-player agent) (all-actions agent)))
  (setf (nth (active-player agent) (money-list agent)) (small-blind game-log))
  (decf (nth (active-player agent) (balance (game agent))) (small-blind game-log))
  ;(decf (money (nth (active-player agent) (players (game agent)))) 
	;  (small-blind game-log))
  ;wyznaczamy big blinda:
  (setf (active-player agent) (next-player agent (active-player agent)))
  (setf (nth (active-player agent) (last-actions agent)) (cons 'big 'blind))
  (push 'bb (nth (active-player agent) (all-actions agent)))
  (setf (nth (active-player agent) (money-list agent)) (big-blind game-log))
  (decf (nth (active-player agent) (balance (game agent))) (big-blind game-log))
;  (decf (money (nth (active-player agent) (players (game agent)))) 
	;  (big-blind game-log))
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
  (setf (nth (active-player agent) (last-actions agent)) 'fold)
  (set-next-player agent))
 
(defun percept-check(agent &optional amount)
  (let* ((how-many (or amount (stake agent)))
	 (ile-zostalo (- how-many (nth (active-player agent) (money-list agent))))
	 (player-money (money (nth (active-player agent) (players (game agent))))))

    (setf (nth (active-player agent) (money-list agent)) how-many)
    (setf (nth (active-player agent) (actions-list agent))'c)
    (push 'c (history (game agent)))


    (decf (money (nth (active-player agent) (players (game agent)))) 
	  ile-zostalo)
    (decf (nth (active-player agent) (balance (game agent)))  ile-zostalo)
    (if (zerop ile-zostalo)
	(setf (nth (active-player agent) (last-actions agent)) 'check)
	(setf (nth (active-player agent) (last-actions agent)) (cons 'called ile-zostalo)))
    (incf (pot agent)  ile-zostalo)

    (when (> ile-zostalo player-money)
	; gracz musi byc all-in
	(decf (nth (active-player agent) (balance (game agent)))
	      player-money)
	(setf (money (nth (active-player agent) (players (game agent)))) 0)
	(setf (nth (active-player agent) (last-actions agent)) (cons 'all-in player-money)))

    (set-next-player agent)))

(defun percept-bet(agent &optional amount)
  (let ((how-many (or amount (if (< (length (table-cards agent)) 4)
				 (small-blind (game agent))
				 (big-blind (game agent)))))
	(real-bet 0))
       
    (setf (stake agent) how-many)
    (setq real-bet (- how-many (nth (active-player agent) (money-list agent))))
    (incf (nth (active-player agent) (money-list agent)) real-bet)
    (setf (nth (active-player agent) (actions-list agent)) (cons 'b how-many))
    (push (cons 'b how-many) (history (game agent)))
    (decf (money (nth (active-player agent) (players (game agent)))) real-bet)
    (decf (nth (active-player agent) (balance (game agent)))  real-bet)
    (if (zerop (money (nth (active-player agent) (players (game agent)))))
	(setf (nth (active-player agent) (last-actions agent)) (cons 'All-In real-bet))
	(setf (nth (active-player agent) (last-actions agent)) (cons 'bet real-bet)))
    (incf (pot agent) 	  real-bet)
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
(defun update-balance!(agent)
  ;agent simulated-game!
  (let ( wnrs how-much )
    (setq wnrs (game-winner agent))
    (setq how-much (/ (pot agent) (length wnrs)))
    (dolist (w wnrs)
      (incf (nth w (balance (game agent))) how-much))))


(defun simulate-game(game-log &optional (update? nil))
  (let ((c (reverse (table-cards game-log)))
	(hist (reverse (history game-log)))
	(a (make-instance 'agent)))

    (percept-new-game-log a game-log)
    (dolist (h hist)
      ;(princ a)
;      (push h (nth (active-player a) (all-actions a))) 
       (cond
	 ((eq h 'F)
	  (push 'F (nth (active-player a) (all-actions a))) 
	  (percept-fold a))
	 ((eq h 'C)
	  (push 'C (nth (active-player a) (all-actions a))) 
	  (percept-check a))
	 ((consp h)
	  (push h (nth (active-player a) (all-actions a))) 
	  (percept-bet a (cdr h)))
	 ((eq h 'FLOP)
	  (dotimes (i (length (players game-log)))
	    (let ((ost (car (nth i (all-actions a)))))
	      (unless (eq 'F ost) (push 'flop (nth i (all-actions a))))))

	  (percept-flop a (nth 0 c) (nth 1 c) (nth 2 c) )  )
	 ((eq h 'TURN)
	  (dotimes (i (length (players game-log)))
	    (let ((ost (car (nth i (all-actions a)))))
	      (unless (eq 'F ost) (push 'turn (nth i (all-actions a))))))
	  (percept-turn a (nth 3 c) )  )
	 ((eq h 'RIVER)
	  (dotimes (i (length (players game-log)))
	    (let ((ost (car (nth i (all-actions a)))))
	      (unless (eq 'F ost) (push 'river (nth i (all-actions a))))))
	  (percept-turn a (nth 4 c)) ))
       )
    (when update? (update-balance! a))
    a))
