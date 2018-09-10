;(declaim (optimize (speed 3) (safety 0)))

(defpackage :poker.engine
  (:use :cl :anaphor :poker.defs :poker.rulebased)
  (:export :percept-new-game :percept-hand :percept-fold
	   :percept-check :percept-bet :percept-flop 
	   :percept-turn :percept-river :percept-get-action) )

(in-package :poker.engine)


(defun next-player(agent biezacy)
  (when (= biezacy (1- (table-size agent)))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr (actions-list agent))
      nr
      (next-player agent nr)) ))

(defmacro set-next-player(a)
  `(setf (active-player ,a)
    (next-player ,a (active-player ,a))))

(defmacro percept-round(agent round)
  `(progn 
    (setf (money-list ,agent) (make-list (table-size ,agent) :initial-element 0) )
    (setf (stake ,agent) 0)
    (push ,round (history (game ,agent)))
    (setf (active-player ,agent) (next-player ,agent -1))))

;------- percepts from environment ----------------------------------
(defun percept-new-game( agent &key (table-size 10) (sblind 0.5) (bblind 1.0))
  (setf (game agent) 
	(make-instance 'game :size table-size :sblind sblind :bblind bblind)

	(actions-list agent) (make-list table-size :initial-element t)
	(money-list agent)   (make-list table-size :initial-element 0)
	(stake agent) bblind)
    (setf (active-player agent) (next-player agent 1))
    (setf (nth 0 (money-list agent)) sblind)
    (setf (nth 1 (money-list agent)) bblind)  )


(defun percept-hand(agent &rest cards)
  (setf (cards agent) cards
	(still-playing? agent) t))

(defun percept-fold(agent)
  (setf (nth (active-player agent) (actions-list agent)) nil)
    (push 'f (history (game agent)))
    (set-next-player agent))
 
(defun percept-check(agent &optional amount)
  (let ((how-many (or amount (stake agent))))
    (setf (nth (active-player agent) (money-list agent)) how-many)
    (setf (nth (active-player agent) (actions-list agent)) (cons 'c how-many))
    (push (cons 'c how-many) (history (game agent)))
    (set-next-player agent)))

(defun percept-bet(agent &optional amount)
  (let ((how-many (or amount (if (< (length (table-cards agent)) 4)
				 (small-blind (game agent))
				 (big-blind (game agent))))))
    (incf (stake agent) how-many)
    (setf (nth (active-player agent) (money-list agent)) how-many)
    (setf (nth (active-player agent) (actions-list agent)) (cons 'b how-many))
    (push (cons 'b how-many) (history (game agent)))
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