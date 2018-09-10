(in-package :poker)


(defun common-percepts-rules(&optional (g *game*))
  (when (null g) (return-from common-percepts-rules "no game!"))
  (setf (percept-memo g) nil)
  (case (bets g)
    (0 (<-percept g BETS-NO))
    (1 (<-percept g BETS-1))
    (2 (<-percept g BETS-2))
    (3 (<-percept g BETS-3)) )

  (when (omaha? g);BEGIN omaha?

    (cond
      ((< (agent-stake g) 0.01)  (<-percept g STAKE-ZERO))		
      ((< (- (agent-stake g) (blind g)) 0.1)  (<-percept g STAKE-MINIMAL))
      ((< (pot-odds g) 2.5)  (<-percept g STAKE-BIG))
      (t  (<-percept g STAKE-NORMAL)))

    (let* ((o1 (omaha-count-outs g))
	   (odds (odds o1 g)))
      (when (plusp o1)
 	(cond  
	  ((> (pot-odds g) odds)   (<-percept g  ODDS-GOOD))
	  ((> (* 1.9 (pot-odds g)) odds) (<-percept g  ODDS-IMPLIED))	  
	  ((>= (* 3 (pot-odds g)) odds) (<-percept g  ODDS-STRONG-IMPLIED)  ))))

  ));END omaha?

  


(defun rule-accepted?(r)
  (let ((level (first r)))
    (when (plusp level)
      (if (= 1 level)
	  r
	  (when (> (/ (random 1000) 1000) level) r)))))

(defun show-action-rules-result(&optional (g *game*))
  (list  (? g o-fold)  (? g o-dont-bet) (? g o-potbet1)
	 (? g o-potbet2) (? g o-bet)  (? g o-call) ))

(defun action-rules(&optional (g *game*))
  (common-percepts-rules g)
  (when   (omaha? g)
    (awhen (rule-accepted? (? g o-fold)) 
	    (return-from action-rules (cons "fold" it)))
    (awhen (rule-accepted? (? g o-dont-bet)) 
	    (return-from action-rules (cons "call" it)))
    (awhen (rule-accepted? (? g o-potbet1)) 
	    (return-from action-rules (cons "potbet1" it)))
    (awhen (rule-accepted? (? g o-potbet2)) 
	    (return-from action-rules (cons "potbet2" it)))
    (awhen (rule-accepted? (? g o-bet)) 
	    (return-from action-rules (cons "bet" it)))
    (awhen (rule-accepted? (? g o-call)) 
	    (return-from action-rules (cons "call" it)))
    (list "fold" 1 'no-rule)  ))


(defun action(&optional (g *game*))
 (let ((act (action-rules g)))
   (first act)))