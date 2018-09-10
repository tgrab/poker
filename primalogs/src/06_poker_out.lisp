(defun common-percepts-rules(&optional (g *game*))
  ;wywolywane przez action-rules
  (when (null g) (return-from common-percepts-rules "no game!"))
  (setf (percept-memo g) nil)
  (case (bets g)
    (0 (<-percept g BETS-NO))
    (1 (<-percept g BETS-1))
    (2 (<-percept g BETS-2))
    (3 (<-percept g BETS-3)) )
  (when (<= (bets g) 1) (<-percept g BETS<=1))
  (when (plusp (bets g) ) (<-percept g BETS-YES))

  (let ((ile (count-active-players g)))
    (when (= 2 ile) (<-percept g PLAYERS-LEFT-2))
    (when (> 4 ile) (<-percept g PLAYERS-LEFT<=3))
    )

  (awhen (get-agent g) (when (minusp (balance it))  (<-percept g STAKE-COMMITED)))

  (let ((l (active-players-positions g))
	(a-pos (agent-position g)))
    (when (eql a-pos (car (last l)))  (<-percept g ACT-LAST))
    (when (eql a-pos (car l))  (<-percept g ACT-FIRST))

    )

  (when (preflop? g)
    (let ((a-pos (agent-position g)))		    
	 (when (= 0 a-pos) (<-percept g POSITION-SBLIND)  )
	 (when (= 1 a-pos) (<-percept g POSITION-BBLIND)  )

	 (if (>= (table-size g) 6)
	     (progn 
	       ;reguly duzych stolow
	       (when (> a-pos (- (table-size g) 2))  (<-percept g POSITION-LATE)  ) )
	     (progn 
	       ;reguly shorthanda
	       (when (= a-pos (- (table-size g) 1))  (<-percept g POSITION-LATE))))

	 (let ((pfr (predict-pfr g)))
	   (when (< pfr 0.2) (<-percept g PREDICT-PASSIVE))
	   (when (< pfr 0.5) (<-percept g PREDICT-NO-BET))
	   (when (>= pfr 0.5) (<-percept g PREDICT-BET))  )

	 )) ;END preflop?
    

    

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

    (when (preflop? g) (prop-checkall-percepts *o-pf-levels* g))

  ));END omaha?

  


(defun rule-accepted?(r)
  (let ((level (first r)))
    (when (plusp level)
      (if (= 1 level)
	  r
	  (when (> (/ (random 1000) 1000) level) r)))))

(defun descr-result(el)
  (format nil "~A(~A)" (first el) (second el) ))

(defun show-action-rules-result(&optional (g *game*))
  (format nil "[fold:~A][dont-bet:~A][potbet1:~A][potbet2:~A][bet:~A][call:~A]" 
	  (descr-result (? g o-fold))  
	  (descr-result(? g o-dont-bet)) 
	  (descr-result(? g o-potbet1))
	  (descr-result(? g o-potbet2)) 
	  (descr-result(? g o-bet))
	  (descr-result(? g o-call)) ))

(defun action-rules(&optional (g *game*))
; prove zwroci liste 2-el: poziom "nazwa reg"
  (common-percepts-rules g)
  (when   (omaha? g)
    (awhen (rule-accepted? (? g o-fold)) 
	    (return-from action-rules "fold"))
    (awhen (rule-accepted? (? g o-potbet1)) 
	    (return-from action-rules "potbet1"))
    (awhen (rule-accepted? (? g o-potbet2)) 
	    (return-from action-rules "potbet2"))
    (awhen (rule-accepted? (? g o-bet)) 
	    (return-from action-rules "bet"))
    (awhen (rule-accepted? (? g o-call)) 
	   (when (<= (car it) 0.5) (return-from action-rules "fuzzy-call"))
	   (return-from action-rules "call"))
    "fold"   ))


(defun action(&optional (g *game*))
 (let ((act (action-rules g)))
   (when *html-log*
     (push (format nil "<hr><h3>Action:~A</h3><br>" act) (html-log g))
     (push (show-action-rules-result g) (html-log g))
     (push (render-game g)  (html-log g))
     (push "<hr>"  (html-log g)))
   act))
 
;;-------------------------------------------------------------
(defmethod view((p player))
  (format t "<p>")
  (if (active? p) (format t "<font color='green'>") (format t "<font color='yellow'>"))
  (format t "<b>~A</b>@~A<br><i>${~A ~A ~A}</i><br>" 
	  (name p)(seat p) 
	  (money p) (total-balance p) (balance p))
  (if (cards p) (format t "CARDS: ~A<br>" (karty->napisy (cards p)))) 
  (format t "history: ~A<br>" (history p))
  (format t "preflop-bets/~A vol-in-pot/~A flop-bet/~A " 
	  (preflop-bets p)
	  (vol-in-pot p) 
	  (flop-bet p))
  (format t "pfr: ~A" 
	  (pfr p))	 
  (format t "</font>") )

(defmethod view((g game))
  (awhen (rank g) (format t "<br>Rank: ~A" (render-rank it) ))
  (format t "<br>pot: ~A stake: ~A agent-stake: ~A<br>" (pot g) (stake g) (agent-stake g))
  (format t "history: ~A<br>" (history g))
  (format t "GAME-MEMO: ~A<br>" (game-memo g))
  (format t "ROUND-MEMO: ~A<br>" (round-memo g))
  (format t "PERCEPT-MEMO: ~A<br>" (mapcar #'(lambda(p) (cond ((eq p 'bets<=1) "BETS&lt;=1")
							      ((eq p 'players-left<=3) "PLAYERS-LEFT&lt;=3")
							      (t p))) (percept-memo g)))
  (format t "players:")
  (dotimes (i (free-seat g))
    (view (aref (players g) i)))
  (format t "<br>)"))


(defun render-game(g)
  (with-output-to-string (s)
	(let ((*standard-output* s))
	  (view g))))

