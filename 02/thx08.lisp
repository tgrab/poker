;wersja z autonomicznymi agentami
;--------------- STRUKTURY -------------------------------
(defstruct table
  active-player
  size
  (pot 0)
  stake
  cards
  sblind
  blind
  money
  history
  actions)

(defstruct agent
  position
  cards
  (hs 1000)
  still-playing
  table)

;---------- FUNKCJE POMOCNICZE ------------------------------
(defun next-player(tbl biezacy)
  (when (= biezacy (1- (table-size tbl)))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr (table-actions tbl))
      nr
      (next-player tbl nr)) ))

(defun next-player!(tbl)
  (setf (table-active-player tbl) (next-player tbl (table-active-player tbl))))

(defun game-round(tbl)
  (case (length tbl)
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )

(defun round-history( r h )
  (cond
    ((eq r 'pre-flop)  (if (zerop (length *table*)) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 

;--------- PERCEPTION ----------------------------------------
;wywolane przez new-hand:
(defun new-agent(&key (size 10) (sblind 0.15) (blind 0.25))
  (let ((tbl(make-table
	   :actions (make-list size :initial-element t)
	   :money (make-list size :initial-element 0) 
	   :sblind sblind
	   :blind  blind
	   :stake  blind
	   :size size)))
    (setf (nth 0 (table-money tbl)) sblind)
    (setf (nth 1 (table-money tbl)) blind)
    (setf (table-active-player tbl) (next-player tbl 1))
    (make-agent :table tbl)
))

(defun percept-hand(agent cards)
  (setf (agent-cards agent) cards)
  (setf (agent-still-playing agent) t))

(defun percept-flop(agent cards)
  (let ((tbl (agent-table agent)))
    (setf (table-cards tbl) cards)
    (setf (table-pot tbl) (reduce #'+ (table-money tbl)))
    (setf (table-money tbl) (make-list (table-size tbl) :initial-element 0) )
    (setf (table-stake tbl) 0)
    (push 'flop (table-history tbl))
    (setf (table-active-player tbl) (next-player tbl -1))
    (when (agent-still-playing agent) 
      (setf (agent-hs agent) 
		      (moja-ocena 1 (agent-cards agent) cards))) ) )

(defun percept-turn(agent card)
  (let ((tbl (agent-table agent)))
    (push card (table-cards tbl) )
    (incf (table-pot tbl) (reduce #'+ (table-money tbl)))
    (setf (table-money tbl) (make-list (table-size tbl) :initial-element 0) )
    (setf (table-stake tbl) 0)
    (push 'turn (table-history tbl))
    (setf (table-active-player tbl) (next-player tbl -1))
    (when (agent-still-playing agent) 
      (setf (agent-hs agent) 
		      (moja-ocena 1 (agent-cards agent) (table-cards tbl)))) ) )

(defun percept-river(agent card)
  (let ((tbl (agent-table agent)))
    (push card (table-cards tbl) )
    (incf (table-pot tbl) (reduce #'+ (table-money tbl)))
    (setf (table-money tbl) (make-list (table-size tbl) :initial-element 0) )
    (setf (table-stake tbl) 0)
    (push 'river (table-history tbl))
    (setf (table-active-player tbl) (next-player tbl -1))
    (when (agent-still-playing agent) 
      (setf (agent-hs agent) 
		      (moja-ocena 1 (agent-cards agent) (table-cards tbl)))) ) )

(defun percept-check(agent)
  (let ((tbl (agent-table agent)))
    (setf (nth (table-active-player tbl) (table-money tbl)) (table-stake tbl))
    (setf (nth (table-active-player tbl) (table-actions tbl)) 'c)
    (push 'c (table-history tbl))
    (next-player! tbl)))

(defun percept-bet(agent)
  (let* ((tbl (agent-table agent))
	 (after-flop (= 4 (length (table-cards tbl))))
	 (blind (table-blind tbl))
	 (stake (if after-flop (* 2 blind) blind )))
    (incf (table-stake tbl) stake)
    (setf (nth (table-active-player tbl) (table-money tbl)) stake )
    (setf (nth (table-active-player tbl) (table-actions tbl)) 'b)
    (push 'b (table-history tbl))
    (next-player! tbl)))

(defun percept-fold(agent)
  (let ((tbl (agent-table agent)))
    (setf (nth (table-active-player tbl) (table-actions tbl)) nil)
    (push 'f (table-history tbl))
    (next-player! tbl)))

;;--------------- ACTION ----------------------------

(defun get-action(agt prvr)
  (cond
    ((prove-for-agent agt prvr 'bet) "bet")
    ))

;;------------- Reasoners ---------------------------
(defstruct prover
  rules
  memory)

(defun prove(prover goal)
  (let ((chosen-rule 0))
    (dolist (r (prover-rules prover))
      (when (and (eq goal (car r))(prove-all prover (cdr r)))
	(return-from prove chosen-rule))
      (incf chosen-rule))))

(defun prove-all(prover goals)
  (every 
   #'(lambda (g)
       (if (consp g) (eval g) (fact? prover g)))
   goals))

(defun fact?(prover f)
 (member f (prover-memory prover)))



;--- end of reasoners ---------
;------- PREDYKATY ------------------------

(defvar +facts-defs+ nil)
(defvar +rules1+ nil)
(let (agent hand ocena table-cards hs history)



  (defun get-state()
    (list agent hand table-cards))

  (defun set-agent(ag)
    (let ((tbl (agent-table ag)))
    (setq agent ag hand (agent-cards agent))
    (setq table-cards (table-cards tbl))
    (setq history (table-history tbl))
    (setq ocena (ocena (append hand table-cards)))
    (setq hs (agent-hs ag))
    ))


  (defun prove-for-agent(ag prover goal)
    (set-agent ag)
    (setf (prover-memory prover) nil)
    (dolist (f +facts-defs+)
      (when (prove-all prover (cdr f))
	(pushnew (car f) (prover-memory prover))))
    (prove prover goal))

  (defun hs()
    hs)

  (defun stol()
    table-cards)

  (defun count-bets()
    (count 'b (round-history table-cards history)))


   );---end of let......

(setq +facts-defs+
      '((pre-flop (null (stol)))
	(post-flop (not (null (stol))))
	(flop (= 3 (length (stol))))
	(turn (= 4 (length (stol))))
	(river (= 5 (length (stol))))
	(after-flop (> (length (stol)) 3))
	(before-river (< (length (stol)) 5))

	(no-bets (zerop (count-bets)))

	))



(setq +rules1+
 '((check no-bets post-flop)
   (bet (< (hs) 0.1))
 ));-- end of +rules1+