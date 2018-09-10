(in-package :poker)


(defmethod insert-player(name (g game))
  (assert (< (free-seat g) 10))
  (when (string= name (agent-name g))
    (setf (agent-pos g) (free-seat  g)))
  (setf (aref (players g) (free-seat g))
	(make-instance 'player :name name :seat (free-seat g)  ))
  (incf (free-seat g)))

(defmethod insert-player :after (name (g game-log))
 (let ((db-pl (find name (db-players g) :key #'car :test #'string=)))
   (when db-pl
     (let ((pl (get-player name g)))
       (setf (money pl) (second db-pl))
       (awhen (third db-pl)
	 (setf (cards pl) it)) 
       (when (and (cards g) (string= name (agent-name g)))
	 (setf (cards pl) (cards g)) )))))

(defun insert-player?(name &optional (g *game*))
  (and (null (table-cards g)) ;preflop 
       (not (get-player name g))
       (insert-player name g)))



;url: newgame
;params: id
(defun new-game (&key (id "0") (czas (get-universal-time)) (prev-game nil) (game-type 'game))
  (let ((game
	 (make-instance game-type
		 :id id
		 :table-size (if *game* (table-size *game*) 6)
		 :prev-game prev-game
		 :czas czas )))
    (<-round game PREFLOP)
    (when prev-game
      (setf (profile game) (make-instance 'profile))
      (let ((games (games-history game)))
	(setf (pfr (profile game))
	      (/ (count-if #'(lambda (g) (plusp (preflop-bets (cortex g)))) games)
		 (length games)))
      ))
    game))


;url: smallblind
;params: name,amount
(defun small-blind(name amount &optional (g *game*))				
  (when (null (history g)) ;player wszedl na stol
    (incf (pot g) amount)
    (setf (stake g) amount (blind g) amount)			
    (push (list 0 'SB amount) (history g))
    (insert-player name g)
    (push (list 'SB amount) (history (get-player 0 g)))
    (setf (balance (get-player 0 g)) (- amount))) )

;url: bigblind
;params: name,amount
(defun big-blind(name amount &optional (g *game*))
  (when (= 1 (length (history g))) ;player wszedl na stol
    (incf (pot g) amount)
    (setf (stake g) amount (blind g) amount)
    (push (list 1 'BB amount) (history g))
    (insert-player name g)
    (push (list 'BB amount) (history (get-player 1 g)))
    (setf (balance (get-player 1 g)) (- amount)) ))


;url: call
;params: name,amount
(defun call(name amount &optional (g *game*))
  (unless (null (history g))
    (insert-player? name g)
    (let ((plr (get-player name g)))
      (when plr
      (when (preflop? g)
	(setf (vol-in-pot (cortex plr)) t));end preflop?
      (push (list (seat plr) 'C amount) (history g))
      (push (list 'c amount) (history plr))
      (decf (balance plr) amount)
      (incf (pot g) amount)))))

;url: fold
;params: name
(defun fold(name &optional (g *game*))
  (unless (null (history g))
    (insert-player? name g)
    (let ((plr (get-player name g)))
      (when plr
	(push (list (seat plr) 'F) (history g))
	(push (list 'f) (history plr))
	(setf (active? plr) nil) ))))

;url: bet
;params: name,amount
(defun common-for-bet(name amount kind game)
  (unless (null (history game))
    (insert-player? name gAME)
    (let ((plr (get-player name game)))
      (when plr
      (incf (bets game))
      (when (preflop? game)
	(setf (vol-in-pot (cortex plr)) t)
	(incf (preflop-bets (cortex plr)))
	(setf (preflop-bets (cortex game)) (bets game)));end preflop?
      (setf (stake game) amount)
      (push (list (seat plr) kind amount) (history game))
      (push (list kind amount) (history plr))
      (decf (balance plr) amount)
      (incf (pot game) amount)
;      (setq ratio (/ (get-stake) (blind +game+)))
  ))))

;url: bet
;params: name,amount
(defun bet(name amount &optional (game *game*))
  (common-for-bet name amount 'B game))
      

;url: bet
;params: name,amount
(defun raise(name amount &optional (game *game*))
  (common-for-bet name amount 'R game))


;url: allin
;params: name,amount
(defun allin(name amount &optional (g *game*))
  (unless (null (history g))
    (insert-player? name g)
    (let ((plr (get-player name g)))
      (when plr
	(push (list (seat plr) 'a amount) (history g))
        (push (list 'a amount) (history plr))
	(decf  (balance plr) amount)
	(when (> amount (stake g))
	  (incf (bets g))	 
	  (when (preflop? g)
	    (setf (vol-in-pot (cortex plr)) t)
	    (incf (preflop-bets (cortex plr)))
	    (setf (preflop-bets (cortex g)) (bets g))) ;end preflop
	  (setf (stake g) amount)) ;end really bet
	(setf (active? plr) nil)
	(incf (pot g) amount)
;	(setq ratio (/ (get-stake) (blind +game+)))
))))



;;-------------------------------------------------------------------
(defun wektor-wystepowania(lista ilosc-wartosci)
  (let ((wektor (make-array (1+ ilosc-wartosci) :initial-element 0)))
    (dolist (e lista wektor)
      (incf (aref wektor e)))))

(defun common-round-rules( &optional (kind 'symbol-rundy) (g *game*))
  (setf (round-memo g) nil)
  (dotimes (i (free-seat g))			
    (let ((plr (get-player i g)))
      (incf (total-balance plr) (balance plr))
      (push (list kind) (history plr))
      (setf (balance plr) 0)        ))
  (setf (stake g) 0)
  (setf (bets g) 0)
  (when (null (cards g)) (return-from common-round-rules nil))
  (let* ((tabela-figur (sort (mapcar #'figura (table-cards g)) #'<))
	 (czest-figur (sort (coerce (wektor-wystepowania tabela-figur 13) 'list) #'>))
	 (tabela-kolorow (mapcar #'kolor (table-cards g)))
	 (czest-kolorow (sort (coerce (wektor-wystepowania tabela-kolorow 4) 'list) #'>)))
    (setf (ilosc-figur g) (first czest-figur))
    (setf (ilosc-figur-1 g) (second czest-figur))
    (setf (ilosc-kolorow g) (first czest-kolorow))
    (setf (ilosc-kolorow-1 g) (second czest-kolorow))
    (setf (t1 g) (nth 0 tabela-figur))
    (setf (t2 g) (nth 1 tabela-figur))
    (setf (t3 g) (nth 2 tabela-figur))
    (setf (t4 g) (aif (nth 3 tabela-figur) it 100))
    (setf (t5 g) (aif (nth 4 tabela-figur) it 200))  )

  (if (>= (ilosc-figur g) 2)
      (<-round g TABLE-PAIRS)
      (<-round g TABLE-NO-PAIRS))

  (if (>= (ilosc-figur-1 g) 2)
      (<-round g TABLE-2PAIRS)
      (<-round g TABLE-NO-2PAIRS))

  (if (>= (ilosc-figur g) 3)
      (<-round g TABLE-TRIPS)
      (<-round g TABLE-NO-TRIPS))

  (when (= (ilosc-kolorow g) 1)
    (<-round g TABLE-RAINBOW))

  (when (>= (ilosc-kolorow g) 3)
    (<-round g TABLE-COLORS))

  (when (< (ilosc-kolorow g) 3)
    (<-round g TABLE-NO-COLORS))

  (when (omaha? g) ;BEGIN omaha?
    (when (cards g)
      (setf (rank g) (omaha-ocena (cards g) (table-cards g))))
    (if (two-str8-table? g)
	 (<-round g TABLE-STR8)
	 (<-round g TABLE-NO-STR8))
    (when (one-str8-table? g)
	 (<-round g TABLE-VERY-STR8))

  
    (if (omaha-top-set? g) 
	(<-round g RANK-TOP-SET)
	(when (omaha-set? g)
	  (<-round g RANK-SET)) )


    (when (omaha-top-trips? g) 
      (<-round g RANK-TOP-TRIPS))

    (when (omaha-top-two-pairs? g) 
      (<-round g  RANK-TOP-2PAIRS))

    (when (omaha-overpair? g) 
      (<-round g RANK-OVERPAIR))

    (with-slots (h1 h2 h3 h4 t1) g
      (when (or (= h1 t1) (= h2 t1) (= h3 t1) (= h4 t1) )
	(<-round g RANK-TOP-PAIR))
      (when (< h1 t1)
	(if (= 0 h1)
	    (progn
	      (<-round g RANK-KICKER)
	      (<-round g RANK-ACE-KICKER)) 
	      (<-round g RANK-KICKER)))   )

    (cond
      ((= 3 (car (rank g))) 
       (if  (omaha-color-nuts? g)
	    (<-round g RANK-COLOR-NUTS)
	    (<-round g RANK-COLOR-NOT-NUTS)  ))
      ((= 4 (car (rank g)))
       (if  (omaha-str8-nuts? g)
	    (<-round g RANK-STR8-NUTS)
	    (<-round g RANK-STR8-NOT-NUTS)  ))
      ((= 5 (car (rank g)))
        (if  (omaha-full-nuts? (cards g) (table-cards g))
	    (<-round g RANK-FULLHOUSE-NUTS)
	    (<-round g RANK-FULLHOUSE-NOT-NUTS)  )) )

   ;testujemy nutsa
    (cond
      ((>= (ilosc-figur g) 2)
       (if (omaha-full-nuts? (cards g) (table-cards g))
	    (<-round g RANK-NUTS)
	    (<-round g RANK-NOT-NUTS)  ))
      ((>= (ilosc-kolorow g) 3)
       (if (omaha-color-nuts? g)
	    (<-round g RANK-NUTS)
	    (<-round g RANK-NOT-NUTS) ))
      ((two-str8-table? g)
       (if (omaha-str8-nuts? g)
	    (<-round g RANK-NUTS)
	    (<-round g RANK-NOT-NUTS)  ))
      (t 
       (if (omaha-top-set? g)
	    (<-round g RANK-NUTS)
	    (<-round g RANK-NOT-NUTS) ))      )

    );END omaha?
  (case (car (rank g))
    (8 (<-round g RANK-HIGHCARD))
    (7 (<-round g RANK-1PAIR))
    (6 (<-round g RANK-2PAIRS))
    (5 (<-round g RANK-TRIPS))
    (4 (<-round g RANK-STR8))
    (3 (<-round g RANK-COLOR))
    (2 (<-round g RANK-FULLHOUSE))
    (1 (progn (<-round g RANK-QUADS) 
	      (<-round g RANK-NUTS)))
    (0 (<-round g RANK-STR8FLUSH))     ) )
;;-------------------------------------------------------------------

;url: flop
;params: c1,c2,c3
(defun round-flop(c1 c2 c3 &optional (g *game*))
  (unless (null (history g))
    (setf (table-cards g) (list c1 c2 c3)
	  (table-size g) (free-seat g))
    (push (list 'flop c1 c2 c3) (history g))
    (common-round-rules 'flop g)
    (<-round g FLOP)
    (<-round g PRERIVER)
    (setf (on-flop-players (cortex g)) 
	  (count-if #'(lambda(p) (when p (active? p))) (players g))) ))


;url: turn
;params: c1
(defun round-turn(c1 &optional (g *game*))
 (when (= 3 (length (table-cards g)))
   (push c1 (table-cards g))
   (push (list 'turn c1) (history g))
   (common-round-rules 'turn g)
   (<-round g TURN)
   (<-round g PRERIVER)))

;url: river
;params: c1
(defun round-river(c1 &optional (g *game*))
 (when (= 4 (length (table-cards g)))
   (push c1 (table-cards g))
   (push (list 'river c1) (history g))
   (common-round-rules 'river g)
   (<-round g RIVER)))

;----------------------------------------------------
(defun holecards(game &rest cards)
  (setf (cards game) cards)
  (awhen (get-agent game) (setf (cards it) cards)) 
  (push `(holecards ,@cards) (history game))
  (when (omaha? game) ;BEGIN omaha?
    (let ((tbl (sort (copy-list (cards game)) #'< :key #'figura)) )
      (setf
       (h1 game)  (figura (nth 0 tbl))
       (h2 game)  (figura (nth 1 tbl))
       (h3 game) (figura (nth 2 tbl)) 
       (h4 game) (figura (nth 3 tbl)) 
       (hc1 game) (kolor (nth 0 tbl))
       (hc2 game) (kolor (nth 1 tbl))
       (hc3 game) (kolor (nth 2 tbl)) 
       (hc4 game) (kolor (nth 3 tbl)) ))
    (with-slots (h1 h2 h3 h4 hc1 hc2 hc3 hc4) game

      (when (and (= 0 h1)
		 (or (= hc1 hc2) (= hc1 hc3) (= hc1 hc4)))
	(<-round game HAND-ACE-SUITED))

      (when (or (and (= (+ 1 h1) h2) (< h2 9))
		(and (= (+ 1 h2) h3) (< h3 9))
		(and (= (+ 1 h3) h4) (< h4 9) ))
	(<-round game HAND-CONNECTORS))

      (when (and (< h4 10) (<= (- h4 h1) 4))
	(<-round game HAND-CONNECTED))

      (when (and (= h4 (+ 1 h3) (+ 2 h2) (+ 3 h1)) (< h4 10))
	(<-round game HAND-STR8))

      (when (or (and (= h4 (+ 1 h3) (+ 2 h2))  (< h4 10))
		(and (= h3 (+ 1 h2) (+ 2 h1)) (< h3 10)))
	(<-round game HAND-3-STR8))


      (when (and (or (and (= h1 h2) (< h1 5))
		(and (= h2 h3) (< h2 5))
		(and (= h3 h4) (< h3 5)) )
		(not (= h1 h2 h3)))
	(<-round game HAND-BIG-PAIR))

      (when (or (and (= h1 h2) (>= h1 5) (< h1 8))
		(and (= h2 h3) (>= h2 5) (< h2 8))
		(and (= h3 h4) (>= h3 5) (< h3 8)) )
	(<-round game HAND-MIDDLE-PAIR))

      (if      (and (< (- h2 h1) 4)
		    (< (- h4 h3) 4))
	       (<-round game HAND-NO-DANGLER)
	       (<-round game HAND-DANGLER))

      (when (or (and (= h1 h2) (>= h1 8))
		(and (= h2 h3) (>= h2 8))
		(and (= h3 h4) (>= h3 8)) )
	(<-round game HAND-SMALL-PAIR))

      (when (and (= h1 h2)  (= h3 h4))
	(<-round game HAND-2PAIRS))

	  ))) ;END omaha?
;----------------------------------------------------
