(defun set-table-size(&optional (ilosc (table-size +game+)))
  (cond
    ((= ilosc 2) (<=into (memo +game+) (table-size heads-up)))
    ((> ilosc 6) (<=into (memo +game+) (table-size full)))
    (t (<=into (memo +game+) (table-size shorthand)))))
 

;;(BETS ) (ROUND )
(defun rules-new-game()
  (clear-game-memory)
  (<=into (percept-memo +game+) (bets 0))   )


;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
(defun cons-symb(string1 symbol2)
 (intern (concatenate 'string string1 (symbol-name symbol2))))

(defun rules-round-common(&optional which-round)

  (add-clause  (round-memo +game+) `((table-colors ,table-colors )) )
  (add-clause  (round-memo +game+) `((table-pairs ,table-pairs )) )

  (if (> table-pairs 1)
      (<-into  (round-memo +game+) (table-pairs yes))
      (<-into  (round-memo +game+) (table-pairs no)))

  (if (> table-colors 2)
      (<-into  (round-memo +game+) (table-colors yes))
      (<-into  (round-memo +game+) (table-colors no)))

    (if (two-str8-table?)
	(progn
	  (<-into  (round-memo +game+) (table-str8 yes))
	  (if (one-str8-table?)
	      (<-into  (round-memo +game+) (table-str8 1))
	      (<-into  (round-memo +game+) (table-str8 2))))
	(<-into  (round-memo +game+) (table-str8 no)))

    (case (car *o*)
      (8 (progn
	   (add-clause (game-memo +game+) `((,(cons-symb "ON-" which-round ) rank highcard)))
	   (<-into (round-memo +game+) (rank highcard)))  )
      (7 (progn
	   (add-clause (game-memo +game+) `((,(cons-symb "ON-" which-round) rank one-pair)))
	   (<-into (round-memo +game+) (rank one-pair))))
      (6 (progn
	   (add-clause (game-memo +game+) `((,(cons-symb "ON-" which-round) rank two-pairs)))
	   (<-into (round-memo +game+) (rank two-pairs))))
      (5 (progn
	   (add-clause (game-memo +game+) `((,(cons-symb "ON-" which-round) rank trips)))
	   (<-into (round-memo +game+) (rank trips))))
      (4 (progn
	   (add-clause (game-memo +game+) `((,(cons-symb "ON-" which-round) rank str8)))
	   (<-into (round-memo +game+) (rank str8))))
      (3 (progn
	   (add-clause (game-memo +game+) `((,(cons-symb "ON-" which-round) rank color)))
	   (<-into (round-memo +game+) (rank color))))
      (2 (<-into (round-memo +game+) (rank fullhouse)))
      (1 (progn (<-into (round-memo +game+) (rank nuts))
		(<-into (round-memo +game+) (rank quads))))
      (0 (progn (<-into (round-memo +game+) (rank nuts))
		(<-into (round-memo +game+) (rank str8flush)))))

    (if (omaha?)
	(omaha-round-logic)
	(holdem-round-logic))
    

    (if (?- (table-safe yes))
	(<-into (round-memo +game+) (table-safe yes))
	(if (?- (table-safe almost))
	    (<-into (round-memo +game+) (table-safe almost))
	    (<-into (round-memo +game+) (table-safe no))))

    (<=into (percept-memo +game+) (bets 0))   )


;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------

;; analiza pozostalych przeciwnikow na stole:
(defun analyze-opponents()
  (dolist (name (players-list 1))
    (let ((idx (player-pos name)))
      ;mamy gracza ktory gra
      (when (and idx (maniac? name) (eq 'r (last-action :idx idx)))
	(<-into (percept-memo +game+) (comments maniac-bets))
	)
      ;gracze ktorzy jeszcze nie weszli do gry:
      (when (and (null idx) (maniac? name))
	(<-into (percept-memo +game+) (comments before-maniac)))
      )))



(defun make-comments(&optional (r (game-round)))

  (remhash 'comments (percept-memo +game+))
  (when (check-raise? r)
    (<-into (percept-memo +game+) (comments check-raise))  )

  (let ((b (bets-history r))
	(a (agent-pos +game+)))
    (when a
      (when (first-to-act? a)  (<-into (percept-memo +game+) (comments first-to-act)))
      (when (last-to-act? a)  (<-into (percept-memo +game+) (comments last-to-act)))
      (when b 
	(when (> a (car b))  (<-into (percept-memo +game+) (comments agent-behind-bettor)))
	(when (< a (car b))  (<-into (percept-memo +game+) (comments bettor-behind-agent)))   ))

    (when *games-list* (analyze-opponents))))


(defun rules-flop()
  (set-table-size)
  (remhash 'table-style (memo +game+))
;  (let* ((o (ocena-stolu))
;	 (o1 (first o))
;	 (o2 (second o)) )
    ;dla shorthanda:
;    (when (<= o1 0.25)
;      (<-into (memo +game+) (table-style passive)))
;    (when (> o1 0.7)
;      (<-into (memo +game+) (table-style agressive)))
;    (when (<= o2 2.5)
;      (<-into (memo +game+) (table-style tight)))
;    (when (> o1 3.5)
;      (<-into (memo +game+) (table-style loose)))    )
  (make-comments 'pre-flop)
  (when (?- (comments check-raise)) (<-into (game-memo +game+) (on-pre-flop check-raise)))
  (when (?- (comments bettor-behind-agent)) (<-into (game-memo +game+) (on-pre-flop bettor-behind-agent)))
  (when (?- (comments agent-behind-bettor)) (<-into (game-memo +game+) (on-pre-flop agent-behind-bettor)))
  (add-clause (game-memo +game+) `((on-pre-flop bets ,(bets 'pre-flop) )))
  (clear-round-memory)
  (rules-round-common 'flop))

(defun rules-turn()
  (make-comments 'flop)
  (when (?- (rank nuts)) (<-into (game-memo +game+) (on-flop nuts)))
  (when (?- (comments check-raise)) (<-into (game-memo +game+) (on-flop check-raise)))
  (when (?- (comments bettor-behind-agent)) (<-into (game-memo +game+) (on-flop bettor-behind-agent)))
  (when (?- (comments agent-behind-bettor)) (<-into (game-memo +game+) (on-flop agent-behind-bettor)))
  (add-clause (game-memo +game+) `((on-flop bets ,(bets 'flop) )))
  (clear-round-memory)
  (rules-round-common 'turn))

(defun rules-river()
  (make-comments 'turn)
  (when (?- (rank nuts)) (<-into (game-memo +game+) (on-turn nuts)))
  (when (?- (comments check-raise)) (<-into (game-memo +game+) (on-turn check-raise)))
  (when (?- (comments bettor-behind-agent)) (<-into (game-memo +game+) (on-turn bettor-behind-agent)))
  (when (?- (comments agent-behind-bettor)) (<-into (game-memo +game+) (on-turn agent-behind-bettor)))
  (add-clause (game-memo +game+) `((on-turn bets ,(bets 'turn) )))
  (clear-round-memory)
  (rules-round-common 'river))


;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;(SKLANSKY ) (HAND )
(defun rules-holecards()
  (when (omaha?)

      (when (and (= 0 h1)
		 (or (= hc1 hc2) (= hc1 hc3) (= hc1 hc4)))
	(<-into (round-memo +game+) (hand ace-suited)))

      (when (or (and (= (+ 1 h1) h2) (< h2 9))
		(and (= (+ 1 h2) h3) (< h3 9))
		(and (= (+ 1 h3) h4) (< h4 9) ))
	(<-into (round-memo +game+) (hand connectors)))

      (when (and (< h4 10) (<= (- h4 h1) 4))
	(<-into (round-memo +game+) (hand connected)))

      (when (and (= h4 (+ 1 h3) (+ 2 h2) (+ 3 h1)) (< h4 10))
	(<-into (round-memo +game+) (hand str8)))

      (when (or (and (= h4 (+ 1 h3) (+ 2 h2))  (< h4 10))
		(and (= h3 (+ 1 h2) (+ 2 h1)) (< h3 10)))
	(<-into (round-memo +game+) (hand 3-str8)))

      (when (and (or (and (= h1 h2) (< h1 5))
		(and (= h2 h3) (< h2 5))
		(and (= h3 h4) (< h3 5)) )
		(not (= h1 h2 h3)))
	(<-into (round-memo +game+) (hand big-pair)))

      (when (or (and (= h1 h2) (>= h1 5) (< h1 8))
		(and (= h2 h3) (>= h2 5) (< h2 8))
		(and (= h3 h4) (>= h3 5) (< h3 8)) )
	(<-into (round-memo +game+) (hand middle-pair)))

      (if
       (and (< (- h2 h1) 4)
	    (< (- h4 h3) 4))
       (<-into (round-memo +game+) (hand no-dangler)) )
;       (<-into (round-memo +game+) (hand dangler)))


      (when (or (and (= h1 h2) (>= h1 8))
		(and (= h2 h3) (>= h2 8))
		(and (= h3 h4) (>= h3 8)) )
	(<-into (round-memo +game+) (hand small-pair)))

      (when (and (= h1 h2)  (= h3 h4))
	(<-into (round-memo +game+) (hand two-pairs)))

      )				;Omaha End
  ;-----------------------------------------------------------------------------
  (when (fixed?)
    (add-clause (game-memo +game+) `((hand ,(nth h1 *figury*) ,(nth h2 *figury*) ,(if (= hc1 hc2) 's 'o))))

    (let ((skl (sklansky-group (first (cards +game+)) (second (cards +game+)))))
      (if skl
	  (set-clause (round-memo +game+) `((sklansky ,skl)))  )
     ))				;Holdem Fixed End
  )


;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
;;(THIS-ROUND ) (PLAYERS-LEFT ) (BETS )
(defun rules-percept-memo()

					;okreslamy (STAKE )
  (remhash 'stake (percept-memo +game+))
  (remhash 'outs (percept-memo +game+))
  (remhash 'odds (percept-memo +game+))
  (remhash 'comments (percept-memo +game+))
  (let ((blind (if (and (fixed?) 
			(or (turn?)(river?)))
		   (* 2 (blind +game+))
		   (blind +game+))))
    (cond
      ((< (get-stake) 0.01)  (<-into (percept-memo +game+) (stake zero)))		
      ((< (- (get-stake) blind) 0.1)  (<-into (percept-memo +game+) (stake minimal)))
      ((< (pot-odds) 2.5)  (<-into (percept-memo +game+) (stake big)))
      (t  (<-into (percept-memo +game+) (stake normal))))   )



  (awhen (agent-pos +game+)
	 (if  (minusp (aref (balance +game+) it))
	      (<-into (percept-memo +game+) (stake commited))
	      (<-into (percept-memo +game+) (stake not-commited)))    )



  (when (or (flop?) (turn?))
    (when (omaha?)
      (omaha-outs-logic))

    (when (fixed?)
      (when (table-cards +game+)
	(let ((outs (holdem-count-outs)))
	  (when (plusp outs)
	    (cond  
	      ((> (pot-odds) (/ (- 50 outs (length (table-cards +game+))) outs))
	       (<-into (percept-memo +game+)  (odds good)))
	      ((> (+ (* 2 (blind +game+)) (pot-odds)) (/ (- 50 outs (length (table-cards +game+))) outs)) ;zle
	       (<-into (percept-memo +game+)  (odds implied)))  )))) )    )

  (make-comments)
  (setq players-left (count-active-players))
  (set-clause (percept-memo +game+) `((players-left ,players-left )))
  (set-clause (percept-memo +game+) `((bets ,bets)) )   )



;liczone gdy nie bylo podbicia
;sumujemy i odejmujemy graczy ktorzy juz zagrali
(defun safe-pfr-short(name)
  (if name
  (aif (get-profile-from-cortex name)
       (pfr-short it)
       0)
  0))


(defun count-game-pfr()
  (let* ((plrs  (mapcar #'car (reverse (name->seat +game+)) ))
	 (sb (nth 0 plrs))
	 (bb (nth 1 plrs))
	 (nonb (cddr plrs)))
  (- (reduce #'+ (mapcar #'pfr-short *cortex*))
     (reduce #'+
	  (mapcar #'safe-pfr-short
		  nonb))
     (* 0.5 (safe-pfr-short sb))
     (* 0.5 (safe-pfr-short bb))
     (safe-pfr-short (agent-name +game+))) )    )


;;(POSITION )
(defun rules-action-prove()
  (unless (?- (position ?x))
    (let ((p (agent-pos +game+))
	  (s (table-size +game+)))
      (cond
	((= p 0)   (progn (<=into (game-memo +game+) (position s-blind)) 
			  (<-into (game-memo +game+) (position blind))))
	((= p 1)   (progn (<=into (game-memo +game+) (position b-blind))
			  (<-into (game-memo +game+) (position blind))))
	;ostatnie dwie pozycje:
	((>= p (- s 2))   (<=into (game-memo +game+) (position late)))
	((and (<= s 6) (= p 2)) (<=into (game-memo +game+) (position early)) )
	((and (> s 6) (<= p 4)) (<=into (game-memo +game+) (position early)) )
	(t   (<=into (game-memo +game+) (position middle))))))
  (rules-percept-memo)
  (when (preflop?)
    (when (>= bets 1)
      (set-clause (percept-memo +game+) `((raised-by ,(describe-pfr (pfr (get-profile-from-cortex (player-at (car (bets-history)))))))))
      )
    (when (zerop bets)
      (let ((raise-prob (count-game-pfr)))
	(when  (<= raise-prob 0.1)
	  (<-into (percept-memo +game+) (table-style passive))  )
	(when  (< raise-prob 0.5)
	   (<-into (percept-memo +game+) (table-style not-agressive))  )
		 (when  (>= raise-prob 0.5)
		   (<-into (percept-memo +game+) (table-style agressive))  ) )) )    )