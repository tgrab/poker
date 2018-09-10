;;(BETS ) (ROUND )
(defun rules-new-game()
  (clear-game-memory)
  (<=into (percept-memo +game+) (bets 0))  
  (<=into (round-memo +game+) (round pre-flop)) )



;;(BETS ) (RANK ) (TABLE-RANK ) (TABLE-COLOR ) (LAST-ROUND )
(defun rules-round-common()

  (<=into (percept-memo +game+) (bets 0)) 

  ;Holdem Fixed
  (when (?- (game-kind fixed))
    (let ((o (ocena (append (cards +game+) (table-cards +game+))))) 
	(case (car o)
	  (8 (<-into (round-memo +game+) (rank highcard)))
	  (7 (<-into (round-memo +game+) (rank one-pair)))
	  (6 (<-into (round-memo +game+) (rank two-pairs)))
	  (5 (<-into (round-memo +game+) (rank trips)))
	  (4 (<-into (round-memo +game+) (rank str8)))
	  (3 (<-into (round-memo +game+) (rank color)))
	  (2 (<-into (round-memo +game+) (rank fullhouse)))
	  (1 (<-into (round-memo +game+) (rank quads)))
	  (0 (<-into (round-memo +game+) (rank str8flush)))

	  ))
    (let* ((tbl (sort (copy-list (table-cards +game+)) #'< :key #'figura))
	   (t1 (figura (nth 0 tbl)))
 	   (t2 (figura (nth 1 tbl)))
	   (t3 (figura (nth 2 tbl)))
	   (t4 (aif 
		(nth 3 tbl) 
		(figura it) 100))
	   (t5 (aif 
		(nth 3 tbl) 
		(figura it) 200))
	   (hn (sort (copy-list (cards +game+)) #'< :key #'figura))
	   (h1 (figura (nth 0 hn))) 	   (h2 (figura (nth 1 hn)))  )
      (declare (ignore t2 t3 t4 t5))
      (when (or (= h1 t1) (= h2 t1)) (<-into (round-memo +game+) (rank top-pair)))
      (when (and (= h1 h2) (< h1 t1)) (<-into (round-memo +game+) (rank overpair)))

      )  );Holdem Fixed End

    ;figury na stole
    (let* ((tbl (sort (copy-list (table-cards +game+)) #'< :key #'figura))
	   (t1 (figura (nth 0 tbl)))
 	   (t2 (figura (nth 1 tbl)))
	   (t3 (figura (nth 2 tbl)))
	   (t4 (aif 
		(nth 3 tbl) 
		(figura it) 100))
	   (t5 (aif 
		(nth 3 tbl) 
		(figura it) 200))
	   (w (apply #'max (coerce (wektor-figur (table-cards +game+)) 'list))))
      (declare (ignore t1 t2 t3 t4 t5))
      (when (= w 1) (<-into (round-memo +game+) (table-rank no-pair)))
      (when (= w 2) (<-into (round-memo +game+) (table-rank one-pair)))
      (when (= w 3) (<-into (round-memo +game+) (table-rank trips)))  )  

  ;kolory na stole
  (let ((w (apply #'max (coerce (wektor-kolorow (table-cards +game+)) 'list))))
    (add-clause  (round-memo +game+) `((table-color ,w )) )
    (case w
      (1 (progn (<-into (round-memo +game+) (table-color rainbow)) 
		(<-into (round-memo +game+) (table-color no-color))))
      (2 (<-into (round-memo +game+) (table-color no-color)))
      (t (<-into (round-memo +game+) (table-color color))) ))    
    (set-clause (game-memo +game+) `((last-round ,@(stos-akcji))))
    )

(defun rules-flop()
  (clear-round-memory)
  (<=into (round-memo +game+) (round flop))
  (rules-round-common))

(defun rules-turn()
  (clear-round-memory)
  (<=into (round-memo +game+) (round turn))
  (rules-round-common))

(defun rules-river()
  (clear-round-memory)
  (<=into (round-memo +game+) (round river))
  (rules-round-common))

;;(SKLANSKY ) (HAND )
(defun rules-holecards()
  (when (?- (game-kind fixed))
    (let ((skl (sklansky-group (first (cards +game+)) (second (cards +game+)))))
      (if skl
	  (set-clause (round-memo +game+) `((sklansky ,skl)))  )


     )) ;Holdem Fixed End
    )


;TODO- table-size

;;(THIS-ROUND ) (PLAYERS-LEFT ) (BETS )
(defun rules-percept-memo()

  ;okreslamy (STAKE )
  (remhash 'stake (percept-memo +game+))
  (awhen (agent-pos +game+)
    (if  (minusp (aref (balance +game+) it))
       (<-into (percept-memo +game+) (stake commited))
       (<-into (percept-memo +game+) (stake not-commited)))    )
  (cond
    ((zerop (get-stake))  (<-into (percept-memo +game+) (stake zero)))
    ((= (blind +game+) (get-stake))  (<-into (percept-memo +game+) (stake minimal)))  
    ((< (pot-odds) 2.5)  (<-into (percept-memo +game+) (stake big)))
    (t  (<-into (percept-memo +game+) (stake normal)))  )

  (set-clause (percept-memo +game+) `((players-left ,(count-if #'identity (coerce (active-players +game+) 'list)))))
  (set-clause (percept-memo +game+) `((this-round ,@(stos-akcji (game-round)))))
  (set-clause (percept-memo +game+) `((bets ,(bets)))) )


;;(POSITION )
(defun rules-action-prove()
  (unless (?- (position ?x))
    ;TODO:
    (let ((p (agent-pos +game+)))
      (cond
	((= p 0)   (<-into (game-memo +game+) (position s-blind)))
	((= p 1)   (<-into (game-memo +game+) (position b-blind)))
	((< p 4)   (<-into (game-memo +game+) (position early)))
	((< p 7)   (<-into (game-memo +game+) (position middle)))
	(t   (<-into (game-memo +game+) (position late)))  )) )

  (rules-percept-memo)
  )