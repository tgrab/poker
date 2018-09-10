(in-package :poker)



;;-- PERCEPTS-- game-kind,...

;url: newgame
(defun new-game(&key (id "0") (czas (get-universal-time)) (with-rules t))

  ;zerujemy wszystko:
  (setf +game+ 
	(make-instance 'game
		       :id id
		       :czas czas ))
;  (set-table-size)
  (setq bets 0 ratio 1)
  (when with-rules
    (rules-new-game)
    ;(setq *cortex* (make-game-profiles))
    )
  'ok)




;url: smallblind
;params: name,amount
(defun small-blind(name amount)				
  (when (null (history +game+)) ;player wszedl na stol
    (incf (pot +game+) amount)
    (setf (stake +game+) amount (blind +game+) amount)
    (push (cons 'sb amount) (history +game+))				
    (push (list 0 'sb amount) (history-log +game+))
    (setf (aref (balance +game+) 0) (- amount))
    (insert-player name)))

;url: bigblind
;params: name,amount
(defun big-blind(name amount)
  (let ((hist (history +game+)))
	(when (and hist (= 1 (length hist))) ;player wszedl na stol
	  (incf (pot +game+) amount)
	  (setf (stake +game+) amount (blind +game+) amount)
	  (push (cons 'bb amount) (history +game+))
	  (push (list 1 'bb amount) (history-log +game+))
	  (setf (aref (balance +game+) 1) (- amount))
	  (insert-player name))))

;url: call
;params: name,amount
(defun call(name amount)
  (unless (null (history +game+))
    (insert-player? name)
    (let ((nr (player-pos name)))
      (when nr
	(push (cons 'c amount) (history +game+))
	(push (list nr 'c amount) (history-log +game+))
	(if (fixed?)
	    (progn
	      (let ((ile (player-stake name)))
	      (decf (aref (balance +game+) nr) ile)
	      (incf (pot +game+) ile)))	    
	    (progn
	      (decf (aref (balance +game+) nr) amount)
	      (incf (pot +game+) amount)))  ))))


;url: fold
;params: name
(defun fold(name)
  (unless (null (history +game+))
    (insert-player? name)
    (let ((nr (player-pos name)))
      (when nr
	(push 'f (history +game+))
	(push (list nr 'f) (history-log +game+))
	(setf (aref (active-players +game+) nr) nil) ))))

;url: bet
;params: name,amount
(defun bet(name amount &optional (smb 'b))
  (unless (null (history +game+))
    (insert-player? name)
    (let ((nr (player-pos name)))
      (when nr
      (incf bets)
      (when (preflop?)
	(setf (pre-flop-bets +game+) bets))
	(if (fixed?)
	    (let ((st (player-stake name)))
	      (if (or (preflop?) (flop?))
		  (setq amount (blind +game+))
		  (setq amount (* 2 (blind +game+))))
	      (incf (stake +game+) amount)
	      (push (cons smb amount) (history +game+))
	      (push (list nr smb amount) (history-log +game+))
	      (decf (aref (balance +game+) nr) (+ st amount))
	      (incf (pot +game+) (+ st amount)))
	    (progn
	      (setf (stake +game+) amount)
	      (push (cons smb amount) (history +game+))
	      (push (list nr smb amount) (history-log +game+))
	      (decf (aref (balance +game+) nr) amount)
	      (incf (pot +game+) amount)
	      (setq ratio (/ (get-stake) (blind +game+)))  )))
      )))

;url: allin
;params: name,amount
(defun allin(name amount)
  (unless (null (history +game+))
    (insert-player? name)
    (let ((nr (player-pos name)))
      (when nr
	(push (cons 'a amount) (history +game+))
	(push (list nr 'a amount) (history-log +game+))
	(decf (aref (balance +game+) nr) amount)
	(when (> amount (stake +game+))
	  (incf bets)
	  (when (preflop?)
	    (setf (pre-flop-bets +game+) bets)) 
	  (setf (stake +game+) amount))
	(setf (aref (active-players +game+) nr) nil)
	(incf (pot +game+) amount)
	(setq ratio (/ (get-stake) (blind +game+)))))
    ))

(defun update-globals!(&optional (stol (table-cards +game+)))
  (let ((TBL (sort (copy-list stol) #'< :key #'figura))
	(tbl2 (sort (coerce (wektor-figur stol) 'list) #'>)) )
    (setq
	 T1 (figura (nth 0 tbl))
	 T2 (figura (nth 1 tbl))
	 T3 (figura (nth 2 tbl))
	 T4 (aif 
	      (nth 3 tbl) 
	      (figura it) 100)
	 T5 (aif 
	      (nth 4 tbl) 
	      (figura it) 200)
	 TC1 (kolor (nth 0 tbl))
	 TC2 (kolor (nth 1 tbl))
	 TC3 (kolor (nth 2 tbl))
	 TC4 (aif 
	      (nth 3 tbl) 
	      (kolor it) 10)
	 TC5 (aif 
	      (nth 4 tbl) 
	      (kolor it) 20)
	 bets 0
	 ratio 1
	 table-colors (apply #'max (coerce (wektor-kolorow stol) 'list)) 
	 table-pairs (first tbl2)
	 table-pairs2 (second tbl2))
    (when (flop?)
      (setq flop1 t1 flop2 t2 flop3 t3 flopc1 tc1 flopc2 tc2 flopc3 tc3))
    (when (cards +game+)
      (if (holdem?)
	(setq *o* (ocena (append (cards +game+) (table-cards +game+))))
	(setq *o* (omaha-ocena (cards +game+) (table-cards +game+)))))
    (setq rank (- 8 (car *o*)))))

;url: flop
;params: c1,c2,c3
(defun round-flop(c1 c2 c3  &key (with-rules t))
  (unless (null (history +game+))
    (setf (table-cards +game+) (list c1 c2 c3)
	  (table-size +game+) (length (name->seat +game+)) (stake +game+) 0)
    (push 'flop (history +game+))
    (push 'flop (history-log +game+))
    (dotimes (i 10)				
      (incf (aref (total-balance +game+) i) (aref (balance +game+) i))
      (setf (aref (balance +game+) i) 0))
    (update-globals!)
    (setf (on-flop-players +game+) (count-active-players))
    (when (and with-rules (cards +game+)) (rules-flop)) ))

;url: turn
;params: c1
(defun round-turn(c1 &key (with-rules t))
 (when (= 3 (length (table-cards +game+)))
  (setf (stake +game+) 0)
  (push c1 (table-cards +game+))
  (push 'turn (history +game+))
  (push 'turn (history-log +game+))
 (dotimes (i 10)
   (incf (aref (total-balance +game+) i) (aref (balance +game+) i))
   (setf (aref (balance +game+) i) 0))
 (update-globals!)
 (setq turn1 (figura c1) turnc1 (kolor c1))
 (when (and with-rules (cards +game+)) (rules-turn))))

;url: river
;params: c1
(defun round-river(c1  &key (with-rules t))
 (when (= 4 (length (table-cards +game+)))
  (setf (stake +game+) 0)
  (push c1 (table-cards +game+))
  (push 'river (history +game+))
  (push 'river (history-log +game+))
 (dotimes (i 10)
   (incf (aref (total-balance +game+) i) (aref (balance +game+) i))
   (setf (aref (balance +game+) i) 0))   
 (update-globals!)
 (setq river1 (figura c1) riverc1 (kolor c1))
 (when (and with-rules (cards +game+)) (rules-river))))


;TODO nowa zmienna
(defun holecards(c1 c2 &optional c3 c4  (with-rules t))
  (if (and c3 c4)
      (progn 
	(setf (cards +game+) (list c1 c2 c3 c4)) )
      (progn
	(setf (cards +game+) (list c1 c2))  ))
  (let ((TBL (sort (copy-list (cards +game+)) #'< :key #'figura)) )
    (setq
     H1  (figura (nth 0 tbl))
     H2  (figura (nth 1 tbl))
     H3  (aif  (nth 2 tbl)  (figura it) 100)
     H4  (aif  (nth 3 tbl)  (figura it) 200)
     HC1 (kolor (nth 0 tbl))
     HC2 (kolor (nth 1 tbl))
     HC3 (aif (nth 2 tbl) (kolor it) 10)
     HC4 (aif (nth 3 tbl) (kolor it) 20) ))
  (when with-rules (rules-holecards)))

;;-------------------------------------------------------------------------------

(defun choose-startgame(&key nr id)
  (let ((games *games-list*))
  (when id
    (setq nr (position id games :test #'string= :key #'id)))
  (setq *log* (make-logs
	       :game (nth nr games)
	       :game-nr nr ))    ))


(defun percept-executor(p name->seat table-cards &optional (rules nil) )
	(if (consp p)
	    (cond
	      ((eq (second p) 'SB)  (small-blind (player-at-seat (first p) name->seat) (third p) ))
	      ((eq (second p) 'BB)  (big-blind (player-at-seat (first p) name->seat ) (third p) )  )
	      ((eq (second p) 'F)  (fold (player-at-seat (first p)  name->seat ))  )
	      ((eq (second p) 'C)  (call (player-at-seat (first p)  name->seat) (third p) ))  
	      ((eq (second p) 'B)  (bet (player-at-seat (first p)  name->seat) (third p) )  )
	      ((eq (second p) 'R)  (bet (player-at-seat (first p)  name->seat) (third p) 'R )  )
	      ((eq (second p) 'A)  (allin (player-at-seat (first p)  name->seat) (third p) )  )  )
	    (let ((karty (reverse table-cards)))
	    (cond
	      ((eq p 'FLOP)  (round-flop (nth 0 karty ) 
				   (nth 1 karty )
				   (nth 2 karty)) :with-rules rules)
	      ((eq p 'TURN)  (round-turn (nth 3 karty)  :with-rules rules)) 
	      ((eq p 'RIVER)  (round-river (nth 4 karty)  :with-rules rules))    ))))





(defun go-to-percept(id p-nr)
  (choose-startgame :id id)
  (dotimes (i (1+ p-nr))
    (next-percept)))

(defun next-percept()
  ;TODO ustawic kind
  (when (= -1 (logs-percept-nr *log*))
    (let ((g (logs-game *log*)))
      (new-game :id (id g) :czas (czas g))
      (setf (agent-name +game+) (agent-name g))
      (setf (table-name +game+) (table-name g))
      (setf (agent-pos +game+) (agent-pos g))
      (setf (money-log +game+) (money-log g))
      (awhen (cards g) (holecards (nth 0 it) (nth 1 it) (nth 2 it) (nth 3 it)))
      ))
     
  (when (<= (logs-percept-nr *log*)  (length  (history-log (logs-game *log*))))
    (incf (logs-percept-nr *log*))
    (percept-executor (nth (logs-percept-nr *log*) (reverse (history-log (logs-game *log*))))
		      (name->seat (logs-game *log*)) 
		      (table-cards (logs-game *log*))
		      t)))


(defmacro simulate-game(id &body body)
  `(progn
    (choose-startgame :id ,id)
    (dotimes (nr (length  (history-log (logs-game *log*))))
      (next-percept)
      ,@body)))

;;----------------------- ACTIONS -----------------------------------------------
; nowa wersja action-prove

(defun action-prove()
  (let ((answer
	 (if (cards +game+)
	     (progn
	       (when (null (agent-pos +game+)) (setf (agent-pos +game+) (free-seat +game+)))
	       (rules-action-prove)
	       (if (omaha?)
					;omaha
		   (omaha-action-prove)
		   ;---------------------   holdem fixed ------------------------
		   (or (when (preflop?)
			 ;----fixed-preflop
			 (or
			  (rozwin-regule (fixed-preflop-bet ?rule) "bet")
			  (rozwin-regule (fixed-preflop-call ?rule) "call")
			  "fold no-rule"))
				;---------fixed-postflop
				;---------fixed flop
		       (when (flop?)		  
			 (or
			  (rozwin-regule (fixed-flop-dont-bet ?rule) "call")
			  (rozwin-regule (fixed-flop-bet ?rule) "bet")
			  (rozwin-regule (fixed-bet ?rule) "bet")
			  (rozwin-fuzzy-regule (fixed-flop-fuzzy-bet ?level ?rule) "fuzzy-bet")
			  (rozwin-regule (fixed-flop-call ?rule) "call")
			  (rozwin-regule (fixed-call ?rule) "call")
			  (rozwin-fuzzy-regule (fixed-flop-fuzzy-call ?level ?rule) "fuzzy-call")    ))
				;---------fixed turn
		       (when (turn?)		  
			 (or
			  (rozwin-regule (fixed-turn-bet ?rule) "bet")
			  (rozwin-regule (fixed-bet ?rule) "bet")
			  (rozwin-fuzzy-regule (fixed-turn-fuzzy-bet ?level ?rule) "fuzzy-bet")
			  (rozwin-regule (fixed-turn-call ?rule) "call")
			  (rozwin-regule (fixed-call ?rule) "call")
			  (rozwin-fuzzy-regule (fixed-turn-fuzzy-call ?level ?rule) "fuzzy-call")   ))
				;---------fixed river
		       (when (river?)		  
			 (or
			  (rozwin-regule (fixed-river-bet ?rule) "bet")
			  (rozwin-regule (fixed-bet ?rule) "bet")
			  (rozwin-fuzzy-regule (fixed-river-fuzzy-bet ?level ?rule) "fuzzy-bet")
			  (rozwin-regule (fixed-river-call ?rule) "call")
			  (rozwin-regule (fixed-call ?rule) "call")
			  (rozwin-fuzzy-regule (fixed-river-fuzzy-call ?level ?rule) "fuzzy-call")  ))		       
		       "fold no-rule")
		   ))
	     "fold no-agent-cards")))
     answer))

(defun check-capped-betting(act)
  (flet ((is-allin?(hist)
	   (when (and (consp (car hist))
		      (eq 'A (caar hist))))))
  (if (and (or (and (fixed?) (= 4 bets))
	       (and (omaha?) (is-allin? (history +game+)) ))
	   (or (string= act "bet") (string= act "potbet1") (string= act "potbet2")))
      "call"
      act)))


(defun agent-comitted?()
  (minusp (+ (player-money *agent-name*) (blind +game+))))

(defun speech-expansion(act rule)
  (if (not *speech*)
      (list act rule)
      ;else
      (list
       (cond
	  ((and (string= act "call") (?- (stake big))) (format nil "call callhigh"))
	  ((and (string= act "fold") (agent-comitted?)) (format nil "fold foldcommit"))
	  ((string= act "potbet1") (format nil "potbet1 potbet1"))
	  (t act))
       rule)  	))
	   
      
(defun action()
  (when (and (omaha?)
	     (<= (table-size +game+) *omaha-size-limit*))
    (return-from action (list "sitout sitout" "limit"))
  (when (and (fixed?)
	     (<= (table-size +game+) *fixed-size-limit*))
    (return-from action (list "sitout sitout" "limit"))))
  (when (null (name->seat +game+))
	(save-game (format nil "B~A.dat" (get-universal-time)))
	(return-from action (list "fold" "Zle dane!!!")))
  (handler-case
      (let ((a (tokens (action-prove))))
	(cond
	  ((string= (car a) "fuzzy-potbet1") (if (< (/ (random 1000) 1000.0) 
						(read-from-string (second a)))
					     (speech-expansion (check-capped-betting "potbet1") a)
					     (speech-expansion "call" a)))
	  ((string= (car a) "fuzzy-bet") (if (< (/ (random 1000) 1000.0) 
						(read-from-string (second a)))
					     (speech-expansion (check-capped-betting "bet") a)
					     (speech-expansion "call" a)) )
	  ((string= (car a) "fuzzy-call") (if (< (/ (random 1000) 1000.0) 
						 (read-from-string (second a)))
					      (speech-expansion "call" a)
					      (speech-expansion "fold" a)))
	  (t (speech-expansion (check-capped-betting (car a)) a))) )

    (error (e)
      (progn
	(format t "Blad: ~A" e)
	(save-game (format nil "B~A.dat" (get-universal-time)))
	(return-from action (list "fold" "error"))))))
