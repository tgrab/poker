(defvar *conflicts* nil) ; error reports!
; Game globals
(defvar +agent-name+ "grabola")
(defvar +game-kind+ 3) ; 3 - Omaha Pot Limit
(defvar +name->seat+ nil) ; assoc table 
(defvar +history+ nil)
(defvar +table-cards+ nil)
(defvar +pot+ 0)
(defvar +stake+ 0)
(defvar +blind+ 0)
(defvar +taken-seats+ (make-array 10 :initial-element nil))
(defvar +table-size+ nil)
(defvar +table-size-alarm+ nil) ;(setq +table-size-alarm+ '(4 "192.168.0.10" 10000))
(defvar +active-players+ (make-array 10 :initial-element nil))
(defvar +free-seat+ 0)

;tables for every player:
(defvar +balance+ (make-array 10 :initial-element 0)) ;money spent in this round
(defvar +total-balance+ (make-array 10 :initial-element 0)) ;money spent in prev. rounds
(defvar +player-history+  (make-array 10 :initial-element nil))


; Agent's state:
(defvar +cards+ nil)
(defvar +pos+ nil)


;;----------------------------------------------------------
;;------------------- CARDS Descr. -------------------------
;;----------------------------------------------------------

(defvar *kolory* '(S H D C))

(defvar *figury* '(A K Q J T 9 8 7 6 5 4 3 2))

(defvar *color-htm* '("<li class='spades'>~A&#9824;"
		      "<li class='hearts'>~A&#9829;" 
		      "<li class='diamonds'>~A&#9830;"
		      "<li class='clubs'>~A&#9827;" ))

(defvar *ranks* '("Straight Flush" "Quads" "Full House" "Color" "Straight" 
		  "Tripple" "Two pairs" "One pair" "High card"))

;karty to liczby od 1 do 52, 1=As Pik,2=Krol Pik,52=2 Trefl
;kolory: 0=Pik, 1=Kier, 2=Karo, 3=Trefl
;figury: 0=As, 1=Krol, 12=Dwojka

; karta to zawsze liczba z 1..52
(defun kolor(liczba)
  (floor (1- liczba) 13))

(defun figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a zeby nie bylo ostrzezen
   b))

(defun karta->napis (liczba)
   (format nil "~A~A" 
	   (nth (figura liczba) *figury*)
	   (nth (kolor liczba) *kolory*)    ))

(defun karty->napisy(lista-liczb)
 (mapcar #'karta->napis lista-liczb))

(defun karta->html(liczba)
 (format nil 
	 (nth (kolor liczba) *color-htm*) 
	 (nth (figura liczba) *figury*) ))

(defun karty->html(liczby)
  (format nil "<ul class='karty'>~{~A~}</ul>" (mapcar #'karta->html liczby)))    


(defun napis->karta( napis )
 (let ((o1 (char  (string-upcase napis) 0))
       (o2 (char  (string-upcase napis) 1)))
   (+
     (coerce    ;zeby uniknac ostrzezen kompilatora  
       (case (char-code o1)
        (65 1) (75 2) (81 3) (74 4) (84 5) (57 6) (56 7) (55 8) (54 9)
        (53 10) (52 11) (51 12) (50 13)) 'number) 
     (* 13
       (coerce ;zeby uniknac ostrzezen kompilatora  
         (case (char-code o2)
           (83 0)(72 1)(68 2)(67 3))  'number)  )) ))

(defun napisy->karty(napisy)
  (mapcar #'napis->karta napisy))

(defun render-rank(ocena)
  (format nil "~A, ~A"
	  (nth (car ocena) *ranks*)
	  (mapcar  (lambda (o) (nth o *figury*)) (cdr ocena)) ))


(defun send-message(host port msg)
  (handler-case
	 (let ((str (trivial-sockets:open-stream host port)))
	   (format str "~A" msg)
	   (close str))
	 (error (c) (format t "BLAD ~A~&" c))))
	 
(defparameter +say+ nil)

(defun say-text(text)
  (send-message #(127 0 0 1) 1314  (format nil "(SayText \" ~A \")" text) ))



;;--------------------------------------------------------------------------
;;------------------------------- Game Logic -------------------------------
;;--------------------------------------------------------------------------

; rezygnujemy z okreslania active-playera wykorzystujemy <player-pos>
(defun show-game()
  (format t "~A <br>~%~A<br>~%~A<br>~%~A<br>~%~A<br>~%~A<br>~%~A<br>~%" 
	  (reverse +name->seat+) 
	  +taken-seats+
	  +active-players+
	  +balance+
	  +total-balance+
	  +history+
	  +player-history+))

(let ((h (make-hash-table)))
  (defun game-prop(property)
    (multiple-value-bind (val found) (gethash property h)
      (if found
	  val
	  (setf (gethash property h)
		(case property
		  (preflop  (null +table-cards+))
		  (flop     (= 3 (length +table-cards+)))
		  (turn     (= 4 (length +table-cards+)))
		  (river    (= 5 (length +table-cards+)))		  
		  (game-round
		   (case (length +table-cards+)
		     (0 'pre-flop)
		     (3 'flop)
		     (4 'turn)
		     (5 'river)))
		  (ocena (if +table-cards+
			     (if (= 3 +game-kind+)
				 (omaha-ocena +cards+ +table-cards+ )
				 (ocena (append +cards+ +table-cards+)))
			     '(10)))
		  (max-of-colors   (apply #'max (coerce (wektor-kolorow +table-cards+) 'list)) )
		  )))))

  (defun get-table()   h)
  (defun clear-table()
    (setq h (clrhash h))))


(defun previous-round(smb)
  (case smb
    ('flop 'pre-flop)
    ('turn 'flop)
    ('river 'turn)))

(defun next-round(smb)
  (case smb
    ('pre-flop 'flop)
    ('flop 'turn)
    ('turn 'river)))

(defun round-history(&optional (r (game-prop 'game-round)) (h +history+))
  (cond
    ((eq r 'pre-flop)  (if (game-prop 'preflop) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 

(defun bets(&optional (r (game-prop 'game-round)) (h +history+))
 (count-if #'(lambda(el)
	       (and (consp el)
		    (or (eq (car el) 'b) 
			(eq (car el) 'r)
			(eq (car el) 'a))))
	   (round-history r h)))


(defun smb->val(s)
    (case s
      ('highcard 8)
      ('pair 7)
      ('two-pairs 6)
      ('trips 5)
      ('straight 4)
      ('color 3)
      ('fullhouse 2)
      ('quads 1)
      ('strflush 0)
      ('< #'<)
      ('> #'>)
      ('<= #'<=)
      ('>= #'>=)
      ('= #'=)))


(defmacro rank (op rnk)
    `(funcall (smb->val ',op) (smb->val ',rnk) (car (game-prop 'ocena)) ))

(defun player-pos(name &optional (ta +name->seat+))
  (cdr (assoc name ta :test #'string=)))

(defun agent-pos()
  (or +pos+ (player-pos +agent-name+)))

;TODO- chyba zle gdy ppos
(defun stake(&optional (ppos (agent-pos)))
  (if ppos
       (+ +stake+ (aref +balance+ ppos))
       +stake+))

(defun round-balance(&optional (ppos (agent-pos)))
  (if ppos
    (- (aref +balance+ ppos))
    0))

(defun balance(&optional (ppos (agent-pos)))
  (if ppos
    (- (+ (aref +total-balance+ ppos) (aref +balance+ ppos)))
    -1))
       

(defun player-stake(name)
  (stake (player-pos name)))

(defun safe-ratio(num den)
  (if (plusp den)
      (/ num den)
      1000))

(defun pot-odds()
  (aif (agent-pos)
       (safe-ratio +pot+ 
		   (+ +stake+ (aref +balance+ it)))
       (safe-ratio +pot+ +stake+)))

(defun insert-player(name)
  (if (< +free-seat+ 10)
      (progn
	(when (string= name +agent-name+)
	  (setq +pos+ +free-seat+))
	(push (cons name +free-seat+) +name->seat+)
	(setf (aref +taken-seats+ +free-seat+) t)
	(setf (aref +active-players+ +free-seat+) t)
	(incf +free-seat+))
      (push (list name +free-seat+) *conflicts*)))

(defun insert-player?(name)
  (and (null +table-cards+) ;preflop 
       (not (assoc name +name->seat+ :test #'string=))
       (insert-player name)))

(defun odds(num-of-outs)
  (/ (- 52 (length +cards+) (length +table-cards+) num-of-outs)
     num-of-outs))

(defun good-odds?(num-of-outs)
  (and (plusp num-of-outs)
       (> (pot-odds) (odds num-of-outs))))

; Rank utilities:

; sluzy do sprawdzenia czy moze byc u kogos kolor:
; liczy maksymalna ilosc kart w kolorze na stole!
(defun max-of-colors(&optional (w-kolorow (wektor-kolorow +table-cards+)))
  (apply #'max (coerce w-kolorow 'list) ))

(defun color-table?(&optional (w-kolorow  (wektor-kolorow +table-cards+)))
  (>= (max-of-colors w-kolorow) 3))

(defun no-color-table?(&optional (w-kolorow  (wektor-kolorow +table-cards+)))
  (<= (max-of-colors w-kolorow) 2))

(defun rainbow-table?(&optional (w-kolorow  (wektor-kolorow +table-cards+)))
  (= (max-of-colors w-kolorow) 1))

(defun pair-table?(&optional (vtable  (wektor-figur +table-cards+)))
  (>= (apply #'max (coerce vtable 'list) ) 2))

(defun trips-table?(&optional (vtable (wektor-figur +table-cards+)))
  (= (apply #'max (coerce vtable 'list) ) 3))

(defun two-pair-table?(&optional (cards +table-cards+))
  (= 6 (car (ocena cards))))

(defun no-pair-table?(&optional (vtable (wektor-figur +table-cards+)) )
   (<= (apply #'max (coerce vtable 'list) ) 1))

(defun on-small-blind?()
  (when +pos+ (zerop +pos+)))

(defun on-big-blind?()
  (when +pos+ (= 1 +pos+)))

;;--------------------------------- UPDATE MEMORY------------------------------
;TODO clear-game-memory, clear-round-memory, clear-memory


(defun set-flop(r1 r2 r3)
  (setf (get 'flop 'clauses) nil
	(get 'T1 'clauses) nil (get 'T1F 'clauses) nil (get 'T1C 'clauses) nil
	(get 'T2 'clauses) nil (get 'T2F 'clauses) nil (get 'T2C 'clauses) nil
	(get 'T3 'clauses) nil (get 'T3F 'clauses) nil (get 'T3C 'clauses) nil)
  (let ((c (sort (list r1 r2 r3) #'< :key #'figura)))
    (add-clause `((T1 ,(nth 0 c)  )))
    (add-clause `((T2 ,(nth 1 c) )))
    (add-clause `((T3 ,(nth 2 c) )))
    (add-clause `((T1F ,(figura (nth 0 c))  )))
    (add-clause `((T2F ,(figura (nth 1 c))  ))) 
    (add-clause `((T3F ,(figura (nth 2 c))  )))
    (add-clause `((T1C ,(kolor (nth 0 c))  )))
    (add-clause `((T2C ,(kolor (nth 1 c))  ))) 
    (add-clause `((T3C ,(kolor (nth 2 c))  )))   )
  (mapc
   #'(lambda (l) 
       (add-clause (list (cons 'flop l))) )
   (moje:permutacje (list r1 r2 r3))))

;dla omahy, dla fixeda jest w holecards(?)
(defun set-hand(r1 r2 r3 r4)
  (setf (get 'hand 'clauses) nil)
  (setf	(get 'H1 'clauses) nil (get 'H1F 'clauses) nil (get 'H1C 'clauses) nil
	(get 'H2 'clauses) nil (get 'H2F 'clauses) nil (get 'H2C 'clauses) nil
	(get 'H3 'clauses) nil (get 'H3F 'clauses) nil (get 'H3C 'clauses) nil
	(get 'H4 'clauses) nil (get 'H4F 'clauses) nil (get 'H4C 'clauses) nil)
  (let ((c (sort (list r1 r2 r3 r4) #'< :key #'figura)))
    (add-clause `((H1 ,(nth 0 c)  )))
    (add-clause `((H2 ,(nth 1 c) )))
    (add-clause `((H3 ,(nth 2 c) )))
    (add-clause `((H4 ,(nth 3 c) )))
    (add-clause `((H1F ,(figura (nth 0 c))  )))
    (add-clause `((H2F ,(figura (nth 1 c))  ))) 
    (add-clause `((H3F ,(figura (nth 2 c))  )))
    (add-clause `((H4F ,(figura (nth 3 c))  )))
    (add-clause `((H1C ,(kolor (nth 0 c))  )))
    (add-clause `((H2C ,(kolor (nth 1 c))  ))) 
    (add-clause `((H3C ,(kolor (nth 2 c))  ))) 
    (add-clause `((H4C ,(kolor (nth 3 c))  )))  )
  (mapc
   #'(lambda (l) 
       (add-clause (list (cons 'hand l))) )
   (moje:permutacje (list r1 r2 r3 r4))))

;;--------------------------- PERCEPTS ----------------------------------------

(defun set-game-kind(kind)
  (setq +game-kind+ kind)
  (case kind
    (3 (set-clause (game-kind omaha)))
    (1 (set-clause (game-kind hnolomit)))
    (t (set-clause (game-kind hfixed)))))


(defun new-game(amount)
  ;zerujemy wszystko:
  (clear-table)
  (setq +name->seat+ nil +table-cards+ nil +pot+ 0 +stake+ 0
	+taken-seats+ (make-array 10 :initial-element nil)
	+active-players+ (make-array 10 :initial-element nil)
	+free-seat+ 0 +history+ nil
	+player-history+  (make-array 10 :initial-element nil)
	+total-balance+ (make-array 10 :initial-element 0)
	+balance+ (make-array 10 :initial-element 0)
	+cards+ nil +pos+ nil)
  (setq +pot+ amount +stake+ amount +blind+ amount)
  (setf (aref +balance+ 0) (- amount)) )


(defun small-blind(name amount)
  (new-game amount)
  (push (cons 'sb amount) +history+)
  (push (cons 'sb amount) (aref +player-history+ 0))
  (insert-player name))

(defun big-blind(name amount)
  (incf +pot+ amount)
  (setq +stake+ amount +blind+ amount)
  (setf (aref +balance+ 1) (- amount))
  (push (cons 'bb amount) +history+)
  (push (cons 'bb amount) (aref +player-history+ 1))
  (insert-player name))

(defun call(name amount)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push (cons 'c amount) +history+)
	  (push (cons 'c amount) (aref +player-history+ nr))
	  (decf (aref +balance+ nr) amount)
	  (incf +pot+ amount))
	(push (list 'call name) *conflicts*))))


(defun fold(name)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push 'f +history+)
	  (push 'f (aref +player-history+ nr))
	  (setf (aref +active-players+ nr) nil)
	  'ok)
	(push (list 'fold name) *conflicts*)))   )



(defun bet(name amount &optional (smb 'b))
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push (cons smb amount) +history+)
	  (push (cons smb amount) (aref +player-history+ nr))
	  (decf (aref +balance+ nr) amount)
	  (setq +stake+ amount)
	  (incf +pot+ amount)
	(push (list 'bet name) *conflicts*)))))



(defun all-in(name amount)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push (cons 'a amount) +history+)
	  (push (cons 'a amount) (aref +player-history+ nr))
	  (decf (aref +balance+ nr) amount)
	  (when (> amount +stake+) (setq +stake+ amount))
	  (incf +pot+ amount)
	  (setf (aref +active-players+ nr) nil)
	  'ok))
	(push (list 'bet name) *conflicts*)))

(defun flop(c1 c2 c3)
  (clear-table)
  (set-flop c1 c2 c3)
 (setq
  +table-cards+ (list c1 c2 c3)
  +stake+ 0 )
 (push 'flop +history+)
 (dotimes (i 10)
   (when (aref +active-players+ i) (push 'flop (aref +player-history+ i)))
   (incf (aref +total-balance+ i) (aref +balance+ i))
   (setf (aref +balance+ i) 0))

   (let ((ts  (count-if #'identity +taken-seats+)))
     (when +table-size-alarm+
	 (let ((alarm-size (first +table-size-alarm+))
	       (alarm-host (second +table-size-alarm+))
	       (alarm-port (third +table-size-alarm+)))
	   (when (and  +table-size+ 
	              (<= +table-size+ alarm-size)
		      (<= ts alarm-size))
	     (send-message alarm-host alarm-port "sitout" )
	     (setq +table-size-alarm+ nil))))
     (setq +table-size+ ts)) )


(defun turn(c1)
  (clear-table)
 (setq +stake+ 0 )
 (push c1 +table-cards+)
 (push 'turn +history+)
 (dotimes (i 10)
   (when (aref +active-players+ i) (push 'turn (aref +player-history+ i)))
   (incf (aref +total-balance+ i) (aref +balance+ i))
   (setf (aref +balance+ i) 0))  )


(defun river(c1)
  (clear-table)
 (setq +stake+ 0 )
 (push c1 +table-cards+)
 (push 'river +history+)
 (dotimes (i 10)
   (when (aref +active-players+ i) (push 'river (aref +player-history+ i)))
   (incf (aref +total-balance+ i) (aref +balance+ i))
   (setf (aref +balance+ i) 0))  )


(defun holecards(c1 c2 &optional c3 c4)
  (clear-table)
  (if (and c3 c4)
      (progn 
	(set-hand c1 c2 c3 c4 )
	(setq +cards+ (list c1 c2 c3 c4)) )
      (progn
	(setq +cards+ (list c1 c2))
	(clear-predicate 'sklansky)
	(clear-predicate 'hand)
	(clear-predicate 'handF)
	(clear-predicate 'H1)
	(clear-predicate 'H2)
	(clear-predicate 'H1F)
	(clear-predicate 'H1C)
	(clear-predicate 'H2F)
	(clear-predicate 'H2C)
	(awhen (sklansky-group c1 c2)
	  (add-clause `((sklansky ,it))) )
	(add-clause `((hand ,c1 ,c2)))
	(add-clause `((hand ,c2 ,c1)))
	(add-clause `((handf ,(figura c1) ,(figura c2) )))
	(add-clause `((handf ,(figura c2) ,(figura c1) )))
	(if (< (figura c1) (figura c2))
	    (progn      
	      (add-clause `((H1 ,c1)))
	      (add-clause `((H1F ,(figura c1))))
	      (add-clause `((H1C ,(kolor c1))))
	      (add-clause `((H2 ,c2)))
	      (add-clause `((H2F ,(figura c2))))
	      (add-clause `((H2C ,(kolor c2)))) )
	    (progn
	      (add-clause `((H1 ,c2)))
	      (add-clause `((H1F ,(figura c2))))
	      (add-clause `((H1C ,(kolor c2))))
	      (add-clause `((H2 ,c1)))
	      (add-clause `((H2F ,(figura c1))))
	      (add-clause `((H2C ,(kolor c1))))  ))	  )))



;;----------------------- ACTIONS -----------------------------------------------

(defun action-prove()
  (if +cards+
      (progn
      (when (null +pos+) (setq +pos+ +free-seat+))
      (or
       (awhen (?- (decision fold ?rule)) (format nil "fold ~A" (subst-bindings it '?rule )))
       (awhen (?- (decision dont-bet ?rule)) (format nil "call ~A" (subst-bindings  it '?rule)))
       (awhen (?- (decision potbet1 ?rule)) (format nil "potbet1 ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision potbet2 ?rule)) (format nil "potbet2 ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision bet ?rule)) (format nil "bet ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision fuzzy-bet ?level ?rule)) (format nil "fuzzy-bet ~A ~A" (subst-bindings it '?level) (subst-bindings it '?rule)))
       (awhen (?- (decision call ?rule)) (format nil "call ~A" (subst-bindings it '?rule)))
       (awhen (?- (decision fuzzy-call ?level ?rule)) (format nil "fuzzy-call ~A ~A" (subst-bindings it '?level) (subst-bindings it '?rule)))
       "fold no decision"))
    "fold not enough data"))

(defun action()
  (let ((a (tokens (action-prove)))
	final-action)
    (setq final-action
	  (cond
	    ((string= (car a) "fuzzy-bet") (if (< (/ (random 1000) 1000.0) 
					     (read-from-string (second a)))
					  "fuzzybet"
					  "fuzzycall"))
	    ((string= (car a) "fuzzy-call") (if (< (/ (random 1000) 1000.0) 
					     (read-from-string (second a)))
					  "fuzzycall"
					  "fuzzyfold"))
	    (t (car a)))   )
    (when +say+
      (when (string= final-action "fuzzycall") (say-text "fuzzy call"))
      ;TODO REMOVE it:
      (when (and (string= final-action "fold") (< (* 4 +blind+) (balance))) (say-text "commited")) )
    final-action))


(defun set-alarm()
    (setq +table-size-alarm+ '(3 "192.168.1.10" 10000)))




 


