(defvar *poker-defs* nil)
(defvar *agent-funcs* nil)
(defvar *rules* nil)



(defclass table ()
  ((cards :initform nil :accessor cards)
   (size  :initarg :size :accessor size)
   (active-player :accessor active-player)
   (sblind  :initarg :sblind)
   (bblind  :initarg :bblind)
   (money :initarg :money :accessor money)
   (pot :initarg :pot :accessor pot)
   (stake :initarg :stake :accessor stake)
   (actions :initarg :actions :accessor actions)
   (history :initform nil :accessor history))
)

(defmethod print-object((tbl table) str)
  (aif (slot-value tbl 'cards) 
       (format str " table cards: ~A " it )
       (format str " pre-flop "))
  (format str " pot: ~A " (pot tbl) )
  (format str " active player: ~A " (active-player tbl) )
  (format str " history: [~{~A ~}]" (history tbl))
  (format str " actions: [~{~A ~}]" (actions tbl)))

(defclass agent ()
  ((position :initform nil :accessor agent-position)
   (still-playing :initform nil :accessor still-playing)
   (cards :initform nil :accessor cards)
   (table :initarg :table :accessor table)
   (decisions :initform nil :accessor decisions)
   (memory :initform (make-hash-table :test #'equal) :accessor memory)) )

(defmethod print-object((a agent) str)
  (format str "Agent: ~A~%" (agent-position a) )
  (if  (slot-value a 'still-playing)
       (princ "playing, " str)
       (princ "non playing, " str))
  (awhen (slot-value a 'cards) (format str "hand: ~A, " it ))
  (format str "decisions: ~A~%" (decisions a) )
  (format str "~%~A~%" (table a) ))

(defun clear-memory(agent)
  (setf (memory agent) (make-hash-table :test #'equal)))

(defun show-memory(agent)
  (maphash #'(lambda (k v)
		 (format t "~A=>~A~&" k v))
	     (memory agent)))

(defun table-cards( agent )
  (cards (table agent)))

(defun table-size( agent )
  (size (table agent)))

(defun num-of-active-players(table)
  (- (size table) (count nil (actions table))))

;;------------------------------------------------

(defun next-player(tbl biezacy)
  (when (= biezacy (1- (size tbl)))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr (actions tbl))
      nr
      (next-player tbl nr)) ))

(defun game-round(agent)
  (case (length (cards (table agent)))
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )

(defun next-round(smb)
  (case smb
    ('pre-flop 'flop)
    ('flop 'turn)
    ('turn 'river)))

(defun round-history(agent &optional (round (game-round agent)))
  (let ((h (history (table agent))))
    (cond
      ((eq round 'pre-flop)
       (if (zerop (length (cards (table agent)))) ;sprawdzamy czy sa nowe rundy
			     h 
			     (subseq h (1+ (position 'flop h))))  )
      ((not (find round h)) nil)
      ((eq round 'river) (if (find 'river h)
			 (subseq h 0 (position 'river h))
			 nil ))
      (t        (if (find (next-round round) h)
		    (subseq h (1+ (position (next-round round) h))
			    (position round h))
		    (subseq h 0 (position round h))) ))) )


(defun count-bets(agent &optional (r (game-round agent)))
 (count 'b (round-history agent r)))

(defun get-position(agent)
  (let ((pos (agent-position agent))
	(size (table-size agent)))
  (cond
   ((null pos) 'dont-know)
   ((= 0 pos)  'small-blind)
   ((= 1 pos)  'big-blind)
   ((or (= (1- size) pos)
        (= (- size 2) pos)) 'late-pos)
   ((<= size 5) 'late-pos)
   ((and (>= size 9)(< pos 5)) 'early-pos)
   ((and (>= size 7)(< pos 4)) 'early-pos)
   ((and (= size 6)(= pos 2)) 'early-pos)
   (t 'middle-pos)    )))

;;---------- EVENTS -----------------------------
;; W odpowiedzie na zdarzenia beda uzywane metody:

;wywolane przez new-game:
(defun new-agent(&key (size 10) (sblind 0.15) (bblind 0.25))
  (let ((tbl (make-instance 'table :size size 
		:sblind sblind :bblind bblind
		:actions (make-list size :initial-element t)
		:money   (make-list size :initial-element 0)
		:stake bblind
		:pot (+ sblind bblind))) )
    (setf (active-player tbl) (next-player tbl 1))
    (setf (nth 0 (money tbl)) sblind)
    (setf (nth 1 (money tbl)) bblind)
    (make-instance 'agent :table tbl)))


(defun percept-hand(agent cards)
  (setf (cards agent) cards)
  (setf (slot-value agent 'still-playing) t))

(defun percept-flop(agent cards)
  (let ((tbl (table agent)))
    (setf (cards tbl) cards)
    (setf (pot tbl) (reduce #'+ (money tbl)))
    (setf (money tbl) (make-list (size tbl) :initial-element 0) )
    (setf (stake tbl) 0)
    (push 'flop (history tbl))
    (setf (active-player tbl) (next-player tbl -1))   ))


(defun percept-turn(agent card)
  (let ((tbl (table agent)))
    (push card (cards tbl) )
    (incf (pot tbl) (reduce #'+ (money tbl)))
    (setf (money tbl) (make-list (size tbl) :initial-element 0) )
    (setf (stake tbl) 0)
    (push 'turn (history tbl))
    (setf (active-player tbl) (next-player tbl -1))  ))

(defun percept-river(agent card)
  (let ((tbl (table agent)))
    (push card (cards tbl) )
    (incf (pot tbl) (reduce #'+ (money tbl)))
    (setf (money tbl) (make-list (size tbl) :initial-element 0) )
    (setf (stake tbl) 0)
    (push 'river (history tbl))
    (setf (active-player tbl) (next-player tbl -1))  ))

(defun percept-check(agent)
  (let ((tbl (table agent)))
    (setf (nth (active-player tbl) (money tbl)) (stake tbl))
    (setf (nth (active-player tbl) (actions tbl)) 'c)
    (push 'c (history tbl))
    (setf (active-player tbl)
	  (next-player tbl (active-player tbl)))   ))


(defun percept-bet(agent)
  (let* ((tbl (table agent))
	 (after-flop (= 4 (length (cards tbl))))
	 (blind (slot-value tbl 'bblind))
	 (stake (if after-flop (* 2 blind) blind )))
    (incf (stake tbl) stake)
    (setf (nth (active-player tbl) (money tbl)) stake )
    (setf (nth (active-player tbl) (actions tbl)) 'b)
    (push 'b (history tbl))
    (setf (active-player tbl)
	  (next-player tbl (active-player tbl)))   ))


(defun percept-fold(agent)
  (let ((tbl (table agent)))
    (setf (nth (active-player tbl) (actions tbl)) nil)
    (push 'f (history tbl))
    (setf (active-player tbl)
	  (next-player tbl (active-player tbl)))   ))
 
(defun percept(agent what)
;  (when (and (agent-position agent) 
	;     (= (agent-position agent) (active-player (table agent))))
					;    (push what (decisions agent)))
  (case what
    ('check (percept-check agent))
    ('bet (percept-bet agent))
    ('fold (percept-fold agent)) ))


(defun get-action(agent)
 (when (null (agent-position agent))
   (setf (agent-position agent) (active-player (table agent))))
 (clear-memory agent)
 (awhen
     (prove-that 'bet agent)
   (push it (decisions agent))
   (return-from get-action 'bet))
 (awhen
     (prove-that 'check agent)
   (push it (decisions agent))
   (return-from get-action 'check))
 (push 'no-rule (decisions agent))
 'fold)




;;--------------------------------------------

(defun sklansky( agent )
 (if (cards agent)
   (aif (sklansky-group (first (cards agent))
		   (second (cards agent)))
	it
	9)
  9))

(defun s->val(symb)
 (case symb
  ('para 7)
  ('dwie-pary 6)
  ('trojka 5)
  ('street 4)
  ('kolor  3)
  ('full  2)  ))

(defun opposite-relation(symb)
  (case symb
    ('= #'=)
    ('> #'<=)
    ('< #'>=)  ))

(defun rank(agent operator smb &optional what)
 (let ((ocena (or (and (null what) (ocena (append (table-cards agent)(cards agent)))) 
                  (or (and (eq what 'hand) (ocena (cards agent)))
                      (ocena (table-cards agent))))))
   (funcall (opposite-relation operator) (car ocena) (s->val smb) )))

;;--------------------------------------------

(defun eval-expr(agent expr)
  (multiple-value-bind
	(val found-p)
      (gethash expr (memory agent))
    (if found-p
	val
	(setf (gethash expr (memory agent))      
	      (cond
		((and (atom expr) (member expr *agent-funcs*))
		 (funcall expr agent))
		((and (atom expr) (assoc expr *poker-defs*))
		 (eval-expr agent (cdr (assoc expr *poker-defs*)) ))
		((atom expr) expr)
		((member (car expr) *agent-funcs*)
		 (apply (car expr) agent (cdr expr)))
		(t (apply (car expr) (mapcar #'(lambda (e)
				     (eval-expr agent e))
				 (cdr expr))))))  )))


(defun eval-rule(agent conds)
 "conds to lista wyrazen, wszystkie musza byc prawdziwe"
 (if (null conds) 
     t
     (and (eval-expr agent (car conds))
	  (eval-rule agent (cdr conds)))))

(defun prove-that(that agent &optional (rules *rules*))
  (if (null rules)
      nil
      (if (and (eq (first (car rules)) that) (eval-rule agent (rest (car rules))))
	  (car rules)
	  (prove-that that agent (cdr rules)))))

;;----------- ENGINE -------------------------

(defclass game ()
  ((agents :initarg :agents :accessor agents)
   (table :initarg :table :accessor table)
   (size :initarg :size :accessor size)
   (cards :initarg :cards :accessor cards)
   ))

(defun new-game(size)
  (let ((agents nil)
	(table nil))
    (dotimes (i size)
      (push (new-agent :size size) agents))
    (setq table (table (first agents)))
    (deal-cards (make-instance 'game :agents agents :table table
		   :size size :cards (tasuj (talia))))))
(defmethod print-object((g game) str)
  (format str "~A~%" (table g))
  (format str "~{~A~%~}~%" (agents g ) ))

;kolejnosc gry: 
; 1. rozdac karty
; runda: dla kazdego aktywnego agenta wywolac get-action i wynik przeslac do 
; pozostalych

(defun deal-cards(game)
 (dotimes (i (size game) game)
   (percept-hand (nth i (agents game))
		 (subseq (cards game) (* 2 i) (+ 2 (* 2 i)) ))))

(defun send-percept(game percept)
 "sends percept to all agents"
 (mapc #'(lambda(agent)
	   (percept agent percept))
       (agents game)))

;lista aktywnych agentow zaczynajac od ...
(defun active-agents(game starting-agent)
 (let ((wynik nil)
       (ciecie 0))
   (dotimes (i (size (table game)))
     (when (nth i  (actions (table game)))
		(push i wynik)))
 (setq wynik  (reverse wynik))
 (setq ciecie (find starting-agent wynik))
 (append (subseq wynik  ciecie)(subseq wynik 0  ciecie))  ))

(defun one-round(game starting-agent &optional (num-of-bets 0))
 (mapc #'(lambda (nr)
	   (let ((action nil))
	     ;(format t "~%Pobieramy akcje dla agenta ~A~%" nr)
	     (when (= 1 (num-of-active-players (table game))) 
	       (return-from one-round 'koniec))
	     (setq action (get-action (nth nr (agents game))))
	     (when (and (eq action 'bet)
			(< num-of-bets 3))
	       (send-percept game 'bet)
	       (return-from one-round
		 (one-round game (next-player (table game) nr) (1+ num-of-bets))))
	     ;wiemy ze jest to czwarte bet to zamieniamy na check
	     (when (eq action 'bet) (setq action 'check))	     
	     ;(format t "To bylo ~A, wiec rozsylamy do wszystkich~%" action)
	     (send-percept game action)
	     ;(format t "Stan stolu jest nastepujacy: ~a~%" (table game)) 
	     ))
   (active-agents game starting-agent)))

(defun play-with(game)
 ; (print "PRE FLOP")
  (one-round game (next-player (table game) 1))
 ; (print "FLOP")
  (mapc #'(lambda (agent)
	    (percept-flop agent 
			  (subseq
			   (cards game)
			   (* 2 (size game))
			   (+ 3 (* 2 (size game))))))
   (agents game))
  (one-round game  (next-player (table game) -1)) 
 ; (print "TURN")
  (mapc #'(lambda (agent)
	    (percept-turn agent 
			  (nth
			   (+ 3 (* 2 (size game)))
			   (cards game))))
   (agents game))
  (one-round game  (next-player (table game) -1))
 ; (print "RIVER")
  (mapc #'(lambda (agent)
	    (percept-river agent 
			  (nth
			   (+ 4 (* 2 (size game)))
			   (cards game))))
   (agents game))
  (one-round game  (next-player (table game) -1))
  game)

(defun history->table(history size
		      &optional
		      (row 2) (col 0)
		      (wynik (make-array (list size 10) :initial-element nil))
		      (folded-players nil))
  (if history
      (cond
	((= row size)   (history->table history size 0 (1+ col) wynik folded-players) )
	((member row folded-players)   (history->table history size (1+ row)  col wynik folded-players)  )
	((or (eq (car history) 'flop)  
	     (eq (car history) 'turn)
	     (eq (car history) 'river))
	         (setf (aref wynik row col) (car history))	 
	         (history->table (cdr history) size  0 (1+ col) wynik folded-players) )
	((eq (car history) 'f)  
         	 (push row folded-players)
	         (setf (aref wynik row col) (car history))
	         (history->table (cdr history) size (1+ row) col wynik folded-players) )
	(t       (setf (aref wynik row col) (car history))
	         (history->table (cdr history) size (1+ row) col wynik folded-players) )
	)
    wynik))

(defun print-game(&optional (size 8))
  (let ((game (new-game size))
	(hist nil))
    (play-with game)
    (setq hist (history->table
		(reverse (history (table game)))
		size))
    (princ "<table border=1>")
    (format t "<tr>~{~A~}" (liczby->opis (reverse (cards (table game)))))
    (princ "</table>")
    (princ "<table border=1>")
    (dotimes (row (array-dimension hist 0))
      (format t "<tr>~{~A~}" (liczby->opis (cards (nth row (agents game)))))
      (format t "<td>~A"  (reverse (decisions (nth row (agents game)))))
      (dotimes (col (array-dimension hist 1))
      (format t "<td>~A" (aif (aref hist row col) it "")  )

	))
    (princ "</table>")	
  ))

(defun save-game()
  (with-open-file (plik "wynik.html" 
			       :direction :output 
 			       :if-exists :supersede
			       :if-does-not-exist :create)
   (let ((*standard-output* plik))
      (print-game 7))
    'ok))

;;--------------------------------------------
(defun i(a b)
 (and a b))


(setq *agent-funcs*
  '(table-cards table-size get-position sklansky count-bets rank))

(setq *poker-defs*
      '((pre-flop     . (null table-cards))
        (from-flop    . table-cards)
	(flop         . (= 3 (length table-cards)))
	(turn         . (= 4 (length table-cards)))
	(river        . (= 5 (length table-cards)))

	(s-blind      . (eq small-blind get-position))
	(b-blind      . (eq big-blind get-position))
        (e-pos        . (eq early-pos get-position))
	(m-pos        . (eq middle-pos get-position))
	(l-pos        . (eq late-pos get-position))

        (heads-up     . (= 2  table-size))
        (fulltable    . (> table-size 5))
        (shorthand    . (i
		         (>  table-size 2)
		         (<= table-size 5)))

	(no-bets      . (zerop count-bets))
	(with-bets    . (plusp count-bets))
	(one-bet      . (= 1 count-bets))
	(with-raise   . (> count-bets 2))
	))

(setq *rules*
  '(

   (check pre-flop b-blind no-bets)
   (bet   pre-flop (= 1 sklansky))

   (bet   pre-flop  heads-up (< sklansky 3))
   (bet   pre-flop  heads-up no-bets b-blind (< sklansky 9))
   (check pre-flop  heads-up (< sklansky 9))

   (bet   pre-flop  shorthand sklansky (<= sklansky 2))
   (check pre-flop  shorthand b-blind sklansky)
   (check pre-flop  shorthand s-blind one-bet (< sklansky 9))
   (check pre-flop  shorthand s-blind (<= sklansky 7))
   (check pre-flop  shorthand e-pos   (<= sklansky 6))
   (check pre-flop  shorthand m-pos  (<= sklansky 7))
   (check pre-flop  shorthand l-pos    (<= sklansky 9))

   (check pre-flop  fulltable e-pos no-raise      (<= sklansky 3))
   (check pre-flop  fulltable e-pos with-raise    (<= sklansky 2))

   (check pre-flop  fulltable m-pos no-bets    (<= sklansky 6))
   (check pre-flop  fulltable m-pos one-bet    (<= sklansky 4))
   (check pre-flop  fulltable m-pos with-raise (<= sklansky 2))

   (check pre-flop  fulltable l-pos no-bets      (<= sklansky 8))
   (check pre-flop  fulltable l-pos one-bet      (<= sklansky 7))
   (check pre-flop  fulltable l-pos with-raise   (<= sklansky 4))
  
   (check pre-flop  fulltable s-blind no-bets   (<= sklansky 8))
   (check pre-flop  fulltable s-blind one-bet   (<= sklansky 5))
  
   (check pre-flop  fulltable b-blind with-bets   (<= sklansky 5)) 


    (check from-flop)

  ));end of *rules*