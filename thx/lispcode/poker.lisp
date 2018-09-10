;sluza do ustalania parametrow nowych gier
(defvar *agent-name* "grabola")
(defvar *table-name* "Unknown")
(defvar *casino*     "purple")
(defvar *kind* 3) ;3omaha pl,2holdem nl,1holdem fixed

(defvar *game* nil)
(defvar *game2* nil)
(defvar *agent-loose-level* 0)
(defvar *agent-agression-level* 0)
(defvar *html-log* t)

;;-------------------------------------
(defclass player()
((name :initform "" :initarg :name :accessor name) 
 (seat :initform 0 :initarg :seat :accessor seat)
 (active? :initform t :accessor active?)
 (cards :initform nil :initarg :cards :accessor cards)
 (history :initform nil :accessor history)
 (money :initform  0 :accessor money) 
 (balance :initform 0 :accessor balance)
 (total-balance :initform  0 :accessor total-balance) 
 ;player-cortex - modyfikowane w trakcie gry:
 (preflop-bets :initform 0 :accessor preflop-bets)
 (flop-bets :initform 0 :accessor flop-bets)
 (vol-in-pot :initform nil :accessor vol-in-pot)
 (flop-bet :initform nil :accessor flop-bet)
 ;player-profile - opisuja prawdopodobienstwa roznych zagran
 ;tego gracza, wyznaczane z historii gier na podstawie cortexow:
 (vip :initform 0 :accessor vip :documentation "voluntarly in pot")
 (pfr :initform 0 :accessor pfr :documentation "pre flop raise")
 (cont-bet :initform nil :accessor cont-bet)    ))

(defclass game()
(;game-info, dane niezmienne w czasie gry:
 (html-log :initform nil  :accessor html-log) 
 (casino :initform *casino*  :initarg :casino :accessor casino) 
 (id :initform "0" :initarg :id :accessor id)
 (czas :initform (get-universal-time) :initarg :czas :accessor czas)
 (kind :initform *kind* :initarg :kind :accessor kind) 
 (blind :initform 0 :initarg :blind :accessor blind)
 (table-name :initform *table-name* :initarg :table-name :accessor table-name) 
 (table-size :initform 6 :initarg :table-size :accessor table-size)
 (agent-name :initform *agent-name* :initarg :agent-name :accessor agent-name)
 (agent-pos :initform nil :initarg :agent-pos :accessor agent-pos)
 ;game-state:
 (players :initform (make-array 10 :initial-element nil) :initarg :players :accessor players)
 (free-seat :initform 0 :accessor free-seat) 
 (table-cards :initarg :table-cards :initform nil :accessor table-cards) 
 (cards :initarg :cards :initform nil :accessor cards) ;karty agenta
 (history :initform nil :initarg :history :accessor history)
 (pot :initform 0 :accessor pot)
 (stake :initform 0 :accessor stake)
 ;cortex:
 (preflop-bets :initform 0 :accessor preflop-bets)
 (flop-bets :initform 0 :accessor flop-bets)
 (on-flop-players :initform 0 :accessor on-flop-players) 
 ;profile:
 (vip :initform 0 :accessor vip :documentation "voluntarly in pot")
 (pfr :initform 0 :accessor pfr :documentation "pre flop raise")
 ;engine:
 (prev-game :initform nil :initarg :prev-game :accessor prev-game)
 ;wnioskowanie, listy symboli:
 (game-memo  :initform nil  
	     :accessor game-memo 
	     :documentation "immutable dla calej gry, ustawiane w new-game z cortexow")
 (round-memo :initform nil
	     :accessor round-memo 
	     :documentation "tworzone na pocz. rund")
 (percept-memo  :initform nil  
		:accessor percept-memo 
		:documentation "ustawiane przed kazda akcja")
 ;game-vars
 (bets :initform 0  :accessor bets)
 (rank :initform nil :accessor rank)
 (ilosc-figur :initform 0 :accessor ilosc-figur)
 (ilosc-figur-1 :initform 0 :accessor ilosc-figur-1)
 (ilosc-kolorow :initform 0 :accessor ilosc-kolorow)
 (ilosc-kolorow-1 :initform 0 :accessor ilosc-kolorow-1)
 (t1 :accessor t1)
 (t2 :accessor t2)
 (t3 :accessor t3)
 (t4 :accessor t4)  
 (t5 :accessor t5)
 (h1 :accessor h1)
 (h2 :accessor h2)
 (h3 :accessor h3)
 (h4 :accessor h4) 
 (hc1 :accessor hc1)
 (hc2 :accessor hc2)
 (hc3 :accessor hc3)
 (hc4 :accessor hc4 )   ))



;;-------------------------- VIEW -----------------------------------
;;-------------------------------------------------------------------

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
 (if liczba
    (format nil 
	 (nth (kolor liczba) *color-htm*) 
	 (nth (figura liczba) *figury*) )
   ""))

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


;;-------------------------- GENERIC TOOLS---------------------------
;;-------------------------------------------------------------------

(defmethod get-player((idx number) (g game))
  (aref (players g) idx))

(defmethod get-player((name string) (g game))
  (dotimes (i (free-seat g))
    (when (string= name  
		   (name (aref (players g) i)))
      (return-from get-player (aref (players g) i)))))

(defun get-agent(&optional (g *game*))
  (awhen (agent-pos g)
      (aref (players g) it)))

(defun agent-stake(&optional (g *game*))
  (aif (get-agent g)
       (+ (stake g) (balance it))
       (stake g)))

(defun agent-position(game)
  (let ((a (get-agent game)))
    (if a
	(seat a)
	(free-seat game))))

(defun game-history( g &optional (depth 10))
 (if (or (= 0 depth) (null (prev-game g)))
     nil
     (cons (prev-game g) (game-history (prev-game g) (1- depth)))))

(defun player-history(name game)
  (mapcan
   #'(lambda (g)
       (awhen (get-player name g)
	      (when (string= name (name it))
		(list it))))
   (game-history game)))

(defun set-pfr(player game)
  (let ((hist (player-history (name player) game)))
    (when (plusp (length hist))
      (setf (pfr player)
	    (/ (count-if #'(lambda (p) (plusp (preflop-bets p))) hist)  (length hist) 1.0))
  )))

(defun predict-pfr(game)
  (if (prev-game game)
      (let ((old-players (players (prev-game game)))
	    (sum 0))
	(dotimes (i 10)
	  (let ((p (aref old-players i))  )
	    (when (not (and p 
			(or (get-player (name p) game)
			    (string= *agent-name* (name p)))))			
	      ;(print p)
	      (when p (incf sum (pfr p))))
	  ))
	sum)
      0))

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------


(defmethod print-object((p player) str)
  (format str "#P(~A@~A~A{~A ~A ~A$}~A)" 
	  (name p)(seat p) (if (cards p) (format nil "_~A_" (karty->napisy (cards p))) "") 
	  (money p) (total-balance p) (balance p)
	  (format nil "/pb:~A/v-pot:~A/f-bet:~A/ /pfr:~A/" (preflop-bets p)(vol-in-pot p) (flop-bet p) (pfr p)) ))

(defmethod print-object((g game) str)
  (format str "#GAME{~A}(~&" (id g))
  (let ((c (cards g))
	(tc (table-cards g)))
	(when c (format str "cards: ~A " (karty->napisy c)))
	(when tc (format str "table-cards: ~A" (karty->napisy tc)))
	(awhen (rank g) (format str " rank: ~A" (render-rank it) ))
	(when (or c tc) (format str "~&")))
  (format str "pot: ~A stake: ~A agent-stake: ~A~&" (pot g) (stake g) (agent-stake g))
  (format str "history: ~A~&" (history g))
  (format str "players:")
  (dotimes (i (free-seat g))
    (format str " ~A" (aref (players g) i)))
  (format str "~&)"))

;;-----------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------
;;--------------------------------- UTILITIES ---------------------------------------
;;-----------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------


(defun safe-division(e d)
  (if (zerop d)
      1000
      (float (/ e d))))


(defun odds (outs &optional (g *game*))
  (if (omaha? g)
      (safe-division (- 48 (length (table-cards g)) outs) outs)
      (safe-division (- 50 (length (table-cards g)) outs) outs)))       


(defun pot-odds(&optional (g *game*))
  (safe-division (pot g) (agent-stake g)))


(defun count-active-players(game)
  (count-if #'(lambda (p) (and p (active? p))) 
	    (coerce (players game) 'list)))

(defun active-players-positions(game)
  (mapcan #'(lambda (p) (when (and p (active? p)) (list (seat p))))
	  (coerce (players game) 'list)))
;-----------------------------------------------------------------------

(defun omaha?(&optional (g *game*))
  (= 3 (kind g)))

(defun fixed?(&optional (g *game*))
  (= 2 (kind g)))

(defun holdem?(&optional (g *game*))
  (<= (kind g) 2))

(defun preflop?(&optional (g *game*))
  (null (table-cards g)))

(defun flop?(&optional (g *game*))
  (= 3 (length (table-cards g))))

(defun turn?(&optional (g *game*))
  (= 4 (length (table-cards g))))

(defun river?(&optional (g *game*))
  (= 5 (length (table-cards g))))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------


(defun two-str8-table?(&optional (g *game*))
  (when (table-cards g)
    (let ((figury-stolu  (mapcar #'figura (table-cards g))))
      (some
       #'(lambda (para)
	   (poker.ranks::street (wektor-figur 
			       (mapcar #'1+ 
				       (append para figury-stolu)))))
       (podzbiory-2  
	(set-difference 
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12)
	 figury-stolu)) ))))

(defun one-str8-table?(&optional (g *game*))
  (let ((figury-stolu  (mapcar #'figura (table-cards g))))
    (when (>= (length figury-stolu) 4)
      (some
       #'(lambda (karta)
	   (poker.ranks::street (wektor-figur 
				 (mapcar #'1+ 
					 (cons karta figury-stolu)))))
       (set-difference 
	'(0 1 2 3 4 5 6 7 8 9 10 11 12)
	figury-stolu)) )))


;;--------------------------------------------------------------------------
;;------------------------------- Omaha Predicates--------------------------
;;--------------------------------------------------------------------------


;zle dla A 2 3 5 - 4 5
(defun omaha-str8-nuts?(&optional (g *game*))
  (let* ((stol (table-cards g))
	 (figury-stolu  (mapcar #'figura stol))
	 (rank (rank g)))
    (and (= 4 (car rank))
	 (<=
	  ;(1+ (second rank))
	  (second rank)
	  (apply #'min
		 (mapcar
		  #'(lambda (para)
		      (let  ((str (poker.ranks::street 
				   (wektor-figur (mapcar #'1+ (append para figury-stolu))))))
			    (if str str 100)))
		      (podzbiory-2  
		       (set-difference 
			'(0 1 2 3 4 5 6 7 8 9 10 11 12)
			figury-stolu)) ))))))

;;testy czy mam nutsa?:
(defun kolor-stolu(table-cards)
  (let ((vec (wektor-kolorow table-cards)))
    (dotimes (i 4 -1)
      (when (>= (aref vec i) 3) (return-from kolor-stolu i)))))

(defun omaha-color-nuts?( game )
;nie sprawdzamy czy w ogole lezy kolor
  (with-slots (cards table-cards) game
    (let ((col (kolor-stolu table-cards)))
      (cond
	((member  (+ 1 (* 13 col)) cards)   ;czy mam asa w tym kolorze
	  (values t 0))
	((member  (+ 2 (* 13 col)) cards)   ;czy mam krola w tym kolorze
	   (if (member (+ 1 (* 13 col)) table-cards) ;czy as lezy na stole
	       (values t 0)
	       (values nil 1) ))
	((member  (+ 3 (* 13 col)) cards)   ;czy mam Q w tym kolorze
	   (if (member (+ 1 (* 13 col)) table-cards) ;czy as lezy na stole
	       	   (if (member (+ 2 (* 13 col)) table-cards) ;czy K lezy na stole
		       (values t 0)
		       (values nil 1) )
	       	   (if (member (+ 2 (* 13 col)) table-cards) ;brak Asa czy K lezy na stole
		       (values nil 1)
		       (values nil 2) ) ))
	(t (values nil 100))))))


;(defun omaha-color-nuts?(&optional  (g *game*) )
;  (with-slots (cards table-cards) g
;	      (let (col)
;		(when (some 
;		       #'(lambda (uklad) 
;			   (and (apply #'= (mapcar #'kolor uklad))
;				(setq col (kolor (first uklad)))))
;		       (poker.ranks::omaha-zestawy cards table-cards))
;		  (or(member (+ 1 (* 13 col)) cards)   ;czy mam asa na reku
;		     (and (member (+ 1 (* 13 col)) table-cards)  ;czy mam krola na reku
;			  (member (+ 2 (* 13 col)) cards))
;		     (and (member (+ 1 (* 13 col)) table-cards)  ;czy mam dame na reku
;			  (member (+ 2 (* 13 col)) table-cards)
;			  (member (+ 3 (* 13 col)) cards)) ))  )))

(defun omaha-full-nuts?(&optional (cards (cards *game*)) (table-cards (table-cards *game*)))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (H1 (nth 0 reka)) (H2 (nth 1 reka))
	 (H3 (nth 2 reka)) (H4 (nth 3 reka))
	 (T1 (nth 0 stol)) (T2 (nth 1 stol)) (T3 (nth 2 stol))
	 (o (car (omaha-ocena cards table-cards))))
    (when
	(<= o 2) ;kareta?
      (or 
       (= o 1)
       (= H1 H2 T1) 
       (= H2 H3 T1) 
       (= H3 H4 T1) 
       (and (= H1 T1 T2) (= H2 T3) )
       (and (= H1 T1 T2) (= H3 T3) )
       (and (= H1 T1 T2) (= H4 T3) )
       
       (and (= H2 T1 T2) (= H1 T3) )
       (and (= H2 T1 T2) (= H3 T3) )
       (and (= H2 T1 T2) (= H4 T3) )

       (and (= H3 T1 T2) (= H2 T3) )
       (and (= H3 T1 T2) (= H1 T3) )
       (and (= H3 T1 T2) (= H4 T3) )

       (and (= H4 T1 T2) (= H2 T3) )
       (and (= H4 T1 T2) (= H3 T3) )
       (and (= H4 T1 T2) (= H1 T3) ) )))) 


(defun omaha-set?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 t2 t3 t4 t5) g
  (and 
    ;mamy pare na reku:
    (or (= h1 h2) (= h2 h3) (= h3 h4))
    ;zatem:
    (or 
     (= h1 h2 t1) 
     (= h1 h2 t2) 
     (= h1 h2 t3)
     (= h3 h2 t1)
     (= h3 h2 t2)
     (= h3 h2 t3)
     (= h3 h4 t1)
     (= h3 h4 t2)
     (= h3 h4 t3)
     (or  (= h1 h2 t4)  (= h3 h2 t4)  (= h3 h4 t4))
     (or  (= h1 h2 t5) (= h3 h2 t5)  (= h3 h4 t5)  ))))) 

(defun omaha-top-two-pairs?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 t2) g
  (or 
    (and (= h1 t1) (or (= h2 t2) (= h3 t2) (= h4 t2) ))
    (and (= h2 t1) (or (= h3 t2) (= h4 t2)))
    (and (= h3 t1) (= h4 t2))  )))

(defun omaha-overpair?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1) g
    (or
     (and (= h1 h2) (< h1 t1))
     (and (= h2 h3) (< h2 t1))
     (and (= h3 h4) (< h3 t1)) )));mamy na reku pare ktora jest wyzsza od wszystkich kart na stole, np. AA, KK 


(defun omaha-top-set?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 ) g
   (or (= h1 h2 t1) (= h2 h3 t1) (= h3 h4 t1))))

(defun omaha-top-trips?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 t2) g
    (and (= t1 t2)
	 (or (= h1 t1) (= h2 t1) (= h3 t1) (= h4 t1)))))
 

;; temp
(defun brak-koloru-w-ukladzie(karty)
  (< (apply 'max (coerce (wektor-kolorow karty) 'list)) 3))

(defun brak-pary-w-ukladzie(karty)
  (< (apply 'max (coerce (wektor-figur karty) 'list)) 2))

(defun omaha-count-color-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (and (brak-pary-w-ukladzie table)
		   (omaha-color-nuts? g))
	    (incf nr)))))))

(defun omaha-count-not-nut-color-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (= 3 (car (omaha-ocena cards table)))
	    (incf nr)))))))


(defun omaha-str8-nuts-v2(reka stol)
  (let ((figury-stolu  (mapcar #'figura stol))
	(rank (omaha-ocena reka stol)))
    (and (= 4 (car rank))
	 (<=
	  ;(1+ (second rank))
	  (second rank)
	  (apply #'min
		 (mapcar
		  #'(lambda (para)
		      (let  ((str (poker.ranks::street 
				   (wektor-figur (mapcar #'1+ (append para figury-stolu))))))
			    (if str str 100)))
		      (podzbiory-2  
		       (set-difference 
			'(0 1 2 3 4 5 6 7 8 9 10 11 12)
			figury-stolu)) ))))))

(defun omaha-count-str8-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (and (brak-koloru-w-ukladzie table) ;nie ma koloru ani pary na stole
		   (brak-pary-w-ukladzie table)
		   (omaha-str8-nuts-v2 cards  table)) 
	    (incf nr)))))))


(defun omaha-count-not-nut-str8-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (= 4 (car (omaha-ocena cards table))) 
	    (incf nr)))))))      


(defun omaha-count-full-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when (omaha-full-nuts? cards table) (incf nr)))))))


(defun omaha-count-not-nut-full-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when (<= (car (omaha-ocena cards table)) 2) (incf nr)))))))


(defun omaha-count-outs(&optional  (g *game*) )
  (if (table-cards g)
      (let ((outs 0))
	(incf outs (omaha-count-full-outs g))
	(when (and (< (ilosc-kolorow g) 3) (< (ilosc-figur g) 2))
	   (incf outs (omaha-count-str8-outs g)))
	(when (< (ilosc-figur g) 2)  (incf outs (omaha-count-color-outs g)))
	outs)
      0))



;;--------------------------------------------------------
;;---------------- LOGIC TOOLS ---------------------------
;;--------------------------------------------------------

;reguly sa postaci:
;(call 1 id_speech mem1 mem2 ... (<= bets 0)  ... memn)
(defvar *poker-rules* nil)
(defvar *o-pf-levels* nil)
(defvar *o-holecards* nil)

(defun game-memory(&optional (game *game*))
  (append (round-memo game) (game-memo game) (percept-memo game)))

(defmacro <-(&body rule)
  `(push ',rule *poker-rules*))

(defmacro <-game(game smb)
  `(pushnew ',smb (game-memo ,game)))

(defmacro <-round(game smb)
  `(pushnew ',smb (round-memo ,game)))

(defmacro <-percept(game smb)
  `(pushnew ',smb (percept-memo ,game)))

(defun prove-all(game memory symbols)
  (declare (ignore game))
   (every
     #'(lambda(s)
	 (member s memory)
	 ;dodawanie zaprzec:zen
	 ;(let ((str (symbol-name s)))
	  ; (if (eql (char str 0) #\!)
	   ;    (not (member (intern (subseq str 1)) memory))
	    ;   (member s memory)))
	 ;old code
	 ;(if (symbolp s)
	 ;    (member s memory)
	 ;    ;lub postaci (< stake 2)
	 ;    (funcall (symbol-function (first s))
	 ;	      (funcall (symbol-function (second s)) game)
	 ;	      (third s)))
	 )
     symbols))


(defun prove(game sent &optional 
;zwraca akumulator postaci: (list poziom "nazwa")
	     (mem (game-memory game)) 
	     (reguly *poker-rules*)
	     (acc (list 0 'no-rule)))
;  (format t "~A ~A~&" (car reguly) acc)
  (if (or (null reguly) (= 1 (car acc)))
      acc
      (if (and (eq sent (first (car reguly))) 
	       (> (second (car reguly)) (first acc))
	       (prove-all game mem  (cdddr (car reguly))))
	  (prove game sent mem (cdr reguly) (list (second (car reguly)) (third (car reguly)) ))
	  (prove game sent mem (cdr reguly) acc)) ))

(defmacro ?(game smb)
  `(prove ,game ',smb))


(defun prop-prove(smb rules memory)
  (some
   #'(lambda(r)
       (when (eq (car r) smb)
	 (every #'(lambda (smb2)
		    (or (member smb2 memory)
			(prop-prove smb2 rules memory)))
		(cdr r))   ))
   rules))

(defun prop-predicates(rules)
  (let ((result nil))
    (dolist (r rules result)
      (pushnew (car r) result))))

(defun prop-checkall-percepts(rules game)
  (dolist (smb (prop-predicates rules))
    (when (prop-prove smb rules (percept-memo game))
	  (pushnew smb (percept-memo game))  )))

(defun prop-checkall-rounds(rules game)
  (dolist (smb (prop-predicates rules))
    (when (prop-prove smb rules (round-memo game))
	  (pushnew smb (round-memo game))  )))

;;-------------------------------------------------------------
