(in-package :poker)


;sluza do ustalania parametrow nowych gier
(defvar *agent-name* "Grabol")
(defvar *table-name* "Unknown")
(defvar *casino*     "crypto")
(defvar *kind* 3) ;3omaha pl,2holdem nl,1holdem fixed

(defvar *game* nil)

;;----- CORTEX ------------------------
;klasy potrzebne do badan statystycznych
(defclass cortex()
 ((preflop-bets :initform 0 :accessor preflop-bets)	))

(defclass game-cortex(cortex)
 ((on-flop-players :initform 0 :accessor on-flop-players) 	))

(defclass player-cortex(cortex)
 ((vol-in-pot :initform nil :accessor vol-in-pot)
  (flop-bet :initform nil :accessor flop-bet)  ))

;klasy wykorzystujace dane z cortex na podstawie historii gier
;zawiraja pr-stwa roznych zagran
(defclass profile()
  ((vip :initform 0 :accessor vip :documentation "voluntarly in pot")
   (pfr :initform 0 :accessor pfr :documentation "pre flop raise")))

(defclass player-profile(profile)
  ((cont-bet :initform nil :accessor cont-bet)))


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
 (gain :initform 0 :accessor gain) 
 (profile :initform (make-instance 'player-profile) :accessor profile)
 (cortex  :initform (make-instance 'player-cortex) :initarg :cortex :accessor cortex   )  ))

; dane immutable dla gry
(defclass game-info()
  ((casino :initform *casino*  :initarg :casino :accessor casino) 
   (id :initform "0" :initarg :id :accessor id)
   (czas :initform (get-universal-time) :initarg :czas :accessor czas)
   (kind :initform *kind* :initarg :kind :accessor kind) 
   (blind :initform 0 :initarg :blind :accessor blind)
   (table-name :initform *table-name* :initarg :table-name :accessor table-name) 
   (table-size :initform 6 :initarg :table-size :accessor table-size)
   (agent-name :initform *agent-name* :initarg :agent-name :accessor agent-name)
   (agent-pos :initform nil :initarg :agent-pos :accessor agent-pos)   ))


; stan gry - wartosci liczbowe:
(defclass game-state()
((players :initform (make-array 10 :initial-element nil) :initarg :players :accessor players)
 (free-seat :initform 0 :accessor free-seat) 
 (table-cards :initarg :table-cards :initform nil :accessor table-cards) 
 (cards :initarg :cards :initform nil :accessor cards) 
 (history :initform nil :initarg :history :accessor history)
 (pot :initform 0 :accessor pot)
 (stake :initform 0 :accessor stake) ))

(defclass game-vars()
  ((bets :initform 0  :accessor bets)
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
   (hc4 :accessor hc4 )))



(defclass game(game-info game-state game-vars)
((prev-game :initform nil :initarg :prev-game :accessor prev-game)
 (profile :initform nil :accessor profile)
 (cortex  :initform (make-instance 'game-cortex) :accessor cortex)
 ;listy symboli:
 (game-memo  :initform nil  
	     :accessor game-memo 
	     :documentation "immutable dla calej gry, ustawiane w new-game z cortexow")
 (round-memo  :initform nil
	      :accessor round-memo 
	      :documentation "tworzone na pocz. rund")
 (percept-memo  :initform nil  
		:accessor percept-memo 
		:documentation "ustawiane przed kazda akcja") ))



(defclass game-log(game)
  ((db-players :initform nil :accessor db-players)
   (db-history :initform nil :accessor db-history)
   (db-log :initform nil :accessor db-log)
   (percept-nr :initform -1 :accessor percept-nr )))




(defclass games-list()
 ((db-games :initform nil :accessor db-games) ;lista par id,czas
  (games :initform nil :accessor games)
  (head :initform nil :accessor head)) )

(defvar *gameslist* (make-instance 'games-list))


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

(defmethod games-history((g game))
 (if (null (prev-game g))
     nil
     (cons (prev-game g) (games-history (prev-game g)))))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------

(defmethod print-object((c player-cortex) str)
  (format str "/~A/~A/~A/" (preflop-bets c)(vol-in-pot c) (flop-bet c)))

(defmethod print-object((p player) str)
  (format str "#P(~A@~A~A{~A ~A ~A$}~A)" 
	  (name p)(seat p) (if (cards p) (format nil "_~A_" (karty->napisy (cards p))) "") 
	  (money p) (total-balance p) (balance p)
	  (cortex p)))

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

