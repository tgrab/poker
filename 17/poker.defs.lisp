(defclass game()
  ((pot :initform 0 :accessor pot)
   (stake :initform 0 :accessor stake)
   (blind :initform 0 :accessor blind)
   (kind :initform 2 :initarg :kind :accessor kind) ;fixed
   (balance :initform (make-array 10 :initial-element 0) :accessor balance)
   (total-balance :initform (make-array 10 :initial-element 0) :accessor total-balance)
   (history :initform nil :accessor history)
   (player-history :initform (make-array 10 :initial-element nil) :accessor player-history)

   (free-seat :initform 0 :accessor free-seat)
   (agent-name :initform "grabola" :accessor agent-name)
   (agent-pos :initform nil :accessor agent-pos)
   (name->seat :initform nil :accessor name->seat)
   (table-size :initform 6 :accessor table-size :allocation :class) ;?! ustalane w poprzedniej grze na flopie!
   (taken-seats :initform (make-array 10 :initial-element nil) :accessor taken-seats)
   (active-players :initform (make-array 10 :initial-element nil) :accessor active-players)

   (table-cards :initarg :table-cards :initform nil :accessor table-cards)
   (cards :initform nil :accessor cards)
   ;-------- Logic properties:
   (predicates :initform nil :allocation :class :accessor predicates)
   ;levels of memory:
   (rules :initform (make-hash-table) :allocation :class :accessor rules)
   (memo :initform (make-hash-table) :allocation :class :accessor memo)
   (game-memo :initform (make-hash-table) :accessor game-memo)
   (round-memo :initform (make-hash-table) :accessor round-memo)
   (percept-memo :initform (make-hash-table) :accessor percept-memo)   ))

(defclass game-log(game)
  ((id :initform nil :initarg :id :accessor id)
   (czas :initform nil :initarg :czas :accessor czas)
   (table-name :initform nil :initarg :table-name :accessor table-name)
   (percept-nr :initform -1 :accessor percept-nr)
   (players-log :initform nil :initarg :players-log :accessor players-log) ;jaki gracz gdzie siedzi
   (table-cards-log :initarg :table-cards-log :initform nil :accessor table-cards-log)
   (money-log :initform (make-array 10 :initial-element 0) :initarg :money-log :accessor money-log)
   (cards-log :initform (make-array 10 :initial-element nil) :initarg :cards-log :accessor cards-log)
   (history-log :initform "" :initarg :history-log :accessor history-log) ;dotyczy graczy z players-log
   ))

(defvar +game+ (make-instance 'game-log))
(defvar +games-list+ nil)

(defvar *calendar* '((|January| . 31) (|February| . 28) (|March| . 31)
			   (|April| . 30) (|May| . 31)(|June| . 30)
			   (|July| . 31)(|August| . 31) (|September| . 30)
			   (|October| . 31) (|November| . 30)(|December| . 31) ))

(defparameter +say+ nil)
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
	 

(defun say-text(text)
  (send-message #(127 0 0 1) 1314  (format nil "(SayText \" ~A \")" text) ))

