(defpackage :poker.defs
  (:use :cl :anaphor :poker.ranks)
  (:export :game :agent :game-id :table-name :game-date :game-time
	   :small-blind :big-blind :button :history :players :table-size
	   :table-cards :cards :agent-position :still-playing? :money-list
	   :stake :actions-list :active-player :get-player
	   :player :name :money
	   :*kolory* :*figury* :karta->kolor :karta->figura
	   :karta->napis :karty->napisy
	   :napis->karta :napisy->karty
	   :rank= :rank< :rank> :rank>= :rank<=
	   :pre-flop :with-flop :post-flop :flop :turn :river 
	   :heads-up :shorthand :fulltable :on-small-blind :on-big-blind
	   :on-early-pos :on-middle-pos :on-late-pos :no-bets :one-bet
	   :with-bets :with-raise :re-raise :sklansky :agent-value
	   :table-value :one-pair :two-pairs :trips :street :color
	   :fullhouse :quads :straight-color
	   :H1 :H2 :H3 :H4 :T1 :T2 :T3 :T4 :T5))

(in-package :poker.defs)

;;-------- OPISY KART ----------------------------------------

(defvar *kolory* '(S H D C))
(defvar *figury* '(A K Q J T 9 8 7 6 5 4 3 2))

;karty to liczby od 1 do 52, 1=As Pik,2=Krol Pik,52=2 Trefl
;kolory: 0=Pik, 1=Kier, 2=Karo, 3=Trefl
;figury: 0=As, 1=Krol, 12=Dwojka

; karta to zawsze liczba z 1..52
(defun karta->kolor(liczba)
  (floor (1- liczba) 13))

(defun karta->figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a zeby nie bylo ostrzezen
   b))

(defun karta->napis (liczba)
   (format nil "~A~A" 
	   (nth (karta->figura liczba) *figury*)
	   (nth (karta->kolor liczba) *kolory*)    ))

(defun karty->napisy(lista-liczb)
 (mapcar #'karta->napis lista-liczb))    


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

;;-------------------- ----------------------------------------

(defclass game ()
  ((id :accessor game-id)
   (name :accessor table-name)
   (date :accessor game-date)
   (time :accessor game-time)
   (sblind :initarg :sblind :reader small-blind)
   (bblind :initarg :bblind :reader big-blind)
   (button :initform 0 :accessor button)
   (history :initform nil :accessor history)
   (players :initform nil :accessor players)
   (size  :initform 6 :initarg :size :reader table-size)
   (cards :initform nil :accessor table-cards)))

(defclass player ()
  ((name  :initform "Anonim" :initarg :name :accessor name)
   (money :initform nil :initarg :money :accessor money)
   (cards :initform nil :initarg :cards :accessor cards)))

(defclass agent ( player )
  ((game :initform (make-instance 'game) :initarg :game :accessor game)
   (game-kind :initform 'holdem :initarg :game-kind :reader game-kind)
   (pos :initform 0 :accessor agent-position)
   (still-playing :initform nil :accessor still-playing?)
   (money-list :initform nil :accessor money-list)
   (stake :accessor stake)
   (actions :initform nil :accessor actions-list)
   (active-player :initform 0 :accessor active-player)))

(defmethod print-object((g game) str)
  (aif (table-cards g) 
       (format str " table cards: ~A " (karty->napisy it) )
       (format str " pre-flop "))
  (awhen (players g) (format str " players: [~{~A ~}]" it))
  (format str " history: [~{~A ~}]" (history g)))

(defmethod print-object((a player) str)
  (format str "~A " (name a))
  (awhen (cards a) (format str ", hand: ~A" (karty->napisy it )))
  (awhen (money a) (format str ", money: ~A" (money a))))

(defmethod print-object ((a agent) str)
  (call-next-method)
  (if  (still-playing? a)
       (princ " is playing " str)
       (princ " is not playing " str))
  (format str "~%Game: ~A~%" (game a) )
  (format str " active: ~A" (active-player a))
  (format str " money-list: [~{~A ~}]" (money-list a))
  (format str " actions-list: [~{~A ~}]" (actions-list a)) )


(defmethod table-size((a agent))
 (or (table-size (game a))
     (length (players (game agent)))))

(defmethod table-cards((a agent))
  (table-cards (game a)))

(defmethod history((a agent))
  (history (game a)))


(defun get-player (name game)
 "return player object from his name"
  (find name (players game) :test #'string= :key #'name))

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------

(defun sklansky-group(karta1 karta2)
 "karta1, karta2 to  karty na rece"
  (macrolet
     ((oba(&body wartosci)
       `(or ,@(mapcar 
              (lambda (w)  `(and (eq ,w figura1) (eq ,w figura2)) )
               wartosci) ))
       (oba2(wart1 wart2)
        `(or(and (eq ,wart2 figura1) (eq ,wart1 figura2))
            (and (eq ,wart1 figura1) (eq ,wart2 figura2))) )
      (same-color(warunek)
          `(and (= kolor1 kolor2) ,warunek) ))
 (let ((kolor1 (karta->kolor karta1)) 
       (figura1 (nth (karta->figura karta1) *figury*))
       (kolor2 (karta->kolor karta2))
       (figura2 (nth (karta->figura karta2) *figury*)))
 (cond
  ((oba 'A 'K 'Q 'J)  1)
  ((same-color (oba2 'A 'K)) 1)
  ((oba 'T) 2)
  ((same-color (or (oba2 'A 'Q) (oba2 'A 'J) (oba2 'K 'Q) ) ) 2)
  ((oba2 'A 'K) 2)  
  ((oba '9) 3)
  ((same-color (or (oba2 'J 'T) (oba2 'Q 'J) (oba2 'K 'J) (oba2 'A 'T))) 3)
  ((oba2 'A 'Q) 3)
  ((oba '8) 4)
  ((or (oba2 'K 'Q) (oba2 'A 'J)) 4)
  ((same-color (or (oba2 'T '9) (oba2 'Q 'T) (oba2 '9 '8) (oba2 'J '9))) 4)
  ((oba '7) 5)
  ((or (oba2 'K 'J) (oba2 'Q 'J) (oba2 'J 'T)) 5)
  ((same-color (or (oba2 '8 '7) (oba2 'Q '9) (oba2 '10 '8) (oba2 '7 '6) (oba2 '9 '7)(oba2 '6 '5))) 5)
  ((same-color (or (eq figura1 'A)(eq figura2 'A))) 5)
  ((oba '6 '5) 6)
  ((or (oba2 'A 'T) (oba2 'K 'T) (oba2 'Q 'T)) 6)
  ((same-color (or (oba2 '8 '6) (oba2 '5 '4) (oba2 'K '9) (oba2 'J '8) (oba2 '7 '5))) 6)
  ((oba '4 '3 '2) 7)
  ((or (oba2 'J '9) (oba2 'T '9) (oba2 '9 '8)) 7)
  ((same-color (or (oba2 '6 '4) (oba2 '5 '3) (oba2 '4 '3) (oba2 'T '7) (oba2 'Q '8))) 7)
  ((same-color (or (eq figura1 'K)(eq figura2 'K))) 7)

  ((or (oba2 '8 '7) (oba2 'A '9) (oba2 'Q '9) (oba2 '7 '6) (oba2 'J '8) (oba2 '6 '5)
       (oba2 '5'4) (oba2 'K '9) (oba2 'T '8)) 8)
  ((same-color (or (oba2 '4 '2) (oba2 '3 '2) (oba2 '9 '6) (oba2 '8 '5) (oba2 'J '7) (oba2 '7 '4))) 8) ))))


(defun game-round( agent )
  (case (length (table-cards agent))
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )

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

(defun round-history(agent 
		     &optional 
		     (r (game-round agent)) 
		     (h (history agent)))
  (cond
    ((eq r 'pre-flop)  (if (pre-flop agent) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 

(defun count-bets(agent &optional (r (game-round agent)))
 (count-if #'(lambda(el)
	       (and (consp el)
		    (eq (car el) 'b)))
	   (round-history agent r)))

(defun position->symbol(agent)
  (let ((my-pos (agent-position agent))
	(ts (table-size agent)))
    (cond
      ((= 0 my-pos)  'small-blind)
      ((= 1 my-pos)  'big-blind)
      ((or (= (1- ts) my-pos)
	   (= (- ts 2) my-pos)) 'late-pos)
      ((<= ts 5) 'late-pos)
      ((and (>= ts 9)(< my-pos 5)) 'early-pos)
      ((and (>= ts 7)(< my-pos 4)) 'early-pos)
      ((and (= ts 6)(= my-pos 2)) 'early-pos)
      (t 'middle-pos)    )))

(defun s->val(symb)
  (case symb
    ('para 7)
    ('dwie-pary 6)
    ('trojka 5)
    ('strit 4)
    ('kolor  3)
    ('full  2)))

(defun sortuj-karty(lista)
  (sort (mapcar 
	       #'karta->figura 
	       (copy-list lista))
	      #'<))

;;----------------------------------------------------------
;;---------- predykaty wykorzystywane do konstrukcji regul:
;;---------- wszystkie musza byc wyeksportowane! -----------

;--------- pomocnicze:

; (rank= agent-value 'para)
; (rank= table-value 'para)
(defun rank=(lista smb)
  (= (car lista) (s->val smb)))

(defun rank<(lista smb)
  (> (car lista) (s->val smb)))

(defun rank<(lista smb)
  (> (car lista) (s->val smb)))

(defun rank<=(lista smb)
  (>= (car lista) (s->val smb)))

(defun rank<=(lista smb)
  (>= (car lista) (s->val smb)))

;--------- funkcje agenta:

(defun pre-flop(agent)
  (null (table-cards agent)))

(defun with-flop(agent)
  (table-cards agent))

(defun post-flop(agent)
  (> (length (table-cards agent)) 3))

(defun flop(agent)
  (= 3 (length (table-cards agent))))

(defun turn(agent)
  (= 4 (length (table-cards agent))))

(defun river(agent)
  (= 5 (length (table-cards agent))))

(defun heads-up(agent)
  (= 2 (table-size agent)))

(defun shorthand(agent)
  (and
   (> (table-size agent) 2)
   (< (table-size agent) 6)))

(defun fulltable(agent)
  (>= (table-size agent) 6))

(defun on-small-blind(agent)
 (eq (position->symbol agent) 'small-blind))

(defun on-big-blind(agent)
 (eq (position->symbol agent) 'big-blind))

(defun on-early-pos(agent)
 (eq (position->symbol agent) 'early-pos))

(defun on-middle-pos(agent)
 (eq (position->symbol agent) 'middle-blind))

(defun on-late-pos(agent)
 (eq (position->symbol agent) 'late-pos))

(defun no-bets(agent)
  (zerop (count-bets agent)))

(defun one-bet(agent)
  (= 1 (count-bets agent)))

(defun with-bets(agent)
  (plusp (count-bets agent)))

(defun with-raise(agent)
  (>= (count-bets agent) 2))

(defun re-raise(agent)
  (> (count-bets agent) 2))

(defun sklansky(agent)
  (awhen (cards agent)
    (sklansky-group (car it) (cadr it))))

(defun agent-value(agent)
  (if (eq 'holdem (game-kind agent))
      (ocena (append (cards agent)
		     (table-cards agent)))
      (omaha-ocena (cards agent) (table-cards))))

(defun table-value(agent)
  (ocena (table-cards agent)))

(defun one-pair(agent)
  (= (car (agent-value agent)) 7))

(defun two-pairs(agent)
  (= (car (agent-value agent)) 6))

(defun trips(agent)
  (= (car (agent-value agent)) 5))

(defun street(agent)
  (= (car (agent-value agent)) 4))

(defun color(agent)
  (= (car (agent-value agent)) 3))

(defun fullhouse(agent)
  (= (car (agent-value agent)) 2))

(defun quads(agent)
  (= (car (agent-value agent)) 1))

(defun straight-color(agent)
  (= (car (agent-value agent)) 0))

(defun H1(agent)
 (nth 0 (sortuj-karty (cards agent))))

(defun H2(agent)
 (nth 1 (sortuj-karty (cards agent))))

(defun H3(agent)
 (nth 2 (sortuj-karty (cards agent))))

(defun H4(agent)
 (nth 3 (sortuj-karty (cards agent))))

(defun T1(agent)
 (nth 0 (sortuj-karty (table-cards agent))))

(defun T2(agent)
 (nth 1 (sortuj-karty (table-cards agent))))

(defun T3(agent)
 (nth 2 (sortuj-karty (table-cards agent))))

(defun T4(agent)
 (nth 3 (sortuj-karty (table-cards agent))))

(defun T5(agent)
 (nth 4 (sortuj-karty (table-cards agent))))
