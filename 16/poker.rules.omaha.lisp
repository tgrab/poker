;;(load "poker.rules.lisp")

;;--------------------------------------------------------------------------------------
;;----------------------------- Decision Rules -----------------------------------------
;;--------------------------------------------------------------------------------------


;;PRE-FLOP
(<- (decision call ?why)  (game-kind omaha) (round preflop)  (omaha-preflop ?why))
(<- (decision fuzzy-call ?level ?why)  (game-kind omaha) (round preflop)  (omaha-preflop-fuzzy ?level ?why))

;;FLOP
(<- (decision call ?why)  (game-kind omaha) (round flop) (omaha-flop call ?why))
(<- (decision fuzzy-call ?level ?why)  (game-kind omaha) (round flop)  (omaha-flop-fuzzy-call ?level ?why))
(<- (decision potbet2 ?why)  (game-kind omaha) (round flop) (omaha-flop potbet2 ?why))


;;---------- przydatne definicje -----------------

;; 1..52 = 1+13*kolor+figura
;; A K Q J 10 9 8 7 6 5 4  3  2
;; 0 1 2 3 4  5 6 7 8 9 10 11 12

(<- (stake big)
    (<= (pot-odds) 2.5))

(<- (stake average)
    (> (pot-odds) 2.5)
    (<= (pot-odds) 4))

(<- (stake average)
    (stake small) )

(<- (stake small)
    (> (pot-odds) 4))

(<- (stake small)
    (stake normal) )

(<- (stake normal)
    (progn  (and +blind+ (<= (stake) (* 2 +blind+)))) )

(<- (stake minimal)
    (<= (stake)  +blind+))


(<- (commitment minimal)
    (= +blind+ (round-balance)))


(<- (commitment big)
    (<= (* 4 +blind+) (round-balance)))

(<- (same-color ?a ?b) 
    (= (kolor ?a) (kolor ?b)))

(<- (same-color ?a ?b ?c ?d) 
    (or (= (kolor ?a) (kolor ?b))
	(= (kolor ?a) (kolor ?c))
	(= (kolor ?a) (kolor ?d))))

; K Q , 9 8
(<- (connectors ?a ?b) 
    (= (1+ (figura ?a)) (figura ?b)))

; K Q , K J
(<- (one-gap ?a ?b)
    (<= (abs (- (figura ?a) (figura ?b)))  2))

(<- (one-gap ?a ?b ?c)
    (connectors ?a ?b)
    (one-gap ?b ?c))


(<- (rundown ?a ?b ?c ?d)
    (str8 ?a ?b ?c)
    (one-gap ?d ?a))

(<- (rundown ?a ?b ?c ?d)
    (str8 ?a ?b ?c)
    (one-gap ?c ?d))
    

(<- (str8 ?a ?b ?c) 
    (= (1+ (figura ?a)) (figura ?b))
    (= (1+ (figura ?b)) (figura ?c)))

(<- (str8 ?a ?b ?c ?d) 
    (= (1+ (figura ?a)) (figura ?b))  
    (= (1+ (figura ?b)) (figura ?c))
    (= (1+ (figura ?c)) (figura ?d)))

;;preflop defs.:
(<- (connected-hand)
    (h1f ?x)
    (h4f ?y)
    (<= (abs (- ?x ?y)) 5))

(<- (3connected-hand)
    (h1f ?x)
    (h3f ?y)
    (<= (abs (- ?x ?y)) 4))


(<- (no-dangler)
    (h1f ?x)
    (h2f ?y)
    (h3f ?w)
    (h4f ?z)
    (<= (abs (- ?x ?y)) 3)
    (<= (abs (- ?w ?z)) 3)  )

(<- (big-pair-hand)
    (hand ?a ?b ? ?) 
    (pair ?a ?b)
    (big-card ?a))

(<- (middle-pair-hand)
    (hand ?a ?b ? ?) 
    (pair ?a ?b)
    (middle-card ?a))

(<- (middle-pair-hand)
    (big-pair-hand))
 


;;--------------------------------------------------------------------------------------
;;----------------------------- PRE-FLOP -----------------------------------------------
;;--------------------------------------------------------------------------------------

;Group 5
(<- (group5 G5-ace-double-suited) (hand ?a ?b ?c ?d) (pair ?a ?b) (ace ?a) (same-color ?a ?c) (same-color ?b ?d))
(<- (group5 G5-aces-suited-with-connectors) (hand ?a ?b ?c ?d) (pair ?a ?b) (ace ?a) (same-color ?a ?c) (connectors ?c ?d))

;Group4+
(<- (group4+ G4+aces-with-color) (hand ?a ?b ?c ?d) (pair ?a ?b) (ace ?a) (same-color ?a ?c))


;Group 4
(<- (group4 G4-rundown4connecting) (hand ?a ?b ?c ?d) (rundown ?a ?b ?c ?d))
(<- (group4 G4-bare-aces) (hand ?a ?b ? ?) (pair ?a ?b) (ace ?a))
(<- (group4 G4-bare-kings) (hand ?a ?b ? ?) (pair ?a ?b) (king ?a))
(<- (group4 G4-pair-suited-ace) (hand ?a ?b ?c ?d) (ace ?a) (pair ?b ?c) (same-color ?a ?b ?c ?d) )

;Group 3
;Ace suited
(<- (group3 G3-ace-suited) (hand ?a ?b ? ?) (ace ?a) (same-color ?a ?b))
;Middle Pair with all cards connected:
(<- (group3 G3-pair-all-connected)     (connected-hand) (middle-pair-hand) )
;Big pair:
(<- (group3 G3-big-pair) (hand ?a ?b ?c ?) (pair ?a ?b) (big-card ?a)   )
;Not small connected hand:
(<- (group3 G3-connected-hand) (connected-hand) (h4 ?a) (middle-card ?a)   )

;Group 2
(<- (group2 G2-3big-one-gap) (hand ?a ?b ?c ?) (middle-card ?a) (one-gap ?a ?b ?c) (no-dangler));chyba j.w.

;           ----- CALL with:
;small blind with a pair:
(<- (omaha-preflop sb-with-pair) (hand ?a ?b ?c ?) (pair ?a ?b) (progn  (and +blind+ (< (stake) +blind+)))  )

;shorthand -> middle pair
(<- (omaha-preflop shorth-middle-pair) (table-size shorthand) (stake normal) (hand ?a ?b ?c ?) (pair ?a ?b) (middle-card ?a)    )
(<- (omaha-preflop short-3big-one-gap) (table-size shorthand) (hand ?a ?b ?c ?) (middle-card ?a) (one-gap ?a ?b ?c))

;agressive play-> Ace suited and  connectors
(<- (omaha-preflop agressive-ace-suited-connectors) (style agressive) (hand ?a ?b ?c ?) (ace ?a) (same-color ?a ?b) (3connected-hand))

(<- (omaha-preflop ?why) (stake minimal) (group2 ?why))
(<- (omaha-preflop ?why) (stake normal) (group3 ?why))
(<- (omaha-preflop ?why) (stake normal) (group4 ?why))
(<- (omaha-preflop ?why) (>= (balance) +blind+) (stake average) (group4 ?why))
(<- (omaha-preflop ?why) (stake average) (group4+ ?why))
(<- (omaha-preflop ?why) (group5 ?why))

; ----------- Fuzzy call
(<- (omaha-preflop-fuzzy 0.3 fuzzy-1) (commitment ?) (stake big) (<= (stake) (* 4 +blind+)) )


;;-------------------------------------------------------------------------------------
;;-------------------------- OMAHA FLOP -----------------------------------------------
;;-------------------------------------------------------------------------------------

(<- (omaha-flop potbet2 flop-nuts-set-good-texture)  (progn (omaha-top-set?)) )
(<- (omaha-flop potbet2 trips-no-bets)  (rank trips) (bets 0)  )

(<- (omaha-flop call call-trips-one-bet)  (rank trips) (bets 1)  )
(<- (omaha-flop call call-dojscia) (plusp (omaha-count-outs))  (< (odds (omaha-count-outs)) (pot-odds) )  )

(<- (omaha-flop-fuzzy-call 0.6 fuzzy-dojscia) (> (omaha-count-outs) 6) )

;gdy na stole lezy para a na reku wyzsza to call 1424994347