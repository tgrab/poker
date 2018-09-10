(in-package :poker)

(clear-db)
(<=into (memo +game+) (table-size shorthand))
;(load "rules_fixed.lisp")

;;---------------------------  COMMON RULES  --------------------------------------
(<- (omaha-call no-bets) (stake zero))
(<- (omaha-preflop-call no-bets) (stake zero))


(<- (stake minimal) (stake zero))
(<- (stake normal) (stake minimal))


(<- (table-safe almost) (table-safe yes))
(<- (table-safe yes)    (table-colors no) (table-pairs no) (table-str8 no))
(<- (table-safe almost) (table-str8 yes) (table-colors no) (table-pairs no) )
(<- (table-safe no)     (table-colors yes))
(<- (table-safe no)     (table-pairs yes))
(<- (table-safe no)     (table-str8 yes))

(<- (hand top-20%) (hand top-10%))
(<- (hand top-30%) (hand top-20%))
(<- (hand 3-str8) (hand str8))
(<- (hand one-pair) (hand two-pairs))  
(<- (hand one-pair) (hand big-pair))     
(<- (hand one-pair) (hand middle-pair))
(<- (hand one-pair) (hand small-pair))

(<- (rank top-trips) (rank top-set))
(<- (odds implied)   (odds good))

;;---------------------------  PRE FLOP ---------------------------------------
(<- (omaha-preflop-call sblind)                      (position s-blind) (bets 0)  (hand connected))
(<- (omaha-preflop-call sblind4)                     (position s-blind) (bets 0)  (hand one-pair))
(<- (omaha-preflop-call sblind2)                     (position s-blind) (bets 0)  (hand ace-suited))
(<- (omaha-preflop-call sblind3)                     (position s-blind) (bets 0)  (< h3 4))


(<- (omaha-preflop-call str8)                        (bets 0) (hand str8))
(<- (omaha-preflop-call 2p)                          (bets 0) (hand two-pairs))
(<- (omaha-preflop-call 3_str8_nd)                   (bets 0) (hand 3-str8) (hand no-dangler))
(<- (omaha-preflop-call a_suited_c)                  (bets 0) (hand ace-suited) (hand connectors))
(<- (omaha-preflop-call a_suited2)                   (bets 0) (hand ace-suited) (position late))
(<- (omaha-preflop-call big_pair2)                   (bets 0) (position late) (hand big-pair))

;j.w.
(<- (omaha-preflop-call str8)                        (stake minimal) (hand str8))
(<- (omaha-preflop-call 3_str8_nd)                   (stake minimal) (hand 3-str8) (hand no-dangler))
(<- (omaha-preflop-call a_suited_c)                  (stake minimal) (hand ace-suited) (hand connectors))
(<- (omaha-preflop-call a_suited2)                   (stake minimal) (hand ace-suited) (position late))
(<- (omaha-preflop-call big_pair2)                   (stake minimal) (position late) (hand big-pair))

(<- (omaha-preflop-call pair_ace_suited)             (<= bets 1) (stake normal) (hand one-pair) (hand ace-suited))
(<- (omaha-preflop-call pair_ace_suited)             (<= bets 1) (hand big-pair) (hand ace-suited))
(<- (omaha-preflop-call aces)                        (<= bets 1) (hand big-pair) (= h1 h2 0))
(<- (omaha-preflop-call two_pairs)                   (<= bets 1) (hand two-pairs) (< h4 8))


(<- (omaha-preflop-fuzzy-call 0.5 middle_pair)       (bets 0) (table-style passive) (hand middle-pair) (hand no-dangler) )
(<- (omaha-preflop-fuzzy-call 0.7 big_pair)          (bets 0) (table-style not-agressive) (hand big-pair))
(<- (omaha-preflop-fuzzy-call 0.7 3_str8)            (bets 0) (table-style not-agressive) (hand 3-str8))
(<- (omaha-preflop-fuzzy-call 0.8 a_suited)          (bets 0) (table-style not-agressive) (hand ace-suited))



;;---------------------------  UNIVERSAL ---------------------------------------
(<- (omaha-potbet1  top_trips)	                (rank top-trips) (bets 0) (table-str8 no) (table-colors no) )
(<- (omaha-potbet1  fullhouse_nobets)	        (rank fullhouse) (bets 0) (table-pairs 2))
(<- (omaha-potbet1  set_table_really_safe)	(table-safe yes) (rank set) (<= bets 1))
(<- (omaha-potbet1  nuts)                	(rank nuts) (> bets 0))


(<- (omaha-call  univ_trips_small_stake)	(stake minimal) (rank trips))
(<- (omaha-call  univ_trips_small_stake)	(stake normal) (rank trips) (table-colors no) (table-str8 no))
(<- (omaha-call  univ_fh)	                (rank fullhouse))
;(<- (omaha-call  univ_color_nuts)               (stake minimal) (rank color) (e! (omaha-color-nuts?))) ;moze lezec para


;;---------------------------  FLOP ---------------------------------------
(<- (omaha-flop-potbet1 1597920888_6)		      (table-safe almost) (bets 0) (outs ?some) (rank str8))
(<- (omaha-flop-potbet1 1065715351_11)		      (rank fullhouse))

;^-- universal potbet1

(<- (omaha-flop-potbet2  nuts)	                      (bets 0) (rank nuts)) ;slowplaying
(<- (omaha-flop-potbet2 1597916354_6)		      (table-str8 no) (table-colors no) (bets 0) (rank top-trips))
(<- (omaha-flop-potbet2 color_outs)		      (table-safe yes) (outs ?o1) (outs color ?o2) (bets 0))

(<- (omaha-flop-fuzzy-potbet1 0.7  top_pairs)         (players-left 2) (bets 0) (rank top-two-pairs) (comments last-to-act) ) 
(<- (omaha-flop-fuzzy-potbet1 0.6  safe_top_trips)    (rank top-trips) (<= bets 1) (table-str8 no) (table-colors no)) 
(<- (omaha-flop-fuzzy-potbet1 0.4  trips1)            (rank trips) (bets 1) (stake normal) (rank ace-kicker) (table-str8 no) (table-colors no)) 

(<- (omaha-flop-call chasing__implied)            (odds implied))
(<- (omaha-flop-call set_small_stake)                 (stake small)    (rank set)   (table-safe no))
(<- (omaha-flop-call overpair)                        (stake minimal) (rank overpair) (table-pairs yes))
(<- (omaha-flop-call top_trips)                       (rank top-trips))
(<- (omaha-flop-call trips)                       (bets 1) (rank trips))
(<- (omaha-flop-call two_pairs)                       (stake minimal) (rank two-pairs) (table-safe yes))

;^-- universal call

(<- (omaha-flop-fuzzy-call 0.5  not_nut_str8)	      (rank str8) (table-safe almost) (bets 0) ) 
(<- (omaha-flop-fuzzy-call 0.5  univ_trips)	      (rank top-trips) ) 
(<- (omaha-flop-fuzzy-call 0.5 chase_color)           (stake minimal) (outs color ?some) (<= players-left 3)) ;nie nuts
(<- (omaha-flop-fuzzy-call 0.8 chasing_implied)       (odds implied)   (stake normal) (table-safe yes))
(<- (omaha-flop-fuzzy-call 0.9 fuzzy_safe_top_pairs)  (bets 1) (table-safe yes) (rank top-two-pairs))
(<- (omaha-flop-fuzzy-call 0.7 fuzzy_str8)            (rank str8) (rank not-nuts) (table-safe almost) (stake big))
(<- (omaha-flop-fuzzy-call 0.5 fuzzy_overpair)        (rank overpair) (table-safe yes) (stake minimal))




;;---------------------------  TURN  ---------------------------------------
(<- (omaha-turn-potbet1  nuts)	                         (rank nuts))
(<- (omaha-turn-potbet1  fullhouse_small-stake)          (rank fullhouse) (stake minimal)  (table-pairs 2))

;^-- universal potbet1

(<- (omaha-turn-fuzzy-potbet1 0.6  safe_top_trips)       (rank top-trips) (<= bets 1) (table-str8 no) (table-colors no)) 
(<- (omaha-turn-fuzzy-potbet1 0.8  safe_trips)           (rank trips)(bets 0)(table-str8 no)(table-colors no)(on-flop bets 0)(rank ace-kicker)) 


(<- (omaha-turn-call chasing_implied)                    (odds implied))
(<- (omaha-turn-call chase_color)                        (stake minimal) (outs color ?some))

;^-- universal call

(<- (omaha-turn-fuzzy-call 0.5  not_nut_str8)	         (rank str8) (table-safe almost) (bets 1) (stake normal) ) 
(<- (omaha-turn-fuzzy-call 0.5  univ_trips)		 (rank top-trips) ) 
(<- (omaha-turn-fuzzy-call 0.4  strong_implied)          (odds strong-implied) (bets 1))
(<- (omaha-turn-fuzzy-call 0.7 fuzzy_set_small_stake)    (stake minimal) (rank set) (table-safe no))
(<- (omaha-turn-fuzzy-call 0.5 fuzzy_two-pairs)          (stake minimal) (rank two-pairs) (rank top-pair) (table-safe yes))
(<- (omaha-turn-fuzzy-call 0.5 not-nuts_str8)            (rank str8)  (stake big) (table-safe almost)  (rank not-nuts))




;;---------------------------  RIVER  ---------------------------------------
(<- (omaha-river-potbet1  nuts)	                         (rank nuts))

;^-- universal potbet1

(<- (omaha-river-fuzzy-potbet1 0.6  safe_top_trips)    (rank top-trips) (<= bets 1) (table-str8 no) (table-colors no)) 

;^-- universal call
(<- (omaha-river-call  nut_color)	                (rank color nuts) (bets 1) (stake normal)) 

(<- (omaha-river-fuzzy-call 0.4 nut_color)	        (rank color nuts) (bets 1))
(<- (omaha-river-fuzzy-call 0.5 not_nut_str8)	        (rank str8) (table-safe almost) (bets 1) (stake normal)) 
(<- (omaha-river-fuzzy-call 0.5 univ_trips)		(rank top-trips) ) 
(<- (omaha-river-fuzzy-call 0.3 1597909290_22)          (stake minimal) (rank set) (table-safe no))
(<- (omaha-river-fuzzy-call 0.5 1597927170_18)          (stake minimal) (rank top-pair) (table-colors no) (table-str8 no))
(<- (omaha-river-fuzzy-call 0.3 1065715610_str8)        (rank str8) (table-safe almost)  (stake big) (rank not-nuts)  )
