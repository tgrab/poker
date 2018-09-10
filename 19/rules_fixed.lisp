;;---------------------------  PRE FLOP  ---------------------------------------
(<- (fixed-preflop-bet top-hand)               (sklansky 1))
(<- (fixed-preflop-call no-bets)               (stake zero))
(<- (fixed-preflop-call strong-hand)           (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-call commited0)             (stake commited) (stake minimal) (nie (position blind)))

(<- (fixed-preflop-bet 1a)                     (position early) (bets 0) (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-bet 1b)                     (position early) (bets 0) (hand A Q o))
(<- (fixed-preflop-call 1e)                    (position early) (bets 0) (= h1 h2) (< h1 8))
(<- (fixed-preflop-call 1f)                    (position early) (bets 0) (hand K Q o))
(<- (fixed-preflop-call 1g)                    (position early) (bets 0) (hand A 9 s))
(<- (fixed-preflop-bet 1c)                     (position early) (bets 1) (= h1 h2) (< h1 4))
(<- (fixed-preflop-call 1d)                    (position early) (sklansky ?val) (<= ?val 3))

(<- (fixed-preflop-bet 2a)                     (position middle) (bets 0) (= h1 h2) (< h1 7)) ;AA-88
(<- (fixed-preflop-bet 2b)                     (position middle) (bets 0) (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-bet 2c)                     (position middle) (bets 0) (lub (hand Q J s) (hand K J s) (hand A T s) (hand K Q o)
                                                                               (hand A Q o) (hand A J o) ) )
(<- (fixed-preflop-call 2d)                    (position middle) (bets 0) (sklansky ?val) (<= ?val 4))
(<- (fixed-preflop-call 2e)                    (position middle) (bets 0) (= h1 h2) (< h1 10)) ;77-55
(<- (fixed-preflop-call 2f)                    (position middle) (bets 0) (lub (hand A ?x s) (hand K J o) (hand Q J o) (hand J T o)) )
(<- (fixed-preflop-call 2g)                    (position middle) (bets 0) 
                                                  (>= players-left 4) (lub (= h1 h2) (hand 8 7 s) (hand 7 6 s) (hand T 8 s) (hand K 9 s) 
                                                                           (hand K 8 s)(hand K 7 s)) )
(<- (fixed-preflop-call 2h)                    (position middle) (bets 1) (sklansky ?val) (<= ?val 3))
(<- (fixed-preflop-call 2i)                    (position middle) (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-bet 2j)                     (position middle) (sklansky ?val) (<= ?val 1))


(<- (fixed-preflop-bet 3a)                     (position late) (bets 0) (= h1 h2) (< h1 7)) ;AA-88
(<- (fixed-preflop-bet 3b)                     (position late) (bets 0) (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-bet 3c)                     (position late) (bets 0) (lub (hand Q J s) (hand K J s) (hand A T s) (hand K Q o)
                                                                             (hand A Q o) (hand A J o) ) )
(<- (fixed-preflop-call 3d)                    (position late) (bets 0) (sklansky ?val) (<= ?val 5))
(<- (fixed-preflop-call 3e)                    (position late) (bets 0) (= h1 h2) (< h1 10)) ;77-55
(<- (fixed-preflop-call 3f)                    (position late) (bets 0) (lub  (hand 7 5 s) (hand K ?x s) (hand Q T o) 
                                                                              (hand K T o)))
(<- (fixed-preflop-call 3g)                    (position late) (bets 0) 
                                                  (>= players-left 5) (lub (= h1 h2) (hand 6 4 s) (hand 5 3 s) (hand Q ?x s)))
(<- (fixed-preflop-call 3h)                    (position late) (bets 1) (sklansky ?val) (<= ?val 4))
(<- (fixed-preflop-call 3i)                    (position late) (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-bet 3j)                     (position late) (sklansky ?val) (<= ?val 1))



(<- (fixed-preflop-call sb)                    (position s-blind) (bets 0) (sklansky ?val))
(<- (fixed-preflop-call sb2)                   (position s-blind) (bets 1) (sklansky ?val) (<= ?val 3))
(<- (fixed-preflop-call bb)                    (position b-blind) (bets 1) (sklansky ?val) (<= ?val 6))





;;---------------------------  UNIVERSAL ---------------------------------------

(<- (fixed-bet u_nuts)                   (rank nuts))
(<- (fixed-bet u_set)                    (table-safe yes) (rank set))
(<- (fixed-bet u_set2)                   (bets 1) (rank set)  (table-str8 2) (table-colors 2))
(<- (fixed-bet u_fhb)                    (rank good-fullhouse)) 
(<- (fixed-bet u_trips)                  (bets 0)   (rank trips) (< table-pairs 3) )
(<- (fixed-bet u_str8)                   (bets 0)   (rank str8) )
(<- (fixed-bet u_toppair)                (bets 0)   (rank top-pair) (rank ace-kicker) (table-safe yes) )
(<- (fixed-bet u_two_pairs)              (table-safe yes)   (rank two-pairs))
(<- (fixed-bet u_no_bets_two_pairs)      (bets 0)   (rank two-pairs) (table-pairs 1) (< table-colors 4))
(<- (fixed-bet u_safe_str8)              (table-colors no) (table-pairs no) (table-str8 2) (rank str8))
(<- (fixed-bet u_color)                  (<= bets 1) (rank color)  (table-colors 3))
(<- (fixed-bet u_top2pairs)              (bets 2) (stake commited) (rank top-two-pairs) (table-safe almost))

(<- (fixed-call no-bets)                 (stake zero))
(<- (fixed-call u_call_str8)             (table-colors no) (table-pairs no) (rank str8)  )
(<- (fixed-call u_color_one_bet)         (bets 1) (rank color)  (table-colors 3) )
(<- (fixed-call u_top_color)             (rank top-color)  (= table-pairs2 1) )
(<- (fixed-call u_top_pair)              (bets 1) (rank top-pair) (table-safe almost)  )
(<- (fixed-call u_call_two-pairs)        (bets 1) (rank two-pairs) (table safe)  )
(<- (fixed-call u_trips)                 (bets 1) (rank trips) (table-colors no)  (< table-pairs 3)  )
(<- (fixed-call u_str8)                  (bets 1) (rank str8) (table-colors no)  )
(<- (fixed-call u_fh)                    (bets 1) (rank fullhouse)   )
;nie pasowac gdy bylem raised:
(<- (fixed-call u_raised1)               (bets 2) (stake commited) (rank top-pair) (table-safe almost))
(<- (fixed-call u_raised1)               (bets 2) (stake commited) (rank two-pairs))
(<- (fixed-call u_raised2)               (bets 2) (stake commited) (rank str8))
(<- (fixed-call u_raised3)               (bets 2) (stake commited) (rank trips)  (< table-pairs 3) )
(<- (fixed-call u_raised4)               (bets 2) (stake commited) (rank color))
(<- (fixed-call u_raised5)               (bets 2) (stake commited) (rank fullhouse))

;;---------------------------  FLOP ---------------------------------------
(<- (fixed-flop-dont-bet u_slowplay_set)       (rank set)  (bets 0) (nie (comments last-to-act)))
(<- (fixed-flop-dont-bet u_slowplay_str8)      (rank str8) (comments first-to-act) (bets 0) )
(<- (fixed-flop-dont-bet u_slowplay_trips)     (rank trips) (comments first-to-act) (bets 0) )

(<- (fixed-flop-bet f_strong_pair)             (<= bets 1) (rank strong-pair) (table-safe yes))
(<- (fixed-flop-bet f_strong_pair2)            (bets 0) (rank top-pair) (players-left 2))
(<- (fixed-flop-bet f_strong_pair3)            (bets 0) (rank strong-pair) (table-safe almost))
(<- (fixed-flop-bet f_ace_kicker)              (bets 0) (rank aces-with-kicker) )
(<- (fixed-flop-bet f_trips)                   (bets 1) (rank trips) (rank top-pair)  (table-pairs 2))
(<- (fixed-flop-bet f_overpair)                (bets 0) (rank overpair))
(<- (fixed-flop-bet f_set)                     (bets 0) (rank set) (comments last-to-act)) ;nie slowplay z setem
(<- (fixed-flop-bet f_overpair2)               (table-safe yes) (rank overpair))

(<- (fixed-flop-fuzzy-bet 0.6 u_strong_pair)  (bets 0) (rank top-pair) (table-safe yes) (players-left ?x) (< ?x 5))
(<- (fixed-flop-fuzzy-bet 0.6 f_weak_pair)    (bets 0)  (rank top-pair) (table-safe almost) (players-left ?x) (< ?x 5))

(<- (fixed-flop-call f_top_pair)               (rank top-pair)  (table-safe yes))
(<- (fixed-flop-call f_top_pair)               (bets 1) (rank top-pair))
(<- (fixed-flop-call f_weak_pair)              (bets 1)  (rank one-pair) (rank kicker) (table-safe yes))
(<- (fixed-flop-call f_overpair)               (bets 1)  (rank overpair) )
(<- (fixed-flop-call f_top_pair_outs)          (<= bets 2)  (rank strong-pair) (outs yes))
(<- (fixed-flop-call f_strong_outs)            (bets 2)  (rank strong-pair) (stake commited)) ;bylem raised
(<- (fixed-flop-call chasing)                  (odds good))

(<- (fixed-flop-fuzzy-call 0.7 f_weak_pair)        (bets 1)  (rank one-pair) (table-safe yes))
(<- (fixed-flop-fuzzy-call 0.35 f_weak_pair)       (bets 1)  (rank one-pair) (table-safe almost))
(<- (fixed-flop-fuzzy-call 0.5 f_call_two-pairs)   (bets 1) (rank two-pairs) (= table-pairs2 2)  )
(<- (fixed-flop-fuzzy-call 0.2 f_call_ace_kicker)  (= table-pairs2 2)  (bets 1) (rank two-pairs) (rank ace-kicker) )  


;;---------------------------  TURN  ---------------------------------------
(<- (fixed-turn-bet t_last_with_pair)                    (bets 0) (rank top-pair) (comments last-to-act) (table-safe yes) (nie (on-flop check-raise)))
(<- (fixed-turn-bet t_last_with_pair2)                   (bets 0) (rank top-pair) (comments last-to-act) (outs yes))
(<- (fixed-turn-bet  t_overpair)                         (bets 0) (rank overpair) (table-colors no) (table-str8 no) (nie (on-flop bets 4)))
(<- (fixed-turn-bet  t_trips)                            (bets 0) (rank trips)  (table-pairs 2) )
(<- (fixed-turn-bet  t_set)                              (bets 0) (rank set) )
(<- (fixed-turn-bet  t_a_kicker)                         (bets 0) (rank aces-with-kicker) (table-str8 no) (table-colors no))
(<- (fixed-turn-bet  t_trips)                            (bets 1) (rank trips) (table-colors no) (table-str8 no) (table-pairs 2))
(<- (fixed-turn-bet  t_set)                              (<= bets 1) (rank set) (table-safe almost))


(<- (fixed-turn-fuzzy-bet 0.7 t_top_pair)                (bets 0) (rank top-pair) (table-safe yes))
(<- (fixed-turn-fuzzy-bet 0.6 t_strong_pair)             (bets 0) (rank top-pair) (players-left 2))
(<- (fixed-turn-fuzzy-bet 0.8 t_had_nuts)                (on-flop nuts))
(<- (fixed-turn-fuzzy-bet 0.4 t_last_pair)               (bets 0) (rank top-pair) (comments last-to-act))
(<- (fixed-turn-fuzzy-bet 0.3 t_top_pair)                (bets 0) (rank top-pair) (rank kicker) (table-safe almost) (on-flop bets ?x) (<= ?x 1) )
(<- (fixed-turn-fuzzy-bet 0.5 t_overpair)                (bets 0) (rank overpair) (table-safe almost))
(<- (fixed-turn-fuzzy-bet 0.5 t_set)                     (bets 1) (rank set))
(<- (fixed-turn-fuzzy-bet 0.5 t_toppair_ace)             (bets 1) (rank top-pair) (rank ace-kicker)  (table-str8 no) (table-colors no) 
                                                                   (= t3 t4) (> t3 9))

(<- (fixed-turn-call t_top_pair)                        (bets 1) (rank top-pair) (outs yes) (table-str8 no) (table-pairs 1))
(<- (fixed-turn-call t_str8)                            (<= bets 2) (rank str8) (table-colors no) (table-str8 2))
(<- (fixed-turn-call t_str8b)                           (bets 1) (rank str8)  (table-str8 2))
(<- (fixed-turn-call r_trips)                           (bets 2) (rank trips) (stake commited)) ;bylem raised
(<- (fixed-turn-call t_last_with_pair)                  (bets 2) (rank top-pair) (stake commited) (table-safe yes)) ;j.w
(<- (fixed-turn-call t_overpair)                        (<= bets 2) (rank overpair) (table-str8 no) (<= table-colors 3))
(<- (fixed-turn-call t_chasing)                         (odds good))

(<- (fixed-turn-fuzzy-call 0.7 t_top_pair)              (bets 1) (rank top-pair) (table-str8 no) (table-pairs 1) (players-left ?x) (< ?x 4))
(<- (fixed-turn-fuzzy-call 0.7 t_top_two-pairs)         (bets 1) (rank top-two-pairs) (table-colors 3) (table-str8 2) )
(<- (fixed-turn-fuzzy-call 0.7 t_overpair)              (<= bets 2) (rank overpair))
(<- (fixed-turn-fuzzy-call 0.6 t_2pairs)                (bets 1)   (rank two-pairs) (table-safe almost)) 
(<- (fixed-turn-fuzzy-call 0.9 t_a_kicker)              (bets 1) (rank aces-with-kicker) (table-str8 no) (table-colors no))
(<- (fixed-turn-fuzzy-call 0.5 t_call_two-pairs)        (bets 1) (rank two-pairs) (= table-pairs2 1))
(<- (fixed-turn-fuzzy-call 0.2 t_call_ace_kicker)       (= table-pairs2 2)  (bets 1) (rank two-pairs) (rank ace-kicker) )  
(<- (fixed-turn-fuzzy-call 0.33 t_second_pair)          (bets 1)  (rank one-pair) (table-colors 3) (table-str8 no) (table-pairs 1) (< h2 t3))
(<- (fixed-turn-fuzzy-call 0.6 t_top)                   (bets 2) (stake commited) (rank top-pair) (rank kicker) (table-str8 no) 
                                                        (table-colors no) (= t3 t4) (> t3 9) )


;;---------------------------  RIVER  ---------------------------------------
(<- (fixed-river-bet  r_overpair)                       (bets 0) (rank overpair) (table-colors no) (table-str8 no) )
(<- (fixed-river-bet  r_set)                            (bets 0) (rank set) )
(<- (fixed-river-bet  r_top_color)                      (bets 1) (rank top-color))
(<- (fixed-river-bet  r_color)                          (bets 2) (stake commited) (rank color) (table-colors 3) )
(<- (fixed-river-bet  r_2p)                             (bets 0) (rank two-pairs) (on-turn rank two-pairs) (= turn1 river1) (< h2 turn1) )


(<- (fixed-river-fuzzy-bet 0.7 r_top_pair)               (bets 0) (rank top-pair) (comments last-to-act) (< table-colors 4))

(<- (fixed-river-call r_had_nuts)                        (on-turn nuts))
(<- (fixed-river-call r_had_strong)                      (on-flop nuts) (stake commited))
(<- (fixed-river-call r_overpair)                        (bets 2) (stake commited) (rank overpair) (table-str8 no)  (< table-colors 4) )
(<- (fixed-river-call r_set_comm)                        (bets 2) (stake commited) (rank set) (table-str8 2)  (< table-colors 4) )
(<- (fixed-river-call r_str8)                            (bets 1) (rank str8)  (table-str8 2))
(<- (fixed-river-call r_top_pair)                        (bets 1) (rank top-pair) (rank two-pairs)  )
(<- (fixed-river-call r_set)                             (bets 1)  (rank set)  (< table-colors 4))
(<- (fixed-river-call r_color)                           (bets 1)  (rank color)  (<= table-colors 4))


(<- (fixed-river-fuzzy-call 0.5 r_top_two-pairs)        (bets 1) (rank top-two-pairs) (table-colors 3) (table-str8 2) )
(<- (fixed-river-fuzzy-call 0.6 r_top_pair)             (bets 1) (rank strong-pair) (table-safe yes))
(<- (fixed-river-fuzzy-call 0.6 r_set)                  (bets 1) (rank set) (table-colors 4))
(<- (fixed-river-fuzzy-call 0.5 r_ace_kicker)           (bets 1) (rank aces-with-kicker) (lub (table-colors no) (table-colors 3)))
(<- (fixed-river-fuzzy-call 0.7 r_top_pair)             (bets 1) (rank top-pair)  (table-colors 3) (table-str8 no) (table-pairs 1))
(<- (fixed-river-fuzzy-call 0.8 r_str8_comm)            (bets 2) (stake commited) (rank str8) (table-colors no) (table-str8 2))
(<- (fixed-river-fuzzy-call 0.8 r_top_pair_big_pot)     (bets 1) (rank top-pair)  (table-colors 3) (> (pot-odds) 10))
(<- (fixed-river-fuzzy-call 0.7 r_call_two-pairs)       (bets 1) (rank two-pairs) (table-safe almost) (table-str8 2))
(<- (fixed-river-fuzzy-call 0.2 r_call_ace_kicker)      (= table-pairs2 2)  (bets 1) (rank two-pairs) (rank ace-kicker) )  