;;------------------------   RULES [OMAHA POT LIMIT] ----------------
;;-------------------------------------------------------------------
(setq *sblind* 0.25 *blind* 0.25)

(setq +post-flop-rules+
 '((check    before-river rainbow no-pair-table (street-chase?));chasing
   (check    before-river rainbow no-pair-table (color-chase?));chasing

   (potbet1 (rank> 'full)) 
   (dont-bet flop no-bets (not (last-to-act?)) (rank>= 'trojka)) ;slowplaying

   ;;jezeli trojka na stole to moze byc kareta!
   ;co robic z fullem:
   (full-nuts  full (full-nuts?))
   (full-no-nuts full (not (full-nuts?)))
   (potbet1  full-nuts)
   (potbet2  no-bets full)
   (check    full-no-nuts (> (pot-odds) 3))
   (fold     (losuj 3)  reraise full-no-nuts);to chyba przesada na 0.25

   ;co robic z kolorem:
   (kolor-nuts  kolor (color-nuts?))
   (kolor-no-nuts kolor (not (color-nuts?)))
   (potbet1  no-pair-table kolor-nuts)
   (potbet2  no-pair-table no-bets kolor)
   (check    no-pair-table kolor-no-nuts (> (pot-odds) 4))
   (fold     (losuj 2) no-pair-table reraise kolor-no-nuts)
   (check    (losuj 2) pair-table kolor  (> (pot-odds) 4) )

   ;co robic ze stritem:
   (street-nuts  street (street-nuts?))
   (street-no-nuts street (not (street-nuts?)))
   (potbet1  no-pair-table rainbow street-nuts  (push 'nuts-bet *memory*))
   (potbet2  no-pair-table no-bets rainbow street)
   (check    no-pair-table rainbow street-no-nuts (> (pot-odds) 4))
   (fold     (losuj 3) no-pair-table reraise rainbow street-no-nuts)
   (check    pair-table street one-bet (> (pot-odds) 6))
   (check    no-rainbow street one-bet (> (pot-odds) 6))
   (fold     pair-table no-rainbow street with-bets)

   ;jak grac trojki:
   (set-nuts (or (= H1 H2 T1) (= H2 H3 T1) (= H3 H4 T1)))
   (potbet1  no-pair-table rainbow no-street-table set-nuts (push 'nuts-bet *memory*));set
   (potbet2  rainbow no-street-table no-reraise set) ;set
   (dont-bet flop no-bets rainbow set);slowplay - uzaleznic od pozycji
   (check    rainbow no-street-table reraise set) ;set
             ;;mamy pare na stole:
   (bet      rainbow no-street-table no-bets trojka)
   (check    rainbow no-street-table no-raise trojka)
   (fold     full-table (losuj 3) rainbow no-street-table reraise trojka)
   (check    (losuj 3) (>= (max-of-colors) 3) (> (pot-odds) 4) no-street-table trojka)
   (check    (losuj 3) rainbow street-table (> (pot-odds) 4) trojka)

   ;jak grac dwie pary:
   ;gramy tylko gorne dwie pary lub okazyjnie overpair
   (dwie-pary (rank= 'dwie-pary))
   (potbet2   (last-to-act?) flop dwie-pary no-bets safe-table)
   (check     dwie-pary (> (implied-odds) (odds 4))) ;gra do fulla?

; dziwne przypadki
   (check       nuts-bet (> (pot-odds) 3) (= 2 (players-left)) ) ;kiedys zrobilem potbet1 to kontynuujemy to!!! RISKY
   (check       nuts-bet (losuj 3) (> (pot-odds) 3) (> (players-left) 2) )
   (check       nuts-bet (> (pot-odds) 6)  )
   (potbet1     nuts-bet (last-to-act?) (> (pot-odds) 5)) ;j.w.

; dziwne stoly
   (dont-bet    flop (= 3 (max-of-colors)) trojka no-bets)   
   (dont-bet    (rank= 'trojka 'table) trojka)

))

;dodac gre na kolor z asem:
(setq +pre-flop-rules+
 '((check     (> (pot-odds) 3) late-pos (< (pre-flop-eval) 10))
   (check     (> (pot-odds) 3) middle-pos (< (pre-flop-eval) 9))
   (check     (< (pre-flop-eval) 8))

    ;gdy nie ma duzych podbic gramy duze pary:
   (check     (= H1 H2) (< H1 5)  (> (pot-odds) 4))
   (check     (= H2 H3) (< H3 6)  (> (pot-odds) 4)) 
   (check     (= H3 H4) (< H4 7)  (> (pot-odds) 4))

   (check     heads-up)
   (bet       heads-up (< (pre-flop-eval) 8))

   (check     shorthand (< (pre-flop-eval) 8))

   (check     big-blind no-bets)
   (check     big-blind (> (pot-odds) 4)   (< (pre-flop-eval) 10))
   (check     small-blind (> (pot-odds) 4)  (< (pre-flop-eval) 10))
   (check     small-blind no-bets   ))  )