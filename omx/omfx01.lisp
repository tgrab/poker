
;;-------------------------------------------------------------------
;;------------------------   RULES [OMAHA FIXED ]--------------------
;;-------------------------------------------------------------------
(setq +post-flop-rules+
 '((check    before-river rainbow no-pair-table (street-chase?));chasing
   (check    before-river rainbow no-pair-table (color-chase?));chasing

   (bet (rank> 'full)) 
   (dont-bet  flop (rank>= 'trojka)) ;slowplaying
;;jezeli trojka na stole to moze byc kareta!
   ;co robic z fullem:
   (full-nuts  full (full-nuts?))
   (full-no-nuts full (not (full-nuts?)))
   (bet     full-nuts)
   (bet     no-bets full)
   (check   no-reraise full-no-nuts)
   (fold     (losuj 3)  reraise full-no-nuts);to chyba przesada na 0.25

   ;co robic z kolorem:
   (kolor-nuts  kolor (color-nuts?))
   (kolor-no-nuts kolor (not (color-nuts?)))
   (bet      no-pair-table kolor-nuts)
   (bet      no-pair-table no-bets kolor)
   (check    no-pair-table no-reraise kolor-no-nuts)
   (fold     (losuj 2) no-pair-table reraise kolor-no-nuts)
   (check    (losuj 2) pair-table kolor one-bet)

   ;co robic ze stritem:
   (street-nuts  street (street-nuts?))
   (street-no-nuts street (not (street-nuts?)))
   (bet      no-pair-table rainbow street-nuts)
   (bet      no-pair-table no-bets rainbow street)
   (check    no-pair-table no-reraise street-no-nuts)
   (fold     (losuj 3) no-pair-table reraise rainbow street-no-nuts)
   (check    (losuj 2) pair-table street one-bet)
   (check    (losuj 2) no-rainbow street one-bet)
   (fold     pair-table no-rainbow street with-bets)

   ;jak grac trojki:
   (set-nuts (or (= H1 H2 T1) (= H2 H3 T1) (= H3 H4 T1)))
   (bet      no-pair-table rainbow no-street-table  set-nuts);set
   (bet      rainbow no-street-table no-reraise set) ;set
   (check    rainbow no-street-table reraise set) ;set
             ;;mamy pare na stole:
   (bet      rainbow no-street-table no-bets trojka)
   (check    rainbow no-street-table no-reraise trojka)
   (fold     full-table (losuj 3) rainbow no-street-table reraise trojka)
   (check    (losuj 3) (>= (max-of-colors) 3) one-bet no-street-table trojka)
   (check    (losuj 3) rainbow street-table one-bet trojka)

   ;jak grac dwie pary:
   (bet      flop dwie-pary no-bets rainbow)
   (bet      safe-table dwie-pary)
   (check    rainbow no-street-table dwie-pary one-bet) ;gra do fulla?
   (check    (losuj 4) no-street-table dwie-pary one-bet)
   (check    (losuj 4) rainbow dwie-pary one-bet)
))

;dodac gre z wysokimi parami i kolor z asem:
(setq +pre-flop-rules+
 '((check     (< (pre-flop-eval) 8))
   (check     (= H1 H2) (< H1 5))
   (check     (= H2 H3) (< H3 6))
   (check     (= H3 H4) (< H4 7))
   (check     heads-up)
   (bet       heads-up (< (pre-flop-eval) 7))
   (check     shorthand (< (pre-flop-eval) 12))
   (check     big-blind no-bets)
   (check     big-blind one-bet   (< (pre-flop-eval) 11))
   (check     small-blind no-bets (< (pre-flop-eval) 11))   ))