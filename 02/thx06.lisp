;;-------------------------------------------------------------------
;;------------------------ TEXAS  RULES -----------------------------
;;-------------------------------------------------------------------
;(setq *sblind* 0.10 *blind* 0.15)
(setq *sblind* 0.15 *blind* 0.25)

(undebuguj 'dbg-servers)

(setq +pre-flop-rules+
 '((check   big-blind no-bets)
   (bet     sklansky (= (sklansky) 1))

   (bet     heads-up sklansky)
   (check   heads-up big-blind)

   (bet     shorthand sklansky (<= (sklansky) 2))
   (check   shorthand big-blind sklansky)
   (check   shorthand small-blind sklansky (<= (sklansky) 7))
   (check   shorthand early-pos       sklansky  (<= (sklansky) 6))
   (check   shorthand middle-pos      sklansky  (<= (sklansky) 7))
   (check   shorthand late-pos        sklansky  (<= (sklansky) 8))

   (check   fulltable early-pos no-raise     sklansky  (<= (sklansky) 3))
   (check   fulltable early-pos with-raise   sklansky  (<= (sklansky) 2))

   (check   fulltable middle-pos no-bets     sklansky (<= (sklansky) 6))
   (check   fulltable middle-pos one-bet     sklansky (<= (sklansky) 4))
   (check   fulltable middle-pos with-raise  sklansky (<= (sklansky) 2))

   (check   fulltable late-pos no-bets       sklansky (<= (sklansky) 8))
   (check   fulltable late-pos one-bet       sklansky (<= (sklansky) 7))
   (check   fulltable late-pos with-raise    sklansky (<= (sklansky) 4))
  
   (check   fulltable small-blind no-bets    sklansky (<= (sklansky) 8))
   (check   fulltable small-blind one-bet    sklansky (<= (sklansky) 5))
  
   (check   fulltable big-blind with-bets    sklansky (<= (sklansky) 5))   ))


;;------------------------------------------------------------------------------
(setq +post-flop-rules+
      '((check     before-river (rank< 'dwie-pary 'table) (plusp (my-outs)) (> (implied-odds) (odds (my-outs))))

;reguly oparte na HandStrength:
	(check     flop     (rank> 'para))
	(bet       flop     no-bets mocna-para)
	(check     flop     mocna-para)
	
	(bet      after-flop            no-bets     (< *hs* 1)   (no-raiser?))
	(bet      after-flop            no-bets     (< *hs* 1)   (after-raiser?))
        (bet      after-flop            no-bets     (< *hs* 0.3)) ;before-raiser	
	(check    after-flop  one-bet     (< *hs* 1.1))
	(check    after-flop  with-raise  (< *hs* 0.6))

	(bet      after-flop              (< *hs* 0.1))

; z kolorem:
	(check   kolor) ; z kolorem nie pasujemy
	(bet     (color-nuts?))
	(bet     no-bets kolor)

	(fold    reraise after-flop para (zerop (my-outs)))
	(fold    (rank< 'kolor) with-raise (>= (max-of-colors) 4))
	(fold    (szansa 1 2) (rank< 'kolor) one-bet (>= (max-of-colors) 4))
	(fold    (rank= 'trojka 'table) turn (> H1 T4) i-checked with-raise)

        (dont-bet flop  no-bets (rank>= 'dwie-pary) (not (last-to-act?)));slowplaying
	(dont-bet flop  one-bet (rank>= 'trojka));slowplaying
	(dont-bet flop  (> H1 T2)) ;2 karty na flopie wyzsze
	(dont-bet dwie-pary (rank= 'dwie-pary 'table))
	(dont-bet trojka (rank= 'trojka 'table))
	(dont-bet (rank< 'para))
	(dont-bet dwie-pary one-bet pair-table)
	(dont-bet flop (= 3 (max-of-colors) (max-of-colors (append *hand* *table*))))

))


