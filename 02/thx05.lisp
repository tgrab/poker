(defun test1()
  (setq +pre-flop-rules+ '((check (zerop 0)))))

;;-------------------------------------------------------------------
;;------------------------ TEXAS  RULES -----------------------------
;;-------------------------------------------------------------------
(setq *sblind* 2.5 *blind* 5)
(undebuguj 'dbg-servers)

(setq +pre-flop-rules+
 '((check   big-blind no-bets)
   (bet     sklansky (= (sklansky) 1))

   (bet     heads-up sklansky)
   (check   heads-up big-blind)

   (check   fulltable early-pos no-raise    sklansky  (<= (sklansky) 4))
   (check   fulltable early-pos with-raise  sklansky  (<= (sklansky) 2))

   (check   fulltable middle-pos no-bets     sklansky (<= (sklansky) 7))
   (check   fulltable middle-pos one-bet     sklansky (<= (sklansky) 4))
   (check   fulltable middle-pos with-raise  sklansky (<= (sklansky) 2))

   (check   fulltable late-pos no-bets       sklansky (<= (sklansky) 8))
   (check   fulltable late-pos one-bet       sklansky (<= (sklansky) 7))
   (check   fulltable late-pos with-raise    sklansky (<= (sklansky) 4))
  
   (check   fulltable small-blind no-bets    sklansky (<= (sklansky) 8))
   (check   fulltable small-blind one-bet    sklansky (<= (sklansky) 6))
  
   (check   fulltable big-blind one-bet      sklansky (<= (sklansky) 6))   ))


;;------------------------------------------------------------------------------
(setq +post-flop-rules+
      '((check   before-river (> (implied-odds) (odds (my-outs)))) 
	(check   (> (pot-odds) (moje-odds)))
	(bet     rainbow trojka no-street-table);moze byc mocniejsza trojka,full lub dlugi street
	(bet     rainbow street);moze byc full
	(bet     (rank>= 'kolor));moze byc mocniejszy kolor lub full
	(bet     no-raise safe-table mocna-para)
	(bet     safe-table (rank>= 'dwie-pary))

        (dont-bet flop  no-bets (rank>= 'trojka) (not (last-to-act?)));slowplaying
))


