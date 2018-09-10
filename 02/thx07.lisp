(defvar +pre-flop-rules+)
(defvar +heads-up-rules+)
(defvar +shorthand-rules+)
(defvar +fulltable-rules+)

(defun ustaw-baze-faktow()
 (setq *facts* nil *memory* nil)
 (let ((stol  (sort (mapcar #'l->figura *table*) #'<) )
       (reka  (sort (mapcar #'l->figura *hand*) #'<) ))
  (setq H1 (nth 0 reka) H2 (nth 1 reka))
  (setq T1 (nth 0 stol) T2 (nth 1 stol) T3 (nth 2 stol))
  (setq T4 (nth 3 stol) T5 (nth 4 stol)))

 (when (plusp (length *table*)) 
     (setq *ocena* (ocena (append *hand* *table*)))
     (setq *vhand* (wektor-figur *hand*)) 
     (setq *vtable* (wektor-figur *table*)) ))

(defun get-action-from-rules(rules)
  (let ((table (make-reasoner rules))
	(temp nil))
  (flet ((usun (lista) (if (> (length lista) 1) (last lista) lista  ) ))
  (or
   (and (prove table 'fold) (setq *chosen-rule* (usun *chosen-rule*)) "fold")
   (and (prove table 'bet)
	(progn (setq temp (usun *chosen-rule*) *chosen-rule* nil ) t)
	(if (prove table 'dont-bet)
	    (progn (setq *chosen-rule* (usun *chosen-rule*)) "check")
	    (progn (setq *chosen-rule* temp)"bet"))) 
   (and (prove table 'check)  (setq *chosen-rule* (usun *chosen-rule*)) "check")
   (and (progn  (setq *still-playing* nil) (setq *chosen-rule* 'no-rule)) "fold"))) ))

(defun get-action()
  (let ((pre-flop (null *table*))
	(heads-up (= 2 *table-size*))
	(shorthand (<= *table-size* 6)) )
	
    (or
     (and pre-flop (get-action-from-rules '+pre-flop-rules+) )
     (and *table* heads-up (get-action-from-rules '+heads-up-rules+))
     (and *table* (= 2 *players-on-flop*) (get-action-from-rules '+heads-up-rules+))
     (and *table* shorthand  (get-action-from-rules '+shorthand-rules+))
     (and *table* (get-action-from-rules '+fulltable-rules+))
)))


(defun action()
 (let (wynik)
  (when (null *hand*) (return-from action "repeat"))
  (when (null *my-pos*) (setq *my-pos* *active-player*))
  (ustaw-baze-faktow)
  (setq *chosen-rule* nil)
  (setq wynik   (get-action))
  (logs wynik)
  wynik))


;;-------------------------------------------------------------------
;;------------------------ TEXAS  RULES -----------------------------
;;-------------------------------------------------------------------
;(setq *sblind* 0.10 *blind* 0.15)
(setq *sblind* 0.15 *blind* 0.25)

(undebuguj 'dbg-servers)

(setq +pre-flop-rules+
 '((check   big-blind no-bets)
   (bet     sklansky (= (sklansky) 1))

   (bet     heads-up (< (sklansky) 3))
   (bet     heads-up no-bets big-blind sklansky)
   (check   heads-up sklansky)
   (check   heads-up small-blind no-bets (szansa 2 3))

   (bet     shorthand sklansky (<= (sklansky) 2))
   (check   shorthand big-blind sklansky)
   (check   shorthand small-blind one-bet sklansky)
   (check   shorthand small-blind sklansky (<= (sklansky) 7))
   (check   shorthand early-pos       sklansky  (<= (sklansky) 6))
   (check   shorthand middle-pos      sklansky  (<= (sklansky) 7))
   (check   shorthand late-pos        sklansky)

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

(setq +heads-up-rules+
      '((bet   no-bets (< *hs* 1))
	(bet   with-bets (< *hs* 0.1))
	(check (< *hs* 3.1))
	(check (rank>= 'para))
	(check before-river (plusp (my-outs)) (> (implied-odds) (odds (my-outs))))
))

(setq +shorthand-rules+
      '((bet   no-bets (< *hs* 1))
	(bet   one-bet (< *hs* 0.1))
	(check (< *hs* 2))
	(check (= 3 *players-on-flop*) one-bet (< *hs* 3))
	(check flop (rank< 'para) (szansa 1 2))
	(check before-river (plusp (my-outs)) (> (implied-odds) (odds (my-outs))))
))

(setq +fulltable-rules+
      '((check     before-river (rank< 'dwie-pary 'table) (plusp (my-outs)) (> (implied-odds) (odds (my-outs))))

;reguly oparte na HandStrength:
	(check     flop     (rank> 'para)) ;!!! ale nie na stole !!!
	(bet       flop     no-bets mocna-para)
	(check     flop     mocna-para)
	
	(bet      after-flop            no-bets     (< *hs* 1)   (no-raiser?))
	(bet      after-flop            no-bets     (< *hs* 1)   (after-raiser?))
        (bet      after-flop            no-bets     (< *hs* 0.3)) ;before-raiser	
	(check    one-bet     (< *hs* 1))
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

