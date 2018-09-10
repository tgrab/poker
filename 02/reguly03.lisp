(defvar +common-rules+ nil)
(defvar +defs-list+ nil)
(defvar +flop-rules+ nil)
;;------------------------------------------------------------------------------
(setq +defs-list+
  '((no-bets          (zerop (count-bets)))
    (with-bets        (plusp (count-bets)))
    (one-bet          (= 1 (count-bets)))
    (raise            (= 2 (count-bets)))
    (reraise          (= 3 (count-bets)))
    (no-raise         (< (count-bets) 2))
    (no-reraise       (< (count-bets) 3))

    (late-pos         (eq (my-pos) 'late-pos))

    (flop             (= 3 (length *table*)))
    (turn             (= 4 (length *table*)))
    (river            (= 5 (length *table*)))

    (top-pair         (rank= 'para) (= H1 T1))
    (top-pair-kicker  (rank= 'para) (= H2 T1))
    (top-pair         top-pair-kicker)
    (overpair         (= H1 H2) (rank= 'para) (< H1 T1))
    (second-pair      (or (= H1 T2) (= H2 T2)))
    (second-pair      (= H1 H2) (< H2 T2))
    (no-pair-table    (rank< 'para 'table))
    (rainbow          (< (max-of-colors) 3))
    (no-street-table  (zerop (num-of-1-street-outs)))
    (safe-table       no-pair-table rainbow no-street-table)
    (very-safe-table  safe-table (zerop (num-of-2-street-outs)))
    (trojka           (rank= 'trojka))
    (street           (rank= 'street))
    (kolor            (rank= 'kolor))
    (mocna-para       top-pair)
    (mocna-para       overpair)    ))

;;------------------------------------------------------------------------------
(setq +common-rules+ 
      (append +defs-list+
	      '((check   no-bets)
		(bet     rainbow trojka no-street-table);moze byc mocniejsza trojka,full lub dlugi street
		(bet     rainbow street);moze byc full
		(bet     (rank>= 'kolor));moze byc mocniejszy kolor lub full
		(bet     no-raise safe-table mocna-para)
		(bet     safe-table (rank>= 'dwie-pary))
                (check   (< (length *table*) 5) (color-outs?) (> (pot-odds) (odds 9)))
		(check   (< (length *table*) 5) (street-outs?) (> (pot-odds) (street-odds)))
		(check   one-bet (losuj 3) safe-table second-pair)
		(check   one-bet (losuj 5) safe-table (= H2 T3))
;dziwne reguly z pierwszej wersji:
		(bet     no-bets        (rank>= 'dwie-pary))
		(bet     no-bets        mocna-para)
		(check   one-bet        mocna-para)
                (check                  (rank>= 'trojka))
		(check   one-bet        (losuj 3) second-pair)

		(dont-bet  (rank< 'kolor) (>= (max-of-colors)  4))
		(fold    reraise (rank<= 'dwie-pary) (or (> (max-of-colors) 2) (rank>= 'para 'table)))

		
		)))

;;------------------------------------------------------------------------------
(setq +flop-rules+
      (append +defs-list+
	      '(
		(dont-bet  no-bets      (rank>= 'trojka) (not (eq (my-pos) 'late-pos)))
		(check  mocna-para)
		)))


;;------------------------------------------------------------------------------
(setq +poker-rules+
      (append +common-rules+
	      '(;(bet       flop (prove (make-reasoner +flop-rules+) 'bet))
		(dont-bet  flop (prove (make-reasoner +flop-rules+) 'dont-bet))
		;(check     flop (prove (make-reasoner +flop-rules+) 'check))
		;(fold      flop (prove (make-reasoner +flop-rules+) 'fold))
		)))

