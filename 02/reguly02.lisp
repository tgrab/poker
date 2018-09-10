(kasuj-reguly)

;;---------------    definicje pewnych pojec:
(<- no-bets          (zerop (count-bets)))
(<- with-bets        (plusp (count-bets)))
(<- one-bet          (= 1 (count-bets)))
(<- raise            (= 2 (count-bets)))
(<- reraise          (= 3 (count-bets)))
(<- no-raise         (< (count-bets) 2))
(<- no-reraise       (< (count-bets) 3))

(<- late-pos         (eq (my-pos) 'late-pos))

(<- flop             (= 3 (length *table*)))
(<- turn             (= 4 (length *table*)))
(<- river            (= 5 (length *table*)))

(<- top-pair         (rank= 'para) (= H1 T1))
(<- top-pair-kicker  (rank= 'para) (= H2 T1))
(<- top-pair         top-pair-kicker)
(<- overpair         (= H1 H2) (rank= 'para) (< H1 T1))
(<- second-pair      (or (= H1 T2) (= H2 T2)))
(<- second-pair      (= H1 H2) (< H2 T2))
(<- no-pair-table    (rank< 'para 'table))
(<- rainbow          (< (max-of-colors) 3))
(<- no-street-table  (zerop (num-of-1-street-outs)))
(<- safe-table       no-pair-table rainbow no-street-table)
(<- very-safe-table  safe-table (zerop (num-of-2-street-outs)))

(<- mocna-para       top-pair)
(<- mocna-para       overpair)


;;----------------    reguly uniwersalne obowiazujace zawsze:
(<- check           no-bets)

(<- bet             no-raise       safe-table mocna-para)
(<- bet                            safe-table (rank>= 'dwie-pary))
(<- bet                            (rank>= 'trojka) no-street-table rainbow)
(<- bet                            (rank>= 'street) rainbow)
(<- bet                            (rank>= 'kolor))

(<- check                          (color-outs?) (> (pot-odds) (odds 9)))
(<- check                          (street-outs?) (> (pot-odds) (street-odds)))

(<- check           one-bet        (losuj 3) safe-table second-pair)
(<- check           one-bet        (losuj 5) safe-table (= H2 T3))



;;----------------    reguly uniwersalne negatywne:
(<- dont-bet                      (rank< 'kolor) (>= (max-of-colors)  4))
(<- dont-bet                      (rank= 'dwie-pary) (rank>= 'para 'table))
(<- fold            reraise       (rank<= 'dwie-pary) (or (> (max-of-colors) 2) (rank>= 'para 'table)))


;;----------------   reguly RIVER

; z samymi dojsciami juz nie gramy:
(<- fold           river with-bets (rank< 'para))