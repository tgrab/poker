(kasuj-reguly)


;definicje pewnych pojec:
(<- no-bets          (zerop (count-bets)))
(<- one-bet          (= 1 (count-bets)))
(<- raise            (= 2 (count-bets)))
(<- reraise          (= 3 (count-bets)))

(<- flop             (= 3 (length *table*)))
(<- turn             (= 4 (length *table*)))
(<- river            (= 5 (length *table*)))

(<- top-pair         (rank= 'para) (= H1 T1))
(<- top-pair-kicker  (rank= 'para) (= H2 T1))
(<- top-pair         top-pair-kicker)
(<- overpair         (= H1 H2) (rank= 'para) (< H1 T1))
(<- second-pair      (or (= H1 T2) (= H2 T2)))
(<- no-pair-table    (rank< 'para 'table))
(<- rainbow          (< (max-of-colors) 3))
(<- no-street-table  (zerop (num-of-1-street-outs)))
(<- safe-table       no-pair-table rainbow no-street-table)
(<- dojscia          (or (street-outs?) (color-outs?)))
(<- mocna-para       top-pair)
(<- mocna-para       overpair)




;reguly uniwersalne obowiazujace zawsze:
(<- check           no-bets)
(<- bet             no-bets        (rank>= 'dwie-pary))
(<- bet             no-bets        top-pair)
(<- bet             no-bets        overpair)

(<- check                          (rank>= 'trojka))

(<- check           one-bet        top-pair)
(<- check           one-bet        (losuj 3) second-pair)
(<- bet             one-bet        (rank>= 'dwie-pary))

;reguly uniwersalne negatywne:
(<- dont-bet                      (rank< 'kolor) (>= (max-of-colors)  4))
(<- dont-bet                      (rank= 'dwie-pary) (rank>= 'para 'table))
(<- fold            reraise       (rank<= 'dwie-pary) (or (> (max-of-colors) 2) (rank>= 'para 'table)))

;reguly FLOP-a:
(<- bet       flop                (rank>= 'trojka))
(<- check     flop                mocna-para)
(<- check     flop                (rank>= 'dwie-pary))

(<- dont-bet  flop   no-bets      (rank>= 'trojka) (not (eq *my-pos* 'late-pos)))

(<- check     flop   one-bet      dojscia)
(<- check     flop   one-bet      (rank>= 'para))
(<- check     flop   one-bet      (losuj 2) (< H2 T1))
(<- bet       flop   one-bet      overpair)

(<- check     flop   raise        (rank>= 'dwie-pary))




;reguly TURN-a:

(<- check     turn   one-bet      (losuj 2) (rank= 'para) (or (= H2 T2) (= H1 T2) ))
(<- check     turn   one-bet      (losuj 2) (rank= 'para 'hand) (< H2 T2) )
(<- check     turn   one-bet      (losuj 2) (< H2 T1))
(<- check     turn   one-bet      mocna-para)
(<- check     turn   one-bet      dojscia)
(<- check     turn   raise        (losuj 2) mocna-para)

;reguly RIVER-a:

(<- check     river               (rank>= 'dwie-pary) no-pair-table)
(<- bet       river               (rank>= 'dwie-pary) safe-table)
(<- check     river    one-bet    (losuj 3) mocna-para)






;; ---- Testy:
; (setq *hand* (karty 'ah 'ks) *table* (karty 'qd 'js '5c) *history* '(bet flop check check))