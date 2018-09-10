(kasuj-reguly)

;definicje pewnych pojec:
(<- dojscia                        (ten-sam-kolor? 2 2))


;reguly uniwersalne obowiazujace zawsze:
(<- check           no-bets)
(<- bet             no-bets        (better-than? 'dwie-pary))
(<- bet             no-bets        (top-pair?))
(<- bet             no-bets        (overpair?))

(<- check                          (better-than? 'trojka))

(<- check           one-bet        (top-pair?))
(<- bet             one-bet        (better-than? 'dwie-pary))

;reguly uniwersalne negatywne:
(<- dont-bet                      (worse-than? 'kolor) (ten-sam-kolor? 1 4))
(<- dont-bet                      (my-rank? 'dwie-pary) (pair? *vtable*))


;reguly FLOP-a:
(<- bet       flop                (better-than? 'trojka))
(<- check     flop                (or (overpair?) (top-pair?)))
(<- check     flop                (better-than? 'dwie-pary))

(<- check     flop   one-bet      dojscia)
(<- check     flop   one-bet      (better-than? 'para))
(<- check     flop   one-bet      (zerop (random 2)) (> H2 T1))
(<- bet       flop   one-bet      (overpair?))

(<- check     flop   raise        (better-than? 'dwie-pary))



;reguly TURN-a:

(<- check     turn   one-bet      (zerop (random 2)) (my-rank? 'para) (or (= H2 T2) (= H1 T2) ))
(<- check     turn   one-bet      (zerop (random 2)) (pair? *vhand*) (> H2 T2) )
(<- check     turn   one-bet      (zerop (random 2)) (> H2 T1))
(<- check     turn   one-bet      (or (top-pair?) (overpair?)))

(<- check     turn   raise        (zerop (random 2)) (top-pair?))

;reguly RIVER-a:

(<- check     river    one-bet    (better-than? 'dwie-pary) (not (pair? *vtable*)))
(<- check     river    one-bet    (zerop (random 3)) (top-pair?))

