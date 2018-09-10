;TODO sprawdzic b-blinda-getstake zle dziala!
(<- (fixed-preflop-call sklansky2)               (sklansky ?val) (<= ?val 2))
(<- (fixed-preflop-call no-bets-sklansky6)       (bets 0) (sklansky ?val) (<= ?val 6))
(<- (fixed-preflop-call no-bets-late-sklansky7)  (bets 0) (position late) (sklansky ?val) (<= ?val 7))
(<- (fixed-preflop-call sklansky5)               (bets 1) (sklansky ?val) (<= ?val 5))
(<- (fixed-preflop-call preflop-commited)        (stake commited) (sklansky ?) )

(<- (fixed-preflop-bet sklansky1)                (sklansky 1))


;;---------------------------  FLOP ---------------------------------------
(<- (fixed-flop-call r11)			(bets<= 1) (rank strong-pair))

(<- (fixed-flop-bet r12)			(bets 0)   (rank strong-pair))
(<- (fixed-flop-bet r13)			(bets 0)   (table-rank no-pair) (rank> highcard) (last-round c ?))

;;---------------------------  TURN  ---------------------------------------

(<- (fixed-turn-call r21)			(bets<= 1) (rank strong-pair))

(<- (fixed-turn-bet r22)			(bets 0)   (rank strong-pair))
(<- (fixed-turn-bet r23)			(bets 0)    (table-rank no-pair)(rank> highcard) (last-round c?[c]))


;;---------------------------  RIVER  ---------------------------------------

(<- (fixed-river-call r31)			(bets<= 1) (rank strong-pair))

(<- (fixed-river-bet r32)			(bets 0)   (rank strong-pair))


