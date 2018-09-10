(clear-db)

(<- (call no-bets) (stake zero))

(<- (rank strong-pair) (rank top-pair))
(<- (rank strong-pair) (rank overpair))

(<- (bets<= 1) (bets 1))
(<- (bets<= 1) (bets 0))

(<- (rank> highcard) (rank one-pair))
(<- (rank> highcard) (rank two-pairs)) ;TODO?

(<- (last-round c?[c]) (last-round c ?))
(<- (last-round c?[c]) (last-round c ? c))

;(load "poker.rules.omaha.lisp")
(load "poker.rules.fixed.lisp")

;FIXED
(<- (bet ?why) (game-kind fixed) (round pre-flop) (fixed-preflop-bet ?why))
(<- (call ?why) (game-kind fixed) (round pre-flop) (fixed-preflop-call ?why))

(<- (bet ?why) (game-kind fixed) (round flop) (fixed-flop-bet ?why))
(<- (fuzzy-bet ?level ?why) (game-kind fixed) (round flop) (fixed-flop-fuzzy-bet ?level ?why))
(<- (call ?why) (game-kind fixed) (round flop) (fixed-flop-call ?why))
(<- (fuzzy-call ?level ?why) (game-kind fixed) (round flop) (fixed-flop-fuzzy-call ?level ?why))

(<- (bet ?why) (game-kind fixed) (round turn) (fixed-turn-bet ?why))
(<- (fuzzy-bet ?level ?why) (game-kind fixed) (round turn) (fixed-turn-fuzzy-bet ?level ?why))
(<- (call ?why) (game-kind fixed) (round turn) (fixed-turn-call ?why))
(<- (fuzzy-call ?level ?why) (game-kind fixed) (round turn) (fixed-turn-fuzzy-call ?level ?why))


(<- (bet ?why) (game-kind fixed) (round river) (fixed-river-bet ?why))
(<- (fuzzy-bet ?level ?why) (game-kind fixed) (round river) (fixed-river-fuzzy-bet ?level ?why))
(<- (call ?why) (game-kind fixed) (round river) (fixed-river-call ?why))
(<- (fuzzy-call ?level ?why) (game-kind fixed) (round river) (fixed-river-fuzzy-call ?level ?why))

;OMAHA
(<- (call ?why) (game-kind omaha) (round pre-flop) (omaha-preflop-call ?why))
(<- (fuzzy-call ?level ?why) (game-kind omaha) (round pre-flop) (omaha-preflop-fuzzy-call ?level ?why))