(clear-db)

(<- (decision call no-bets) (stake zero))

;(load "poker.rules.omaha.lisp")

(load "poker.rules.hfixed.lisp")

;; external percepts: game-kind hand sklansky flop

;; depend on basic lisp functions:


(<- (stake zero)
    (= 0 (stake)))

(<- (round preflop)
    (game-prop 'preflop))

(<- (round flop)
    (game-prop 'flop))

(<- (round turn)
    (game-prop 'turn))

(<- (round river)
    (game-prop 'river))

(<- (rank str8flush)
    (= 0 (car (game-prop 'ocena))))

(<- (rank quads)
    (= 1 (car (game-prop 'ocena))))

(<- (rank fullhouse)
    (= 2 (car (game-prop 'ocena))))

(<- (rank color)
    (= 3 (car (game-prop 'ocena))))

(<- (rank str8)
    (= 4 (car (game-prop 'ocena))))

(<- (rank trips)
    (= 5 (car (game-prop 'ocena))))

(<- (rank two-pairs)
    (= 6 (car (game-prop 'ocena))))

(<- (rank pair)
    (= 7 (car (game-prop 'ocena))))

(<- (rank no-pair)
    (= 10 (car (game-prop 'ocena))))

(<- (round postflop) (round flop))
(<- (round postflop) (round turn))
(<- (round postflop) (round river))

(<- (pos late) (>= (agent-pos) 6))



;0 1 2 3 = Ace King Queen Jack
(<- (big-card ?a)
    (< (figura ?a) 4))

;4 5 6 = T 9 8
(<- (middle-card ?a)
    (< (figura ?a) 7))

(<- (ace ?a)
    (= 0 (figura ?a)))

(<- (king ?a)
    (= 1 (figura ?a)))

(<- (queen ?a)
    (= 2 (figura ?a)))

(<- (pair ?a ?b)
    (= (figura ?a) (figura ?b)))


