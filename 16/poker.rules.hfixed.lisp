;(<- (decision call ?why) (game-kind hfixed) (hf-call ?why))
;(<- (decision bet ?why) (game-kind hfixed) (hf-bet ?why))
(<- (decision bet ?why) (game-kind hfixed) (round turn)  (hf-turn-bet ?why))
(<- (decision bet ?why) (game-kind hfixed) (round flop)  (hf-flop-bet ?why))
(<- (decision call ?why) (game-kind hfixed) (round flop)  (hf-flop-call ?why))
(<- (decision call ?why) (game-kind hfixed) (round preflop)  (hf-preflop-call ?why))
(<- (decision bet ?why) (game-kind hfixed) (round preflop)  (hf-preflop-bet ?why))

;; ---------------------------- Defs. -----------------------------------------

(<- (bets 0) (= 0 (bets)))
(<- (bets 1) (= 1 (bets)))
(<- (bets <=1) (<= (bets) 1))
(<- (bets 2) (= 2 (bets)))
(<- (bets 3) (= 3 (bets)))

(<- (safe-table) (< (game-prop 'max-of-colors) 3)) ;nie ma 3 w kolorze, pary albo 4 do strita

(<- (top-pair) (handf ?a ?b) (t1f ?c) (= ?a ?c))
(<- (over-pair) (handf ?a ?b) (t1f ?c) (= ?a ?b) (< ?a ?c))
(<- (good-pair) (safe-table) (top-pair))
(<- (good-pair) (safe-table) (over-pair))

(<- (holdem-nuts) (progn (holdem-color-nuts?)))
;; ---------------------------- PRE FLOP RULES --------------------------------

(<- (hf-preflop-call sklansky2)  (sklansky ?val) (<= ?val 2))
(<- (hf-preflop-call no-bets-sklansky6)  (= 0 (bets)) (sklansky ?val) (<= ?val 6))
(<- (hf-preflop-call no-bets-late-sklansky7)  (= 0 (bets)) (pos late) (sklansky ?val) (<= ?val 7))
(<- (hf-preflop-call sklansky5)  (<= (bets) 1) (sklansky ?val) (<= ?val 5))
(<- (hf-preflop-call wasnt-raised-before)  (< (stake) (* 2 +blind+)) (sklansky ?val) (<= ?val 5))
(<- (hf-preflop-call preflop-commited)  (< (stake) +stake+) (<= (stake) +blind+) (sklansky ?) )

(<- (hf-preflop-bet sklansky1)  (sklansky 1))

;; ------------------------------------ RULES ----------------------------------

(<- (hf-bet dobry-kolor) (rank color) (bets <=1) (= 3 (game-prop 'max-of-colors)))
(<- (hf-bet nuts1) (holdem-nuts))
(<- (hf-bet >=dwie-pary) (bets 0) (progn (rank >= two-pairs)))
(<- (hf-bet dwie-pary-bezpieczne) (safe-table)  (bets <=1) (rank two-pairs))
(<- (hf-bet >dwie-pary-bezpieczne) (safe-table) (progn (rank > two-pairs)))

(<- (hf-call dobra-karta) (safe-table) (progn (rank >= trips)))
(<- (hf-call dobry-kolor) (rank color) (= 3 (game-prop 'max-of-colors)))
(<- (hf-call dobre-outsy) (< (length +table-cards+) 5) (progn (holdem-fixed-good-odds? (holdem-outs)))  )

;; ------------------------------- ON FLOP RULES --------------------------------
(<- (hf-flop-bet good-pair) (bets 0) (good-pair))


;; ------------------------------- ON TURN RULES --------------------------------
(<- (hf-turn-bet pair1) (bets 0) (= 0 (bets 'flop))  (rank pair))