(in-package :poker)

(setq *poker-rules* nil)

; o-fold - najbardziej szczegolowa
; o-dont-bet - zabezpieczenie przed groznym bet
; o-potbet1 o-potbet2 o-bet 
; o-call - najbardziej ogolne 

(<- o-call 1 r1 (= stake 0))