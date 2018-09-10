(setq *poker-rules* nil)

(setq *o-pf-levels*
 '((L1.0 bets-no predict-passive)
   (L1.0 bets-no predict-no-bet)
   (L1.0 bets-1 stake-minimal)

   (L2.0 bets-no predict-bet)
   (L2.0 bets-1 stake-normal)

   (L3.0 bets-1 stake-big)

   (L4.0 bets-2)
   (L4.0 bets-3)

   (L2.0 L1.0)(L3.0 L2.0)    ))

(setq *o-holecards*
 '((H0.5 hand-small-pair hand-connectors)
   (H0.5 hand-small-pair hand-king-suited)
   
   (H0.8 hand-middle-pair hand-king-suited)
   (H0.8 hand-middle-pair hand-connectors)

   (H1.0 hand-ace-suited)
   (H1.0 hand-big-pair)
   (H1.0 hand-3-str8)
   (H1.0 hand-middle-pair hand-connectors)

   (H2.0 hand-3-str8 hand-connected) ;JT97
   (H2.0 hand-2pairs)
   (H2.0 hand-str8)

   (H3.0 hand-ace-suited hand-3-str8)
   (H3.0 hand-ace-suited hand-aces)

   (H4.0 hand-ace-suited hand-aces hand-connectors)

   (H1.0 H2.0) (H2.0 H3.0) (H3.0 H4.0) ))



; o-fold - najbardziej szczegolowa
; o-dont-bet - zabezpieczenie przed groznym bet
; o-potbet1 o-potbet2 o-bet 
; o-call - najbardziej ogolne 
; * - ostatnia dodana regula

;;---------------------------  PRE FLOP ---------------------------------------

; Level : bets-no predict-passive
(<- o-call 1     pf01  preflop l1.0       h1.0)
(<- o-call 0.6   pf02  preflop l1.0     h0.8)
(<- o-call 0.20  pf03  preflop l1.0    h0.5)

; Level : bets-1 stake-normal
(<- o-call 1  pf04  preflop l2.0    h2.0)

; Level : bets-1 stake-big
(<- o-call 1    pf05  preflop l3.0    h3.0)
(<- o-call 0.8  pf06  preflop l3.0  stake-commited  h2.0)
(<- o-call 0.6  pf06  preflop l3.0    h2.0)

;Level : bets-2
(<- o-potbet1 0.7  pf07  preflop l4.0    h4.0)
(<- o-call 1       pf08  preflop l4.0    h4.0)
(<- o-call 0.7     pf09  preflop l4.0    h3.0)


;;---------------------------  UNIVERSAL ---------------------------------------
(<- o-potbet1 1 u01      rank-top-trips bets-no table-no-str8 table-no-colors)
(<- o-potbet1 1 u02      rank-fullhouse bets-no table-no-str8 table-2pairs)
(<- o-potbet1 1 u03      rank-set bets<=1 table-safe)
(<- o-potbet1 1 u04      rank-nuts bets-yes)

(<- o-call 1 u00         stake-zero)
(<- o-call 1 u05         stake-minimal rank-trips)
(<- o-call 1 u06         stake-normal rank-trips table-no-colors table-no-str8)
(<- o-call 1 u07         rank-fullhouse)
(<- o-call 1 u08         stake-minimal rank-color-nuts) ;* moze lezec para

;;---------------------------  FLOP ---------------------------------------

(<- o-potbet1 1   f01      flop bets-no table-safe-almost rank-str8 outs-some)
(<- o-potbet1 1   f02      flop bets-no rank-fullhouse) ;nie jesli trojka na stole!
(<- o-potbet1 0.7 f03      flop players-left-2 bets-no rank-top-2pairs act-last) ;TODO
(<- o-potbet1 0.6 f04      flop rank-top-trips bets<=1 table-no-str8 table-no-colors)
(<- o-potbet1 0.4 f05      flop rank-trips bets-1 stake-normal rank-ace-kicker table-no-str8 table-no-colors)

(<- o-potbet2 1 f06      flop bets-no rank-nuts) ;slowplaying
(<- o-potbet2 1 f07      flop bets-no table-no-str8 table-no-colors rank-top-trips)
(<- o-potbet2 1 f08      flop bets-no table-safe outs-some outs-some-color)
;(<- o-potbet2 0.2 f20      flop bets-no bets-preflop-no table-safe odds-good);*

(<- o-call 1   f09      flop odds-implied)
(<- o-call 1   f10      flop stake-minimal rank-set table-safe-no)
(<- o-call 1   f11      flop stake-minimal rank-overpair table-pairs)
(<- o-call 1   f12      flop rank-top-trips)
(<- o-call 1   f13      flop rank-trips bets-1)
(<- o-call 1   f14      flop stake-minimal rank-2pairs table-safe)
(<- o-call 0.5 f15      flop rank-top-trips)
(<- o-call 0.5 f16      flop stake-minimal outs-some-color players-left<=3)
(<- o-call 0.9 f17      flop bets-1 table-safe rank-top-2pairs)
(<- o-call 0.8 f18      flop rank-str8 rank-not-nuts table-safe-almost stake-big)
(<- o-call 0.5 f19      flop rank-overpair table-safe stake-minimal) 

;;---------------------------  TURN ---------------------------------------

(<- o-potbet1 1   t01       turn rank-nuts)
(<- o-potbet1 1   t02       turn rank-fullhouse stake-minimal table-2pairs)
(<- o-potbet1 0.6 t03       turn rank-top-trips bets<=1 table-no-str8 table-no-colors)
(<- o-potbet1 0.8 t04       turn rank-trips bets-no table-no-str8 table-no-colors rank-ace-kicker bets-flop-no)

(<- o-call 1   t11          turn odds-implied)
(<- o-call 1   t12          turn stake-minimal outs-some);*
(<- o-call 0.5 t05          turn rank-str8 table-safe-almost bets-1 stake-normal)
(<- o-call 0.5 t06          turn rank-top-trips)
(<- o-call 0.5 t07          turn odds-strong-implied bets-1)
(<- o-call 0.7 t08          turn stake-minimal rank-set table-safe-no)
(<- o-call 0.5 t09          turn stake-minimal rank-2pairs rank-top-pair table-safe)
(<- o-call 0.5 t10          turn rank-str8 stake-big table-safe-almost rank-not-nuts)

;;---------------------------  RIVER ---------------------------------------

(<- o-potbet1 1   r01       river rank-nuts)
(<- o-potbet1 0.6 r02       river rank-top-trips bets<=1 table-no-str8 table-no-colors)

(<- o-call 1   r03     river rank-color-nuts bets-1 stake-normal)
(<- o-call 0.4 r04     river rank-color-nuts bets-1)
(<- o-call 0.5 r05     river rank-str8 table-safe-almost bets-1 stake-normal)
(<- o-call 0.5 r06     river rank-top-trips) ;? niebezpieczne
(<- o-call 0.3 r07     river stake-minimal rank-set table-safe-no)
(<- o-call 0.5 r08     river stake-minimal rank-top-pair table-no-colors table-no-str8)
(<- o-call 0.3 r09     river rank-str8 table-safe-almost stake-big rank-not-nuts)
(<- o-call 0.5 r10     river rank-color-not-nuts table-no-pairs bets-1);TODO tylko wysokie kolory, male z nizszym wsp.


;;--------------------------------------------------------------------------------------------
(<- o-call 1 rl1     loose+1 river rank-color-strong table-no-pairs bets-1)
(<- o-call 1 rl2     loose+1 preflop stake-normal stake-commited)