(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena :zwyciezca))

(defpackage :poker.defs
  (:use :cl :anaphor :poker.ranks)
  (:export :game :agent :game-id :table-name :game-date :game-time
	   :small-blind :big-blind :button :game-kind :history :players :table-size
	   :table-cards :cards :agent-position :still-playing? :money-list :pot
	   :stake :actions-list :last-active-player :active-player :get-player
	   :get-player-pos
	   :player :name :money :seat :on-button :game-winner
	   :game-log :holdem :omaha :render-rank :game-rank
	   :*kolory* :*figury* :*color-htm* :karta->kolor :karta->figura
	   :karta->napis :karty->napisy :karta->html :karty->html
	   :napis->karta :napisy->karty
	   :rank= :rank< :rank> :rank>= :rank<=
	   :pre-flop :with-flop :post-flop :flop :turn :river 
	   :heads-up :shorthand :fulltable :on-small-blind :on-big-blind
	   :on-early-pos :on-middle-pos :on-late-pos :no-bets :one-bet
	   :with-bets :with-raise :re-raise :sklansky :agent-value
	   :table-value :one-pair :two-pairs :trips :street :color
	   :fullhouse :quads :straight-color
	   :c :b :f :sb :bb :flop :turn :river
	   :H1 :H2 :H3 :H4 :T1 :T2 :T3 :T4 :T5))



(defpackage :poker.rulebased
  (:use :cl :poker.defs :anaphor)
  (:export :rule-based-agent :make-rule-based-agent :rule-based-decision)  )

(defpackage :poker.engine
  (:use :cl :anaphor :poker.defs :poker.rulebased)
  (:export :percept-new-game :percept-new-game-log :percept-hand :percept-fold
	   :percept-check :percept-bet :percept-flop 
	   :percept-turn :percept-river :percept-get-action 
	   :next-player) )

(defpackage :poker.logs
  (:use :cl :anaphor :poker.defs :poker.engine ))

(defpackage :poker.db
  (:use :cl :anaphor :poker.defs :postgresql)
  (:export :get-game))

(defpackage :poker.web
  (:use :cl :anaphor :net.html.generator :poker.defs :poker.db :poker.logs :poker.engine)
  (:export :start-aserve))


(load (compile-file "poker.ranks.lisp" :output-file "/dane/lisp/poker.ranks.x86f"))
(load (compile-file "poker.defs.lisp" :output-file "/dane/lisp/poker.defs.x86f" ))
(load (compile-file "poker.rulebased.lisp" :output-file "/dane/lisp/poker.rulebased.x86f"))
(load (compile-file "poker.engine.lisp" :output-file "/dane/lisp/poker.engine.x86f"))
(load (compile-file "poker.logs.lisp" :output-file "/dane/lisp/poker.logs.x86f"))
(load (compile-file "poker.db.lisp" :output-file "/dane/lisp/poker.db.x86f" ))
(load (compile-file "poker.web.lisp" :output-file "/dane/lisp/poker.web.x86f"))

(import 'poker.defs:c)
(import 'poker.defs:b)
(import 'poker.defs:f)
(import 'poker.defs:sb)
(import 'poker.defs:bb)
(import 'poker.defs:flop)
(import 'poker.defs:turn)
(import 'poker.defs:river)

(import 'poker.web:start-aserve)

(defun start-system(&optional (port 9000))
    (start-aserve port))

(defun create-system()
;  #+sbcl(sb-ext:save-lisp-and-die "poker.exe" :toplevel #'start-system :executable t)
  #+sbcl(sb-ext:save-lisp-and-die "poker.core" :toplevel #'start-system)
  #+cmu(ext:save-lisp "poker.core" :init-function #'start-system))
