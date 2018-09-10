(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :dekoduj-czas :permutacje 
	   :aif :awhen :it :uruchom-program :lispdoc))


(asdf:oos 'asdf:load-op :plain-odbc)
(defpackage :db
  (:use :cl :moje)
  (:export :poker-query :poker-update :with-poker-db))

(defpackage :poker.ranks
  (:use :cl :moje)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))


(defpackage :poker
  (:use :cl :moje :poker.ranks)
  (:export :+game+ :*agent-name* :*games-list*
           :action :set-game-kind :new-game :small-blind :big-blind
	   :choose-startgame :next-percept :create-infos
	   :call :fold :bet :allin :round-flop :round-turn :round-river :holecards  
	   :game :game-info :player-info :percept-memo :round-memo :game-memo 
	   :memo :rules :predicates
	   :table-cards :cards :table-size :blind :name->seat :agent-name :agent-pos
	   :table-name :money-log :pre-flop-actions :karty->napisy :on-flop-players
	   :winners-log :uncalled-bets :gain :bad-actions :remarks
	   :*figury* :*o* :h1 :h2 :h3 :h4 :hc1 :hc2 :hc3 :hc4 :t1 :t2 :t3 :t4 :t5 
	   :tc1 :tc2 :tc3 :tc4 :tc5 :*speech* :*omaha-size-limit* :*fixed-size-limit* 
	   :kolor :color :figura :rank :table-colors :players-left :*kind*
	   :table-pairs :table-pairs2 :one-str8-table? :two-str8-table? 
	   :preflop? :flop? :turn? :river? :flop1 :flop2 :flop3 :flopc1 :flopc2 :flopc3
	   :turn1 :turnc1 :river1 :riverc1 :player-stake :active-players
	   :pot :total-balance :free-seat :history :id :czas :casino :kind :history-log
	   :cards-log :player-at :bets-history :player-at-seat :player-money
	   :game-round :check-raise? :first-to-act? :last-to-act?  :players-list :player-pos 
	   :last-action :maniac? :count-active-players :*cortex* :*player-infos* 
	   :odds :pot-odds :get-stake :balance :omaha? :holdem? :fixed?
	   :insert-player :insert-player? :pre-flop-bets :karty->html :taken-seats
	   :render-rank :sb :bb :c :b :r :a :f :flop :turn :river

  ))
;(defpackage :poker.logic
;  (:use :cl :poker.main)
  ;(:import-from :poker.main :+game+ :percept-memo :round-memo :game-memo :memo :rules :predicates)
;  (:export :?- :<-into :<=into :<- :add-clause :set-clause :rozwin-regule :rozwin-fuzzy-regule
;	   :clear-game-memory :clear-round-memory :clear-db :extend-bindings :variable-p))


;(defpackage :poker.holdem
;  (:use :cl :poker.ranks :poker.main :poker.logic)
;  (:export :holdem-round-logic :sklansky-group :holdem-count-outs))

;(defpackage :poker.omaha
;  (:use :cl :poker.ranks :poker.main :poker.logic)
;  (:export  :omaha-round-logic :omaha-outs-logic :omaha-action-prove))

;(defpackage :poker.logs
;  (:use :cl :moje :poker.main)
;  (:import-from :poker.logic :extend-bindings :variable-p)
;  (:export :pfr :pfr-short :describe-pfr :get-profile-from-cortex :vpip
;	   :add-player-info :clear-player-infos :get-game-infos :make-logs
;	   :logs-prev-nr :logs-current-nr :logs-next-nr :get-player-infos
;	   :logs-percept-nr :logs-game :save-game :*log* :*range* :tokens :make-game-profiles
;	   :same-action? :list-directory :directory-pathname-p))


(defpackage :http
  (:use :cl :moje)
  (:export :start-server :def-url :def-mvc :param :nparam :*log-stream* :*log-request* :*log-list*))


(asdf:oos 'asdf:load-op :cl-who)
(defpackage :view
  (:use :cl-who :cl :moje :poker :poker.ranks :http))