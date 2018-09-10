(defpackage :poker.dao
  (:nicknames :db)
  (:use :cl :moje)
  (:export :poker-query :poker-update))

(defpackage :poker.ranks
  (:use :cl :moje)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))

(defpackage :poker
  (:use :cl :moje :poker.ranks :http)
  (:export :*game* :*gameslist* :player :name :seat :game :game-log :head :get-player :free-seat
	   :flop :turn :river :holecards :sb :bb :c :b :r :f))

(defpackage :view
  (:use :cl :moje :cl-who :poker.ranks :poker :http))