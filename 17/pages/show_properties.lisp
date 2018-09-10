(defun view-show-properties()
  (main-page (:with-menu t)
    (:table :border "1"
     (fmt "<tr><td>pot:<td>~A" (pot +game+))
     (fmt "<tr><td>stake:<td>~A" (stake +game+))
     (fmt "<tr><td>blind:<td>~A" (blind +game+))
     (fmt "<tr><td>kind:<td>~A" (kind +game+))
     (fmt "<tr><td>balance:<td>~A" (balance +game+))
     (fmt "<tr><td>total-balance:<td>~A" (total-balance +game+))
     (fmt "<tr><td>history:<td>~A" (history +game+))
     (fmt "<tr><td>player-history:<td>~A" (player-history +game+))
     (fmt "<tr><td>free-seat:<td>~A" (free-seat +game+))
     (fmt "<tr><td>agent-name:<td>~A" (agent-name +game+))
     (fmt "<tr><td>agent-pos:<td>~A" (agent-pos +game+))
     (fmt "<tr><td>name->seat:<td>~A" (name->seat +game+))
     (fmt "<tr><td>table-size:<td>~A" (table-size +game+))
     (fmt "<tr><td>taken-seats:<td>~A" (taken-seats +game+))
     (fmt "<tr><td>active-player:<td>~A" (active-players +game+))
     (fmt "<tr><td>table-cards:<td>~A" (table-cards +game+))
     (fmt "<tr><td>cards:<td>~A" (cards +game+))
     (fmt "<tr><td colspan='2'>")
     (when (eq (type-of +game+) 'game-log)
       (fmt "<tr><td>id:<td>~A" (id +game+))
       (fmt "<tr><td>czas:<td>~A" (czas +game+))
       (fmt "<tr><td>table-name:<td>~A" (table-name +game+))
       (fmt "<tr><td>percept-nr:<td>~A" (percept-nr +game+))
       (fmt "<tr><td>players-log:<td>~A" (players-log +game+))
       (fmt "<tr><td>table-cards-log:<td>~A" (table-cards-log +game+))
       (fmt "<tr><td>money-log:<td>~A" (money-log +game+))
       (fmt "<tr><td>cards-log:<td>~A" (cards-log +game+))
       (fmt "<tr><td>history-log:<td>~A" (history-log +game+))
       )
 	)))
  
(def-url "/show_properties" view-show-properties)