(defun view-show-short-memory()
  (rules-percept-memo)
  (main-page (:with-menu t)
    (fmt "get stake:~A, " (get-stake))
    (fmt "pot odds:~A, " (pot-odds))
    (fmt "<hr>")
    "Memo:<br>"
    (show-hashtable (memo +game+))
    "<hr>Game memory:<br>"
    (show-hashtable (game-memo +game+))
    "<hr>Round memory:<br>"
    (show-hashtable (round-memo +game+))
    "<hr>Percept memory:<br>"
    (show-hashtable (percept-memo +game+))
 	))

(defun view-show-memory()
  (main-page (:with-menu t)
    (show-hashtable (rules +game+))
 	))
  
(def-url "/show_memory" view-show-memory)
(def-url "/show_short_memory" view-show-short-memory)