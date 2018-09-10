(def-url-0 "/describeplayer"
    (main-page
      (let* ((pl (get-parameter "player"))
	    (q (kwery "SELECT comment FROM comments WHERE player_nr=~A" (get-player-nr pl t))))
	(dolist (c (prima-query q))
	  (html (:princ c))))))
