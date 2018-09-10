(def-url-0 "/gameslist"
    (main-page
	(dolist (g +games-list+)
	  (html (:princ (third g)) "$ " )
	  (case (second g)
	    (3 (html "<b>Omaha</b> Pot Limit     "))
	    (1 (html "<b>Holdem</b> No Limit     "))
	    (0 (html "<b>Holdem</b> Fixed Limit  ")))  
	  (html ((:a href (format nil "/gamelog?id=~A" (first g))) "[" (:princ (first g)) "]") " at " (:princ (time->napis (fourth g)))   :br)
	  )
      ))


