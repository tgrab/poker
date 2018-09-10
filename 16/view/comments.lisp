(def-url-0 "/insertcomment"
      (let ((plr (get-parameter "player"))
	    (com (get-parameter "comment"))
	    (id (or (get-number-parameter "id") "NULL"))
	    query)
	(when (and plr com)
	  (setq query  (format nil "INSERT INTO comments VALUES(~A,DEFAULT,'~A',~A)"
			       (get-player-nr plr)
			       com
			       id))
	  (prima-query query)) 	  
   ))


(def-url-0 "/showcomments"
    (main-page
	(html ((:form method "post" action "/insertcomment")
	       "Player's name:" ((:input name "player" value (if plr plr ""))) :br
	       "Comment:" ((:input name "comment")) :br
	       ((:input type "submit" value "Insert comment")) ))
      (dolist (c (prima-query "SELECT * FROM comments"))
	(html (:princ c)))))
