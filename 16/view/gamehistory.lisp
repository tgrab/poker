(def-url-0 "/gamehistory"
    (main-page

      (let ((id (get-number-parameter "id")))
	(when id (read-game id))
	(html
	 ((:table border 1)
	 ; :tr (dolist (plr (game-players +game-log+))  (html :td (:princ plr)) )
	  (dolist (p (copy-list (game-history +game-log+)))
	    
	    (if (consp p)
		(let ((nr (first p))
		      (action (second p)))
		  (get-percept)
		  (html :tr
			(dotimes (i (length (game-players +game-log+))) 
			  (html :td (:princ (if (eq i nr) action "")  )))  ))
	    (progn (get-percept)
		   (html :tr ((:td colspan "9") (:princ p)))))
	    ) )))))

