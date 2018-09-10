(def-url-0 "/gamelog"
    (main-page

	(awhen (get-number-parameter "id") (read-game it) )

	(aif (get-parameter "agent")
	     (setq +agent-name+ it)
	     (setq +agent-name+ "grabola"))
 

	    (setq +agent-action+ nil)
	    (set-game-kind (game-kind +game-log+))

	(let* ((idx (position (game-id +game-log+) +games-list+ :key #'car))
	       (prev (1- idx))
	       (nxt (1+ idx)))
	  (when (plusp (1+ prev)) 
	    (html ((:a href (format nil "/gamelog?id=~A" (car (nth prev +games-list+)))) "[<<< Prev Game <<<]" )))
	  (when (< nxt (length +games-list+))
	    (html ((:a href (format nil "/gamelog?id=~A" (car (nth nxt +games-list+)))) "[>>> Next Game >>>]" )))
	  (html :hr))


	(case (game-kind +game-log+)
	    (3 (html "<b>Omaha Pot Limit</b><br>"))
	    (1 (html "<b>Holdem No Limit</b><br>"))
	    (0 (html "<b>Holdem Fixed Limit</b><br>")  ))
	(html
	   "Game " ((:a href "/reloadgame") (:princ (game-id +game-log+))) " at "  (:princ (time->napis (game-time +game-log+))) " " (:princ (length (game-players +game-log+))) " players table"  :hr)


	(do ()
	    ((null  (game-history +game-log+)))

	  (let ((p (car  (game-history +game-log+))))
	    ;(when (consp p)
	     ; (html (:princ (get-log-p-name (first p))))   )

	  (get-percept)

	(dotimes (i 10)
	    (awhen (aref  (game-cards +game-log+) i)
	      (html (:b ((:a href (format nil "/gamelog?agent=~A&id=~A" (get-log-p-name i) (game-id +game-log+))) (:princ (get-log-p-name i))))
		    (:princ (karty->html it)) 
		    (when +table-cards+
		      (if (= 4 (length it))
			  (html (:princ (render-rank (omaha-ocena it +table-cards+))) " , ")
			  (html (:princ (render-rank (ocena (append it +table-cards+)))) " , ")) )
		     )))

	(when +table-cards+ (html "<br>Table: " (:princ (karty->html +table-cards+)) :hr))
	(when +agent-action+ (html (:b "Action for " (:princ +agent-name+) ": " ) (:princ +agent-action+) :br :br))
	(html  ;(:b "+history+ ") (:princ (reverse +history+)) 
	       :br (:b "+pot+ ") (:princ +pot+) )

	(html "<table border='1'>")
	(dotimes (i (length +name->seat+))
	  ;(when (aref +active-players+ i)
	    (if (and (consp p) (string= (first (nth i (reverse +name->seat+)))  (get-log-p-name (first p))))
	      (html "<tr><td style='color:green'>"(:princ (first (nth i (reverse +name->seat+)))) 
		;"<td style='color:green'>" (:princ (aref +active-players+ i))
		"<td style='color:green'>" (:princ (aref +balance+ i)) 
		"<td style='color:green'>" (:princ (aref +total-balance+ i))
		"<td style='color:green'>" (:princ (reverse (aref +player-history+ i))) ) 

	      (html "<tr><td>"(:princ (first (nth i (reverse +name->seat+))))
		;"<td>" (:princ (aref +active-players+ i))
		"<td>" (:princ (aref +balance+ i)) 
		"<td>" (:princ (aref +total-balance+ i))
		"<td>" (:princ (reverse (aref +player-history+ i))) ) )   )
	;)
	(html "</table>"  :hr)   ))




     ))