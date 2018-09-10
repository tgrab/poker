(def-url-0 "/showgame"
    (main-page
      (let ((id (get-number-parameter "id")))
	(when (and id (get-parameter "start"))
	  (when (/= id (game-id +game-log+))
	    (read-game id)
	    (set-game-kind (game-kind +game-log+))
	    (setq +agent-name+ (get-player-name (get-number-parameter "agent")))
	    (move-to-percept (get-number-parameter "percept"))))

	(unless (get-parameter "start") (get-percept))
	(format *html-stream* "<a href='/showgame?id=~A'>Next percept</a>" id)
	(dotimes (i 10)
	    (awhen (aref  (game-cards +game-log+) i)
	      (html (:b ((:a href (format nil "/reloadgame?agent=~A" (get-log-p-name i))) (:princ (get-log-p-name i))))
		    (:princ (karty->html it)) 
		    (when +table-cards+
		      (if (= 4 (length it))
			  (html (:princ (render-rank (omaha-ocena it +table-cards+))) " , ")
			  (html (:princ (render-rank (ocena (append it +table-cards+)))) " , ")) )
		    :hr )))

	(when +table-cards+ (html (:princ (karty->html +table-cards+)) :hr))
	(when +agent-action+ (html (:b "Action for " (:princ +agent-name+) ": " ) (:princ +agent-action+) :br :br))
	(html (:b "+history+ ") (:princ (reverse +history+)) :br (:b "+pot+ ") (:princ +pot+) :hr)

	(html "<table border='1'>")
	(dotimes (i (length +name->seat+))
	  (html "<tr><td>"(:b (:princ (first (nth i (reverse +name->seat+))))) 
		"<td>" (:princ (aref +active-players+ i))
		"<td>" (:princ (aref +balance+ i)) 
		"<td>" (:princ (aref +total-balance+ i))
		"<td>" (:princ (reverse (aref +player-history+ i))) ))
	(html "</table>")
	(when (null (game-history +game-log+)) (html (:h2 "Finished!")))
	)))

