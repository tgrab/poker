(defun view-render-game(&optional (with-percept t))
  (with-html
    (let ((p (nth (1+ (percept-nr +game+)) (history-log +game+))))
	    (when (and 
		   (consp p) 
		   (string= (get-log-player-name (car p)) (agent-name +game+)))
	      (fmt "<h3 style='color:red'>Action: ~A</h3>" (action-prove))))
    (when (or (= -1 (percept-nr +game+)) with-percept) (next-percept))

	  (dotimes (i 10)
	    (awhen (aref  (cards-log +game+) i)
	      (fmt "<a href='/game?id=~A&agent=~A'>~A</a>~A" (id +game+) (get-log-player-name i) (get-log-player-name i)  (karty->html it)) 
	      (when (table-cards +game+)
		(if (= 4 (length it))
		    (princ (render-rank (omaha-ocena it (table-cards +game+))))
		    (princ (render-rank (ocena (append it (table-cards +game+))))) )
		(princ "<br>")) ))

	  "<hr>"
	(awhen (table-cards +game+)  (fmt "Table: ~A<br>" (karty->html it)))
	  (let ((p (nth (percept-nr +game+) (history-log +game+))))
	    (cond
	      ((null p) (fmt "<h2>finished !!!</h2>"))
	      ((consp p) (fmt  "Percept: <b>~A</b> ~A<br>" (get-log-player-name (car p)) (cdr p)))
	      (t (fmt "<b>~A</b><br>" p))))

	  ;(fmt  "<br>history: ~A, pot: ~A"  (reverse (history +game+)) (pot +game+) )
		  
	  "<table border=1>"
	  (dotimes (i (length (name->seat +game+)))
	    (if (aref (active-players +game+) i)
	    (fmt "<tr><td><b>~A</b><td>~A<td>~A<td>~A" 
		 (first (nth i (reverse (name->seat +game+))))
		 ;(aref (active-players +game+) i)
		 (aref (balance +game+) i)
		 (aref (total-balance +game+) i)
		 (reverse (aref (player-history +game+) i)) )	    
	    (fmt "<tr style='background-color: silver'><td><b>~A</b><td>~A<td>~A<td>~A" 
		 (first (nth i (reverse (name->seat +game+))))
		 ;(aref (active-players +game+) i)
		 (aref (balance +game+) i)
		 (aref (total-balance +game+) i)
		 (reverse (aref (player-history +game+) i)) ))   )
	  "</table>" )	)
  
(def-url "/render_game" view-render-game)