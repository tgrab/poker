(define-easy-handler (update :uri (fmt "~A/update" *ctxt*)) ()
  (page "Gameslist"

    (let ((ile 0))

    (when (eq *selector* 'day)
      (dolist (tables (get-log-tables *logyear* *logmonth* *logday*))
	(dolist (l (get-log-games *logyear* *logmonth* *logday* tables)) 
	  (when 
	      (and l
		   (not (find (parse-integer l) *log-games1* :key #'log-id )))
	    (incf ile)
	    (push (read-log l *logyear* *logmonth* *logday* tables) *log-games1*)) )))

    (when (eq *selector* 'tname)
       (dolist (l (sort 
		  (get-log-games *logyear* *logmonth* *logday* *logtablename*) 
		  #'string<))
	(when (and l
		   (not (find (parse-integer l) *log-games1* :key #'log-id )))
	  (incf ile)
	  (push (read-log l) *log-games1*) )) )
    (when *logday*
      (out "<h2>selected day:" *logday* (cdr (assoc *logmonth* *monthnames* :test #'equal)) "</h2>"))
    (if (eq *selector* 'tname)
	(out "<h1>" (third (second(log-history (car *log-games1*)))) "$" *logtablename* "</h1>")
	(out "<h2>" ile  "games added ... </h2>"))
    (when (eq *selector* 'tname)
      (dolist (g *log-games1*)
	(a (:href (fmt "~A/showgame?id=~A" *ctxt* (log-id g))) (log-date g))
	(out (cards-view (log-holecards g)) (log-balance g) (if (log-shown g) "[S]" "") )
	(out "<br>") ))))        )