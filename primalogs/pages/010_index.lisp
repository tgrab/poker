(define-easy-handler (index0 :uri (fmt "~A/" *ctxt*)) ()
 (redirect *ctxt*))


(define-easy-handler (index :uri *ctxt*)  (year month day selector)
  (page "Primalogs"
    (when selector
      (cond
	((string= selector "tname") (setq *selector* 'tname))
	((string= selector "day") (setq *selector* 'day))
	((string= selector "month") (setq *selector* 'month))
	((string= selector "year") (setq *selector* 'year))       ))
    (when year (setq *logyear* year *logmonth* nil *logday* nil))
    (when month (setq *logmonth* month))
    (when day (setq *logday* day))

    (unless *logyear*
      (out "select year")
      (return-from index))

    (out "<h1>Selected year:" *logyear* "</h1>")

    (when *logday*
      (out "<h2>selected day:" *logday* (cdr (assoc *logmonth* *monthnames* :test #'equal)) "</h2>")
      (when (eq *selector* 'tname)
	(dolist (tn (get-log-tables *logyear* *logmonth* *logday*))
	  (a (:href (fmt "~A/gameslist?tn=~A" *ctxt* tn) :style "font-size:0.8em") tn )
	  (out "#" (length (get-log-games *logyear* *logmonth* *logday* tn)) "<br>"))))

    (dolist (m (sort (get-log-months *logyear*) #'string> ))
      (out "<hr><h2>" (cdr (assoc m *monthnames* :test #'equal)) "</h2>" )
      (dolist (d (sort (get-log-days *logyear* m) #'string<))
	      (if (eq *selector* 'tname)
		(a (:href (fmt "?year=~A&month=~A&day=~A" *logyear* m d) :style "margin:2px") d)
		(a (:href (fmt "~A/gameslist?year=~A&month=~A&day=~A" *ctxt* *logyear* m d) :style "margin:2px") d))))

    (out "<hr> selection type:" *selector*)
    (out "<form>")
    (out "<select name='selector'>")
    (out "<option value='tname'>Tables")
    (out "<option value='day'>Day")
    (out "<option value='month'>Month")
    (out "<option value='year'>Year")
    (out "</select>")
    (out "<input type='submit' value='modify'>")
    (out "</form>")
    ))
 
 
 