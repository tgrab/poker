(defun licz-gry(d m)
    (prima-query 
	(format nil 
	    "SELECT Count(*) FROM games WHERE  czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59'" d m 2007 d m 2007)))
