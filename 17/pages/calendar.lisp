(defun count-games(&optional (k 3) (d 13) (m 10) (y 2007))
    (clsql:query (format nil "select count(*) from games where game_kind=~A and czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59'" k y d m  y d m)  ))

(defun count-by-day(&optional (k 3) (d 13) (m 10) (y 2007))
      (format nil "<a href='/select_games_list?kind=~A&day=~A&month=~A&year=~A'>~A</a>"
		 k m d y  (caar (count-games k d m y))))

(defun count-by-month(&optional (k 3) (m 10) (y 2007) )
    (with-html
	(fmt "<table border='1'>")
	(fmt "<tr><td>~A" (car (nth (1- m) *calendar*)))
       (dotimes (d (cdr (nth (1- m) *calendar*)))
		(fmt "<td>~A" (1+ d)))
	(fmt "<tr><td>")
       (dotimes (d (cdr (nth (1- m) *calendar*)))
	  (fmt "<td>~A" (count-by-day k m (1+ d) y)))
	(fmt "</table>")))

(defun view-count-month()
  (let ((k (or (get-parameter "kind") "3"))
	(m (or (get-parameter "month") "01"))
	(y (or (get-parameter "year") "2007")) )
    (main-page (:with-menu t)
      (princ (count-by-month (parse-integer k) (parse-integer m) (parse-integer y) ))  )))

(def-url "/calendar" view-count-month)