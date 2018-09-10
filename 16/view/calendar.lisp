(defun ilosc-gier(day month year &optional (game-kind 3))
  (prima-query
   (kwery "select count(*) from games where ~A AND game_kind=~A" 
		(sql-dzien day month year) game-kind)))



(defun show-ilosc-gier(d m y ilosc kind)
  (if (plusp ilosc)
      (format nil "<a href='/search?from_day=~A&from_month=~A&from_year=~A&kind=~A&checked'>~A</a>" d m y kind ilosc)
      ""))

(def-url-0 "/calendar"
      (let ((kind (or (get-number-parameter "kind") 3))
	    (rok (or (get-number-parameter "year") 2007)))
    (main-page
      "<h1>"
      (html (:princ rok))
	  (case kind
	    (3 (html " Omaha Pot Limit Games"))
	    (2 (html " Holdem Fixed Games")))
       "</h1>"
       "Choose: " :br
       "<a href='/calendar?year=2006&kind=2'>[ 2006 Holdem Fixed Games ]</a><br>"
       "<a href='/calendar?year=2006&kind=3'>[ 2006 Omaha Pot Limit Games ]</a><br>"
       "<a href='/calendar?year=2007&kind=2'>[ 2007 Holdem Fixed Games ]</a><br>"
       "<a href='/calendar?year=2007&kind=3'>[ 2007 Omaha Pot Limit Games ]</a><br>"
       :hr
      (dotimes (m 12)
      (html
       (:princ (format nil "<a href='/monthcalendar?month=~A&year=~A&kind=~A'>~A  </a>" (1+ m) rok kind (car (nth m *calendar*))))
       (:princ
	(caar
	 (prima-query
	  (kwery "SELECT count(*) FROM games WHERE date_part('year',czas)=~A AND date_part('month',czas)=~A AND game_kind=~A" rok (1+ m) kind))))  
       :br)
      ))))


(def-url-0 "/monthcalendar"
      (let ((kind (or (get-number-parameter "kind") 3))
	    (mindex (1- (or (get-number-parameter "month") 1)))
	    (rok (or (get-number-parameter "year") 2007)))
	(main-page
	  "Choose Holdem Fixed Limit Games:"
	  (dotimes (m (length *calendar*))
	    (html (:princ (format nil "<a href='/calendar?month=~A&year=~A&kind=2'>[~A ~A]</a>"
				  (1+ m) rok (car (nth m *calendar*)) rok))))
	  :br
	  "Choose Omaha Pot Limit Games:"
	  (dotimes (m (length *calendar*))
	    (html (:princ (format nil "<a href='/calendar?month=~A&year=~A&kind=3'>[~A ~A]</a>"
				  (1+ m) rok (car (nth m *calendar*)) rok))))
	  :hr
	  (case kind
	    (3 (html "Omaha Pot Limit Games"))
	    (2 (html "Holdem Fixed Games")))
	"<table border='1'>"
	"<tr><td>Month<td>1<td>2<td>3<td>4<td>5<td>6<td>7<td>8<td>9<td>10<td>11<td>12"
	"<td>13<td>14<td>15<td>16<td>17<td>18<td>19<td>20<td>21<td>22<td>23<td>24"
	"<td>25<td>26<td>27<td>28<td>29<td>30<td>31"
	(html "<tr><td>" (:princ (car (nth mindex *calendar*))))
	  (dotimes (d (cdr (nth mindex *calendar*)))
	    (html "<td>" (:princ (show-ilosc-gier (1+ d) (1+ mindex) rok
					       (caar (ilosc-gier (1+ d) (1+ mindex) rok kind)) kind))))) 
	"</table>")    )