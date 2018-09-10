(def-url-0 "/players"
   (main-page
     (dolist (p (mapcar #'car (prima-query "SELECT name FROM players ORDER BY name ASC")))
       (html ((:a href (format nil "/describeplayer?player=~A" p)) (:princ p)) :br)        )))