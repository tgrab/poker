(defvar *db* (format nil "~A/baza.dat" *root*))

(defun query(q) 
  (sqlite:with-open-database (con *db*) 
    (sqlite:execute-to-list con q)))

(defun dodaj-comment(y m d cont)
  (sqlite:with-open-database (con *db*) 
    (sqlite:execute-non-query con "insert into comments values(NULL,?,?,?,?)" y m d cont)))


