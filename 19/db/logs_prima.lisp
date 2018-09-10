#+(not :cffi)
 (progn
	(asdf:oos 'asdf:load-op 'plain-odbc)
      (asdf:oos 'asdf:load-op 's-xml))

(defmacro with-prima-db(&body body)
  `(let ((con (plain-odbc:connect "mylite" "" ""))
	 (res nil))
     (setq res (progn ,@body))
     (plain-odbc:close-connection con)
     res))

(defun prima-query(q) 
  (with-prima-db (plain-odbc:exec-query con q)))

;(car (prima-query "select * from HandHistory Limit 1"))
;dwa ostatnie:
;(prima-query "select * from handhistory order by handdate desc limit 0 , 2")

(defstruct prima-record
  handid
  handdate
  wasseated
  xmldump
  tablename
  gametype
  tabletype)


(defun create-prima-record(rec)
  (let ((r (make-prima-record)))
    (setf (prima-record-handid r) (nth 0 rec))
    (setf (prima-record-handdate r) (nth 1 rec))
    (setf (prima-record-wasseated r) (if (zerop (nth 2 rec)) nil t))
    (setf  (prima-record-xmldump r) (s-xml:parse-xml-string (nth 3 rec)))
    (setf (prima-record-tablename r) (nth 4 rec))
    (setf (prima-record-gametype r) (nth 5 rec))
    (setf (prima-record-tabletype r) (nth 6 rec))
    r))


(defun nth-prima-game(nr)
 "zwraca gre liczona od konca"
 (create-prima-record 
  (car
   (prima-query 
    (format nil 
	   "select * from handhistory order by handdate desc limit ~A , ~A" nr (1+ nr))))))