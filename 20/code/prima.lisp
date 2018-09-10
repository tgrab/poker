(in-package :poker)


(defmacro with-prima-db(&body body)
  `(let ((con (plain-odbc:connect "prima" "" ""))
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

(defstruct prima-seat
 num
 alias
 balance
 endbalance)

(defstruct prima-game
 seats
 actions)

(defun prima-date(handdate)
  (let ((year (subseq handdate 0 4))
	(month (subseq handdate 4 6))
	)

   (mapcar #'read-from-string (list year month))))

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
	   
(defvar *prima-agent* "grabola")
(defvar *prima-records* nil)
(defvar *prima-table* nil)
(defvar *prima-game* nil)

(defun set-prima-records(&optional (ile 20))
  (setq *prima-records*
	(mapcar #'create-prima-record (prima-query
				       (format nil "select * from handhistory order by handdate desc limit 0 , ~A" ile))))
  'ok)

(defun prima-record-date(rec)
  (nth 4 (first (prima-record-xmldump rec)) ))

(defun prima-record-seats(rec)
  (mapcar #'car (cdr (second (prima-record-xmldump rec)) )))

(defun prima-record-player(seat)
  (list (nth 4 seat)
	(- (read-from-string (nth 10 seat)) (read-from-string(nth 8 seat)))  ))

(defun find-balance(seats &optional (plr *prima-agent*))
  (dolist (s seats)
    (let ((r (prima-record-player s)))
      (when (string= plr (first r)) (return-from find-balance (second r))))))

(defun prima-get-records(&optional (tbl *prima-table*))
  (mapcan
   #'(lambda (r)
       (if tbl 
	   (when (string= tbl (nth 8 (first (prima-record-xmldump  r))))
	     (list r))
	   (list r)))
   *prima-records*))

(defun view-prima-records(&optional  (tbl *prima-table*))
  (format t "<table border='1'>")
  (dolist (r (prima-get-records tbl))
    (format t "<tr><td><a href='/game?id=~A'>~A</a><td>~A<td>~,2F~&" 
	    (nth 2 (first (prima-record-xmldump  r))) 
	    (prima-record-date  r)
	    (nth 8 (first (prima-record-xmldump  r)))
	    (aif (find-balance (prima-record-seats r))
		 (if (= it 0) "" it)
		 "") )    )
  (format t "</table>"))

(defun create-prima-seat(seats-list)
  (let ((s (make-prima-seat)))
    (setf (prima-seat-num s) (nth 2 seats-list))
  s))

(defun prima-zakoduj-date(data)
  ;(zakoduj-date "2008-12-23 11:29:05")
  (destructuring-bind (rok mies dzien godz min sek)
		      (mapcar #'read-from-string
			      (tokens data '(#\- #\: #\Space)))
    (encode-universal-time sek min godz dzien mies rok)))

(defun prima-simulate(record)
  (let ((game (first  (prima-record-xmldump  record)))
	(seats (prima-record-seats record))
	(actions  (cdr (third (prima-record-xmldump record)) )) )
  (setq *prima-game* 
	(new-game 
	 :id (nth 2 game)
	 :czas (prima-zakoduj-date (nth 4 game))
	 ))
  (dolist (a actions)
    (let ((akind (nth 4 (car a))))
      (when (string= akind "SmallBlind")
	(small-blind
	 (nth 4 (nth (1- (parse-integer (nth 6 (car a))))  seats))
	 (read-from-string (nth 8 (car a)))
	 *prima-game* 	      ))
      (when (string= akind "BigBlind")
	(big-blind 
	 (nth 4 (nth (1- (parse-integer (nth 6 (car a))))  seats))
	 (read-from-string (nth 8 (car a)))
	 *prima-game* 	      ))
      (when (string= akind "Call")
	(call
	 (nth 4 (nth (1- (parse-integer (nth 6 (car a))))  seats))
	 (read-from-string (nth 8 (car a)))
	 *prima-game* 	      ))
      (when (string= akind "Check")
	(call
	 (nth 4 (nth (1- (parse-integer (nth 6 (car a))))  seats))
	 0
	 *prima-game* 	      ))
      (when (string= akind "Bet")
	(bet
	 (nth 4 (nth (1- (parse-integer (nth 6 (car a))))  seats))
	 (read-from-string (nth 8 (car a)))
	 *prima-game* 	      ))
      (when (string= akind "Fold")
	(fold
	 (nth 4 (nth (1- (parse-integer (nth 6 (car a))))  seats))
	 *prima-game* 	      ))
    ;(print akind)
    ))    ))


(defun prima-tables(&optional (rs *prima-records*))
  (let ((names nil))
    (dolist (r rs names)
      (pushnew (nth 8 (first (prima-record-xmldump  r)))   names :test #'equal ))))


(defvar *prima-action* -1)
(defvar *prima-record* nil)

(defun prima-find-record(id)
  (find-if 
   #'(lambda (dump)  
       (string= id (nth 2 (first dump))))
   *prima-records* 
   :key #'prima-record-xmldump ))