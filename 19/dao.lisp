(in-package :db)

(defmacro with-poker-db(&body body)
  `(let ((con (plain-odbc:connect "poker" "root" "xbarg2"))
	 (res nil))
     (setq res (progn ,@body))
     (plain-odbc:close-connection con)
     res))

(defun poker-query(q) 
  (with-poker-db (plain-odbc:exec-query con q)))


(defun poker-update(q)
  (let ((con (plain-odbc:connect "poker" "root" "xbarg2")))
    (plain-odbc:exec-update con q)
    (plain-odbc:commit con)     
    (plain-odbc:close-connection con)))


;;---- usefull tools --------------------
(defvar *calendar* '((|January| . 31) (|February| . 28) (|March| . 31)
			   (|April| . 30) (|May| . 31)(|June| . 30)
			   (|July| . 31)(|August| . 31) (|September| . 30)
			   (|October| . 31) (|November| . 30)(|December| . 31) ))


(defun search-day-games(day month year)
  (let ((begin (encode-universal-time 0 0 0 day month year))
	(end (encode-universal-time 59 59 23 day month year)))
    (poker-query (format nil "select id from crypto where czas>~A and czas<~A" begin end))))

(defun search-month-games(month year)
  (let ((begin (encode-universal-time 0 0 0 1 month year))
	(end (encode-universal-time 59 59 23 (cdr (nth (1- month) *calendar*)) month year)))
    (poker-query (format nil "select id from crypto where czas>~A and czas<~A" begin end))))

(defun game-descr(lista)
  (dolist (g lista)
    (when g (format t "#~A {~A}=~A ~A~&" (first g) (second g) (dekoduj-czas (second g)) (third g) ))))

(defun search-year-games(year)
  (let ((begin (encode-universal-time 0 0 0 1 1 year))
	(end (encode-universal-time 59 59 23 31 12 year)))
    (poker-query (format nil "select id,czas,table_name from crypto where czas>~A and czas<~A order by table_name,czas" begin end))))


(defun create-games-list-set(start-period end-period table-name &optional (casino "crypto"))
  (poker-update (format nil "insert into games_list values(DEFAULT,~A,~A,'~A','~A')" start-period end-period casino table-name ))  )

(defun get-games-list(nr)
  (let ((zbior (poker-query (format nil "select * from games_list where id=~A" nr))))
    (when zbior
      (let ((pocz (nth 1 (car zbior))) 
	    (kon (nth 2 (car zbior)))
	    (cas (nth 3 (car zbior)))
	    (tn (nth 4 (car zbior))))
	(poker-query (format nil "select id from ~A where czas>=~A and czas<=~A and table_name='~A'" cas pocz kon tn))
    ))))