;reprezentacja skonczonej gry w bazie danych
;inne formaty danych niz w game
(defclass game-log()
  ((id  :initarg :id :accessor id)
   (kind :initarg :kind :accessor kind)
   (czas :initarg :czas :accessor czas)
   (percept-nr :initform -1 :accessor percept-nr)
   (table-name :initarg :table-name :accessor table-name)
   (table-cards :initarg :table-cards :accessor table-cards)
   (cards :initform (make-array 10 :initial-element nil) :accessor cards)
   (players :initform nil :accessor players)
   (money :initform (make-array 10 :initial-element 0) :accessor money)
   (history :initarg :history :accessor history);inny format niz w game!!
   ))

(defvar +game-log+ (make-instance 'game-log))

;;--------------------------------------------------------------
(defmacro with-prima-connection(&body body)
  `(let ((con (pg-connect "prima" "postgres" :port 5433))
	 (result nil))
    (unwind-protect
	 (setq result (progn ,@body))
      (when con (pg-disconnect con)))
    result))

(defmacro prima-query( query )
  `(with-prima-connection 
    (pg-result (pg-exec con ,query) :tuples)     ))

(defparameter *calendar* '((|January| . 31) (|February| . 28) (|March| . 31)
			   (|April| . 30) (|May| . 31)(|June| . 30)
			   (|July| . 31)(|August| . 31) (|September| . 30)
			   (|October| . 31) (|November| . 30)(|December| . 31) ))

(defun kwery(tekst &rest opcje)
  (apply #'format `(nil ,tekst ,@opcje)))

(defun set-game-is-checked(id)
  (prima-query
   (kwery "UPDATE games SET checked='true' WHERE id=~A" id)))

(let ((names (make-hash-table)))
  (defun get-player-name-table() names)
  (defun get-player-name( nr )
    (or (gethash nr names)
	(progn
	  (let ((val (caar (prima-query (format nil "SELECT name FROM players WHERE nr=~A" nr)))))
	    (setf (gethash nr names)  val)  	  )))))

(let ((names (make-hash-table :test #'equal)))
  (defun get-player-nr-table() names)
  (defun get-player-nr( name &optional (read-only nil))
    (or  (gethash name names)
	 (with-prima-connection
	   (aif (caar (pg-result (pg-exec con (format nil "SELECT nr FROM players WHERE name='~A'" name)) :tuples))
		(setf (gethash name names) it)		  
		(progn
		  (when read-only (return-from get-player-nr 0))
		  (pg-exec con (format nil "INSERT INTO players VALUES(DEFAULT,'~A')" name))
		  (setf (gethash name names)
			(caar (pg-result (pg-exec con (format nil "SELECT nr FROM players WHERE name='~A'" name)) :tuples)))))))))

(defun str->list(str)
  (read-from-string (format nil "(~A)" str)))

(defun read-game(id)
  (let ((res (car (prima-query (format nil "SELECT * FROM GAMES WHERE id=~A" id)))))
    (when res
      (setq +game-log+ (make-instance 'game-log
			:id (nth 0 res)
			:czas (nth 1 res)
			:kind (nth 2 res)
			:table-name (nth 3 res)
			:table-cards (str->list (nth 4 res))
			:history (str->list (nth 7 res))))
      (dolist (e (str->list (nth 6 res))) 
	(push (cons (first e) (get-player-name (second e)) ) (players +game-log+))
	(setf (aref (money +game-log+) (first e)) (third e)  )
	(when (fourth e) 
	  	(setf (aref (cards +game-log+) (first e)) (cadddr e)  ) ) 	) )))