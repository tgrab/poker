(in-package :poker.db)

(defmacro with-prima-connection(&body body)
  `(let ((con (pg-connect "poker" "tgrab" :port 5433))
	 (result nil))
    (unwind-protect
	 (setq result (progn ,@body))
      (when con (pg-disconnect con)))
    result))

(defmacro prima-query( query )
  `(with-prima-connection 
    (pg-result (pg-exec con ,query) :tuples)     ))


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

(defun insert-table-query( game )
  (format nil "INSERT INTO table_descr VALUES(DEFAULT,'~A',~A,~A,~A,~A)"
	 (table-name game)
	 (length (players game))
	 (small-blind game)
	 (big-blind game)
	 (game-kind game)))

(let ((names (make-hash-table :test #'equal)))
  (defun get-table-nr-table() names)
  (defun get-table-nr( game )
    (or (gethash (table-name game) names)
	(with-prima-connection
	  (aif (caar (pg-result (pg-exec con (format nil "SELECT nr FROM table_descr WHERE name='~A'" (table-name game))) :tuples))
	       (setf (gethash (table-name game) names) it)
	       (progn
		 (pg-exec con (insert-table-query game))
		 (setf (gethash (table-name game) names)
		       (caar
			(pg-result
			 (pg-exec con 
				  (format nil "SELECT nr FROM table_descr WHERE name='~A'" (table-name game))) :tuples)))))))))


(defun insert-game-query( game )
  (format nil "INSERT INTO games VALUES('~A','~A','~A',~A,'~A','~A',~A,'~A','~A')"
	 (game-id game)
	 (game-time game)
	 (game-date game)
	 (get-table-nr game)
	 (format nil "~A" (table-cards game))
	 (format nil "~A" (history game))
	 (button game)
	 (create-player-array (mapcar #'(lambda(p)
					  (when p 
					    (get-player-nr (name p))))
				      (players game)))
	 (create-player-desc (players game))))

(defun insert-game( game )
  (with-prima-connection
    (unless (pg-result (pg-exec con (format nil "SELECT id FROM games WHERE id='~A'" (game-id game))) :tuples) 
      (pg-exec con (insert-game-query game)))))

(defun insert-games( game-list )
  (with-prima-connection
    (dolist (g game-list)
      (unless (pg-result (pg-exec con (format nil "SELECT id FROM games WHERE id='~A'" (game-id g))) :tuples) 
	(pg-exec con (insert-game-query g))))))

(defun create-player-array( player-names )
  (format nil "{~{~A~}~A}"
	  (mapcar
	   #'(lambda(n)
	       (if n
		   (format nil "~A," n)
		   "0,"))
	   (subseq player-names 0 (1- (length player-names))))
	  (aif (car (last player-names)) it "0" )  ))

(defun create-player-desc( players )
  (with-output-to-string(s)
    (format s "(")
    (dolist (p players)
      (if p (format s "(~A ~A)" (format nil "~A " (cards p)) (money p))  
	  (format s "()")) )
    (format s ")")))

(defun time->napis(i)
  (multiple-value-bind (a b c d e f) (decode-universal-time i)
    (progn a b c (format nil "~A-~A-~A" f e d)   )))

(defun get-game( id )
  (let ((g (car(prima-query (format nil "SELECT g.id,t.name,g.data,g.czas,t.small_blind,t.big_blind,t.game_kind,g.button,g.history,g.player_nrs,g.player_desc,g.cards,t.table_size FROM games g, table_descr t WHERE g.table_nr=t.nr AND id='~A'" id)))))
    (if g
	 (let* ((plr (mapcar #'(lambda(p)
				 (let ((n (parse-integer p)))
				   (when (plusp n) (get-player-name n))))
			     (poker.logs::tokens (string-trim '(#\{ #\})(nth 9 g)) '(#\,))))
		(plr2 (mapcar #'(lambda(p d) (when p (make-instance 'player :name p :cards (first d) :money (second d))))
			      plr (read-from-string (nth 10 g)))))
	 (make-instance 'game-log
			:id (nth 0 g)
			:name (nth 1 g)
			:date (time->napis (nth 2 g))
			:time (nth 3 g)
			:sblind (nth 4 g)
			:bblind (nth 5 g)
			:game-kind (nth 6 g)
			:button (nth 7 g)
			:history (read-from-string (nth 8 g))
			:players plr2
			:cards (read-from-string (nth 11 g))
			:size (nth 12 g)    )) 
	 "")))


(defun select-games(&optional (war " AND data>=current_date-interval '2 day'"))
  (prima-query (concatenate 'string "SELECT g.id,t.name,g.data,g.czas,t.small_blind,t.big_blind,t.game_kind FROM games g,table_descr t WHERE g.table_nr=t.nr " war)))
