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



(defun insert-game( game )
  ;dodajemy tylko gry na stolach 6-cio osobowych i nie turniejowe
  (when (and (> (table-size game) 2)
	     (or (= 0 (game-kind game)) (= 1 (game-kind game))))
  (with-prima-connection
    (unless (pg-result (pg-exec con (format nil "SELECT id FROM games WHERE id='~A'" (game-id game))) :tuples)
      (let ((simul (poker.engine::simulate-game game)))
	(pg-exec con "BEGIN WORK")
	(pg-exec con   (format nil "INSERT INTO games VALUES(DEFAULT,'~A','~A',~A,'~A','~A',~A,~A)"
			     (game-id game)
			     (game-time game)
			     (get-table-nr game)
			     (table-cards game)
			     (history game)
			     (button game)
			     (format nil "~6,2F" (pot simul))))
      (let ((nr (caar (pg-result (pg-exec con (format nil "SELECT nr FROM games WHERE id='~A' " (game-id game))) :tuples))))
   
	(dotimes (i (length (players game)))
	  (let ((p (nth i (players game))))
	    (when p
	      (pg-exec con (format nil "INSERT INTO player_log VALUES(~A,~A,~A,'~A',~A,~A)"
				   (get-player-nr (name p))
				   nr
				   i
				   (cards p)
				   (money p)
				   (format nil "~6,2F" (nth i (balance (game simul)))))))))
	(pg-exec con "COMMIT WORK")))))))



(defun insert-games( game-list )
  (with-prima-connection
    (dolist (game game-list)
      (when (and (> (table-size game) 2)
		 (or (= 0 (game-kind game)) (= 1 (game-kind game)) (= 2 (game-kind game))))
	(unless (pg-result (pg-exec con (format nil "SELECT id FROM games WHERE id='~A'" (game-id game))) :tuples)
	  (let ((simul (poker.engine::simulate-game game)))
	    (pg-exec con "BEGIN WORK")
	    (pg-exec con   (format nil "INSERT INTO games VALUES(DEFAULT,'~A','~A',~A,'~A','~A',~A,~A)"
				   (game-id game)
				   (game-time game)
				   (get-table-nr game)
				   (table-cards game)
				   (history game)
				   (button game)
				   (format nil "~6,2F" (pot simul))))
	    (let ((nr (caar (pg-result (pg-exec con (format nil "SELECT nr FROM games WHERE id='~A' " (game-id game))) :tuples))))
	      
	      (dotimes (i (length (players game)))
		(let ((p (nth i (players game))))
		  (when p
		    (pg-exec con (format nil "INSERT INTO player_log VALUES(~A,~A,~A,'~A',~A,~A)"
					 (get-player-nr (name p))
					 nr
					 i
					 (cards p)
					 (money p)
					 (format nil "~6,2F" (nth i (balance (game simul)))))))))
	      (pg-exec con "COMMIT WORK"))))))))


      


(defun time->napis(i)
  (multiple-value-bind (a b c d e f) (decode-universal-time i)
    (progn (format nil "~A-~A-~A ~A:~A:~A" f e d c b a)   )))

(defun get-game( id )
  (with-prima-connection
    (let ((g (car 
	      (pg-result 
	       (pg-exec con 
			(format nil "SELECT g.nr,g.id,g.czas,t.name,t.small_blind,t.big_blind,t.game_kind,g.button,g.history,g.cards,t.table_size FROM games g, table_descr t WHERE g.table_nr=t.nr AND id='~A'" id))
	       :tuples)))
	  (nr nil)
	  (game nil))
      (if g
	  (progn
	    (setq nr (nth 0 g))
	    (setq game
		  (make-instance 'game
				 :id (nth 1 g)
				 :time (time->napis (nth 2 g))
				 :name (nth 3 g)
				 :sblind (nth 4 g)
				 :bblind (nth 5 g)
				 :game-kind (nth 6 g)
				 :button (nth 7 g)
				 :history (read-from-string (nth 8 g))
				 :cards (read-from-string (nth 9 g))
				 :size (nth 10 g)))
	    (setf (players game) (make-list (nth 10 g) :initial-element nil))
	    (dolist (p (pg-result 
			(pg-exec con 
				 (format nil "SELECT pos,player_nr,cards,money from player_log WHERE game_nr=~A" nr)) 
			:tuples))
	      (let ((ps (nth 0 p)))
		(setf  (nth ps (players game))
		       (make-instance 'player 
				      :name (get-player-name (nth 1 p))
				      :cards (read-from-string (nth 2 p))
				      :money (nth 3 p))    )))
	    game)
	  ""))))

