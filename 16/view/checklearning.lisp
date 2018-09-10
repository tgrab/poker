(defun move-to-percept(nr)
  (dotimes (i nr)
    (get-percept)))


(defun check-game(id nr player)
  (read-game id)
  (setq +agent-name+ (get-player-name player))
  (move-to-percept nr)
  (action-prove))

(defun check-learning(nr)
  (let* ((res (car (prima-query (format nil "select * from learning where nr=~A" nr))))
	 (cg 	    (check-game (nth 1 res) (nth 3 res) (nth 4 res) )))
    (if (not (string= (car (tokens cg)) (nth 5 res)))
    (format nil "<div style='color: red'>GAME ~A:<a href='/showgame?id=~A&percept=~A&agent=~A&start=true' target='_blank'>~A</a>, PERC.NR:~A, PLAYER:~A, DB:~A (~A), PROVE-ACTION: ~A  </div><br>~&" 
	    nr
	    (nth 1 res)
	    (nth 3 res)
	    (nth 4 res)
	    (nth 1 res)
	    (nth 3 res)
	    (get-player-name (nth 4 res))
	    (nth 5 res)
	    (nth 6 res)
	    cg)
    (format nil "GAME ~A:<a href='/showgame?id=~A&percept=~A&agent=~A&start=true' target='_blank'>~A</a>, PERC.NR:~A, PLAYER:~A, DB:~A (~A), PROVE-ACTION: ~A<br>~&" 
	    nr
	    (nth 1 res)
	    (nth 3 res)
	    (nth 4 res)
	    (nth 1 res)
	    (nth 3 res)
	    (get-player-name (nth 4 res))
	    (nth 5 res)
	    (nth 6 res)
	    cg)  )))


(defun check-all-learning(&optional (stream *standard-output*) (kind 2))
  (mapcar
   #'(lambda(n) (princ (check-learning n) stream ))
   (prima-query (kwery "select nr from learning where game_kind=~A " kind)))
  'ok)

(def-url-0 "/checklearning"
    (let ((kind (or (get-number-parameter "kind") 2)))
    (main-page
      (check-all-learning *html-stream* kind))))
    
;wstawia do bazy
(def-url-0 "/learning"
  ;(set-game-is-checked  (get-number-parameter "id"))
  (if (get-parameter "nr")
    (html 
     (:princ 
      (prima-query 
       (format nil 
	     "insert into learning values(DEFAULT,~A,~A,~A,~A,'~A','~A')"
	     (get-parameter "id")
	     (get-parameter "kind")
	     (get-parameter "nr")
	     (get-player-nr (get-parameter "player"))
	     (get-parameter "action")
	     (get-parameter "comment")))) )
  (html "checked")))
    