;(load (compile-file "gameanalysis.lisp" :output-file "binaries/gameanalysis.x86f"))

(defvar +player+ "ManneE") ; gracz dla ktorego przeprowadzamy analize!

(defvar +months+ '((|January| . 31) (|February| . 28) ))

(defun dzisiaj()
  (get-universal-time))

(defmethod dzien((czas number))
  (multiple-value-bind (a b c d e f) (decode-universal-time  czas)
    (declare (ignore a b c e f))
     d))


(defmethod dzien((g game))
  (multiple-value-bind (a b c d e f) (decode-universal-time  (game-time g))
    (declare (ignore a b c e f))
     d))

(defmethod miesiac((czas number))
  (multiple-value-bind (a b c d e f) (decode-universal-time  czas)
    (declare (ignore a b c d f))
     e))

(defmethod miesiac((g game))
 (miesiac (game-time g)))


(defmethod rok((g game))
  (multiple-value-bind (a b c d e f) (decode-universal-time  (game-time g))
    (declare (ignore a b c e d))
     f))


(defun get-gamelog-pos(game name)
  (car (find name (game-players game) :key #'cdr :test #'string=)))


(defun player-money(game name)
  (awhen (get-gamelog-pos game name) (aref (game-money game) it)))


;;------------------
(defun find-form(what forms)
  (if (null forms)
    nil
    (if (eq what (car (car forms))) 
	(cdr (car forms))
	(find-form what (cdr forms)))))

(defun insert-inside(forms &optional with-and)
  (if with-and
      (concatenate 'string
		   (format nil "~A" (car forms))
		   (format nil "~{ AND ~A~}" (cdr forms)))
      (concatenate 'string
		   (format nil "~A" (car forms))
		   (format nil "~{,~A~}" (cdr forms)))))

(defun where-form(f)
  (cond
					;(player "ManneE")
    ((eq 'player (car f))  (format nil "player_nr=~A" (get-player-nr (second f)) ) )
					;(from 2007 06 27)
    ((eq 'from (car f))  (format nil "czas > '~A-~A-~A 00:00:00'" (nth 1 f) (nth 2 f) (nth 3 f)  ) )
    ((eq 'to (car f))  (format nil "czas < '~A-~A-~A 23:59:59'" (nth 1 f) (nth 2 f) (nth 3 f)  ) )
    ((eq 'day (car f))  
     (format nil "czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59'" 
	     (nth 1 f) (nth 2 f) (nth 3 f) (nth 1 f) (nth 2 f) (nth 3 f)  ) )
    ((eq 'month (car f))  
     (format nil "czas > '~A-~A-01 00:00:00' AND czas < '~A-~A-30 23:59:59'" 
	     (nth 1 f) (nth 2 f)  (nth 1 f) (nth 2 f)   ) )
    (t (format nil "~A~A~A" (second f) (first f) (third f)  ))))
	  

;; (select (from games) (where (= player_nr 111) (< czas (dzisiaj))))
(defmacro select(&body forms)
  `(let ((what (find-form 'what ',forms))
	 (where (find-form 'where ',forms))
	 (from (or (car (find-form 'from ',forms)) "games")))
    (format nil "SELECT ~A FROM ~A~A" 
     (if what (insert-inside what) "*")
     from
     (if where (format nil " WHERE ~A" (insert-inside (mapcar #'where-form where) t))  ""))))

(defmacro prima-select(&body forms)
  `(prima-query (select ,@forms)))

(defun get-game(id)
  (let ((res (car (prima-query (format nil "SELECT * FROM GAMES WHERE id=~A" id))))
	g)
    (when res
      (setq g      (make-game
			:id (nth 0 res)
			:time (nth 1 res)
			:kind (nth 2 res)
			:table-name (nth 3 res)
			:table-cards (str->list (nth 4 res))
			:history (str->list (nth 7 res))))
      (dolist (e (str->list (nth 6 res))) 
	(push (cons (first e) (get-player-name (second e)) ) (game-players g))
	(setf (aref (game-money g) (first e)) (third e)  )
	(when (fourth e) 
	  	(setf (aref (game-cards g) (first e)) (cadddr e)  ) ) 	) )
    g))

;(get-games (player "ManneE") (month 2007 4))
(defmacro get-games(&rest queries)
  `(mapcar #'get-game
    (mapcar #'car 
     (prima-select (from player_log) (what game_id) (where ,@queries )))))

(defun avg(list-of-nums)
   (/ (reduce #'+ list-of-nums) (length list-of-nums)))

;potrzebujemy strukture opisujaca parametry gry danego gracza
;to bedzie wynik analizy wybranej gry z punktu widzenie tego gracza

(defstruct game-profile
  ;player-name ;wlasciwie niepotrzebne
  cards
  money
  playing-non-blind)

(defun get-game-profile(game-log name)
  (let ((prof (make-game-profile))
	(p-pos (get-gamelog-pos  game-log name)))
    (when p-pos
      (setf (game-profile-money prof)(aref (game-money game-log) p-pos)
	    (game-profile-cards prof)(aref (game-cards game-log) p-pos))
      )
    prof))

(defmacro get-game-profiles(name &rest queries)  
  `(mapcar (lambda(g) (get-game-profile g ,name))
    (get-games (player ,name) ,@queries)))


;(count-if #'game-profile-cards (get-game-profiles "ManneE" (month 2007 4)) )

(defun get-player-games(name year month)
 (mapcar #'get-game
	 (mapcar #'car (prima-query (format nil "select game_id from player_log where player_nr=~A and czas > '~A-~A-01 00:00:00' AND czas < '~A-~A-30 23:59:59'"  (get-player-nr name) year month year month)))))

(def-url-0 "/player"
    (main-page
      (let ((name (get-parameter "name")))
	(dolist (g  (get-player-games name 2007 4))
	  (awhen (game-profile-cards (get-game-profile g name))
	    (html (:princ (karty->html it)) :hr)))

;	  (html (:princ (get-game-profile g name))
	;	:hr))
	)))


(defun insertcomment(name comment)
  (prima-query (format nil "insert into comments values(~A,DEFAULT,'~A')" (get-player-nr name) comment)))


(def-url-0 "/comment"
    (insertcomment (get-parameter "name") (get-parameter "comment")))

