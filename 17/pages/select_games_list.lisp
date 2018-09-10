(defun select-games-list(&optional (k 3) (m 10) (d 13) (y 2007))
    (clsql:query (format nil "select id from games where game_kind=~A and czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59'"  k y d m  y d m)  ))

(defun view-select-games-list()
  (let ((k (or (get-parameter "kind") "3"))
	(d (or (get-parameter "day") "01"))
	(m (or (get-parameter "month") "01"))
	(y (or (get-parameter "year") "2007")) )
    (setq +games-list+ (select-games-list k d m y))
    (main-page (:with-menu t)
      (fmt "selected ~A games..." (length +games-list+))  )))

(def-url "/select_games_list" view-select-games-list)