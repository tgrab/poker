(defun view-choose-selected-games()
    (main-page (:with-menu t)
      (dolist (e +games-list+)
	(fmt "<a href='/game?id=~A'>~A</a><br>" (car e) (car e)) )))


(def-url "/choose_selected_games" view-choose-selected-games)