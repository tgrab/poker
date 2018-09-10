(defun view-game()
    (main-page (:title "Game" :with-menu t :with-ajax t :with-style t)

      (awhen (get-parameter "id")
	(read-game (parse-integer it))
	(set-game-kind (kind +game+))
	(rules-new-game)
	; jesli nie ma graboli wsrod graczy
	; to ustawic agent-name
	(if (find  "grabola" (players-log +game+) :key #'cdr :test #'string= )
	     (setf (agent-name +game+) "grabola")
	     ;wybieramy gracza z kartami:
	     (let ((p (position-if #'identity (coerce (cards-log +game+) 'list))))
	       (when p (setf (agent-name +game+) (get-log-player-name p)))    ))
	;lub agent zostal ustalony z zewnatrz:
	(when (get-parameter "agent") (setf (agent-name +game+) (get-parameter "agent")))
	;przydzielamy mu karty:
	(let ((p (position (agent-name +game+) (players-log +game+) :key #'cdr :test #'string=)))
	  (when (numberp p) 
	    (let ((cards (aref (cards-log +game+)  (car (nth p (players-log +game+))) )))
	      (if (= 4 (length cards))  
		  (holecards (nth 0 cards)  (nth 1 cards)  (nth 2 cards)  (nth 3 cards))
		  (holecards  (nth 0 cards)  (nth 1 cards))    ))  ))
	)

      (let ((el (position (id +game+) +games-list+ :key #'car) ))
	(when (numberp el)
	  (when (> el 0)
	    (fmt "<a href='/game?id=~A'>[<<<]  </a>" (car (nth (1- el) +games-list+))))
	  (when (< el (1- (length +games-list+)))
	    (fmt "<a href='/game?id=~A'>  [>>>]</a>" (car (nth (1+ el) +games-list+)))) ))

      "<br><br>"
      (case (kind +game+)
	(3 (princ "<b>Omaha Pot Limit</b><br>"))
	(1 (princ "<b>Holdem No Limit</b><br>"))
	(t (princ "<b>Holdem Fixed Limit</b><br>")  )) 
      (fmt "Game <a href='/game?id=~A' style='font-weight: bold'>~A</a> at ~A on ~A<br><br>" 
	   (id +game+)  (id +game+) (czas +game+) (table-name +game+))
      
      (:button :onClick "GetIt(\"/render_game\")" "Next Percept")
      "<hr>"
      (:span :id "result"
	     (princ (view-render-game nil))
	     )))
    
(def-url "/game" view-game)