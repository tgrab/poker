(defun date-chooser(name)
  (html
    ((:select name (format nil "~A_day" name)) 
     (dotimes (d 31) (html ((:option value (1+ d)) (:princ (1+ d))))))

    ((:select name (format nil "~A_month" name))  
     ((:option value "1")"January")
     ((:option value "2")"February")
     ((:option value "3")"March")
     ((:option value "4")"April")
     ((:option value "5")"May")
     ((:option value "6")"June")
     ((:option value "7")"July")
     ((:option value "8")"August")
     ((:option value "9")"September")
     ((:option value "10")"October")
     ((:option value "11")"November")
     ((:option value "12")"December"))

    ((:select name (format nil "~A_year" name))  
     ((:option value "2007")"2007")
     ((:option value "2006")"2006"))   ))


(defun searchbyday()
  (html
       ((:form action "/search")
	(:table
	 (:tr (:td "   Day:")     (:td (date-chooser "from") ))
	 (:tr ((:td colspan "2") ((:input type "submit" value "Select games")))   )))
   ))
     

(def-url-0 "/search"
   (main-page
    (let ((player (get-parameter "player"))
	  (fromy   (get-parameter "from_year"))
	  (fromm   (get-parameter "from_month"))
	  (fromd   (get-parameter "from_day"))
	  (toy   (get-parameter "to_year"))
	  (tom   (get-parameter "to_month"))
	  (tod   (get-parameter "to_day"))
	  (kind (or (get-parameter "kind") "3"))
	  (sql ""))

  
      (html
       ((:form action "/executesql")
	"<textarea rows=5 cols=50 name=sql>"
	"select count(*) from games where date_part('year',czas)=2007"
	"</textarea>"
	((:input type "submit" value "Execute Query")))

       :hr
       ((:form action "/search" method "post" )
	(:table
	 (:tr (:td "Player:")  (:td ((:input name "player" value (if player player "grabola")))((:a href "/players")" [choose players]")))
	 (:tr (:td "   From:")     (:td (date-chooser "from") ))
	 (:tr (:td "   To:")     (:td (date-chooser "to") ))
	 (:tr (:td " Game:")     (:td ((:select name "kind")  
				       ((:option value "1")"Holdem No Limit")
				       ((:option value "2")"Holdem Fixed Limit")
				       ((:option value "3")"Omaha Pot Limit")     )))
	 (:tr ((:td colspan "2") ((:input type "submit" value "Select games")))   )))
      :hr
      "Search unchecked games by day:"
      (searchbyday)
      :hr  )

      (when (and player fromy toy kind)
	    (setq sql (format nil "SELECT game_id,game_kind,blind,czas FROM player_log WHERE player_nr=~A AND  czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59' AND game_kind=~A  ORDER BY czas ASC" (get-player-nr player t) fromy fromm fromd toy tom tod kind))
	    (setq +games-list+ (prima-query sql))
	    (html "Found " (:b (:princ (length +games-list+))) " games from " (:princ (format nil "~A/~A/~A" fromd fromm fromy )) 
		  " to " (:princ (format nil "~A/~A/~A" tod tom toy )) 
		  :br ((:a href "/gameslist") "See them!"))  )

      (when fromy
	(if (get-parameter "checked")
	    (setq sql (format nil "SELECT id,game_kind,blind,czas FROM games WHERE game_kind=~A AND czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59' AND checked IS NULL  ORDER BY czas ASC" kind fromy fromm fromd fromy fromm fromd))

(setq sql (format nil "SELECT id,game_kind,blind,czas FROM games WHERE  czas > '~A-~A-~A 00:00:00' AND czas < '~A-~A-~A 23:59:59' ORDER BY czas ASC" fromy fromm fromd fromy fromm fromd)) )
	    (setq +games-list+ (prima-query sql))
	    (html "Found " (:b (:princ (length +games-list+))) "games." 
		  :br ((:a href "/gameslist") "See them!"))  )	  

	    )))
