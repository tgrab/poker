(def-url-0 "/game"
    (main-page

      "<SCRIPT TYPE='text/javascript'>
        <!--
          function popupform(myform, windowname)
          {
           if (! window.focus)return true;
           window.open('', windowname, 'height=200,width=400,scrollbars=yes');
           myform.target=windowname;
           return true;
          }
         function popup(mylink, windowname)
         {
          if (! window.focus)return true;
          var href;
          if (typeof(mylink) == 'string')
            href=mylink;
           else
             href=mylink.href;
          window.open(href, windowname, 'width=400,height=200,scrollbars=yes');
         return false;
        }
       //-->
      </SCRIPT>"

      (let ((id (get-number-parameter "id")))
	(when id
	  (when (/= id (game-id +game-log+))
	    (read-game id)

	    ;testowanie akcji systemu:
	    ;szukamy gracza ktorego karty sa widoczne
	    ;jesli nie ma graboli:

		 (unless (find "grabola" (game-players +game-log+) :test #'string= :key #'cdr)
		   (dotimes (i 10)
		     (when (aref (game-cards +game-log+) i) ;mamy go
					;ustawiamy pod niego gre:
		       (setq +agent-name+  (get-log-p-name i))))) 

	    (setq +agent-action+ nil)
	    (set-game-kind (game-kind +game-log+))))


	(get-percept)

	(let* ((idx (position (game-id +game-log+) +games-list+ :key #'car))
	       (prev (1- idx))
	       (nxt (1+ idx)))
	  (when (plusp (1+ prev)) 
	    (html ((:a href (format nil "/game?id=~A" (car (nth prev +games-list+)))) "[<<< Prev Game <<<]" )))
	  (html ((:a href "/reloadgame") "[  Reload game  ]"))
	  (when (< nxt (length +games-list+))
	    (html ((:a href (format nil "/game?id=~A" (car (nth nxt +games-list+)))) "[>>> Next Game >>>]" )))
	  (html :hr))


	(case (game-kind +game-log+)
	    (3 (html "<b>Omaha Pot Limit</b><br>"))
	    (1 (html "<b>Holdem No Limit</b><br>"))
	    (0 (html "<b>Holdem Fixed Limit</b><br>")  ))
	(html
	   "Game " ((:a href "/game") (:princ (game-id +game-log+))) " at "  (:princ (time->napis (game-time +game-log+))) " " (:princ (length (game-players +game-log+))) " players table"  :hr)

	(html ((:form action "/learning" onSubmit "popupform(this,'join')")
	       "<select name=player>"
	       (dolist (p (game-players +game-log+))
		 (html (:princ (format nil "<option value='~A'>~A</option>" (cdr p) (cdr p)))))
	       "</select>"	     
	       "<select name=action>"  
	        "<option value=fold>fold</option>"
	        "<option value=call>call</option>"
	        "<option value=bet>bet</option>"
	        "<option value=potbet2>potbet2</option>"
	        "<option value=potbet1>potbet1</option>"
	        "<option value=allin>allin</option>"
	       "</select>"
	       ((:input type "hidden" value (game-id +game-log+) name "id"))
	       ((:input type "hidden" value +percept-nr+ name "nr"))
	       ;((:input type "hidden" value (game-kind +game-log+) name "kind"))
	       ; Omaha Pre-Flop
	       ((:input type "hidden" value "30" name "kind"))
	       ((:input type "text"  name "comment"))
	       ((:input type "submit" value "send!")) )
	      (:princ (format nil "<a href='/learning?id=~A' onClick=\"return popup(this,'unchecked')\">uncheck</a>" (game-id +game-log+)))
	      :hr)

	(html ((:form action "/insertcomment" onSubmit "popupform(this,'join')")
	       "<select name=player>"
	       (dolist (p (game-players +game-log+))
		 (html (:princ (format nil "<option value='~A'>~A</option>" (cdr p) (cdr p)))))
	       "</select>"	     
	       "<textarea name=comment cols=30 rows=3>"
	       "</textarea>
"	       ((:input type "hidden" value (game-id +game-log+) name "id"))
	       ((:input type "submit" value "send!")) ))

	(dotimes (i 10)
	    (awhen (aref  (game-cards +game-log+) i)
	      (html (:b ((:a href (format nil "/reloadgame?agent=~A" (get-log-p-name i))) (:princ (get-log-p-name i))))
		    (:princ (karty->html it)) 
		    (when +table-cards+
		      (if (= 4 (length it))
			  (html (:princ (render-rank (omaha-ocena it +table-cards+))) " , ")
			  (html (:princ (render-rank (ocena (append it +table-cards+)))) " , ")) )
		    :hr )))

	(when +table-cards+ (html (:princ (karty->html +table-cards+)) :hr))
	(when +agent-action+ (html (:b "Action for " (:princ +agent-name+) ": " ) (:princ +agent-action+) :br :br))
	(html (:b "+history+ ") (:princ (reverse +history+)) :br (:b "+pot+ ") (:princ +pot+) :hr)

	(html "<table border='1'>")
	(dotimes (i (length +name->seat+))
	  (html "<tr><td>"(:b (:princ (first (nth i (reverse +name->seat+))))) 
		"<td>" (:princ (aref +active-players+ i))
		"<td>" (:princ (aref +balance+ i)) 
		"<td>" (:princ (aref +total-balance+ i))
		"<td>" (:princ (reverse (aref +player-history+ i))) ))
	(html "</table>")
	(when (null (game-history +game-log+)) (html (:h2 "Finished!")))
	;(html :hr)
	;(let ((*standard-output* *html-stream*))
	 ;   (show-game))
	)))
