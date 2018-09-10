;(declaim (optimize (speed 3) (safety 0)))
;; This is web interface to poker engine

(in-package :poker.web)

;; ----- Web Engine ---------------------------------
;(setq net.aserve:*default-aserve-external-format* :latin2-base)

(defvar *clients* nil)
(defvar *session* nil)
(defvar *authorized-only* nil)

(defmacro get-attribute(name)
  `(awhen (pobierz-cookie)
    (gethash ,name (cdr (assoc it *clients* :test #'equal)))))

(defmacro set-attribute(name val)
  `(awhen (pobierz-cookie)
    (setf (gethash ,name (cdr (assoc it *clients* :test #'equal))) ,val)))

(defmacro get-parameter(param) ;param musi byc stringiem!
  `(net.aserve:request-query-value ,param req))

(defmacro get-number-parameter(param)
  `(read-from-string (get-parameter ,param)))


(defmacro pobierz-cookie()
  '(cdr (assoc "moje" (net.aserve:get-cookie-values req) :test #'equal)))


(defmacro define-url(path body &key (content "text/html; charset=ISO-8859-2" ))
 `(net.aserve:publish :path ,path :content-type ,content :function
    (lambda (req ent)
      (net.aserve:with-http-response(req ent)
	(when *session*
	  (if (null (pobierz-cookie))
	      (net.aserve:set-cookie-header req :name "moje" :value (format nil "~A" (random 1000)))
	      (when (null (assoc  (pobierz-cookie) *clients* :test #'equal))
		(let ((tab (make-hash-table))
		      (czas (multiple-value-list (get-decoded-time))))
		  (setf (gethash 'start-time tab) (format nil "~A:~A" (third czas)(second czas)) )
		  (push (cons (pobierz-cookie)  tab) *clients*)) )))
	(net.aserve:with-http-body(req ent)
	 (if (and *authorized-only* (null (get-attribute 'user)))
	     (login-page)
	     ,body )
	  )))))

(defmacro def-url-0(path &rest body)
 `(net.aserve:publish :path ,path :content-type "text/html" :function
    (lambda (req ent)
      (net.aserve:with-http-response(req ent)
	(net.aserve:with-http-body(req ent)
	   (main-page  
	     ,@body ))))))

(defun pokaz-sesje()
  (dolist (c *clients*)
    (format t "~%Klient: ~A~&" (car c))
    (maphash #'(lambda (k v)
		 (format t "~A=>~A~&" k v))
	     (cdr c))))

(defmacro pokaz-html(forma)
  `(let ((net.aserve::*html-stream* *standard-output*))
    ,forma))


(defmacro login-page()
 '(wypisz "Login page not defined"))


(defmacro wypisz(ctrl &rest args)
  `(format net.aserve::*html-stream* ,ctrl ,@args))



(defun start-aserve(&optional (port 9000))
  (print (net.aserve:start :port port))
  #+cmu(mp::startup-idle-and-top-level-loops))

;;--------------------------------------------------------------------------
;;------------------------------- Poker View -------------------------------
;;--------------------------------------------------------------------------

(defvar +games-list+ nil)
(defvar +player+ nil)

(defun main-menu()
  (html 
   ((:a href "/") "[ Main ]")
   ((:a href "/logger") "[ Logger ]")
   ((:a href "/test") "[Tescik]")
   :hr) )

(defmacro main-page(&body body)
  `(html
    (:html
     (:head "<link rel='stylesheet' href='styl.css' type='text/css'>" )
     (:body
      (main-menu)
      ,@body        ))))

(defun generate-query(&optional (user "grabola") (data "2006-11-30"))
  (let ((player_nr (poker.db::get-player-nr user t)))
  (concatenate 'string
	"SELECT g.id FROM games g,table_descr t WHERE g.table_nr=t.nr "
	(if (plusp player_nr) (format nil "AND ~A=ANY(g.player_nrs) " player_nr) "")
	(if  data (format nil "AND g.data='~A' " data) "")         
	"ORDER BY g.czas DESC")))

(defun render-action(agent nr)
  (let ((a (nth nr (last-actions agent))))
    (if (eq a t)
	""
	(if (consp a)
	    (format nil "~A ~A" (car a) (cdr a))
	    a))))


(defun render-player(agent nr)
  (let ((p (nth nr (players (game agent)))))
    (if p
	(html
	 (if (null (nth nr (actions-list agent)))
	     (html "<td style='background-color: grey'><ul class='gracz'>")
	     (html "<td><ul class='gracz'>"))
	 (if (= nr (last-active-player agent))
	      (html "<li style='background-color: red; font-weight: bold;' >" (:princ (render-action agent nr)))
	      (html "<li style='color: red;' >" (:princ (render-action agent nr))  ))
	 ((:li style "font-weight: bold" ) (:princ (name p)))
	 (when (= nr (button (game agent))) (html "<li style='color: orange; font-weight: bold'>Dealer"))
	 (awhen (cards p)  
	    (html :li (:princ (karty->html it ))))

	 (wypisz "<li>~6,2F" (money p))
	 "</ul>"  )

	(html
	 ((:td style "background-color: green")))       )))

(defun render-status(agent)
  (html
   (:ul 
    (awhen (table-cards (game agent) ) (wypisz "<li>Table ~A</li>" (karty->html it)) ) 
    (:li "Pot " (:princ (format nil "~6,2F"(pot agent))))     )))

(defun render-table(agent)
  (if (= 6 (table-size  agent))
      (html
       ((:table class "stol")
	:tr (render-player agent 5) (render-player agent 4) (render-player agent 3)
	:tr (render-player agent 0) (render-player agent 1) (render-player agent 2))
	((:table style "background-color: cyan;border: 4px solid green") :tr :td (render-status agent))
       :p)
      (html
       ((:table class "stol")
	:tr (render-player agent 9) (render-player agent 8) (render-player agent 7)
	    (render-player agent 6) (render-player agent 5)
	:tr (render-player agent 0) (render-player agent 1)(render-player agent 2)
            (render-player agent 3) (render-player agent 4))
	((:table style "background-color: cyan;border: 4px solid green") :tr :td (render-status agent))
       :p)))


(defun render-game(game-log)
  (let ((c (reverse (table-cards game-log)))
	(hist (reverse (history game-log)))
	(a (make-instance 'agent)))

    (percept-new-game-log a game-log)
    (html 
     ((:ul style "list-style: none")
      (:li  (:princ (format nil " ~A" (game-time game-log))  ))
      (:li  (:princ (table-name game-log)  ))
      (:li  (:princ (format nil "[~A ~A/~A]" 
			    (if (plusp (game-kind game-log)) "Hold'em" "Omaha") 
			    (small-blind game-log) 
			    (big-blind game-log))))   )
     ;(format t "~A ~A" a (table-size a))
     (dolist (h hist)
       (cond
	 ((eq h 'F) (percept-fold a) (render-table a) )
	 ((eq h 'C) (percept-check a) (render-table a) )
	 ((consp h) (percept-bet a (cdr h)) (render-table a) )
	 ((eq h 'FLOP)
	  (html (:center
		 ((:table style "background-color: white") 
		  :tr ((:td style "font-size: 25px") "Dealing flop:") 
		  (:td (:princ (karty->html (subseq c 0 3))) )))
		:p)
	  (percept-flop a (nth 0 c) (nth 1 c) (nth 2 c) )  )
	 ((eq h 'TURN)
	  (html (:center
		 ((:table style "background-color: white") 
		  :tr ((:td style "font-size: 25px") "Dealing turn:") 
		  (:td (:princ (karty->html (list (nth 3 c))) ))))
		:p)
	  (percept-turn a (nth 3 c) )  )
	 ((eq h 'RIVER)
	  (html (:center
		 ((:table style "background-color: white") 
		  :tr ((:td style "font-size: 25px") "Dealing river:") 
		  (:td (:princ (karty->html (list (nth 4 c))) ))))
		:p)
	  (percept-turn a (nth 4 c)) ))))))

(defun get-page(nr)
  (subseq +games-list+ (* (1- nr) 40) (* nr 40))) 

(defun game-row(id)
  (let* ((g (poker.db::get-game id))
	 (nr (get-player-pos +player+ g))
	 (result (poker.engine::simulate-game g t)))
    
    (html
     "<tr><td>" (:princ (format nil "<a href='/game?id=~A'>~A</a>" (game-id g) (game-id g)))  
     "<td>" (:princ (game-time g)) 
     (:princ (format nil "<td> [~A (~A/~A)]" (table-name g) (small-blind g) (big-blind g) )) 
     "<td>" (:princ (karty->html (reverse (table-cards g)))) 
     "<td>" (:princ (money (nth nr (players g))))
     "<td>"  (:princ (nth nr (balance (game result))))
     "<td>" (:princ (karty->html (cards (nth nr (players g)))))
     "<td>" (:princ (reverse (nth nr (all-actions result))))
     )    ))

(defun game-table(id)
  (let* ((g (poker.db::get-game id))
	 (result (poker.engine::simulate-game g t)))
    
    (html
     "<tr><td>" (:princ (format nil "<a href='/game?id=~A'>~A</a>" (game-id g) (game-id g)))  
     "<td>" (:princ (game-time g)) 
     (:princ (format nil "<td> [~A (~A/~A)]" (table-name g) (small-blind g) (big-blind g) )) 
     "<td>" (:princ (karty->html (reverse (table-cards g))))
     (dotimes (nr (length (players g)))
       (aif  (nth nr (players g))
	(html
	 "<td>"  
	 (:princ (name it))  
	 (:princ (money it))
	 (:princ (karty->html (cards it))))
	(html "<td>"))

       ))     )    )

;;------------- DEF URLsssssss -----------------------------------------------------------------------------
(net.aserve:publish-directory :prefix "/" :destination "static/")

(def-url-0 "/"
    "Main page")


(def-url-0 "/logger"
    (let ((player (get-parameter "player"))
	  (from   (get-parameter "from"))
	  (to   (get-parameter "to"))
	  (bal 0))
      (html
       ((:form action "/logger" method "post" )
	(:table
	 (:tr (:td "Player:")  (:td ((:input name "player" value (if player player "grabola"))) ))
	 (:tr (:td "   From:")     (:td ((:input name "from" size "26" value (if from from "2006-11-01 00:00:00"))) ))
	 (:tr (:td "   To:")     (:td ((:input name "to" size "26" value (if to to "2006-11-30 23:59:59"))) ))
	 (:tr ((:td colspan "2") ((:input type "submit" value "Show games")))   )))
	
      (when (and player from to)
	(wypisz "<hr><table border=1>")
	(dolist (g (poker.db::prima-query (format nil "SELECT g.id,g.czas,p.balance,p.money,t.name,t.table_size,t.small_blind,t.big_blind,p.cards FROM games g, player_log p, table_descr t WHERE g.nr=p.game_nr AND p.player_nr=~A AND g.table_nr=t.nr  AND czas > '~A' AND czas < '~A' ORDER BY g.table_nr,g.czas ASC" (poker.db::get-player-nr player) from to)))
	  (wypisz "<tr><td>~A<td><a href='/game?id=~A'>~A</a><td>~A<td>~A<td>~A(~A):~A/~A<td>~A" (poker.db::time->napis (second g)) (first g) (first g) (third g) (nth 3 g)  (nth 4 g)(nth 5 g) (nth 6 g) (nth 7 g) (karty->html (read-from-string (nth 8 g))))
	  (incf bal (third g)))
	(wypisz "</table><h2>Total balance: ~6,2F</h2>" bal))) ))


(def-url-0 "/logger2"
    (let ((player (get-parameter "player"))
	  (from   (get-parameter "from"))
	  (to   (get-parameter "to")))
	  
      (html
       ((:form action "/logger2" method "post" )
	(:table
	 (:tr (:td "Player:")  (:td ((:input name "player" value (if player player "grabola"))) ))
	 (:tr (:td "   From:")     (:td ((:input name "from" size "26" value (if from from "2006-11-01"))) ))
	 (:tr (:td "   To:")     (:td ((:input name "to" size "26" value (if to to "2006-11-30"))) ))
	 (:tr ((:td colspan "2") ((:input type "submit" value "Show games")))   )))
	
      (when (and player from to)
	(setq +games-list+ (mapcar #'car (poker.db::prima-query (format nil "SELECT g.id FROM games g, player_log p, table_descr t WHERE g.nr=p.game_nr AND p.player_nr=~A AND g.table_nr=t.nr AND  g.czas > '~A 00:00:00' AND g.czas < '~A 23:59:59' ORDER BY g.czas ASC" (poker.db::get-player-nr player) from to)))
	      +player+ player)
	(html "Wczytano:" (:princ (length +games-list+))) 	)
	(let ((page-nr (aif (get-parameter "page") (parse-integer it) 1)))
	  (html
	   "Total: " (:princ (length +games-list+)) " games.<br>"
	   ((:a href (format nil "/logger2?page=~A" (1- page-nr))) "<<<")
	   "Page " (:princ page-nr) " of " (:princ (1+ (round (/ (length +games-list+) 40.0)))) 
	   ((:a href (format nil "/logger2?page=~A" (1+ page-nr))) ">>>")
	   :hr
	   "<table>"
 	   (mapcar #'game-row (get-page page-nr))
	   "</table>"  ))

)))


(def-url-0 "/logger3"
	(let ((page-nr (aif (get-parameter "page") (parse-integer it) 1)))
	  (html
	   "Total: " (:princ (length +games-list+)) " games.<br>"
	   ((:a href (format nil "/logger3?page=~A" (1- page-nr))) "<<<")
	   "Page " (:princ page-nr) " of " (:princ (1+ (round (/ (length +games-list+) 40.0)))) 
	   ((:a href (format nil "/logger3?page=~A" (1+ page-nr))) ">>>")
	   :hr
	   "<table>"
 	   (mapcar #'game-table (get-page page-nr))
	   "</table>"  )))

)))

(def-url-0 "/game" 
   (awhen (get-parameter "id")
     (let* ((idx (position it +games-list+ :test #'string=))
	    (prev (1- idx))
	    (nxt (1+ idx)))

       (when (plusp prev) 
	 (html ((:a href (format nil "/game?id=~A" (nth prev +games-list+))) "[<<< Prev <<<]" )))

       (when (< nxt (length +games-list+))
	     (html ((:a href (format nil "/game?id=~A" (nth nxt +games-list+))) "[>>> Next >>>]" )))
     (render-game (poker.db::get-game it)))  )  )

(def-url-0 "/test" 
  ((:form action "/test" method "post" )
   (:table
     (:tr (:td "Player: ")  (:td ((:input name "player" value "grabola")) ))
     (:tr (:td "day: ")     (:td ((:input name "data" value "2006-11-30")) ))
     (:tr ((:td colaspan "2") ((:input type "submit" value "Send QUERY")))   )))
	
  (awhen (get-parameter "player")
    (html (:prin1 (generate-query it (get-parameter "data"))))
       ))

(def-url-0 "/score"
	(let ((l   (poker.db::prima-query "SELECt players.name,sum(balance) from player_log,players where player_log.player_nr=players.nr group by players.name order by sum(balance) desc")))
	  (dolist (u l)
	    (html (:princ u) :br))))
