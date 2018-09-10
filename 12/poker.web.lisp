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

(defun render-action(a &optional (show-fold t))
  (when (null a) (if show-fold (return-from render-action "Folded")
		     (return-from render-action "")))
  (if (consp a)
      (cond
	((eq (car a) 'c) (if (zerop (cdr a)) "Checked" (format nil "Called ~A" (cdr a))  ))
	((eq (car a) 'b)  (format nil "Bet ~A" (cdr a))  ))
      (cond
	((eq a 'c) "checked")
	((eq a 'sb) "small blind")
	(t "big blind"))))

(defun render-player(agent nr)
  (let ((p (nth nr (players (game agent)))))
    (if p
	(html
	 (if (null (nth nr (actions-list agent)))
	     (html "<td style='background-color: grey'><ul class='gracz'>")
	     (html "<td><ul class='gracz'>"))
	  (let ((a (nth nr (actions-list agent))))
	    (when  (not (eq a t))
	      (if (= nr (last-active-player agent))
	      (html "<li style='background-color: red; font-weight: bold;' >" (:princ (render-action a)))
	      (html "<li style='color: red;' >" (:princ (render-action a nil))  ))))
	  (awhen (cards p)  
	    (html :li (:princ (karty->html it ))))
	 ((:li style "font-weight: bold" ) (:princ (name p)))
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
      (:li  (:princ (format nil "~A ~A" (game-time game-log)(game-date game-log))  ))
      (:li  (:princ (table-name game-log)  ))
      (:li  (:princ (format nil "[~A ~A/~A]" 
			    (if (plusp (game-kind game-log)) "Hold'em" "Omaha") 
			    (small-blind game-log) 
			    (big-blind game-log))))   )
     (format t "~A ~A" a (table-size a))
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

;;------------- DEF URLsssssss -----------------------------------------------------------------------------
(net.aserve:publish-directory :prefix "/" :destination "static/")

(def-url-0 "/"
    "Main page")

(defun describe-game(game player)
  (let ((agt (poker.engine::simulate-game game))
	(nr (get-player-pos player game))
	(pocz (mapcar #'(lambda(p) (if p (money p) 0)) (players game)))
	(wnrs nil)
	(how-much 0)
	(konc nil))

    (setq wnrs (game-winner agt))
    (setq how-much (/ (pot agt) (length wnrs)))
    (dolist (w wnrs)
      (incf (money (nth w (players (game agt)))) how-much))
    (setq konc (mapcar #'(lambda(p) (if p (money p) 0)) (players (game agt))))

  (wypisz "<tr><td>~A <b>~A</b><td><a href='game?id=~A'>~A</a>" (game-date game) (game-time game) (game-id game) (game-id game))
  (wypisz "<td>~A" (aif (cards (get-player player game)) (karty->html it) "" ))
  (wypisz "<td>~6,2F<td>~6,2F" (nth nr (mapcar #'- konc pocz)) (pot agt))
  ))

(def-url-0 "/logger"
    (let ((player (get-parameter "player"))
	  (data   (get-parameter "data")))
      (html
       ((:form action "/logger" method "post" )
	(:table
	 (:tr (:td "Player:")  (:td ((:input name "player" value (if player player "grabola"))) ))
	 (:tr (:td "   Day:")     (:td ((:input name "data" value (if data data "2006-11-30"))) ))
	 (:tr ((:td colspan "2") ((:input type "submit" value "Get games")))   )))
	
      (when (and player data)
	(wypisz "<hr><table border=1>")
	(dolist (g (poker.db::prima-query (generate-query player data)))
	  (describe-game (poker.db::get-game (first g)) player))
	(wypisz "</table>"))) ))



(def-url-0 "/game" 
   (awhen (get-parameter "id") 
     (render-game (poker.db::get-game it)))    )

(def-url-0 "/test" 
  ((:form action "/test" method "post" )
   (:table
     (:tr (:td "Player: ")  (:td ((:input name "player" value "grabola")) ))
     (:tr (:td "day: ")     (:td ((:input name "data" value "2006-11-30")) ))
     (:tr ((:td colaspan "2") ((:input type "submit" value "Send QUERY")))   )))
	
  (awhen (get-parameter "player")
    (html (:prin1 (generate-query it (get-parameter "data"))))
       ))
