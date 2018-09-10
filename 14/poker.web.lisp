;(declaim (optimize (speed 3) (safety 0)))
;; This is web interface to poker engine

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

(defmacro get-int-parameter(param)
  `(parse-integer (get-parameter ,param)))

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
	     ,@body )))))

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



(defun start-aserve(&optional (port 9001))
  (print (net.aserve:start :port port))
  #+cmu(mp::startup-idle-and-top-level-loops))

;;--------------------------------------------------------------------------
;;------------------------------- Poker View -------------------------------
;;--------------------------------------------------------------------------
(defmacro www-player()
  '(get-parameter "name"))

(defmacro www-amount()
  '(read-from-string (get-parameter "amount")))

(defmacro main-page(&body body)
  `(html
    (:html
     (:head "<link rel='stylesheet' href='styl.css' type='text/css'>" )
     (:body
      
      ,@body        ))))

(defmacro show-number(n)
  `(:princ (format nil "~6,2F" ,n)))

(defun game-stats()
  "WWW description of the game"
  (html
   "pot: " (show-number (game-pot +game+)) :br
   "pot-odds:" 
   (show-number (count-agent-pot-odds))   :br
   (when +cards+
     (html
      "Hand: " (show-number +hs+) :br
      ))   ))

(defun game-stats-1()
  "WWW description of the game"
  (html
   (:b (:prin1 (first (game-history +game+)))) :br
   "stake:" (:prin1 (game-stake +game+)) :br
   "pot:" (:prin1 (game-pot +game+)) :br
   (when +cards+
     (html
      (:prin1 +hs+) :br
      ))
   (dolist (p (game-players +game+))
     (when p
       (html 
	(:princ (player-name p))
	"("  (:prin1 (player-total-balance p))   ") "
	(:prin1 (player-balance p))
	(when (= (player-seat p)
		 (game-active-player +game+))
	  (html " Active, pot odds: "
		(let ((stawka (count-stake +game+ p)))
		  (if (plusp stawka)
		      (html (:prin1 (/ (game-pot +game+)
			   stawka)))
		      (html "Inf."))) ))
	:br ) )) ))

(net.aserve:publish-directory :prefix "/" :destination "static/")

(def-url-0 "/registerclient"
  (setq +game-kind+ (parse-integer (get-parameter "gametype")))
  (html "ok"))

(def-url-0 "/smallblind"
  ; starting new game !
  (push +game+ +game-history+)  
  (setq +game+ (make-game :kind +game-kind+)
	  +cards+ nil
	  +pos+ nil
	  +hs+ nil)
  (percept-small-blind +game+ (www-player) (www-amount))
  (game-stats))

(def-url-0 "/bigblind"
  (percept-big-blind +game+ (www-player) (www-amount))
    (game-stats))

(def-url-0 "/call"
  (percept-call +game+ (www-player) (www-amount))
  (game-stats))

(def-url-0 "/check"
  (percept-check +game+ (www-player))
  (game-stats))
 

(def-url-0 "/fold"
  (percept-fold +game+ (www-player))
  (game-stats))

(def-url-0 "/bet"
  (percept-bet +game+ (www-player) (www-amount))
  (game-stats))

(def-url-0 "/raise"
  (percept-raise +game+ (www-player) (www-amount))
  (game-stats))

(def-url-0 "/allin"
  (percept-allin +game+ (www-player) (www-amount))
  (game-stats))

(def-url-0 "/flop"
    (percept-flop +game+
		  (get-int-parameter "c1")
		  (get-int-parameter "c2")
		  (get-int-parameter "c3"))
    (game-stats))

(def-url-0 "/turn"
    (percept-turn +game+
		  (get-int-parameter "c1"))
    (game-stats))

(def-url-0 "/river"
    (percept-river +game+
		  (get-int-parameter "c1"))
    (game-stats))

(def-url-0 "/holecards"
  (if (= 3 +game-kind+)
      (progn
      (setq +cards+ 
	  (list  (get-int-parameter "c1") (get-int-parameter "c2")
		  (get-int-parameter "c3") (get-int-parameter "c4")))
      (setq +hs+ (log-odds (gethash (sort +cards+ #'<) *h*))))   
      (setq +cards+ 
	  (list  (get-int-parameter "c1") (get-int-parameter "c2"))) )
  (game-stats))

(def-url-0 "/action"
    (unless +pos+ (setq +pos+ (game-active-player +game+)))
    (game-stats))

(def-url-0 "/"
    (html (:princ +game+)))