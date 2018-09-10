(defvar nr 0)
(net.aserve:publish-directory :prefix "/" :destination "static/")

(defun test-page()
  (html "hs:" (:prin1 +hs+) :br
	"pot:" (:prin1 (game-pot +game+)) :br   ))

(def-url-0 "/registerclient"
  (html "ok"))

(def-url-0 "/smallblind"
  (setq +game+ (make-game :kind +game-kind+)
	  +cards+ nil
	  +pos+ nil
	  +hs+ nil)
  (incf (game-pot +game+) (www-amount))
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/bigblind"
  (incf (game-pot +game+) (www-amount))
    (incf nr)(html (:prin1 nr)))

(def-url-0 "/call"

  (incf (game-pot +game+) (www-amount))
    (test-page)
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/check"
    (test-page)
  (incf nr)(html (:prin1 nr)))
 

(def-url-0 "/fold"
    (test-page)
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/bet"
  (incf (game-pot +game+) (www-amount))
    (test-page)
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/raise"
  (incf (game-pot +game+) (www-amount))
    (test-page)
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/allin"
  (incf (game-pot +game+) (www-amount))
    (test-page)
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/flop"
    (test-page)
    (incf nr)(html (:prin1 nr)))

(def-url-0 "/turn"
    (test-page)
    (incf nr)(html (:prin1 nr)))

(def-url-0 "/river"
    (test-page)
    (incf nr)(html (:prin1 nr)))

(def-url-0 "/holecards"
  (if (= 3 +game-kind+)
      (progn
      (setq +cards+ 
	  (list  (get-int-parameter "c1") (get-int-parameter "c2")
		  (get-int-parameter "c3") (get-int-parameter "c4")))
      (setq +hs+ (log-odds (gethash (sort +cards+ #'<) *h*))))   
      (setq +cards+ 
	  (list  (get-int-parameter "c1") (get-int-parameter "c2"))) )
  (incf nr)(html (:prin1 nr)))

(def-url-0 "/action"
    (test-page)
    (incf nr)(html (:prin1 nr)))

(def-url-0 "/"
    (html (:princ +game+)))