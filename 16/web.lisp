;(declaim (optimize (speed 3) (safety 0)))
;; This is web interface to poker engine

;; ----- Web Engine ---------------------------------
(setq net.aserve:*default-aserve-external-format* :latin2-base)

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
  `(let ((p (get-parameter ,param)))
    (when p
      (read-from-string p))))


(defmacro get-int-parameter(param)
  `(parse-integer (get-parameter ,param)))

(defmacro pobierz-cookie()
  '(cdr (assoc "moje" (net.aserve:get-cookie-values req) :test #'equal)))


(defmacro def-url(path body &key (content "text/html; charset=ISO-8859-2" ))
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
 `(net.aserve:publish :path ,path :content-type "text/html; charset=ISO-8859-2" :function
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



;;------------- HTML Tools ----------------------------
;Ajax Code
(defun use-ajax()
  (html
   "<script>"
   "var xmlHttp;"
   "function createHttpRequest(){
     xmlHttp=new XMLHttpRequest();
   }"
   "</script>"

   ))
