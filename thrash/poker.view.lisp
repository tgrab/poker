;(declaim (optimize (speed 3) (safety 0)))
;; This is web interface to poker engine

;; ----- Web Engine ---------------------------------
(setq net.aserve:*default-aserve-external-format* :latin2-base)

(defmacro get-parameter(param) ;param musi byc stringiem!
  `(net.aserve:request-query-value ,param req))

(defmacro get-number-parameter(param)
  `(let ((p (get-parameter ,param)))
    (when p
      (read-from-string p))))


(defmacro get-int-parameter(param)
  `(parse-integer (get-parameter ,param)))

(defmacro def-url-0(path &rest body)
 `(net.aserve:publish :path ,path :content-type "text/html; charset=ISO-8859-2" :function
    (lambda (req ent)
      (net.aserve:with-http-response(req ent)
	(net.aserve:with-http-body(req ent) 
	     ,@body )))))

(defun start-aserve(&optional (port 9001))
  (print (net.aserve:start :port port))
  #+cmu(mp::startup-idle-and-top-level-loops))

;;---------------------------------------------------
;;------------------------------- Poker View --------
;;---------------------------------------------------

(webaction-project "thx"
		   :destination "site/"
		   :index "index"
		   :map
		   '(("index" "index.clp")
		     ("search" "search.clp")))


(dolist (plik (directory (make-pathname :name :wild :type :wild :defaults "sitecode/*")))
    (load  plik ))


