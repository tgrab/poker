(def-url-0 "/test"
    (html (:princ (setf 
		   (websession-variable 
		    (websession-from-req req) "user")
		   "tgrab")) ))

(def-url-0 "/test2"
    (html (:princ (websession-variable 
		    (websession-from-req req) "user")
		   ) ))