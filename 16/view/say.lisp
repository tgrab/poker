(def-url-0 "/say"
      (let ((txt (get-parameter "text")))
	(when txt
	  (say-text txt)) 	  
   ))
