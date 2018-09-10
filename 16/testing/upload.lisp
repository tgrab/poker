(defun getfile-function (posturl)
      (html 
	     ((:form :enctype "multipart/form-data"
		     :method "post"
		     :action posturl)
	      "Let me know what file to grab"
	      :br
	      ((:input :type "file" 
		       :name "thefile"
		       :value "*.txt"))
	      :br
	      ((:input :type "submit")))))

(def-url-0 "/upload"
    (main-page (getfile-function "/uploadpost")))


(def-url-0 "/uploadpost"
    (let 
	((cd (assoc :content-disposition (net.aserve:get-multipart-header req) :test #'eq))
	 (filename)
	 (sep))
      (if (and cd (consp (cadr cd)))
	  (setq filename (cdr (assoc "filename" 
				     (cddr (cadr cd))
				     :test #'equalp)))
	     (when filename
		 (setq sep
		       (max (or (position #\/ filename
					  :from-end t) -1)
			    (or (position #\\ filename
					  :from-end t) -1)))
		 (setq filename
		       (subseq filename (1+ sep) 
			       (length filename)))))
      (if (and filename (not (equal filename "")))
	  (progn
	    (with-open-file (pp filename :direction :output
				:if-exists :supersede
				:element-type '(unsigned-byte 8))
		    (format t "writing file ~s~%" filename)
		    (let ((buffer (make-array 4096
					      :element-type 
					      '(unsigned-byte 8))))
		      
		      (loop (let ((count (net.aserve:get-multipart-sequence 
					  req 
					  buffer)))
			      (if (null count) (return))
			      (write-sequence buffer pp :end count))))))
		      ))
  ;; now send back a response for the browser
 (main-page "-- processed the form, files written --"))



