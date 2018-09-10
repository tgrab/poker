(defvar *fd-handler*)

(defun forcibly-close-stream (s)
  (let ((fd (sys:fd-stream-fd s)))
    (multiple-value-bind (r e) (ignore-errors (close s) t)
      (unless r
	(format t "Unable to close fd ~A: ~A, trying harder ~%" fd e)
	(multiple-value-bind (r e) (ignore-errors (close s :abort t) t)
	  (unless r
	    (format t "still unable to close ~A: ~A, try harder ~%" fd e)
	    (multiple-value-bind (r e)
		(ignore-errors (unix:unix-close fd) t)
	      (unless r
		(format t "Even unix-close failed on ~A:~A, giving up~%"
			fd e)))))))))


(defun serve-request (gniazdo)
   (let ((stream  (sys:make-fd-stream 
                     (ext:accept-tcp-connection gniazdo)
                     :element-type 'base-char :input t :output t ) )
	 (req nil))
        (unwind-protect
           (loop 
	    (setq req (read stream) )
	    (format t "Got: ~A "req)
	    (force-output)
	    (princ (eval req) stream)
	    (force-output stream)

)
           (forcibly-close-stream stream)) ) )

(defun start-listening ()
  (handler-case
    (let ((socket
	 (ext:create-inet-listener 9000
                                   :stream
				   :reuse-address t  )))
      (setq *fd-handler* #'(lambda (fd)
                            (declare (ignore fd))
                            (serve-request socket)))
    
      (sys:add-fd-handler socket :input  *fd-handler*)
    ) (error (r) (format t "~A~&" r)) ))