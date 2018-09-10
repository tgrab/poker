(in-package :http)

(defvar *log-request* t)
(defvar *log-list* nil)


;; ------------- SPLIT-SEQUENCE --------------------------------------------
;; -------------------------------------------------------------------------

(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))
	    
	    
(defun split-sequence-if (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

	    
;; ------------- UTILS -----------------------------------------------------
;; -------------------------------------------------------------------------

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))

(defun s. (&rest args)
  "Concatenate ARGS as strings"
  (declare (optimize (speed 3)))
  (let ((*print-pretty* nil))
    (with-output-to-string (out)
      (dolist (arg args)
        (princ arg out)))))
;; -------- URL     ---------------------------------------------
;; --------------------------------------------------------------
#+clisp(defun urlstring-unescape (url-string) 
  (do* ((n 0 (+ n 1))
        (out '()))
      ((not (< n (length url-string))) (ext:convert-string-from-bytes 
                                              (coerce (reverse out) 'vector)
					      (ext:make-encoding :charset 'charset:iso-8859-2)))
    (let ((c (elt url-string n)))
      (push 
            (cond ((eql c #\%)
                   (progn (setf n (+ 2 n))
			 (or (parse-integer
				      url-string :start (- n 1)
				      :end (+ n 1)
				      :junk-allowed t
				      :radix 16)
			      (char-code #\?))))
                  ((eql c #\+)
                   (char-code #\Space ))
                  (t (char-code c )))
		  out))))

#+cmu(defun urlstring-unescape (url-string)
  (do* ((n 0 (+ n 1))
        (out '()))
      ((not (< n (length url-string))) (coerce (reverse out) 'string ))
    (let ((c (elt url-string n)))
      (setf out 
            (cond ((eql c #\%)
                   (progn (setf n (+ 2 n))
                          (cons (code-char
				 (or (parse-integer
				      url-string :start (- n 1)
				      :end (+ n 1)
				      :junk-allowed t
				      :radix 16) 32))
                                out)))
                  ((eql c #\+)
                   (cons #\Space out))
                  (t (cons c out)))))))



;; -------- REQUEST ---------------------------------------------
;; --------------------------------------------------------------
(defstruct request
  url 
  user
  method
  stream
  headers
  body)

(defun request-send-headers (request &key
                                 (content-type "text/html")
                                 content-length
				 cache-control
				 location
				 refresh
				 pragma
				 set-cookie
				 www-authenticate
                                 extra-http-headers
                                 (response-text "OK")
                                 (response-code 200))
  "Send HTTP/1.0 headers in response to REQUEST.  If the request HTTP version
is less than 1.0, do nothing.  If CONDITIONAL is true, may signal RESPONSE-SENT
instead of returning normally."
 (let ((stream (request-stream request)))
    (labels ((perhaps (if name &optional then)
	       (if if (princ (s. name ": " (or then if)  #.(format nil "~%"))
			     stream))))

     (format stream "HTTP/1.0 ~D ~A
Server: tgrab0.1
Connection: close~%"
      response-code response-text) 
      (perhaps content-type "Content-Type")
      (perhaps content-length "Content-Length")
      (if set-cookie
	  (let ((cookies (if (listp set-cookie) set-cookie (list set-cookie))))
	    (dolist (cookie cookies) (perhaps cookie "Set-Cookie"))))
      (perhaps cache-control "Cache-Control" )
      (perhaps refresh "Refresh" )
      (perhaps location "Location" )
      (perhaps pragma "Pragma" )
      (perhaps www-authenticate "WWW-Authenticate")
      (mapc #'(lambda (header)
                (format stream "~A: ~A~%" (car header) (cdr header)))
            extra-http-headers)
      (terpri stream))
    response-code)) 
  
;; rfc 1945 p26
(defvar *http-error-codes*
  '((400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")))
(defun request-send-error ( request &optional (error-code 404) &rest extra-stuff)
  "Send the client HTTP headers and HTML body for an error message
with numeric code ERROR-CODE.  EXTRA-STUFF is an optional format
control and arguments for additional information which is written to
*log-stream*"
  (let ((stream (request-stream request))
        (extra (if (and extra-stuff (stringp (car extra-stuff)))
		   (apply #'format nil extra-stuff)))
        (error-text (cdr (assoc error-code *http-error-codes*))))
;    (when *log-stream*
 ;     (format *log-stream* "~&Logged error: ~A ~A ~A~%" 
;	      error-code error-text extra))
    (request-send-headers request
                          :response-code error-code :response-text error-text)
    (if extra (princ extra stream)
              (format stream "<H1> REQUEST: ~A</H1><H2> ~A </H2>" (request-url request)  error-text) ) ))

;; --------- DEAMON -----------------------------------------------
;; ----------------------------------------------------------------

(defun read-folded-line (stream &optional eof-error-p eof-value)
  "Read a complete logical header line, including folded continuation lines."
  (with-output-to-string (o)
    (loop
     (let* ((l (read-line stream eof-error-p eof-value))
	    (end (position-if (lambda (x) (or (eql x (code-char 10))
					      (eql x (code-char 13))))
			      l))
	    (next (and (if end (> end 0) t)
		       (> (length l) 0)
		       (peek-char nil stream nil nil))))
       (write-sequence l o :end end)
       (unless (or (eql next #\Space) (eql next #\Tab)) (return))))))
 
 (defun header-value (name header-list)
  "Get the value of the header named NAME in HEADER-LIST"
  (cadr (assoc name header-list)))

(defun read-headers (stream)
  (let ((headers nil))
    (do ((line (read-folded-line stream t) (read-folded-line stream t)))
        ((or (not line) (zerop (length line))) headers)
      
      ;; RFC 1945: "Each header field consists of a name followed
      ;; immediately by a colon (":"), a single space (SP) character,
      ;; and the field value. Field names are case- insensitive."
      (let* ((colon-pos (position #\: line))
	     (keyword (subseq line 0 colon-pos))
	     (value (subseq line (+ 2 colon-pos)))
	     (keyword-symbol (intern (string-upcase keyword) :keyword))
	     (keyword-value (assoc keyword-symbol  headers)))
	(if keyword-value
	    (setf (cdr (last keyword-value)) (list value))
            (setf headers (acons keyword-symbol (list value) headers)))))
    headers))

(defun parse-body (body-string &optional (delimiters '(#\&)) end)
  "Parse BODY-STRING returning list of (var val) pairs"
  (mapcar (lambda (x)
            (mapcar #'urlstring-unescape (split-sequence #\= x :count 2)))
          (split-sequence-if (lambda (x) (member x delimiters))
			     body-string :end end)))
    
              
(defun read-request-from-stream (stream)
  (destructuring-bind
      (method url-string protocol)
      (split (read-line stream nil "Err Err Err") 3 '(#\Space))
    (when (string= method "Err")  (return-from read-request-from-stream nil))
    (let* ((headers (and protocol (read-headers stream)))
	   (content-length (parse-integer
			    (or (header-value :content-length headers) "0")))
	   (body (and (> content-length 0)
		      (make-array content-length :element-type
				  (stream-element-type stream))))
	   (len (and (> content-length 0)
		     (read-sequence body stream)))
	   (parsed-body (if body (parse-body body '(#\&) len) nil))
          )
      (awhen (position #\? url-string) 
               (setq parsed-body  
	                (parse-body (subseq url-string (1+ it)) )
                     url-string 
		        (subseq url-string 0 it)   ))
      (make-request 
		     :url url-string
		     :method method
		     :stream stream
		     :body parsed-body 
                     :headers headers ))))



(defstruct listener
  socket
  DocumentRoot
  handlers)

(defvar *listener* (make-listener :documentroot "./web/files/" 
                                  :handlers (make-hash-table :test #'equal)))
				  
				  
(defvar *content-types*
  '(("html" "text/html")
    ("gif" "image/gif")
    ("jpg" "image/jpeg")
    ("png" "image/png")
    ("css" "text/css")
    ("class" "application/octet-stream")
    ("doc" "application/octet-stream")
    ("zip" "application/octet-stream")
    ("gz" "application/octet-stream")
    ("ASF" "video/x-ms-asf")
    ("tar" "application/octet-stream")
    ("avi" "video/x-msvideo")
    ("txt" "text/plain")))

(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input file.  Do not
translate or otherwise maul anything. "
  (unless (subtypep (stream-element-type from) (stream-element-type to))
    (error "Incompatible streams ~A and ~A. Types are ~A and ~A." from to
           (stream-element-type from)
           (stream-element-type to)))
  (let ((buf (make-array 4096 :element-type (stream-element-type from))))
    (do ((pos (read-sequence buf from) (read-sequence buf from)))
        ((= 0 pos) nil)      
      (write-sequence buf to :end pos))))    

(defun send-file (r file-name &key content-type)
  (let ((stream (request-stream r))
	(content-type 
	 (or content-type
	     (cadr (assoc (or (pathname-type file-name) "txt") *content-types*
			  :test #'string=)))))

    #+cmu(with-open-file (in file-name :direction :input )
      (request-send-headers r :content-type content-type
			    :content-length (file-length in))
      (copy-stream in stream))
      			  
    #+clisp(with-open-file (in file-name :direction :input :element-type '(unsigned-byte 8) )
      (request-send-headers r :content-type content-type
			    :content-length (file-length in))
      (setf (stream-element-type stream) '(unsigned-byte 8))
      (copy-stream in stream))
    ))        


 
(defun do-it (listener stream)
     (let ((request (read-request-from-stream stream)))
        (when (null request) (return-from do-it nil))
        (let* ((url (request-url request)) (handler (gethash url (listener-handlers listener)))  )
	 (when *log-request* (push
					(format nil "~A : ~A ~&" url (request-body request))
					*log-list*) )
	 (cond
	   (handler (funcall handler request))
	   ((position #\. url)  (send-file request  
	                                   (concatenate 'string 
					                 (listener-documentroot listener)
							  url)))
	   (t  (request-send-error request 404)))))) 


#+clisp(defun forcibly-close-stream (s)
  (socket:socket-status s)
  (multiple-value-bind (r e) (ignore-errors (close s) t)
    (unless r
      (format t "Unable to close: ~A, trying harder ~%" e)
      (multiple-value-bind (r e) (ignore-errors (close s :abort t) t)
        (unless r
          (format t "Even close :abort t failed:~A, giving up~%"
                  e))))))

#+cmu(defun forcibly-close-stream (s)
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

#+clisp(defun serve-request (listener)
   (let ((s  (socket:socket-accept (listener-socket listener)
                        :buffered nil
                        :element-type 'character
                        ;:external-format (ext:make-encoding
                         ;          :charset 'charset:iso-8859-2;utf-8
                          ;         :line-terminator :unix)
				    )))
           (unwind-protect
                (do-it listener s)
                (forcibly-close-stream s)) ) )

#+cmu(defun serve-request (listener)
   (let ((stream  (sys:make-fd-stream 
                   (ext:accept-tcp-connection (listener-socket listener))
                   :element-type 'base-char :input t :output t ) ))
        (unwind-protect
           (do-it listener stream)
           (forcibly-close-stream stream)) ) )

#+clisp(defun start-server (&key (port 9000) )
  (let ((socket
	 (socket:socket-server port)))
    (setf (listener-socket *listener*) socket)
    (loop 
     (handler-case
       (when (socket:socket-status socket)
             (serve-request *listener*))
     (error (e) (princ e))))	     
	     ))
 

#+cmu(defun start-server (&key (port 9000))
    (setf  (listener-socket *listener*)
	 (ext:create-inet-listener port
                                   :stream
				   :reuse-address t  ))
     (sys:add-fd-handler  (listener-socket *listener*) :input
			  #'(lambda (fd)
			      (declare (ignore fd))
			      (serve-request *listener*))) )

(defmacro def-url (url name)
 `(setf (gethash ,url (listener-handlers *listener*))
      #'(lambda (request)
         (labels ( (param(name) (cadr (assoc name (request-body request) :test #'string=))  )
		   (nparam (name) (read-from-string (param name))) )
         (request-send-headers request)
         (let ((*standard-output* (request-stream request))
	       (*log-stream* *standard-output*))
	   *log-stream* ;zeby nie bylo ostrzezen
	   ,name ))    )))


(def-url "/admin_lc"
	(print (load (compile-file (param "file")))))


(def-url "/admin_eval"
	(print (eval (nparam "sexp"))))

(def-url "/admin_log"
	(print *log-list*))


;code ma zwrocic funkcje widoku
(defmacro def-mvc (url code)
 `(setf (gethash ,url (listener-handlers *listener*))
      #'(lambda (request)
         (labels ((nparam (name) (read-from-string (param name))) 
			(param(name) 
				(cadr 
					(assoc name (request-body request) :test #'string=))  ))
         (request-send-headers request)
         (let ((*standard-output* (request-stream request))
	       (*log-stream* *standard-output*))
	   *log-stream* ;zeby nie bylo ostrzezen
	   (funcall ,code) ))    )))

