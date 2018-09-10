(defmacro fmt(contr &body args)
  `(format nil ,contr ,@args))

(defun out(&rest s)
  (format t "~{~A ~}" s))

(defun htm-atribs(href id class style)
  (with-output-to-string (s)
    (format s "")
    (when id (format s " id=\"~A\"" id))
    (when class (format s " class=\"~A\"" class))
    (when style (format s " style=\"~A\"" style))
    (when href (format s " href=\"~A\"" href)) )   )

(defun htm-begin(el href id class style block)
  (if block
      (format nil "<~A~A>~&" el (htm-atribs href id class style))
      (format nil "<~A~A>" el (htm-atribs href id class style)  )))

(defun htm-end(el block)
  (if block
      (format nil "</~A>~&" el)
      (format nil "</~A>" el  )))

(defmacro htm(el (&key href id class style block) &body cont)
    `(progn
       (princ (htm-begin ,el ,href ,id ,class ,style ,block))
       ,@cont
       (princ (htm-end ,el ,block))      ))


(defmacro div((&key id class style) &body cont)
  `(htm "div" (:id ,id :class ,class :style ,style :block t) ,@cont))

(defmacro a((&key href id class style) cont)
  `(htm "a" (:href ,href :id ,id :class ,class :style ,style ) (princ ,cont)))

(defun br()
  (princ "<br/>"))

(defmacro page(title &body body)
  `(with-output-to-string(s)
     (let ((*standard-output* s))
       (format t "<html>~&")
       (format t "<head>~&")
       (format t "<title>~A</title>~&" ,title)
       (format t "</head>~&")
       (format t "<body>~&")
       (a (:href "/primalogs") " [Home] ") 
       (a (:href "/primalogs/gameslist") " [Gameslist] ")
       (a (:href "/primalogs/playerslist") " [Playerslist] ")
       (a (:href "/primalogs/update") " [Update] ")
       (out "<hr>")
       ,@body
       (format t "~&</body>~&")
       (format t "<html>~&"))))

