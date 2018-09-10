(in-package :view)


(defmacro prima-page(&body code)
`(with-html-output (*standard-output* nil :prologue t :indent t)
   (:html
    (:head
	(:style (str *style*)))
   (:body
    "[ " ((:a :href "/") "Main page") " ] [ " ((:a :href "/gameslist") "Games list") " ] [ "
     ((:a :href "/selecttable") "Select table") " ]"
    (:hr)
    ,@code
    ))))

(def-url "/gameslist"
    (prima-page (poker::view-prima-records)))


(def-url "/selecttable"
    (prima-page
      (awhen (param "name") (setq poker::*prima-table* it))
      (format t "<h2>Current table: ~A</h2>" poker::*prima-table*)
      (dolist (tbl (poker::prima-tables))
	(format t "<a href='/selecttable?name=~A'>~A</a><br>" tbl tbl))))

(def-url "/game"
    (prima-page
      (awhen (param "id") 
	(print (poker::prima-find-record it)))
))