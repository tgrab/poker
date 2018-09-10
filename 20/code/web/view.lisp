(in-package :view)


(defparameter *style*
"
.karty {
 list-style : none;
 margin: 0;
 padding: 0;
}

.spades {
 display: inline;
 color: black;
 padding-left: 5px;
}

.hearts {
 display: inline;
 color: red;
 padding-left: 5px;
}

.diamonds {
 display: inline;
 color: blue;
 padding-left: 5px;
}

.clubs {
 display: inline;
 color: green;
 padding-left: 5px;
}

body {
    font: 12px monospace;
    margin-left: 20px;
}

")

(defmacro basic-page(&body code)
`(with-html-output (*standard-output* nil :prologue t :indent t)
   (:html
    (:head
	;(:link :href "style.css" :rel "stylesheet" :type "text/css")
	(:style (str *style*)))
   (:body
    "[ " ((:a :href "/") "Main page") " ] [ " ((:a :href "/gameslists") "Games lists") " ]"
    (:hr)
    ,@code
    ))))

(defun menu1()
  (with-html-output (*standard-output* nil)
    "[ " ((:a :href "/perceptpage") "Simulate") " ][ "
    ((:a :href "/view") "View game") " ]"
    ((:a :href "/perceptview") "Percept view") " ]"
    ((:a :href "/choosegame") "Choose game") " ]"
    (:hr)   ))
