;;--------------------------------------------------------------------------
;;------------------------------- Poker View -------------------------------
;;--------------------------------------------------------------------------
(defmacro www-player()
  '(get-parameter "name"))

(defmacro www-amount()
  '(read-from-string (get-parameter "amount")))

(defmacro show-number(n)
  `(:princ (format nil "~6,2F" ,n)))

(defun main-page-menu()
  (html
   ((:a href "/search") " [Select games list] ")
   ((:a href "/gameslist") " [Show games list] ")
   ((:a href "/game") " [Show current game] ")
   ((:a href "/checklearning?kind=3") " [Test Omaha] ")
   ((:a href "/checklearning?kind=30") " [Test Omaha Pre-Flop] ")
   ((:a href "/checklearning?kind=2") " [Test Holdem Fixed] ")
   ((:a href "/calendar") " [Calendar] ")
   ((:a href "/monthcalendar?kind=3&year=2007&month=6") " [Calendar Omaha June] ")
   :hr))


(defmacro main-page(&body body)
  `(html
    (:html
     ;(:head "<link rel='stylesheet' href='styl.css' type='text/css'>" )
     (:head (:style
	     "body { background-color: silver;}"
	     ".karty { list-style : none; margin: 0; padding: 0; font-size: 12pt;}"
	     ".spades { display: inline; color: black; padding-left: 5px;}"
	     ".hearts { display: inline; color: red; padding-left: 5px;}"
	     ".diamonds { display: inline; color: blue; padding-left: 5px;}"
	     ".clubs { display: inline; color: green; padding-left: 5px;}"      ))
     (:body
      (main-page-menu)
      
      ,@body        ))))


;#+cmu
;(dolist (plik (ext:print-directory "view/" nil :return-list t))
;    (load  plik ))

(dolist (plik (directory (make-pathname :name :wild :type :wild :defaults "view/*")))
    (load  plik ))


