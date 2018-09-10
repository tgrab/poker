(def-clp-function thx_mainpage(req ent args body)
  (html
    (:html
     ;(:head "<link rel='stylesheet' href='styl.css' type='text/css'>" )
     (:head
     (awhen (assoc "title" args :test #'string=)
      (html (:title (:princ (cdr it)))) )
     (:style
	     "body { background-color: silver;}"
	     ".karty { list-style : none; margin: 0; padding: 0; font-size: 12pt;}"
	     ".spades { display: inline; color: black; padding-left: 5px;}"
	     ".hearts { display: inline; color: red; padding-left: 5px;}"
	     ".diamonds { display: inline; color: blue; padding-left: 5px;}"
	     ".clubs { display: inline; color: green; padding-left: 5px;}"      ))
     (:body
      (emit-clp-entity req ent body)      
      
         ))))



(def-clp-function thx_menu(req ent args body)
  (html
   ((:a href "/search") " [Select games list] ")
   ((:a href "/gameslist") " [Show games list] ")
   ((:a href "/game") " [Show current game] ")
   ((:a href "/checklearning?kind=3") " [Test Omaha] ")
   ((:a href "/checklearning?kind=2") " [Test Holdem Fixed] ")
   ((:a href "/calendar") " [Calendar] ")
   ((:a href "/monthcalendar?kind=3&year=2007&month=6") " [Calendar Omaha June] ")
   :hr)   )