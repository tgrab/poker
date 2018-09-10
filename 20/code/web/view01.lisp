(in-package :view)

(defmacro capture-output(form)
  `(with-output-to-string(s)
      (let ((*standard-output* s))
	,form)))


(defmacro template-page(title &body code)
`(with-html-output (*standard-output* nil :prologue t :indent t)
   (:html(:head
        (:title ,title)
	(:script :language "Javascript" :src "factory.js")
	(:link :href "default.css" :rel "stylesheet" :type "text/css" :media "screen"))
   (:body
    ((:div :id "logo")
     (:h1 "Poker Systems")
     (:p "ver. 20, checkout 666"))
    ((:div :id "menu") 
     (:ul
	  (:li ((:a :href "/") "Main") )
	  (:li ((:a :href "/game") "Game engine") )
	  (:li ((:a :href "/admin") "Administration") )
		))
    
    ((:div :id "page")
         ((:div :id "page-bg")
	  ,@code )
	  ((:div :style "clear:both") "&nbsp;"))
    ((:div :id "footer") (:p "&copy;2008 Design by Tomasz Grabski"))
      ))))



(def-url "/showroom" (room))

(def-url "/"
 (template-page "Poker Main Page"
   ((:div :id "content") 
      ((:div :class "post")
        ((:h2 :class "title") "System thx ver. 20")
        ((:div :class "entry") 
	 ((:div :id "result1")   
	  ;(:img :src "icons/as.gif") 
	  ;(:img :src "icons/ah.gif")
	  "&nbsp;"))
       );end post
     ) ;end content

     ((:div :id "sidebar")
      (:ul
       (:li
	   (:h2 "Utilities")
	      (:ul (:li ((:a :href "javascript:setResult1(\"/showroom\")") "room"))       

		   ))
       ))  ))




(def-url "/admin"
 (template-page "Poker Admin Page"
    ((:div :id "content") 
    ((:div :class "post")
        ((:h2 :class "title") "Admin Page")
        ((:div :class "entry") ((:div :id "result1")  "&nbsp;"))
     );end post             

     )
    ((:div :id "sidebar")
      (:ul
       (:h2 "For developers")
       (:li ((:a :href "javascript:setResult1(\"/adminreload\")") "Reload view"))
       ))))
      
    
(def-url "/adminreload"
 (load "web/view01.lisp") )  



(def-url "/game"
 (template-page "Poker Game Page"
   ((:div :id "content") 
      ((:div :class "post")
        ((:div :class "entry") ((:div :id "result1")  "&nbsp;"))
       );end post
     ) ;end content

     ((:div :id "sidebar")
      (:ul
       (:li
	   (:h2 "Utilities")
	      (:ul
	        (:li ((:a :href "javascript:setResult1(\"/selectgame\")") "Select game")) 
		(:li ((:a :href "javascript:setResult1(\"/nextpercept\")") "Next percept")) 
		(:li ((:a :href "javascript:setResult1(\"/showgame\")") "Current game"))       
		(:li ((:a :href "javascript:setResult1(\"/showmemo\")") "Memory"))    		
		))
       ))  ))




(def-url "/selectgame"
      (dolist (g (db:poker-query "select id from crypto"))
	(format t "<a href='javascript:setResult1(\"/readgame?id=~A\")'>~A</a><br>" (first g) (first g)) ))

(def-url "/readgame"
    (progn
      (setq *game* (poker::read-game (nparam "id")))
      (print "ok")))

(def-url "/showgame"
    (poker::render-html *game*))

(def-url "/nextpercept"
    (let ((percept (poker::next-percept *game*)))
	   (let ((np (nth (1+ (poker::percept-nr *game*)) (poker::db-history *game*))))
	     (when (numberp (car np))
	       (when (equal (car 
			     (nth (car np) (poker::db-players *game*)))
			    "Grabol")
	       (format t "<div style='border:1px solid red;color:red'>Grabol will: ~A</div>" (poker::action-rules))
	       )))
	   (poker::render-html *game*)
	   (format t "Last percept: ~A" percept)))





(def-url "/showmemo"
    (progn 
      (poker::common-percepts-rules *game*)
      (format t "~A" (poker::game-memory *game*)) ))

