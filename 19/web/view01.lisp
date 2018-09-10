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
     (:p "ver. 19, checkout 666"))
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
        ((:h2 :class "title") "System thx ver. 19")
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
        ((:h3 :class "title") "Current:" (str +game+))
        ((:div :class "entry") ((:div :id "result1")  "&nbsp;"))
       );end post
     ) ;end content

     ((:div :id "sidebar")
      (:ul
       (:li
	   (:h2 "Utilities")
	      (:ul
	        (:li ((:a :href "javascript:setResult1(\"/gameview\")") "Show game")) 
		(:li ((:a :href "javascript:setResult1(\"/showgame\")") "Game details"))       
		(:li ((:a :href "javascript:setResult1(\"/showmemo\")") "Memory"))    
		(:li ((:a :href "javascript:setResult1(\"/showgameslist\")") "Games list"))
		(:li ((:a :href "javascript:setResult1(\"/deletegameslist\")") "Delete games list"))
		))
       ))  ))


(defun view-show-memory(label tbl)
  (format t "<tr><td colspan='2' style='color:white;background-color:black;font-size=12px'>~A~&" label)
    (maphash 
     (lambda (k v)
       (format t "<tr><td>~A<td>" k)
       (dolist (e v)
	     (format t "~A" (cdar e) )  )
       (format t "~&"))
     tbl)
)

(defun view-memory()
  (poker::rules-percept-memo)
  (format t "<table border='1'>")
  (view-show-memory "MEMO" (memo +game+))
  (view-show-memory "GAME" (game-memo +game+))
  (view-show-memory "ROUND" (round-memo +game+))
  (view-show-memory "PERCEPT" (percept-memo +game+))
  (format t "</table>"))

(def-url "/showgame"
    (progn
     (format t "<table border='1'>")
     (format t "<tr><td>casino:<td><b>~A</b>" (casino +game+))
     (format t "<tr><td>table:<td><b>~A</b>" (table-name +game+))
     (format t "<tr><td>id:<td>~A" (id +game+))
     (format t "<tr><td>czas:<td>~A" (dekoduj-czas (czas +game+)))
     (format t "<tr><td>pot:<td>~A" (pot +game+))
     (format t "<tr><td>stake:<td>~A" (poker::stake +game+))
     (format t "<tr><td>blind:<td>~A" (blind +game+))
     (format t "<tr><td>kind:<td>~A" (kind +game+))
     (format t "<tr><td>balance:<td>~A" (balance +game+))
     (format t "<tr><td>total-balance:<td>~A" (total-balance +game+))
     (format t "<tr><td>history:<td>~A" (history +game+))
     (format t "<tr><td>history-log:<td>~A" (history-log +game+))
     (format t "<tr><td>free-seat:<td>~A" (free-seat +game+))
     (format t "<tr><td>agent-name:<td>~A" (agent-name +game+))
     (format t "<tr><td>agent-pos:<td>~A" (agent-pos +game+))
     (format t "<tr><td>name->seat:<td>~A" (name->seat +game+))
     (format t "<tr><td>table-size:<td>~A" (table-size +game+))
     (format t "<tr><td>taken-seats:<td>~A" (taken-seats +game+))
     (format t "<tr><td>active-player:<td>~A" (active-players +game+))
     (format t "<tr><td>table-cards:<td>~A" (karty->napisy (table-cards +game+)))
     (format t "<tr><td>cards:<td>~A" (karty->napisy (cards +game+)))
     (format t "</table>")
;     (view-memory)
   ;  (princ "<div style='color:blue'>")
    ; (dolist (p *cortex*)
     ;  (format t "~A<br>" p))
     ;(princ "</div>")
   ))

(def-url "/showmemo"
    (view-memory))

(def-url "/showgameslist"
    (dolist (g poker::*games-list*)
      (format t "~A <br>~&" g)))


(def-url "/deletegameslist"
    (setq poker::*games-list* nil))



(def-url "/gameview"
    (progn
   (awhen (cards +game+)  
	       (format t "<table style='padding:10px; border:1px solid black'><tr><td>~A cards: ~A" (agent-name +game+) (karty->html it))
	       (when (table-cards +game+)
		 (if (= 4 (length it))
		     (princ (render-rank (omaha-ocena it (table-cards +game+))))
		     (princ (render-rank (ocena (append it (table-cards +game+))))) ))      )
   (aif (table-cards +game+)  
	     (format t "<td style='padding-left: 20px;'>Table: ~A</table>" (karty->html it))
	     (format t "</table>"))

	(format t  "<table border=1>")
	(dotimes (i (length (name->seat +game+)))
	  (let ((player-name (first (nth i (reverse (name->seat +game+))))))
	  (if (aref (active-players +game+) i)
	      (format t "<tr><td><b>~A</b><td>~$<td><b>~$</b><td>~$<td>~A" 
		      player-name (player-money player-name)
		      (aref (balance +game+) i)
		      (aref (total-balance +game+) i)
		      (reverse (poker::player-history i))      )	    
	      (format t "<tr style='background-color: silver'><td><b>~A</b><td>~$<td><b>~$</b><td>~$<td>~A" 
		      player-name (player-money player-name )
		      (aref (balance +game+) i)
		      (aref (total-balance +game+) i)
		      (reverse (poker::player-history i))    ))   ))
	  (format t  "</table>")

    ))


(def-url "/dbgames"
 (let ((lista (db:poker-query "select id,czas from crypto")))
   (dolist (g lista)
	(format t "<a href='/choosegame?id=~A'>~A</a><br>"  (first g) (dekoduj-czas (second g)))

	)))  