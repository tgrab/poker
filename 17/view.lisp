(defmacro with-html(&body body)
    `(with-html-output-to-string (*standard-output* nil :prologue nil)
	,@body))


(defmacro main-page((&key title with-menu with-ajax with-style) &body body)
    `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
      (:html
	(:head
	 (when ,title (htm (:title ,title)))
	 (when ,with-style (htm  
			    "<link rel='stylesheet' type='text/css' href='/files/style.css' />"
			    )) 
	 (when ,with-menu (htm  
			    "<script type='text/javascript' src='/files/dropdowntabfiles/dropdowntabs.js'></script>"
			    "<link rel='stylesheet' type='text/css' href='/files/dropdowntabfiles/bluetabs.css' />"
			    ))
	 (when ,with-ajax (htm  
			    "<script type='text/javascript' src='/files/factory.js'></script>"
			    ))
	 )	
	(:body
	  (when ,with-menu (princ (view-menu)))
	:br :br
	  (htm ,@body)))) )

;(def-url "/url" funkcja)
(defmacro def-url(url func)
    `(pushnew (create-prefix-dispatcher ,url ',func)   *dispatch-table*))

(setq *dispatch-table*
    (list
	(create-folder-dispatcher-and-handler "/files/" "files/")
	(create-prefix-dispatcher "/" 'view-index)
	#'default-dispatcher))

(load (compile-file "pages/index.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/percepts.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/calendar.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/select_games_list.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/choose_selected_games.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/show_properties.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/show_memory.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/render_game.lisp" :output-file "../binaries/page.fasl"))
(load (compile-file "pages/game.lisp" :output-file "../binaries/page.fasl"))
