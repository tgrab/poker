(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :dekoduj-czas :permutacje 
	   :aif :awhen :it :start-server))



(defun sys-compile()
	(load (compile-file "moje.lisp")) 
	(use-package :moje)
	(load (compile-file "poker_ranks.lisp"))
	(use-package :poker.ranks)
	(load (compile-file "poker.lisp"))
	(load "rules.lisp")
	(load (compile-file "poker_in.lisp"))
	(load (compile-file "poker_out.lisp")))

(defun sys-load()
	(load "moje")
	(use-package :moje)
	(load "poker_ranks")
	(use-package :poker.ranks)
	(load "poker")
	(load "rules.lisp")
	(load "poker_in")
	(load "poker_out"))

   
(defun sys-create()
  (sys-compile)
  #+clisp
  (ext:saveinitmem "thx.exe" 
		   :executable t 
		   :quiet t 
		   :init-function #'moje:start-server)) 
