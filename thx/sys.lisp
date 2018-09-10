(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :dekoduj-czas :permutacje 
	   :aif :awhen :it :start-server))




(defun sys-compile()
	(load (compile-file "lispcode/moje.lisp")) 
	(use-package :moje)
	(load (compile-file "lispcode/poker_ranks.lisp"))
	(use-package :poker.ranks)
	(load (compile-file "lispcode/poker.lisp"))
	(load "lispcode/rules.lisp")
	(load (compile-file "lispcode/poker_in.lisp"))
	(load (compile-file "lispcode/poker_out.lisp")))

(defun sys-load()
	(load "lispcode/moje.abcl")
	(use-package :moje)
	(load "lispcode/poker_ranks.abcl")
	(use-package :poker.ranks)
	(load "lispcode/poker.abcl")
	(load "lispcode/rules.lisp")
	(load "lispcode/poker_in.abcl")
	(load "lispcode/poker_out.abcl"))
	
(defun sys-create()
  (sys-compile)
  #+clisp
  (ext:saveinitmem "thx.exe" 
		   :executable t 
		   :quiet t 
		   :init-function #'moje:start-server)) 