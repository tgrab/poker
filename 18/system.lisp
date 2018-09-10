(load "system_asdf.lisp")

(defun compile-system()
    (compile-file "moje")
    (compile-file "ranks" )
    (compile-file "logs")
    (compile-file "logs_prima")    
    (compile-file "logs_wh")
    (compile-file "main" )
    (compile-file "logic" )
    (compile-file "holdem")
    (compile-file "omaha" )
    (compile-file "percepts" )    
    (compile-file "percepts_rules" )
    (compile-file "http" )    
    (compile-file "view00")
    (compile-file "view01")) 

(load  "moje")
(use-package :moje)

(defpackage :poker.ranks
  (:use :cl :moje)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))

(use-package :poker.ranks)
(load "ranks" )

(load "logic" )
(load  "main" )
(load "logs")
(load "logs_wh")
(load "holdem")
(load "omaha" )
(load "rules.lisp" )
(load "percepts" )
(load "percepts_rules" )

(defpackage :http
  (:use :cl)
  (:export :start-server :def-url :get-parameter :*log-stream*))

(use-package :http)

;dziala tylko na clisp, cmucl:
(load  "http")
(load  "view00")
(load  "view01")
(load  "view02")



(defun create-system()
  #+sbcl
  (save-lisp-and-die "thx18.exe" :executable t)
  #+clisp
  (ext:saveinitmem "thx18.exe" :executable t :quiet t :init-function #'start-server))

