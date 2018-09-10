#+cmu(pushnew :hunchentoot-no-ssl *features*)

(asdf:oos 'asdf:load-op :hunchentoot)
(asdf:oos 'asdf:load-op :clsql-postgresql)
(asdf:oos 'asdf:load-op :cl-who)
;(asdf:oos 'asdf:load-op :swank)
(asdf:oos 'asdf:load-op :trivial-sockets)
;#+sbcl(asdf:oos 'asdf:load-op :linedit)

(use-package :hunchentoot)
(use-package :cl-who)

(load (compile-file "main.lisp" :output-file "binaries/main.fasl"))
(use-package :moje)


(defpackage :poker.ranks
  (:use :cl :moje)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))

(use-package :poker.ranks)

(load (compile-file "poker.defs.lisp" :output-file "binaries/defs.fasl"))
(load (compile-file "poker.ranks.lisp" :output-file "binaries/ranks.fasl"))
(load (compile-file "poker.main.lisp" :output-file "binaries/main.fasl"))
(load (compile-file "poker.holdem.lisp" :output-file "binaries/holdem.fasl"))
(load (compile-file "logic.lisp" :output-file "binaries/logic.fasl"))
(load (compile-file "poker.logs.lisp" :output-file "binaries/logs.fasl"))
(load  "poker.rules.lisp" )
(load (compile-file "poker.rules.logic.lisp" :output-file "binaries/rules.logic.fasl"))
(load (compile-file "poker.percepts.lisp" :output-file "binaries/percepts.fasl"))

(load (compile-file "db.lisp" :output-file "binaries/db.fasl"))

(load (compile-file "view.menu.lisp" :output-file "binaries/view.menu.fasl"))
(load (compile-file "view.lisp" :output-file "binaries/view.fasl"))

;(linedit:install-repl)

