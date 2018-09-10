;---- CORE -----------------
; (ext:without-package-locks
;    (asdf:oos 'asdf:load-op :aserve))
; (asdf:oos 'asdf:load-op :pg)
; (asdf:oos 'asdf:load-op :swank)

#+sbcl(require :aserve)
#+sbcl(require :webactions)
#+sbcl(require :pg)
#+sbcl(require :swank)

(load (compile-file "main.lisp" :output-file "binaries/main.x86f"))

(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))


(use-package :poker.ranks)
(use-package :net.html.generator)
(use-package :net.aserve)
(use-package :anaphor)
(use-package :pg)

(load (compile-file "poker.ranks.lisp" :output-file "binaries/poker.ranks.x86f"))
(load (compile-file "poker.defs.lisp" :output-file "binaries/poker.defs.x86f"))
(load (compile-file "poker.logic.lisp" :output-file "binaries/poker.logic.x86f"))
(load (compile-file "poker.db.lisp" :output-file "binaries/poker.db.x86f"))
(load (compile-file "poker.logging.lisp" :output-file "binaries/poker.logging.x86f"))
(load (compile-file "poker.perception.lisp" :output-file "binaries/poker.perception.x86f"))
(load (compile-file "poker.view.lisp" :output-file "binaries/poker.view.x86f"))