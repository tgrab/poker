;---- CORE -----------------
; (ext:without-package-locks
;    (asdf:oos 'asdf:load-op :aserve))
; (asdf:oos 'asdf:load-op :pg)
; (asdf:oos 'asdf:load-op :swank)
(require :aserve)
(load (compile-file "main.lisp" :output-file "binaries/main.x86f"))

(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))


(use-package :poker.ranks)
(use-package :net.html.generator)
(use-package :anaphor)

(load (compile-file "logic.lisp" :output-file "binaries/logic.x86f"))
(load "poker.rules.lisp")
(load (compile-file "web.lisp" :output-file "binaries/web.x86f"))
(load (compile-file "poker.ranks.lisp" :output-file "binaries/poker.ranks.x86f"))
(load (compile-file "poker.main.lisp" :output-file "binaries/poker.main.x86f"))
(load (compile-file "poker.hfixed.lisp" :output-file "binaries/poker.hfixed.x86f"))
(load (compile-file "poker.omaha.lisp" :output-file "binaries/poker.omaha.x86f"))

(load (compile-file "poker.view.lisp" :output-file "binaries/poker.view.x86f"))




