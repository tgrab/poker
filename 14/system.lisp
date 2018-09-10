(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena :zwyciezca))

(use-package :poker.ranks)
(use-package :net.html.generator)


(load (compile-file "poker.ranks.lisp" :output-file "/dane/lisp/poker.ranks.x86f"))
(load (compile-file "poker.main.lisp" :output-file "/dane/lisp/poker.main.x86f"))
(load (compile-file "poker.web.lisp" :output-file "/dane/lisp/poker.web.x86f"))

(wczytaj-baze "/raid/code/lisp/karty/logs/omaha10.dat")

(defun create-system()
;  #+sbcl(sb-ext:save-lisp-and-die "poker.exe" :toplevel #'start-system :executable t)
  #+sbcl(sb-ext:save-lisp-and-die "poker.core" :toplevel #'start-aserve)
  #+cmu(ext:save-lisp "poker.core" :init-function #'start-aserve))
