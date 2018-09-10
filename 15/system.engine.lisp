(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))

(use-package :poker.ranks)
(use-package :net.html.generator)
(use-package :anaphor)
;(use-package :pg)


(load (compile-file "poker.ranks.lisp" :output-file "/dane/lisp/poker.ranks.x86f"))
(load (compile-file "web.lisp" :output-file "/dane/lisp/web.x86f"))
(load (compile-file "poker.main.lisp" :output-file "/dane/lisp/poker.main.x86f"))
(load (compile-file "poker.omaha.lisp" :output-file "/dane/lisp/poker.omaha.x86f"))
;(load (compile-file "poker.holdem.lisp" :output-file "/dane/lisp/poker.holdem.x86f"))

(defun action()
  (if +cards+
    (cond
     ((= 3 +game-kind+) (om-action))
     ((= 2 +game-kind+) (hf-action))
     ((= 1 +game-kind+) (hn-action))
     (t "fold unknown game"))
    "fold not enough data"))

(load (compile-file "poker.view.lisp" :output-file "/dane/lisp/poker.view.x86f"))
;(load (compile-file "poker.db.lisp" :output-file "/dane/lisp/poker.db.x86f"))


(start-aserve)

(defun create-system()
;  #+sbcl(sb-ext:save-lisp-and-die "poker.exe" :toplevel #'start-aserve :executable t)
  #+sbcl(sb-ext:save-lisp-and-die "poker.core" :toplevel #'start-aserve)
  #+cmu(ext:save-lisp "poker.core" :init-function #'start-aserve))
