;---- CORE -----------------
; (ext:without-package-locks
;    (asdf:oos 'asdf:load-op :aserve))
; (asdf:oos 'asdf:load-op :pg)
; (asdf:oos 'asdf:load-op :swak)

;;Usunieto ladowanie omaha logs!!!

(load (compile-file "main.lisp" :output-file "binaries/main.x86f"))

(defpackage :poker.ranks
  (:use :cl :anaphor)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))


(use-package :poker.ranks)
(use-package :net.html.generator)
(use-package :anaphor)
(use-package :pg)


(load (compile-file "poker.ranks.lisp" :output-file "binaries/poker.ranks.x86f"))
(load (compile-file "web.lisp" :output-file "binaries/web.x86f"))
(load (compile-file "poker.main.lisp" :output-file "binaries/poker.main.x86f"))
(load (compile-file "poker.omaha.lisp" :output-file "binaries/poker.omaha.x86f"))
(load (compile-file "poker.holdem.lisp" :output-file "binaries/poker.holdem.x86f"))

(defun action()
  (if +cards+
    (cond
     ((= 3 +game-kind+) (omaha!))
     ((= 2 +game-kind+) (holdem-fixed!))
     ((= 1 +game-kind+) (holdem-nolimit!))
     (t "fold unknown game"))
    "fold not enough data"))

(load (compile-file "poker.view.lisp" :output-file "binaries/poker.view.x86f"))
(load (compile-file "poker.db.lisp" :output-file "binaries/poker.db.x86f"))


;experimental code:
(load (compile-file "gameanalysis.lisp" :output-file "binaries/gameanalysis.x86f"))


(defun set-alarm()
    (setq +table-size-alarm+ '(5 "192.168.0.10" 10000)))

