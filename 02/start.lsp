(load "/raid/code/lisp/http/serwer")
(load "/raid/code/lisp/http/html")
(load "/raid/code/lisp/http/karty")
(load "poker")
(load "thx07")
;(load "omaha")
;(load "ompot01.lisp")
;(wczytaj-baze "logs/omaha10.dat")

(mp:make-process #'(lambda ()
   (sluchaj-udp "192.168.0.3")
   (pobieraj-udp)))
(start-listening *listener*)

;(start-serwer 9000)
