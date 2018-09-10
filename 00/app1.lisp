(defpackage :texas
  (:use :cl :www :net.html.generator :moje))

(in-package :texas)

;Kod opisujacy jakie miejsca sa zajete przy stole
(let ((miejsca (make-array 10 :initial-element t)) 
      (biezacy-gracz 0) (wielkosc-stolu 10))
  (defun inicjuj-stol(wielkosc) (setq wielkosc-stolu wielkosc)
	                        (setq miejsca (make-array wielkosc :initial-element t))
                                (setq biezacy-gracz 0))
  (defun gracz () biezacy-gracz)
  (defun miejsca ()  miejsca)
  (defun wielkosc-stolu () wielkosc-stolu)
  (defun nastepny(gracz) 
    (let ((gracz+1 (1+ gracz)))
      (if (= (1- wielkosc-stolu) gracz) 
               (nastepny -1)
               (if (aref miejsca gracz+1)
                      gracz+1
                     (nastepny gracz+1)))))

  (defun wstan(gracz)
    (setf (aref miejsca gracz) nil))
  (defun usiadz(gracz)
    (setf (aref miejsca gracz) t)) )

;; ----------------------------------------
;; ------        html:
;; ----------------------------------------
(defmacro opis-miejsca(numer)
 `(html
  :td (:princ ,numer)

  ))

(defmacro stol()
 `(html
   ((:table :border "1") :tr (dotimes (i (wielkosc-stolu)) (opis-miejsca i)))

  ))

(defmacro main-form()
 `(html
   "Texas Hold'em"
   (stol)

 ))

(wyslij "/poker"
  ( (main-form) ))