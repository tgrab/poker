(defpackage :poker-www
  (:use :cl :net.aserve :net.html.generator :poker))

(in-package :poker-www)

;;--------------------------------------------------------------------------
(defmacro wyslij(body &optional (tytul "") )
 `#'(lambda (req ent)
       (labels ((parameter (name)  
                         (or
			  (cdr (assoc name (request-query req) :test #'equal))
                          (cdr 
                           (assoc name 
		             (form-urlencoded-to-query
			      (get-request-body req)) :test #'equal))) ))
     (with-http-response(req ent)
      (with-http-body(req ent)
      
       (html :newline 
        (:html :newline     
         (:head  
	     (:title ,tytul) :newline
             ((:meta :http-equiv "content-type" :content "text/html; charset=ISO-8859-2")) :newline
	;;;     ((:link :href "styl.css" :rel "stylesheet" :type "text/css"))
	        (:style
		 "body {background: #ffe29c; font-family: Verdana, Arial, Helvetica; font-size: 12px}"
		 "table {background-color : #7ba1d5; font-family: sans-serif; font-size: 9px}"
		 "table.menu {background-color :green}"
		))   :newline
   	   (:body :newline ,@body :newline)))     )))))



;; --------- SYMULATOR -------------------------------------------------
;; ---------------------------------------------------------------------
(setq reka nil stol nil karty (poker:talia) zwyciestwa 0 remisy 0 porazki 0 stol nil sklansky 0)

(defun reka(k)
        (push k reka)
        (when (= 2 (length reka))    
            (setq sklansky (sklansky-group (l->karta (first reka))
                                          (l->karta (second reka)))) ))
(defun stol(k)
        (push k stol)
        (when (>= (length stol) 3) (ocena-stolu))
        (when (= 5 (length stol))
                    ;(setq zwyciestwa 0 sklansky 0)
                    (setq karty (poker:talia))))

(defun ocena-stolu()
     (destructuring-bind (z r p)
        (case (length stol)
           (3 (ocena-flop2 reka stol))
           (4 (ocena-turn reka stol))
           (5 (ocena-river reka stol)))
        (setq zwyciestwa z remisy r porazki p)   ))

(defun prawdopodobienstwo() 
           (if (plusp zwyciestwa) 
                     (coerce (/ zwyciestwa (+ zwyciestwa porazki)) 'float   )
                     0)) 

(defmacro karty-main()
     '(html
         ((:a :href "karty.main") "[MAIN]") ((:a :href "karty.opcje") "[OPCJE]")
         :hr
         (pokaz-reke) (pokaz-stol)
         :hr
         (pokaz-karty)
       ))


(defun karty-dodaj( k )
   (setf karty (remove k karty))
   (cond 
     ((< (length reka) 2) (reka k)) ;byla jedna karta na rece to ja jeszcze dodamy
     ((< (length stol) 5) (stol k)) ;stol nie zapelniony to dodajemy do niego
     (t (progn (setq reka nil stol nil) (reka k))) ;zaczynamy od poczatku
     ))


;; --------------------------------------------------------------------------
   


(defun obraz-karty(karta)
  (let ((kolorki '("s" "h" "d" "c"))
        (figurki '("a" "k" "q" "j" "t" 9 8 7 6 5 4 3 2)))
     (format nil "<img src=/decks/~A~A.gif >" 
          (nth (l->figura karta) figurki)
	  (nth (l->kolor karta) kolorki))))

(defmacro pokaz-stol()
  `(html
     (dolist (k stol ) (html  
                           (:princ (obraz-karty  k)))) 
     :hr :newline   ))

(defmacro pokaz-reke()
  `(html
     (:table
      :tr :td "Sklansky:" :td (:princ sklansky)
      :tr :td "Prawdopodobienstwo:" :td (:princ (prawdopodobienstwo)) )
     (dolist (k reka ) (html  
                           (:princ (obraz-karty  k)))) 
     :hr :newline   ))

(defmacro pokaz-karty()
  `(html
    (dolist (k karty ) (html  
                           ((:a :href 
                              (format nil "/karty.dodaj?karta=~A" k))
                              (:princ (obraz-karty  k)))) )
     :newline   ))

(defun publikuj()
  (publish-directory :prefix "/decks/" :destination "/raid/code/lisp/karty/decks/")
  (publish :path "/karty.main" :function (wyslij ((karty-main))))
  (publish :path "/karty.dodaj" 
           :function (wyslij 
                      ((let ((karta (parse-integer (parameter "karta"))))
                         (karty-dodaj karta))
                       (karty-main))))

  (publish :path "/karty.sklansky" 
           :function (wyslij 
                      ((let ((r1 (parse-integer (parameter "r1")))
                             (r2 (parse-integer (parameter "r2"))))
                         (karty-dodaj r1)(karty-dodaj r2))
                         `(html
           (:table
            :tr :td "Sklansky:" :td (:princ sklansky)
            :tr :td "Prawdopodobienstwo:" :td (:princ (prawdopodobienstwo))
           (setq reka nil) ))))))
