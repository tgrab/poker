; game 1
(defun t1()
(setq +game+ (make-game :kind 3))
(percept-small-blind +game+ "ematic" 1)
(percept-big-blind +game+ "alexandra" 2)
(percept-fold +game+ "sennans")
(percept-call +game+ "bob" 2)
(percept-fold +game+ "tommy")
(percept-raise +game+ "rooster" 4)

(percept-call +game+ "ematic" 3)
(percept-call +game+ "alexandra" 2)
(percept-call +game+ "bob" 2)

(percept-flop +game+ 29 6 8)

(percept-check +game+ "ematic")
(percept-check +game+ "alexandra")
(percept-bet +game+ "bob" 16)
(percept-call +game+ "rooster" 16)
(percept-fold +game+ "ematic")
(percept-call +game+ "alexandra" 16)

(percept-turn +game+ 37)

(percept-check +game+ "alexandra")
(percept-bet +game+ "bob" 64)
(percept-allin +game+ "rooster" 31.02)
(percept-fold +game+ "alexandra")

(percept-river +game+ 12))


(defvar *h*  (make-hash-table :test #'equal) "Baza wynikow pojedynkow")

(defun wczytaj-baze(plik)
 ;inicjacja bazy:
    ;(dolist (reka (poker.ranks::podzbiory-2 (poker.ranks::talia)))
     ;     (setf (gethash reka *h*) (list 0 0 0)))
     (with-open-file (f plik)
      (do
           ( (klucz (read f nil) (read f nil)) 
             (wartosc (read f nil)(read f nil)))
           ( (null klucz) )
           (setf (gethash klucz *h*) wartosc)  )    ))

(def-url-0 "/test"
    (main-page
      
      (maphash 
       #'(lambda (k v) (html (:princ k)))
       *h*)))

(defun oddsy(l)
  (if (plusp (first l)) 
      (/ (third l) (+ (first l) (* 0.1 (second l))))
      1000))

(defun h2list()
  (let ((w nil))
    (maphash 
     #'(lambda (k v)  (push (list k (oddsy v)) w))
     *h*)    
    w))

(defun zapisz-liste()
  (with-open-file  (f "wynik.html" :direction :output :if-does-not-exist :create :if-exists :supersede)
    (maphash 
     #'(lambda (k v)  (format f "~A ~A <br>" (karty->html k) (oddsy v)))
     *h*) ))

(defun zapisz-liste2(l)
  (with-open-file  (f "wyniks.html" :direction :output :if-does-not-exist :create :if-exists :supersede)
    (dotimes (i (length l))
      (format f "~A ~A ~A <br>" i  (karty->html (car (nth i l))) (cadr (nth i l))))  ))