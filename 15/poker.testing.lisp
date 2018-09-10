(defun hs2(l)
  (mapcar #'hs (podzbiory-2 l)))

(defun hs->rank(h)
  (apply #'+ (mapcar #'(lambda (r) (if r r 11))  h)))

(defun kopiuj-tablice()
  (maphash
   (lambda (k v)
     (unless (or
	      (gethash k *h2*)
	      (gethash (mapcar (lambda(c) (inc-color c 1)) k) *h2*)
	      (gethash (mapcar (lambda(c) (inc-color c 2)) k) *h2*)
	      (gethash (mapcar (lambda(c) (inc-color c 3)) k) *h2*) )
       (setf (gethash k *h2*) -1) ))
   *h*))q

(def-url-0 "/test1"
    (main-page
      (dotimes (i 100)
	(let* ((a (1+ (random 52)))
	      (b (1+ (random 52)))
	      (c (1+ (random 52)))
	      (d (1+ (random 52)))
	      (l (list a b c d)))
	 (when (/= a b c d)
	   (html
	   ; (rank-update a b c d)
	    (:princ (karty->html l)) 
	    (:princ (mapcar #'karty->html (gen-same-hand l))) 
	   ; (:princ (hs l)) " "
	     :hr))))))

(defun inc-color(karta &optional (step 1))
  (+ 1 (karta->figura karta)  (* 13 (mod (+ step (karta->kolor karta)) 4)) )   )

(defun same-sets(set1 set2)
  (every #'(lambda (c) (find c set2)) set1))

(defun same-hand(set1 set2)
  (or (same-sets set1 set2)
      (same-sets set1 (mapcar (lambda(c) (inc-color c 1)) set2))
      (same-sets set1 (mapcar (lambda(c) (inc-color c 2)) set2)) 
      (same-sets set1 (mapcar (lambda(c) (inc-color c 3)) set2)) ))

(defun gen-same-hand(set )
  (list
    set
   (mapcar (lambda(c) (inc-color c 1)) set)
   (mapcar (lambda(c) (inc-color c 2)) set) 
   (mapcar (lambda(c) (inc-color c 3)) set) ))

(defun znajdz-wartosc(karty)
  (let ((k (sort (copy-list karty) #'<)))
     (or
      (gethash k *h2*)
      (gethash (mapcar (lambda(c) (inc-color c 1)) k) *h2*)
      (gethash (mapcar (lambda(c) (inc-color c 2)) k) *h2*)
      (gethash (mapcar (lambda(c) (inc-color c 3)) k) *h2*) )))

(defun rank-update(c1 c2 c3 c4)
 "Tworzy formularz do aktualizacji rankingu kart w omahe"
 (html
   (:princ (karty->html (list c1 c2 c3 c4)))
   ((:form :action (format nil "/handrank?c1=~A&c2=~A&c3=~A&c4=~A" c1 c2 c3 c4)) 
    ((:input :name :rank))
    ((:input :type :submit :value "Send")) )
  ))

(def-url-0 "/rankupdate"
    (main-page (rank-update 1 2 3 4)))

(def-url-0 "/handrank"
    (html "ok"))


