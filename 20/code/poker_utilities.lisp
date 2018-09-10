(in-package :poker)


(defun safe-division(e d)
  (if (zerop d)
      1000
      (float (/ e d))))


(defun odds (outs &optional (g *game*))
  (if (omaha? g)
      (safe-division (- 48 (length (table-cards g)) outs) outs)
      (safe-division (- 50 (length (table-cards g)) outs) outs)))       


(defun pot-odds(&optional (g *game*))
  (safe-division (pot g) (agent-stake g)))

(defun profit(plr)
  (+ (balance plr) (total-balance plr) (gain plr)))

;-----------------------------------------------------------------------

(defun omaha?(&optional (g *game*))
  (= 3 (kind g)))

(defun fixed?(&optional (g *game*))
  (= 2 (kind g)))

(defun holdem?(&optional (g *game*))
  (<= (kind g) 2))

(defun preflop?(&optional (g *game*))
  (null (table-cards g)))

(defun flop?(&optional (g *game*))
  (= 3 (length (table-cards g))))

(defun turn?(&optional (g *game*))
  (= 4 (length (table-cards g))))

(defun river?(&optional (g *game*))
  (= 5 (length (table-cards g))))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------


(defun two-str8-table?(&optional (g *game*))
  (when (table-cards g)
    (let ((figury-stolu  (mapcar #'figura (table-cards g))))
      (some
       #'(lambda (para)
	   (poker.ranks::street (wektor-figur 
			       (mapcar #'1+ 
				       (append para figury-stolu)))))
       (podzbiory-2  
	(set-difference 
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12)
	 figury-stolu)) ))))

(defun one-str8-table?(&optional (g *game*))
  (let ((figury-stolu  (mapcar #'figura (table-cards g))))
    (when (>= (length figury-stolu) 4)
      (some
       #'(lambda (karta)
	   (poker.ranks::street (wektor-figur 
				 (mapcar #'1+ 
					 (cons karta figury-stolu)))))
       (set-difference 
	'(0 1 2 3 4 5 6 7 8 9 10 11 12)
	figury-stolu)) )))


;;--------------------------------------------------------------------------
;;------------------------------- Omaha Predicates--------------------------
;;--------------------------------------------------------------------------


;zle dla A 2 3 5 - 4 5
(defun omaha-str8-nuts?(&optional (g *game*))
  (let* ((stol (table-cards g))
	 (figury-stolu  (mapcar #'figura stol))
	 (rank (rank g)))
    (and (= 4 (car rank))
	 (<=
	  ;(1+ (second rank))
	  (second rank)
	  (apply #'min
		 (mapcar
		  #'(lambda (para)
		      (let  ((str (poker.ranks::street 
				   (wektor-figur (mapcar #'1+ (append para figury-stolu))))))
			    (if str str 100)))
		      (podzbiory-2  
		       (set-difference 
			'(0 1 2 3 4 5 6 7 8 9 10 11 12)
			figury-stolu)) ))))))

;;testy czy mam nutsa?:
(defun omaha-color-nuts?(&optional  (g *game*) )
  (with-slots (cards table-cards) g
	      (let (col)
		(when (some 
		       #'(lambda (uklad) 
			   (and (apply #'= (mapcar #'kolor uklad))
				(setq col (kolor (first uklad)))))
		       (poker.ranks::omaha-zestawy cards table-cards))
		  (or(member (+ 1 (* 13 col)) cards)   ;czy mam asa na reku
		     (and (member (+ 1 (* 13 col)) table-cards)  ;czy mam krola na reku
			  (member (+ 2 (* 13 col)) cards))
		     (and (member (+ 1 (* 13 col)) table-cards)  ;czy mam dame na reku
			  (member (+ 2 (* 13 col)) table-cards)
			  (member (+ 3 (* 13 col)) cards)) ))  )))

(defun omaha-full-nuts?(&optional (cards (cards *game*)) (table-cards (table-cards *game*)))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (H1 (nth 0 reka)) (H2 (nth 1 reka))
	 (H3 (nth 2 reka)) (H4 (nth 3 reka))
	 (T1 (nth 0 stol)) (T2 (nth 1 stol)) (T3 (nth 2 stol))
	 (o (car (omaha-ocena cards table-cards))))
    (when
	(<= o 2) ;kareta?
      (or 
       (= o 1)
       (= H1 H2 T1) 
       (= H2 H3 T1) 
       (= H3 H4 T1) 
       (and (= H1 T1 T2) (= H2 T3) )
       (and (= H1 T1 T2) (= H3 T3) )
       (and (= H1 T1 T2) (= H4 T3) )
       
       (and (= H2 T1 T2) (= H1 T3) )
       (and (= H2 T1 T2) (= H3 T3) )
       (and (= H2 T1 T2) (= H4 T3) )

       (and (= H3 T1 T2) (= H2 T3) )
       (and (= H3 T1 T2) (= H1 T3) )
       (and (= H3 T1 T2) (= H4 T3) )

       (and (= H4 T1 T2) (= H2 T3) )
       (and (= H4 T1 T2) (= H3 T3) )
       (and (= H4 T1 T2) (= H1 T3) ) )))) 


(defun omaha-set?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 t2 t3 t4 t5) g
  (and 
    ;mamy pare na reku:
    (or (= h1 h2) (= h2 h3) (= h3 h4))
    ;zatem:
    (or 
     (= h1 h2 t1) 
     (= h1 h2 t2) 
     (= h1 h2 t3)
     (= h3 h2 t1)
     (= h3 h2 t2)
     (= h3 h2 t3)
     (= h3 h4 t1)
     (= h3 h4 t2)
     (= h3 h4 t3)
     (or  (= h1 h2 t4)  (= h3 h2 t4)  (= h3 h4 t4))
     (or  (= h1 h2 t5) (= h3 h2 t5)  (= h3 h4 t5)  ))))) 

(defun omaha-top-two-pairs?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 t2) g
  (or 
    (and (= h1 t1) (or (= h2 t2) (= h3 t2) (= h4 t2) ))
    (and (= h2 t1) (or (= h3 t2) (= h4 t2)))
    (and (= h3 t1) (= h4 t2))  )))

(defun omaha-overpair?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1) g
    (or
     (and (= h1 h2) (< h1 t1))
     (and (= h2 h3) (< h2 t1))
     (and (= h3 h4) (< h3 t1)) )));mamy na reku pare ktora jest wyzsza od wszystkich kart na stole, np. AA, KK 


(defun omaha-top-set?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 ) g
   (or (= h1 h2 t1) (= h2 h3 t1) (= h3 h4 t1))))

(defun omaha-top-trips?(&optional (g *game*))
  (with-slots (h1 h2 h3 h4 t1 t2) g
    (and (= t1 t2)
	 (or (= h1 t1) (= h2 t1) (= h3 t1) (= h4 t1)))))
 

;; temp
(defun brak-koloru-w-ukladzie(karty)
  (< (apply 'max (coerce (wektor-kolorow karty) 'list)) 3))

(defun brak-pary-w-ukladzie(karty)
  (< (apply 'max (coerce (wektor-figur karty) 'list)) 2))

(defun omaha-count-color-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (and (brak-pary-w-ukladzie table)
		   (omaha-color-nuts? g))
	    (incf nr)))))))

(defun omaha-count-not-nut-color-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (= 3 (car (omaha-ocena cards table)))
	    (incf nr)))))))


(defun omaha-count-str8-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (and (brak-koloru-w-ukladzie table) ;nie ma koloru ani pary na stole
		   (brak-pary-w-ukladzie table)
		   (omaha-str8-nuts? g)) 
	    (incf nr)))))))


(defun omaha-count-not-nut-str8-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when 
	      (= 4 (car (omaha-ocena cards table))) 
	    (incf nr)))))))      


(defun omaha-count-full-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when (omaha-full-nuts? cards table) (incf nr)))))))


(defun omaha-count-not-nut-full-outs (&optional  (g *game*) )
  (with-slots (cards table-cards) g
    (let ((nr 0))
      (dolist (c (set-difference (talia) (append cards table-cards)) nr)
	(let ((table (cons c table-cards)))
	  (when (<= (car (omaha-ocena cards table)) 2) (incf nr)))))))


(defun omaha-count-outs(&optional  (g *game*) )
  (if (table-cards g)
      (let ((outs 0))
	(incf outs (omaha-count-full-outs g))
	(when (and (< (ilosc-kolorow g) 3) (< (ilosc-figur g) 2))
	   (incf outs (omaha-count-str8-outs g)))
	(when (< (ilosc-figur g) 2)  (incf outs (omaha-count-color-outs g)))
	outs)
      0))