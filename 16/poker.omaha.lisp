;;--------------------------------------------------------------------------
;;------------------------------- Omaha Predicates--------------------------
;;--------------------------------------------------------------------------


;;testy czy mam nutsa?:
(defun omaha-color-nuts?(&optional (cards +cards+) (table-cards +table-cards+))
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
	  (member (+ 3 (* 13 col)) cards)) ))  ))

(defun omaha-str8-nuts?(&optional (reka +cards+) (stol +table-cards+))
  (let ((figury-stolu  (mapcar #'figura stol))
	(rank (omaha-ocena reka stol)))
    (and (= 4 (car rank))
	 (<=
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

(defun omaha-full-nuts?(&optional (cards +cards+) (table-cards +table-cards+))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (CH1 (nth 0 reka)) (CH2 (nth 1 reka))
	 (CH3 (nth 2 reka)) (CH4 (nth 3 reka))
	 (CT1 (nth 0 stol)) (CT2 (nth 1 stol)) (CT3 (nth 2 stol)))
    (or (= CH1 CH2 CT1) (= CH2 CH3 CT1) (= CH3 CH4 CT1) 
	(and (= CH1 CT1 CT2) (= CH2 CT3) )
	(and (= CH1 CT1 CT2) (= CH3 CT3) )
	(and (= CH1 CT1 CT2) (= CH4 CT3) )

	(and (= CH2 CT1 CT2) (= CH1 CT3) )
	(and (= CH2 CT1 CT2) (= CH3 CT3) )
	(and (= CH2 CT1 CT2) (= CH4 CT3) )

	(and (= CH3 CT1 CT2) (= CH2 CT3) )
	(and (= CH3 CT1 CT2) (= CH1 CT3) )
	(and (= CH3 CT1 CT2) (= CH4 CT3) )

	(and (= CH4 CT1 CT2) (= CH2 CT3) )
	(and (= CH4 CT1 CT2) (= CH3 CT3) )
	(and (= CH4 CT1 CT2) (= CH1 CT3) ) )))

;czy moze byc street u kogos?
(defun omaha-str8-table?(&optional (table +table-cards+))
  (let ((figury-stolu  (mapcar #'figura table)))
    (some
     #'(lambda (para)
	 (poker.ranks::street (wektor-figur 
			       (mapcar #'1+ 
				       (append para figury-stolu)))))
  (podzbiory-2  
   (set-difference 
    '(0 1 2 3 4 5 6 7 8 9 10 11 12)
    figury-stolu)) )))


(defun omaha-set?(&optional (cards +cards+) (table-cards +table-cards+))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (H1 (nth 0 reka)) (H2 (nth 1 reka))
	 (H3 (nth 2 reka)) (H4 (nth 3 reka))
	 (T1 (nth 0 stol)) (T2 (nth 1 stol)) (T3 (nth 2 stol))
	 (T4 (nth 2 stol)) (T5 (nth 2 stol)))
  (and ;mamy pare na reku:
   (or (= h1 h2) (= h2 h3) (= h3 h4))
   (or (= h1 h2 t1) (= h1 h2 t2) (= h1 h2 t3)
       (= h3 h2 t1) (= h3 h2 t2) (= h3 h2 t3)
       (= h3 h4 t1) (= h3 h4 t2) (= h3 h4 t3)
       (when t4
	 (or  (= h1 h2 t4)  (= h3 h2 t4)  (= h3 h4 t4)))
       (when t5
	 (or  (= h1 h2 t5) (= h3 h2 t5)  (= h3 h4 t5)  )))))  )


(defun omaha-overpair?(&optional (cards +cards+) (table-cards +table-cards+))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (H1 (nth 0 reka)) (H2 (nth 1 reka))
	 (H3 (nth 2 reka)) (H4 (nth 3 reka))
	 (T1 (nth 0 stol))) 
  (or
   (and (= h1 h2) (< h1 t1))
   (and (= h2 h3) (< h2 t1))
   (and (= h3 h4) (< h3 t1)) )));mamy na reku pare ktora jest wyzsza od wszystkich kart na stole, np. AA, KK



(defun omaha-top-set?(&optional (cards +cards+) (table-cards +table-cards+))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (H1 (nth 0 reka)) (H2 (nth 1 reka))
	 (H3 (nth 2 reka)) (H4 (nth 3 reka))
	 (T1 (nth 0 stol)) )
  (or (= h1 h2 t1) (= h2 h3 t1) (= h3 h4 t1))))


(defun omaha-top-two-pairs?(&optional (cards +cards+) (table-cards +table-cards+))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (H1 (nth 0 reka)) (H2 (nth 1 reka))
	 (H3 (nth 2 reka)) (H4 (nth 3 reka))
	 (T1 (nth 0 stol)) (T2 (nth 1 stol)))
  (or (and (= h1 t1) (or (= h2 t2) (= h3 t2) (= h4 t2) ))
      (and (= h2 t1) (or (= h3 t2) (= h4 t2)))
      (and (= h3 t1) (= h4 t2))  )))



(defun omaha-count-outs(&optional (level 3) (cards +cards+) (table-cards +table-cards+))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(cond
            ;jesli para to czy kupimy nutsowego fulla:
	  ;LEVEL 1
	  ((and (> level 0)(pair-table? (wektor-figur table)))
	   (when (omaha-full-nuts? cards table) (incf nr)))
	    ;jesli kolor na stole to czy nutsowy kolor lub full:
	  ;LEVEL 2
	  ((and (> level 1)(color-table? (wektor-kolorow table)))
	   (when (or (omaha-color-nuts? cards table) 
		     ;fullhouse:
		     (= 2 (car (omaha-ocena cards table)))  )
	     (incf nr)))
	  ;LEVEL 3
	  ((and (> level 2) (omaha-str8-table? table))
	   (when (or (omaha-str8-nuts? cards table)
		     (omaha-color-nuts? cards table)
		     ;fullhouse:
		     (= 2 (car (omaha-ocena cards table))))
	     (incf nr)))  	  )))))

