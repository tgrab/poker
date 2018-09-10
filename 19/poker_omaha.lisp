(in-package :poker)
;;--------------------------------------------------------------------------
;;------------------------------- Omaha Predicates--------------------------
;;--------------------------------------------------------------------------


;zle dla A 2 3 5 - 4 5
(defun omaha-str8-nuts?(&optional (reka (cards +game+)) (stol (table-cards +game+)))
  (let ((figury-stolu  (mapcar #'figura stol))
	(rank (omaha-ocena reka stol)))
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
(defun omaha-color-nuts?(&optional  (cards (cards +game+)) (table-cards (table-cards +game+)))
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


(defun omaha-full-nuts?(&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let* ((stol  (sort (mapcar #'figura table-cards) #'<) )
	 (reka  (sort (mapcar #'figura cards) #'<) )
	 (CH1 (nth 0 reka)) (CH2 (nth 1 reka))
	 (CH3 (nth 2 reka)) (CH4 (nth 3 reka))
	 (CT1 (nth 0 stol)) (CT2 (nth 1 stol)) (CT3 (nth 2 stol))
	 (o (car (omaha-ocena cards table-cards))))
    (when
	(<= o 2) ;kareta?
	(or 
	 (= o 1)
	 (= CH1 CH2 CT1) 
	 (= CH2 CH3 CT1) 
	 (= CH3 CH4 CT1) 
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
	 (and (= CH4 CT1 CT2) (= CH1 CT3) ) ))))

(defmacro omaha-set?()
  '(and 
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
     (or  (= h1 h2 t5) (= h3 h2 t5)  (= h3 h4 t5)  ))))  

(defmacro omaha-top-two-pairs?()
  '(or 
    (and (= h1 t1) (or (= h2 t2) (= h3 t2) (= h4 t2) ))
    (and (= h2 t1) (or (= h3 t2) (= h4 t2)))
    (and (= h3 t1) (= h4 t2))  ))


(defmacro omaha-overpair?()
  '(or
    (and (= h1 h2) (< h1 t1))
    (and (= h2 h3) (< h2 t1))
    (and (= h3 h4) (< h3 t1)) ));mamy na reku pare ktora jest wyzsza od wszystkich kart na stole, np. AA, KK


(defmacro omaha-top-set?()
   '(or (= h1 h2 t1) (= h2 h3 t1) (= h3 h4 t1)))

(defmacro omaha-top-trips?()
  '(and (= t1 t2)
       (or (= h1 t1) (= h2 t1) (= h3 t1) (= h4 t1))))

;; temp
(defun brak-koloru-w-ukladzie(karty)
  (< (apply 'max (coerce (wektor-kolorow karty) 'list)) 3))

(defun brak-pary-w-ukladzie(karty)
  (< (apply 'max (coerce (wektor-figur karty) 'list)) 2))

(defun omaha-count-color-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(when 
		    (and (brak-pary-w-ukladzie table)
			 (omaha-color-nuts? cards table))
		  (incf nr))))))

(defun omaha-count-not-nut-color-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(when 
		 (= 3 (car (omaha-ocena cards table)))
		  (incf nr))))))



(defun omaha-count-str8-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(when 
	    (and (brak-koloru-w-ukladzie table) ;nie ma koloru ani pary na stole
		 (brak-pary-w-ukladzie table)
		 (omaha-str8-nuts? cards table)) 
	  (incf nr))))))  

(defun omaha-count-not-nut-str8-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(when 
	   (= 4 (car (omaha-ocena cards table))) 
	  (incf nr)))))) 

(defun omaha-count-full-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(when (omaha-full-nuts? cards table) (incf nr))))))


(defun omaha-count-not-nut-full-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(when (<= (car (omaha-ocena cards table)) 2) (incf nr))))))

(defun omaha-count-outs()
  (if (table-cards +game+)
      (let ((outs 0))
	(incf outs (omaha-count-full-outs))
	(when (?- (table-safe almost))
	   (incf outs (omaha-count-str8-outs)))
	(when (?- (table-pairs no)) (incf outs (omaha-count-color-outs)))
	outs)
      0))

(defun omaha-outs-logic()
  (when (table-cards +game+)
    (let* ((o1 (omaha-count-outs))
	  (odds (odds o1)))
      (when (plusp o1)
 	(cond  
	  ((> (pot-odds) odds)
		  (<-into (percept-memo +game+)  (odds good)))
	  ((> (* 1.9 (pot-odds)) odds)
	   (<-into (percept-memo +game+)  (odds implied)))
	  ((>= (* 3 (pot-odds)) odds)
	   (<-into (percept-memo +game+)  (odds strong-implied))) ))  )    ))


(defun omaha-round-logic()

 ;(add-clause (round-memo +game+) `((hand ,(pojedynek-post-flop)))) 
  
    (if (omaha-top-set?) 
	(<-into (round-memo +game+) (rank top-set))
	(when (omaha-set?)
	  (<-into (round-memo +game+) (rank set))) )


    (when (omaha-top-trips?) 
      (<-into (round-memo +game+) (rank top-trips)))

    (when (omaha-top-two-pairs?) 
      (<-into (round-memo +game+) (rank top-two-pairs)))

    (when (or (= h1 t1) (= h2 t1) (= h3 t1) (= h4 t1) )
      (<-into (round-memo +game+) (rank top-pair)))

    (when (omaha-overpair?) 
      (<-into (round-memo +game+) (rank overpair)))

    (when (< h1 t1)
      (if (= 0 h1)
	  (progn
	    (<-into (round-memo +game+) (rank kicker))
	    (<-into (round-memo +game+) (rank ace-kicker))) 
	  (<-into (round-memo +game+) (rank kicker))))

    (cond
      ((?- (rank color)) 
       (if  (omaha-color-nuts?)
	    (<-into (round-memo +game+) (rank color nuts))
	    (<-into (round-memo +game+) (rank color not-nuts))))
      ((?- (rank str8)) 
       (if  (omaha-str8-nuts?)
	    (<-into (round-memo +game+) (rank str8 nuts))
	    (<-into (round-memo +game+) (rank str8 not-nuts))))
      ((?- (rank fullhouse)) 
       (if  (omaha-full-nuts?)
	    (<-into (round-memo +game+) (rank fullhouse nuts))
	    (<-into (round-memo +game+) (rank fullhouse not-nuts))))     	    )

					;testujemy nutsa
    (cond
      ((?- (table-pairs yes))
       (if (omaha-full-nuts?)
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))
      ((?- (table-colors yes))
       (if (omaha-color-nuts?)
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))
      ((?- (table-str8 yes))
       (if (omaha-str8-nuts?)
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))
      (t 
       (if (omaha-top-set?)
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))      )

    (when (or (flop?)(turn?))
      (let* ((o1 (omaha-count-outs))
	     (o2 (omaha-count-not-nut-full-outs))
	     (o3 (omaha-count-not-nut-color-outs))
	     (o4 (omaha-count-not-nut-str8-outs)))
	(when (plusp o1)
	  (add-clause (round-memo +game+) `((outs ,o1))))   
	(when (plusp o2)
	  (add-clause (round-memo +game+) `((outs fullhouse ,o2))))      
	(when (plusp o3)
	  (add-clause (round-memo +game+) `((outs color ,o3))))
	(when (plusp o4)
	  (add-clause (round-memo +game+) `((outs str8 ,o4)))) ))	  
    )


(defun omaha-action-prove()
  (or (when (preflop?)	  
	(or
	 (rozwin-regule (omaha-preflop-call ?rule) "call")
	 (rozwin-fuzzy-regule (omaha-preflop-fuzzy-call ?level ?rule) "fuzzy-call")
	 "fold no-rule"))
	 ;omaha-postflop
      (when (flop?)		  
	(or
	 
	 (rozwin-regule (omaha-flop-potbet1 ?rule) "potbet1")
	 (rozwin-regule (omaha-potbet1 ?rule) "potbet1")
	 (rozwin-regule (omaha-flop-potbet2 ?rule) "potbet2")
	 (rozwin-fuzzy-regule (omaha-flop-fuzzy-potbet1 ?level ?rule) "fuzzy-potbet1")
	 (rozwin-regule (omaha-flop-call ?rule) "call")
	 (rozwin-regule (omaha-call ?rule) "call")
	 (rozwin-fuzzy-regule (omaha-flop-fuzzy-call ?level ?rule) "fuzzy-call"))   )
					;omaha turn
      (when (turn?)		  
	(or
	 (rozwin-regule (omaha-turn-potbet1 ?rule) "potbet1")
	 (rozwin-regule (omaha-potbet1 ?rule) "potbet1")
	 (rozwin-fuzzy-regule (omaha-turn-fuzzy-potbet1 ?level ?rule) "fuzzy-potbet1")
	 (rozwin-regule (omaha-turn-call ?rule) "call")
	 (rozwin-regule (omaha-call ?rule) "call")
	 (rozwin-fuzzy-regule (omaha-turn-fuzzy-call ?level ?rule) "fuzzy-call"))   )
					;omaha river
      (when (river?)		  
	(or
	 (rozwin-regule (omaha-river-potbet1 ?rule) "potbet1")
	 (rozwin-regule (omaha-potbet1 ?rule) "potbet1")
	 (rozwin-fuzzy-regule (omaha-river-fuzzy-potbet1 ?level ?rule) "fuzzy-potbet1")
	 (rozwin-regule (omaha-call ?rule) "call")
	 (rozwin-regule (omaha-river-call ?rule) "call")
	 (rozwin-fuzzy-regule (omaha-river-fuzzy-call ?level ?rule) "fuzzy-call"))   )
      "fold no-rule"))
