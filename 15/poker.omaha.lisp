;;--------------------------------------------------------------------------
;;------------------------------- Game Predicates---------------------------
;;--------------------------------------------------------------------------

(defun big-bet?()
  (< (pot-odds) 2.5))

(defun zero-stake?()
 ; no bets before or called prev. bets
  (zerop (stake)))


;;--------------------------------------------------------------------------
;;------------------------------- Omaha Predicates--------------------------
;;--------------------------------------------------------------------------


;;testy czy mam nutsa?:
(defun omaha-color-nuts?(&optional (cards +cards+) (table-cards +table-cards+))
 (let (col)
  (when (some 
         #'(lambda (uklad) 
	   (and (apply #'= (mapcar #'karta->kolor uklad))
		(setq col (karta->kolor (first uklad)))))
	   (poker.ranks::omaha-zestawy cards table-cards))
  (or(member (+ 1 (* 13 col)) cards)   ;czy mam asa na reku
     (and (member (+ 1 (* 13 col)) table-cards)  ;czy mam krola na reku
	  (member (+ 2 (* 13 col)) cards))
     (and (member (+ 1 (* 13 col)) table-cards)  ;czy mam dame na reku
	  (member (+ 2 (* 13 col)) table-cards)
	  (member (+ 3 (* 13 col)) cards)) ))  ))

(defun omaha-str8-nuts?(&optional (reka +cards+) (stol +table-cards+))
  (let ((figury-stolu  (mapcar #'karta->figura stol))
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
  (let* ((stol  (sort (mapcar #'karta->figura table-cards) #'<) )
	 (reka  (sort (mapcar #'karta->figura cards) #'<) )
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
  (let ((figury-stolu  (mapcar #'karta->figura table)))
    (some
     #'(lambda (para)
	 (poker.ranks::street (wektor-figur 
			       (mapcar #'1+ 
				       (append para figury-stolu)))))
  (podzbiory-2  
   (set-difference 
    '(0 1 2 3 4 5 6 7 8 9 10 11 12)
    figury-stolu)) )))

(defun omaha-overpair?()
  (or
   (and (= h1 h2) (< h1 t1))
   (and (= h2 h3) (< h2 t1))
   (and (= h3 h4) (< h3 t1)) ));mamy na reku pare ktora jest wyzsza od wszystkich kart na stole, np. AA, KK

(defun omaha-set?()
  (and ;mamy pare na reku:
   (or (= h1 h2) (= h2 h3) (= h3 h4))
   (or (= h1 h2 t1) (= h1 h2 t2) (= h1 h2 t3)
       (= h3 h2 t1) (= h3 h2 t2) (= h3 h2 t3)
       (= h3 h4 t1) (= h3 h4 t2) (= h3 h4 t3)
       (when t4
	 (or  (= h1 h2 t4)  (= h3 h2 t4)  (= h3 h4 t4)))
       (when t5
	 (or  (= h1 h2 t5) (= h3 h2 t5)  (= h3 h4 t5)  )))))

(defun omaha-top-set?()
  (or (= h1 h2 t1) (= h2 h3 t1) (= h3 h4 t1)))

(defun omaha-trips!()
  (cond
    ((omaha-top-set?) "potbet2 top set")
    (t "call with trips")     ))


(defun omaha-top-two-pairs?()
  (or (and (= h1 t1) (or (= h2 t2) (= h3 t2) (= h4 t2) ))
      (and (= h2 t1) (or (= h3 t2) (= h4 t2)))
      (and (= h3 t1) (= h4 t2))  ))



(defun count-outs(&optional (level 3) (cards +cards+) (table-cards +table-cards+))
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

;;--------------------------------------------------------------------------
;;------------------------------- Omaha Logic-------------------------------
;;--------------------------------------------------------------------------

(setq +game-kind+ 3)

(defun wczytaj-baze(plik)
   (with-open-file (f plik)
      (do
           ( (klucz (read f nil) (read f nil)) 
             (wartosc (read f nil)(read f nil)))
           ( (null klucz) )
           (setf (gethash klucz *h*) wartosc)  )    ))
;#-(or baza)
;(progn
;  (pushnew :baza *features*)
;  (wczytaj-baze "../logs/omaha10.dat"))


;gramy to co ma oddsy<7.6 jest to 1/5 zbioru

(defun omaha-preflop!()
  (cond
    ;s-blindy, no big bets:
    ((and (< (stake) +stake+) (<= +hs+ 10) (> (pot-odds) 2.9)) "call preflop, commited")
    ;b-blindy lub check:
    ((zero-stake?) "call preflop,bblind check")
    ;normalne dolozenie do stawki:
    ((and (= (stake) +blind+) (<= +hs+ 8))  "call preflop,8")
    ((and (<= (pot-odds) 3.5) (<= +hs+ 7))  "call preflop,bets,7")
    ((and (big-bet?) (< +hs+ 6))  "call preflop,big bets,6")
    (t "fold preflop,no match")))

(defun omaha-fullhouse!()
  ;wiemy, ze mamy fulla jaka teraz akcje wykonac!
  (cond
    ((omaha-full-nuts?) "potbet1 nuts")
    ((big-bet?) "fold full no nuts")
    ((zero-stake?) "call no bets")
    (t "call got fh")))
     

(defun omaha-pair-table!()
  (cond
    ((rank = fullhouse) (omaha-fullhouse!))
    ((and (not (river?)) (good-odds? (count-outs 1))) "call chasing nut fh")
    ((zero-stake?) "call no bets")
    ((and (omaha-overpair?) (> (pot-odds) 4)) "call overpair,small bet")
    (t "fold policzyc dojscia do fulla")))

(defun omaha-color-table!()
  (cond
    ;liczymy dojscia do fulla lub nutsa
    ((omaha-color-nuts?) "potbet1 nut color")
    ((and (not (river?)) (good-odds? (count-outs 2))) "call chasing nut color or fh")
    ((zero-stake?) "call no bets")
    ((big-bet?) "fold full no nuts")
    (t "fold policzyc dojscia")))

(defun omaha-str8-table!()
  (cond
    ((omaha-str8-nuts?) "potbet1 nut straight")
    ;liczymy dojscia do kolor lub nutsa
    ((and (rank = straight) (zero-stake?)) "potbet2 str8 first2act")
    ((and (not (river?)) (good-odds? (count-outs 3))) "call chasing nut str8 or color or fh")
    ((zero-stake?) "call no bets")
    ((big-bet?) "fold street no nuts")
    ((and (omaha-top-two-pairs?) (> (pot-odds) 4)) "call nie bylo duzego bet")
    (t "fold nie oplaca sie"))) ;uwzglednic implied odds!



(defun omaha-two-pairs!()
  (cond 
    ((big-bet?) "fold 2pair big bet")
    ((omaha-top-two-pairs?) "potbet2 top2pair, no bet")
    (t "call 2pair")  ))


(defun omaha-postflop!()
  (cond
    ((rank > fullhouse) "potbet1 kareta lub poker")
    ((pair-table?)  (omaha-pair-table!))
    ((color-table?) (omaha-color-table!))
    ((omaha-str8-table?) (omaha-str8-table!))
    ;teraz trojka to bezpieczny set!
    ((rank = trips) (omaha-trips!))
    ((rank = two-pairs) (omaha-two-pairs!))
    ((zero-stake?) "call no bets")
    ;byly bets sprawdzmy czy oplaca sie dokupowac,
    ;na razie na stole nie ma nic szczegolnego ale ktos gra ostro:
    ((and (not (river?)) (good-odds? (count-outs))) "call chasing")
    (t  "fold postflop,no rule")))


(defun omaha!()
 (if (preflop?) 
	(omaha-preflop!)
	(omaha-postflop!)))