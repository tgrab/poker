(in-package :poker)

;;---------- Holdem Fixed Funcs ---------------------------------

(defun sklansky-group(karta1 karta2)
 "karta1, karta2 to  karty na rece"
  (macrolet
     ((oba(&body wartosci)
       `(or ,@(mapcar 
              (lambda (w)  `(and (eq ,w figura1) (eq ,w figura2)) )
               wartosci) ))
       (oba2(wart1 wart2)
        `(or(and (eq ,wart2 figura1) (eq ,wart1 figura2))
            (and (eq ,wart1 figura1) (eq ,wart2 figura2))) )
      (same-color(warunek)
          `(and (= kolor1 kolor2) ,warunek) ))
 (let ((kolor1 (kolor karta1)) 
       (figura1 (nth (figura karta1) *figury*))
       (kolor2 (kolor karta2))
       (figura2 (nth (figura karta2) *figury*)))
 (cond
  ((oba 'A 'K 'Q 'J)  1)
  ((same-color (oba2 'A 'K)) 1)
  ((oba 'T) 2)
  ((same-color (or (oba2 'A 'Q) (oba2 'A 'J) (oba2 'K 'Q) ) ) 2)
  ((oba2 'A 'K) 2)  
  ((oba '9) 3)
  ((same-color (or (oba2 'J 'T) (oba2 'Q 'J) (oba2 'K 'J) (oba2 'A 'T))) 3)
  ((oba2 'A 'Q) 3)
  ((oba '8) 4)
  ((or (oba2 'K 'Q) (oba2 'A 'J)) 4)
  ((same-color (or (oba2 'T '9) (oba2 'Q 'T) (oba2 '9 '8) (oba2 'J '9))) 4)
  ((oba '7) 5)
  ((or (oba2 'K 'J) (oba2 'Q 'J) (oba2 'J 'T)) 5)
  ((same-color (or (oba2 '8 '7) (oba2 'Q '9) (oba2 '10 '8) (oba2 '7 '6) (oba2 '9 '7)(oba2 '6 '5))) 5)
  ((same-color (or (eq figura1 'A)(eq figura2 'A))) 5)
  ((oba '6 '5) 6)
  ((or (oba2 'A 'T) (oba2 'K 'T) (oba2 'Q 'T)) 6)
  ((same-color (or (oba2 '8 '6) (oba2 '5 '4) (oba2 'K '9) (oba2 'J '8) (oba2 '7 '5))) 6)
  ((oba '4 '3 '2) 7)
  ((or (oba2 'J '9) (oba2 'T '9) (oba2 '9 '8)) 7)
  ((same-color (or (oba2 '6 '4) (oba2 '5 '3) (oba2 '4 '3) (oba2 'T '7) (oba2 'Q '8))) 7)
  ((same-color (or (eq figura1 'K)(eq figura2 'K))) 7)

  ((or (oba2 '8 '7) (oba2 'A '9) (oba2 'Q '9) (oba2 '7 '6) (oba2 'J '8) (oba2 '6 '5)
       (oba2 '5'4) (oba2 'K '9) (oba2 'T '8)) 8)
  ((same-color (or (oba2 '4 '2) (oba2 '3 '2) (oba2 '9 '6) (oba2 '8 '5) (oba2 'J '7) (oba2 '7 '4))) 8) ))))

;(defun holdem-str8-outs(&optional (karty (append (cards +game+) (table-cards +game+))))
;"Ile kart mozna dolozyc do listy karty aby dostac strita"
;(let ((ilosc 0))
; (dotimes (i 13 ilosc)
;   (when  (poker.ranks::street (wektor-figur (cons i karty))) (incf ilosc)))))


;(defun holdem-str8-outs?()
;   (let ((o1 (holdem-str8-outs)) (o2 (holdem-str8-outs (table-cards +game+))))
;     (and (plusp o1) (> o1 o2))))

;(defun holdem-str8-table?()
;  (plusp (holdem-str8-outs (table-cards +game+))))

(defun holdem-count-color-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (when (<= (car (ocena (append cards table-cards))) 3) (return-from holdem-count-color-outs 0) )
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(if (>= (length table) 5)
	    (when 
		(and (= (car (ocena (append cards table))) 3)
		    (> (car (ocena table))) 3)
	      (incf nr))
	    (when  (= (car (ocena (append cards table))) 3)
	      (incf nr)) )))))



(defun holdem-count-full-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
;juz mamy fulla to nie kupujemy dalej
  (when (<= (car (ocena (append cards table-cards))) 2) (return-from holdem-count-full-outs 0) )
;specyficzne uklady:
  (when (and (= h1 h2) (< t1 h1 t2) (= 2 table-pairs) ) (return-from holdem-count-full-outs 2))
  (let ((W (apply #'max (coerce (wektor-figur (table-cards +game+)) 'list))))
    ;jesli lezy trojka na stole to liczymy ilosc overcards:
    (when (= 3 w)
      (let ((t1 (figura (nth 0 (sort (copy-list table-cards) #'< :key #'figura))))
	    (nr 0))
	(when (< (figura (first cards)) t1) (incf nr 3) )
	(when (< (figura (second cards)) t1) (incf nr 3) )      
	(return-from holdem-count-full-outs nr))))
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(if (>= (length table) 5)
	    (when 
		(and (<= (car (ocena (append cards table))) 2)
		     (> (car (ocena table))) 2)
	      (incf nr))
	    (when  (<= (car (ocena (append cards table))) 2)
	      (incf nr)) )))))


(defun holdem-count-str8-outs (&optional (cards (cards +game+)) (table-cards (table-cards +game+)))
  (when (<= (car (ocena (append cards table-cards))) 4) (return-from holdem-count-str8-outs 0) )
  (let ((nr 0))
    (dolist (c (set-difference (talia) (append cards table-cards)) nr)
      (let ((table (cons c table-cards)))
	(if (>= (length table) 5)
	    (when 
		(and (= (car (ocena (append cards table))) 4)
		    (> (car (ocena table))) 4)
	      (incf nr))
	    (when  (= (car (ocena (append cards table))) 4)
	      (incf nr)) )))))



(defun holdem-color-outs?(&optional (reka (cards +game+))(stol (table-cards +game+)))
 (let ((colhand (wektor-kolorow reka))
       (coltable (wektor-kolorow stol)))
 (flet ((silna-karta (colr) (or
			      (member (+ 1 (* 13 colr)) reka)
			      (member (+ 2 (* 13 colr)) reka)
			      (member (+ 3 (* 13 colr)) reka)) ))
  (or (dotimes (i 4) (when (and (= 2 (aref colhand i))
                            (= 2 (aref coltable i)))
                        (return t)))
     (dotimes (i 4) (when (and (= 1 (aref colhand i))
                            (= 3 (aref coltable i)))
                         (return (silna-karta i))))
   ))))

;(defun holdem-count-outs()
; (let ((ilosc 0) (nc (apply #'max (coerce (wektor-kolorow (table-cards +game+)) 'list))) )
;   (when (holdem-color-outs?) (incf ilosc 9))
;   (when (and (< nc 3) (holdem-str8-outs?))
;     (if (= 2 nc)
;	 (incf ilosc (* 3 (holdem-str8-outs)))
;	 (incf ilosc (* 4 (holdem-str8-outs)))) )
;   ilosc))

(defun holdem-count-outs()
  (+ (holdem-count-str8-outs) (holdem-count-full-outs) (holdem-count-color-outs)))

(defun holdem-str8-nuts-2?(&optional (reka (cards +game+)) (stol (table-cards +game+)))
  (let ((figury-stolu  (mapcar #'figura stol))
	(rank (ocena (append reka stol))))
    (and (= 4 (car rank))
	 (<=
	  (second rank)
	  (apply #'min
		 (mapcar
		  #'(lambda (karta)
		      (let  ((str (poker.ranks::street 
				   (wektor-figur (mapcar #'1+ (append karta figury-stolu))))))
			    (if str str 100)))
			(podzbiory-2
			 (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11 12) 
					 figury-stolu))) )))))

(defun holdem-str8-nuts-1?(&optional (reka (cards +game+)) (stol (table-cards +game+)))
  (let ((figury-stolu  (mapcar #'figura stol))
	(rank (ocena (append reka stol))))
    (and (= 4 (car rank))
	 (<=
	  (second rank)
	  (apply #'min
		 (mapcar
		  #'(lambda (karta)
		      (let  ((str (poker.ranks::street 
				   (wektor-figur (mapcar #'1+ (cons karta figury-stolu))))))
			    (if str str 100)))
			(set-difference '(0 1 2 3 4 5 6 7 8 9 10 11 12) figury-stolu)) )))))

(defun holdem-str8-nuts?()
  (cond
    ((one-str8-table?) (holdem-str8-nuts-1?))
    ((two-str8-table?) (holdem-str8-nuts-2?))  ))

(defun holdem-color-nuts?(&optional  (reka (cards +game+))(stol (table-cards +game+)))
  (flet ((get-color ()
	   (loop for c from 0 to 3 do
		 (when (>= (aref (wektor-kolorow (append reka stol)) c) 5)
		   (return-from get-color c)))))
    (let ((col (get-color)))
      (when col
      (or  (member (+ 1 (* 13 col)) reka)
	   (and (member (+ 1 (* 13 col)) stol)
		(member (+ 2 (* 13 col)) reka))
	   (and (member (+ 1 (* 13 col)) stol)
		(member (+ 2 (* 13 col)) stol)
		(member (+ 3 (* 13 col)) reka))
	   (and (member (+ 1 (* 13 col)) stol)
		(member (+ 2 (* 13 col)) stol)
		(member (+ 3 (* 13 col)) stol)
		(member (+ 4 (* 13 col)) reka))   )))))


(defun holdem-round-logic()
  
  ; opisy gornych parek
  (when (and (= 0 h1 t1) (< h2 3)) ;K,Q
    (<-into (round-memo +game+) (rank strong-pair))
    (<-into (round-memo +game+) (rank aces-with-kicker))    )

  (when (= h1 t1)   ;top-pair
    (<-into (round-memo +game+) (rank top-pair)))

  (when (= h2 t1)
    (when (< h1 3)
      (progn
	(<-into (round-memo +game+) (rank top-pair-kicker))
	(<-into (round-memo +game+) (rank strong-pair))))
    (<-into (round-memo +game+) (rank top-pair)))

  (when (and (= h1 h2) (< h1 t1)) ;overpair
    (<-into (round-memo +game+) (rank strong-pair))
    (<-into (round-memo +game+) (rank overpair)))

  ;strong pair to gorna para z dobra druga karta j.w


  (when (or (< h1 t1) (< h2 t1)) ;kicker=karta wyzsza od stolu
    (when (= 0 h1)  (<-into (round-memo +game+) (rank ace-kicker)))
    (<-into (round-memo +game+) (rank kicker)))

  (when (and (= h1 t1) (= h2 t2)) ;two-pairs
    (<-into (round-memo +game+) (rank top-two-pairs)))

    ;sety
    (when (or (= h1 h2 t1) (= h1 h2 t2) (= h1 h2 t3) (= h1 h2 t4) (= h1 h2 t5))
      (if (= h1 h2 t1)
	  (progn
	    (<-into (round-memo +game+) (rank set))
	    (<-into (round-memo +game+) (rank top-set)))
	  (<-into (round-memo +game+) (rank set))))

    ;outsy i nutsy:
    (when (or (flop?) (turn?))

      (let ((ot (holdem-count-outs)))
	(if (plusp ot)
	    (progn
	      (add-clause (round-memo +game+) `((outs ,ot)))
	      (<-into (round-memo +game+) (outs yes)))
	    (<-into (round-memo +game+) (outs no))))

      (let ((ot (holdem-count-color-outs)))
	(if (plusp ot)
	    (progn
	      (add-clause (round-memo +game+) `((outs color ,ot)))
	      (<-into (round-memo +game+) (outs color yes)))
	    (<-into (round-memo +game+) (outs color no))))

      (let ((ot (holdem-count-str8-outs)))
	(if (plusp ot)
	    (progn
	      (add-clause (round-memo +game+) `((outs str8 ,ot)))
	      (<-into (round-memo +game+) (outs str8 yes)))
	    (<-into (round-memo +game+) (outs str8 no))))

      (let ((ot (holdem-count-full-outs)))
	(if (plusp ot)
	    (progn
	      (add-clause (round-memo +game+) `((outs fullhouse ,ot)))
	      (<-into (round-memo +game+) (outs fullhouse yes)))
	    (<-into (round-memo +game+) (outs fullhouse no))))   )
    ;good-fullhouse
    (when
	(and (= 2 (car *o*))
	     (or
	      (?- (rank set))
	      (= h1 t1 t2) (= h1 t2 t3) (= h2 t1 t2) (= h2 t2 t3)))
	(<-into (round-memo +game+) (rank good-fullhouse)))
    (when (and (>= rank color) (holdem-color-nuts?))
      	(<-into (round-memo +game+) (rank top-color)))
    ;czy jest nuts:
    (cond
      ((?- (table-pairs yes))
       (if	
	(or (?- (rank top-set))
	    (and (= h1 t1 t2) (or (= h2 t2) (= h2 t3) (= h2 t4))) ;no prawie!
	    (and (= h1 t1 t2) (or (= t3 t4) (= t4 t5) ))
	    (and (= h2 t1 t2) (or (= t3 t4) (= t4 t5) )))
	(<-into (round-memo +game+) (rank nuts))
	(<-into (round-memo +game+) (rank not-nuts)))   )
      ((?- (table-colors yes))
       (if (holdem-color-nuts?)
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))
      ((?- (table-str8 yes))
       (if (holdem-str8-nuts?)
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))
      (t 
       (if (?- (rank top-set))
	   (<-into (round-memo +game+) (rank nuts))
	   (<-into (round-memo +game+) (rank not-nuts))))      )    )

