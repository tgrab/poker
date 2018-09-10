(defvar *loglist* nil)

(defstruct game-log 
  holecards
  tablecards
  shown)


(defun pojedynek-pre-flop-1(holecards shown)
  ;1 player in shown
  (let ((win 0)
	(loose 0)
	(reszta (set-difference (talia)  (append holecards (first shown) ))) )
      (dotimes (i 1000 (cons win loose))
	(let* ((stol (subseq (tasuj reszta) 0 5))
	       (o1 (omaha-ocena holecards stol))
	       (o2 (omaha-ocena (first shown) stol)))
	  (if (= 1 (poker.ranks::lepsza o1 o2))
	      (incf win)
	      (incf loose)) ))    
    ))

(defun pojedynek-flop-1(table holecards shown)
  ;1 player in shown
  (let* ((win 0)
	 (loose 0)
	 (flop (subseq (reverse table) 0 3))
	 (reszta (set-difference (talia)  (append flop  holecards (first shown) ))))
	(dolist (p (poker.ranks::podzbiory-2 reszta) (cons win loose))
	  (let ((o1 (omaha-ocena holecards (append p flop)))
		(o2 (omaha-ocena (first shown) (append p flop))))
	    (if (= 1 (poker.ranks::lepsza o1 o2))
		(incf win)
		(incf loose)) ))    
	))

(defun pojedynek-river-1(table holecards shown)
  	  (let ((o1 (omaha-ocena holecards table))
		(o2 (omaha-ocena (first shown) table)))
	    (if (= 1 (poker.ranks::lepsza o1 o2))
		1
		0)))

(defun temp1()
  (mapcar
   #'(lambda (g)
       (let ((v (pojedynek-pre-flop-1 
		 (game-log-holecards g)
		 (game-log-shown g)  )))
	 (/ (car v) (+ (car v) (cdr v) 1.0 ) )
	 ))
   *loglist*))

(defun temp2()
  (mapcar
   #'(lambda (g)
       (let ((v (pojedynek-flop-1 
		 (game-log-tablecards g)
		 (game-log-holecards g)
		 (game-log-shown g)  )))
	 (/ (car v) (+ (car v) (cdr v) 1.0 ) )
	 ))
   *loglist*))

(defun temp3()
  (mapcar
   #'(lambda (g)
       (let* ((v1 (pojedynek-pre-flop-1 
		   (game-log-holecards g)
		   (game-log-shown g)  ))
	      (v1p (/ (car v1) (+ (car v1) (cdr v1) 1.0 ) ))

	      (v2 (pojedynek-flop-1 
		 (game-log-tablecards g)
		 (game-log-holecards g)
		 (game-log-shown g)  ))
	      (v2p (/ (car v2) (+ (car v2) (cdr v2) 1.0 ) )))
	 (- v2p v1p)))
   *loglist*))

(defun temp4()
  (mapcar
   #'(lambda (g)
       (let* ((v2 (pojedynek-flop-1 
		 (game-log-tablecards g)
		 (game-log-holecards g)
		 (game-log-shown g)  ))
	      (v2p (/ (car v2) (+ (car v2) (cdr v2) 1.0 ) ))
	      (v3p (pojedynek-river-1 
		    (game-log-tablecards g)
		    (game-log-holecards g)
		    (game-log-shown g)  )))
	 (- v3p v2p)))
   *loglist*))

(defun pojedynek (game karty)
  (with-output-to-string (s)
    (let* ((reka (cards game))
	   (table (table-cards game))
	   (reszta (set-difference (talia)  (append karty reka)) )
	   ;(p5 (poker.ranks::podzbiory-5 reszta))
	   (win 0)
	   (loose 0))
      (dotimes (i 1000)
	(let* ((stol (subseq (tasuj reszta) 0 5))
	       (o1 (omaha-ocena reka stol))
	       (o2 (omaha-ocena karty stol)))
	  (if (= 1 (poker.ranks::lepsza o1 o2))
	      (incf win)
	      (incf loose)) ))
    (format s "Hand1: ~A<br>" (cards->html reka))
    (format s "Hand2: ~A<br>" (cards->html karty))
    (format s "Preflop: wins: ~A looses: ~A probab of winning: ~A<hr>" 
	    win loose (/ win 1000.0) )

    (when table
      (let* ((flop (subseq (reverse table) 0 3))
	     (reszta2 (set-difference reszta flop)))
	(format s "Flop: ~A<br>" (cards->html flop))
	(format s "Hand1: ~A<br>" (render-rank (omaha-ocena reka flop)))  
	(format s "Hand2: ~A<br>" (render-rank (omaha-ocena karty flop))) 
	(setq win 0 loose 0)
	(dolist (p (poker.ranks::podzbiory-2 reszta2))
	  (let ((o1 (omaha-ocena reka (append p flop)))
		(o2 (omaha-ocena karty (append p flop))))
	    (if (= 1 (poker.ranks::lepsza o1 o2))
		(incf win)
		(incf loose)) ))
	(format s "Flop: wins: ~A looses: ~A probab of winning: ~A<hr>" 
	    win loose (/ win (+ win loose) 1.0))    )

      (when (>= (length table) 4)
      (let* ((turn (subseq (reverse table) 0 4))
	     (reszta2 (set-difference reszta turn)))
	(format s "Turn: ~A<br>" (cards->html turn))
	(format s "Hand1: ~A<br>" (render-rank (omaha-ocena reka turn)))  
	(format s "Hand2: ~A<br>" (render-rank (omaha-ocena karty turn))) 
	(setq win 0 loose 0)
	(dolist (p reszta2)
	  (let ((o1 (omaha-ocena reka (cons p turn)))
		(o2 (omaha-ocena karty (cons p turn))))
	    (if (= 1 (poker.ranks::lepsza o1 o2))
		(incf win)
		;(progn
		 ; (format s "~A " (karta->napis p))
		  (incf loose))
		;)
	    ))
	(format s "Turn: wins: ~A looses: ~A probab of winning: ~A<hr>" 
	    win loose (/ win (+ win loose) 1.0))    )
	)
      (when (= (length table) 5)
	(format s "River: ~A<br>" (cards->html table))
	(format s "Hand1: ~A<br>" (render-rank (omaha-ocena reka table)))  
	(format s "Hand2: ~A<br>" (render-rank (omaha-ocena karty table))) )
	   )   )))