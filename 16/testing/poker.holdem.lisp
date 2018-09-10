(defun holdem-fixed-implied-odds()
  (aif (agent-pos)
       (safe-ratio (+ (* 2 +blind+) +pot+ )
		   (+ +stake+ (aref +balance+ it)))
       (safe-ratio  (+ (* 2 +blind+) +pot+ ) +stake+)))


(defun holdem-fixed-good-odds?(num-of-outs)
  (and (plusp num-of-outs)
       (> (holdem-fixed-implied-odds) (odds num-of-outs))))
;;--------------------------------------------------------------------------
;;------------------------------Holdem Predicates---------------------------
;;--------------------------------------------------------------------------

(defun holdem-top-pair?()
  (and (rank >= pair)(or (= h1 t1) (= h2 t1))))

(defun holdem-top-pair-kicker?()
 ;stronger than top-pair
  (and (rank >= pair) (= h2 t1)))

(defun holdem-overpair?()
  (and (= h1 h2) (< h1 t1)))

(defun holdem-strong-pair?()
  (or (holdem-top-pair?) 
      (holdem-top-pair-kicker?)
      (holdem-overpair?)))

(defun holdem-set?()
  (and (= h1 h2)
       (or (= h1 t1) (= h1 t2) (= h1 t3) (eq h1 t4) (eq h1 t5))))

(defun holdem-color-outs?(&optional (reka +cards+)(stol +table-cards+))
 (let ((colhand (wektor-kolorow reka))
       (coltable (wektor-kolorow stol)))
 (flet ((silna-karta (color) (or
			      (member (+ 1 (* 13 color)) reka)
			      (member (+ 2 (* 13 color)) reka)
			      (member (+ 3 (* 13 color)) reka)) ))
  (or (dotimes (i 4) (when (and (= 2 (aref colhand i))
                            (= 2 (aref coltable i)))
                        (return t)))
     (dotimes (i 4) (when (and (= 1 (aref colhand i))
                            (= 3 (aref coltable i)))
                         (return (silna-karta i))))
   ))))

(defun holdem-color-nuts?(&optional (reka +cards+)(stol +table-cards+))
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
		(member (+ 3 (* 13 col)) reka))   )))))


(defun holdem-str8-outs(&optional (karty (append +cards+ +table-cards+)))
 "Ile kart mozna dolozyc do listy karty aby dostac strita"
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (when  (poker.ranks::street (wektor-figur (cons i karty))) (incf ilosc)))))

(defun holdem-str8-outs?()
   (let ((o1 (holdem-str8-outs)) (o2 (holdem-str8-outs +table-cards+)))
     (and (plusp o1) (> o1 o2))))

(defun holdem-outs()
 (let ((ilosc 0) (nc (max-of-colors)))
   (when (holdem-color-outs?) (incf ilosc 9))
   (when (and (< nc 3) (holdem-str8-outs?))
     (if (= 2 nc)
	 (incf ilosc (* 3 (holdem-str8-outs)))
	 (incf ilosc (* 4 (holdem-str8-outs)))) )
   ilosc))

(defun holdem-str8-table?()
  (plusp (holdem-str8-outs +table-cards+)))

(defun no-holdem-str8-table?()
  (zerop (holdem-str8-outs +table-cards+)))

;;--------------------------------------------------------------------------
;;------------------------------Fixed Holdem Logic--------------------------
;;--------------------------------------------------------------------------
(setq +game-kind+ 2)



(defun holdem-fixed-fold?()
  (cond
    ((and (rank < color) (one-bet?) (= (max-of-colors) 4) (< (random 4) 3)) "random 3/4, smbd got color")
    ((and (rank < color) (with-raise?) (>= (max-of-colors) 4)) "smbd got color") ))


(defun holdem-fixed-bet?()
  (cond
    ((rank >= fullhouse) "got fh or higher")  
    ((and (flop?) (holdem-strong-pair?) (no-bets?)) "top pair")
    ((and (holdem-strong-pair?) (no-bets?) (zerop (random 3))) "top pair,bet with random3")
    ((and (flop?) (rank > pair) (no-bets?)) "better than pair")
    ((and (rank >= trips) (no-bets?)) "better than pair")
    ;mamy nutsy
    ((and (no-color-table?) (no-pair-table?) (rank >= trips)) "set or str8 is ok")
    ((and (no-color-table?) (no-pair-table?) (no-holdem-str8-table?) (no-bets?) (holdem-overpair?)) "overpair is ok")
    ((and (rank = color) (holdem-color-nuts?) (no-pair-table?)) "color is nuts")
))

(defun holdem-fixed-dont-bet?()
  (cond
    ((and (with-raise?) (rank = fullhouse) (trips-table?) (not (holdem-strong-pair?)))  "only 1 pair")
    ((and (with-raise?) (rank < fullhouse) (two-pair-table?) )  "only 1 pair")
    ((and (with-bets?) (rank = pair))  "only 1 pair")  ))


(defun holdem-fixed-call?()
  (cond
    ((zerop (stake)) "can check")
    ((and (flop?) (holdem-strong-pair?) (< (stake) +stake+) (= 2 (bets))) "top pair, commited") ; was raised
    ((and (flop?) (holdem-strong-pair?) (one-bet?)) "top pair, one bet") ; call one bet
    ((and (flop?) (one-bet?) (= h1 h2) (< h1 4) (< h1 t2)) "moze ta para dobra")
    ((and (no-pair-table?) (rank > pair) (one-bet?)) "better than pair, one bet") ; TODO jesli bezpieczny stol? 
    ((and (holdem-strong-pair?) (one-bet?)  ) "strong pair, one bet") ; TODO jesli bezpieczny stol? 
    ((and (rank >= trips) (<= (max-of-colors) 2)) "with trips") ;moga lezec strity
    ((and (rank >= trips) (one-bet?)) "with trips onet bet") ;moga lezec strity
    ((and (not (river?)) (plusp (holdem-outs))  (one-bet?) (holdem-fixed-good-odds? (holdem-outs))) "chasing str8 or color, one bet")
    ((and (not (river?)) (plusp (holdem-outs))  (<= (max-of-colors) 2) (holdem-fixed-good-odds? (holdem-outs))) "chasing str8 or color")
    ((and (not (river?)) (<= (max-of-colors) 2) (no-pair-table?) (one-bet?) (< h2 4) (>= t1 4) (holdem-fixed-good-odds? 6)) "chasing 2 overcards")
    ((rank >= color) "z kolorem nie pasujemy")
 ))


; dowodzimy roznych twierdzen fold,bet,call,dont-bet
; fold jest najmocniejsze zawiera ekstremalne sytacje w ktorych trzeba pasowac
; bet jest przeslanka do bet ale specjalne przypadki moga by zabronione w dont-bet
; jesli call jest spelnione to wykonujemy call
; ostatecznie zostaje tylko fold
(defmacro send-action(ac val)
  `(format nil "~A ~A" ,ac ,val))

(defun holdem-fixed!()
  (if (preflop?)
      ;reguly preflop sa proste:
      (progn
	(when (null +pos+) (setq +pos+ +free-seat+))
	(cond
	  ((and (null +hs+) (zerop (stake))) "call on big blind")
	  ((null +hs+) "fold no group")
	  ((= +hs+ 1) "bet preflop, sklansky1")
	  ((and +pos+ (> +pos+ 7) (no-bets?) (= +hs+ 2)) "bet no bets late pos")
	  ((and +pos+ (> +pos+ 7) (no-bets?) (<= +hs+ 7)) "call no bets late pos")
	  ((and +pos+ (> +pos+ 3) (no-bets?) (<= +hs+ 6)) "call no bets middle pos")
	  ((<= +hs+ 2) "call preflop, group 2")
	  ((and (<= +hs+ 5) (<= (bets) 1)) "call preflop")
	  ((and (<= +hs+ 5) (< (stake) (* 2 +blind+))) "call preflop wasnt raise before")
	  ((zerop (stake)) "call no bets")
	  ((and (< (stake) +stake+) (<= (stake) +blind+) +hs+) "call commited") ;commited no 2 bets before
	  (t "fold preflop, no rule")) )
      ;postflop:
	(let ((fold? (holdem-fixed-fold?)))
	  (if fold?
	      (send-action "fold" fold?)
	      (let
		  ((bet? (holdem-fixed-bet?))
		   (dont-bet? (holdem-fixed-dont-bet?)))
		(if bet? 
		    (if dont-bet? (send-action "call" dont-bet?) (send-action "bet" bet?))
		    (let ((call? (holdem-fixed-call?)))
		      (if call?
			  (send-action "call" call?)
			  "fold no rule applied"))))))))
      

;;--------------------------------------------------------------------------
;;------------------------------Nolimit Holdem Logic------------------------
;;--------------------------------------------------------------------------

(defun holdem-nolimit!()
	"fold not implemented")