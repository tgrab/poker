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

#|
(defun holdem-fixed-implied-odds()
  (aif (agent-pos)
       (safe-ratio (+ (* 2 +blind+) +pot+ )
		   (+ +stake+ (aref +balance+ it)))
       (safe-ratio  (+ (* 2 +blind+) +pot+ ) +stake+)))


(defun holdem-fixed-good-odds?(num-of-outs)
  (and (plusp num-of-outs)
       (> (holdem-fixed-implied-odds) (odds num-of-outs))))

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
 (let ((ilosc 0) (nc (game-prop 'max-of-colors)))
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

|#