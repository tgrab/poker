(defun insert-player(name)
  (assert (< (free-seat +game+) 10))
  (when (string= name (agent-name +game+))
    (setf (agent-pos +game+) (free-seat +game+)))
  (push (cons name (free-seat +game+)) (name->seat +game+))
  (setf (aref (taken-seats +game+) (free-seat +game+)) t)
  (setf (aref (active-players +game+) (free-seat +game+)) t)
  (incf (free-seat +game+)))

(defun insert-player?(name)
  (and (null (table-cards +game+)) ;preflop 
       (not (assoc name (name->seat +game+) :test #'string=))
       (insert-player name)))


(defun game-round(&optional (g +game+))
  (case (length (table-cards g))
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)))

(defun previous-round(smb)
  (case smb
    ('pre-flop 'pre-flop)
    ('flop 'pre-flop)
    ('turn 'flop)
    ('river 'turn)))

(defun next-round(smb)
  (case smb
    ('pre-flop 'flop)
    ('flop 'turn)
    ('turn 'river)))

(defun round-history(&optional (r (game-round)) (h (history +game+)))
  (cond
    ((eq r 'pre-flop)  (if (null (table-cards +game+)) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 

(defun bets(&optional (r (game-round)) (h (history +game+)))
 (count-if #'(lambda(el)
	       (and (consp el)
		    (or (eq (car el) 'b) 
			(eq (car el) 'r)
			(eq (car el) 'a))))
	   (round-history r h)))


(defun player-pos(name &optional (g +game+))
  (cdr (assoc name (name->seat g) :test #'string=)))

;TODO- chyba zle gdy ppos
(defun get-stake(&optional (ppos (agent-pos +game+)))
  (if ppos
       (+ (stake +game+) (aref (balance +game+) ppos))
       (stake +game+)))
     

(defun player-stake(name)
  (get-stake (player-pos name)))

(defun safe-division(e d)
  (if (zerop d)
      1000
      (float (/ e d))))

(defun pot-odds()
  (safe-division (pot +game+) (get-stake)))

;; Tworzenie listy akcji w danej rundzie
(defun usun-sasiednie-c(lista &optional (acc nil))
  (if (null lista)
      (nreverse acc)
      (if (and (eq 'c (car lista))
	       (eq 'c (car acc)))
	(usun-sasiednie-c (cdr lista) acc)
	(usun-sasiednie-c (cdr lista) (cons (car lista) acc)))))

(defun stos-akcji(&optional (runda (previous-round (game-round))))
  (let (res)
    (dotimes (i 10)
	  (when (aref (active-players +game+) i)  
	    (push 
	     (cons i 
		   (bets runda  (aref (player-history +game+) i)) )
	     res))  )  
    (usun-sasiednie-c
     (mapcan
      (lambda (x) (cond
		   ((eq (car x) (agent-pos +game+))  (case (cdr x)
						       (0 (list 'AC))
						       (1 (list 'AB))
						       (2 (list 'A2B))  ))
		   ((eq (cdr x) 0)    (list 'C))
		   ((eq (cdr x) 1)    (list 'B)) 
		   ((eq (cdr x) 2)   (list '2B))))
      (nreverse res)))))

