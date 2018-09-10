(defvar *my-pos* nil "My position at the table")
(defvar *history* nil "History of actions")
(defvar *table-size* 10 "Initial number of players")
(defvar *table* nil)
(defvar *hand* nil)

;(eval-when (:load-toplevel)
 ;  (load "poker"))

;; ------------------------  EVENTS -------------------------------------

(defun new-hand(&optional (number 10))
  #+cmu(when (<= number 3) (saytext "Grabola, help me!") )
  (setq *table* nil *hand* nil  *history* nil 
        *table-size* number *my-pos* nil))


(defun hand(&rest h)
  (setq *hand* h))


(defun flop(&rest a)
    (setq *table* a)
    (push 'flop *history*))


(defun turn(a)
  (push a *table*)
  (push 'turn *history*))


(defun river(a) 
  (push a *table*)
  (push 'river *history*))


(defun check()
    (push 'c *history*))


(defun bet()
    (push 'b *history*))


(defun fold() 
   (push 'f *history*))

;;--------------------------------------------------------------------------
(defun my-pos()
 (or *my-pos*
     (setq *my-pos*
       (let ((lh (length *history*)))
          (flet ((early-pos?()
                   (case *table-size*
                      (10 (<= lh 2))
                      (9  (<= lh 2))
                      (8  (<= lh 1))
                      (7  (<= lh 1))
                      (6  (<= lh 0))
                      (5  (<= lh 0))))
		 (middle-pos?()
		   (case *table-size*
		     (10 (<= lh 5))
		     (9  (<= lh 4))
		     (8  (<= lh 4))
		     (7  (<= lh 3))
		     (6  (<= lh 2)) 
		     (5  (<= lh 1))))
		 (late-pos?()
		   (<= (length *history*) (- *table-size* 3)))
		 (small-blind?()
		   (= (length *history*) (- *table-size* 2)))
		 (big-blind?()
		   (= (length *history*) (1- *table-size*))))
      (cond
	((early-pos?) 'early-pos)
	((middle-pos?) 'middle-pos)
	((late-pos?) 'late-pos)
	((small-blind?) 'small-blind)
	((big-blind?) 'big-blind)))))))


;;---------------------- LOGIC SUBSYSTEM  ------------------------

(defun konkluzja (zdanie) (first zdanie))
(defun przeslanki (zdanie) (rest zdanie))

(defun pobierz-reguly(konkluzja) (get konkluzja 'reguly))
(defvar *konkluzje* nil "Lista zdan o ktorych mozna wnioskowac")

(defvar *fakty* nil)

(defun fakt(cel)
  (find cel *fakty*))

(defmacro <-(&rest zdanie)
 `(dodaj-reguly ',zdanie))


(defun dodaj-reguly(regula)
 (let ((kk (konkluzja regula)))
   (pushnew kk *konkluzje*)
   (setf (get kk 'reguly) (nconc (get kk 'reguly) (list regula)))
   kk))

(defun pokaz-reguly()
 (mapc #'(lambda (r)
           (princ (get r 'reguly)))
       *konkluzje*))

(defun kasuj-reguly()
 (mapc #'(lambda (r)
           (setf (get r 'reguly) nil))
       *konkluzje*)
  (setq *konkluzje* nil))


(defun dowod-wszystkich(cele)
 (if (null cele) t (and (dowod (first cele)) 
                        (dowod-wszystkich (rest cele)))))

(defun dowod(cel)
 (if (consp cel)
  (eval cel)
  (or (fakt cel)
      (when (some
              #'(lambda(rl)
                 (dowod-wszystkich (cdr rl)))
            (pobierz-reguly cel))
            (push cel *fakty*)    ))))      



(defmacro ?- (&rest cele) `(dowod-wszystkich ',cele))

;; --------------------  ACTONS  ------------------------------------------

(defun game-round()
  (case (length *table*)
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )

(defun previous-round(smb)
  (case smb
    ('flop 'pre-flop)
    ('turn 'flop)
    ('river 'turn)))

(defun next-round(smb)
  (case smb
    ('pre-flop 'flop)
    ('flop 'turn)
    ('turn 'river)))

(defun round-history(&optional (r (game-round)) (h *history*))
  (cond
    ((eq r 'pre-flop)  (if (zerop (length *table*)) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 
 


(defun count-bets(&optional (r (game-round)))
 (count 'b (round-history r)))



(defmacro without-bet(&rest body)
 `(and (zerop (count-bets))
      (or ,@body )))

(defun pre-flop-eval()
  (let ((skl (sklansky *hand*)))
     (or 
        (and (null skl) (if (and (eq *my-pos* 'big-blind) (zerop (count-bets))) "check" "fold"))
        (and (eq *my-pos* 'early-pos)
             (or
               (without-bet
                   (and (= 1 skl) "bet")
                   (and (<= skl 4) "check")
                   "fold") 
               (and (= 1 (count-bets))  (if (<= skl 4) "check" "fold"))
               (and (> (count-bets) 1)  (if (<= skl 2) "check" "fold") )))

        (and (eq *my-pos* 'middle-pos)
             (or
               (without-bet
                   (and (= 1 skl) "bet")
                   (and (<= skl 7) "check")
                   "fold") 
               (and (= 1 (count-bets)) (if (<= skl 4) "check" "fold"))
               (and (> (count-bets) 1)  (if (<= skl 2) "check" "fold") )))

        (and (eq (my-pos) 'late-pos)
             (or
               (without-bet
                   (and (= 1 skl) "bet")
                   (and (<= skl 8) "check"))
               (and (= 1 (count-bets))  (if (<= skl 7) "check" "fold"))
               (and (> (count-bets) 1)  (if (<= skl 4) "check" "fold") )))

        (and (eq (my-pos) 'small-blind)
             (or
               (without-bet
                   (and (= 1 skl) "bet")
                   (and (<= skl 8) "check"))
               (and (= 1 (count-bets))  (if (<= skl 6) "check" "fold"))
               (and (> (count-bets) 1)  (if (= 1 skl) "check" "fold") )))

        (and (eq (my-pos) 'big-blind)
             (or
               (and (zerop (count-bets))  (if (= 1 skl) "bet" "check")) 
               (and (= 1 (count-bets)) (if (<= skl 6) "check" "fold"))
               (and (> (count-bets) 1)   (if (= 1 skl) "check" "fold") )))

        "fold") ))


;; predykaty regul:

(defvar *ocena* nil) ;lista
(defvar *vhand* nil) ;wektor figur na rece
(defvar *vtable* nil) ;wektor figur na stole
(defvar *colhand* nil) ;wektor kolorow na rece
(defvar *coltable* nil) ;wektor kolorow na stole
(defvar *vall* nil) ;wektor figur 
(defvar H1 0)
(defvar H2 0)
(defvar T1 0)
(defvar T2 0)
(defvar T3 0)
(defvar T4 0)
(defvar T5 0)

(defun v->l(vect)
  (coerce vect 'list))

(defun s->val(symb)
 (case symb
  ('hand *vhand*)
  ('table *vtable*)
  ('preflop 0)
  ('flop 3)
  ('turn 4)
  ('river 5)

  ('para 7)
  ('dwie-pary 6)
  ('trojka 5)
  ('street 4)
  ('kolor  3)

  ('sblind 0)
  ('bblind 1)))


(defmacro rank(smb what oper)
 `(let ((ocena (or (and (null ,what) *ocena*) 
                  (or (and (eq ,what 'hand) (ocena *hand*))
                      (ocena *table*)))))
   (funcall ,oper (car ocena) (s->val ,smb) )))

(defun rank=(smb &optional what)
  (rank smb what #'=))

(defun rank>=(smb &optional what)
  (rank smb what #'<=))

(defun rank<(smb &optional what)
  (rank smb what #'>))

(defun rank<=(smb &optional what)
  (rank smb what #'>=))

(defun color-outs?()
 (flet ((silna-karta (color) nil ))
  (or (dotimes (i 4) (when (and (= 2 (aref *colhand* i))
                            (= 2 (aref *coltable* i)))
                        (return t)))
     (dotimes (i 4) (when (and (= 1 (aref *colhand* i))
                            (= 3 (aref *coltable* i)))
                         (return (silna-karta i)))))))

(defun max-of-colors()
  (apply #'max (coerce *coltable* 'list) ));ilosc na stole!


(defun num-of-1-street-outs(&optional (elems *table*))
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (when  (street (wektor-figur (cons i elems))) (incf ilosc)))))

(defun num-of-2-street-outs(&optional (elems *table*))
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (dotimes (j 13 ilosc)
      (when  (street (wektor-figur (cons j (cons i elems)))) (incf ilosc))))))

(defun street-outs?()
 (> (num-of-1-street-outs (append *hand* *table*))
    (num-of-1-street-outs   *table*)))

(defun losuj(ile)
 (zerop (random ile)))

(defun base-stake(&optional (s (game-round)))
  (if (or (eq s 'pre-flop) (eq s 'flop)) 1 2))

(defun current-stake(&optional (base-stake (base-stake)) ( r-history (round-history)))
 "Wylicza ile trzeba dodac aby pozostac w grze"
      (+ base-stake (* base-stake (count 'b r-history))))

(defun round-pot( base-stake r-history );private
  (let ((h (remove 'f (reverse r-history))) (sum 0))
     (dotimes (i (length h) sum) 
        (incf sum (current-stake base-stake (subseq h 0 (1+ i))))  )))

(defun pot()
 (apply #'+ (mapcar
	       #'(lambda (s)
                  (if (null (round-history s)) 0
                      (round-pot (base-stake s) (round-history s))))
                '(pre-flop flop turn river))))

(defun pot-odds()
  (/ (pot) (current-stake)))

(defun simple-game-eval()
 (or (and (?- bet) (if (?- dont-bet) "check" "bet")) 
     (and (?- check) (if (?- fold) "fold" "check"))
     "fold"))


(defun ustaw-baze-faktow()
 (setq *fakty* nil)
 (let ((stol  (sort (mapcar #'l->figura *table*) #'<) )
       (reka  (sort (mapcar #'l->figura *hand*) #'<) ))
  (setq H1 (nth 0 reka) H2 (nth 1 reka))
  (setq T1 (nth 0 stol) T2 (nth 1 stol) T3 (nth 2 stol))
  (setq T4 (nth 3 stol) T5 (nth 4 stol)))

 (when (plusp (length *table*)) 
     (setq *ocena* (ocena (append *hand* *table*)))
     (setq *vhand* (wektor-figur *hand*))
     (setq *vtable* (wektor-figur *table*))
     (setq *colhand* (wektor-kolorow *hand*))
     (setq *coltable* (wektor-kolorow *table*))
     (setq *vall* (wektor-figur (append *hand* *table*)))))


(defun action()
 (let (wynik)
  (when (null *hand*) (return-from action "repeat"))
  (setq wynik  (if (zerop (length *table*))
                    (progn (my-pos)(pre-flop-eval))
                    (progn (ustaw-baze-faktow)
                           (simple-game-eval))))
  (logs wynik)
  wynik))

;-----------------------------------------------------------------
(defun show-stats()
  (format t "~A ~A ~A ~%" *hand* *table* *history*))

(defvar *logging* t)

(defun czas()
 (let ((c (multiple-value-list (get-decoded-time))))
     (format nil "~A:~A.~A" (nth 2 c) (nth 1 c) (nth 0 c) )))

(let ((log-messages nil))

 (defun short-logs(m)
  (and *logging* (push m log-messages)))

 (defun log-message(m)
  (format nil "~A:[~A/~A] ~A ~A ~A ~A" (czas)  *my-pos* *table-size* 
                       (liczby->opis *hand*)  (liczby->opis *table*) *history* m)) 


 (defun logs(m)
   (and *logging* 
    (let ((msg (log-message m)))
      ;(princ msg)
      (push msg log-messages))
   'ok))

 (defun clear-logs()
   (setq log-messages nil))

 (defun show-logs()
  (princ log-messages) 'ok)

 (defun write-logs()
  (with-open-file (f "logs.dat" :direction :output 
                                :if-does-not-exist :create :if-exists :supersede)
 (dolist (el log-messages)
      (format f "~A~%"  el)))) )
;--------------------------------------------------------------------