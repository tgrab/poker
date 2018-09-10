(defvar *my-pos* nil "My position at the table")
(defvar *history* nil "History of actions")
(defvar *table-size* 10 "Initial number of players")
(defvar *table* nil)
(defvar *hand* nil)

(eval-when (:load-toplevel)
   (load "poker"))

;; ------------------------  EVENTS -------------------------------------

(defun new-hand(&optional (number 10)) 
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
    (push 'check *history*))


(defun bet()
    (push 'bet *history*))


(defun fold() 
   (push 'fold *history*))



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
 ;(or (find cel *fakty*)
  ;   (and (fboundp cel) (funcall cel))))



(defmacro <-(&rest zdanie)
 `(dodaj-reguly ',zdanie))

;(defmacro defrule( (&rest przeslanki) -> konkluzja  )
 ;`(dodaj-reguly '(,konkluzja ,@przeslanki)))

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
      (some
        #'(lambda(rl)
             (dowod-wszystkich (cdr rl)))
        (pobierz-reguly cel)))))      



(defmacro ?- (&rest cele) `(dowod-wszystkich ',cele))


;; --------------------  ACTONS  ------------------------------------------
(defun game-round()
  (case (length *table*)
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )

(defun round-history(&optional (r (game-round)) (h *history*))
  (if (eq r 'pre-flop) 
       h
       (subseq h 0 (position r h))))

(defun count-bets(&optional (r (game-round)))
 (count 'bet (round-history r)))


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
 (let ((kolor1 (l->kolor karta1)) 
       (figura1 (nth (l->figura karta1) figury))
       (kolor2 (l->kolor karta2))
       (figura2 (nth (l->figura karta2) figury)))
 (cond
  ((oba 'A 'K 'Q 'J)  1)
  ((same-color (oba2 'A 'K)) 1)
  ((oba '10) 2)
  ((same-color (or (oba2 'A 'Q) (oba2 'A 'J) (oba2 'K 'Q) ) ) 2)
  ((oba2 'A 'K) 2)  
  ((oba '9) 3)
  ((same-color (or (oba2 'J '10) (oba2 'Q 'J) (oba2 'K 'J) (oba2 'A '10))) 3)
  ((oba2 'A 'Q) 3)
  ((oba '8) 4)
  ((or (oba2 'K 'Q) (oba2 'A 'J)) 4)
  ((same-color (or (oba2 '10 '9) (oba2 'Q '10) (oba2 '9 '8) (oba2 'J '9))) 4)
  ((oba '7) 5)
  ((or (oba2 'K 'J) (oba2 'Q 'J) (oba2 'J '10)) 5)
  ((same-color (or (oba2 '8 '7) (oba2 'Q '9) (oba2 '10 '8) (oba2 '7 '6) (oba2 '9 '7)(oba2 '6 '5))) 5)
  ((same-color (or (eq figura1 'A)(eq figura2 'A))) 5)
  ((oba '6 '5) 6)
  ((or (oba2 'A '10) (oba2 'K '10) (oba2 'Q '10)) 6)
  ((same-color (or (oba2 '8 '6) (oba2 '5 '4) (oba2 'K '9) (oba2 'J '8) (oba2 '7 '5))) 6)
  ((oba '4 '3 '2) 7)
  ((or (oba2 'J '9) (oba2 '10 '9) (oba2 '9 '8)) 7)
  ((same-color (or (oba2 '6 '4) (oba2 '5 '3) (oba2 '4 '3) (oba2 '10 '7) (oba2 'Q '8))) 7)
  ((same-color (or (eq figura1 'K)(eq figura2 'K))) 7)

  ((or (oba2 '8 '7) (oba2 'A '9) (oba2 'Q '9) (oba2 '7 '6) (oba2 'J '8) (oba2 '6 '5)
       (oba2 '5'4) (oba2 'K '9) (oba2 '10 '8)) 8)
  ((same-color (or (oba2 '4 '2) (oba2 '3 '2) (oba2 '9 '6) (oba2 '8 '5) (oba2 'J '7) (oba2 '7 '4))) 8) ))))

(defun sklansky(&optional (h *hand*))
  (sklansky-group (first h) (second h)))

(defmacro without-bet(&rest body)
 `(and (zerop (count-bets))
      (or ,@body )))

(defun pre-flop-eval()
  (let ((skl (sklansky)))
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

(defun rank->kod(rank);deprecated
 (case rank
  ('para 7)
  ('dwie-pary 6)
  ('trojka 5)
  ('street 4)
  ('kolor  3)))

;; predykaty dotyczace wlasciwosci stolu:

;; potrzebne predykaty:
;; rank= rank< rank<=
(defmacro rank(smb what oper)
 `(let ((ocena (or (and (null ,what) *ocena*) 
                  (or (and (eq ,what 'hand) (ocena *hand*))
                      (ocena *table*)))))
   (funcall ,oper (car ocena) (s->val ,smb) )))

(defun rank=(smb &optional what)
  (rank smb what #'=))

(defun rank>=(smb &optional what)
  (rank smb what #'<=))

;; inne:
(defun my-rank?(rank)
 (= (car *ocena*) (rank->kod rank) ))

(defun better-than?(rank)
  (<= (car *ocena*) (rank->kod rank) ))

(defun worse-than?(rank)
  (> (car *ocena*) (rank->kod rank) ))

(defun top-pair? (&optional (v1 *vhand*)  (v2 *vtable*))
  (dotimes (i 13)
    (when (= 1 (aref v2 i)) 
      (return-from top-pair? (= 1 (aref v1 i))) )))


(defun top-pair-with-kicker?(&optional (v1 *vhand*)  (v2 *vtable*) (v *vall*))
 (and (top-pair? v1 v2)
      (dotimes (i 13) 
        (when (plusp (aref v i)) 
             (return-from top-pair-with-kicker? (= 1 (aref v i))) ))))

(defun pair?(&optional (v *vall*))
 (dotimes (i 13)
   (when (= 2 (aref v i)) (return-from pair? i))))

(defun overpair?(&optional (v1 *vhand*)  (v2 *vtable*))
 (let ((p (pair? v1)))
   (and p (dotimes (i p t)
            (when (plusp (aref v2 i)) (return-from overpair? nil))))))

(defun ten-sam-kolor?(na-rece na-stole)
  (dotimes (i 4) (when (and (= na-rece (aref *colhand* i))
                            (= na-stole (aref *coltable* i)))
                        (return-from ten-sam-kolor? t))))




(defun simple-game-eval()
 (or (and (?- bet) (if (?- dont-bet) "check" "bet")) 
     (and (?- check) (if (?- dont-check) "fold" "check"))
     "fold"))


(defun ustaw-baze-faktow()
 (setq *fakty* nil)
 (case (count-bets)
  (0 (push 'no-bets *fakty*))
  (1 (push 'one-bet *fakty*))
  (2 (push 'raise *fakty*))
  (3 (push 'reraise *fakty*)))

 (case (length *table*)
  (3 (push 'flop *fakty*))
  (4 (push 'river *fakty*))
  (5 (push 'turn *fakty*)))

 (let ((stol  (sort (mapcar #'l->figura *table*) #'<) )
       (reka  (sort (mapcar #'l->figura *hand*) #'<) ))
  (setq H1 (nth 0 reka))
  (setq H2 (nth 1 reka))
  (setq T1 (nth 0 stol))
  (setq T2 (nth 1 stol))
  (setq T3 (nth 2 stol))
  (setq T4 (nth 3 stol))
  (setq T5 (nth 4 stol)))

 (when (plusp (length *table*)) 
     (setq *ocena* (ocena (append *hand* *table*)))
     (setq *vhand* (wektor-figur *hand*))
     (setq *vtable* (wektor-figur *table*))
     (setq *colhand* (wektor-kolorow *hand*))
     (setq *coltable* (wektor-kolorow *table*))
     (setq *vall* (wektor-figur (append *hand* *table*)))))


(defun action()
 (let (wynik)
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
     (format nil "~A:~A" (nth 2 c) (nth 1 c)  )))

(let ((log-messages nil))

 (defun logs(m)
   (and *logging* (push (format nil "[~A/~A] ~A ~A ~A ~A" *my-pos* *table-size* 
				                          (liczby->opis *hand*) 
                                                          (liczby->opis *table*)
                                                          *history* m) 
                        log-messages))
   'ok)

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