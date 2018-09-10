;; ------- PREDICATES -------------------------------

(defun pre-flop?()
 (zerop (length *table*)))

(defun flop?()
 (when (= 3 (length *table*))))

(defun turn?()
 (when (= 4 (length *table*))))

(defun river?()
 (when (= 5 (length *table*))))

(defun betted?()
  (plusp (count-bets)))

(defun no-bets?()
  (zerop (count-bets))) 

(defun one-bet?()
  (= 1 (count-bets)))

(defun raised?()
  (= 2 (count-bets)))

(defun re-raised?()
  (= 3 (count-bets)))


; to ma sens tylko na pierwszym okrazeniu:
(defun small-blind?()
  (or (eq 'small-blind *my-pos*) (= (length *history*) (- *table-size* 2))))
; j.w.:
(defun big-blind?()
  (or *my-pos* (= (length *history*) (1- *table-size*))))
; j.w.:
(defun early-pos?()
 (or *my-pos*
 (let ((lh (length *history*)))
  (case *table-size*
    (10 (<= lh 2))
    (9  (<= lh 2))
    (8  (<= lh 1))
    (7  (<= lh 1))
    (6  (<= lh 0))
    (5  (<= lh 0))   ))))
; j.w.:
(defun middle-pos?()
 (or *my-pos*
 (let ((lh (length *history*)))
  (case *table-size*
    (10 (<= lh 5))
    (9  (<= lh 4))
    (8  (<= lh 4))
    (7  (<= lh 3))
    (6  (<= lh 2)) 
    (5  (<= lh 1))    ))))
; j.w.:
(defun late-pos?()
  (or *my-pos* (<= (length *history*) (- *table-size* 3))))
  
;; 10: (1,2,3) (3,4,5,6) (7,8) SB BB
;;  9: (1,2,3) (3,4,5) (6,7) SB BB
;;  8: (1,2) (3,4,5) (6) SB B
;;  7: (1,2) (3,4) (5) SB BB
;;  6: (1) (2,3) (4) SB BB
;;  5: (1) (2) (3) SB BB
;;  4: () () (1,2) SB BB
;;  3: () () (1) SB BB
;;  2: () () () SB BB

(defun my-pos()
 (if *my-pos*
     *my-pos* ; return saved position
    (setq *my-pos*
      (cond
	((early-pos?) 'early-pos)
	((middle-pos?) 'middle-pos)
	((late-pos?) 'late-pos)
	((small-blind?) 'small-blind)
	((big-blind?) 'big-blind)))))
       
; argumentami tych funkcji sa wektory figur na rece i stole:
(defmacro with-cards(&body body)
  `(let* ((vhand (wektor-figur *hand*))
         (vtable (wektor-figur *table*))
         (ev (wektor-figur (append *hand* *table*)))
         (ocena (ocena (append *hand* *table*)))
         ;(two-pairs? (= 6 (first ocena)))
         (top-pair? (top-pair? vhand vtable))
         (with-kicker? (top-pair-with-kicker? vhand vtable ev))
         (overpair? (overpair? vhand vtable)))
      (or ,@body )))

(defun top-pair?(v1 v2)
 (dotimes (i 13)
    (when (= 1 (aref v2 i)) (return-from top-pair? (= 1 (aref v1 i))) )))

(defun top-pair-with-kicker?(v1 v2 v)
 (and (top-pair? v1 v2)
      (dotimes (i 13) 
        (when (plusp (aref v i)) 
             (return-from top-pair-with-kicker? (= 1 (aref v i))) ))))

(defun pair?(v)
 (dotimes (i 13)
   (when (= 2 (aref v i)) (return-from pair? i))))

(defun overpair?(v1 v2)
 (let ((p (pair? v1)))
   (and p (dotimes (i p t)
            (when (plusp (aref v2 i)) (return-from overpair? nil))))))

;; ------------------------ PARAMETRY ---------------------------------

(defvar *logging* t)

(defparameter *table-size* 10) ; poczatkowa ilosc graczy
(defvar *table* nil)
(defvar *hand* nil)
(defvar *pot* 0)
(defvar *blind* 1)
(defvar *stake* 1)
(defvar *history* nil)
(defvar *still-playing* nil) ; I am still playing

(defvar *my-pos* nil) ; my position at the table
;; ------------------------  EVENTS -------------------------------------

(defun new-hand(&optional (number 10)) 
  (setq *table* nil *hand* nil 
        *pot* (* 1.5 *blind*) *stake* *blind* 
        *history* nil *still-playing* t 
        *table-size* number *my-pos* nil))


(defun hand(&rest h)
  (setq *hand* h))



(defun flop(&rest a)
  (when (and *hand* *still-playing*)
    (setq *table* a)
    (setq *stake* *blind*)
    (push 'flop *history*)))


(defun turn(a)
 (when (and *hand* *still-playing*)
  (push a *table*)
  (setq *stake* (* 2 *blind*))
  (push 'turn *history*)))


(defun river(a) 
 (when (and *hand* *still-playing*)
  (push a *table*)
  (setq *stake* (* 2 *blind*))
  (push 'river *history*)))


(defun check()
 (when *still-playing*
    (push 'check *history*)
    (incf *pot* *stake*)))

(defun bet()
 (when *still-playing*
    (push 'bet *history*)
    (if (<= (length *table*) 3) 
               (incf *stake*  *blind*) 
               (incf *stake* (* 2 *blind*)))
    (incf *pot* *stake*)))

(defun fold() 
 (when *still-playing*
   (push 'fold *history*)))

;; --------------------  ACTONS  ------------------------------------------

(defun action()
 (if (zerop (length *table*))
       (progn (my-pos)(pre-flop-eval))
       (simple-game-eval)))



(defmacro without-bet(&rest body)
 `(and (zerop (count-bets))
      (or ,@body )))

(defmacro with-bet(&rest body)
 `(and (= 1 (count-bets))
      (or  ,@body )))

(defmacro with-raise(&rest body)
 `(and (= 2 (count-bets))
      (or  ,@body )))

(defmacro with-reraise(&rest body)
 `(and (= 3 (count-bets))
      (or  ,@body )))


(defun fold!()
  ;no bet no fold policy:
  (cond
     ((and (no-bets?)  (not (pre-flop?)))  "check")
     ((and (no-bets?)  (eq (my-pos) 'big-blind) (pre-flop?)) "check")
     (t  (setq *still-playing* nil) "fold")))



;dostepne komunikaty: s1-s11
(defun pre-flop-eval()
  (let ((skl (sklansky)))
     (or 
        (and (null skl) (logs 's1) (fold!))


        (and (eq (my-pos) 'early-pos)
             (or
               (without-bet
                   (and (logs 'searly0) nil)
                   (and (= 1 skl) "bet")
                   (and (<= skl 4) "check")
                   "fold") 
               (and (one-bet?) (logs 'searly1) (if (<= skl 3) "check" "fold"))
               (and (> (count-bets) 1) (logs 'searly2)  (if (<= skl 2) "check" "fold") )))

        (and (eq (my-pos) 'middle-pos)
             (or
               (without-bet
                   (and (logs 'smid0) nil)
                   (and (= 1 skl) "bet")
                   (and (<= skl 6) "check")
                   "fold") 
               (and (one-bet?) (logs 'smid1) (if (<= skl 4) "check" "fold"))
               (and (> (count-bets) 1) (logs 'smid2)  (if (<= skl 2) "check" "fold") )))

        (and (eq (my-pos) 'late-pos)
             (or
               (without-bet
                   (and (logs 'slate0) nil)
                   (and (= 1 skl) "bet")
                   (and (<= skl 8) "check"))
               (and (one-bet?) (logs 'slate1) (if (<= skl 7) "check" "fold"))
               (and (> (count-bets) 1) (logs 'slate2)  (if (<= skl 4) "check" "fold") )))

        (and (eq (my-pos) 'small-blind)
             (or
               (without-bet
                   (and (logs 'sb0) nil)
                   (and (= 1 skl) "bet")
                   (and (<= skl 6) "check")
                   "fold") 
               (and (one-bet?) (logs 'sb1) (if (<= skl 6) "check" "fold"))
               (and (> (count-bets) 1) (logs 'sb2)  (if (= 1 skl) "check" "fold") )))

        (and (eq (my-pos) 'big-blind)
             (or
               (and (no-bets?) (logs 'bb0) (if (= 1 skl) "bet" "check")) 
               (and (one-bet?) (logs 'bb1) (if (<= skl 6) "check" "fold"))
               (and (> (count-bets) 1) (logs 'bb2)  (if (= 1 skl) "check" "fold") )))

        (progn (logs 's11) (fold!)) )))

(defmacro with-odds(&rest body)
  `(let ((podds (pot-odds)) (odds (odds)))
     (or
       ,@body )))


(defun simple-game-eval()
 (or
  (and (< (random 100) 80)
       (with-cards
         (with-reraise    
           (and (<= (first ocena) 6) (logs 'r13) "check")
           (progn (logs 'r14) "fold")   )
         (with-raise             
            (and (<= (first ocena) 5) (logs 'r15) "check") 
            (and (= (first ocena) 6) (logs 'r16) "check")
	    (progn (logs 'r17) "fold") )
         (with-bet
            (and (or overpair?  (<= (first ocena) 6)) (logs 'r18) "bet")
            (and with-kicker? (logs 'r19) (if (zerop (random 3)) "bet" "check"))
            (progn (logs 'r20) "fold")        )
          ;without-bet: 
          (or  (and (or top-pair? (<= (first ocena) 6)) (logs 'r5) "bet" )
               (progn (logs 'r21) "check")    ))
       )
  (with-odds
    (and (> odds podds) (logs 'r7) (fold!))
    (and (plusp (count-bets))
         (or
           (and (> odds (* 0.6 podds)) (logs 'r8) (fold!))
           (and (> odds (* 0.2 podds)) (logs 'r9) "check")
           (progn (logs 'r10) "bet") ))
    (and (< odds (* 0.4 podds)) (logs 'r11) "bet")
    (progn (logs 'r12) "check")    )))


(defun show-stats()
  (format t "~A ~A ~A ~%" *hand* *table* *history*))

;; ------------------------------------------------------------------------

(defun round-history(&optional (r (game-round)) (h *history*))
  (if (eq r 'pre-flop) 
       h
       (subseq h 0 (position r h))))

(defun players-left(&optional (h *history*))
 (- *table-size*  (count 'fold h)))

(defun game-round()
  (case (length *table*)
    (0 'pre-flop)
    (3 'flop)
    (4 'turn)
    (5 'river)) )


(defun odds()
   (wynik->odds (pojedynek :ilosc-powtorzen 3000 :ilosc-graczy (players-left))))

(defun pot-odds()
   (/ *pot* *stake*))

(defun count-bets(&optional (r (game-round)))
 (count 'bet (round-history r)))




(defun wyjmij(skad &rest listy)
 "Usuwa z listy <skad> elementy z list <listy>"
  (dolist (l listy)
    (dolist (el l)
      (setf skad (remove el skad)))) 
  skad)

(defun pojedynek(&key (reka *hand*) (stol *table*) (ilosc-graczy 1) (ilosc-powtorzen 1))
 (when (< ilosc-graczy 1) (setq ilosc-graczy 1))
 (let ( (reszta (wyjmij (talia) reka stol))
        (rece  (make-array (1+ ilosc-graczy))) 
        (oceny (make-array (1+ ilosc-graczy))) 
        (wygrana 0)(remis 0)(porazka 0) (zwyciezcy nil))
  (setf (aref rece 0) reka)
  (dotimes (i ilosc-powtorzen)
    (tasuj reszta)  
    (dotimes (r ilosc-graczy)
         (setf (aref rece (1+ r))
            (subseq reszta (* 2 r) (* 2 (1+ r)))  ))                
    (dotimes (r (1+ ilosc-graczy))
           (setf (aref oceny r) (ocena (append (aref rece r) stol)) ))
    (setq zwyciezcy (zwyciezca (coerce oceny 'list)))
    ;(format t "~A ~A ~A~&" rece stol zwyciezcy)
    (if (find 0 zwyciezcy)
        (if (> (length zwyciezcy) 1) 
                              (incf remis) 
                              (incf wygrana))
        (incf porazka))  )
  (list wygrana remis porazka))) 



(defun sklansky-group(karta1 karta2)
 "karta1, karta2 to symbole kart na rece"
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

;;-----------------  inne ------------------

(let ((log-messages nil))

 (defun logs(m)
   (and *logging* (push (format nil "[~A/~A] ~A ~A ~A ~A" (my-pos) *table-size* (liczby->opis *hand*) 
                                                  (liczby->opis *table*)
                                                   *history* m) log-messages))
   'ok)

 (defun clear-logs()
   (setq log-messages nil))

 (defun show-logs()
  (princ log-messages) 'ok)

 (defun write-logs()
  (with-open-file (f "logs.dat" :direction :output :if-does-not-exist :create :if-exists :supersede)
 (dolist (el log-messages)
      (format f "~A~%"  el))))
)