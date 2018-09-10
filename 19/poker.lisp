(in-package :poker)

;sluza do ustalania parametrow nowych gier
(defvar *agent-name* "Grabol")
(defvar *table-name* "Unknown")
(defvar *casino*     "crypto")
(defvar *kind* 3)

(defclass game-info()
  ((casino :initform *casino*  :initarg :casino :accessor casino)
   (id :initform "0" :initarg :id :accessor id)
   (czas :initform (get-universal-time) :initarg :czas :accessor czas)
   (table-name :initform *table-name* :initarg :table-name :accessor table-name) 
   (kind :initform *kind* :initarg :kind :accessor kind) ;omaha pt limit
   (blind :initform 0 :initarg :blind :accessor blind)
   (table-size :initform 6 :initarg :table-size :accessor table-size) 
   ;new
   (pre-flop-bets :initform 0 :initarg :pre-flop-bets :accessor pre-flop-bets)
   (on-flop-players :initform 0 :initarg :on-flop-players :accessor on-flop-players)    ))

(defmethod print-object((g game-info) str)
  (format str "[~A ~A: ~A ~A] " (type-of g) (id g) (dekoduj-czas (czas g)) (table-name g)))

(defclass player-info(game-info)
  ((cards :initform nil :initarg :cards :accessor cards) 
   (pre-flop-actions :initform nil :accessor pre-flop-actions)
   (remarks :initform nil :accessor remarks)
   (balance :initform 0 :accessor balance)
   (gain :initform 0 :accessor gain)
   (uncalled-bets :initform 0 :accessor uncalled-bets)
   (bad-actions :initform nil :accessor bad-actions) ))

(defvar *player-infos* (make-hash-table :test #'equal)) ;kluczem name wartosc lista
(defvar *games-list* nil) ;lista obiektow typu game ktore sa analizowane


(defstruct logs
  (percept-nr -1)
  game-nr
  game)

(defvar *log* (make-logs)) ;tutaj kopiujemy gre z *games-list* i jest ona wyswietlana

(defclass game(game-info)
  ((pot :initform 0 :accessor pot)
   (stake :initform 0 :accessor stake)
   (balance :initform (make-array 10 :initial-element 0) :accessor balance)
   (total-balance :initform (make-array 10 :initial-element 0) :accessor total-balance)
   (history :initform nil :accessor history)
   ;j.w. ale z info o graczu:
   (history-log :initform nil :initarg :history-log :accessor history-log)
   (free-seat :initform 0 :accessor free-seat)
   (agent-name :initform *agent-name* :initarg :agent-name :accessor agent-name)
   (agent-pos :initform nil :initarg :agent-pos :accessor agent-pos)
   (name->seat :initarg :name->seat :initform nil :accessor name->seat)
   (taken-seats :initform (make-array 10 :initial-element nil) :accessor taken-seats)
   (active-players :initform (make-array 10 :initial-element nil) :accessor active-players)
   (table-cards :initarg :table-cards :initform nil :accessor table-cards)
   (cards :initform nil :initarg :cards :accessor cards)
   ;--------------------------
   ;-------- Logic properties:
   (predicates :initform nil :allocation :class :accessor predicates)
   ;levels of memory:
   (rules :initform (make-hash-table) :allocation :class :accessor rules)
   (memo :initform (make-hash-table) :allocation :class :accessor memo)
   (game-memo :initform (make-hash-table) :accessor game-memo)
   (round-memo :initform (make-hash-table) :accessor round-memo)
   (percept-memo :initform (make-hash-table) :accessor percept-memo)
   ;Logowanie gier:
   (winners-log :initform nil :accessor winners-log) ;lista asocjacyjna gracz.zysk
   (uncalled-bets :initform nil :accessor uncalled-bets) ;lista asocjacyjna 
   (money-log :initform nil :initarg :money-log :accessor money-log) ;lista asocjacyjna z poczatkowymi pieniedzmi
   (cards-log :initform nil :initarg :cards-log :accessor cards-log))) ;lista asocjacyjna z kartami graczy



;----- Obliczane wspolczynniki gry -----------------
(defvar h1 nil) (defvar h2 nil) (defvar h3 nil) (defvar h4 nil)
(defvar t1 nil) (defvar t2 nil) (defvar t3 nil) (defvar t4 nil) (defvar t5 nil)
(defvar hc1 nil) (defvar hc2 nil) (defvar hc3 nil) (defvar hc4 nil)
(defvar tc1 nil) (defvar tc2 nil) (defvar tc3 nil) (defvar tc4 nil) (defvar tc5 nil)
(defvar flop1 nil) (defvar flop2 nil) (defvar flop3 nil)
(defvar flopc1 nil) (defvar flopc2 nil) (defvar flopc3 nil)
(defvar turn1 nil) (defvar turnc1 nil) (defvar river1 nil) (defvar riverc1 nil)
(defvar *o* '(100)) (defvar rank 0) ; rank=9-ocena
(defvar table-colors 0) (defvar table-pairs 0)  (defvar table-pairs2 0) (defvar bets 0)
(defvar players-left 0)

;; TODO locked type !!!!!:
(defvar ratio 0) ;get-stake/blind
;----------------------------------------------------
(defconstant highcard 0) (defconstant one-pair 1)  (defconstant two-pairs 2)
(defconstant trips 3) (defconstant str8  4) (defconstant color 5)
(defconstant fullhouse 6)  (defconstant quads 7) (defconstant str8flush 8)

(defvar +game+ (make-instance 'game))
(defvar *cortex* nil)

(defvar *speech* t)
(defvar *omaha-size-limit* 3)
(defvar *fixed-size-limit* 5)

(defvar *calendar* '((|January| . 31) (|February| . 28) (|March| . 31)
			   (|April| . 30) (|May| . 31)(|June| . 30)
			   (|July| . 31)(|August| . 31) (|September| . 30)
			   (|October| . 31) (|November| . 30)(|December| . 31) ))

;;---------  DB  -------------
(defun show-game(&optional (g +game+))
  (princ "{ 'game ")
  (format t ":id ~A " (id g))
  (format t ":czas ~A " (dekoduj-czas (czas g)))
  (format t ":table-name ~A " (table-name g))
  (format t ":casino ~A " (casino g))
  (format t ":kind ~A~&" (kind g))
  (format t ":history-log ~A~&" (history-log g))
  (format t ":cards-log ~A~&" (cards-log g))
  (format t ":agent-name ~A " (agent-name g))
  (format t ":table-cards ~A " (karty->napisy (table-cards g)))
  (format t ":cards ~A ~&" (karty->napisy (cards g)))
  (format t "--------- state ----------~&")
  (format t ":pot ~A " (pot g))
  (format t ":blind ~A " (blind g))
  (format t ":stake ~A ~&" (stake g))
  (format t ":balance ~A~&" (balance g))
  (format t ":total-balance ~A~&" (total-balance g))
  (format t ":free-seat ~A " (free-seat g))
  (format t ":agent-pos ~A " (agent-pos g))
  (format t ":name->seat ~A~&" (name->seat g))
  (format t ":history ~A~&" (history g))
  (princ "}") 'ok)



(defun get-game-schema(&optional (g +game+))
  (format nil "(~A,~A,'~A',~A,~A,~S,'~S','~S','~S','~S','~S','~S')" 
	 ;(if (string= "Crypto" (casino g)) 1 2  );1-crypto 2-prima
	  (id g) (czas g) (table-name g) (kind g) (blind g)
	  (table-size g) (name->seat g) (cards g) (table-cards g)
	  (cards-log g) (history-log g) (money-log g)
	  ))

(defun save-game(&optional (g +game+))
  (db:poker-update (format nil "insert into ~A values ~A" (casino g) (get-game-schema g)) ))

(defun load-game(id &optional (cas *casino*))
  (flet ((rs (s) (if (null s) nil (read-from-string s))))
  (let ((res (db:poker-query (format nil "select * from ~A where id=~A" cas id))))
    (when res 
      (let ((g (car res)))
	(make-instance 'game :id id :czas (nth 1 g)  :table-name (nth 2 g)
		        :kind (nth 3 g)  :blind (nth 4 g)  :table-size (nth 5 g)
			:name->seat (rs (nth 6 g))  :cards (rs(nth 7 g))  
			:table-cards (rs (nth 8 g)) :cards-log (rs (nth 9 g))
			:history-log (rs (nth 10 g)) :money-log (rs (nth 11 g))  ))))))

;;----------------------------------------------------------
;;------------------- CARDS Descr. -------------------------
;;----------------------------------------------------------

(defvar *kolory* '(S H D C))

(defvar *figury* '(A K Q J T 9 8 7 6 5 4 3 2))

(defvar *color-htm* '("<li class='spades'>~A&#9824;"
		      "<li class='hearts'>~A&#9829;" 
		      "<li class='diamonds'>~A&#9830;"
		      "<li class='clubs'>~A&#9827;" ))

(defvar *ranks* '("Straight Flush" "Quads" "Full House" "Color" "Straight" 
		  "Tripple" "Two pairs" "One pair" "High card"))

;karty to liczby od 1 do 52, 1=As Pik,2=Krol Pik,52=2 Trefl
;kolory: 0=Pik, 1=Kier, 2=Karo, 3=Trefl
;figury: 0=As, 1=Krol, 12=Dwojka

; karta to zawsze liczba z 1..52
(defun kolor(liczba)
  (floor (1- liczba) 13))

(defun figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a zeby nie bylo ostrzezen
   b))

(defun karta->napis (liczba)
   (format nil "~A~A" 
	   (nth (figura liczba) *figury*)
	   (nth (kolor liczba) *kolory*)    ))

(defun karty->napisy(lista-liczb)
 (mapcar #'karta->napis lista-liczb))

(defun karta->html(liczba)
 (if liczba
    (format nil 
	 (nth (kolor liczba) *color-htm*) 
	 (nth (figura liczba) *figury*) )
   ""))

(defun karty->html(liczby)
  (format nil "<ul class='karty'>~{~A~}</ul>" (mapcar #'karta->html liczby)))    


(defun napis->karta( napis )
 (let ((o1 (char  (string-upcase napis) 0))
       (o2 (char  (string-upcase napis) 1)))
   (+
     (coerce    ;zeby uniknac ostrzezen kompilatora  
       (case (char-code o1)
        (65 1) (75 2) (81 3) (74 4) (84 5) (57 6) (56 7) (55 8) (54 9)
        (53 10) (52 11) (51 12) (50 13)) 'number) 
     (* 13
       (coerce ;zeby uniknac ostrzezen kompilatora  
         (case (char-code o2)
           (83 0)(72 1)(68 2)(67 3))  'number)  )) ))

(defun napisy->karty(napisy)
  (mapcar #'napis->karta napisy))

(defun render-rank(ocena)
  (format nil "~A, ~A"
	  (nth (car ocena) *ranks*)
	  (mapcar  (lambda (o) (nth o *figury*)) (cdr ocena)) ))



;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;                                 Engine
;;------------------------------------------------------------------------------

(defun players-list(&optional ile)
  (let ((lista nil)
	(zrodlo (if ile  (subseq  *games-list* 0 ile) *games-list*)))
    (dolist (g zrodlo)
      (dolist (n (name->seat g))
	(pushnew (car n) lista :test #'string=)  ))
    (sort lista #'string<  )))


(defun omaha?()
  (= 3 (kind +game+)))

(defun fixed?()
  (= 2 (kind +game+)))

(defun holdem?()
  (<= (kind +game+) 2))

(defun preflop?()
  (null (table-cards +game+)))

(defun flop?()
  (= 3 (length (table-cards +game+))))

(defun turn?()
  (= 4 (length (table-cards +game+))))

(defun river?()
  (= 5 (length (table-cards +game+))))


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
    (pre-flop 'pre-flop)
    (flop 'pre-flop)
    (turn 'flop)
    (river 'turn)))

(defun next-round(smb)
  (case smb
    (pre-flop 'flop)
    (flop 'turn)
    (turn 'river)))

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

(defun bets-history (&optional (r (game-round)) )
  (mapcan #'(lambda (x) 
	      (when (member (second x) '(b r a))
		(list (car x))))
	  (round-history r (history-log +game+)))  )

(defun bets(&optional (r (game-round)) (h (history +game+)))
  "Zlicza podbicia w danej rundzie"
 (count-if #'(lambda(el)
	       (and (consp el)
		    (or (eq (car el) 'b) 
			(eq (car el) 'r)
			(eq (car el) 'a))))
	   (round-history r h)))

(defun check-raise?(&optional (r (game-round)) )
  (let* ((akcje (bets-history r))
	 (dl (length akcje)))
    (and akcje
	 (> dl 1) 
	 (> (nth (1- dl) akcje) (nth (- dl 2) akcje)) ) ))

  

(defun player-pos(name &optional (g +game+))
  (cdr (assoc name (name->seat g) :test #'string=)))

(defun player-at(nr &optional (g +game+))
  (car (find nr (name->seat g) :key #'cdr)))


(defun player-at-seat(nr name->seat)
  (car (find nr name->seat :key #'cdr)))

(defun player-money(name &optional (g +game+))
  ;TODO
  (let ((pocz (or (cdr (assoc name (money-log g) :test #'string=)) 0))
	(bal (aif (player-pos name)
		  (+ (aref (balance g) it)  (aref (total-balance g) it) )
		  0))  )
  (+ pocz bal)))

(defun player-history(player-nr &optional (g +game+))
  (mapcan
   (lambda(e)  (if (consp e) 
		   (when 
		       (= player-nr (first e))
		   (list (cdr e)))
		 ;flop ...
		   (list e)  ))
   (history-log g)))

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


(defun odds (outs)
  (if (= 3 (kind +game+))
      (safe-division (- 48 (length (table-cards +game+)) outs) outs)
      (safe-division (- 50 (length (table-cards +game+)) outs) outs)))       


(defun pot-odds()
  (safe-division (pot +game+) (get-stake)))

(defun count-active-players()
  (count-if #'identity (coerce (active-players +game+) 'list)))

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
		   (bets runda  (player-history i +game+)) )
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
;deprecated:
(defun opis-akcji(&optional (akcje (stos-akcji (game-round))))
  (block nil
    (when (equal akcje '(b ac)) (return 'b_ac))
    (when (equal akcje '(ac b)) (return 'ac_b))
    (when (equal akcje '(c b ac)) (return 'c_b_ac))
    (when (equal akcje '(c b c ac)) (return 'c_b_ac))
    (when (equal akcje '(c ac b)) (return 'c_ac_b))
    (when (equal akcje '(c ac)) (return 'c_ac))
    (when (equal akcje '(ac c)) (return 'ac_c))
    akcje))


(defun first-to-act?(player-pos)
  (or (zerop player-pos)
      (dotimes (i player-pos t)
	(when (aref (active-players +game+) i)
	  (return-from first-to-act? nil)))))

(defun last-to-act?(player-pos)
  (do ((i (1+ player-pos) (+ i 1)))
      ((> i (1- (table-size +game+))) t)
	(when (aref (active-players +game+) i)
	  (return nil)) ))



(defun last-action(&key player-name idx)
  (when player-name
    (setq idx (player-pos player-name)))
  (cond
    ((null idx) 'waiting)
    ((aref (active-players +game+) idx)
     (second
      (find-if	   
       #'(lambda (h) (when (consp h) (= idx (car h))))
       (history-log +game+) )))
    (t 'f)  ))

;;------------------------------------------------------------------------

(defun two-str8-table?(&optional (figury-stolu  (mapcar #'figura (table-cards +game+))))
  (when (>= (length figury-stolu) 3)
    (some
     #'(lambda (para)
	 (poker.ranks::street (wektor-figur 
			       (mapcar #'1+ 
				       (append para figury-stolu)))))
     (podzbiory-2  
      (set-difference 
       '(0 1 2 3 4 5 6 7 8 9 10 11 12)
       figury-stolu)) )))

(defun one-str8-table?(&optional (figury-stolu  (mapcar #'figura (table-cards +game+))))
  (when (>= (length figury-stolu) 4)
    (some
     #'(lambda (karta)
	 (poker.ranks::street (wektor-figur 
			     (mapcar #'1+ 
				     (cons karta figury-stolu)))))
     (set-difference 
      '(0 1 2 3 4 5 6 7 8 9 10 11 12)
      figury-stolu)) ))

;funkcje oceny profili graczy
;profile sa tworzone na podstawie historii lub ustawiane
;zewnetrznie

(let ((maniacs nil))
  (defun maniac?(player-name)
    ;TODO ocenic player-infos
    (find player-name maniacs :test #'string=)  )

  (defun add-maniac(player-name)
    (pushnew player-name maniacs :test #'string=))

  (defun get-maniacs()
    maniacs)

  )



