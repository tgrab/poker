(defstruct player
  name
  money
  cards
  seat
  (balance 0)
  (total-balance 0)
  (playing? t)
  history)


(defstruct game
  id
  table-name
  time
  sblind
  bblind
  kind
  (players (make-list 10 :initial-element nil))
  (active-player 0)
  (active-players (make-list 10 :initial-element t))
  table-cards
  (pot 0)
  (stake 0)
  history)



;;-------- OPISY KART ----------------------------------------

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
(defun karta->kolor(liczba)
  (floor (1- liczba) 13))

(defun karta->figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a zeby nie bylo ostrzezen
   b))

(defun karta->napis (liczba)
   (format nil "~A~A" 
	   (nth (karta->figura liczba) *figury*)
	   (nth (karta->kolor liczba) *kolory*)    ))

(defun karty->napisy(lista-liczb)
 (mapcar #'karta->napis lista-liczb))

(defun karta->html(liczba)
 (format nil 
	 (nth (karta->kolor liczba) *color-htm*) 
	 (nth (karta->figura liczba) *figury*) ))

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


;;-----------------------------------------------------------------------------------

(defun next-player(game &optional (biezacy (game-active-player game)))
  ;jesli zostal tylko jeden:
  (when (= 1 (count-if #'identity (game-active-players game)))
    (return-from next-player (position-if #'identity (game-active-players game))  ))
  (when (= biezacy 9)
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr (game-active-players game))    
      nr
      (next-player game nr)) ))

(defmacro get-active-player()
  '(nth (game-active-player game) (game-players game)))

(defmacro find-player(game name)
  `(find ,name (game-players ,game) :test #'equal :key #'(lambda(p) (when p (player-name p)))))

(defun count-stake(game plr)
  (+ (game-stake game) (player-balance plr)))

(defun safe-ratio(num den)
  (if (plusp den)
      (/ num den)
      1000))

(defun count-pot-odds(game plr)
  (let ((stawka (count-stake game plr)))
    (safe-ratio (game-pot game) stawka)))

(defun count-agent-pot-odds()
  (if (and +pos+ (nth +pos+ (game-players +game+)))
      (count-pot-odds 
         +game+ 
         (nth +pos+ (game-players +game+)))
      (safe-ratio 
         (game-pot +game+)
         (game-stake +game+) ))  )


(defun set-percept(game name amount percept-symbol)
  (let ((pl
	 (cond
	   ((game-table-cards game) (get-active-player)) ; byl flop gracze sa okresleni
	   ((find-player game name))
	   (t (setf (get-active-player)
		    (make-player :name name :seat (game-active-player game))))    )))
    (setf (game-active-player game) (player-seat pl))
    (push percept-symbol (player-history pl))
    (push percept-symbol (game-history game))
    (incf (game-pot game) amount)
    (decf (player-balance pl) amount)
    (when (eq percept-symbol 'f)
      (setf (player-playing? (get-active-player)) nil)
      (setf (nth (game-active-player game) (game-active-players game)) nil)   )
    (setf (game-active-player game) (next-player game))   ))

(defun set-round-percept(game round &rest cards)
  (setf (game-table-cards game)
	(nconc cards (game-table-cards game)))

  (setf (game-active-players game)
	(mapcar #'(lambda(p) (when (and p (nth (player-seat p) (game-active-players game)) (player-playing? p)) t))
		(game-players game)))

  (setf (game-active-player game)
	(next-player game -1))
  (setf (game-stake game) 0)
  (dolist (p (game-players game))
    (when (and p (player-playing? p))
      (incf (player-total-balance p) (player-balance p))
      (setf (player-balance p) 0)
      (push round (player-history p))))
  (push round (game-history game))  )

(defun percept-small-blind(game name amount)
  (setf (game-sblind game) amount
	(game-stake game) amount)
  (set-percept game name amount 'sb))

(defun percept-big-blind(game name amount)
  (setf (game-bblind game) amount
	(game-stake game) amount)
  (set-percept game name amount 'bb))

(defun percept-fold(game name)
  (set-percept game name 0 'f))

(defun percept-call(game name amount)
  (set-percept game name amount (cons 'c amount)))

(defun percept-check(game name)
  (percept-call game name 0))

(defun percept-bet(game name amount)
  (setf	(game-stake game) amount)
  (set-percept game name amount (cons 'b amount)))

(defun percept-raise(game name amount)
  (setf	(game-stake game) amount)
  (set-percept game name amount (cons 'r amount)))

(defun percept-allin(game name amount)
  (when (> amount (game-stake game))
    (setf (game-stake game) amount))
  (setf (nth (game-active-player game) (game-active-players game)) nil)
  (set-percept game name amount (cons 'a amount)))

(defun percept-flop(game c1 c2 c3)
  (set-round-percept game 'flop c1 c2 c3))

(defun percept-turn(game c1)
  (set-round-percept game 'turn c1))

(defun percept-river(game c1)
  (set-round-percept game 'river c1))

(defvar +game+ (make-game :kind 3))
(defvar +game-kind+ 3)
(defvar +cards+ nil)
(defvar +pos+ nil)
(defvar +hs+ nil)
(defvar +game-history+ nil)

(defun return-action()
  ; our position:

  )


;; ---------------------------------------------------------------------
;; --------------------------- OMAHA Code ------------------------------
;; ---------------------------------------------------------------------

(defvar *h*  (make-hash-table :test #'equal) "Baza wynikow pojedynkow")

(defun wczytaj-baze(plik)
   (with-open-file (f plik)
      (do
           ( (klucz (read f nil) (read f nil)) 
             (wartosc (read f nil)(read f nil)))
           ( (null klucz) )
           (setf (gethash klucz *h*) wartosc)  )    ))


(defun log-odds(l)
  (if (plusp (first l)) 
      (/ (third l) (+ (first l) (* 0.1 (second l))))
      1000))

;gramy to co ma oddsy<7.6 jest to 1/5 zbioru