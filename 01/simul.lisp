(eval-when (compile load eval)
    (load  "poker.lisp"))


(defvar *wielkosc-stolu* 6)
(defvar *stol* (make-list *wielkosc-stolu*)) ;lista graczy siedzacych przy stole
(defvar *dealer* 0)
(defvar *biezacy* 1)
(defvar *pot* 0)
(defvar *blind* 1) ;wartosc zaciemnienia
(defvar sblind? nil) (defvar bblind? nil) ;czy w danym rozdaniu bylo zaciemnienie
(defvar *table* nil) ;lista kart na stole
(defvar *reka* nil) ;lista kart na rece
(defvar *ja* -1) ;polozenie gracza z rekomendacja

(defstruct player
 numer ;miejsce przy stole
 alias
 (akcje nil) ;lista wykonaych akcji: call,bet,fold,...
 (active? nil)
 (debt 0) 
)

(defun get-player(numer)
  (nth numer *stol*))

(defun pokaz-player(nr)
 (let ((player (get-player nr)))
 (when (null player)  (return-from pokaz-player (format nil "<td bgcolor=gray>[~A]" nr) ))
 (with-output-to-string(s)
   (if (player-active? player)  (format s "<td bgcolor=blue>")
                                (format s "<td bgcolor=gray>"))
 (format s "[~A]" nr)
 (if  (= *biezacy* (player-numer player))
        (format s "<font color=red><b>~A</b><font>" (player-alias player) )
        (format s "<b>~A</b>" (player-alias player) )     )
 (aif (player-akcje player)
        (format s " ~A " (first it)) ))))

(defun pokaz-stol-10()
 (with-output-to-string(s)
  (format s "<table border=1 width=100%><tr><td bgcolor=green>~A~A~A~A<td bgcolor=green>"(pokaz-player 0) (pokaz-player 1)
                                                         (pokaz-player 2)(pokaz-player 3))
  (format s "<tr>~A<td colspan=4 bgcolor=green>~A"(pokaz-player 9) (pokaz-player 4))
  (format s "<tr><td bgcolor=green>~A~A~A~A<td bgcolor=green>" (pokaz-player 8) (pokaz-player 7)
                                     (pokaz-player 6)(pokaz-player 5))
  (format s "</table>")
))

(defun pokaz-stol-6()
 (with-output-to-string(s)
  (format s "<table border=1 width=100%><tr>~A<td colspan=4 bgcolor=green>~A"
               (pokaz-player 0)   (pokaz-player 1))
   (format s "<tr><td bgcolor=green>~A~A~A~A<td bgcolor=green>" (pokaz-player 5) (pokaz-player 4)
                                     (pokaz-player 3)(pokaz-player 2))
  (format s "</table>")
))

(defun pokaz-stol()
 (with-output-to-string(s)
  (princ "<body bgcolor=green color=white>" s)
  (when (not (zerop *pot*))(format s "Pot: <font color=blue>~A</font><br>" *pot*))
  (when *reka* (format s "Hand: <font color=blue>~A</font><br>" (liczby->opis *reka*)))
  (when *table* (format s "Table: <font color=blue>~A</font><br>" (liczby->opis *table*)))
  (princ (pokaz-stol-6) s)
  (when (= *ja* *biezacy*)
        (format s "Recommend: ~A" (recommend)))
    
   (princ "</body>" s)))

(defun stol(&key (wielkosc *wielkosc-stolu*)  (blind *blind*))
 (setq *wielkosc-stolu* wielkosc *blind* blind)
 (setq *stol* (make-list wielkosc))
 (pokaz-stol) )

(defun sit-down(numer alias)
  (when (string= alias "grabola") (setq *ja* numer))
  (setf (nth numer *stol*) (make-player :numer numer :alias alias))
  (pokaz-stol))

(defun sit-up(numer)
  (setf (nth numer *stol*) nil)
  (when (= numer *biezacy*) (next-player!))
  (pokaz-stol))

(defun next-player(biezacy)
  (when (= biezacy (1- *wielkosc-stolu*))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if (and (nth nr *stol*) (player-active? (get-player nr)))
      nr
      (next-player nr)) ))

(defun next-player!()
  (setq *biezacy* (next-player *biezacy*)))
      
(defun dealer(numer)
 (dolist (p *stol*) (when p (setf (player-active? p) t) 
                            (setf (player-akcje p) nil )
                            (setf (player-debt p) *blind*)))
 (setq *dealer* numer sblind? nil bblind? nil *pot* 0 *reka* nil *table* nil)
 (push 'Dealer (player-akcje (get-player numer)))
 (setq *biezacy* (next-player numer))
 (pokaz-stol))

(defun next-hand()
 (dealer (next-player *dealer*) ))

(defun fold()
 (let ((p (get-player *biezacy*)))
   (next-player!)
   (setf (player-active? p) nil)
   (push 'Fold (player-akcje p))
   (pokaz-stol)))


(defun call()
 (let ((p (get-player *biezacy*)))
   (next-player!)
   (cond
     ((not sblind?) (setq sblind? t)(incf *pot* (/ *blind* 2.0))
                    (decf (player-debt p) (/ *blind* 2.0)) (push 'SBlind (player-akcje p)))
     ((not bblind?) (setq bblind? t) (incf *pot*  *blind*) (setf (player-debt p) 0)
                    (push 'BBlind (player-akcje p)))
     (t (incf *pot* (player-debt p)) (setf (player-debt p) 0)  (push 'Call (player-akcje p)))  )
   (pokaz-stol)))
   

(defun karta(numer)
  (if (and (> *ja* -1) (< (length *reka*) 2)) (push numer *reka*)
                            (push numer *table*))
   (when (>= (length *table*) 3) (setq *biezacy* (next-player *dealer*))
       (dolist (p *stol*) (when p (push (case (length *table*) (3 'flop)(4 'turn)(5 'river)) (player-akcje p)))))
   (pokaz-stol)) 

(defun bet(&optional (ile nil))
 (unless ile (if (<= (length *table*) 3) (setq ile *blind*)
                                         (setq ile (* 2 *blind*))  ))
 (incf *pot* (+ ile (player-debt (get-player *biezacy*))))
 (mapc #'(lambda(p) (when p (incf (player-debt p) ile))) *stol*)
 (setf (player-debt (get-player *biezacy*)) 0)
 (push (cons 'Bet ile) (player-akcje (get-player *biezacy*)))
 (next-player!)
 (pokaz-stol))

(defun preflop?()
  (< (length *table*) 3))
(defun flop?()
  (= (length *table*) 3))
(defun turn?()
  (= (length *table*) 4))
(defun river?()
  (= (length *table*) 5))

(defun recommend()
 (cond
  ((preflop?) (preflop))
  ((flop?) (flop))
  ((turn?) (turn))
  ((river?) (river))))

(defun count-active-players(&optional (lista *stol*))
 (if (null lista) 0
     (+ (count-active-players (cdr lista))
        (if (and (car lista) (player-active? (car lista))) 1 0))))

(defun preflop())
(defun flop())
(defun turn())
(defun river())