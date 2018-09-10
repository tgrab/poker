;; -------- funkcje bazy danych --------------
(defvar *h*  (make-hash-table :test #'equal) "Baza wynikow pojedynkow")


(defun podzbiory-2(lista)
"Zwraca liste podzbiorow dwu-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)) wynik)))
  wynik )) 

(defun inicjuj-baze() 
                (dolist (reka (podzbiory-2 (talia)))
                    (setf (gethash reka *h*) (list 0 0 0)))
  (wczytaj-baze "logs/texas10.dat"))



(defun wczytaj-baze(plik)
     (prin1 "Czytam baze")
     (with-open-file (f plik)
      (do
           ( (klucz (read f nil) (read f nil)) 
             (wartosc (read f nil)(read f nil)))
           ( (null klucz) )
           (setf (gethash klucz *h*) wartosc)  )    ))

;;-------------------------------------------
;;-------------------------------------------
;;                      serwer Texas Holdem'a

(defparameter *table-size* 10)
(defvar *table* nil)
(defvar *hand* nil)
(defvar *pot* 0)
(defvar *blind* 1)
(defvar *stake* 1)
(defvar *base-stake* 1) ;mozna usunac
(defvar *history* nil)
(defvar *still-playing* nil) ; I am still playing
(defvar *odds* 0) ; uzywamy aby nie liczyc tego dwa razy
(defvar *e* "current evaluation of hand") ; j.w.

(defun new-hand(&optional (number 10)) 
  (setq *table* nil *hand* nil *pot* (* 1.5 *blind*) *odds* 0
     *e* nil *base-stake* *blind* *history* nil *still-playing* t *table-size* number))


(defun action()
  (show-game)
  (case (length *table*)
    (0 (pre-flop-eval))
    (3 (simple-game-eval))
    (4 (simple-game-eval))
    (5 (simple-game-eval))))

(defun simple-game-eval()
 (let ((p (pot-odds)))
  (cond
   ((> *odds* p) (setq *still-playing* nil) "fold")
   ((and (plusp (count-bets)) (> *odds* (* 0.6 p))) (setq *still-playing* nil) "fold")
   ((> *odds* (* 0.3 p)) "check")
   ((= 1 (count-bets))  (if (mam-dwie-pary?) "bet" "check" ) )
   ((= 2 (count-bets))  "check" )
   (t  "bet"))   ))


(defun pre-flop-eval()
  (cond
   ((and (big-blind?) (zerop (count-bets))) (if (< *odds* 4) "bet" "check"))
   ((and (small-blind?) (zerop (count-bets)))   (if (< *odds* 3) "bet" "check")   )
   ((> *odds* 9) (setq *still-playing* nil) "fold")
   ((and (> *odds* 6)  (plusp (count-bets))) (setq *still-playing* nil) "fold")
   ((> *odds* 3) "check")
   (t "bet"))   )


(defun show-game()
  (format t "Hand: ~A, Table: ~A, Pot: ~A odds: ~A, pot-odds: ~A~%"
        (liczby->opis *hand*) (liczby->opis *table*) *pot*  *odds* (pot-odds))
  (when *e* (format t "~A~%" (opisz-ocene *e*))))

(defun pre-flop-odds(&optional (h *hand*))
  (when (zerop (hash-table-count *h*)) (inicjuj-baze))
  (if (< (first h) (second h))
            (wynik->odds (gethash h *h*))
            (pre-flop-odds (list (second h) (first h))))  )

(defun odds()
   (wynik->odds (pojedynek :ilosc-powtorzen 3000 :ilosc-graczy (players-left))))

(defun pot-odds()
   (/ *pot* *stake*))


(defun hand(&rest h)
  (setq *hand* h)
  (setq *odds* (pre-flop-odds h)))


(defun flop(&rest a)
  (when (and *hand* *still-playing*)
    (setq *table* a)
    (setq *stake* *base-stake*)
    (push 'flop *history*)
    (setq *e* (ocena (append *hand* *table*)))
    (setq *odds* (odds))))




(defun turn(a)
 (when (and *hand* *still-playing*)
  (push a *table*)
  (setq *base-stake* (* 2 *blind*))
  (setq *stake* *base-stake*)
  (push 'turn *history*)
  (setq *e* (ocena (append *hand* *table*)))
  (setq *odds* (odds))))


(defun river(a) 
 (when (and *hand* *still-playing*)
  (push a *table*)
  (setq *stake* *base-stake*)
  (push 'river *history*)
  (setq *e* (ocena (append *hand* *table*)))
  (setq *odds* (odds))))


(defun check()
 (when *still-playing*
    (push 'check *history*)
    (incf *pot* *stake*)))

(defun bet()
 (when *still-playing*
    (push 'bet *history*)
    (incf *stake* *base-stake*)
    (incf *pot* *stake*)))

(defun fold() 
 (when *still-playing*
   (push 'fold *history*)))

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

(defun count-bets(&optional (r (game-round)))
 (count 'bet (round-history r)))

; to ma sens tylko na pierwszym okrazeniu:
(defun small-blind?()
  (= (length *history*) (- *table-size* 2)))
(defun big-blind?()
  (= (length *history*) (- *table-size* 1)))


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


(defun mam-dwie-pary?(&optional (e *e*))
 (< (car e) 7))    
