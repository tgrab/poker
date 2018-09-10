(defvar *conflicts* nil) ; error reports!
; Game globals
(defvar +agent-name+ "grabola")
(defvar +game-kind+ 3) ; 3 - Omaha Pot Limit
(defvar +name->seat+ nil) ; assoc table 
(defvar +history+ nil)
(defvar +table-cards+ nil)
(defvar +pot+ 0)
(defvar +stake+ 0)
(defvar +blind+ 0)
(defvar +taken-seats+ (make-array 10 :initial-element nil))
(defvar +table-size+ nil)
(defvar +table-size-alarm+ nil) ;(setq +table-size-alarm+ '(4 "192.168.0.10" 10000))
(defvar +active-players+ (make-array 10 :initial-element nil))
(defvar +free-seat+ 0)

;tables for every player:
(defvar +balance+ (make-array 10 :initial-element 0)) ;money spent in this round
(defvar +total-balance+ (make-array 10 :initial-element 0)) ;money spent in prev. rounds
(defvar +player-history+  (make-array 10 :initial-element nil))


; Agent's state:
(defvar +cards+ nil)
(defvar +pos+ nil)

; Evaluation globals, used by eval-table

(defvar *h*  (make-hash-table :test #'equal) "Baza wynikow pojedynkow")
(defvar +hs+ nil)

(defvar +rank+ nil)
(defvar +colhand+ nil) ;wektor kolorow na rece
(defvar +coltable+ nil) ;wektor kolorow na stole
(defvar +vhand+ nil)
(defvar +vtable+ nil)
(defvar H1 nil)
(defvar H2 nil)
(defvar H3 nil)
(defvar H4 nil)
(defvar T1 nil)
(defvar T2 nil)
(defvar T3 nil)
(defvar T4 nil)
(defvar T5 nil)

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


(defun send-message(host port msg)
  #+cmu(handler-case
	 (let* ((fd (ext:connect-to-inet-socket host port))
		(str (system:make-fd-stream fd :output t :buffering :none)))
	   (format str "~A" msg)
	   (close str))
	 (error (c) (format t "BLAD ~A~&" c))))
	 


;;--------------------------------------------------------------------------
;;------------------------------- Game Logic -------------------------------
;;--------------------------------------------------------------------------

; rezygnujemy z okreslania active-playera wykorzystujemy <player-pos>
(defun show-game()
  (format t "~A <br>~%~A<br>~%~A<br>~%~A<br>~%~A<br>~%~A<br>~%~A<br>~%" 
	  (reverse +name->seat+) 
	  +taken-seats+
	  +active-players+
	  +balance+
	  +total-balance+
	  +history+
	  +player-history+))

(defun preflop?()
  (null +table-cards+))

(defun flop?()
  (= 3 (length +table-cards+)))

(defun turn?()
  (= 4 (length +table-cards+)))

(defun river?()
  (= 5 (length +table-cards+)))

(defun game-round()
  (case (length +table-cards+)
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

(defun round-history(&optional (r (game-round)) (h +history+))
  (cond
    ((eq r 'pre-flop)  (if (preflop?) ;sprawdzamy czy sa nowe rundy
                            h 
                            (subseq h (1+ (position 'flop h))))  )
   ((not (find r h)) nil)
   ((eq r 'river) (if (find 'river h)
                      (subseq h 0 (position 'river h))
                       nil ))
   (t        (if (find (next-round r) h)
                  (subseq h (1+ (position (next-round r) h)) (position r h))
                  (subseq h 0 (position r h))) ))) 

(defun bets(&optional (r (game-round)) (h +history+))
 (count-if #'(lambda(el)
	       (and (consp el)
		    (or (eq (car el) 'b) 
			(eq (car el) 'r)
			(eq (car el) 'a))))
	   (round-history r h)))

(defun no-bets?()
  (zerop (bets)))

(defun one-bet?()
  (= 1 (bets)))

(defun with-bets?()
  (>= (bets) 1))

(defun with-raise?()
  (> (bets) 1))


(defun smb->val(s)
    (case s
      ('highcard 8)
      ('pair 7)
      ('two-pairs 6)
      ('trips 5)
      ('straight 4)
      ('color 3)
      ('fullhouse 2)
      ('quads 1)
      ('strflush 0)
      ('< #'<)
      ('> #'>)
      ('<= #'<=)
      ('>= #'>=)
      ('= #'=)))


(defmacro rank (op rnk)
    `(when +rank+ 
      (funcall (smb->val ',op) (smb->val ',rnk) (car +rank+) )))

(defun player-pos(name &optional (ta +name->seat+))
  (cdr (assoc name ta :test #'string=)))

(defun agent-pos()
  (or +pos+ (player-pos +agent-name+)))

(defun stake(&optional (ppos (agent-pos)))
  (if ppos
       (+ +stake+ (aref +balance+ ppos))
       +stake+))

(defun player-stake(name)
  (stake (player-pos name)))

(defun safe-ratio(num den)
  (if (plusp den)
      (/ num den)
      1000))

(defun pot-odds()
  (aif (agent-pos)
       (safe-ratio +pot+ 
		   (+ +stake+ (aref +balance+ it)))
       (safe-ratio +pot+ +stake+)))

(defun insert-player(name)
  (if (< +free-seat+ 10)
      (progn
	(when (string= name +agent-name+)
	  (setq +pos+ +free-seat+))
	(push (cons name +free-seat+) +name->seat+)
	(setf (aref +taken-seats+ +free-seat+) t)
	(setf (aref +active-players+ +free-seat+) t)
	(incf +free-seat+))
      (push (list name +free-seat+) *conflicts*)))

(defun insert-player?(name)
  (and (preflop?) 
       (not (assoc name +name->seat+ :test #'string=))
       (insert-player name)))

; Rank utilities:

; sluzy do sprawdzenia czy moze byc u kogos kolor:
; liczy maksymalna ilosc kart w kolorze na stole!
(defun max-of-colors(&optional (w-kolorow +coltable+))
  (apply #'max (coerce w-kolorow 'list) ))

(defun color-table?(&optional (w-kolorow +coltable+))
  (>= (max-of-colors w-kolorow) 3))

(defun no-color-table?(&optional (w-kolorow +coltable+))
  (<= (max-of-colors w-kolorow) 2))

(defun rainbow-table?(&optional (w-kolorow +coltable+))
  (= (max-of-colors w-kolorow) 1))

(defun pair-table?(&optional (vtable +vtable+))
  (>= (apply #'max (coerce vtable 'list) ) 2))

(defun trips-table?(&optional (vtable +vtable+))
  (= (apply #'max (coerce vtable 'list) ) 3))

(defun two-pair-table?(&optional (cards +table-cards+))
  (= 6 (car (ocena cards))))

(defun no-pair-table?(&optional (vtable +vtable+) )
   (<= (apply #'max (coerce vtable 'list) ) 1))

(defun on-small-blind?()
  (when +pos+ (zerop +pos+)))

(defun on-big-blind?()
  (when +pos+ (= 1 +pos+)))

(defun odds(num-of-outs)
  (/ (- 52 (length +cards+) (length +table-cards+) num-of-outs)
     num-of-outs))

(defun good-odds?(num-of-outs)
  (and (plusp num-of-outs)
       (> (pot-odds) (odds num-of-outs))))

(defun log-odds(l)
  (if (plusp (first l)) 
      (/ (third l) (+ (first l) (* 0.1 (second l))))
      1000))

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
 (let ((kolor1 (karta->kolor karta1)) 
       (figura1 (nth (karta->figura karta1) *figury*))
       (kolor2 (karta->kolor karta2))
       (figura2 (nth (karta->figura karta2) *figury*)))
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


(defun hs(&optional (cards +cards+))
  (when cards
    (if (= 3 +game-kind+)
	;omaha
	;rezygnujemy z czytania logu
	;TODO odzyt z postgresa:
	;(log-odds (gethash (sort (copy-list cards) #'<) *h*))
	108
	;holdem
	(sklansky-group (first cards) (second cards)))))
	      


;wywolywane po flopie,turnie i rivierze:
(defun eval-table()
  (when (and +cards+ +table-cards+)
    (let ((stol  (sort (mapcar #'karta->figura +table-cards+) #'<) )
	  (reka  (sort (mapcar #'karta->figura +cards+) #'<) ))
      (setq H1 (nth 0 reka) H2 (nth 1 reka))
      (setq H3 (nth 2 reka) H4 (nth 3 reka))
      (setq T1 (nth 0 stol) T2 (nth 1 stol) T3 (nth 2 stol))
      (setq T4 (nth 3 stol) T5 (nth 4 stol)))
    (setq +rank+ (if (= 3 +game-kind+)
		     (omaha-ocena +cards+ +table-cards+)
		     (ocena (append +cards+ +table-cards+)))
	  +vhand+ (wektor-figur +cards+)
	  +vtable+ (wektor-figur +table-cards+)
	  +colhand+ (wektor-kolorow +cards+)
	  +coltable+ (wektor-kolorow +table-cards+)) ))


(defun small-blind(name amount)
  ;zerujemy wszystko:
  (setq +name->seat+ nil +table-cards+ nil +pot+ 0 +stake+ 0
	+taken-seats+ (make-array 10 :initial-element nil)
	+active-players+ (make-array 10 :initial-element nil)
	+free-seat+ 0 +history+ nil
	+player-history+  (make-array 10 :initial-element nil)
	+total-balance+ (make-array 10 :initial-element 0)
	+balance+ (make-array 10 :initial-element 0)
	+cards+ nil +pos+ nil h1 nil h2 nil h3 nil h4 nil
	t1 nil t2 nil t3 nil t4 nil t5 nil +hs+ nil )
  (setq +pot+ amount +stake+ amount +blind+ amount)
  (setf (aref +balance+ 0) (- amount)) 
  (push (cons 'sb amount) +history+)
  (push (cons 'sb amount) (aref +player-history+ 0))
  (insert-player name))

(defun big-blind(name amount)
  (incf +pot+ amount)
  (setq +stake+ amount +blind+ amount)
  (setf (aref +balance+ 1) (- amount))
  (push (cons 'bb amount) +history+)
  (push (cons 'bb amount) (aref +player-history+ 1))
  (insert-player name))

(defun call(name amount)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push (cons 'c amount) +history+)
	  (push (cons 'c amount) (aref +player-history+ nr))
	  (decf (aref +balance+ nr) amount)
	  (incf +pot+ amount))
	(push (list 'call name) *conflicts*))))


(defun fold(name)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push 'f +history+)
	  (push 'f (aref +player-history+ nr))
	  (setf (aref +active-players+ nr) nil)
	  'ok)
	(push (list 'fold name) *conflicts*)))   )



(defun bet(name amount &optional (smb 'b))
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push (cons smb amount) +history+)
	  (push (cons smb amount) (aref +player-history+ nr))
	  (decf (aref +balance+ nr) amount)
	  (setq +stake+ amount)
	  (incf +pot+ amount)
	(push (list 'bet name) *conflicts*)))))



(defun all-in(name amount)
  (insert-player? name)
  (let ((nr (player-pos name)))
    (if nr
	(progn
	  (push (cons 'a amount) +history+)
	  (push (cons 'a amount) (aref +player-history+ nr))
	  (decf (aref +balance+ nr) amount)
	  (when (> amount +stake+) (setq +stake+ amount))
	  (incf +pot+ amount)
	  (setf (aref +active-players+ nr) nil)
	  'ok))
	(push (list 'bet name) *conflicts*)))

(defun flop(c1 c2 c3)
 (setq
  +table-cards+ (list c1 c2 c3)
  +stake+ 0 )
 (push 'flop +history+)
 (dotimes (i 10)
   (when (aref +active-players+ i) (push 'flop (aref +player-history+ i)))
   (incf (aref +total-balance+ i) (aref +balance+ i))
   (setf (aref +balance+ i) 0))

   (let ((ts  (count-if #'identity +taken-seats+)))
     (when +table-size-alarm+
	 (let ((alarm-size (first +table-size-alarm+))
	       (alarm-host (second +table-size-alarm+))
	       (alarm-port (third +table-size-alarm+)))
	   (when (and  +table-size+ 
	              (<= +table-size+ alarm-size)
		      (<= ts alarm-size))
	     (send-message alarm-host alarm-port "sitout" )
	     (setq +table-size-alarm+ nil))))
     (setq +table-size+ ts))
 (eval-table))


(defun turn(c1)
 (setq +stake+ 0 )
 (push c1 +table-cards+)
 (push 'turn +history+)
 (dotimes (i 10)
   (when (aref +active-players+ i) (push 'turn (aref +player-history+ i)))
   (incf (aref +total-balance+ i) (aref +balance+ i))
   (setf (aref +balance+ i) 0))
 (eval-table))


(defun river(c1)
 (setq +stake+ 0 )
 (push c1 +table-cards+)
 (push 'river +history+)
 (dotimes (i 10)
   (when (aref +active-players+ i) (push 'river (aref +player-history+ i)))
   (incf (aref +total-balance+ i) (aref +balance+ i))
   (setf (aref +balance+ i) 0)) 
 (eval-table))


(defun holecards(&rest cards)
(let  ((reka  (sort (mapcar #'karta->figura cards) #'<) ))
      (setq H1 (nth 0 reka) H2 (nth 1 reka))
      (setq H3 (nth 2 reka) H4 (nth 3 reka)))
  (setq +cards+ cards
	+hs+ (hs)))



 


