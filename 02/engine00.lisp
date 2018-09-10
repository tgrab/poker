(defstruct (player (:print-function
		    (lambda (p s k)
		      k (format s "~A" (player-name p))
			(aif (player-money p) (format s " ~A " it))
			(aif (player-cards p) (format s "[~A]"(karty->opis it))))))

  "statyczny opis gracza"
  name
  money
  cards)


(defstruct game
  "pelna reprezentacja odbytej gry"
  id
  table-name
  date
  time
  structure ;list of blinds
  button
  history
  players
  size ;moze byc wyliczona z ilosci playersow
  cards)

; methods:
; sblind bblind get-player get-size

(defstruct agent
  game
  position
  still-playing?
  money
  actions
  stake
  active-player
  strategy)

; agent's methods:
; new-game,fold,bet,check,get-action
; <<uses>>:
; pot

;; ----------------------- Game Methods -----------------------------

(defun get-player (name game)
 "return player object from his name"
  (find name (game-players game) :test #'string= :key #'player-name))


(defun get-size(game)
  (or (game-size game)
      (length (game-players game))))



;; ----------------------- Log Viewer --------------------------------

(defun tokens (str &optional (delimiters (list #\Space))  (start 0))
  (let ((p1 (position-if #'(lambda (c) (not (find c delimiters)))
			 str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda(c) (find c delimiters))
			       str :start  (1+ p1))))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str delimiters (1+ p2))
		    nil)))
	nil)))

(defvar *kolory* '(S H D C))
(defvar *figury* '(A K Q J T 9 8 7 6 5 4 3 2))

;karty to liczby od 1 do 52, 1=As Pik,2=Krol Pik,52=2 Trefl
;kolory: 0=Pik, 1=Kier, 2=Karo, 3=Trefl
;figury: 0=As, 1=Krol, 12=Dwojka

(defun l->kolor(liczba)
  (floor (1- liczba) 13))

(defun l->figura(liczba)
 (multiple-value-bind (a b) (floor (1- liczba) 13) 
   a ;uzyjmy a ¿eby nie bylo ostrzezen
   b))

(defun napis->karta( opis )
 "(napis->karta ''as'') = 1"
 (let ((o1 (char  (string-upcase opis) 0))
       (o2 (char  (string-upcase opis) 1)))
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

(defun karta->napis (liczba)
   (format nil "~A~A" 
	   (nth (l->figura liczba) *figury*)
	   (nth (l->kolor liczba) *kolory*)    ))

(defun karty->napis(lista)
 (mapcar #'karta->napis lista))    



(defun remove-squares-from-tokens(tokens)
  (mapcan #'(lambda (token)
	      (tokens token '(#\[ #\])))
	  tokens))

(defun money->number(string)
  "$12.5 -> 12.5"
  (if (digit-char-p (char string 0))
      (read-from-string string) ;gdy sa chipsy w turnieju
      (if (digit-char-p (char string 1))
	  (read-from-string (subseq string 1))
	  0 )))


;Game #2091631443: Hold'em NL (£0.15/£0.25) - 2006/08/01 - 22:35:53 (UK)
(defun recogn-game(dl tokens game)
  (when (and (= 10 dl) (string= "Game" (first tokens)))  
    (setf (game-id game) (string-trim "#:" (second tokens)))
    (setf (game-date game) (nth 6 tokens))
    (setf (game-time game) (nth 8 tokens))     ))

;Table "Virginie" Seat 2 is the button.
(defun recogn-table(dl tokens game)
  (when (and (string= "button." (car (last tokens))) 
	     (string= "Table" (first tokens)))   
    (setf (game-table-name game) (string-trim "#\"" (nth 1 tokens )))
    (setf (game-button game)
	  (1- (parse-integer (nth (- dl 4) tokens ) :junk-allowed t)))))

;Seat 1: Matsash (£26.25 in chips)
(defun recogn-seat(dl tokens game)
  (when (and (= 6 dl)
	     (string= "Seat" (first tokens))
	     (string= "chips)" (car (last tokens))))
    (let ((p (make-player :name (third tokens) 
			  :money (money->number (subseq (nth 3 tokens) 1)))))
      (setf (game-players game)
	    (append (game-players game) (list  p)))
    )))

;dealt to Grabol [As 8h]
(defun recogn-my-cards(dl tokens game)
  (when (and (= dl 5) (string= "dealt" (first tokens)))
    (setf (player-cards (get-player (third tokens) game))
	  (napisy->karty (cdddr (remove-squares-from-tokens tokens))))))

;Grabol: shows [Ac 5d] (Two Pairs, Tens and Fives, Ace High)
(defun recogn-showdown(tokens game)
  (when (string= "shows" (second tokens))
    (let ((new-tokens (remove-squares-from-tokens tokens)))
      (setf (player-cards (get-player (string-right-trim ":" (first tokens)) game))
	  (napisy->karty (list (nth 2 new-tokens) (nth 3 new-tokens) ))))))


;Mrnormski: folds
(defun recogn-folds(dl tokens game)
  (when (and (= 2 dl) (string= "folds" (second tokens)))
    (push 'fold (game-history game))))


;RiverAce7: calls £0.25	  
(defun recogn-calls(tokens game)
  (when  (or (string= "calls" (second tokens))
	     (string= "checks" (second tokens)))
    (push (list 'call (money->number (car (last tokens))))
	  (game-history game))))

;Wonderer: bets £0.25
;Grabol: raises to £1
(defun recogn-bets(tokens game)
  (when (or (string= "bets" (second tokens))
	    (string= "raises" (second tokens)))
    (push (list 'bet (money->number (car (last tokens))))
	  (game-history game))))

;----- FLOP ----- [5h 2d 8d]
(defun recogn-flop(dl tokens game)
  (when (and (= 6 dl) (string= "FLOP" (second tokens)))
    (setf (game-cards game)
	   (napisy->karty (cdddr (remove-squares-from-tokens tokens))))
    (push 'flop  (game-history game))))

;----- TURN ----- [5h 2d 8d][Ks]
(defun recogn-turn(dl tokens game)
  (when (and (= 6 dl) (string= "TURN" (second tokens)))
    (setf (game-cards game)
	  (napisy->karty 
	   (reverse
	    (cdddr (remove-squares-from-tokens tokens)))))
    (push 'turn  (game-history game))))

;----- RIVER ----- [5h 2d 8d Ks][Js]
(defun recogn-river(dl tokens game)
  (when (and (= 7 dl) (string= "RIVER" (second tokens)))
    (setf (game-cards game)
	  (napisy->karty
	   (reverse
	    (cdddr (remove-squares-from-tokens tokens)))))
    (push 'river  (game-history game))))


(defun remove-return(line)
  (let ((dl (length line)))
    (if (eq #\Return (char line (1- dl)))
	(subseq line 0 (1- dl))
	line)))

(defun serve-log-game(file games-list)
 (let ((game (make-game)))
    (do ((line (read-line file nil 'no-more) (read-line file nil 'end)))
	((or (eq line 'no-more)
	     (eq line 'end)
	     (search "------HAND" line)) 
	 (if (eq line 'no-more)
	     games-list
	     (serve-log-game file (append (list game) games-list))))
      (let* ((words (tokens (remove-return line)))
	     (dl (length words)))
	(cond
	  ((recogn-bets  words game))
	  ((recogn-river dl words game))
	  ((recogn-turn dl words game))
	  ((recogn-flop dl words game))
	  ((recogn-folds dl words game))
	  ((recogn-calls  words game))
	  ((recogn-showdown words game))
	  ((recogn-seat dl words game))
	  ((recogn-my-cards dl words game))
	  ((recogn-table dl words game))
	  ((recogn-game dl words game))    )))))


(defun serve-log-file(name)
  (let ((games-list))
  (with-open-file (f name)
    (do ((line (read-line f) (read-line f nil 'end)))
	((or (eq line 'end)
	     (search "------HAND" line ))
	 (serve-log-game f games-list))))))

(defmacro get-games-from-log!(games! name)
  `(progn 
    (setq ,games! (serve-log-file ,name))
    (length ,games!)))
    

;; ----------------------- Agent Code --------------------------------

(defun size(agent)
  (get-size (agent-game agent)))


(defun next-player(agent biezacy)
  (when (= biezacy (1- (size agent)))
        (setq biezacy -1) )
  (let ((nr (1+ biezacy)))
  (if  (nth nr (agent-actions agent))
      nr
      (next-player agent nr)) ))

(defun new-game(agent game-size &key (sblind 0.5) ( bblind 1.0))
  (setf (agent-game agent) 
	(make-game :size game-size
		   :structure (cons sblind bblind) )

	(agent-actions agent)
	(make-list game-size :initial-element t)

	(agent-money agent) 
	(make-list game-size :initial-element 0)
	
	(agent-stake agent)	
	bblind )

    (setf (agent-active-player agent) (next-player agent 1))
    (setf (nth 0 (agent-money agent)) sblind)
    (setf (nth 1 (agent-money agent)) bblind)  )

(defun fold(agent)
  (setf (nth (agent-active-player agent) (agent-actions agent)) nil)
  (push 'f (game-history (agent-game agent)))
  (setf (agent-active-player agent)
	(next-player agent (agent-active-player agent)))   ))


(defun game-simulator(game)
  (let ((agent (make-agent))
	(history (reverse (game-history game))))
    (new-game agent (game-size game))
    #'(lambda (action)
	(case action
	  ('agent agent)
	  
	  ))))
