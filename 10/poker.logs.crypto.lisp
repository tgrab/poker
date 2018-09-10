;(declaim (optimize (speed 3) (safety 0)))

(defpackage :poker.logs.crypto
  (:use :cl :anaphor :poker.defs :poker.logs) )

(in-package :poker.logs.crypto)

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


;Game #2091631443: Hold'em NL (,B#(B0.15/,B#(B0.25) - 2006/08/01 - 22:35:53 (UK)
(defun recogn-game(dl tokens game)
  (when (and (= 10 dl) (string= "Game" (first tokens)))  
    (setf (game-id game) (string-trim "#:" (second tokens)))
    (setf (game-date game) (nth 6 tokens))
    (setf (game-time game) (nth 8 tokens))     ))

;Table "Virginie" Seat 2 is the button.
(defun recogn-table(dl tokens game)
  (when (and (string= "button." (car (last tokens))) 
	     (string= "Table" (first tokens)))   
    (setf (table-name game) (string-trim "#\"" (nth 1 tokens )))
    (setf (button game)
	  (1- (parse-integer (nth (- dl 4) tokens ) :junk-allowed t)))))

;Seat 1: Matsash (,B#(B26.25 in chips)
(defun recogn-seat(dl tokens game)
  (when (and (= 6 dl)
	     (string= "Seat" (first tokens))
	     (string= "chips)" (car (last tokens))))
    (let ((p (make-instance 'player
			    :name (third tokens) 
			    :money (money->number (subseq (nth 3 tokens) 1)))))
      (setf (players game)
	    (append (players game) (list  p)))
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
    (push 'f (history game))))


;RiverAce7: calls ,B#(B0.25	  
(defun recogn-calls(tokens game)
  (when  (or (string= "calls" (second tokens))
	     (string= "checks" (second tokens)))
    (push (list 'c (money->number (car (last tokens))))
	  (history game))))

;Wonderer: bets ,B#(B0.25
;Grabol: raises to ,B#(B1
(defun recogn-bets(tokens game)
  (when (or (string= "bets" (second tokens))
	    (string= "raises" (second tokens)))
    (push (list 'b (money->number (car (last tokens))))
	  (history game))))

;----- FLOP ----- [5h 2d 8d]
(defun recogn-flop(dl tokens game)
  (when (and (= 6 dl) (string= "FLOP" (second tokens)))
    (setf (table-cards game)
	   (napisy->karty (cdddr (remove-squares-from-tokens tokens))))
    (push 'flop  (history game))))

;----- TURN ----- [5h 2d 8d][Ks]
(defun recogn-turn(dl tokens game)
  (when (and (= 6 dl) (string= "TURN" (second tokens)))
    (setf (table-cards game)
	  (napisy->karty 
	   (reverse
	    (cdddr (remove-squares-from-tokens tokens)))))
    (push 'turn  (history game))))

;----- RIVER ----- [5h 2d 8d Ks][Js]
(defun recogn-river(dl tokens game)
  (when (and (= 7 dl) (string= "RIVER" (second tokens)))
    (setf (table-cards game)
	  (napisy->karty
	   (reverse
	    (cdddr (remove-squares-from-tokens tokens)))))
    (push 'river  (history game))))


(defun remove-return(line)
  (let ((dl (length line)))
    (if (eq #\Return (char line (1- dl)))
	(subseq line 0 (1- dl))
	line)))

(defun serve-log-game(file games-list)
 (let ((game (make-instance 'game)))
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
    

