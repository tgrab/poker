;use s-xml
(defpackage :dao
  (:use :cl :sqlite :moje)
  (:export :seat :xml2info :gameinfo))

(in-package :dao)


(defvar *HistoryFile* "/home/tgrab/GameHistory.dat")

(defun my-query(q) 
  (with-open-database (con *HistoryFile*) 
    (execute-to-list con q)))


(defun select-all-games(&optional ilosc)
  (mapcar #'(lambda(s) (s-xml:parse-xml-string (car s)))
	  (if ilosc
	      (my-query 
	       (format nil "SELECT XMLDump FROM HandHistory ORDER BY HandDate DESC LIMIT 0,~A" ilosc))
	      (my-query "SELECT XMLDump FROM HandHistory ORDER BY HandDate DESC"))))

(defstruct seat
  num
  alias
  balance
  endbalance
  cards
  pfb ;preflop-bet
  vol ; vol-in-pot
)

(defclass game-info()
  ((id :initarg :id :accessor id)
   (date :initarg :date :accessor date)
   (gametype :initarg :gametype :accessor gametype)
   (tablename :initarg :tablename :accessor tablename)
   (seats :initform (make-array 10 :initial-element nil) :accessor seats)
   (name2seat :initform nil :accessor name2seat)
   (flop :initform nil :accessor flop)
   (turn :initform nil :accessor turn)
   (river :initform nil :accessor river)
   (cards :initform nil :accessor cards)
   (actions :initform nil :accessor actions)
   ;simulations TODO remove:
;   (action-nr :initform -1 :accessor action-nr)
;   (game :initform nil :accessor game)
     ))


(defun xml2seat(x)
  (make-seat :num (parse-integer (nth 2 x))
	     :alias (nth 4 x)
	     :balance  (read-from-string (nth 8 x))
	     :endbalance  (read-from-string (nth 10 x))))

(defun xml2card(l)
  (let ((v (nth 2 l))
	(s (nth 4 l)))
    (let ((v2
	   (cond
	     ((string= v "2") 13)
	     ((string= v "3") 12)
	     ((string= v "4") 11)
	     ((string= v "5") 10)
	     ((string= v "6") 9)
	     ((string= v "7") 8)
	     ((string= v "8") 7)
	     ((string= v "9") 6)
	     ((string= v "10") 5)
	     ((string= v "J") 4)
	     ((string= v "Q") 3)
	     ((string= v "K") 2)
	     ((string= v "A") 1)    ))
	  (s2
	   (cond
	     ((string= s "c") 3)
	     ((string= s "d") 2)
	     ((string= s "h") 1)
	     ((string= s "s") 0)    )))
      (+ (* 13 s2) v2))))

(defun info-get-player(name game)
  (find name (seats game) :test #'equal :key #'(lambda (s) (when s (seat-alias s)))))

(defun info-balance(seat)
  (if seat
      (- (seat-endbalance seat) (seat-balance seat))
      0))

(defun xml2info(xml)
  (let* ((game (first xml))
	 (seats (mapcar #'car (cdr (second xml))))
	 (gameplay (cdr (third xml)))
	 (info (make-instance 'game-info
			      :id (nth 2 game)
			      :date (nth 4 game)
			      :gametype (nth 18 game)
			      :tablename (format nil "(~A)~A" (nth 10 game) (nth 8 game)))) )



    (dolist (xs seats)
      (let ((s (xml2seat xs)))
	(push (cons (seat-alias s) (1- (seat-num s))) (name2seat info))
	(setf (aref (seats info) (1- (seat-num s))) s)  ))

    (when (not (string= "Omaha" (gametype info))) (return-from xml2info info))

    (flet ((alias (int)
	     (seat-alias (aref (seats info) (1- (parse-integer int)))))
	   (val (v) (read-from-string v))
	   (attr (name l)  (awhen (position name l) (nth (1+ it) l)) ))
      (dolist (action gameplay)
      ;(format t "~A ~A~&" action (attr :|type| (car action)))
	(let* ((a (car action))
	       (type (attr :|type| (car action)) )	   
	       (el (cond
		     ((string= type "SmallBlind") (list 'sb (alias (nth 6 a)) (val (nth 8 a)))) 
		     ((string= type "BigBlind")   (list 'bb (alias (nth 6 a)) (val (nth 8 a)))) 
		     ((string= type "Bet")   (list 'bet (alias (nth 6 a)) (val (nth 8 a))))
		     ((string= type "Raise")   (list 'raise (alias (nth 6 a)) (val (nth 8 a))))
		     ((string= type "Call")   (list 'call (alias (nth 6 a)) (val (nth 8 a))))
		     ((string= type "Check")   (list 'call (alias (nth 6 a)) 0))
		     ((string= type "Fold")   (list 'fold (alias (nth 6 a))))
		     ((string= type "ShowCards") (cons 
						  'cards
						  (cons (alias (nth 6 a))
						       (sort
							(mapcar #'xml2card 
								(mapcar #'car (cdr action)))
							#'<))) )
		     ((string= type "DealCards") (cons 'holecards
						       (sort (mapcar #'xml2card 
							       (mapcar #'car (cdr action)))
							     #'<)) )
		     ((string= type "DealFlop") (cons 'flop
						       (mapcar #'xml2card 
							       (mapcar #'car (cdr action)))) )
		     ((string= type "DealTurn") (cons 'turn (mapcar #'xml2card
								    (mapcar #'car (cdr action)))) )
		     ((string= type "DealRiver") (cons 'river (mapcar #'xml2card
								    (mapcar #'car (cdr action)))) )
		     (t '(unknown))     )))

	  (when (null (flop info)) ;preflop
	    (when (or (eq 'bet (first el)) (eq 'raise (first el))) ;preflop-bet
	      (setf (seat-pfb (info-get-player (second el) info)) t))

	    )

	  (cond
	   ((eq (first el) 'cards) (setf (seat-cards (info-get-player (second el) info)) (cddr el)) )
	   ((eq (first el) 'holecards) (setf (cards info) (cdr el)))
	   ((eq (first el) 'flop) (setf (flop info) (cdr el)) (push '(flop) (actions info)))
	   ((eq (first el) 'turn) (setf (turn info) (cadr el)) (push '(turn) (actions info)))
	   ((eq (first el) 'river) (setf (river info) (cadr el)) (push '(river) (actions info)))
	   ((not (eq (first el) 'unknown))  (push el (actions info))) )

	  )))
    (setf (actions info) (reverse (actions info)))
    info))




