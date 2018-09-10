(in-package :poker)

#+clisp(defvar no-bindings '((t . t)))
#+cmu(defconstant no-bindings '((t . t)))

;; ----------- PATTERN MATCHING ------------------------------------
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


(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

;(defmacro tokens->string(tokens)
;  `(concatenate 'string ,@tokens))


(defun match-variable(var input bindings)
  (let ((b (assoc var bindings)))
    (cond
      ((not b) (extend-bindings var input bindings))
      ((string= input (cdr b)) bindings)
      (t nil))))

;(pat-match '("la" ?x "la") (tokens "la la la"))
(defun pat-match(pattern input &optional (bindings '((t . t))) )
  ;(format t "Wywolanie: ~A , ~A z podst.~A~%" pattern input bindings)
  (cond
    ((null bindings) nil)
    ((and (null pattern) input) nil)

    ((variable-p  pattern)
     (if (null input)
	 (cons (cons pattern "") bindings)
	 (match-variable  pattern input bindings) ))
    ((and (stringp pattern) (string= pattern input)) bindings)
    ((and (symbolp pattern) (search (symbol-name pattern) (string-upcase input)))
     bindings  )
    ((and (consp pattern) (listp input))

     (if (and (variable-p (car pattern))
	      (equal (char (symbol-name (car pattern)) 1) #\?))
	 (cons (cons '?? (if input input "")) bindings)
	 (pat-match (cdr pattern) (cdr input)
		    (pat-match (car pattern) (car input) bindings))) )  
    (t nil)    ))


(defun match-string(pattern input)
  (if (stringp input)
      (pat-match pattern (tokens input))
      (pat-match pattern input)))



;;-------- funkcje historii gier:



(defun eval-percept(p game) 
    (if (symbolp (first p))
	(cond
	  ((eq (first p ) 'show) (format t "~A~&" game))
	  ((eq (first p ) 'flop)  (round-flop (nth 1 p) (nth 2 p) (nth 3 p) game ))
	  ((eq (first p ) 'turn)  (round-turn (nth 1 p) game  ))
	  ((eq (first p ) 'river) (round-river (nth 1 p) game ))
	  ((eq (first p ) 'holecards) (holecards game (nth 1 p) (nth 2 p) (nth 3 p) (nth 4 p) ))   
	  (t (format t "dziwny symbol!!!! ~A" (first p))))
	(cond
	  ((eq (second p) 'SB) (small-blind (car (nth (first p) (db-players game))) (third p) game))
	  ((eq (second p) 'BB)   (big-blind (car (nth (first p) (db-players game))) (third p) game))
	  ((eq (second p) 'C) (call (car (nth (first p) (db-players game))) (third p) game))
	  ((eq (second p) 'B) (bet (car (nth (first p) (db-players game))) (third p) game))
	  ((eq (second p) 'R) (raise (car (nth (first p) (db-players game))) (third p) game))
	  ((eq (second p) 'A) (allin (car (nth (first p) (db-players game))) (third p) game))
	  ((eq (second p) 'F) (fold (car (nth (first p) (db-players game))) game))
	  (t (format t "dziwny symbol!!!! ~A ~A" (symbol-name (second p)) (package-name(symbol-package (second p))))) )   )
    (format nil "~A</div>" 
	    (if (symbolp (first p))  
		(first p)
		(format nil "~A ~A" (car (nth (first p) (db-players game))) (second p) ))))


(defmethod next-percept((g game-log))
  (incf (percept-nr g))
  (eval-percept (nth (percept-nr g) (db-history g))  g))

(defun read-game(id &optional (prev-game nil))
 (let* ((dane (car (db:poker-query (format nil "select * from crypto where id=~A" id))))
	(g (new-game  :id (nth 0 dane) :czas (nth 1 dane) :game-type 'game-log :prev-game prev-game )))
   (setf (table-name g) (nth 2 dane))
   (setf (kind g) (nth 3 dane))
   (setf (db-players g) (read-from-string (nth 4 dane)))
   (setf (db-history g) (nreverse (read-from-string (nth 5 dane))))   
   (setf (db-log g) (read-from-string (nth 6 dane)))   
 g))

(defmethod simulate((g game-log))
  (dotimes (p-nr (length (db-history g)))
    (next-percept g)  ))


(defun simulate-games-list(nr)
  (let ((gl (db::get-games-list nr)))
    (when gl
      (setq *gameslist* (make-instance 'games-list))
      (setf (db-games *gameslist*)  gl)
      (dolist (opis (db-games *gameslist*))
	(let ((g (read-game (car opis)  (head *gameslist*)  )))
	  (setf (head *gameslist*) g)
	  (simulate g)
	  ;ustawic wygrane
	  ;WINNERS
	  (dolist (e (second (first (db-log g))))
	    (setf (gain (get-player (car e) g)) (cdr e)  ))
	  ;UNCALLED-BETS
	  (dolist (e (second (second (db-log g))))
	    (incf (gain (get-player (car e) g)) (cdr e)  ))
	  (push g (games *gameslist*)) 	)   )
     (setf (games *gameslist*) (reverse (games *gameslist*))) )
    'ok))

