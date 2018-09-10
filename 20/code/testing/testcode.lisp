;evaluator warunkow:
; (> outs 2) (rank str8)TODO slot-value:
(defun ewaluator(game war)
  (cond
    ;(< pot 2)
    ((member (car war) '(< > <= = >=)) (funcall (symbol-function (car war)) 
						(slot-value game (second war))
						(third war)))
    ;(rank one-pair)
    ((listp (funcall (symbol-function (first war)) game )  )
     (member (second war) (funcall (symbol-function (first war)) game )))
    ;(bets 0)
    (t   (equal (slot-value game (first war)) (second war) )    )))

;czesc :pre-con
;(?and *game* '( (stake 0) (pot 0) ))
(defun ?and(game expr)
  (if (null expr) t
      (and (ewaluator game (car expr))
	   (?and game (cdr expr)))))

;czesc :con ( (()()) (()()) )
(defun ?or(game expr)
  (if (null expr) nil
      (or (?and game (car expr))
	   (?or game (cdr expr)))))

;(prove *game* :pre-con ((stake 0)) :con ( ((stake 0)(pot 0))  ((pot 1))     ))
(defmacro prove(game &key (pre-con nil) (con nil ))
  (if pre-con
      `(when (?and ,game ',pre-con) (?or ,game ',con))
      `(?or ,game ',con)))

;;------------------------------------
;;------------------------------------
(defun prove-all(game zdania )
  (every #'(lambda (zdanie)
	    (prove game zdanie))
	 zdania))

(defun get-zdania(zdanie)
  (mapcan #'(lambda(z)
	      (when (eq (car z) zdanie) (list z)))
	  *poker-rules*))

(defun prove(game smb)
  (or (member smb (game-memory game))
      (some #'(lambda (rule)
		(prove-all game (cdr rule)))
	    (get-zdania smb))))

;(defmacro ?(game smb)
;  `(prove ,game ',smb))

(defmacro ?(game &body smbs)
  `(prove-all ,game ',smbs))