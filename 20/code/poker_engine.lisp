(in-package :poker)

;;--------------------------------------------------------
;;---------------- LOGIC TOOLS ---------------------------
;;--------------------------------------------------------

;reguly sa postaci:
;(call 1 id_speech mem1 mem2 ... (<= bets 0)  ... memn)
(defvar *poker-rules* nil)

(defun game-memory(&optional (game *game*))
  (append (round-memo game) (game-memo game) (percept-memo game)))

(defmacro <-(&body rule)
  `(push ',rule *poker-rules*))

(defmacro <-game(game smb)
  `(pushnew ',smb (game-memo ,game)))

(defmacro <-round(game smb)
  `(pushnew ',smb (round-memo ,game)))

(defmacro <-percept(game smb)
  `(pushnew ',smb (percept-memo ,game)))

(defun prove-all(game memory symbols)
   (every
     #'(lambda(s)
	 (if (symbolp s)
	     (member s memory)
					;lub postaci (< stake 2)
	     (funcall (symbol-function (first s))
		      (funcall (symbol-function (second s)) game)
		      (third s))))
     symbols))


(defun prove(game sent &optional 
	     (mem (game-memory game)) 
	     (reguly *poker-rules*)
	     (acc (list 0 'no-rule)))
;  (format t "~A ~A~&" (car reguly) acc)
  (if (null reguly)
      acc
      (if (and (eq sent (first (car reguly))) 
	       (> (second (car reguly)) (first acc))
	       (prove-all game mem  (cdddr (car reguly))))
	  (prove game sent mem (cdr reguly) (list (second (car reguly)) (third (car reguly)) ))
	  (prove game sent mem (cdr reguly) acc)) ))

(defmacro ?(game smb)
  `(prove ,game ',smb))

;;-------------------------------------------------------------
;;-------------------------------------------------------------

