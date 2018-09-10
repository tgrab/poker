(in-package :view)

(let ((historia nil))
  (defun oblicz(&optional sexp)
    (when sexp
      (push sexp historia)
      (format t "<hr>> ~A<hr> " sexp)
      (format t "> ~A<hr>" (eval (read-from-string sexp))))
    (dolist (e (subseq historia 0 (min 30 (length historia))))
      (format t "<a href='/eval?sexp=~A'>~A</a><br>" e e))))  


(defmacro show-game-stats()
  '(progn
    (awhen (table-cards +game+) (format *log-stream* "Table: ~A   " (karty->napisy it)))
    (awhen (cards +game+) (format *log-stream* "Hand: ~A   ~&" (karty->napisy it)))
    (dotimes (i (length (name->seat +game+)))
      (when (aref (active-players +game+) i)
	(format *log-stream* "   seat ~A:   <~A> ~A~&" i
		(aref (balance +game+) i)
		(first (nth i (reverse (name->seat +game+))))
		;(reverse (player-history i))
		)))
    (format *log-stream* "GET-STAKE:~$ STAKE:~$ POT:~$~%~%" (get-stake) (stake +game+) (pot +game+))
    (force-output *log-stream*)))
