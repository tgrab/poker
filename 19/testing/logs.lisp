(defvar *games-list-dir* "gameslist")


(defun save-game-output(&optional (g +game+))
  "Pokazuje wazne info potrzebne do odtworzenia tej gry"
  (princ "(make-instance 'game ")
  (format t ":id ~S " (id g))
  (format t ":czas ~A " (czas g))
  (format t ":table-name ~S " (table-name g))
  (format t ":casino ~S " (casino g))
  (format t ":kind ~A~&" (kind g))
  (format t ":history-log '~A~&" (history-log g))
  (format t ":agent-name ~S " (agent-name g))
  (format t ":agent-pos ~A " (agent-pos g))
  (format t ":name->seat '~S~&" (name->seat g))
  (format t ":table-cards '~A " (table-cards g))
  (format t ":cards '~A " (cards g))
  (awhen (cards-log g)    (format t ":cards-log '~S " it))
  (princ ")") 'ok)

(defun save-game-to-file
	(&optional 
		 (nazwa-pliku 
		  (format nil "~A" (get-universal-time)) )
		 (g +game+) )
  "zapisuje biezaca gre aby mozna ja bylo wczytac przez (load-game)"
  (when (or (fixed?) (omaha?))
    (with-open-file (f (if (fixed?) 
			   (format nil "~A/~A.dat" *games-list-dir* (concatenate 'string "f" nazwa-pliku))
			   (format nil "~A/~A.dat" *games-list-dir* (concatenate 'string "o" nazwa-pliku)))
		     :direction :output
		     :if-does-not-exist :create)
    (let ((*standard-output* f))
      (save-game-output g)    ))))


(defun read-games-list()
  (let ((games-list nil))
    (dolist (name (directory (format nil "~A/*.dat" *games-list-dir*)) games-list)
      (print name)
      (push (with-open-file  (f name) 
	      (eval (read f))) 
	    games-list)       )))

