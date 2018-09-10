(defun view-register-client()
	(set-game-kind (parse-integer (get-parameter "kind")))
	(with-html "ok"))

(def-url "/registerclient" view-register-client)
