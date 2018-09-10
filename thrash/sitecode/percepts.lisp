(defmacro www-player()
  '(get-parameter "name"))

(defmacro www-amount()
  '(get-number-parameter "amount"))

(defmacro show-number(n)
  `(:princ (format nil "~6,2F" ,n)))


(def-url-0 "/registerclient"
  (set-game-kind (parse-integer (get-parameter "kind")))
  (html "ok"))

(defun game-stats()
    (html "ok"))

(def-url-0 "/smallblind"
  (small-blind (www-player) (www-amount))
  (game-stats))


(def-url-0 "/bigblind"
  (big-blind (www-player) (www-amount))
  (game-stats))



(def-url-0 "/call"
  (call (www-player) (www-amount))
  (game-stats))

(def-url-0 "/check"
  (call (www-player) 0)
  (game-stats))


(def-url-0 "/fold"
  (fold (www-player))
  (game-stats))


(def-url-0 "/bet"
  (bet (www-player) (www-amount))
  (game-stats))

(def-url-0 "/raise"
  (bet (www-player) (www-amount) 'r)
  (game-stats))


(def-url-0 "/allin"
  (all-in (www-player) (www-amount))
  (game-stats))


(def-url-0 "/flop"
    (flop
     (get-int-parameter "c1")
     (get-int-parameter "c2")
     (get-int-parameter "c3"))
    (game-stats))


(def-url-0 "/turn"
    (turn (get-int-parameter "c1"))
    (game-stats))


(def-url-0 "/river"
    (river (get-int-parameter "c1"))
    (game-stats))


(def-url-0 "/holecards"
  (holecards  (get-number-parameter "c1") (get-number-parameter "c2")
		  (get-number-parameter "c3") (get-number-parameter "c4"))     
  (game-stats))

(def-url-0 "/action"
    (html (:princ (action)) ))

