(define-easy-handler (player :uri (fmt "~A/player" *ctxt*)) (name pgnr)
  (page "Player"
 
    (if pgnr
	(a (:href (fmt "~A/player?name=~A&pgnr=~A" *ctxt* name (1+ (parse-integer pgnr)))) " >>>")
	(a (:href (fmt "~A/player?name=~A&pgnr=2" *ctxt* name)) "[ next page ]<p>"))
    (let ((ip 0)
	  (ik 50)
	  (games (mapcan 
		 #'(lambda(g) 
		     (if 
		      (find name (log-history g) :test #'equal :key #'second)
		      (list g) ))
		 *log-games1*) ))
      (out "<h1>" name  "</h1>" "<h2>total" (length games) "games</h2>")
      (when pgnr
	(setq ip (* 50 (1- (parse-integer pgnr)))
	      ik (* 50 (parse-integer pgnr))))
      (dolist (g  (subseq  games  ip (min ik (length games))))
	(when (eq *selector* 'day)
	  (out (log-tablename g)))
	(a (:href (fmt "~A/showgame?id=~A" *ctxt* (log-id g))) (log-date g))
	(let ((h (find name (hands g) :test #'equal :key #'car)))
	  (when h (out (cards-view (cdr h))))   )
	(out " $:" (log-balance g name))
	(out "<br>") ))))        