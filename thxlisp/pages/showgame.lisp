(defmethod flop((g log))
  (subseq (reverse (log-tablecards g)) 0 3))

(defmethod turn((g log))
  (subseq (reverse (log-tablecards g)) 0 4))

(defmethod river((g log))
  (subseq (reverse (log-tablecards g)) 0 5))

(define-easy-handler (showgame :uri "/primalogs/showgame") (id)
  (page "ShowGame"
    (let ((g 
	   (find (parse-integer id) 
		 *log-games1* 
		 :test #'= 
		 :key #'log-id)))
      (out "<h2 style='text-align:center;'>" *logtablename* " at " (log-date g) "</h2>")
      (dolist (h (hands g))
	(out "<div style='margin:10px;'>" (car h) "<br>" (cards-view (cdr h)) "</div>"))
      (out "<h2>Preflop</h2>")
      (dolist (a (log-history g))
	(cond
	  ((eq 'flop (car a)) 
	   (out "<h2>Flop</h2>" (cards-view (flop g) ) "<br>" )
	   (dolist (h (hands g))
	     (out "<b>" (car h)"</b>" (render-rank (omaha-ocena (cdr h) (flop g)))  "<br>"))
	   (out "<p>"))
	  ((eq 'turn (car a)) 
	   (out "<h2>Turn</h2>" (cards-view (turn g) ) "<br>" )
	   (dolist (h (hands g))
	     (out "<b>" (car h)"</b>" (render-rank (omaha-ocena (cdr h) (turn g)))  "<br>"))
	   (out "<p>"))
	  ((eq 'river (car a))  
	   (out "<h2>River</h2>" (cards-view (river g)) "<br>" )
	   (dolist (h (hands g))
	     (out "<b>" (car h)"</b>" (render-rank (omaha-ocena (cdr h) (river g)))  "<br>"))
	   (out "<p>"))
	  ((or (eq 'bet (car a))  (eq 'raise (car a)))  
	   (out "<div style='border:1px solid red;display:inline'>" a "</div><br>" ))
	  ((eq 'fold (car a))  
	   (out "<div style='background-color:grey;display:inline'>" a "</div><br>" ))
	  (t (out a "<br>" ))
	  )
	 )
      )))