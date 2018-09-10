(defparameter *refresh-code* "<script>function refresh() {window.location.href='/nextpercept?action=simulate'} setTimeout('refresh()',3000);</script>")

(defparameter *page-style*
"
body { font: 10px arial}
h4 {margin: 0px }
a  {text-decoration :none }

.karty {
 list-style : none;
 margin: 0;
 padding: 0;
 font-size: 12pt;
}

.spades {
 display: inline;
 color: black;
 padding-left: 5px;
}

.hearts {
 display: inline;
 color: red;
 padding-left: 5px;
}

.diamonds {
 display: inline;
 color: blue;
 padding-left: 5px;
}

.clubs {
 display: inline;
 color: green;
 padding-left: 5px;
}")

(defparameter *page-menu*
"[ <a href='/'>Current Game</a> ] 
[ <a href='/rendergame'>Render Game</a> ] 
[ <a href='/options'>Options</a> ] 
[ <a href='/nextpercept?action=simulate'>Simulate</a> ]
[ <a href='/dirlist'>Choose Directory</a> ]
[ <a href='/readlogs'>Read Logs</a> ]  
[ <a href='/gameslist'>Games List</a> ] 
[ <a href='/statistics'>Statistics</a> ] 
[ <a href='/eval'>eval</a> ]")

(def-url "/" (main-page
	      (view-index)))

(def-url "/startgame"  (progn
			 (let ((nr (get-parameter "nr"))
			       (id (get-parameter "id"))
			       (simul (get-parameter "simulate"))
			       (agent (get-parameter "agent")))
			   (when nr (choose-startgame :nr (parse-integer nr)))			     
			   (when id (choose-startgame :id id))			 
			   (when simul
			     (format t *refresh-code*))
			   (when agent (setf (agent-name (logs-game *log*)) agent))  )
		       (next-percept)
		       (render-game)
		       ))

(def-url "/nextpercept" (main-page
			 (aif (get-parameter "action")
			     (progn
			       (when (string= it "stop")
				 (render-game-view))
			       (when (string= it "simulate")
				 (next-percept)
				 (when (<= (logs-percept-nr *log*)  (length  (history-log (logs-game *log*))))
				     (format t *refresh-code*))
				 (render-game-view)))
			     (progn
			       (next-percept)
			       (render-game-view) ))))

(def-url "/eval"
    (main-page
      (princ "<form action='/eval'>")
      (aif (get-parameter "sexp")
	   (format t "<textarea name='sexp' rows=5 cols=60>~A</textarea><br>" it)
	   (princ"<textarea name='sexp' rows=5 cols=60></textarea><br>"))
      (princ "<input type='submit'></form>")
      (oblicz (get-parameter "sexp"))))


(def-url "/logging" 
    (main-page (setq *logging* (not *logging*))
	   (format t "Logging: ~A" *logging*)))

(def-url "/options"
    (main-page
     (if (omaha?) 
	 (princ "<h2>Omaha Pot Limit</h2>")
	 (princ "<h2>Holdem Fixed</h2>"))
     (format t "Logging: ~A <a href='/logging'>Change It</a><br>" *logging*)
     (format t "Holdem Fixed: <a href='/registerclient?kind=2'>Turn On</a><br>")
     (format t "Omaha Pot Limit: <a href='/registerclient?kind=3'>Turn On</a><br>")
     ))

(def-url "/gameslist"
    (main-page
     (princ "<table border='1'><tr><td>TABLE<td>ID<td>TIME<td>Winners<td>Grabol's Cards<td>Others")
     (dotimes (nr (length (get-game-infos)))
       (view-game-log nr))
     (princ "</table>")))

(def-url "/memory" (view-memory))
(def-url "/rendergame" (render-game))
(def-url "/dirlist"
    (main-page
     (view-dir (get-parameter "dir"))))


(def-url "/readlogs"
    (main-page
     (format t "<a href='/readlogs?wh_dir=true'>WillHill Logs from ~A</a>" (view-get-dir))
     (format t "<hr>")
     (when (get-parameter "wh_dir") (add-games-list))  ))

;for testing new player infos
(def-url "/updateinfos"
    (main-page
      (setq *player-infos* (make-hash-table :test #'equal))
      (dolist (g *games-list*)
	(format t "~A " (id g))
	(create-infos g))))


(def-url "/statistics"
    (main-page
      (awhen  (get-parameter "clear")
	(setf *range* nil))
      (awhen  (get-parameter "table")
       (if *range*
	    (setf (range-table-name *range*) it)
	    (setf *range* (make-range :table-name it))))
     (format t "Total num. of games: <b>~A</b><br>" (length *games-list*))
     (format t "Selected num. of games: <b>~A</b><br>" (length (get-game-infos)))
     (format t "Total num. of players: ~A<br>" (hash-table-count *player-infos*))
     (link "/profile" "Set player profiles")
     (link "/statistics?action=listofplayers" "List of Players")
     (format t "<form>")
     (format t "<select name='table'>~{<option>~A</option>~}</select>" 
	     (list-of-tables))
     (format t "<input type='submit' value='Select table'></form><br>")
     (link "/statistics?clear=true" "Clear range")
     (format t "RANGE: ~A<br>" *range*)
     (when (string= "listofplayers" 
		    (get-parameter "action"))
       (dolist (p (players-list))
	 (format t "<a href='/playerprofile?player=~A'>~A</a><br>~&" 
		 p (view-player-line p) )))

     ))

(def-url "/profile"
    (main-page
      (awhen (get-parameter "maniac")
	(add-maniac it))
      (dolist (name (players-list 2))
	(when (maniac? name) (format t "*M* "))
	(format t "<a href='/playerprofile?player=~A'>~A</a><br>" name (view-player-line name)))
     (format t "<form>")
     (format t "<select name='maniac'>~{<option>~A</option>~}</select>" 
	     (players-list 2))
     (format t "<input type='submit' value='Select maniac'></form><br>")
    ))

(def-url "/playerprofile"
    (main-page
      (playerprofile-view (get-parameter "player"))))

;;-------------------------------------------------------------------------------------------
;;----------------------------------- VIEW --------------------------------------------------
;;-------------------------------------------------------------------------------------------
(defmacro main-page(&body body)
  `(progn
    (princ "<html><head>")
    (format t "<style>~A</style>" *page-style*)
    (format t  "</head><body>~&")
    (format t "~A<hr>~&" *page-menu*)
    ,@body
    (princ "</body></html>")
    'main-page))


(defun view-game-log(nr)
  (let ((g (nth nr (get-game-infos))))
    (format t "<tr><td>~A<td><a href='/startgame?nr=~A'>~A</a><td>~A<td>~A<td>~A" (table-name g) nr (id g) (dekoduj-czas (czas g)) (winners-log g) (karty->html (cards g)))
    (dolist (a (cards-log g))
      (when (not (string= "Grabol" (car a))) (format t "<td>~A ~A" (car a) (karty->html (cdr a))))
      )
))



(defun view-show-memory(label tbl)
  (format t "<tr><td colspan='2' style='color:white;background-color:black;font-size=12px'>~A~&" label)
    (maphash 
     (lambda (k v)
       (format t "<tr><td>~A<td>" k)
       (dolist (e v)
	     (format t "~A" (cdar e) )  )
       (format t "~&"))
     tbl)
)

(defun view-memory()
  (rules-percept-memo)
  (format t "<table border='1'>")
  (view-show-memory "MEMO" (memo +game+))
  (view-show-memory "GAME" (game-memo +game+))
  (view-show-memory "ROUND" (round-memo +game+))
  (view-show-memory "PERCEPT" (percept-memo +game+))
  (format t "</table>"))



(defun view-index(&optional (game +game+))
     (format t "<table border='1'>")
     (format t "<tr><td>casino:<td><b>~A</b>" (casino game))
     (format t "<tr><td>table:<td><b>~A</b>" (table-name +game+))
     (format t "<tr><td>id:<td>~A" (id game))
     (format t "<tr><td>czas:<td>~A" (dekoduj-czas (czas game)))
     (format t "<tr><td>pot:<td>~A" (pot game))
     (format t "<tr><td>stake:<td>~A" (stake game))
     (format t "<tr><td>blind:<td>~A" (blind game))
     (format t "<tr><td>kind:<td>~A" (kind game))
     (format t "<tr><td>balance:<td>~A" (balance game))
     (format t "<tr><td>total-balance:<td>~A" (total-balance game))
     (format t "<tr><td>history:<td>~A" (history game))
     (format t "<tr><td>history-log:<td>~A" (history-log game))
     (format t "<tr><td>free-seat:<td>~A" (free-seat game))
     (format t "<tr><td>agent-name:<td>~A" (agent-name game))
     (format t "<tr><td>agent-pos:<td>~A" (agent-pos game))
     (format t "<tr><td>name->seat:<td>~A" (name->seat game))
     (format t "<tr><td>table-size:<td>~A" (table-size game))
     (format t "<tr><td>taken-seats:<td>~A" (taken-seats game))
     (format t "<tr><td>active-player:<td>~A" (active-players game))
     (format t "<tr><td>table-cards:<td>~A" (karty->napisy (table-cards game)))
     (format t "<tr><td>cards:<td>~A" (karty->napisy (cards game)))
     (format t "</table>")
     (view-memory)
     (princ "<hr><div style='color:blue'>")
     (dolist (p *cortex*)
       (format t "~A<br>" p))
     (princ "</div>"))
		      

(defun show-chances(&optional (cards-log  (cards-log (logs-game *log*))))
  (when (= 3 (length cards-log))
	(pojedynek-3 (cdr (nth 0 cards-log)) (cdr (nth 1 cards-log))  (cdr (nth 2 cards-log)) (table-cards +game+)))
  (when (= 2 (length cards-log))
    (if (table-cards +game+)
	(pojedynek-flop (cdr (nth 0 cards-log)) (cdr (nth 1 cards-log)) (table-cards +game+))
	(pojedynek-pre-flop (cdr (nth 0 cards-log)) (cdr (nth 1 cards-log))))


    ))



(defun pojedynek-pre-flop(cards1 cards2)
 (let ((reszta (set-difference (talia) (append cards1 cards2)))
       (n1 0)
       (n2 0))
   (dotimes (i 500)
     (let* ((stol (subseq 
			  (tasuj reszta)
			   0  5))
	    (o1 (omaha-ocena cards1 stol ))
	    (o2 (omaha-ocena cards2 stol ))
	    (wynik (poker.ranks::lepsza o1 o2)))
       (when (= wynik 1)
	      (incf n1))
       (when (= wynik -1)
	      (incf n2)))) 
   (when (plusp (+ n1 n2))(format t "<div style='border:1px solid orange'>~A ~$%, ~A ~$%</div>" 
	   (karty->napisy cards1)  (* 100.0 (/ n1 (+ n1 n2)))
	   (karty->napisy cards2)  (* 100.0 (/ n2 (+ n1 n2)))  ))  ))


(defun pojedynek-flop(cards1 cards2 table)
 (let ((reszta (set-difference (talia) (append cards1 cards2 table)))
       (n1 0)
       (n2 0))
   (dotimes (i 500)
     (let* ((stol (subseq 
			  (tasuj reszta)
			   0  (- 5 (length table))  ))
	    (o1 (omaha-ocena cards1 (append table stol) ))
	    (o2 (omaha-ocena cards2 (append table stol) ))
	    (wynik (poker.ranks::lepsza o1 o2)))
;       (format t "~A ~A ~A ~A ~A" cards1 cards2 stol o1 o2)
       (when (= wynik 1)
	      (incf n1))
       (when (= wynik -1)
	      (incf n2)))) 
   (when (plusp (+ n1 n2))(format t "<div style='border:1px solid orange'>~A ~$%, ~A ~$% on ~A</div>" 
	   (karty->napisy cards1)  (* 100.0 (/ n1 (+ n1 n2)))
	   (karty->napisy cards2)  (* 100.0 (/ n2 (+ n1 n2)))
	   (karty->napisy table)))  ))

(defun pojedynek-3(cards1 cards2 cards3 table)
 (let ((reszta (set-difference (talia) (append cards1 cards2 cards3 table)))
       (n1 0)
       (n2 0)
       (n3 0))
   (dotimes (i 500)
     (let* ((stol (subseq 
			  (tasuj reszta)
			   0  (- 5 (length table))  ))
	    (o1 (omaha-ocena cards1 (append table stol) ))
	    (o2 (omaha-ocena cards2 (append table stol) ))
	    (o3 (omaha-ocena cards3 (append table stol) ))
	    (wynik1 (poker.ranks::lepsza o1 o2))
	    (wynik2 (poker.ranks::lepsza o1 o3))
	    (wynik3 (poker.ranks::lepsza o2 o3)))
;       (format t "~A ~A ~A ~A ~A" cards1 cards2 stol o1 o2)
       (when (and (= wynik1 1) (= wynik2 1))
	      (incf n1))
       (when (and (= wynik1 -1) (= wynik3 1))
	      (incf n2))
       (when (and (= wynik3 -1) (= wynik2 -1))
	      (incf n3))))
   (when (plusp (+ n1 n2 n3))(format t "<div style='border:1px solid orange'>~A ~$%, ~A ~$%, ~A ~$% on ~A</div>" 
	   (karty->napisy cards1)  (* 100.0 (/ n1 (+ n1 n2 n3)))
	   (karty->napisy cards2)  (* 100.0 (/ n2 (+ n1 n2 n3)))
	   (karty->napisy cards3)  (* 100.0 (/ n3 (+ n1 n2 n3)))
	   (karty->napisy table))))  )

(defun render-game-view()
   (format t "<div style='font-size: 1.5em'><a href='/nextpercept'>[ Next Percept]</a><a href='/nextpercept?action=stop'>[ Stop simulation]</a><br>")
   (format t "<a href='/startgame?nr=~A'>  [ <<<< ]  </a>" (logs-prev-nr *log*) )
   (format t "<a href='/startgame?nr=~A'>  [ Reread ~A ]  </a>" (logs-current-nr *log*) 
	   (id (logs-game *log*)))
   (format t "<a href='/startgame?nr=~A'>  [ >>>> ]  </a><hr>" (logs-next-nr *log*) )
   (format t "Game ~A/~A , ~A on table ~A<hr>" (id +game+) (logs-percept-nr *log*) (dekoduj-czas (czas +game+)) (table-name +game+) )
   (awhen (cards-log (logs-game *log*)) 
	  (dolist (o it)
	    (format t "player: <a href='/startgame?id=~A&agent=~A'>~A</a>~A<br>" (id +game+) (car o) (car o) (karty->html (cdr o)))))
   (when (or (= 0 (logs-percept-nr *log*)) 
	     (symbolp (nth (logs-percept-nr *log*)   (reverse (history-log (logs-game *log*)))))) 
     (show-chances))

   (awhen (cards +game+)  
	       (format t "<table style='padding:10px; border:1px solid black'><tr><td>~A cards: ~A" (agent-name +game+) (karty->html it))
	       (when (table-cards +game+)
		 (if (= 4 (length it))
		     (princ (render-rank (omaha-ocena it (table-cards +game+))))
		     (princ (render-rank (ocena (append it (table-cards +game+))))) ))      )
   (aif (table-cards +game+)  
	     (format t "<td style='padding-left: 20px;'>Table: ~A</table>" (karty->html it))
	     (format t "</table>"))

	(let ((p (nth (1+ (logs-percept-nr *log*))
		      (reverse (history-log (logs-game *log*))))  ))
	    (when (and (consp p)
		       (agent-pos +game+)
		       (= (car p) (agent-pos +game+)))
	      	      (format  t "<h2 style='color:red'>Action: ~A</h2>" (action-prove))))
	
	(format  t "<div style='margin:10px; font:12px'><b>pot:~$ stake:~$ get-stake:~$ pot-odds:~$ ratio:~$</b></div>"  
		 (pot +game+)(stake +game+)(get-stake)(pot-odds) ratio)
	
	(format t  "<table border=1>")
	(dotimes (i (length (name->seat +game+)))
	  (let ((player-name (first (nth i (reverse (name->seat +game+))))))
	  (if (aref (active-players +game+) i)
	      (format t "<tr><td><b>~A</b><td>~$<td><b>~$</b><td>~$<td>~A" 
		      player-name (player-money player-name)
		      (aref (balance +game+) i)
		      (aref (total-balance +game+) i)
		      (reverse (player-history i))      )	    
	      (format t "<tr style='background-color: silver'><td><b>~A</b><td>~$<td><b>~$</b><td>~$<td>~A" 
		      player-name (player-money player-name )
		      (aref (balance +game+) i)
		      (aref (total-balance +game+) i)
		      (reverse (player-history i))    ))   ))
	  (format t  "</table>")
	 ; (princ (ocena-stolu 10 (id (logs-game *log*))))
	  (view-memory)
	  (princ "<hr><div style='color:blue'>")
	  (dolist (p *cortex*)
	    (format t "~A<br>" p))
  	  (princ "</div>"))


(defun render-game()
  (main-page (render-game-view)))



; View dla logow




(let (#+unix(dir "/media/win/Program Files/William Hill Poker/log/")
      #+unix(old-dir "/")
      #+win32(dir "c:\\Program Files\\William Hill Poker\\log\\")
      #+win32(old-dir "c:\\Program Files\\"))
  (defun view-get-dir()
    dir)
  (defun view-dir(&optional new-dir)
    (when new-dir      
      (setq old-dir dir)
      (setq dir new-dir))
    (format t "<h2>Current Directory: ~A</h2><a href='/dirlist?dir=~A'>[ BACK ]</a><br>" dir old-dir) 
    (dolist (name (list-directory dir))
      (if (directory-pathname-p name)
	  (format t "<a href='/dirlist?dir=~A'><b>~A</b></a><br>" name (car (last (pathname-directory name)))) 
	  (aif  (pathname-type name)
		(format t "~A.~A<br>" (pathname-name name) it)
		(format t "~A<br>" (pathname-name name) )))))  )




(defmacro link(adres opis)
  `(format t "<a href='~A'>~A</a><br>" ,adres ,opis ))


(defun view-player-info(info)
  (let ((played? (not (eq (car (pre-flop-actions info)) 'f)))
	(raised? (member 'r (pre-flop-actions info)))
	(blind? (member (car (last (pre-flop-actions info))) '(sb bb))))
    (cond
      ((and played? raised?) (princ "<div style='border: 1px solid red;margin: 5px;background-color:yellow'>"))
      ((and played? blind?) (princ "<div style='border: 1px dotted red;margin: 5px'>"))
      (played?  (princ "<div style='border: 1px solid red;margin: 5px'>"))
      (t  (princ "<div style='border: 1px solid black;margin: 5px;background-color:grey'>")) )
    (aif (cards info) (princ (karty->html it)))
    (format t "<a href='/startgame?id=~A'>" (id info))
    (princ (table-name info))
    (format t "[ ~A ]" (table-size info))
    (princ (dekoduj-czas (czas info)))
    (format t "</a>")
    (format t "pre-flop:~A" (reverse (pre-flop-actions info)))
    (format t "  balance:~$+~$" (balance info) (uncalled-bets info))
    (format t "  gain:~$" (gain info) )
    (awhen (remarks info) (format t "<div style='background-color:black;color:white'>~A</div>" it))
    (awhen (bad-actions info) (format t "<div style='background-color:black;color:white'>~A</div>" it))
    (princ "</div>")) )
  


(defun view-player-line(name)
  (let ((infos (get-player-infos name))
	(vpip (vpip name))
	(pfr (pfr name))
	(label1 "")
	(label2 ""))
    (cond
      ((>= vpip 0.44) (setq label1 "extremely loose"))
      ((>= vpip 0.33) (setq label1 "loose"))
      ((<= vpip 0.18) (setq label1 "tight")))
    (cond
      ((>= pfr 0.2) (setq label2 "maniac"))
      ((>= pfr 0.09) (setq label2 "aggressive"))
      ((<= pfr 0.03) (setq label2 "passive")) )
    (if (null infos)
	""
    (format nil "~A [ <b>~$</b>/~A ], vpip=~A, pfr=~A <b>~A ~A</b> win rate:~A" 
	    name
	    (let ((s 0))
	      (dolist (i infos s)
		(incf s (+ (gain i) (uncalled-bets i) (balance i) ))))
	    (length infos) vpip pfr label1 label2 
	    (/ (count-if #'(lambda(i) (plusp (gain i))) infos) 
	       (length infos) 1.0 )         ))))


(defun playerprofile-view(player-name)
  (let ((infos (get-player-infos player-name)))
    (format t "<h2>~A</h2>" 
	    (view-player-line player-name))
    (format t "<h4>win rate:~A outcome: ~$</h4>" 
	    (/ (count-if #'(lambda(i) (plusp (gain i))) infos) 
	       (length infos) 1.0 )
	    (let ((s 0))
	      (dolist (i infos s)
		(incf s (+ (gain i) (uncalled-bets i) (balance i) )))))
    (dolist (i (reverse infos))
      (view-player-info i))))


(defun list-of-tables()
  (let ((wynik nil))
    (dolist (s (mapcar #'table-name *games-list*) wynik)
      (pushnew s wynik :test #'string=))))