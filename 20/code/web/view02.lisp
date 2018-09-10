(in-package :view)

(def-url "/"
 (basic-page))

(defun view-ocena(cards game)
  (if (poker::table-cards game)
    (let ((val (if (poker::omaha? game)
		   (omaha-ocena cards (poker::table-cards game))
		   (ocena (append cards (poker::table-cards game))))))
      (format nil "<b>~A</b>" (poker::render-rank val)))
    ""))

(defmethod view(obiekt)
  (format nil "~A" obiekt))

(defmethod view((c poker::profile))
  (with-output-to-string (s)
  (format s "    <table>~&")
  (format s "      <tr><td>vip<td>~A~&" (* 1.0 (poker::vip c)))
  (format s "      <tr><td>pfr<td>~A~&" (* 1.0 (poker::pfr c)))
  (format s "    </table>")))

(defmethod view((c poker::player-profile))
  (with-output-to-string (s)
  (format s "    <table>~&")
  (format s "      <tr><td>cont-bet<td>~A~&" (poker::cont-bet c))
  (format s "      <tr><td>vip<td>~A~&" (poker::vip c))
  (format s "      <tr><td>pfr<td>~A~&" (* 1.0 (poker::pfr c)))
  (format s "    </table>")))

(defmethod view((c poker::game-cortex))
  (with-output-to-string (s)
  (format s "    <table>~&")
  (format s "      <tr><td>on-flop-players<td>~A~&" (poker::on-flop-players c))
  (format s "      <tr><td>preflop-bets<td>~A~&" (poker::preflop-bets c))
  (format s "    </table>")))

(defmethod view((c poker::player-cortex))
  (with-output-to-string (s)
  (format s "    <table>~&")
  (format s "      <tr><td>vol-in-pot<td>~A~&" (poker::vol-in-pot c))
  (format s "      <tr><td>flop-bet<td>~A~&" (poker::flop-bet c))
  (format s "      <tr><td>preflop-bets<td>~A~&" (poker::preflop-bets c))
  (format s "    </table>")))



(defmethod view((p player))
  (with-output-to-string (s)
  (if (poker::active? p)
      (format s "<table border='1'>~&")
      (format s "<table border='1' style='background-color: grey'>~&"))
  (format s "  <tr><td>Name<td><b>~A</b>~&" (name p))
  (format s "  <tr><td>Seat<td>~A~&" (seat p))
  (format s "  <tr><td>Active?<td>~A~&" (poker::active? p))
  (format s "  <tr><td>Cards<td>~A~&" (poker::karty->html (poker::cards p)) )
  (format s "  <tr><td>History<td>~A~&" (poker::history p))
  (format s "  <tr><td>Money<td>~,2F~&" (poker::money p))
  (format s "  <tr><td>Balance<td>~,2F~&" (poker::balance p))
  (format s "  <tr><td>T.B.<td>~,2F~&" (poker::total-balance p))
  (format s "  <tr><td>gain<td>~,2F~&" (poker::gain p))  
  (format s "  <tr><td>profile<td>~&~A~&~%" (view(poker::profile p)))
  (format s "  <tr><td>cortex<td>~&~A~&~%" (view (poker::cortex p)))
  (format s "</table>~&~%")))



(defmethod view((g game))
  (with-output-to-string (s)
    (format s "<table>~&")
    (format s "  <tr><td colspan='2'><b>GAME INFO</b>")
    (format s "  <tr><td>casino<td>~A~&" (poker::casino g))
    (format s "  <tr><td>id<td>~A~&" (poker::id g))
    (format s "  <tr><td>czas<td>~A~&" (dekoduj-czas (poker::czas g)))
    (format s "  <tr><td>kind<td>~A~&" (poker::kind g))
    (format s "  <tr><td>blind<td>~A~&" (poker::blind g))
    (format s "  <tr><td>table-name<td>~A~&" (poker::table-name g))
    (format s "  <tr><td>table-size<td>~A~&" (poker::table-size g))
    (format s "  <tr><td>agent-name<td>~A~&" (poker::agent-name g))
    (format s "  <tr><td>agent-pos<td>~A~&" (poker::agent-pos g))
    (format s "  <tr><td colspan='2'><b>GAME STATE</b>")
    (format s "  <tr><td>free-seat<td>~A~&" (poker::free-seat g))
    (format s "  <tr><td>table-cards<td>~A~&" (poker::karty->html (poker::table-cards g)))
    (format s "  <tr><td>cards<td>~A~&" (poker::cards g))
    (format s "  <tr><td>history<td>~A~&" (poker::history g))
    (format s "  <tr><td>pot<td>~A~&" (poker::pot g))
    (format s "  <tr><td>stake<td>~A~&" (poker::stake g))
    (format s "</table>~&")

    (format s "<b>Memory:</b>~A<br>" (poker::game-memory g))
    (format s "<b>Cortex:</b>~A<br>" (view (poker::cortex g)))
    (when (poker::profile g)
       (format s "<b>Profile:</b>~A<br>" (view (poker::profile g)))    )
    (format s "<table><tr>~&")
    (dotimes (i (free-seat g))
     (format s "<td>~A~&" (view (get-player i g))))
    (format s "</table>~&")   ))

(def-url "/view"
    (basic-page
      (menu1)
      (aif (param "id")
	   (str (view 
		 (find (read-from-string it)
		       (poker::games *gameslist*) 
		       :test #'equal
		       :key #'poker::id)))
	   (str (view 
		 (head *gameslist*)))  )))

(def-url "/perceptview"
    (basic-page
      (menu1)
      (poker::next-percept (head *gameslist*))
      (str (view (head *gameslist*)))))


(defmethod render((p poker::player) game)
  (format nil "~A<td>~A<td>~A<td>~A<td>~A<td>~A<td>~A<td>~A" 
	  (if (poker::active? p) "<tr>" "<tr style='background-color: grey'>")
	  (poker::name p)
	  (poker::money p) (poker::total-balance p) (poker::balance p)
	  (aif (poker::cards p) (format nil "~A~A" (poker::karty->html it) (view-ocena it game))  "")
	  (first (poker::history p))
	  (rest (poker::history p))) )

(defun prev-game(g)
  (let ((nr (position (poker::id g) (poker::db-games poker::*gameslist*) :key #'car))
	(ilosc (length (poker::db-games poker::*gameslist*) )))
    (with-output-to-string (s)
      (when (plusp nr) (format s "[ <a href='/readgame?id=~A'>prev game</a> ] " 
				(car (nth (1- nr) (poker::db-games poker::*gameslist*)  ))))
      (format s " [ <a href='/readgame?id=~A'> reload  </a> ] " (poker::id g))
      (when (< nr (1- ilosc)) (format s " [  <a href='/readgame?id=~A'>next game</a> ]" 
				(car (nth (1+ nr) (poker::db-games poker::*gameslist*)  ))))

    )))

(defun percept-page(g)
  (basic-page
    (str (prev-game g)) (menu1)
    (poker::next-percept g)
    (:h2 "Game " (str (poker::id g)))
    (:h3 "played on table: " (str (poker::table-name g)) " ; " (str (dekoduj-czas (poker::czas g))))
    (awhen (poker::table-cards g) (fmt "<h3 style='border-left:10px solid green;padding-left: 10px'>Table cards: ~A</h3>"
				       (poker::karty->html (sort (copy-list it) #'<))))
    (fmt "agent-pos: ~A, free-seat: ~A, pot: ~A<br>" 
	 (poker::agent-pos g) 
	 (poker::free-seat g) 
	 (poker::pot g))
    (let ((pn (poker::percept-nr g)))
      (when (plusp pn)
	(let ((p0 (nth pn (poker::db-history g))))
	  (htm (fmt "Last percept: <b>~A</b>" 
		    (if (symbolp (first p0))
			(first p0)
			(format nil " ~A ~A ~A" 
				(car (nth (first p0) (poker::db-players g)))
				(second p0) (third p0)	    )))) )))
   
    ((:table :border "1")
     (dotimes (i (poker::free-seat g))
       (str (render (aref (poker::players g) i) g))    ))
    (let ((np (nth (1+ (poker::percept-nr g)) (poker::db-history g))))
      (when (numberp (car np))
	(when (equal (car 
		      (nth (car np) (poker::db-players g)))
		     (poker::agent-name g))
	  (fmt "<div style='border-left:10px solid red;color:red;padding-left:10px;'>~A should: ~A</div>" (poker::agent-name g) (poker::action-rules g))
	  (fmt "<div style='border-left:10px solid black;padding-left:10px;'>Memory :~A</div>" (poker::game-memory  (poker::head poker::*gameslist*) ))
	  )))
   ))

(def-url "/perceptpage"
      (percept-page  (poker::head poker::*gameslist*)  ))


(defun describe-table(name)
  (cond
    ((string= name "Ferrero")     "Omaha 0.25/0.5")
    ((string= name "Angers")      "Omaha    1/2  ")
    ((string= name "Electrified") "HL NL    1/2  ")
    (t "")))

(def-url "/gameslists"
  (basic-page
    (let ((zbiory (db:poker-query "select nr,start_period,table_name from games_list")))
      (dolist (z zbiory)
	(fmt "<a href='/selectgameslist?nr=~A'>~A,~A</a><br>" 
	     (first z) (describe-table (third z)) (dekoduj-czas(second z))  )) )))

(defun selectgameslist(nr)
  (basic-page
    (menu1)
    (when nr
      (poker::simulate-games-list nr)
      (fmt "~A games selected..." (length (poker::games poker::*gameslist*)))
	)))

(def-url "/selectgameslist"
    (selectgameslist (nparam "nr")))



(defun view-profits(game)
  (with-output-to-string(s)
    (dotimes (i (poker::free-seat game))
      (let* ((plr (poker::get-player i game))
	     (pr (poker::profit plr)))
	(when (/= pr 0)
	  (format s "~A:~,2F   " (poker::name plr) pr))))))

(def-url "/choosegame"
    (basic-page
      (fmt "<table border='3'>")
      (let ((suma 0))
      (dolist (g (poker::games poker::*gameslist*))
	(incf suma (poker::profit (poker::get-agent g)))
	(fmt "<tr><td><a href='/view?id=~A'>Show</a><td><a href='/readgame?id=~A'>~A</a><td>~A<td>~A<td>~,2F<td>~A" 
	     (poker::id g)
	     (poker::id g)  (poker::id g)
	     (dekoduj-czas(poker::czas g))
	     (poker::karty->html (poker::cards g))
	     (poker::profit (poker::get-agent g))
	     (view-profits g) ))
      (fmt "</table>")
      (fmt "<h2>profit: ~,2F</h2>" suma))        ))

(defun id->nr(id)
  (position id (poker::games poker::*gameslist*) :key #'poker::id :test #'equal))

(defun find-prev-game(id)
  (let* ((nid (read-from-string id))
	 (nr (id->nr nid)))
    (when (plusp nr)
      (nth (1- nr) (poker::games *gameslist*)))))

(def-url "/readgame"
    (basic-page
      (let ((id (param "id")))
	(when id (setf (poker::head poker::*gameslist*) 
		       (poker::read-game (read-from-string id) (find-prev-game id)))
	      (fmt "Loaded ...<a href='/perceptpage'>~A</a>" id)
	      ))))


;;---- view games----------------


(def-url "/gameslist"
    (basic-page
      (fmt "<table border='3'>")
      (dolist (g (poker::games poker::*gameslist*))
	(fmt "<tr><td><a href='/view?id=~A'>~A</a><td>~A<td>~A" 
	     (poker::id g)
	     (poker::id g)
	     (dekoduj-czas(poker::czas g))
	     (poker::karty->html (poker::cards g)) ))
      (fmt "</table>"))        )