(in-package :poker)



#|
;;----------- old code --------------------------------



;zawiera prawdopodobienstwa roznych akcji na danym stole
;korzysta z listy elementow player-info
(defclass player-profile()
  ((name :initform nil :initarg :name :accessor name)
   (vpip :initform 0 :initarg :vpip :accessor vpip)
   (vpip-short :initform 0 :initarg :vpip-short :accessor vpip-short)
   (pfr :initform 0 :initarg :pfr :accessor pfr)
   (pfr-short :initform 0 :initarg :pfr-short :accessor pfr-short)
   (cont-bet :initform 0 :accessor profile-cont-bet)
  ))


(defmethod print-object((p player-profile) str)
  (format str "[~A: ~As ~A->~A   ~As ~A->~A]~&" (name p) (vpip-short p) (vpip p) (describe-vpip (vpip p))
	  (pfr-short p)  (pfr p) (describe-pfr (pfr p))))

  

(defun srednia-if(fun lista)
  (let ((ile (length lista)))
    (if (plusp ile)
	(/ (count-if fun lista) ile 1.0)
	0)))

(defun make-player-profile(name &optional id)
  "liczy dla danego stolu"
  (let* ((all-infos (get-table-player-history name id))
	 (infos (subseq all-infos 0 (min 10 (length all-infos)))))
    (make-instance 'player-profile
		   :name name
		   :vpip-short (srednia-if #'player-playing infos)
		   :vpip (srednia-if #'player-playing all-infos)
		   :pfr-short (srednia-if #'player-raising? infos)
		   :pfr (srednia-if #'player-raising? all-infos))
  ))




(defun make-game-profiles(&optional id)
  (let ((old-games (get-table-history id)))
    (when old-games
      (let* ((pls (mapcar #'car (name->seat (car  old-games))))
	    (profs (mapcar (lambda (p) (make-player-profile p id))  pls)))
	 profs))))




;TODO dodac rozne stoly
(defun describe-vpip(vpip)
   (cond
      ((>= vpip 0.44) 'extremely_loose)
      ((>= vpip 0.33) 'loose)
      ((<= vpip 0.18) 'tight)
      (t 'normal)))

(defun describe-pfr(pfr)
   (cond
      ((>= pfr 0.2) 'maniac)
      ((>= pfr 0.09) 'aggressive)
      ((<= pfr 0.03) 'passive)
      (t 'normal)))

(defun get-profile-from-cortex(name)
  (find name *cortex* :test #'string= :key #'name))

;tylko do celow statystycznych
(defun make-player-profile-global(name)
  (let ((infos (gethash player-name *player-infos*) ))
    (make-instance 'player-profile 
		   :vpip (srednia-if #'player-playing infos)
		   :pfr (srednia-if #'player-raising? infos))
  ))



(defun add-player-info(player-name info)
  (push info (gethash player-name *player-infos*)))



(defun same-action?(a p)
  (cond
    ((eq p 'F) (or (string= a "fuzzy-call") (string= a "fold")))
    ((eq p 'C) (or (string= a "fuzzy-potbet1")(string= a "call") (string= a "fuzzy-call")  (string= a "fuzzy-bet")))
    ((or (eq p 'B) (eq p 'R)) (or (string= a "potbet1") (string= a "bet")  (string= a "fuzzy-potbet1") (string= a "fuzzy-bet"))   )))




(defun get-log-percept(&optional (nr  (logs-percept-nr *log*) ))
  (nth nr (reverse (history-log (logs-game *log*)))))



(defun player-playing(info)
  "czy wszedl do gry nie bedac na blindzie"
  (member (car (last (pre-flop-actions info)))
	  '(c b r a)))

(defun player-raising?(info)
  (or (member 'r (pre-flop-actions info))
      (member 'b (pre-flop-actions info))))


(defmethod VPIP(player-name)
  (vpip (make-player-profile-global player-name)))

(defmethod PFR(player-name)
  (pfr (make-player-profile-global player-name)))



(defun create-infos(&optional (g +game+))
  (dolist (player (mapcar #'car (name->seat g)))
    (add-player-info player (create-player-info g player)))      )



(defun create-player-info(g &optional (agent "Grabol") (testing nil))
  ;g musi zawierac cards-log jesli chcemy zmienic agenta!
  (let ((hl (reverse (copy-list (history-log g))))
	;(hl (copy-list (history-log g)))
	(ns (copy-list (name->seat g)))
	(tc (copy-list (table-cards g)))
	(p-nr -1)
	(info (make-instance 'player-info
			     :id (id g)
			     :czas (czas g)
			     :blind (blind g)
			     :table-name (table-name g)
			     :table-size (table-size g))))
    (aif (assoc agent (winners-log g) :test #'string=)
	 (setf (gain info) (cdr it)))
    (aif (assoc agent (uncalled-bets g) :test #'string=)
	 (setf (uncalled-bets info) (cdr it)))
    (unless (assoc agent ns :test #'string=)
      (return-from create-player-info (make-instance 'player-info)))
    ;kontynuujemy jesli agent gral w tej grze
    (new-game :id (id g) :czas (czas g))
    ;TODO - ustawic kind
    (setf (agent-name +game+) agent
	  (agent-pos +game+) (player-pos agent g))
    (let ((karty (if (string= "Grabol" agent) 
		     (cards g)
		     (cdr (assoc agent (cards-log g) :test #'equal)))))
	(when karty
	  (if (= 4 (length karty))
	      (holecards (nth 0 karty) (nth 1 karty)(nth 2 karty)(nth 3 karty))
	      (holecards (nth 0 karty) (nth 1 karty))  )
	  (setf (cards info) (cards +game+))))

    (dolist (p hl)
      (incf p-nr)
      (when testing (print p))
      ;przed akcja
      (when (and (consp p) (= (agent-pos +game+) (car p)))
	(when (and (cards +game+) (> p-nr 1)) ;testujemy akcje tylko gdy znamy karty
	                                      ;i nie sa to poczatkowe blindy
	  (let* ((ap (action-prove))
		(akcja (car (tokens ap)))
		(percept (second p)))
	    (unless (same-action? akcja percept)
	      (when testing (format t "~A {~A} ~A~&" p-nr ap p))
	      (push p-nr (bad-actions info)))

	  ))

	(when (preflop?)
	  (push (second p) (pre-flop-actions info))
	  (when (cards +game+)
	    (when (or (eq 'b (second p)) (eq 'r (second p)) )
	      (unless (or (?- (hand 3-str8))
			  (?- (hand ace-suited))
			  (?- (hand connected))
			  (?- (hand big-pair)) )
		(push 'raise_bad_hand (remarks info)))  )
	    (when (and (eq 'c (second p)) (plusp bets)  )
	      (unless (or (?- (hand 3-str8))
			  (?- (hand ace-suited))
			  (?- (hand connected))
			  (?- (hand big-pair)) )
		(push 'call_bad_hand (remarks info)))  ) )
	  ) )
	
      ;wykonujemy ja
      (percept-executor p ns tc t)
      ;jesli gracz spasowal to wychodzimy:
      (when (and (table-cards +game+) (null (aref (active-players +game+) (agent-pos +game+) )))
	(setf (balance info) (+ (aref (balance +game+) (agent-pos +game+)) (aref (total-balance +game+) (agent-pos +game+)) ) )	
	(return-from create-player-info info))
      )
    (setf (balance info) (+ (aref (balance +game+) (agent-pos +game+)) (aref (total-balance +game+) (agent-pos +game+)) ) )
    info    ))



|#
