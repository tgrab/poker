(defstruct logs
  (prev-nr 0)
  (current-nr 0)
  (next-nr 0)
  (percept-nr -1)
  game)

(defstruct range
  (from 4)
  (to 6)
  (kind 3)
  (table-name nil)
  (blind nil))

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
  (let ((infos (get-player-infos name)))
    (make-instance 'player-profile 
		   :vpip (srednia-if #'player-playing infos)
		   :pfr (srednia-if #'player-raising? infos))
  ))

(defvar *log* (make-logs)) ;tutaj kopiujemy gre z listy i jest ona wyswietlana

(defvar *range* nil)

(defun range-filter(info) ;info jest typu game-info
  (let ((s (table-size info)))
    (and (>= s (range-from *range*)) 
	 (<= s (range-to *range*))
	 (aif (range-table-name *range*)
	      (string= (table-name info) it)
	      t)
	 (aif (range-blind *range*)
	      (= (blind info) it)
	      t))))

(defun filter(lista)
  (mapcan #'(lambda(e)
	     (when (range-filter e)
	       (list e)))
	 lista))

(defun get-player-infos(player-name)
  (if *range*
      (filter  (gethash player-name *player-infos*))
      (gethash player-name *player-infos*)))

(defun add-player-info(player-name info)
  (push info (gethash player-name *player-infos*)))

(defun get-game-infos()
  (if *range*
      (filter  *games-list*)
      *games-list*))


(defun filter-game-history(lista id)
  "zwraca liste wczesniejszych gier na podstawie ktorych profilujemy stol"
  (unless lista (return-from filter-game-history))
  (let* ((g (if id
	       (or (find id lista :test #'string= :key #'id)
		   (car lista))
	       (car lista)))
	 (tbl (table-name g))
	 (czas1 (czas g))
	 (wynik nil))
    (dolist (g1 lista)
      (when (and (string= tbl (table-name g1)) ;ten sam stol
		 (if id 
		     (< (czas g1) czas1)
		     (<= (czas g1) czas1));wczesniej
		 (if id 
		     (< (- czas1 (czas g1)) 3600)
		     (<= (- czas1 (czas g1)) 3600))) ;najwyzej godzine temu
	(push g1 wynik)))    
    (sort wynik #'> :key #'czas)))

(defun get-table-history( &optional id )
  (filter-game-history *games-list* id))

(defun get-table-player-history(player-name &optional id )
     (filter-game-history  (gethash player-name *player-infos*)  id))

(defun clear-player-infos()
  (setq *player-infos* (make-hash-table :test #'equal)))

(defun clear-infos()
  (setq *player-infos* (make-hash-table :test #'equal)
	*games-list* nil))


(defun set-games-list(&optional (directory (format nil "~A/*.log" (view-get-dir))))
  (clear-infos)
  (read-wh-dir directory))
	

(defun add-games-list(&optional (directory (format nil "~A/*.log" (view-get-dir))))
  (read-wh-dir directory))
 

(defun create-infos(&optional (g +game+))
  (dolist (player (mapcar #'car (name->seat g)))
    (add-player-info player (create-player-info g player)))      )


(defun recreate-player-infos()
  (clear-player-infos)
  (mapc #'create-infos *games-list*)
  'ok)



(defun choose-startgame(&key nr id)
  (let ((games (get-game-infos)))
  (when id
    (setq nr (position id games :test #'string= :key #'id)))
  (setq *log* (make-logs
	       :game (nth nr games)
	       :current-nr nr
	       :prev-nr (1- nr)
	       :next-nr (1+ nr)))    ))

(defun go-to-percept(id p-nr)
  (choose-startgame :id id)
  (dotimes (i (1+ p-nr))
    (next-percept)))


(defun same-action?(a p)
  (cond
    ((eq p 'F) (or (string= a "fuzzy-call") (string= a "fold")))
    ((eq p 'C) (or (string= a "fuzzy-potbet1")(string= a "call") (string= a "fuzzy-call")  (string= a "fuzzy-bet")))
    ((or (eq p 'B) (eq p 'R)) (or (string= a "potbet1") (string= a "bet")  (string= a "fuzzy-potbet1") (string= a "fuzzy-bet"))   )))

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

(defun players-list(&optional ile)
  (let ((lista nil)
	(zrodlo (if ile  (subseq  *games-list* 0 ile) *games-list*)))
    (dolist (g zrodlo)
      (dolist (n (name->seat g))
	(pushnew (car n) lista :test #'string=)  ))
    (sort lista #'string<  )))



(defun get-log-percept(&optional (nr  (logs-percept-nr *log*) ))
  (nth nr (reverse (history-log (logs-game *log*)))))

(defmacro simulate-game(id &body body)
  `(progn
    (choose-startgame :id ,id)
    (dotimes (nr (length  (history-log (logs-game *log*))))
      (next-percept)
      ,@body)))




(defun next-percept()
  ;TODO ustawic kind
  (when (= -1 (logs-percept-nr *log*))
    (let ((g (logs-game *log*)))
      (new-game :id (id g) :czas (czas g))
      (setf (agent-name +game+) (agent-name g))
      (setf (table-name +game+) (table-name g))
      (setf (agent-pos +game+) (agent-pos g))
      (setf (money-log +game+) (money-log g))
      (awhen (cards g) (holecards (nth 0 it) (nth 1 it) (nth 2 it) (nth 3 it)))
      ))
     
  (when (<= (logs-percept-nr *log*)  (length  (history-log (logs-game *log*))))
    (incf (logs-percept-nr *log*))
    (percept-executor (nth (logs-percept-nr *log*) (reverse (history-log (logs-game *log*))))
		      (name->seat (logs-game *log*)) 
		      (table-cards (logs-game *log*))
		      t)))

(defun percept-executor(p name->seat table-cards &optional (rules nil) )
	(if (consp p)
	    (cond
	      ((eq (second p) 'SB)  (small-blind (player-at-seat (first p) name->seat) (third p) ))
	      ((eq (second p) 'BB)  (big-blind (player-at-seat (first p) name->seat ) (third p) )  )
	      ((eq (second p) 'F)  (fold (player-at-seat (first p)  name->seat ))  )
	      ((eq (second p) 'C)  (call (player-at-seat (first p)  name->seat) (third p) ))  
	      ((eq (second p) 'B)  (bet (player-at-seat (first p)  name->seat) (third p) )  )
	      ((eq (second p) 'R)  (bet (player-at-seat (first p)  name->seat) (third p) 'R )  )
	      ((eq (second p) 'A)  (allin (player-at-seat (first p)  name->seat) (third p) )  )  )
	    (let ((karty (reverse table-cards)))
	    (cond
	      ((eq p 'FLOP)  (flop (nth 0 karty ) 
				   (nth 1 karty )
				   (nth 2 karty)) :with-rules rules)
	      ((eq p 'TURN)  (turn (nth 3 karty)  :with-rules rules)) 
	      ((eq p 'RIVER)  (river (nth 4 karty)  :with-rules rules))    ))))




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



;funkcje oceny profili graczy
;profile sa tworzone na podstawie historii lub ustawiane
;zewnetrznie

(let ((maniacs nil))
  (defun maniac?(player-name)
    ;TODO ocenic player-infos
    (find player-name maniacs :test #'string=)  )

  (defun add-maniac(player-name)
    (pushnew player-name maniacs :test #'string=))

  (defun get-maniacs()
    maniacs)

  )

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;                                 GAME I/O
;;------------------------------------------------------------------------------
(defun show-game(&optional (g +game+))
  (princ "{ 'game ")
  (format t ":id ~A " (id g))
  (format t ":czas ~A " (dekoduj-czas (czas g)))
  (format t ":table-name ~A " (table-name g))
  (format t ":casino ~A " (casino g))
  (format t ":kind ~A~&" (kind g))
  (format t ":history-log ~A~&" (history-log g))
  (format t ":cards-log ~A~&" (cards-log g))
  (format t ":agent-name ~A " (agent-name g))
  (format t ":table-cards ~A " (karty->napisy (table-cards g)))
  (format t ":cards ~A ~&" (karty->napisy (cards g)))
  (format t "--------- state ----------~&")
  (format t ":pot ~A " (pot g))
  (format t ":blind ~A " (blind g))
  (format t ":stake ~A ~&" (stake g))
  (format t ":balance ~A~&" (balance g))
  (format t ":total-balance ~A~&" (total-balance g))
  (format t ":free-seat ~A " (free-seat g))
  (format t ":agent-pos ~A " (agent-pos g))
  (format t ":name->seat ~A~&" (name->seat g))
  (format t ":history ~A~&" (history g))
  (princ "}") 'ok)


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

(defun save-game(&optional 
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




;; ----------- PATTERN MATCHING ------------------------------------
(defun tokens (str &optional (delimiters (list #\Space))  (start 0))
  (let ((p1 (position-if #'(lambda (c) (not (find c delimiters)))
			 str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda(c) (find c delimiters))
			       str :start  (1+ p1))))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str delimiters (1+ p2))
		    nil)))
	nil)))


;(defmacro tokens->string(tokens)
;  `(concatenate 'string ,@tokens))


(defun match-variable(var input bindings)
  (let ((b (assoc var bindings)))
    (cond
      ((not b) (extend-bindings var input bindings))
      ((string= input (cdr b)) bindings)
      (t nil))))

;(pat-match '("la" ?x "la") (tokens "la la la"))
(defun pat-match(pattern input &optional (bindings '((t . t))) )
  ;(format t "Wywolanie: ~A , ~A z podst.~A~%" pattern input bindings)
  (cond
    ((null bindings) nil)
    ((and (null pattern) input) nil)

    ((variable-p  pattern)
     (if (null input)
	 (cons (cons pattern "") bindings)
	 (match-variable  pattern input bindings) ))
    ((and (stringp pattern) (string= pattern input)) bindings)
    ((and (symbolp pattern) (search (symbol-name pattern) (string-upcase input)))
     bindings  )
    ((and (consp pattern) (listp input))

     (if (and (variable-p (car pattern))
	      (equal (char (symbol-name (car pattern)) 1) #\?))
	 (cons (cons '?? (if input input "")) bindings)
	 (pat-match (cdr pattern) (cdr input)
		    (pat-match (car pattern) (car input) bindings))) )  
    (t nil)    ))


(defun match-string(pattern input)
  (if (stringp input)
      (pat-match pattern (tokens input))
      (pat-match pattern input)))


(defmacro with-pattern(pattern &rest body)
  `(let ((pat (match-string ,pattern line)))
    (when pat
      ,@body)))

(defmacro get-var(var)
  `(cdr (assoc ',var pat)))

(defun clean-var(var znak)
  (if (consp znak)
    (string-trim znak var)
    (string-trim (list znak) var)))

(defmacro get-money(var)
  `(read-from-string  (string-left-trim '(#\$) (get-var ,var))))

;;------------- File IO -----------------------------------------
(defun remove-return(line)
  (let ((dl (length line)))
    (if (and (plusp dl) (eq #\Return (char line (1- dl))))
	(subseq line 0 (1- dl))
	line)))


(defun directory-pathname-p  (p)
    "Is the given pathname the name of a directory? This function can
  usefully be used to test whether a name returned by LIST-DIRECTORIES
  or passed to the function in WALK-DIRECTORY is the name of a directory
  in the file system since they always return names in `directory normal
  form'."
    (and
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))))

(defun component-present-p (value)
    (and value (not (eql value :unspecific))))

  (defun pathname-as-directory (pathname)
    "Return a pathname reperesenting the given pathname in `directory
  form', i.e. with all the name elements in the directory component and
  NIL in the name and type components. Can not be used on wild
  pathnames. Returns its argument if name and type are both nil or
  :unspecific."
    (setf pathname (pathname pathname))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames to directory names."))
    (cond
     ((or (component-present-p (pathname-name pathname))
          (component-present-p (pathname-type pathname)))
      (make-pathname
       :directory (append (pathname-directory pathname) (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname))
      (t pathname))) 


(defun list-directory (dirname)
    "Return a list of the contents of the directory named by dirname.
  Names of subdirectories will be returned in `directory normal form'.
  Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept wildcard
  pathnames; `dirname' should simply be a pathname that names a
  directory. It can be in either file or directory form."
    (let ((wildcard (make-pathname :name :wild
                                   :type :wild
                                   :defaults (pathname-as-directory dirname))))

      #+(or sbcl cmu lispworks)
      ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
      ;; form just the way we want.
      (directory wildcard)

      #+clisp
      ;; CLISP has a particularly idiosyncratic (though arguably
      ;; logically consistent) view of things. And (as of 2.32) it has a
      ;; slight bug to work around. But we can bludgeon even it into
      ;; doing what we want.
      (nconc
       ;; CLISP 2.32 won't list files without an extension when :type is
       ;; wild so we make a special wildcard for it.
       (directory (make-pathname :type nil :defaults wildcard))
       ;; And CLISP doesn't consider subdirectories to match unless
       ;; there is a :wild in the directory component.
       (directory (make-pathname
                   :directory (append (pathname-directory wildcard) (list :wild))
                   :name nil
                   :type nil
                   :defaults wildcard))))) 