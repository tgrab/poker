(in-package :poker)

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


;;---------------------------------------------------------------------

(defun wh-card(card)
  (let ((rank (char card 0))
	(kolor (char card 1)))
  (+
   (cond 
     ((eq #\2 rank) 13)
     ((eq #\3 rank) 12)
     ((eq #\4 rank) 11)
     ((eq #\5 rank) 10)
     ((eq #\6 rank) 9)
     ((eq #\7 rank) 8)
     ((eq #\8 rank) 7)
     ((eq #\9 rank) 6)
     ((eq #\T rank) 5)
     ((eq #\J rank) 4)
     ((eq #\Q rank) 3)
     ((eq #\K rank) 2)
     ((eq #\A rank) 1)
     (t 0)    )

  (* 13
     (cond 
       ((eq #\c kolor) 3)
       ((eq #\d kolor) 2)
       ((eq #\h kolor) 1)
       ((eq #\s kolor) 0)
       (t 0)))     )))

(defclass crypto-log(game)
  ((money :initform nil :accessor money)
   (winners :initform nil :accessor winners)
   (cards-log :initform nil :accessor cards-log)
   (uncalled-bets :initform nil :accessor uncalled-bets))  )


(defun zakoduj-date(dzien godzina)
  ;(zakoduj-date "2008/04/02" "23:34:41")
  (destructuring-bind (rok mies dzien godz min sek)
		      (mapcar #'read-from-string
			      (tokens (concatenate 'string dzien "/" godzina) '(#\/ #\:)))
    (encode-universal-time sek min godz dzien mies rok)))

(defun whlog->game(filename)
  (macrolet ((player () '(clean-var (get-var ?who) #\:))
	     (amount () '(let ((a (get-var ?amount)))
			  (if (digit-char-p (char a 0))
			      (read-from-string a)
			      (read-from-string (subseq a 1)))))
	     (karta (c) `(wh-card (clean-var (get-var ,c) '(#\[ #\])))))
    (format t "Opening ~A~&" filename)
   (with-open-file (f filename)
    (do ((line (read-line f nil 'end) (read-line f nil 'end)))
	((eq line 'end))
      #+cmu(setq line (remove-return line))     
      (or
       (with-pattern '("Game" ?id ?kind ?limit ?stakes "-" ?day "-" ?hour "(UK)")
	 ;(print pat)
	 (setq *game* ;(new-game :id (clean-var (get-var ?id) '(#\: #\#)))
	       (make-instance 'crypto-log :id (clean-var (get-var ?id) '(#\: #\#))))
	 (cond
	   ((string= "Omaha" (get-var ?kind))  (setf (kind *game*) 3) )
	   ((and 
	     (string= "Hold'em" (get-var ?kind))
	     (string= "NL" (get-var ?limit)))  (setf (kind *game*) 2) )
	   (t (return-from whlog->game nil)))
	 (setf (agent-name *game*) "Grabol")
	 (setf (czas *game*) (zakoduj-date (get-var ?day)(get-var ?hour))))
       (with-pattern '("Seat" ?nr ?who ?amount "in" "chips)")
	 (push (cons  (get-var ?who)  (read-from-string (subseq (get-var ?amount) 2)))
	       (money *game*)))
       (with-pattern '("Table" ?name ??)
	(setf (table-name *game*) (string-trim '(#\")(get-var ?name))) )
       (with-pattern '(?who "collected" ?amount "from" "Main" "pot")
	(push (cons (get-var ?who) (read-from-string (subseq (get-var ?amount) 1))) (winners *game*)) )
       (with-pattern '("Returned" "uncalled" "bets" ?amount "to" ?who)
	(push (cons (get-var ?who) (read-from-string (subseq (get-var ?amount) 1))) (uncalled-bets *game*)) )
       (with-pattern '(?who posts small blind ?amount)
	(small-blind (player) (amount)))
      (with-pattern '(?who posts big blind ?amount)
	(big-blind (player) (amount)))
      (when (omaha? *game*)
	(with-pattern '("dealt" "to" "Grabol" ?c1 ?c2 ?c3 ?c4)
	  (holecards *game*  (karta ?c1)  (karta ?c2) (karta ?c3) (karta ?c4) )))
      (when (holdem? *game*)
	(with-pattern '("dealt" "to" "Grabol" ?c1 ?c2)
	  (holecards *game*  (karta ?c1)  (karta ?c2) )))
      (with-pattern '(?who "folds")
	(fold (player) ))
      (with-pattern '(?who "calls" ?amount)
	(call (player) (amount) ))
      (with-pattern '(?who "checks")
	(call (player) 0 ))
      (with-pattern '(?who "bets" ?amount)
	(bet (player) (amount) ))
      (with-pattern '(?who "raises" "to" ?amount)
	(raise (player) (amount) ))
      (with-pattern '(?who "raises" "to" ?amount "and" "is" "all-in")
	(allin (player) (amount) ))
      (with-pattern '(?who  "is" "all-in" ?amount)
	(allin (player) (amount) ))
      (with-pattern '(?who "bets"  ?amount "and" "is" "all-in")
	(allin (player) (amount) ))
      (with-pattern '("-----" "FLOP" "-----" ?c1 ?c2 ?c3)
	(round-flop  (karta ?c1)  (karta ?c2) (karta ?c3) ))
      (with-pattern '("-----" "TURN" "-----" ?c1 ?c2 ?c3)
	(round-turn  (wh-card (car (last (tokens (get-var ?c3) '(#\[ #\])))))  ))
      (with-pattern '("-----" "RIVER" "-----" ?c1 ?c2 ?c3 ?c4)
	(round-river  (wh-card (car (last (tokens (get-var ?c4) '(#\[ #\])))))  ))

      (when (omaha? *game*)
	(with-pattern '(?who "shows" ?c1 ?c2 ?c3 ?c4 ??)
	  (push (cons (player)
		    (list (karta ?c1)  (karta ?c2) (karta ?c3) (karta ?c4)))
		(cards-log *game*)))
	(with-pattern '(?who "mucks" "hand" ?c1 ?c2 ?c3 ?c4)
	  (push (cons (player)
		      (list (karta ?c1)  (karta ?c2) (karta ?c3) (karta ?c4)))
		(cards-log *game*)))   )
      (when (holdem? *game*)
	(with-pattern '(?who "shows" ?c1 ?c2 ??)
	  (push (cons (player)
		      (list (karta ?c1)  (karta ?c2) ))
		(cards-log *game*)))
	(with-pattern '(?who "mucks" "hand" ?c1 ?c2)
	  (push (cons (player)
		      (list (karta ?c1)  (karta ?c2)))
		(cards-log *game*))))
       ))))
  'ok)

(defun wh-postprocessing(crypto-log)
;  (dolist (c (winners crypto-log))
 ;   (setf
  ;   (money (get-player (first c) crypto-log))
   ;  (rest c))
  (dolist (c (money crypto-log))
    (aif (get-player (first c) crypto-log)
	 (setf    (money it)      (rest c)))) 
  (dolist (c (cards-log crypto-log))
   (aif (get-player (first c) crypto-log)
	 (setf    (cards it)      (rest c))) ))

(defmethod sql-view((g crypto-log))
  (format nil "(~A,~A,'~A',~A,'~S','~S','~S')" 
	  (id g) (czas g) 
	  (table-name g) (kind g)
	  (mapcan #'(lambda (p) (when p (list (list (name p) (money p) (cards p)))))
		  (coerce (players g) 'list))
	  (history g)
	  (list (list 'winners (winners g))
		(list 'uncalled-bets (uncalled-bets g)))  ))


(defmethod insert-game((g crypto-log))
  (handler-case 
      (db:poker-update (format nil "insert into crypto values ~A" (sql-view g)))
    (error (e) (format t "!!! ~A !!!~&" e))))

(defun logs-wh-readdir(dir)
  (dolist (f (list-directory dir))
    (if (directory-pathname-p f)
	(logs-wh-readdir f)
	(let ((znak (char-code (char (pathname-name f) (1- (length  (pathname-name f)))))))
	  (when (and (>= znak 48) (<= znak 57))
	    (whlog->game f)
	    (wh-postprocessing *game*)
	    (insert-game *game*))))))


(defun logs-wh-temp(dir)
  (setf (games *gameslist*) nil)
  (dolist (f (list-directory dir))
    (if (directory-pathname-p f)
	(logs-wh-temp f)
	(let ((znak (char-code (char (pathname-name f) (1- (length  (pathname-name f)))))))
	  (when (and (>= znak 48) (<= znak 57))
	    (when (whlog->game f)
	      (wh-postprocessing *game*)
	      (push *game* (games *gameslist*)))
	      (insert-game *game*)
	    )))))

(defun wh-gameslist-postprocessing()
  (let ((st (lista-stolow (games *gameslist*))))
    (when st
      (dolist (s st)
	(let ((prz (zbior-przedzialow (lista-gier-na-stole s (games *gameslist*)))))
	  (dolist (p prz)
	    (db:poker-update
	     (format nil "insert into games_list values(DEFAULT,'crypto','~A',~A,~A)~&"
		     s (car p) (cdr p)))  	    ))))))

(defun lista-stolow(gry)
  (let ((wynik nil))
    (dolist (g gry wynik)
      (pushnew (table-name g) wynik :test #'equal ))))

(defun lista-gier-na-stole(stol gry)
  (let ((wynik nil))
    (dolist (g gry (sort wynik #'<))
      (when (and (string= stol (table-name g))  )
	(push (czas g) wynik)))))

(defun zbior-przedzialow(czasy)
  (let ((pocz (first czasy))
	(koniec (first czasy))
	(wynik nil))
    (dolist (c (cdr czasy))
      (if (> (+ koniec 300) c) ;5 minut
	  (setq koniec c)
	  (progn
	    (push (cons pocz koniec) wynik)
	    (setq pocz c koniec c)    ))  )
    (push (cons pocz koniec) wynik)
    wynik))


;;------------- File IO -----------------------------------------
(defun remove-return(line)
"Usuwa znak konca wiersza na koncu linii
Arg: line:String
Out: String"
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