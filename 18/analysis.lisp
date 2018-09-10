;zbior uczacy zapisany symbolicznie
(defvar *l-set* nil)

;(setq *l-set*
;      '(((fold bet1)        (bets 1))
;       ((call stake-zero)  (stake zero))))

;j.w. zawiera tylko car z l-set
;trzba te gry wczytac i pozniej rozwinac
(defvar *l-short-set* nil) 


(defvar *examples* nil)

;l-set zamieniony na wektory
;(setq *examples*
;      '(#((call rule1) 0 1 1 0)
;	#((fold rule2) 1 0 nil 0)
;	#((bet rule3)  1 0 0 1)))

;zdania ktore mozna uzywac w systemie
(defvar *questions* nil)

(setq *questions*
      '((bets 0) (bets 1) (bets 2)
	(position blind) (position early) (position middle) (position late)
	(stake zero) (stake minimal) (stake big) (stake commited)
	(hand ace-suited) (hand big-pair) (hand middle-pair) (hand small-pair) (hand two-pairs) (hand str) (hand 3-str) 
	(hand connectors) (hand no-dangler)))


;------ wnioskowanie
(defun gen-integers(ile)
  (let (w)
    (dotimes (i ile (nreverse w))
      (push i w))))


(defun constrain-examples(key val subset)
  (let ((new-set nil))
  (dolist (e subset new-set)
    (when (or (null (aref (nth e *examples*) (1+ key)))
	      (eq val (aref (nth e *examples*) (1+ key))))
      (push e new-set))  )))

(defun select-decision(vect &optional (key 0) (subset (gen-integers (length *examples*))))
  (let ((outcome (constrain-examples key (aref vect key) subset)))
    (if (or (null outcome) (=  key (1- (array-dimension vect 0))) )
	outcome
	(select-decision vect (1+ key) outcome)) ))

;decyzja to (select-decision(ask-questions))
(defun ask-questions()
  "dla obecnej gry zwraca wektor typu *examples*"
  (let* ((ile (length *questions*))
	(wynik (make-array ile)))
    (dotimes (i ile wynik)
      (if (prove-all (list (nth i *questions*)) no-bindings)
	  (setf (aref wynik i) 1)
	  (setf (aref wynik i) 0)))))

(defun get-response()
  (mapcar #'(lambda (n) (nth n *examples*)) (select-decision(ask-questions))))
;-----------------------------------------------------------------------


(defun gen-example(ex)
  "zamienia *l-set* na *examples* wykorzystujac *questions*"
  (let* ((ile (length *questions*))
	(wynik (make-array (1+ ile))))
    (setf (aref wynik 0) (car ex))
    (dotimes (i ile wynik)
      (if (find (nth i *questions*) ex :test #'equal)
	  (setf (aref wynik (1+ i)) 1)
	  (setf (aref wynik (1+ i)) 0)))))

(defun gen-examples()
  (setq *examples* nil)
  (dolist (e *l-set*)
    (push (gen-example e) *examples*)))

(defun make-learning-sentence(&optional (conclusion (list 'rule (random 1000))))
  (let ((wynik (list conclusion)))
    (dolist (s *questions* (nreverse wynik))
       (when (prove-all (list s) no-bindings)
	 (push s wynik)))))
      


(def-url "/learn"
    (main-page
      (format t "<form><select name='action'>
                 <option>FOLD</option><option>CALL</option><option>POTBET1</option>
                 </select><input type='submit'></form>")
      (let ((action (get-parameter "action")))
	(when action
	  (let ((sent (make-learning-sentence (list (intern action)
					      (id +game+) (czas +game+)
					      (logs-percept-nr *log*)))))
	    (push sent *l-set*)
	    (princ sent))  	))))

;------------ Obsluga skroconych zbiorow l-short-set

(def-url "/learnshort"
    (main-page
      (format t "<form><select name='action'>
                 <option>FOLD</option><option>CALL</option><option>POTBET1</option>
                 </select><input type='submit'></form>")
      (let ((action (get-parameter "action")))
	(when action
	  (let ((sent (list (intern action)
			    (id +game+) (czas +game+)
			    (logs-percept-nr *log*))))
	    (push sent *l-short-set*)
	    (princ sent))  	))))

(defun short-set-loadable?()
  (let ((p t))
    (dolist (e *l-short-set* p)
      (unless (find (second e) *games-list* :test #'string= :key #'id)
	(setq p nil)
	(format t "Brakuje ~A ,~A~&" (second e) (dekoduj-czas (third e)) )))))

(defun expand-short-set()
  (unless (short-set-loadable?)
    (return-from expand-short-set ))
  (setq *l-set* nil)
  (dolist (e *l-short-set*)
    (go-to-percept (second e) (fourth e))
    (rules-action-prove)
    (push (make-learning-sentence e) *l-set*)))

; (analyze-all-actions) (expand-short-set) (gen-examples)


;;TODO 
;a. przeleciec gry i utworzyc *l-set* z decyzji uzytkownikow(moze tylko zwyciezkich)
;b. poprawic funkcje decyzyjne (wiekszosc akcji itd.)
;c. automatyczna aktualizacja po zmianie *questions*
;d. siec Instar(?)



;test1
(defun analyze-actions(id)
  (simulate-game id
    (let ((nxt (get-log-percept (1+ nr))))
      (when (and (consp nxt) (= (car nxt) (agent-pos +game+)) (plusp nr))
	;nastepna bedzie akcja agenta
	;(print nxt)
	(push (list (second nxt) id (czas +game+) nr ) *l-short-set*))
    )))

;odrazu tworzy wektory w *examples*
(defun analyze-actions2(id)
  (simulate-game id
    (let ((nxt (get-log-percept (1+ nr))))
      (when (and (consp nxt) (= (car nxt) (agent-pos +game+)) (plusp nr))
	;nastepna bedzie akcja agenta
	;(print nxt)
	(rules-action-prove)
	(let* ((ile (length *questions*))
	       (wynik (make-array (1+ ile))))
	  (setf (aref wynik 0) (list (list (second nxt) id (czas +game+) nr )))
	  (dotimes (i ile wynik)
	    (if (prove-all (list (nth i *questions*)) no-bindings)
		(setf (aref wynik (1+ i)) 1)
		(setf (aref wynik (1+ i)) 0)))
	    (push wynik *examples*))
    ))))

(defun analyze-all-actions()
  (setq *l-short-set* nil)
  (dolist (g (get-game-infos))
    (analyze-actions (id g))))

(defun analyze-all-actions2()
  (setq *examples* nil)
  (dolist (g (get-game-infos))
    (analyze-actions2 (id g))))


;;---------- tablica haszujaca bit wektorow
(defvar *rules* nil)

(defstruct action-ratio
  (fold 0)
  (call 0)
  (bet 0)
  (potbet1 0))

(defun dodaj-vector(v akcja)
  (let ((val (gethash v *rules*)))
    (if val
	(case akcja
	  (f (incf (action-ratio-fold val)))
	  (c (incf (action-ratio-call val)))
	  (r (incf (action-ratio-bet val))) )
	(progn
	  (setf (gethash v *rules*) (make-action-ratio))
	  (dodaj-vector v akcja))) ))

(defun analyze-actions3(id)
  (simulate-game id
    (let ((nxt (get-log-percept (1+ nr))))
      (when (and (consp nxt) (= (car nxt) (agent-pos +game+)) (plusp nr))
	;nastepna bedzie akcja agenta
	;(print nxt)
	(rules-action-prove)
	(let* ((ile (length *questions*))
	       (wynik (make-array ile :element-type 'bit )))
	  (dotimes (i ile wynik)
	    (if (prove-all (list (nth i *questions*)) no-bindings)
		(setf (aref wynik i) 1)
		(setf (aref wynik i) 0)))
	    (dodaj-vector wynik (second nxt)))
    ))))

(defun analyze-all-actions3()
  (setq *rules* (make-hash-table :test #'equal))
  (dolist (g (get-game-infos))
    (analyze-actions3 (id g))))

(defun get-questions-bit-vector()
  "dla obecnej gry zwraca wektor typu *examples*"
  (let* ((ile (length *questions*))
	(wynik (make-array ile :element-type 'bit)))
    (dotimes (i ile wynik)
      (if (prove-all (list (nth i *questions*)) no-bindings)
	  (setf (aref wynik i) 1)
	  (setf (aref wynik i) 0)))))