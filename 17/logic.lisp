;------------------------ Logic -------------------------
(defconstant fail nil)
#+cmu(defconstant no-bindings '((t . t)))
#+sbcl(defvar no-bindings '((t . t)))

;; ------- Funkcje pomocnicze ----------------

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eq x '?))))

(defun replace-?-vars (exp)
    "Replace any ? within exp with a var of the form ?123."
    (cond ((eq exp '?) (gensym "?"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-?-vars (first exp))
			 (replace-?-vars (rest exp))
			 exp))))


(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))


(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))

;; --------------------------------------------------

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) 
                (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        (t (extend-bindings var x bindings))))


(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))

;; -------- BAZA WIEDZY -----------------------------

;; Clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; Clauses are stored on the predicate's plist
(defun get-clauses (pred) 
  (append (gethash pred (percept-memo +game+))
	  (gethash pred (round-memo +game+))
	  (gethash pred (game-memo +game+))
	  (gethash pred (memo +game+))
	  (gethash pred (rules +game+)) ))

(defun predicate (relation) (first relation))

(defmacro <- (&rest clause)
  "add a clause to the data base."
  `(add-clause (rules +game+) ',(replace-?-vars clause)))

(defmacro <-into (place &rest clause)
  "add a clause to the data base."
  `(add-clause ,place ',(replace-?-vars clause)))

(defmacro <=into (place &rest clause)
  "add a clause to the data base."
  `(set-clause ,place ',(replace-?-vars clause)))

(defun add-clause (place clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (first clause))))
    (pushnew pred (predicates +game+))
    (setf (gethash pred place)
          (nconc (gethash pred place)  (list clause)))
    pred))

(defun set-clause (place clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (first clause))))
    (pushnew pred (predicates +game+))
    (setf (gethash pred place)
          (list clause))
    pred))



;; -------- WNIOSKOWANIE ----------------------------------
(defparameter *computable* '(= < > <= >= ?- figura kolor progn 1+))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
;  (format t "P:~A~&--~A~&--~A~&--------~&" goal bindings other-goals)
  (if (member (predicate goal) *computable*)
      (and (eval (sublis bindings goal))
	   (prove-all other-goals bindings))
      (some #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all 
		 (append (clause-body new-clause) other-goals)
		 (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal)))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
;  (format t "ALL:~A~&--~A~&--------~&" goals bindings)
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)  ))))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defmacro ?- (&rest goals) `(prove-all ',(replace-?-vars goals) no-bindings))


(defun get-memory(&optional (game +game+))
  (list (percept-memo game)
	(round-memo game)
	(game-memo game)
	(memo game)
	(rules game)))

(defun clear-percept-memory()
   (clrhash (percept-memo +game+)))

(defun clear-round-memory()
  (clear-percept-memory)
  (clrhash (round-memo +game+)))

(defun clear-game-memory()
  (clear-round-memory)
  (clrhash (game-memo +game+)))

(defun clear-memory()
  (clear-game-memory)
  (clrhash (memo +game+)))

(defun clear-db()
  (clear-memory)
  (clrhash (rules +game+)))

(defun show-db()
    (mapc #'(lambda (x) (format t "~%") (moje:show-hashtable x) )
	  (get-memory))
    'ok)

