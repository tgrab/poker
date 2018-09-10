(defstruct log
  id
  tablename
  date
  history
  holecards
  tablecards
  shown)

#+win(defvar *logroot* "d:\\logs")
#+linux(defvar *logroot* "/opt/logs")
(defvar *logyear* 2010)
(defvar *logmonth* nil)
(defvar *logday* nil)
(defvar *logtablename* nil)

(defvar *log-games1* nil)
(defvar *g1* nil)

(defun get-log-years()
  (mapcar 
   #'(lambda(d) 
       (car (last (pathname-directory d))))
   (cl-fad:list-directory *logroot*)))


(defun get-log-months(year)
  (mapcar 
   #'(lambda(d) 
       (car (last (pathname-directory d))))
   (cl-fad:list-directory (format nil "~A/~A" *logroot* year))))


(defun get-log-days(year month)
  (mapcar 
   #'(lambda(d) 
       (car (last (pathname-directory d))))
   (cl-fad:list-directory (format nil "~A/~A/~A" *logroot* year month))))

(defun get-log-tables(year month day)
  (mapcar 
   #'(lambda(d) 
       (car (last (pathname-directory d))))
   (cl-fad:list-directory (format nil "~A/~A/~A/~A" *logroot* year month day))))

(defun get-log-games(year month day tablename)
  (mapcar 
   #'(lambda(d) 
       (pathname-name d))
   (cl-fad:list-directory (format nil "~A/~A/~A/~A/~A" *logroot* year month day tablename))))

(defun read-log(name &optional (y *logyear*) (m *logmonth*) (d *logday*) (tn *logtablename*))
  (with-open-file 
      (f 
       (format nil "~A/~A/~A/~A/~A/~A.lsp" *logroot* y m d tn  name))
    (eval (read f))))


(defun simulate-log(l)
  (setq *g1* (new-game :id (log-id l) :czas (log-date l) :prev-game *g1* :save-log nil))
  (awhen (log-holecards l)
    (holecards *g1* (nth 0 it)  (nth 1 it) (nth 2 it) (nth 3 it)))
  (dolist (p (log-history l))
    (when (eq 'sb (car p)) (small-blind (second p) (third p) *g1*))
    (when (eq 'bb (car p)) (big-blind (second p) (third p) *g1*))
    (when (eq 'raise (car p)) (raise (second p) (third p) *g1*))
    (when (eq 'bet (car p)) (bet (second p) (third p) *g1*))
    (when (eq 'allin (car p)) (allin (second p) (third p) *g1*))
    (when (eq 'fold (car p)) (fold (second p)  *g1*))
    (when (eq 'call (car p)) (call (second p) (third p) *g1*))
    (when (eq 'check (car p)) (call (second p) 0 *g1*))
    (when (eq 'flop (car p)) (let ((tbl (reverse (log-tablecards l))))
			       (round-flop (nth 0 tbl) (nth 1 tbl) (nth 2 tbl) *g1* )))
    (when (eq 'turn (car p)) (let ((tbl (reverse (log-tablecards l))))
			       (round-turn (nth 3 tbl) *g1* )))
    (when (eq 'river (car p)) (let ((tbl (reverse (log-tablecards l))))
			       (round-river (nth 4 tbl)  *g1* )))  )

  (dolist (p (log-shown l))
   (let ((plr (get-player (car p) *g1*)))
     (setf (cards plr) (cdr p))) )
  (awhen (log-holecards l)    
    (setf (cards (get-player "grabola" *g1*)) it)  ))

(defun log-balance(l &optional ( name "grabola"))
  (let ((b 0))
    (dolist (e (log-history l) b)
      (when (and (= 3 (length e))
		 (equal name (second e)))
	(decf b (third e)))  )))

(defmethod hands((l log))
  (let ((h nil))
    (awhen (log-holecards l)
      (push (cons "grabola" it) h))
    (dolist (s (log-shown l))
      (unless (string= "grabola" (car s))
	(push s h)))
    h))
