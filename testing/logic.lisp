(defvar *facts* nil)
(defvar +r+ nil)

(let ((memo nil) (memorize? nil))
  (defun make-reasoner(rules)
   (if (and memorize? (assoc rules memo))
	(cdr (assoc rules memo))
	(let ((rlz (make-hash-table)))
	  ;(princ "tworzymy hashtable")
	  (dolist (r (symbol-value rules))
	    (setf (gethash (first r) rlz)
		  (nconc (gethash (first r) rlz) (list (rest r)))))
	  (push (cons rules rlz) memo)
	  rlz)))
  (defun memorize-reasoner()
    (setq memorize? (not memorize?)))
  (defun show-reasoners()
   memo))

(defun show-rules(table)
  (maphash #'(lambda (k v)
	       (format t "~A <= ~A~%" k v))
	   table))


(defun fact?(f)
  (member f *facts*))

(defun prove(table goal &optional (dowody t))
 (let (temp temp2)
 (if (consp goal)
     (and (eval goal) (cons 'proved (list goal))) 
     (or (and (fact?  goal) (cons 'fact (list goal)))
         (when (some
		#'(lambda(rule)
		    (setq temp rule)
		    (setq temp2 (prove-all table rule dowody)))
		(gethash goal table))
	   (push goal *facts*)
	   (cons goal (cons temp temp2)))))  ))

(defun prove-all(table goals &optional dowody)
 (let (temp)
  (if (null goals) dowody (and (setq temp (prove table (first goals) dowody))
			  (prove-all table (rest goals) (list (first goals) temp  dowody))))))


(setq +r+
 '((a b c)
   (a e d)
   (a (= 3 4))
   (a (= 1 1) b)
   (b y z w)
   (b (zerop 0)  (> 2 1) )))