; definition of rule based poker agents


(in-package :poker.rulebased)

(defclass rule-based-agent (agent)
  ((memory :initform nil :accessor memory)
   (rules :initform nil :initarg :rules :accessor rules)) )

(defmethod print-object((a rule-based-agent) str)
  (format str "Agent with rules: ~A~%" (rules a))
  (call-next-method))

(defun make-reasoner(rules)
  "make hashtable from list of rules"
  (let ((rlz (make-hash-table)))
    (dolist (r rules rlz)
      (setf (gethash (first r) rlz)
	    (nconc (gethash (first r) rlz) (list (cdr r)))))))

(defun make-rule-based-agent(&optional (rules-list (poker-rules)))
  (make-instance 'rule-based-agent :rules (make-reasoner rules-list)))

(defun rule-based-decision( agent )
  (setf (memory agent) nil)
  (list  (prove agent 'bet)
	 (prove agent 'dont-bet)
	 (prove agent 'check)
	 (prove agent 'fold) ))

(defvar +agent-funcs+) ;list of agents functions

(defun eval-agent-memory(agent goal)
  (when (symbolp goal)
    (let ((el (assoc goal (memory agent))))
      (if el
	  (cdr el)
	  (when (find goal +agent-funcs+)
	    (let ((val (funcall goal agent)))
	      (push (cons goal val)
		    (memory agent))
	      val))))))

; rule: (= 2 (length cards))
; zakladamy, ze reguly beda poprawne!
(defun eval-rule(agent rule)
  (cond
    ;czy jest to funkcja agenta:
    ((eval-agent-memory agent rule))
    ;wywolanie zwyklej funkcji
    ((consp rule)
     (apply (car rule)
	      (mapcar #'(lambda (r)
			  (eval-rule agent r))
		      (cdr rule))))
    ;liczba badz napis
    (t rule)))
     

(defun prove (agent goal)
  (format t "prove: ~A~&" goal)
  (cond
    ;czy mamy go w pamieci?:
    ;czy jest to funkcja agenta:
    ((eval-agent-memory agent goal))
    ;funkcja do policzenia:
    ((consp goal) 
      (eval-rule agent goal))
    ;moze go dowiedziemy z bazy regul:
    ((some
       #'(lambda (rule)
	   (prove-all agent rule))
       (gethash goal (rules agent))))))

(defun prove-all(agent goals)
  (format t "prove-all: ~A~&" goals)
  (if (null goals) t (and (prove agent (first goals))
			  (prove-all agent (rest goals)))))

(defvar +poker-rules+)
(defun poker-rules()
  +poker-rules+)

;;------------------------------------------------------------
;;------------------------------------------------------------
(setf +agent-funcs+
      '(table-cards table-size cards H1 H2 H3 H4 T1 T2 T3 T4 T5
	one-pair two-pairs trips street))

(setq 
 +poker-rules+
 '((top-pair             one-pair (= H1 T1))
   (top-pair-kicker      one-pair (= H2 T1))
   (top-pair             top-pair-kicker)
   (overpair             (= H1 H2) one-pair (< H1 T1))))
      


;------- fultable pre-flop ---------------
;(setq +fulltable-pre-flop
;      )
;------- fulltable post-flop -------------