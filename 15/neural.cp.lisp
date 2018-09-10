(defvar +percepts+)

(setq +percepts+
      '((one-pair)
	(two-pairs)))

(defvar +actions+)

(defun random-weight()
  (/ (random 100) 100.0))


(defun distance(list1 list2)
  (reduce #'+
	  (mapcar (lambda (a b) (expt (- a b) 2)) 
		  list1 list2)))

(defun multiply(list1 list2)
  (reduce #'+ (mapcar #'* list1 list2)))

(defun norm(list)
  (sqrt (multiply list list)))

(defun normalize!(list)
  (let ((n (norm list)))
    (dotimes (i (length list))
      (setf (nth i list) (/ (nth i list) n)))))

(defun normalize(list)
  (let ((n (norm list)))
    (mapcar (lambda(w) (/ w n)) list)))

(defun create-weights-list(&optional (l +percepts+))
    (if (null l)
	nil
	(cons (random-weight) (create-weights-list (cdr l)))))

(defstruct neuron
  nr
  name
  value
  (weights (normalize (create-weights-list))))

(defvar +neurons+ nil)


(defun initialize-neurons(n)
  (setq +neurons+ nil)
  (dotimes (i n)
    (push (make-neuron :nr i) +neurons+))
  (setq +neurons+ (nreverse +neurons+))
  (setq +actions+ 
	(list
	 ;fold
	 (normalize (create-weights-list +neurons+))
	 ;call
	 (normalize (create-weights-list +neurons+))
	 ;bet
	 (normalize (create-weights-list +neurons+)) )))


(defun eval-layer!(input)
  (dolist (n +neurons+)
    (setf (neuron-value n) (multiply input (neuron-weights n)))))

;choose-winner2

(defun teach-neuron(input neuron)
  (setf (neuron-weights neuron)
	(mapcar #'+ 
		(neuron-weights neuron)
		(mapcar (lambda (a b) (* 0.1 (- a b)))
			input (neuron-weights neuron)))))
  
;(defun choose-winner(input 
;		     &optional 
;		     (lista +neurons+) 
;		     (acc (car lista)) 
;		     (max-val (multiply input (neuron-weights acc))))
;  (if (null lista)
;      acc
;      (let ((val (multiply input (neuron-weights (car lista))) ))
;	(if (> val max-val)
;	    (choose-winner input (cdr lista) (car lista) val)
;	    (choose-winner input (cdr lista) acc max-val)))))


(defun choose-winner(&optional 
		     (lista +neurons+) 
		     (acc (car lista)) 
		     (max-val (neuron-value acc)))
  (if (null lista)
      acc
      (let ((val (neuron-value (car lista))) )
	(if (> val max-val)
	    (choose-winner (cdr lista) (car lista) val)
	    (choose-winner (cdr lista) acc max-val)))))
