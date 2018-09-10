(defun srednia-ilosc-podbic()
  ;przed flopem
  (let* ((ifos (get-game-infos))
	 (ile (length ifos))
	 (s (apply #'+ (mapcar #'game-info-pre-flop-bets ifos)))  )


    (list ile s (/ s ile 1.0) ) ))



;---------------------------------------------------------------------
(defun pojedynek(cards1 cards2 stol)
 (let* ((reszta (subseq 
		(tasuj 
		 (set-difference (talia) (append cards1 cards2 stol)))
		0  (- 5 (length stol))))

	(o1 (omaha-ocena cards1 (append stol reszta)))
	(o2 (omaha-ocena cards2 (append stol reszta))))
   ;reszta to karty ktore zostana dodane do stolu
   ;(princ  (list o1 o2 reszta))

   (poker.ranks::lepsza o1 o2)) )


(defun pojedynek2(cards1 cards2 stol)
 (let ((reszta (set-difference (talia) (append cards1 cards2 stol)))
       (n1 0)
       (n2 0))
   (dotimes (i 100)
     (let* ((stol-reszta (subseq 
			 (tasuj reszta)
			 0  (- 5 (length stol))))
	   (o1 (omaha-ocena cards1 (append stol stol-reszta)))
	   (o2 (omaha-ocena cards2 (append stol stol-reszta))))
       (when (= (poker.ranks::lepsza o1 o2) -1) (incf n1))
       (when (= (poker.ranks::lepsza o1 o2) 1) (incf n2))))
   (format t "~A ~A ~A" n1 n2 (/ n1 n2))))






(defmacro with-all-hands(&body code)
`(loop for c1 from 1 to 49 do
  (loop for c2 from (1+ c1) to 50 do
   (loop for c3 from (1+ c2) to 51 do
    (loop for c4 from (1+ c3) to 52 do
	  ,@code    )))))


(defvar *hands* (make-hash-table :test #'equal))

;(with-all-hands  (setf (gethash (list c1 c2 c3 c4) *hands* ) (list 0 0 0) ))

(defun zapisz-baze()
  (with-open-file (f "baza.txt" 
		     :direction :output 
		     :if-does-not-exist :create)
    (maphash 
     #'(lambda (k v) (format f "~A ~A~&" k v))
     *hands*)))

(defun baza->list1()
  (let ((wynik nil))
    (maphash 
     #'(lambda (k v) (push (list k (odds-val v)) wynik))
     *hands*)
  wynik))

;(progn (setq *l1* (baza->list1)) 'ok)
;(progn (setq *ls* (sort (copy-list *l1*) #'< :key #'second)) 'ok)


(defun wczytaj-baze()
  (with-open-file (f "baza.txt" 
		     :direction :input)
    (do ( (hand (read f nil 'end) (read f nil 'end)) )
	( (eq hand 'end) )
      (setf (gethash hand *hands*) (read f)))))

(defun odds-val(wynik)
  "odds porazki do zwyciestw, im mniej tym lepiej"
  (if (zerop (first wynik))
      10000
      (/ (third wynik) (first wynik) 1.0)))


(defun hand-val(c1 c2 c3 c4)
  (odds-val (gethash (sort (list c1 c2 c3 c4) #'<) *hands*)))

;-----------------------------------------------------------
;(defvar *top30%* nil)

(defun wczytaj-top30()
  (let ((w nil))
  (with-open-file (f "top80000.txt" 
		     :direction :input)
    (do ( (hand (read f nil 'end) (read f nil 'end)) )
	( (eq hand 'end) )
      (push hand w)))
  w))

;(setq *top30%* (wczytaj-top30))


(defun pojedynek-post-flop(&optional (cards1 (cards +game+)) (stol (table-cards +game+)))
 (let ((reszta (set-difference (talia) (append cards1 stol)))      
       (n1 0)
       (n2 0))
   (dotimes (i 500)
     (let* ((reszta (tasuj reszta))
	    (cards2 (subseq reszta 0 4))
	    (stol-reszta (subseq   reszta 4  (- 9 (length stol)))))
       (when (<= (odds-val (gethash (sort (copy-list cards2) #'<) *hands*)) 9.36 )
	 (let ((o1 (omaha-ocena cards1 (append stol stol-reszta)))
	       (o2 (omaha-ocena cards2 (append stol stol-reszta))))
       ;(format t "~a ~a ~a ~a" cards2 stol-reszta o1 o2)
       (when (= (poker.ranks::lepsza o1 o2) -1) (incf n1))
       (when (= (poker.ranks::lepsza o1 o2) 1) (incf n2))))))
   (if (plusp n2) (/ n1 n2 1.0) 1000)))