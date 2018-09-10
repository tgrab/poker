(load "poker")
(defvar *h*  (make-hash-table :test #'equal) "Baza wynikow pojedynkow")

;; ------------------------------------------------------
;; -------------------redefinicje -----------------------
;; ------------------------------------------------------

(setq +poker-rules+
 '((check    pre-flop  (reason-check-with +pre-flop-rules+))
   (bet      pre-flop  (reason-bet-with +pre-flop-rules+))

   (check    post-flop (reason-check-with +post-flop-rules+))
   (dont-bet post-flop (reason-dont-bet-with +post-flop-rules+))
   (fold     post-flop (reason-fold-with +post-flop-rules+))
   (bet      post-flop (reason-bet-with +post-flop-rules+))
   (potbet1  post-flop (prove (make-reasoner '+post-flop-rules+) 'potbet1))
   (potbet2  post-flop (prove (make-reasoner '+post-flop-rules+) 'potbet2))
   (check    post-flop no-bets)
))


(defun get-action(table)
  (or
     (and (prove table 'potbet1) (if (prove table 'dont-bet) "check" "potbet 1")) 
     (and (prove table 'potbet2) (if (prove table 'dont-bet) "check" "potbet 2")) 
     (and (prove table 'bet) (if (prove table 'dont-bet) "check" "bet")) 
     (and (prove table 'dont-bet) "check")
     (and (prove table 'check) (if (prove table 'fold) "fold" "check"))
     "fold"))

(setq +poker-rules+
 '((check    pre-flop  (reason-check-with +pre-flop-rules+))
   (bet      pre-flop  (reason-bet-with +pre-flop-rules+))

   (check    post-flop (reason-check-with +post-flop-rules+))
   (dont-bet post-flop (reason-dont-bet-with +post-flop-rules+))
   (fold     post-flop (reason-fold-with +post-flop-rules+))
   (bet      post-flop (reason-bet-with +post-flop-rules+))
   (potbet1  post-flop (prove (make-reasoner '+post-flop-rules+) 'potbet1))
   (potbet2  post-flop (prove (make-reasoner '+post-flop-rules+) 'potbet2))
   (check    post-flop no-bets)
))

;;--------------------------------------------------------
(defun wczytaj-baze(plik)
 ;inicjacja bazy:
    (dolist (reka (podzbiory-2 (talia)))
          (setf (gethash reka *h*) (list 0 0 0)))
     (with-open-file (f plik)
      (do
           ( (klucz (read f nil) (read f nil)) 
             (wartosc (read f nil)(read f nil)))
           ( (null klucz) )
           (setf (gethash klucz *h*) wartosc)  )    ))


(defun pre-flop-eval(&optional (h *hand*))
  (when (zerop (hash-table-count *h*)) (wczytaj-baze "logs/omaha10.dat"))
  (wynik->odds (gethash  (sort (copy-list h) #'<) *h*)))


(defun omaha-zestawy(&optional (reka *hand*) (stol *table*))
           (let (wynik)
             (dolist (r (podzbiory-2 reka))
               (dolist (s (podzbiory-3 stol))
                   (push (append r s) wynik)))
          wynik))

(defun omaha-ocena(reka stol)
 (labels ((najlepszy(lista f-oceny &optional (best '(10)))
           (if (null lista) 
            best
             (let ((nowy (ocena (car lista))))
              (if (= 1 (lepsza nowy best))
                (najlepszy (cdr lista) f-oceny nowy)
                (najlepszy (cdr lista) f-oceny best))))))


 (najlepszy (omaha-zestawy reka stol) #'ocena)))





;; --------------------  ACTONS  ------------------------------------------

;; predykaty regul:

(defvar *colhand* nil) ;wektor kolorow na rece
(defvar *coltable* nil) ;wektor kolorow na stole
(defvar *vall* nil) ;wektor figur 
(defvar H1 0)
(defvar H2 0)
(defvar H3 0)
(defvar H4 0)
(defvar T1 0)
(defvar T2 0)
(defvar T3 0)
(defvar T4 0)
(defvar T5 0)

;do liczenia dojsc trzeba rozpatrywac uklady 2 kart z reki i 2 ze stolu
(defun generuj-dojscia(&optional (reka *hand*) (stol *table*))
     (let (wynik)
          (dolist (r (podzbiory-2 reka))
             (dolist (s (podzbiory-2 stol))
                   (push (append r s) wynik)))
          wynik))


;sluzy do sprawdzenia czy moze byc u kogos kolor:
(defun max-of-colors(&optional (w-kolorow *coltable*))
  (apply #'max (coerce w-kolorow 'list) ));ilosc na stole!

;zwraca kolor oraz ilosc dojsc:
(defun color-outs()
  (dotimes (i 4) (when (and (>= (aref *colhand* i) 2)
                            (= 2 (aref *coltable* i)))
                        (return (cons i (- 11 (aref *colhand*  i))))))  )

;czy moze byc street u kogos?
(defun street-table?(&optional (stol *table*))
  (let ((figury-stolu  (mapcar #'l->figura stol)))
 (some
  #'(lambda (para)
      (street (wektor-figur (mapcar #'1+ (append para figury-stolu)))))
  (podzbiory-2  
        (set-difference 
             '(0 1 2 3 4 5 6 7 8 9 10 11 12)
             figury-stolu)) )))

;czy moga sie pojawic karty dajace mi streeta
(defun num-of-1-street-outs(&optional (reka *hand*) (stol *table*))
 (let ((ilosc 0))
  (dotimes (i 13  ilosc)
    (when (some #'(lambda(uklad)
		    (street (wektor-figur (cons i uklad))))
           (generuj-dojscia reka stol))
      (incf ilosc)) )))

(defun num-of-nut-street-outs(&optional (reka *hand*) (stol *table*))
 (let ((ilosc 0))
 (dotimes (nowa-karta 13 ilosc)
 ;musimy sprawdzic czy jesli nowa karta da nam strita to czy jest to nuts
   (let ((nowy-stol (cons (1+ nowa-karta) stol)))

   (when (some    #'(lambda(dojscie)
		      (and (street (wektor-figur dojscie))
			   (street-nuts? reka nowy-stol)))
		  (omaha-zestawy reka nowy-stol))
     (incf ilosc)  )))  ))

;czy oplaca sie grac do strita?
(defun street-chase?()
  (let ((ilosc (num-of-nut-street-outs)))    (and (plusp ilosc) 
       (if (= 1 (max-of-colors)) (> (implied-odds) (odds (* 4 ilosc)))
	                         (> (implied-odds) (odds (* 3 ilosc))))  )))

;j.w. do koloru?
(defun color-chase?()
  (let* ((dojscia  (color-outs)) (col (car dojscia)))
    (and dojscia
         (or (member (+ 1 (* 13 col)) *hand*) ;czy mamy asa w tym kolorze?
	     (and (member (+ 1 (* 13 col)) *table*)  ;czy mam krola na reku
		  (member (+ 2 (* 13 col)) *hand*))
	     (and (member (+ 1 (* 13 col)) *table*)  ;czy mam dame na reku
		  (member (+ 2 (* 13 col)) *table*)
		  (member (+ 3 (* 13 col)) *hand*)) )
         (> (implied-odds) (odds (cdr dojscia))))  ))

;;testy czy mam nutsa?:
(defun color-nuts?()
 (let (col)
  (when (some 
         #'(lambda (uklad) 
	   (and (apply #'= (mapcar #'l->kolor uklad))
		(setq col (l->kolor (first uklad)))))
	   (omaha-zestawy *hand* *table*))
  (or(member (+ 1 (* 13 col)) *hand*)   ;czy mam asa na reku
     (and (member (+ 1 (* 13 col)) *table*)  ;czy mam krola na reku
	  (member (+ 2 (* 13 col)) *hand*))
     (and (member (+ 1 (* 13 col)) *table*)  ;czy mam dame na reku
	  (member (+ 2 (* 13 col)) *table*)
	  (member (+ 3 (* 13 col)) *hand*)) ))  ))


(defun street-nuts?(&optional (reka *hand*) (stol *table*))
  (let ((figury-stolu  (mapcar #'l->figura stol)))
 (<=
 (second (omaha-ocena reka stol));zakladamy,ze MAM strita
 (apply #'min
 (mapcar
  #'(lambda (para)
      (let ((str (street (wektor-figur (mapcar #'1+ (append para figury-stolu))))))
	(if str str 100)))
  (podzbiory-2  
        (set-difference 
             '(0 1 2 3 4 5 6 7 8 9 10 11 12)
             figury-stolu)) )))))

(defun full-nuts?()
 (or (= H1 H2 T1) (= H2 H3 T1) (= H3 H4 T1) 
     (and (= H1 T1 T2) (= H2 T3) )
     (and (= H1 T1 T2) (= H3 T3) )
     (and (= H1 T1 T2) (= H4 T3) )

     (and (= H2 T1 T2) (= H1 T3) )
     (and (= H2 T1 T2) (= H3 T3) )
     (and (= H2 T1 T2) (= H4 T3) )

     (and (= H3 T1 T2) (= H2 T3) )
     (and (= H3 T1 T2) (= H1 T3) )
     (and (= H3 T1 T2) (= H4 T3) )

     (and (= H4 T1 T2) (= H2 T3) )
     (and (= H4 T1 T2) (= H3 T3) )
     (and (= H4 T1 T2) (= H1 T3) ) ))
 

(defun ustaw-baze-faktow()
 (setq *facts* nil)
 (setq *chosen-rule* nil)
 (let ((stol  (sort (mapcar #'l->figura *table*) #'<) )
       (reka  (sort (mapcar #'l->figura *hand*) #'<) ))
  (setq H1 (nth 0 reka) H2 (nth 1 reka))
  (setq H3 (nth 2 reka) H4 (nth 3 reka))
  (setq T1 (nth 0 stol) T2 (nth 1 stol) T3 (nth 2 stol))
  (setq T4 (nth 3 stol) T5 (nth 4 stol)))

 (when (plusp (length *table*)) 
     (setq *ocena* (omaha-ocena  *hand* *table*))
     (setq *vhand* (wektor-figur *hand*))
     (setq *vtable* (wektor-figur *table*))
     (setq *colhand* (wektor-kolorow *hand*))
     (setq *coltable* (wektor-kolorow *table*))
     (setq *vall* (wektor-figur (append *hand* *table*)))))


(defun testuj(reka stol)
 (setq *hand* reka *table* stol)
 (ustaw-baze-faktow)
 (format t "~A max-of-colors:~A color-outs:~A~%" (opisz-ocene(omaha-ocena reka stol)) (max-of-colors) (color-outs))
 (format t "street-table:~A num-of-1-street-outs:~A street-chase:~A~%" (street-table?) (num-of-1-street-outs) (street-chase?))
 (format t "color-chase:~A color-nuts:~A street-nuts:~A~%" (color-chase?) (color-nuts?) (street-nuts?))
 (format t "full-nuts:~A~%" (full-nuts?))       )



