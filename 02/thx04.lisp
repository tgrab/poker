;(eval-when (:load-toplevel)
 ;  (load "poker"))

(defvar +common-rules+ nil)
(defvar +pre-flop-rules+ nil)
(defvar +flop-rules+ nil)
(defvar +turn-rules+ nil)
(defvar +river-rules+ nil)


;; --------------------  ACTONS  ------------------------------------------

;; predykaty regul:

(defvar *colhand* nil) ;wektor kolorow na rece
(defvar *coltable* nil) ;wektor kolorow na stole
(defvar *vall* nil) ;wektor figur 
(defvar H1 0)
(defvar H2 0)
(defvar T1 0)
(defvar T2 0)
(defvar T3 0)
(defvar T4 0)
(defvar T5 0)



(defun color-outs?()
 (flet ((silna-karta (color) nil ))
  (or (dotimes (i 4) (when (and (= 2 (aref *colhand* i))
                            (= 2 (aref *coltable* i)))
                        (return t)))
     (dotimes (i 4) (when (and (= 1 (aref *colhand* i))
                            (= 3 (aref *coltable* i)))
                         (return (silna-karta i))))
   )))

(defun max-of-colors(&optional (w-kolorow *coltable*))
  (apply #'max (coerce w-kolorow 'list) ));ilosc na stole!


(defun num-of-1-street-outs(&optional (elems *table*))
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (when  (street (wektor-figur (cons i elems))) (incf ilosc)))))

(defun num-of-2-street-outs(&optional (elems *table*))
 (let ((ilosc 0))
  (dotimes (i 13 ilosc)
    (dotimes (j 13 ilosc)
      (when  (street (wektor-figur (cons j (cons i elems)))) (incf ilosc))))))

(defun street-outs?()
 (> (num-of-1-street-outs (append *hand* *table*))
    (num-of-1-street-outs   *table*)))

(defun street-odds()
 (odds (* 4 (num-of-1-street-outs (append *hand* *table*)))))



(defun my-last-action()
 (first (nth *my-pos* *akcje*))  )



(defun ustaw-baze-faktow()
 (setq *facts* nil)
 (let ((stol  (sort (mapcar #'l->figura *table*) #'<) )
       (reka  (sort (mapcar #'l->figura *hand*) #'<) ))
  (setq H1 (nth 0 reka) H2 (nth 1 reka))
  (setq T1 (nth 0 stol) T2 (nth 1 stol) T3 (nth 2 stol))
  (setq T4 (nth 3 stol) T5 (nth 4 stol)))

 (when (plusp (length *table*)) 
     (setq *ocena* (ocena (append *hand* *table*)))
     (setq *vhand* (wektor-figur *hand*))
     (setq *vtable* (wektor-figur *table*))
     (setq *colhand* (wektor-kolorow *hand*))
     (setq *coltable* (wektor-kolorow *table*))
     (setq *vall* (wektor-figur (append *hand* *table*)))))


(defun action()
 (let (wynik)
  (when (null *hand*) (return-from action "repeat"))
  (when (null *my-pos*) (setq *my-pos* *active-player*))
  (ustaw-baze-faktow)
  (setq wynik   (get-action (make-reasoner '+poker-rules+)))
  (logs wynik)
  (when *decision*
     (format t "My decision is ~A " wynik)
     (setq wynik *decision*)
     (setq *decision* nil))
  wynik))




;;-------------------------------------------------------------------
;;------------------------   RULES ----------------------------------
;;-------------------------------------------------------------------


(setq +pre-flop-rules+
 (append +defs-list+
 '((check   big-blind no-bets)
   (bet     (= (sklansky) 1))

   (bet     heads-up sklansky)
   (check   heads-up big-blind)

   (check   fulltable early-pos no-raise    (<= (sklansky) 4))
   (check   fulltable early-pos with-raise  (<= (sklansky) 2))
   (check   fulltable middle-pos no-bets    (<= (sklansky) 7))
   (check   fulltable middle-pos one-bet    (<= (sklansky) 4))
   (check   fulltable middle-pos with-raise (<= (sklansky) 2))
   (check   fulltable late-pos no-bets      (<= (sklansky) 8))
   (check   fulltable late-pos one-bet      (<= (sklansky) 7))
   (check   fulltable late-pos with-raise   (<= (sklansky) 4))
   (check   fulltable small-blind no-bets   (<= (sklansky) 8))
   (check   fulltable small-blind one-bet   (<= (sklansky) 6))
   (check   fulltable big-blind one-bet     (<= (sklansky) 6))


)))


;;------------------------------------------------------------------------------
(setq +common-rules+ 
      (append +defs-list+
	      '((check   no-bets)
		(bet     rainbow trojka no-street-table);moze byc mocniejsza trojka,full lub dlugi street
		(bet     rainbow street);moze byc full
		(bet     (rank>= 'kolor));moze byc mocniejszy kolor lub full
		(bet     no-raise safe-table mocna-para)
		(bet     safe-table (rank>= 'dwie-pary))
                (check   (< (length *table*) 5) (color-outs?) (>  (implied-odds)  (odds 9))) 
		(check   (< (length *table*) 5) (street-outs?) (> (implied-odds) (street-odds))) 

;dziwne reguly z pierwszej wersji:
		(bet     no-bets        (rank>= 'dwie-pary))
		(bet     no-bets        mocna-para)
		(check   one-bet        mocna-para)
                (check                  (rank>= 'trojka))
		(check   one-bet        (losuj 3) second-pair)

		(dont-bet  (rank< 'kolor) (>= (max-of-colors)  4))
		(fold    reraise (rank<= 'dwie-pary) (or (> (max-of-colors) 2) (rank>= 'para 'table)))

		
		)))

;;------------------------------------------------------------------------------
(setq +flop-rules+
      (append +defs-list+
	      '((dont-bet  no-bets   (rank>= 'trojka) (not (last-to-act?)));slowplaying
		(check  mocna-para)
		(check  sklansky1)
		(check  one-bet  second-pair)
		(check  one-bet (losuj 2) safe-table (= H2 T3))
		)))


;;------------------------------------------------------------------------------
(setq +turn-rules+
      (append +defs-list+
	      '((check     safe-table no-raise doszla-blotka i-checked)

		)))



;;------------------------------------------------------------------------------
;(setq +poker-rules+
 ;     (append +common-rules+
	;      '(;(bet       flop (prove (make-reasoner +flop-rules+) 'bet))
		;(dont-bet  flop (prove (make-reasoner +flop-rules+) 'dont-bet))
;		(check     flop (prove (make-reasoner +flop-rules+) 'check))
	;	(check     turn (prove (make-reasoner +turn-rules+) 'check))
		;)))



