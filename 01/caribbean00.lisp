(in-package :poker)
;; ---- Caribbean Stud --------------------------


(defvar *money* 100)
(defvar *bet* 0.2)
(defvar *przyrost* 0.1)
(defvar *record-money* 100)

(defun reset()
 (setq *money* 100 *record-money* 100 *bet* 0.2 *przyrost* 0.1) )

(defun caribbean-valid(lista)
 "Sprawdza czy lista kart krupiera jest wazna"
 (let ((ocena (ocena lista)))
 (cond
   ((< (car ocena) 8) T) ;para lub lepiej
   ((and (= 0 (second ocena))(= 1 (third ocena)))  T) ;as z krolem
   (T nil)) ))

(defun caribbean-wyplata(bet gracz kasyno)
 "Bet to juz potrojona stawka poczatkowa"
 (cond
  ((not (caribbean-valid kasyno))  
               (progn
                  (podsumowanie '0)
                  (* 4 (/ bet 3))))
  ((= 1 (lepsza (ocena gracz) (ocena kasyno)))
              (progn
               (podsumowanie '+)
               (when (< (car (ocena gracz)) 6) 
                        ;(print (liczby->opis gracz)
                         ); wypiszemy trojke
               (* 2 bet)))
  (t   (progn (podsumowanie '-) 0)))  )

(defun nowa-stawka()
 (cond
   ((>= *money* *record-money*)
        (progn (setq *bet* 0.2) 
               (setq *record-money* *money*)))
 
   ((> *bet* 3)  (setq *bet* 1))
   ((< (odleglosc) 5) (incf *bet* (* 2 *przyrost*)))
   (t (incf *bet* *przyrost*))      
  ))

(defun caribbean-pojedynek()
 "Ma rozdac karty"
  (let* ((talia (tasuj (tasuj (tasuj))))
         (gracz (subseq talia 0 5))
         (kasyno (subseq talia 5 10)) 
          ocena-gracza)
 
   (nowa-stawka)
   (decf *money*  *bet*) ;wplata ante
   (setq ocena-gracza (ocena gracz))
   (if (or (< (first ocena-gracza) 7) ;gramy z dwiema parami
             (and (= (first ocena-gracza) 7) 
                  ;(< (second ocena-gracza) 10)
                      ) )      ;lub para 5
      (progn
         (decf *money* (* 2 *bet*)) ;wplacamy stawke
         (incf *money* (caribbean-wyplata (* 3 *bet*) gracz kasyno)))
      (podsumowanie '=))
   ))

(let ((zer 0) (plusow 0) (rownosci 0) (minusow 0) (biezacy 0)
      (ostatni+ 0))
 (defun podsumowanie-reset()
    (setq zer 0 plusow 0 rownosci 0 minusow 0 biezacy 0 ostatni+ 0))
 (defun wynik()
   (-  (+ zer (* 3 plusow)) (+ rownosci (* 3 minusow))) )
 (defun odleglosc() (- biezacy ostatni+ 1))
 (defun podsumowanie(symbol)
  (format t "~A" symbol)
  (incf biezacy)
  (case symbol
    ('? (list zer plusow rownosci minusow))
    ('0 (incf zer))
    ('+ (progn 
            (format t "[~A]~&" (odleglosc))
            (setq ostatni+ biezacy)
            (incf plusow)))
    ('= (incf rownosci)) ('- (incf minusow))
  )))


(defun caribbean-zapisz()
 (reset)(podsumowanie-reset)
 (with-open-file (f "caribbean.csv" :direction :output 
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
  (dotimes (i 1000)
     (caribbean-pojedynek)
     (format f "~A ~A~&" *bet* *money*))
 ))