;(in-package :poker)


;;---------- BAZA DANYCH -------------------------------------------



(defvar *h*  (make-hash-table :test #'equal) "Baza wynikow pojedynkow")
(defvar *h2* (make-hash-table :test #'equal) "Wyniki koncowe (miejsce . odds)") 

(defun inicjuj-baze() 
                (dolist (reka (podzbiory-4 (talia)))
                    (setf (gethash reka *h*) (list 0 0 0))))

(defun inicjuj-baze-texas() 
                (dolist (reka (podzbiory-2 (talia)))
                    (setf (gethash reka *h*) (list 0 0 0))))



(defun wczytaj-baze(plik)
     (with-open-file (f plik)
      (do
           ( (klucz (read f nil) (read f nil)) 
             (wartosc (read f nil)(read f nil)))
           ( (null klucz) )
           (setf (gethash klucz *h*) wartosc)  )    )
  (aktualizuj-tabele-wynikow))

(defun zapisz-baze(plik)
   (with-open-file (f plik :direction :output :if-exists :overwrite :if-does-not-exist :create)
       (maphash 
         (lambda (k v) (format f "~A ~A~&" k v))
         *h*)))

(defun zwroc-liste-wynikow()
     (let (wynik)
       (maphash 
         (lambda (k v) (push (cons k (wynik->odds v))
                        wynik))
         *h*)
      (sort wynik #'< :key #'cdr)))

(defun aktualizuj-tabele-wynikow()
   (let ((wyniki (zwroc-liste-wynikow)) (indeks 0))
     (dolist (w wyniki)
       (setf (gethash (car w) *h2*) 
             (cons 
                (cons (incf indeks) (cdr w)) 
                (gethash (car w) *h2*))))
    'ok))

;;------------------------------------------------------------------------


(defun podzbiory-2(lista)
"Zwraca liste podzbiorow dwu-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)) wynik)))
  wynik )) 

(defun podzbiory-3(lista)
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)  (nth k lista))
             wynik))))
   wynik ))

(defun podzbiory-4(lista)
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
      (loop for l from (1+ k) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)  (nth k lista)
                   (nth l lista)) wynik)))))
   wynik ))
       
(defun podzbiory-5(lista)
"Zwraca liste podzbiorow piecio-elementowych z <lista>"
 (let ((ilosc (length lista)) wynik)
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
      (loop for l from (1+ k) to (1- ilosc) do
       (loop for m from (1+ l) to (1- ilosc) do
       (push (list (nth i lista) (nth j lista)  (nth k lista)
                   (nth l lista) (nth m lista) ) wynik))))))
   wynik ))


;----------------------------------------------------------------

(defun wyjmij(skad &rest listy)
 "Usuwa z listy <skad> elementy z list <listy>"
  (dolist (l listy)
    (dolist (el l)
      (setf skad (remove el skad)))) 
  skad)

(defun opisz-wynik(wektor)
 (let* ((wynik (coerce wektor 'list))(suma (apply #'+ wynik)))
   (mapcar (lambda(x) (* 1.0 (/ x suma)))  wynik)))

(defun opisz-dojscia(pozycja ilosc-kart dojscia)
 "pozycja to wartosc oceny poczatkowej"
  (labels ( (niezerowy (x)
                   (if (zerop x) "" x))
            (odds (x) 
                (if (zerop x) "" (format nil "~3,2F" 
                                          (/ (- ilosc-kart x) x) )))) 
  (let  ( (ile 0) (suma 0)
          (suma-dojsc (make-array 9 :initial-element 0)))
     (dotimes (uklad pozycja)
        (setq ile (aref dojscia uklad))
        (incf suma ile)
        (when (plusp ile)(setf (aref suma-dojsc uklad) suma)))
    (format t "<table border=1 width=100%>~&")        
    (format t "<tr>")
    (dotimes (uklad pozycja)
               (format t "<td>~A" (nth uklad opisy-ocen)))
    (format t "<tr>")
    (dotimes (uklad pozycja) 
         (format t "<td><b>~A</b> ~A <b>~A</b>" (niezerowy (aref dojscia uklad))
                              (niezerowy (aref suma-dojsc uklad))
                              (odds (aref suma-dojsc uklad))))
    (format t "</table>")  )))


(let ((talia (talia)))
  (defun test-main(&optional (ile 1))
    (let (r1 r2 o1 o2)
    (flet ((pokaz (liczba) (format nil "~A~A" (nth (l->figura liczba) figury)
                                              (nth (l->kolor liczba) kolory)) ))
    (dotimes (i ile)
       (tasuj talia)
       (setq r1 (subseq talia 0 7))
       (setq r2 (subseq talia 7 14))
       (setq o1 (ocena r1))
       (setq o2 (ocena r2))
       (format t "~A ~A | ~A ~A | ~A~&" (mapcar #'pokaz r1) (opisz-ocene o1) 
	                                (mapcar #'pokaz r2) (opisz-ocene o2)
					(zwyciezca (list o1 o2)))
      )  ))))

(defun opisz-rozdanie(ilosc-graczy rece stol oceny zwyciezcy)
  (princ (liczby->opis stol))
  (dotimes (r ilosc-graczy)
      (when (find r zwyciezcy) (princ "Winner:"))
      (princ (liczby->opis (aref rece r)))
      (princ (opisz-ocene (aref oceny r))))
)

(defmacro zapisz-html(arg &optional (nazwa-pliku "wynik.html"))
 `(with-open-file (f ,nazwa-pliku :direction :output :if-exists :overwrite :if-does-not-exist :create)
   (let ((*standard-output* f))
     ,arg
    )))


(let ((talia (talia)))
  (defun pojedynek-omaha2(ilosc)
    (let ((rece (make-array ilosc)) (oceny (make-array ilosc)) 
          (wynik (make-array ilosc :initial-element 2))
           stol (jeden? 0) zwyciezcy )
      (tasuj talia)
      (dotimes (r ilosc) 
          (setf (aref rece r) 
             (subseq talia (* 4 r) (* 4 (1+ r)))  ))
      (setq stol (subseq talia (* 4 ilosc) (+ 5 (* 4 ilosc))))
      (dotimes (r ilosc) 
               (setf (aref oceny r) 
                     (omaha-ocena (aref rece r) stol) ))
      (setq zwyciezcy (zwyciezca (coerce oceny 'list)))
      (when (> (length zwyciezcy) 1) (setq jeden? 1))
    (opisz-rozdanie ilosc rece stol oceny zwyciezcy)))  );let


(let ((talia (talia)))
  (defun pojedynek-omaha(ilosc)
    (let ((rece (make-array ilosc)) (oceny (make-array ilosc)) 
          (wynik (make-array ilosc :initial-element 2))
           stol (jeden? 0) zwyciezcy )
      (tasuj talia)
      (dotimes (r ilosc) 
          (setf (aref rece r) 
             (subseq talia (* 4 r) (* 4 (1+ r)))  ))
      (setq stol (subseq talia (* 4 ilosc) (+ 5 (* 4 ilosc))))
      (dotimes (r ilosc) 
               (setf (aref oceny r) 
                     (omaha-ocena (aref rece r) stol) ))
      (setq zwyciezcy (zwyciezca (coerce oceny 'list)))
      (when (> (length zwyciezcy) 1) (setq jeden? 1))
      (dolist (i zwyciezcy)
          (setf (aref wynik i) jeden?))
      (dotimes (r ilosc)
        (incf (nth (aref wynik r)  
                   (gethash (sort (copy-list (aref rece r)) #'<) *h* )) ))
     (list rece stol oceny zwyciezcy)))  );let


(let ((talia (talia)))
  (defun pojedynek-texas (&optional (ilosc 10))
    (let ((rece (make-array ilosc)) (oceny (make-array ilosc)) 
          (wynik (make-array ilosc :initial-element 2))
           stol (jeden? 0) zwyciezcy )
      (tasuj talia)
      (dotimes (r ilosc) 
          (setf (aref rece r) 
             (subseq talia (* 2 r) (* 2 (1+ r)))  ))
      (setq stol (subseq talia (* 2 ilosc) (+ 5 (* 2 ilosc))))
      (dotimes (r ilosc) 
               (setf (aref oceny r) 
                     (ocena (append (aref rece r) stol)) ))
      (setq zwyciezcy (zwyciezca (coerce oceny 'list)))
      (when (> (length zwyciezcy) 1) (setq jeden? 1))
      (dolist (i zwyciezcy)
          (setf (aref wynik i) jeden?))
      (dotimes (r ilosc)
        (incf (nth (aref wynik r)  
                   (gethash (sort (copy-list (aref rece r)) #'<) *h* )) ))
     (list rece stol oceny zwyciezcy)))  );let

(defun temp1()
 (with-open-file (f "wynik.html" :direction :output
			 :if-does-not-exist :create)
  (dolist (e (zwroc-liste-wynikow))
       (format f "~A ~A~&" (liczby->opis (car e)) (cdr e))
      )
  ))



(defun pojedynek2(reka &key (ilosc-graczy 1) (ilosc-powtorzen 1) (stol nil))
"Reka to cztery karty na rece. Funkcja ocenia konkretny uklad"
 (let ( (reszta (wyjmij (talia) reka stol))
        (rece  (make-array (1+ ilosc-graczy))) 
        (oceny (make-array (1+ ilosc-graczy))) 
        (wygrana 0)(remis 0)(porazka 0) (zwyciezcy nil) (zmieniaj-stol t))
  (when stol (setq zmieniaj-stol nil))
  (setf (aref rece 0) reka)
  (dotimes (i ilosc-powtorzen)
    (tasuj reszta)  
    (dotimes (r ilosc-graczy)
         (setf (aref rece (1+ r))
         (subseq reszta (* 4 r) (* 4 (1+ r)))  ))                
     (when zmieniaj-stol
        (setq stol
             (subseq reszta (* 4 ilosc-graczy) (+ 5 (* 4 ilosc-graczy)))))
    (dotimes (r (1+ ilosc-graczy))
           (setf (aref oceny r) (omaha-ocena (aref rece r) stol) ))
    (setq zwyciezcy (zwyciezca (coerce oceny 'list)))
    ;(format t "~A ~A ~A~&" rece stol zwyciezcy)
    (if (find 0 zwyciezcy)
        (if (> (length zwyciezcy) 1) 
                              (incf remis) 
                              (incf wygrana))
        (incf porazka))  )
  (list wygrana remis porazka 
        (/ porazka (+ wygrana (* 0.5 remis))))))







  
(defun test3(reka &optional (poziom 990))
"Zwraca liste podzbiorow piecio-elementowych z <lista>"
 (let* ((stol (wyjmij (talia) reka))(ilosc (length stol))
        (wynik (make-array 9 :initial-element 0)))
  (loop for i from 0 to ilosc do
    (loop for j from (1+ i) to (1- ilosc) do
     (loop for k from (1+ j) to (1- ilosc) do
      (when (> (random 1000)  poziom)
      (loop for l from (1+ k) to (1- ilosc) do

       (loop for m from (1+ l) to (1- ilosc) do
         ; (print (list (nth i stol) (nth j stol)  (nth k stol) (nth l stol)(nth m stol))))))))
                  ; (nth l stol) (nth m stol) ) 
       (incf (aref wynik (car (omaha-ocena reka (list (nth i stol) (nth j stol)  (nth k stol)
                   (nth l stol) (nth m stol) ) ))))))))))
  (format t "~A~&"  (opisz-wynik wynik))
  wynik ))
  

  
(defun test1(uklad liczby)
"(test1 'poker '(1 2 3 4 5 6 7))"
  (dolist (reka (podzbiory-5 liczby)) 
     (when  (eq uklad (car (ocena reka)))  
           (format t "reka=[~A] , ocena= ~A ~&" (mapcar #'l->opis reka)  (cdr (ocena reka))))
    ))
    
;mamy piatke liczb dokladamy do nich z tali po jednej i opisujemy mozliwe szostki
(defun test2(liczby)
 (let (o1 pozostale o2)
   (setq o1 (ocena liczby))
   (format t "~A~&" (opisz-ocene o1))
   (setq pozostale (wyjmij (talia) liczby))
   (dolist (l pozostale)
      (setq o2 (ocena `(,@liczby ,l)))
     (format t "~A, uklad: ~A , lepszy: ~A~&" (l->opis l)  (opisz-ocene o2) (lepsza o1 o2) ) 
     )))



(defun test3-oceny()
 (let (zestaw o temp)
  (dotimes (i 5)
   (setq zestaw (mapcar (lambda(l) (1+ (random 52))) '(1 1 1 1 1 1 1)))
   (setq o (ocena zestaw))
   (push zestaw temp)
   (format t "~A [~A]= ~A ~&" (mapcar #'l->opis zestaw) (opisz-ocene o) o))
  (najlepszy temp #'ocena )))


(defun ocena-carribean(reka stol)
  (let ((reszta (wyjmij (talia) reka (cons stol nil))) (wygrane 0)
        ( remisy 0) (porazki 0) ocena-reki ocena-stolu)
     (setq ocena-reki (ocena reka))
     (dolist (r (podzbiory-4 reszta))
       (setq ocena-stolu (ocena (cons stol r)))
       (case (lepsza ocena-reki ocena-stolu)
          (1 (incf wygrane))
          (0 (incf remisy))
          (-1 (incf porazki))    ) )
  (list wygrane remisy porazki
        (if (zerop wygrane) 163185 (* 1.0 (/ porazki wygrane)) ))))



(defun odds->stawka(odds)
  (format nil "~3,2F%" (* 100.0 (/ (1+ odds)))))
