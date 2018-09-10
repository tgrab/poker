;(defun testuj-rozdanie(ilosc-graczy)
; (let ((t1 (tasuj (talia)))) 
;
; t1))(with-pg-connection (c "poker" "postgres")
;	   (pg-exec c "select * from test"))
;
;(defvar *talia* (talia))

(use-package :postgresql)

(defun dodaj-rozdanie(ilosc-graczy)
 (with-pg-connection (con "poker" "postgres")
  (maphash
     (lambda (k v)
            (pg-exec con
                 (format nil "insert into omaha~A values(~A,~A,~A,~A,~A,~A,~A)" ilosc-graczy
                        (nth 0 k) (nth 1 k) (nth 2 k) (nth 3 k) (nth 0 v) (nth 1 v) (nth 2 v) )      ))
     *h*)    ))


(defun pobierz-rozdanie(wielkosc-stolu a b c d)
 (with-pg-connection (con "poker" "postgres")
 (wynik->odds
  (car
   (pg-result 
      (pg-exec con
         (format nil "select wygrane,remisy,porazki from omaha~A where k1=~A and k2=~A and k3=~A and k4=~A" wielkosc-stolu a b c d ))       
      :tuples)))))

(defmacro pokaz-rozdanie(wielkosc-stolu k1 k2 k3 k4)
   `(destructuring-bind (a b c d) (sort (karty ,k1 ,k2 ,k3 ,k4) #'<)
       (pobierz-rozdanie ,wielkosc-stolu a b c d))    )


(defun prove-all(cele)
 (if (null cele) t (and (prove (first cele)) 
                        (prove-all (rest cele)))))
(let ((+memory+ nil))

 (defun fact(f)
   (find f +memory+))

 (defun prove(cel)
  (cond
    ((consp cel)
       (let ((smb (make-symbol (princ-to-string cel))))
         (cond
            ((fact smb) t)
            (t (if (eval cel) (push smb +memory+)   nil)))))
    ((fact cel) t)
    ((some
        #'(lambda(rl)
             (prove-all (cdr rl)))
        (get cel 'reguly))  t)

     (t nil)))
 (defun show-memory()
   +memory+)
)