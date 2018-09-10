(defun opis(reka stol)
  (format nil "<tr>~{~A~}<td>~{~A~}<td>~A<td>~A<td>~A<td>~A" 
	  (liczby->opis reka)
	  (liczby->opis (reverse stol))
	  (opisz-ocene (ocena (append reka stol)))
	  (ocena-flop reka (subseq (reverse stol) 0 3))
	  (ocena-post-flop reka (subseq (reverse stol) 0 4))
	  (ocena-post-flop reka stol)))

(defmacro policz()
  `(agent-cards ag))

(defun test1(&optional (ile 1000))
  (let ((talia (talia)))
  (with-open-file (f "wynik.html" :direction :output 
                                  :if-does-not-exist :create :if-exists :supersede)
 (princ "<table border=1>" f)
 (dotimes (i ile)
   (let* ((karty (tasuj talia))
	  (reka  (subseq karty 0 2))
	  (stol  (subseq karty 2 7)))  
     (princ (opis reka stol) f)) )
 (princ "</table>" f))))


(defun test2(trojka)
 (with-open-file (f "wynik2.html" :direction :output 
                                  :if-does-not-exist :create :if-exists :supersede)
 (princ "<table border=1>" f)
 (format f "<tr>~{~A~}" (liczby->opis trojka))
 (dolist (p (pary-do-trojki nil trojka))
   (format f "<tr>~{~A~}" (liczby->opis p)))
 (princ "</table>" f)) )