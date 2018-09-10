(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :dekoduj-czas :permutacje 
   :aif :awhen :it))

(defpackage :poker.ranks
  (:use :cl :moje)
  (:export :ocena :omaha-ocena :zwyciezca :wektor-figur :wektor-kolorow :podzbiory-2 :tasuj :talia))

(use-package :moje)
(use-package :poker.ranks)
(use-package :hunchentoot) ;bo moglo nie byc wbudowane
(use-package :webapps)

(defvar *ctxt* "/primalogs")
(defvar *root* "/opt/projects/webapps/primalogs")
(install 'primalogs  *root*  *ctxt*) 
