;tbnl.image:
;(require :swank)
(defpackage :moje
  (:use :cl)
  (:export :mac :mappend :show-hashtable :dekoduj-czas :permutacje 
	   :aif :awhen :it))


(load (compile-file "moje.lisp" :output-file "build/moje"))

(use-package :moje)
(load (compile-file "poker_ranks.lisp" :output-file "build/ranks"))
(use-package :poker.ranks)
(load (compile-file "poker.lisp" :output-file "build/poker"))
(load "rules.lisp")
(load (compile-file "poker_in.lisp" :output-file "build/in"))
(load (compile-file "poker_out.lisp" :output-file "build/out"))

(load (compile-file "logs.lisp" :output-file "build/logs"))
(load (compile-file "view.lisp" :output-file "build/view"))


(defun start-server(&optional (port 10000))
 (start (make-instance 'hunchentoot:acceptor :port port))
 (setq *access-log-pathname* "access.log"))


(defun load-pages()
  (push 
   (create-folder-dispatcher-and-handler 
    "/images/primalogs/"
    (make-pathname :directory "images"))
   *dispatch-table* )
  (push 
   (create-folder-dispatcher-and-handler 
    "/files/primalogs/"
    (make-pathname :directory "files"))
   *dispatch-table* )
  (dolist (p (cl-fad:list-directory "pages"))
    (let ((name (pathname-name p)))
     (format t "Loading ~A  ...~&" p)
      (load (compile-file p 
			  :output-file 
			  (format nil "build/~A" name)))
 )))
 
(load-pages)
