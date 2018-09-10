(in-package :poker)
(use-package :ltk)


(let (f-main f-talia f-przyciski b-reset b2 l-opis lista stol talia)

  (defun app()
   (labels ((opis () 
               (setf (text l-opis) 
                         (format nil "Reka: ~A  Stol:~A  Ocena:~A" 
                             (mapcar #'l->opis lista)
                             (mapcar #'l->opis stol)
                             (if (> (length stol) 2)
                               (opisz-ocene (omaha-ocena lista stol))
                               " brak "))))
            (karta(numer)
                #'(lambda ()
                   (when (= 5 (length stol)) (reset))
                   (if (> (length lista) 3)
                     (progn
                       (push numer stol)
                       (setq stol (sort stol #'<))
                       (opis)                        
                       (when (= 3 (length stol))
                                (omaha-flop lista stol)(force-output))
                       (when (= 4 (length stol))
                                (omaha-river lista stol)(force-output)))
                     (progn
                        (push numer lista)
                        (setq lista (sort lista #'<))
                        (opis)
                        (when (= 4 (length lista))
                                (omaha-pre-flop lista)(force-output))
                      ) )))
                   
            (reset()
                (format t "~&------------ Reset -----------~&")
                (force-output)
                (setq lista nil stol nil)(opis)))

    (with-ltk
       (setq f-main (make-instance 'frame )
             f-talia (make-instance 'frame :master f-main)
             f-przyciski (make-instance 'frame :master f-main)
             b-reset (make-instance 'button :master f-przyciski :text "Reset"
                                                 :command #'reset)
             b2 (make-instance 'button :master f-przyciski :text "Przycisk" 
                                                 )
             l-opis (make-instance 'label :master f-przyciski) )
   (dotimes (i 52) 
        (push (make-instance 'button
                             :text (l->opis (1+ i))
                             :master f-talia
                             :command 
                               (karta (1+ i)))
               talia))

       (pack f-main)(pack f-talia :side :top)(pack f-przyciski :side :bottom)
  (dotimes (i 4)
      (dotimes (j 13) 
        (grid (nth (- 51 (* 13 i) j) talia) i j )))

       (pack b-reset)(pack l-opis)
       (opis)    )))  )


(let (f-main f-talia f-przyciski b-reset b2 l-opis lista stol talia)

  (defun carribean()
   (labels ((opis () 
               (setf (text l-opis) 
                         (format nil "Reka: ~A  Ocena:~A" 
                             (mapcar #'l->opis lista)
                             (if (> (length lista) 4)
                               (opisz-ocene (ocena (append lista stol)))
                               " brak "))))
            (karta(numer)
                #'(lambda ()
                   (when (= 1 (length stol)) (reset))
                   (if (> (length lista) 4)
                     (progn
                       (push numer stol)
                       (setf (text l-opis) "Licze....")
                       (setf (text l-opis)
                                (format nil "~A" (ocena-carribean lista (car stol)))))
                     (progn
                        (push numer lista)(opis)

                      ) )))
                   
            (reset()
                (format t "~&------------ Reset -----------~&")
                (force-output)
                (setq lista nil stol nil)(opis)))

    (with-ltk
       (setq f-main (make-instance 'frame )
             f-talia (make-instance 'frame :master f-main)
             f-przyciski (make-instance 'frame :master f-main)
             b-reset (make-instance 'button :master f-przyciski :text "Reset"
                                                 :command #'reset)
             b2 (make-instance 'button :master f-przyciski :text "Przycisk" 
                                                 )
             l-opis (make-instance 'label :master f-przyciski) )
   (dotimes (i 52) 
        (push (make-instance 'button
                             :text (l->opis (1+ i))
                             :master f-talia
                             :command 
                               (karta (1+ i)))
               talia))

       (pack f-main)(pack f-talia :side :top)(pack f-przyciski :side :bottom)
  (dotimes (i 4)
      (dotimes (j 13) 
        (grid (nth (- 51 (* 13 i) j) talia) i j )))

       (pack b-reset)(pack l-opis)
       (opis)    )))  )



;;-------------------------------------------------------------------
;; KOMUNIKACJA
;; zaladowac trivial-sockets
#|
(defun sluchaj (port)
 (let ((got nil))
 (trivial-sockets:with-server (s (:port port :reuse-address t))
   (loop
     (handler-case
      (with-open-stream (c (trivial-sockets:accept-connection s))
         (format t "Polaczenie ~A" c) 
         (loop  (setq got (read c))
	        (format t "Got: ~A~&" got)
	        (format c "~A" (eval got)) 
                (force-output c)))
     (error (e) (print "Blad polaczenia")))))))
|#