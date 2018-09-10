(in-package :moje)

;(defun start-server (&key (port 9000))
  ;(let ( (gniazdko (socket:socket-server port)) (polaczenie nil) (wiersz nil) (wynik nil) )
   ; (loop
    ;  (handler-case
     ;  (progn
      ; (setq polaczenie (socket:socket-accept gniazdko))
       ;(format t "*")
       ;(setq wynik 'ok)
       ;(setq wiersz (read polaczenie))
       ;(format t "Otrzymano: ~A~%" wiersz)
       ;(setq wynik (eval wiersz))
	; (format polaczenie "~A" wynik)
       ;(close polaczenie))
      ;(error (e) (progn (close polaczenie)(format t "blad: ~A~%" e)))))
   ;(socket:socket-server-close gniazdko)))

#+clisp
(defun start-server (&key (port 9000))
 (LET ((server (SOCKET:SOCKET-SERVER port)))
  (FORMAT t "~&Waiting for a connection on ~S:~D~%"
          (SOCKET:SOCKET-SERVER-HOST server) (SOCKET:SOCKET-SERVER-PORT server))
  (UNWIND-PROTECT
      ;; infinite loop, terminate with Control+C
      (LOOP (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-ACCEPT server))
              ;(MULTIPLE-VALUE-BIND (local-host local-port) (SOCKET:SOCKET-STREAM-LOCAL socket)
                ;(MULTIPLE-VALUE-BIND (remote-host remote-port) (SOCKET:SOCKET-STREAM-PEER socket)
                  ;(FORMAT T "~&Connection: ~S:~D -- ~S:~D~%" remote-host remote-port local-host local-port)
		;))
              ;; loop is terminated when the remote host closes the connection or on EXT:EXIT
              (WHEN (EQ :eof (SOCKET:SOCKET-STATUS (cons socket :input))) (RETURN))
	          (let ((got (read socket)))
		    (format t "<= ~A~&" got)
                    (format socket "~A" (EVAL got) )   )
                    ;; flush everything left in socket
                    (LOOP :for c = (READ-CHAR-NO-HANG socket nil nil) :while c)
                    ;(TERPRI socket)
			))
    ;; make sure server is closed
    (SOCKET:SOCKET-SERVER-CLOSE server))))


#+cmu
(defun start-server (&key (port 9000) (echo nil))
 (let (p s1 s2 wynik wiersz)
  (setq s1 (ext:create-inet-listener port :stream :reuse-address t))
  (loop
  (setq s2 (ext:accept-tcp-connection s1))
  (setq p  (system:make-fd-stream s2 :input t :output t))
  (handler-case
  (progn
      (setq wiersz (read p))
      (when echo (format t "Got: ~A~%" wiersz))
      (setq wynik (eval wiersz))
      (princ wynik p)
      (force-output p) 
      (ext:close-socket s2))  
   (error (e) (progn (ext:close-socket s2)(format t  "Blad: ~A~%" e)))))))



(defun permutacje (lista)
  (if (null lista)
      '(())
      (mapcan #'(lambda (e)
		  (mapcar #'(lambda (p) (cons e p))
			  (permutacje
			   (remove e lista :count 1 ))
		  )) 
        lista)))


(defun mappend(fn lista)
 (apply #'append (mapcar fn lista)))


(defun show-hashtable(table)
  (maphash
   #'(lambda (k v) (format t "~A => ~A~%" k v))
   table))

(defun dekoduj-czas(czas)
    (multiple-value-bind (s m h d mn y) (decode-universal-time czas)
      (format nil "~A:~A:~A at ~A/~A/~A" h m s d mn y )))
    
;; ---- MOJE MAKRA -----------------------------------

(defmacro mac( makro )
  `(pprint (macroexpand-1 ',makro)))


;; ---- MAKRA ANAFORYCZNE  ---------------------------
(defmacro aif (warunek then-form &optional else-form)
  `(let ((it ,warunek))
     (if it ,then-form ,else-form)))


(defmacro awhen(test &body exprs)
  `(let ((it ,test))
    (when it ,@exprs)))


