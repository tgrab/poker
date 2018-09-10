(let ((fd  nil)
      (bufor (make-string 100))
      (dane nil)
      (wynik nil)
      (dlugosc 0)
      (zdalny  nil))

(defun sluchaj-udp(&optional (zdalny-komp "127.0.0.1") (port-lokalny 9000))
  (setq zdalny  (host-entry-addr
		 (lookup-host-entry zdalny-komp)))
  (setq fd (ext:create-inet-listener port-lokalny :datagram)))


(defun pobieraj-udp()
    (loop
     (setq dlugosc (ext:inet-recvfrom fd bufor 100))
     (setq dane (read-from-string (subseq bufor 0 dlugosc)))
     (dbg 'dbg-servers "Got: ~A~%" dane)
     (setq wynik (eval dane))
     (when (and (consp dane) (eq (car dane) 'action))
	 (wyslij-udp wynik 10000))  ))

(defun wyslij-udp(co port)
     (ext:inet-sendto fd co (length co)
		      zdalny
		      port))              );koniec let