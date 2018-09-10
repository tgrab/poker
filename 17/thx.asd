(asdf:defsystem #:thx
	;:depends-on (#:hunchentoot #:cl-who #:trivial-sockets #:clsql-postgresql)
	:components (
	    (:file "poker.defs")
	    (:file "poker.main"))  )
