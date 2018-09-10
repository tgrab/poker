(asdf:defsystem :cl-http
  :version "0.1"
  :serial t
  :components ((:file "packages")
               (:file "http"))

  :depends-on (:cl-moje))
