;kopiuj ten plik do katalogu z systemem
#+(not asdf)(load (compile-file "../../systems/asdf"))
(pushnew "../../systems/cl-store/" asdf:*central-registry*)
(pushnew "../../systems/slime/" asdf:*central-registry*)
(pushnew "../../systems/plain-odbc/trunk/" asdf:*central-registry*)
(pushnew "../../systems/cffi/" asdf:*central-registry*)
(pushnew "../../systems/s-xml/" asdf:*central-registry*)
(pushnew "../../systems/cl-who/" asdf:*central-registry*)

