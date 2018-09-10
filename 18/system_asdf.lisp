#+(not asdf)(load (compile-file "..\\..\\systems\\asdf"))
#+clisp(pushnew "..\\..\\systems\\cl-store\\" asdf:*central-registry*)
#+clisp(pushnew "..\\..\\systems\\slime\\" asdf:*central-registry*)
#+clisp(pushnew "..\\..\\systems\\plain-odbc\\trunk\\" asdf:*central-registry*)
#+clisp(pushnew "..\\..\\systems\\cffi\\" asdf:*central-registry*)
#+clisp(pushnew "..\\..\\systems\\s-xml\\" asdf:*central-registry*)
#+clisp(pushnew "..\\..\\systems\\cl-who\\" asdf:*central-registry*)

