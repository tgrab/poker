(load "system_asdf.lisp")
(load "system_packages.lisp")

(load (compile-file "moje" :output-file "../moje"))
(load (compile-file "dao" :output-file "../dao" ))
(load (compile-file "poker_ranks" :output-file "../poker_ranks" ))
(load (compile-file "poker" :output-file "../poker"))
(load (compile-file "poker_logic" :output-file "../poker_logic" ))
(load (compile-file "poker_holdem":output-file "../poker_holdem"))
(load (compile-file "poker_omaha" :output-file "../poker_omaha"))

(load (compile-file "poker_logs" :output-file "../poker_logs"))


(load (compile-file "poker_percepts_rules" 
			:output-file "../poker_percepts_rules"))
(load (compile-file "poker_percepts" :output-file "../poker_percepts"))
   
(load (compile-file "http" :output-file "../http" ))

(load     (compile-file "web/minimal.lisp" :output-file "../minimal"))
(load     (compile-file "web/view01" :output-file "../view01"))

(load "rules.lisp")
;(load     (compile-file "logs_wh" :output-file "../logs_wh"))
; #+cffi(compile-file "logs_prima")  ;wczytano juz prime