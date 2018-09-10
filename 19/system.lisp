(load "system_asdf")
(load "system_packages")

(load "../moje")
(load "../dao")
(load "../poker_ranks")
(load "../poker")
(load "../poker_logic")
(load "../poker_holdem")
(load "../poker_omaha")
(load "../poker_logs")
(load "../poker_percepts_rules")
(load "../poker_percepts")
(load "../http")
(load "../minimal")
(load "../view01")
(load "rules.lisp")


(defun create-system()
  #+sbcl
  (save-lisp-and-die "thx19.exe" :executable t)
  #+clisp
  (ext:saveinitmem "thx19.exe"  :quiet t :executable t ; :init-function #'http:start-server
		   ))

