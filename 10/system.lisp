(load (compile-file "poker.ranks.lisp"))
(load (compile-file "poker.defs.lisp"))
(load (compile-file "poker.rulebased.lisp"))
(load (compile-file "poker.engine.lisp"))

(use-package :poker.ranks)
(use-package :poker.defs)
(use-package :poker.rulebased)
(use-package :poker.engine)