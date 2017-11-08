(in-package :config)

(st:define-system :ltk
    :required-systems '(lisp-extensions tk)
    :files `("ltk-pkg.lisp"
	     "ltk-lisptk.lisp"
	     "ltk-generic.lisp"
	     "ltk-tile.lisp"
	     "ltk-cvv.lisp"
	     ))

#|
(st:load-system :ltk :initialize t)

|#