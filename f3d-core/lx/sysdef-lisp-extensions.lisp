(in-package :cl-user)

(defvar config::weak-eval-cache nil)

(st:define-system :lisp-extensions
    :required-systems '(common-symbols ev-pathnames lcl)
    :files `("lx-pkg.lisp"
	     "custom.lisp"
	     "colors.lisp"             ; Not the best place, but see file for issues.
	     "file-property-list.lisp" ; this probably should move to system-tool
	     "lisp-extensions.lisp"
	     "pathname-extensions.lisp"	; would like to eliminate this
	     "struct-class.lisp"
	     ,@(if config::weak-eval-cache
		  `("weak-eval-cache.lisp" 
		    ;; swank-weak-hash-table.lisp is broken with recent slime/swank versions
		    #+never ,@(when (find-package :swank)
				  '("swank-weak-hash-table.lisp"))
		    )
		  '("eval-cache.lisp"))
	     "lisp-io.lisp"
	     "universal-time.lisp"
	     "clos-tools.lisp"
	     "binary-search.lisp"
	     ))
