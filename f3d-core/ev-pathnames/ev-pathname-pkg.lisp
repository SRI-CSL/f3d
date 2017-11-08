(in-package :cl)
#+sbcl
(defpackage :lisp (:use "COMMON-LISP")
	    (:export load open probe-file delete-file truename dribble 
		     file-author file-write-date cd ensure-directories-exist rename-file compile-file))


(in-package :system-tool)

(defpackage :ev-pathnames (:use :common-lisp ) (:nicknames :evp)
	    (:import-from :system-tool "CD" "PWD" "ALLOW-REDEFINITION" "WITHOUT-PACKAGE-LOCKS" 
		"UNWIND-PROTECT-CASE" "GETENV" "MERGE-FREEDIUS-PATH" "*FREEDIUS-EXEC-PREFIX*"
		"EV-PATHNAME-TRANSLATE" "EV-PATHNAME-BACKTRANSLATE" "EV-PATHNAME-P" 
		"EV-TO-LOGICAL-PATHNAME" 
		;"EV-PATHNAME-REGISTER"
		)
	    #+allegro (:import-from :system-tool "DEFINE-SHADOW-SYSTEM" "COMPILE-OBJECT-FILE-PATHNAME")
	    )

