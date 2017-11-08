(in-package :cl-user)



(defpackage :qffi 
  (:use :common-lisp :lcl)
	    (:export freedius-prefix def-foreign-constants set-foreign-value-constants foreign-code-address
		     load-foreign-library merge-shared-library-pathname-type require-shared-libraries
		     remap-foreign-symbol-name 
		     ;def-foreign-function def-foreign-callable

		     make-foreign-vector unmake-foreign-vector make-foreign-array
		     stationary-array-check make-stationary-vector unmake-stationary-vector
		     intern-foreign-pointer *PROHIBIT-TYPE-T-FOREIGN-VECTORS*
		     )
	    #+allegro (:import-from :cl-user make-pathname)
	    #+cmu (:import-from :system #:foreign-symbol-address #:sap-int #:int-sap)
	    #+cmu (:import-from :alien #:%sap-alien #:%sap-alien 
				#:parse-alien-type #:symbol-trampoline)
	    #+sbcl (:import-from :sb-alien #:sap-int #:int-sap)
	    )
