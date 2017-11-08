(in-package :gl)

;;; Export all symbols in *package* that have function or global bindings.

(defun gl-export-all()
  (let ((pkg (find-package :gl)))
    (do-symbols (sym )
      (when (and (eq (symbol-package sym) pkg)
		 (or (fboundp sym)
		     ;;(member sym lcl::*foreign-function-symbols*)
		     (boundp sym)))
	(export sym pkg)))))


;;(st:add-system-initialization :gl '(gl-export-all))

(gl-export-all)
