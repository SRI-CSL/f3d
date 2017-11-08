(in-package :transforms)

;;(defstruct-class CS-WEAK-CHILDREN-HASH-TABLE-MIXIN (CS-CHILDREN-HASH-TABLE-MIXIN) ())

;;; (defmethod make-children-hash-table ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN))
;;;   (lx::make-weak-hash-table ))

(defmethod children ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN))
  (loop with ht = (%children cs)
	for child being the hash-keys of ht
	collect (lx::follow-weak-pointer child)))

(defmethod map-over-children-int ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN) fn)
  (loop with ht = (%children cs)
	for child being the hash-keys of ht
	do (funcall fn (lx::follow-weak-pointer child))))

(defmethod remove-child ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN) child)
  (let ((ht (%children cs))
	(key (weak-child-handle child)))
    (when (gethash key ht)
      (setf (parent child) nil)
      (remhash key ht))))

(defmethod add-child ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN) child)
  (setf (parent child) cs)
  (setf (gethash (weak-child-handle child) (%children cs)) t))

