(in-package :swank)

(eval-when (eval load compile)
(import '(lx::weak-pointer-p lx::weak-pointer-value lx::make-weak-pointer))
) ; end eval-when

;;#+hack-swank
(progn

(defun weak-value-gethash (key ht &optional default)  
  "Access hash-table with weak value."
  (multiple-value-bind (val foundp) (gethash key ht)
    (if foundp
	(values (if (weak-pointer-p val)
		    (weak-pointer-value val)
		    val)
		t)
	default)))

(defun (setf weak-value-gethash) (object key ht &optional default)  
  "Access hash-table with weak value."
  (if (and object (not (weak-pointer-p object))) ; FIXME to selectively wrap with a weak-pointer.
      (setf (gethash key ht) (make-weak-pointer object))
      (setf (gethash key ht) object))
  object)

(defun save-presented-object (object)
  "Save OBJECT and return the assigned id.
If OBJECT was saved previously return the old id."
  (or (gethash object *object-to-presentation-id*)
      (let ((id (incf *presentation-counter*)))
        (setf (weak-value-gethash id *presentation-id-to-object*) object)
        (setf (gethash object *object-to-presentation-id*) id)
        id)))

;(describe *object-to-presentation-id*)

(defun lookup-presented-object (id)
  "Retrieve the object corresponding to ID.
The secondary value indicates the absence of an entry."
  (weak-value-gethash id *presentation-id-to-object*))

) ; end #+hack-swank progn
