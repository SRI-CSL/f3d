(IN-PACKAGE :transforms)

#|

New Sat Jan 16 1999

GENERAL WAY TO FIND THE CHAIN OF COORDINATE-TRANSFORMS BETWEEN TWO OBJECTS.

BASED ON SRIQCT CANONICAL COORDINATE-SYSTEM/COORDINATE-TRANSFORM TREE.

;;; Find the transform path between two arbitrary objects
(defmethod coordinate-transform-path (object-a object-b)
  ...
  )

Each object can be one of the following:

   gl-object
   gl-2d-world
   gl-3d-world or a LVCS
   view
   GEO-CS         geographic coordinate-system

The general idea is that COORDINATE-TRANSFORMS are ARCS in a graph and OBJECTS
are NODES in that graph.  We assume that each coordinate-transform has an
inverse-transform, making it possible to traverse each arc in either direction.
(It is possible that we actually have a set of isolated graphs).

We can construct a set of CANONICAL-COORDINATE-TRANSFORM-TREEs if each object
(except the root of the tree) has a unique SUPERIOR-COORDINATE-SYSTEM to which
it is connected by an OBJECT-TO-PARENT-TRANSFORM.  For the above list of
objects there is a natural choice of SUPERIOR-COORDINATE-SYSTEM:

   OBJECT TYPE    SUPERIOR-CS              OBJECT-TO-PARENT-TRANSFORM
____________________________________________________________________________________
   gl-object      superior		   object-to-parent-transform
   view		  2d-world		   (inverse-transform 2d-to-window-transform)
   gl-2d-world	  3d-world		   (inverse-transform 3d-to-2d-projection)
   gl-3d-world    geocentric-CS		   lvcs-to-geocentric-transform
   LVCS           geocentric-CS		   lvcs-to-geocentric-transform
   GEO-CS	  NIL or superior-GEO-CS   to-superior-transform

The SUPERIOR-COORDINATE-SYSTEM of a gl-object is either a composite-object or a gl-2d-world or
gl-3d-world.  In the implementation, SUPERIOR-COORDINATE-SYSTEM is the same as PARENT.

The superior-GEO-CS of a GEO-CS is chosen as follows:

   GEO-CS TYPE	      SUPERIOR-CS		  OBJECT-TO-PARENT-TRANSFORM
____________________________________________________________________________________
   WGS-84-geocentric  NIL		  NIL
   xxx-geocentric     WGS-84-geocentric   a linear 3x4 coordinate-transform     
   xxx-lat-long	      xxx-geocentric	  xxx-lat-long-to-geocentric-transform
   xxx-UTM	      xxx-lat-long	  xxx-utm-to-lat-long-transform
   xxx-state-plane    xxx-lat-long	  xxx-state-plane-to-lat-long-transform
   xxx-other	      xxx-lat-long	  xxx-other-to-lat-long-transform

Here we somewhat arbitrarily pick the WGS-84-geocentric-coordinate-system as the
root of the tree.  We allow disjoint coordinate-transform-trees, each having
their own root coordinate-system.  

Having constructed the CANONICAL-COORDINATE-TRANSFORM-TREE, it is trivial
to find a path between any two nodes in the tree:  

canonical-coordinate-transform-path (ROOT-CS ROOT-CS)
  = NIL

canonical-coordinate-transform-path (a ROOT-CS)
  = (cons (OBJECT-TO-PARENT-TRANSFORM a)
          (canonical-coordinate-transform-path (SUPERIOR-COORDINATE-SYSTEM a) ROOT-CS))

canonical-coordinate-transform-path (ROOT-CS a)
  = (reverse-transform-path (canonical-coordinate-transform-path a ROOT-CS))

canonical-coordinate-transform-path (a b)
  = (join-coordinate-transform-paths
       (canonical-coordinate-transform-path ROOT-CS a)
       (canonical-coordinate-transform-path ROOT-CS b))

reverse-transform-path (transform-list)
  = (mapcar #'inverse-transform (reverse transform-list))

Typically, the two objects a and b share a common sub-path of the tree connected
to the ROOT-CS.  This is trivially eliminated

join-coordinate-transform-paths (patha pathb)
  = (cond ((not (eq (from-coordinate-system (car patha))
		    (from-coordinate-system (car pathb))))
	   'no-path) ; no path exists between the objects

	  ((eq (car patha) (car pathb))
            ;; If the paths have a common prefix, join the remainder of the paths
	   (join-coordinate-transform-paths (cdr patha) (cdr pathb)))
	  
          (t ;; Finally we join the path from a to x with the path from x to b,
	   ;; where x is the common node in the tree furthest from the root.
	   (append (reverse-transform-path patha) pathb)))
		
The above computation of CANONICAL-COORDINATE-TRANSFORM-PATH is unique, ie there
is only one path between two objects.  The coordinate-transforms of the
COORDINATE-TRANSFORM-GRAPH that are not present in the CANONICAL-COORDINATE-
TRANSFORM-TREE provide alternative paths between objects.  For the present, we
propose that alternative paths be explicitly specified using a hash-table:

coordinate-transform-path (a b)
  = (or (gethash (cons a b) 'coordinate-transform-path-hash-table)
        (canonical-coordinate-transform-path a b))

|#



(defmacro superior-coordinate-system (cs)
  `(parent ,cs))

(defun canonical-coordinate-transform-path-from-root (object)
  (loop with path
	for obj = object then sup
	for to-sup-transform = (object-to-parent-transform obj)
	for sup = (superior-coordinate-system obj)
	while sup
	do (push (inverse-transform to-sup-transform) path)
	finally (return path)))

(defun canonical-coordinate-transform-path-to-root (object)
  (loop for obj = object then sup
	for to-sup-transform = (object-to-parent-transform obj)
	for sup = (superior-coordinate-system obj)
	while sup
	collect to-sup-transform))

(defun make-transform-path (from-object to-object)
  (canonical-coordinate-transform-path from-object to-object))

(defun canonical-coordinate-transform-path (object-a object-b)
  (if (eq object-a object-b)
      nil ; FIXME: should be identity-coordinate-transform 
      (let ((supa (superior-coordinate-system object-a))
	    (supb (superior-coordinate-system object-b)))
	(cond ((null supa)
	       (canonical-coordinate-transform-path-from-root object-b))
	      ((null supb)
	       (canonical-coordinate-transform-path-to-root object-a))
	      (t (join-coordinate-transform-paths
		  (canonical-coordinate-transform-path-from-root object-a)
		  (canonical-coordinate-transform-path-from-root object-b)))))))

(defmethod transform-path ((from-cs basic-coordinate-system) (to-cs basic-coordinate-system))
  (canonical-coordinate-transform-path from-cs to-cs))

(defun cs-to-cs-transform (obj1 obj2)
  (canonical-coordinate-transform-path obj1 obj2))

(defun cs-to-superior-cs-transform (object superior)
  (loop for obj = object then sup
	for to-sup-transform = (object-to-parent-transform obj)
	for sup = (superior-coordinate-system obj)
	collect to-sup-transform
	until (eq sup superior)))

;(cs-to-superior-cs-transform (gui::selected-object) (3d-world (top-view)))


;;;(defun-cached cached-cs-to-cs-transform (obj1 obj2)
;;;  (canonical-coordinate-transform-path obj1 obj2))

(defun-cached cached-cs-to-cs-transform-internal (obj1 obj2)
  (canonical-coordinate-transform-path obj1 obj2))
  
;;; This version is smart about the transform and its inverse.
(defun cached-cs-to-cs-transform (obj1 obj2)
  (or (lx::eval-cache-probe (cached-cs-to-cs-transform-internal obj1 obj2))
      (inverse-transform (lx::eval-cache-probe (cached-cs-to-cs-transform-internal obj2 obj1)))
      (cached-cs-to-cs-transform-internal obj1 obj2)))

(defun-cached cached-optimized-cs-to-cs-transform-internal (obj1 obj2)
  (optimize-transform (canonical-coordinate-transform-path obj1 obj2)))
  
;;; This version is smart about the transform and its inverse.
(defun cached-optimized-cs-to-cs-transform (obj1 obj2)
  (or (lx::eval-cache-probe (cached-optimized-cs-to-cs-transform-internal obj1 obj2))
      (inverse-transform (lx::eval-cache-probe (cached-optimized-cs-to-cs-transform-internal obj2 obj1)))
      (cached-optimized-cs-to-cs-transform-internal obj1 obj2)))


(defun cs-to-superior-cs-transform (object superior)
  (loop for obj = object then (parent obj)
	unless obj
	  return nil
	until (eq obj superior)
	collect (object-to-parent-transform obj)))

(defun superior-cs-to-cs-transform (superior object)
  (labels ((rev (obj path)
	     (if (eq obj superior)
		 path
		 (rev (parent obj) (cons (inverse-transform (object-to-parent-transform obj)) path)))))
    
    (collapse-transforms (rev object nil))))

#|
(superior-cs-to-cs-transform (gui::3d-world (gui::top-view)) (gui::selected-object))
(superior-cs-to-cs-transform (gcc wgs84) (gui::selected-object))
|#

(defun join-coordinate-transform-paths (patha pathb)
  (if (and patha pathb
	   (not (eq (from-coordinate-system (car patha))
		    (from-coordinate-system (car pathb)))))
      'no-path				; no path exists between the objects
      (loop while (and patha pathb (eq (car patha) (car pathb)))
	    ;; Skip over common prefix
	    do (pop patha) (pop pathb)
	       ;; and join the remainder of the paths
	    finally (return (nconc (reverse-transform-path patha) pathb)))))
	
;;;(defun reverse-transform-path (transform-path)
;;;  (loop with rev-path
;;;        for transform in transform-path
;;;        do (push (inverse-transform transform) rev-path)
;;;        finally (return rev-path)))

;;; nreverse based implementation to avoid consing
;;; WARNING: REVERSE-TRANSFORM-PATH modifies the original argument
(defun reverse-transform-path (transform-path)
  (do ((rest (cdr transform-path) (if (atom rest) rest (cdr rest)))
       (cell transform-path rest)
       (rev-path '() cell))
      ((atom cell) rev-path)
    (rplaca cell (inverse-transform (car cell)))
    (rplacd cell rev-path)))


(defun cs-to-lvcs-transform-path (cs-start)
  (loop for cs = cs-start then parent
	for parent = (parent cs)
	until (typep cs 'local-vertical-coordinate-system)
	unless parent
	  do (error "No LVCS found for ~a" cs-start)
	else collect (object-to-parent-transform cs)))

(defun lvcs-to-cs-transform-path (cs &optional path)
  (if (typep cs 'local-vertical-coordinate-system)
      path
      (let ((parent (parent cs)))
	(if parent
	    (lvcs-to-cs-transform-path parent (cons (inverse-transform (object-to-parent-transform cs)) path))
	    (error "No LVCS found for ~a" cs)))))

(defun cs-to-cs-of-class-transform-path (cs-start class)
  (loop for cs = cs-start then parent
	for parent = (parent cs)
	until (typep cs class)
	unless parent
	  do (error "No parent of class ~a found for ~a" class cs-start)
	else collect (object-to-parent-transform cs)))

(defun cs-of-class-to-cs-transform-path (cs class &optional path)
  (if (typep cs class)
      path
      (let ((parent (parent cs)))
	(if parent
	    (cs-of-class-to-cs-transform-path parent class (cons (inverse-transform (object-to-parent-transform cs)) path))
	    (error "No parent of class ~a found for ~a" class cs)))))


#|
(cs-to-lvcs-transform-path (gui::selected-object))
(lvcs-to-cs-transform-path (gui::selected-object))
(cs-to-cs-of-class-transform-path (gui::selected-object) 'obj::gl-2d-world)
(setq t1 (cached-optimized-cs-to-cs-transform (gui::3d-world (gui::top-view)) (lvcs (gdc wgs84) 39.5 -105.125)))
(setq t1inv (cached-optimized-cs-to-cs-transform (lvcs (gdc wgs84) 39.5 -105.125) (gui::3d-world (gui::top-view))))
(list t1 t1inv (inverse-transform t1))
(canonical-coordinate-transform-path (gui::3d-world (gui::top-view)) (lvcs (gdc wgs84) 39.5 -105.125))

|#

#|
(maybe-compile-file-load "$FREEDIUS/lisp/transforms/transform-path.lisp")

(canonical-coordinate-transform-path (gui::selected-object )
				     (gui::selected-view))
(canonical-coordinate-transform-path (gui::selected-object )
				     (2d-world (gui::selected-view)))
(canonical-coordinate-transform-path-from-root (gui::selected-view))
(canonical-coordinate-transform-path-from-root (gui::selected-object ))
(gui::selected-object )
(describe (object-to-parent-transform (gui::selected-view)))
(canonical-coordinate-transform-path (gui::selected-view)
				     (gui::selected-object ))

(canonical-coordinate-transform-path
 (gui::selected-object ) (get-gcc 'nad27))

(canonical-coordinate-transform-path
 (gui::selected-object ) (get-gdc 'nad27))

(canonical-coordinate-transform-path-to-root (get-gdc 'nad27))

(canonical-coordinate-transform-path
 (gui::selected-object ) (get-gcc 'wgs84))

(canonical-coordinate-transform-path
 (gui::selected-object ) (get-gdc 'wgs84))

(canonical-coordinate-transform-path
 (gui::selected-object ) (get-gdc 'wgs72))


(gui::selected-object )
(set-parent-transform (gui::selected-object )
		      (object-to-parent-transform (gui::selected-object )))
(gui::selected-view)
(2d-to-window-transform (gui::selected-view))
(object-to-parent-transform (gui::selected-view))
(parent (gui::selected-object ))
(superior-coordinate-system (gui::selected-object ))
(object-to-parent-transform (gui::selected-object ))
(canonical-coordinate-transform-path-from-root (gui::selected-object ))
(canonical-coordinate-transform-path-from-root (get-gcc 'wgs84))
(inspect (world (gui::selected-object )))
(inspect (gui::selected-object ))
(inspect (top-view))
|#


;;; **************************  CS-TO-CS-TRANSFORM-MATRIX  ********************************

(defvar *canonical-coordinate-transform-matrix-to-root-transform-matrix* (make-4x4-identity-matrix))

(defun canonical-coordinate-transform-matrix-to-root (object &optional (result-mat *canonical-coordinate-transform-matrix-to-root-transform-matrix*))
  (let ((trans (object-to-parent-transform object)))
    (unless trans
      (return-from canonical-coordinate-transform-matrix-to-root
	nil))

    (copy-4x4-matrix (transform-matrix trans) result-mat)
    (loop for cs = (parent object) then sup
	      for sup = (parent cs)
	      while sup
	      do (multiply-4x4-matrices 
		  (transform-matrix (object-to-parent-transform cs)) 
		      result-mat result-mat))
    result-mat))
   
(defvar *canonical-coordinate-transform-matrix-from-root-transform-matrix* (make-4x4-identity-matrix))

(defun canonical-coordinate-transform-matrix-from-root (object &optional (result-mat *canonical-coordinate-transform-matrix-from-root-transform-matrix*))
  (let ((parent (parent object))
	(trans (object-to-parent-transform object)))
    (cond ((null parent)
	   nil)
	  ((null (parent parent))
	   (copy-4x4-matrix (transform-matrix (inverse-transform trans)) result-mat))
	  (t
	   (let ((sup-mat (canonical-coordinate-transform-matrix-from-root parent)))
	     (multiply-4x4-matrices (transform-matrix (inverse-transform trans)) sup-mat sup-mat))))))

;;; This implementation has the unfortunate property that transforms that are
;;; common to the paths from obj1 and obj2 to the root-cs have their
;;; transform-matrices multiplied by their inverses.  Thus there will be small errors
;;; on the order of 1e-9 in the result.
(defun cs-to-cs-transform-matrix (obj1 obj2)
  (let ((mat1 (canonical-coordinate-transform-matrix-to-root obj1))
	(mat2 (canonical-coordinate-transform-matrix-from-root obj2)))
    (cond ((and mat1 mat2) (multiply-4x4-matrices mat2 mat1))
	  (mat1 mat1)
	  (mat2 mat2)
	  (t (make-4x4-identity-matrix)) ; should NIL be returned here instead?
	  )))

;;; The only thing wrong with this implementation is that there is CONSING of the order of the length
;;; of the transform-path.  Probably not a big deal.
(defun cs-to-cs-transform-matrix (obj1 obj2)
  (multiply-4x4-transform-matricies 
   (canonical-coordinate-transform-path obj1 obj2)))

(defun-cached cached-cs-to-cs-transform-matrix (obj1 obj2)
  (multiply-4x4-transform-matricies 
   (canonical-coordinate-transform-path obj1 obj2)))



#|
(cs-to-cs-transform-matrix (get-gcc 'wgs84) (get-gcc 'wgs72))

(defun test-cs-to-cs-transform-matrix (from to)
  (let ((path (canonical-coordinate-transform-path from to)))
    (list (cs-to-cs-transform-matrix from to)
	  (multiply-4x4-transform-matricies path)
	  path)))

(test-cs-to-cs-transform-matrix (gui::selected-object )
					(make-lvcs "WGS84" 39.50586158474837 -105.11441597675547
						   :local-units-per-meter *feet-per-meter*))

(test-cs-to-cs-transform-matrix (gui::selected-object ) (3d-world (gui::selected-view)))
(test-cs-to-cs-transform-matrix (gui::selected-object ) (gui::selected-view))

(test-cs-to-cs-transform-matrix (gui::selected-object ) (gui::selected-object ))
(test-cs-to-cs-transform-matrix (parent (gui::selected-object )) (gui::selected-object ))
(test-cs-to-cs-transform-matrix (gui::selected-object ) (parent (gui::selected-object )))


(canonical-coordinate-transform-path (gui::selected-object) (3d-world (gui::top-view)))
(canonical-coordinate-transform-path (gui::selected-object) (gui::view-image (gui::top-view)))
|#
