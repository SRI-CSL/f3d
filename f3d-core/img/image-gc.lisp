(in-package :img)

;; (load (compile-file "$FREEDIUS/lisp/img/image-gc.lisp"))

;; This must be loaded AFTER the IMG subsystem, because the IMG package isn't defined yet.

#+cmu
(in-package :ext)

#+never
(lx::without-package-locks 

(defun finalize-corpses ()
  (setq *objects-pending-finalization*
	(set-difference *objects-pending-finalization*
			(loop for pair in *objects-pending-finalization*
			      when (multiple-value-bind
					 (object valid)
				       (weak-pointer-value (car pair))
				     (declare (ignore object))
				     (unless valid
				       (funcall (cdr pair))
				       t))
				collect pair)
			:test #'equal))
  nil)
) ; end lx::without-package-locks

(in-package :img)

(defmethod lx::weak-object-handle ((object image))
  (or (get-prop object :weak-object-handle)
      (setf (get-prop object :weak-object-handle)
	    (lx::make-weak-pointer object))))

(defun image-from-cimage-id (image-id)
  (let ((entry (gethash (image-id-hash-key image-id) *c-image-id-hash-table*)))
    (and entry (lx::follow-weak-pointer entry))))

(defvar *enable-image-finalization* t)

(defun make-cimage-id-entry (image-id image)
  ;; This next should really be part of make_lisp_array_image_internal.

  (when *enable-image-finalization*
    (typecase image
      (array-image (make-image-finalization image)
		   (make-image-pixel-container-finalization image))))

  (setf (gethash (image-id-hash-key image-id) *c-image-id-hash-table*) 
	(lx::weak-object-handle image))
  image)

;;; image-pixel-container is just a gc-able object what will get a finalization
(defstruct image-pixel-container
  value)

(defparameter *image-pixel-container-counter* 0)

(defmethod clone-image-after-copy-slots (image new-image)
  (setf (image-prop new-image :indirected-to) image)
  (push new-image (image-prop image :inferiors))
  
  (format t "clone-image-after-copy-slots ~a~%" new-image)
  (setf (image-prop new-image ::shared-pixel-container)
	(image-prop image ::shared-pixel-container))
  )

;;; all image clones call this
#+never
(defmethod set-image-maps :after ((image array-image) x-map y-map)
  (setf (image-prop new-image ::shared-pixel-container)
	(image-prop image ::shared-pixel-container))
  (format t "set-image-maps :after make-image-finalization ~a~%" image)
  (make-image-finalization image)
  (set_array_image_arrays (image-id image)
			  x-map y-map (length x-map) (length y-map) (image-array image) 1)
  )

;;#+cmu
(progn ; image finalization

;;; This finalizes the xmap, ymap, and foreign-code image-object
(defmethod make-image-finalization ((image array-image))
  (let ((xmap (image-x-map image))
	(ymap (image-y-map image))
	;;(image-id (image-id image))
	(msg (or (get-prop image :pathname) ; crap -- pathname isn't set when this is called.
		 (format nil "~a" image))))
    (lx::finalize-object image
			 #'(lambda(#+allegro object)
			     (format t ";; finalizing image ~a~%" msg)
			 ;    (unmake-stationary-vector xmap)
			 ;    (unmake-stationary-vector ymap)
			     ;; (delete_c_array_image image-id )
			     )
			 (lx::weak-object-handle image)
			 )))

;;; This finalizes the image-array
(defmethod make-image-pixel-container-finalization ((image array-image))
  ;; create closure
  (let* ((image-array  (image-array image))
	 (count (incf *image-pixel-container-counter*))
	 (msg (let ((*print-array* nil)) (format nil "counter= ~a ~a" count image-array))))
    (lx::finalize-object 
     (setf (image-prop image ::shared-pixel-container)
	   (make-image-pixel-container :value count))
     #'(lambda (#+allegro objec) 
	 (format t ";; finalizing array-image pixel-array ~a~%" msg)
	 ;(unmake-stationary-vector image-array)
	 ))))

) ; end #+cmu progn image finalization


#|
(let* ((fa ;(make-stationary-vector 100 :element-type '(unsigned-byte 8))
	(make-array 100 :element-type '(unsigned-byte 8)))
       (msg (let ((*print-array* nil)) (format nil "~a" fa)))
       )
  (lx::finalize-object (make-image-pixel-container) 
		       #'(lambda () (format t "finalizing ~s~%" msg)))
  nil)

(length ext::*objects-pending-finalization*)

(ext::weak-pointer-value (car (nth 0 ext::*objects-pending-finalization*)))
(ext::finalize-corpses)
(setf ext::*objects-pending-finalization* (last ext::*objects-pending-finalization*))


|#




;;; TEST CODE

(in-package :img)

#+never
(progn

(defun tst-wec (n)
  (eval-cache (tst-wec n)
    (make-image '(100 100))))
		   
(defclass foo () ((property-list :initform nil :initarg :property-list :accessor property-list)))

(defmethod woh ((o foo))
  (getf (property-list o) :woh))

(defmethod (setf woh) (val (o foo))
  (setf (getf (property-list o) :woh) val))

(defparameter *foo-ht* (make-hash-table))
(defparameter *foo-cnt* 0)

(defun tst-wec2 (n)
  (lx::weak-pointer-value
   (eval-cache (tst-wec2 n)
     (let* ((o (make-instance 'foo)) 
	    (woh (lx::make-weak-pointer o)))
       (setf (woh o) woh)
       (setf (gethash (incf *foo-cnt*) *foo-ht*) woh)
       woh))))

(defun print-foo-ht (&optional (ht *foo-ht*))
   (let ((broken-cnt 0) (total-cnt 0))
	 (maphash #'(lambda (key val) 
		      (format t "~s =~%   ~s~%" key val)
		      (incf total-cnt)
		      (when (consp val) (setq val (car val)))
		      (when (and (weak-pointer-p val) (null (weak-pointer-value val)))
			(incf broken-cnt)))
		  ht)
	 (format t "~a of ~a broken-pointers~%" broken-cnt total-cnt)))

(defun gc-and-test-for-broken-pointers (&optional (ht *c-image-id-hash-table*))
  (progn (setq * nil ** nil *** nil / nil // nil /// nil)
	 ;;(setq $ nil $$ nil $$$ nil LISPTK::$$ nil LISPTK::$ nil)
	 ;;#+allegro (setq EXCL::*RESOURCE-VECTOR* nil)
					; #+never
	 (progn (clrhash swank::*object-to-presentation-id*)
		(clrhash swank::*presentation-id-to-object*) 
		(swank::reset-inspector)
		)
	 (lx::full-gc)
	 (let ((broken-cnt 0) (total-cnt 0))
	   (maphash #'(lambda (key val) 
			(format t "~s =~%   ~s~%" key val)
			(incf total-cnt)
			(when (and (lx::weak-pointer-p val) (null (lx::weak-pointer-value val)))
			  (incf broken-cnt)))
		    ht)
	   (format t "~a of ~a broken-pointers~%" broken-cnt total-cnt))
	 ))

(defparameter *tst-wec-cnt* 0)

(defparameter *imgs* nil)


(defun getenv (var)
  #+cmu (cdr (assoc var ext:*environment-list* :test #'string=))
  #+sbcl (posix-getenv var)
  #+allegro (sys::getenv var))

) ; end progn


#|
(load (format nil "~a/lisp/boot.lisp" (getenv "FREEDIUS")))
(load-cme :start nil)
(st::initialize-system :lisp-extensions)

(setq *foo* (lx::make-weak-pointer (make-image '(100 100))))

(progn (load-image "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g0")
       nil)

(progn (load-image-pyramid "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB")
       nil)

(progn (load-image-pyramid "$RADIUS/site-2d-worlds/alv/alv-3-42")
       nil)

(let ((img (load-image "$RADIUS/site-2d-worlds/alv/alv-3-42/image.g0")))
  (gui::push-image img (gui::selected-window))
  nil)

(let ((img (load-image "$RADIUS/site-2d-worlds/alv/alv-3-42/image.g0")))
  (setq *img* img)
  nil)

(let ((img (load-image "$RADIUS/site-2d-worlds/alv/alv-3-42/image.g0")))
  ;(setq *img* img)
  (push img *imgs*)
  nil)

;;; this breaks pointers first time
(let* ((*enable-image-finalization* t)
       (img (tst-wec (incf *tst-wec-cnt*))))
  (setq *img* img)
  ;(push img *imgs*)
  nil)

;;; this breaks pointers first time but not after first time
(let* ((*enable-image-finalization* t)
       (img (tst-wec (incf *tst-wec-cnt*))))
  ;(setq *img* img)
  (push img *imgs*)
  nil)

;;; this breaks pointers first time
(let* ((*enable-image-finalization* nil)
       (img (tst-wec (incf *tst-wec-cnt*))))
  (setq *img* img)
  nil)

;;; this breaks pointers first time but not after first time
(let* ((*enable-image-finalization* nil)
       (img (tst-wec (incf *tst-wec-cnt*))))
  (push img *imgs*)
  nil)

(setq *img* nil)
(setq *imgs* nil)

(pop *imgs*)
(progn (pop *imgs*) nil)

(gc-and-test-for-broken-pointers)


(clrhash  *foo-ht*)
(clrhash  (get 'tst-wec :function-cache))
(clrhash  (get 'tst-wec2 :function-cache))


(loop for val being the hash-values of (get 'tst-wec :function-cache)
      collect (lx::weak-pointer-value (car val)))

lx::*weak-pointer-count* 15 initially

(print-foo-ht (get 'tst-wec :function-cache))
(print-foo-ht (get 'tst-wec2 :function-cache))
(print-foo-ht *foo-ht*)

(let ((o (tst-wec2 1)))
  (setq *img* o)
  nil)

(let () 
  (progn (setq * nil ** nil *** nil 
	       ;; $ nil $$ nil $$$ nil 
	       / nil // nil /// nil)
	 (clrhash swank::*object-to-presentation-id*)
	 (clrhash swank::*presentation-id-to-object*) 
	 (swank::reset-inspector)
	 )
  (lx::full-gc)
  (print-foo-ht)) ; weak pointers do not break


(setq *img* (gui::view-image (gui::selected-view)))
(setq *img* nil)

(get 'load-image :function-cache)
(lx::weak-object-handle *img*)

(hash-table-count (get 'load-image :function-cache))
(clrhash lx::*eval-cache-object-hash-table*)


|#


