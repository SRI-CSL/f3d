(in-package :img)

(defmethod image-fill ((image scalar-image) val)
  (let ((buf (make-dfloat-scan-line-buffer image)))
    (setq val (dfloat val))
    (loop for x fixnum from 0 below (length buf)
	  do (setf (aref buf x) val))
    (loop for y fixnum from 0 below (image-y-dim image)
	  do (image-putline image buf 0 y))
    image))
	

(defmethod image-fill ((image vector-image) val)
  (loop for im in (component-images image)
        do (image-fill im val))
  image)

;;; This should be renamed to image-linear-photometric-transform
(defmethod image-linear-transform ((image scalar-image) scale offset 
				   &optional (element-type (image-element-type image)))
  (declare (double-float scale offset))
  (let* ((new-image (similar-image image :element-type element-type))
	 (xdim (image-x-dim image)))
    (declare (fixnum xdim))
    (with-scan-line-buffers ((buf (make-dfloat-scan-line-buffer image)))
      (require-image-rows (image new-image)
	(loop for y fixnum from 0 below (image-y-dim image)
	      do (image-getline image buf 0 y)
		 (loop for x fixnum from 0 below xdim
		       do (setf (aref buf x) (+ (* scale (aref buf x)) offset)))
		 (image-putline new-image buf 0 y))
	new-image))))



;;; This is broken:
;;; It is totally accidental that this works then source-image is RGBA8 and destination-image is
;;; (signed-byte 8).  getline on the RGBA8 image puts its results into 4 bytes per pixel buffer.
;;; putline to the destination-image expects an (unsigned-byte 32) buffer.
;;;(defmethod insert-image-into-image (source-image destination-image
;;;                                                 to-x to-y
;;;                                                 &optional
;;;                                                 (nx (image-x-dim source-image))
;;;                                                 (ny (image-y-dim source-image)))
;;;  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
;;;  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
;;;  (with-scan-line-buffers ((buf (make-scan-line-buffer source-image nx)))
;;;    (loop for y from 0 below ny
;;;          do (image-getline source-image buf 0 y)
;;;             (image-putline destination-image buf to-x (+ y to-y))))
;;;  destination-image)

;;(fmakunbound 'insert-image-into-image)

;;; This works for all combinations of vector and scalar images.
(defmethod insert-image-into-image ((source-image image) (destination-image image)
				    to-x to-y
				    &optional
				    (nx (image-x-dim source-image))
				    (ny (image-y-dim source-image)))
  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
  (with-scan-line-buffers ((buf (make-scan-line-buffers source-image nx)))
    (require-image-rows (source-image destination-image)
      (let* ((from-buf buf)
	     (to-buf (cond ((eq (typep source-image 'vector-image) (typep destination-image 'vector-image))
			    buf)
			   ((consp buf) (setq from-buf (list (car buf))) (car buf)) ; copy first component only
			   (t (loop repeat (length (component-images destination-image))
				    collect buf)))))
	(loop for y from 0 below ny
	      do (image-getline source-image from-buf 0 y)
		 (image-putline destination-image to-buf to-x (+ y to-y))))
      destination-image)))

#|
(setq rgb-image (load-image "$HOME/pix/forestcat.tif"))
(progn rgb-image)
(setq bigger-rgb-image (make-image '(512 512) :image-type 'color-image
				   :element-type '(unsigned-byte 8):n-bands 3))

(setq bigger-rgb-image (make-color-image-from-band-interleaved-image
			(make-image '(512 512) :element-type 'rgba8)))

(setq bigger-rgb-image (make-color-image '(512 512)))

(insert-image-into-image rgb-image bigger-rgb-image 0 0 )
(gui::push-image bigger-rgb-image (gui::selected-window))
(gui::pop-view (gui::selected-window))

(setq bw-image (make-image '(512 512)))
(insert-image-into-image rgb-image bw-image 0 0 )
(gui::push-image bw-image (gui::selected-window))
(make-scan-line-buffers rgb-image)
(class-of rgb-image)
(band-interleaved-image rgb-image)
(setq buf (make-scan-line-buffer rgb-image))
(image-getline rgb-image (list buf) 0 0)
(image-getline rgb-image (list buf) 0 0)

|#

;;;(defmethod insert-image-into-image ((source-image vector-image) (destination-image vector-image)
;;;                                    to-x to-y
;;;                                    &optional
;;;                                    (nx (image-x-dim source-image))
;;;                                    (ny (image-y-dim source-image)))
;;;  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
;;;  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
;;;  (with-scan-line-buffers ((bufs (make-integer-scan-line-buffers source-image nx)))
;;;    (loop for y from 0 below ny
;;;          do (image-getline source-image bufs 0 y)
;;;             (image-putline destination-image bufs to-x (+ y to-y))))
;;;  destination-image)

;;; What are the semantics of this? What component should we choose?
;;;(defmethod insert-image-into-image ((source-image vector-image) (destination-image scalar-image)
;;;                                    to-x to-y
;;;                                    &optional
;;;                                    (nx (image-x-dim source-image))
;;;                                    (ny (image-y-dim source-image)))
;;;  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
;;;  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
;;;  (with-scan-line-buffers ((bufs (make-integer-scan-line-buffers source-image nx)))
;;;    (loop for y from 0 below ny
;;;          do (image-getline source-image bufs 0 y)
;;;             (image-putline destination-image (car bufs) to-x (+ y to-y))))
;;;  destination-image)

;;; Copy single-component of source-image to all components of destination-image.
;;;(defmethod insert-image-into-image ((source-image scalar-image) (destination-image vector-image)
;;;                                    to-x to-y
;;;                                    &optional
;;;                                    (nx (image-x-dim source-image))
;;;                                    (ny (image-y-dim source-image)))
;;;  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
;;;  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
;;;  (with-scan-line-buffers ((buf (make-integer-scan-line-buffers source-image nx)))
;;;    (loop with bufs = (loop for comp in (component-images destination-image)
;;;                            collect buf)
;;;          for y from 0 below ny
;;;          do (image-getline source-image buf 0 y)
;;;             (image-putline destination-image bufs to-x (+ y to-y))))
;;;  destination-image)





;;;(defmethod insert-image-into-image ((source-image array-image-rgb) (destination-image array-image-rgb)
;;;                                    to-x to-y
;;;                                    &optional
;;;                                    (nx (image-x-dim source-image))
;;;                                    (ny (image-y-dim source-image)))
;;;  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
;;;  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
;;;  (with-scan-line-buffers ((buf (make-integer-scan-line-buffers source-image nx)))
;;;    (loop for y from 0 below ny
;;;          do (image-getline source-image buf 0 y)
;;;             (image-putline destination-image buf to-x (+ y to-y))))
;;;  destination-image)

;;;(defmethod insert-image-into-image ((source-image array-image-rgba) (destination-image array-image-rgba)
;;;                                    to-x to-y
;;;                                    &optional
;;;                                    (nx (image-x-dim source-image))
;;;                                    (ny (image-y-dim source-image)))
;;;  (setq nx (min nx (- (image-x-dim destination-image) to-x)))
;;;  (setq ny (min ny (- (image-y-dim destination-image) to-y)))
;;;  (with-scan-line-buffers ((buf (make-integer-scan-line-buffer source-image nx)))
;;;    (loop for y from 0 below ny
;;;          do (image-getline source-image buf 0 y)
;;;             (image-putline destination-image buf to-x (+ y to-y))))
;;;  destination-image)


(eval-when (eval load compile)
  (import '(lx::2^))
)

(defun fill-array (array value)
  (cond ((equal (array-element-type array) 'double-float)
	 (let ((array array)
	       (value (dfloat value)))
	   (declare (double-float value))
	   (declare (type (simple-array double-float (*)) array))
	   (loop for i fixnum from 0 below (length array)
		 do (setf (aref array i) value))))
	((equal (array-element-type array) '(unsigned-byte 8))
	 (let ((array array)
	       (value (round value)))
	   (declare (fixnum value))
	   (declare (type (simple-array (unsigned-byte 8) (*)) array))
	   (loop for i fixnum from 0 below (length array)
		 do (setf (aref array i) value))))

	((equal (array-element-type array) '(signed-byte 32))
	 (let ((array array)
	       (value (round value)))
	   (declare (fixnum value))
	   (declare (type (simple-array (signed-byte 32) (*)) array))
	   (loop for i fixnum from 0 below (length array)
		 do (setf (aref array i) value))))
	(t (error "fill-array: unsupported array element-type"))))
	
(defmethod image-get-bordered-line
	   ((image scalar-image) y &key into-array border (left-border (or border 0))(right-border left-border))
  (declare (fixnum left-border right-border))
  (let* ((x-dim (image-x-dim image))
	 (rt-index (+ x-dim (the fixnum (+ -1 left-border)))))
    (declare (fixnum x-dim rt-index))
    (image-getline image into-array 0 y x-dim left-border)
    (with-element-types (array-element-type into-array)
      (double-float single-float fixnum (signed-byte 32) t)
      (lambda (type)
	(let ((scalar-type (if (memq type '(single-float double-float))
			       'double-float 'fixnum)))
	  `(let ((into-array into-array))
	    (declare (type (simple-array ,type (*)) into-array))
	    (loop with left-pix ,scalar-type = (the ,scalar-type (aref into-array left-border))
		  for i fixnum from 0 below left-border do
		    (setf (aref into-array i) left-pix))
	    (loop with right-pix ,scalar-type = (the ,scalar-type (aref into-array rt-index))
		  for i fixnum from (the fixnum (+ x-dim left-border))
		  repeat right-border do
		    (setf (aref into-array i) right-pix))))))))


(defmethod image-neighborhood-min-max
	   ((image scalar-image) x-size &optional (y-size x-size) (overlap-p t))
  (declare (optimize (speed 3) (safety 2)))
  (declare (fixnum x-size y-size))
  (let* ((ylast (1- (image-y-dim image)))
	 (overlap (if overlap-p 1 0))
	 (minus-x-border 0)
	 ignore1
	 (xdim (multiple-value-setq (ignore1 minus-x-border) (ceiling (image-x-dim image) x-size)))
	 (ydim (ceiling (image-y-dim image) y-size))
	 ;; output produced in transposed order
	 (min-image (make-image (list xdim ydim) :element-type (image-element-type image)))
	 (max-image (make-image (list xdim ydim) :element-type (image-element-type image)))
	 (rt-border (+ (- minus-x-border) overlap))
	 ;;(top-border (+ (- minus-y-border) overlap))
	 (init-min-value (if (image-fixp image)
			     (dfloat (1- (the fixnum (2^ (image-element-size image)))))
			     most-positive-double-float))
	 (init-max-value  (if (image-fixp image)
			      0.0
			      most-negative-double-float))
	 )
    (declare (fixnum ylast overlap minus-x-border xdim ydim rt-border))
    (declare (double-float init-min-value init-max-value))
    ;;(declare (ignore ignore1 ))
    (require-image-rows
     (image min-image max-image)
     (progn		     ; unmake-images-on-abort (list min-image max-image)
       (with-scan-line-buffers
	   ((line (make-dfloat-scan-line-buffer image
						(+ (* xdim x-size) overlap)))
	    (min-line (make-dfloat-scan-line-buffer min-image xdim))
	    (max-line (make-dfloat-scan-line-buffer max-image xdim)))
	 (progn				; ic::noting-progress ("min-max" ydim)
	   (loop with v-rep fixnum = (+ (1- y-size) overlap)
		 with h-rep fixnum = (+ (1- x-size) overlap)
		 for out-y fixnum from 0 below ydim
		 for in-y fixnum from 0 by y-size
		 do
	      ;;(ic::note-progress out-y)
	      (fill-array  min-line init-min-value)
	      (fill-array max-line init-max-value)
	      (loop with count fixnum = (min v-rep (the fixnum (- ylast in-y)))
		    for i fixnum from 0 to count 
		    for y fixnum from in-y
		    do
		 (image-get-bordered-line image y :into-array line :right-border rt-border)
		 (loop for out-x fixnum from 0 below xdim
		       for in-x fixnum from 0 by x-size
		       do (loop for i fixnum from 0 below h-rep
				with min of-type image-float-buffer-element-type = (aref line in-x)
				with max of-type image-float-buffer-element-type = min
				for x fixnum from (1+ in-x)
				for val of-type image-float-buffer-element-type = (aref line x)
				do (when (< val min) (setq min val))
				   (when (> val max) (setq max val))
				finally
			     (setf (aref min-line out-x)
				   (the image-float-buffer-element-type (min (aref min-line out-x) min)))
			     (setf (aref max-line out-x)
				   (the image-float-buffer-element-type (max (aref max-line out-x) max)))))
		 ;;(break)
		 (image-putline min-image min-line 0 out-y )
		 (image-putline max-image max-line 0 out-y )))))
       (values min-image max-image)))))

;;; Attempted to remove Sun Oct 23 1994
;;; this makes a mapped image of the same size as the original with the borders
;;; Called by (unused) convolution code in gauss-image.lisp.
;;; Wrong! --- Called by IC::MAP-DTM-TO-OCTANT
(defun-cached make-bordered-image (image x-border y-border &key (border-type :replicate ))
  (let ((new-image (clone-image image)))
    (set-image-maps new-image
		    (make-bordered-map (image-x-map image) x-border border-type)
		    (make-bordered-map (image-y-map image) y-border border-type))
    new-image))





;;;
;;; This only works for double-float images????
;;; No, it is generic.  Only the scan-line-buffers are double-float.
;;; There was a bug in make-scan-line-buffer trying to fill the buffer with the 
;;; 0 instead of 0.0.

;(disassemble '(pcl::fast-method small-image-element-min-max (scalar-image)))
;(time (small-image-element-min-max (gui::view-image (gui::top-view))))
(defmethod small-image-element-min-max ((image scalar-image) &rest args &key
					(step 1)
					(zeros-invalid (image-prop image :zeros-invalid)))
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1) (debug 1)))
  ;#+cmu (declare (optimize (ext:inhibit-warnings 0)))
  (declare (ignore args zeros-invalid))
  (declare (fixnum step))
  (with-scan-line-buffers ((buf (make-dfloat-scan-line-buffer image)))
    (require-image-rows     (image)
     (let ((xdim (image-x-dim image))
	   (ydim (image-y-dim image)))
       (declare (fixnum xdim ydim))       
       (loop with vmin double-float = (diref image 0 0)
	     with vmax double-float = (diref image 0 0) ;this looks braindead, but CMUCL compiler needs it
	     for y fixnum from 0 below ydim by step
	     do (image-getline image buf 0 y )
		(loop for x fixnum from 0 below xdim by step
		      for val double-float = (aref buf x)
		      when (< val vmin)
			do (setf vmin val)
		      when (> val vmax)
			do (setf vmax val))						
	     finally (return (values vmin vmax))
	     )))))

;(fmakunbound 'image-element-min-max)
(defgeneric image-element-min-max (image &key step zeros-invalid)
  (declare (values double-float double-float)))

;;; Cached, but what happens when the image changes?  Is there an easy
;;; way to flush all pixel-level cached results when changes occur to
;;; the image?
;;;

;;; By caching this, we disallow photometric adjustments on re-used images...

(defmethod-cached image-element-min-max
    ((image scalar-image) &rest args &key
     (step 1) ;;(step (image-histogram-compute-step-size image 256))
     (zeros-invalid (image-prop image :zeros-invalid)))
  (unless (< (* (image-x-dim image) (image-y-dim image)) (* 4096 4096))
    (format t ";; Warning small-image-element-min-max applied to very large image."))
  (apply #'small-image-element-min-max image args))

;;;(defmethod image-element-min-max
;;;    ((image vector-image) &rest args &key
;;;     (step 1) ;;(step (image-histogram-compute-step-size image 256))
;;;     (zeros-invalid (image-prop image :zeros-invalid)))
;;;  (loop for img in (component-images image)
;;;        collect (mv-list (apply 'image-element-min-max img args))))
    
(defmethod image-element-min-max
	   ((image vector-image) &rest args &key
	    (step 1) ;;(step (image-histogram-compute-step-size image 256))
	    (zeros-invalid (image-prop image :zeros-invalid)))
  
  (loop for img in (component-images image)
	for (min max) = (mv-list (apply 'image-element-min-max img args))
	minimize min into min2
	maximize max into max2
	finally (return (values min2 max2))))

  
;(fmakunbound 'normalized-image-element-min-max)
(defmethod normalized-image-element-min-max ((image img::scalar-image))
  (mv-bind (min max) (img::image-element-min-max image)
    (let ((range 
	   (case (image-element-size image)
	     (8 256.0)
	     (16 (dfloat (ash 1 16)))
	     (32 1e7)
	     (otherwise (case (image-element-type image)
			  (single-float 1.0)
			  (double-float 1.0)
			  (otherwise (error "Bad image-element-type")))))))
      (values (/ min range) (/ max range))
      ;(values 0.0 (/ max range))
      )))

(defmethod normalized-image-element-min-max ((image img::vector-image))
  (loop for img in (img::component-images image)
	for (min max) = (mv-list (normalized-image-element-min-max img))
	minimize min into min2
	maximize max into max2
	finally (return (values min2 max2))))
    
#|
(disassemble 'small-image-element-min-max)
(disassemble' image-element-min-max)
(time (loop repeat 100 do (small-image-element-min-max gui::*z-image*)))
32,000 bytes consed.
(small-image-element-min-max gui::*z-image*)
(image-element-min-max (red-image (gui::view-image (gui::top-view))))
(image-element-min-max (gui::view-image (gui::top-view)))
(image-element-min-max2 (gui::view-image (gui::top-view)))
(image-element-size (gui::view-image (gui::top-view)))
|#


#|

;;; This apparently does no consing inner inner loop
(defun loop-test (buf)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*)) buf))
  (let ((vmin most-positive-double-float)
	(vmax most-negative-double-float)
	(val 0.0))
    (declare (double-float vmin vmax val))
    (loop for x fixnum from 0 below (length buf)
	  do (setq val (aref buf x))
	  when (< val vmin)
	    do (setq vmin val)
	  when (> val vmax)
	    do (setq vmax val)
	  finally (return (values vmin vmax))
	  )))

;;; This does 1 double-float cons in inner loop
(defun loop-test (buf)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*)) buf))
  (let ((vmin most-positive-double-float)
	(vmax most-negative-double-float))
    (declare (double-float vmin vmax))
    (loop for x fixnum from 0 below (length buf)
	  for val double-float = (aref buf x)
	  when (< val vmin)
	    do (setq vmin val)
	  when (> val vmax)
	    do (setq vmax val)
	  finally (return (values vmin vmax))
	  )))

;;; This does no consing in inner loop
(defun loop-test (buf)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*)) buf))
  (loop for x fixnum from 0 below (length buf)
	for val double-float = (aref buf x)
	minimize val into vmin double-float
	maximize val into vmax double-float
	finally (return (values vmin vmax))
	))

(disassemble 'loop-test)

(setq arr (make-array 10000 :element-type 'double-float :initial-element 1.0))

(time (loop repeat 100 do (loop-test arr)))
|#


#+not-converted
(defmethod-cached image-element-min-max
    ((image scalar-image)
     &key
     (step (image-histogram-compute-step-size image 256))
     (zeros-invalid (image-prop image :zeros-invalid)))
  ;; In case zeros-invalid and (iref image 0 0) was zero.
  (cond ((image-fixp image)
	 (let* ((min-val most-positive-fixnum)
		(max-val most-negative-fixnum)
		(invalid-value-p (if zeros-invalid
				     0
				     (image-prop image :invalid-value)))
		(invalid-value invalid-value-p ) )
	   (declare (fixnum min-val max-val invalid-value ))
	   ;;(declare (lcl::type-reduce number fixnum))
	   (if invalid-value-p
	       (apply-fn-over-image-sparsely
		(image step *getline-integer-buffer-element-type*)
		(lambda(v) 
		  `(let ((v ,v))
		     (declare (fixnum v))
		     (unless (eql v invalid-value ) 
		       (if (< v min-val) (setq min-val v))
		       (if (> v max-val) (setq max-val v))))))
	
	       (apply-fn-over-image-sparsely
		(image step *getline-integer-buffer-element-type*)
		(lambda(v) 
		  `(let ((v ,v))
		     (declare (fixnum v))
		     (if (< v min-val) (setq min-val v))
		     (if (> v max-val) (setq max-val v))))))
	   (values min-val max-val)))
	
	((image-floatp image)
	 (let* ((min-val most-positive-double-float)
		(max-val most-negative-double-float)
		(invalid-value-p (if zeros-invalid
				     0.0
				     (image-prop image :invalid-value)))
		(invalid-value invalid-value-p ))
	   (declare (double-float min-val max-val invalid-value ))
	   (declare (lcl::type-reduce number fixnum))
	   (if invalid-value-p
	       (apply-fn-over-image-sparsely
		(image step *getline-float-buffer-element-type*)
		(lambda(v) 
		  `(let ((v ,v))
		     (declare (double-float v))
		     (unless (= v invalid-value) ; Strat change from eql to =
		       (if (< v min-val) (setq min-val v))
		       (if (> v max-val) (setq max-val v))))))
	
	       (apply-fn-over-image-sparsely
		(image step *getline-float-buffer-element-type*)
		(lambda(v) 
		  `(let ((v ,v))
		     (declare (double-float v))
		     (if (< v min-val) (setq min-val v))
		     (if (> v max-val) (setq max-val v))
		     ))))
	   (values min-val max-val)))

	(t;; general type -- compute min-max over the numerical values - asssume floats
	 (let* ((min-val most-positive-double-float)
		(max-val most-negative-double-float))
	   (declare (double-float min-val max-val ))
	   (declare (lcl::type-reduce number fixnum))
	   (apply-fn-over-image-sparsely
	    (image step *getline-float-buffer-element-type*)
	    (lambda(v) 
	      `(let ((v ,v))
		 (declare (double-float v))
		 (when (numberp v)
		   (if (< v min-val) (setq min-val v))
		   (if (> v max-val) (setq max-val v)))
		 )))
	   (values min-val max-val)) )
	))



;;; dumb implementation
(defun linear-remap-image (image &key (into-image (similar-image image))
				 (gain 1.0) (offset 0.0))
  (declare (optimize (speed 3)(safety 0)))
  (declare (double-float gain offset))
  (loop for i fixnum from 0 below (image-x-dim image)
	do (loop for j fixnum from 0 below (image-y-dim image)
		 do (setf (diref into-image i j)
			  (+ (* (diref image i j) gain) offset))))
  into-image)

;; smarter
(defun linear-remap-image (image &key 
			   (element-type '(unsigned-byte 8))
			   (into-image (similar-image image :element-type element-type))
			   
				 (gain 1.0) (offset 0.0))
  (declare (optimize (speed 3)(safety 0)))
  (declare (double-float gain offset))
  (with-scan-line-buffers
      ((in (make-dfloat-scan-line-buffer image))
       (out (make-dfloat-scan-line-buffer into-image)))
    (loop with max double-float = (if (member (image-element-type into-image) 
					      '((unsigned-byte 8) (unsigned-byte 16))
					      :test #'equal)
				      (dfloat (1- (ash 1 (image-element-size into-image))))
				      (dfloat most-positive-single-float))
	  for y fixnum from 0 below (image-y-dim image)
	  do (image-getline image in 0 y)
	     (loop for x fixnum from 0 below (image-x-dim image)
		   for y double-float = (+ (* (aref in x) gain) offset)
		   when (> y max)
		     do (setq y max)
		   when (< y 0.0)
		     do (setf y 0.0)
		   do (setf (aref out x) y))
	     (image-putline into-image out 0 y)))
  into-image)





(defun image-round (float-image int-image &optional (float-min 0.0) (float-max 255.0)
		    (int-min 0) (int-max 255))
  (declare (optimize (speed 3)(safety 0))
	   (double-float float-min float-max)
	   (fixnum int-min int-max))
  (with-scan-line-buffers
      ((in (make-dfloat-scan-line-buffer float-image))
       (out (make-integer-scan-line-buffer int-image)))
    (loop with scale = (/ (dfloat (- int-max int-min)) (- float-max float-min))
	  for y fixnum from 0 below (image-y-dim float-image)
	  do (image-getline float-image in 0 y)
	     (loop for x fixnum from 0 below (image-x-dim float-image)
		   do (setf (aref out x) (round (+ int-min (* scale (- (aref in x) float-min)))  )))
	     (image-putline int-image out 0 y)))
  int-image)





(defmethod image-add (image-a image-b &key (into-image (similar-image image-a :element-type 'double-float)))
  (declare (optimize (speed 3)(safety 0)))
  (with-scan-line-buffers
      ((in-a (make-dfloat-scan-line-buffer image-a))
       (in-b (make-dfloat-scan-line-buffer image-b))
       (out (make-dfloat-scan-line-buffer into-image)))
    (loop for y fixnum from 0 below (image-y-dim image-a)
	  do (image-getline image-a in-a 0 y)
	     (image-getline image-b in-b 0 y)
	     (loop for x fixnum from 0 below (image-x-dim image-a)
		   do (setf (aref out x) (+ (aref in-a x) (aref in-b x))))
	     (image-putline into-image out 0 y)))
  into-image)


;;;
;;; Image type must be integer - we should be checking this:
(defmethod image-boole (image-a image-b &key (boole-op boole-ior) (into-image (similar-image image-a)))
  (declare (optimize (speed 3)(safety 0)))
  (with-scan-line-buffers
      ((in-a (make-scan-line-buffer image-a))
       (in-b (make-scan-line-buffer image-b))
       (out (make-scan-line-buffer into-image)))
    (loop for y fixnum from 0 below (image-y-dim image-a)
	  do (image-getline image-a in-a 0 y)
	     (image-getline image-b in-b 0 y)
	     (loop for x fixnum from 0 below (image-x-dim image-a)
		   do (setf (aref out x) (boole boole-op (aref in-a x) (aref in-b x))))
	     (image-putline into-image out 0 y)))
  into-image)


(defmethod image-subtract (image-a image-b &key (into-image (similar-image image-a :element-type 'double-float)))
  (declare (optimize (speed 3)(safety 0)))
  (with-scan-line-buffers
      ((in-a (make-dfloat-scan-line-buffer image-a))
       (in-b (make-dfloat-scan-line-buffer image-b))
       (out (make-dfloat-scan-line-buffer into-image)))
    (loop for y fixnum from 0 below (image-y-dim image-a)
	  do (image-getline image-a in-a 0 y)
	     (image-getline image-b in-b 0 y)
	     (loop for x fixnum from 0 below (image-x-dim image-a)
		   do (setf (aref out x) (- (aref in-a x) (aref in-b x))))
	     (image-putline into-image out 0 y)))
  into-image)

(defmethod image-linear-combine (image-a image-b &key (factor-a 1.0) (factor-b -1.0) (offset 0.0)
					 (into-image (similar-image image-a :element-type 'double-float)))
  (declare (optimize (speed 3)(safety 0)))
  (declare (double-float factor-a factor-b offset))
  (with-scan-line-buffers
      ((in-a (make-dfloat-scan-line-buffer image-a))
       (in-b (make-dfloat-scan-line-buffer image-b))
       (out (make-dfloat-scan-line-buffer into-image)))
    (loop for y fixnum from 0 below (image-y-dim image-a)
	  do (image-getline image-a in-a 0 y)
	     (image-getline image-b in-b 0 y)
	     (loop for x fixnum from 0 below (image-x-dim image-a)
		   do (setf (aref out x) 
			    (+ (* factor-a (aref in-a x)) (* factor-b (aref in-b x)) offset)))
	     (image-putline into-image out 0 y)))
  into-image)


#|| the remainder of this file in inside comments

(defparameter *rgb-map* nil)

(defun get-rgb-map ()
  (or *rgb-map*
      (setf *rgb-map*
	    (let ((map (make-array 256 :element-type '(unsigned-byte 32)))
		  (k1 (ash 255 24)))
	      (declare (type (simple-array (unsigned-byte 32) (*)) map))
	      (loop for i fixnum from 0 below 256
		    do ;;(setf (aref map i) (+ k1 (* i #x010101)))
		    (setf (aref map i) (* i #x01010101)))
	      map))))

(defun 8bit-to-rgb-image (img)
  (let* ((xdim (image-x-dim img))
	 (ydim (image-y-dim img))
	 (buf (make-array xdim :element-type '(unsigned-byte 32)))
	 (rgbbuf (make-array xdim :element-type '(unsigned-byte 32)))
	 (rgb-map (get-rgb-map))
	 (rgba-image (make-image (list xdim ydim)
				 :element-type '(unsigned-byte 32)))
	 )
    (declare (type (simple-array (unsigned-byte 32) (*)) buf rgbbuf rgb-map))
    (loop for y fixnum from 0 below ydim
	  do (image-getline img buf 0 y xdim)
	     (loop for x fixnum from 0 below xdim
		   for pix fixnum = (aref buf x)
		   do (setf (aref rgbbuf x) (aref rgb-map pix)))
	     (image-putline rgba-image rgbbuf 0 y xdim))
    rgba-image
    ))

 


#| Fast dithering algorithm:

Suppose you have a signal x[i], where each x[i] has m states: 0 to m-1.

We want to map x[i] into another signal y[i], where each y[i] has n states: 0 to n-1.

Do the following:

    y[i] = floor(x[i] + random(0,n),  n)

Where random(lo, hi) generates uniformly distributed integers in the range [lo:hi).

You can precompute (and reuse) a table of random numbers:

    r[k] = random(0,n)


    y[i] = floor(x[i] + r[i] , n)

You could also generate a table to compute floor:


    floortab[j] = floor(j, n);    (j from 0 to m)

    y[i] = floortab[x[i] + r[i]]


Of course, this algorithm assumes that n is large enough that error diffusion
(Floyd-Steinberg) can be avoided.  We are merely trying to reduce the contouring
created by truncating to 32 (or fewer) levels.


There are probably better ways to generate r[i], particularly when we really
have 2-dimensional signals x[i,j], and y[i,j].  For example, r[i,j] could be
a relatively small wxw (eg. 4x4 or 8x8) pattern which has the following properties:

   y[i,j] = floortab[x[i,j] + r[i mod w, j mod w]]

   1.  The histogram of r is approximately uniform.  

   2.  The local mean (with wrap-around) of r is approximately (n-1)/2.
       (Floyd-Steinberg error diffusion attempts to do this.)
   
   3.  When r is replicated both vertically and horizontally, there is no
       visible structure (not sure how to quantify this).

If 3 can be quantified, we can design an objective function and compute the
elements of r by optimizing the objective function.



|#


;;;
;;; Exhaustive search for maximum.  This assumes a SMALL image!!!
;;;
;;; What's wrong here?  In Geofex, this doesn't provide a meaningful
;;; value unless you crop the margin...
;;;

#-cmu
(defun image-true-min-max (image &optional (upper-limit 1050000))
  (let ((xdim (ic::image-x-dim image))
	(ydim (ic::image-y-dim image)))
    (if (> (* xdim ydim) upper-limit)
	(error "You cannot use this function on large images.")
	(let ((max 0)
	      (min 0))
	  (ic::with-image-elements (image)
	    (setq min (setq max (img::iref image 1 1)))
	    (loop for i fixnum from 1 below (1- xdim)
		  do (loop for j fixnum from 1 below (1- ydim)
			   for v =  (ic::iref image i j)
			   do (when (> v max) (setf max v))
			      (when (< v min) (setf min v)))
		  ))
	  (values max min)))))



(defun remap-image-display (image &optional (gain 2.0s0) (bias 0.0s0))
  (GL::glPixelTransferf GL::GL_RED_SCALE gain)
  (GL::glPixelTransferf GL::GL_GREEN_SCALE gain)
  (GL::glPixelTransferf GL::GL_BLUE_SCALE gain)

  (GL::glPixelTransferf GL::GL_RED_BIAS bias)
  (GL::glPixelTransferf GL::GL_GREEN_BIAS bias)
  (GL::glPixelTransferf GL::GL_BLUE_BIAS bias)
  (gui::release_image_textures (img::image-id image) 1))



(defun linear-remap-image (image &key (into-image (ic::similar-image image))
				 (gain 1.0) (offset 0.0))
  (loop for i fixnum from 0 below (ic::image-x-dim image)
	do (loop for j fixnum from 0 below (ic::image-y-dim image)
		 do (setf (ic::iref into-image i j)
			  (round (+ (* (ic::iref image i j) gain) offset)))))
  into-image)

(def-foreign-function (copy_bytes (:name "bcopy"))
  from to nbytes)

(defun make-image-from-rgba-array (array)
  (let* ((xdim (array-dimension array 0))
	 (ydim (array-dimension array 1))
	 (image (make-image (list xdim ydim) :element-type 'rgba8))
	 (image-array (image-array image))
	 (1d-input-array-32b (sys:underlying-simple-vector array))
	 )
    (copy_bytes 1d-input-array-32b image-array (length image-array))
    image))

	 
    
	 
#|
(setq img (make-image-from-rgba-array  (gui::backing-store (gui::view-window (gui::top-view)))))

(gui::push-image img (gui::selected-window *interactor*))

(setq img2 (make-image-from-rgba-array  (gui::backing-store (gui::view-window (gui::top-view)))))

(gui::push-image img2 (gui::selected-window *interactor*))

(let ((arr (viref img 0 0)))
  (values (aref arr 0) (aref arr 1) (aref arr 2) (aref arr 3)))

(describe img)
|#




||#





;(fmakunbound '8-bit-image-histogram)
;(disassemble '(pcl::fast-method 8-bit-image-histogram (scalar-image)))
(defmethod 8-bit-image-histogram ((image scalar-image) &rest args &key
				  level step (hits-per-bin 1024)
				  (zeros-invalid (image-prop image :zeros-invalid)))
;;  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1) (debug 2.5))
;;		 (optimize (speed 3) (safety 0) (debug 2.5) (ext:inhibit-warnings 0)))
  #-cmu (declare (optimize (speed 3) (safety 1) (debug 2)))
  (declare (ignore args zeros-invalid))
  (declare (type (or null fixnum) step))
  (let* ((nbins 256)
	 (level (or level (log2 (max 1 (floor (sqrt (/ (* (image-x-dim image) (image-y-dim image))
						       (dfloat (* hits-per-bin nbins)))))))))
	 (image (get-image-pyramid-level image level))
	 (hist (make-array nbins :element-type 'fixnum :initial-element 0))
	 (xdim (image-x-dim image)) 
	 (ydim (image-y-dim image))
	 (step (max 1 (floor (sqrt (/ (dfloat (* xdim ydim))
                                      (dfloat (* hits-per-bin nbins))))))))
    (declare (fixnum nbins step level xdim ydim ))
    (declare (type (simple-array fixnum (*)) hist))
    (with-scan-line-buffers ((buf (make-integer-scan-line-buffer image)))
      (require-image-rows     (image)
	(loop for y fixnum from 0 below ydim by step
	      do (image-getline image buf 0 y )
		 (loop for x fixnum from 0 below xdim by step
					;for val of-type (integer 0 255) = (aref buf x)
		       for val fixnum = (aref buf x)
		       do (incf (aref hist val))
		       do (setf (aref hist val) (the fixnum (1+ (aref hist val))))
		       ))))
    ;;(setq *foo* (list image level step))
    hist))



#|
;(8-bit-image-histogram (gui::view-image (gui::top-view)) :level 0)
(typep element-type 'image-int-buffer-element-type)
(subtypep (image-element-type (gui::view-image (gui::top-view)))  'image-int-buffer-element-type) 
(image-fixp (gui::view-image (gui::top-view))) 

|#



(defmethod zero-crossing-image (image &key zero (into-image (similar-image image)))
  (intensity-contour-image image zero into-image))

(defmethod intensity-contour-image (image &optional contour-level into-image)
  (unless contour-level
    (setq contour-level (if (image-floatp image) 0.0  128)))
  (format t "~%contour level: ~d" contour-level)
  (let* ((result (or into-image (make-image (image-dimensions image))))
	 (x-dim (image-x-dim image))
	 (y-dim (image-y-dim image)))
    (with-scan-line-buffers ((buf1 (make-scan-line-buffer image))
			     (buf2 (make-scan-line-buffer image))
			     (obuf (make-scan-line-buffer result)))
      (image-getline image buf1 0 0) 
      (let ((this-line buf1)
	    (next-line buf2))
	(loop for y from 0 below (1- y-dim) 
	      ;; Allegro doesn't like AND clauses (the bitch)...
	      ;; for this-line = buf1 then next-line
	      ;; and for next-line = buf2 then this-line
	      do
	   (image-getline image next-line 0 (1+ y))
	   (progn ; (with-array-registers (obuf this-line next-line) ...)
	     (loop for x from 0 below (1- x-dim) do
	       (let ((pt (aref this-line x))
		     (rt (aref this-line (1+ x)))
		     (up (aref next-line x)))
		 (setf (aref obuf x)
		       (if (or (neq (< pt contour-level) (< rt contour-level))
			       (neq (< pt contour-level) (< up contour-level)))
			   255 0))))
	     (image-putline result obuf 0 y))
	   (psetq this-line next-line
		  next-line this-line)))
      result)))





(defmethod image-sobel (image &key (into-image (similar-image image :element-type 'double-float)))
  "run sobel over image and return in result-image.
If result-image isnt given, 1 is created similar to image."
  (declare (optimize (speed 3)(safety 0)))
  (with-scan-line-buffers
      ((in-0 (make-scan-line-buffer image))
       (in   (make-scan-line-buffer image))
       (in-1 (make-scan-line-buffer image))
       (out  (make-dfloat-scan-line-buffer into-image)))

    (image-getline image in-0 0 0) 
    (image-getline image in-0 0 1)
    (loop for y fixnum from 2 below (1- (image-y-dim image))
	  do
       (image-getline image in-1 0 y)
       (loop for x fixnum from 1 below (1- (image-x-dim image))
	     for x-1 fixnum = (1- x)
	     for x+1 fixnum = (1+ x)
	     for dx double-float = (dfloat (- (+ (aref in-0 x-1) (* 2 (aref in x-1)) (aref in-1 x-1))
					      (+ (aref in-0 x+1) (* 2 (aref in x+1)) (aref in-1 x+1))))
	     for dy double-float = (dfloat (- (+ (aref in-0 x-1) (* 2 (aref in-0 x)) (aref in-0 x+1))
					      (+ (aref in-1 x-1) (* 2 (aref in-1 x)) (aref in-1 x+1))))
	     for grad double-float = (sqrt (+ (* dx dx) (* dy dy)))
	     do (setf (aref out x) grad))
       (image-putline into-image out 0 (1- y))
       (psetq in-0 in
	      in in-1
	      in-1 in-0))
   into-image))


(defmethod image-y-derivative (image &key (into-image (similar-image image :element-type 'double-float)))
  "run sobel over image and return in result-image.
If result-image isnt given, 1 is created similar to image."
  (declare (optimize (speed 3)(safety 0)))
  (with-scan-line-buffers
      ((in-0 (make-scan-line-buffer image))
       (in   (make-scan-line-buffer image))
       (in-1 (make-scan-line-buffer image))
       (out  (make-dfloat-scan-line-buffer into-image)))

    (image-getline image in-0 0 0) 
    (image-getline image in-0 0 1)
    (loop for y fixnum from 2 below (1- (image-y-dim image))
	  do
       (image-getline image in-1 0 y)
       (loop for x fixnum from 1 below (1- (image-x-dim image))
	     for x-1 fixnum = (1- x)
	     for x+1 fixnum = (1+ x)
	     for dy double-float = (dfloat (- (+ (aref in-0 x-1) (* 2 (aref in-0 x)) (aref in-0 x+1))
					      (+ (aref in-1 x-1) (* 2 (aref in-1 x)) (aref in-1 x+1))))
	     do (setf (aref out x) dy))
       (image-putline into-image out 0 (1- y))
       (psetq in-0 in
	      in in-1
	      in-1 in-0))
   into-image))



(defmethod image-x-derivative (image &key (into-image (similar-image image :element-type 'double-float)))
  "Pure X derivative, smoothed 3-point difference (partial sobel):"
  (declare (optimize (speed 3)(safety 0)))
  (with-scan-line-buffers
      ((in-0 (make-scan-line-buffer image))
       (in   (make-scan-line-buffer image))
       (in-1 (make-scan-line-buffer image))
       (out  (make-dfloat-scan-line-buffer into-image)))

    (image-getline image in-0 0 0) 
    (image-getline image in-0 0 1)
    (loop for y fixnum from 2 below (1- (image-y-dim image))
	  do
       (image-getline image in-1 0 y)
       (loop for x fixnum from 1 below (1- (image-x-dim image))
	     for x-1 fixnum = (1- x)
	     for x+1 fixnum = (1+ x)
	     for dx double-float = (dfloat (- (+ (aref in-0 x-1) (* 2 (aref in x-1)) (aref in-1 x-1))
					      (+ (aref in-0 x+1) (* 2 (aref in x+1)) (aref in-1 x+1))))
	     do (setf (aref out x) dx))
       (image-putline into-image out 0 (1- y))
       (psetq in-0 in
	      in in-1
	      in-1 in-0))
   into-image))



(defmethod image-flip-y ((image img::image))
  (with-scan-line-buffers ((buf1 (make-scan-line-buffer image))
			   (buf2 (make-scan-line-buffer image)))
    (loop for i fixnum from 0 below (floor (image-y-dim image) 2)
	  for j fixnum = (- (image-y-dim image) i 1)
	  do (image-getline image buf1 0 i)
	     (image-getline image buf2 0 j)
	     (image-putline image buf1 0 j)
	     (image-putline image buf2 0 i)))
  image)



;;;
;;; Single-index flip:
;;;
(defmethod image-flip-both ((image array-image))
  (let ((buf (image-array image)))
    (loop with n fixnum = (length buf)
          for i fixnum from 0 below (floor n 2)
          for i1 fixnum = (- n i 1)
          do (let ((x (aref buf i)))
               (setf (aref buf i) (aref buf i1))
               (setf (aref buf i1) x)))))


(defmethod image-flip-both ((image array-image-rgb))
  (let ((buf (image-array image)))
    (destructuring-bind (n ncomp)
        (array-dimensions buf)
      (loop for i fixnum from 0 below (floor n 2)
            for i1 fixnum = (- n i 1)
            do (let ((r (aref buf i 0))
                     (g (aref buf i 1))
                     (b (aref buf i 2))
                     (a (when (> ncomp 3) (aref buf i 3))))
                 (loop for k fixnum from 0 below ncomp
                       do (setf (aref buf i k) (aref buf i1 k)))
                 (setf (aref buf i1 0) r)
                 (setf (aref buf i1 1) g)
                 (setf (aref buf i1 2) b)
                 (when (> ncomp 3) (setf (aref buf i1 3) a)))))))
      
           
