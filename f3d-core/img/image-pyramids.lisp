(in-package :img)

#|

Historically, this file mostly provided an interface between C code dealing with image-pyramids,
and file-system conventions for storing the pyramids.

NEW Sun May 18 2003:

IMAGE-PYRAMID  is a first-class object now.  The C code image pyramid support is being
replaced with Lisp image pyramids.

|#


(defparameter *image-pyramid-default-pyramid-type* 'gauss-2)
;;; Permitted pyramid-types: gauss-2, gauss-2-min-alias, box-2
;;; Currently, only box-2 is supported for color-images.

(defstruct-class image-pyramid (property-list-struct)
  ((levels :initform (make-array 1 :initial-element nil) :type simple-vector )
   (level-constructor :initform nil)
   (pyramid-type :initform *image-pyramid-default-pyramid-type*) 
   (damaged :initform nil :accessor pyramid-damaged)
   ))


;;; This seems to be necessary under the 11/2004 snapshot of MacOSX
;;; CMUCL - it's possible that some part of LOOP is broken, but I'm
;;; not sure.  In any case, the pyramid-type is never reset to box-2
;;; as it should be in create-pyramid-level-image below.

;;; LHQ Tue Nov 23 2004 Chris: Yes, the problem is a change in the loop macro to
;;; conform to the ANSI standard.  The problem is with the order of INITIALLY
;;; clauses vs. FOR clauses.  I have made a change to avoid the using the INITIALLY
;;; clause.


;;;(defmethod maybe-force-box-2-pyramid-type ((pyramid image-pyramid))
;;;  (with-class-slots image-pyramid (levels pyramid-type) pyramid
;;;      (when (some #'(lambda (x) (and x (member (image-element-type x) '(RGB8 RGBA8))))
;;;                  levels)
;;;        (setf pyramid-type 'box-2))))
	     

(defun set-pyramid-damaged (image)
  (let ((pyramid (get-prop image :pyramid)))
    (when pyramid
      (setf (pyramid-damaged pyramid) t))))

(defun make-image-pyramid (&optional level-constructor)
  (make-instance 'image-pyramid
		 :level-constructor level-constructor))

(defun make-pyramid-level-image (from-image)
  (let* ((xdim (image-x-dim from-image))
	 (ydim (image-y-dim from-image))
	 (element-type (image-element-type from-image))
	 (reduction 2)
	 (into-xdim (floor xdim reduction))
	 (into-ydim (floor ydim reduction))
	 )
    (make-image (list into-xdim into-ydim) :element-type element-type)))

(defmethod create-pyramid-level-image ((image-pyramid image-pyramid) level)
  ;(maybe-force-box-2-pyramid-type image-pyramid)
  (with-class-slots image-pyramid (levels level-constructor pyramid-type) image-pyramid
    (if level-constructor
	(let ((image (funcall level-constructor level)))
	  (setf (get-prop image :pyramid) image-pyramid)
	  (setf (aref levels level) image)
	  image)
		
	(let ((defined-level 
	       (loop for i from (1- level) downto 0
		     when (aref levels i) 
		       return i
		     finally (error ";;; CREATE-PYRAMID-LEVEL-IMAGE no levels in pyramid"))))
	  (when (member (image-element-type (aref levels defined-level))
			'(RGB8 RGBA8))
	    (setf pyramid-type 'box-2))
	  
	  (loop	;; create all of the gauss levels that are needed 
		for i from (1+ defined-level) to level
		for into-image = (make-pyramid-level-image (aref levels (1- i)))
		for image = (image-filter-decimate2 (aref levels (1- i)) pyramid-type into-image)
		do (setf (get-prop image :pyramid) image-pyramid)
		   (setf (aref levels i) image)
		finally (return image))))))
		

#|
(setq img (gui::view-image (gui::selected-view)))
(describe img)
(describe (gui::selected-view))
(describe (get-prop img :pyramid))
(let ((*print-array* t)) (pprint (image-pyramid-levels (get-prop img :pyramid))))
(eq (aref (image-pyramid-levels (get-prop img :pyramid)) 0)
    (aref (image-pyramid-levels (get-prop img :pyramid)) 1)) = T
(property-list (aref (image-pyramid-levels (get-prop img :pyramid)) 4))

(setq img (gui::view-image (gui::selected-view)))
(describe (gui::image-for-display img))
(describe (get-prop (gui::image-for-display img) :pyramid ))
(image-to-2d-transform (gui::image-for-display img))
|#

(defmethod create-pyramid-level ((image-pyramid image-pyramid) level &optional image)
  (declare (fixnum level))
  (with-class-slots image-pyramid (levels pyramid-type) image-pyramid
    (when (>= level (length levels))
      ;; extend the levels vector as needed.
      (let ((new-levels (make-array (+ level 1) :initial-element nil)))
	(loop for i from 0 below (length levels)
	      do (setf (aref new-levels i) (aref levels i)))
	(setq levels new-levels))
      (setq levels (make-array (+ level 1) :initial-contents levels)))
    
    (unless (aref levels level)
      (unless image
	(setq image (create-pyramid-level-image image-pyramid level)))
      (setf (get-prop image :pyramid) image-pyramid)
      (setf (aref levels level) image)
      (set-image-to-2d-transforms image-pyramid level))
    
    (aref levels level)))

(defun make-new-image-pyramid (image &optional (level 0))
  (let ((pyramid (make-image-pyramid)))
    (create-pyramid-level pyramid level image)
    pyramid))

(defmethod pyramid-level ((image-pyramid image-pyramid) level &optional (instantiate t))
  (declare (fixnum level))
  (when (< level 0) (setq level 0))
  (with-class-slots image-pyramid (levels damaged) image-pyramid
    (if (or (>= level (length levels))
	    (null (aref levels level)))
	(when instantiate
	  (let ((image (create-pyramid-level image-pyramid level)))
	    ;;(set-image-to-2d-transforms image-pyramid level)
	    image))
	
	(progn (when damaged
		 (recompute-pyramid image-pyramid)
		 (setq damaged nil))	       
	       (aref levels level)))))

(defmethod pyramid-level ((image image) level &optional (instantiate t))
  (let ((pyramid (get-prop image :pyramid)))
    (unless pyramid
      ;; image must the the base-image
      (setq pyramid (make-new-image-pyramid image)))      
    (pyramid-level pyramid level instantiate)))

(defun image-pyramid-level-index (image)
  (let ((pyramid (get-prop image :pyramid)))
    (unless pyramid
      ;;; hope that image is really the base-image
      (setq pyramid (make-new-image-pyramid image)))
    (loop with levels = (slot-value pyramid 'levels)
	  for i from 0 below (length levels)
	  when (eq (aref levels i) image)
	    return i)))

;;; This assumes that image at pyramid level 0 has been modified.
(defmethod recompute-pyramid ((pyramid image-pyramid))
  (with-class-slots image-pyramid (levels pyramid-type damaged) pyramid
    (release-image-textures (aref levels 0))
    (loop for level from 1 below (length levels)
	  for image = (aref levels level)
	  unless image
	    do (create-pyramid-level-image pyramid level)
	  else
	    do ;;(format t "recompute-pyramid ~a~%" level)
	       (release-image-textures image)
	       (image-filter-decimate2 (aref levels (1- level)) pyramid-type image))
    (setf damaged nil)))

#|
(setq img (gui::view-image (gui::selected-view)))
(transforms::transform-matrix (img::image-to-2d-transform img))
(gui::image-for-display img)
(img::image-to-2d-transform (gui::image-for-display img))
(describe img)
(describe (get-prop img :pyramid))
(image-pyramid-level-index img)
(image-pyramid-level-index (pyramid-level (get-prop img :pyramid) 6))
(pyramid-level img 8)
(let ((*print-array* t) (math::*print-vector-length* 100)) (print (slot-value (get-prop img :pyramid) 'levels)))
(gui::get-image-pyramid-level img 4)
(pyramid-level (get-prop img :pyramid) 6)

(setq rugby (load-image "/homedir/quam/pix/rugby.pic"))
(pyramid-level rugby 1)
(pyramid-level rugby 3)
(setq forestcat (load-image "/homedir/quam/pix/forestcat.tif"))
(gui::push-image forestcat (gui::selected-window gui::*interactor*))
(describe forestcat)
(image-pyramid-level-index (gui::image-for-display forestcat))
(image-pyramid-level-index (red-image forestcat))


|#


#+from-cme-6
(defmethod set-image-to-2d-transforms ((pyramid image-pyramid) top-image-to-2d-transform
				       &optional 2d-world)
  (unless 2d-world
    (setq 2d-world (loop for image in (image-list pyramid)
			 thereis (and image (image-prop image :2d-world)))))
  (unless top-image-to-2d-transform
    (setq top-image-to-2d-transform
	  (cme::make-4x4-coordinate-transform (cme::make-4x4-identity-matrix))))
  (loop with relative-matrix = (pyramid-relative-transform-matrix  pyramid)
	for image-to-2d-transform first top-image-to-2d-transform
	  then (cme::make-4x4-coordinate-transform
		(math:multiply-matrices (transform-matrix image-to-2d-transform) relative-matrix))
	for image in (image-list pyramid)
	when image
	  do (setf (image-prop image :image-to-2d-transform) image-to-2d-transform)
	     (when 2d-world (setf (image-prop image :2d-world) 2d-world))
	))

;;; mapping for Burt's gaussian
(defun gauss-image-mapping (x scale)
  (declare (double-float x scale))
  (1- (* (1+ x) scale)))

(defvar *gauss-2-relative-transform-matrix* nil)

(defmethod pyramid-relative-transform-matrix ((pyramid-type (eql 'gauss-2)))
  (or *gauss-2-relative-transform-matrix*
      (setq *gauss-2-relative-transform-matrix*
	    (math::make-and-fill-4x4-matrix
	     2.0 0.0 0.0 1.0 ; (gauss-image-mapping 0.0 2.0)
	     0.0 2.0 0.0 1.0 ; (gauss-image-mapping 0.0 2.0)
	     0.0 0.0 1.0 0.0 
	     0.0 0.0 0.0 1.0))))

(defvar *box-2-relative-transform-matrix* nil)

(defmethod pyramid-relative-transform-matrix ((pyramid-type (eql 'box-2)))
  (or *box-2-relative-transform-matrix*
      (setq *box-2-relative-transform-matrix*
	    (math::make-and-fill-4x4-matrix
	     2.0 0.0 0.0 .5
	     0.0 2.0 0.0 .5
	     0.0 0.0 1.0 0.0 
	     0.0 0.0 0.0 1.0))))

(defmethod pyramid-relative-transform-matrix ((pyramid image-pyramid))
  (with-class-slots image-pyramid (pyramid-type) pyramid
    (pyramid-relative-transform-matrix pyramid-type)))

(defmethod set-image-to-2d-transforms ((pyramid image-pyramid) &optional
				       level top-image-to-2d-transform)
  (with-class-slots image-pyramid (levels) pyramid
    (unless level
      (setq level (1- (length levels))))
    (let* ((defined-level (loop for lev from level downto 0
				when (probe-image-to-2d-transform (aref levels lev))
				  do (return lev)))
	   (relative-matrix (pyramid-relative-transform-matrix pyramid))
	   )
      (loop for lev from (or defined-level 0) to level
	    for image = (aref levels lev)
	    for image-to-2d-transform = (if defined-level
					    (probe-image-to-2d-transform (aref levels defined-level))
					    (or top-image-to-2d-transform
						(transforms::make-4x4-coordinate-transform
						 (math::make-4x4-identity-matrix))))
	      then (or (and image (probe-image-to-2d-transform image))
		       (transforms::make-4x4-coordinate-transform
			(math:multiply-matrices (transforms::transform-matrix image-to-2d-transform)
			     relative-matrix)))
	    when image
	      do (setf (image-to-2d-transform image) image-to-2d-transform)))))


#|
(setq pyr (get-prop (gui::view-image (gui::selected-view gui::*interactor*)) :pyramid))
(describe (pyramid-level pyr 2))
(setq *print-array* t)
(transforms::transform-matrix (get-prop (pyramid-level pyr 2) :image-to-2d-transform))
|#

(defmethod recompute-image-to-2d-transforms ((pyramid image-pyramid) &optional top-image-to-2d-transform)
  (with-class-slots image-pyramid (levels) pyramid
    (unless top-image-to-2d-transform
      (setq top-image-to-2d-transform (transforms::make-4x4-coordinate-transform
				       (math::make-4x4-identity-matrix))))
    (let* ((relative-matrix (pyramid-relative-transform-matrix pyramid)))
      (loop for lev from 0 below (length levels)
	    for image = (aref levels lev)
	    for image-to-2d-transform = top-image-to-2d-transform
	      then (transforms::make-4x4-coordinate-transform
		    (math:multiply-matrices (transforms::transform-matrix image-to-2d-transform)
			 relative-matrix))
	    when image
	      do (setf (image-to-2d-transform image) image-to-2d-transform)))))


;;; This assumes that wild-string is of the form <prefix>* and we return true if
;;; string has <prefix> is its prefix.
(defun match-wild-string-suffix (wild-string string)
  (let ((n (1- (length wild-string))))
    (string= wild-string string :end1 n :end2 n)))

(defun wildcard-search-for-pyramid-files (full-wildcard-pathname)
  (let* ((dir-path (if (equal (pathname-type full-wildcard-pathname)  "*")
		       full-wildcard-pathname
		       (make-pathname :defaults full-wildcard-pathname :type "*")))
	 (paths (directory dir-path)))
    (if (equal dir-path full-wildcard-pathname)
	paths
	(loop with type-spec = (pathname-type full-wildcard-pathname)
	      for path in paths
	      when (match-wild-string-suffix type-spec (pathname-type path))
		collect path))))

(defun read-image-pyramid-descr-file (path)
  (let ((*package* (find-package "IC")))
    (setq path (ev-pathname-translate path))
    ;;(format t "read-descr-file ~a~%" path)
    (with-open-file (st path)
      (values-list (loop with flag = (list nil)
			 for form = (read st nil flag)
			 until (eq form flag)
			 collect form)))))

(declaim (special *cme-descriptor-filename-prefix*))

#+mswindows
(defun case-insensitive-probe (pathname)
  (and (probe-file pathname) pathname))

#-mswindows
(defun case-insensitive-probe (pathname)
  ;;
  ;; Needed to locate a file that might reside on a FAT partition,
  ;; since the flaming idiots that "designed" windoze didn't get it
  ;; right.  Check for the original pathname, the lowercase pathname,
  ;; and then the pathname with the FIRST letter upcased, in that
  ;; order.
  ;;
  (or (and (probe-file pathname) pathname)
      (let ((new-pathname (make-pathname
			   :host (pathname-host pathname)
			   :directory (pathname-directory pathname)
			   :device (pathname-device pathname)
			   :name (string-downcase (pathname-name pathname))
			   :type (string-downcase (pathname-type pathname))
			   )))
	(or (and (probe-file new-pathname) new-pathname)
	    (progn
	      (setf (aref (pathname-name new-pathname) 0)
		    (char-upcase (aref (pathname-name new-pathname) 0)))
	      (and (probe-file new-pathname) new-pathname))))))

(defun find-pyramid-file (prefix-pathname)
  (or (case-insensitive-probe
       (merge-directories-and-pathname prefix-pathname
				       (make-pathname :name *cme-descriptor-filename-prefix*
						      :type "PYRAMID")))
      (case-insensitive-probe
       (merge-directories-and-pathname prefix-pathname
				       (make-pathname :name "CME-DESC"
						      :type "PYR"))) ;; Frigging windows.
      ))
					      
  
(defun image-pyramid-pathnames (prefix-pathname)
  (when prefix-pathname (setq prefix-pathname (ev-pathname-translate prefix-pathname)))
  (let ((descr-path  (find-pyramid-file prefix-pathname)
	  #+never
	  (format nil "~a/~a.PYRAMID"
		  prefix-pathname *cme-descriptor-filename-prefix*)
	  ))
    ;;(format t "image-pyramid-pathnames ~a ~a~%" prefix-pathname descr-path)
    (when (and descr-path (probe-file descr-path))
      (multiple-value-bind (description creation-form)
	  (read-image-pyramid-descr-file descr-path)
	(ignore description)
        (loop for path in (eval (getf (cddr creation-form) :pathnames))
               for full-path = (if prefix-pathname
                                   (merge-directories-and-pathname prefix-pathname path)
                                   ;;(format nil "~a/~a" prefix-pathname path)
                                   path)
               collect full-path)
        ))))

(defmethod pyramid-directory-p ((thing string))
  (and (directory-p thing)
       (let ((descr-path
	      (merge-directories-and-pathname 
	       thing
	       (make-pathname :name img::*cme-descriptor-filename-prefix* :type "PYRAMID"))))
	 (when (probe-file descr-path)
	   thing))))

(defun image-pyramid-loadable-p (prefix-pathname)
  (loop for path in (image-pyramid-pathnames prefix-pathname)
	thereis (probe-file path)))


(defmethod pyramid-descriptor-p ((thing string))
  (and (equal (pathname-name thing) "CME-DESCR")
       (equal (pathname-type thing) "PYRAMID")))


;;; This looks totally bogus: IMAGE-TO-2D-TRANSFORM-PYRAMID-LEVEL is called with an image rather
;;; than a image-to-2d-transform.  
#+failed
(defun image-to-2d-transform-pyramid-level (image-to-2d-transform)
  (and image-to-2d-transform
       ;; This is only needed when LOAD-IMAGE-PYRAMID is called with
       ;; :TOP-IMAGE-TO-2D-TRANSFORM keyword specified.
       (let* ((mat (transform-matrix image-to-2d-transform))
	      (level (round (log (math::euclidean-length (aref mat 0 0) (aref mat 0 1)) 2.0))))
	 level)))

(defun image-to-2d-transform-pyramid-level (image-to-2d-transform)
  (and image-to-2d-transform
       ;; This is only needed when LOAD-IMAGE-PYRAMID is called with
       ;; :TOP-IMAGE-TO-2D-TRANSFORM keyword specified.
       (let ((mat (transform-matrix image-to-2d-transform)))
	 (round (log (math::euclidean-length (aref mat 0 0) (aref mat 0 1)) 2.0)))))


(defun image-pathname-pyramid-level (image-path)
  (loop with path-string = (namestring image-path) ;(format nil "~a" image-path)
	for pos from (1- (length path-string)) downto 0
	while (digit-char-p (aref path-string pos))
	finally (return (read-from-string (subseq path-string (1+ pos))))))

(defparameter *cme-descriptor-filename-prefix* "CME-DESCR")
		
;;;
;;; Some 2d-worlds (e.g., those in the Ft. Irwin site) are not 1:1
;;; with the top-image, i.e. they don't use pixels as the units.  This
;;; fixes it, although I don't understand the purpose of the assoc et
;;; al. in Lynn's code.  Fri Feb 5 1999 heller (cribbed from
;;; image-calc 2/11/01 - cc)

;;; Sun Mar 17 2002 LHQ: I had the idea that image-pyramids in the file system
;;; could become "depopulated" by deleting the highest resolution levels
;;; in order to save disk space.  Thus the starting level number would not be
;;; zero.  The assoc used in MAKE-IMAGE-PYRAMID-LEVEL-LIST deals with the
;;; problem of missing levels, ie, the level list will have NILs where images
;;; are missing.  Maybe this is a bad idea. 

(defun make-image-pyramid-level-list (path-list image-list)
  (loop for path in path-list
	for image in image-list
	for level-number = (or (image-to-2d-transform-pyramid-level image)
			       ;; If image-to-2d-transform doesn't exist we really need to
			       ;; add it to the image.
			       (image-pathname-pyramid-level path))
	collect (list level-number image path) into list
	finally (let ((sorted-images (print (sort list #'< :key #'car))))
		  (return #+never
			  (loop for level from 0 to (car (car (last sorted-images)))
				collect (cadr (assoc level sorted-images)))
			  (loop for (level image path) in sorted-images
				collect image)
			  ))))



(defun load-image-pyramid (prefix-pathname &key
			   pathnames
			   wildcard-spec
			   load-image-args
			   TOP-IMAGE-TO-2D-TRANSFORM
			   &allow-other-keys)
  (when prefix-pathname 
    (setq prefix-pathname (ev-pathname-translate prefix-pathname))
    (setq prefix-pathname (pathname-as-directory prefix-pathname))
    )
  
  (if (and (null pathnames) (null wildcard-spec))
      (multiple-value-bind (description creation-form)
	  (read-image-pyramid-descr-file
	   (find-pyramid-file prefix-pathname)
	   #+never
	   (format nil "~a/~a.PYRAMID"
		   prefix-pathname *cme-descriptor-filename-prefix*))
	  
	(ignore description)
	(if creation-form
	    (let ((*path* prefix-pathname))
	      (declare (special *path*))
              (format t "~%Creation form: ~a" creation-form)
	      (eval creation-form))
	    (error "Illegal Pyramid Descriptor File ~a" prefix-pathname)))

      
      (let* ((pathnames (or pathnames
			    (wildcard-search-for-pyramid-files
			     (merge-directories-and-pathname prefix-pathname wildcard-spec)
			     ;;(merge-pathnames wildcard-spec prefix-pathname)
			     ;;(format nil "~a/~a" prefix-pathname wildcard-spec)
			     )))
	     (pyramid (make-image-pyramid))
	     base-image)
	(loop for path in pathnames
	      ;; LHQ Sun Mar 17 2002: cc apparently added the 0 in 
	      ;;   (or 0 (image-to-2d-transform-pyramid-level ...))
	      ;; This breaks things for depopulated pryamids
	      for level from (or 0 (image-to-2d-transform-pyramid-level
					   TOP-IMAGE-TO-2D-TRANSFORM) 0)
	      for full-path = (merge-pathnames path
					       (lx::pathname-as-directory prefix-pathname))
	      for image = (and (probe-file full-path)
			       (apply 'load-image full-path load-image-args))
	      when image
		do (when (null base-image)
		     (setq base-image image)
		     (unless (= level 0)
		       (error "1st image in pyramid not at level 0"))
		     )
		   (create-pyramid-level pyramid level image)
	      collect image)
        ;;
        ;; Sometimes the image-to-2d-transform is nontrivial.  Let's use it:
        (when top-image-to-2d-transform
          (setf (image-to-2d-transform base-image) top-image-to-2d-transform))

	(set-image-to-2d-transforms pyramid)
	#+no-longer-needed
	(setf (slot-value pyramid 'level-constructor)
	      #'(lambda(level) (get-image-pyramid-level base-image level t)))
	base-image)))



(defmethod top-of-image-hierarchy (image)
  (top-of-image-pyramid image))

;;; This has a problem wrt image-mapping.  We want to be able to create a window at the top of the
;;; pyramid if given a window of a lower level.  We do not know the scale to use until we

(defmethod top-of-image-pyramid (image)
  (let ((pyramid (get-prop image :pyramid)))
    (if (null pyramid)
	image
	(with-class-slots image-pyramid (levels) pyramid
	  (aref levels 0)))))


(def-eval-cache-search-rule top-of-image-pyramid (form)
  (non-scaling-image-window-commuter form 1))

;;; This really sucks -- conses approx. 220848 bytes
(defun get-image-pyramid-level (img level &optional
				(instantiate t)
				(pyramid-type *IMAGE_PYRAMID_DEFAULT_type*))
  (declare (ignore  pyramid-type))
  (pyramid-level img level instantiate))

(def-eval-cache-search-rule get-image-pyramid-level (form)
  (let ((level (caddr form)))
    (non-scaling-image-window-commuter form (if (<= level 0) 1 (/ 1 (lx::2^ level))))))

(defmethod image-pyramid-level (image)
  (let ((top (top-of-image-pyramid image)))
    (log2 (floor (image-x-dim top) (image-x-dim image)))))

#|
(defmethod-cached zoom-in ((image image) &rest args)
  (declare (ignore args))
  (get-image-pyramid-level image (1- (image-pyramid-level image)) nil))

(defmethod-cached zoom-out ((image image) &rest args)
  (declare (ignore args))
  (get-image-pyramid-level image (1+ (image-pyramid-level image)) t)) 

(def-eval-cache-search-rule zoom-in (form)
  (non-scaling-image-window-commuter form 2 ))

(def-eval-cache-search-rule zoom-out (form)
  (non-scaling-image-window-commuter form 1/2))
|#

(defmethod zoom-in ((image image) &rest args)
  (declare (ignore args))
  (get-image-pyramid-level image (1- (image-pyramid-level image)) nil))

(defmethod zoom-out ((image image) &rest args)
  (declare (ignore args))
  (get-image-pyramid-level image (1+ (image-pyramid-level image)) t)) 
