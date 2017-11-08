(IN-PACKAGE "CME")

#| Site Glue API

generic functions

(site-list site-glue)

(load-site-glue site-name site-glue)
  needed before doing anything else with site

(load-3d-world site-name site-glue)

(generic-load-site site-name site-glue ...)

(get-2d-world-names 3d-world site-glue)

;;; 2d-world site-glue methods

(get-3d-to-2d-projection 2d-world site-glue)

(load-property-list 2d-world site-glue)

(save-3d-to-2d-projection 2d-world site-glue &optional path)

(site-2d-world-pyramid-names 2d-world site-glue)

(load-2d-world-image-pyramid 2d-world pyramid-name site-glue)

(feature-set-names site-glue site-glue)
(feature-set-pathnames world site-glue)
|#


;;; Flush site and pyramid info.
(defun flush-all-site-caches ()
  (eval-cache-flush-function 'load-2d-world-image-pyramid)
  (eval-cache-flush-function 'load-3d-world)
  (eval-cache-flush-function 'load-site-glue)
  (eval-cache-flush-function 'site-2d-world-pyramid-loadable-p)
  )

(defparameter *lisp-mode-file-header* (format nil ";;; -*- Lisp -*-~%~%"))

  
(defmethod site-glue ((3d-world 3d-world))
  (or (get-prop 3d-world :site-glue)
      ))

(defmethod site-glue ((2d-world 2d-world))
  (let ((3d-world (3d-world 2d-world)))
    (when 3d-world (site-glue (3d-world 2d-world)))))

(defmethod site-path ((3d-world 3d-world) (site-glue basic-site-glue))
  (get-prop 3d-world :site-path)
  )

(defmethod site-path ((site-name string) (site-glue radius-site-glue))
  (dir-relative-path (sites-path site-glue) site-name)
  ;;(format nil "~a/~a" (sites-path site-glue) site-name)
  )

(defmethod site-path ((site-name string) (site-glue (eql nil)))
  (gui::freedius-warning t "~%The site named ~a has no recognizable site glue." site-name)
  )

(defmethod site-path ((3d-world 3d-world) (site-glue radius-site-glue))
  (or (get-prop 3d-world :site-path)
      (site-path (site-glue-world-name 3d-world site-glue) site-glue)))


;;;(defmethod make-site-glue-structures ((3d-world 3d-world) (site-glue radius-site-glue2))
;;;  (let* (;;(site-name (site-glue-world-name 3d-world site-glue))
;;;         (site-path (site-path 3d-world site-glue))
;;;         (glue-path (format nil "~a/glue.lisp" site-path)))
;;;    (create-directory site-path)
;;;    (create-directory (format nil "~a/camera-models" site-path))
;;;    (create-directory (format nil "~a/FeatureSets" site-path))
;;;    (unless (probe-file glue-path)
;;;      (with-open-file (st glue-path :direction :output)
;;;        (format st "~s~%~%" '(in-package "CME"))
;;;        (pprint `(setq *loaded-site-glue* ,(fasd-form site-glue)) st)))))

(defmethod make-site-glue-structures ((3d-world 3d-world) (site-glue radius-site-glue2))
  (let* (;;(site-name (site-glue-world-name 3d-world site-glue))
	 (site-path (site-path 3d-world site-glue))
	 (glue-path (dir-relative-path site-path "glue.lisp")))
    (create-directory site-path)
    (create-directory (dir-relative-path site-path "camera-models"))
    (create-directory (dir-relative-path site-path "FeatureSets"))
    (unless (probe-file glue-path)
      (with-open-file (st glue-path :direction :output)
	(format st "~s~%~%" '(in-package "CME"))
	(pprint `(setq *loaded-site-glue* ,(fasd-form site-glue)) st)))))

(defmethod make-site-glue-structures ((2d-world 2d-world) (site-glue radius-site-glue2))
  (let* ((2d-world-name (site-glue-world-name 2d-world site-glue))
	 (2d-world-path (2d-world-path 2d-world-name site-glue)))
    (create-directory 2d-world-path)
    ))

(defun string-last-word (string)
  (let ((pos (position #\space string :from-end t)))
    (if pos
	(substring string (1+ pos))
	string)))

(defmethod site-glue-world-name ((world 2d-world) (site-glue radius-site-glue))
  (string-last-word (name world)))

(defmethod site-glue-world-name ((world 3d-world) (site-glue radius-site-glue))
  (string-last-word (get-prop world :site-name)))

(defmethod 2d-world-path ((2d-world 2d-world) (site-glue radius-site-glue))
  ;;(format nil "~a/~a" (2d-worlds-path site-glue) (string-last-word (name 2d-world)))
  (2d-world-path (site-glue-world-name 2d-world site-glue) site-glue)
  ;;(get-prop 2d-world :2d-path)
  )

(defmethod 2d-world-path ((2d-world string) (site-glue radius-site-glue))
  (dir-relative-path (2d-worlds-path site-glue) 2d-world)
  ;;(format nil "~a/~a" (2d-worlds-path site-glue) 2d-world)
  ;;(get-prop 2d-world :2d-path)
  )


;; this is old stuff from CME-5
(defmethod get-2d-world-names ((3d-world 3d-world) (site-glue radius-site-glue))
  (let ((2d-world-names
	 (with-open-file (st (dir-relative-path (site-path 3d-world site-glue) "2d-worlds")
			  ;;(format nil "~a/2d-worlds" (site-path 3d-world site-glue))
			     )
	   (loop for 2d-world-name = (read-line st nil nil)
		 while 2d-world-name
		 collect 2d-world-name))))
    (setf (get-prop 3d-world :2d-world-names) 2d-world-names)
    2d-world-names))


(defmethod site-path :around (3d-world (glue radius-site-glue2))
  (with-slots (site-path) glue
    (or site-path
	(call-next-method))))

(defmethod initialize-instance :after ((glue radius-site-glue2) &key &allow-other-keys)
  (with-slots (2d-worlds-path site-2d-worlds-path site-path) glue
    (unless site-2d-worlds-path
      (setf site-2d-worlds-path
	    (dir-relative-path site-path "images")
	    ;;(format nil "~a/images" site-path)
	    ))))

(defmethod get-2d-world-names ((3d-world-name string) (site-glue radius-site-glue2))
  (let* ((2d-world-paths
	  (directory-list-filter (directory
				  (dir-relative-path (site-2d-worlds-path site-glue) "*")
				  ;;(format nil "~a/*" (site-2d-worlds-path site-glue))
				  #+cmu :check-for-subdirs #+cmu nil)))
	 (2d-world-names (loop for path in 2d-world-paths collect (pathname-name path)))
	 )
    2d-world-names))

(defmethod get-2d-world-names  ((3d-world 3d-world) (site-glue radius-site-glue2))
  (setf (get-prop 3d-world :2d-world-names)
	(get-2d-world-names (site-glue-world-name 3d-world site-glue) site-glue)))

#|
(dir-relative-path (sites-path *radius-site-glue*) "*")
(dir-relative-path (sites-path (car cme::*site-glue-list*)) "*")
(directory (dir-relative-path (sites-path (car cme::*site-glue-list*)) "*"))
(dir-relative-path "/opt/IU/radius/sites/ft-irwin-1989" (3d-world-file-name (car cme::*site-glue-list*)) )
(dir-relative-path "/m/opt/IU/radius/sites/alv/" (3d-world-file-name (car cme::*site-glue-list*)))
(3d-world-file-name (car cme::*site-glue-list*))
(dir-relative-path (sites-path *radius-site-glue*) "*")
(directory-p "/m/opt/IU/radius/sites/alv/" :follow-links t)
(probe-file (dir-relative-path "/m/opt/IU/radius/sites/alv/" (3d-world-file-name (car cme::*site-glue-list*))))
(site-name-glue-alist *radius-site-glue*)
(ev-pathnames::old-directory )
(get-2d-world-names "ALV" *radius-site-glue2*)

|#

;;;(defmethod site-name-glue-alist ((site-glue radius-site-glue))
;;;  (loop for path in (directory (dir-relative-path (sites-path site-glue) "*")
;;;                               ;;(format nil "~a/*" (sites-path site-glue))
;;;                               #+cmu :check-for-subdirs #+cmu nil
;;;                               )
;;;        when (and (directory-p (truename path) :follow-links t)
;;;                  (not (unix-dot-directory-p path))
;;;                  (probe-file
;;;                   (dir-relative-path path (3d-world-file-name site-glue))))
;;;          collect (list (pathname-name path) site-glue )))
  
(defmethod site-name-glue-alist ((site-glue radius-site-glue))
  (loop for path0 in
	#-cmu (directory (dir-relative-path (sites-path site-glue) "*"))
	#+cmu (directory (dir-relative-path (sites-path site-glue) "*") 
			 :check-for-subdirs nil :truenamep nil)
	for path = (pathname-as-file path0)
	;; site-path is usually <site-dir>/3d-world.lisp
	for site-path = (dir-relative-path path (3d-world-file-name site-glue))
	for broken-for-freedius-path = (dir-relative-path path "SITE-BROKEN-FOR-FREEDIUS")
	when (and (directory-p path :follow-links t)
		  (not (unix-dot-directory-p path))
		  (probe-file site-path)
		  (not (probe-file broken-for-freedius-path))
		  )
	  collect (list (pathname-name path) site-glue )
	#+never (let* ((site-glue2 (load-site-glue site-path))
		       (3d-world (load-3d-world path site-glue2)))
		  (list (pathname-name path) site-glue ))
	))



(defmethod site-name-glue-alist (site-glue-list)
  (loop for site-glue in site-glue-list
	append (site-name-glue-alist site-glue)))

;;(directory (dir-relative-path "$RADIUS/sites" "*"))

(defmethod site-list (glue)
  (loop for (site-name) in (site-name-glue-alist glue)
	collect site-name))
;(site-list *radius-site-glue*)

(defparameter *2d-world-property-list-filename* "property-list.lisp")

(defmethod property-list-pathname ((2d-world 2d-world) (site-glue radius-site-glue))
  (dir-relative-path (2d-world-path 2d-world site-glue)
		     *2d-world-property-list-filename*)
  ;;(format nil "~a/~a" (2d-world-path 2d-world site-glue) *2d-world-property-list-filename*)
  )

(defmethod load-property-list ((2d-world 2d-world) (site-glue radius-site-glue))
  (let ((path (property-list-pathname 2d-world site-glue)))
    (when (probe-file path)
      (with-open-file (st path )
	(loop with last-read-value
	      with eof-flg = (list)
	      for form = (read st nil eof-flg)
	      when (eq form eof-flg)
		return last-read-value
	      do (setq last-read-value (eval form)))))))

(defparameter *2d-world-properties-not-to-dump* '(:2D-BUCKET-SIZE :2D-BBOX))

(defmethod save-property-list ((2d-world 2d-world) (site-glue radius-site-glue))
  (let ((path (property-list-pathname 2d-world site-glue)))
    (backup-file-by-copy path)
    (with-open-file (st path :direction  :output)
      (format st "(IN-PACKAGE \"CME\")~%~%")
      (pprint-key-val-pairs
       `(list ., (loop for (key val) on (property-list 2d-world) by #'cddr
		       unless (memq key *2d-world-properties-not-to-dump*)
			 collect key 
			 and collect (fasd-form val)))
       :stream st :n-initial-elements 1)
      (format st "~%"))))

#|
(save-property-list (2d-world (top-view)) (site-glue (2d-world (top-view))))
(load-property-list (2d-world (top-view)) (site-glue (2d-world (top-view))))
|#



(defmethod get-3d-to-2d-projection ((2d-world 2d-world) (site-glue t))
  (let ((*make-perspective-transform-default-2d-world* 2d-world))
    (get-3d-to-2d-projection 2d-world (site-glue 2d-world))))


;;;(defmethod site-camera-model-pathname ((2d-world 2d-world) (site-glue radius-site-glue))
;;;  (format nil "~a/camera-models/~a"
;;;          (site-path (3d-world 2d-world) site-glue)
;;;          (site-glue-world-name 2d-world site-glue)))

(defmethod site-camera-model-pathname ((2d-world 2d-world) (site-glue radius-site-glue))
  (dir-relative-path
   (site-path (3d-world 2d-world) site-glue)
   "camera-models"
   (site-glue-world-name 2d-world site-glue)))

;;; need to really decide where cameras models live, and where adjustments to camera models live.
(defmethod site-camera-model-pathname ((2d-world 2d-world) (site-glue radius-site-glue2))
  ;;(format nil "~a/camera-model" (2d-world-path 2d-world site-glue))
  (dir-relative-path (2d-world-path 2d-world site-glue) "camera-model")
  )

(defparameter *load-3d-to-2d-projection-compose-transforms* t)

;;; This is also defined in load-cme-objects.lisp
(defun compose-3d-transform-with-2d-world-projection (transform &optional (2d-world *2d-world* ))
  (declare (special *2d-world*))
  (let ((projection (if *load-3d-to-2d-projection-compose-transforms*
			(compose-transforms transform (3d-to-2d-projection 2d-world))
			(make-composite-coordinate-projection
			 (list transform (3d-to-2d-projection 2d-world))))))
    (when (and (not (consp transform)) (get-prop transform :covariance-model))
      (setf (get-prop projection :covariance-model) (get-prop transform :covariance-model))
      (rem-prop transform :covariance-model))
    projection))

;;; This needs to be flushed, but there are many files in RADIUS/sites/xxx/camera-models/ that
;;; need to be modified first.
(defun compose-transform-with-projection (transform)
  (compose-3d-transform-with-2d-world-projection transform))

#|
;;; the camera-model file should contain something of the form:


      (make-4x4-coordinate-projection ...)

or
      (load-bip-file)

or 
      (make-composite-coordinate-projection
       (list (make-4x4-coordinate-transform ...)
	     (3d-to-2d-projection *2d-world*)))

or    (compose-3d-transform-with-2d-world-projection
         (make-4x4-coordinate-transform ...))


|#

(defun make-site-2d-world-named (2d-world-name 3d-world
					       &optional
					       (site-glue (site-glue 3d-world)))
  (or (get-2d-world-named 2d-world-name (list 3d-world))
      (let ((2d-world (make-instance  ;;make-cached-instance
		       '2d-world
		       :name 2d-world-name
		       :3d-world 3d-world
		      ;; :world 3d-world
		       )))
	(loop for (key val) on (load-property-list 2d-world site-glue) by #'cddr
	      do (setf (get-prop 2d-world key) val))
	;; Save this information - useful for some old sites like ALV:
	(setf (get-prop 2d-world :3d-world) 3d-world)
	2d-world)))

(defun get-image-2d-world-name (image)
  (or (get-prop image :2d-world-name)
      (let ((2d-world (get-prop image :2d-world)))
	(when 2d-world
	  ;;(setf (get-prop image :2d-world) nil (3d-to-2d-projection 2d-world) nil)
	  (name 2d-world)))))

(defmethod load-site-image (image-path 3d-world (site-glue basic-site-glue)
				     &key 2d-world-name)
  (let* ((image (load-image image-path))
	 (2d-world-name (or 2d-world-name (get-image-2d-world-name image)))
	 (2d-world (make-site-2d-world-named 2d-world-name 3d-world site-glue))
	 )
    (add-image-to-world 2d-world image)
    (setup-image-worlds image :2d-world 2d-world)
    (get-3d-to-2d-projection 2d-world site-glue)
    image))


(defun set-image-pyramid-labels (pyramid-base image-label)
  (loop for level from 0
	for image = (get-image-pyramid-level pyramid-base level nil)
	while image
	do (setf (image-prop image :name) (format nil "~a.g~a" image-label level)
		 )))

(defmethod pyramid-path (2d-world pyramid-name (site-glue radius-site-glue))
  (let ((2d-world-path (2d-world-path 2d-world site-glue)))
    (if pyramid-name
	;;(format nil "~a/~a" 2d-world-path pyramid-name)
	(dir-relative-path 2d-world-path pyramid-name)
	2d-world-path)))

(defparameter *load-2d-world-image-pyramid-verbose* nil)
#|
(eval-cache-flush-function 'load-2d-world-image-pyramid)
|#

(defmethod purge-2d-world-pyramids (2d-world-name (site-glue basic-site-glue)
						  &rest args
						  &key to-level size-threshold do-deletions)
  (ignore to-level size-threshold do-deletions)
  (loop for pyramid-name in (site-2d-world-pyramid-names 2d-world-name site-glue)
	for pyramid-path = (pyramid-path 2d-world-name pyramid-name site-glue)
	append (apply 'purge-image-pyramid-levels pyramid-path args)
	))

(defmethod purge-site-image-pyramids ((3d-world 3d-world) (site-glue basic-site-glue)
				      &rest args &key to-level size-threshold do-deletions)
  (ignore to-level size-threshold do-deletions)
  (loop for 2d-world-name in (get-2d-world-names 3d-world site-glue)
	append (apply 'purge-2d-world-pyramids 2d-world-name site-glue args)))

#|
(purge-site-image-pyramids (3d-world (top-view)) (site-glue (3d-world (top-view))) :size-threshold 4e6)
(purge-2d-world-pyramids "4-7" (site-glue (3d-world (top-view))) :size-threshold 4e6)
(pyramid-path "4-7" nil (site-glue (3d-world (top-view))))
(site-2d-world-pyramid-names "4-7" (site-glue (3d-world (top-view))))
|#

				    
(defmethod load-2d-world-image-pyramid
	   ((2d-world 2d-world)
	    pyramid-name
	    (site-glue basic-site-glue)
	    &key &allow-other-keys)
  (eval-cache (load-2d-world-image-pyramid 2d-world pyramid-name)
      (let* ((2d-world-name (site-glue-world-name 2d-world site-glue))
	     (pyramid-path (pyramid-path 2d-world pyramid-name site-glue))
	     pyramid-base
	     (verbose *load-2d-world-image-pyramid-verbose*)
	     )
	(when verbose (format t ";;; Loading Image Pyramid ~a ~a ... " 2d-world-name (or pyramid-name "")))
	(setq pyramid-base (load-image-pyramid pyramid-path))
	(when verbose (format t "  Done.~%"))
    
	(loop for level from 0
	      for image = (get-image-pyramid-level pyramid-base level nil)
	      while image
		do (setf (get-prop image :name) pyramid-name) ;; is this needed?
		   (add-image-to-world 2d-world image))
	;; oops -- this next will remove 2d-world from the children of its 3d-world.
	;; Apparently this is needed to force reloading the projection.
	#+never   ; FIXME -- is removing this correct?
	(setf (3d-to-2d-projection 2d-world) nil) 
	(setup-image-worlds pyramid-base :2d-world 2d-world)
	(get-3d-to-2d-projection 2d-world site-glue)
	(set-image-pyramid-labels pyramid-base
				  (if pyramid-name
				      (format nil "~a-~a" 2d-world-name pyramid-name)
				      2d-world-name))
	pyramid-base)))

(defmethod load-2d-world-image-pyramid
    ((2d-world-name string)
     pyramid-name
     (site-glue basic-site-glue)
     &key 3d-world &allow-other-keys)
  (load-2d-world-image-pyramid (make-site-2d-world-named 2d-world-name 3d-world site-glue)
			   pyramid-name
			   site-glue))


;;; (eval-cache-flush-function 'site-2d-world-pyramid-loadable-p)
(defmethod-cached site-2d-world-pyramid-loadable-p ((site-glue radius-site-glue) pyramid-path)
  (image-pyramid-loadable-p pyramid-path))

(defmethod site-2d-world-pyramid-names (2d-world-name (site-glue radius-site-glue))
  ;;(format t "radius-2d-world-pyramid-names ~a~%" 2d-world-path)
  (flet ((loadable-pyramid-p (dir-path)
	   (site-2d-world-pyramid-loadable-p site-glue dir-path)))
  
    (let* ((2d-world-path (2d-world-path 2d-world-name site-glue))
	   (subdir-paths (loop for path in (directory
					     (dir-relative-path 2d-world-path "*")
					     ;;(format nil "~a/*" 2d-world-path)
					     #+cmu :check-for-subdirs #+cmu nil
					     )
			       when (and (directory-p path)
					 (not (unix-dot-directory-p path))
					 (loadable-pyramid-p path)
					 )
				 collect (pathname-immediate-directory-name path))))
      (if (loadable-pyramid-p 2d-world-path)
	  (list* nil subdir-paths)
	  subdir-paths))))

(defmethod site-pyramid-names ((3d-world 3d-world) (site-glue basic-site-glue))
  (loop for 2d-world-name in (get-2d-world-names 3d-world site-glue)
	append (loop for pyramid-name
		     in (site-2d-world-pyramid-names 2d-world-name site-glue)
		     collect (list 2d-world-name pyramid-name))))


(defmethod load-site-2d-world-pyramids (2d-world-name
					3d-world
					(site-glue basic-site-glue)
					&key (load-pyramids t))
  (let* ((2d-world (make-site-2d-world-named 2d-world-name 3d-world site-glue))
	 (pyramid-names (cond ((consp load-pyramids)
			       load-pyramids)
			      (load-pyramids (site-2d-world-pyramid-names 2d-world-name site-glue)))))
    ;;(setq *foo* (list 2d-world-name 3d-world 2d-world pyramid-names site-glue))
    (loop for pyramid-name in pyramid-names
	  collect (load-2d-world-image-pyramid 2d-world pyramid-name site-glue))))


;;;
;;; Note: CMUCL is unhappy if you feed it a versioned file with no
;;; type.  For example, in my local copy of the SRI site, there were
;;; edited files like "Airfields.~1~" that caused CMUCL heartburn.  I
;;; moved these elsewhere, but perhaps there's a better workaround...
;;;

(defgeneric  feature-set-pathnames (world site-glue)
  (:argument-precedence-order site-glue world))

(defmethod feature-set-pathnames ((3d-world 3d-world) (site-glue radius-site-glue))
  (loop with site-path = (site-path 3d-world site-glue)
	for fs-path in (feature-set-paths site-glue)
	collect (dir-relative-path site-path fs-path)
	;;(format nil "~a/~A" site-path fs-path)
	))

(defmethod feature-set-file-p (pathname (site-glue radius-site-glue2))
  (ignore pathname)
  (not (directory-p pathname)))

;;; It isn't at all clear that any feature sets except in top level models directory should be loaded.
(defmethod feature-set-pathnames (world (site-glue radius-site-glue2))
  (let* ((models-dir-path (feature-set-pathname "" world site-glue)))
    (when (probe-file models-dir-path)
      (let* ((dir-path (feature-set-pathname "" world site-glue))
	     (feature-set-paths (directory-list-filter 
				 #+cmu (directory dir-path :truenamep nil)
				 ;;#+sbcl (directory (format nil "~a*.*" dir-path) )
				 ;;#+allegro (directory dir-path))))
				 #-cmu (directory (pathname-as-directory dir-path)))))
	(setq *foo* (list dir-path feature-set-paths))
	(loop for path in feature-set-paths
	      when (feature-set-file-p path site-glue)
		collect path)))))
#|
(setq *foo* nil)
(feature-set-pathnames (get-3d-world-named "ALV") *radius-site-glue2*)
(feature-set-pathname "" (get-3d-world-named "ALV") common-lisp-user::*alv-site-glue*)
(directory-list-filter (directory (format nil "~a*" (feature-set-pathname "" (get-3d-world-named "ALV") common-lisp-user::*alv-site-glue*))))
(feature-set-file-p (nth 0 (cadr *foo*)) common-lisp-user::*alv-site-glue*)
(directory "/opt/IU/radius/sites/alv/models/*")
|#

(defmethod feature-set-name-alist (world (site-glue radius-site-glue2))
  (loop for pathname in (feature-set-pathnames world site-glue)
	for path = (namestring pathname)
	with dir-path = (feature-set-pathname "" world site-glue)
	with prefix-length = (length dir-path)
	collect (list (if (string-equal dir-path path :end2 prefix-length)
			  (substring path prefix-length)
			  path)
		      path)))

(defmethod feature-set-names (world (site-glue radius-site-glue2))
  (loop for (name) in (feature-set-name-alist world site-glue)
	collect name))


(defmethod feature-set-pathname (fs-name (3d-world 3d-world) (site-glue radius-site-glue2))
  (dir-relative-path (site-path 3d-world site-glue)
		     (feature-set-subdir-name site-glue)
		     fs-name)  
  ;;(format nil "~a/~a/~a" (site-path 3d-world site-glue) (feature-set-subdir-name site-glue) fs-name)
  )

;;;(defmethod feature-set-pathname (fs-name (2d-world 2d-world) (site-glue radius-site-glue2))
;;;  (format nil "~a/~a/~a/~a"
;;;          (site-2d-worlds-path site-glue)
;;;          (site-glue-world-name 2d-world site-glue)
;;;          (feature-set-subdir-name site-glue)
;;;          fs-name
;;;          ))

(defmethod feature-set-pathname (fs-name (2d-world 2d-world) (site-glue radius-site-glue2))
  (dir-relative-path (site-2d-worlds-path site-glue)
		     (site-glue-world-name 2d-world site-glue)
		     (feature-set-subdir-name site-glue)
		     fs-name))

;;(feature-set-pathname "foo" (2d-world (top-view)) (get-prop (3d-world (top-view)) :site-glue))

(defmethod load-site-feature-sets (world (site-glue basic-site-glue))
  (loop for path in (feature-set-pathnames world site-glue)
	do (load-feature-sets path)))

(defmethod world-name ((world 3d-world))
  (get-prop world :site-name))

#|
(eval-cache-flush-function  'load-3d-world)
(eval-cache-flush-function 'load-site-glue)
(get 'load-3d-world :function-cache)
|#

(declaim (special *radius-site-glue* *site-glue*))

(defstruct-class lvcs-coordinate-system (transforms::local-vertical-coordinate-system) ())


;;; (undefmethod '(load-3d-world (t radius-site-glue)))
(defmethod-cached load-3d-world (3d-world-name site-glue)
  (let* ((site-glue (or  (load-site-glue 3d-world-name (or site-glue *radius-site-glue*))))
	 (*site-glue* site-glue)
	 *loaded-3d-world*
	 (3d-world-path
	  (dir-relative-path (site-path 3d-world-name site-glue)
			     (3d-world-file-name site-glue))
	  #+never
	   (format nil "~a/~a"
				(site-path 3d-world-name site-glue)
				(3d-world-file-name site-glue)))
	 )
    (declare (special *site-glue* *loaded-3d-world*))
    (load 3d-world-path)
    (unless *loaded-3d-world*
      (error "*loaded-3d-world* not set by 3d-world-file ~a" 3d-world-path))
    (setf (get-prop *loaded-3d-world* :site-name) 3d-world-name)
    (setf (get-prop *loaded-3d-world* :site-glue) site-glue)
    *loaded-3d-world*))

#|
(load-site-glue "Tokyo" *radius-site-glue*)
(load-site-glue "alv" *radius-site-glue*)
(load-site-glue "Tokyo" (nth 2 *site-glue-list*))
(site-glue (get-3d-world-named "Tokyo"))
(site-glue (load-3d-world "Tokyo" (nth 2 *site-glue-list*)))
(progn *glue-path*)
(site-path "alv" *radius-site-glue*)
(eval-cache-flush-function 'load-site-glue)

|#

(defmethod-cached load-site-glue (3d-world-name (site-glue radius-site-glue))
  (let* ((glue-path
	  (dir-relative-path (site-path 3d-world-name site-glue)
			     (glue-file-name site-glue))
	   #+never (format nil "~a/~a"
			   (site-path 3d-world-name site-glue)
			   (glue-file-name site-glue)))
	 (*site-glue* site-glue))
    (when (probe-file glue-path)
      (let ((*loaded-site-glue* nil))
	(declare (special *loaded-site-glue*))
	(let ((*package* (find-package 'cme))) (load glue-path))
	*loaded-site-glue*))))

(defmethod-cached load-site-glue ((3d-world-name pathname) (site-glue radius-site-glue))
  (let* ((glue-path
	  (dir-relative-path 3d-world-name (glue-file-name site-glue)))
	 (*site-glue* site-glue))
    (when (probe-file glue-path)
      (let ((*loaded-site-glue* nil))
	(declare (special *loaded-site-glue*))
	(let ((*package* (find-package 'cme))) (load glue-path))
        *loaded-site-glue*))))
	
(defmethod-cached load-site-glue (3d-world-name (site-glue freedius-site-glue))
  (print 3d-world-name)
  (autoglue-site 3d-world-name))

;;;
;;; In the event that we do not want menus:
;;;
(defvar *default-load-pyramids-value* :menu)

;;; when LOAD-PYRAMIDS is a list, each element is either a 2d-world-name meaning to load all
;;; pyramids of the 2d-world, or a list of (2d-world-name . pyramid-names).

(defmethod generic-load-site (site-name
			      (site-glue basic-site-glue)
			      &rest args
			      &key
			      (load-pyramids :menu)
			      pyramid-filter
			      frame)
  (ignore load-pyramids pyramid-filter frame)
  (setq site-glue (or (load-site-glue site-name site-glue)
		      site-glue))
  (let ((3d-world (load-3d-world site-name site-glue)))
    (setf (get-prop 3d-world :site-glue) site-glue)
    (apply 'generic-load-site-part-2
	   site-name 3d-world
	   (get-prop 3d-world :site-glue) ; use the site-glue specified by the 3d world
	   args)))

;;; 
;;; ?? This whole glue thing seems a little off.  Ideally, we would
;;; want a sensible scheme that maintains some backward compatibility
;;; with old CME sites, but allows a more minimal and portable site
;;; specification method (radius-site-glue wants to see glue files and
;;; explicit pathnames).
;;;
(defmethod generic-load-site (site-name
			      (site-glue list)
			      &rest args
                              &key frame load-pyramids pyramid-filter
                              &allow-other-keys
                              )
  (loop for glue in site-glue
        when (site-loadable-p glue site-name)
          return (funcall 'generic-load-site site-name glue
                          :frame frame
                          :load-pyramids load-pyramids
                          :pyramid-filter pyramid-filter
                        args)))



#+never
(defmethod generic-load-site (site-name
			      (site-glue freedius-site-glue)
			      &rest args
			      &key
			      (load-pyramids :menu)
			      pyramid-filter
			      frame)
  (ignore load-pyramids pyramid-filter frame)
  (setq site-glue (or (load-site-glue site-name site-glue)
		      site-glue))
  (let ((3d-world (load-3d-world site-name site-glue)))
    (setf (get-prop 3d-world :site-glue) site-glue)
    (apply 'generic-load-site-part-2
	   site-name 3d-world
	   (get-prop 3d-world :site-glue) ; use the site-glue specified by the 3d world
	   args)))

(defmethod generic-load-site-part-2 (site-name 3d-world
					       (site-glue basic-site-glue)
					       &key
				     ;; (load-pyramids :menu)
				              (load-pyramids *default-load-pyramids-value*)
					       pyramid-filter
					       frame)
  (let* (images)
    (setf (get-prop 3d-world :site-path) (site-path site-name site-glue))

    #+never
    (unless (find-terrain-model 3d-world)
      (setup-phoney-terrain-model 3d-world))

    ;; need to fix this so same feature sets  do not get loaded over and over.
    ;; Fixed:  load-feature-sets now looks at file-write-date and does caching.
    (load-site-feature-sets 3d-world  site-glue)
 
    (cond ((eq load-pyramids :menu)
	   (setq images (load-site-pyramid-menu 3d-world frame site-glue
						:pyramid-filter pyramid-filter)))
	  
	  (load-pyramids
	   (let ((pyramid-names (if (consp load-pyramids)
				    load-pyramids
				    (site-pyramid-names 3d-world site-glue))))
	     (when pyramid-filter
	       (setq pyramid-names (funcall pyramid-filter pyramid-names site-glue)))
	     (setq images (loop for entry in pyramid-names
				for 2d-world-name = (if (consp entry) (car entry) entry)
				append (load-site-2d-world-pyramids 2d-world-name 3d-world site-glue
					    :load-pyramids (if (consp entry) (cdr entry) t))))
	     (when (and frame images)
	       ;;(format t "load-site displaying ~a~%" images)
	       (display-site-images 3d-world :frame frame
				    :image-list (image-list-from-pyramid-list images)
				    :load-images nil))
	     )))

    (values 3d-world images)))

;;; broken with image-pyramids in C++ code.
;;;(defun image-list-from-pyramid-list (pyramid-list)
;;;  (loop for thing in pyramid-list
;;;        collect (if (typep thing 'image-pyramid)
;;;                    (top-of-image-pyramid thing)
;;;                    thing)))

(defun image-list-from-pyramid-list (pyramid-list)
  pyramid-list)

(defparameter *radius-site-glue* (make-instance 'radius-site-glue :name "RADIUS"))
(defparameter *radius-site-glue2* (make-instance 'radius-site-glue2 :name "RADIUS"))

(defparameter *site-glue-list* (list *radius-site-glue* *freedius-site-glue*))


(defvar *fasd-form-must-call-next-method-once* nil)
(defparameter *object-io-site-glue* nil )

(defmethod fasd-form :around ((object t))
  (if (and *object-io-site-glue* (not *fasd-form-must-call-next-method-once*))
      (site-glue-fasd-form *object-io-site-glue* object)
      (let ((*fasd-form-must-call-next-method-once* nil)) ; this is ugly, but ...
	(call-next-method))))

;;; default site-glue-fasd-form
(defmethod site-glue-fasd-form ((site-glue basic-site-glue) (thing t))
  (let* ((*fasd-form-must-call-next-method-once* t))
    (fasd-form thing)))


(defmethod site-glue-fasd-form ((site-glue basic-site-glue) (2d-world 2d-world))
  `(make-site-2d-world-named
    ,(name 2d-world)
    ,(site-glue-fasd-form site-glue (3d-world 2d-world))
    ))

(defmethod site-glue-fasd-form ((site-glue basic-site-glue) (3d-world 3d-world))
  `(load-3d-world ,(or (get-prop 3d-world :site-name) (name 3d-world))
    nil
    ))


;;; ****************************  RTS-SITE-GLUE  ****************************

#|

This is a specialization of the RADIUS-SITE-GLUE class for the way in which
MM M&DSO is storing its site models for the Radius Testbed System (RTS).

The major differences are:

Feature- sets:  The subdirectory FeatureSets contains a collection of feature sets,
                vs. a single site-baseline.fs feature-set.

|#

(defclass rts-site-glue (radius-site-glue)
    ()
  (:default-initargs :feature-set-paths nil)
  )

(defparameter *rts-site-glue* (make-instance 'rts-site-glue :name "RADIUS"))
       
(defmethod feature-set-pathnames ((3d-world 3d-world) (site-glue rts-site-glue))
  (loop with site-path = (site-path 3d-world site-glue)
	for fs-path in (directory-list-filter (directory (dir-relative-path site-path "*")
							 ;;(format nil "~a/FeatureSets/*" site-path)
							 ))
	;;for fs-path-namestring = (namestring fs-path)
	unless (path-with-version-p fs-path)
	  ;; Various radius sites have totally random names for their feature-sets.
	  ;; We really want to just eliminate pathnames with tilde versions
	  ;;(string-equal fs-path-namestring "-fs" :start1 (- (length fs-path-namestring) 3))
	  collect
	  (dir-relative-path site-path (pathname-name fs-path) "FeatureSets")
	  ;;(format nil "~a/FeatureSets/~a" site-path (pathname-name fs-path))
	))

;;; Eliminate unix dot files and files with version numbers.
;;; Sort the resulting paths by file [.type].
;;; Make sure pathname-name is non-null in each result path.
(defun directory-list-filter (paths)
  ;; callers expect that the returned paths have non-null pathname-name
  (setq paths
	(loop for path in paths
	      do (unless (pathnamep path) (setq path (pathname path)))
	      collect (if (and (null (pathname-name path)) (null (pathname-type path)))
			  (let ((dirs (pathname-directory path)))
			    (make-pathname :defaults path
					   :directory (butlast dirs)
					   :name (car (last dirs))))
			  path)))
  (let ((l (sort (loop for path in paths
		       unless (or (unix-dot-directory-p path)
				  (path-with-version-p path))
			 collect (let ((name (or (pathname-name path)
						 (car (last (pathname-directory path)))))
				       (type (pathname-type path)))
				   (cons (if type
					     (format nil "~a.~a" name type)
					     name)
					 path)))
		 #'string-lessp :key #'car)))
    (loop for (name . path) in l
	  do (ignore name)
	  collect (namestring path) ; uggh -- some callers expect a namestring rather than a pathname.
	  )))

#|
(feature-set-pathnames (3d-world (top-view)) *rts-site-glue*)

(load-site-feature-sets (3d-world (top-view)) *rts-site-glue*)

(create-directory "$RADIUS/sites/alv/2d-worlds/alv-oblique-tower")

(setf (get-prop (3d-world (top-view)) :site-glue) (make-instance 'radius-site-glue2))
(setf (get-prop (3d-world (top-view)) :site-glue)
      (make-instance 'radius-site-glue1 :name "alv-site-glue" :2d-worlds-file-name "2d-world-names" ))
|#



;;; ********************   CONTAINERS  ********************

(defmethod draw-on-view ((object forward-referenced-object) view vis)
  (ignore view vis))

(defmethod draw-on-view-internal ((object forward-referenced-object) view vis)
  (ignore view vis))

;;; newv copy or dump
(defmethod fasd-form ((object forward-referenced-object))
  nil)

(defmethod view-pt-near-p ((object forward-referenced-object) view u v)
  (ignore view u v))

(defmethod remove-object ((object forward-referenced-object) (feature-set forward-referenced-object))
  nil)


#|
(defstruct-class object-unique-id (fasd-form-init-plist-struct)
  ((container :initarg :container :accessor container)
   (instance-id :initarg :instance-id :accessor instance-id)))
|#

(defmethod fasd-form ((object-unique-id object-unique-id))
  `(make-object-unique-id ,(instance-id object-unique-id)
    ',(container-spec (container object-unique-id))))

(defmethod print-object ((object-unique-id object-unique-id) stream)
  (format stream "#<UNIQUE-ID~{ ~s~} ~s>"
	  (container-spec (container object-unique-id)) (instance-id object-unique-id))) 

(defun object-with-id (&rest unique-id-list)
  (let* ((container (reference-container (butlast unique-id-list)))
	 (object-id (car (last unique-id-list)))
	 (object (gethash object-id (hash-table container))))
    (or object
	;; fall thru for forward referenced containers
	(let ((obj (make-instance 'forward-referenced-object)))
	  (set-object-unique-id obj (make-instance 'object-unique-id
						   :container container
						   :instance-id object-id))
	  obj))))
	

(defparameter *save-site-models-optimize-container-references* nil)
(defparameter *save-site-models-optimize-world-references* nil)


(defmethod print-object ((container basic-container) stream)
  (let ((pathname (container-pathname container))
	(container-spec (container-spec container)))
    (if (container-pathname container)
	(format stream "#<Container ~a>"pathname)
	(if (listp container-spec)
	    (format stream "#<Container~{ ~a~}>" container-spec)
	    (format stream "#<Container ~a>" container-spec)))))

(defmethod make-object-unique-id ((object basic-object) (container basic-container))
  (let ((id (unique-id object)))
    (if (and id (equal (gethash (instance-id id) (hash-table container)) object))
	id
	(set-object-unique-id object
			      (make-instance 'object-unique-id
					     :container container
					     :instance-id (incf (instance-count container)))))))

(declaim (special  *LOADING-MODELS-FROM-CONTAINER*))

;;; SET-OBJECT-UNIQUE-ID should only be called during object creation while loading container files.
;;; The container should always be defined and the container being loaded.
(defmethod set-object-unique-id ((object t) (unique-id-spec number))
  (set-object-unique-id object
			(make-instance 'object-unique-id
				       :container *LOADING-MODELS-FROM-CONTAINER*
				       :instance-id unique-id-spec)))

(defmethod set-object-unique-id ((object t) (unique-id-spec list))
  (unless (eq (reference-container (butlast unique-id-spec)) *LOADING-MODELS-FROM-CONTAINER*)
    (error "Attempted to create an object in the wrong container ~s while loading container ~s."
	   unique-id-spec (container-spec *LOADING-MODELS-FROM-CONTAINER*)))

  (set-object-unique-id object
			(make-instance 'object-unique-id
				       :container *LOADING-MODELS-FROM-CONTAINER*
				       :instance-id (car (last unique-id-spec)))))
  


;;; NEED TO FIX THINGS HERE TO PERMIT RELOADING A MODEL FILE
;;; (perhaps a we had to abort in the middle of loading)

(defmethod set-object-unique-id ((object t) (unique-id object-unique-id))
  (let ((object-in-container (gethash (instance-id unique-id) (hash-table (container unique-id)))))
    (when (and object-in-container (neq object-in-container object))
      (unless (typep object-in-container 'forward-referenced-object)
	(error "Attempting to assign the same unique-id ~a to ~a, but ~a already has that id."
	       unique-id object object-in-container))
      (fixup-object-forward-references object-in-container object)
      ))

  (setf (gethash (instance-id unique-id) (hash-table (container unique-id))) object)
  (setf (unique-id object) unique-id))

(defmethod unique-id ((object gl-object))
  (get-prop object :unique-id))

(defmethod (setf unique-id) (id (object gl-object))
  (setf (get-prop object :unique-id) id))

  
(defmethod add-to-container (object (container basic-container)
				    &key (if-present-in-another-container :error))
  (let* ((unique-id (unique-id object))
	 (current-container (and unique-id (container unique-id))))
    (if current-container
	(if (eq current-container container)
	    t
	    (if (eq if-present-in-another-container :error)
		(error "add-to-container ~a found that ~a is already in container ~a"
		       container object current-container)
		nil))			; return nil
	
	(let ((unique-id (make-object-unique-id object container)))
	  (getf (gethash (instance-id unique-id) (hash-table (container unique-id))) object)
	  unique-id)
	)))

(defmethod parse-container-spec ((container-spec list))
  (let* ((l container-spec)
	 (ev-prefix
	  (cond ((ev-pathname-p (car l))
		 (pop l))
		((char= (aref (car l) 0) #\/)
		 (error "container path prefixes must start with an environment variable: ~a"
			container-spec))
		(t "$RADIUS/sites")))
	 (site-name (when (equal (pathname-name ev-prefix)  "sites") (pop l)))
	 (image-name (when (> (length l) 1) (pop l)))
	 (container-name (pop l)))
    (when l (error "container container-spec is has too many elements: ~a ~a"
		   container-spec (list ev-prefix site-name image-name container-name)))
    (values ev-prefix site-name image-name container-name)))

;;; This is also defined in load-cme-objects.lisp
(defmethod canonicalize-container-spec ((container-spec list))
  (mv-bind (ev-prefix site-name image-name container-name)
      (parse-container-spec container-spec)
    (let ((postfix (if image-name
		       (list image-name container-name)
		       (list container-name))))
      (if (equal ev-prefix "$RADIUS/sites")
	  (list* site-name postfix)
	  (if site-name
	      (list* ev-prefix site-name postfix)
	      (list* ev-prefix postfix))))))

;;; This is also defined in load-cme-objects.lisp
(defmethod canonicalize-container-spec ((container basic-container))
  (setf (container-spec container) (canonicalize-container-spec (container-spec container))))

;;; This is also defined in load-cme-objects.lisp
(defmethod make-container-pathname ((container basic-container))
  (mv-bind (ev-prefix site-name image-name container-name)
      (parse-container-spec (container-spec container))
  
    (setf (container-pathname container)
	  (cond ((and site-name image-name)
		 (dir-relative-path ev-prefix site-name "images" image-name "models" container-name)
		 ;;(format nil "~a/~a/images/~a/models/~a" ev-prefix site-name image-name container-name)
		 )
		(site-name
		 (dir-relative-path ev-prefix site-name "models" container-name)
		 ;;(format nil "~a/~a/models/~a" ev-prefix site-name container-name)
		 )
		(image-name
		 (dir-relative-path ev-prefix image-name "models" container-name)
		 ;;(format nil "~a/~a/models/~a" ev-prefix image-name container-name)
		 )
		))))

;;; This is also defined in load-cme-objects.lisp
(defmethod fasd-form ((container basic-container))
  `(make-container ',(container-spec container)))

;;; This is also defined in load-cme-objects.lisp
(defmethod fasd-form-p ((container basic-container)) t)

;;; The use of the term "site" is confusing here.  We are referring to the
;;; location where the model extraction is taking place.  It would be nice to
;;; use the IP address of the facility, but that might not be stable or even
;;; exist for classified facilities.

;;;(defparameter *site-name* "QuamHouse")
(defparameter *extraction-facility-name* "QuamHouse")

(defun get-extraction-facility-unique-id ()
  *extraction-facility-name*)

(defparameter *unix-whoami-program-pathname* "/usr/ucb/whoami")

#|
ln -s /usr/ucb/whoami $CMEHOME/sunos/bin/whoami
ln -s /usr/ucb/whoami $CMEHOME/solaris/bin/whoami
ln -s /bin/whoami $CMEHOME/irix5/bin/whoami
|#


(defun get-user-unique-id ()
  (username))

;;; MAKE-SITE-CONTAINER should be renamed to 

(defmethod make-site-container ((world 3d-world) container-name &key prefix)
  (let ((l (list (world-name world) container-name)))
    (when prefix (push prefix l))
    (make-container l)))

(defmethod make-site-container ((world 2d-world) container-name &key prefix )
  (let ((l (list (world-name (3d-world world)) (world-name world) container-name)))
    (when prefix (push prefix l))
    (make-container l)))

;;;(defmethod make-user-container ((world t) container-name)
;;;  (make-site-container world (format nil "~a/~a/~a" (get-site-unique-id) (get-user-unique-id) container-name) ))

(defmethod make-user-container ((world t) container-name)
  (make-site-container world container-name
		       :prefix
		       (dir-relative-path "$RADIUS/shoeboxes" ; $RADIUS is probably bogus
					  (get-extraction-facility-unique-id)
					  (get-user-unique-id)
					  "sites")
		       #+never
		       (format nil "$RADIUS/shoeboxes/~a/~a/sites"
			       (get-extraction-facility-unique-id) (get-user-unique-id))))

(defmethod make-local-container ((world t) container-name)
  (make-site-container world
		       ;; FIXME: Unix specific pathname merging
		      (format nil "~a/~a" (get-extraction-facility-unique-id) container-name)
		       ))


(defun pprint-key-val-pairs (pairs &key (stream t) (n-initial-elements 2) parent)
  (let ((*standard-output* stream))
    (pprint-logical-block
     (*standard-output* pairs :prefix "(" :suffix ")")
     (when t
       (loop repeat n-initial-elements
	     do (write (pprint-pop))
		(pprint-tab :current 1 1))
       (pprint-indent :block 4)
       (pprint-newline :mandatory)
       )
   
     (pprint-exit-if-list-exhausted)
     (pprint-logical-block
      (*standard-output* (nthcdr n-initial-elements pairs))
      (pprint-exit-if-list-exhausted)
      (pprint-indent :block 0)
    
      (loop (pprint-exit-if-list-exhausted)
	    (pprint-indent :block 0)
	    (mv-bind (key value) (map-key-value (pprint-pop) (pprint-pop))
	      (when key
		(pprint-thing key :stream *standard-output*)
		(pprint-tab :section-relative  1 4)
		(pprint-newline :fill)
		(pprint-tab :line  4 4)
		(pprint-slot parent key value *standard-output*)
		;;(pprint-thing value :stream *standard-output*)
		(pprint-exit-if-list-exhausted)
		(pprint-newline :mandatory))))))))

(defun optimize-eval-form (value)
  (if (consp value)
      (cond ((and (eq (car value) 'quote)
		  (or (numberp (cadr value))
		      (stringp (cadr value))
		      (keywordp (cadr value))
		      (memq (cadr value) '(t nil))))
	     (cadr value))
	    (t value))
      value))


(defun map-key-value (key value)
  (values key
	  (optimize-eval-form
	   (case key
	     (:object-to-world-transform
	      (if (eq (car value) 'MAKE-COORD-FRAME)
		  `',(substitute :z-deg :kappa-degrees (cdr value))
		  value))
	     (:world (if *save-site-models-optimize-world-references*
			 (return-from map-key-value nil)
			 value))
	     ((:vertices :top-vertices) (cadr value))
	     (otherwise value)))))


;;; **********************  PPRINT DISPATCH CUSTOMIZATION  **********************

(defparameter *print-vector-length* 10)

;;; printer for coordinate-vectors
(defun cv-print (s x colon? atsign?)
  (declare (ignore colon? atsign?))
  (if (and (eq (array-element-type x) 'double-float)
	   (<= (length x) *print-vector-length* ))
      (format s "#.(~s" 'cv)
      (format s "#("))
  (loop for i from 0 below (length x)
	do (format s " ~a" (aref x i)))
  (format s ")" ))

(defparameter *print-float-format-fill* "~,3f")
(defparameter *print-float-format* nil)
;;; "~,3f"

(defmethod print-float (stream (object float) &optional colon? atsign?)
  (ignore colon? atsign?)
  (format stream (or *print-float-format* *print-float-format-fill*) object))
  
(defun pprint-thing (thing &key stream)
  (if (floatp thing)
      (format stream (or *print-float-format* *print-float-format-fill*) thing)
      (let ((*print-float-format* *print-float-format-fill*))
	;;(format *terminal-io* "pprint-thing ~a~%" thing)
	(write thing :stream stream))))

;;; These slots must have slot-value form  (LIST key1 val1 key2 val2 ...)
(defmethod pprint-slot ((parent t) slot-name slot-val stream)
  (ignore slot-name)
  (pprint-thing slot-val :stream stream))

(defparameter *basic-object-key-value-slot-names* '(:attributes :metadata :property-list))

(defmethod pprint-slot ((parent basic-object) slot-name slot-val stream)
  (if (and (memq slot-name *basic-object-key-value-slot-names*) (eq (car slot-val) 'list))
      (pprint-key-val-pairs slot-val :n-initial-elements 1  :stream stream)

      (pprint-thing slot-val :stream stream)))




(declaim (special *pyramids* *LOAD-PYRAMID-PANEL*))

(defmethod load-site-pyramid-menu ((3d-world 3d-world) frame (site-glue basic-site-glue)
				   &key pyramid-filter)
  ;;(break)
  (let* ((panel (make-cvv-panel
		 '((pyramids nil :multiple-choose-list :alist nil :height 10 :scroll "right")
		   (load-pyramids nil :button :text "LOAD Selected")
		   (load-all-pyramids nil :button :text "LOAD ALL"))
		 :resource-name "Obj" ; TK class
		 :callback-function 'load-site-pyramid-menu-callback
		 :title "Load Site Images"
		 :position :mouse
		 :cache-p t))
	 pyramids
	 (pyramids-item (cvv-item panel 'pyramids))
	 pyramid-itemlist
	 )
    (set-listbox-items pyramids-item nil)
    
    (setq pyramids (site-pyramid-names 3d-world site-glue))
    (setq *pyramids* pyramids)
    (when pyramid-filter
      (setq pyramids (funcall pyramid-filter pyramids)))

    (setq pyramid-itemlist (loop for (2d-world-name pyramid-name) in pyramids
				 collect (if pyramid-name
					     (format nil "~a ~a" 2d-world-name pyramid-name)
					     2d-world-name)))
    (set-listbox-items pyramids-item pyramid-itemlist)
    (setf (get-prop panel :pyramids) pyramid-itemlist)
    (setf (get-prop panel :3d-world) 3d-world)
    (setf (get-prop panel :frame) frame)
    (setf (get-prop panel :site-glue) site-glue)
    (setq *load-pyramid-panel* panel)
    (pop-up-panel panel)
    ;;(tk::set-toplevel-widget-position (tk::widget panel) :mouse)
    (setf (get-prop panel :panel-done) nil
	  (get-prop panel :image-list) nil)
    ))

#|
(get-3d-world-named "Alv")
(get-2d-world-names (get-3d-world-named "Alv") *radius-site-glue*)
(get-3d-world-named "Alv")
(let ((3d-world (get-3d-world-named "Alv")))
  (site-pyramid-names 3d-world (load-site-glue 3d-world *radius-site-glue*)))

|#

(defun load-site-pyramid-menu-callback (panel widget item-name &rest ignore)
  (ignore ignore)
  (setq *panel* panel)
  (load-site-pyramid-menu-callback-int panel item-name (get-prop panel :site-glue)))

;;;(defmethod load-site-pyramid-menu-callback-int (panel item-name (site-glue basic-site-glue))
;;;  ;;(break)
;;;  (case item-name
;;;    ((load-pyramids load-all-pyramids)
;;;     ;;; yuk --  we shouldn't have to work so hard to get the menu popped down
;;;     (pop-down-panel panel)
;;;     (loop with 3d-world = (get-prop panel :3d-world)
;;;           with (2d-world-name pyramid-name pyramid)
;;;           for 2d-and-pyramid-name
;;;           in (if (eq item-name 'load-all-pyramids)
;;;                  (get-prop panel :pyramids)
;;;                  (get-listbox-selected-items (cvv-item panel 'pyramids)))
;;;           do (let ((pos (position #\space 2d-and-pyramid-name)))
;;;                (if pos
;;;                    (setq 2d-world-name (substring 2d-and-pyramid-name 0 pos)
;;;                          pyramid-name (substring 2d-and-pyramid-name (1+ pos)))
;;;                    (setq 2d-world-name 2d-and-pyramid-name
;;;                          pyramid-name nil))
;;;                                  
;;;                (setq pyramid (car (load-site-2d-world-pyramids
;;;                                    2d-world-name
;;;                                    3d-world
;;;                                    site-glue
;;;                                    :load-pyramids (list pyramid-name)))))
;;;           when pyramid
;;;             collect pyramid into pyramid-list
;;;           finally
;;;        (let ((image-list (image-list-from-pyramid-list pyramid-list)))
;;;          (setf (get-prop panel :image-list) image-list)
;;;          (when image-list
;;;            (display-site-images 3d-world
;;;                                 :frame (or (get-prop panel :frame)
;;;                                            (setf (get-prop panel :frame)
;;;                                                  (make-cme-frame (format nil "~a 2x2" (name 3d-world))
;;;                                                                  :nx 2 :ny 2)))
;;;                                 :image-list image-list
;;;                                 :load-images nil)))
;;;        (setf (get-prop panel :panel-done) t)
;;;           ))
;;;    
;;;    (quit-panel (setf (get-prop panel :panel-done) t))))

(defmacro with-object-properties (prop-specs object &body body)
  `(symbol-macrolet
    ,(loop for prop-spec in prop-specs
	   for var = (if (consp prop-spec)
			 (car prop-spec)
			 prop-spec)
	   for slot = (if (consp prop-spec)
			  (or (cadr prop-spec) var)
			  (lx::make-keyword (symbol-name var)))
	   collect `(,var (get-prop ,object ',slot)))
    (progn .,body)))

;(get-listbox-selected-items ".top7.pyramids_frm.pyramids")
;(cvv-item-value ".top7.pyramids_frm.pyramids")
;(tk::tcl-eval 'widget_value ".top7.pyramids_frm.pyramids")

;;; new version Sun Aug 29 1999
#+four-pane
(defmethod load-site-pyramid-menu-callback-int (panel item-name (site-glue basic-site-glue))
  (with-object-properties ((all-pyramids :pyramids)
			   (panel-image-list :image-list)
			   frame panel-done 3d-world)
      panel
    (tk::with-cvv-items ((selected-pyramids pyramids)) panel
      (case item-name
	((load-pyramids load-all-pyramids)
	 (pop-down-panel panel)

	 (loop with (2d-world-name pyramid-name pyramid)
	       for 2d-and-pyramid-name
	       in (if (eq item-name 'load-all-pyramids)
		      all-pyramids
		      selected-pyramids)
	       do (let ((pos (position #\space 2d-and-pyramid-name)))
		    (if pos
			(setq 2d-world-name (substring 2d-and-pyramid-name 0 pos)
			      pyramid-name (substring 2d-and-pyramid-name (1+ pos)))
			(setq 2d-world-name 2d-and-pyramid-name
			      pyramid-name nil))
				  
		    (setq pyramid (car (load-site-2d-world-pyramids
					2d-world-name
					3d-world
					site-glue
					:load-pyramids (list pyramid-name)))))
	       when pyramid
		 collect pyramid into pyramid-list
	       finally
	    (let ((image-list (image-list-from-pyramid-list pyramid-list)))
	      (setf panel-image-list image-list)
	      (when image-list
		(display-site-images 3d-world
				     :frame (or frame
						(setf frame
						      (make-cme-frame (format nil "~a 2x2" (name 3d-world))
								      :nx 2 :ny 2)))
				     :image-list image-list
				     :load-images nil)))
	    (setf panel-image-list nil 3d-world nil) ; clear these to allow gc to recliam.
	    (setf panel-done t)))
    
	(quit-panel (setf panel-done t))))))

#-four-pane
(defmethod load-site-pyramid-menu-callback-int (panel item-name (site-glue basic-site-glue))
  (with-object-properties ((all-pyramids :pyramids)
			   (panel-image-list :image-list)
			   frame panel-done 3d-world)
      panel
    (tk::with-cvv-items ((selected-pyramids pyramids)) panel
      (case item-name
	((load-pyramids load-all-pyramids)
	 (pop-down-panel panel)

	 (loop with (2d-world-name pyramid-name pyramid)
	       for 2d-and-pyramid-name
	       in (if (eq item-name 'load-all-pyramids)
		      all-pyramids
		      selected-pyramids)
	       do (let ((pos (position #\space 2d-and-pyramid-name)))
		    (if pos
			(setq 2d-world-name (substring 2d-and-pyramid-name 0 pos)
			      pyramid-name (substring 2d-and-pyramid-name (1+ pos)))
			(setq 2d-world-name 2d-and-pyramid-name
			      pyramid-name nil))
				  
		    (setq pyramid (car (load-site-2d-world-pyramids
					2d-world-name
					3d-world
					site-glue
					:load-pyramids (list pyramid-name)))))
	       when pyramid
		 collect pyramid into pyramid-list
	       finally
	    (let ((image-list (image-list-from-pyramid-list pyramid-list)))
	      (setf panel-image-list image-list)
	      (when image-list
		(display-site-images 3d-world
				     :frame (or frame
						(setf frame
						      (let ((n (length image-list)))
							(cond
							  ((= n 1)  (make-cme-frame (format nil "~a 1x1" (name 3d-world))
										    :nx 1 :ny 1))
							  ((= n 2)  (make-cme-frame (format nil "~a 2x1" (name 3d-world))
										    :nx 2 :ny 1))
							  ((< 2 n 5)  (make-cme-frame (format nil "~a 2x2" (name 3d-world))
										      :nx 2 :ny 2))
							  ((< 4 n 7)  (make-cme-frame (format nil "~a 3x2" (name 3d-world))
										      :nx 3 :ny 2))
							  (t  (make-cme-frame (format nil "~a 2x2" (name 3d-world))
									      :nx 2 :ny 2))))))

				     :image-list image-list
				     :load-images nil)))
	    (setf panel-image-list nil) ; needed for weak-eval-cache.
	    (setf panel-done t)))
    
	(quit-panel (setf panel-done t))))))




(defun display-site-images (3d-world &key
				     frame
				     ;;(frame (pane-frame (selected-pane)))
				     (window-list (inferiors frame))
				     image-list
				     (load-images t) (rev t))
  (when rev (setq window-list (reverse window-list)))
  (let ((n-panes (length window-list))
	(i 0))
    (labels ((pushimg (image pane)
	       (push-image image pane)
	       (let ((view (top-view pane)))
		 (setf (2d-to-window-transform view)
		       (gui::default-2d-to-window-transform view image)))
	       ;; (zoom-to-fit image pane)
	       #+never
	       (when (and (2d-world image) (3d-to-2d-projection (2d-world image)))
		 (loop for fs in (default-visible-feature-sets 3d-world)
		       do (add-feature-set fs (top-view pane) :sensitize t)))
	       )
	     (display (image)
	       (when image
		 (unless (image-prop image :name)
		   (let ((name (name (2d-world image))))
		     (setf (image-prop image :name) name)))
		 ;;(format t ";;; display ~a ~a~%" image (nth (mod i n-panes) window-list))
		 (pushimg image (nth (mod i n-panes) window-list))
		 (incf i))))

      (if image-list
	  (loop for image in image-list
		do (display image))
	  (loop for 2d-world in (if rev (2d-worlds 3d-world) (reverse (2d-worlds 3d-world)))
		do (loop for pyramid in (pyramid-list 2d-world)
			 do (display (top-of-image-hierarchy pyramid))))))
    frame))



(defparameter *cme-site-model-list*
	      '(("TEC-2" make-site-frame-from-site-builder
		 "radius/tec-2/tec-setup.lisp" 'show-tec2-images )
		("TEC-3ov" make-site-frame-from-site-builder
		 "radius/tec-3ov/tec-setup.lisp" 'show-tec3-images )
		
		))

(defparameter *sri-site-model-list*
	      '(("ALV-UGV1" make-site-frame-from-site-builder
		 "radius/alv-ugv1/alv-ugv1.lisp" 'get-alv-ugv1-data-set)
		))

(defun get-site-model-list ()
  (append *cme-site-model-list* *sri-site-model-list*))


(defun filter-cme-site-model-menu-item-list (&optional (site-model-list (get-site-model-list) ))
  (loop for (site-name site-maker pathname . options) in site-model-list
	when (probe-file pathname)
	  collect `(,site-name :eval (,site-maker ,site-name ,pathname . ,options))))


;;;(defun select-cme-site ()
;;;  (mv-bind (chosen-site-name non-site-choice)
;;;      (menu-choose
;;;       (make-menu
;;;        (list*
;;;         '("NONE" :eval nil :documentation "Do Not Load a Site Model")
;;;         '("Other" :command com-load-site-model :documentation "Select Another Site Model")
;;;         (append
;;;          (filter-cme-site-model-menu-item-list)
;;;                  
;;;          (loop with list
;;;                for site-glue in *site-glue-list*
;;;                do (loop for site-name in (site-list site-glue)
;;;                         unless (assoc site-name list)
;;;                           do (pushnew `(,site-name :quote ,site-name) list
;;;                                       :test 'equal))
;;;                finally (return (nreverse list))
;;;                )
;;;                                
;;;          ))
;;;        :label "Load a Site Model"))
;;;
;;;    (setq foo (list chosen-site-name non-site-choice))
;;;    (unless non-site-choice
;;;      (loop for site-glue in *site-glue-list*
;;;            do (loop for site-name in (site-list site-glue)
;;;                     when (equal site-name chosen-site-name)
;;;                       return (generic-load-site site-name site-glue))))
;;;    ))

;;(site-list (car *site-glue-list*))
;;(site-name-glue-alist (car *site-glue-list*))

(defun all-sites ()
  (append
   (filter-cme-site-model-menu-item-list)
		  
   (loop with list
	 for site-glue in *site-glue-list*
	 do (loop for site-name in (site-list site-glue)
		  do (pushnew site-name list :test 'equal))
	 finally (return
		   (loop for site-name in (directory-list-filter list)
			 collect `(,site-name :quote ,site-name)))
		 ;;(return (nreverse list))
	 )))
 

;;;
;;; Returns the glue if the named site is loadable using that glue.
;;; This allows us to generalize the glue without depending on
;;; hard-wired names.
;;;

(defmethod site-loadable-p ((glue t) site-name)
  (loop for name in (site-list glue)
        when (equal site-name name)
          return glue))

(defmethod load-site ((site-name string))
  (loop for site-glue in *site-glue-list*
        when (site-loadable-p site-glue site-name)
             return (generic-load-site site-name site-glue)))
  
(defun select-cme-site ()
  (mv-bind (chosen-site-name non-site-choice)
      (menu-choose
       (list*
	'("NONE" :eval nil :documentation "Do Not Load a Site Model")
	'("Other" :command com-load-site-model :documentation "Select Another Site Model")
	(all-sites))
       :label "Load a Site Model")

    ;;(setq foo (list chosen-site-name non-site-choice))
    ;; allegro doesn't allow (namestring symbol)

    (when (symbolp chosen-site-name)
      (setq chosen-site-name (symbol-name chosen-site-name))
      (unless non-site-choice
	(loop for site-glue in *site-glue-list*
              when (site-loadable-p site-glue chosen-site-name)
                return (generic-load-site chosen-site-name site-glue))
        ))))


(defun com-load-site-model ()
  (format t "~%This doesn't work at present.")
  #+never
  (lisptk::tcl-eval "tk_getOpenFile"  ;; sometimes pukes madly.
		    "-initialdir" "/"
		    "-filetypes"
		    '{
		    '{ "Worlds" '{ ".lisp" ".lis" '} '}
		    '{ "All Files" "*" '}
		    '}
		    ))

#|
(all-sites)
(select-cme-site)
(cvv-item 'pyramids *LOAD-PYRAMID-PANEL*)
(get-listbox-selected-items (cvv-item 'pyramids *LOAD-PYRAMID-PANEL*))
(cvv-item 'pyramids *LOAD-PYRAMID-PANEL*)

(loop with list
	       for site-glue in *site-glue-list*
	       do (loop for site-name in (site-list site-glue)
			do (pushnew site-name list :test 'equal))
	       finally (return
			 (loop for site-name in (directory-list-filter list)
			       collect `(,site-name :quote ,site-name)))
		       ;;(return (nreverse list))
	       )
(pprint (site-list *site-glue-list*))
|#
