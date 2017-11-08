(in-package :cme)

;;; New Thu Oct 17 1996

#|  ***************   State of CAMERA-MODEL-IO prior to Thu Oct 17 1996   ***************

Known problems:

  1. Bug in some old FBIP files:
        FBIP header incorrectly says WORLD-COORDINATE-SYSTEM CARTESIAN rather than LONG-LAT

  2. File naming conventions used to distinguish camera-model classes.  (fbip camera-model etc)

  3. Same pathnames can store either a 4x4-coordinate-projection,
       a composite containing a 4x4-coordinate-projection, or a 3d-world transform adjustment.
     Ie, the same path can hold either a relative model (adjustment), a simple  absolute model,
     or a composite absolute model.



Here are some situations that occur in datasets built with the old conventions:


<2d-world> directory contains:

     fbip			 => unadjusted FBIP
     camera-model                => either unadjusted or adjusted 4x4-projection


<site>/camera-models directory contains:

     <2d-world-name>.fbip        => either unadjusted or adjusted FBIP (not sure unadjusted case occurs)
     <2d-world-name>             => either absolute or relative model






 ***************   State of CAMERA-MODEL-IO after to Thu Oct 17 1996   ***************


  1. Patch to detect FBIP files with bad headers.  Issues warnings about the file.
       A function is provided to repair the file (to be implemented).

  2. Site-glue changed to generate and look for files with names:

     in <2d-world> directory:            camera-model
        (adjustments are not allowed in 2d-world directory)

     in <site>/camera-models directory:  <2d-world-name>


     If the <site>/camera-models directory contains files with either of the names
       <2d-world-name>.camera-model or <2d-world-name>.3d-adjustment, then the
       directory is assumed to support only the new file conventions.

     otherwise the old pathnames conventions are handled.

  3. Generic functions LOAD-CAMERA-MODEL and SAVE-CAMERA-MODEL functions are provided.  
     LOAD-CAMERA-MODEL determines the camera-model class from the first line of
     text in the file header.

     These functions are independent of the site-glue naming conventions and only
     require that camera-model-path be passed (as well as the 2d-world).
     
   4. The function UPDATE-CAMERA-MODEL-PATHNAME-CONVENTIONS is provided to rename
        files to the new naming conventions.

        In the 2d-world directory, files with name fbip are renamed to camera-model,
        first making a backup version of any file named camera-model.
         Any files in the 2d-world directory, named camera-model are ok.

        In the <site>/camera-models directory, files with names <2d-world-name>.fbip
         are renamed to <2d-world-name>.


|#





(defparameter *bip-file-header-id-string* "RADIUS BLOCK INTERPOLATION PROJECTION")

;;; Camera models now can be loaded generically using the RECOGNIZE-HEADER strategy of image-io.

(defmethod recognize-header ((format-id (eql 'fbip)) header-first-line &optional stream)
  (ignore stream)
  (when (string-equal header-first-line *bip-file-header-id-string*)
    'fbip))


;;; Perhaps this was intended to be in a separate system?
(defparameter *RPC-FILE-HEADER* "RCDE RATIONAL POLYNOMIAL PROJECTION")
;; (defparameter *RPC-FILE-HEADER* "bogus") ; replaced in $RADIUSCODE/SRI/rpc/rpc00a-rpc.lisp

(defmethod recognize-header ((format-id (eql 'rpc)) header-first-line &optional stream)
  (ignore stream)
  (when (string-equal header-first-line *RPC-FILE-HEADER*)
    'rpc))


(defmethod recognize-header ((format-id (eql 'lisp-camera-model)) header-first-line &optional stream)
  (ignore stream)
  (let ((pos (position #\newline *LISP-MODE-FILE-HEADER*)))
    (when (or (string-equal header-first-line *LISP-MODE-FILE-HEADER* :end2 pos)
	      ;; This next clause should be flushed, but many old lisp camera model files.
	      ;; do not have a *LISP-MODE-FILE-HEADER*
	      ;; This should be flushed at some time in the future.
	      ;; Perhaps we should offer the option to add the header to the file.
	      (and (> (length header-first-line) 0)
		   (eql (aref header-first-line 0) #\()) ; assume LISP if it starts with left parenthesis
	      )
      'lisp-camera-model)))


(defparameter *camera-model-file-format-id-list*
  '(fbip rpc lisp-camera-model)
  )

;;(defparameter *camera-model-file-format-id-list*
;;  '(lisp-camera-model))


(defmethod get-camera-model-file-format (pathname)
  (with-open-file (stream pathname)
    (let ((first-line (read-line stream nil nil)))
      (when first-line
	(loop for format-id in *camera-model-file-format-id-list*
	      thereis (recognize-header format-id first-line stream))))))


#|
(get-camera-model-file-format "$RADIUS/site-2d-worlds/ft-hood-2/fhn713/camera-model")
(load-camera-model "$RADIUS/site-2d-worlds/ft-hood-2/fhn713/camera-model" nil)
(setq 2d-world (make-instance '2d-world :name "foo"))
(make-default-coordinate-system 2d-world)
(load-camera-model "$RADIUS/site-2d-worlds/ft-hood-2/fhn713/camera-model" 2d-world)
|#

#+unimplemented
(defmethod load-camera-model2 ((format-id (eql 'fbip)) pathname 2d-world)
  (let ((projection (read-bip-file pathname)))
    (when 2d-world (change-projection 2d-world projection))
    ;; read-bip-file has been hacked to detect FBIP files with bad headers,
    ;; and set from-coordinate-system correctly.
    (when (typep (from-coordinate-system projection) 'lat-long-coordinate-system)
      (convert-lat-long-fbip 2d-world))
    projection))
  
(defmethod load-camera-model2 ((format-id (eql 'rpc)) pathname 2d-world)
  (let ((projection (transforms::read-rat-poly-projection pathname)))
    (when 2d-world (change-projection 2d-world projection))
    projection))

(defun read-eval-forms-until-eof (st)
  (loop with eof = (list nil)
	with last-value
	for form = (read st nil eof)
	when (eq form eof)
	  return last-value
	do (setf last-value (eval form))))


(defmethod load-camera-model2 ((format-id (eql 'lisp-camera-model)) pathname 2d-world)
  (with-open-file (st pathname)
    (let ((projection
	   (let ((*package* (find-package "CME"))
		 ;;(*site-glue* site-glue)
		 (*2d-world* 2d-world))
	     (declare (special *2d-world* *site-glue*))
	     (read-eval-forms-until-eof st))))
      (when 2d-world (change-projection 2d-world projection))
      projection)))



(defmethod load-camera-model (2d-world simple-camera-model-path &optional adjustment-path)
  (flet ((load-camera-model1 (pathname 2d-world)
	   (let ((format-id (get-camera-model-file-format pathname)))
	     (if format-id
		 (load-camera-model2 format-id pathname 2d-world)
		 (error "LOAD-CAMERA-MODEL does not recognize the format of file ~a~%" pathname)))))
    (let* ((simple-projection (load-camera-model1 simple-camera-model-path 2d-world))
	   (projection (if adjustment-path
			   (load-camera-model1 adjustment-path 2d-world)
			   simple-projection)))
      (setf (get-prop simple-projection :pathname) simple-camera-model-path)
      (unless (eq projection simple-projection)
	(setf (get-prop projection :3d-adjustment-pathname) adjustment-path))
      projection)))


	    
(defmacro save-lisp-format-camera-model-expander (path &body body)
  `(let* ((path ,path)
	  (dir-path (pathname-directory-path path)))
    (unless (probe-file dir-path)
      (create-directory dir-path))
    (backup-file path)
    (with-open-file (st path :direction :output)
      (format t ";;; Writing camera model to ~a~%" path)
      (format st *LISP-MODE-FILE-HEADER*)
      ;; current format expected by get-3d-to-2d-projection expects 1 form in file
      ;;(format st "~s~%" '(in-package "CME"))
      (progn . ,body)
      (format st "~%")
      )))

#+unfinished
(defmethod save-simple-camera-model
	   ((projection fast-block-interpolation-projection)
	    pathname
	    &key (format 'fbip))
  (ignore format)
  ;; write-binary-data-bip-file backs up the file if it exists
  (write-binary-data-bip-file projection pathname))

#+unfinished
(defmethod save-simple-camera-model
	   ((projection rat-poly-projection)
	    pathname
	    &key (format 'rpc))
  (ignore format)
  ;; write-rat-poly-projection-to-file backs up the file if it exists
  (write-rat-poly-projection-to-file projection pathname))


(defmethod save-simple-camera-model
	   ((projection t)
	    pathname
	    &key (format 'lisp-camera-model) (site-glue *radius-site-glue*))
  (ignore format)
  ;; write-rat-poly-projection-to-file backs up the file if it exists
  (save-lisp-format-camera-model-expander
   pathname
   (pprint (site-glue-fasd-form site-glue projection) st)))

(defmethod save-camera-model-adjustment
	   ((3d-transform-list t)
	    pathname
	    &key (format 'lisp-camera-model) (site-glue *radius-site-glue*))
  (ignore format)
  (if 3d-transform-list
      (save-lisp-format-camera-model-expander
       pathname
       (pprint
	`(compose-3d-transform-with-2d-world-projection
	  ,(if (consp 3d-transform-list)
	       `(list . ,(loop for 3d-trans in 3d-transform-list
			       collect (site-glue-fasd-form site-glue 3d-trans)))
	       `(site-glue-fasd-form ,site-glue ,3d-transform-list)))
	st))
      ;; no adjustment --  rename the any existing camera-model-adjustment file
      ;; so that it not no longer used
      (when pathname (backup-file-by-rename pathname) ))
  )


(defmethod save-camera-model
	   ((2d-world 2d-world)
	    simple-camera-model-path
	    camera-model-adjustment-path
	    &key
	    format
	    (site-glue *radius-site-glue*))
  (ignore format)
  (let ((projection (3d-to-2d-projection 2d-world)))
    (mv-bind (3d-transform-list simple-proj 2d-transform-list)
	(transforms::decompose-composite-coordinate-projection projection)

      (if (and 3d-transform-list
	       (null camera-model-adjustment-path)
	       (fasd-savable simple-proj))
	  ;; It isn't clear that we really want this optimization.
	  ;; It is ok, I guess, since we allow fully composed projections to go here too.
	  ;; simple-camera-model-path should not be in the 2d-world directory.
	  (unless (get-prop projection :pathname)
	    ;; save the lisp-readable composite coordinate projection fasd-form
	    (save-simple-camera-model projection
				      simple-camera-model-path
				      :format format :site-glue site-glue)
	    )
	  
	  (progn
	    (when 2d-transform-list
	      (error "Cannot save composite 3d-to-2d-projection with a 2d-transform-list: ~a"
		     (list 3d-transform-list simple-proj 2d-transform-list)))

	    (when (and 3d-transform-list (null camera-model-adjustment-path))
	      (error "Must specify camera-model-adjustment-pathname."))

	    (unless (get-prop simple-proj :pathname)
	      ;;(string-equal simple-camera-model-path (get-prop simple-proj :pathname))
	      ;; do not write back the camera model if it already has a pathname
	      (save-simple-camera-model simple-proj simple-camera-model-path
					:format format :site-glue site-glue))
	  
	    (unless (equal simple-camera-model-path camera-model-adjustment-path)
	      (save-camera-model-adjustment 3d-transform-list camera-model-adjustment-path
					    :format format :site-glue site-glue)))))))
  
  


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






;;; Changes to site-glue -- move to site-glue.lisp

;;; This reads legacy files
;;;(defmethod old-get-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue))
;;;  (or (3d-to-2d-projection 2d-world)
;;;      ;; FIXME: Unix specific pathname merging.
;;;      (loop for path in (list (format nil "~a/fbip"
;;;                                          (2d-world-path 2d-world site-glue))
;;;                              (format nil "~a/camera-model"
;;;                                          (2d-world-path 2d-world site-glue))
;;;                              (format nil "~a/camera-models/~a.fbip"
;;;                                          (site-path (3d-world 2d-world) site-glue)
;;;                                          (site-glue-world-name 2d-world site-glue))
;;;                              (format nil "~a/camera-models/~a"
;;;                                          (site-path (3d-world 2d-world) site-glue)
;;;                                          (site-glue-world-name 2d-world site-glue)))
;;;            do (old-load-3d-to-2d-projection 2d-world site-glue path)
;;;            finally (return (3d-to-2d-projection 2d-world)))))

;;; Sun Dec 16 2001 - flushed - do not want legacy camera models in 2d-world-path except for fbip
;;;(defmethod old-get-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue))
;;;  (or (3d-to-2d-projection 2d-world)
;;;      (loop for path 
;;;            in (list
;;;                 (dir-relative-path (2d-world-path 2d-world site-glue) "fbip")
;;;                 (dir-relative-path (2d-world-path 2d-world site-glue) "camera-model")
;;;                 (dir-relative-path (site-path (3d-world 2d-world) site-glue)
;;;                     "camera-models"
;;;                     (format nil "~a.fbip" (site-glue-world-name 2d-world site-glue)))
;;;                 (dir-relative-path (site-path (3d-world 2d-world) site-glue)
;;;                     "camera-models"
;;;                     (site-glue-world-name 2d-world site-glue)))
;;;            do (old-load-3d-to-2d-projection 2d-world site-glue path)
;;;            finally (return (3d-to-2d-projection 2d-world)))))


(defmethod old-get-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue))
  (or (3d-to-2d-projection 2d-world)
      (loop for path 
	    in (list
		 (dir-relative-path (2d-world-path 2d-world site-glue) "fbip")
		 ;; Connolly change:
		 ;; This next might try to load an old CME camera model with a unknown projection class.
		 (dir-relative-path (2d-world-path 2d-world site-glue) "camera-model")
		 ;; The new coordinate system parenting scheme blows
		 ;; away the 3d-world of a 2d-world during the load
		 ;; process for some older site models.  To work
		 ;; around this, I place the 3d-world on the
		 ;; 2d-world's property list to save it.  Check the
		 ;; property-list here as a fallback for an unparented
		 ;; 2d-world:
		 (dir-relative-path (site-path (or (3d-world 2d-world)
						   (get-prop 2d-world :3d-world))
					 site-glue)
		     "camera-models"
		     (format nil "~a.fbip" (site-glue-world-name 2d-world site-glue)))
		 (dir-relative-path (site-path (or (3d-world 2d-world)
						   (get-prop 2d-world :3d-world))
					 site-glue)
		     "camera-models"
		     (site-glue-world-name 2d-world site-glue)))
	    when (old-load-3d-to-2d-projection 2d-world site-glue path)
	      ;; return first camera model encountered
	      return (3d-to-2d-projection 2d-world)
	    finally (return (3d-to-2d-projection 2d-world)))))


;;; This reads legacy files
(defmethod old-load-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue)
				     path)
  (when (probe-file path)
    (format t ";;; Loading camera model ~a with old pathname conventions.~%" path) ;
	       
    (cond #+unimplemented
	  ((equal (pathname-name path) "fbip")
	   (let ((projection (read-bip-file path)))
	     (setf (3d-to-2d-projection 2d-world) projection)
	     ;; read-bip-file has been hacked to detect FBIP files with bad headers,
	     ;; and set from-coordinate-system correctly.
	     (when (typep (from-coordinate-system projection) 'lat-long-coordinate-system)
	       (convert-lat-long-fbip 2d-world))
	     projection))
	  #+unimplemented
	  ((equal (pathname-type path) "fbip")
	   (let ((projection (read-bip-file path)))
	     (setf (3d-to-2d-projection 2d-world) projection)
	     projection))
	
	  (t (with-open-file (st path)
	       (let ((*package* (find-package "CME"))
		     (*site-glue* site-glue)
		     (*2d-world* 2d-world))
		 (declare (special *2d-world* *site-glue*))
		 (let ((projection (read-eval-forms-until-eof st)))
		   (change-projection 2d-world projection)
		   projection)))))))

;;;(defmethod camera-model-paths ((2d-world 2d-world) (site-glue radius-site-glue))
;;;  (let* ((site-path (site-path (3d-world 2d-world) site-glue))
;;;         (2d-world-name (site-glue-world-name 2d-world site-glue))
;;;        )
;;;    ;; FIXME: Unix specific pathname merging.
;;;    (values (format nil "~a/camera-model" (2d-world-path 2d-world site-glue))
;;;            (format nil "~a/camera-models/~a" site-path 2d-world-name))))

(defmethod camera-model-paths ((2d-world 2d-world) (site-glue radius-site-glue))
  (let* ((site-path (site-path (3d-world 2d-world) site-glue))
	 (2d-world-name (site-glue-world-name 2d-world site-glue))
	 (2d-world-path (pathname-as-directory (2d-world-path 2d-world site-glue)))
	 )
    (values (merge-pathnames "camera-model" (pathname-as-directory 2d-world-path))
	    (merge-pathnames (make-pathname :name 2d-world-name 
					    :directory  (list :relative "camera-models"))
			     (pathname-as-directory site-path)))))

;;;(defmethod old-camera-model-paths ((2d-world 2d-world) (site-glue radius-site-glue))
;;;  (let* ((site-path (site-path (3d-world 2d-world) site-glue))
;;;         (2d-world-name (site-glue-world-name 2d-world site-glue))
;;;        )
;;;    (values (format nil "~a/fbip" (2d-world-path 2d-world site-glue))
;;;            (format nil "~a/camera-models/~a" site-path 2d-world-name)
;;;            (format nil "~a/camera-models/~a.fbip" site-path 2d-world-name))))

;;;(defmethod old-camera-model-paths (2d-world-name 3d-world-name &optional (site-glue *radius-site-glue*))
;;;  (let* ((site-path (site-path 3d-world-name site-glue))
;;;        )
;;;    ;; FIXME: Unix specific pathname merging.
;;;    (values (format nil "~a/fbip" (2d-world-path 2d-world-name  site-glue))
;;;            ;;(format nil "~a/camera-models/~a" site-path 2d-world-name)
;;;            (format nil "~a/camera-models/~a.fbip" site-path 2d-world-name))))

(defparameter *load-legacy-camera-models* t)
;(setq *load-legacy-camera-models* nil)

(defmethod old-camera-model-paths (2d-world-name 3d-world-name &optional (site-glue *radius-site-glue*))
  (when *load-legacy-camera-models*
    (values (merge-pathnames "fbip" (pathname-as-directory (2d-world-path 2d-world-name site-glue)))
	    (merge-pathnames (make-pathname :name 2d-world-name 
					    :directory  (list :relative "camera-models"))
			     (pathname-as-directory (site-path 3d-world-name site-glue))))))

(declaim (special *get-3d-to-2d-projection-site-paths*))

(defmethod get-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue))
  (or (3d-to-2d-projection 2d-world)
      (let* ((*make-perspective-transform-default-2d-world* 2d-world)
	     (old-camera-model-paths
	      (mv-list (old-camera-model-paths
			(site-glue-world-name 2d-world site-glue)
			;; Hack - setf 3d-to-2d-projection nukes the parent, so it's saved as a property:
			(site-glue-world-name (or (3d-world 2d-world) 
						  (get-prop 2d-world :3d-world))
					      site-glue)
			site-glue)))
	     (old-camera-models-exist (loop for path in old-camera-model-paths
					    thereis (and path (probe-file path))))
	     )
	(if old-camera-models-exist
	    (old-get-3d-to-2d-projection 2d-world site-glue)
	
	    (mv-bind (2d-world-camera-model-path site-camera-model-path)
		(camera-model-paths 2d-world site-glue)
	      ;;(setq *foo* (list 2d-world site-glue 2d-world-camera-model-path site-camera-model-path))
	      (let ((2d-world-camera-model-path-exists (probe-file 2d-world-camera-model-path))
		    (site-camera-model-path-exists (probe-file site-camera-model-path)))
		(cond ((and 2d-world-camera-model-path-exists
			    site-camera-model-path-exists
			    (camera-model-file-is-adjustment-p site-camera-model-path))
		       (load-camera-model 2d-world 2d-world-camera-model-path site-camera-model-path))

		      (site-camera-model-path-exists
		       (load-camera-model 2d-world site-camera-model-path ))

		      (2d-world-camera-model-path
		       (load-camera-model 2d-world 2d-world-camera-model-path))
		      )))))))


(defun lisp-camera-model-file-is-3d-adjustment-p (path)
  (with-open-file (st path)
    (let* ((*package* (find-package "CME"))
	   (form (read st)))
      (eq (car form) 'compose-3d-transform-with-2d-world-projection)
      )))

(defmethod camera-model-file-is-adjustment-p (camera-model-path)
  (and (eq (get-camera-model-file-format camera-model-path) 'lisp-camera-model)
       (lisp-camera-model-file-is-3d-adjustment-p camera-model-path)))

(defmethod fasd-savable ((projection t))
  nil)

#+unfinsihed
(defmethod fasd-savable ((projection 4x4-coordinate-projection))
  t)

(defmethod save-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue) &optional path)
  (let* ((projection (3d-to-2d-projection 2d-world))
	 (composite-p (typep projection 'composite-coordinate-projection))
	 (simple-proj (and composite-p (transforms::composite-coordinate-projection-simple-projection projection)))
	 )
    (if path
	(if (and composite-p
		 (not (fasd-savable simple-proj)))
	    (error ";;; The projection is a composite projection that cannot be saved into a specific file.")
	    (save-camera-model 2d-world path nil))
      
	(mv-bind (2d-world-camera-model-path site-camera-model-path)
	    (camera-model-paths 2d-world site-glue)

	  (if composite-p
	      (save-camera-model 2d-world
				 ;; If there is a camera model stored in the
				 ;; 2d-world directory, store it in the site
				 ;; camera-models directory
				 (if (probe-file 2d-world-camera-model-path)
				     (unless (get-prop simple-proj :pathname)
				       (error "SAVE-3D-TO-2D-PROJECTION attempting to save a ~
                                               composite-projection whose simple-projection ~%~
                                               did not originate in the 2d-world directory."))
				     2d-world-camera-model-path)
				 
				 site-camera-model-path)

	      (save-camera-model 2d-world
				 site-camera-model-path
				 ;; If there is a camera model stored in the
				 ;; 2d-world directory, store it in the site
				 ;; camera-models directory
				 (if (probe-file 2d-world-camera-model-path)
				     site-camera-model-path
				     2d-world-camera-model-path))
	      )))))



    
      
(defmethod update-camera-model-pathname-conventions ((2d-world 2d-world) (site-glue radius-site-glue))
  (let* ((old-camera-model-paths (mv-list (old-camera-model-paths
						       (site-glue-world-name 2d-world site-glue)
						       (site-glue-world-name (3d-world 2d-world) site-glue)
						       site-glue)))
	 (old-camera-models-exist (loop for path in old-camera-model-paths
					thereis (probe-file path)))
	 )
    (if (not old-camera-models-exist)
	(format t ";;; Camera model pathnames convertions are up-to-date for ~a ~a~%"
		(3d-world 2d-world) 2d-world)

	(mv-bind (old-2d-world-camera-model-path
			      old-site-camera-model-path
			      old-site-fbip-path)
	    (values-list old-camera-model-paths)
	  (mv-bind (2d-world-camera-model-path
				site-camera-model-path)
	      (camera-model-paths 2d-world site-glue)
	    (flet ((move-file (from to)
		     (when (probe-file from)
		       (when (probe-file to) (backup-file-by-rename to))
		       (rename-file from to))))
	      (move-file old-2d-world-camera-model-path 2d-world-camera-model-path)
	      (when (probe-file old-site-camera-model-path)
		(move-file old-site-camera-model-path site-camera-model-path))
	      (move-file old-site-fbip-path site-camera-model-path)))))))

		
(defmethod 2d-world-old-camera-model-pathnames (2d-world-name 3d-world-name &optional (site-glue *radius-site-glue*))
  (let* ((old-camera-model-paths
	  (mv-list (old-camera-model-paths 2d-world-name 3d-world-name
				    site-glue)))
	 (old-camera-models-exist (loop for path in old-camera-model-paths
					when (probe-file path)
					  collect path))
	 )
    old-camera-models-exist))


(defmethod 3d-world-old-camera-model-pathnames (3d-world-name)
  (loop with site-glue = (load-site-glue 3d-world-name *radius-site-glue*)
	for 2d-world-name in (get-2d-world-names 3d-world-name site-glue)
	for old-camera-model-paths = (2d-world-old-camera-model-pathnames 2d-world-name 3d-world-name site-glue)
	when old-camera-model-paths
	  collect (list* 2d-world-name old-camera-model-paths))) 

(defmethod all-sites-old-camera-model-pathnames ()
  (loop for site-name in (site-list *radius-site-glue*)
	for 2d-names-with-old-camera-models = (3d-world-old-camera-model-pathnames site-name)
	when 2d-names-with-old-camera-models
	  collect (list* site-name 2d-names-with-old-camera-models)))
#|
(all-sites-old-camera-model-pathnames)

(let ((3d-world (3d-world (top-view))))
  (3d-world-contains-old-camera-model-pathnames-p (site-glue-world-name 3d-world (site-glue 3d-world))))


(let* ((2d-world (2d-world (top-view)))
       (site-glue (site-glue (2d-world (top-view)))))
  (2d-world-old-camera-model-pathnames 
   (site-glue-world-name 2d-world site-glue)
   (site-glue-world-name (3d-world 2d-world) site-glue)
   site-glue))
|#

;;;
;;; Was in the FBIP file:
;;;

(defun read-generic-header (stream expected-format-id)
  (let ((*package* (find-package "CME")))
    ;; HEADER-END and other keywords are excpeted to be in the CME package.
    (when (equal (read-line stream) expected-format-id)
      (loop for keyword = (read-preserving-whitespace stream)
	    until (eq keyword 'HEADER-END)
	    for value = (read stream)
	    collect keyword into keyword-value-list
	    collect value into keyword-value-list
	    finally (loop until (char= (read-char stream) #\newline))
		    (return keyword-value-list)))))

;;;
;;; I don't know where to put this: -CC
;;;
(defun transforms::read-rat-poly-projection (pathname)
  (with-open-file (st pathname)
    #+never
    (unless (equal (read-line st) *RPC-FILE-HEADER*)
      (error "rpc file ~a has improper header" pathname))

    (let* ((plist (prog1 (read-generic-header st transforms::*rpc-file-header*) (read-line st)))
	   (coeff-normalization (getf plist 'coeff-normalization)))
      (labels ((read-line-of-things ()
		 (let ((line (read-line st))
		       (pos 0))
		   (unless (zerop (length line))
		     (loop with x
			   do (multiple-value-setq (x pos)
				(read-from-string line nil nil :start pos))
			   while x collect x))))

	       (double-float-vector (l)
		 (make-array (length l) :element-type 'double-float
			     :initial-contents l))
	     
	       (read-norms ()
		 (if (eq coeff-normalization 'min-max)
		     (let ((mins (double-float-vector (read-line-of-things)))
			   (maxs (double-float-vector (read-line-of-things))))
		       (loop for i from 0 below (length mins)
			     for min = (aref mins i)
			     for max = (aref maxs i)
			     do (setf (aref mins i) (* .5 (+ min max))
				      (aref maxs i) (* .5 (- max min))))
		       (values mins maxs)
		       )
	       
		     (values (double-float-vector (read-line-of-things))
			     (double-float-vector (read-line-of-things)))))
	     
	       (read-coeffs ()
		 (double-float-vector (loop for coeffs = (read-line-of-things)
					    while coeffs
					    nconc coeffs)))

	       )

	(let* ((comment1 (read-line st))
	       scales
	       (means (multiple-value-bind (m s) (read-norms) (setq scales s) m))
	       (sep1 (read-line st))
	       (coeffs-vector-vector (apply 'vector (loop repeat 4 collect (read-coeffs))))
	       (sep2 (read-line st nil nil))
	       inv-means inv-scales inv-coeffs-vector-vector sep3)
	  (ignore sep1 sep3 comment1)
	
	  (when (equal sep2 "Image-to-World-Transform")
	    (setq inv-means (multiple-value-bind (m s) (read-norms)
			      (setq inv-scales s)
			      m)
		  sep3 (read-line st)
		  inv-coeffs-vector-vector (apply 'vector (loop repeat 4 collect (read-coeffs)))))
	
	  (flet ((cvt-errs (errs)
		   (destructuring-bind (u-rms u-max v-rms v-max) errs
		     `(((,u-rms ,u-max ,u-rms ,u-max))
		       ((,v-rms ,v-max ,v-rms ,v-max))))))
	    (let* ((proj (transforms::make-rat-poly-projection2 means scales coeffs-vector-vector
						    inv-means inv-scales inv-coeffs-vector-vector))
		   (inverse (inverse-transform proj))
		   (fit-errs (getf plist 'fit-errors))
		   (inverse-fit-errs (and inverse (getf plist 'inverse-fit-errors))))

	      (setf (get-prop proj :rpc00a-errors) (getf plist 'rpc00a-errors))
	      (when fit-errs
		(get-prop proj :rpc-fit-errors) (cvt-errs fit-errs))
	      (when inverse-fit-errs
		(setf (get-prop inverse :rpc-fit-errors) (cvt-errs inverse-fit-errs)))
	  
								    
	      proj)))))))
