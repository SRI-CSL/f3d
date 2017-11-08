(in-package :cme)

#|
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/terrain-models.lisp")
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/usgs-dem.lisp")
|#

#|
The following sequence reads the raw data from the magtape suppied by George Lukes of ETL:

Save the following C main program on the file readtape.c

#include <stdio.h>
#include <fcntl.h>

main(argc,argv)
 char *argv[];
{
  char buf[8192];
  int fd,fd1;	
  int n;
  int recno = 1;

  fd = open("/dev/rmt8", O_RDONLY); /* sunset 9 track drive 6250 bpi */
  if (fd == -1) {exit(1);}
  fd1 = creat(argv[1]);
  /* fprintf(fd1,";;; -*-\n");*/ /* make a phoney attribute list to get around Lispm READ-ATTRIBUTE-LIST problem */ 
  while(1)
   {n = read(fd,&buf[0],sizeof(buf));
	if (n <= 0) break;
	write(fd1,&buf[0],n);
	recno++;
    }
  if (n < 0) perror("read");

}

On the SUNSET 9 track tape drive

/* cc -o readtape readtape.c
   readtape <filename>
*/
|#

(defun quick-read-decimal (stream)
  (loop with char-code-0 fixnum = (char-code #\0)
	with char-code-9 fixnum = (char-code #\9)
	for ch fixnum = (char-code (read-char stream))
	until (<= char-code-0 ch char-code-9)
	finally
	  (return
	    (loop with num fixnum = (- ch char-code-0)
		  for ch fixnum = (char-code (read-char stream))
		  while (<= char-code-0 ch char-code-9)
		  do (setq num (+ (* 10 num) (- ch char-code-0)))
		  finally (return num)))))

(defmacro read-next (bufr pos &optional how-many)
  `(multiple-value-bind (val new-pos)
       (read-from-string ,bufr nil nil :start ,pos
			 :end ,(and how-many `(+ ,pos ,how-many)))
     (setq ,pos new-pos)
     val))



(defclass usgs-generic-dem-header
     ()
     ((title :initform nil :initarg :title :accessor title)
      (header-string :initform nil)
      (planimetric-reference-system-code :initform nil
					 :initarg :planimetric-reference-system-code
					 :accessor planimetric-reference-system-code) ; (1 = UTM)
      (planimetric-zone :initform nil :initarg :planimetric-zone :accessor planimetric-zone)
      (x-dim :initarg :x-dim :accessor x-dim)
      (y-dim :initarg :y-dim :accessor y-dim)
      (x-scale :initarg :x-scale :accessor x-scale) ; units between horizontal DEM elements 
      (y-scale :initarg :y-scale :accessor y-scale) ; units between vertical DEM elements 
      (z-scale :initform 1.0  :initarg :z-scale :accessor z-scale) ; multiplier on DEM z values.
      (horizontal-units :initarg :horizontal-units :accessor horizontal-units)
      (xy-units-per-meter :initarg :xy-units-per-meter :accessor xy-units-per-meter) ;This may be a misnomer.  It is XY-UNITS-PER-horizontal-unit
      (z-units-per-meter :initarg :z-units-per-meter :accessor z-units-per-meter)    ;It is XY-UNITS-PER-vertical-unit ??
      (corners :initarg :corners :accessor corners)
      (x-min :initarg most-positive-single-float)
      (x-max :initarg most-negative-single-float)
      (y-min :initarg most-positive-single-float)
      (y-max :initarg most-negative-single-float)
      (z-min :initarg :z-min :accessor z-min)
      (z-max :initarg :z-max :accessor z-max)
      (z-origin :initform 0.0 :initarg :z-origin :accessor z-origin)
      (pathname :initarg :pathname ))
  )

(defvar *semi-colon-char* #\;)

(defun read-usgs-dem-header (path)
  (with-open-file (st path :direction :input)
    (when (char-equal *semi-colon-char*	 (peek-char nil st)) ;WMT Does it have the ;;; line that we sometimes put there?
      (read-line st))			;Yes, munch it up and spit it out
    (let ((header-string (make-string 1024 )))
      (loop for i from 0 below 1024 do (setf (aref header-string i) (read-char st)))
      (parse-usgs-dem-info header-string path))))

(defun parse-usgs-dem-info (dem-info &optional path)
  (cond ((stringp dem-info)
	 (let* ((header-string dem-info)
		(planimetric-reference-system-code (read-from-string header-string nil nil :start 156))
		(dem (make-instance (case planimetric-reference-system-code
				      (0 'usgs-arcsecond-dem-header)
				      (1 'usgs-utm-dem-header)
				      (2 'usgs-state-plane-dem-header)
				      (otherwise (error "Unrecognized dem planimetric-reference-system-code ~a"
							planimetric-reference-system-code))
				      ))))
	   (setf (slot-value dem 'header-string) header-string)
	   (read-header-internal dem header-string path)
	   dem))
	((consp dem-info)
	 (destructuring-bind (dem-system-type zone xscale xorg yscale yorg &optional zscale zorg) dem-info
	   (let* ((dem (make-instance (case dem-system-type
					(:geographic-transform 'usgs-arcsecond-dem-header)
					(:utm-transform 'usgs-utm-dem-header)
					(:state-plane-transform 'usgs-state-plane-dem-header)
					(otherwise (error "unknown dem-system-type ~a" dem-system-type))))))
	     (with-slots (planimetric-zone x-scale y-scale z-scale x-min y-min z-origin) dem
	       (setf x-scale xscale
		     y-scale yscale
		     z-scale (or zscale 1.0)
		     planimetric-zone zone
		     x-min xorg
		     y-min yorg
		     z-origin (or zorg 0.0)
		     ))
	     dem)))))


(defun get-image-dem-info (image)
  (loop for prop in '(:geographic-transform :utm-transform :state-plane-transform)
	when (get-prop image prop)
	  ;; This looks wrong -- make-usgs-terrain-model passes this to
	  ;; parse-usgs-dem-info which will barf with this list.
	  return (cons prop (get-prop image prop))
	finally 
     (return (or (get-prop image :usgs-dem-info)
		 (getf (img::load-image-property-list image) :usgs-dem-info)))))

(defvar *radians-per-arc-second* (/ pi 3600.0))

(defmethod read-header-internal ((dem usgs-generic-dem-header) header-string &optional path)
  (with-slots (planimetric-reference-system-code horizontal-units
						 title x-dim y-dim planimetric-zone xy-units-per-meter z-units-per-meter
						 x-scale y-scale z-scale corners z-min z-max pathname)
      dem
    (let (pos horizontal-units-code vertical-units-code)
      (setq planimetric-reference-system-code (read-from-string header-string nil nil :start 156)
	    title (substring header-string 0 144)
	    x-dim (read-from-string header-string nil nil :start 858)
	    y-dim (read-from-string header-string nil nil :start 852)
	    planimetric-zone (read-from-string header-string nil nil :start 162)
	    horizontal-units-code (read-from-string header-string nil nil :start 528)
	    horizontal-units (case horizontal-units-code
			       ((0 3) :arc-seconds)
			       ((1 2) :meters)
			       (otherwise (error "Illegal horiz units code: ~D" horizontal-units-code)))
	    xy-units-per-meter (case horizontal-units-code
				 (0 *radians-per-arc-second*) ; radians
				 (1 *feet-per-meter*)
				 (2 1.0)
				 (3 1.0) ; arc-seconds
				 (otherwise (error "Unknown horizontal units code (byte 528-531) = ~a" horizontal-units-code)))
	    vertical-units-code (read-from-string header-string nil nil :start 534)
	    z-units-per-meter (case vertical-units-code
				(1 *feet-per-meter*)
				(2 1.0)
				(otherwise (error "Unknown vertical units code (byte 528-531) = ~a" vertical-units-code )))
	    pos 816
	    x-scale (/ (read-next header-string pos 12) xy-units-per-meter)
	    y-scale (/ (read-next header-string pos 12) xy-units-per-meter)
	    z-scale (/ (read-next header-string pos 12) z-units-per-meter)
	    corners (loop with pos = 546
			  repeat 4
			  collect (list (/ (read-next header-string pos) xy-units-per-meter)
					(/ (read-next header-string pos) xy-units-per-meter)))
	    pos 738
	    z-min (/ (read-next header-string pos 24) z-units-per-meter)
	    z-max (/ (read-next header-string pos 24) z-units-per-meter)
	    pathname path
	    )
      (case planimetric-reference-system-code ;WMT
	((0 1) nil)			;We've done these
	(2 (cerror "Continue with this reference-system anyway"
		   "Haven't ever handled State Plane systems before"))
	(otherwise (cerror "Continue with this reference-system anyway"
			   "Haven't ever handled planimetric-system-code ~D before" planimetric-reference-system-code)))
      (unless (eql 0.0 (read-from-string header-string nil nil :start 786)) ;WMT
	(cerror "Continue with this orientation"
		"Angle from the primary axis of ground planimetric ref. to DEM local ref. is not 0"))
      dem
      )))

(defmethod dem-info ((dem usgs-generic-dem-header))
  (with-slots (x-scale y-scale z-scale planimetric-reference-system-code planimetric-zone x-min y-min z-origin) dem
    (unless (and (slot-boundp dem 'x-min) (slot-boundp dem 'y-min))
      (compute-dem-bounding-box dem))
    (list (case planimetric-reference-system-code
	    (1 :utm-transform)
	    (0 :geographic-transform)
	    (2 :state-plane-transform))
	  planimetric-zone x-scale x-min y-scale y-min z-scale z-origin)))

(defun make-dem-image-properties (dem-header-list x-min y-min)
  (list :usgs-dem-info
	(with-slots (x-scale y-scale z-scale z-origin
			     planimetric-reference-system-code planimetric-zone)
	    (first dem-header-list)
	  (list (case planimetric-reference-system-code
		  (1 :utm-transform)
		  (0 :geographic-transform)
		  (2 :state-plane-transform))
		planimetric-zone x-scale x-min y-scale y-min z-scale z-origin))
	:usgs-dem-header-string
	(loop for dem in dem-header-list collect (slot-value dem 'header-string))))

(defun set-dem-image-info (dem-header-list image x-min y-min)
  (loop for (key val) on (make-dem-image-properties dem-header-list x-min y-min)
	by #'cddr
	do (setf (get-prop image key) val))
  )

(defmethod compute-dem-bounding-box ((dem usgs-generic-dem-header) &optional (clip t))
  (with-slot-values (corners ) dem
    (loop for (x y) in corners
	  minimize x into xmin minimize y into ymin 
	  maximize x into xmax maximize y into ymax
	  finally (when clip
		    (with-slot-values (x-scale y-scale) dem
		      (setq xmin (* x-scale (ceiling xmin x-scale))
			    ymin (* y-scale (ceiling ymin y-scale))
			    xmax (* x-scale (floor xmax x-scale))
			    ymax (* y-scale (floor ymax y-scale)))
		      (with-slots (x-min x-max y-min y-max) dem
			(setf x-min xmin x-max xmax
			      y-min ymin y-max ymax)))
		    
		    (return (values xmin xmax ymin ymax))))))

(defmethod make-dem-image ((dem usgs-generic-dem-header))
  (with-slots (x-scale y-scale x-min y-min planimetric-reference-system-code
		       planimetric-zone header-string)
      dem
    (multiple-value-bind (x-min x-max y-min y-max) (compute-dem-bounding-box dem)
      (let* ((xdim (1+ (- (ceiling x-max x-scale) (ceiling x-min x-scale))))
	     (ydim (1+ (- (ceiling y-max y-scale) (ceiling y-min y-scale))))
	     (properties (make-dem-image-properties (list dem) x-min y-min))
	     (image (img::make-image (list xdim ydim)
				     :element-type 'single-float
				     :initial-element -0.0
				     :property-list properties)))
	(img::with-scan-line-buffers ((z-bufr (img::make-dfloat-scan-line-buffer image ydim)))
	  (progn		      ; img::unmake-images-on-abort (list image)
	    (read-elevations dem image x-min y-min z-bufr))
	  ;;(set-dem-image-info (list dem) image x-min y-min)
	  image)))))

(defmethod read-elevations
    ((dem usgs-generic-dem-header) image x-origin y-origin z-bufr &optional verbose)
  (setf (get-prop image :zeros-invalid) t)
  (with-slot-values (x-dim pathname x-scale y-scale) dem
    (with-open-file (stream pathname :direction :input)
      (let ((bufr (make-string 1024)))
	(loop for i from 0 below 1024 do (setf (aref bufr i) (read-char stream)))
	(loop repeat x-dim  
	      do (multiple-value-bind (x0 y0 n-cols n-rows col row)
		     (read-profile dem stream  z-bufr)
		   (ignore n-cols)
		   (when verbose
		     (format t "col= ~d, row= ~d, n-rows= ~d, x= ~d, y= ~d~%"
			     col row n-rows x0 y0))
		   (img::image-putcolumn image z-bufr
					(round (- x0 x-origin) x-scale)
					(round (- y0 y-origin) y-scale)
					n-rows)))))))

#||
(defmethod read-profile ((dem usgs-generic-dem-header) stream  z-bufr)
   (declare (type (simple-array double-float (*)) z-bufr))
   (with-slot-values (z-scale xy-units-per-meter z-units-per-meter) dem
     (declare (single-float z-scale))
     (let* ((row (read stream))
 	   (col (read stream))
 	   (n-rows (read stream))
 	   (n-cols (read stream))
 	   ;; x0, y0, z0 are coordinates of first pixel in profile -- these are exactly divisible by x-scale, y-scale ...
 	   (x0 (/ (read stream) xy-units-per-meter))
 	   (y0 (/ (read stream) xy-units-per-meter))
 	   (z0 (/ (float (read stream) 0.0) z-units-per-meter)) ; coerce to single-float
 	   (zmin (/ (read stream) z-units-per-meter))
 	   (zmax (/ (read stream) z-units-per-meter))
 	   )
       (declare (single-float z0))
       (ignore zmin zmax)
       (loop for i fixnum from 0 below n-rows
 	    for z fixnum = (quick-read-decimal stream)
 	    for mapped-z double-float = (+ z0 (* z z-scale))
 	    do (setf (aref z-bufr i) mapped-z))
       (values x0 y0 n-cols n-rows col row))))
||#


;;; Read-profiles was scaling the vertical units in the DEM to the site's
;;; vertical units.  Unfortunately, so was dtm transform.  As long as the
;;; DEM and sites were using meters as the vertical unit, no one noticed.
;;; The 10-meter USGS DEMs use feet as the vertical unit.
;;; This version leaves the vertical units alone and assumes that
;;; dtm-transform wiil handle the scaling.   heller Wed Jun  2 1999
(defmethod read-profile ((dem usgs-generic-dem-header) stream  z-bufr)
  (declare (type (simple-array double-float (*)) z-bufr))
  (with-slot-values (z-scale xy-units-per-meter z-units-per-meter) dem
    (declare (ignore z-scale))
    ;;(declare (double-float z-scale))
    (let* ((row (read stream))
	   (col (read stream))
	   (n-rows (read stream))
	   (n-cols (read stream))
	   ;; x0, y0, z0 are coordinates of first pixel in profile -- these are exactly divisible by x-scale, y-scale ...
	   (x0 (/ (read stream) xy-units-per-meter))
	   (y0 (/ (read stream) xy-units-per-meter))
	   ;; this can result in meters or feet for the vertical
	   ;; units, conversion is taken care of in the dtm-transform
	   ;; (z0 (/ (dfloat (read stream) 0.0) z-units-per-meter)) ;
	   ;; coerce to double-float
	   (z0 (float (read stream) 0.0)) ; coerce to double-float
	   (zmin (/ (read stream) z-units-per-meter))
	   (zmax (/ (read stream) z-units-per-meter))
	   )
      (declare (double-float z0))
      (ignore zmin zmax)
      (loop for i fixnum from 0 below n-rows
	    for z fixnum = (quick-read-decimal stream)
	    ;;for mapped-z double-float = (+ z0 (* z z-scale))
	    for mapped-z double-float = (+ z0  z)
	    do (setf (aref z-bufr i) mapped-z))
      (values x0 y0 n-cols n-rows col row))))


(defun clip-dem-image (dtm left bottom right top)
  (let ((clipped-dtm
	 (img::image-window dtm left bottom (1+ (- right left)) (1+ (- top bottom))))
	(dem-info (get-image-dem-info dtm)))
    (lcl::destructuring-bind (dem-type zone x-scale utm-x-left y-scale utm-y-bottom . rest) dem-info
      (setf (get-prop clipped-dtm :usgs-dem-info)
	    `(,dem-type ,zone ,x-scale ,(+ utm-x-left (* x-scale left ))
			,y-scale ,(+ utm-y-bottom (* y-scale bottom ))
			. ,rest))
      clipped-dtm)))

(defun get-dem-hdrs (path-prefix dem-names)
  (loop for dem-name in dem-names
	for path = (format nil path-prefix dem-name)
	collect (read-usgs-dem-header path)))

(defun mosaic-dems (&rest dem-header-list)
  (loop for dem-header in dem-header-list
	with xmin and xmax and ymin and ymax
	do (multiple-value-setq (xmin xmax ymin ymax)  (compute-dem-bounding-box dem-header))
	minimize xmin into x-min minimize ymin into y-min 
	maximize xmax into x-max maximize ymax into y-max
	finally
     (let* ((first-dem (car dem-header-list))
	    (x-scale (x-scale first-dem))
	    (y-scale (y-scale first-dem))
	    (xdim (1+ (- (ceiling x-max x-scale) (ceiling x-min x-scale))))
	    (ydim (1+ (- (ceiling y-max y-scale) (ceiling y-min y-scale))))
	    (image (img::make-image (list xdim ydim) :element-type 'single-float :initial-element -0.0))
	    )
       (img::with-scan-line-buffers ((z-bufr (img::make-dfloat-scan-line-buffer image ydim)))
	 (progn			       ; ic::unmake-images-on-abort (list image)
	   (loop for dem-header in dem-header-list
		 do
	      (format t "~&;;; Working on ~a" (title dem-header))
	      (read-elevations dem-header image x-min y-min z-bufr)))
	 (set-dem-image-info dem-header-list image x-min y-min)
	 (return image)))))



(defclass usgs-utm-dem-header
     (usgs-generic-dem-header)
     ())


#|
(defmethod make-planimetric-to-geocentric-transform
    ((dem-header usgs-utm-dem-header) &optional lat-long-to-geocentric-transform)
  (unless lat-long-to-geocentric-transform
    (setq lat-long-to-geocentric-transform transforms::*default-lat-long-to-geocentric-transform*))
  (let* ((utm-zone (planimetric-zone dem-header))
	 (utm-zone-coordinate-system
	  (make-utm-coordinate-system (central-meridian-from-utm-zone utm-zone)
				      (from-coordinate-system lat-long-to-geocentric-transform ))))
    (make-composite-coordinate-transform
     (list (transforms::make-utm-to-lat-long-transform utm-zone-coordinate-system)
	   lat-long-to-geocentric-transform )
     )))
|#

(defvar *default-usgs-dem-lat-long-coordinate-system* (get-gdc 'nad27))

(in-package :transforms)

(st::add-system-initialization :terrain-models 
			       '(setq cme::*default-usgs-dem-lat-long-coordinate-system* (gdc :nad27)))

(in-package :cme)

(defmethod make-planimetric-to-geocentric-transform
    ((dem-header usgs-utm-dem-header) &optional lat-long-to-geocentric-transform)
  (unless lat-long-to-geocentric-transform 
    (setq lat-long-to-geocentric-transform 
	  (to-geocentric-transform *default-usgs-dem-lat-long-coordinate-system*)))
  (let* ((utm-zone (planimetric-zone dem-header)))
    (make-composite-coordinate-transform
     (list (transforms::make-utm-zone-to-lat-long-transform utm-zone lat-long-to-geocentric-transform)
	   lat-long-to-geocentric-transform )
     )))


(defmethod make-dem-coordinate-system ((dem-header usgs-utm-dem-header) name )
  (with-slots (x-scale y-scale z-scale) dem-header
    (make-coordinate-system name 3
			    :component-names '("i" "j" "h")
			    :component-units
			    (list (format nil "~d*meters" x-scale)
				  (format nil "~d*meters" y-scale)
				  (format nil "~d*meters" z-scale))
			    :property-list `(:dem-info ,(dem-info dem-header)
					     :component-units-in-meters ,(list x-scale y-scale z-scale)
					     )
			    )))

(defclass usgs-arcsecond-dem-header
     (usgs-generic-dem-header)
     ())

(defmethod make-planimetric-to-geocentric-transform
    ((dem-header usgs-arcsecond-dem-header) &optional lat-long-to-geocentric-transform)
  (or lat-long-to-geocentric-transform (to-geocentric-transform (get-gdc 'wgs72))))

(defmethod make-dem-coordinate-system ((dem-header usgs-arcsecond-dem-header) name )
  (with-slots (x-scale y-scale z-scale) dem-header
    (make-coordinate-system name 3
			    :component-names '("i" "j" "h")
			    :component-units
			    (list (format nil "~d*arcseconds" x-scale)
				  (format nil "~d*arcseconds" y-scale)
				  (format nil "~d*meters" z-scale))
			    :property-list `(:dem-info ,(dem-info dem-header))
			    )))


(defun usgs-dem-to-planimetric-system-matrix (dem-info)
  (lcl::destructuring-bind (dem-cooordinate-system-name zone-code long-scale long-origin lat-scale lat-origin &optional z-scale z-origin)
      dem-info
    (ignore dem-cooordinate-system-name zone-code)
    (let ((matrix (make-and-fill-4x4-matrix long-scale 0.0 0.0 long-origin
					    0.0 lat-scale 0.0 lat-origin
					    0.0 0.0 (or z-scale 1.0) (or z-origin 0.0)
					    0.0 0.0 0.0 1.0)))
      (case dem-cooordinate-system-name
	(:geographic-transform (loop for i from 0 to 1
				      do (loop for j from 0 to 3
					       do (setf (aref matrix i j) (/ (aref matrix i j) 3600.0))))) ; arcseconds to degrees
	(:utm-transform)
	(:state-plane-transform)
	)
      
      matrix)))

(defun make-usgs-dem-to-planimetric-system-transform
    (dem-coordinate-system
     planimetric-coordinate-system
     &key (dem-info (get-prop dem-coordinate-system :dem-info))) ; ***********************   
  (make-4x4-coordinate-transform
   (usgs-dem-to-planimetric-system-matrix dem-info)
   :from-coordinate-system dem-coordinate-system
   :to-coordinate-system planimetric-coordinate-system))

(defun make-usgs-terrain-model (dem-image geocentric-to-lvcs-transform dem-coordinate-system-name &key exact lat-long-to-geocentric-transform )
  (unless lat-long-to-geocentric-transform 
    (setq lat-long-to-geocentric-transform 
	  (to-geocentric-transform *default-usgs-dem-lat-long-coordinate-system*)))

  (let* ((dem-info (get-image-dem-info dem-image))
	 (dem-header (parse-usgs-dem-info dem-info))
	 (dem-coordinate-system (make-dem-coordinate-system dem-header dem-coordinate-system-name))
	 (planimetric-to-geocentric-transform
	  (make-planimetric-to-geocentric-transform dem-header lat-long-to-geocentric-transform ))
	 (dem-to-planimetric-transform
	  (make-usgs-dem-to-planimetric-system-transform
	   dem-coordinate-system
	   (from-coordinate-system planimetric-to-geocentric-transform)
	   :dem-info dem-info))
	 (geocentric-to-geocentric-transform
	  (and (neq (to-coordinate-system planimetric-to-geocentric-transform)
		    (from-coordinate-system geocentric-to-lvcs-transform))
	       (find-coordinate-transform (to-coordinate-system planimetric-to-geocentric-transform)
					  (from-coordinate-system geocentric-to-lvcs-transform))))
	 (exact-dem-to-lvcs-transform
	  (make-composite-coordinate-transform (list* dem-to-planimetric-transform
						      planimetric-to-geocentric-transform
						      (if geocentric-to-geocentric-transform
							  (list geocentric-to-geocentric-transform
								geocentric-to-lvcs-transform)
							  (list geocentric-to-lvcs-transform)))))
	 (terrain-model
	  (cme::make-terrain-model dem-image
				   (if exact
				       exact-dem-to-lvcs-transform
				       (solve-dtm-to-lvcs-transform
					dem-image exact-dem-to-lvcs-transform
					;;:template-polynomial-transform pt-quadratic-3d-to-3d
					;;:sampling-info '(:uniform 7 7 5)
					))))
	 )
    (setf (get-prop terrain-model :exact-dem-to-lvcs-transform) exact-dem-to-lvcs-transform)
    (make-terrain-model-3d-to-2d-projection terrain-model :exact exact)
    terrain-model
    ))


(defun find-coordinate-transform (from-coordinate-system to-coordinate-system)
  (find-transform-to from-coordinate-system to-coordinate-system))


;;;(defun make-terrain-model-3d-to-2d-projection (terrain-model &key (exact nil))
;;;  (let* ((image (dtm terrain-model))
;;;	 (dem-to-lvcs-transform (dtm-frame terrain-model)))
;;;    (setf (image-prop image :3D-TO-2D-PROJECTION)
;;;	  (make-composite-coordinate-projection
;;;	   (list (if exact
;;;		     (or (inverse-transform (get-prop terrain-model :exact-dem-to-lvcs-transform))
;;;			 (inverse-transform dem-to-lvcs-transform))
;;;		     (inverse-transform dem-to-lvcs-transform))
;;;		 
;;;		 (make-3d-to-2d-coordinate-projection
;;;		  :from-coordinate-system (from-coordinate-system dem-to-lvcs-transform)))))))


;;;(defun make-terrain-model-3d-to-2d-projection (terrain-model &key (exact nil))
;;;  (let* ((image (dtm terrain-model))
;;;	 (dem-to-lvcs-transform (dtm-frame terrain-model)))
;;;    (setf (image-prop image :3D-TO-2D-PROJECTION)
;;;	  (make-composite-coordinate-projection
;;;	   (list (if exact
;;;		     (or (inverse-transform (get-prop terrain-model :exact-dem-to-lvcs-transform))
;;;			 (inverse-transform dem-to-lvcs-transform))
;;;		     (inverse-transform dem-to-lvcs-transform))
;;;		 
;;;		 ;; with this projection, 
;;;		 (make-perspective-transform
;;;		  :CAMERA-TO-WORLD-MATRIX (make-4x4-identity-matrix)
;;;		  :1/F 0.0 :R/F -1.0
;;;		  :from-coordinate-system (from-coordinate-system dem-to-lvcs-transform)))))))

#|; not converted
;;; experimental Mon Aug  9 1993
(defun make-terrain-model-3d-to-2d-projection (terrain-model &key (exact nil))
  (let* ((image (dtm terrain-model))
	 (dem-to-lvcs-transform (dtm-frame terrain-model))
	 (dem-to-lvcs2 (if exact
			   (or (inverse-transform (get-prop terrain-model :exact-dem-to-lvcs-transform))
			       (inverse-transform dem-to-lvcs-transform))
			   (inverse-transform dem-to-lvcs-transform)))
	 (simple-projection (make-perspective-transform
			     :CAMERA-TO-WORLD-MATRIX (make-4x4-identity-matrix)
			     :1/F 0.0 :R/F -1.0
			     :positive-w-clip-plane 1e6
			     :from-coordinate-system (from-coordinate-system dem-to-lvcs-transform))) )

    (setf (image-prop image :3D-TO-2D-PROJECTION)
	  (if (linear-transform-p dem-to-lvcs2)
	      (compose-transforms dem-to-lvcs2 simple-projection)
	      (make-composite-coordinate-projection
	       (list dem-to-lvcs2 simple-projection))))))
|#

(defun make-terrain-model-3d-to-2d-projection (terrain-model &key (exact nil))
  nil)


;;; *************************  FITTING POLYNOMIAL TRANSFORMS  **************************

#| ; not converted
    
(defparameter *solve-dtm-to-lvcs-transform-default-template-polynomial-transform* pt-quadratic-3d-to-3d)

(defparameter *solve-dtm-to-lvcs-transform-default-sampling-info* '(:uniform 7 7 5))

(defun solve-dtm-to-lvcs-transform
    (dtm-image dtm-to-lvcs-transform
	       &key (sampling-info *solve-dtm-to-lvcs-transform-default-sampling-info*)
	       (template-polynomial-transform *solve-dtm-to-lvcs-transform-default-template-polynomial-transform*)
	       (inverse-template-transform template-polynomial-transform))
  (with-class-slots
      coordinate-transform (from-coordinate-system to-coordinate-system) dtm-to-lvcs-transform
      (let (zmin zmax wx wy)
	(if (consp dtm-image)
	    (setq zmin (first dtm-image) zmax (second dtm-image)
		  wx (third dtm-image) wy (fourth dtm-image))
	
	    (progn (multiple-value-setq (zmin zmax) (ic::image-element-min-max dtm-image))
		   (setq wx (image-x-dim dtm-image)
			 wy (image-y-dim dtm-image))))

	(3d-least-squares-fit-polynomial-transform-pair
	 template-polynomial-transform inverse-template-transform dtm-to-lvcs-transform
	 (cv 0.0 (float wx) 0.0 (float wy) zmin zmax)
	 sampling-info))))

(defun calibrate-map-image-uv-to-xy2 (template-polynomial-transform uv-stz-list stx-to-xy-transform
								    &key
								    (default-z 0.0)
								    (normalize-from-vector nil))
  (loop with from-vector-vector = (make-array (length uv-stz-list))
	with to-vector-vector = (make-array (length uv-stz-list))
	for i from 0
	for (u v x y z) in uv-stz-list
	do (setf (aref to-vector-vector i)
		 (if stx-to-xy-transform
		     (multiple-value-bind (x y) (transform-point stx-to-xy-transform x y (or z default-z))
		       (coordinate-vector x y))
		     (coordinate-vector x y )))
	   (setf (aref from-vector-vector i) (coordinate-vector u v))
	finally
     (return (least-squares-fit-polynomial-transform
	      (copy-polynomial-transform template-polynomial-transform
					 :from-coordinate-system
					 (and stx-to-xy-transform (from-coordinate-system stx-to-xy-transform))
					 :to-coordinate-system
					 (and stx-to-xy-transform (to-coordinate-system stx-to-xy-transform)))
	      from-vector-vector to-vector-vector
	      :normalize-from-vector normalize-from-vector)
	     )))

|#
