(in-package :gui)

#|
This file is intended (as much as possible) to be a drop-in replacement
for the C++ supported implementation of the function DISPLAY-IMAGE defined in display.lisp.

This implementation is (also) based on the FREEDIUS image block storage conventions 
inherited from the CME (aka RCDE) for BLOCKED-IMAGE-MIXIN.
        
The primary assumptions are:

  . images consist of an array of fixed sized blocks.

  . image pixels are ALWAYS referenced using LRBT (1st quadrant) indexing.

  . In the usual case when block-y-dim < 0, 

    .. pixels within the blocks are stored left-to-right, top-to-bottom (LRTB).
    .. blocks are also numbered LRTB

  . When block-y-dim >0, the pixel order and block order is LRBT.

  . In the simplest case of RASTER-IMAGES

    .. block-x-dim = image-x-dim
    .. block-y-dim = (- image-y-dim) for LRTB rasters  (the usual case for historical reasons).
                   = image-y-dim     for LRBT rasters


BUGS:  

Fri Feb 23 2007:  
 Crashes to LDB monitor with large paged images.

    GC lossage.  No transport function for object 0x581cc1ff
    LDB monitor
    ldb> 

    I suspect a GC problem wrt. the tile-array.  I will attempt to fix it
    by using foreign-vectors.  

    No luck.  I load a large TIFF paged image, scroll/zoom around in it. 
    Calling (gc :full t) triggers the problem.  

    Maybe I am overwriting the array bounds.

    Overzooming is not required to produce the bug.

|#

(eval-when (eval load compile)
(import '(qffi::make-stationary-array qffi::unmake-stationary-array))
)

(defvar *OPENGL_VERSION* nil)
(defvar *USE-GENERATE_MIPMAP* T)
(defvar *NON-POWER-OF-TWO-TEXTURES-OK* T)
(defvar *default-GL_UNPACK_ROW_LENGTH* 4)

(defun parse-gl-version-string (string)
  (let* ((dot1 (position #\. string))
	 (dot2 (position #\. string :start (1+ dot1))))
    (read-from-string string nil nil :end (or dot2 (position #\space string)))))

;(check-gl-extensions)
(defun check-gl-extensions ()
  (unless *opengl_version*
    (setq *opengl_version* (parse-gl-version-string (glGetString GL_VERSION)))
    ;;(setq *default-GL_UNPACK_ROW_LENGTH* (glGetInteger GL_UNPACK_ROW_LENGTH))

    (let ((extensions-string (glGetString GL_EXTENSIONS)))
      (flet ((check-gl-extension (extension-name &optional (missing-action :error))
	       (if (search extension-name extensions-string)
		   t
		   (case missing-action
		     (:error (format t "OpenGL extension ~a is not supported." extension-name)
			     nil)
		     (otherwise nil)))))
	(setq *USE-GENERATE_MIPMAP* (check-gl-extension "generate_mipmap"))
	(setq *NON-POWER-OF-TWO-TEXTURES-OK* (check-gl-extension "GL_ARB_texture_non_power_of_two" nil))
	(unless *NON-POWER-OF-TWO-TEXTURES-OK*
	  (setq *texmap-specs-force-power-of-2-tiles* t))
	))))

;;; This doesn't work because glGetString used the "current GL connection" which doesn't yet exist.
;;;(st::add-system-initialization :gl '(check-gl-extensions))

      
;;; *****************   Customization Variables  *****************

(defparameter *texmap-specs-force-power-of-2-tiles* #+(or agl cocoa win32) t #-(or agl cocoa win32) nil)

(defparameter *pyramid-level-for-texture-borders* -2) ; borders when overzooming 4x or more
;(setq *pyramid-level-for-texture-borders* -999) ; never use borders
;(setq *pyramid-level-for-texture-borders* 999) ; always use borders

;;(defparameter *force-texture-tile-dimensions* '(256 256))
(defparameter *force-texture-tile-dimensions* nil)

;;; *default-texture-unpack-alignment* has been eliminated.  See SET-DEFAULT-PIXEL-TRANSFER-PARAMETERS
;;; There might be a performance advantage setting this to 4
;;; Changing this from 1 will probably break things.
;;;(defparameter *default-texture-unpack-alignment* 1); glPixelStore GL_UNPACK_ALIGNMENT
;;(setq *default-texture-unpack-alignment* 4)


#|
*TexImage2D-format-info-alist* is an alist associating FREEDIUS image-element-types 
to triples of the form (texture-internal-format texture-format element-type)
as specified for glTexImage2d.
|#
;;; This allows OpenGL to select the internal format bits per component.
(defparameter *TexImage2D-format-info-alist-default*
  (let ((lum GL_LUMINANCE))
  `((img::rgb8 ,GL_RGB ,GL_RGB ,GL_UNSIGNED_BYTE)
    (img::rgba8 ,GL_RGBA ,GL_RGBA ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 8) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 16) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_SHORT)
    ((signed-byte 16) ,lum ,GL_LUMINANCE ,GL_SHORT)
    (single-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    (double-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    ((signed-byte 8) ,lum ,GL_LUMINANCE ,GL_BYTE)
    ((signed-byte 32) ,lum ,GL_LUMINANCE ,GL_INT)
    ((unsigned-byte 32) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_INT))))

;;; This specifically asks for 8-bits per component internal formats.
(defparameter *TexImage2D-format-info-alist-8bit*
  (let ((lum GL_LUMINANCE8))
  `((img::rgb8 ,GL_RGB8 ,GL_RGB ,GL_UNSIGNED_BYTE)
    (img::rgba8 ,GL_RGBA8 ,GL_RGBA ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 8) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 16) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_SHORT)
    ((signed-byte 16) ,lum ,GL_LUMINANCE ,GL_SHORT)
    (single-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    (double-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    ((signed-byte 8) ,lum ,GL_LUMINANCE ,GL_BYTE)
    ((signed-byte 32) ,lum ,GL_LUMINANCE ,GL_INT)
    ((unsigned-byte 32) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_INT))))


;; Experiment to see the effects of reduced numbers of bits per component in internal formats.
;; Can't see any visible degradation from these.
;; Apparently NVidia always uses 8 or more bits/component internally?
;; No, see PROBE-TEXTURE-MAP-INTERNAL-FORMAT at the end of the file.
(defparameter *TexImage2D-format-info-alist-minbits*
  (let ((lum GL_LUMINANCE4))
  `((img::rgb8 ,gl::GL_R3_G3_B2 ,GL_RGB ,GL_UNSIGNED_BYTE)
    (img::rgba8 ,gl::GL_RGB5_A1 ,GL_RGBA ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 8) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 16) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_SHORT)
    ((signed-byte 16) ,lum ,GL_LUMINANCE ,GL_SHORT)
    (single-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    (double-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    ((signed-byte 8) ,lum ,GL_LUMINANCE ,GL_BYTE)
    ((signed-byte 32) ,lum ,GL_LUMINANCE ,GL_INT)
    ((unsigned-byte 32) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_INT))))

#+never ;;#+(or agl cocoa)
(defparameter *TexImage2D-format-info-alist-default*
  (let ((lum GL_RGB8))
  `((img::rgb8 ,GL_RGB8 ,GL_RGB ,GL_UNSIGNED_BYTE)
    (img::rgba8 ,GL_RGBA8 ,GL_RGBA ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 8) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 16) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_SHORT)
    ((signed-byte 16) ,lum ,GL_LUMINANCE ,GL_SHORT)
    (single-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    (double-float ,lum ,GL_LUMINANCE ,GL_FLOAT)
    ((signed-byte 8) ,lum ,GL_LUMINANCE ,GL_BYTE)
    ((signed-byte 32) ,lum ,GL_LUMINANCE ,GL_INT)
    ((unsigned-byte 32) ,lum ,GL_LUMINANCE ,GL_UNSIGNED_INT))))


(defparameter *TexImage2D-format-info-alist* *TexImage2D-format-info-alist-default*)
;;;(setq *TexImage2D-format-info-alist* *TexImage2D-format-info-alist-8bit*)
;;;(setq *TexImage2D-format-info-alist* *TexImage2D-format-info-alist-minbits*)

;;; *****************  Debugging Variables  *****************

(defparameter *get-tile-pixels-opt-force-image-get-rectangle* nil)
;; (setq *get-tile-pixels-opt-force-image-get-rectangle* t) ; for debugging only

(defparameter *inhibit-destroy-image-texture-maps* nil)
;(setq *inhibit-destroy-image-texture-maps* t)

(defparameter *force-texmap-reload* nil)
;;(setq *force-texmap-reload* t) ; for debugging only

(defparameter *show-borders* nil)
;(setq *show-borders* t)

(defparameter *get-tile-pixels-verbose* nil)
;(setq *get-tile-pixels-verbose* t)

;;; ______________________________________________________

;;; TEXMAP-SPECS contains the texture map info for a single level of an image pyramid.

;;; FIXME:  rename class to something better
(defstruct-class texmap-specs (base-struct-class)
  ((image :initarg :image)  ; The image at the specified pyramid level
   (border :initform 0 :initarg :border)
   (pyramid-level :initarg :pyramid-level)
   (tile-xdim :initform nil :initarg :tile-xdim)
   (padded-tile-xdim :initform 0)
   (tile-ydim :initform nil :initarg :tile-ydim)
   (tiles-wide :initform nil :initarg :tiles-wide)
   (tiles-hi :initform nil :initarg :tiles-hi)
   texids
   (comp-eltype :initform nil)
   (ncomps-per-pixel :initform nil)
   (pixels-arr :initform nil)
   ;; oops status needs to be per tile, not one per image.
   (status :initform :released) ; t means tile is filled; :released means tile is not filled.
   ))

(defmethod pyramid-level-needs-borders (pyramid-level)
  (and pyramid-level (<= pyramid-level *pyramid-level-for-texture-borders*)))

(defmethod pyramid-level-needs-borders ((o texmap-specs))
  (with-class-slot-values texmap-specs (pyramid-level tiles-wide tiles-hi) o
    (and (pyramid-level-needs-borders pyramid-level)
	 (not (and (= tiles-wide 1) (= tiles-hi 1))))))

;(pixel-parameters (view-image (top-view)))

;;; underlying number of pixel components (bands) and component element-type.
(defmethod pixel-parameters (image)
  (let ((pixel-type (image-element-type image)))
    (case pixel-type
      (img::rgb8 (values 3 '(unsigned-byte 8)))
      (img::rgba8 (values 4 '(unsigned-byte 8)))
      ((single-float double-float) (values 1 'single-float))
      (otherwise 
       (if (and (consp pixel-type)
		(or (eq (car pixel-type) 'unsigned-byte) 
		    (eq (car pixel-type) 'signed-byte)))
	   (values 1 pixel-type)
	   (error "unimplemented pixel type ~a" pixel-type))))))


;;; underlying number of pixel components (bands) and component element-type.
;;; new (Wed Jun  6 2007) version that also handles double-float images
(defmethod pixel-parameters (image)
  (let ((pixel-type (image-element-type image)))
    (case pixel-type
      (img::rgb8 (values 3 '(unsigned-byte 8)))
      (img::rgba8 (values 4 '(unsigned-byte 8)))
      ((single-float double-float) (values 1 pixel-type))
      (otherwise 
       (if (and (consp pixel-type)
		(or (eq (car pixel-type) 'unsigned-byte) 
		    (eq (car pixel-type) 'signed-byte)))
	   (values 1 pixel-type)
	   (error "unimplemented pixel type ~a" pixel-type))))))




;;; This always uses image block-dims for the tile-dims, which might lead to
;;; performance problems with some graphics boards (chips).
#+old
(defmethod initialize-instance :after ((o texmap-specs) &key)
  (with-class-slots texmap-specs 
      (image tile-xdim padded-tile-xdim tile-ydim tiles-wide tiles-hi 
	     texids comp-eltype ncomps-per-pixel pyramid-level)
      o
    (setq tile-xdim (image-block-x-dim image)
	  padded-tile-xdim (img::image-padded-block-x-dim image)
	  tile-ydim (abs (image-block-y-dim image))
	  tiles-wide (ceiling (image-x-dim image) tile-xdim)
	  tiles-hi (ceiling (image-y-dim image) tile-ydim))
    (setf (texmap-specs-border o) (if (pyramid-level-needs-borders o) 2 0))
    (mv-setq (ncomps-per-pixel comp-eltype) (pixel-parameters image))
    (setf texids (make-array (* tiles-wide tiles-hi) 
			     :element-type '(unsigned-byte 32) :initial-element 0))
    ))

;;;(destroy-all-image-texture-maps)
;;; *last-texmap-spec*
;;; (setq *print-array* nil)

(defmethod initialize-instance :after ((o texmap-specs) &key)
  (with-class-slots texmap-specs 
      (image tile-xdim padded-tile-xdim tile-ydim tiles-wide tiles-hi 
	     texids comp-eltype ncomps-per-pixel pyramid-level border)
      o
    (setq tile-xdim (image-block-x-dim image)
	  padded-tile-xdim (img::image-padded-block-x-dim image)
	  tile-ydim (abs (image-block-y-dim image))
	  tiles-wide (ceiling (image-x-dim image) tile-xdim)
	  tiles-hi (ceiling (image-y-dim image) tile-ydim)
	  border (if (pyramid-level-needs-borders o) 2 0))
 
    (let ((2bdr (+ border border)))
      (when (and *texmap-specs-force-power-of-2-tiles*
		 (not (and (lx::^2p (+ 2bdr tile-xdim)) (lx::^2p (+ 2bdr tile-ydim)))))
	(flet ((pad-to-power-of-2 (n) (ash 1 (log2 n))))
	  ;; Force the bordered tile dimensions to a power of 2 
	  (setq tile-xdim (- (pad-to-power-of-2 (+ tile-xdim 2bdr)) 2bdr)
		padded-tile-xdim tile-xdim
		tile-ydim (- (pad-to-power-of-2 (+ tile-ydim 2bdr)) 2bdr)
		tiles-wide (ceiling (image-x-dim image) tile-xdim)
		tiles-hi (ceiling (image-y-dim image) tile-ydim)
		;;border (if (pyramid-level-needs-borders o) 2 0)
		))))
    
    (mv-setq (ncomps-per-pixel comp-eltype) (pixel-parameters image))
    (setf texids (make-array (* tiles-wide tiles-hi) 
			     :element-type '(unsigned-byte 32) :initial-element 0))
    ))

(defparameter *use-stationary-pixels-arrays* nil)

(defmethod destroy ((o texmap-specs))
  (with-class-slots texmap-specs (texids pixels-arr) o
    (glDeleteTextures (length texids) texids)
    (when pixels-arr
      (when *use-stationary-pixels-arrays*
	  (unmake-stationary-array pixels-arr))  ;; not normally called
      (setq pixels-arr nil))
    nil))

#+experiment
(defmethod destroy ((o texmap-specs))
  (with-class-slots texmap-specs (texids pixels-arr) o
    (glDeleteTextures (length texids) texids)
    (when pixels-arr
     ; (unmake-stationary-array pixels-arr)
      (setq pixels-arr nil))
    nil))

;;; **************************  *ACTIVE-IMAGE-TEXMAP-SPECS*  **************************

#|
*ACTIVE-IMAGE-TEXMAP-SPECS* contains the current state of texture map allocation.
It consists of a list of the form:

    ((base-image-1 (pyramid-level-1-i . texmap-spec-1-i) (pyramid-level-1-j . texmap-spec-1-j) ...)
     (base-image-2 (pyramid-level-2-i . texmap-spec-2-i) (pyramid-level-2-j . texmap-spec-2-j) ...)
     ...)
|#

(defvar *ACTIVE-IMAGE-TEXMAP-SPECS* nil)

#|
PYRAMID-LEVEL-KEY returns a key which distinguishes the texmap-specs entries for an image pyramid.  
Except for the overzoomed situation, it returns the argument "pyramid-level".
For overzoomed images there are 2 cases:
  . minor overzooming without needing texture-map borders returns pyramid-level-key = 0
  . major overzooming with texture-map borders returns pyramid-level-key = *pyramid-level-for-texture-borders*.   
|#

(defun pyramid-level-key (pyramid-level)
  (cond ((>= pyramid-level 0)
	 pyramid-level)
	((> pyramid-level *pyramid-level-for-texture-borders*)
	 0)
	(t *pyramid-level-for-texture-borders*)))

(defun get-image-texmap-specs (base-image image &key pyramid-level (create t))
  (setq base-image (image-for-display base-image))
  (let* ((key (pyramid-level-key pyramid-level ))
	 (pyramid-entry (assoc base-image *active-image-texmap-specs*)))
    (or (and pyramid-entry
	     (cdr (assoc key (cdr pyramid-entry))))
	(when create
	  (let ((specs (make-instance 'texmap-specs :base-image base-image 
				      :image image
				      :pyramid-level pyramid-level)))
	    (if pyramid-entry
		(push (cons pyramid-level specs) (cdr pyramid-entry))
		(push (list base-image (cons pyramid-level specs)) *active-image-texmap-specs*))
	    specs)))))

(defun destroy-image-texture-maps (image)
  (unless *inhibit-destroy-image-texture-maps*
      (if (null image)
	  (destroy-all-image-texture-maps)
	  (gl::with-gl-locked
	    (let* ((base-image (img::top-of-image-pyramid image))
		   (pyramid-entry (assoc base-image *active-image-texmap-specs*)))
	      (when pyramid-entry
		(setq *active-image-texmap-specs*
		      (remove pyramid-entry *active-image-texmap-specs*))
		(loop for (pyramid-level . spec) in (cdr pyramid-entry)
		      do (destroy spec))))))))

(defun destroy-all-image-texture-maps ()
  (unless *inhibit-destroy-image-texture-maps*
    (gl::with-gl-locked
      (loop for pyramid-entry = (pop *active-image-texmap-specs*)
	    while pyramid-entry
	    do (loop for (pyramid-level . spec) in (cdr pyramid-entry)
		     do (destroy spec))))))

#| ;; this code is bogus -- doesn't really do anything

(defun release-image-texture-maps (image)
 ; (format t "release-image-texture-maps ~a~%" image)
  (if (null image)
      (release-all-image-texture-maps)
      (let* ((base-image (img::top-of-image-pyramid (image-for-display image)))
	     (pyramid-entry (assoc base-image *active-image-texmap-specs*)))
	#+never
	(unless (or pyramid-entry  (null *active-image-texmap-specs*))
	  (setq *foo* (list base-image image *active-image-texmap-specs*) )
	  (break)
	  )
	(when pyramid-entry
	  (loop for (pyramid-level . spec) in (cdr pyramid-entry)
		do (setf (texmap-specs-status spec) :released))))))
;(trace destroy-all-image-texture-maps destroy-image-texture-maps release-image-texture-maps release-all-image-texture-maps)
;(untrace)

(defun release-all-image-texture-maps ()
  (loop for pyramid-entry in *active-image-texmap-specs*
	do (loop for (pyramid-level . spec) in (cdr pyramid-entry)
		 do (setf (texmap-specs-status spec) :released))))

|#

(defvar img::*inhibit-release-image-textures* nil)

;;; Replacement version for this new DISPLAY-IMAGE implementation
;;; Originally defined in display.lisp
;;; FIXME:  This is supposed to release textures for the entire pyramid of image
(defun img::release-image-textures (image &optional (release-texid 1))
  (declare (ignore release-texid))
  (unless img::*inhibit-release-image-textures*
    (setq image (image-for-display image))
    ;; (format t "img::release-image-textures ~a~%" image)
    (if nil ;(= release-texid 0)
	(release-image-texture-maps image)
	(destroy-image-texture-maps image))))
	      
;;; Replacement version for this new DISPLAY-IMAGE implementation
;;; Originally defined in display.lisp
(defun img::release-image-pool-textures (img &optional (release-texid 1))
  (declare (ignore img release-texid))
  (unless img::*inhibit-release-image-textures*
    (if nil ;release-texid
	(release-all-image-texture-maps)
	(destroy-all-image-texture-maps))))



#|
(destroy-all-image-texture-maps)
(length *active-image-texmap-specs*)

(setq *print-array* nil)

*last-texmap-spec*

(* 184 119)
(trace gl::glTexImage2D-non-simple)
(untrace)
(let ((img  (texmap-specs-image *last-texmap-spec*)))
  (list (image-x-dim img)
	(img::image-block-x-dim img)
	(img::image-padded-block-x-dim img)
	(img::image-block-y-dim img)
	))

(let* ((specs (get-image-texmap-specs (image-for-display(view-image (top-view)))))
       (texid (aref (texmap-specs-texids specs) 0)))
  (glBindTexture GL_TEXTURE_2D texid)
  (loop for lev from 0 to 1
	collect (glGetTexLevelParameter GL_TEXTURE_2D lev GL_TEXTURE_WIDTH)))

|#

(defvar *allocate-texid-tmp-arr* nil)

(defun allocate-texid-tmp-arr ()
  ;; This must be done at runtime, otherwise CMUCL disksaves are broken.
  (or *allocate-texid-tmp-arr* 
      (setq *allocate-texid-tmp-arr*
	    (make-stationary-array 1 :element-type '(unsigned-byte 32)))))

(defun allocate-texid (texmap-specs tilenum)
  (declare (fixnum tilenum))
  (with-class-slot-values texmap-specs (texids) texmap-specs
    (declare (type (simple-array (unsigned-byte 32) (*)) texids))
    (let ((tmp-arr (allocate-texid-tmp-arr)))
      (glGenTextures 1 tmp-arr)
      (setf (aref texids tilenum) (aref tmp-arr 0)))))

;;; unused
(defun deallocate-texid (texmap-specs tilenum)
  (declare (fixnum tilenum))
  (with-class-slot-values texmap-specs (texids) texmap-specs
    (declare (type (simple-array (unsigned-byte 32) (*)) texids))
    (let ((texid (aref texids tilenum)))
      (when (> texid 0)
	(let ((tmp-arr (allocate-texid-tmp-arr)))
	  (setf (aref tmp-arr 0) texid)
	  (glDeleteTextures 1 tmp-arr)
	  (setf (aref texids tilenum) 0))))))

;;; **************************  LOAD-TEXTURE-TILE    DISPLAY-TEXTURE-TILE  ***************************

;;; (defun set-default-pixel-transfer-parameters (texmap-specs)
;;;   (with-class-slot-values texmap-specs (padded-tile-xdim tile-xdim border) texmap-specs
;;;     #+(or agl cocoa) (glPixelStorei GL::GL_UNPACK_CLIENT_STORAGE_APPLE 1)
;;;     (glPixelStorei GL_UNPACK_ALIGNMENT *default-texture-unpack-alignment*)
;;; ;    (glPixelStorei GL_UNPACK_ROW_LENGTH  (if (= border 0) padded-tile-xdim 0))
;;;     (glPixelStorei GL_UNPACK_ROW_LENGTH (if (= border 0) padded-tile-xdim (+ tile-xdim border border)))
;;;     (glHint gl::GL_GENERATE_MIPMAP_HINT GL_FASTEST)) ; GL_FASTEST GL_NICEST
;;;   )

(defun set-default-pixel-transfer-parameters (texmap-specs)
  (with-class-slot-values texmap-specs (padded-tile-xdim tile-xdim border) texmap-specs
    #+(or agl cocoa) (glPixelStorei GL::GL_UNPACK_CLIENT_STORAGE_APPLE 1)
    (let ((unpack-row-length (if (= border 0) padded-tile-xdim (+ tile-xdim border border))))
      ;; When #+(or agl cocoa), tile dimensions should be powers of 2 and hence unpack-row-length will 
      ;; be a multiple of 4 causing GL_UNPACK_ALIGNMENT to be 4.
      (glPixelStorei GL_UNPACK_ALIGNMENT (if (logtest unpack-row-length 3) 1 4))
      (glPixelStorei GL_UNPACK_ROW_LENGTH unpack-row-length))
    ;;(glHint gl::GL_GENERATE_MIPMAP_HINT GL_FASTEST) ; GL_FASTEST GL_NICEST
    ))

(defun set-default-texture-parameters (texmap-specs)
  (with-class-slot-values texmap-specs (border tiles-wide tiles-hi) texmap-specs
    (let ((clamp-mode (if (>= *OPENGL_VERSION* 1.2) GL_CLAMP_TO_EDGE GL_CLAMP)))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clamp-mode)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clamp-mode)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
      (when (>= *OPENGL_VERSION* 1.2)
	(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
	(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 1)
	;;(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 2))
	)
      ;; FIXME: Currently, there is no code to handle the *USE-GENERATE_MIPMAP* = NIL case.
      (when *USE-GENERATE_MIPMAP* (glTexParameteri GL_TEXTURE_2D GL_GENERATE_MIPMAP GL_TRUE))
      )))

#+experiment
(defun set-default-texture-parameters (texmap-specs)
  (with-class-slot-values texmap-specs (border tiles-wide tiles-hi) texmap-specs
    (let ((clamp-mode (if (>= *OPENGL_VERSION* 1.2) GL_CLAMP_TO_EDGE GL_CLAMP)))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clamp-mode)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clamp-mode)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_NEAREST)
      ;;(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST)
      (when (>= *OPENGL_VERSION* 1.2)
	(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
      ;(glTexParameteri GL_TEXTURE_2D gl::GL_TEXTURE_MAX_LEVEL 1)
	(glTexParameteri GL_TEXTURE_2D gl::GL_TEXTURE_MAX_LEVEL 2))

      (when *USE-GENERATE_MIPMAP* (glTexParameteri GL_TEXTURE_2D GL_GENERATE_MIPMAP GL_TRUE))
      )))

;;(get-TexImage2D-format-info (view-image (top-view)))
(defun get-TexImage2D-format-info (image)
  (let ((pixel-type (image-element-type image)))
    (values-list (cdr (or (assoc pixel-type *TexImage2D-format-info-alist* :test #'equal)
			  (error "unimplemented pixel type ~a" pixel-type))))))

(defun load-texture-tile (image x y tilenum texmap-specs)
  (declare (fixnum x y tilenum))
  (with-class-slot-values texmap-specs (texids status) texmap-specs
    (declare (type (simple-array (unsigned-byte 32) (*)) texids))
    (let ((texid (aref texids tilenum)))
      (declare (fixnum texid))
      (if (and (> texid 0) (eq status t) (not *force-texmap-reload*))
	  texid				; the tile is already loaded
	  (with-class-slot-values texmap-specs (tile-xdim tile-ydim border) texmap-specs
	    (declare (fixnum tile-xdim tile-ydim border))
	    (when (=  texid 0) (setq texid (allocate-texid texmap-specs tilenum)))
	    (glBindTexture GL_TEXTURE_2D texid)
	    (set-default-texture-parameters texmap-specs)
	    (mv-bind (internal-format tex-format tex-type) (get-TexImage2D-format-info image)
	      (let ((2bdr (+ border border))
		    (pixels (get-tile-pixels image texmap-specs x y tilenum)))
		(declare (fixnum 2bdr))
		(glTexImage2D-non-simple GL_TEXTURE_2D 0 internal-format 
					 (+ tile-xdim 2bdr) (+ tile-ydim 2bdr)
					 0 ;border
					 tex-format tex-type pixels))
	      #+never
	      (let ((chosen-format (glGetTexLevelParameter GL_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT)))
		(unless (eql chosen-format internal-format)
		  (format t "load-texture-tile internal format = ~a, requested ~a~%" 
			  chosen-format internal-format)))
	      )
	    ;;(glBindTexture GL_TEXTURE_2D 0) ; restore default texture  -- why?
	    (glFlush)
	    texid)))))

(defparameter *display-texture-tile-z* 1.0)
;(setq *display-texture-tile-z* .5)

(defun display-texture-tile (image x y tilenum texmap-specs)
  (declare (optimize (speed 3) (safety 1)))
  (declare (fixnum x y))
  (with-class-slot-values texmap-specs (tile-xdim tile-ydim border ) texmap-specs
    (declare (fixnum tile-xdim tile-ydim border))
    (let* ((texid (load-texture-tile image x y tilenum texmap-specs))
	   (bx tile-xdim)
	   (by tile-ydim)
	   (remx (- (image-x-dim image) x))
	   (nx (if (< remx bx) remx bx))
	   (ny by)
	   (image-by (image-block-y-dim image))
	   )
      (declare (fixnum bx by remx nx ny image-by))
      (when (< y 0)	   ; handle fractional tile at bottom of image
	(setq ny (+ by y)
	      y 0))
      (let* ((off -.5)			; pixel origin correction 
	     (x0 (+ (dfloat x) off)) (x1 (+ x0 (dfloat nx)))
	     (y0 (+ (dfloat y) off)) (y1 (+ y0 (dfloat ny)))
	     (z *display-texture-tile-z*)
	     (bx+2b (+ bx border border))
	     (by+2b (+ by border border))
	     (tx0 (/ (dfloat border) (dfloat bx+2b)))
	     (tx1 (+ tx0 (/ (dfloat nx) (dfloat bx+2b))))
	     (ty0 (/ (dfloat border) (dfloat by+2b)))
	     (ty1 (+ ty0 (/ (dfloat ny) (dfloat by+2b))))
	     )
	;; FIXME: lotsa dfloat consing - unavoidable without some new C code?
	;;(declare (double-float off tx0 tx1 ty0 ty1 x0 x1 y0 y1 z))
	(declare (double-float off))
	(when *show-borders* (decf x1 1.0d0) (decf y1 1.0d0))
	;;(format t "display-texture-tile ~a ~%" (list (list x y) tilenum texid (list x0 y0) (list x1 y1)))
	(glBindTexture GL_TEXTURE_2D texid)
	(glBegin GL_TRIANGLE_STRIP)
	(cond ((< image-by 0)
	       (glTexCoord2d tx0 ty1) (glVertex3d x0 y0 z) ; bottom-left
	       (glTexCoord2d tx0 ty0) (glVertex3d x0 y1 z) ; top-left 
	       (glTexCoord2d tx1 ty1) (glVertex3d x1 y0 z) 
	       (glTexCoord2d tx1 ty0) (glVertex3d x1 y1 z))
	      (t
	       (glTexCoord2d tx0 ty0) (glVertex3d x0 y0 z) ; bottom-left
	       (glTexCoord2d tx0 ty1) (glVertex3d x0 y1 z) ; top-left 
	       (glTexCoord2d tx1 ty0) (glVertex3d x1 y0 z) 
	       (glTexCoord2d tx1 ty1) (glVertex3d x1 y1 z)))
	       
	(glEnd)))))

#|
(setq *BREAK-ON-SIGNALS* t)
(setq *BREAK-ON-SIGNALS* nil)
|#

(defun apply-to-visible-texture-tiles (fn image image-bbox texmap-specs)
  (declare (type (dvector 4) image-bbox))
  (let* ((bx (texmap-specs-tile-xdim texmap-specs))
	 (by (texmap-specs-tile-ydim texmap-specs))
	 (tiles-wide (texmap-specs-tiles-wide texmap-specs))
	 (xdim (image-x-dim image))
	 (ydim (image-y-dim image))
	 (block-y-dim (image-block-y-dim image))
	 
	 )
    (declare (fixnum xdim ydim block-y-dim))
    (declare (type (integer 1 4096)  bx by))
    (bind-vector-elements (x0 x1 y0 y1) image-bbox
      (let ((xstart (* bx (floor x0 bx)))
	    (xlimit (floor x1)))
	 
	(if (< block-y-dim 0)
	    (loop with ystart fixnum = (* by (floor y0 by))
		  with yfract fixnum = (mod ydim by)
		  with yoff fixnum = (if (= yfract 0) 0 (- by yfract))
		  with ylimit fixnum = (+ (floor y1) yoff) ;this sucks -- hack for top-to-bottom flip of tiles
		  ;; iterate over rows of tiles from bottom to top
		  for y fixnum from ystart below ylimit by by 
		  ;; FIXME: assuming LRTB tile order and block-x-dim < 0.
		  for tile-ypos fixnum downfrom (floor (- ydim 1 ystart) by)
		  for start-tile-xpos fixnum = (floor xstart bx)
		  do (loop for x fixnum from xstart below xlimit by bx
			   for tile-number fixnum from (+ start-tile-xpos (* tile-ypos tiles-wide))
			   do (funcall fn x (- y yoff) tile-number)))
	  
	    (loop with ystart fixnum = (* by (floor y0 by))
		  with yfract fixnum = (mod ydim by)
		  with ylimit fixnum = (floor y1)
		  ;;initially (format t "apply-to-visible-texture-tiles LRBT~%")
		  for y fixnum from ystart below ylimit by by 
		  for tile-ypos fixnum from (floor ystart by)
		  for start-tile-xpos fixnum = (floor xstart bx)
		  do (loop for x fixnum from xstart below xlimit by bx
			   for tile-number fixnum from (+ start-tile-xpos (* tile-ypos tiles-wide))
			   do (funcall fn x y tile-number))))))))

;;; ***********************************  DISPLAY-IMAGE  ***********************************
	   
(defun display-image-texture-maps (image image-bbox texmap-specs)
  (glEnable GL_TEXTURE_2D)
  (set-default-pixel-transfer-parameters texmap-specs)
  (apply-to-visible-texture-tiles
   #'(lambda (x y tilenum) (display-texture-tile image x y tilenum texmap-specs))
   image image-bbox texmap-specs)
  (glDisable GL_TEXTURE_2D)) 

(defmethod window-bbox-in-image (image (image-to-window-transform t) window)
  (mv-bind (width height) (dimensions window)
    (bind-vector-elements (x0 x1 y0 y1)
	(transforms::transform-2d-bbox (inverse-transform image-to-window-transform)
				       (cv 0.0 (dfloat width) 0.0 (dfloat height)))
      (cv (max 0.0 x0) (min x1 (dfloat (1- (image-x-dim  image))))
	  (max 0.0 y0) (min y1 (dfloat (1- (image-y-dim  image))))))))

(defmethod window-bbox-in-image (image (image-to-window-matrix array) window)
  (window-bbox-in-image image (make-4x4-coordinate-transform image-to-window-matrix) window))
 
#|
(setq *inhibit-image-display* t)
(setq *inhibit-image-display* nil)
|#

(defparameter *last-texmap-spec* nil); for debugging only
(defvar *displayed-pyramid-level*)   ; for debugging only

;;; Replacement for the version original version defined in display.lisp
(defmethod display-image ((view view) base-image 2d-to-window-matrix
			  &rest args &key (max-mipmap-level 1)
			  (min-level *display-image-min-level*)
			  image-modulation-color
			  perspective-correction-hint
			  )
  (declare (ignorable args max-mipmap-level min-level min-level))
  (unless *inhibit-image-display*
    (check-gl-extensions)
    (mv-bind (img pyramid-level) 
	(select-display-image-image view base-image 2d-to-window-matrix)

      ;;(format t "*display-image-pyramid-level* = ~a~%" *display-image-pyramid-level*)
      ;;(format t "display-image~%")
      (when img
	(glFlush) (handle_gl_errors "display-image1~%")
	(glDisable GL_POLYGON_STIPPLE)
	;;(glHint GL_PERSPECTIVE_CORRECTION_HINT  GL_FASTEST) 
	(when perspective-correction-hint
	  (glHint GL_PERSPECTIVE_CORRECTION_HINT  perspective-correction-hint))
	(glDisable GL_DITHER)
	;; (glEnable GL_DITHER)
	(maybe-photometric-transform-for-display view img base-image)
	(glcolor (or image-modulation-color (image-modulation-color (display-attributes view))))
	(glMatrixMode GL_TEXTURE) (glLoadIdentity)
	(let* ((texmap-specs (get-image-texmap-specs base-image img :pyramid-level pyramid-level))
	       (image-to-2d-transform (image-to-2d-transform img))
	       (image-to-2d-matrix (and image-to-2d-transform (transform-matrix image-to-2d-transform)))
	       (image-bbox (window-bbox-in-image img
						 (if image-to-2d-transform
						     ;; (multiply-4x4-matrices )
						     (multiply-matrices 2d-to-window-matrix
									image-to-2d-matrix)
						     2d-to-window-matrix)
						 (view-window view)))
	       )
	  ;;(setq *last-texmap-spec* texmap-specs *displayed-pyramid-level* pyramid-level) ; for debugging
	  (set-2d-to-ndc-matrix 2d-to-window-matrix)
	  ;;(unless image-to-2d-matrix (format t "display-image: image missing image-to-2d-transform~%"))
	  (glMatrixMode GL_MODELVIEW) (glLoadIdentity) 
	  (when image-to-2d-matrix (glMultMatrixd_transposed image-to-2d-matrix))
	  (display-image-texture-maps img image-bbox texmap-specs)
	  (setf (texmap-specs-status texmap-specs) t)
	  )
	    
	(glFlush) (handle_gl_errors "display-image2~%")
	(glPixelStorei GL_UNPACK_ROW_LENGTH *default-GL_UNPACK_ROW_LENGTH*) ; reset to default state
	))))


#+never ; replaced by C++ versions in image-ffi.lisp
(progn 

;;; This should be more efficiently supported by C code.
(defun image-get-rectangle (image buf xleft ybot nx ny pbx)
  (when *get-tile-pixels-verbose*
    (format t "image-get-rectangle slow case ~%" ))
  ;; FIXME: For general usage there should be more bounds testing.
  (when (< ybot 0)
    (incf ny ybot)
    (setq ybot 0))
  (loop with comps-per-pixel fixnum = (pixel-parameters image)
	with comps-per-line fixnum = (* comps-per-pixel nx)
	with ystart = (min (+ ybot ny -1) (1- (image-y-dim image)))
	for y fixnum from ystart downto ybot ; top to bottom in tile
	for pixel-offset fixnum from 0 by pbx
	do (image-getline image buf xleft y comps-per-line pixel-offset))
  buf)

(defun image-get-rectangle-bordered (image buf xleft ybot nx ny pbx border)
  ;;(declare (optimize (speed 2) (safety 1) (debug 3)))
  (when *get-tile-pixels-verbose*
    (format t "image-get-rectangle-bordered slow case~%"))
  ;; FIXME: For general usage there should be more bounds testing.
  (when (< ybot 0)
    (incf ny ybot)
    (setq ybot 0))
  ;; fill the interior
  (loop with xdim-1 fixnum = (1- (image-x-dim image))
	with ydim-1 fixnum = (1- (image-y-dim image))
	with lbdr fixnum = (if (>= xleft border) border 0)
	with rbdr fixnum = (if (< (+ xleft nx border) xdim-1) border 0)
	;;with bbdr fixnum = (if (> ybot 0) border 0)
	;;with tbdr fixnum = (if (< (+ ybot ny 1) ydim-1) border 0)
	with nx+2b fixnum = (+ nx border border) ; width of tile including borders 
	with comps-per-pixel fixnum = (pixel-parameters image)
	with comps-per-line fixnum = (* comps-per-pixel (+ nx lbdr rbdr)) ; getline count
	with ystart = (min (+ ybot ny -1) ydim-1)
	with fill-lbdr = (= lbdr 0)
	with fill-rbdr = (= rbdr 0)
	for y fixnum from (+ ystart border) downto (- ybot border) ; top to bottom in tile plus borders
	for pixel-offset fixnum from (- border lbdr) by pbx ; nx+2b ; pbx
	do (image-getline image buf (- xleft lbdr) (max 0 (min y ydim-1))
		 comps-per-line pixel-offset)
	   ;; Finally, handle left and right edge conditions.
	   ;; This is complicated for vector-images because getcolumn isn't supported there. 
	   (cond ((= comps-per-pixel 1)	; scalar-image
		  (when fill-lbdr
		    (let ((pixel (aref buf pixel-offset)))
		      (setf (aref buf (- pixel-offset 1)) pixel
			    (aref buf (- pixel-offset 2)) pixel)))
		  (when fill-rbdr
		    (let* ((rt-pixel-offset (+ pixel-offset lbdr nx -1))
			   (pixel (aref buf rt-pixel-offset)))
		      (setf (aref buf (+ rt-pixel-offset 1)) pixel
			    (aref buf (+ rt-pixel-offset 2)) pixel))))
		 (t (format t "image-get-rectangle-bordered vector-image border fill~%")
		    (when fill-lbdr
		      (loop with nb fixnum = comps-per-pixel
			    for i fixnum from 0 below nb
			    for v fixnum =  (aref buf pixel-offset i)
			    do (setf (aref buf (- pixel-offset 1) i) v
				     (aref buf (- pixel-offset 2) i) v)))
		    (when fill-rbdr
		      (loop with rt-pixel-offset fixnum = (+ pixel-offset nx lbdr -1)
			    for i fixnum from 0 below comps-per-pixel
			    for v fixnum =  (aref buf pixel-offset i)
			    do (setf (aref buf (+ pixel-offset 1) i) v
				     (aref buf (+ pixel-offset 2) i) v)))
		  )))
  buf)

#+experimental ; the previous version looks wrong for vector-images and 
(defun image-get-rectangle-bordered (image buf xleft ybot nx ny pbx border)
  (declare (optimize (speed 2) (safety 3) (debug 3)))
  (when *get-tile-pixels-verbose*
    (format t "image-get-rectangle-bordered slow case~%"))
  ;; FIXME: For general usage there should be more bounds testing.
  (when (< ybot 0)
    (incf ny ybot)
    (setq ybot 0))
  ;; fill the interior
  (loop with xdim-1 fixnum = (1- (image-x-dim image))
	with ydim-1 fixnum = (1- (image-y-dim image))
	with lbdr fixnum = (if (>= xleft border) border 0)
	with xstart fixnum = (- xleft lbdr);
	with rbdr fixnum = (if (< (+ xleft nx border) xdim-1) border 0) ; <= ?
	;;with bbdr fixnum = (if (> ybot 0) border 0)
	;;with tbdr fixnum = (if (< (+ ybot ny 1) ydim-1) border 0)
	with nx+2b fixnum = (+ nx border border) ; width of tile including borders 
	with comps-per-pixel fixnum = (pixel-parameters image)
	with pix-per-line fixnum = (+ nx lbdr rbdr)
	with npix fixnum = (min pix-per-line (- (1+ xdim-1) xstart))
	with comps-per-line fixnum = (* comps-per-pixel pix-per-line) ; getline count
	with ystart = (min (+ ybot ny -1) ydim-1)
	with fill-lbdr = (= lbdr 0)
	with fill-rbdr = (= rbdr 0)
	;;with foo10 = (setq *foo10* (list pbx xdim-1 ydim-1 lbdr rbdr nx+2b pix-per-line ystart fill-lbdr fill-rbdr))
	for y fixnum from (+ ystart border) downto (- ybot border) ; top to bottom in tile plus borders
	for pixel-offset fixnum from (- border lbdr) by pbx ; nx+2b ; pbx
	do (image-getline image buf xstart (max 0 (min y ydim-1))
		 npix pixel-offset)
	   ;; Finally, handle left and right edge conditions.
	   ;; This is complicated for vector-images because getcolumn isn't supported there. 
	   (cond ((= comps-per-pixel 1)	; scalar-image
		  (when fill-lbdr
		    (let ((pixel (aref buf pixel-offset)))
		      (setf (aref buf (- pixel-offset 1)) pixel
			    (aref buf (- pixel-offset 2)) pixel)))
		  (when fill-rbdr
		    (let* ((rt-pixel-offset (+ pixel-offset npix -1))
			   (pixel (aref buf rt-pixel-offset)))
		      (setf (aref buf (+ rt-pixel-offset 1)) pixel
			    (aref buf (+ rt-pixel-offset 2)) pixel))))
		 (t (format t "image-get-rectangle-bordered vector-image border fill~%")
		    (when fill-lbdr
		      (loop with nb fixnum = comps-per-pixel
			    for i fixnum from 0 below nb
			    for v fixnum =  (aref buf pixel-offset i)
			    do (setf (aref buf (- pixel-offset 1) i) v
				     (aref buf (- pixel-offset 2) i) v)))
		    (when fill-rbdr
		      (loop with rt-pixel-offset fixnum = (+ pixel-offset npix -1)
			    for i fixnum from 0 below comps-per-pixel
			    for v fixnum =  (aref buf rt-pixel-offset i)
			    do (setf (aref buf (+ rt-pixel-offset 1) i) v
				     (aref buf (+ rt-pixel-offset 2) i) v)))
		  )))
  buf)

(defun image-get-rectangle-bordered (image buf xleft ybot nx ny pbx border)
  (when *get-tile-pixels-verbose*
    (format t "image-get-rectangle-bordered slow case~%"))
  ;; FIXME: For general usage there should be more bounds testing.
  (when (< ybot 0)
    (incf ny ybot)
    (setq ybot 0))
  ;; fill the interior
  (loop with top-to-bottom-p = (< (image-block-y-dim image) 0)
	with xdim-1 fixnum = (1- (image-x-dim image))
	with ydim-1 fixnum = (1- (image-y-dim image))
	with lbdr fixnum = (if (>= xleft border) border 0)
	with rbdr fixnum = (if (< (+ xleft nx border) xdim-1) border 0)
	;;with bbdr fixnum = (if (> ybot 0) border 0)
	;;with tbdr fixnum = (if (< (+ ybot ny 1) ydim-1) border 0)
	with nx+2b fixnum = (+ nx border border) ; width of tile including borders 
	with comps-per-pixel fixnum = (pixel-parameters image)
	with pix-per-line fixnum = (+ nx lbdr rbdr)
	with comps-per-line fixnum = (* comps-per-pixel pix-per-line) ; getline count
	with ytop = (min (+ ybot ny -1) ydim-1)
	with fill-lbdr = (= lbdr 0)
	with fill-rbdr = (= rbdr 0)
	repeat (+ (- ytop ybot) border border 1)
	for dy fixnum = (if top-to-bottom-p -1 1)
	for y fixnum from (if top-to-bottom-p (+ ytop border) (- ybot border)) by dy
	for pixel-offset fixnum from (- border lbdr) by pbx ; nx+2b ; pbx
	do (image-getline image buf (- xleft lbdr) (max 0 (min y ydim-1))
		 comps-per-line pixel-offset)
	   ;; Finally, handle left and right edge conditions.
	   ;; This is complicated for vector-images because getcolumn isn't supported there. 
	   (cond ((= comps-per-pixel 1)	; scalar-image
		  (when fill-lbdr
		    (let ((pixel (aref buf pixel-offset)))
		      (setf (aref buf (- pixel-offset 1)) pixel
			    (aref buf (- pixel-offset 2)) pixel)))
		  (when fill-rbdr
		    (let* ((rt-pixel-offset (+ pixel-offset lbdr nx -1))
			   (pixel (aref buf rt-pixel-offset)))
		      (setf (aref buf (+ rt-pixel-offset 1)) pixel
			    (aref buf (+ rt-pixel-offset 2)) pixel))))
		 (t (format t "image-get-rectangle-bordered vector-image border fill~%")
		    (when fill-lbdr
		      (loop with nb fixnum = comps-per-pixel
			    for i fixnum from 0 below nb
			    for v fixnum =  (aref buf pixel-offset i)
			    do (setf (aref buf (- pixel-offset 1) i) v
				     (aref buf (- pixel-offset 2) i) v)))
		    (when fill-rbdr
		      (loop with rt-pixel-offset fixnum = (+ pixel-offset nx lbdr -1)
			    for i fixnum from 0 below comps-per-pixel
			    for v fixnum =  (aref buf pixel-offset i)
			    do (setf (aref buf (+ pixel-offset 1) i) v
				     (aref buf (+ pixel-offset 2) i) v)))
		  )))
  buf)

) ; end progn ********************

(defmethod texmap-pixels-arr ((o texmap-specs))
  (with-class-slot-values texmap-specs (ncomps-per-pixel padded-tile-xdim tile-xdim tile-ydim comp-eltype border) o
    (let* ((2bdr (+ border border))
	   (npixels (* (+ padded-tile-xdim 2bdr) (+ tile-ydim 2bdr))) ; is this correct?
	   (dims (if (= ncomps-per-pixel 1) npixels (list npixels ncomps-per-pixel))))
      (with-class-slots texmap-specs (pixels-arr) o
	(or pixels-arr
	    (setf pixels-arr 
		  (if *use-stationary-pixels-arrays*
		      (make-stationary-array dims :element-type comp-eltype)
		      (make-array dims :element-type comp-eltype))))))))

;;; new (Wed Jun  6 2007) version that also handles double-float images
(defmethod texmap-pixels-arr ((o texmap-specs))
  (with-class-slot-values texmap-specs 
      (image ncomps-per-pixel padded-tile-xdim tile-xdim tile-ydim comp-eltype border) o
    (let* ((2bdr (+ border border))
	   (npixels (* (+ padded-tile-xdim 2bdr) (+ tile-ydim 2bdr))) ; is this correct?
	   (dims (if (= ncomps-per-pixel 1) npixels (list npixels ncomps-per-pixel)))
	   (buf-eltype comp-eltype))
      (when (eq (image-element-type image) 'double-float) (setq buf-eltype 'single-float))
      (with-class-slots texmap-specs (pixels-arr) o
	(or pixels-arr
	    (setf pixels-arr 
		  (if *use-stationary-pixels-arrays*
		      (make-stationary-array dims :element-type buf-eltype)
		      (make-array dims :element-type buf-eltype))))))))

#+experiment ;; trying to track down gc-lossage bug
(defmethod texmap-pixels-arr ((o texmap-specs))
  (with-class-slot-values texmap-specs (ncomps-per-pixel padded-tile-xdim tile-xdim tile-ydim comp-eltype border) o
    (let* ((2bdr (+ border border))
	   (npixels (+ 1024  ; no luck
		       (* (+ padded-tile-xdim 2bdr) (+ tile-ydim 2bdr)))))
      (with-class-slots texmap-specs (pixels-arr) o
	(or pixels-arr
	    (setf pixels-arr 
		  (if *use-stationary-pixels-arrays*
		      (make-stationary-array dims :element-type comp-eltype)
		      (make-array dims :element-type comp-eltype))))))))


;;; When the texture-map tiles exactly match the image tiles, then a
;;; single copy of the entire subimage is possible rather than
;;; multiple calls to image-getline.

#+old
(progn

(defmethod get-tile-pixels ((image img::paged-image) texmap-specs x0 y0 tilenum)
  (with-class-slot-values texmap-specs (tile-xdim tile-ydim padded-tile-xdim border) texmap-specs
    (let ((pbx (img::image-padded-block-x-dim image))
	  (buf-pbx (max (+ tile-xdim border border) padded-tile-xdim )))
      (if (and (img::image-bitbltable image)
	       (= border 0) (not *get-tile-pixels-opt-force-image-get-rectangle*)
	       (= pbx tile-xdim)
	       (= (abs (img::image-block-y-dim image)) tile-ydim))
	  (let ((page (aref (img::paged-image-block-map image) tilenum)))
	    (if (eql page 0)
		(progn (img::paged-image-iref-fault image tilenum)
		       (aref (img::paged-image-block-map image) tilenum))
		page))
	  (if (= border 0)
	      (image-get-rectangle image (texmap-pixels-arr texmap-specs) x0 y0 tile-xdim tile-ydim buf-pbx)
	      (image-get-rectangle-bordered image (texmap-pixels-arr texmap-specs)
					    x0 y0 tile-xdim tile-ydim buf-pbx border))))))


(defmethod get-tile-pixels ((image img::array-image) texmap-specs x0 y0 tilenum)
  (with-class-slot-values texmap-specs (tile-xdim tile-ydim padded-tile-xdim border) texmap-specs
    (let ((pbx (img::image-padded-block-x-dim image))
	  (bx (img::image-block-x-dim image))
	  (by (abs (img::image-block-y-dim image)))
	  (buf-pbx (max (+ tile-xdim border border) padded-tile-xdim ))) ; this sucks ---
      (cond ((and (img::image-bitbltable image)
		  (= border 0)  (not *get-tile-pixels-opt-force-image-get-rectangle*)
		  ;;(= bx tile-xdim) 
		  (= pbx tile-xdim)  ; using UNPACKING_ROW_LENGTH = pbx
		  (= by tile-ydim))
	     ;;(format t "get-tile-pixels-opt :displaced-to~%")
	     ;; This fails if IMAGE is double-float.
	     (make-array (* tile-xdim tile-ydim) :element-type (texmap-specs-comp-eltype texmap-specs)
			 :displaced-to (img::array-image-array image)
			 :displaced-index-offset (* tilenum pbx by)))
	    ((= border 0)
	     (image-get-rectangle image (texmap-pixels-arr texmap-specs)
				  x0 y0 tile-xdim tile-ydim buf-pbx))
	    (t (image-get-rectangle-bordered image (texmap-pixels-arr texmap-specs)
					     x0 y0 tile-xdim tile-ydim buf-pbx border))))))

) ; end #+old progn

;;; new (Wed Jun  6 2007) version that also handles double-float images
(defmethod get-tile-pixels ((image img::paged-image) texmap-specs x0 y0 tilenum)
  (with-class-slot-values texmap-specs (tile-xdim tile-ydim padded-tile-xdim border comp-eltype) 
      texmap-specs
    (let ((pbx (img::image-padded-block-x-dim image))
	  (buf-pbx (max (+ tile-xdim border border) padded-tile-xdim )))
      (if (and (img::image-bitbltable image)
	       (= border 0) (not *get-tile-pixels-opt-force-image-get-rectangle*)
	       (= pbx tile-xdim)
	       (= (abs (img::image-block-y-dim image)) tile-ydim)
	       (not (eq comp-eltype 'double-float)))
	  (let ((page (aref (img::paged-image-block-map image) tilenum)))
	    (if (eql page 0)
		(progn (img::paged-image-iref-fault image tilenum)
		       (aref (img::paged-image-block-map image) tilenum))
		page))
	  (if (= border 0)
	      (image-get-rectangle image (texmap-pixels-arr texmap-specs) x0 y0 tile-xdim tile-ydim buf-pbx)
	      (image-get-rectangle-bordered image (texmap-pixels-arr texmap-specs)
					    x0 y0 tile-xdim tile-ydim buf-pbx border))))))


;;; new (Wed Jun  6 2007) version that also handles double-float images
(defmethod get-tile-pixels ((image img::array-image) texmap-specs x0 y0 tilenum)
  (with-class-slot-values texmap-specs (tile-xdim tile-ydim padded-tile-xdim border ncomps-per-pixel comp-eltype) 
      texmap-specs
    (let ((pbx (img::image-padded-block-x-dim image))
	  ;;(bx (img::image-block-x-dim image))
	  (by (abs (img::image-block-y-dim image)))
	  (buf-pbx (max (+ tile-xdim border border) padded-tile-xdim ))) ; this sucks ---
      (cond ((and (img::image-bitbltable image)
		  (= border 0)  (not *get-tile-pixels-opt-force-image-get-rectangle*)
		  ;;(= bx tile-xdim) 
		  (= pbx tile-xdim) ; using UNPACKING_ROW_LENGTH = pbx
		  (= by tile-ydim)
		  (not (eq comp-eltype 'double-float))
		  )
	     ;;(format t "get-tile-pixels-opt :displaced-to~%")
	     ;; creating a displaced array could be avoided if array-offset were returned and 
	     ;; glTexImage2D-non-simple accepted an pixels-offset argument.
	     (make-array (* ncomps-per-pixel tile-xdim tile-ydim) 
			 :element-type comp-eltype
			 :displaced-to (img::array-image-array image)
			 :displaced-index-offset (* tilenum pbx by ncomps-per-pixel)))
	    ((= border 0)
	     (image-get-rectangle image (texmap-pixels-arr texmap-specs)
				  x0 y0 tile-xdim tile-ydim buf-pbx))
	    (t (image-get-rectangle-bordered image (texmap-pixels-arr texmap-specs)
					     x0 y0 tile-xdim tile-ydim buf-pbx border))))))


#|
(img::image-block-y-dim *img*)
(img::image-padded-block-x-dim *img*)
Tests

(create-directory "/tmp/CME_TMP_PIX")

(setq *img8* (view-image (top-view)))


(setq *img16* (img::image-linear-transform *img8* 256.0 0.0 '(unsigned-byte 16)))

(push-image *img16* (selected-window))

(setq *img16-win* (img::image-window *img16*  100 100 300 600))

(push-image *img16-win* (selected-window))

(setq *img-sf* (img::image-linear-transform *img8* (/ 256.0) 0.0 'single-float))

(push-image *img-sf* (selected-window))

(setq *img-sf-win* (img::image-window *img-sf*  100 100 300 600))

(push-image *img-sf-win* (selected-window))

(setq *img-sf-rot* (img::image-rottate *img-sf*))
(push-image *img-sf-rot* (selected-window))


(img::image-bitbltable *img-sf*)
(img::image-bitbltable *img16-win*)


(let ((x 10) (y 10)) (list (img::diref *img8* x y) (img::diref *img-sf* x y)))

(/ 11.0 0.04296875)

(pop-view (selected-window))

(defparameter *max-drag-pixels* (* 1600 1200)) ; full screen 
(defparameter *max-drag-pixels* (* 800 800))
(defparameter *max-drag-pixels* (* 600 600))
(dimensions (selected-window))
(display-image-reduce-level (selected-window) t)


(defun copy-to-raster-image  (image &rest key-args)
  (let ((rast (apply 'img::make-raster-image (image-dimensions image) 
		     :element-type (image-element-type image)
		     key-args)))
    (copy-image image rast)))

(defun copy-to-LRBT-image  (image)
  (let ((cpy (img::make-image (image-dimensions image) 
			      :element-type (image-element-type image)
			      :block-x-dim (image-block-x-dim image)
			      :block-y-dim (abs (image-block-y-dim image))
			      :padded-block-x-dim (abs (img::image-padded-block-x-dim image)))))
    (copy-image image cpy)))

(setq *raster* (copy-to-raster-image (view-image (top-view))))

(setq *raster* (copy-to-raster-image (img::image-neg-y (view-image (top-view))) :top-to-bottom-p t))
(setq *raster* (copy-to-raster-image (view-image (top-view)) :top-to-bottom-p t))

(push-image *raster* (selected-window))
(image-block-y-dim *raster*)

(setq *lrbt* (copy-to-LRBT-image (view-image (top-view))))
(push-image *lrbt* (selected-window))


(setq *san-diego* (load-image "$RADIUS/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif"))

(push-image *san-diego* (selected-window))
(push-image (img::image-window *san-diego* 700 2500 1024 1024) (selected-window))
(pop-view (selected-window))
(typep *san-diego* 'img::vector-image) = T


|#




#|
(defparameter *TexImage2D-format-info-alist* *TexImage2D-format-info-alist-default*)
;;;(setq *TexImage2D-format-info-alist* *TexImage2D-format-info-alist-8bit*)
;;;(setq *TexImage2D-format-info-alist* *TexImage2D-format-info-alist-minbits*)


(defun probe-texture-map-internal-format (internal-format format element-type)
  (glBindTexture GL_TEXTURE_2D 0)
  (glTexImage2D GL_PROXY_TEXTURE_2D 0 internal-format 63 63 0 format element-type nil)
  (let ((selected-format  (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT)))
    (values (= selected-format internal-format) selected-format)))
 
(defun internal-format-name (internal-format)
  (loop for name in '(GL_RGB GL_RGBA GL_LUMINANCE 
		      GL_RGB8 GL_RGBA8 GL_LUMINANCE8 
		      GL_R3_G3_B2 GL_RGB5_A1 GL_LUMINANCE4)
	when (eql (symbol-value name) internal-format)
	  return name))

(defun probe-texture-map-internal-format (internal-format format element-type)
  (glBindTexture GL_TEXTURE_2D 0)
  (glTexImage2D GL_PROXY_TEXTURE_2D 0 internal-format 63 63 0 format element-type nil)
  (let* ((selected-format  (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT))
	 (rsize (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_RED_SIZE))
	 (rgize (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_GREEN_SIZE))
	 (bsize (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_BLUE_SIZE))
	 (asize (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_ALPHA_SIZE))
	 (lsize (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_LUMINANCE_SIZE)))
    (values (internal-format-name internal-format) rsize rgize bsize asize lsize)))
 
(loop for (image-element-type internal-format format type) in *TexImage2D-format-info-alist-8bit*
      collect (mv-list (probe-texture-map-internal-format internal-format format type)) )

((GL_RGB8 8 8 8 0 0) (GL_RGBA8 8 8 8 8 0) (GL_LUMINANCE8 0 0 0 0 8)
 (GL_LUMINANCE8 0 0 0 0 8) (GL_LUMINANCE8 0 0 0 0 8) (GL_LUMINANCE8 0 0 0 0 8)
 (GL_LUMINANCE8 0 0 0 0 8) (GL_LUMINANCE8 0 0 0 0 8) (GL_LUMINANCE8 0 0 0 0 8)
 (GL_LUMINANCE8 0 0 0 0 8))

(loop for (image-element-type internal-format format type) in *TexImage2D-format-info-alist-minbits*
      collect (mv-list (probe-texture-map-internal-format internal-format format type)) )
((GL_R3_G3_B2 5 6 5 0 0) (GL_RGB5_A1 5 5 5 1 0) (GL_LUMINANCE4 0 0 0 0 8)
 (GL_LUMINANCE4 0 0 0 0 8) (GL_LUMINANCE4 0 0 0 0 8) (GL_LUMINANCE4 0 0 0 0 8)
 (GL_LUMINANCE4 0 0 0 0 8) (GL_LUMINANCE4 0 0 0 0 8) (GL_LUMINANCE4 0 0 0 0 8)
 (GL_LUMINANCE4 0 0 0 0 8))

(loop for (image-element-type internal-format format type) in *TexImage2D-format-info-alist-default*
      collect (mv-list (probe-texture-map-internal-format internal-format format type)) )
((GL_RGB 8 8 8 0 0) (GL_RGBA 8 8 8 8 0) (GL_LUMINANCE 0 0 0 0 8)
 (GL_LUMINANCE 0 0 0 0 8) (GL_LUMINANCE 0 0 0 0 8) (GL_LUMINANCE 0 0 0 0 8)
 (GL_LUMINANCE 0 0 0 0 8) (GL_LUMINANCE 0 0 0 0 8) (GL_LUMINANCE 0 0 0 0 8)
 (GL_LUMINANCE 0 0 0 0 8))


(let () 
  (glBindTexture GL_TEXTURE_2D 0)
  (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 gl::GL_COMPRESSED_TEXTURE_FORMATS 
			  (glGetTexLevelParameter GL_PROXY_TEXTURE_2D 0 gl::GL_NUM_COMPRESSED_TEXTURE_FORMATS)))
 no COMPRESSED_TEXTURE_FORMATS
     
(let ()
  (glBindTexture GL_TEXTURE_2D 0)
  (glTexImage2D GL_PROXY_TEXTURE_2D 0 GL_RGB 64 64 0 GL_RGB GL_UNSIGNED_BYTE nil))

|#
