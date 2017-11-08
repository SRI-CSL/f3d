(in-package :gl)

;;;
;;; This file is legacy code for use with an external GLUT library.
;;; To use the native FREEDIUS string drawing functions, simply set
;;; CONFIG::*LIBGLUT-LIBRARY-FILE* to NIL.


(eval-when (compile)
  (when config::*enable-openglut*
    (pushnew :openglut *features*)
    (format t ";;; Compiling for openglut")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign function declarations:
;;;

(lcl::def-foreign-synonym-type glutfont dummy-pointer)


(def-foreign-function (glutInit (:name "glutInit"))
    (argc* (:simple-array :unsigned-long))
  (argv** (:simple-array :unsigned-long)))


(def-foreign-function (glutBitmapCharacter
		       (:name "glutBitmapCharacter")
		       (:return-type :int))
  (font glutfont)
  (char :int))


(def-foreign-function (glutBitmapWidth
		       (:name "glutBitmapWidth")
		       (:return-type :int))
  (font glutfont)
  (char :int))


;;;;;;



(def-foreign-function (glutBitmapLength (:name "glutBitmapLength")
					(:return-type :int))
  (font glutfont)
  (string :simple-string))




;;; Unused??
(def-foreign-function (glutStrokeLength (:name "glutStrokeLength")
					(:return-type :int))
  (font glutfont)
  (string :simple-string))



;;;
;;; If / when we locally implement draw-string operations, these two
;;; are emulated in Lisp below...
;;;
#+openglut
(def-foreign-function (glutBitmapString
		       (:name #+openglut "glutBitmapString"
			      #-openglut (freedius-prefix "glutBitmapString"))
		       (:return-type :int))
  (font glutfont)
  (string :simple-string))


;;; crap!  openglut provides glutBitmapHeight, but  the "official" glut does not.
;;; There appears to be no good way to figure out the bitmap height in "official" glut.

#+openglut
(def-foreign-function (glutBitmapHeight
		       (:name "glutBitmapHeight")
		       (:return-type :int))
  (font glutfont))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun init-glut ()
  (let (
	(argc* (make-array 1 :element-type `(unsigned-byte ,qffi::*machine-word-length*) :initial-element 0))
	(argv** (make-array 1 :element-type  `(unsigned-byte ,qffi::*machine-word-length*) :initial-element 0)))
    ;; glutInit fails on Windows with the latest FREEDIUS3M release (10.29.2007)
    ;; Fortunately, we only need the bitmaps, which appear to be loaded in any case:
    (glutInit argc* argv**)))

#-(or mswindows macosx)
(st::add-system-initialization :gl '(init-glut))

#|
(symbol-package 'dummy-pointer)
(ffi::expand-foreign-type 'dummy-pointer)
(disassemble 'def-foreign-function)
|#

(defun glutfont-p (thing) (numberp thing))

#|
Running with freeglut loaded I get the values for *glut-fontname-height-alist*

(setq *glut-fontname-height-alist*
      (loop for fontname in '("Helvetica10" "Helvetica12" "Helvetica18"
			      "TimesRoman10"  "TimesRoman24" "9By15" "8By13")
	    for font = (get-glut-font :font fontname)
	    collect (list fontname (glutBitmapHeight font))))

|#

#-openglut
(progn

;;; This sucks -- 
(defparameter *glut-fontname-height-alist*
  ' (("Helvetica10" 13)
     ("Helvetica12" 15)
     ("Helvetica18" 22)
     ("TimesRoman10" 13)
     ("TimesRoman24" 28)
     ("9By15" 15)
     ("8By13" 13)))


(defparameter *glut-font-height-alist* nil)

(defun glutBitmapHeight (font)
  (unless *glut-font-height-alist*
    (setq *glut-font-height-alist*
	  (loop for (fontname height) in *glut-fontname-height-alist*
		for font = (get-glut-font :font fontname)
		collect (list font height))))
  (cadr (assoc font *glut-font-height-alist*))
  )

(defun glutBitmapString (font string)
  (loop for char across string
	do (glutBitmapCharacter font (char-code char))))


) ; end #-openglut progn


(defun glutStrokeString (font string)
  (loop for char across string
	do (glutStrokeCharacter font (char-code char))))



#| GLUT defines the following bitmap fonts:

glutBitmap9By15;
glutBitmap8By13;
glutBitmapTimesRoman10;    10-point variable-width Times Roman font.
glutBitmapTimesRoman24;    24-point variable-width Times Roman font.
glutBitmapHelvetica10;     10-point variable-width Helvetica font.
glutBitmapHelvetica12;     12-point variable-width Helvetica font.
glutBitmapHelvetica18;     18-point variable-width Helvetica font.

|#

(progn

#-mswindows
(defun-cached get-glut-font (&key (family "Helvetica") (size 12) font (error-ok #-mswindows nil #+mswindows t) stroke-p)
  (when (consp font)
    (destructuring-bind (tfamily tsize) font
      (setq family tfamily size tsize)))

  (unless (stringp font)
    (setq font
	  (if stroke-p
	      (format nil "~a" family)
	      (format nil "~a~d" family size))
	  ))
  
  (let* ((symbolname
	  (format nil
		  (if stroke-p
		      #+(and macosx allegro) "_glutStroke~a"
		      #-(and macosx allegro) "glutStroke~a"
		      #+(and macosx allegro) "_glutBitmap~a"
		      #-(and macosx allegro) "glutBitmap~a")
		  font))
	 (ptr   (lcl::foreign-variable-pointer symbolname)))
    (or (and ptr (lcl::foreign-pointer-address ptr))
	(unless error-ok
	  (error "get-glut-font failed to find ~a~%" symbolname))
	)))

#+mswindows
(defun-cached get-glut-font (&key (family "Helvetica") (size 12) font error-ok stroke-p)
  (let ((font-table #("glutStrokeRoman"
		      "glutStrokeMonoRoman"
		      "glutBitmap9By15"
		      "glutBitmap8By13"
		      "glutBitmapTimesRoman10"
		      "glutBitmapTimesRoman24"
		      "glutBitmapHelvetica10"
		      "glutBitmapHelvetica12"
		      "glutBitmapHelvetica18")))
    (when (consp font)
      (destructuring-bind (tfamily tsize) font
	(setq family tfamily size tsize)))
    (unless (stringp font)
      (setq font (format nil "~A~D" family size)))
    (let ((name (format nil (if stroke-p "glutStroke~A" "glutBitmap~A") font))
	  )
      (or (position name font-table :test #'string=)
	  (unless error-ok
	    (error "get-glut-font failed to find ~A~%" name))))))

)


(defun font-character-raster-width (character font)
  (unless (glutfont-p font) (setq font (get-glut-font :font font)))
  (glutBitmapWidth font (char-code character)))

(defun string-raster-width (string font)
  (unless (glutfont-p font) (setq font (get-glut-font :font font)))
  (glutBitmapLength font string))

(defun font-raster-height (font)
  (unless (glutfont-p font) (setq font (get-glut-font :font font)))
  (glutBitmapHeight font))

(defparameter *default-glut-font* '("Helvetica" 12))

(defun string-raster-size (string font)
  (unless font (setq font *default-glut-font*))
  (let ((h (font-raster-height font)))
    (values (string-raster-width string font)
	    h
	    0 ;;FIXME
	    (- (floor h 3)) ; baseline offset
	    )))

;;; FIXME: centering is broken unless OpenGL matrices are set to window_to_ndc transform
;;; FIXME: broken for multi-line strings

(defun glDrawString (string position &key centering (font *default-glut-font*))
  (declare (type (simple-array double-float (*)) position))
  (unless font (setq font *default-glut-font*))
  (unless (glutfont-p font) (setq font (get-glut-font :font font)))
  (when font
    (let ((x (aref position 0))
	  (y (aref position 1))
	  (z (if (< (length position) 3) 0.0 (aref position 2))))
      (when centering
	(multiple-value-bind (xoff yoff)
	   (compute-string-centering string centering font)
	   (incf x xoff) (incf y yoff)))
      (glRasterPos3d x y z)
      (glutBitmapString font string)
      
      )))

#+broken
(defun glDrawString (string position &key centering (font *default-glut-font*))
  (declare (type (simple-array double-float (*)) position))
  (unless font (setq font *default-glut-font*))
  (unless (glutfont-p font) (setq font (get-glut-font :font font)))
  (glRasterPos3dv position)
  (when centering
    (multiple-value-bind (xoff yoff)
	(compute-string-centering string centering font)
      (glMatrixMode GL_MODELVIEW) (glPushMatrix) (glLoadIdentity)
      (glMatrixMode GL_PROJECTION) (glPushMatrix) (glLoadIdentity) ; wrong -- need window_to_ndc_matrix
      (gl::glBitmap 0 0 0.0f0 0.0f0 (float xoff 0.0f0) (float yoff 0.0f0) nil)
      (glMatrixMode GL_PROJECTION) (glPopMatrix)
      (glMatrixMode GL_MODELVIEW) (glPopMatrix)
      ))
  (glutBitmapString font string))


(defun glDrawChar (char position &rest args &key centering (font *default-glut-font*))
  (apply #'glDrawString (make-string 1 :initial-element char) position args))


(defvar *multi-line-string-break-chars* '(#\- #\space #\tab #\newline))

(defun multi-line-string-raster-size (string font max-chars &optional (break-chars *multi-line-string-break-chars*))
  (loop with n = (length string)
	with total-height = 0
	with max-wid = 0
	with pos = 0
	with line-raster-height = (font-raster-height font)
	with left-off = 0 
	with bottom-off = 0
	while (< pos n)
	for next-pos =  (or (let ((pos (position #\newline string :start (1+ pos))))
			      (and pos (1+ pos)))
			    (if (< (- n pos) max-chars)
				n
				(1+ (or (position-if
					 #'(lambda(char)
					     (member char break-chars :test #'char=))
					 string :from-end t :start (1+ pos) :end (min n (+ pos max-chars)))
					(+ pos max-chars -1)))))
	do (multiple-value-bind (wid height lo bo)
	       (string-raster-size (substring string pos next-pos) font)
	     (setq left-off lo
		   bottom-off bo
		   ;; line-raster-height height
		   max-wid (max max-wid wid)
		   total-height (+ total-height height)
		   pos next-pos))
	finally (return (values max-wid total-height left-off bottom-off line-raster-height))))



(defun compute-string-centering (string centering font)
  (multiple-value-bind (raster-width raster-height x-off y-off line-raster-height)
      (multi-line-string-raster-size string font 999)
    (ignore x-off)
    ;;(format t "~a~%" (list raster-width raster-height x-off y-off line-raster-height))
    (let ((x 0) (y 0))
      (loop for keys on centering
	    do (case (car keys)
		 (:center
		  (decf x (ash raster-width -1))
		  (decf y (+ line-raster-height (ash (- y-off raster-height) -1)))
		  )
		 (:h-center (decf x (ash raster-width -1)))
		 (:right (decf x raster-width))
		 (:top (decf y (+ line-raster-height y-off)))
		 (:bottom (decf y (+ y-off (- line-raster-height raster-height))))
		 (:v-center (decf y (ash raster-height -1)))
		 (:v-offset (incf y (cadr keys)))
		 (:h-offset (incf x (cadr keys)))
		 ))
      (values x y raster-width raster-height line-raster-height))))

(eval-when (eval load compile)
  (setq *features* (remove :openglut *features*)))
