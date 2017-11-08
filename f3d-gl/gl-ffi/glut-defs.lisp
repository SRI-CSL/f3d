(in-package :gl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  GLUT definitions - for GL string drawing.  No more dependency on
;;;  any foreign GLUT library.


(defun glutInit (&rest ignore) nil)

(defun glutBitmapCharacter (font-id character)
  (let ((font (get-font-by-id font-id))
	(code (if (characterp character) (char-code character) character)))
    (declare (fixnum code))
    (when (and font (<= 1 code 255))
      (unwind-protect
	   (destructuring-bind (char-width . char-bitmap)
	       (aref (font-character font) code)
	     (glPushClientAttrib  GL_CLIENT_PIXEL_STORE_BIT )
	     (glPixelStorei GL_UNPACK_SWAP_BYTES  GL_FALSE )
	     (glPixelStorei GL_UNPACK_LSB_FIRST   GL_FALSE )
	     (glPixelStorei GL_UNPACK_ROW_LENGTH  0        )
	     (glPixelStorei GL_UNPACK_SKIP_ROWS   0        )
	     (glPixelStorei GL_UNPACK_SKIP_PIXELS 0        )
	     (glPixelStorei GL_UNPACK_ALIGNMENT   1        )
	     (glBitmap char-width (font-height font)
		       (coerce (font-xorig font) 'single-float)
                       (coerce (font-yorig font) 'single-float)
		       (coerce char-width 'single-float) 0.0s0
		       char-bitmap))
	(glPopClientAttrib))
      )))

;;;
;;; Each entry in the character map is a cons pair (width . glyph), so
;;; the car is the width:
;;;
(defun glutBitmapWidth(font-id char-code)
  (let ((font (get-font-by-id font-id)))
    (and (<= 0 char-code (font-quantity font))
         (car (aref (font-character font) char-code))
         )))

;;;;;;

(defun glutBitmapLength (font-id string)
  ;; Return the pixel "length" of the string, taking newlines into
  ;; account:
  (let* ((font (get-font-by-id font-id))
         (max (font-quantity font))
         (font-chars (font-character font))
         (this-line-length 0)
         (length 0))
    (declare (fixnum this-line-length length))
    (flet ((font-char-width (code)
             (and (<= 0 code max) (car (aref font-chars code)))))
      (and font
           (loop for char across string
                 for w fixnum = (font-char-width (char-code char))
                 do (if (char/= char #\newline)
                        (incf this-line-length w)
                        (progn
                          (when (< length this-line-length)
                            (setf length this-line-length))
                          (setf this-line-length 0)))
                    finally (return (max length this-line-length)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun init-glut () nil)

(defun glutfont-p (thing) (typep thing 'glut-font))


(defun glutBitmapHeight (font)
  (font-height font))

(defun glutBitmapString (font string)
  (declare (optimize (speed 3) (safety 0)))
  (loop for char of-type character across string
	do (glutBitmapCharacter font (char-code char))))

#| GLUT defines the following bitmap fonts:

glutBitmap9By15;
glutBitmap8By13;
glutBitmapTimesRoman10;    10-point variable-width Times Roman font.
glutBitmapTimesRoman24;    24-point variable-width Times Roman font.
glutBitmapHelvetica10;     10-point variable-width Helvetica font.
glutBitmapHelvetica12;     12-point variable-width Helvetica font.
glutBitmapHelvetica18;     18-point variable-width Helvetica font.

|#

(defun-cached get-glut-font (&key (family "Helvetica") (size 12) font error-ok stroke-p)
  (get-font-by-id (list family size)))


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
  (declare (type (simple-array double-float (*)) position)
           (optimize (speed 3) (safety 0)))
  (unless font (setq font *default-glut-font*))
  (unless (glutfont-p font) (setq font (get-glut-font :font font)))
  (when font
    (let ((x (aref position 0))
	  (y (aref position 1))
	  (z (if (< (length position) 3) 0.0 (aref position 2))))
      (declare (double-float x y z))
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
