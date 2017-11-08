(in-package :gl)

#|
Code to examine the OpenGL internal state of the current rendering context.


(maybe-compile-file-load "$FREEDIUS/lisp/gl-ffi/gl-state.lisp")
|#

(defparameter *gl-parameter-type-alist*
  '((GL_POLYGON_MODE ((signed-byte 32) 2))
    (GL_POLYGON_OFFSET_FACTOR float)
    (GL_POLYGON_OFFSET_UNITS float)
    ))

(defun list-array (array)
  (loop for i from 0 below (length array)
	collect (aref array i)))

(defun glstate (parameters)
  (loop for parameter0 in parameters
	for parameter-name = (intern (symbol-name parameter0) :gl)
	for type = (or (cadr (assoc parameter-name *gl-parameter-type-alist*))
		       'integer)
	for parameter = (symbol-value parameter-name)
	collect parameter-name
	collect (case type
		  (integer (glGetInteger parameter))
		  (float (glGetFloat parameter))
		  (boolean (glGetBoolean parameter))
		  (otherwise
		   (if (listp type)
		       (let ((array (make-array0 (cadr type) :element-type (car type)))
			     (type (car type)))
			 (cond ((equal type '(signed-byte 32)) (glGetIntegerv parameter array))
			       ((eq type 'single-float) (glGetFloatv parameter array))
			       ((equal type '(unsigned-byte 8)) (glGetBooleanv parameter array))
			       (t (error "Unknown parameter type for ~a" parameter-name)))
			 (list-array array))
		       (error "Unknown parameter type for ~a" parameter-name))))))

#|
			 
(glstate '(GL_DEPTH_TEST GL_DEPTH_WRITEMASK
	   GL_POLYGON_OFFSET_FILL GL_POLYGON_OFFSET_LINE GL_POLYGON_OFFSET_FACTOR GL_POLYGON_OFFSET_UNITS
	   GL_POLYGON_STIPPLE GL_POLYGON_MODE))

(list GL_LINE GL_FILL) = (6913 6914)

(list GL_FRONT GL_BACK GL_FRONT_AND_BACK) = (1028 1029 1032)

(list GL_FLAT GL_SMOOTH) = (7424 7425)




|#
