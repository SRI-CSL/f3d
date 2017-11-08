(in-package :img)


#|
(maybe-compile-file-load
 '("$FREEDIUS/lisp/img/vector-image.lisp"
   "$FREEDIUS/lisp/img/color-image.lisp"
  ))

(setq img0 (load-image "~/pix/forestcat.color"))
(save-image (load-image "~/pix/forestcat.color") "~/pix/forestcat.rgb")
(load-image "~/pix/forestcat.rgb")

(setq crl1 (make-image '(100 100) :element-type '(unsigned-byte 8) :image-type 'color-image))
(describe crl1)


(setq forestcat-rgb8 (load-image "/homedir/quam/pix/forestcat.tif"))
(describe forestcat-rgb8)

(setq forestcat (make-band-interleaved-vector-image forestcat-rgb8))

(describe forestcat)

(gui::push-image (red-image forestcat) (gui::selected-window gui::*interactor*))

(setq forestcat-red (nth 0 (component-images forestcat)))
(gui::push-image forestcat-red (gui::selected-window gui::*interactor*))

(iref forestcat 100 100)
(iref forestcat 101 100)
(iref forestcat-red 100 100)
(iref forestcat-red 101 100)


(image-samples-per-pixel forestcat-rgb8)
|#

(defstruct-class color-image
    (vector-image)
  ((red-image :initarg :red-image :accessor red-image)
   (green-image :initarg :green-image :accessor green-image)
   (blue-image :initarg :blue-image :accessor blue-image))
  (:default-initargs
      :component-names '(RED GREEN BLUE)
    :component-pathnames '(".red" ".grn" ".blu")))

(defmethod initialize-instance :after ((image color-image) &rest ignore)
  (ignore ignore)
  (with-class-slots color-image (red-image green-image blue-image component-images) image
    (setf red-image (nth 0 component-images)
	  green-image (nth 1 component-images)
	  blue-image (nth 2 component-images))
    (setf (image-iref-fn image) #'color-image-iref
	  (image-iset-fn image) #'color-image-iset)))

(defun color-image-iref (image x y)
  (declare (type color-image image))
  (declare (optimize (safety 0) (speed 3)))
  (values (iref (red-image image) x y)
	  (iref (green-image image) x y)
	  (iref (blue-image image) x y)))

(defun color-image-iset (image x y r g b)
  (declare (type color-image image))
  (declare (optimize (safety 0) (speed 3)))
  (setf (iref (red-image image) x y) r
	(iref (green-image image) x y) g
	(iref (blue-image image) x y) b)
  (values r g b))


#+unfinished
(progn
  
(defun make-color-image (red green blue)
  (make-vector-image (list red green blue) :image-type 'color-image))

(defmethod initialize-instance :after ((image color-image) &rest ignore)
  (ignore ignore)
  (with-class-slots color-image (red-image green-image blue-image component-images) image
    (setf red-image (nth 0 component-images)
	  green-image (nth 1 component-images)
	  blue-image (nth 2 component-images))
    (setf (image-iref-fn image) #'color-image-iref)
    ))

(defmethod iref* ((image color-image) x y)
  (with-class-slots color-image (red-image green-image blue-image) image
    (values (iref red-image x y)
	    (iref green-image x y)
	    (iref blue-image x y))))

;;; The generic function for vector-image-iset* will require 0 values,
;;; and allow up to 3 optional values.  This will accomodate color-images,
;;; anaglyph-images, and complex-images.

;;; This is incorrect for the (SETF (IREF ... )) expander.
(defmethod vector-image-iset* ((image color-image) x y &optional r g b)
  (with-class-slots color-image (write-lock red-image green-image blue-image) image
    (when write-lock (prepare-to-modify image))
    (setf (iref red-image x y) r)
    (setf (iref green-image x y) g)
    (setf (iref blue-image x y) b)
    (values r g b)))

(defmethod stretch-params :around ((image color-image) &rest args)
  (apply 'call-mext-method image :allow-offset nil args))

(defun load-color-image
       (file-or-list &key (rgb-defaults '(".red" ".grn" ".blu")) file-format)
  (load-vector-image file-or-list
		     :component-paths rgb-defaults
		     :file-format file-format
		     :image-type 'color-image))


(defclass color-image-file
     (vector-image-file)
     ((rgb-pathnames :initarg :rgb-pathnames)
      (red-header :initarg :red-header)
      ))

(defmethod image-file-format-recognize-header
    ((image-format color-image-file) header-string new-stream)
  (ignore new-stream)
  (and (string-equal header-string "COLOR-IMAGE"  :end1 11 :end2 11)
       (allocate-file-format 'color-image-file))
  )


#|
COLOR-IMAGE header format -- slightly different from method for vector-image

     COLOR-IMAGE
     <list or red, grn, blu pathnames>
|#

(defmethod image-file-format-read-header
	   ((image-format color-image-file) new-stream &optional ignore)
  (ignore new-stream ignore)
  
  (with-class-slots color-image-file (pathname vector-image-class
					       component-pathnames
					       x-dim y-dim element-size element-type header-length
					       block-x-dim block-y-dim block-size rgb-pathnames)
      image-format
    (with-open-file (strm (ev-pathname-translate pathname) :direction :input)
      (read strm)			; skip COLOR-IMAGE
      (setq rgb-pathnames (read strm))
      )
    (setq rgb-pathnames
	  (loop for path in rgb-pathnames
		collect (string-downcase path)))
    (let (ignore1)
      (ignore ignore1)
      (multiple-value-setq
	  (ignore1 x-dim y-dim element-size element-type header-length
		   block-x-dim block-y-dim block-size)
	
	(load-image-header
	 (image-calc-merge-pathnames (string (car rgb-pathnames)) pathname)))
      )))

(defmethod image-file-format-load-image
    ((image-format color-image-file) file into-image)
  (when into-image (unmake-image into-image :expunge t))
  (with-class-slots color-image-file (rgb-pathnames) image-format
    (load-color-image file :rgb-defaults rgb-pathnames)))

(defmethod save-image*
    ((image color-image)
     pathname
     &key 
     (rgb-pathnames (component-pathnames image))
     (file-format *save-image-default-file-format*)
     &allow-other-keys)
  (with-open-file (stream (ev-pathname-translate pathname) :direction :output)
    (format stream "COLOR-IMAGE~%~s~%" rgb-pathnames))
  (with-class-slots color-image (component-images component-pathnames ) image
    (loop for image in component-images
	  for path in component-pathnames
	  do (save-image* image
			  (image-calc-merge-pathnames (string path) pathname) 
		  :file-format file-format))
    image))

;;; perhaps this should be appended to list rather than on front.
(setup-image-file-formats '(color-image-file))

(defparameter *color-screen-gamma* 1.0)

(defmethod gamma-correct ((image color-image) &optional (gamma *color-screen-gamma*))
  (unary-operator image 'gamma-correct gamma))
#|
(setq xx (multiple-value-list (load-image-header "ti:>pix>burt-hacks>forestcat.color")))
(setq forest-cat2 (load-image "ti:>pix>burt-hacks>forestcat.color"))
(setq portland2 (load-image "iu:aux:[tbpic.portland]portland.rgb"))
(save-image pwin "ti:>pix>pwin.rgb")
(setq pwin2 (load-image "ti:>pix>pwin.rgb"))
|#


(defmethod image-bitbltable ((image color-image)) 
  t) ; ???

;;; Replace this next with something more general

(defmethod direct-display-image
    ((image color-image)
     window from-x from-y to-x to-y width height
     &rest args &key 
     alu reverse-video  (set-xform t)
     (color-map (color-map image window))
     &allow-other-keys)
  (ignore reverse-video set-xform color-map alu)
  (let ((screen (screen window)))
    ;;(setup-color-map window color-map image)
    (case (screen-depth screen)
      (8 (apply 'direct-display-image
		(map-to-8-bits image screen)
		window from-x from-y to-x to-y width height args)
	 )
      (otherwise (error "unsupported color screen: ~a" screen)))))

(defmethod map-to-8-bits ((image color-image) screen &optional (gain 1.0) (offset 0.0))
  (dither image screen :gain gain :offset  offset))

#|
(defmethod background-alu ((image color-image) window)
  (if (not (color-p window))
      (erase-aluf window)
      
      (let ((map-manager 
	      (or (send image :color-map (send window :screen))
		  (send (send window :color-map-owner)  :get-grey-color-map))))
	(xw::make-color-alu
	  (rgb-fill-data 
	    (if (typep map-manager 'rgb-dither-color-map-manager-mixin)
		(or (fourth (send map-manager :range-list)) 0)
		0))
	  -1))))
|#

#|
(defmethod color-map ((image basic-image) window)
  (if (image-prop image :color-map-spec)
      (let* ((screen (screen window))
	     (map-manager (current-color-map-manager screen))
	     (attenuation 1.0)
	     (map-gamma 1.0)
	     (offset 0.0))
	(when map-manager
	  (setq attenuation (or (color-attenuation map-manager)
				(grey-level-attenuation map-manager))
		map-gamma 1.0
		offset (color-offset map-manager)))
	
	(load-rgb-dither-color-map screen (image-prop image :color-dither-info)
				   :color-gamma map-gamma
				   :color-attenuation attenuation
				   :color-offset offset
				   ))

      (image-prop image :color-map)))
|#


(defmethod displayed-object ((image color-image) &optional view)
  (let ((view-name (get-prop view :display-object-name)))
    (case view-name
      (rgb image)
      (red (red-image image))
      (green (green-image image))
      (blue (blue-image image))
      (max-intensity (max-intensity-image image))
      (ntsc-luminance (ntsc-luminance-image image))
      (hue (hue-image image))
      (color-hue (colorful-hue-image image))
      (hack (colorful-hack image))
      (saturation (saturation-image image))
      (otherwise image))))

(defmethod select-display-view ((image color-image) view displayed-object)
  (ignore view)
  (with-cvv-items
      (((displayed-object
	 "Displayed Image:" :assoc :alist
	 (("RGB" rgb "Full Color Image")
	  ("Red" red "Red Channel")
	  ("Green" green "Green Channel")
	  ("Blue" blue "Blue Channel")
	  ("NTSC-Lum" ntsc-luminance
		      "NTSC Luminance Image = .3R + .59G + .11B")
	  ("Max-Int" max-intensity
		     "Hexcone Intensity Image = max (R G B)")
	  ("Hue" hue "Hexcone Hue Image, Red = 0") 
	  ("Color-Hue" Color-hue
		       "Color Image of Fully Saturated Hues")
	  ("Sat" saturation
		 "Saturation Image = (max - min) / max")
	  ("Mix" :linear-combination-image "Linearly Combine R G B")
	  )
	 :value (or displayed-object 'rgb)))
       :label "Color Image Display"
       )
  
    (case displayed-object
	     (:linear-combination-image
	      (setq displayed-object
		    (linear-mix-menu (get-prop view :display-object-name)))))
  
    displayed-object))

;;; ********************************  COLOR-IMAGE COLOR TRANSFORMATIONS  ********************************

;;; From color:color;color-transformations.lisp
;;; from alvy ray smith, siggraph 1978
;;; r,g,b must be normalized in range 0.0 to 1.0
;;; All outputs are normalized in range 0.0 to 1.0
(defmacro rgb->hexcone-ihs (r g b &environment env)
  (ignore env)
  `(let ((r ,r) (g ,g) (b ,b))
     (if (and (zerop r) (zerop g) (zerop b))
	 (values 0.0 0.0 0.0)
	 (let* ((v (max* r (max* g b)))
		(x (min* r (min* g b)))
		(sat (/ (- v x) v)))
	   (if (not (zerop sat))
	       (let* ((denom (/ (- v x)))
		      (lr (* (- v r) denom))
		      (lg (* (- v g) denom))
		      (lb (* (- v b) denom))
		      (hh (cond ((= r v)
				 (if (= g x) (+ 5.0 lb) (- 1.0 lg)))
				((= g v)
				 (if (= b x) (+ 1.0 lr) (- 3.0 lb)))
				(t
				 (if (= r x) (+ 3.0 lg) (- 5.0 lr))))))
		 (values v (* hh #.(/ 6.0)) sat))
	       (values v 0.0 0.0))))))


;;; from alvy ray smith, siggraph 1978
(defmacro hexcone-ihs->rgb (v hh ss &optional r g b &environment env)
  (ignore env)
  `(let ((v ,v) (hh ,hh) (ss ,ss))
     (let* ((h (* 6.0 hh))
	    (int (floor h))
	    (f (- h int))
	    (m (* v (- 1.0 ss)))
	    (n (* v (- 1.0 (* ss f))))
	    (k (* v (- 1.0 (* ss (- 1.0 f))))))
       (declare (fixnum int))
       ,(if (and r g b)
	    `(case int
	       (0 (setq ,r v ,g k ,b m))
	       (1 (setq ,r n ,g v ,b m))
	       (2 (setq ,r m ,g v ,b k))
	       (3 (setq ,r m ,g n ,b v))
	       (4 (setq ,r k ,g m ,b v))
	       (5 (setq ,r v ,g m ,b n))
	       (6 (setq ,r v ,g k ,b m)))
	    
	    `(case int
	       (0 (values v k m))
	       (1 (values n v m))
	       (2 (values m v k))
	       (3 (values m n v))
	       (4 (values k m v))
	       (5 (values v m n))
	       (6 (values v k m)))))))

(defparameter *linear-mix-coeffs* '(.30 .59 .11 0))

(defun linear-mix-menu (displayed)
  (when (and (listp displayed)
	     (eq (car displayed) :linear-combination-image))
    (let ((coeffs (cdr displayed)))
      (with-cvv-items
	  (((red-coeff  "Red coeff" :double-float :value (first coeffs))
	    (green-coeff "Green coeff" :double-float :value (second coeffs))
	    (blue-coeff "Blue coeff" :double-float :value (third coeffs))
	    (offset "Offset" :double-float :value (fourth coeffs)))
	   :label "Linear Mix Components of Color Image")
	(list :linear-combination-image
	      red-coeff green-coeff blue-coeff offset)))))


(defmethod-cached hexacone-ihs-image ((image color-image))
  (with-slot-values (red-image green-image blue-image) image
    (let ((inten-image (similar-image red-image))
	  (hue-image (similar-image red-image))
	  (sat-image (similar-image red-image)))
	  
      (make-vector-image
       (multiple-value-list
	   (multi-image-point-operator ((red-image green-image blue-image)
					(inten-image hue-image sat-image)
					:buffer-type float)
	       (lambda(r g b) (declare (double-float r g b))
		      ;; the normalization here is wrong -----------------------------
		      (multiple-value-bind (i h s) (rgb->hexcone-ihs r g b)
			(declare (double-float i h s))
			(pixel-values i h s)))))))))

(defmethod-cached colorful-hue-image ((image color-image))
  (let* ((hue-image (hue-image image))
	 (red-image (similar-image hue-image))
	 (grn-image (similar-image hue-image))
	 (blu-image (similar-image hue-image)))
    (apply 'make-color-image
	   (multiple-value-list
	       (multi-image-point-operator ((hue-image)
					    (red-image grn-image blu-image))
		   (lambda(h) (declare (double-float h))
			  (multiple-value-bind (r g b) 
			      ;; the normalization here is wrong -----------------------
			      (hexcone-ihs->rgb 1.0 (* h #.(/ 256.0)) 1.0)
			    (declare (double-float r g b))
			    (pixel-values (* 255.0 r) (* 255.0 g) (* 255.0 b)))))))))


#| experimental hack
(eval-cache-flush-function 'color-image-colorful-hack)
(defmethod (:colorful-hack color-image) ()
  (eval-cache (color-image-colorful-hack image)
    (let ((hue-image (send image :hue-image))
	  (sat-image (send image :saturation-image))
	  ;(int-image (send image :intensity-image))
	  )
      (lexpr-funcall
	'make-color-image
	(multiple-value-list
	  (multi-image-point-operator ((hue-image sat-image)
				       ((similar-image hue-image)
					(similar-image hue-image)
					(similar-image hue-image)))
	    (lambda(h s)
	      `(multiple-value-bind (r g b)
		   (hexcone-ihs->rgb 1.0 (* ,h #.(/ 256.0)) (* ,s #.(/ 256.0)))
		 (pixel-values (fix (* 255 r)) (fix (* 255 g)) (fix (* 255 b)))))))))))
|#

#| unfinished
(defmethod (:max-intensity-image color-image) ()
  (eval-cache (color-intensity-max-image image)
    (image-point-operator ((red-image green-image blue-image))
      (lambda(r g b) `(max* ,r ,g ,b)))))

(defmethod (:ntsc-luminance-image color-image) ()
  (eval-cache (color-image-ntsc-luminance image)
    (image-point-operator ((red-image green-image blue-image))
      (lambda(r g b) `(fix (+ (* .3 ,r) (* .59 ,g) (* .11 ,b)))))))

(defmethod (:linear-combination-image color-image) (cr cg cb offset)
  (let ((max-val (image-element-max red-image)))
    (eval-cache (color-image-ntsc-linear-combination-image image cr cg cb offset)
      (image-point-operator ((red-image green-image blue-image))
	(lambda(r g b) `(max* 0
			      (min* max-val
				    (fix (+ (* cr ,r) (* cg ,g) (* cb ,b) offset)))))))))

(defmethod (:saturation-image color-image) ()
  (eval-cache (color-image-saturation image)
    (image-point-operator ((red-image green-image blue-image))
      (lambda(r g b) `(let* ((r ,r) (g ,g) (b ,b)
			     (v (max* r g b))
			     (x (min* r g b)))
			(if (zerop v) 0
			    (fix (/ (* 256.0 (- v x)) v))))))))

;(eval-cache-flush-function 'color-image-hue)
 
(defmethod (:hue-image color-image) ()
  (eval-cache (color-image-hue image)
    (image-point-operator ((red-image green-image blue-image))
      (lambda(r g b)
	`(multiple-value-bind (nil hue)
	     (rgb->hexcone-ihs (float ,r) (float ,g) (float ,b))
	   (min* 255 (fix (* 256 hue))))))))

|#
;;; *****************************  COLOR-IMAGE INTENSITY TRANSFORMATIONS  *****************************
#|
Photometric model information on image property-list.
Unfinished

   (image-prop image :intensity-model)

A property-list containing the following keywords
     :linear t
     :gamma <gamma>
     :density <n>
     :table <array>

This list can be passed to :intensity-map method of image.

Unsolved problems:  
     Control of inheritance of image-properties by processed versions of the image, eg.
    zoom-in, zoom-out and other purely geometric operators.

|#

(defmethod intensity-map ((image basic-image) &rest prop-list)
  (with-class-slots basic-image(element-size) image
    (and (image-fixp image)
	 (or (image-prop image :intensity-map)
	     (let ((element-range
		    (2^ (if (listp element-size) (car element-size) element-size)))
		   linear table density gamma)
	       (unless prop-list (setq prop-list (image-prop image :intensity-model)))
	       (keyword-extract prop-list foo (linear table density gamma))
	       (cond (table table)
		     (linear  (gamma-intensity-map 1.0 element-range) )
		     (density (exponential-intensity-map density element-range))
		     (t (unless gamma (setq gamma  (or (image-prop image :gamma) 1.0)))
			(gamma-intensity-map gamma element-range))))))))

(defun-cached exponential-intensity-map (density-range range)
  (loop with table = (make-array range)
	with ratio = (expt 10.0 (- (/ (float density-range) range)))
	for i from (1- range) downto 0
	for val first 1.0 then (* ratio val)
	do (setf (aref table i)  (* (1- range) val))
	   finally (return table)))

(defun-cached gamma-intensity-map (gamma range &optional (scale 1.0))
  (when (zerop (fraction gamma)) (setq gamma (fix gamma)))
  (loop with array = (make-array range)
	with k = (/ (* scale range) (float (expt range gamma)))
	for i from 0 below range
	for x from 0 to range
	do (setf (aref array i)
		 (* k (expt x gamma)))
	   finally (return array)))

(defun map-array (array fn &optional (element-type 'single-float))
  (loop with n = (length array)
	with new-array = (make-array n :element-type element-type)
	for i from 0 below n
	do (setf (aref new-array i) (funcall fn (aref array i)))
	finally (return new-array)))

(defmethod set-intensity-model ((image color-image) new-model)
  (with-class-slots color-image (red-image green-image blue-image) image
    (setf (image-prop red-image :intensity-model) new-model)
    (setf (image-prop green-image :intensity-model) new-model)
    (setf (image-prop blue-image :intensity-model) new-model)))



(defmethod image-linear-combination-3-input
	   ((image-a scalar-image) (image-b scalar-image) (image-c scalar-image)
	    ka kb kc offset &key into-image (buffer-type 'fixnum))
  ;;(setq foo (list image-a image-b image-c ka kb kc offset))
  (if (eq buffer-type 'float)
      (let ((ka (float ka))
	    (kb (float kb))
	    (kc (float kc))
	    (offset (float offset)))
	(declare (float ka kb kc offset))
	;; 6.57 secs for 1k x 1k image
	(image-point-operator ((image-a image-b image-c) into-image
			       :buffer-type float )
	    (lambda(a b c) (declare (float a b c))
		   (+ (* ka a) (* kb b) (* kc c) offset))))

      (if nil
	  (flet ((mktab (a b n)
		   (let ((arr (make-array n :element-type '(unsigned-byte 8))))
		     (declare (type (simple-array (unsigned-byte 8) (*)) arr)
			      ;;(type-reduce number fixnum)
			      )
		     (loop with a fixnum = (floor a)
			   with b fixnum = (floor b)
			   for i fixnum from 0 below n
			   do (setf (aref arr i) (+ (* i a) b)))
		     arr)))
	    (let ((atab (mktab ka 0.0 256))
		  (btab (mktab kb 0.0 256))
		  (ctab (mktab kc offset 256)))
	      (declare (type (simple-array (unsigned-byte 8) (*)) atab btab ctab))
	      ;; 4.41 secs for 1kx1k image
	      (image-point-operator ((image-a image-b image-c) into-image
				     :buffer-type fixnum)
		  (lambda(a b c) (declare (fixnum a b c) 
			 (+ (aref atab a) (aref btab b) (aref ctab c))))))

	  (flet ((mktab (a b n)
		   (let ((arr (make-array n :element-type '(unsigned-byte 8)))
			 (a (floor a) )
			 (b (floor b)))
		     (declare (type (simple-array (unsigned-byte 8) (*)) arr)
			      )
		     (loop for i fixnum from 0 below n
			   do (setf (aref arr i) (+ (* i a) b)))
		     arr)))
	    (let ((atab (mktab ka 0 256))
		  (btab (mktab kb 0 256))
		  (ctab (mktab kc offset 256)))
	      (unless into-image (setq into-image (similar-image image-a)))
	      (with-scan-line-buffers
		  ((abuf (make-integer-scan-line-buffer image-a nil '(unsigned-byte 8)))
		   (bbuf (make-integer-scan-line-buffer image-b nil '(unsigned-byte 8)))
		   (cbuf (make-integer-scan-line-buffer image-c nil '(unsigned-byte 8)))
		   (rbuf (make-integer-scan-line-buffer into-image nil '(unsigned-byte 8))))
		(loop with nx = (image-x-dim image-a)
		      for y from 0 below (image-y-dim image-a)
		      do (image-getline image-a abuf 0 y)
			 (image-getline image-b bbuf 0 y)
			 (image-getline image-c cbuf 0 y)
			 (rgb_table_loop abuf bbuf cbuf rbuf atab btab ctab nx)
			 (image-putline into-image rbuf 0 y))
		into-image)))
	  )))


#| ;;; *****************************  EXPERIMENTS  *****************************

  
(image-prop *LAST-COLOR-IMAGE-DITHER* :color-map-spec)

(set-frame-color-maps) ; this resets the color maps on all CME frames

(load-default-color-map (default-screen))
(make-default-color-map (default-screen))

(progn (eval-cache-flush-function  'color-image-dither)
       (eval-cache-flush-function  'make-rgb-dither-color-map))

(set-dither-color-map `(4 4 3 ,(+ 64 *free-slot*) 4)) ; this is the current default

(set-dither-color-map `(6 10 4 16 1))	; this works fairly well

(set-dither-color-map `(5 6 4 8 2)) 

(set-dither-color-map `(4 5 3 ,*free-slot* 4))
(set-dither-color-map `(4 4 4 ,*free-slot* 4))

(set-dither-color-map `(4 5 3 ,(+ 16 *free-slot*) 4)); better, but too few public colors

(set-dither-color-map `(4 6 2 ,(+ (* 4 (- 64 (* 4 6 2))) *free-slot*) 4))
(set-dither-color-map  *rgb-dither-range-list*)

(setq rugby (load-image "~/pix/dog.win"))
(setq cat (load-image "~/pix/cat.win"))
(setq colimg (make-color-image cat rugby rugby))

(setq colimg2 (make-color-image rugby rugby rugby))

(setq rugby2 (image-expand-by-2 rugby))

(setq colimg2 (make-color-image rugby2 rugby2 rugby2))

(setq alv-col (make-color-image cme::alv-2-44-win cme::alv-2-44-win cme::alv-2-44-win))
(progn
  (setq alv-pair (make-color-image cme::alv-3-41 cme::alv-3-42 cme::alv-3-42))
  nil)
;;*rgb-dither-range-list* = '(6 10 4 4)

(eval-cache-flush-function  'color-image-dither)

(setq dither (dither colimg (screen (selected-pane))))
(setq dither2 (dither colimg2 (screen (selected-pane))))

(unmake-image alv-pair-dither)
(time (setq alv-pair-dither  (dither alv-pair (screen (selected-pane)))))
;; 27.43 seconds 111408 Bytes Consed
;; 24.5 seconds 99k Ephemeral Bytes Consed
;; 14 secs 75k bytes   -- C code.


;; each get-dither takes about 6.3 secs, 20k Ephemeral Bytes (lisp version)
;; each get-dither takes about 4  secs, 30k Ephemeral Bytes (C version)

;; (4 images x 2 maps/image x 4k bytes/map ) = 32k bytes

(time (setq foo (make-image '(1024 1024) :element-type '(unsigned-byte 8))))
;;; 1.02 secs, 1.8k Ephemeral Bytes Consed
;;; maps are constructed in static area.

;;; with r g b dithers in cache
(eval-cache-flush-function  'color-image-dither)
(unmake-image alv-pair-dither)
(time (setq alv-pair-dither (dither alv-pair (screen (selected-pane)))))
;; 6.57 secs 36k Ephemeral Bytes Consed (float buffers )
;; 4.41 secs 21k Ephemeral Bytes Consed (fixnum buffers)
;; 2.67 secs 9k Ephemeral Bytes c inner loop (uses 8-bit buffers)

(time (setq alv-pair-dither (apply 'image-linear-combination-3-input foo)))
;; 6.73 33k Ephemeral Bytes Consed
;; 4 getline buffers x 8bytes/double x 1k doubles/line = 32kbytes

(setq *default-dither-table-type* '(signed-byte 32))
(setq *default-dither-table-type* t)
(unmake-image alv-dither)
(eval-cache-flush-value alv-dither)
(eval-cache-flush-function 'get-dither)
(unmake-image alv-dither)
(time (setq alv-dither (dither-image-into-image cme::alv-3-41 nil
						:to-bps 8 :max-dither-level 3)))
;; 12.13 seconds 8448 Ephemeral Bytes Consed, 31k Dynamic Bytes Consed
;; 5.73 seconds 14k Ephemeral Bytes Consed
;; 3.04 seconds 14k Ephemeral Bytes Consed (too much consing)

(car rgb-dither-images)
(cadr rgb-dither-images)
(caddr rgb-dither-images)
(progn rgb-dither-images)

(color-pattern-image-8x8 :start-int *free-slot* )
(color-pattern-image-8x8 :start-int *free-slot* :x-cells 16)
(color-pattern-image-8x8 :start-int 0 )

(color-pattern-image-16x16)

(setq  *color-dither-screen-gamma* 1.0)
(setq  *color-dither-screen-gamma* .6)
(setq  *color-dither-screen-gamma* .8)
;;; default is (/ .55) = 1.8
(defparameter *color-dither-screen-gamma* (/ .55))
|#


#| this is for debugging

;;; This only works right for the hires-paddle-hardware because of format of color-map array.
(defun-cached expand-compacted-color-image-component
	      (dithered-color-image color-map screen component-index)
  (let ((map (make-array 256))
	(shift (typecase screen
		 (color:chroma-managed-screen 0)
		 (color:sc-screen -2))))
    (loop for i from 0 below 256
	  do
      (setf (aref map i)
	    (ash (aref color-map i 0 component-index) shift))) 
    (image-point-operator ((dithered-color-image))
      (lambda(x) `(aref map ,x)))))
    
(defun-cached expand-compacted-color-image
	      (dithered-color-image color-map screen screen-gamma)
  (let ((result
	  (make-color-image 
	    (expand-compacted-color-image-component dithered-color-image color-map screen 0) 
	    (expand-compacted-color-image-component dithered-color-image color-map screen 1) 
	    (expand-compacted-color-image-component dithered-color-image color-map screen 2))))
    #|
    (setf (image-prop result :color-map)
	  (send screen :make-gamma-corrected-color-map (/ screen-gamma)
		:from-map (send screen :get :color-map)))
   |#
    result))

(defun expand-dither-color-image
       (color-image range-list
	&rest args
	&key 
	gamma screen-gamma
	(screen (send *image-calc* :screen)))
  (ignore gamma screen screen-gamma)
  (let ((8b (lexpr-send color-image :dither screen :range-list range-list args))) 
    (expand-compacted-color-image 8b
				  (send (send 8b :color-map screen) :map)
				  screen screen-gamma)))
|#


) ; end #+unfinished progn

;;; ***********************************  PACKED-COLOR-IMAGE  ***********************************

(defstruct-class packed-color-image (color-image)
     ((alpha-image :initform nil :accessor alpha-image)
      (packed-image :initform nil :initarg :packed-image :accessor packed-image))
  (:default-initargs :component-images nil :initial-value 0)
  )

#|
packed-color-images suck for unary-operators that allocate new images.

The problem is that the r,g,b components must be created via build-bands-from-packed-image,
not via some random scalar image operator.

The best solution is to make sure that all unary operators accept a keyword
argument named :into-image.  Unfortunately, this requires that the code for unary operator
understand how to build the images for the underlying scalar-image operation.  BIG LOSE.

The alternative is to copy the allocated image into the component and expunge which is very ugly.

(method make-component (packed-color-image)) does some very ugly things !!
   There is a random bug probably due to make-component which clobbers the image_struct
   of component images.  Is this a GC interaction?

|#

(defmethod MAKE-IMAGE-STRUCT ((image packed-color-image))
  )

(defmethod image-hack-maps ((image-struct (eql nil)) new-x-map new-y-map)
  (ignore new-x-map new-y-map))
  
(defparameter *pack-color-image-unary-methods-needing-packed-image*
	      '(image-linear-geom-map-internal ))

(defparameter *packed-color-expunge-list* nil)

;;; This is sufficiently generic that it could move to vector-image.
(defmethod unary-operator ((color-image packed-color-image ) opr &rest args)
  (with-class-slots packed-color-image (component-images element-type element-size) color-image
    (let* ((opr-images (loop for img in component-images
			     collect (apply opr img args)))
	   (first (car opr-images))
	   (result (make-image (image-dimensions first)
			       :element-type element-type
			       :element-size element-size
			       :image-type (type-of color-image)
			       :component-images opr-images))
	   )
      ;; delete opr-images unless result image contains exactly them
      ;; (which will occur for windowing operators)
      (loop for img1 in opr-images
	    for img2 in (component-images result)
	    do (unless (eq img1 img2)
		 (unless (image-prop img1 :inferiors)
		   (if nil
		       (push img1 *packed-color-expunge-list*)

		       (progn ;;(format t "unary-operator packed-color-image expunging ~a~%" img1)
			      (unmake-image img1 :expunge t))
		       ))))

      result)))

(defmethod find-packed-image ((image packed-color-image ) component)
  (when component
    (let ((indirected-to (image-indirected-to component)))
      (if indirected-to
	  (find-packed-image image indirected-to)
	  component))))

(defmethod bytes-per-tile ((image packed-color-image))
  (bytes-per-tile (packed-image image)))
  
(defmethod tile-offset-bits ((image packed-color-image))
  (tile-offset-bits (packed-image image)))

(defparameter *packed-color-image-bytes-per-pixel* 4)
(defparameter *packed-color-image-element-type* 'rgba8)
;;(defparameter *packed-color-image-element-type* '(unsigned-byte 32))
  
;;;(defmethod verify-packed-components ((image packed-color-image ))
;;;  (with-class-slots packed-color-image (red-image green-image blue-image alpha-image packed-image ) image
;;;    (let ((pki (or packed-image (find-packed-image image red-image))))
;;;      (when (and pki
;;;                 (equal (image-element-type pki) *packed-color-image-element-type*)
;;;                 (eq pki (find-packed-image image red-image))
;;;                 (eq pki (find-packed-image image green-image))
;;;                 (eq pki (find-packed-image image blue-image)))
;;;        (setf packed-image pki) ))))

(defmethod verify-packed-components ((image packed-color-image ))
  (with-class-slots packed-color-image (red-image green-image blue-image alpha-image packed-image ) image
    (let ((pki (or packed-image (find-packed-image image red-image))))
      (if (and pki
	       (equal (image-element-type pki) *packed-color-image-element-type*)
	       (eq pki (find-packed-image image red-image))
	       (eq pki (find-packed-image image green-image))
	       (eq pki (find-packed-image image blue-image)))
	  (setf packed-image pki)
	  (progn
	    (unless pki (error "Cannot find packed image"))
	    (unless (equal (image-element-type pki) *packed-color-image-element-type*)
	      (error "Packed image has wrong element-type"))
	    (unless (eq pki (find-packed-image image red-image))
	      (error "red image has different packed image"))
	    (unless (eq pki (find-packed-image image green-image))
	      (error "green image has different packed image"))
	    (unless (eq pki (find-packed-image image blue-image))
	      (error "blue image has different packed image"))
	    nil

	    )))))

(defmethod initialize-instance :after ((image packed-color-image ) &key &allow-other-keys)
  (with-class-slots packed-color-image (red-image green-image blue-image component-images packed-image ) image
    ;;(break)
    (if (and red-image green-image blue-image)
	(unless (verify-packed-components image)
	  (let ((red red-image) (green green-image) (blue blue-image))
	    (build-bands-from-packed-image image)
	    (copy-image red red-image )
	    ;;(break) ; ok to here 
	    (copy-image  green green-image )
	    (copy-image  blue blue-image )
	    ;;(break)
	    ))
	
	(build-bands-from-packed-image image)))
  (slots-from-components image)
  )

;;;(defmethod init-new :after ((image packed-color-image ) &rest ignore)
;;;  (ignore ignore)
;;;  (with-slots (red-image green-image blue-image alpha-image component-images packed-image ) image
;;;    ;;(break)
;;;    (if (and red-image green-image blue-image)
;;;        (unless (verify-packed-components image)
;;;          (let ((red red-image) (green green-image) (blue blue-image)
;;;                (alpha alpha-image))
;;;            (build-bands-from-packed-image image)
;;;            (copy-image red :into-image red-image )
;;;            ;;(break) ; ok to here 
;;;            (copy-image  green :into-image green-image )
;;;            (copy-image  blue :into-image blue-image )
;;;            (when alpha (copy-image  alpha  :into-image alpha-image ))
;;;            ;;(break)
;;;            ))
;;;        
;;;        (build-bands-from-packed-image image)))
;;;  (slots-from-components image)
;;;  )

(defmethod init-new :after ((image packed-color-image ) &rest ignore &key abgr-order)
  (ignore ignore)
  (with-class-slots packed-color-image (red-image green-image blue-image alpha-image component-images packed-image ) image
    ;;(break)
    (if (and red-image green-image blue-image)
	(unless (verify-packed-components image)
	  (let ((red red-image) (green green-image) (blue blue-image)
		(alpha alpha-image))
	    (build-bands-from-packed-image image abgr-order)
	    (copy-image red red-image )
	    ;;(break) ; ok to here 
	    (copy-image  green green-image )
	    (copy-image  blue blue-image )
	    (when alpha (copy-image  alpha  alpha-image ))
	    ;;(break)
	    ))
	
	(build-bands-from-packed-image image abgr-order)))
  (slots-from-components image)
  )

(defmethod build-bands-from-packed-image ((image packed-color-image ) &optional abgr-order)
  (with-class-slots packed-color-image (red-image green-image blue-image alpha-image component-images packed-image ) image
    (unless packed-image
      (setq packed-image
	    (make-image (image-dimensions image)
			:element-type *packed-color-image-element-type*
			:initial-element -1)))
    (if abgr-order
	(setf (internal-get-prop image :abgr-order) t
	      red-image (make-component image 3)
	      green-image (make-component image 2)
	      blue-image (make-component image 1)
	      alpha-image (make-component image 0))
	(setq red-image (make-component image 0)
	      green-image (make-component image 1)
	      blue-image (make-component image 2)
	      alpha-image (make-component image 3)))
    (setf (component-images image)
	  (list red-image green-image blue-image alpha-image
		;;packed-image
		))
    (slots-from-components image)
    ))


(defmethod clone-image ((image array-image) &optional (element-type (image-element-type image)))
  (let* ((element-type-code (cdr (assoc element-type
					*image-element-type-index-alist*
					:test 'equal)))
	 (clone (wrap-foreign-image 
		 (map_array_image (image-id image)
				  (image-x-dim image) (image-y-dim image)
				  element-type-code 
				  (image-x-map image) (image-y-map image)))))
    (setf (image-array clone) (image-array image))

    ;; are these needed?
    (setf (image-element-type clone) element-type)
    (setf (c-image-element-type-code (image-id clone)) element-type-code)
    (set-image-struct-slots clone)
    clone))

(defun copy-to-stationary-vector (vector)
  (let ((new-vector (make-stationary-vector (length vector) :element-type (array-element-type vector))))
    (copy-array-contents vector new-vector)
    new-vector))

(defmethod set-image-maps ((image image) x-map y-map)
  (setf (image-x-map image) x-map
	(image-y-map image) y-map
	(image-x-dim image) (length x-map)
	(image-y-dim image) (length y-map)
	))

(defmethod set-image-maps :after ((image array-image) x-map y-map)
  (set_array_image_arrays (image-id image) x-map y-map (image-array image) 1)
  )


(defmethod make-component ((image packed-color-image) offset)
  (progn;; eval-cache (packed-color-image-component image offset)
    (with-class-slots packed-color-image (packed-image ) image
      (let* ((new-element-type '(unsigned-byte 8))
	     (new-image (clone-image packed-image new-element-type)))
       	(set-image-maps new-image
			(copy-to-stationary-vector (image-x-map packed-image))
			(copy-to-stationary-vector (image-y-map packed-image)))
	
	#+unfinished
	(progn
	  ;; clone-image mucks with these next -- reset them
	  (setf (image-write-action packed-image ) 'write-in-place
		(image-write-lock packed-image ) nil)
	  (setf (image-write-action new-image ) 'write-in-place
		(image-write-lock new-image ) nil)
	  ;;(make-image-struct new-image)
	
	  (when (typep new-image 'paged-image)
	    (let* ((struct (image-struct new-image))
		   (cstruct (image_struct new-image))
		   (bits (+ 2 (paged-image-struct-tile-offset-bits struct)))
		   (mask (logior 3 (ash (paged-image-struct-tile-offset-mask struct) 2))))
	      (setf (paged-image-struct-tile-offset-bits struct) bits
		    (paged-image-struct-tile-offset-mask struct) mask
		    (c_paged_image_struct-tile_offset_bits cstruct) bits
		    (c_paged_image_struct-tile_offset_mask cstruct) mask)
	      (set-image-struct-slots struct new-image bits)))
	  )
	
	(packed-color-image-hack-maps new-image packed-image
				      *packed-color-image-bytes-per-pixel* offset)
	new-image
	))))

  
#|
(setq *packed-image_struct-info* nil)

(loop for (pimg img . rest) in *packed-image_struct-info*
      when (eq img bad-img) return (list* pimg  img rest))
|#

(defmethod packed-color-image-hack-maps ((image image) packed-image mult offset)
  (with-class-slots image (x-map y-map) image
    (packed-color-image-hack-map image packed-image x-map mult offset)
    (packed-color-image-hack-map image packed-image y-map mult 0)
    (set-image-maps image x-map y-map)
    ))

(defmethod packed-color-image-hack-map ((image array-image) packed-image map mult offset)
  (declare (type image-map-type map))
  (ignore packed-image)
  (loop for i fixnum from 0 below (length map)
	do (setf (aref map i) (+ offset (* mult (aref map i) )))))

#+unfinished
(defmethod packed-color-image-hack-map ((image paged-image) packed-image map mult offset)
  (declare (type image-map-type map))
  (loop for i fixnum from 0 below (length map)
	do (multiple-value-bind (tile-number tile-index)
	       (decompose-paged-image-pixel-index packed-image (aref map i) )
	     (declare (fixnum tile-number tile-index))
	     (setf (aref map i)
		   (compose-paged-image-pixel-index
		    image tile-number (+ offset (* mult tile-index ))))))
  (update-paged-image_struct image (image_struct image))
  )

#|
(let* ((new-image (green-image cme::img))
       (cstruct (image_struct new-image)))
  (values (c_paged_image_struct-tile_offset_bits cstruct)
	  (c_paged_image_struct-tile_offset_mask cstruct)))
|#

(defun make-packed-color-image (red green blue)
  (make-image (image-dimensions red) :image-type 'packed-color-image
	      :component-images (list red green blue)))

#|
(setq cimg1 (make-image '(20 20) :image-type 'packed-color-image))

(defun iref-pci (img x y) 
  (values (iref (red-image img) x y)
	  (iref (green-image img) x y)
	  (iref (blue-image img) x y)
	  (iref (packed-image img) x y) ))

(let ((x 2) (y 3))
  (setf (iref (red-image cimg1) x y) 32
	(iref (green-image cimg1) x y) 64
	(iref (blue-image cimg1) x y) 128)
  (iref-pci cimg1 x y)
  )

(iref cimg1 2 3)
(iref-pci cimg1 2 3)
(format nil "~x" 541097984)

(set-dither-color-map `(6 10 4 16 1))	; this works fairly well
(set-dither-color-map `(4 4 3 ,(+ 64 *free-slot*) 4)) ; this is the current default
(load-default-color-map (default-screen))


(defun set-dither-color-map (range-list &optional (pane (selected-pane) ))
  ;;(setq *rgb-dither-range-list* range-list)
  ;;(load-default-color-map (screen pane)) ; this resets the greys ---
 
  (setup-color-map pane
		   (rgb-dither-color-map-spec range-list (screen pane))
		   nil )
  (set-frame-color-maps)
  )

(setq *packed-image_struct-info* nil)


(setq cme::forestcat (load-image "~/pix/forestcat.color"))
(eval-cache-flush-value cme::forestcat)

(setq packed-forestcat2 (make-packed-color-image (red-image cme::forestcat)
						(green-image cme::forestcat)
						(blue-image cme::forestcat)))

(let ((*make-image-default-image-type* 'blocked-file-mapped-image))
  (setq packed-forestcat (make-packed-color-image (red-image cme::forestcat)
						(green-image cme::forestcat)
						(blue-image cme::forestcat))))

(iref-pci packed-forestcat 100 100)
(loop for v in (multiple-value-list (iref-pci packed-forestcat 100 100))
      collect (format nil "~x" v))

|#


(defmethod copy-image ((image array-image-rgb) &optional into-image)
  (let* ((xdim (image-x-dim image))
	 (ydim (image-y-dim image))
	 (image2 (or into-image
		     (make-image (list xdim ydim)
				 :element-type (image-element-type image)))))
    (with-scan-line-buffers ((buf (make-scan-line-buffer image)))
      (loop for y fixnum from 0 below ydim
	    do (image-getline image buf 0 y)
	       (image-putline image2 buf 0 y))
      image2)))



(defmethod image-flip-y ((image color-image))
  (with-slots (red-image green-image blue-image) image
     (image-flip-y red-image)
     (image-flip-y green-image)
     (image-flip-y blue-image))
  image)
