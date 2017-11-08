(in-package :img)


;;; Use the function versions instead of the macros. The compiler will not
;;; optimize (mv-bind (r g b) (values ...)).  It cannot, since the values
;;; returned from mv-iref! is dependent the length of the vector that viref
;;; returns.




  
;;;(defmacro vdiset2 (image x y &rest forms)
;;;  `(let ((%image% ,image)
;;;         (%array% ,(get-pre-allocated-float-iref-array (length forms))))
;;;    (declare (type math::coordinate-vector %array%))
;;;    (declare (type image %image%))
;;;    (setf .,(loop for form in forms
;;;                  for i from 0
;;;                  collect `(aref %array% ,i)
;;;                  collect form))
;;;    (the image-float-buffer-type
;;;        (funcall (the function (image-diset-fn %image%))
;;;                 %image% ,x ,y %array%))))



(defun iref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (destructuring-bind (red-image green-image blue-image)
      (component-images img)
    (require-image-rows
     (img)
     (loop for j fixnum from 0 below (image-y-dim img)
	   do (loop for i fixnum from 0 below (image-x-dim img)
		    for r fixnum = (iref red-image i j)
		    for g fixnum = (iref green-image i j)
		    for b fixnum = (iref blue-image i j)
		    sum  (the fixnum (+ r g b)) fixnum)))))


(defun diref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (destructuring-bind (red-image green-image blue-image)
      (component-images img)
    (require-image-rows
     (red-image green-image blue-image)
     (loop for j fixnum from 0 below (image-y-dim img)
	   do (loop for i fixnum  from 0 below (image-x-dim img)
		    for r double-float = (diref red-image i j)
		    for g double-float = (diref green-image i j)
		    for b double-float = (diref blue-image i j)
		    sum  (+ r g b) double-float)))))


(defun viref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (require-image-rows
   (img)
   (loop for j fixnum from 0 below (image-y-dim img)
	 do (loop for i fixnum from 0 below (image-x-dim img)
		  sum (bind-int-pixel-vector-elements (r g b) (viref img i j)
							      (+ r g b))
		  fixnum))))

(defun viset-test (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	with vector = (int-pixel-vector 1 2 3)
	do (setf (viref img 0 0) vector)))

(defun viset-test (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	with vector = (int-pixel-vector 1 2 3)
	do (setf (viref img 0 0) vector)))

(defun viset-test2 (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	do (setf (viref img 0 0) (int-pixel-vector 1 2 3))))


;;; One level of macro redefinition of int-pixel-vector is recognized by (setf (viref ...))
(defmacro ipv (&rest components)
  `(int-pixel-vector .,components))

(defun viset-test2 (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	do (setf (viref img 0 0) (ipv 1 2 3))))

(defun vdiset-test (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	with vector = (dfloat-pixel-vector 1.0 2.0 3.0)
	do (setf (vdiref img 0 0) vector)))

(defun vdiset-test2 (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	do (setf (vdiref img 0 0) (dfloat-pixel-vector 1.0 2.0 3.0))))

;;; One level of macro redefinition of float-pixel-vector is recognized by (setf (vdiref ...))
(defmacro dpv (&rest components)
  `(dfloat-pixel-vector .,components))

(defun vdiset-test2 (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	do (setf (vdiref img 0 0) (dpv 1.0 2.0 3.0))))


#|
(fmakunbound 'viset-test)
(fmakunbound 'vdiset-test)
(disassemble 'viset-test)
(disassemble 'vdiset-test)

(time (loop repeat 1 do (viset-test img 1000000))) .3 secs => 300 ns/pixel, no consing
(time (loop repeat 1 do (vdiset-test img 1000000))) .32 secs => 320 ns/pixel, no consing

(time (loop repeat 1 do (viset-test2 img 1000000))) .30 secs =>  300 ns/pixel, no consing
(time (loop repeat 1 do (vdiset-test2 img 1000000))) .33 secs => 330 ns/pixel, no consing

|#

(defun vdiref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (require-image-rows
   (img)
   (loop for j fixnum from 0 below (image-y-dim img)
	 do (loop for i fixnum from 0 below (image-x-dim img)
		  sum (bind-dfloat-pixel-vector-elements (r g b) (vdiref img i j)
								 (+ r g b))
		  double-float))))



#+never ;; eliminate these
(progn
(defmacro mv-iref! (image x y)
  `(let ((v (viref ,image ,x ,y nil)))
    (declare (type math::coordinate-ivector v))
    (case (length v)
      (1 (aref v 0))
      (2 (values (aref v 0) (aref v 1)))
      (3 (values (aref v 0) (aref v 1) (aref v 2)))
      (4 (values (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
      (otherwise (values-list (loop for i fixnum from 0 below (length v)
				    collect (aref v i)))))))

(defmacro mv-diref! (image x y)
  `(let ((v (vdiref ,image ,x ,y nil)))
    (declare (type math::coordinate-vector v))
    (case (length v)
      (1 (aref v 0))
      (2 (values (aref v 0) (aref v 1)))
      (3 (values (aref v 0) (aref v 1) (aref v 2)))
      (4 (values (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
      (otherwise (values-list (loop for i fixnum from 0 below (length v)
				    collect (aref v i)))))))

  
(defun mv-iset-test (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	do (setf (mv-iref img 0 0) (values 1 2 3))))

(defun mv-diset-test (img n)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i fixnum from 0 below n
	do (setf (mv-diref img 0 0) (values 1.0 2.0 3.0))))

(defun mv-iref!-test (img)
  (declare (optimize (speed 3) (safety 0)))
   (loop for j fixnum from 0 below (image-y-dim img)
	 do (loop for i fixnum from 0 below (image-x-dim img)
		  sum (mv-bind (r g b) (mv-iref! img i j)
			(declare (fixnum r g b) )
			(the fixnum (+ r g b)))
		  fixnum)))

(defun mv-diref!-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (require-image-rows
   (img)
   (loop for j fixnum from 0 below (image-y-dim img)
	 do (loop for i fixnum from 0 below (image-x-dim img)
		  sum (mv-bind (r g b) (mv-diref! img i j)
			(declare (double-float r g b) )
			(+ r g b))
		  double-float))))

(defun mv-iref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (mv-bind (r g b) (mv-iref img i j)
		       (declare (fixnum r g b) )
		       (the fixnum (+ r g b)))
		 fixnum)))

(defun mv-diref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (mv-bind (r g b) (mv-diref img i j)
		       (declare (double-float r g b) )
		       (+ r g b))
		 double-float)))

(time (loop repeat 1 do (mv-iset-test img 1000000))) .62 secs => 620 us/pixel, 48 bytes/pixel
(time (loop repeat 1 do (mv-diset-test img 1000000))) .66 secs => 660 us/pixel, 56 bytes/pixel

(time (loop repeat 100 do (mv-diref-test img2)))                       ; 7.71 secs 1.116e9 bytes consed
(/ 1.116e9 (* 100 (image-x-dim img) (image-y-dim img)))                ; 48 bytes/pixel
(/ 7.71e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 331 ns/pixel

(time (loop repeat 100 do (mv-diref-test img)))                         ; 6.98 secs 1.116e9 bytes consed
(/ 1.116e9 (* 100 (image-x-dim img) (image-y-dim img)))                ; 48 bytes/pixel
(/ 6.96e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 299 ns/pixel

(time (loop repeat 100 do (mv-diref-test (band-interleaved-image img)))); 6.86 secs 1.116e9 bytes consed
(/ 6.86e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 295 ns/pixel

(time (loop repeat 100 do (mv-iref-test img2)))                         ; 2.69 secs, no consing
(/ 2.69e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 116 ns/pixel

(time (loop repeat 100 do (mv-iref-test img)))                         ; 1.36 secs, no consing
(/ 1.36e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 58 ns/pixel

(time (loop repeat 100 do (mv-iref-test (band-interleaved-image img)))); 1.06 secs, no consing 
(/ 1.06e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 46 ns/pixel


(time (loop repeat 100 do (mv-diref!-test img2)))                        ; 7.66 secs 1.116e9 bytes consed
(/ 1.116e9 (* 100 (image-x-dim img) (image-y-dim img)))                ; 48 bytes/pixel
(/ 7.66e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 329 ns/pixel

(time (loop repeat 100 do (mv-diref!-test img)))                         ; 6.86 secs 1.116e9 bytes consed
(/ 1.116e9 (* 100 (image-x-dim img) (image-y-dim img)))                ; 48 bytes/pixel
(/ 6.86e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 295 ns/pixel

(time (loop repeat 100 do (mv-diref!-test (band-interleaved-image img)))); 6.78 secs 1.116e9 bytes consed
(/ 6.78e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 291 ns/pixel

(time (loop repeat 100 do (mv-iref!-test img2)))                          ; 2.67 secs, no consing
(/ 2.67e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 115 ns/pixel

(time (loop repeat 100 do (mv-iref!-test img)))                          ; 1.37 secs, no consing
(/ 1.37e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 59 ns/pixel

(time (loop repeat 100 do (mv-iref!-test (band-interleaved-image img)))); 1.03 secs, no consing 
(/ 1.03e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 44 ns/pixel


); end #+never progn



;;; Just for the hell of it, how do we compare with straight multi-dimensional array access?
(defun darray-ref-test (arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (* * 3)) arr))
  (loop for j fixnum from 0 below (array-dimension arr 1)
	do (loop for i fixnum from 0 below (array-dimension arr 0)
		 sum (+ (aref arr i j 0)
			(aref arr i j 1)
			(aref arr i j 2))
		 double-float)))

(defun scalar-darray-ref-test (arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (* *)) arr))
  (loop for j fixnum from 0 below (array-dimension arr 1)
	do (loop for i fixnum from 0 below (array-dimension arr 0)
		 sum (aref arr i j)
		 double-float)))

(defun iarray-ref-test (arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (signed-byte 8) (* * 3)) arr))
  (loop for j fixnum from 0 below (array-dimension arr 1)
	do (loop for i fixnum from 0 below (array-dimension arr 0)
		 sum (the fixnum (+ (the fixnum (+ (aref arr i j 0)
						   (aref arr i j 1)))
				    (aref arr i j 2)))
		 fixnum)))

(defun scalar-iarray-ref-test (arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (signed-byte 8) (* *)) arr))
  (loop for j fixnum from 0 below (array-dimension arr 1)
	do (loop for i fixnum from 0 below (array-dimension arr 0)
		 sum (aref arr i j)
		 fixnum)))

#|
(disassemble 'darray-ref-test)
(disassemble 'scalar-iarray-ref-test)
(setq *print-array* nil)
(setq arr (make-array0 '(1000 100 3) :element-type 'double-float))
(setq sarr (make-array0 '(1000 100) :element-type 'double-float))
		      
(time (loop repeat 100 do (darray-ref-test arr))) ; .51 secs
(/ .51e9 (* 100 (array-dimension arr 0) (array-dimension arr 1))) = 51 ns/pixel

(time (loop repeat 100 do (scalar-darray-ref-test sarr))) ; .15 secs
(/ .15e9 (* 100 (array-dimension arr 0) (array-dimension arr 1))) = 15 ns/pixel

(setq iarr (make-array0 '(1000 100 3) :element-type '(signed-byte 8)))
(setq siarr (make-array0 '(1000 100) :element-type '(signed-byte 8)))

(time (loop repeat 100 do (iarray-ref-test iarr)))
cmu (/ .56e9 (* 100 (array-dimension iarr 0) (array-dimension iarr 1))) 56 ns/pixel
acl (/ .47e9 (* 100 (array-dimension iarr 0) (array-dimension iarr 1))) 47 ns/pixel

(time (loop repeat 1000 do (scalar-iarray-ref-test siarr)))
cmu (/ 2.2e9 (* 1000 (array-dimension siarr 0) (array-dimension siarr 1))) 22 ns/pixel
acl (/ 1.26e9 (* 1000 (array-dimension siarr 0) (array-dimension siarr 1))) 12.9 ns/pixel

|#


#|
(maybe-compile-file-load "$FREEDIUS/lisp/img/iref-tests.lisp")

(eval-cache-flush img )
(setq img (load-image "$HOME/pix/forestcat.tif"))
(setq img2 (make-color-image-from-rgb (red-image img) (green-image img) (blue-image img)))

(setq pimg (load-image "$RADIUS/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/39943_rgb_0000010000.tiled512.tif"))
(setq pimg2 (make-color-image-from-rgb (red-image pimg) (green-image pimg) (blue-image pimg)))

;; To create a band-interleaved-paged-image:
(array_image_size_limit 0)
(setq old-array_image_size_limit
      (array_image_size_limit (* 100 100)))
(setq bi-image (make-image '(100 100) :element-type '(unsigned-byte 8) :samples-per-pixel 3))
(setq v-image (make-vector-image-from-band-interleaved-image bi-image))
(setq img2 v-image)


(setq forestcat (load-image "$HOME/pix/forestcat.tif"))
(setq cimg (make-tiff-file-image "/tmp/pix/test5.tif" (image-x-dim forestcat) (image-y-dim forestcat)
				 :block-x-dim 64 :block-y-dim -64 :element-type 'rgb8 ))
(copy-image forestcat cimg)
(setq img forestcat img2 cimg)

(gui::push-image img (gui::selected-window))
(gui::push-image pimg (gui::selected-window))

(viref (band-interleaved-image img) 0 0)
(viref (band-interleaved-image img2) 0 0)

(list (vdiref img2 0 0) (vdiref img2 1 1))
(list (vdiref img 0 0) (vdiref img 1 1))
(list (viref img2 0 0) (viref img2 1 1))
(list (viref img 0 0) (viref img 1 1))


(list (vdiref pimg 0 0) (vdiref pimg 1 1))
(list (vdiref pimg2 0 0) (vdiref pimg2 1 1))

(iref img 0 0)
(diref (red-image img2) 0 0)
(setf (diref (red-image img2) 0 0) 1.0)

(diref img 0 0)
(setq scalar-image (make-image '(100 100)))
(viref scalar-image 0 0)
(vdiref scalar-image 0 0)

(eq (vdiref img 0 0) (vdiref img 0 0)) = NIL
(eq (vdiref img 0 0 nil ) (vdiref img 0 0 nil)) = T
(eq (viref img 0 0) (viref img 0 0)) = NIL
(eq (viref img 0 0 nil ) (viref img 0 0 nil)) = T

(eq (vdiref img2 0 0) (vdiref img2 0 0)) = NIL
(eq (vdiref img2 0 0 nil ) (vdiref img2 0 0 nil)) = T
(eq (viref img2 0 0) (viref img2 0 0)) = NIL
(eq (viref img2 0 0 nil ) (viref img2 0 0 nil)) = T
 
(time (loop repeat 100 do (viref-test img2)))                           ; 1.73 secs, no consing
(/ 1.73e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 74 ns/pixel.


(time (loop repeat 100 do (viref-test img)))                           ; .83 secs, no consing
(/ .83e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 36 ns/pixel.

(time (loop repeat 10 do (viref-test pimg2)))                           ; 1.93 secs, no consing
(/ 1.93e9 (* 10 (image-x-dim img) (image-y-dim img)))                 ; 88 ns/pixel.

(time (loop repeat 10 do (viref-test pimg)))                           ; .95 secs, no consing
(/ .95e9 (* 10 (image-x-dim img) (image-y-dim img)))                  ; 43 ns/pixel.

(time (loop repeat 100 do (viref-test (band-interleaved-image img))))  ; .53 secs, no consing
(/ .53e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 23 ns/pixel.

(time (loop repeat 100 do (vdiref-test img2)))                          ; 2.25 secs, no consing ??? why
(/ 2.25e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 97 ns/pixel

(time (loop repeat 100 do (vdiref-test img)))                          ; .92 secs, no consing
(/ .92e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 40 ns/pixel

(time (loop repeat 100 do (vdiref-test (band-interleaved-image img)))) ; .66 secs, no consing
(/ .66e9 (* 100 (image-x-dim img) (image-y-dim img)))                  ; 28 ns/pixel.

(time (loop repeat 100 do (iref-test img)))                          ; 1.62 secs, no consing
(/ 1.62e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 70 ns/pixel

(time (loop repeat 100 do (diref-test img)))                          ; 2.42 secs, no consing
(/ 2.42e9 (* 100 (image-x-dim img) (image-y-dim img)))                 ; 104 ns/pixel



(vdiref (red-image img) 0 0)  = #(197.0) ok or should it be an error
(viref (red-image img) 0 0)   = 197      what should this be?  error or #(197)?
(iref (red-image img) 0 0)   = 197  ok
(diref (red-image img) 0 0) =  197.0  ok

(vdiref img 0 0)  = #(197.0 64.0 40.0) ok
(viref img 0 0)  = #(197 64 40)        ok
(iref img 0 0)  = #(197 64 40)           what should this be?  error?
(diref img 0 0)  = 197.0                 what should this be?  error?



(progn img)

(mv-list (mv-iref! img 0 0)) = (197 64 40)

(mv-list (mv-diref! img 0 0)) = (197.0 64.0 40.0)

(mv-list (mv-iref! (red-image img) 0 0))  ; error

(mv-list (mv-diref! (red-image img) 0 0)) = (197.0)


(setq bi-image (make-image '(100 100) :element-type '(unsigned-byte 8) :samples-per-pixel 3))
(setq v-image (make-vector-image-from-band-interleaved-image bi-image))

(iref (nth 0 (vector-image-component-images v-image)) 0 0)

(viref v-image 0 0)
(setf (viref v-image 0 0) (int-pixel-vector 1 2 3))
(vdiref v-image 0 0)
(setf (vdiref v-image 0 0) (dfloat-pixel-vector 10.0 20.0 30.0))

(setq bufs (make-dfloat-scan-line-buffers v-image))

(image-getline img bufs 0 0)
(image-putline v-image bufs 0 0)

(viref v-image 20 0)



(defparameter *mono-img* (make-image '(1000 100)))

;;; safety 1 sucks in allegro -- calls fixnum-check.
(defun mono-diref-test1 (img)
  (declare (optimize (speed 3) (safety 1)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (the double-float (diref img i j)) double-float)))

;;; this is ok in allegro
(defun mono-diref-test1a (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (diref img i j) double-float)))

(defun mono-diref-test1b (img)
  (declare (optimize (speed 3) (safety 1)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum  from 0 below (the fixnum (image-x-dim img))
		 for p double-float = (diref img i j)
		 sum p double-float)))

(defun mono-diref-test1c (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum  from 0 below (the fixnum (image-x-dim img))
		 for p double-float = (diref img i j)
		 sum p double-float)))

(disassemble 'mono-diref-test1)
(disassemble 'mono-diref-test1a)
(disassemble 'mono-diref-test1b)
(disassemble 'mono-diref-test1c)
(disassemble (image-diref-fn *mono-img*))


(time (loop repeat 100 do (mono-diref-test1 *mono-img*))) ; acl .69 secs => 69 ns/pixel  cmu .22 secs
(time (loop repeat 100 do (mono-diref-test1a *mono-img*))) ; acl .69 secs => 69 ns/pixel  cmu .22 secs
(time (loop repeat 100 do (mono-diref-test1b *mono-img*))) ; acl .50 secs => 50 ns/pixel cmu .22 secs
(time (loop repeat 100 do (mono-diref-test1c *mono-img*))) ; acl .44 secs => 44 ns/pixel cmu .2 secs

(disassemble 'vector-image-diref)
(disassemble 'color-image-diref)
(disassemble (image-diref-fn img))


(image-diref-fn (band-interleaved-image img))
(disassemble (image-diref-fn (band-interleaved-image img)))

(defun dfloat-test (i arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*)) arr))
  (declare (fixnum i))
  (setf (aref arr 0) (dfloat i))
  nil)

(disassemble 'dfloat-test)

;;; Allegro requires (safety 0) in order to optimize this.
(defun loop-test (arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*)) arr))
  (loop for i fixnum from 0 below (the fixnum (length arr))
	sum (aref arr i) double-float))

(disassemble 'loop-test)

(defparameter *band-interleaved-array-image-float-buffer-1*
  (make-array 1 :element-type 'double-float))

(def-band-interleaved-array-image-float-ref-fns (unsigned-byte 8) (unsigned-byte 8) 1
					      *band-interleaved-array-image-float-buffer-1*)

(defparameter *band-interleaved-array-image-int-buffer-1*
  (make-array 1 :element-type '(signed-byte 32)))

(def-band-interleaved-array-image-int-ref-fns (unsigned-byte 8) (unsigned-byte 8) 1
					      *band-interleaved-array-image-int-buffer-1*)


(defun mono-diref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (diref img i j) double-float)))


(defun mono-iref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (iref img i j) fixnum)))

(defun mono-iref-test3 (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (the fixnum (+ (iref img i j)
				    (the fixnum (+ (iref img i j)
						   (iref img i j)))))
		 fixnum)))

(defmacro xxiref (image x y)
  `(let* ((%image% ,image)
	  (%darray% (funcall (the function (image-iref-fn %image%))
		     %image% ,x ,y nil)))
    (declare (type image %image%))
    (declare (type (simple-array (signed-byte 32) (*)) %darray%))
    (aref %darray% 0)))

(defun mono-xxiref-test (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (xxiref img i j) fixnum)))

(defun mono-xxiref-test3 (img)
  (declare (optimize (speed 3) (safety 0)))
  (loop for j fixnum from 0 below (image-y-dim img)
	do (loop for i fixnum from 0 below (image-x-dim img)
		 sum (the fixnum (+ (xxiref img i j)
				    (the fixnum (+ (xxiref img i j)
						   (xxiref img i j)))))
		 fixnum)))

(defun foo (arr darr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*)) darr))
  (declare (type (simple-array (unsigned-byte 8) (*)) arr))
  (setf (aref darr 0) (dfloat (aref arr 0)))
  nil)

(describe 'dfloat)
(disassemble 'foo)

#|
(disassemble 'mono-iref-test2)
(disassemble 'mono-iref-test3)
(disassemble 'mono-diref-test)

(setq *img* (load-image "$HOME/pix/rugby.pic"))
(list (diref *img* 0 0) (iref *img* 0 0))
(setq buf (make-scan-line-buffers *img*))
(image-getline *img* buf 0 0)
(progn buf)

(setq *img* (make-image '(1000 100)))
(setq *img2* (make-image '(1000 100)))

(image-diref-fn *img*)
(image-diref-fn *img2*)

(setf (image-diref-fn *img2*)  #'ARRAY-IMAGE-UNSIGNED-8BIT-1-DIREF)
(setf (image-iref-fn *img2*)  #'ARRAY-IMAGE-UNSIGNED-8BIT-1-IREF)

(setf (image-diref-fn *img2*)  #'ARRAY-IMAGE-UNSIGNED-8BIT-DIREF)

(disassemble 'ARRAY-IMAGE-UNSIGNED-8BIT-1-DIREF)
(disassemble 'ARRAY-IMAGE-UNSIGNED-8BIT-DIREF)

(disassemble 'ARRAY-IMAGE-UNSIGNED-8BIT-1-IREF)
(disassemble 'ARRAY-IMAGE-UNSIGNED-8BIT-IREF)

(ARRAY-IMAGE-UNSIGNED-8BIT-1-DIREF *img* 0 0)

(time (loop repeat 1000 do (mono-diref-test *img*))) 1.92 secs => 19.2 ns/pixel
 ;; 
(time (loop repeat 1000 do (mono-diref-test *img2*))) 2.14 secs => 21.4 ns/pixel  11% more
 ;; 2.14 secs



(time (loop repeat 1000 do (mono-iref-test *img*)))    1.58 secs => 15.8 ns/pixel
(time (loop repeat 1000 do (mono-xxiref-test *img2*))) 1.84 secs => 18.4 ns/pixel


(time (loop repeat 1000 do (mono-iref-test3 *img*)))    5.69 secs => 57 ns/pixel
(time (loop repeat 1000 do (mono-xxiref-test3 *img2*))) 6.49 secs => 65 ns/pixel  14% more

(vdiref img 0 0)
(vdiref 'x 0 0)
|#


(defun test-getline (image buf)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (signed-byte 32) (*)) buf))
  (loop with xdim fixnum = (array-dimension (image-x-map image) 0)
	with ydim fixnum = (array-dimension (image-y-map image) 0)
	for y fixnum from 0 below ydim
	do (image-getline image buf 0 0)
	   (loop for x fixnum from 0 below xdim
		 sum  (aref buf x) fixnum)
	))

(defun test-iref (image)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (loop with xdim fixnum = (array-dimension (image-x-map image) 0)
	with ydim fixnum = (array-dimension (image-y-map image) 0)
	for y fixnum from 0 below ydim
	do (loop for x fixnum from 0 below xdim
		 sum  (iref image x y) fixnum
		 )))

(defun test-iref2 (image)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (loop with xdim fixnum = (array-dimension (image-x-map image) 0)
	with ydim fixnum = (array-dimension (image-y-map image) 0)
	with iref-fn = (image-iref-fn image)
	for y fixnum from 0 below ydim
	do (loop for x fixnum from 0 below xdim
		 sum  (iref image x y) fixnum
		 )))

(setq *simg* (make-image '(1000 100)))
(time (loop repeat 1000 do (test-iref *simg*)))
;;; 1.44 secs CMU, .22 secs ACL6.0
(/ 1.44e9 (* 1000 1000 100))  ; 14.4 ns/pixel

(time (loop repeat 1000 do (test-iref2 *simg*)))  ;; wierd -- this is actually slower
;;; 1.5 secs CMU, .22 secs ACL6.0
(/ 1.5e9 (* 1000 1000 100))  ; 15 ns/pixel

(time
 (let* ((img *simg*)
	(buf (make-integer-scan-line-buffer img)))
   (loop repeat 1000 do (test-getline img buf))))

;;; 1.0 secs
(/ 1.0e9 (* 1000 1000 100))  10.0 ns/pixel

(/ (* 1000 1000 100) (* 5000 4000)) = 5

(setq *pimg* (make-image '(5000 4000)))
(time (loop repeat 5 do (test-iref *pimg*)))
;;; 1.98 secs = 19.8 ns/pixel

(time (loop repeat 5 do (test-iref2 *pimg*)))
;;; 2.0 secs

(time
 (let* ((img *pimg*)
	(buf (make-integer-scan-line-buffer img)))
   (loop repeat 5 do (test-getline img buf))))
;; 2.51 secs => 25 ns/pixel


(disassemble 'test-iref)
(disassemble 'test-iref2)

|#

  





#|
(setq san-diego-rgb8 (img::load-image "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif"))

(setq san-diego-rgb8-g1 (zoom-out san-diego-rgb8))
(image-block-x-dim san-diego-rgb8-g1)

(gui::push-image san-diego-rgb8 (gui::selected-window gui::*interactor*))
(img::all_image_page_pool_stats 0)

(defun test-paging1 (img &key (delta 64))
  (prog1
      (loop for y from 0 by delta below (1- (image-y-dim img))
	    sum (loop for x from 0 by delta below (image-x-dim img)
		      sum (bind-dfloat-pixel-vector-elements (r g b)
							     (vdiref img x y nil)
							     (+ r g b))
		      double-float
		      when (> y 0)
			sum (bind-dfloat-pixel-vector-elements (r g b)
							       (vdiref img x (1- y) nil)
							       (+ r g b))
		      double-float
		      ))
    (img::all_image_page_pool_stats 0)))

(time (test-paging1 san-diego-rgb8 :delta 512))
(time (test-paging1 san-diego-rgb8 :delta 64))
(time (test-paging1 san-diego-rgb8 :delta 1))

(defun test-paging2 (img &key (delta 64))
  (prog1
      (loop for y from 0 by delta below (1- (image-y-dim img))
	    do (loop for x from 0 by delta below (image-x-dim img)
		     do (bind-dfloat-pixel-vector-elements (r g b)
							   (vdiref img x y nil)
							   (format t "~a " r)))
	    (terpri))
    (img::all_image_page_pool_stats 0)))


(time (test-paging2 san-diego-rgb8 :delta 512))
(setq bufs (make-dfloat-scan-line-buffers san-diego-rgb8))
(image-getline san-diego-rgb8 bufs 2048 2048)
(img::all_image_page_pool_stats 0)      
(float-getline-viref-vector-image= san-diego-rgb8 san-diego-rgb8)


(let ((img(band-interleaved-image  san-diego-rgb8)))
  (values (ceiling (image-x-dim img) (image-block-x-dim img))
	  (ceiling (image-y-dim img) (abs (image-block-y-dim img)))))


(setq *print-array* nil)
(paged-image-block-map (band-interleaved-image san-diego-rgb8))

(loop with arr = (paged-image-block-map (band-interleaved-image san-diego-rgb8))
      for i from 0 below (length arr)
      collect (aref arr i))

(release_image_pages (image-id (band-interleaved-image san-diego-rgb8)))

(def-foreign-function (test_page_maps (:name (freedius-prefix "test_page_maps")))
  (img (:pointer c-image)))


(test_page_maps (image-id (band-interleaved-image san-diego-rgb8)))

(test_page_maps (image-id (gui::view-image (top-view))))
|#
