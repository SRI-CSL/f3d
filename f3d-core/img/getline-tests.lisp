(in-package :img)


(defun iref-scalar-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (and (equal (image-dimensions img1) (image-dimensions img2))
       (require-image-rows
	(img1 img2)
	(loop for j fixnum from 0 below (image-y-dim img1)
	      always (loop for i fixnum from 0 below (image-x-dim img1)
			   always (= (iref img1 i j) (iref img2 i j)))))))
	    

(defun viref-vector-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (and (equal (image-dimensions img1) (image-dimensions img2))
       (require-image-rows
	(img1 img2)
	(loop for j fixnum from 0 below (image-y-dim img1)
	      always (loop for i fixnum from 0 below (image-x-dim img1)
			   always (bind-int-pixel-vector-elements
				   (r1 g1 b1)
				   (viref img1 i j)
				   (bind-int-pixel-vector-elements
				    (r2 g2 b2)
				    (viref img2 i j)
				    (and (= r1 r2) (= g1 g2) (= b1 b2)))))))))

(defun vdiref-vector-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (require-image-rows
   (img1 img2)
   (loop for j fixnum from 0 below (image-y-dim img1)
	 always (loop for i fixnum from 0 below (image-x-dim img1)
		      always (bind-dfloat-pixel-vector-elements
			      (r1 g1 b1)
			      (vdiref img1 i j)
			      (bind-dfloat-pixel-vector-elements
			       (r2 g2 b2)
			       (vdiref img2 i j)
			       (and (= r1 r2) (= g1 g2) (= b1 b2))))))))

(defun getline-scalar-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (with-scan-line-buffers ((buf1 (make-integer-scan-line-buffer img1))
			   (buf2 (make-integer-scan-line-buffer img2)))
    (require-image-rows (img1 img2)
      (loop for j fixnum from 0 below (image-y-dim img1)
	    do (image-getline img1 buf1 0 j)
	       (image-getline img2 buf2 0 j)
	    always   (loop for i fixnum from 0 below (image-x-dim img1)
			   always (= (aref buf1 i) (aref buf2 i)))))))

(defun int-array-= (buf1 buf2)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type image-int-buffer-type buf1 buf2))
  (loop for i fixnum from 0 below (array-dimension buf1 0)
	always (= (aref buf1 i) (aref buf2 i))))

(defun double-array-= (buf1 buf2)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type image-float-buffer-type buf1 buf2))
  (loop for i fixnum from 0 below (array-dimension buf1 0)
	always (= (aref buf1 i) (aref buf2 i))))

(defun int-getline-vector-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (with-scan-line-buffers ((bufs1 (make-integer-scan-line-buffers img1))
			   (bufs2 (make-integer-scan-line-buffers img2)))
    (require-image-rows
     (img1 img2)
     (loop for j fixnum from 0 below (image-y-dim img1)
	   do (image-getline img1 bufs1 0 j)
	      (image-getline img2 bufs2 0 j)
	   always (loop for buf1 in bufs1
			for buf2 in bufs2
			always (int-array-= buf1 buf2))))))


(defun float-getline-vector-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (with-scan-line-buffers ((bufs1 (make-dfloat-scan-line-buffers img1))
			   (bufs2 (make-dfloat-scan-line-buffers img2)))
    (require-image-rows
     (img1 img2)
     (loop for j fixnum from 0 below (image-y-dim img1)
	   do (image-getline img1 bufs1 0 j)
	      (image-getline img2 bufs2 0 j)
	   always (loop for buf1 in bufs1
			for buf2 in bufs2
			always (double-array-= buf1 buf2))))))

(defun float-getline-viref-vector-image= (img1 img2)
  (declare (optimize (speed 3) (safety 0)))
  (with-scan-line-buffers ((bufs1 (make-dfloat-scan-line-buffers img1)))
    (destructuring-bind (rbuf gbuf bbuf) bufs1
      (declare (type image-float-buffer-type rbuf gbuf bbuf))
      (require-image-rows
       (img1 img2)
       (loop for j fixnum from 0 below (image-y-dim img1)
	     do (image-getline img1 bufs1 0 j)
	     always (loop for i fixnum from 0 below (image-x-dim img1)
			  always (bind-dfloat-pixel-vector-elements (r2 g2 b2)
								    (vdiref img2 i j)
								    (and (= (aref rbuf i) r2)
									 (= (aref gbuf i) g2)
									 (= (aref bbuf i) b2)))))))))


(defun test-int-getline0 (img)
  (with-scan-line-buffers ((buf (make-integer-scan-line-buffer img)))
    (loop for y from 0 below (image-y-dim img)
	  do (image-getline img buf 0 y))))

(defun test-int-getline1 (img)
  (with-scan-line-buffers ((bufs (make-integer-scan-line-buffers img)))
    (loop for y from 0 below (image-y-dim img)
	  do (image-getline img bufs 0 y))))


(defun test-int-getline2 (img bufs)
  (loop for y from 0 below (image-y-dim img)
	do (image-getline img bufs 0 y)))

(defun test-float-getline1 (img)
  (with-scan-line-buffers ((bufs (make-dfloat-scan-line-buffers img)))
    (loop for y from 0 below (image-y-dim img)
	  do (image-getline img bufs 0 y))))

(defun copy-vector-image (img1 &optional (img2 (similar-image img1)))
  (declare (optimize (speed 3) (safety 0)))
  (with-scan-line-buffers ((bufs (make-integer-scan-line-buffers img1)))
    (require-image-rows
     (img1 img2)
     (loop for j fixnum from 0 below (image-y-dim img1)
	   do (image-getline img1 bufs 0 j)
	      (image-putline img2 bufs 0 j)))
    img2))

#|

(maybe-compile-file-load "$FREEDIUS/lisp/img/getline-tests.lisp")

(setq old-array_image_size_limit
      (array_image_size_limit (* 100 100)))


(eval-cache-flush img )
(setq img (load-image "$HOME/pix/forestcat.tif"))
(setq img2 (make-color-image-from-rgb (red-image img) (green-image img) (blue-image img)))

(setq img2 (copy-vector-image img))

(gui::push-image img2 (gui::selected-window))

(setq pimg (load-image "$RADIUS/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/39943_rgb_0000010000.tiled512.tif"))
(setq pimg2 (make-color-image-from-rgb (red-image pimg) (green-image pimg) (blue-image pimg)))


(setq bi-image (make-image '(100 100) :element-type '(unsigned-byte 8) :samples-per-pixel 3))
(setq v-image (make-vector-image-from-band-interleaved-image bi-image))

(setq bufs (make-dfloat-scan-line-buffers v-image))
(setq ibufs (make-integer-scan-line-buffers v-image))

(image-getline img bufs 0 0)
(image-putline v-image bufs 0 0)
(image-getline v-image bufs 0 0)
(image-getline v-image ibufs 0 0)

(setq pbufs (make-integer-scan-line-buffers pimg))
(image-getline pimg pbufs 0 0)
(let ((x 10)) (list (viref pimg x 0) (viref pimg2 x 0)(loop for buf in pbufs collect (aref buf x))))

(viref bi-image 20 0)
(setf (viref bi-image 20 0) (math::civ 1 2 3))
(viref v-image 20 0)
(viref img 20 0)

(setq *print-array* t)

(list
(iref-scalar-image= (red-image img) (red-image img2))
(iref-scalar-image= (red-image img) (green-image img2))
(getline-scalar-image=  (red-image img) (red-image img2))
(viref-vector-image= img img2)
(vdiref-vector-image= img img2)
(int-getline-vector-image= img img2)
(float-getline-vector-image= img img2)
(float-getline-viref-vector-image= img img2)
 )


(time (loop repeat 100 do (test-int-getline1 img))) ; .75 secs              using rgb8 getline
(/ .75e9 (* 100 (image-x-dim img) (image-y-dim img)))  ; 32 ns/pixel


(time (loop repeat 100 do (test-float-getline1 img))) ; .81 secs
(/ .81e9 (* 100 (image-x-dim img) (image-y-dim img)))  ; 35 ns/pixel

(time (loop repeat 100 do (test-int-getline1 img2))) ; 1.15 secs              using vector-image-getline
(/ 1.15e9 (* 100 (image-x-dim img) (image-y-dim img)))  ; 49 ns/pixel
(/ 6.333e6 (* 100 (image-y-dim img)))                  ; 128 bytes/iter

(time (loop repeat 100 do (test-float-getline1 img2))) ;1.18 secs
(/ 1.18e9 (* 100 (image-x-dim img) (image-y-dim img)))  ; 51 ns/pixel

(time (loop repeat 10 do (test-int-getline1 pimg))) ; 1.26 secs              using rgb8 getline
(/ 1.26e9 (* 10 (image-x-dim pimg) (image-y-dim pimg)))  ; 57 ns/pixel

(time (loop repeat 10 do (test-int-getline1 pimg2))) ; 2.12 secs              using rgb8 getline
(/ 2.12e9 (* 10 (image-x-dim pimg2) (image-y-dim pimg2)))  ; 97 ns/pixel

(setq *print-array* nil)

(time (loop repeat 100 do (test-int-getline0 (first (component-images img2))))) ; .37 secs  no consing


(time (let ((img img2))
	(with-scan-line-buffers ((bufs (make-integer-scan-line-buffers img)))  ;  1.13 secs lots of consing
	  (loop repeat 100 do (test-int-getline2 img bufs)))))

(time (let ((img img))
	(with-scan-line-buffers ((buf (make-integer-scan-line-2d-buffer img))) ;  .49 secs lots of consing
	  (loop repeat 100 do (test-int-getline2 img buf)))))

(time (let ((img img2))
	(with-scan-line-buffers ((buf (make-integer-scan-line-2d-buffer img))) ;  .49 secs lots of consing
	  (loop repeat 100 do (test-int-getline2 img buf)))))

(time (let* ((img img) (biimg (band-interleaved-image img)))
	(with-scan-line-buffers ((bufs (make-integer-scan-line-buffers img)))   ; .72 secs lots of consing
	  (loop repeat 100 do (test-int-getline2 biimg bufs)))))

(/ 6.37e6 (* 100 (image-y-dim img))) ; 128 bytes/line
(* 100 (image-x-dim img) 3 8) = 1130400
(/ 6.37e6 1130400)

(setq rimg (red-image img))
(image-getline rimg (make-dfloat-scan-line-buffer rimg) 0 0)

(let ((buf (make-dfloat-scan-line-buffer rimg)))
  (image_getline_uchar (image-id rimg) buf 0 0 (length buf) 0 1 0))
|#





#|

(defmethod foo (image buffer x y
		      &optional (n (length buffer)) (to-start 0) (dx 1) (band 0))
)
  
(defmethod foo ((vector-image vector-image) buffer x0 y0 &optional n (to-start 0) (dx 1) (band 0))
  (with-class-slots vector-image (component-images) vector-image
    (if (consp buffer)
	(loop for image in component-images
	      for array in buffer
	      when array 
		do (foo image array x0 y0 n to-start dx band)
	      finally (return buffer))
	(error "Buffer arg must be a list of arrays: ~a" buffer))))

(defmethod foo ((image color-image) buffer x y
			  &optional n (to-start 0) (dx 1) (band 0))
  (with-class-slot-values color-image (band-interleaved-image) image
    (if band-interleaved-image
	(foo band-interleaved-image buffer x y n to-start dx band)
	(call-next-method))))


(defmethod foo ((image ARRAY-IMAGE-RGB) buffer x y
			  &optional n (to-start 0) (dx 1) (band 0))
  (declare (ignore band))
   buffer)


(defun test-foo (img bufs)
  (loop for y from 0 below (image-y-dim img)
	do (foo img bufs 0 y)))

(time (loop repeat 100 do (test-foo img nil)))  ; 1,580,800 bytes consed.
(/ 1.58e6 (* 100 (image-y-dim img))) ; 32 bytes/iter

(time (loop repeat 100 do (test-foo img2 '(t t t)))) ; 6,328,888 bytes consed.
(/ 6.323e6 (* 100 (image-y-dim img))); 128 bytes/iter




(setq complex-img (make-band-interleaved-vector-image '(100 100) :element-type 'single-float
						      :samples-per-pixel 2))

(setq cbufs (make-dfloat-scan-line-buffers complex-img))

(image-getline complex-img cbufs 0 0)


(setf (vdiref complex-img 0 0) (math::cv 1.0 2.0))
(vdiref complex-img 0 0)
(vdiref complex-img 1 0)

(setf (viref complex-img 0 0) (math::civ 1 2))
(viref complex-img 0 0)
(viref complex-img 1 0)

(diref (nth 0 (vector-image-component-images complex-img)) 0 0)
(diref (nth 1 (vector-image-component-images complex-img)) 0 0)

(setf (diref (nth 0 (vector-image-component-images complex-img)) 0 0) 10.0)

(disassemble 'array-image-single-float-diset)


(iref (nth 0 (vector-image-component-images complex-img)) 0 0) 
(setf (iref (nth 0 (vector-image-component-images complex-img)) 0 0) 20)
|#




#|
band-interleaved-paged-image is broken for this example:


(defparameter *erdas-img* (load-image "/m/rom1/downloads/tiff/other-pics/leica-example.tif"))

(defparameter *erdas-imgv* (make-vector-image-from-band-interleaved-image *erdas-img*))
(gui::push-image (car (component-images *erdas-imgv*)) (gui::selected-window))
;; lose
(image-block-x-dim (car (component-images *erdas-imgv*))) 256
(image-block-y-dim (car (component-images *erdas-imgv*))) -256

(eval-cache-flush-function 'load-image)

(defparameter *erdas-img2*
  (let ((*make-image-fn* 'make-array-image))
    (load-image "/m/rom1/downloads/tiff/other-pics/leica-example.tif")))


(defparameter *erdas-img2v* (make-vector-image-from-band-interleaved-image *erdas-img2*))
(gui::push-image (car (component-images *erdas-img2v*)) (gui::selected-window))
;;; good

(viref *erdas-img* 100 100)
#(44 53 57)
(gui::push-image (car (component-images *erdas-imgv*)) (gui::selected-window))

(eval-cache-flush-function 'load-image)

(defparameter *forestcat* (load-image "/homedir/quam/pix/forestcat.tif"))

(defparameter *forestcat2* 
  (let ((*array-image-default-block-dims* '(16 -16 16)))
    (load-image "/homedir/quam/pix/forestcat.tif")))

(gui::push-image *forestcat2* (gui::selected-window))

(defparameter *tst1* (new-make-image (image-dimensions *forestcat*)
				     :element-type '(unsigned-byte 8) :samples-per-pixel 3
				     :block-x-dim 16 :block-y-dim 16))

(defparameter *tst1* 
  (let ((*make-image-fn* 'make-tiff-file-image))
    (new-make-image (image-dimensions *forestcat*)
		    :element-type '(unsigned-byte 8) :samples-per-pixel 3
		    :block-x-dim 16 :block-y-dim 16)))

(defparameter *tst1v* (make-vector-image-from-band-interleaved-image *tst1*))

(setq *buf2d* (make-array (list (image-x-dim *tst1v*) 3) :element-type '(unsigned-byte 8)))

(setq *buf1d* (make-array (array-dimensions *buf2d*) :element-type '(unsigned-byte 8)))

(image-block-size (nth 0 (component-images *tst1v*)))
(image-samples-per-pixel *forestcat*)
(image-samples-per-pixel *tst1v*)
(image-samples-per-pixel *tst1*)
(image-samples-per-pixel (nth 0 (component-images *tst1v*)))
				
(describe *tst1*)


;;; this appears to work correctly
(with-scan-line-buffers ((bufs1 (make-dfloat-scan-line-buffers *tst1v*))
			 ;;(bufs2 (make-dfloat-scan-line-buffers *forestcat*))
			 )
  (loop for y fixnum from 0 below (image-y-dim *tst1v*)
	do (image-getline *forestcat* bufs1 0 y)
	   (image-putline *tst1v* bufs1 0 y)))

(viref *tst1v* 100 100)     => #(84 32 24)
(viref *forestcat* 100 100) => #(84 32 24)

(list (viref *tst1v* 0 0) (viref *forestcat* 0 0))
(#(197 64 40) #(197 64 40))

(list (viref *tst1v* 1 1) (viref *forestcat* 1 1))
(#(59 57 35) #(188 57 35))

(type-of *forestcat*) => color-image
(disassemble (color-image-viref-fn *forestcat*))
(image-element-size *forestcat*) => 24

(image-getline *forestcat* *buf2d* 0 100)
(loop for j from 0 below 3 collect (aref *buf2d* 100 j)) => (84 32 24)

(image-getline *tst1v* *buf2d* 0 100)
(Image-getline *tst1v* *buf2d* 0 0)
(image-getline *forestcat* *buf1d* 0 1)
(image-getline *tst1v* *buf2d* 0 1)
(loop for j from 0 below 3 collect (list (aref *buf1d* 0 j) (aref *buf2d* 0 j)))

(let ((x0 0) (y 1))
  (image-getline *forestcat* *buf1d* x0 y)
  (image-getline *tst1v* *buf2d* x0 y)
  (loop with red-image = (red-image *forestcat*)
	for x from 0 below 50 collect (list ;(aref (viref *forestcat*(+ x x0) y) 0)
					    ;;(iref red-image x y)
				       (aref *buf1d* x 0)(aref *buf2d* x 0))))
  
 
(let* ((buf (make-array (list (image-x-dim *tst1v*) 3) :element-type '(unsigned-byte 8))))
  (loop for y fixnum from 0 below (image-y-dim *tst1v*)
	do (image-getline *forestcat* buf 0 y)
	   (image-putline *tst1v* buf 0 y)))

(gui::push-image *tst1v* (gui::selected-window))
(gui::pop-view (gui::selected-window))

(loop for a in (component-images *forestcat*)
      for b in (component-images *tst1v*)
      collect (mv-list (image-element-min-max (image-subtract a b))))
((0.0 0.0) (0.0 0.0) (0.0 0.0))


(defun map-tile-nums (map offset-bits) 
  (loop for i from 0 below (length map)
	collect (ash (aref map i) (- offset-bits))))

(defun map-tile-nums (map offset-bits delta) 
  (loop for i from 0 below (length map) by delta
	collect (ash (aref map i) (- offset-bits))))


(let* ((img *erdas-img*) (offset-bits (tile-offset-bits img)))
  (list (map-tile-nums (image-x-map img) offset-bits (image-block-x-dim img))
	(map-tile-nums (image-y-map img) offset-bits (abs (image-block-y-dim img)))))

(let* ((img (car (component-images *erdas-imgv*)))
       (offset-bits (tile-offset-bits img)))
  (list (map-tile-nums (image-x-map img) offset-bits (image-block-x-dim img))
	(map-tile-nums (image-y-map img) offset-bits (abs (image-block-y-dim img)))))


(car (component-images *erdas-imgv*))
|#