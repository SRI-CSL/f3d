(in-package :img)

#|
Convert IU-TESTBED image to TIFF.

|#
#|
raw2tiff -H <header-length> -w xdim -l ydim -d <element-type> -c none 

raw2tiff doesn't support tiling

|#


(def-foreign-function (iu_testbed_image_file_header_params 
		       (:name (freedius-prefix "iu_testbed_image_file_header_params")))
    (path :simple-string)
  (params :simple-array) ; int
  )

(defun get-iu-testbed-header-params (path)
  (let ((params (make-array 10 :element-type '(signed-byte 32))))
    (iu_testbed_image_file_header_params (namestring (truename path)) params)
    params))



#|
(listarray (get-iu-testbed-header-params "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g0"))
(3 4096 4096 128 -64 128 0 0 0 0)
(parse-property-list-string (read-image-property-list-string (namestring (truename "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g5"))))

(:SUN-VECTOR (0.6082805 -0.1486257 0.7796828) :NAME "Alv-2-44"
 :IMAGE-TO-2D-TRANSFORM
 #<TRANSFORMS:4X4-COORDINATE-TRANSFORM (NIL to NIL) {10043A3AF1}> :2D-WORLD
 (EVAL
  (IC::MAKE-CACHED-INSTANCE '2D-WORLD :NAME '"Alv 3d World 2d World 21"
   :PROPERTY-LIST
   '(:BASE-IMAGE-LOAD-FORM
     (IC::LOAD-IMAGE "/tmp_mnt/home/clam2/quam-data/alv/alv-2-44.g0"))
   :3D-TO-2D-PROJECTION
   (MAKE-INSTANCE 'CME::4X4-COORDINATE-PROJECTION :R/F 0.0 :|1/F|
                  -1.9734315333914284e-4 :PRINCIPAL-POINT-U 1665.6791
                  :PRINCIPAL-POINT-V 1986.6626 :POSITIVE-W-CLIP-PLANE 0.0
                  :TRANSFORM-MATRIX
                  (MATH:MAKE-AND-FILL-4X4-MATRIX 0.9996994 0.013477996
                                                 -0.021072589 1812.722
                                                 -0.014055679 0.9992905
                                                 -0.03458094 2137.357
                                                 0.020597093 0.03486902
                                                 0.9991798 21716.98 0.0 0.0 0.0
                                                 1.0)
                  :PROPERTY-LIST (LIST))
   :3D-WORLD (GUI:GET-3D-WORLD-NAMED '"Alv 3d World"))))
|#

(defun convert-iu-testbed-image-to-tiff (iutb-path tiff-path &key block-x-dim block-y-dim
					 (samples-per-pixel 1))
  (let* ((img (load-image iutb-path))
	 (xdim (image-x-dim img))
	 (ydim (image-y-dim img)))
    (unless (and block-x-dim block-y-dim)
      ;; default to raster 
      (setq block-x-dim xdim
	    block-y-dim (- ydim)))
  
    (let ((to-img (make-image (list xdim ydim) :element-type (image-element-type img)
			      :block-x-dim block-x-dim
			      :block-y-dim block-y-dim
			      :samples-per-pixel samples-per-pixel))
	  (buf (make-dfloat-scan-line-buffer img)))
      (loop for y fixnum from 0 below ydim
	    do (image-getline img buf 0 y)
	       (image-putline to-img buf 0 y))
      (save-image to-img tiff-path)
      (unmake-image img)
      (unmake-image to-img))))


#|
(convert-iu-testbed-image-to-tiff "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g5"
				  "/tmp/tiff-tests/alv-2-44.g5.tiff")


tiffinfo /tmp/tiff-tests/alv-2-44.g5.tiff
TIFF Directory at offset 0x4008 (16392)
  Image Width: 128 Image Length: 128
  Bits/Sample: 8
  Sample Format: unsigned integer
  Compression Scheme: None
  Photometric Interpretation: min-is-black
  Orientation: row 0 top, col 0 lhs
  Samples/Pixel: 1
  Rows/Strip: 1
  Planar Configuration: single image plane


(convert-iu-testbed-image-to-tiff "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g0"
				  "/tmp/tiff-tests/alv-2-44.g0.tiff"
				  :block-x-dim 256 :block-y-dim -256)

(convert-iu-testbed-image-to-tiff "$RADIUS/sites/alv/alv-dtm.g0"
				  "/tmp/tiff-tests/alv-dtm.g0.tif")

(defparameter *alv-dtm* (load-image "$RADIUS/sites/alv/alv-dtm.g0"))
(image-element-min-max *alv-dtm*)
55.500003814697266
79.80000305175781

(defparameter *alv-dtm-tif* (load-image "/tmp/tiff-tests/alv-dtm.g0.tif"))

(gui::push-image *alv-dtm-tif* (gui::selected-window))
(image-element-min-max *alv-dtm-tif*)
55.500003814697266
79.80000305175781

(iref *alv-dtm-tif* 100 100); 60


(def-foreign-function (swap_endian_single_float (:name (qffi::freedius-prefix "swap_endian_single_float")))
    (arr :simple-array)
  (nbytes :int))

(let ((arr (make-array 1 :element-type 'single-float)))
  (setf (aref arr 0) (float 7.707141553786494e-44 1f0))
  (swap_endian_single_float arr 4)
  (aref arr 0))
7.6293945f-6

#define	EINVAL		22	/* Invalid argument */
#define	EBADF		 9	/* Bad file number */
#define	EOVERFLOW	75	/* Value too large for defined data type */
#define	ESPIPE		29	/* Illegal seek */



Breakpoint 1, FREEDIUS_brk (str=0x7fffeff25d01 "seek error")
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image.c++:120
Current language:  auto; currently c++
(gdb) bt
#0  FREEDIUS_brk (str=0x7fffeff25d01 "seek error")
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image.c++:120
#1  0x00007fffeff12e31 in FREEDIUS::file_image_page_handler::read_page (this=0x8e5400, page_number=77)
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image.c++:157
#2  0x00007fffefee08b6 in FREEDIUS::basic_page_handler::get_page (this=0x8e5400, page_number=77)
    at /opt/IU/FREEDIUS/F3D-modular/c/img/page_handler.c++:661
#3  0x00007fffeff1299b in FREEDIUS::file_image_page_handler::force_page_initialization (this=0x8e5400)
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image.c++:221
#4  0x00007fffeff129d6 in FREEDIUS::file_image_page_handler::unmap_file (this=0x8e5400, closep=1)
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image.c++:230
#5  0x00007fffeff12abf in FREEDIUS::file_image_page_handler::save_image (this=0x8e5400, img=0x8d2fb0, 
    path=0x1003121820 "/tmp/tiff-tests/alv-2-44.g0.tiff")
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image.c++:341
#6  0x00007fffeff1c131 in FREEDIUS::paged_image_base::save_image (this=0x8d2fb0, 
    path=0x1003121820 "/tmp/tiff-tests/alv-2-44.g0.tiff", error_ok=0)
    at /opt/IU/FREEDIUS/F3D-modular/c/img/file-image-io.c++:233
#7  0x00007fffeff1b92b in FREEDIUS::save_image (img=0x8d2fb0, 
    path=0x1003121820 "/tmp/tiff-tests/alv-2-44.g0.tiff", error_ok=0)
    at /opt/IU/FREEDIUS/F3D-modular/c/img/image-io.c++:219
#8  0x00007fffeff15e83 in FREEDIUS_save_image (img=0x8d2fb0, 
    path=0x1003121820 "/tmp/tiff-tests/alv-2-44.g0.tiff")
    at /opt/IU/FREEDIUS/F3D-modular/c/img/cimage.c++:288

*foo*


|#



