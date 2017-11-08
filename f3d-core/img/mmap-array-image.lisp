(in-package :cl-user)

(assert (and (member :sbcl *features*) (member :x86-64 *features*)))

#|

(SB-SYS:GET-PAGE-SIZE )  = 4096

ARRAY-TOTAL-SIZE-LIMIT 
1152921504606846973

 #include <sys/mman.h>

int mprotect(const void *addr, size_t len, int prot);

tiffinfo -s /opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0
TIFF Directory at offset 0xe100008 (235929608)
  Image Width: 15240 Image Length: 15240
  Tile Width: 256 Tile Length: 256 Tile Depth: 1
  Bits/Sample: 8
  Sample Format: unsigned integer
  Compression Scheme: None
  Photometric Interpretation: min-is-black
  Orientation: row 0 top, col 0 lhs
  Samples/Pixel: 1
  Min Sample Value: 0
  Max Sample Value: 255
  Planar Configuration: single image plane
  Software: SGI's Image Format Library/1.0


Crap -- not aligned to multiple of 16  -- loses for SBCL64 array header.

Here are the rules for mmap:

    void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);

    The address returned will be aligned to a page boundary.
    offset must be a multiple of page_size

So, with the above tiled tiff file, mmap with offset=0.  Data starts 

    
  addr = mmap(0, 3600*256*256, prot, flags, fd, 0);
  data for tile 0 starts at addr+8
  
  There isn't room for the array header (needs 16 bytes) before the first tile.
  This means that the array-header goes in bytes 0-15, clobbering 8 bytes of the first tile.
  
  Note:  bigtiff headers have 8 more bytes which solves the problem.

|#


#|
mmap must start on a page boundary of both virtual memory and the file.
Where should the Lisp vector-header be placed?  
Options:
  1.  start of the page, returning the element-offset to the start of data.
  2.  immediately before the start of the data, returning the element-offset to the start of data.

Always:  array-data-addr must be divisible by 16
For element-type = RGB8, array-data-addr - offset-in-page must be divisible by 3.
|#

;;; SOLVE-LINEAR-DIOPHANTINE-EQUATION

(in-package :math)

#|
Solve a*x - b*y = c, where x, y, a, b, and c are integers.

Returns NIL if there are no solutions.

Returns x, y

|#

(defun euclid-series (a b)
  (loop when (< a b) 
	  do (rotatef a b)
	when (= b 1)
	  collect a
	  and do (loop-finish)
	else collect (mv-bind (q r) (floor a b)
		       (setq a b
			     b r)
		       q)))

;;(euclid-series 87 64) = (1 2 1 3 1 1 2)

(defun solve-linear-diophantine-equation (a b c)
  (let ((gcd (gcd a b)))
    (when (> gcd 1)
      (unless (zerop (mod c gcd))
	(return-from solve-linear-diophantine-equation nil))
      (setq a (floor a gcd)
	    b (floor b gcd)
	    c (floor c gcd)))

    (loop for top in (euclid-series a (- b))
	  with a00 = 0
	  with a01 = 1
	  with a10 = 1
	  with a11 = 0
	  do (let ((a02 (+ (* top a01) a00))
		   (a12 (+ (* top a11) a10)))
	       (setq a00 a01 a01 a02
		     a10 a11 a11 a12))
	     ;;collect (list a01 a11)
	  finally 
       (return (let* ((x a10)
		      (y a00)
		      (rhs (+ (* a a10) (* b a00))))
		 (when (< rhs 0) (setq x (- x) y (- y)))
		 (values (* c x) (* c y) a00 a10 rhs)
	 
		 )))))

#|

(mv-list (solve-linear-diophantine-equation 87 -64 3)) (-75 -102 34 25 -1)

(mv-list (solve-linear-diophantine-equation 16 -3 8)) (8 40 5 1 1)

(mv-bind (x y) (solve-linear-diophantine-equation 16 -3 8)
  ;; 16x-3y=8  
  ;; 16*(x+3n) - 3(y+16n) = 8
  ;; want 16*x' =< 0, thus x' = x+3n =< 0, n >= -x/3
  (let ((n (floor (- x) 3)))
    (values (+ x (* 3 n)) (+ y (* 16 n)))))
-1 -8
  
(mv-bind (x y) (solve-linear-diophantine-equation 16 -3 16)
  ;; 16x-3y=8  
  ;; 16*(x+3n) - 3(y+16n) = 8
  ;; want 16*x' =< 0, thus x' = x+3n =< 0, n >= -x/3
  (let ((n (floor (- x) 3)))
    (values (+ x (* 3 n)) (+ y (* 16 n)))))
  -2 -16

(floor (+ 4096 8) 3) 

|#


(in-package :img)
  
(defparameter *mmapped-regions* nil)

(defun unmmap-regions ()
  (loop while *mmapped-regions*
	for (sap length) = (pop *mmapped-regions*)
	do (sb-posix:munmap sap length)))

;  (unmmap-regions)

(defun make-mmapped-foreign-vector (total-elems element-type path file-offset-bytes
				    &key
				    (prot sb-posix::prot-read) 
				    (type sb-posix:map-private)
				    (array-byte-offset 0))
  (multiple-value-bind (typecode element-size)
      (qffi::array-element-type-code-from-type-spec element-type)
    (let* ((bytes-per-element (ash element-size -3))
	   (total-bytes (* bytes-per-element total-elems))
	   (page-size (SB-SYS:GET-PAGE-SIZE ))
	   (byte-offset-in-page (logand file-offset-bytes (1- page-size)))
	   (byte-offset-to-page (- file-offset-bytes byte-offset-in-page))
	   ;; byte-offset from first element of array to file-offset-bytes
	   ;; must be divisible by (* bytes-per-element alignment-modulus)
	   ;; Thus header-offset = (+ 16 (- ))
	   ;; byte offset in page for first array element
	   (header-offset (+ page-size (- byte-offset-in-page array-byte-offset 16)))
	   (mmap-bytes (+ page-size byte-offset-in-page total-bytes))
	   (fd-null (sb-unix::unix-open "/dev/zero"sb-unix::O_RDONLY #o666))
	   (sap (sb-posix:mmap nil mmap-bytes
				(logior sb-posix::prot-read  sb-posix::prot-write)
				sb-posix::map-private 
				fd-null 0))
	   )
      (when sap
	;;(setq *foo* (list header-offset array-byte-offset bytes-per-element))
	(push (list sap mmap-bytes) *mmapped-regions*)
	(let* ((fd (sb-unix::unix-open path sb-unix::O_RDONLY #o666)))

	  (sb-posix:mmap (sb-sys:int-sap (+ page-size (sb-sys:sap-int sap)))
			 (+ byte-offset-in-page total-bytes)
			 prot  
			 (logior type sb-posix:map-fixed)
			 fd
			 byte-offset-to-page))
	  (setf (sb-kernel::sap-ref-64 sap header-offset) 
		typecode
		(sb-kernel::sap-ref-64 sap (+ header-offset 8))
		(ash (+ total-elems (floor array-byte-offset bytes-per-element)) 3)) ; fixnum

	  (values (sb-kernel:%make-lisp-obj (+ (sb-kernel::sap-int sap) header-offset 15))
		  (floor array-byte-offset bytes-per-element)
		  sap
		  total-bytes)))))

;(trace math::solve-linear-diophantine-equation)
(defun make-mmapped-array-image (path )
  (destructuring-bind (pixel-element-type xdim ydim blk-xdim blk-ydim first-tile-offset-bytes flags)
      (tiff-file-header-params path)
    (let* ((samples-per-pixel 1)
	   (abs-blk-ydim (abs blk-ydim))
	   (blks-wide (ceiling xdim blk-xdim))
	   (blks-hi (ceiling ydim abs-blk-ydim))
	   (total-pixels (* blks-wide blks-hi blk-xdim abs-blk-ydim))
	   (element-type (case pixel-element-type
			  (RGB8 (setq samples-per-pixel 3)
				'(unsigned-byte 8))
			  (RGBA8 (setq samples-per-pixel 4)
				 '(unsigned-byte 8))
			  (otherwise pixel-element-type)))
	   (total-elements (* samples-per-pixel total-pixels))
	   (page-size (SB-SYS:GET-PAGE-SIZE ))
	   (byte-offset-in-page (logand first-tile-offset-bytes (1- page-size)))
	   (element-size (nth-value 1 (qffi::array-element-type-code-from-type-spec element-type)))
	   (bytes-per-element (ash element-size -3))
	   (bytes-per-pixel (* element-size samples-per-pixel))
	   (array-byte-offset
	    (if (or ;;(and (= samples-per-pixel 1) (= element-size 8))
		    (zerop (mod byte-offset-in-page bytes-per-pixel)))
		byte-offset-in-page
		(mv-bind (x y) (math::solve-linear-diophantine-equation 
				  16 (- bytes-per-pixel) byte-offset-in-page)
		    (let* ((n (floor (- x) bytes-per-pixel))
			   (xp (+ x (* bytes-per-pixel n))))
		      (- byte-offset-in-page (* 16 xp))))))
	   )
      (mv-bind (array element-offset sap total-bytes) 
	  (make-mmapped-foreign-vector total-elements element-type path first-tile-offset-bytes
				       :array-byte-offset array-byte-offset
				       )
	(let* ((*mmapped-array-image-array* array)
	       (pixel-offset (floor element-offset samples-per-pixel))
	       (image-id (make_array_image xdim ydim 
	            				  (l2c-element-type pixel-element-type) samples-per-pixel 
					  blk-xdim blk-ydim blk-xdim))
	       (image (load-image-wrap-image 
		       (wrap-foreign-image image-id :samples-per-pixel samples-per-pixel))))
	  (flet ((offset-y-map (image element-offset)
		   (offset-image-ymap image element-offset)
		   (set-image-maps image (image-x-map image) (image-y-map image))
		   ))
	    (if (typep image 'vector-image)
		(offset-y-map (band-interleaved-image image) pixel-offset)
		(offset-y-map image pixel-offset)))
	      
	  (setf (get-prop image :mmap-params) (list sap total-bytes pixel-offset ))
	  image)))))

(defun offset-image-ymap (image element-offset)
  (declare (fixnum element-offset))
  ;;(setq *foo-ymap (list image (image-y-map image) element-offset))
  ;; We have a serious problem here when element-offset is negative.
  ;; The C++ image code contains "typedef unsigned int MAP_ELEMENT_TYPE;", causing
  ;; negative numbers to become huge (2^32-n) positive numbers.  I do not really want to 
  ;; change to "typedef signed int MAP_ELEMENT_TYPE;", because that would reduce the size of 
  ;; paged images by a factor of 2.
  ;; The best answer is to use "typedef signed long MAP_ELEMENT_TYPE;" on x86-64.

  ;; We have another serious problem here: For element-type = RGB8, the element-offset must be
  ;; divided by 3, and most offsets are not divisable by 3.  This really sucks big time.  For
  ;; element-type = RGBA8, the element-offset must be divided by 4.
  (loop with ymap = (image-y-map image)
	with n fixnum = (length ymap)
	for y fixnum from 0 below n
	for new-elem = (+ (aref ymap y) element-offset)
	when (>= new-elem 0)
	  do (setf (aref ymap y) new-elem)
	else when (= y (1- n))
	       do (setf (aref ymap y) (aref ymap (1- y)))
	else when (= y 0) 
	       do (setf (aref ymap y) (+ element-offset (aref ymap (1+ y)) ))
	))
		 
(def-foreign-function (remap_file_pages_int (:name "remap_file_pages"))
    (addr :unsigned-long)
  (size :unsigned-long)
  (prot :int)
  (pgoff :unsigned-long)
  (flags :int))

(defun remap_file_pages (sap size prot pgoff flags)
  (remap_file_pages_int (sb-sys::sap-int sap) size prot pgoff flags))
 

#|

(tiff-file-header-params "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0")
((UNSIGNED-BYTE 8) 15240 15240 256 -256 8 0)

(tiff-file-header-params "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif")
(RGB8 5908 5936 512 -512 8 0)
load_tiff_lazy_image element_size=24, element_type=14, spp=1, bytes_per_page=786432
(* 3 512 512)
(- 786440 8)

(logand 139938708123647 15)
(logand 139938472194063 15)

(defparameter *Olalla69-file-image* (load-image "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0"))
(gui::push-image *Olalla69-file-image* (gui::selected-window))



(defparameter *Olalla69-mapped-image* 
  (make-mmapped-array-image "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0"))
(gui::push-image *Olalla69-mapped-image* (gui::selected-window))


(defparameter *san-diego-mmapped-image* 
  (make-mmapped-array-image "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif"))
(gui::push-image *san-diego-mmapped-image* (gui::selected-window))

(defparameter *san-diego-file-image* 
  (load-image "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif"))
(gui::push-image *san-diego-file-image* (gui::selected-window))
(image-padded-block-x-dim *san-diego-file-image*)512

tiffinfo "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif"
Unhandled memory fault at #x7F82A5FD1008.
#.(SB-SYS:INT-SAP #X7F81A5FD1000)
(- #x7F82A5FD1008 #X7F81A5FD1000) = 4294967304


(defparameter *douglas-co-mapped-image* 
  (make-mmapped-array-image "/home/rugby/quam/pix/douglasco/MO3946CCC403802A5.tif"))
(gui::push-image *douglas-co-mapped-image* (gui::selected-window))
Unhandled memory fault at #x7F84AC773FF8.
(let* ((array (image-array (band-interleaved-image *douglas-co-mapped-image*)))
       (addr (%pointer array)))
  (format nil "#X~x" addr))
#X7F0ADC15EFEF
(get-prop *douglas-co-mapped-image* :mmap-params)
(#.(SB-SYS:INT-SAP #X7F0ADB5D6000) 4572000 8) old (correct) version

(typep *douglas-co-mapped-image* 'vector-image)
(gui::pop-view (gui::selected-window))
(gui::view-image (gui::top-view))

(let ((y-map (image-y-map (band-interleaved-image *douglas-co-mapped-image*))))
  (values (aref y-map (1- (length y-map)))
	  (aref y-map (- (length y-map) 2))))
(loop with y-map = (image-y-map (band-interleaved-image *douglas-co-mapped-image*))
      for y downfrom (1- (length y-map)) repeat 4
      collect (aref y-map y))
(loop with x-map = (image-x-map (band-interleaved-image *douglas-co-mapped-image*))
      for x from 0 repeat 4
      collect (aref x-map x))
(array-element-type (image-array (band-interleaved-image *douglas-co-mapped-image*)))
(UNSIGNED-BYTE 8)

(defparameter *douglas-co-file-image* 
  (load-image "/home/rugby/quam/pix/douglasco/MO3946CCC403802A5.tif"))
(gui::push-image *douglas-co-file-image* (gui::selected-window))

(tiff-file-header-params  "/home/rugby/quam/pix/douglasco/MO3946CCC403802A5.tif")
(RGB8 1270 1200 1270 -1200 8 0)
(image-padded-block-x-dim *douglas-co-mapped-image*) 1270
(image-padded-block-x-dim *douglas-co-file-image*) 1270

(floor (* 3 1270) 4) 952 2

(let* ((array (image-array (band-interleaved-image *san-diego-mmapped-image*)))
       (addr (%pointer array)))
  addr)

(defparameter *alv-2-44-g2-mmapped-image* ; lose -- jpeg file
  (make-mmapped-array-image "RADIUS/site-2d-worlds/alv/alv-2-44/full-tiff/image.g2"))

(defun get-pixels1 (img1
		    img2
		    &key 
		    (x0 8) (nx 16)
		    ;;(y (1- (image-y-dim img1)))
		    (y (- (image-y-dim img1) 2))
		    )
  (loop for x from x0 repeat nx
	collect (vdiref img1 x y (math::make-coordinate-vector 3)) into l1
	collect (vdiref img2 x y (math::make-coordinate-vector 3 )) into l2
	finally (return (list l1 l2))))

(get-pixels1 *san-diego-mmapped-image* *san-diego-file-image* :x0 1000 :y 1000)
(get-pixels1 *san-diego-mmapped-image* *san-diego-file-image*)

(get-pixels1 *douglas-co-mapped-image* *douglas-co-file-image* :x0 270 :y 252)
(get-pixels1 *douglas-co-mapped-image* *douglas-co-file-image* :x0 0 :y 1269)
(get-pixels1 *douglas-co-mapped-image* *douglas-co-file-image* :x0 0 :y 1268)

(let* ((img1  *san-diego-mmapped-image*)
       (img2  *san-diego-file-image*)
       (x0 8) (nx 16)(y (1- (image-y-dim img1)))
       )
  (loop for x from x0 repeat nx
	collect (list (vdiref img1 x y (math::make-coordinate-vector 3 ))
		      (vdiref img2 x y (math::make-coordinate-vector 3 )))))

(gui::pop-view (gui::selected-window))

(let ((x 4000) (y 4000))
  (list (iref *mapped-image* x y)
	(iref *file-image* x y)))

(loop with img1 = *mapped-image*
      with img2 = *file-image*
      with y = 4000
      for x fixnum from 0 below (image-x-dim img1)
      always (= (iref img1 x y) (iref img2 x y)))

(loop with img1 = *mapped-image*
      with img2 = *file-image*
      with y = (1- (image-y-dim img1))
      for x fixnum from 0 below (image-x-dim img1)
      unless (= (iref img1 x y) (iref img2 x y))
	collect x)



(%pointer (array-image-array *mapped-image*))
139938236260399
(sb-kernel::sap-int (caar *mmapped-regions*))
139938472194048
(- (%pointer (array-image-array *mapped-image*)) (sb-kernel::sap-int (caar *mmapped-regions*)))




|#
	   
       
      







#|

(def-foreign-function (linux_mprotect (:name "mprotect"))
  (addr :pointer)
  (len :unsigned-long)
  (prot :int))

(let ((sap (car (get-prop *douglas-co-mapped-image* :mmap-params))))
  (linux_mprotect sap 4096 mmap-PROT_NONE))

(let ((sap (car (get-prop *douglas-co-mapped-image* :mmap-params))))
  (linux_mprotect sap 4096 mmap-PROT_READ))


|#