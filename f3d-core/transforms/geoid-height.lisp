(in-package :transforms)

#|     ww15mgh.bin

  DMA (aka. NIMA aka. NGA) Worldwide 15 minute gridded geoid heights.

|#

(def-foreign-function (swap_endian_single_float (:name (qffi::freedius-prefix "swap_endian_single_float")))
    (arr :simple-array)
  (nbytes :int))


#+never ;; not needed
(defun swap-endian-u32 (bytes)
  (loop with n fixnum = (length bytes)
	for i fixnum from 0 below n by 4
	do (rotatef (aref bytes 3) (aref bytes 0))
	   (rotatef (aref bytes 2) (aref bytes 1)))
  bytes)
		    
(defclass geoid-height-table ()
    (minlat maxlat minlon maxlon delta-lat delta-lon table))

(defun-cached read-geoid-height-table (path)
  (with-open-file (st path :element-type '(unsigned-byte 32))
    (let ((hdr (make-array 7 :element-type 'single-float))
	  (o (make-instance 'geoid-height-table))
	  swap-endian )
      (read-sequence hdr st)
      (setq swap-endian (not (= (aref hdr 0) 1.0)))
      (when swap-endian
	(swap_endian_single_float hdr (* 4 (length hdr))))
      (with-class-slots geoid-height-table (minlat maxlat minlon maxlon delta-lat delta-lon table) o
	(setf minlat (aref hdr 1)
	      maxlat (aref hdr 2)
	      minlon (aref hdr 3)
	      maxlon (aref hdr 4)
	      delta-lat (aref hdr 5)
	      delta-lon (aref hdr 6))
	(let* ((xdim (1+ (ceiling (- maxlon minlon) delta-lon)))
	       (ydim (1+ (ceiling (- maxlat minlat) delta-lat)))
	       (img (img::make-image (list xdim ydim) :element-type 'single-float
				     :block-x-dim xdim :block-y-dim ydim))
	       (arr-1d (img::array-image-array img)))
	  (read-sequence arr-1d st)			 
	  (when swap-endian (swap_endian_single_float arr-1d (* 4 xdim ydim)))
	  (setf table img)
	  o)))))

(defmethod interpolate-height ((o geoid-height-table) long lat)
  (with-class-slots geoid-height-table (minlat maxlat minlon maxlon delta-lat delta-lon table) o
    (img::interpolate-iref table
			   (/ (mod (- long minlon) 360.0) delta-lon)
			   (/ (- lat minlat) delta-lat)))) 

;(interpolate-height *ww15mgh-geoid-height-table*  -71.666 42.49) = -28.748855170898416

(defvar *ww15mgh-geoid-height-table* (read-geoid-height-table "/opt/IU/data/photogrammetry/ww15mgh.bin"))

(defmethod to-mean-sea-level (gcd-position &optional (geoid-height-table *ww15mgh-geoid-height-table*))
  (bind-vector-elements (long lat h) gcd-position
    (cv long lat (- h (interpolate-height geoid-height-table long lat))))) 

(defmethod from-mean-sea-level (msl-gdc-position &optional (geoid-height-table *ww15mgh-geoid-height-table*))
  (bind-vector-elements (long lat h) msl-gdc-position
    (cv long lat (+ h (interpolate-height geoid-height-table long lat))))) 

