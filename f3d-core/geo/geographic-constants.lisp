(IN-PACKAGE :transforms)

(export '(WGS84 WGS72 NAD27))

#|
(transforms::INITIALIZE-GEOGRAPHIC-CONSTANTS)
|#

;;; for backward compatibility
(export '(*CLARKE-1866-ELLIPSOID*
	  *NAD-27-lat-long-to-geocentric-transform* 
	  *WGS-84-lat-long-to-geocentric-transform*
	  *WGS-72-lat-long-to-geocentric-transform*))

;;; For backward compatibility
(defvar *CLARKE-1866-ELLIPSOID*)
(defvar *NAD-27-lat-long-to-geocentric-transform*)
(defvar *WGS-84-lat-long-to-geocentric-transform*)
(defvar *WGS-72-lat-long-to-geocentric-transform*)


(defun initialize-geographic-constants ()

  (make-ellipsoid :name 'WGS84 :a 6378137d0 :b 6356752.3142d0)
  
  ;; parameters from DMA paper "The DOD World Geodetic System 1972" May 1974.
  (make-ellipsoid :name 'WGS72
		  :a 6378135d0 :b 6356750.5d0 
		  :shift-to-wgs84-gcc (cv -22.0 157.0 176.0))
	
  (make-ellipsoid :name 'NAD27
		  :a 6378206.4d0 :b 6356583.8d0   ; Clarke-1866 ellipsoid parameters
		  :shift-to-wgs84-gcc (cv -8.0 160.0 176.0))

  (make-ellipsoid :name 'NAD83
		  :a 6378137d0 :b 6356752.314 
		  :shift-to-wgs84-gcc (cv 0.0 0.0 0.0))
  (make-ellipsoid :name 'GRS80
		  :a 6378137d0 :b 6356752.314 
		  :shift-to-wgs84-gcc (cv 0.0 0.0 0.0))

  (setf *CLARKE-1866-ELLIPSOID* (get-ellipsoid 'nad27))

  ;; for backward compatibility
  (setf *NAD-27-lat-long-to-geocentric-transform* (to-geocentric-transform (get-gdc 'nad27))
	*NAD-83-lat-long-to-geocentric-transform* (to-geocentric-transform (get-gdc 'nad83))
	*WGS-84-lat-long-to-geocentric-transform* (to-geocentric-transform (get-gdc 'wgs84))
	*WGS-72-lat-long-to-geocentric-transform* (to-geocentric-transform (get-gdc 'wgs72)))
  )




(st::add-system-initialization :geographic-transforms
                               '(INITIALIZE-GEOGRAPHIC-CONSTANTS))
