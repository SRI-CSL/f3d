(in-package :transforms)

#|
(get-ellipsoid 'wgs-84)
(get-gdc 'wgs-84)
(get-gcc 'wgs-84)
(gdc wgs-84 wgs84-15m)
(gcc wgs84)
(canonical-coordinate-transform-path (gdc wgs84) (gcc wgs84))

(make-frame-camera )
|#
 
(defun make-frame-camera-and-2d-world (&rest keyvals &key parent name 2d-world &allow-other-keys)
  (let* ((frame-camera-projection (apply #'make-frame-camera keyvals))
	 (2d-world (or 2d-world (gui::get-or-make-2d-world :name name :3d-world parent))))
    ;; (CME::CHANGE-PROJECTION 2d-world frame-camera-projection) also does this next:
    ;; and is called by LOAD-CAMERA-MODEL2 
    (transforms::set-transform-coordinate-systems frame-camera-projection parent 2d-world)
    (when 2d-world
      (setf (object-to-parent-transform 2d-world) (inverse-transform frame-camera-projection)))
    2d-world))
	 
   
(defmacro FRAME-CAMERA (&rest args &key parent name 2d-world
			origin 
			Euler-angles
			principal-point focal-length skew aspect-ratio
			near-far 
			&allow-other-keys)
  (declare (ignore parent name 2d-world origin Euler-angles 
		   principal-point focal-length skew aspect-ratio near-far))
  `(make-frame-camera-and-2d-world
    .,(loop for (key val) on args by #'cddr
	    collect key
	    collect (if (and (consp val) (not (eq key :parent))) `',val val))))
    
#|
(fmakunbound 'FRAME-CAMERA)

(FRAME-CAMERA :PARENT (LVCS (GDC NAD27):lat 39.50586158474837 :long -105.11441597675547
			       :LOCAL-UNITS-PER-METER *feet-per-meter*)
	      :ORIGIN (0.0 0.0 21716.98)
	      :Euler-angles (:OMEGA-PHI-KAPPA -2.004457029560062 1.185066214677839 0.8097247922823747)
	      :PRINCIPAL-POINT (1665.6791 1986.6626 -5067.3154)
	      :SKEW -0.0083902 :ASPECT-RATIO 1.0000105)

(describe (setq *frame-camera*
(FRAME-CAMERA :name "alv-2-44"
	      :PARENT (LVCS (GDC NAD27):lat 39.5 :long -105.12083333333332 
			    :LOCAL-UNITS-PER-METER *feet-per-meter*) 
	      :ORIGIN (1812.722 2137.357 21716.98)
	      :OMEGA-PHI-KAPPA (-1.998524969960044 1.1801977455201356 0.8055201643433042)
	      :PRINCIPAL-POINT (1665.6791 1986.6626)
	      :FOCAL-LENGTH -5067.3154
	      :SKEW -0.0083902 :ASPECT-RATIO 1.0000105)

PROJECTION-MATRIX: #2A((1.006613883593994 -0.002834755877018616
                        -0.30784895598998513 4866.897373276152)
                       (0.02159348341135494 1.012871143007154
                        -0.35686593034122976 5546.040061861141)
                       (4.158531457344631e-6 6.824311031253989e-6
                        -1.9718127589835556e-4 2.4132236475529867)
                       (4.158531457344631e-6 6.824311031253989e-6
                        -1.9718127589835556e-4 4.260057574645821)).
3D-TO-CAMERA-MATRIX: #2A((0.9996890569143533 -0.014055533577194183
                          0.0205968798984447 -2229.418682364875)
                         (0.013331744653748348 0.9993030575429711
                          0.03486637018326328 -2917.226395935629)
                         (-0.021072590495186894 -0.03458093638306322
                          0.999179715951386 -21587.05535288942)
                         (0.0 0.0 0.0 1.0)).

(describe (3d-to-2d-projection (gui::top-view)))
PROJECTION-MATRIX: #2A((1.006603540097247 -0.0028346116582674964
                        -0.3078491447629036 4866.919914490999)
                       (0.021593482935399755 1.0128711592805353
                        -0.3568658961351635 5546.039285089431)
                       (4.158531107723449e-6 6.8243104575121e-6
                        -1.9718125932068832e-4 3.273341449793296)
                       (4.158531107723449e-6 6.8243104575121e-6
                        -1.9718125932068832e-4 4.2600572164890105)).
3D-TO-CAMERA-MATRIX: #2A((0.9996767617444122 -0.01420172295925684
                          0.020591557799247182 -2228.9683558189213)
                         (0.01333188471274901 0.9993135569238071
                          0.034866737178149414 -2917.2570607693883)
                         (-0.02107258872354609 -0.034580933475732115
                          0.9991796319471176 -21587.053537995896)
                         (0.0 0.0 0.0 1.0))

(cme::camera-position (3d-to-2d-projection (gui::top-view)))
#(1812.722000000001 2137.357000000001 21716.980000000003)

(loop for x in '(552.5187706375413 651.467716535433 6619.348742697485)
      collect (* *feet-per-meter* x))
(1812.722 2137.357 21716.98)
|#
