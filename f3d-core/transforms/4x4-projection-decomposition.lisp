(in-package :transforms)
;;(in-package :math)

(eval-when (eval load compile)
(import '(math::RQ-DECOMPOSE math::SWAP-MATRIX-ROWS))
)

;;; Should this be in the TRANSFORMS package or the MATH package?

;;(export 'DECOMPOSE-PROJECTION-MATRIX)


#| *********************   4X4-PROJECTION-MATRIX DECOMPOSITION   ********************

Wed Jun 25 2003  LHQ:  There is a bug somewhere in this math.  Perhaps a sign error.
I am scrapping this implementation of DECOMPOSE-PROJECTION-MATRIX in favor
of one based on QR decomposition.  

Sun Nov 28 2004 LHQ: I think the sign of r/f is wrong.  Replace -r/f with GSD

THEORY:

Projection-matrix = s * camera-to-2d-matrix * 3d-to-camera-matrix

    where 3d-to-camera-matrix has an orthonormal upper 3x3 submatrix.


    and camera-to-2d-matrix
           =  (make-and-fill-4x4-matrix 1.0 m01 (* up 1/f) (- (* up r/f))        
                                        0.0 m11 (* vp 1/f) (- (* vp r/f))       
                                        0.0 0.0 1.0        0.0
                                        0.0 0.0 1/f        (- r/f))          

The 3d-to-camera-matrix 6 degrees of freedom: xc, yc, zc, omega, phi, kappa.

Non-orthographic case (1/f /= 0, r/f = 0):

  The camera-to-2d-matrix has 5 degrees of freedom: up vp 1/f m01 m11

  Define the vectors Pu, Pv, and Ps to contain the first 3 elements of rows 0, 1, and 3 respectively

  From the orthonormality of the upper 3x3 submatrix of P, we get the following 6 equations in 6 unknowns

             Ps.Ps = q = s^2/f^2   /= 0                               (a)

             Pu.Ps = up*q                                             (b)

             Pv.Ps = vp*q                                             (c)

             Pu.Pu = s^2 + s^2*m01*m01 + up*up*q                      (d)

             Pu.Pv =       s^2*m01*m11 + up*vp*q                      (e)

             Pv.Pv =       s^2*m11*m11 + vp*vp*q                      (f)

  Solution:

               up = Pu.Ps/q

               vp = Pv.Ps/q

   from (d)    s^2 = (Pu.Pu - up^2*q) / (1 + m01^2)  = kuu / (1 + m01^2) (g)

   from (e)    s^2 = (Pu.Pv - up*vp*q) / (m01*m11)   = kuv / (m01*m11)   (h)

   from (f)    s^2 = (Pv.Pv - vp^2*q) / m11^2        = kvv / m11^2       (i)

   from (g,h)  m11 = kuv * (1 + m01^2) / (m01 * kuu)                     (j)

   from (g,i)  m11^2 = kvv * (1 + m01^2) / kuu                           (k)

   from (j,k)  kuv^2 * (1 + m01^2)^2 / (m01 * kuu)^2 = kvv * (1 + m01^2) / kuu

               kuv^2 * (1 + m01^2) / m01^2 = kvv * kuu

               m01^2/(1 + m01^2) = kuv^2 / (kuu * kvv)

               a/(1+a) = b/c => a = b/(c-b)

               m01^2 = kuv^2 / (kuu*kvv - kuv^2)                         (l)

   compute s^2 by substituting m01^2 into (g)

   compute m11^2 by substituting s^2 into (i)

   compute 1/f  by substituting s^2 into (a):  1/f = - sqrt(q/s^2)


Orthographic case (1/f = 0, up = 0, vp = 0),

  The camera-to-2d-matrix has 3 degrees of freedom:  m01 m11 r/f

             Ps.Ps = q = 0

             r/f = -P33/s

             Pu.Pu = s^2 + s^2*m01*m01

             Pu.Pv =       s^2*m01*m11

             Pv.Pv =       s^2*m11*m11

   from (d)    s^2 = Pu.Pu / (1 + m01^2)  = kuu / (1 + m01^2) (g)

   from (e)    s^2 = Pu.Pv / (m01*m11)    = kuv / (m01*m11)   (h)

   from (f)    s^2 = Pv.Pv / m11^2        = kvv / m11^2       (i)

   solve for m01 m11 s as for non-orthographic case

Based on tests, the resulting solution is very accurate - approaching the full double-float precision.


The above derivation can be improved considerably:

   m01^2 = kuv^2 / (kuu*kvv - kuv^2) = -1 + kuu*kvv / (kuu*kvv - kuv^2)

   m11^2 = kvv/ss = kvv * (1 + m01^2) / kuu = kvv/kuu * kuu*kvv / (kuu*kvv - kuv^2)

   m11 = kvv /sqrt(kuu*kvv - kuv^2)

   m01^2/m11^2 = m01^2 / (kvv * (1 + m01^2) / kuu )
               = kuu/kvv * m01^2/(1 + m01^2)
               = kuu/kvv * (kuv^2/(kuu*kvv - kuv^2)) / (1 + kuv^2/(kuu*kvv - kuv^2))
               = kuu/kvv * kuv^2 / (kuu*kvv - kuv^2 + kuv^2)
               = kuu/kvv * kuv^2 / (kuu*kvv)
               = kuu/kvv * kuv^2 / (kuu*kvv)
               = kuv^2 / kvv^2

   m01/m11 = kuv/kvv

   ss = kuu / (1 + m01^2)
      = kuu / (1 + kuv^2/(kuu*kvv - kuv^2))
      = kuu * (kuu*kvv - kuv^2) / (kuu*kvv - kuv^2 + kuv^2)
      = kuu * (kuu*kvv - kuv^2) / (kuu*kvv)
      = kuu - kuv^2/kvv

|#

;;; *************************  DECOMPOSE-PROJECTION-MATRIX  *************************
;;; Perhaps this should move to math/transform-matrix.lisp

#|
(mv-list (decompose-projection-matrix (projection-matrix (3d-to-2d-projection (top-view)))))
|#

;;; The projection-matrix is assumed to be compatible with SGI 4x4 homogeneous
;;; perspective or orthographic transform matrix where the projection equations are:
;;; <x' y' z' w'> = M <x y z 1>
;;; u = x'/w', v = y'/w', depth = z'/w'

;;; When the projection is orthographic, row 3 of M (the last row) is <0 0 0 w>,
;;; where w is a scale factor relating world to image units.

;;; If the matrix is 4x4, only rows 0, 1, and 3 are used.  Row 2 is ignored.
;;; If the matrix is 3x4 a 4x4 matrix is made with row 2 moved to row 3.

(defparameter *decompose-projection-matrix-1/f-threshold* 1e-7)
;;; Wed Jun 25 2003  LHQ:  There is a bug somewhere in this math.  Perhaps a sign error.
;;; I am scrapping this implementation of DECOMPOSE-PROJECTION-MATRIX in favor
;;; of one based on QR decomposition.  

;;; This needs rewriting for allegro -- no type-reduce
;;; The resulting matrix is orthonormal in its upper 3x3 submatrix.

;;; broken
;;;(defun decompose-projection-matrix (3d-to-2d-matrix
;;;                                    &key (1/f-threshold *decompose-projection-matrix-1/f-threshold*))
;;;  (when (eql (array-dimension 3d-to-2d-matrix 0) 3)
;;;    (let ((m 3d-to-2d-matrix))
;;;      (declare (type (simple-array double-float (* *)) m))
;;;      (setq 3d-to-2d-matrix
;;;            (make-and-fill-4x4-matrix
;;;             (aref m 0 0) (aref m 0 1) (aref m 0 2) (aref m 0 3)
;;;             (aref m 1 0) (aref m 1 1) (aref m 1 2) (aref m 1 3)
;;;             0.0 0.0 0.0 0.0            ; this row doesn't matter
;;;             (aref m 2 0) (aref m 2 1) (aref m 2 2) (aref m 2 3)
;;;             ))))
;;;  
;;;  (let* ((m 3d-to-2d-matrix)
;;;         (xu (aref m 0 0))
;;;         (yu (aref m 0 1))
;;;         (zu (aref m 0 2))
;;;         (xv (aref m 1 0))
;;;         (yv (aref m 1 1))
;;;         (zv (aref m 1 2))
;;;         (xs (aref m 3 0))
;;;         (ys (aref m 3 1))
;;;         (zs (aref m 3 2))
;;;         ;; This solution is based on the dot-products between rows of the upper 3x3 submatrix of m.
;;;         ;; We know that the camera-to-world-matrix has an orthonormal upper 3x3
;;;         (uu (inline-inner-prod (xu yu zu) (xu yu zu)))
;;;         (uv (inline-inner-prod (xu yu zu) (xv yv zv)))
;;;         (vv (inline-inner-prod (xv yv zv) (xv yv zv)))
;;;         (uw (inline-inner-prod (xu yu zu) (xs ys zs)))
;;;         (vw (inline-inner-prod (xv yv zv) (xs ys zs)))
;;;         (ww (inline-inner-prod (xs ys zs) (xs ys zs)))
;;;         (orthographic-p (= ww 0.0))
;;;         
;;;         (up 0.0)
;;;         (vp 0.0)
;;;         (r/f 0.0)
;;;         (ss/ff ww)                     ; only for non-orthographic-p
;;;         )
;;;    (declare (type (simple-array double-float (* *)) m))
;;;                  
;;;    
;;;    (unless orthographic-p
;;;      ;; This is the only place where division by ww is needed.
;;;       
;;;      (setq up (/ uw ww )
;;;            vp (/ vw ww )))
;;;    
;;;    ;; The remaining solution for interior orientation parameters is correct for
;;;    ;; both orthographic and non-orthographic cases.
;;;      
;;;    (let* ((kuu (- uu (* up up ww)))
;;;           (kvv (- vv (* vp vp ww)))
;;;           (kuv (- uv (* up vp ww)))
;;;           (kuv^2 (^2 kuv))
;;;           (m11 (/ kvv (sqrt (- (* kuu kvv) kuv^2))))
;;;           (m01/m11 (/ kuv kvv))
;;;           (m01 (* m11 m01/m11))
;;;           (ss (- kuu (/ kuv^2 kvv)))
;;;           (1/f (- (sqrt (/ ss/ff ss))))
;;;           (s (sqrt ss))
;;;           (v-axis-scale (inline-euclidean-length m01 m11))
;;;           ;;(v-axis-scale (* (/ m11 kvv) (inline-euclidean-length kuv kvv)))
;;;           (v-axis-skew (degrees (atan m01 m11)))
;;;           m22 m23
;;;           (return-vals `(,@(when (> (abs (- v-axis-scale 1.0)) 1e-6)
;;;                                  `(:v-axis-aspect-ratio ,v-axis-scale))
;;;                          ,@(when (> (abs v-axis-skew) 1e-6)
;;;                                  `(:v-axis-skew ,v-axis-skew)))))
;;;      
;;;      (setq *bar* (list 1/f 1/f-threshold))
;;;      (if (< (abs 1/f) 1/f-threshold)
;;;          ;; focal length is very large -- consider this to be orthographic
;;;          ;; otherwise, the calculation of principal-point is likely to be quite
;;;          ;; noisy.  You can set the value of
;;;          ;; *decompose-projection-matrix-1/f-threshold* to 0.0 if you want to
;;;          ;; accept only truly orthographic cases.  I have found that
;;;          ;; matrix-inversion sometimes puts very small non-zero elements into
;;;          ;; xs, ys, zs, therefore a threshold was needed.
;;;          (setq m22 1.0 m23 0.0 orthographic-p t)
;;;          ;; perspective case
;;;          (setq m22 0.0 m23 (/ 1/f)) 
;;;          )
;;;      (let* ((2d-to-camera-matrix
;;;             ;; This is wrong for perspective case, but leads to correct calculation
;;;             ;; of row 2 of 3d-to-camera-matrix.
;;;             ;; 2d-to-camera-matrix is corrected later.
;;;             (make-and-fill-4x4-matrix 
;;;              1.0 (- (/ m01 m11)) 0.0 (- (* (/ m01 m11) vp) up )
;;;              0.0 (/ m11)         0.0 (- (/ vp m11))
;;;              0.0 0.0             m22 m23
;;;              0.0 0.0             0.0 1.0))
;;;      
;;;            (3d-to-camera-matrix
;;;             (math::multiply-matrices 2d-to-camera-matrix 3d-to-2d-matrix)))
;;;        (declare (type (simple-array double-float (* *)) 2d-to-camera-matrix 3d-to-camera-matrix))
;;;        (unless (= s 1.0) (scale-matrix 3d-to-camera-matrix (/ s)))
;;;
;;;        ;; fill last row of 3d-to-camera-matrix with <0 0 0 1>
;;;        (setf (aref 3d-to-camera-matrix 3 0) 0.0
;;;              (aref 3d-to-camera-matrix 3 1) 0.0
;;;              (aref 3d-to-camera-matrix 3 2) 0.0
;;;              (aref 3d-to-camera-matrix 3 3) 1.0)
;;;      
;;;        (cond ((not orthographic-p)
;;;               ;; fix up 2d-to-camera-matrix
;;;               (setf (aref 2d-to-camera-matrix 2 2) 1.0
;;;                     (aref 2d-to-camera-matrix 2 3) 0.0)
;;;               
;;;               (values 3d-to-camera-matrix
;;;                       `(:1/f ,1/f :r/f ,r/f
;;;                         :principal-point-u ,up
;;;                         :principal-point-v ,vp
;;;                         . ,return-vals)
;;;                       (math::invert-matrix 2d-to-camera-matrix)))
;;;          
;;;              ;; ORTHOGRAPHIC case
;;;              (t (setq r/f (- (abs (/ (aref m 3 3) s))))
;;;                 (let ((ortho-origin-range -1.0e6);; ad hoc number here 
;;;                       ;; Cannot recover this element.
;;;                       ;; Only affects near-clip-plane test, not projection equations.
;;;                       xs ys zs)
;;;                   (inline-cross-prod (xu yu zu) (xv yv zv) (xs ys zs))
;;;                   (normalize-vector-elements xs ys zs)
;;;                   (setf (aref 3d-to-camera-matrix 2 0) xs
;;;                         (aref 3d-to-camera-matrix 2 1) ys
;;;                         (aref 3d-to-camera-matrix 2 2) zs
;;;                         (aref 3d-to-camera-matrix 2 3) ortho-origin-range
;;;                         ))
;;;                 (values 3d-to-camera-matrix
;;;                         ;;`(:1/f ,1/f :r/f ,r/f . ,return-vals)
;;;                         `(:1/f 0.0 :r/f ,r/f . ,return-vals)
;;;                         (math::invert-matrix 2d-to-camera-matrix))
;;;                 ))))))


(defun decompose-projection-matrix (projection-matrix
				    &key (1/f-threshold *decompose-projection-matrix-1/f-threshold*))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type 4x4-matrix projection-matrix))
  (declare (double-float 1/f-threshold))
  (let* ((p (copy-matrix projection-matrix))
	 (1/f (euclidean-length (aref p 3 0) (aref p 3 1) (aref p 3 2)))
	 (orthographic-p (< (abs 1/f) 1/f-threshold)))
    (declare (type 4x4-matrix p))
    ;; Undo the OpenGl swap of the last 2 rows, putting the zp/f calculation in row 2.

    (cond (orthographic-p
	   ;(loop for i from 0 to 3 do (setf (aref p 2 i) 0.0))
	   ;(setf (aref p 2 2) 1.0)
	   )
	  (t
	   (swap-matrix-rows p 2 3)
	   ;; place unit vector into row 3
	   (setf (aref p 3 0) 0.0 (aref p 3 1) 0.0 (aref p 3 2) 0.0 (aref p 3 3) 1.0)))

    (multiple-value-bind (r q) (rq-decompose p 2)
      (declare (type 4x4-matrix r q))
      (let ((scale (/ (aref r 0 0))))
	;; normalize the r matrix so that (aref r 0 0) = 1.
	(math::scale-matrix r scale r 3 4)
	;; Move the translation component into q.
	;; The matrix inversion could be eliminated using backsubstitution.
	(loop with xp = (matrix-times-vector (invert-matrix r) 
			       (if orthographic-p 
				   (cv (aref r 0 3) (aref r 1 3) 0.0 0.0)
				   (cv (aref r 0 3) (aref r 1 3) (aref r 2 3) 0.0)))
	      for i from 0 to 2 do (setf (aref q i 3) (aref xp i)))
	;; and remove the translation component from r
	(loop for i fixnum from 0 to 2 do (setf (aref r i 3) 0.0))
	(when t	;; suppress round-off errors
	  (setf (aref r 3 2) 0.0)
	  (setf (aref r 1 0) 0.0 
		(aref r 2 0) 0.0 (aref r 2 1) 0.0
		(aref r 3 0) 0.0 (aref r 3 1) 0.0))
	;; Make sure R is not singular
	#+never
	(if (< (abs (aref r 2 2)) 1/f-threshold)
	    (setf (aref r 3 2) -1.0)	; orthographic case
	    (setf (aref r 3 3) -1.0) 
	    )
	;; Convert r back into OpenGL row order, swapping the last 2 rows.
	(unless orthographic-p
	  (swap-matrix-rows r 2 3))
	;; recompute the opengl depth calculation parameters
	(let* ((p projection-matrix) ; original projection matrix
	       (r-row2
		(vector-times-matrix (cv (aref p 2 0) (aref p 2 1) (aref p 2 2) (aref p 2 3))
				     (invert-matrix q))))
	  (loop for i from 2 to 3 do (setf (aref r 2 i) (* scale (aref r-row2 i)))))
	
	(values q			; 3d-to-camera-matrix
		r			; camera-to-2d-matrix
		)))))


;;; new Sun Apr 10 2005
;;; This has a singularity with (invert-matrix q) when 1/f=0
;;;(defun decompose-projection-matrix (projection-matrix
;;;                                    &key (1/f-threshold *decompose-projection-matrix-1/f-threshold*))
;;;  (declare (optimize (speed 3)(safety 1)))
;;;  (declare (type 4x4-matrix projection-matrix))
;;;  (declare (double-float 1/f-threshold))
;;;  (let* ((p (copy-matrix projection-matrix))
;;;         (1/f (inline-euclidean-length (aref p 3 0) (aref p 3 1) (aref p 3 2)))
;;;         (orthographic-p (< (abs 1/f) 1/f-threshold)))
;;;    (declare (type 4x4-matrix p))
;;;    ;; Undo the OpenGl swap of the last 2 rows, putting the zp/f calculation in row 2.
;;;    (swap-matrix-rows p 2 3)
;;;    ;; place unit vector into row 3
;;;    (setf (aref p 3 0) 0.0 (aref p 3 1) 0.0 (aref p 3 2) 0.0 (aref p 3 3) 1.0)
;;;
;;;    (multiple-value-bind (r q) (rq-decompose p 2)
;;;      (declare (type 4x4-matrix r q))
;;;      (let ((scale (/ (aref r 0 0))))
;;;        ;; normalize the r matrix so that (aref r 0 0) = 1.
;;;        (math::scale-matrix r scale r 3 4)
;;;
;;;        (bind-vector-elements (xp yp zp) 
;;;            (matrix-times-vector 
;;;             (invert-matrix r)  ; lose -- this will be singular when 1/f=0
;;;             (cv (aref r 0 3) (aref r 1 3) (if orthographic-p 0.0 (aref r 2 3)) 0.0))
;;;          ;; Move the translation component into q.
;;;          (setf (aref q 0 3) xp (aref q 1 3) yp (aref q 2 3) zp)
;;;          ;; and remove the translation component from r
;;;          (setf (aref r 0 3) 0.0 (aref r 1 3) 0.0 )
;;;          (unless orthographic-p (setf (aref r 2 3) 0.0)))
;;;
;;;        (swap-matrix-rows r 2 3)
;;;        ;; now everything is in the correct place.
;;;
;;;        ;; recompute the opengl depth calculation parameters
;;;        (let* ((p projection-matrix)    ; original projection matrix
;;;               (r-row2
;;;                (vector-times-matrix (cv (aref p 2 0) (aref p 2 1) (aref p 2 2) (aref p 2 3))
;;;                                     (invert-matrix q))))
;;;          (loop for i fixnum from 2 to 3 
;;;                do (setf (aref r 2 i) (* scale (aref r-row2 i)))))
;;;        
;;;        (values q                       ; 3d-to-camera-matrix
;;;                r                       ; camera-to-2d-matrix
;;; 
;;;               )))))

(defvar *decompose-projection-matrix-cache* (make-hash-table))
;(setq *decompose-projection-matrix-cache* nil)

;;; This sucks because the projection-matrix is often recreated freshly.
(defun decompose-projection-matrix (projection-matrix)
  (let ((ht *decompose-projection-matrix-cache*))
    (if ht
	(let ((hit (gethash projection-matrix ht)))
	  (if hit 
	      (values-list hit)
	      (mv-bind (q r) (decompose-projection-matrix-internal projection-matrix)
		(setf (gethash projection-matrix ht) (list q r))
		(values q r))))
	(decompose-projection-matrix-internal projection-matrix))))
		
      
#|  RQ Note: 

    P = R * Q

         |  R3x3  V |   | Q3x3  0 |
       = |          | * |         |
         |   0    1 |   |  0    1 |

 
         |  R3x3  0 |   | Q3x3  inv(R3x3)*V |
       = |          | * |                   |
         |   0    1 |   |  0         1      |


|#
(defvar *reset-row-3* t)

;;; new Sun Apr 10 2005
;;; I think this finally works correctly.
(defun decompose-projection-matrix-internal (projection-matrix
					     &key (1/f-threshold *decompose-projection-matrix-1/f-threshold*)
					     (reset-row-3 *reset-row-3*))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type 4x4-matrix projection-matrix))
  (declare (double-float 1/f-threshold))
  (when *decompose-projection-matrix-cache*
    (or (gethash projection-matrix *decompose-projection-matrix-cache*)))
      
  (if (typep projection-matrix 'transforms::frame-camera)
      (values (transforms::3d-to-camera-matrix projection-matrix)
	      (transforms::camera-to-2d-matrix projection-matrix))
      
      (let* ((p (copy-matrix projection-matrix))
	     (-1/f (inline-euclidean-length (aref p 3 0) (aref p 3 1) (aref p 3 2)))
	     (orthographic-p (< -1/f 1/f-threshold)))
	(declare (type 4x4-matrix p))

	;; This next causes the load of ft-benning-2 to fail:

	(progn
	  ;; Undo the OpenGl swap of the last 2 rows, putting the zp/f calculation in row 2.
	  (swap-matrix-rows p 2 3)
	  ;; place unit vector into row 3 - what if it's the "wrong" unit vector??
	  ;; In the ft-benning-2 site, this causes row 3 to duplicate raw 2:
	  (when reset-row-3
	    ;; (break)
	    (setf (aref p 3 0) 0.0 (aref p 3 1) 0.0 (aref p 3 2) 0.0 (aref p 3 3) 1.0))
	  )

	(multiple-value-bind (r q) (rq-decompose p 2)
	  (declare (type 4x4-matrix r q))
	  (let ((scale (/ (aref r 0 0))))
	    ;; normalize the r matrix so that (aref r 0 0) = 1.
	    (math::scale-matrix r scale r 3 4)
	
	    (let* ((a (aref r 0 1))
		   (s (aref r 1 1))
		   (1/f (aref r 2 2)) ; same as (- -1/f) calculated above
		   (g (aref r 2 3))
		   (up (if orthographic-p (/ (aref r 0 3) g) (/ (aref r 0 2) 1/f))) ; r02/r22
		   (vp (if orthographic-p (/ (aref r 1 3) g) (/ (aref r 1 2) 1/f))) ; r12/r22
		   (rinv01 (- (/ a s))) ; - r01/r11
		   ;; (rinv02 (- (* a vp) up)) ; r01*r12/r22-r02/r22   --- wrong
		   (rinv02 (- (* rinv01 vp) up)) ; r12/r22 * r01/r11 - r02/r22
		   (rinv11 (/ s))     ; 1/r11
		   ;;(rinv12 (- vp))  ; -r12/r22   wrong
		   (rinv12 (- (* rinv11 vp ))) ; -r12/(r11*r22)
		   (rinv22 (if orthographic-p 0.0 (/ 1/f))) ; 1/r22
		   (x (aref r 0 3)) 
		   (y (aref r 1 3))
		   (z (if orthographic-p 0.0 (aref r 2 3)))
		   ;; z could be chosen to be other values than these, such as center the origin
		   ;; in the OpenGL depth clipping range if we could figure out
		   ;; how to compute zp.  When 1/f->0, we have problems.
		   (xp (+ x (* rinv01 y) (* rinv02 z)))
		   (yp (+   (* rinv11 y) (* rinv12 z)))
		   (zp                   (* rinv22 z))) ; 
	      (declare (double-float a s 1/f g up vp rinv01 rinv02 rinv11 rinv12 rinv22 x y z xp yp z))
	      ;; See RQ Note above.
	      ;; The 3rd column of R contains <x,y,z> = -R*Qtranspose*Xcam
	      ;; We now have <xp, yp, zp> = Rinv*<x,y,z> = -Qtranspose*Xcam
	      ;; Move the translation component into the 3rd column of Q (which should be <0 0 0 1>)
	      (setf (aref q 0 3) xp (aref q 1 3) yp (aref q 2 3) zp)
	      ;; and remove the translation component from the 3rd column of  r
	      (setf (aref r 0 3) 0.0 (aref r 1 3) 0.0 )
	      (decf (aref r 2 3) z))
	
	    (swap-matrix-rows r 2 3)
	    ;; now everything is in the correct place.

	    ;; recompute the opengl depth calculation parameters
	    (let* ((p projection-matrix) ; original projection matrix
		   (r-row2
		    (vector-times-matrix (cv (aref p 2 0) (aref p 2 1) (aref p 2 2) (aref p 2 3))
					 (invert-matrix q))))
	      (loop for i fixnum from 2 to 3 
		    do (setf (aref r 2 i) (* scale (aref r-row2 i)))))
	
	    (values q			; 3d-to-camera-matrix
		    r			; camera-to-2d-matrix
		    ))))))


#|

Sat Apr 9 2005 
A much simpler version of DECOMPOSE-PROJECTION-MATRIX for non-orthographic
case based a better understanding of the matrix algebra.

      P = |    R*Q'     -R*Q'*Xc |
          | <0,0,alpha>   beta   |

Note: Actually, the last 2 rows are interchanged in FREEDIUS and OpenGL

where P is a 4x4 matrix and R and Q are 3x3 matrices.

RECOVERING THE INTERNAL AND EXTERNAL CAMERA PARAMETERS FROM THE PROJECTION MATRIX:

It is important to understand that an arbitrary square matrix M can be
decomposed into the product of an upper-right triangular matrix R and an
orthonormal matrix Q' using a variant of the well known QR-decomposition.

        M = R * Q'.

Thus, if we are given the only the camera projection matrix P, it is possible to
derive Xc, R, and Q' from it.  We compute R and Q' by performing
RQ-decomposition of M, the upper 3x3 submatrix of P.  

RQ-decomposition does not produce a unique result.  There are arbitrary choices
of sign for the diagonal elements of R.  For the camera-orientation matrix R, we
want R00 = 1, R11>0, and R22<0.  To change the sign of diagonal element Rii we
must negate column i of R and negate row i of Q, thus preserving the product of
the two matrices.  If R00 /= 1, we must divide R by R00.  (Since matrix R
is used in the homogeneous vector calculation <us,vs,s> = R*Q'*(X - Xc),
dividing all elements by a non-zero scalar preserves the result of the
homogeneous vector calculation).

Xc is trivially computed by:

         Xc = - inverse(M) * P-3rd-column


This version can be used when (abs 1/f) > threshold, where 1/f = p30+p31+p32.

;; MAKE-CAMERA-TO-2D-MATRIX

(in-package :math)

;;; Returns R, Q and Xc, unlike decompose-projection-matrix.
;;; The OpenGL depth parameters are ignored. 
;;; The resulting matrices are 3x3, not 4x4.
(defun transforms::simple-decompose-projection-matrix (projection-matrix)
  (declare (type dmatrix projection-matrix))
  (let ((rq (make-array '(3 3) :element-type 'double-float))
	(col3 (make-dvector 3)))
    (declare (type dvector col3) (type dmatrix rq))
    (loop for row fixnum in '(0 1 3) ; exchange rows 2 and 3
	  for to-row fixnum from 0 
	  do (setf (aref col3 to-row) (- (aref projection-matrix row 3))) ; R*Q'*Xc
	     (loop for col fixnum from 0 below 3
		   ;; elements of R*Q
		   do (setf (aref rq to-row col) 
			    (aref projection-matrix row col))))
    (mv-bind (r q) (rq-decompose rq)
      (RQ-flip-row-col r q 2) ; r22 = 1/focal-length  must be negative
      (math::scale-matrix r (/ (aref r 0 0)) r)
      (values r (transpose-matrix q) (g* (inverse rq) col3)))))


To handle the extraction of GSD

For the non-orthographic case (where abs(1/f) > espilon)


            | 1    a    up/f   up*g |

    R  =    | 0    s    vp/f   vp*g |

            | 0    0     1/f      g |

            | 0    0      0       1 |

and

            | 1  -a/s  -up+a*vp  0  |

  Rinv =    | 0   1/s    -vp     0  |

            | 0    0      f    -g*f |

            | 0    0      0      1  |


For the (nearly) orthographic case (where abs(1/f) <= espilon)


            | 1    a    up*g   up/f |

     R   =  | 0    s    vp*g   vp/f |

            | 0    0      g     1/f |

            | 0    0      0      1  |


            | 1  -a/s  -up/g*1/f+a*vp/s     0    |

    Rinv =  | 0   1/s      -vp/s            0    |

            | 0    0        1/g         -1/g*1/f |

            | 0    0         0              1    |



(in-package :transforms)
|#


#|

(let ((p (projection-matrix (gui::3d-to-2d-projection (top-view))))
     ; (*decompose-projection-matrix-1/f-threshold* 1e-3)
      )
  (mv-bind (cam-to-2d q xc) (transforms::simple-decompose-projection-matrix p)
    (mv-bind (3d-to-cam-2 cam-to-2d-2) (transforms::decompose-projection-matrix p)
      (let ((err-mat (math::matrix-difference p (multiply-matrices cam-to-2d-2 3d-to-cam-2))))
	(list (math::matrix-rms-norm err-mat)
	      err-mat
	      (list cam-to-2d q xc)
	      (list cam-to-2d-2 (invert-matrix 3d-to-cam-2))
	      p)))))

|#


;;; ***************************  DECOMPOSE-CAMERA-TO-2D-MATRIX  ***************************

;(disassemble 'decompose-camera-to-2d-matrix)
;;; This should be renamed to camera-to-2d-matrix-keyvals
(defun decompose-camera-to-2d-matrix (mat &optional 
				      (1/f-threshold *decompose-projection-matrix-1/f-threshold*))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type 4x4-matrix mat))
  (declare (double-float 1/f-threshold))
  (unless (= (aref mat 0 0) 1.0)
    (setq mat (scale-matrix mat (/ 1.0 (aref mat 0 0)))))
  (let* ((p00 (aref mat 0 0))
	 (p01 (aref mat 0 1))
	 (p11 (aref mat 1 1))
	 ;(P22 (aref mat 2 2))
	 ;(P23 (aref mat 2 3))
	 (P32 (aref mat 3 2))
	 (P33 (aref mat 3 3))
	 (1/f P32)
	 (GSD P33)
	 (scale p00)
	 (epsilon 1e-8)
	 ;(orthographic-p (zerop 1/f))
	 (quasi-orthographic-p (< (abs 1/f) 1/f-threshold))
	 (v-axis-skew (degrees (asin p01)))
	 (v-axis-aspect-ratio (/ p11 (cos (radians v-axis-skew))))
	 (ppu (if quasi-orthographic-p 
		  (/ (aref mat 0 3) p33)
		  (/ (aref mat 0 2) p32)))
	 (ppv (if quasi-orthographic-p
		  (/ (aref mat 1 3) p33)
		  (/ (aref mat 1 2) p32))))
    (mv-bind (near far) (extract-near-far mat)
      `(,@(unless (zerop GSD) `(:gsd ,GSD))
	:principal-point (,ppu ,ppv ,@(if (zerop 1/f) nil (list (/ 1/f))))
	,@(unless (= v-axis-aspect-ratio 1.0) `(:aspect-ratio ,v-axis-aspect-ratio))
	,@(unless (zerop v-axis-skew) `(:skew ,v-axis-skew))
	,@(when (> (abs (- scale 1.0)) epsilon) `(:scale ,scale))
	:near ,near
	,@(when far `(:far ,far))
	))))
	 
	
#|
(make-camera-to-2d-matrix :principal-point '(0.0 0.0) :GSD 1.0 :near-far '(1.0 10.0 ))
(make-camera-to-2d-matrix :principal-point '(0.0 0.0 1.0) :near-far '(1.0 10.0))
(setq p (transforms::camera-to-2d-matrix  (3d-to-2d-projection (top-view))))
(setq p (make-camera-to-2d-matrix :principal-point '(321.32 .01 (/ -1.234)) :near-far '(1000.0 2000.0)
				  :GSD 3.4 :aspect-ratio .97 :skew 2.0))

(setq p (make-camera-to-2d-matrix :principal-point '(321.32 .01) :near-far '(1000.0 2000.0)
				  :GSD 3.4 :aspect-ratio .97 :skew 2.0))
(decompose-camera-to-2d-matrix p)
|#



(in-package :math)

;;; Moved here from math/quaternions.lisp
;;; INTERPOLATE-PROJECTION-MATRICES assumes that the camera-to-2d-matrices are
;;; the same for both projections.
(defun interpolate-projection-matrices (proj0 proj1 alpha &optional 
					       (interpolator 'log-quaternion-lerp))
  ;(unless (<= 0.0 alpha 1.0) (break))
  (cond ((= alpha 0.0) proj0)
	((= alpha 1.0) proj1)
	(t (mv-bind (3d-to-camera-mat0 camera-to-2d-mat0)
	       (transforms::decompose-projection-matrix proj0)  
	     (mv-bind (3d-to-camera-mat1 camera-to-2d-mat1)
		 (transforms::decompose-projection-matrix proj1)
	       (let ((3d-to-camera-matrix 
		      (interpolate-transform-matrices 3d-to-camera-mat0 3d-to-camera-mat1 alpha interpolator))
		     (camera-to-2d-mat 
		      (interpolate-camera-to-2d-matrices camera-to-2d-mat0 camera-to-2d-mat1 alpha)))
		 (multiply-matrices camera-to-2d-mat 3d-to-camera-matrix)))))))

(in-package :transforms)




#|

(setq *projection* (3d-to-2d-projection (top-view)))

(transforms::camera-to-2d-matrix (3d-to-2d-projection (top-view)))

(extract-near-far (3d-to-2d-projection (top-view))) ; 10000 nil

(defun test-projection (projection)
  (setq *projection* projection)
  (list (list (transform-vector *projection* (cv 0.0 0.0 -5000.0)) 
	      (transform-vector *projection* (cv 0.0 0.0 0.0))
	      (transform-vector *projection* (cv 0.0 0.0 5000.0)))
	(mv-list (decompose-projection-matrix (projection-matrix projection)))
	(mv-list (extract-near-far *projection*))))

(test-projection (make-perspective-transform 
		  :camera-to-3d-matrix (invert-matrix (3d-to-camera-matrix *projection*))
		  :PRINCIPAL-POINT-U 1665.679116604388
		  :PRINCIPAL-POINT-V 1986.6625966352024
		  :GSD -1.0
		  :near 10000.0 :far 20000.0
		  ))

(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
	       :focal-length -5067.3154
	       :principal-point-u 1665.6791
	       :principal-point-v 1986.6626
	       :transform-matrix
	       (cme::make-and-fill-2d-array
		'((0.9996994 0.013477996 -0.021072589 1812.722)
		  (-0.014055679 0.9992905 -0.03458094 2137.357)
		  (0.020597093 0.03486902 0.9991798 21716.98)
		  (0.0 0.0 0.0 1.0)))
		:near 16000.0 
	 	:far 22000.0		
	       ))


(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
       :focal-length -1000.0
       :principal-point-u 0.0
       :principal-point-v 0.0
       :camera-to-3d-matrix  (cme::make-coord-frame-matrix 0.0 0.0 10000.0)
       :near 5000.0 
       :far 10000.0
       ))

(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
       :focal-length -1000.0
       :principal-point-u 0.0
       :principal-point-v 0.0
       :camera-to-3d-matrix  (cme::make-coord-frame-matrix 0.0 0.0 10000.0)
       ))


;; new gsd is zero -- origin displaced 100
(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
       :1/f -1e-2
       :GSD 1.0
       :principal-point-u 0.0
       :principal-point-v 0.0
       :camera-to-3d-matrix  (cme::make-coord-frame-matrix 0.0 0.0 0.0)
       :near -5000.0 
       :far 5000.0
       ))

;; new gsd is zero -- origin displaced 2000
(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
       :1/f -1e-3
       :GSD 2.0
       :principal-point-u 0.0
       :principal-point-v 0.0
       :camera-to-3d-matrix  (cme::make-coord-frame-matrix 0.0 0.0 10000.0)
       :near 5000.0 
       :far 10000.0
       ))

(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
       :GSD 2.0
       :principal-point-u 10.0
       :principal-point-v 20.0
       :camera-to-3d-matrix  (cme::make-coord-frame-matrix 0.0 0.0 10000.0)
       :near 5000.0 
       :far 10000.0
       ))

(test-projection
      (MAKE-PERSPECTIVE-TRANSFORM
       :GSD 2.0
       :principal-point-u 10.0
       :principal-point-v 20.0
       :camera-to-3d-matrix  (cme::make-coord-frame-matrix 0.0 0.0 0.0)
       :near -5000.0 
       :far 0.0
       ))
(decompose-projection-matrix (projection-matrix *projection*))


(projection-matrix *projection*)
(extract-near-far *projection*)
(untrace)
(trace make-camera-to-2d-matrix )
(trace rq-decompose)
(trace decompose-camera-to-2d-matrix)
(trace vector-times-matrix)
(untrace cme::make-gl-camera-matrices)
(trace set-camera-to-2d-matrix-depth-normalization)

(list (transform-vector *projection* (cv 0.0 0.0 -5000.0)) 
      (transform-vector *projection* (cv 0.0 0.0 0.0))
      (transform-vector *projection* (cv 0.0 0.0 5000.0)))

(transform-vector *projection* (cv 0.0 0.0 0.0)) 
#(1142.4541190792136 1301.8696705816271 1.0367593830070991)

(transform-vector *projection* (cv 0.0 0.0 5000.0))
#(205.70150168565664 165.65964690296843 5.926331954417257e-4)


(set-camera-to-2d-matrix-depth-normalization )
(trace set-camera-to-2d-matrix-depth-normalization)

(let ((decomp-fn 'math::decompose-azimuth-tilt-swing-rotation-matrix)
      (new-projection-matrix
       (transform-projection-matrix-to-camera-centered-lvcs (3d-to-2d-projection (top-view))
							    ;:local-units-per-meter *feet-per-meter*
							    )))
  (mv-bind (world-to-camera-matrix camera-to-2d-parameters camera-to-2d-matrix) 
      (decompose-projection-matrix new-projection-matrix)
    (setq *new-projection* (make-instance '4x4-projection :projection-matrix new-projection-matrix))
    (values new-projection-matrix
	    (multiply-matrices camera-to-2d-matrix world-to-camera-matrix)
	    world-to-camera-matrix
	    camera-to-2d-matrix
	    (mapcar #'degrees 
		    (mv-list (funcall decomp-fn (invert-matrix world-to-camera-matrix))))
	    camera-to-2d-parameters)))
(extract-near-far *new-projection*)

(projection-matrix (3d-to-2d-projection (top-view)))
(projection-matrix *new-projection*)

(defun gl-project-to-view (pt &optional (view (top-view)))
  (gl::with-gl-window ((gui::view-window view))
    (gl::glMakeCurrent (gui::view-window view))
    (gui::set-2d-to-ndc-matrix (2d-to-window-matrix view))
    (gui::set-3d-matrices view)
    (gl::glProject_to_window pt)))

(defun gl-project-to-2d (pt &optional (view (top-view)))
  (inverse-transform-vector (2d-to-window-transform view) (gl-project-to-view pt view)))

(gl-project-to-2d (gui::selected-object-world-position)) WIN
#(1017.999893081173 2550.0000674590456 0.3621155002108404)

(gl-project-to-2d (cv 0.0 0.0 17000.0 ))


(transform-vector *projection* (gui::selected-object-world-position))

(transform-vector (3d-to-2d-projection (top-view)) (gui::selected-object-world-position))
#(1017.9998030690964 2550.00002642399 0.3621155683648941)

(let ((*transform-dammit* t))
  (transform-vector *new-projection*
		    (inverse-transform-vector *lvcs-to-lvcs-transform* 
					      (gui::selected-object-world-position))))
#(1017.9998030690961 2550.0000264239898 -0.32323281611990523)


(decompose-projection-matrix (projection-matrix (3d-to-2d-projection (top-view))))
#2A((0.9996890569143531 -0.014055533577194313 0.020596879898444866
     -2229.418682364879)
    (0.013331744653748345 0.9993030575429711 0.03486637018326328
     -2917.22639593563)
    (-0.021072590495186887 -0.034580936383063206 0.9991797159513858
     -21587.055352889416)
    (0.0 0.0 0.0 1.0))

(:|1/F| -1.9734313674786607e-4 :GSD 0.0 :PRINCIPAL-POINT-U 1665.6791166043904
 :PRINCIPAL-POINT-V 1986.6625966352044 :V-AXIS-ASPECT-RATIO 1.0000105174483886
 :V-AXIS-SKEW -0.008390220651967912)

#2A((0.9999896538042876 -1.4643697482230475e-4 -0.32871034168612495 0.0)
    (0.0 1.000010506726382 -0.3920542284796518 0.0)
    (0.0 0.0 0.0 -1.0)
    (0.0 0.0 -1.9734313674786607e-4 0.0))

(vector-times-scalar (cv 7395.897408598901 9677.638082367821 71613.12857321149)
		     (/ -2229.418682364879 7395.897408598901))
#(-2229.418682364879 -2917.2263959356305 -21587.05535288941)

(list (/ -2229.418682364879 7395.897408598901)
      (/ 4.2600572164890105 -14.11022117886782))
|#
