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