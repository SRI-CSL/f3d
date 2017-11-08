(in-package :f3d)

;;;
;;; Could be much better.  Need make/unmake.
;;;

(defvar *img-element-types*
  '((unsigned-byte 8)
    (unsigned-byte 16)
    (unsigned-byte 32)
    (signed-byte 16)
    (signed-byte 32)))


(defun test-integer-image (image)
  (destructuring-bind (xdim ydim)
      (img::image-dimensions image)

    (loop for i from 0 below xdim
          do (loop for j from 0 below ydim
                   for value = (+ i j)
                   do (setf (img::iref image i j) value)))

    (loop for i from 0 below xdim
          always (loop for j from 0 below ydim
                       for v1 = (+ i j)
                       for v2 = (img::iref image i j)
                       always (= v1 v2)))
    ))

(defun array-image-tests (&key (xdim 64) (ydim 64))
  ;; Need to add
  (if (loop for type in *img-element-types*
            always
            (test-integer-image
             (img::make-image (list xdim ydim)
                 :element-type type)))
      (format t "~%Integer array-image tests passed.~%")
      (format t "~%Integer array-image tests FAILED!~%")))
       
