(in-package :img)

#|


This has not been tested for ELEMENT-TYPE other than (unsigned-byte 8).

It is probably broken for other element-types.

|#

(defun load-noheader-image (path &key xdim ydim (element-type '(unsigned-byte 8)) (data-offset 0))
  (unless (and xdim ydim) (error "load-noheader-image must specify xdim and ydim"))

  (flet ((read-array (st arr nelems)
	   (declare (ignorable nelems))
	   #+cmu (ext::read-n-bytes st arr nelems xdim )
	   #+allegro (excl::read-vector arr st)))
    (with-open-file (st path :element-type element-type)
      (let* ((img (make-image (list xdim ydim) :element-type element-type ))
	     (buf (make-array xdim :element-type element-type)))
	(when (> data-offset 0)
	  (let ((skip-buf (make-array data-offset :element-type element-type)))
	    (read-array st skip-buf data-offset)))

	(loop for y from (1- ydim) downto 0
	      do (read-array st buf xdim)
		 (image-putline img buf 0 y))
	img))))
	       
#|
(setq lena (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/lena.img"
				:xdim 256 :ydim 256))
(gui::push-image lena (gui::selected-window *interactor*))
(gui::selected-window)

(setq lena45 (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/lena45.img"
				:xdim 256 :ydim 256))
(gui::push-image lena45 (gui::selected-window *interactor*))

(setq lena45b (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/lena45b.img"
				:xdim 256 :ydim 256))
(gui::push-image lena45b (gui::selected-window *interactor*))

(gui::push-image (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/lenaxxx.img"
				      :xdim 256 :ydim 256)
		 (gui::selected-window *interactor*))

(defun moms-rotate (in out theta xdim ydim &key (order 3) (mask 1))
  (let* ((proc (ext::run-program "/homedir/quam/papers/ieee/unser/interpol/demo" nil
				 :wait nil
				 :input :stream
				 :output :stream))
	 (ost (ext:process-output proc))
	 (st (ext:process-input proc)))
    (flet ((send (x)
	     (format t (read-line ost))
	     (format st "~a~%" x)))
      (send in)
      (send xdim) (send ydim)
      (send (floor xdim 2)) (send (floor ydim 2))
      (send theta)
      (send 0) (send 0)
      (send 0)
      (send order) (send mask)
      (send out)
      (read-line ost)
      (close st) (close ost)
      (ext::process-close proc)
      
      )))


(defun moms-rotate (in out theta xdim ydim &key (order 3) (mask 1))
  (let* ((proc (ext::run-program "/homedir/quam/papers/ieee/unser/interpol/demo" nil
				 :wait nil
				 :input :stream
				 ;;:output t
				 ))
	 ;;(ost (ext:process-output proc))
	 (st (ext:process-input proc)))
    (flet ((send (x)
	     ;;(format t (read-line ost))
	     (format st "~a~%" x)))
      (send in)
      (send xdim) (send ydim)
      (send (floor xdim 2)) (send (floor ydim 2))
      (send theta)
      (send 0) (send 0)
      (send 0)
      (send order) (send mask)
      (send out)
      (close st)
      (loop for status = (ext:process-status proc)
	    while (eq status :running)
	    finally (ext::process-close proc)
		    (format t "~a Done rotating ~a into ~a by ~a degrees~%" status in out theta)
	    ))))


(defun mons-multi-rotate (path xdim ydim dtheta nrots &key (order 3))
  (loop  with prefix = (format nil "O~a-R~d" order dtheta)
	 with xdim = 256 with ydim = 256
	 for theta from dtheta by dtheta
	 repeat nrots
	 for inpath = path then outpath
	 for outpath = (make-pathname :defaults path
			    :name (format nil "~a-~a-~a" prefix (pathname-name path) (round theta)))
	 do (moms-rotate inpath outpath dtheta xdim ydim :order order)))

(mons-multi-rotate "/homedir/quam/papers/ieee/unser/interpol/lena.img" 256 256 24 15)

(gui::push-image (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/xx-lena360.img"
				      :xdim 256 :ydim 256)
		 (gui::selected-window *interactor*))


(mons-multi-rotate "/homedir/quam/papers/ieee/unser/interpol/lena.img" 256 256 24 15 :order 2)

(gui::push-image (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/O2-R24-lena-360.img"
				      :xdim 256 :ydim 256)
		 (gui::selected-window *interactor*))

(mons-multi-rotate "/homedir/quam/papers/ieee/unser/interpol/lena.img" 256 256 24 15 :order 4)

(gui::push-image (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/O4-R24-lena-360.img"
				      :xdim 256 :ydim 256)
		 (gui::selected-window *interactor*))

(mons-multi-rotate "/homedir/quam/papers/ieee/unser/interpol/lena.img" 256 256 24 15 :order 5)

(gui::push-image (load-noheader-image "/homedir/quam/papers/ieee/unser/interpol/O5-R24-lena-360.img"
				      :xdim 256 :ydim 256)
		 (gui::selected-window *interactor*))

(moms-rotate "/homedir/quam/papers/ieee/unser/interpol/lena.img"
	     "/homedir/quam/papers/ieee/unser/interpol/lenaxxx.img"
	     24 256 256)



(def-foreign-function (kill-proc (:name "kill"))
  (pid :int)
  (sig :int))


(setq proc-ids
      (with-input-from-string (st psout)
	(loop for line = (read-line st nil nil)
	      while line
	      with x
	      for pos = 0
	      do (loop repeat 4
		       do (multiple-value-setq (x pos) (read-from-string  line nil nil :start pos)))
	      collect x)))

(loop for proc-id in proc-ids
      do (kill-proc proc-id 9))
|#
