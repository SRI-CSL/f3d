(in-package :cl-user)
#|

PROCEDURE FOR USING GDB WITH FREEDIUS

1. Start up FREEDIUS to the point that all shared libraries are loaded.

   Get the process id (and pathname of the executable) of the Lisp corresponding
   to the FREEDIUS started in step 1.
   (UNIX:UNIX-GETPID )
   (sb-UNIX:UNIX-GETPID )


2. start up a shell window and ps -Af --cols 100
   ps -p <pid-from-step-1> -o cmd=
  /opt/cmucl/cmucl-2006-02-x86-linux/bin/lisp
  /opt/cmucl/cmucl-2006-09-x86-linux/bin/lisp
3. In Emacs do:

   M-X gdb <return>
  --fullname  <lisp_executable-path> <process-id> <return>
  --fullname  /opt/sbcl/sbcl-1.0.29-x86_64-linux/src/runtime/sbcl <process-id> <return>

   This will attach gdb to the Lisp process


4. For CMUCL, execute the following in the gdb window:

handle SIGSEGV nostop
handle SIGSEGV noprint

handle SIGTRAP nostop
handle SIGTRAP noprint
handle SIGTRAP pass


handle SIGSEGV nostop
handle SIGSEGV noprint
break FREEDIUS_brk
handle SIGTRAP pass


5. In the gdb window install any breakpoints you might need, and do

   continue


FREEDIUS has now regained control.
Do your thing with FREEDIUS.

SIGINT <control-C> will be caught by GDB.  You can
use this to set new breakpoints.  Alternatively, you
can turn of GDB handling of SIGINT by doing:

   handle SIGINT nostop
   handle SIGINT noprint


See the file $FREEDIUS/lisp/cme-compat/cme-test.lisp for
random other GDB debugging fragments.

END OF PROCEDURE FOR USING GDB WITH FREEDIUS
______________________________________________________________________________________

Random stuff 

ps -elf | grep lisp
M-X gdb /opt/cmucl/cmucl-2003-05-15-x86-linux/bin/lisp <procid>
M-X gdb /opt/bin/cmucl-lisp.20020218 <procid>
M-X gdb /opt/bin/cmucl-lisp-20020404-x86-linuxglibc22
M-X gdb /opt/cmucl/cmucl-18e-pre1-x86-linux/bin/lisp
M-X gdb /opt/acl/acl60/alisp 
directory /usr/src/redhat/SOURCES/glibc-2.2.4/malloc
handle SIGSEGV nostop
handle SIGSEGV noprint
handle SIGINT nostop
handle SIGINT noprint
break error
break interrupt_handle_now
break lisp_error_handler

break display_tiled_image
break put_prop
handle SIGSEGV stop
p *((RGB8_PIXEL *) (buffer +1))

;; turn off CMUCL triggered seg-faults.
(setf (alien:extern-alien "enable_page_protection" alien:unsigned) 0)
set a breakpoint in ldb_monitor().
break ldb_monitor  
cont

handle SIGSEGV stop
handle SIGSEGV print
cont

in Emacs, before starting FREEDIUS do
(setenv "MALLOC_CHECK_" "2")
(setenv "MALLOC_CHECK_" "1")
(setenv "MALLOC_CHECK_" nil)

M-X gdb /opt/acl/acl60/alisp <procid>


(setq gui::*inhibit-object-display* t)

(load (format nil "~a/lisp/boot.lisp"
	      (#+allegro sys::getenv  "GEOFEX")))

;;; delete all compiled files
;;; cd ~/cp/lisp; find . -name '*.mbin' | xargs rm
;;; cd ~/cp/lisp/gl-ffi; find . -name '*.mbin' | xargs rm
;;; cd ~/cp/lisp/basic-gui; find . -name '*.mbin' | xargs rm
;;; cd ~/cp/lisp/globj; find . -name '*.mbin' | xargs rm
;;; cd ~/cp/lisp/cme-compat; find . -name '*.mbin' | xargs rm


(load (format nil "~a/lisp/boot.lisp"
	      (#+allegro sys::getenv "GEOFEX")))

(st::load-system 'cme)
;;(setq *tk-verbose* t)
;; disksave should be here

(init-lisptk '((in-package :gui)
		   ;;(tk-wm "withdraw" ".")
		   ))
(cme::select-cme-site)

(setq win (cdr (nth 3 (gui::frame-PANE-ALIST (gui::pane-frame (gui::selected-window gui::*interactor*))))))
(gui::view-image (gui::top-view gui::win))


(img::resize-image-page-pool (gui::view-image (gui::top-view t)) -1000)
(img::resize-image-page-pool (gui::view-image (gui::top-view t)) 1000)
(img::get_page_pool  1000)

(gui::release-image-pool-textures gui::*null-image* 1)

(setq san-diego-rgb8 (img::load-image "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_rgb_msi_tiff/po_39949_rgb_0010000000.tiled512.tif"))

(lx::environment-variable "CME_TMP_FILE_IMAGE_DIR") "/tmp/CME_TMP_PIX"

(setq gui::*max-drag-pixels* (* 1600 1200))
(setq gui::*max-drag-pixels* (* 2 1600 1200))
(img::resize-image-page-pool (img::band-interleaved-image san-diego-rgb8) 100)
(img::resize-image-page-pool (img::band-interleaved-image san-diego-rgb8) 200)
(gui::push-image san-diego-rgb8 (gui::selected-window gui::*interactor*))
(img::all_image_page_pool_stats 0)

(gui::push-image (img::red-image san-diego-rgb8) (gui::selected-window gui::*interactor*))
(img::tile-offset-bits cl-user::san-diego-rgb8)
(img::tile-offset-bits (img::red-image cl-user::san-diego))


(gui::pop-view (gui::selected-window gui::*interactor*))

(lx:maybe-compile-file-load '("$GEOFEX/lisp/basic-gui/test-gui.lisp"))
(gui::make-image-browser)

(gui::top-view t)
(gui::view-image (gui::selected-view))

(st::initialize-all-systems)
(img::load-image "$RADIUS/site-2d-worlds/alv/alv-oblique-tower/image.g0")

(setq *tk-verbose* t)'
(setq *tk-verbose* nil)

(setq img (gui::view-image (gui::top-view t)))
(img::resize-image-page-pool img 200)


;;; get current size of texture map page pool:
(img::resize-texid-page-pool 0)

(img::resize-texid-page-pool 200)

(cme::selected-objects cme::*interactor*)

(st::load-system 'inspect)
(init-lisptk
 '(;; initialize-allegro-bindings needed to init *read-default-float-format* ...
   ;;(lcl::initialize-allegro-bindings) 
   (tk-wm "withdraw" ".")
   ))
(inspect::make-inspector)

(excl::build-lisp-image "~/cp/lisp/allegro/disksave-test.dxl"
			:discard-source-file-info nil
			;;:include-all t
			:include-compiler t
			:include-devel-env t
			:include-tpl t
			
			:lisp-files '("~/cp/lisp/allegro/disksave-forms.lisp")
			;;:user-shared-libraries t
			:restart-init-function
			'cl-user::cme-initialize-allegro-bindings
			)

M-X random allegro
$GEOFEX/arch/irix-acl/bin/geofex


(ff::list-all-foreign-libraries)


(ff::list-all-foreign-libraries :return-structs t)
/usr/local/cme/lisp/allegro/irix-5.0b/libacl50b5.so

To print the dynamic libraries referenced by a dynamic library:

elfdump -Dl $GEOFEX/arch/irix-acl/lib/liblisptk.so
[1]	Apr 22 09:43:13 1998	0x9ab13c87	-----	libtcl8.1.so	0
[2]	Apr 22 09:48:03 1998	0x2fa365b5	-----	libtk8.1.so	0
[3]	Feb 24 13:26:57 1998	0x93e9163e	-----	libX11.so.1	sgi1.0
[4]	Apr 28 16:00:33 1998	0x74889846	-----	libacl50b5.so	0

elfdump -Dl $GEOFEX/arch/irix-acl/lib/libCME.so
[1]	Apr 23 10:10:54 1998	0xa1b52bfc	-----	libtiff2.so	0
[2]	Feb 24 16:48:45 1998	0x60d1d5cd	-----	libGLU.so	sgi1.0
[3]	Feb 24 16:48:45 1998	0x70e69c8c	-----	libGL.so	sgi1.0
[4]	Feb 24 13:26:57 1998	0xa162154f	-----	libC.so.2	sgi2.0

elfdump -Dl $CME/lisp/allegro/irix-5.0b/libacl50b5.so
[1]	Dec 17 15:13:02 1997	0x38fe9819	-----	libm.so	sgi1.0




$CME/lisp/allegro/irix-5.0b/lisp -I $GEOFEX/lisp/allegro/cme-test.dxl
|#


#|
CONVERSION PROBLEMS



|#



#| loaded pathnames

(loop for bin-path in (append (lx::get-file-property-list-hash-table-pathname-list)
			      (lx::get-loaded-pathnames))
      for source-path = (make-pathname :defaults bin-path :type "lisp")
      ;;collect source-path
      do (format t "~a~%" (namestring source-path)))

(loop for bin-path in (append (lx::get-file-property-list-hash-table-pathname-list)
			      (lx::get-loaded-pathnames))
      for source-path = (make-pathname :defaults bin-path :type "lisp")
      for namestring = (namestring source-path)
      ;;collect source-path
      with prefix = "$GEOFEX/lisp/"
      when (string-equal namestring prefix :end1 (length prefix))
	do (format t "~a~%" (subseq namestring (length prefix)))
      else do (format t "~a~%" namestring))

(progn st::*all-systems*)

(setq *cme-test-system-names*
      '("EV-PATHNAMES" "LISP-EXTENSIONS" "FFI-EXTENSION" 
	"TK" "CVV" 
	"MATH" "TRANSFORMS" "GEOGRAPHIC-TRANSFORMS"
	"GL"
	"IMAGE"
	"BASIC-GUI"
	"CME" 
	"RADIUS-CLASSES"
	))

(defparameter *qcme-bootstrap-paths*
  '("allegro/lcl-compat.lisp"
    "lisp/bootstrap.lisp"
    "lisp/system-tool-bootstrap.lisp"
    "lisp/system-tool.lisp"
    ))

(defparameter *cme-test-files*
  (append *qcme-bootstrap-paths*
	  (loop for sysname in *cme-test-system-names*
		for system = (st::find-system-named sysname)
		append (loop for file in (st::system-files system)
			     for namestring = (namestring (st::merge-system-source-pathnames file system))
			     with prefix = "$GEOFEX/lisp/"
			     collect (if (string-equal namestring prefix :end1 (length prefix))
					 (subseq namestring (length prefix))
					 namestring)))))

(loop for namestring in *cme-test-files*
      do (format t "~a~%" namestring))

\rm TAGS;cat cme-compat/cme-test-files | xargs etags




|#


#|

(setq m
      (find-method #'stream::stream-write-char nil
		   (list (find-class 'EXCL:OUTPUT-TERMINAL-STREAM)
			 (find-class t))))

(method-function m)
(disassemble (method-function m))

(defclass hacked-BIDIRECTIONAL-TERMINAL-STREAM)

(excl::explain-compiler-settings)
|#




#|
(defun bless-pathname (pathname default-type error-name name)
  (when (stringp pathname) (setq pathname (pathname pathname)))

  (unless pathname (error ";;; ~a not defined for system ~a" error-name name))
  (when (eq (car (pathname-directory (lp::ev-pathname-translate pathname)))
	    :relative)
    (error ";;; ~a ~a is a relative pathname" error-name  pathname))

  (unless (pathname-type pathname)
    (setq pathname (make-pathname :name "*" :type default-type :defaults pathname)))
	     
  (lp::ev-pathname-backtranslate pathname))

(bless-pathname "/homedir/quam/cp/lisp/lisp" "lisp" 'foo "bar")

st::*all-systems*


(namestring (st::system-default-pathname (car st::*all-systems*)))

(loop for sys in st::*all-systems*
      collect (st::system-name sys))

(loop for sys in st::*all-systems*
      do (describe sys))

(loop for sysname in
      '("EV-PATHNAMES" "LISP-EXTENSIONS" "FFI-EXTENSION" 
	"TK" "CVV" 
	"MATH" "TRANSFORMS" "GEOGRAPHIC-TRANSFORMS"
	"GL"
	"IMAGE"
	"BASIC-GUI"
	"CME" 
	"RADIUS-CLASSES"
	)
      for sys = (st::find-system-named sysname)
      do (describe sys))


System: EV-PATHNAMES
  :required-systems NIL
  :default-pathname /homedir/quam/cp/lisp/lisp/*.lisp
  :files
    "ev-pathnames.lisp"


System: LISP-EXTENSIONS
  :required-systems NIL
  :default-pathname $GEOFEX/lisp/lisp/*.lisp
  :files
    "lisp-extensions.lisp"
    "lisp-io.lisp"
    "universal-time.lisp"
    "acl-foreign-vector.lisp"
    "struct-class.lisp"


System: FFI-EXTENSION
  :required-systems (LISP-EXTENSIONS)
  :default-pathname $GEOFEX/lisp/lisp/*.lisp
  :files
    "ffi-extensions.lisp"


System: TK
  :required-systems (LISP-EXTENSIONS FFI-EXTENSION)
  :default-pathname $GEOFEX/lisp/tk/*.lisp
  :files
    "tk-pkg-def.lisp"
    "tk-ffi.lisp"
    "tk-interface.lisp"
    "tk-widget.lisp"
    "repl.lisp"
    "xungrabpointer.lisp"
    "widget-panel.lisp"


System: CVV
  :required-systems (TK)
  :default-pathname $GEOFEX/lisp/tk/*.lisp
  :files
    "cvv.lisp"


System: MATH
  :required-systems (LISP-EXTENSIONS FFI-EXTENSION)
  :default-pathname $GEOFEX/lisp/math/*.lisp
  :files
    "matrices.lisp"
    "vectors.lisp"
    "transform-matrix.lisp"


System: TRANSFORMS
  :required-systems (FFI-EXTENSION MATH)
  :default-pathname $GEOFEX/lisp/transforms/*.lisp
  :files
    "transforms-pkg.lisp"
    "transform-ffi.lisp"
    "transform-objects.lisp"
    "coordinate-transforms.lisp"
    "4x4-transform.lisp"
    "composite-transforms.lisp"


System: GEOGRAPHIC-TRANSFORMS
  :required-systems (TRANSFORMS)
  :default-pathname $GEOFEX/lisp/transforms/*.lisp
  :files
    "geographic-transforms.lisp"
    "geographic-constants.lisp"


System: GL
  :required-systems (FFI-EXTENSION TK MATH TRANSFORMS)
  :default-pathname $GEOFEX/lisp/gl-ffi/*.lisp
  :files
    "gl-pkg-def.lisp"
    "gl-ffi.lisp"
    "glext-ffi.lisp"
    "glu-ffi.lisp"
    "gl-utils.lisp"
    "tk-gl-utils.lisp"


System: IMAGE
  :required-systems (FFI-EXTENSION TRANSFORMS)
  :default-pathname $GEOFEX/lisp/img/*.lisp
  :files
    "image-ffi.lisp"
    "image-pyramids.lisp"


System: BASIC-GUI
  :required-systems (GL TRANSFORMS IMAGE)
  :default-pathname $GEOFEX/lisp/basic-gui/*.lisp
  :files
    "basic-gui-pkg-def.lisp"
    "macros.lisp"
    "gl-qcme-ffi.lisp"
    "gl-matrices.lisp"
    "gl-objects.lisp"
    "view.lisp"
    "commands.lisp"
    "display.lisp"
    "lighting.lisp"
    "pick.lisp"
    "motions.lisp"


System: CME
  :required-systems (CVV MATH BASIC-GUI GEOGRAPHIC-TRANSFORMS)
  :default-pathname $GEOFEX/lisp/cme-compat/*.lisp
  :files
    "cme-pkg-def.lisp"
    "object-classes.lisp"
    "feature-sets.lisp"
    "worlds.lisp"
    "load-cme-objects.lisp"
    "camera-models.lisp"
    "graphics-styles.lisp"
    "site-glue.lisp"
    "camera-model-io.lisp"


System: RADIUS-CLASSES
  :required-systems (CME)
  :default-pathname $GEOFEX/lisp/cme-compat/*.lisp
  :files
    "radius-classes.lisp"


|#



#|
(excl:source-file 'lcl:environment-variable)
(lcl:environment-variable ...)
(excl:source-file 'math::invert-matrix)
(math::invert-matrix )
(translate-logical-pathname (excl:source-file 'math::invert-matrix))

(setq tiff-img (img::load-image "~/tmp/pix/alv-2-44.g0-win.tif"))
(setq tiff-img (img::load-image "/homedir/quam/tmp/pix/alv-2-44.g0-win.tif"))
(gui::view-window (gui::top-view t))
(gui::push-image tiff-img (gui::view-window (gui::top-view t)))
|#

#|

(time (st::load-system 'cme))

;; recompiling bootstrap
;;; On garlic
; cpu time (non-gc) 1,240 msec user, 140 msec system
; cpu time (gc)     570 msec user, 0 msec system
; cpu time (total)  1,810 msec user, 140 msec system
; real time  4,148 msec
; space allocation:
;  804,953 cons cells, 2,691 symbols, 4,130,576 other bytes, 6624 static bytes

;; recompiling cme
(time (st::load-system 'cme))
; cpu time (non-gc) 26,000 msec user, 930 msec system
; cpu time (gc)     8,190 msec user, 20 msec system
; cpu time (total)  34,190 msec user, 950 msec system
; real time  52,668 msec
; space allocation:
;  16,180,914 cons cells, 29,211 symbols, 74,195,808 other bytes, 5192 static bytes

Totals: elapsed: 56.82 cpu:     36.0

;; recompiling bootstrap
On langley
; cpu time (non-gc) 4,120 msec user, 270 msec system
; cpu time (gc)     2,230 msec user, 80 msec system
; cpu time (total)  6,350 msec user, 350 msec system
; real time  7,553 msec
; space allocation:
;  779,785 cons cells, 2,669 symbols, 6,135,536 other bytes, 6248 static bytes

;; recompiling cme
(time (st::load-system 'cme))
; cpu time (non-gc) 82,280 msec (00:01:22.280) user, 2,940 msec system
; cpu time (gc)     37,360 msec user, 750 msec system
; cpu time (total)  119,640 msec (00:01:59.640) user, 3,690 msec system
; real time  137,570 msec (00:02:17.570)
; space allocation:
;  15,979,146 cons cells, 29,181 symbols, 130,681,672 other bytes, 5240 static bytes

Totals: elapsed: 145.15 cpu:     126.0

loading only

On garlic
;; loading bootstrap
; cpu time (non-gc) 330 msec user, 0 msec system
; cpu time (gc)     80 msec user, 20 msec system
; cpu time (total)  410 msec user, 20 msec system
; real time  543 msec
; space allocation:
;  59,659 cons cells, 1,948 symbols, 1,092,104 other bytes, 2472 static bytes

;; loading cme
(time (st::load-system 'cme))
; cpu time (non-gc) 3,850 msec user, 380 msec system
; cpu time (gc)     1,560 msec user, 10 msec system
; cpu time (total)  5,410 msec user, 390 msec system
; real time  7,263 msec
; space allocation:
;  946,995 cons cells, 12,091 symbols, 13,762,312 other bytes, 0 static bytes

Totals: elapsed: 7.8  cpu: 5.81     

On langley:
;; loading bootstrap
; cpu time (non-gc) 1,070 msec user, 140 msec system
; cpu time (gc)     320 msec user, 30 msec system
; cpu time (total)  1,390 msec user, 170 msec system
; real time  1,755 msec
; space allocation:
;  33,728 cons cells, 1,935 symbols, 1,117,808 other bytes, 2048 static bytes

;; loading cme
(time (st::load-system 'cme))
; cpu time (non-gc) 13,200 msec user, 1,140 msec system
; cpu time (gc)     6,600 msec user, 400 msec system
; cpu time (total)  19,800 msec user, 1,540 msec system
; real time  24,278 msec
; space allocation:
;  920,893 cons cells, 12,077 symbols, 17,015,480 other bytes, 0 static bytes

Totals: elapsed: 26.05 cpu:     21.19




|#
