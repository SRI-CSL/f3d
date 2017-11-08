;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)

;;;
;;; Add features that will control the UI substrate.  The user has the
;;; option of adding :cocoa to *features* to induce MacOSX-native UI
;;; windows and widgets, as opposed to X11.

;;; Add features that will control the UI substrate.  The user has the
;;; option of adding :cocoa to *features* to induce MacOSX-native UI
;;; windows and widgets, as opposed to X11.  These need to be set here
;;; primarily because the compilation of lisptk will depend on this.
;;; I'm not thrilled about that, but for now that's the way it is:

;;; (pushnew :tkgui *features*)

#-(or cocoa win32)
(pushnew :glx *features*)

#+win32
(pushnew :wgl *features*)

;;; (sb-posix::setenv "RADIUS" "/opt/IU/radius")

;;; This is FREEDIUS modified to be usable with the quicklisp / asdf
;;; framework.  The ultimate goals are (a) to modularize functionality
;;; so that the base stuff (images, geometry, transforms, objects) can
;;; be loaded easily within quicklisp, and (b) to become more
;;; compatible with the Lisp community, which is moving toward
;;; standards for FFIs, network-based system definition, retrieval,
;;; etc.
;;;
;;; The UI layer should retain most of its concepts, but we should be
;;; able to exploit other Lisp GUI efforts.  I continue to be troubled
;;; by the maze of OS dependencies induced by Tcl/Tk, not to mention
;;; the forced presence of a Tcl interpreter with its own opaque
;;; interpreter stack.

;;; The cl-cme-tk system should represent a "reference" that works
;;; just like the original FREEDIUS, possibly restricted to X windows.
;;; It has no dependencies on other systems.

;;; This system uses ASDF extensions that are provided by a companion
;;; system called :cl-cmake, that allows us to build and load shared
;;; objects.  This is still unsatisfying, since there are other
;;; dependencies lurking in those systems that will probably throw
;;; people off.  In particular, this system depends on jpeg, OpenGL,
;;; and Tcl/Tk, and requires the installation of header files in
;;; "nice" locations.


;;; Reader macros are used here to designate modules that can be split
;;; off into other systems:
;;;
;;; f3d-core (no macro)
;;; f3d-geom (OpenGL and object geometry)
;;; f3d-tk   (Tcl/Tk-specific code)
;;; f3d-gui  (generic GUI code)

(asdf:defsystem :f3d-core
  :name "f3d-core"
  :defsystem-depends-on ("cl-cmake")
  :maintainer "Chris Connolly, SRI International <connolly@ai.sri.com>"
  :license "Mozilla"
  :description "Core FREEDIUS system, no GUI."
  :components 
  ((:module :system-tool
            :serial t
            :components ((:file "boot")
                         (:file "system-tool-bootstrap")
                         (:file "system-tool")))

   (:module :lcl
            :serial t
            :depends-on (:system-tool)
            :components ((:file "lcl-pkg")
                         (:file "generic-lcl-fns")
                         #+cmu     (:file "cmucl-lcl-fns")
                         #+allegro (:file "acl-lcl-fns")
                         #+sbcl    (:file "sbcl-lcl-fns")
                         ))

   (:module :ev-pathnames
            :serial t
            :depends-on (:system-tool)
            :components ((:file "ev-pathname-pkg")
                         (:file "ev-pathnames")
                         ))

   (:module :lx
            :serial t
            :depends-on (:ev-pathnames :lcl)
            :components
            ((:file "common-symbols-pkg")
             (:file "lx-pkg")
             (:file "custom")
             (:file "file-property-list") ; this probably should move to system-tool)
             (:file "lisp-extensions")
             (:file "colors")             ; Not the best place, but see file for issues.
             (:file "pathname-extensions")	; would like to eliminate this
             (:file "struct-class")
             (:file "eval-cache")
             (:file "lisp-io")
             (:file "universal-time")
             (:file "clos-tools")
             (:file "binary-search")
             )
            )

   (:module :qffi
            :serial t
            :depends-on (:c :lx)
            :components
            (#-allegro (:shared-object "libf3d-foreign-vector")
             (:file "qffi-pkg" )
             (:file "qffi")
             (:file "lcl-ffi-frontend")

             #+cmu
             (:module :cmucl
                      :serial t
                      :components
                      ((:file "cmucl-array-address" )
                       (:file "cmucl-ffi-fns" )
                       (:file "cmucl-ffi-backend")
                      ))

             #+sbcl
             (:module :sbcl
                      :serial t
                      :components
                      ((:file "sbcl-array-address" )
                       (:file "sbcl-ffi-fns" )
                       (:file "sbcl-ffi-backend")
                       ))

             #+(and allegro (not cffi))
             (:module :aclnew
                      :serial t
                      :components
                      ((:file "acl-ffi-fns" )
                       (:file "acl-ffi-backend")
                       ))

             #+(and allegro cffi)
             (:module :cffi
                      :serial t
                      :components
                      ((:file "cffi-backend")))

             (:file "generic-ffi-fns")
             (:file "mixed-lisp-system")
             ))

   (:module :c
            :components
            ((:shared-object "libfreedius")))


   (:module :foreign-vector
	    :serial t
	    :depends-on (:qffi :c)
	    :components
	    (
	     (:file "load-libfreedius")
	     #+cmu (:file "cmucl-foreign-vector")
	     #+allegro (:file "acl-foreign-vector")
	     #+sbcl (:file "sbcl-foreign-vector")
	     ))

   (:module :math
            :serial t
            :depends-on (:lx :qffi)
            :components
            (#-cmu (:shared-object "libf3dmath")
             (:file "math-pkg")
             (:file "vectors")
             (:file "matrices")
             (:file "bounding-boxes")
	     #+cmu (:file "math-ffi-replacement")
	     #-cmu (:file "math-ffi")
             (:file "transform-matrix")
             (:file "rq-decomposition")
             (:file "geometry")
             (:file "quaternions")
             (:file "generic-arith")
             (:file "polynomial-fit")
             (:file "polynomial-fit-1d")
             ))

   (:module :transforms
            :serial t
            :depends-on (:math)
            :components
            ((:file "transforms-pkg")
	     (:file "coordinate-transforms")
             ;; (:file "weak-children-hash-table-mixin")
	     (:file "transform-path")
	     (:file "4x4-transform")
	     (:file "4x4-projection")
	     (:file "frame-camera")
	     (:file "composite-transforms")
	     (:file "4x4-motions")
	     (:file "frame-camera-motions")
	     (:file "numeric-inverse-transform")
	     (:file "4x4-projection-fit")
	     (:file "4x4-projection-decomposition")
	     (:file "transform-fit")
             (:file "temporal-transforms")
	     ))

   (:module :geo
	    :serial t
	    :depends-on (:transforms)
	    :components
	    ((:file "geographic-transforms")
	     (:file "geographic-constants")))

   (:module :rpc
	    :serial t
	    :depends-on (:geo)
	    :components
	    ((:file "rpc-macros")
	     (:file "polynomial-transforms")
	     (:file "rational-polynomial")
	     (:file "rpc-projection")
	     (:file "dppdb-rpc")
	     (:file "rpc00a")
	     (:file "rpc-zerop")))

   (:module :libfreedius
	    :serial t
	    :depends-on (:foreign-vector)
	    :components
	    ((:file "libfreedius")))
             
   (:module :img
            :serial t
            :depends-on (:math :libfreedius :transforms)
            :components
            ((:file "image-config")
	     (:file "image-pkg")
	     (:file "image-defs")
	     (:file "image-ffi")
	     (:file "array-image")
	     (:file "paged-image")
	     (:file "vector-image")
	     ;;(:file "color-image.lisp" ;")
	     (:file "band-interleaved-image")
	     (:file "image-mapping")
	     (:file "image-ops")
	     ;; ,@(if config::weak-eval-cache '((:file "image-gc"))
             (:file "image-pyramids")
	     #+(and :sbcl :x86-64) (:file "mmap-array-image")
	     ))

   (:file "f3d-pkg")

   #+f3d-gui
   (:file "finalize-cme" :depends-on (:cme :basic-gui :lisp-tk))

   (:file "tests")
   ))

;;;
;;; Not sure this is the best way to handle this.  Should the system
;;; initialization facility be in package ASDF?
;;;
(in-package :asdf)


(defmethod perform :after ((o load-op) (c (eql (find-system :f3d-core))))
  (initialize-all-systems)
  )
