;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)
;;;
;;; Agnostic GUI system - this is loaded before any backend and
;;; contains a lot of interaction, world handling, etc.  The backend
;;; is loaded last.  Current default is Tcl/Tk....

;;;
;;; Add features that will control the UI substrate.  The user has the
;;; option of adding :cocoa to *features* to induce MacOSX-native UI
;;; windows and widgets, as opposed to X11.

;;; Core will not need these:

;;; (pushnew :tkgui *features*)

;;; #-(or cocoa win32)
;;; (pushnew :glx *features*)

;;; #+win32
;;; (pushnew :wgl *features*)

;; (sb-posix::setenv "RADIUS" "/opt/IU/radius")

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

(asdf:defsystem :f3d-gui
  :name "f3d-gui"
  :depends-on (:f3d-geom)
  :maintainer "Chris Connolly, SRI International <connolly@ai.sri.com>"
  :license "Mozilla"
  :description "FREEDIUS GUI front-end (no tk-specific code)"
  :components 
  (
   (:module :basic-gui
	    :serial t
	    :components
	    ((:file "gui-pkg")
	     (:file "macros")
	     
	     ;; GL image-pane machinery
	     (:file "gl-qcme-ffi")
	     (:file "window")
	     (:file "screen")

	     ;; GUI infrastructure 
	     (:file "object-sets")   ; must be loaded before worlds.lisp and display.lisp
	     (:file "worlds")        ; this can go almost anywhere in this file (after object-sets.lisp).
	     ;;,@(when config::weak-eval-cache '((:file "weak-children-hash-table-mixin")))
	     (:file "view")          ; needs object-sets.lisp
	     (:file "interactor")    ;
	     (:file "window-panel")  ; glwin-mouse-buttonpress-callback needs interactor class def)

	     ;; View display machinery
	     (:file "gl-matrices")
	     (:file "display")
	     #+cppdisplay (:file "display-image-ffi")
	     #-cppdisplay (:file "display-image")
	     (:file "lighting")
	     (:file "pick")          ; need interactor class def before pick.lisp

	     ;; GUI interaction methods
	     (:file "commands")        ; no-tk
	     (:file "object-motions")  ; no-tk
	     (:file "object-ops")      ; tk dependency: GUI-USAGE-ERROR
	     (:file "composite-objects") ; no-tk
	     (:file "transform-tools") ; this file should move to the basic-gui directory
	     (:file "image-gui-defs")  ; no-tk
	     
	     (:file "temporal")        ; no-tk
	     ))

   (:module :ui-objects
	    :serial t
	    :depends-on (:basic-gui)
	    :components
	    ((:file "image-object")
	     (:file "object-labels") 
	     (:file "conjugate-point")
	     ))

   ;; I don't like this at all - why is this dependent on basic-gui??
   (:module :model-io
	    :serial t
	    :depends-on (:ui-objects)
	    :components
	    ((:file "io-pkg")
	     (:file "io-forms")
	     (:file "transform-io")
	     ;;(:file "object-io")
	     (:file "model-input")
	     (:file "pprint-object")))

   (:module :terrain
	    :serial t
	    :depends-on (:basic-gui)
	    :components
	    ((:file "terrain-models")
	     (:file "dtm-intersect")
	     (:file "usgs-dem")
	     ))

   (:module :radius
	    :serial t
	    :depends-on (:basic-gui :terrain)
	    :components
	    ((:file "smc-pkg")
	     (:file "radius-classes")))

   (:module :cme
	    :serial t
	    :depends-on (:basic-gui :radius #+never :clim-glx )
	    :components
	    ((:file "cme-pkg")
	     (:file "load-cme-objects")
	     (:file "camera-models")
	     (:file "site-glue")
	     (:file "camera-model-io")
	     (:file "compat-defs") 

	     (:file "start-cme")
	     (:file "cme-control-panel")
	     (:file "selection-panel")
	     (:file "cvv-object-menus") ; this should move to the gui subsystem
	     ))
   #+f3d-tk
   (:file "finalize-cme" :depends-on (:cme :basic-gui :lisp-tk))
   ))

;;;
;;; Not sure this is the best way to handle this.  The system
;;; initialization facility should not be in asdf, in my opinion:
;;;
(in-package :asdf)

#+f3d-gui
(defmethod perform :after ((o load-op) (c (eql (find-system :cl-cme-tk))))
  (initialize-all-systems))
