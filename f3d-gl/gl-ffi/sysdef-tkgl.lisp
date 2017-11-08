(in-package :cl-user)

;;; Do the files of this system really belong in anpther directory?

;;; wgl-image-panes glx-image-panes  set in config.lisp

(st:define-system "tkgl"
    :system-class 'st::mixed-lisp-system
    :required-systems '(gl tk)
    :files `("tkgl.lisp"
	     ,@(when config::wgl-image-panes '("wgl-ffi.lisp"))
	     ,@(when config::agl-image-panes '("agl-ffi.lisp"))
	     ,@(when config::cocoa-image-panes '("cocoa-ffi.lisp"))
	     ,@(when config::glx-image-panes '("x11-error-handler.lisp" "glx-ffi.lisp"))
	     ))
	     
