(in-package :gui)

;;; This is unfinished.

(defclass screen (property-list-mixin)
  ())

(defparameter *the-screen*
  (make-instance 'screen
		 :property-list
		 '(:selected-window-border-color "green"
		   :unselected-window-border-color "white")))

