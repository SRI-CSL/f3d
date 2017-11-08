(in-package :gui)

;;;#+broken
;;;(defun-cached make-cvv-dashed-line-mask (dashes)
;;;  (let* ((bytes-per-line  (ceiling wid 8))
;;;         (array (lcl::with-static-area (make-array (list hi wid) :element-type 'bit))))
;;;    (loop named fill
;;;          with x = 0
;;;          with state = 1
;;;          with y0 = (ash hi -1)
;;;          do (loop for dash in (apply 'ic::circular-list dashes)
;;;                   do (loop repeat (* expand-x dash)
;;;                            do (when (>= (incf x) wid) (return-from fill nil))
;;;                               (loop for y from y0 repeat expand-y
;;;                                     do (setf (aref array y (Xlib-bitmap-index-hack x))
;;;                                              ;; XBitmap bit order
;;;                                              state)))
;;;                      (setq state (- 1 state))))
;;;    ;;(setq *dash-arr* array)
;;;    (xw::XCreateImage (xw::xdisplay screen) (ic::visual screen) ;; (ic::xrootvisual screen)
;;;                      1 ic::*XYBitmap* 0 (sys:underlying-simple-vector array)
;;;                      wid hi 8 bytes-per-line)))


#+broken
(defun make-dashed-line-alist-item (name dashes &key foreground background )
  (list (make-cvv-dashed-line-mask dashes)
	dashes
	(string-upcase name)))
  
(defparameter *dashed-line-list*
  '(("     " nil) ("Solid" :none)
    (1 1) (1 2) (1 3) (2 2) (3 3) (2 4) (4 4) (6 6) (8 8) (6 4 2 4)))

#|
(eval-cache::eval-cache-flush-function 'make-dashed-line-alist)
|#

(defun make-dashed-line-alist ()
  (eval-cache (make-dashed-line-alist *dashed-line-list*)
      (loop for dashes in *dashed-line-list*
	    collect (if (stringp (car dashes))
			dashes
			;; In CME-6 version, a list of (pixmap dashes name)

			(list (make-cvv-dashed-line-mask dashes) ; 
			      dashes
			      (string-upcase (format nil "~a" dashes)))))))



(defparameter *default-color-palette*
  '("black"
    "white"
    "red"
    "green"
    "blue"
    "magenta"
    "cyan"
    "yellow"
    "light blue"
    "orange"
    "light green"
    "turquoise"
    "salmon"
    "olive drab" 
    "brown"
    
    ))

(defun color-spec-alist (&optional (color-names *default-color-palette*))
  (list* '("Default" nil)
	 '("Invisible" invisible-color)
	 (loop for name in color-names
	       collect (list name name))))

(defun color-spec-alist (&optional (color-names *default-color-palette*))
  `("Default" "Invisible" . ,color-names))


(defun color-spec-alist (&optional (color-names *default-color-palette*))
  `("Default" "Invisible"
    . ,(loop for color-name in color-names
	     for index from 2
	     collect `(radiobutton ,index
		       :background ,color-name
		       :label ,color-name))))


(defun color-spec-alist (&optional (color-names *default-color-palette*))
  `("Default" "Invisible"
    . ,(loop for color-name in color-names
	     for index from 2
	     collect `(,index ,color-name
		       :class radiobutton
		       :background ,color-name
		       :label ,color-name))))

(defun hyphenate-whitespace (string)
  (loop with output
	for prev-pos = 0 then (1+ pos)
	for pos = (position #\space string :start prev-pos)
	do (setq output
		 (if output
		     (string-append output "-" (subseq string prev-pos pos))
		     (subseq string prev-pos pos)))
	while pos
	finally (return output)))

#|
(hyphenate-whitespace "foo")
(hyphenate-whitespace "foo bar baz")
(setq dashes-2x2-bitmap (tk::tcl-cmd '(image create bitmap -file "/tmp/dashes-2x2.bmp")))
|#
			    
(defun color-spec-alist (&optional (color-names *default-color-palette*))
  `((default-color "Default")
    ;;(invisible-color "Invisible" -bitmap ,(tk::tcl-cmd '(image create bitmap -file "/tmp/dashes-2x2.bmp")))
    (invisible-color "Invisible")
    . ,(loop for color-name in color-names
	     for index from 2
	     collect `(,(intern (string-upcase (hyphenate-whitespace color-name))) ,color-name
		       ;;:class radiobutton
		       ;;:label ,color-name
		       :background ,color-name
		       ))))

;;; call gl::make-glstipple with these patterns to get a bitmap
(defparameter *graphics-style-stipple-patterns*
  '(("stipple-solid" ((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1)))
    ("stipple2" ((0 1 0 1) (1 0 1 0) (0 1 0 1) (1 0 1 0)))
    ("stipple1" ((0 1 0 0) (0 0 0 1) (1 0 0 0) (0 0 1 0)))
    ("stipple3" ((0 1 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 0)))
    ("stipple4" ((0 1 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
    ))

;;; OpenGL constrains glLineStipple patterns to repeat mod 16.  This greatly limits
;;; the possible regular patterns to be a multiple of 2 4 8 or 16 in length.
;;; This list contains arguments to glLineStipple.
(defparameter *dashed-line-list*
  '(("     " nil) ("Solid" :none)
    (1 #b0101010101010101)
    (1 #b0001000100010001)
    (2 #b0101010101010101)
    (2 #b0001000100010001)
    (4 #b0101010101010101)
    (4 #b0001000100010001)
    ))


(defun make-edit-graphics-style-filter-panel (&optional (screen (default-menu-screen)))
  (let* ((panel (make-cvv-panel
		 `((panel-controls nil :button-list :items
		    ((quit-panel "Quit") (update-panel "Update")))
		   (pick-object "Pick Object" :button 
		    :documentation "Pick an OBJECT in a VIEW")
		   (graphics-style-filter "Filter:" :exclusive-list :visible-Item-Count 4)
		   (predicate "Predicate:" :exclusive-list :visible-Item-Count 6)
		   (color "Color:" :abbrev-assoc)
		   ;;(dash-style "Dash Style" :abbrev-assoc :alist ,(make-dashed-line-alist) :initial-value nil)
		   ;;#+never
		   (line-width "Line Width:" :assoc :alist
		    (("  " nil  :documentation "Draw Lines using Global Default Line Width")
		     ("0" 0) ("1" 1) ("2" 2) ("3" 3) ("4" 4)("5" 5) ("6" 6) )
		    :initial-value nil)
		   ;;(stipple "Stipple:" :assoc :alist ,(get-sg-stipple-alist :screen screen) :initial-value nil)
		   )
		 :title "Graphics Style Editor"
		 :callback-function  'edit-graphics-style-filter-panel-item-changed
		 :screen screen
		 ;;:panel-controls nil
		 )))
    (tk::set-item-list (tk::get-named-item panel 'color) (color-spec-alist))
    (tk::set-item-value panel 'color 'red)
    panel))

#|
(setq egsfp (make-edit-graphics-style-filter-panel))
(tk::tcl-cmd `(source ,(tk::tcl-filename-filter "$FREEDIUS/tk/library/composite-widgets.tcl")))
(tk::tcl-cmd `(source ,(tk::tcl-filename-filter "$FREEDIUS/tk/library/tk-utils.tcl")))
(tk::tcl-cmd '(puts "$errorInfo"))
(describe (setq color-item (tk::get-named-item egsfp 'color)))
(setq color-widget (tk::widget color-item)
      color-menu (format nil "~a.color_menu" color-widget))
(describe (get-prop (tk::get-named-item egsfp 'color) :items))
(tk::set-item-value egsfp 'color 'red)
(tk::set-item-value egsfp 'color 'default-color)
(tk::get-item-value egsfp 'color)
(tk::tcl-cmd `(,color-menu entrycget 0 -background))
(tk::tcl-cmd `(,color-menu entrycget 4 -background))
(tk::tcl-cmd `(,color-menu entrycget 1 -bitmap))
(tk::tcl-cmd `(,color-widget configure -background false))
(tk::tcl-cmd `(.top24.panel-controls.update-panel cget -background))
(tk::tcl-cmd `(,color-widget configure -background))	    
(tk::tcl-cmd `(nth 3 (,color-widget configure -background))	    )
(tk::tcl-cmd `(nth 3 (,color-widget configure -bitmap))	    )
|#

(defun edit-graphics-style-filter-panel-item-changed (panel widget item-name event args)
  (declare (ignore panel))
  (format t "edit-graphics-style-filter-panel-item-changed ~a~%"
	  (list widget item-name event args))
  ;;(break)
  )
  
#|
(defun edit-graphics-style-filter-panel-item-changed (panel widget item-name event args)
  (declare (ignore widget event args))
  (case item-name
    (pick-view
     (let ((pane (gui::pick-a-pane "Pick a View")))
       (when pane
	 (edit-graphics-style-filter-panel-set-object panel (get-prop panel :obj)  (top-view pane)))))
    
    (pick-object
     (multiple-value-bind (object pane) (pick-an-object "Pick an Object")
       (if (typep object 'obj::gl-object)
	   (edit-graphics-style-filter-panel-set-object panel object (top-view pane))
	   (gui::report-error-to-gui "Must select a CME object")
	   )))

    ((graphics-style-filter)
     (set-panel-predicate-list panel)
     (update-panel-graphics-style-attributes panel))

    ((update-panel predicate) (update-panel-graphics-style-attributes panel))

    ((color line-width dash-style stipple )
     (let ((object (get-prop panel :obj))
	   (filter (get-item-value panel 'graphics-style-filter))
	   (predicate (get-item-value panel 'predicate)))
       (when (and object filter predicate)
	 (set-graphics-style-with-predicate object filter predicate
					    :color (get-item-value panel 'color)
					    :line-width (get-item-value panel 'line-width)
					    :dash-style (get-item-value panel 'dash-style)
					    :stipple (get-item-value panel 'stipple)
					    :recompute-stipples (and (eq item-name 'stipple)
								     ;;(typep object 'planar-solid)
								     ))
	 ;; This causes the predicate item-list to be regenerated so that the coverage codes are correct
	 (update-panel-graphics-style-attributes panel))))))
|#


