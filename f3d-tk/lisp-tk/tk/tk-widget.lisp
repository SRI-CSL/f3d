(in-package :lisptk)


#|
TODO:

Decision point:  Currently we use tcl-create-command for define-tk-binding.
   This requires widget destruction to do delete-on-destroy of the tcl-commands
   associated with the widget.  Currently non-cvv widgets have no associated Lisp
   data structure for remembering commands. (I guess it is simple to add another
   hash-table mapping from widget-id to a property list of stuff.

   An alternative is to (almost) always use qcme-callback to dispatch these commands.
   Let the event-id be a gensymed name of a function to call, or a number, indicating
   to call the function TKCALLBACK<number>.  We thereby avoid all the tcl command
   definition stuff for individual objects.

|#

(defun string-to-number (string)
  (read-from-string string))

;;; string is assumed to start 0x
(defun hex-string-to-number (string)
  (read-from-string (format nil "\#x~a" (substring string 2))))

(defun string-to-lisp (string)
  (read-from-string string))

(defun number-to-string (number)
  (format nil "~a" number))

(defun string-to-symbol (string)
  (intern string))

(defun string-contains-whitespace (string)
  (position #\space string))

(defvar *toplevel-widget-counter* 0)

(defun gensym-toplevel-widget-path (&optional name)
  (format nil "~a~d" (or name ".top") (incf *toplevel-widget-counter*)))

(defun gensym-widget-path (parent)
  (format nil "~a~d"
	  (if parent (format nil "~a.w" parent) ".top")
	  (incf *toplevel-widget-counter*)))

(declaim (special *default-parent*))

(defmethod merge-widget-pathname (parent name)
  (when (typep parent 'widget-panel) (setq parent (widget parent)))
  (when (symbolp name) (setq name (string-downcase  name)))
  (if (eql (aref name 0)  #\.) 
      name  ; name is an absolute path -- return it
      (let ()
	(when (null parent)
	  (setq parent *default-parent*))
	(if (equal parent ".")
	    (format nil ".~a" name)
	    (format nil "~a.~a" parent name)))))

(defmethod widget-named (parent item-name)
  ;;(break)
  (when (typep parent 'widget-panel) (setq parent (widget parent)))
  (let ((widget (tcl-cmd `(widget_named ,parent ,item-name))))
    (if (equal widget "")
	nil
	widget)))

;;; This version avoids calling TCL-EVAL
(defmethod widget-name (widget)
  (let ((pos (position #\. widget :from-end t)))
    (if pos
	(subseq widget (1+ pos))
	widget)))

(defmethod decompose-widget-name (widget)
  (loop for pos = 1 then (1+ next-pos)
	for next-pos = (position #\. widget :start pos)
	collect (subseq widget pos next-pos)
	while next-pos))

;(decompose-widget-name ".top15.fsbuts9.present")

;;; The handling of package-names for symbols associated with tk-widgets sucks.
;;; How can we have special item names like QUIT-PANEL UPDATE-PANEL LOCK-PANEL
;;; whose package is guaranteed but also allow other item names to belonw to the
;;; package that defined the panel

;;; These always map to :tk package rather than panel-package.
(defparameter *tk-widget-names-mapping-alist*
  '(("quit-panel" quit-panel)
    ;;("update-panel" update-panel) ; should this map to panel-package?
    ("lock-panel" lock-panel)))

;; widget-symbol is upper case symbol of the last name of the widget pathname.
(defmethod widget-symbol (widget-pathname (package t))
  (unless package
    #+neveer
    (format t "WIDGET-SYMBOL cannot determine panel-package for widget ~a~%" widget-pathname)
    (setq package *package*))
  (let ((name (widget-name widget-pathname)))
    (or (cadr (assoc name *tk-widget-names-mapping-alist* :test 'string-equal))
	(intern (string-upcase name) package))))

;;;(defun widget-toplevel0 (widget)
;;;  (tcl-eval "winfo" "toplevel" widget))

;; This version avoids round trips thru tcl-eval
(defmethod widget-toplevel (widget)
  (let ((pos (position #\. widget :start 1)))
    (if pos
	(substring widget 0 pos)
	".")))

(defmethod window-exists (window)
  (equal (tcl-cmd `(winfo exists ,window)) "1"))

(defmethod window-width ((win string))
  (string-to-number (tk-winfo 'width win)))

(defmethod window-height ((win string))
  (string-to-number (tk-winfo 'height win)))

(defmethod window-x ((win string))
  (string-to-number (tk-winfo 'x win)))

(defmethod window-y ((win string))
  (string-to-number (tk-winfo 'y win)))

(defmethod window-rootx ((win string))
  (string-to-number (tk-winfo 'rootx win)))

(defmethod window-rooty ((win string))
  (string-to-number (tk-winfo 'rooty win)))

(defmethod screen-width ((win string))
  (string-to-number (tk-winfo 'screenwidth win)))

(defmethod screen-height ((win string))
  (string-to-number (tk-winfo 'screenheight win)))

(defun widget-parent (win)
  (string-to-lisp (tk-winfo 'parent win)))  ;; is this right?

(defun widget-parent (win)
  (tk-winfo 'parent win))

(defun widget-window-parameters  (widget)
  (tk::tcl-list-to-lisp
   (tk::tcl-cmd `(widget_screen_parameters ,widget))))

#|
(let ((w ".top3.frm.0-0.gl"))
  (list (widget-toplevel0 w) (widget-toplevel w)))
(window-width ".frm3.frm.1-1.1-1_gl")
|#


;;; Specialized widget methods

(defun text-widget-xy-stringpos (widget x y)
  (read-from-string
   (tcl-eval 'text_widget_xy_stringpos widget x y)))

(defmethod get-listbox-selected-item-indices (widget)
  (tcl-list-to-lisp (tcl-eval widget 'curselection)))

;;; Return the item strings of the selected items
;;; cvv defines this for cvv-items
(defmethod get-listbox-selected-items (widget)
  (loop with items = (tcl-list-to-strings  (tcl-eval widget "get" 0 "end") )
	for index in (get-listbox-selected-item-indices widget)
	collect (nth index items)))

(defmethod set-listbox-items (widget list)
  (tcl-eval "set_listbox_items"
	    widget
	    `(list .,(loop for item in list
			   for text = (if (consp item) (car item) item)
			   ;;collect (tcl-maybe-brace-string text)
			   collect (tcl-string text)))))

(defmethod set-optionmenu-items (widget list &optional select-item )
  (tcl-eval "set_optionmenu_items"
	    widget
	    `(list .,(loop for item in list
			   for text = (if (consp item) (car item) item)
			   ;;collect `(list ,(tcl-maybe-brace-string text))
			   collect `(list ,(tcl-string text))))
	    "{}")
  (tcl-eval 'set_optionmenu_selected_item
	    widget
	    (or select-item 0))
  )

(defmethod set-widget-items (widget list)
  (tcl-eval "set_widget_items"
	    widget
	    `(list .,(loop for item in list
			   for text = (if (consp item) (car item) item)
			   ;;collect (tcl-maybe-brace-string text)
			   collect (tcl-string text)
			   ))))

(defmethod set-widget-items (widget list)
  (tcl-eval "set_widget_items"
	    widget
	    `(list .,(loop for item in list
			   for text = (if (consp item) (car item) item)
			   ;;collect (tcl-maybe-brace-string text)
			   collect (tcl-string text)
			   ))))

(defmethod set-widget-items (widget list)
  (tcl-eval "set_widget_items" widget (tcl-constant list)))


(defmethod add-listbox-item (widget item)
  (tcl-eval widget "insert" "end" item))

#|
(setq *tk-verbose* t)
(set-listbox-items (widget-named (widget *inspector-panel*) 'history)
		   '("foo1" "foo2"))
(get-listbox-selected-items (widget-named (widget *inspector-panel*) 'history))
|#


;;; Be careful with this -- widget_value was changed to return class and value in a
;;; tcl list.
;;;(defmethod widget-value ((widget t) &optional parent)
;;;  (tcl-eval "widget_value" (widget-named parent widget)))

(defmethod widget-value ((widget t) &optional parent)
  (let ((l (tcl-list-to-toplevel-strings (tcl-eval "widget_value" (widget-named parent widget)))))
    (values (car l)  ; value
	    (cadr l) ; class
	    )))

(defmethod set-widget-value ((widget t) value &optional parent)
  (tcl-eval "set_widget_value" (widget-named parent widget) value))


#|
(widget-value )
|#

;;; CLIDGETS are anything all all you want to associate with a TK widget-pathname

(defparameter *widget-clidget-hash-table* (make-hash-table :test 'equal))

;;; WHY DO WE USE STRINGS HERE FOR THE HASH-TABLE KEY, rather than lisp symbols?

(defun widget-clidget (widget-name)
  (gethash (to-string widget-name) *widget-clidget-hash-table*))

(defun remove-widget-to-clidget (widget)
  (remhash (to-string widget) *widget-clidget-hash-table*))
    
(defun (setf widget-clidget)(clidget widget-name)
  (setf (gethash (to-string widget-name) *widget-clidget-hash-table*) clidget))


(defparameter *pathname-to-widget-allow-tk-pathnames* t)

;;; called from tk-bindings args-to-template ...
(defun pathname-to-widget (pathname)
  (or ;;(gethash pathname *pathname-to-widget-hash-table*)
      (and *pathname-to-widget-allow-tk-pathnames* pathname)
      ))


;;; ***************************  MULTIVIEW_SCREENS  ***************************

;;; Need to initialize this from an environment-variable (or something).
(defparameter *multiview-screen-spec-alist* nil)

;;(defparameter *multiview-screen-spec-alist* `((":0.0" (1600 1200 0 0) (1280 1024 1600 0))))
;(setf (environment-variable "MULTIVIEW_SCREENS") "((\":0.0\" (1600 1200 0 0) (1280 1024 1600 0)))")
;(multiview-screen-spec-alist)

(defun multiview-screen-spec-alist ()
  (or *multiview-screen-spec-alist*
      (setf *multiview-screen-spec-alist*
	    (let ((s (environment-variable "MULTIVIEW_SCREENS")))
	      (and s (read-from-string s))))))

;;; return (width height xleft ytop) of the physical screen at (x y)
(defun multiview-screen-spec-at-position (widget x y)
  (loop for screen-spec in (cdr (assoc (tcl-cmd `(winfo screen ,widget)) (multiview-screen-spec-alist)
				       :test #'equal))
	for (w h xl yt) = screen-spec
	for xr = (+ xl w)
	for yb = (+ yt h)
	when (and (>= x xl) (>= y yt) (<= x xr) (<= y yb))
	  return screen-spec
	;; return the entire screen if multiview-screen-spec-alist is empty 
	;; or if the (x y) position is outside each screen.
	finally (return (list (screen-width widget)
			      (screen-height widget)
			      0 0))))

;(tcl-cmd `(wm geometry ,(widget gui::*cme-control-panel*))) => "732x77+1810+19"
;(tcl-cmd `(winfo pointerxy ,(widget gui::*cme-control-panel*))) => "496 386"
;(screen-width ".")
(defun parse-geometry (s)
  ;; If s is a null string, this will throw an error.  BUT, that means
  ;; that the window hasn't been properly initialized, so something
  ;; deeper is the cause of the problem...
  (let* ((p1 (position #\x s))
	 (p2 (position #\+ s :start (1+ p1)))
	 (p3 (position #\+ s :start (1+ p2))))
    (values (read-from-string  s nil nil :start 0 :end p1)
	    (read-from-string s nil nil :start (1+ p1) :end p2)
	    (read-from-string s nil nil :start (1+ p2) :end p3)
	    (read-from-string s nil nil :start (1+ p3)))))

(defun encode-geometry (width height x y)
  (if (and width height)
      (format nil "~ax~a+~a+~a" width height x y)
      (format nil "+~a+~a" x y)))

;(multiview-screen-spec-at-position (widget gui::*cme-control-panel*) 100 100)	
;(multiview-screen-spec-at-position (widget gui::*cme-control-panel*) 1700 100)	


;;; *****************************  SET-TOPLEVEL-WIDGET-POSITION  *****************************


(defun confine-to-screen-position (widget &optional (position (tk-pointer-position2)))
  (labels ((get-geometry (widget)
	     (parse-geometry (tcl-cmd `(winfo geometry ,widget)))))
    (destructuring-bind (x y) position
      (destructuring-bind (sw sh sx sy)
	  (multiview-screen-spec-at-position widget x y)
	(mv-bind (ww wh) (get-geometry widget)
	  ;; make sure widget does not extend beyond right or bottom of screen
	  (when (> (+ x ww) (+ sx sw)) (setq x (- (+ sx sw) ww)))
	  (when (> (+ y wh) (+ sy sh)) (setq y (- (+ sy sh) wh)))
	  ;;(format t "wm geometry ~a +~a+~a ~a" widget x y (list ww wh sx sy sw sh))
	  (tcl-cmd `(wm geometry ,widget ,(encode-geometry nil nil x y)))
	  (list x y))))))

;;; POSITION is a list (x y) 
;;;          or one of the following keywords: :MOUSE, :MOUSE-SCREEN-CENTER, or :UPPER-LEFT
;;; FIXME -- WM decorations aren't being accounted for.
(defun set-toplevel-widget-position (widget position)
  (labels ((get-geometry (widget)
	     (parse-geometry (tcl-cmd `(winfo geometry ,widget))))
	   (set-position (x y) ; x and y are position relative to virtual screen
	     (setq x (round (or x 0)) y (round (or y 0)))
	     (destructuring-bind (sw sh sx sy)
		 (multiview-screen-spec-at-position widget x y)
	       (mv-bind (ww wh) (get-geometry widget)
		 ;; make sure widget does not extend beyond right or bottom of screen
		 (when (> (+ x ww) (+ sx sw)) (setq x (- (+ sx sw) ww)))
		 (when (> (+ y wh) (+ sy sh)) (setq y (- (+ sy sh) wh)))
		 ;;(format t "wm geometry ~a +~a+~a ~a" widget x y (list ww wh sx sy sw sh))
		 (tcl-cmd `(wm geometry ,widget ,(encode-geometry nil nil x y)))
		 (list x y)))))

    (cond ((keywordp position)
	   (case position
	     (:mouse 
	      (destructuring-bind (x y) (tk-pointer-position2)
		(set-position x y)))
	     (:mouse-screen-center   ; center of the screen that the mouse is on
	      (destructuring-bind (x y) (tk-pointer-position2)
		(mv-bind (ww wh) (get-geometry widget)
		  (destructuring-bind (sw sh sx sy)
		      (multiview-screen-spec-at-position widget x y)
		    ;(format t ":mouse-screen-center ~a ~a ~a~%" (list ww wh) (list sw sh sx sy) (list (+ sx (ash (- sw ww) -1) ) (+ sy (ash (- sh wh) -1) )))
		    (set-position (+ sx (ash (- sw ww) -1) )
				  (+ sy (ash (- sh wh) -1) ) )))))
	     (:upper-left
	      (mv-bind (ww wh wx wy) (get-geometry widget)
		(destructuring-bind (sw sh sx sy) (multiview-screen-spec-at-position widget wx wy)
		  (set-position (+ sx 10) (+ sy 10)))))))
	  ((stringp position) ; assume it is the widget spec of a window to place below.
	   (mv-bind (w h xl yt) (get-geometry position)
	     (set-position xl (+ yt h 10))))
	  ((or (consp position) (vectorp position))
	   (let (x y)			; position relative to virtual screen
	     (if (consp position)
		 (setq x (car position) y (cadr position))
		 (setq x (aref position 0) y (aref position 1)))
	     (set-position x y)))
	  (t (error "SET-TOPLEVEL-WIDGET-POSITION unknown position ~a~%" position)))))


;(setq *frame* (gui::pane-frame (gui::selected-window)))
;(set-toplevel-widget-position (widget *frame*) :mouse) 
;(set-toplevel-widget-position (widget *frame*) :upper-left) 
;(set-toplevel-widget-position (widget *frame*) (widget gui::*cme-control-panel*))
;(set-toplevel-widget-position (widget *frame*) '(200 200)) 
;(tcl-cmd `(wm geometry ".frm4")) 
;(tcl-cmd `(wm geometry ".frm4" ,(tcl-cmd `(wm geometry ".frm4"))))

#|
(set-toplevel-widget-position '.frm :mouse)
|#


;;; ****************************  FRAME-PANES-TK-SCRIPT  ****************************

(defmethod glwindow-script
	   (parent
	    winid
	    &rest args &key
	    width height
	   (borderwidth 1)
	    &allow-other-keys)
  (let* ((wframe-name (format nil "~a.~a" parent winid))
	 (tkglwin-name (format nil "~a.~a_gl" wframe-name winid))
	 (script `((qframe ,wframe-name
			   -highlightthickness ,borderwidth
			   -highlightbackground black
			   -relief flat 
			   -weight 1
			   )
		   ,@(apply 'tkglwin-script tkglwin-name args)
		   (pack ,tkglwin-name :expand true -fill both))))
    (values script
	    wframe-name
	    tkglwin-name)))

;; #+ttk-widgets
(defmethod glwindow-script
	   (parent
	    winid
	    &rest args &key
	    width height
	   (borderwidth 1)
	    &allow-other-keys)
  (let* ((wframe-name (format nil "~a.~a" parent winid))
	 (tkglwin-name (format nil "~a.~a_gl" wframe-name winid))
	 (script `((frame ,wframe-name
			   -highlightthickness ,borderwidth
			   -highlightbackground black
			   -relief flat 
			   ;; -weight 1  ;; fixme -- removed for TTK widgets
			   ;; oops removing causes problems with resizing frame
			   )
		   (putprop ,wframe-name widget_weight 1) ; this is what qwidgets.tcl does
		   ,@(apply 'tkglwin-script tkglwin-name args)
		   (pack ,tkglwin-name :expand true :fill both))))
    (values script
	    wframe-name
	    tkglwin-name)))

(defmethod frame-panes-tk-script
	   (parent
	    &rest args &key
	    nx ny			; number of panes horiz and vert
	    (frame-name "frm")
	    (width 1024) (height 800)
	    (borderwidth 1)
	    (weight 1)			; TK resize weighting
	    &allow-other-keys)
  (ignore width height borderwidth )
  (let* ((frame-name (format nil "~a.~a" parent frame-name))
	 (pane-width (floor width nx))
	 (pane-height (floor height ny))
	 pane-alist
	 (script
	  `((qframe ,frame-name -weight ,weight 
	     ;; -highlightcolor is shown when widget has input focus.
	     ;; Is this really useful?
	     ;;-highlightcolor red -highlightthickness 2
	     )
	    ,@(loop for iy from (1- ny) downto 0
		    for y from 0
		    nconc
		    (loop for ix from 0 below nx
			  for winid = (format nil "~a-~a" ix iy)
			  with (pane-script wframe-name tkglwin-name)
			  do (mv-setq (pane-script wframe-name tkglwin-name)
			       (apply 'glwindow-script frame-name winid
				      :width pane-width :height pane-height
				      args))
			  append pane-script   
			  collect `(grid ,wframe-name -sticky news
				    -row ,y -column ,ix)
			  collect `((,ix ,iy) . ,tkglwin-name) into pane-strip-alist
			  finally
		       (setq pane-alist (nconc pane-strip-alist pane-alist))
			  )
		    collect `(grid rowconfigure ,frame-name ,y -weight 1))
	    ,@(loop for ix from 0 below nx
		    collect `(grid columnconfigure ,frame-name ,ix -weight 1)))
	   ))
    (values script pane-alist)))


