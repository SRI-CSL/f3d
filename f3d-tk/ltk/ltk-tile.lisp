(in-package :cl-user)
#|

 This software is Copyright (c) 2005  Peter Herth <herth@peter-herth.de>

 Peter Herth grants you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
|#

;;(export 'ltk::style :ltk)

;;;
;;; This is a mess.  Lots of duplicate symbols...

(defpackage :lttk
  (:use :common-lisp :ltk)
  (:export #:style-default #:style-element-names #:theme-names
	   #:use-theme #:tile-test)
  (:import-from :ltk #:defargs #:defwidget)
  )

(in-package :lttk)

(eval-when (eval load compile)

;;(export 'style)

(defparameter ltk::*lttk-widget-symbols*
  '(widget frame labelframe scrollbar scale
    entry label 
    button check-button radio-button  pulldownmenu-button
    notebook paned treeview
    ))

) ; end eval-when


(defmacro with-lttk-widgets (&body body)
  ;; The functions must be in the package of the compile environment.
  (flet ((intern* (x) (intern (symbol-name x) *package*)))
    `(flet (,@(loop for wn in (list* 'ltk::menubutton ltk::*lttk-widget-symbols*)
		    collect `(,(intern* wn) (name &rest initargs) 
			       (apply #'make-instance ',wn :name name initargs)))
	    (,(intern* 'toplevel) (&rest initargs) 
	      (apply #'make-instance 'ltk::toplevel initargs)))
       (macrolet ,(unless (eql (intern* 'with-parent) 'with-parent)
			   `((,(intern* 'with-parent) (parent &body body) 
			       `(lttk::with-parent ,parent ,@body))))
	 ,@body))))

(defpackage :lttkw (:use :common-lisp :ltk :lttk))

(in-package :ltk)

(eval-when (eval load compile)
  (defparameter ltk::*lttkw-symbols-from-ltk*
    '(with-parent *current-master* )))
  
(shadowing-import ltk::*lttk-widget-symbols* :lttkw)
(shadowing-import ltk::*lttkw-symbols-from-ltk* :lttkw)

(export ltk::*lttk-widget-symbols* :lttkw)
(export ltk::*lttkw-symbols-from-ltk* :lttkw)

(in-package :lttkw)

(defmacro define-lttkw-macros ()
  (flet ((intern* (x) (intern (symbol-name x) *package*)))
    `(progn ,@(loop for wn in (list* 'ltk::menubutton ltk::*lttk-widget-symbols*)
		    collect `(defun ,(intern* wn) (name &rest initargs) 
			       (apply #'make-instance ',wn :name name initargs))))))

(define-lttkw-macros)

(in-package :ltk)

(import *ltk-export-symbols* :lttk)
(export *ltk-export-symbols* :lttk)

(eval-when (eval load compile)
(setf *initargs*
      (append *initargs*
	      '((style style "~@[ -style {~a}~]" style "")
		(padding padding "~@[ -padding {~{~a ~}}~]" (if (consp padding) padding (and padding  (list padding padding))) "")


		(tvcolumns columns "~@[ -columns [list ~{{~a} ~}]~]" columns
		 "-columns option for treeview widget")
		
		       
		 )))
) ; end eval-when

#+official
(progn 
;;; The advertised options in ~/downloads/tcltk/tile-0.7.8/doc/html

(defargs lttk::widget ()
  class cursor takefocus style)

(defargs lttk::frame (lttk::widget)
  borderwidth relief padding width height)

(defargs lttk::labelframe (lttk::frame)
  labelanchor labelwidget underline )   ; borderwidth isn't shown in docs

;;; not used?
(defargs lttk::scrollable-widget (lttk::widget)
  xscrollcommand yscrollcommand)

(defargs lttk::label (lttk::widget)
  anchor background borderwidth compound font foreground image justify padding relief 
  text textvariable underline width wraplength)

(defargs lttk::button (lttk::widget)   
  command compound default image state text textvariable underline width) ;  missing padding?

(defargs lttk::check-button (lttk::button)  ; should exclude default   - missing padding?
  onvalue offvalue variable)  

(defargs lttk::radio-button (lttk::button)  ; should exclude default 
  command value-radio-button variable-radio-button)

(defargs lttk::pulldownmenu-button (lttk::widget); really menubutton
  compound image state text textvariable underline width direction menu) ; missing padding?

(defargs lttk::entry (lttk::widget) 
  background exportselection font foreground invalidcommand justify  ; background foreground font ?
  show state text textvariable validate validatecommand width xscrollcommand)  ; text?

(defargs lttk::scrollbar (lttk::widget)
  command orient width) ; width ?

(defargs lttk::scale (lttk::widget)
  cbcommand orient variable from to length width) ; width

) ; end progn

(progn

;;; options actually supported in the Tile code

(defargs lttk::widget () class cursor takefocus style width)

(defargs lttk::frame (lttk::widget)
    borderwidth height padding relief )

(defargs lttk::labelframe (lttk::widget)
    borderwidth height labelanchor labelwidget padding relief text underline)

(defargs lttk::entry (lttk::widget)  ; text ?
    background exportselection font foreground invalidcommand justify show state textvariable validate validatecommand xscrollcommand )

(defargs lttk::label (lttk::widget)
    anchor background borderwidth compound font foreground image justify padding relief state text textvariable underline wraplength )

(defargs lttk::button (lttk::widget)
    command compound default image padding state text textvariable underline )

(defargs lttk::pulldownmenu-button (lttk::widget)
    compound direction image menu padding state text textvariable underline )

(defargs lttk::check-button (lttk::widget)
    command compound image offvalue onvalue padding state text textvariable underline variable )

(defargs lttk::radio-button (lttk::widget)
    command compound image padding state text textvariable underline value-radio-button variable-radio-button )

(defargs lttk::scrollbar (lttk::widget)
    command orient )

(defargs lttk::scale (lttk::widget)
  cbcommand orient variable from to length)

) ; end progn


(defwidget lttk::label (tktextvariable widget) () "ttk::label")

(defwidget lttk::button (tktextvariable widget) () "ttk::button")

(defmethod (setf command) (val (button lttk::button))
  (format-wish "~a configure -command {callback ~a}" (widget-path button) (callback-name button))
  (add-callback button val)
  val)

(defwidget lttk::check-button (tktextvariable widget tkvariable) () "ttk::checkbutton")

(defmethod (setf command) (val (check-button lttk::check-button))
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
	       (name check-button) (name check-button))
  (add-callback check-button val)
  val)


(defwidget lttk::radio-button (tktextvariable widget) 
  ((val :accessor radio-button-value :initarg :value :initform nil)
   (var :accessor radio-button-variable :initarg :variable :initform nil)) 
  "ttk::radiobutton")

(defmethod value ((rb lttk::radio-button))
  "reads the content of the shared variable of the radio button set"
  (if (radio-button-variable rb)
      (progn
	(format-wish-and-read "senddata $~a" (radio-button-variable rb))
	)
      nil))

(defmethod (setf value) (val (rb lttk::radio-button))
  "sets the content of the shared variable of the radio button set"
  (when (radio-button-variable rb)
    (format-wish "set ~a ~a" (radio-button-variable rb) val))
  val)

(defmethod (setf command) (val (rb lttk::radio-button))
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path rb) (callback-name rb) (radio-button-variable rb))
  (add-callback rb val)
  val)

(defwidget lttk::entry (tktextvariable widget) () "ttk::entry")

(defwidget lttk::frame (widget) () "ttk::frame")

(defargs lttk::separator (lttk::widget) orient)

(defwidget lttk::separator (widget) () "ttk::separator")

(defwidget lttk::labelframe (widget) () "ttk::labelframe")

(defmethod (setf text) :after (val (l lttk::labelframe))
  (format-wish "~a configure -text {~a}" (widget-path l) val)
  val)

(defargs lttk::combobox (lttk::entry) 
  postcommand values)

(defwidget lttk::combobox (widget) () "ttk::combobox")


(defwidget lttk::scrollbar (widget) () "ttk::scrollbar")

(defwidget lttk::scale (tkvariable widget) () "ttk::scale")

(defmethod (setf command) (val (scale lttk::scale))
  (add-callback scale val)					
  (format-wish "proc ~a-command {val} {callbackval ~a $val}" (name scale) (callback-name scale))
  (format-wish "~a configure -command ~a-command" (widget-path scale) (callback-name scale))
  val)
	

;;; This looks wrong -- should use defwidget.
(defclass lttk::pulldownmenu-button(widget)
  ((text :accessor text :initarg :text :initform "")
   (help :accessor menu-help :initarg :help :initform nil)
   (menu-widget-path :accessor menu-widget-path :initarg :menu-widget-path)
   (widget-class-name :accessor widget-class-name :initform "ttk::menubutton" :allocation :class)
   ))

(defmethod initialize-instance :after ((m lttk::pulldownmenu-button) &rest options &key underline 
				       &allow-other-keys)
  (when (menu-help m) ;; special treatment for help menu
    (setf (name m) "help")
    (setf (slot-value m 'widget-path) (create-path (master m) (name m))))
  (let* ((widget-path (widget-path m))
	 (menu-widget-path (format nil "~a.menu" widget-path)))
    (format-wish "~a ~A~@[ -text {~a} ~]" (widget-class-name m) widget-path (text m))
    (when (master m)
      (format-wish "menu ~a" menu-widget-path)
      (format-wish "~a configure -menu ~a" widget-path menu-widget-path)
      (setf (menu-widget-path m) menu-widget-path)
      )))

(defwidget lttk::pulldownmenu-button (tktextvariable widget)
  ((help :accessor menu-help :initarg :help :initform nil)
   (menu-widget-path :accessor menu-widget-path :initarg :menu-widget-path))
  "ttk::menubutton"
  (when (menu-help widget) ;; special treatment for help menu
    (setf (name widget) "help")
    (setf (slot-value widget 'widget-path) (create-path (master widget) (name widget))))
  ;;(format-wish "menu ~A -tearoff 0" (widget-path m))
  (let* ((widget-path (widget-path widget))
	 (menu-widget-path (format nil "~a.menu" widget-path)))
    (when (master widget)
      (format-wish "menu ~a" menu-widget-path)
      (format-wish "~a configure -menu ~a" widget-path menu-widget-path)
      (setf (menu-widget-path widget) menu-widget-path)
      )))

(defun lttk::make-pulldownmenu-button (master text &key underline name)
  (make-instance 'lttk::pulldownmenu-button :master master :text text :underline underline :name name))

#|
# scrolled -- create a widget with attached scrollbars.
#
proc scrolled {class w args} {
    set sf "${w}_sf"

    frame $sf
    eval [linsert $args 0 $class $w]
    scrollbar $sf.hsb -orient horizontal -command [list $w xview]
    scrollbar $sf.vsb -orient vertical -command [list $w yview]

    configure.scrolled $sf $w
    return $sf
}


# tile::scrolled -- create a widget with attached Tile scrollbars.
#
proc tile::scrolled {class w args} {
    set sf "${w}_sf"

    ttk::frame $sf
    eval [linsert $args 0 $class $w]
    ttk::scrollbar $sf.hsb -orient horizontal -command [list $w xview]
    ttk::scrollbar $sf.vsb -orient vertical -command [list $w yview]

    configure.scrolled $sf $w
    return $sf
}

# configure.scrolled -- common factor of [scrolled] and [tile::scrolled]
#
proc configure.scrolled {sf w} {
    $w configure -xscrollcommand [list $sf.hsb set]
    $w configure -yscrollcommand [list $sf.vsb set]

    grid $w -in $sf -row 0 -column 0 -sticky nwse
    grid $sf.hsb -row 1 -column 0 -sticky we
    grid $sf.vsb -row 0 -column 1 -sticky ns

    grid columnconfigure $sf 0 -weight 1
    grid rowconfigure $sf 0 -weight 1
}
|#

(defun configure-scrolled (sf w hscroll vscroll)
  (let ((wp (widget-path w))
	(sfp (widget-path sf)))
    (when hscroll 
      (configure w "xscrollcommand" (format nil "~a set" (widget-path hscroll)))
      (grid hscroll 1 0 :sticky 'we))
    (when vscroll
      (configure w "yscrollcommand" (format nil "~a set" (widget-path vscroll)))
      (grid vscroll 0 1 :sticky 'ns))
    (grid w 0 0 :sticky 'nwse)
    (grid-columnconfigure sf 0 :weight 1)
    (grid-rowconfigure sf 0 :weight 1)))

(defmacro lttk::with-scrollbars ((widget-form &rest options) &body body)
  `(with-parent (make-instance 'lttk::frame :master *current-master*
			       :pack '(:fill both :side top :expand true))
     (let* ((w ,widget-form)
	    (wp (widget-path w))
	    (hsb (make-instance 'lttk::scrollbar :orientation 'horizontal))
	    (vsb (make-instance 'lttk::scrollbar :orientation 'vertical)))
       (configure hsb "command" (format nil "~a xview" wp))
       (configure vsb "command" (format nil "~a yview" wp))
       (configure-scrolled *current-master* w hsb vsb)
       (with-parent w ,@body))))
       

(defun make-scrollbars-internal (sf w options continuation)
  (let* ((wp (widget-path w))
	 (hsb (and (getf options :horizontal) 
		   (make-instance 'lttk::scrollbar :orientation 'horizontal)))
	 (vsb (and (getf options :vertical) 
		   (make-instance 'lttk::scrollbar :orientation 'vertical))))
    (when hsb (configure hsb "command" (format nil "~a xview" wp)))
    (when vsb (configure vsb "command" (format nil "~a yview" wp)))
    (configure-scrolled sf w hsb vsb)
    (with-parent w (funcall continuation))))

(defmacro lttk::with-scrollbars ((widget-form &rest options) &body body)
  `(with-parent (make-instance 'lttk::frame :master *current-master* 
			       :pack '(:fill both :side top :expand true))
     (make-scrollbars-internal *current-master* ,widget-form ',options
			       #'(lambda () ,@body))))
       

(in-package :ltk)

(defclass lttk::container-mixin () ())

;;; NOTEBOOK WIDGET
(defargs lttk::notebook (lttk::widget) padding height)
(defwidget lttk::notebook (widget lttk::container-mixin) () "ttk::notebook")

;;; PANED WIDGET
(defargs lttk::paned (lttk::widget) orient)
(defwidget lttk::paned (widget lttk::container-mixin) () "ttk::paned")

(defargs lttk::treeview (lttk::widget) 
  yscrollcommand tvcolumns displaycolumns height padding selectmode show)

(defwidget lttk::treeview (widget) () "ttk::treeview")

#| incompelete   

proc sbset {sb first last} {
    if {$first <= 0 && $last >= 1} {
    	grid remove $sb
    } else {	
        grid $sb
    }
    $sb set $first $last
}


    ttk::scrollbar $w.vsb -command [list $w.t yview]
    ttk::treeview $w.t -columns [list Class] \
	-padding 4 \
	-yscrollcommand [list sbset $w.vsb] 
|#

#| broken -- initargs are for frame, not treeview widget.

(defclass lttk::scrolled-treeview (frame)
    ((treeview :accessor treeview)
     (vscroll :accessor vscroll :initarg :vscroll)
     ))

(defmethod initialize-instance :after ((st lttk::scrolled-treeview) &rest options &key padding columns)
  (remf options :padding) (remf options :columns)
  (let* ((vscroll (make-scrollbar st))
	 (treeview (apply #'make-instance 'lttk::treeview :master st :vscroll vscroll 
			  :padding padding :columns columns)))
    (setf (vscroll st) vscroll)
    (setf (treeview st) treeview)
    (grid treeview 0 0 :sticky "news")
    (grid vscroll 0 1 :sticky "ns")
    (grid-columnconfigure st 0 :weight 1)
    (grid-columnconfigure st 1 :weight 0)
    (grid-rowconfigure st 0 :weight 1)
    (grid-rowconfigure st 1 :weight 0)
    (configure vscroll "command" (concatenate 'string (widget-path (textbox st)) " yview"))
    (configure treeview "yscrollcommand" (concatenate 'string (widget-path vscroll) " set"))
    ))
|#

(defgeneric grid-remove (widget))
(defmethod grid-remove ((w widget))
  (format-wish "grid remove ~A" (widget-path w))
  w)

(defmethod scrollbar-set ((sb scrollbar) first last)
  (format-wish "~a set ~a ~a" (widget-path sb) first last))

(defun auto-hiding-scroolbar-set (sb first last)
  ;; Sucks!  shouldn't need to convert the args.  ltk-callback needs more smarts, or
  ;; need a smarter callback mechanism -- callbackval2 should be detectable to ltk-callback.
  (when (stringp first) (setq first (read-from-string first)))
  (when (stringp last) (setq last (read-from-string last)))
  (if (and (<= first 0) (>= last 1))
      (grid-remove sb)
      (grid sb 0 1))
  (scrollbar-set sb first last))


;;; this works ok
(defun lttk::make-scrolled-treeview (&rest options &key (master *current-master*) name grid 
				     &allow-other-keys)
  (remf options grid)
  (let ((st (make-instance 'frame :name (format nil "~(~a~)_frm" name) :master master :grid grid))
	treeview)
    (with-parent st
      (let* ((vscroll (make-scrollbar st)))
	(setq treeview (apply #'make-instance 'lttk::treeview :name name options))
	(grid treeview 0 0 :sticky "news")
	(grid vscroll 0 1 :sticky "ns")
	(grid-columnconfigure st 0 :weight 1)
	(grid-columnconfigure st 1 :weight 0)
	(grid-rowconfigure st 0 :weight 1)
	(grid-rowconfigure st 1 :weight 0)
	(configure vscroll "command" (concatenate 'string (widget-path treeview) " yview"))
	;;(configure treeview "yscrollcommand" (concatenate 'string (widget-path vscroll) " set"))

	(configure treeview "yscrollcommand" 
		   (format nil "callbackval2 ~a"
			   (add-callback (create-name)
					 #'(lambda (first last) 
					     (auto-hiding-scroolbar-set vscroll first last)))))
	;;(setf (vscroll st) vscroll)
	;;(setf (treeview st) treeview)
	(setf (get-prop st :vscroll) vscroll
	      (get-prop st :treeview) treeview)
	))
    treeview))


(in-package :lttk)

(shadowing-import '(ltk::format-wish-and-read-string ltk::format-wish-and-read-strings))

(defmethod insert-item ((tv lttk::treeview) parent-id index &rest options
			&key id text image values open tags)
  (declare (ignorable id text image values open tags))
  (remf options :text) (remf options :values) (remf  options :id)
  (format-wish-and-read-string 
   "~a insert ~a ~(~a~) ~@[ -id ~a~]~@[ -text {~a}~] ~@[ -values [list ~{ ~a~}]~] ~{ -~(~a~) ~(~a~)~}" 
   (widget-path tv) parent-id index id text values options))

(defmethod delete-item ((tv lttk::treeview) &rest items)
  (when items
    (format-wish "~a delete ~{~a ~}" (widget-path tv) items)) )
  
(defmethod children ((tv lttk::treeview) item &rest newchildren)
  (if newchildren
      (format-wish "~a children ~(~a~) ~{~(~a~) ~}" (widget-path tv) item newchildren)
      (format-wish-and-read-strings "~a children ~(~a~)" (widget-path tv) item)))
  
;;(format nil "foo ~c bar" #\null)

(defmethod heading ((tv lttk::treeview) column &rest options &key text image anchor command)
  (declare (ignorable text image anchor command))
  (if (eql column :tree)
      (format-wish "~a heading ~c ~{ -~(~a~) {~(~a~)}~}" 
		   (widget-path tv) #\null options)
      (format-wish "~a heading {~a} ~{ -~(~a~) {~a}~}" 
		   (widget-path tv) column options)))

(defmethod focus-item ((tv lttk::treeview) &optional item)
  (if item
      (format-wish "~a focus ~a" (widget-path tv) item)
      (format-wish-and-read-string "~a focus" (widget-path tv))))

(defmethod add-child ((w lttk::container-mixin) (child ltk::widget) &rest options)
  (format-wish "~a add ~a ~{ -~(~a~) {~(~a~)}~}" 
	       (widget-path w) (widget-path child) options ))  

(defmethod insert-child ((w lttk::container-mixin) position (child ltk::widget) &rest options)
  (format-wish "~a insert ~a ~{ -~(~a~) {~(~a~)}~}" 
	       (widget-path w) position (widget-path child) options )) 


(in-package :ltk)

(defun format-wish-and-read-symbols (format-spec &rest args)
  (tk::tcl-list-to-lisp (apply 'format-wish-and-read-string format-spec args)
			:upcase t :package *package*)) ; FIXME:  How should package be specified?

(defun format-wish-and-read-strings (format-spec &rest args)
  (tk::tcl-list-to-strings (apply 'format-wish-and-read-string format-spec args)))

(in-package :lttk)

(defun tcl-require-package (package-name)
  (ltk::format-wish "package require ~a" package-name))

(defun require-tile ()
  (tcl-require-package "tile"))

(defun theme-names ()
  (ltk::format-wish-and-read-strings "senddatastring [style theme names]"))

(defun use-theme(name)
  (ltk::format-wish "ttk::setTheme ~a" name))

;; not used.
(defun style-element-names ()
  (ltk::format-wish-and-read-symbols "senddatastring [style element names]"))

(defun style-default (style &rest params)
  (ltk::format-wish "style default ~A ~{ -~(~a~) {~a}~}" style params))

;;; legacy function.
(defun activate-tile())


#|
(theme-names)
(style-element-names)

|#



;;; Helper functions to make combinations of widgets, usually inside a new frame.

;;; Make a horizonatal array of radiobuttons.
(defun make-radiobuttons (items &key initial-value)
  (unless initial-value (setq initial-value (car (first items))))
  (with-parent (make-instance 'frame :pack '(:side left :anchor nw))
    (loop with variable = (widget-path ltk::*current-master*)
	  for (name text . keyvals) in items
	  do (make-instance 'radio-button :name name :text text :value name :variable variable 
		  :style "Toolbutton" :pack '(:side left) :tooltip (getf keyvals :documentation))
	  finally (format-wish "set ~a ~(~a~)" variable initial-value))))

;;; Make a frame containing an entry and a scale widget.
(defun make-entry-scale (&rest keyvals &key name entry-args scale-args (scale-class 'scale)
			 &allow-other-keys)
  (remf keyvals :scale-args) (remf keyvals :entry-args) (remf keyvals :scale-class)
  (with-parent (apply #'make-instance 'frame :name (ltk::concat-symbol-name name "_FRM") keyvals)
    (let ((variable (format nil "~a_var" (widget-path ltk::*current-master*))))
      (apply #'make-instance 'entry :name (ltk::concat-symbol-name name "_ENTRY")
	     :textvariable variable :pack '(:side left) 
	     entry-args)
      (apply #'make-instance scale-class :name name :variable variable 
	     :pack '(:side left :fill x :expand true) scale-args))))

#|
(lttk::with-lttk-widgets
  (with-parent (toplevel :title "make-entry-scale test")
    (with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
      (make-entry-scale :name 'es :entry-args '(:width 4) :scale-args '(:from 0 :to 100)
			:pack '(:side left :fill x :expand 1)))))

(lttk::with-lttk-widgets
  (with-parent (toplevel :title "make-entry-scale test")
    (with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
      (make-entry-scale :name 'es :entry-args '(:width 4) 
			:scale-args '(:orientation horizontal :from 0 :to 100)
			:scale-class 'ltk::scale
			:pack '(:side left :fill x :expand 1)))))

|#




(in-package :lttk)

(defun tile-test ()
  (let* ((master (make-instance 'toplevel :title "Tile Widgets Test"))
	 (mb (make-menubar master))
	 (mtheme (make-menu mb "Theme" ))
	 (b (make-instance 'button :text "a button" :master master))
	 (l (make-instance 'label :text "a label" :master master))
	 (e (make-instance 'entry :text "an entry" :master master))
	 )
    (pack (list l e b) :side :left)
    (dolist (theme (theme-names))
      (let ((theme theme))
	(make-menubutton mtheme theme (lambda ()
					(use-theme theme)))))
    ))

(defun tile-test1 ()
  (activate-tile)
  (with-ltk ()
     (let* ((parent (make-instance 'ltk::toplevel :title "Tile Widgets Test"))
	    (mb (make-menubar parent))
	    (mtheme (make-menu mb "Theme" ))
	    (b (make-instance 'button :text "a button" :parent parent))
	    (l (make-instance 'label :text "a label" :parent parent))
	    (e (make-instance 'entry :text "an entry" :parent parent))
	    )
       (pack (list l e b) :side :left)
       (dolist (theme (theme-names))
	 (let ((theme theme))
	   (make-menubutton mtheme theme (lambda ()
					   (use-theme theme)))))
       )))

(defun tile-test2 ()
  (activate-tile)
  (with-ltk ()
     (let* ((master (make-instance 'ltk::toplevel :title "Tile Widgets Test 2"))
	    (mb (make-frame master))
	    (mtheme (ltk::make-pulldownmenu-button mb "Theme" ))
	    (mf2 (make-frame master))
	    (b (make-instance 'button :text "a button" :master mf2))
	    (l (make-instance 'label :text "a label" :master mf2))
	    (e (make-instance 'entry :text "an entry" :master mf2))
	    )
       (pack (list mb mf2) :side :top)
       (pack (list mtheme l e b) :side :left)
       (dolist (theme (theme-names))
	 (let ((theme theme))
	   (make-menubutton mtheme theme (lambda ()
					   (use-theme theme)))))
       )))

(defun tile-test2b ()
  (activate-tile)
  (with-ltk ()
    (with-master (make-instance 'ltk::toplevel :title "Tile Widgets Test 2b")
      (let* ((mb (make-instance 'frame :name "frm1"))
	     (mtheme (make-instance 'pulldownmenu-button :master mb :text "Theme" :name "theme"))
	     (mf2 (make-instance 'frame :name "frm2")))
	(pack (list mb mf2) :side :top)
	(pack mtheme :side :left)
	(with-master mf2
	  (pack (list (make-instance 'label :text "a label" :name "label-a")
		      (make-instance 'entry :text "an entry" :name "entry-a")
		      (make-instance 'button :text "a button" :name "button-a")
		      )
		:side :left)
	  (dolist (theme (theme-names))
	    (let ((theme theme))	; closure
	      (make-menubutton mtheme theme (lambda ()
					      (use-theme theme)))))
	  )))))

;;; (defun tile-test3-callback (panel widget widget-path event &rest args)
;;;   (setq *tile-test3-callback-args* (list* panel widget widget-path event args))
;;;   (format t "tile-test3-callback: ~{~a ~}~%" *tile-test3-callback-args*)
;;;   ;;(setq *event-widget* widget)
;;;   (case event 
;;;     (:buttonpress 
;;;      (cond ((member (ltk::name widget) (theme-names) :test #'string=)
;;; 	    (use-theme (ltk::name widget)))))
;;;     (tk::entry_changed (format t "~a has been modified: new value is: ~a~%"
;;; 			    widget (value widget)))
     
;;;      ))


(defun tile-test3-callback (panel widget-path item-name event &rest args)
  (setq *tile-test3-callback-args* (list* panel widget-path item-name event args))
  (format t "tile-test3-callback: ~{~a ~}~%" *tile-test3-callback-args*)
  ;;(setq *event-widget* widget)
  (let ((widget (tk::widget-clidget widget-path)))
    (case event 
      (:buttonpress 
       (cond ((member (ltk::name widget) (theme-names) :test #'string=)
	      (use-theme (ltk::name widget)))))
      (tk::entry_changed (format t "~a has been modified: new value is: ~a~%"
				 widget (text widget)))
     
      )))

;;(tile-test3)
(defun tile-test3 ()
  (tk::make-widget-panel 
   (with-parent (make-instance 'ltk::toplevel :title "Tile Widgets Test 3")
     (let* ((mb (make-instance 'frame :name "frm1"))
	    (mtheme (make-instance 'pulldownmenu-button :parent mb :name "theme" :text "Theme"))
	    (mf2 (make-instance 'frame :name "frm2")))
       (pack (list mb mf2) :side :top)
       (pack mtheme :side :left)
       (with-parent mf2
	 (pack (list (make-instance 'label :name "label-a" :text "a label")
		     (make-instance 'entry :name "entry-a" :text "an entry")
		     (make-instance 'button :name "button-a" :text "a button")
		     (make-instance 'radio-button :name "radio-button-a" :text "a radio button"
				    :style "Toolbutton")
		     )
	       :side :left))
       (dolist (theme (theme-names))
	 (make-menubutton mtheme theme nil :name theme))
       ))
   'tile-test3-callback))

(defun validate-entry (&rest args)
  (format t "validate-entry ~a~%" args)
  "1")


#|
(send-wish "set class_bindtags(TEntry) [list manage_qentry show_widget_doc]")
(send-wish "set class_bindtags(TCheckbutton) [list manage_qentry show_widget_doc]")

(send-wish "set_bindingtags .w6.frm2.entry-a")
(tk::widget-clidget (nth 1 *tile-test3-callback-args*))
(tk::tcl-cmd `(,(nth 1 *tile-test3-callback-args*) configure))q
(setq tk::*events-verbose* t)
(setq tk::*events-verbose* nil)
(ltk::format-wish-and-read-string "senddatastring ${text_~a}"
			     (ltk::name (tk::widget-clidget (nth 1 *tile-test3-callback-args*))))
(ltk::text (tk::widget-clidget (nth 1 *tile-test3-callback-args*)))
(setf (ltk::text (tk::widget-clidget (nth 1 *tile-test3-callback-args*)))
      "foo bar")

(tk::widget-clidget ".w35.frm1.theme.classic")
(tk::widget-clidget ".w47.mf.f2.pc.middle")
(tk::widget-panel ".w47.mf.f2.pc.middle")
|#

(defparameter *random-callback-counter* 0)

(defun generate-random-callback-name ()
  (format nil "rcb~a" (incf *random-callback-counter*)))
  
(defun add-random-callback (callback-fn form)
  (let ((cbname (generate-random-callback-name)))
    (add-callback cbname form)
    (format nil "{~a ~a}" callback-fn cbname)))
;;:validate 'key :validatecommand (add-random-callback "callback_ret" 'validate-entry)

;;; this is ok
(defun tile-test3b ()
  (tk::make-widget-panel 
   (with-parent (make-instance 'ltk::toplevel :title "Tile Widgets Test 3b")
     (with-parent (make-instance 'frame :name 'frm1 :pack '(:side :top))
       (with-parent (make-instance 'pulldownmenu-button :name 'theme :text "Theme" 
				   :pack '(:side :left))
	 (dolist (theme (theme-names)) (make-instance 'menubutton :text theme :name theme))))

     (with-parent (make-instance 'frame :name 'frm2 :pack '(:side :left))
       (make-instance 'label :name 'label-a :text "a label" :pack '(:side :left))
       (make-instance 'entry :name 'entry-a :text "an entry" :pack '(:side :left))
       (make-instance 'button :name 'button-a :text "a button":pack '(:side :left))
       (make-instance 'radio-button :name 'radio-button-a :text "a radio button"
		      :style "Toolbutton" :pack '(:side :left))))
   'tile-test3-callback))

;;;(tile-test4)
(defun tile-test4 ()
  (tk::make-widget-panel
   (with-lttk-widgets
     (with-parent (toplevel :title "Tile Widgets Test 4")
       (with-parent (frame 'frm1 :pack '(:side :top))
	 (with-parent (pulldownmenu-button "theme" :text "Theme":pack '(:side :left))
	   (dolist (theme (theme-names)) (menubutton theme :text theme))))

       (with-parent (frame 'frm2 :pack '(:side :left))
	 (label 'label-a :text "a-label":pack '(:side :left))
	 (entry 'entry-a :text "an entry"  :pack '(:side :left))
	 (button 'button-a :text "a button" :pack '(:side :left))
	 (radio-button 'radio-button-a :text "a radio button" :style "Toolbutton" 
		       :pack '(:side :left)))))
   'tile-test3-callback))



(in-package :gui)

;;;(tile-test4b)
;;; This is ok
(defun tile-test4b ()
  (tk::make-widget-panel
   (lttk::with-lttk-widgets
     (with-parent (toplevel :title "Tile Widgets Test 4b")
       (with-parent (frame 'frm1 :pack '(:side :top))
	 (with-parent (pulldownmenu-button "theme" :text "Theme":pack '(:side :left))
	   (dolist (theme (lttk::theme-names)) (menubutton theme :text theme))))

       (with-parent (frame 'frm2 :pack '(:side :left))
	 (label 'label-a :text "a-label":pack '(:side :left))
	 (entry 'entry-a   :pack '(:side :left))
	 (button 'button-a :text "a button" :pack '(:side :left))
	 (radio-button 'radio-button-a :text "a radio button" :style "Toolbutton" 
		       :pack '(:side :left)))))
   'lttk::tile-test3-callback))

;(tile-test4c)
(defun tile-test4c ()
  (tk::make-widget-panel
   (lttk::with-lttk-widgets
     (with-parent (toplevel :title "Tile Widgets Test 4c")
       (with-parent (frame 'frm1 :pack '(:side :top :fill both :anchor nw :expand 1 ))
	 (with-parent (pulldownmenu-button "theme" :text "Theme":pack '(:side :top))
	   (dolist (theme (lttk::theme-names)) (menubutton theme :text theme)))

	 (with-parent (frame 'frm2 :pack '(:side :top))
	   (label 'label-a :text "a-label":pack '(:side :left))
	   (entry 'entry-a   :pack '(:side :left))
	   (button 'button-a :text "a button" :pack '(:side :left))
	   (radio-button 'radio-button-a :text "a radio button" :style "Toolbutton" 
			 :pack '(:side :left)))
	 (scale 'scale :from 0 :to 100 :pack '(:side :top :fill both :anchor nw :expand 1))
	 )))
   'lttk::tile-test3-callback))


(in-package :lttk)

#||

(st:load-system :ltk :initialize t :recompile t)
(st:load-system :ltk :initialize t)

(tile-test)
(tile-test1)
(tile-test3)
(tile-test3b)
(tile-test4)
(gui::tile-test4b)
(gui::make-cme-control-panel-xxx)
(tk::cvv-item-value gui::*cme-control-panel* 'gui::left)

(eq 'ltk::style 'lttk::style )

(setq *debug-tk* t)
(setq *debug-tk* nil)

ltk::*all-widgets*

(tile-test)
(tile-test2)
(tile-test2b)
(tile-test3)
(tile-test4)

(gethash  (ltk::wish-callbacks ltk::*wish*))

(load (compile-file "$FREEDIUS/lisp/ltk/ltk-tests.lisp"))
(ltk::ltktest)

(theme-names)
(tk::tcl-cmd `(style theme names))
(setq tk::*tk-verbose* t)
(tq tk::*tk-verbose* nil)

(tk::widget-clidget ".w22.mf.f2.mb.file.load-site")
(gethash ".w15.mf.f2.mb.panels.preferences-panel" (ltk::wish-callbacks ltk::*wish*))
(tk::widget-panel ".w14.frm1.theme.classic")
(tk::widget-clidget ".w14.frm1.theme.classic")
(tk::widget-panel (tk::widget-clidget ".w14.frm1.theme.classic"))
(tk::tcl-list-to-toplevel-strings "foo bar")
(tk::tcl-list-to-lisp "foo bar" :upcase t :package :ltk)

(symbol-package 'menubutton)

(setq swank::*macroexpand-printer-bindings*
      '((*print-circle* . nil)
	(*print-lines*    . nil)
	(*print-pretty* . t)
	(*print-escape* . t)
	(*print-level* . nil)
	(*print-length* . nil)))

(defun widget-class-options (class)
  (loop for (opt . rest) in (tk::tcl-list-to-strings
			     (format-wish "[~a ~a] configure" 
				    class (ltk::create-path nil (ltk::create-name)))
			       2)
	collect (subseq opt 1)))
  
(loop with set = (widget-class-options "ttk::frame")
      for cls in '("ttk::entry" "ttk::label" "ttk::button" "ttk::menubutton" 
		   "ttk::checkbutton" "ttk::radiobutton")
      do (setq set (intersection set (widget-class-options cls) :test #'string=))
	 finally (return set))

(loop with ltk::*debug-tk* = nil
      with set = '("width" "takefocus" "cursor" "style" "class")
      for cls in '("ttk::entry" "ttk::label" "ttk::labelframe" "ttk::frame"  "ttk::button" 
		   "ttk::menubutton" "ttk::checkbutton" "ttk::radiobutton" "ttk::scrollbar" )
      do (format t "(defargs l~a (lttk::widget)~%    ~{~a ~})~%~%" 
		 cls
		 (sort (set-difference (widget-class-options cls) set :test #'string=)
		       #'string<)))

(widget-class-options "ttk::paned")
;;("orient" "takefocus" "cursor" "style" "class")

||#
