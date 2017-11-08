(in-package :ltk)

#| 

This software is a version of LTK (LTK Copyright notice below)
modified by Lynn H. Quam to operate directly with the Tcl/Tk libraries
without using pipe or socket connections to wish.

The original file ltk.lisp has been split into pieces to 
separate the communication specific parts from the generic parts.

Additional modifications have been made to support compatibility with the 
:LISPTK infrastructure of FREEDIUS.

|#


#|

 This software is Copyright (c) 2003, 2004, 2005, 2006  Peter Herth <herth@peter-herth.de>
 Parts Copyright (c) 2005 Thomas F. Burdick
 Parts Copyright (c) Cadence Design Systems, GmbH

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

#|
All tk commands as of version 8.4 with support information. "-" means not
supported by purpose (look comment), "x" means supported, though some
options may not be supported. 

command      supported comment
bell                 x
bind                 x 
bindtags               modifly the tag list of a widget that describes which events it gets
bitmap               - see image
button               x
canvas               x 
checkbutton          x
clipboard            x (canvas get missing... tricky...)
colors               - constants only
console              - only on some platforms
cursors              x 
destroy              x
entry                x
event                  create and manage virtual events
focus                x focus management functions
font
frame                x
grab                  
grid                 x
image                x 
keysyms              - constants only
label                x
labelframe           x
listbox              x
loadTk               -
lower                x
menu                 x
menubutton           x
message              x 
option               -
options              - only helpfile
pack                 x
panedwindow          x
photo                x 
place                x geometry manager using coordinates
radiobutton          x
raise                x
scale                x 
scrollbar            x
selection
send
spinbox              x
text                 x
tk
tk_bisque            - only for tk backwards compatibility
tk_chooseColor
tk_chooseDirectory
tk_dialog
tk_focusFollowsMouse 
tk_focusNext
tk_focusPrev
tk_getOpenFile       x
tk_getSaveFile       x
tk_menuSetFocus      -
tk_messageBox        x
tk_optionMenu
tk_popup
tk_setPalette        -
tk_textCopy
tk_textCut
tk_textPaste
tkerror              -
tkvars               -
tkwait               
toplevel             x
winfo                x
wm                   x 


support of all config args as keywords to make-instance:

bitmap               
button               x
canvas               x 
checkbutton          x
entry                x
frame                x
image                 
label                x 
labelframe           x 
listbox              x 
menu                  
menubutton            
message                
panedwindow          x
photo                  
radiobutton          x
scale                x
scrollbar            x 
spinbox              x 
text                 x
toplevel             x

|#

;;; global connection information

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf
   (documentation 'make-ltk-connection 'function)
   "Create a new LTK-CONNECTION object.  This represents a connection to a
    specific wish.  You can maintain connections to several distinct wish
    processes by binding *WISH* to the one you desire to communicate with, and
    using LTK functions within that dynamic scope."))

(define-condition ltk-error (simple-error) ())
(defun ltk-error (format &rest args)
  (error 'ltk-error :format-control format :format-arguments args))

(defvar *wish* (make-ltk-connection)
  "The current connection to an inferior wish.")

(defvar *wish-connections* ()
  "Connections pushed aside by invoking the NEW-WISH restart in START-WISH.")

;;; verbosity of debug messages, if true, then all communication
;;; with tk is echoed to stdout
(defvar *debug-tk* nil)

(defvar *trace-tk* nil)

(defvar *init-wish-hook* nil)

(defun dbg (fmt &rest args)
  (when *debug-tk*
    (apply #'format t fmt args)
    (finish-output)))

;;; sanitizing strings: lisp -> tcl (format (wish-stream *wish*) "{~a}" string)
;;; in string escaped : {} mit \{ bzw \}  und \ mit \\

(defun replace-char (txt char with)
  (let ((pos (search char txt)))
    (loop
       while pos
       do
         (progn
           ;;(dbg "txt: ~a -> " txt)
           (setf txt (concatenate 'string (subseq txt 0 pos) with (subseq txt (1+ pos))))
           ;;(dbg " ~a~&" txt)
           (setf pos (search char txt :start2 (+ pos (length with)))))))
  txt)


(defun tkescape (txt)
  (setf txt (format nil "~a" txt))
  (replace-char (replace-char (replace-char (replace-char (replace-char txt "\\" "\\\\") "$" "\\$") "[" "\\[") "]" "\\]") "\"" "\\\""))


;;; table used for callback every callback consists of a name of a widget and
;;; a function to call

(defmethod add-callback (sym fun)
  "create a callback sym is the name to use for storage, fun is the function to call"
  (when fun
    (when *debug-tk*
      (format t "add-callback (~A ~A)~%" sym fun))
    (setf (gethash sym (wish-callbacks *wish*)) fun)
    sym))

(defun remove-callback (sym)
  (when *debug-tk*
    (format t "remove-callback (~A)~%" sym))
  (setf (gethash sym (wish-callbacks *wish*)) nil))

(defun callback (sym arg)
  "perform the call of the function associated with sym and the args arg"
  (let ((fun (gethash sym (wish-callbacks *wish*))))
    (when fun
      (apply fun arg))))

(defun after (time fun)
 "after <time> msec call function <fun>, returns the after event id,
which can be passed to AFTER-CANCEL"
 (let ((name (format nil "after~a" (incf (wish-after-counter *wish*)))))
   (let ((id (format-wish-and-read "senddatastring [after ~a {callback ~A}]" time name))
         (blah (wish-after-ids *wish*)))
     (setf (gethash id blah) name)
     (add-callback name
                   (lambda ()
                     (funcall fun)
                     (remhash id blah)
                     (remove-callback name)))
     id)))

(defun after-idle (fun)
 "call fun when tk becomes idle, returns the after event id, which
can be passed to AFTER-CANCEL"
 (let ((name (format nil "afteridle~a" (incf (wish-after-counter *wish*)))))
   (let ((id (format-wish-and-read "senddatastring [after idle {callback ~A}]" name))
         (blah (wish-after-ids *wish*)))         
     (add-callback name
                   (lambda ()
                     (funcall fun)
                     (remhash id blah)
                     (remove-callback name)))
     id)))

(defun after-cancel (id)
 "cancels a call scheduled with AFTER or AFTER-IDLE by its id"
 (format-wish "after cancel ~a" id)
 (let ((blah (wish-after-ids *wish*)))
   (remove-callback (gethash id blah))
   (remhash id blah)))

;; tool functions used by the objects

(defun get-counter()
  "incremental counter to create unique numbers"
  (incf (wish-counter *wish*)))

(defun create-name ()
  "create unique widget name, append unique number to 'w'"
  (format nil "w~A" (get-counter)))


(defun create-path (master name)
  "create pathname from master widget <master> and widget name <name>"
  (let ((master-path (if master
                         (widget-path master)
                         "")))
    (format nil "~A.~A" master-path name)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
;;; widget class built helper functions

;;(defparameter *generate-accessors* nil)
  
  (defun iarg-name (arg) (nth 0 arg))
  (defun iarg-key (arg) (nth 1 arg))
  (defun iarg-format (arg) (nth 2 arg))
  (defun iarg-code (arg) (nth 3 arg))
  (defun iarg-comment (arg) (nth 4 arg))

  (defparameter *initargs*
    '(
      (button.background Button.background "~@[ -Button.background ~(~a~)~]" button.background "")
      (Button.cursor Button.cursor "~@[ -Button.cursor ~(~a~)~]" Button.cursor "")
      (Button.relief Button.relief "~@[ -Button.relief ~(~a~)~]" Button.relief "")
      
      (activebackground activebackground "~@[ -activebackground ~(~a~)~]" activebackground
       "background of the active area")

      (activeborderwidth activeborderwidth "~@[ -activeborderwidth ~(~a~)~]" activeborderwidth
       "the border width for active widgets (when the mouse cursor is over the widget)")

      (activeforeground activeforeground "~@[ -activeforeground ~(~a~)~]" activeforeground
       "foreground color for active widgets (when the mouse cursor is over the widget)")

      (activerelief activerelief "~@[ -activerelief ~(~a~)~]" activerelief
       "the border relief for active widgets (when the mouse cursor is over the widget)")
      
      (activestyle activestyle "~@[ -activestyle ~(~a~)~]" activestyle
       "the style for drawing the active part (dotbox, none, underline (default))")
      
      (anchor anchor "~@[ -anchor ~(~a~)~]" anchor
       "specify the alignment of text/image drawn on the widget, one of (:n :w :s :e :nw :sw :se :ne) with :nw designating the top left corner")
      
      (aspect aspect "~@[ -aspect ~(~a~)~]" aspect
       "Aspect ratio for the wrapping of the text. 100 means that the text is redered as wide as, tall, 200 twice as wide.")
      (autoseparators autoseparators "~:[~; -autoseparators 1~]" autoseparators
       "when t, separators are added automatically to the undo stack")
      (background background "~@[ -background ~(~a~)~]" background
       "background color of the widget")
      (bigincrement bigincrement "~@[ -bigincrement ~(~a~)~]" bigincrement
       "size of the big step increment")
      (bitmap bitmap "~@[ -bitmap ~(~a~)~]" bitmap
       "the bitmap to display on the widget, the display is affected by the options 'anchor' and 'justify'")

      (borderwidth borderwidth "~@[ -borderwidth ~(~a~)~]" borderwidth
       "width of the border around the widget in pixels")

      (class class "~@[ -class ~(~a~)~]" class
       "the class of the widget, used for lookup in the option database. This option cannot be changed after the widget creation.")

      (closeenough closeenough "~@[ -closeenough ~(~a~)~]" closeenough
       "dermines when the mouse coursor is considered to be inside a shape, the default is 1.0")

      (colormap colormap "~@[ -colormap ~(~a~)~]" colormap
       "The colormap to use for the widget.")

      (command command "~@[ -command {callback ~a}~]" (add-callback widget command)
       "function to call when the action of the widget is executed")
      
      (cbcommand command "~@[ -command {callbackval ~{~a $~a~}}~]" (let ((sym (add-callback widget command)))
								      (and sym (list sym sym)))
       "function to call when the action of the widget is executed")
      (scale-command command "~@[ -command {callbackval ~a}~]" (add-callback widget command)
       "function to call when the action of the widget is executed")

      (spinbox-command command "~@[ -command {callbackstring ~a %s}~]" (add-callback widget command) "")
      (command-radio-button command "~@[ -command {callbackval ~{~a $~a~}}~]" (let ((sym (add-callback widget command)))
										 (and sym (list sym (radio-button-variable widget))))
       "function to call when the action of the widget is executed")
      
      (command-scrollbar command "~@[ -command {callback ~a}~]" (add-callback widget command)
			 "")
      
      (compound compound "~@[ -compound ~(~a~)~]" compound
       "")
      
      (confine confine "~:[~; -confine 1~]" confine
       "if t (default) allowed values for view are confined to the scrollregion")
      
      (container container "~:[~; -container 1~]" container
       "if t, then the widget will be used as a container for other widgets.")
      
      (cursor cursor "~@[ -cursor ~(~a~)~]" cursor
       "mouse pointer to display on the widget (valid values are listed in *cursors*)")
      
      (default default "~@[ -default ~(~a~)~]" default
       "")

      (digits digits "~@[ -digits ~(~a~)~]" digits
       "number of digits to use when converting the value to a string.")
      
      (direction direction "~@[ -direction ~(~a~)~]" direction "")
      (disabledbackground disabledbackground "~@[ -disabledbackground ~(~a~)~]" disabledbackground "")
      (disabledforeground disabledforeground "~@[ -disabledforeground ~(~a~)~]" disabledforeground "")
      (elementborderwidth elementborderwidth "~@[ -elementborderwidth ~(~a~)~]" elementborderwidth "")
      (exportselection exportselection "~@[ -exportselection ~(~a~)~]" exportselection "")
      (font font "~@[ -font {~a}~]" font "font to use to display text on the widget")
      (foreground foreground "~@[ -foreground ~(~a~)~]" foreground "foreground color of the widget")
      (format format "~@[ -format ~(~a~)~]" format "")
      (from from "~@[ -from ~(~a~)~]" from "")
      (handlepad handlepad "~@[ -handlepad ~(~a~)~]" handlepad "")
      (handlesize handlesize "~@[ -handlesize ~(~a~)~]" handlesize "")
      (height height "~@[ -height ~(~a~)~]" height "height of the widget")
      (highlightbackground highlightbackground "~@[ -highlightbackground ~(~a~)~]" highlightbackground "")
      (highlightcolor highlightcolor "~@[ -highlightcolor ~(~a~)~]" highlightcolor "")
      (highlightthickness highlightthickness "~@[ -highlightthickness ~(~a~)~]" highlightthickness "")
      (image image "~@[ -image ~(~a~)~]" (and image (name image))
       "the image to display on the widget, the display is affected by the options 'anchor' and 'justify'")
      (increment increment "~@[ -increment ~(~a~)~]" increment "size of the increment of the widget")
      (indicatorOn indicatorOn "~@[ -indicatorOn ~(~a~)~]" indicatorOn "")
      (insertbackground insertbackground "~@[ -insertbackground ~(~a~)~]" insertbackground "")
      (insertborderWidth insertborderWidth "~@[ -insertborderWidth ~(~a~)~]" insertborderWidth "")
      (insertofftime insertofftime "~@[ -insertofftime ~(~a~)~]" insertofftime "")
      (insertontime insertontime "~@[ -insertontime ~(~a~)~]" insertontime "")
      (insertwidth insertwidth "~@[ -insertwidth ~(~a~)~]" insertwidth "")
      (invalidcommand invalidcommand "~@[ -invalidcommand ~(~a~)~]" invalidcommand "")
      (jump jump "~@[ -jump ~(~a~)~]" jump "")
      (justify justify "~@[ -justify ~(~a~)~]" justify "justification of the text on the widget")
      (label label "~@[ -label ~(~a~)~]" label "text to display on the widget")
      (labelanchor labelanchor "~@[ -labelanchor ~(~a~)~]" labelanchor "")
      (labelwidget labelwidget "~@[ -labelwidget ~(~a~)~]" labelwidget "")
      (length length "~@[ -length ~(~a~)~]" length "")
      (listvariable listvariable "~@[ -listvariable ~(~a~)~]" listvariable "")
      (maxundo maxundo "~@[ -maxundo ~(~a~)~]" maxundo "")
      (menu menu "~@[ -menu ~(~a~)~]" menu "")
      (offrelief offrelief "~@[ -offrelief ~(~a~)~]" offrelief "")
      (offvalue offvalue "~@[ -offvalue ~(~a~)~]" offvalue "")
      (offset offset "~@[ -offset ~(~a~)~]" offset "")
      (onvalue onvalue "~@[ -onvalue ~(~a~)~]" onvalue "")
      (opaqueresize opaqueresize "~@[ -opaqueresize ~(~a~)~]" opaqueresize "")
      (orient orientation "~@[ -orient ~(~a~)~]" orientation "orientation of the widget (horizontal, vertical)")
      (overrelief overrelief "~@[ -overrelief ~(~a~)~]" overrelief "relief of the border, when the mouse is over the widget")
      (padx padx "~@[ -padx ~(~a~)~]" padx "padding around text displayed on the widget")
      (pady pady "~@[ -pady ~(~a~)~]" pady "padding around text displayed on the widget")
      (postcommand postcommand "~@[ -postcommand ~(~a~)~]" postcommand "")
      (readonlybackground readonlybackground "~@[ -readonlybackground ~(~a~)~]" readonlybackground "")
      (relief relief "~@[ -relief ~(~a~)~]" relief "relief of the widgets border (raised, sunken, ridge, groove)")
      (repeatdelay repeatdelay "~@[ -repeatdelay ~(~a~)~]" repeatdelay "")
      (repeatinterval repeatinterval "~@[ -repeatinterval ~(~a~)~]" repeatinterval "")
      (resolution resolution "~@[ -resolution ~(~a~)~]" resolution "")
      (sashcursor sashcursor "~@[ -sashcursor ~(~a~)~]" sashcursor "")
      (sashpad sashpad "~@[ -sashpad ~(~a~)~]" sashpad "")
      (sashrelief sashrelief "~@[ -sashrelief ~(~a~)~]" sashrelief "")
      (sashwidth sashwidth "~@[ -sashwidth ~(~a~)~]" sashwidth "")
      (screen screen "~@[ -screen ~(~a~)~]" screen "screen on which the toplevel is to be shown")
      (scrollregion scrollregion "~@[ -scrollregion ~(~a~)~]" scrollregion "region in which the canvas should be scolled")
      (selectbackground selectbackground "~@[ -selectbackground ~(~a~)~]" selectbackground "")
      (selectborderwidth selectborderwidth "~@[ -selectborderwidth ~(~a~)~]" selectborderwidth "")
      (selectcolor selectcolor "~@[ -selectcolor ~(~a~)~]" selectcolor "")
      (selectforeground selectforeground "~@[ -selectforeground ~(~a~)~]" selectforeground "")
      (selectimage selectimage "~@[ -selectimage ~(~a~)~]" selectimage "")
      (selectmode selectmode "~@[ -selectmode ~(~a~)~]" selectmode "")
      (setgrid setgrid "~@[ -setgrid ~(~a~)~]" setgrid "")
      (show show "~@[ -show ~(~a~)~]" show "")
      (showhandle showhandle "~@[ -showhandle ~(~a~)~]" showhandle "")
      (showvalue showvalue "~@[ -showvalue ~(~a~)~]" showvalue "")
      (sliderlength sliderlength "~@[ -sliderlength ~(~a~)~]" sliderlength "")
      (sliderrelief sliderrelief "~@[ -sliderrelief ~(~a~)~]" sliderrelief "")
      (spacing1 spacing1 "~@[ -spacing1 ~(~a~)~]" spacing1 "")
      (spacing2 spacing2 "~@[ -spacing2 ~(~a~)~]" spacing2 "")
      (spacing3 spacing3 "~@[ -spacing3 ~(~a~)~]" spacing3 "")
      (state state "~@[ -state ~(~a~)~]" state "")
      (tabs tabs "~@[ -tabs ~(~a~)~]" tabs "")
      (takefocus takefocus "~@[ -takefocus ~(~a~)~]" takefocus "if true, the widget can take the focus")
      (tearoff tearoff "~@[ -tearoff ~(~a~)~]" tearoff "if true, the menu can be torn off")
      (tearoffcommand tearoffcommand "~@[ -tearoffcommand ~(~a~)~]" tearoffcommand "")
      (text text "~@[ -text \"~a\"~]" (tkescape text) "")
      ;;(textvariable textvariable "~@[ -textvariable ~a_text~]" (and textvariable (make-widget-path widget)) "")
      (textvariable textvariable "~@[ -textvariable ~a~]" textvariable "")
      #+old ;; original ltk version
      (textvariable text "~@[ -textvariable text_~a~]" 
       (progn
	 (when text
	   (format-wish "set text_~a \"~a\"" (name widget) (tkescape text)))
	 (name widget)) "")
      (tickinterval tickinterval "~@[ -tickinterval ~(~a~)~]" tickinterval "")
      (title title "~@[ -title ~(~a~)~]" title "")
      (to to "~@[ -to ~(~a~)~]" to "")
      (troughcolor troughcolor "~@[ -troughcolor ~(~a~)~]" troughcolor "")
      (type type "~@[ -type ~(~a~)~]" type "")
      (underline underline "~@[ -underline ~(~a~)~]" underline "")
      (undo undo "~@[ -undo ~(~a~)~]" undo "")
      (use use "~@[ -use ~(~a~)~]" use "")
      (validate validate "~@[ -validate ~(~a~)~]" validate "")
      (validatecommand validatecommand "~@[ -validatecommand ~(~a~)~]" validatecommand "")
      (value value "~@[ -value ~(~a~)~]" value "")
      (value-radio-button nil "~@[ -value ~(~a~)~]" (radio-button-value widget)
       "value for the radio button group to take, when the button is selected")
      (values values "~@[ -values ~(~a~)~]" values "")
      (variable variable "~@[ -variable ~(~a~)~]" variable "name of the variable associated with the widget")
      (variable-radio-button nil "~@[ -variable ~(~a~)~]" (radio-button-variable widget)
       "name of the radio button group the button shall belong to as a string")
      (visual visual "~@[ -visual ~(~a~)~]" visual "")
      (width width "~@[ -width ~(~a~)~]" width "width of the widget")
      (wrap wrap "~@[ -wrap ~(~a~)~]" wrap "")
      (wraplength wraplength "~@[ -wraplength ~(~a~)~]" wraplength "")
      (xscrollcommand xscrollcommand "~@[ -xscrollcommand ~(~a~)~]" xscrollcommand "")
      (xscrollincrement xscrollincrement "~@[ -xscrollincrement ~(~a~)~]" xscrollincrement "")
      (yscrollcommand yscrollcommand "~@[ -yscrollcommand ~(~a~)~]" yscrollcommand "")
      (yscrollincrement yscrollincrement "~@[ -yscrollincrement ~(~a~)~]" yscrollincrement "")
      ))
  

  (defparameter *class-args*
    '())
  
  (defun build-args (class parents defs)
    (declare (ignore class))
    ;;(format t  "class ~s parents ~s defs ~s~%" class parents defs) (finish-output)
    (let ((args nil))
      (dolist (p parents)
	(let ((arglist (rest (assoc p *class-args*))))
	  ;;(format t "parent: ~s arglist: ~s~%" p arglist) (finish-output)
	  (dolist (arg arglist)
	    (unless (member arg args)
	      (setf args (append args (list arg)))))))
      (loop 
         while defs
         do
           (let ((arg (pop defs)))
             (cond
               ((eq arg :inherit)	 
                (let* ((inheritedclass (pop defs))
                       (arglist (rest (assoc inheritedclass *class-args*))))
                  (dolist (arg arglist)
                    (unless (member arg args)
                      (setf args (append args (list arg)))
                      ))))
               ((eq arg :delete)
                (setf args (delete (pop defs) args)))	    
               (t
                (setf args (append args (list arg)))))))
      ;;(format t "class: ~a args: ~a~&" class args) (finish-output)
      args
      ))
  )

(defmacro defargs (class parents &rest defs)
  (let ((args (build-args class parents defs)))
    (setf *class-args* (append *class-args* (list (cons class args))))
    `(setf *class-args* (append *class-args* (list '(,class ,@args))))))

;;; Modified version that replaces any preexisting defarg entries rather than appending.
;;#+never
(defmacro defargs (class parents &rest defs)
  (let ((args (build-args class parents defs)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((entry (assoc ',class *class-args*)))
	 (if entry
	     (setf (cdr entry) ',args)
	     (setf *class-args* (append *class-args* (list '(,class ,@args)))))
	 ',class))))

(defargs widget () 
  relief cursor borderwidth background)

;(defargs button (widget) anchor)
;(defargs text (widget button) :delete anchor color)

(defargs button (widget) 
  activebackground activeforeground anchor bitmap command compound default disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image justify overrelief padx pady repeatdelay repeatinterval state takefocus textvariable underline width wraplength)

(defargs canvas ()
  background borderwidth closeenough confine cursor height highlightbackground highlightcolor highlightthickness insertbackground insertborderwidth insertofftime insertontime insertwidth offset relief scrollregion selectbackground selectborderwidth selectforeground state takefocus width xscrollcommand xscrollincrement yscrollcommand yscrollincrement)

(defargs check-button ()
  activebackground activeforeground anchor background bitmap borderwidth cbcommand compound cursor disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image indicatorOn justify offrelief offvalue onvalue overrelief padx pady relief selectcolor selectimage state takefocus textvariable underline variable width wraplength)

(defargs entry () background borderwidth cursor disabledbackground disabledforeground exportselection font foreground highlightbackground highlightcolor highlightthickness insertbackground insertborderwidth insertofftime insertontime insertwidth invalidcommand justify readonlybackground relief selectbackground selectborderwidth selectforeground show state takefocus textvariable validate validatecommand width xscrollcommand )

(defargs frame ()
  borderwidth class relief background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

(defargs label ()
  activebackground activeforeground anchor background bitmap borderwidth compound cursor disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image justify padx pady relief state takefocus textvariable underline width wraplength )

(defargs labelframe ()
  borderwidth class font foreground labelanchor labelwidget relief text background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

(defargs listbox ()
  activestyle background borderwidth cursor disabledforeground exportselection font foreground height highlightbackground highlightcolor highlightthickness relief selectbackground selectborderwidth selectforeground selectmode setgrid state takefocus width xscrollcommand yscrollcommand listvariable)

(defargs menu ()
  activebackground activeborderwidth activeforeground background borderwidth cursor disabledforeground font foreground postcommand relief selectcolor takefocus tearoff tearoffcommand title type)

(defargs menubutton ()
  activebackground activeforeground anchor background bitmap borderwidth cursor direction disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image indicatorOn justify menu padx pady relief compound state takefocus textvariable underline width wraplength)

(defargs message ()
  anchor aspect background borderwidth cursor font foreground highlightbackground highlightcolor highlightthickness justify padx pady relief takefocus textvariable width)

(defargs paned-window ()
  background borderwidth cursor handlepad handlesize height opaqueresize orient relief sashcursor sashpad sashrelief sashwidth showhandle width)

(defargs radio-button ()
  activebackground activeforeground anchor background bitmap borderwidth command-radio-button compound cursor disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image indicatorOn justify offrelief overrelief padx pady relief selectcolor selectimage state takefocus textvariable underline value-radio-button variable-radio-button width wraplength)

#+never
(defargs scale ()
  activebackground background bigincrement borderwidth cbcommand cursor digits font foreground from highlightbackground highlightcolor highlightthickness label length orient relief repeatdelay repeatinterval resolution showvalue sliderlength sliderrelief state takefocus tickinterval to troughcolor variable width)

(defargs scale ()
  activebackground background bigincrement borderwidth scale-command cursor digits font foreground from highlightbackground highlightcolor highlightthickness label length orient relief repeatdelay repeatinterval resolution showvalue sliderlength sliderrelief state takefocus tickinterval to troughcolor variable width)

(defargs scrollbar ()
  activebackground activerelief background borderwidth command-scrollbar cursor elementborderwidth highlightbackground highlightcolor highlightthickness jump orient relief repeatdelay repeatinterval takefocus troughcolor width)

(defargs spinbox ()
  activebackground background borderwidth Button.background Button.cursor Button.relief spinbox-command cursor disabledbackground disabledforeground exportselection font foreground format from highlightbackground highlightcolor highlightthickness increment insertbackground insertborderwidth insertofftime insertontime insertwidth invalidcommand justify relief readonlybackground repeatdelay repeatinterval selectbackground selectborderwidth selectforeground state takefocus textvariable to validate validatecommand values width wrap xscrollcommand)

(defargs text ()
  autoseparators  background borderwidth cursor exportselection font foreground height highlightbackground highlightcolor highlightthickness insertbackground insertborderwidth insertofftime insertontime insertwidth maxundo padx  pady relief selectbackground selectborderwidth selectforeground setgrid spacing1 spacing2 spacing3 state tabs takefocus undo width wrap xscrollcommand yscrollcommand)

(defargs toplevel ()
  borderwidth class menu relief screen use background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

(defmacro defwidget (class parents slots cmd &rest code)
  (let ((args (sort (copy-list (rest (assoc class *class-args*)))
		    (lambda (x y)
		      (string< (symbol-name x) (symbol-name y))))))
    (let ((cmdstring (format nil "~~a ~~~~A "))
	  (codelist nil)
	  (keylist nil)
	  (accessors nil))
      (dolist (arg args)
	(let ((entry (assoc arg *initargs*)))
	  (cond
            (entry 
             (setf cmdstring (concatenate 'string cmdstring (third entry)))
             (when (iarg-key entry)
               (setf keylist (append keylist (list (iarg-key entry)))))
             (setf codelist (append codelist (list (iarg-code entry))))
             #+:generate-accessors
             (when (and (iarg-key entry)
                        (not (equal (iarg-key entry) 'variable))
                        (not (equal (iarg-key entry) 'class))
                        (not (equal (iarg-key entry) 'length))
                        (not (equal (iarg-key entry) 'values))
                        (not (equal (iarg-key entry) 'format))
                        (not (equal (iarg-key entry) 'scrollregion)))
               (push
                `(defmethod (setf ,(iarg-key entry)) (value (widget ,class))
                   (format-wish ,(format nil "~~a configure ~a" (third entry)) (widget-path widget) value)
		   )	   
                accessors)
               (push
                `(defmethod ,(iarg-key entry) ((widget ,class))
                   (format-wish-and-read ,(format nil "senddata \"[~~a cget -~(~a~)]\"" (iarg-key entry)) (widget-path widget)))
                accessors))
             )
            (t 
             (setf cmdstring (concatenate 'string cmdstring (format nil "~~@[ -~(~a~) ~~(~~A~~)~~]" arg)))
             (setf keylist (append keylist (list arg)))
             (setf codelist (append codelist (list arg)))
	  ))))
      (push `(widget-class-name :accessor widget-class-name :initform ,cmd :allocation :class) slots)
      `(progn
	 (defclass ,class (,@parents)
	   ,slots)
	 (defmethod initialize-instance :after ((widget ,class) &key ,@keylist)
           ;;(format-wish ,cmdstring (widget-class-name widget) (widget-path widget) ,@codelist)
	   ;;(format t "setting initarg for ~a~%" (quote ,class)) (finish-output)
	   (setf (init-command widget)
		 (format nil ,cmdstring (widget-class-name widget) ,@codelist))
	   ,@code)
	 ,@accessors
	 ))))

;;; the library implementation 

(defvar *cursors*
  (list
   "X_cursor" "arrow" "based_arrow_down" "based_arrow_up" "boat" "bogosity"
   "bottom_left_corner" "bottom_right_corner" "bottom_side" "bottom_tee"
   "box_spiral" "center_ptr" "circle" "clock" "coffee_mug" "cross"
   "cross_reverse" "crosshair" "diamond_cross" "dot" "dotbox" "double_arrow"
   "draft_large" "draft_small" "draped_box" "exchange" "fleur" "gobbler"
   "gumby" "hand1" "hand2" "heart" "icon" "iron_cross" "left_ptr" "left_side"
   "left_tee" "leftbutton" "ll_angle" "lr_angle" "man" "middlebutton" "mouse"
   "pencil" "pirate" "plus" "question_arrow" "right_ptr" "right_side"
   "right_tee" "rightbutton" "rtl_logo" "sailboat" "sb_down_arrow"
   "sb_h_double_arrow" "sb_left_arrow" "sb_right_arrow" "sb_up_arrow"
   "sb_v_double_arrow" "shuttle" "sizing" "spider" "spraycan" "star"
   "target" "tcross" "top_left_arrow" "top_left_corner" "top_right_corner"
   "top_side" "top_tee" "trek" "ul_angle" "umbrella" "ur_angle" "watch" "xterm"))

(defun bell ()
  (send-wish (format nil "bell")))

(defun destroy (widget)
  (send-wish (format nil "destroy ~a" (widget-path widget))))

(defun clipboard-clear ()
  (send-wish "clipboard clear"))

(defun clipboard-get ()
  (format-wish-and-read "senddatastring [clipboard get]"))

(defun clipboard-append (txt)
  (format-wish "clipboard append {~a}" txt))

;; basic tk object
(defclass tkobject ()
  ((name :accessor name :initarg :name :initform nil)
   )
  (:documentation "Base class for every Tk object"))


(defvar *current-master* nil)

;; basic class for all widgets 
(defclass widget (tkobject)
  ((master :accessor master :initarg :master :initform *current-master*) ;; parent widget or nil
   (widget-path :initarg :path :initform nil :accessor %widget-path)         ;; pathname to refer to the widget
   (init-command :accessor init-command :initform nil :initarg :init-command)
   )
  (:documentation "Base class for all widget types"))

(defmethod print-object ((w widget) stream)
  (print-unreadable-object (w stream :type t :identity t)
    (prin1 (%widget-path w) stream)))
    
;;; This is the original ltk version -- works fine, except user supplied names must be unique.
(defmethod callback-name ((w widget))
  (name w))

;;; callback-name defines the hashtable key used for callback lookup.
;;#+never
(defmethod callback-name ((w widget))
  (make-widget-path w))

(defmethod add-callback ((w widget) fun)
  (add-callback (callback-name w) fun))

(defparameter *all-widgets* nil)

;; creating of the tk widget after creating the clos object
(defmethod initialize-instance :after ((w widget) &key parent)
  (when parent (setf (master w) parent))
  (push w *all-widgets*)
  (let ((name (name w)))
    (cond ((and name (symbolp name)) 
	   (setf (name w) (string-downcase (symbol-name name))))
	  ((null name)
	   (setf (name w) (create-name))))))

;; around - initializer
(defmethod initialize-instance :around ((w widget) &key pack place grid)
  (call-next-method)
  ;; pack widget if parameter has been supplied
  (when pack
    (apply #'pack w pack))
  (when place
    (apply #'place w place))
  (when grid
    (apply #'grid w grid)))


(defgeneric widget-path (widget))
(defmethod widget-path ((w (eql nil))) nil)

(defun make-widget-path (widget)
  (create-path (master widget) (name widget)))

;;; Be careful calling this. If (%widget-path widget) is non-NIL, CREATE doesn't do anything.
(defmethod widget-path ((widget widget))
  "retrieve the slot value widget-path, if not given, create it"
  (or (%widget-path widget)
      (prog1
	  (setf (slot-value widget 'widget-path)
		(create-path (master widget) (name widget)))
	;; This is a weird place to do the widget creation.
	(create widget)
	)))

(defgeneric create (w))

(defmethod create ((widget widget))
  ;(unless (init-command widget) (break))
  (when (init-command widget)
    ;;(format t "creating: ~a~%" (init-command widget)) (finish-output)
    (format-wish (init-command widget) (widget-path widget))))

(defgeneric (setf command) (value widget))
(defgeneric command (widget))

(defmethod command ((widget widget))
  (gethash (name widget) (wish-callbacks *wish*)))

(defgeneric lower (widget &optional other))
(defmethod lower ((widget widget) &optional other)
  (send-wish (format nil "lower ~a~@[ ~a~]" (widget-path widget) (and other (widget-path other)))))

(defgeneric raise (widget &optional above))
(defmethod raise ((widget widget) &optional above)
  (send-wish (format nil "raise ~a~@[ ~a~]" (widget-path widget) (and above (widget-path above)))))

(defstruct event
  x
  y
  keycode
  char
  width
  height
  root-x
  root-y
  mouse-button
  )

(defun construct-tk-event (properties)
  "create an event structure from a list of values as read from tk"
  (make-event
   :x (first properties)
   :y (second properties)
   :keycode (third properties)
   :char (fourth properties)
   :width (fifth properties)
   :height (sixth properties)
   :root-x (seventh properties)
   :root-y (eighth properties)
   :mouse-button (ninth properties)
   ))

(defgeneric bind (w event fun &key append exclusive))
(defmethod bind ((w widget) event fun &key append exclusive)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    ;;(format-wish "bind  ~a ~a {sendevent ~A %x %y %k %K %w %h %X %Y}" (widget-path w) event name)
    (format-wish "bind  ~a ~a {~:[~;+~]sendevent ~A %x %y %k %K %w %h %X %Y %b ~:[~;;break~]}" 
		 (widget-path w) event append name exclusive)
    w))

(defmethod bind (s event fun &key append exclusive)
  "bind fun to event within context indicated by string ie. 'all' or 'Button'"
  (let ((name (create-name)))
    (add-callback name fun)
    ;;(format-wish "bind  ~a ~a {sendevent ~A %x %y %k %K %w %h %X %Y}" s event name)
    (format-wish "bind  ~a ~a {~:[~;+~]sendevent ~A %x %y %k %K %w %h %X %Y %b ~:[~;;break~]}" 
		 s event append name exclusive)))

(defvar *tk* (make-instance 'widget :name "." :path ".")
  "dummy widget to access the tk root object")

;;; generic functions

(defgeneric canvas (w))

(defgeneric value (widget)
  (:documentation "reads the value of the variable associated with the widget"))

(defclass tkvariable ()
  ())

;;; original version
(defmethod variable-name ((v tkvariable))
  (name v))

(defmethod variable-name ((v tkvariable))
  (format nil "~a_var" (widget-path v)))

;;; FIXME:  (name v) should be (variable-name v) ?
;;; LHQ: Should this really be an :around method?
;;; It isn't clear that we shoulkd automatically set the variable name.
#+never
(defmethod initialize-instance :around ((v tkvariable) &key)
  (call-next-method)
  (when (master v)
    (format-wish "~a configure -variable ~a" (widget-path v) (variable-name v))))

#+never
(defmethod initialize-instance :around ((v tkvariable) &key)
  (call-next-method))


(defmethod value ((v tkvariable))
  (format-wish-and-read-string "senddata $~a" (variable-name v)))

(defgeneric (setf value) (widget val))
(defmethod (setf value) (val (v tkvariable))
  (format-wish "set ~a {~a}" (variable-name v) val)
  val)

(defclass tktextvariable ()
  ())

(defgeneric text (widget)
  (:documentation "reads the value of the textvariable associated with the widget")
  )

#+never 
(defmethod initialize-instance :around ((v tktextvariable) &key)
  (call-next-method)
  ;;(format-wish "~a configure -textvariable text_~a" (widget-path v) (name v))
  )

(defmethod text ((v tktextvariable))
  (format-wish-and-read-string "senddatastring ${text_~a}" (name v))
  )

(defgeneric (setf text) (val variable))

(defmethod (setf text) (val (v tktextvariable))
  (format-wish "set text_~a \"~a\"" (name v) (tkescape val))
  val)

;;; window menu bar

(defclass menubar(widget)
  ())

(defun make-menubar(&optional (master nil))
 (make-instance 'menubar :master master :name "menubar"))

;(defmethod create ((mb menubar))
(defmethod initialize-instance :after ((mb menubar) &key)
  (format-wish "menu ~a -tearoff 0 -type menubar" (widget-path mb))
  (format-wish "~a configure -menu ~a" (if (master mb)
                                           (widget-path (master mb))
                                           ".")
               (widget-path mb)))

;;; menues

(defclass menu(widget)
  ((text :accessor text :initarg :text)
   (help :accessor menu-help :initarg :help :initform nil)
   ))

;(defmethod create ((m menu))

(defmethod initialize-instance :after ((m menu) &key underline)
  (when (menu-help m) ;; special treatment for help menu
    (setf (name m) "help")
    (setf (slot-value m 'widget-path) (create-path (master m) (name m))))
  (format-wish "menu ~A -tearoff 0" (widget-path m))
  (when (master m)
    (format-wish "~A add cascade -label {~A} -menu ~a~@[ -underline ~a ~]"
                 (widget-path (master m)) (text m) (widget-path m) underline)))

(defun make-menu(menu text &key underline name)
  (if name
      (make-instance 'menu :master menu :text text :underline underline :name name)
      (make-instance 'menu :master menu :text text :underline underline)))

(defmethod menu-widget-path ((m menu)) (widget-path m))

(defclass pulldownmenu-button(widget)
  ((text :accessor text :initarg :text :initform "")
   (help :accessor menu-help :initarg :help :initform nil)
   (menu-widget-path :accessor menu-widget-path :initarg :menu-widget-path)
   (widget-class-name :accessor widget-class-name :initform "menubutton" :allocation :class)
   ))

(defmethod initialize-instance :after ((m pulldownmenu-button) &key underline)
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

(defun make-pulldownmenu-button (master text &key underline name)
  (make-instance 'pulldownmenu-button :master master :text text :underline underline :name name))


(defun add-separator (menu)
   (format-wish "~A add separator" (widget-path menu))
   menu)

(defclass menubutton-mixin () ())

(defmethod wish-menubutton-options-string (options)
  (loop for string  = "" then (concatenate 'string string opt-string)
	for (key val) on options by #'cddr
	for opt-string 
	  = (case key
	      (:columnbreak " -columnbreak true")
	      ((:underline )
	       " -~(~a~) ~a" key val)
	      ((:foreground :background :selectcolor :activebackground :activeforeground
			    :font :justify :underline :acceleraqtor
			    )
	       " -~(~a~) {~a}" key val)
	      )
	when opt-string
	  do (setq string (concatenate 'string string opt-string))
	finally (return string)))


;;; menu button

;;; Perhaps this should be called menucmdbutton since Tk menubutton is a totally different widget.
(defclass menubutton(widget) 
  ((text :accessor text :initarg :text :initform "")
   ))

(defmethod initialize-instance :after ((m menubutton) &key command underline accelerator)
  (format-wish "~A add command -label {~A}  -command {callback ~A}~@[ -underline ~a ~]~@[ -accelerator {~a} ~]"
               (widget-path (master m)) (text m) (callback-name m) underline accelerator)
  (add-callback m command))

;;; FIXME:  There are a lot of missing options for all of the menubutton types.
(defmethod initialize-instance :after ((m menubutton) &key command underline accelerator)
  (when (master m)
    (format-wish "~A add command -label {~A}  -command {callback ~A}~@[ -underline ~a ~]~@[ -accelerator {~a} ~]"
		 (menu-widget-path (master m)) (text m) (callback-name m) underline accelerator))
  (add-callback m command))

(defmethod initialize-instance :after ((m menubutton) &rest options &key command &allow-other-keys)
  (when (master m)
    (format-wish "~A add command -label {~A}  -command {callback ~A}~a"
		 (menu-widget-path (master m)) (text m) (callback-name m) 
		 (wish-menubutton-options-string options)
		 ))
  (add-callback m command))

;;; Perhaps this should be called menucmdbutton since Tk menubutton is a totally different widget.
(defun make-menubutton(menu text command &key underline accelerator name)
  (let* ((mb (make-instance 'menubutton :master menu :text text :command command :underline underline
			    :name name :accelerator accelerator)))
    mb))

(defclass menucheckbutton(widget) 
  ((text :accessor text :initarg :text)
   (command :accessor command :initarg :command :initform nil)))

(defmethod initialize-instance :after ((m menucheckbutton) &key)
  (format-wish "~A add checkbutton -label {~A} -variable ~a ~@[ -command {callback ~a}~]"
	       (widget-path (master m)) (text m) (callback-name m) (and (command m) (name m)))
  (add-callback m (command m)))

(defmethod value ((cb menucheckbutton))
  (format-wish-and-read "senddata $~a" (name cb))
  )

(defmethod (setf value) (val (cb menucheckbutton))
  (format-wish "set ~a ~a" (name cb) val)
  val)

(defclass menuradiobutton(widget) 
  ((text :accessor text :initarg :text)
   (command :accessor command :initarg :command :initform nil)
   (group :accessor group :initarg :group :initform nil)))

(defmethod initialize-instance :after ((m menuradiobutton) &key)
  (unless (group m) (setf (group m) (name m)))
  (format-wish "~A add radiobutton -label {~A} -value ~a -variable ~a ~@[ -command {callback ~a}~]"
               (widget-path (master m)) (text m) (callback-name m) (group m)
               (and (command m) (name m)))
  (add-callback m (command m)))

(defmethod value ((cb menuradiobutton))
  (format-wish-and-read "senddata $~a" (group cb))
  )

(defmethod (setf value) (val (cb menuradiobutton))
  (format-wish "set ~a ~a" (group cb) val)
  val)


;;; method to pop up a menue at the root window coordinates x and y

(defgeneric popup (menu x y))
(defmethod popup ((menu menu) x y)
  (format-wish "tk_popup ~A ~A ~A" (widget-path menu) x y)
  menu)

(defgeneric menu-delete (menu index))
(defmethod menu-delete ((menu menu) index)
  (format-wish "~A delete ~A" (widget-path menu) index)
  menu)

;;; standard button widget

(defwidget button (tktextvariable widget) () "button")

(defmethod (setf command) (val (button button))
  (format-wish "~a configure -command {callback ~a}" (widget-path button) (callback-name button))
  (add-callback button val)
  val)

;;; check button widget

(defwidget check-button (tktextvariable widget tkvariable) () "checkbutton")

(defmethod (setf command) (val (check-button check-button))
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
	       (name check-button) (name check-button))
  (add-callback check-button val)
  val)

;;; radio button widget

(defwidget radio-button (tktextvariable widget) 
  ((val :accessor radio-button-value :initarg :value :initform nil)
   (var :accessor radio-button-variable :initarg :variable :initform nil)) 
  "radiobutton")

(defmethod value ((rb radio-button))
  "reads the content of the shared variable of the radio button set"
  (if (radio-button-variable rb)
      (progn
	(format-wish-and-read "senddata $~a" (radio-button-variable rb))
	)
      nil))

(defmethod (setf value) (val (rb radio-button))
  "sets the content of the shared variable of the radio button set"
  (when (radio-button-variable rb)
    (format-wish "set ~a ~a" (radio-button-variable rb) val))
  val)

(defmethod (setf command) (val (rb radio-button))
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path rb) (callback-name rb) (radio-button-variable rb))
  (add-callback rb val)
  val)

;; text entry widget

(defwidget entry (tktextvariable widget) () "entry")

(defun entry-select (e from to)
  (format-wish "~a selection range ~a ~a" (widget-path e) from to)
  e)

(defgeneric cursor-index (widget)
  (:documentation "returns the cursor index in the widget"))

(defmethod cursor-index ((e entry))
  (format-wish-and-read "senddata [~a index insert]" (widget-path e))
  )

(defun split (string at)
  (let ((pos (search at string))
        erg)
    (loop
       while pos
       do
         (when (> pos 0)
           (push (subseq string 0 pos) erg))
         (setf string (subseq string (+ pos (length at))))
         (setf pos (search at string)))
    (when (> (length string) 0)
      (push string erg))
    (nreverse erg)))
        


;;; frame widget 

(defwidget frame (widget) () "frame")

(defun make-frame (master)
  (make-instance 'frame :master master))

;;; labelframe widget 

(defwidget labelframe (widget) () "labelframe")

(defmethod (setf text) :after (val (l labelframe))
  (format-wish "~a configure -text {~a}" (widget-path l) val)
  val)

;;; panedwindow widget


(defwidget paned-window (widget) () "panedwindow")

(defgeneric add-pane (window widget))
(defmethod add-pane ((pw paned-window) (w widget))
  (format-wish "~a add ~a" (widget-path pw) (widget-path w))
  pw)

(defgeneric forget-pane (window widget))
(defmethod forget-pane ((pw paned-window) (w widget))
  (format-wish "~a forget ~a" (widget-path pw) (widget-path w))
  pw)

;;; listbox widget

(defwidget listbox (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   ) "listbox")

(defmethod (setf command) (val (listbox listbox))
  (format-wish "bind ~a <<ListboxSelect>> {callbackval ~a ([~a curselection])}" (widget-path listbox)
	       (callback-name listbox) (widget-path listbox))
  (add-callback listbox val)
  val)

(defgeneric listbox-append (l vals))
(defmethod listbox-append ((l listbox) values)
  "append values (which may be a list) to the list box"
  (if (listp values)
      (format-wish "~a insert end ~{ \{~a\}~}" (widget-path l) values)
      (format-wish "~a insert end \{~a\}" (widget-path l) values))
  l)

(defgeneric listbox-get-selection (l))
(defmethod listbox-get-selection ((l listbox))
  (format-wish-and-read "senddata \"([~a curselection])\"" (widget-path l))
  )

(defgeneric listbox-select (l val))
(defmethod listbox-select ((l listbox) val)
  "modify the selection in listbox, if nil is given, the selection is cleared,
if a number is given the corresponding element is selected, alternatively
a list of numbers may be given"
  (if (null val)
      (format-wish "~a selection clear 0 end" (widget-path l))
      (if (listp val)
          (format-wish "~a selection set ~{ ~a~}" (widget-path l) val)
          (format-wish "~a selection set ~a" (widget-path l) val)))
  l)

(defgeneric listbox-clear (l))

(defmethod listbox-clear ((l listbox))
  (format-wish "~a delete 0 end" (widget-path l))
  l)


(defgeneric listbox-configure (l i &rest options))
(defmethod listbox-configure ((l listbox) index &rest options)
  (format-wish "~a itemconfigure ~a ~{ -~(~a~) {~(~a~)}~}" (widget-path l) index options)
  l)

(defgeneric listbox-nearest (listbox y))
(defmethod listbox-nearest ((l listbox) y)
  (format-wish-and-read "senddata [~a nearest ~a]" (widget-path l) y)
  )


(defclass scrolled-listbox (frame)
  ((listbox :accessor listbox)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod initialize-instance :after ((sl scrolled-listbox) &key)
  (setf (hscroll sl) (make-scrollbar sl :orientation "horizontal"))
  (setf (vscroll sl) (make-scrollbar sl))
  (setf (listbox sl) (make-instance 'listbox :master sl :xscroll (hscroll sl) :yscroll (vscroll sl)))
  (grid (listbox sl) 0 0 :sticky "news")
  (grid (hscroll sl) 1 0 :sticky "we")
  (grid (vscroll sl) 0 1 :sticky "ns")
  (grid-columnconfigure sl 0 :weight 1)
  (grid-columnconfigure sl 1 :weight 0)
  (grid-rowconfigure sl 0 :weight 1)
  (grid-rowconfigure sl 1 :weight 0)
 
  (configure (hscroll sl) "command" (concatenate 'string (widget-path (listbox sl)) " xview"))
  (configure (vscroll sl) "command" (concatenate 'string (widget-path (listbox sl)) " yview"))
  (configure (listbox sl) "xscrollcommand" (concatenate 'string (widget-path (hscroll sl)) " set"))
  (configure (listbox sl) "yscrollcommand" (concatenate 'string (widget-path (vscroll sl)) " set")))

(defmethod listbox-append ((l scrolled-listbox) values)
  (listbox-append (listbox l) values)
  l)

(defmethod listbox-get-selection ((l scrolled-listbox))
  (listbox-get-selection (listbox l)))

(defmethod listbox-select ((l scrolled-listbox) val)
  (listbox-select (listbox l) val)
  l)

;;; scrolled-text

(defclass scrolled-text (frame)
  ((textbox :accessor textbox)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod initialize-instance :after ((st scrolled-text) &key)
  (setf (hscroll st) (make-scrollbar st :orientation "horizontal"))
  (setf (vscroll st) (make-scrollbar st))
  (setf (textbox st) (make-instance 'text :master st :xscroll (hscroll st) :yscroll (vscroll st)))
  (grid (textbox st) 0 0 :sticky "news")
  (grid (hscroll st) 1 0 :sticky "we")
  (grid (vscroll st) 0 1 :sticky "ns")
  (grid-columnconfigure st 0 :weight 1)
  (grid-columnconfigure st 1 :weight 0)
  (grid-rowconfigure st 0 :weight 1)
  (grid-rowconfigure st 1 :weight 0)
 
  (configure (hscroll st) "command" (concatenate 'string (widget-path (textbox st)) " xview"))
  (configure (vscroll st) "command" (concatenate 'string (widget-path (textbox st)) " yview"))
  (configure (textbox st) "xscrollcommand" (concatenate 'string (widget-path (hscroll st)) " set"))
  (configure (textbox st) "yscrollcommand" (concatenate 'string (widget-path (vscroll st)) " set"))
  )

(defgeneric append-text (txt text &rest tags))
(defmethod append-text ((txt scrolled-text) text &rest tags )
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path (textbox txt)) (tkescape text) tags)
  txt)

(defmethod (setf text) (new-text (self scrolled-text))
  (setf (text (textbox self)) new-text))

(defgeneric insert-object (txt object))
(defmethod insert-object ((txt scrolled-text) obj)
  (format-wish "~a window create end -window ~a" (widget-path (textbox txt)) (widget-path obj))
  txt)

(defgeneric see (txt pos))
(defmethod see ((txt scrolled-text) pos)
  (format-wish "~a see ~a" (widget-path (textbox txt)) pos)
  txt)

(defmethod see ((lb listbox) pos)
  (format-wish "~a see ~a" (widget-path lb) pos)
  lb)

;;; scale widget

(defwidget scale (tkvariable widget) () "scale")

(defmethod (setf command) (val (scale scale))
  (add-callback scale val)					
  (format-wish "proc ~a-command {val} {callbackval ~a $val}" (name scale) (callback-name scale))
  (format-wish "~a configure -command ~a-command" (widget-path scale) (callback-name scale))
  val)

;;; spinbox widget

(defwidget spinbox (tktextvariable widget) () "spinbox")

(defmethod (setf command) (val (sp spinbox))
  (add-callback sp val)					
  (format-wish "~a configure -command {callbackstring ~a %s}" (widget-path sp) (callback-name sp))
  val)

;;; toplevel (window) widget 

(defwidget toplevel (widget) 
  ((protocol-destroy :accessor protocol-destroy :initarg :on-close :initform nil)
   (title :accessor title :initform nil :initarg :title)
   ) 
  "toplevel"
  (when (title widget)
    (wm-title widget (title widget)))
  (unless (protocol-destroy widget)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {wm withdraw ~a}" (widget-path widget) (widget-path widget))))

(defun make-toplevel (master &rest initargs)
  (apply #'make-instance 'toplevel :master master initargs))

;;; label widget

(defwidget label (tktextvariable widget) () "label")

;(defun make-label (master text)
;  (make-instance 'label :master master  :text text))

;;; message widget

(defwidget message (tktextvariable widget) () "message")

;;; scrollbar

(defwidget scrollbar (widget) () "scrollbar")

(defun make-scrollbar(master &key (orientation "vertical"))
  (make-instance 'scrollbar :master master :orientation orientation))

(defclass scrolled-canvas (frame)
  ((canvas :accessor canvas)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defun make-scrolled-canvas (master)
  (make-instance 'scrolled-canvas :master master ))

(defmethod initialize-instance :after ((sc scrolled-canvas) &key)
  (setf (hscroll sc) (make-scrollbar sc :orientation "horizontal"))
  (setf (vscroll sc) (make-scrollbar sc))
  (setf (canvas sc) (make-canvas sc :xscroll (hscroll sc) :yscroll (vscroll sc)))
  (grid (canvas sc) 0 0 :sticky :news)
  (grid (hscroll sc) 1 0 :sticky :we)
  (grid (vscroll sc) 0 1 :sticky :ns)
  (grid-columnconfigure sc 0 :weight 1)
  (grid-columnconfigure sc 1 :weight 0)
  (grid-rowconfigure sc 0 :weight 1)
  (grid-rowconfigure sc 1 :weight 0)
 
  (configure (hscroll sc) "command" (concatenate 'string (widget-path (canvas sc)) " xview"))
  (configure (vscroll sc) "command" (concatenate 'string (widget-path (canvas sc)) " yview"))
  (configure (canvas sc) "xscrollcommand" (concatenate 'string (widget-path (hscroll sc)) " set"))
  (configure (canvas sc) "yscrollcommand" (concatenate 'string (widget-path (vscroll sc)) " set"))
  )


(defclass scrolled-frame (frame)
  ((inner :accessor interior)
   (displayframe :accessor scrolled-frame-display)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod initialize-instance :after ((sf scrolled-frame) &key)
  (let ((f (make-instance 'frame :master sf)))
    (setf (scrolled-frame-display sf) f)
    (setf (interior sf) (make-instance 'frame :master f))
    (setf (hscroll sf) (make-instance 'scrollbar :master sf :orientation "horizontal"))
    (setf (vscroll sf) (make-instance 'scrollbar :master sf :orientation "vertical"))
    (grid f 0 0 :sticky "news")
    (grid (hscroll sf) 1 0 :sticky "we")
    (grid (vscroll sf) 0 1 :sticky "ns")
    (grid-columnconfigure sf 0 "weight" 1)
    (grid-columnconfigure sf 1 "weight" 0)
    (grid-rowconfigure sf 0 "weight" 1)
    (grid-rowconfigure sf 1 "weight" 0)
    (place (interior sf) 0 0)
    (send-wish (format nil "~a set  0.1 0.5" (widget-path (hscroll sf))))
    (send-wish (format nil "~a set  0.1 0.5" (widget-path (vscroll sf))))
    (send-wish (format nil "~a configure -command ~axv" (widget-path (hscroll sf)) (name sf)))
    (send-wish (format nil "~a configure -command ~ayv" (widget-path (vscroll sf)) (name sf)))
    (send-wish (format nil "
proc ~axv {{com moveto} {val 0} {unit 0}} {
set x [winfo x ~a]
set y [winfo y ~a]
set wx [winfo width ~a]
set w [winfo width ~a]
if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wx-$w)/$wx]} {set val  [expr 1.0*($wx-$w)/$wx]}
place ~a -x [expr -($val * $wx)] -y $y
set x [winfo x ~a]
~a set [expr -1.0*$x/$wx] [expr 1.0*($w-$x)/$wx]
}
proc ~ayv {{com moveto} {val 0} {unit 0}} {
set x [winfo x ~a]
set y [winfo y ~a]
set wy [winfo height ~a]
set h [winfo height ~a]
if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wy-$h)/$wy]} {set val  [expr 1.0*($wy-$h)/$wy]}
place ~a -x $x -y [expr -($val * $wy)]
set y [winfo y ~a]
~a set [expr -1.0*$y/$wy] [expr 1.0*($h-$y)/$wy]
}

" (name sf)
  (widget-path (interior sf))
  (widget-path (interior sf))
  (widget-path (interior sf))
  (widget-path f)
  (widget-path (interior sf))
  (widget-path (interior sf))		   
  (widget-path (hscroll sf))
  
  (name sf)
  (widget-path (interior sf))
  (widget-path (interior sf))
  (widget-path (interior sf))
  (widget-path f)
  (widget-path (interior sf))
  (widget-path (interior sf))		   
  (widget-path (vscroll sf))
  ))))

;;; canvas widget

(defwidget canvas (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   (scrollregion-x0 :accessor scrollregion-x0 :initform nil)
   (scrollregion-y0 :accessor scrollregion-y0 :initform nil)
   (scrollregion-x1 :accessor scrollregion-x1 :initform nil)
   (scrollregion-y1 :accessor scrollregion-y1 :initform nil)
   ) 
  "canvas"
  )

;; wrapper class for canvas items
(defclass canvas-item ()
  ((canvas :accessor canvas :initarg :canvas)
   (handle :accessor handle :initarg :handle))
  )

(defmethod canvas ((canvas canvas)) canvas)

(defun make-canvas (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'canvas :master master :width width :height height :xscroll xscroll :yscroll yscroll))

(defgeneric scale (canvas factor &optional factory))
(defmethod scale ((canvas canvas) factor &optional factory)
  (format-wish "~a scale all 0 0 ~a ~a" (widget-path canvas) factor (or factory factor))
  canvas)

(defun move-all (canvas dx dy)
  (format-wish "~a move all ~a ~a" (widget-path canvas) dx dy)
  canvas)


(defgeneric bbox (item))
(defmethod bbox ((item canvas-item))
  (canvas-bbox (canvas item) (handle item)))

(defmethod bbox ((canvas canvas))
  (format-wish-and-read "senddata \"([~a bbox all])\"" (widget-path canvas))
  )


(defun canvas-bbox (canvas handle)
  (format-wish-and-read "senddata \"([~a bbox ~a])\"" (widget-path canvas) handle)
  )

(defmethod calc-scroll-region ((canvas canvas))
  (format-wish "~a configure -scrollregion [~a bbox all]" (widget-path canvas) (widget-path canvas))
  canvas)

(defgeneric set-coords (canvas item coords))

(defmethod set-coords (canvas item coords)
  (format-wish "~a coords ~a~{ ~a~}" (widget-path canvas) item coords)
  canvas)

(defmethod set-coords ((canvas canvas) (item canvas-item) (coords list))
  (set-coords canvas (handle item) coords))

(defgeneric set-coords* (canvas item &rest coords))

(defmethod set-coords* (canvas item &rest coords)
  (funcall #'set-coords canvas item coords))

(defmethod set-coords* ((canvas canvas) (item canvas-item) &rest coords)
  (funcall #'set-coords canvas (handle item) coords))

(defgeneric coords (item))
(defmethod coords ((item canvas-item))
     (list 0 0)				; not implemented yet
     )
 
(defun format-number (stream number)
  (cond
   ((complexp number)
    (format-number stream (realpart number))
    (format-number stream (imagpart number)))
   ((integerp number)
    (format stream " ~d" number))	    
   ((typep number 'single-float)
    (format stream " ~a" number))
   ((numberp number)
    (format-number stream (coerce number 'single-float)))
   ((null number)
    )
   ((listp number)
    (format-number stream (car number))
    (format-number stream (cdr number)))
   ((arrayp number)
    (dotimes (i (length number))
      (format-number stream (aref number i))))
   ))
 
(defun process-coords (input)
  (with-output-to-string (s)
			 (format-number s input)))

(defgeneric (setf coords) (val item))

(defmethod (setf coords) (val (item canvas-item))
  (let ((coord-list (process-coords val)))
    (format-wish "~a coords ~a ~a" (widget-path (canvas item)) (handle item) coord-list)
    coord-list))

(defgeneric itembind (canvas w event fun))
(defmethod itembind ((canvas canvas) (item canvas-item) event fun)
  (itembind canvas (handle item) event fun))

(defmethod itembind ((canvas canvas) (item integer) event fun)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a bind ~a ~a {sendevent ~A %x %y %k %K %w %h %X %Y %b}" (widget-path canvas) item event name))
  canvas)

(defmethod bind ((w canvas-item) event fun &key append exclusive)
  (declare (ignore append exclusive))
  (itembind (canvas w) (handle w) event fun))

(defgeneric scrollregion (canvas x0 y0 x1 y1))
(defmethod scrollregion ((c canvas) x0 y0 x1 y1)
  (setf (scrollregion-x0 c) x0)
  (setf (scrollregion-y0 c) y0)
  (setf (scrollregion-x1 c) x1)
  (setf (scrollregion-y1 c) y1)
  (configure c :scrollregion (format nil "~a ~a ~a ~a" x0 y0 x1 y1))
  c)

(defgeneric canvasx (canvas screenx))
(defmethod canvasx ((canvas canvas) screenx)
  (format-wish-and-read "senddata [~a canvasx ~a]" (widget-path canvas) screenx)
  )

(defgeneric canvasy (canvas screeny))
(defmethod canvasy ((canvas canvas) screeny)
  (format-wish-and-read "senddata [~a canvasy ~a]" (widget-path canvas) screeny)
  )

(defgeneric itemmove (canvas item dx dy))
(defmethod itemmove ((canvas canvas) (item integer) dx dy)
  (format-wish "~a move ~a ~a ~a" (widget-path canvas) item dx dy)
  canvas)

(defmethod itemmove ((canvas canvas) (item canvas-item) dx dy)
  (itemmove (canvas item) (handle item) dx dy))

(defgeneric itemdelete (canvas item))
(defmethod itemdelete ((canvas canvas) (item integer))
  (format-wish "~a delete ~a" (widget-path canvas) item)
  canvas)

(defmethod itemdelete ((canvas canvas) (item canvas-item))
  (format-wish "~a delete ~a" (widget-path canvas) (handle item))
  canvas)

(defgeneric move (item dx dy))
(defmethod move ((item canvas-item) dx dy)
  (itemmove (canvas item) (handle item) dx dy))

(defgeneric clear (widget))
(defmethod clear ((canvas canvas))
  "delete all items within a canvas"
  (format-wish "~a delete all" (widget-path canvas))
  canvas)

;; canvas item functions

(defun create-line (canvas coords)
  (format-wish-and-read "senddata [~a create line~{ ~a~}]" (widget-path canvas) coords)
  )

(defun create-line* (canvas &rest coords)
  (funcall #'create-line canvas coords))

(defclass canvas-line (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-line) &key canvas coords)
  (setf (handle c) (create-line canvas coords)))

(defun make-line (canvas coords)
  (make-instance 'canvas-line :canvas canvas :coords coords))


(defun create-polygon (canvas coords)
  (format-wish-and-read "senddata [~a create polygon~{ ~a~}]" (widget-path canvas) coords)
  )

(defclass canvas-polygon (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-polygon) &key canvas coords)
  (setf (handle c) (create-polygon canvas coords)))

(defun make-polygon (canvas coords)
  (make-instance 'canvas-polygon :canvas canvas :coords coords))


(defun create-oval (canvas x0 y0 x1 y1)
  (format-wish-and-read "senddata [~a create oval ~a ~a ~a ~a]" (widget-path canvas) x0 y0 x1 y1)
  )

(defclass canvas-oval (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-oval) &key canvas x0 y0 x1 y1)
  (setf (handle c) (create-oval canvas x0 y0 x1 y1)))

(defun make-oval (canvas x0 y0 x1 y1)
  (make-instance 'canvas-oval :canvas canvas :x0 x0 :y0 y0 :x1 x1 :y1 y1))


(defun create-rectangle (canvas x0 y0 x1 y1)
  (format-wish-and-read "senddata [~a create rectangle ~a ~a ~a ~a]" (widget-path canvas) x0 y0 x1 y1)
  )

(defclass canvas-rectangle (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-rectangle) &key canvas x0 y0 x1 y1)
  (setf (handle c) (create-rectangle canvas x0 y0 x1 y1)))

(defun make-rectangle (canvas x0 y0 x1 y1)
  (make-instance 'canvas-rectangle :canvas canvas :x0 x0 :y0 y0 :x1 x1 :y1 y1))



(defun create-text (canvas x y text)
  (format-wish-and-read "senddata [~a create text ~a ~a -anchor nw -text {~a}]" (widget-path canvas) x y text)
  )

(defclass canvas-text (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-text) &key canvas x y text)
  (setf (handle c) (create-text canvas x y text)))


(defun create-image (canvas x y &key image)
  (format-wish-and-read "senddata [~a create image ~a ~a -anchor nw~@[ -image ~a~]]" (widget-path canvas) x y
	       (and image (name image)))
  )

(defclass canvas-image (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-image) &key canvas x y image)
  (setf (handle c) (create-image canvas x y :image image)))

(defun image-setpixel (image data x y &optional x2 y2 )
  (format-wish "~A put {~{{~:{#~2,'0X~2,'0X~2,'0X ~} } ~} } -to ~a ~a~@[ ~a~]~@[ ~a~]" (name image) data x y x2 y2)
  image)

(defun create-bitmap (canvas x y &key (bitmap nil))
  (format-wish-and-read "senddata [~a create image ~a ~a -anchor nw~@[ -bitmap ~a~]]" (widget-path canvas) x y
	       (and bitmap (name bitmap)))
  )


(defun create-arc (canvas x0 y0 x1 y1 &key (start 0) (extent 180) (style "pieslice"))
  (format-wish-and-read "senddata [~a create arc ~a ~a ~a ~a -start ~a -extent ~a -style ~a]"
	       (widget-path canvas) x0 y0 x1 y1 start extent style)
  )

(defclass canvas-arc (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-arc) &key canvas x0 y0 x1 y1 (start 0) (extent 180) (style "pieslice"))
  (setf (handle c) (create-arc canvas x0 y0 x1 y1 :start start :extent extent :style style)))


(defun create-window (canvas x y widget &key (anchor :nw))
  (format-wish-and-read "senddata [~a create window ~a ~a -anchor ~(~a~) -window ~a]"
 	       (widget-path canvas) x y anchor (widget-path widget))
  )

(defun postscript (canvas filename &key rotate pagewidth pageheight)
  (if (and (scrollregion-x0 canvas)
	   (scrollregion-x1 canvas)
	   (scrollregion-y0 canvas)
	   (scrollregion-y1 canvas))
      (format-wish "~a postscript -file ~a -x ~a -y ~a -width ~a -height ~a~@[ -rotate ~a~]~@[ -pagewidth ~a~]~@[ -pageheight ~a~]"
		(widget-path canvas) filename
		(scrollregion-x0 canvas) (scrollregion-y0 canvas)
		(- (scrollregion-x1 canvas) (scrollregion-x0 canvas))
		(- (scrollregion-y1 canvas) (scrollregion-y0 canvas))
		rotate pageheight pagewidth
		)
    (format-wish "~a postscript -file ~a" (widget-path canvas) filename))
  canvas)

;;; text widget

(defwidget text (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
  )  "text")

(defmethod cursor-index ((text text))
  (let* ((index (split (format-wish-and-read "senddatastring [~a index insert]" (widget-path text)) ".")))
        (values (parse-integer (first index))
                (parse-integer (second index)))))

(defun make-text (master &key (width nil) (height nil))
  (make-instance 'text :master master :width width :height height))

(defmethod append-text ((txt text) text &rest tags)
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path txt) (tkescape text) tags)
  txt)

(defmethod insert-object ((txt text) obj)
  (format-wish "~a window create end -window ~a" (widget-path txt) (widget-path obj))
  txt)

(defun append-newline (text)
  (append-text text (coerce '(#\Linefeed) 'string)))

(defgeneric clear-text (txt))
(defmethod clear-text ((txt text))
  (format-wish "~A delete 0.0 end" (widget-path txt))
  txt)

(defmethod see((txt text) pos)
  (format-wish "~a see ~a" (widget-path txt) pos)
  txt)

(defgeneric tag-configure (txt tag option value))
(defmethod tag-configure ((txt text) tag option value)
  (format-wish "~a tag configure ~a -~(~a~) {~(~a~)}" (widget-path txt)
	       (if (stringp tag)
		   tag
		 (format nil "~(~a~)" tag))
	       option value)
  txt)

(defgeneric tag-bind (txt tag event fun))
(defmethod tag-bind ((txt text) tag event fun)
  "bind fun to event of the tag of the text widget txt"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a tag bind ~a ~a {callback ~A}" (widget-path txt) tag event name)
    )
  txt)

(defmethod text ((text text))
  (format-wish-and-read "senddatastring [~a get 1.0 end]" (widget-path text))
  )

(defmethod (setf text) (val (text text))
  (format-wish "~A delete 0.0 end;~A insert end {~A}" (widget-path text) (widget-path text) val)
  val)

(defgeneric save-text (txt filename))
(defmethod save-text ((txt text) filename)
  "save the content of the text widget into the file <filename>"
  (format-wish "set file [open {~a} \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" filename (widget-path txt))
  (read-line (wish-stream *wish*))
  txt)

(defgeneric load-text (txt filename))
(defmethod load-text((txt text) filename)
  "load the content of the file <filename>"
;  (format-wish "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"asdf\"" filename (widget-path txt) (widget-path txt))
  (format-wish-and "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"(:DATA asdf)\"" filename (widget-path txt) (widget-path txt))
  (read-data))

;;; photo image object

(defclass photo-image(tkobject)
  ()
  )

(defmethod widget-path ((photo photo-image))
  (name photo))

(defmethod initialize-instance :after ((p photo-image) &key width height)
  (setf (name p) (create-name))
  (format-wish "image create photo ~A~@[ -width ~a~]~@[ -height ~a~]" (name p) width height))

(defun make-image ()
  (let* ((name (create-name))
	 (i (make-instance 'photo-image :name name)))
    ;(create i)
    i))

(defgeneric image-load (p filename))
(defmethod image-load((p photo-image) filename)
  ;(format t "loading file ~a~&" filename)
  (send-wish (format nil "~A read {~A} -shrink" (name p) filename))
  )

(defgeneric ishow (p name))
(defmethod ishow((p photo-image) name)
  (convert (concatenate 'string name ".jpg")
	   "ishow.ppm")
  (image-load p "ishow.ppm"))

;;;; generic methods on widgets

;;; pack method for widget arrangement in container

(defgeneric pack (w &key side fill expand after before padx pady ipadx ipady anchor))

(defmethod pack ((w widget) &key (side :top) (fill :none) expand after before padx pady ipadx ipady anchor)
  (cond ((stringp side)
         (warn "Using a string for the :SIDE parameter is deprecated."))
        ((stringp fill)
         (warn "Using a string for the :FILL parameter is deprecated.")))
  (format-wish "pack ~A -side ~(~A~) -fill ~(~A~)~@[~* -expand 1~]~
             ~@[ -after ~A~]~@[ -before ~A~]~@[ -padx ~A~]~
             ~@[ -pady ~A~]~@[ -ipadx ~A~]~@[ -ipady ~A~]~@[ -anchor ~(~A~)~]"
          (widget-path w) side fill expand (and after (widget-path after)) (and before (widget-path before)) padx pady ipadx ipady anchor)
  w)

(defmethod pack ((list list) &rest rest)
  (mapcar #'(lambda (w)
              (apply #'pack w rest))
	  list))

(defgeneric pack-propagate (widget flag))
(defmethod pack-propagate ((w widget) flag)
  (format-wish "pack propagate ~A ~A"
	       (widget-path w)
	       (if flag "true" "false"))
  w)

(defgeneric pack-forget (widget))
(defmethod pack-forget ((w widget))
  (format-wish "pack forget ~A" (widget-path w))
  w)


;;; place manager

(defgeneric place (widget x y &key width height))
(defmethod place (widget x y &key width height)
  (format-wish "place ~A -x ~A -y ~A~@[ -width ~a~]~@[ -height ~a~]" (widget-path widget) x y width height)
  widget)

(defgeneric place-forget (widget))
(defmethod place-forget ((w widget))
  (format-wish "place forget ~A" (widget-path w))
  w)

;;; grid manager

(defgeneric grid (widget r c &key columnspan ipadx ipady padx pady rowspan sticky))
(defmethod grid ((w widget) row column &key columnspan ipadx ipady padx pady rowspan sticky)
  (format-wish "grid ~a -row ~a -column ~a~@[ -columnspan ~a~]~@[ -ipadx ~a~]~
             ~@[ -ipady ~a~]~@[ -padx ~a~]~@[ -pady ~a~]~@[ -rowspan ~a~]~
             ~@[ -sticky ~(~a~)~]" (widget-path w) row column columnspan ipadx ipady padx pady rowspan  sticky)
  w)

(defgeneric grid-columnconfigure (widget c o v))
(defmethod grid-columnconfigure (widget column option value)
  (format-wish "grid columnconfigure ~a ~a -~(~a~) {~a}" (widget-path widget) column option value)
  widget)

(defgeneric grid-rowconfigure (widget r o v))
(defmethod grid-rowconfigure (widget row option value)
  (format-wish "grid rowconfigure ~a ~a -~(~a~) {~a}" (widget-path widget) row option value)
  widget)

(defgeneric grid-configure (widget o v))
(defmethod grid-configure (widget option value)
  (format-wish "grid configure ~a -~(~a~) {~a}" (widget-path widget) option value)
  widget)

(defgeneric grid-forget (widget))
(defmethod grid-forget ((w widget))
  (format-wish "grid forget ~A" (widget-path w))
  w)

;;; configure a widget parameter

(defgeneric configure (widget option value &rest others))
(defmethod configure (widget option value &rest others)
  ;(format t "normal config~&")
  (format-wish "~A configure -~(~A~) {~A} ~{ -~(~a~) {~(~a~)}~}" (widget-path widget) option 
	    (if (stringp value) ;; There may be values that need to be passed as
		value           ;; unmodified strings, so do not downcase strings
	      (format nil "~(~a~)" value)) ;; if its not a string, print it downcased (eg. symbols)
	    others)
  widget)

(defmethod configure ((item canvas-item) option value &rest others)
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}~{ -~(~a~) {~(~a~)}~}" (widget-path (canvas item)) (handle item) option
	       (if (stringp value) ;; There may be values that need to be passed as
		   value           ;; unmodified strings, so do not downcase strings
		 (format nil "~(~a~)" value))
	       others)
  item)

;;; for tkobjects, the name of the widget is taken
(defmethod configure (widget option (value tkobject) &rest others)
  (format-wish "~A configure -~(~A~) {~A} ~{ -~(~a~) {~(~a~)}~}" (widget-path widget) option (widget-path value) others)
  widget)

(defgeneric cget (widget option))
(defmethod cget ((widget widget) option)
  (format-wish-and-read "senddatastring [~a cget -~(~a~)]" (widget-path widget) option)
  )

;(defun background (widget)
;  (cget widget :background))

#-:gcl
;(defun (setf background) (val widget)
;  (configure widget :background val))

#|
(defmacro defoption (option)
  `(progn
     (defun ,option (widget)
       (cget widget "asdf"))
     (export ,option)))

(defoption fill)
|#

(defgeneric itemconfigure (widget item option value))

(defmethod itemconfigure ((widget canvas) item option value)
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}" (widget-path widget) item option
	    (if (stringp value) ;; There may be values that need to be passed as
		value           ;; unmodified strings, so do not downcase strings
	      (format nil "~(~a~)" value))) ;; if its not a string, print it downcased
  widget)


;;; for tkobjects, the name of the widget is taken
(defmethod itemconfigure ((widget canvas) item option (value tkobject))
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}" (widget-path widget) item option (widget-path value))
  widget)

(defgeneric itemlower (w i &optional below))
(defmethod itemlower ((widget canvas) item &optional below)
  (format-wish "~A lower ~A ~@[~A~]" (widget-path widget)
	       item below)
  widget)

(defmethod lower ((item canvas-item) &optional below)
  (itemlower (canvas item) (handle item) (and below (handle below))))

(defgeneric itemraise (w i &optional above))
(defmethod itemraise ((widget canvas) item &optional above)
  (format-wish "~A raise ~A ~@[~A~]" (widget-path widget)
	       item above)
  widget)

(defmethod raise ((item canvas-item) &optional above)
  (itemraise (canvas item) (handle item) (and above (handle above))))

;;; grab functions

(defgeneric grab (toplevel))
(defmethod grab ((toplevel toplevel))
  (format-wish "grab set ~a" (widget-path toplevel))
  toplevel)

(defgeneric grab-release (toplevel))
(defmethod grab-release ((toplevel toplevel))
  (format-wish "grab release ~a" (widget-path toplevel))
  toplevel)

;;; wm functions

(defgeneric set-wm-overrideredirect (widget value))
(defmethod set-wm-overrideredirect ((w widget) val)
  (format-wish "wm overrideredirect ~a ~a" (widget-path w) val)
  w)

(defgeneric wm-title (widget title))
(defmethod wm-title ((w widget) title)
  (format-wish "wm title ~a {~a}" (widget-path w) title)
  w)

(defgeneric wm-state (widget))
(defmethod wm-state ((w widget))
  (format-wish-and-read "senddatastring [wm state ~a]" (widget-path w))
  (read-wish))

(defgeneric (setf wm-state) (new-state widget))
(defmethod (setf wm-state) (new-state (w widget))
  (format-wish "wm state ~a ~a" (widget-path w) new-state)
  new-state)

(defgeneric minsize (widget x y))
(defmethod minsize ((w widget) x y)
  (format-wish "wm minsize ~a ~a ~a" (widget-path w) x y)
  w)

(defgeneric maxsize (widget x y))
(defmethod maxsize ((w widget) x y)
  (format-wish "wm maxsize ~a ~a ~a" (widget-path w) x y)
  w)

(defgeneric withdraw (toplevel))
(defmethod withdraw ((tl widget))
  (format-wish "wm withdraw ~a" (widget-path tl))
  tl)

(defgeneric normalize (toplevel))
(defmethod normalize ((tl widget))
  (format-wish "wm state ~a normal" (widget-path tl))
  tl)

(defgeneric iconify (toplevel))
(defmethod iconify ((tl toplevel))
  (format-wish "wm iconify ~a" (widget-path tl))
  tl)

(defgeneric deiconify (toplevel))
(defmethod deiconify ((tl toplevel))
  (format-wish "wm deiconify ~a" (widget-path tl))
  tl)

(defgeneric geometry (toplevel))
(defmethod geometry ((tl widget))
  (format-wish-and-read "senddatastring [wm geometry ~a]" (widget-path tl))
  )

(defgeneric set-geometry (toplevel width height x y))
(defmethod set-geometry ((tl widget) width height x y)
  ;;(format-wish "wm geometry ~a ~ax~a+~a+~a" (widget-path tl) width height x y)
  (format-wish "wm geometry ~a ~ax~a~@D~@D" (widget-path tl) width height x y)
  tl)

(defgeneric set-geometry-wh (toplevel width height))
(defmethod set-geometry-wh ((tl widget) width height)
  (format-wish "wm geometry ~a ~ax~a" (widget-path tl) width height)
  tl)

(defgeneric set-geometry-xy (toplevel x y))
(defmethod set-geometry-xy ((tl widget) x y)
  (format-wish "wm geometry ~a ~@D~@D" (widget-path tl) x y)
  tl)
 
(defgeneric on-close (toplevel fun))
(defmethod on-close ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {callback ~A}" (widget-path tl) name))
  tl)

(defgeneric on-focus (toplevel fun))
(defmethod on-focus ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol WM_TAKE_FOCUS {callback ~A}"
	      name))
  tl)

(defun iconwindow (tl wid)
  (format-wish "wm iconwindow ~a ~a" (widget-path tl) (widget-path wid))
  tl)  

;;; winfo functions

(defun screen-width (&optional (w nil))
  "give the width of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish-and-read "senddata [winfo screenwidth ~a]" (if w (widget-path w) "."))
  )

(defun screen-height (&optional (w nil))
  "give the height of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish-and-read "senddata [winfo screenheight ~a]" (if w (widget-path w) "."))
  )

(defun screen-width-mm (&optional (w nil))
  "give the width of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish-and-read "senddata [winfo screenmmwidth ~a]" (if w (widget-path w) "."))
  )

(defun screen-height-mm (&optional (w nil))
  "give the height of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish-and-read "senddata [winfo screenmmheight ~a]" (if w (widget-path w) "."))
  )

(defun screen-mouse-x (&optional (w nil))
  "give x position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish-and-read "senddata [winfo pointerx ~a]" (if w (widget-path w) "."))
  )

(defun screen-mouse-y (&optional (w nil))
  "give y position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish-and-read "senddata [winfo pointery ~a]" (if w (widget-path w) "."))
  )

(defun screen-mouse (&optional (w nil))
  "give the position of the mouse on screen as (x y) (if w is given, of the screen the widget w is displayed on)"
  (let ((vals  (format-wish-and-read "senddata \"([winfo pointerxy ~a])\"" (if w (widget-path w) "."))))
    (values (first vals) (second vals))))

(defun window-id (tl)
  "get the window id of the toplevel"
  (format-wish-and-read "senddatastring [winfo id ~a]" (widget-path tl))
  )

(defun window-width (tl)
  "give the width of the toplevel in pixels"
  (format-wish-and-read "senddata [winfo width ~a]" (widget-path tl))
  )

(defun window-height (tl)
  "give the height of the toplevel in pixels"
  (format-wish-and-read "senddata [winfo height ~a]" (widget-path tl))
  )

(defun window-x (tl)
  "give the x position of the toplevel in pixels"
  (format-wish-and-read "senddata [winfo rootx ~a];flush stdout" (widget-path tl))
  )

(defun window-y (tl)
  "give the y position of the toplevel in pixels"
  (format-wish-and-read "senddata [winfo rooty ~a];flush stdout" (widget-path tl))
  )

;;; misc functions

(defun focus (widget)
  (format-wish "focus ~a" (widget-path widget))
  widget)

(defun force-focus (widget)
  (format-wish "focus -force ~a" (widget-path widget))
  widget)

;;; Dialog functions

(defun choose-color (&key parent title initialcolor )
  (format-wish-and-read "senddatastring [tk_chooseColor ~@[ -parent ~A~]~@[ -title {~A}~]~@[ -initialcolor {~A}~]]" (when parent (widget-path parent)) title initialcolor)
  )

(defun get-open-file (&key (filetypes '(("All Files" "*")))
			   (initialdir (namestring *default-pathname-defaults*))
			   multiple parent title)
  (let ((files
        (with-output-to-string (s)
          (format s "{")
          (dolist (type filetypes)
            (let ((name (first type))
                  (wildcard (second type)))
              (format s "{{~a} {~a}} " name wildcard)))
          (format s "}"))))
    (if multiple
	(format-wish-and-read "senddatastrings [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~] -multiple 1 ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
		      files initialdir 
		      (and parent (widget-path parent)) title)
	(format-wish-and-read "senddatastring [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~]  ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
		      files initialdir 
		      (and parent (widget-path parent)) title))))

(defun get-save-file (&key (filetypes '(("All Files" "*"))))
  (let ((files
        (with-output-to-string (s)
          (format s "{")
          (dolist (type filetypes)
            (let ((name (first type))
                  (wildcard (second type)))
              (format s "{{~a} {~a}} " name wildcard)))
          (format s "}"))))
    (format-wish-and-read "senddatastring [tk_getSaveFile -filetypes ~a]" files)
    ))

(defun choose-directory (&key (initialdir (namestring *default-pathname-defaults*))
			      parent title mustexist)
  (format-wish-and-read "senddatastring [tk_chooseDirectory ~@[ -initialdir {~a}~]~@[ -parent ~a~]~@[ -title {~a}~]~@[ -mustexist ~a~]]" initialdir (and parent (widget-path parent)) title (and mustexist 1))
  )

(defvar *mb-icons* (list "error" "info" "question" "warning")
  "icon names valid for message-box function")

;;; see make-string-output-string/get-output-stream-string
(defun message-box (message title type icon &key parent)
  ;;; tk_messageBox function
  (format-wish-and-read "senddatastring [tk_messageBox -message {~a} -title {~a} -type ~(~a~) -icon ~(~a~)~@[ -parent ~a~]]" message title type icon (and parent (widget-path parent)))
  (read-keyword))


(defun ask-yesno(message &optional (title ""))
  (equal (message-box message title "yesno" "question") :yes))

(defun ask-okcancel(message &optional (title ""))
  (equal (message-box message title "okcancel" "question") :ok))

(defun do-msg(message  &optional (title "") parent)
  (message-box message title "ok" "info" :parent parent))

#|
-type predefinedType
              Arranges for a predefined set of buttons to be dis
              played. The following values are possible for  pre
              definedType:

              abortretryignore  Displays three buttons whose sym
                                bolic names are abort, retry  and
                                ignore.

              ok                Displays  one  button  whose sym
                                bolic name is ok.

              okcancel          Displays two buttons  whose  sym
                                bolic names are ok and cancel.

              retrycancel       Displays  two  buttons whose sym
                                bolic names are retry and cancel.

              yesno             Displays  two  buttons whose sym
                                bolic names are yes and no.

              yesnocancel       Displays three buttons whose sym
                                bolic  names are yes, no and can
                                cel.
     -icon iconImage
              Specifies an icon to display. IconImage must be one
              of the following: error, info, question or warning.
              If this option is not specified, then the info icon
              will be displayed.

|#

;;;

(defun cm (tree widget-path)
  (cond
   ((eq tree :separator)
    (send-wish (format nil "~A add separator" widget-path)))
   ((listp (second tree))
    (let ((newpath (format nil "~A.~A" widget-path (create-name))))
      (when (and (equal widget-path ".menubar")
		 (or (equal (first tree) "Help")
		     (equal (first tree) "help")
		     (equal (first tree) "Hilfe")))
	(setf newpath ".menubar.help"))
      (send-wish (format nil "menu ~A -tearoff 0" newpath))
      (send-wish (format nil "~a add cascade -label \"~a\" -menu ~a" widget-path (first tree) newpath))
      (dolist (entry (second tree))
	(cm entry newpath))))
   (t
    (let* ((name (create-name)))
      (add-callback name (second tree))		     
      (send-wish (format nil "~A add command -label {~A} -command {puts -nonewline  {(\"~A\")};flush stdout}" widget-path (first tree) name))
      ))))

(defun create-menu2 (menutree)
  (send-wish (format nil "menu .menubar -tearoff 0 -type menubar"))
  (dolist (e menutree)
    (cm e ".menubar"))
  (send-wish (format nil ". configure -menu .menubar"))
  )  


;;;; Visual error handlers

(defun error-popup (message title icon &key (allow-yesno-p t))
  (ecase (message-box message title
                     (if (and allow-yesno-p (find-restart 'continue))
                         "yesno"
                         "ok")
                     icon)
    (:yes (continue))
    ((:ok :no) (abort))))

(defun debug-popup (condition title)
  (ecase (message-box (format nil "~A~%~%Do you wish to invoke the debugger?"
			      condition)
		      title "yesno" "question")
    (:yes (cond (*debugger-hook*
                 (let ((hook *debugger-hook*)
                       (*debugger-hook* nil))
                   (funcall hook condition hook)))
                (t
                 (invoke-debugger condition))))
    (:no (abort))))

(defun show-error (error)
  (error-popup (format nil "~A~@[~%~%~A?~]" error (find-restart 'continue))
	       "Error" "error"))

(defun note-error (error)
  (declare (ignore error))
  (error-popup "An internal error has occured." "Error" "error"
               :allow-yesno-p nil))

(defun debug-error (error)
  (debug-popup error "Error"))

(defun show-warning (warn)
  (message-box (princ-to-string warn) "Warning" "ok" "warning"))

(defun debug-warning (warn)
  (debug-popup warn "Warning"))

(defun trivial-debugger (condition hook)
  (declare (ignore hook))
  (format *error-output* "~&An error of type ~A has occured: ~A~%"
	  (type-of condition) condition)
  #+sbcl (progn (sb-debug:backtrace most-positive-fixnum *error-output*)
                ;; FIXME - this should be generalized
		(unless (or (find-package :swank)
                            (find-package :fly))
                  (quit)))
  #+(or cmu scl)
  (progn (debug:backtrace most-positive-fixnum *error-output*)
         ;; FIXME - this should be generalized
         (unless (or (find-package :swank)
                     (find-package :fly))
           (ext:quit))))

;;;; Error handling

(defvar *ltk-default-debugger*
  '((fdefinition (find-symbol (symbol-name '#:debugger) :fly))
    (fdefinition (find-symbol (symbol-name '#:swank-debugger-hook)  :swank)))
  "A list of debuggers to try before falling back to the Lisp system's debugger.
  An item in this list may be a function, a symbol naming a function, or a
  complex form to evaluate.  If it is a complex form, it will be evaled inside
  an IGNORE-ERRORS, and should return a function, a symbol naming a function,
  or NIL.")

(defparameter *debug-settings-table*
  (copy-tree
   '(((0 :minimum) :handle-errors nil    :handle-warnings nil     :debugger nil)
     ((1 :deploy)  :handle-errors t      :handle-warnings nil     :debugger t)
     ((2 :develop) :handle-errors :debug :handle-warnings :simple :debugger t)
     ((3 :maximum) :handle-errors :debug :handle-warnings t       :debugger t))))

(defun debug-setting-keys (debug-setting)
  "Given a debug setting (see WITH-LTK for details), return a list of appropriate
   keyword arguments to pass to START-WISH."
  (let ((debug (if (numberp debug-setting)
                   (min 3 (max 0 (ceiling debug-setting)))
                   debug-setting)))
    (or (cdr (assoc (list debug) *debug-settings-table* :test #'intersection))
        (error "Unknown debug setting ~S" debug))))

(defun compute-error-handlers (handle-errors)
  (let ((nothing (constantly nil)))
    (ecase handle-errors
      ((t) (values #'show-error #'note-error))
      (:simple (values #'show-error nothing))
      (:debug (values nothing #'debug-error))
      ((nil) (values nothing nothing)))))

(defun compute-warning-handlers (handle-warnings)
  (let ((nothing (constantly nil)))
    (ecase handle-warnings
      ((t) (values #'show-warning #'show-warning))
      (:simple (values #'show-warning nothing))
      (:debug (values #'debug-warning #'debug-warning))
      ((nil) (values nothing nothing)))))

(defun compute-call-with-debugger-hook (debugger)
  "Return a function that will call a thunk with debugger-hook bound appropriately."
  (labels ((find-a-debugger ()
             (loop for attempt in *ltk-default-debugger*
                   when (typecase attempt
                                  (symbol (and (fboundp attempt) attempt))
                                  (function attempt)
                                  (list (ignore-errors (eval attempt))))
                     return it))
           (use-debugger (debugger thunk)
             (let* ((*debugger-hook* debugger)
                    #+sbcl (sb-ext:*invoke-debugger-hook* (constantly nil)))
               (funcall thunk)))
           (use-default-debugger (thunk)
             (let ((debugger (find-a-debugger)))
               (if debugger
                   (use-debugger debugger thunk)
                   (funcall thunk))))
	   (use-trivial-debugger (thunk)
             (use-debugger #'trivial-debugger thunk))
	   (use-custom-debugger (thunk)
             (use-debugger debugger thunk)))
    (case debugger
      ((t) #'use-default-debugger)
      ((nil) #'use-trivial-debugger)
      (t (if (or (functionp debugger)
		 (and (symbolp debugger)
		      (fboundp debugger)))
	     #'use-custom-debugger
	     (error "~S does not designate a function" debugger))))))

(defun make-condition-handler-function
    (&key handle-errors handle-warnings (debugger t) &allow-other-keys)
  "Return a function that will call a thunk with the appropriate condition handlers in place, and *debugger-hook* bound as needed."
  (multiple-value-bind (simple-error error)
      (compute-error-handlers handle-errors)
    (multiple-value-bind (simple-warning warning)
        (compute-warning-handlers handle-warnings)
      (let ((call-with-debugger-hook (compute-call-with-debugger-hook debugger)))
        (lambda (thunk)
          (funcall call-with-debugger-hook
                   (lambda ()
                     (handler-bind ((simple-error simple-error)
                                    (error error)
                                    (simple-warning simple-warning)
                                    (warning warning))
                       (funcall thunk)))))))))
       
;;; with-widget stuff
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun process-layout (line parent)
  (let ((class-name (first line))
	(instance-name (second line)))
    (multiple-value-bind (keyargs subwidgets)
	(do ((params (cddr line)) ; all other parameters to the widget/subwidget defs
	     (keywords+values nil)	; keyword args for the widget
	     (sublists nil))	; list of the subwidgets	      
	    ((null params) (values (reverse keywords+values) (reverse sublists)))
	  (cond ((listp (car params))
		 (dolist (subwidget (process-layout (pop params) instance-name))
		   (push subwidget sublists)))
		(t (push (pop params) keywords+values)
		   (push (pop params) keywords+values))))
      (cons
       (list instance-name
	     (append
	      (list 'make-instance (list 'quote class-name))
	      (if parent (list :master parent) nil)
	      keyargs))
       subwidgets))))

) ; end eval-when

(defmacro with-widgets (layout &rest body)
  (let* ((defs (process-layout layout nil))
	 (widgets (mapcar #'car defs)))
    `(let* ,defs
       (declare (ignorable ,@widgets))
       ,@body)))


(defmacro with-master (master &body body)
  `(let ((*current-master* ,master))
     ,@body
     *current-master*))

(defmacro with-parent (master &body body)
  `(let ((*current-master* ,master))
     ,@body
     *current-master*))
