(in-package :cl-user)

(defvar *default-freedius-tk-path* (cl-user::system-source-path :f3d-tk))

(st::setenv "FREEDIUSTK" (namestring cl-user::*default-freedius-tk-path*))

(defpackage :lisptk
  (:use :common-lisp :lcl :qffi :lx :common-symbols)
  (:nicknames :tk)
	    	    
  (:export
	
   ;; tk-init
   "INIT-LISPTK"

   ;; tcl-eval
   "TCL-SCRIPT" "TCL-CMD" "TCL-EVAL" "TCL-EVAL-TO-LIST"
   "TCL-LIST-TO-LISP" "*TK-VERBOSE*"
 
   ;; tk-commands
   "TK-CONFIGURE" "TK-CGET" "TK-WM" "TK-DESTROY" "TK-LOAD-APP-DEFAULTS"
  
   ;; repl     
   "REPL" "QUIT-REPL" "PRINT-AND-SET-TOP-LEVEL-VARIABLES" "DO-EVENTS" 

   ;; tk-bindings
   "CREATE-TK-BINDING" "CREATE-TK-CALLBACK"
   
   ;; tk-callbacks
   "TK-CALLBACK" "INSTALL-TK-EVENT-HANDLER"

   ;; mouse-events 
   "ADD-MOUSE-EVENTS" "GET-MOUSE-EVENT-FUNCTION" "MOUSE-EVENT-BINDING"
   "MOUSE-EVENT-PROPERTY" "MAKE-MOUSE-EVENT-MAP" "MOUSE-EVENT-NAME"

   ;; tk-widget 
   "WIDGET-CLIDGET" "WIDGET" "WIDGET-NAMED" "WIDGET-PARENT" "MERGE-WIDGET-PATHNAME"
   "TEXT-WIDGET-XY-STRINGPOS"
   "SET-LISTBOX-ITEMS" "ADD-LISTBOX-ITEM"
   "GET-LISTBOX-SELECTED-ITEMS" "GET-LISTBOX-SELECTED-ITEM-INDICES"
   "WINDOW-WIDTH" "WINDOW-HEIGHT" "WINDOW-X" "WINDOW-Y"
   "WINDOW-ROOTX" "WINDOW-ROOTY" "WINDOW-EXISTS"
   "SCREEN-WIDTH" "SCREEN-HEIGHT"
   ;; widget-panel
   "QUIT-PANEL" "WIDGET-PANEL" "MAKE-WIDGET-PANEL" "POP-DOWN-PANEL" "POP-UP-PANEL"

   ;; tkgl 
;; moved to gl package   "GLMAKECURRENT" "GLSWAPBUFFERS" "COLOR-NAME-TO-GL"

   ;; menus
   "MENU-CHOOSE" "MENU-CHOOSE-INTERNAL" "MAKE-MENU" 
   
   ;; cvv
   "PANEL" "ITEM-CLASS"
   "CVV-ITEM" "CVV-WIDGET" "CVV-ITEM-VALUE" "WITH-CVV-ITEMS" "WIDGET-ITEM-VALUE"
   "CVV-PANEL" "MAKE-CVV-PANEL"
   "MAKE-CVV-ITEM" "MAKE-CVV-ITEM-CLIDGET" "MAKE-CVV-ITEM-WIDGET"
   "MAKE-CVV-FRAME-ITEM-LIST" "CVV-WIDGET"
   "GET-NAMED-ITEM" "GET-ITEM-VALUE" "SET-ITEM-VALUE"

   "WARP-POINTER" "SET-BLANK-CURSOR" "UNSET-BLANK-CURSOR"
   "ENABLE-NO-MOUSE-BUTTON-MOTION-CALLBACK"
   "DISABLE-NO-MOUSE-BUTTON-MOTION-CALLBACK"
   "WINDOW-SCREEN-POSITION"
   
  "SET-DOCUMENTATION" "GET-DOCUMENTATION" "DOCUMENTATION-WIDTH"
  )
  ;; moved to sysdef-tkgl.lisp
  ;;(:import-from :gl "GLMAKECURRENT" "GLSWAPBUFFERS" "COLOR-NAME-TO-GL")
		     
  )


;;(load-system-library "libtk8.6")
;;(load-system-library "libtcl8.6")
;;(load-system-library "liblisptk")

