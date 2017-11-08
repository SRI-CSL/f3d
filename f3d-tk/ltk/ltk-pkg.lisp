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

(defpackage :ltk
  (:use :common-lisp
        #+(or :cmu :scl) :ext
	#+:sbcl :sb-ext
	))

(in-package :ltk)

(eval-when (eval load compile)

(defparameter *ltk-widget-symbols*
  '(button check-button entry frame label labelframe 
    pulldownmenu-button radio-button scale scrollbar widget))

(defparameter *ltk-export-symbols*
  `(ltktest                           
    *ltk-version*
    *cursors*
    *debug-tk*
    *break-mainloop*
    *exit-mainloop*
    *init-wish-hook*
    *mb-icons*
    *tk*
    *wish*
    wish-stream
    *wish-args*
    *wish-pathname*
    *default-ltk-debugger*
    add-callback
    add-pane
    add-separator
    after
    after-cancel
    after-idle
    append-text
    append-newline
    ask-okcancel
    ask-yesno
    background
    bbox
    bell
    bind
    calc-scroll-region
    canvas
    canvas-line
    canvas-oval
    canvas-polygon
    canvas-rectangle
    canvas-text
    canvas-image
    canvas-arc
    canvas-bbox
    canvasx
    canvasy
    cget
    choose-color
    choose-directory
    clear-text
    clear
    clipboard-append
    clipboard-clear
    clipboard-get
    command
    coords
    configure
    create-arc
    create-bitmap
    create-image
    create-line
    create-line*
    create-menu2
    create-oval
    create-polygon
    create-rectangle
    create-text
    create-window
    debug-setting-keys
    defargs
    deiconify
    destroy
    do-execute
    do-msg
    entry-select
    exit-wish
    event
    event-x
    event-y
    event-keycode
    event-char
    event-mouse-button
    event-root-x
    event-root-y
    focus
    force-focus
    forget-pane
    format-wish
    geometry
    get-open-file
    get-save-file
    grab
    grab-release
    grid
    grid-columnconfigure
    grid-configure
    grid-forget
    grid-rowconfigure
    iconify
    iconwindow
    image-load
    image-setpixel
    cursor-index
    input-box
    insert-object
    interior
    itembind
    itemconfigure
    itemdelete
    itemmove
    itemlower
    itemraise
    listbox
    listbox-append
    listbox-clear
    listbox-configure
    listbox-get-selection
    listbox-nearest
    listbox-select
    load-text
    lower
    mainloop
    make-canvas
    make-frame
    make-image
    make-label
    make-menu
    make-menubar
    make-menubutton
    make-menucmdbutton
    make-scrollbar
    make-scrolled-canvas
    make-text
    make-toplevel
    make-line
    make-oval
    make-polygon
    make-rectangle
    master
    maxsize
    menu
    menubar
    menubutton
    menucheckbutton
    menu-delete
    menuradiobutton
    message
    message-box
    minsize
    move
    move-all
    normalize
    on-close
    on-focus
    pack
    pack-forget
    pack-propagate
    paned-window
    photo-image
    place
    place-forget
    popup
    postscript
    process-events
    raise
    read-event
    save-text
    screen-height
    screen-height-mm
    screen-mouse
    screen-mouse-x
    screen-mouse-y
    screen-width
    screen-width-mm
    scrolled-canvas
    scrolled-frame
    scrolled-listbox
    scrolled-text
    scrollregion
    see
    send-wish
    set-coords
    set-coords*
    set-geometry
    set-geometry-wh
    set-geometry-xy
    set-wm-overrideredirect
    spinbox
    start-wish
    style
    tag-bind
    tag-configure
    text
    textbox
    tkobject
    toplevel
    value
 ;   widget
    widget-path
    window-height
    window-id
    window-width
    window-x
    window-y
    make-ltk-connection
    widget-class-name
    with-ltk
    call-with-ltk
    with-remote-ltk
    with-widgets
    withdraw
    wm-title
    wm-state
    with-master with-parent
    pulldownmenu-button
    make-pulldownmenu-button
    ))

) ; end eval-when



(export *ltk-export-symbols* :ltk)

(import '(lx::get-prop))


(defpackage :ltk-user
  (:use :common-lisp :ltk)) 

