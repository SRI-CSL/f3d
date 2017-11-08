package provide qtk 1.0

msg "Loading TILE version of qwidgets.tcl..."

foreach class [list Label Button Checkbutton Radiobutton Menubutton Entry Listbox Text Scale \
                    TLabel TButton TCheckbutton TRadiobutton TMenubutton TEntry TScale] {
    set class_bindtags($class) [list show_widget_doc]
}

set class_bindtags(Toplevel) [list manage_toplevel]
set class_bindtags(Entry) [list manage_qentry show_widget_doc]
set class_bindtags(TEntry) [list manage_qentry show_widget_doc]

proc qet_qwidget_bindingtags  {w} {
    global class_bindtags
    set class [winfo class $w]
    if {[info exists class_bindtags($class)]} {
	return $class_bindtags($class)
    } else {
	return {}
    }
}

# This is called from labelled_widget
proc set_bindingtags  {w} {
    bindtags $w [concat qwidget \
	    [qet_qwidget_bindingtags $w] \
	    [bindtags $w] \
	    qwidget_after]	    
}

# *********************  QCME WIDGET MAKERS  *********************

# This handles special case where parent is .
proc merge_widget_pathname {parent name} {
    global default_parent
    if {[string range $name 0 0] == "."} {
	return $name
    }
    if {$parent == {}} {set parent $default_parent}
    if {$parent == "."} {return .$name}
    return $parent.$name
}


proc widget_tree_int {w} {
    upvar #0 widget_tree tree
    foreach w2 [winfo children $w] {
	lappend tree $w2
	widget_tree_int $w2
}   }

proc widget_tree {w} {
    set_global widget_tree {}
    widget_tree_int $w
    return [get_global widget_tree]
}

proc widget_name_tree_int {w} {
    upvar #0 widget_tree tree
    foreach w2 [winfo children $w] {
	lappend tree [string toupper [widget_name $w2]] $w2
	widget_name_tree_int $w2
}   }

proc widget_name_tree {w} {
    set_global widget_tree {}
    widget_name_tree_int $w
    return [get_global widget_tree]
}

proc widget_name_tree {w} {
    set_global widget_tree {}
    widget_name_tree_int $w
    set tree [get_global widget_tree]
    set_global widget_tree {}
    return $tree
}

set default_scrollbar_width 10

# arg keys are -left -right -top -bottom
# arg values are empty or the full arg list for making the scrollbar
proc make_scrollers {frm w args} {
    set left 0; set right 0; set top 0; set bottom 0; set itemcol 0; set itemrow 0;
    set name [widget_name $w]
    foreach arg $args {
	switch -exact -- $arg {
	    left {set left 1; set itemcol 1}
	    right {set right 1}
	    top {set top 1; set itemrow 1}
	    bottom {set bottom 1}
    }   }
    if {$left || $right} {
	set y_scroller $frm.${name}_yscroll
	scrollbar $y_scroller -orient vertical \
		-width [get_global default_scrollbar_width] \
		-command [list $w yview]
	$w configure -yscrollcommand [list $y_scroller set]
    }
    if {$top || $bottom} {
	set x_scroller $frm.${name}_xscroll
	scrollbar $x_scroller -orient horizontal \
		-width [get_global default_scrollbar_width] \
		-command [list $w xview]
	$w configure -xscrollcommand [list $x_scroller set]
# for some reason I get height = 1 from $frm.xscroll
# probably because it hasn't been managed using grid command
#	$frm configure	-height [winfo height $x_scroller]
#	$frm configure -height 30
    }

    if {$top} {grid $x_scroller -sticky news -column $itemcol}
    if {$left} {grid $y_scroller $w -sticky news}
    if {$right} {grid $w $y_scroller -sticky news}
    if {$bottom} {grid $x_scroller -sticky news -column $itemcol}

    $frm configure -height 40
    grid rowconfigure $frm $itemrow -weight 1
    grid columnconfigure $frm $itemcol -weight 1
    return $frm
}

proc qwidget_class {w} {
    if {[getp $w qwidget_class cls] == 0} {
	set cls q
	append cls [winfo class $w]
    }
    return $cls
}
    
proc labelled_widget {class w args} {
    global default_parent
    array set arg $args
    set parentp [arefp_remove arg -parent parent]
    set scroll_p [arefp_remove arg -scroll scroll_args]
    set qwidgetclass_p [arefp_remove arg -qwidget_class qwidgetclass]
    if {$scroll_p} {
	if {[string range $w 0 0] == "."} {
	    error {scrolled widget must have relative path}
	}
	if {! $parentp} {set parent $default_parent}
	set top $parent.${w}_frm
	set w $top.$w
    } elseif {[string range $w 0 0] != "."} {
	if {$parentp} {
	    set w $parent.$w
	} elseif {[info exists default_parent]} {
	    set w $default_parent.$w
	} else  {
	    error {illegal widget path}
	}  
	set top $w
    } else {
	set top $w
    }
    
    if {[arefp_remove arg -labeltext labeltext]} {
	# Create the widget label	
	putprop $top label_widget [qlabel ${top}_label -text $labeltext -justify right]
	if {[arefp_remove arg -labeldoc labeldoc]} {
	    add_widget_doc [getprop $top label_widget] $labeldoc
	}	
    } elseif {[arefp_remove arg -labelwidget lw]} {
	putprop $top label_widget $lw
    } elseif {[arefp_remove arg -labelargs labelargs]} {
	putprop $top label_widget [eval {qlabel ${top}_label} $labelargs]
    }

    if {[arefp_remove arg -documentation doc]} {add_widget_doc $w $doc}
    if {[arefp_remove arg -weight weight]} {
	putprop $top widget_weight $weight
    }
    if {[arefp_remove arg -minheight minheight]} {
	putprop $top widget_minheight $minheight
    }
    if {[arefp_remove arg -minwidth minwidth]} {
	putprop $top widget_minwidth $minwidth
    }
    set initial_value_p [arefp_remove arg -initial_value val]
    if {$scroll_p} {qframe $top}
    # Create the widget
    eval {$class $w} [array get arg]
    if {$scroll_p} {eval {make_scrollers $top $w} $scroll_args}
    if {$initial_value_p} {set_widget_value $w $val}
    set_bindingtags $w
    if {$qwidgetclass_p ==1} {
	putprop $w qwidget_class $qwidgetclass
    }
    return $w
}

proc set_item_label {w labeltext} {
    set label ${w}_label
    if {[winfo exists $label]} {
	$label configure -text $labeltext
    }
}

proc qtoplevel {w args} {
    array set arg $args
    set titlep [arefp_remove arg -title title]
    set w [eval {labelled_widget toplevel $w} [array get arg]]
    if {$titlep} {
	wm title $w $title
    }
    wm protocol $w "_XA_WM_UNMAP_WINDOW" [list wm withdraw $w]
# Alternative:  iterate over children of . to get this info
# This next moved to group-iconification.tcl
#    putprop . slaved_frames [lappend [getprop . slaved_frames] $w]

    return $w
}

proc qframe {w args} {
    eval {labelled_widget ttk::frame $w} $args
}

proc qlabel {w args} {
    eval {labelled_widget ttk::label $w} $args
}

proc qentry {w args} {
    array set arg $args
    set formatp [arefp_remove arg -format format]
    set oformatp [arefp_remove arg -oformat oformat]
    set initial_value_p [arefp_remove arg -initial_value val]
    set w [eval {labelled_widget ttk::entry $w} [array get arg]]
    if {$formatp} {    
	putprop $w format $format
    }
    if {$oformatp} {    
	putprop $w oformat $oformat
    }
    if {$initial_value_p} {set_widget_value $w $val}
    return $w	
}

proc qstring {w args} {
    eval {qentry $w -qwidget_class qstring -qwidget_class qstring} $args
}

proc qinteger {w args} {
    eval {qentry $w -format %i -qwidget_class qinteger} $args
}

proc qfloat {w args} {
    eval {qentry $w -format %f -qwidget_class qfloat} $args
}


#proc qtext {w args} {
#    eval {labelled_widget text $w -qwidget_class qtext} $args
#}

proc qtext {w args} {
    set w [eval {labelled_widget text $w -qwidget_class qText} $args]
    bindtags $w [concat [bindtags $w] text_after]
    return $w
}

proc qseparator {w args} {
    array set arg $args
    set height 2; arefp_remove arg -height height
    -highlightcolor black -highlightthickness 1
    eval {labelled_widget qframe $w -height $height -background black} [array get arg]
}

proc qseparator {w args} {
    array set arg $args
    set height 0; arefp_remove arg -height height
    
    eval {labelled_widget qframe $w -highlightbackground black -highlightthickness 1 \
	    -height $height} [array get arg]
}

proc qcanvas {w args} {
    eval {labelled_widget canvas $w} $args
}

proc labelled_button {class w args} {
    array set arg $args
    set cmd_p [arefp arg -command cmd]
    set w [eval {labelled_widget $class $w} [array get arg]]
    if {! $cmd_p} {
# default command when none is specified - qcme_callback with full widget name	
	$w configure -command [list qcme_callback $w button]
    }
    return $w
}

proc qbutton {w args} {
    eval {labelled_button ttk::button $w} $args
}
proc qflatbutton {w args} {
    eval {labelled_button ttk::button $w -borderwidth 0 -highlightthickness 0}  $args
}
 

proc qcheckbutton {w args} {
    eval {labelled_button ttk::checkbutton $w} $args
}

proc qtogglebutton {w args} {
    array set arg $args
    set var_p [arefp_remove arg -variable varName]
    if {! $var_p} {set varName $w}
    set w [eval {labelled_button ttk::checkbutton $w -style Toolbutton  -variable $varName} [array get arg]]
    
    putprop $w variable $varName
    return $w
}

proc qtogglebutton {w args} {
    set w [eval {labelled_button ttk::checkbutton $w -style Toolbutton} $args]
    if {[$w cget -variable] == ""} {$w configure -variable $w}
    putprop $w variable [$w cget -variable]
    return $w
}

proc qtogglebutton {w args} {
    set w [eval {labelled_button ttk::checkbutton $w -style Toolbutton } $args]
    if {[$w cget -variable] == ""} {$w configure -variable $w}
    putprop $w variable [$w cget -variable]
    return $w
}

proc qradiobutton {w args} {
    array set arg $args
    set value [widget_name $w]; arefp_remove arg -value value
    eval {labelled_button ttk::radiobutton $w -value $value} [array get arg] 
}

proc qmenubutton {w args} {
    eval {labelled_widget ttk::menubutton $w} $args
}

proc qmenubutton0 {w args} {
    eval {labelled_widget menubutton $w} $args
}

proc qoptionbutton {w args} {
    eval {labelled_widget ttk::menubutton $w -qwidget_class qoptionbutton} $args
}

# ttk::scale does not support -showvalue option.  Do not use it for now
proc qslider {w args} {
    set initial_value_p [arefp_remove arg -initial_value val]
    set w [eval {labelled_widget scale $w -qwidget_class qslider } $args]
    if {$initial_value_p} {set_widget_value $w $val}
    $w configure -command [list qcme_callback $w entry_changed]
    if {[$w cget -variable] == ""} {$w configure -variable $w}
    putprop $w variable [$w cget -variable]
    return $w
}



# *********************  QCME HANDLERS *********************

set qcme_popup_state {}

# This is used for button presses on canvases 
# Handles popup menu on right mouse button
# Set the popup_menu property of the canvas to enable.
# This remembers where the menu was popped up so that %w %x and %y 
# in the eventual call to qcme_callback will be relative to the button-press
# that invoked the popup menu.
# 

#proc qcme_buttonpress_handler {w button state xroot yroot x y} {
#    if {$button == 3 && $state == 0 && [getp $w popup_menu pumenu]} {
#        set_global qcme_popup_state [list $w $x $y]
#	tk_popup $pumenu $xroot $yroot {}
#    } else {
#        set_global qcme_mouse_state {}
#	qcme_mouse_handler $w buttonpress $button $state $x $y
#}   }

proc qcme_buttonpress_handler {w button state xroot yroot x y} {
#    puts [list qcme_mouse_handler $w $button $state]
    if {[getp $w popup_menu(${button},${state}) pumenu]} {
        set_global qcme_popup_state [list $w $x $y]
	tk_popup $pumenu $xroot $yroot {}
    } else {
        set_global qcme_mouse_state {}
	qcme_mouse_handler $w buttonpress $button $state $x $y
}   }

# buttonpress on a canvas or glwin
# w button state x y
proc qcme_mouse_handler {w event args} {
    eval {qcme_callback $w $event } $args
} 


proc qcme_keypress_handler {w action keysym char args} {
    msg "Key pressed. $w $keysym $char $args"
    qcme_callback $w $action $keysym $char
}

# Do this specifically on canvases you build
#bind Canvas <ButtonPress> {qcme_mouse_handler %W %b %s %X %Y %x %y ; break}

# Remove all property list infomation about a widget when it is destroyed.
# Need to handle submenus under cascade buttons that are

# FIXME LHQ Sun Nov  7 2004 - need to unset variables associated with toggle-buttons, radio buttons, ...

if {1} {

proc destroy_qwidget_callback {w} {
#    puts [list destroy_qwidget_callback $w]
    # Nothing appears to add any slaved_widgets, thus this is bogus
    foreach slave [getprop $w slaved_widgets] {
	destroy_qwidget_callback $slave
	destroy $slave	
    }
    qcme_callback $w destroy
    remprop_all $w
}

} else {

# experiment Wed May 24 2006 in attempt to fix x11_lisp_error_handler
# (X protocol error BadWindow (invalid Window parameter)) with destroy.
# Doesn't appear to affect the bug.


proc destroy_inf_qwidget_callback {w} {
    foreach slave [getprop $w slaved_widgets] {
	destroy_inf_qwidget_callback $slave
    }
    remprop_all $w
}

proc destroy_qwidget_callback {w} {
#    puts [list destroy_qwidget_callback $w]
    foreach slave [getprop $w slaved_widgets] {
	destroy_inf_qwidget_callback $slave
    }
    qcme_callback $w destroy
    remprop_all $w
}
}

bind qwidget <Destroy> {
    destroy_qwidget_callback %W
}

proc verify_qentry_format {w} {
    set string [$w get]
    if {[getp $w format format]} {
	if {[scan $string $format%s val rest] == 1} {
	    if {[getp $w background background]} {
		$w config -background $background
		remprop $w background
	    }
	    return 1; # win
	} else {
	    if {! [getp $w background background]} {
		putprop $w background [$w cget -background]
	    }
	    $w config -background #ea7680
	    return 0; # fail
    }   } else {
	return 1; # win
}   }

bind manage_qentry <Return> {
    if {[verify_qentry_format %W] == 1} {
	if {[getp %W oformat oformat]} {
#	    set_entry_value %W [%W get]
	    set_entry_value %W [entry_value %W]
	}
	qcme_callback %W entry_changed
}   }   



# redefine this from lisp or your application
# QCME replaces this with a call to lisptk::qcme-callback
proc qcme_lisp_callback {w event otherargs} {
#    puts [list qcme_lisp_callback $w $event $otherargs]
}

# generic callback
proc qcme_callback {w event args} {
    set popup_state [get_global qcme_popup_state]
    if { [llength $popup_state] > 0 } {
	qcme_lisp_callback $w $event [concat $args $popup_state]
    } else {	
	qcme_lisp_callback $w $event $args
    }
}

proc qcme_callback {w event args} {
    set popup_state [get_global qcme_popup_state]
    set_global qcme_popup_state {}
    if { [llength $popup_state] > 0 } {
#	puts $args
#	puts [concat $args $popup_state]
	qcme_lisp_callback $w $event [concat $args [list $popup_state]]
    } else {	
	qcme_lisp_callback $w $event $args
    }
}

#proc qcme_callback {w event args} {
#    qcme_lisp_callback $w $event $args
#}


proc initialize_qwidgets {} {
}


if {0} {
qfloat .e2; pack .e2
verify_qentry_format .e2 
getp .e2 background foo
.e2 cget -background
.e2 config -background
.e2 config -background #d9d9d9
destroy .e2
}

# LHQ Mon May 26 2003  I believe this next has no effect.  
# The actual bindings are in toglwin.tcl


# Bindings for image_pane which is used in place of Togl widget.

if {1} {
    bind image_pane <ButtonPress> {qcme_mouse_handler %W buttonpress %b %s %x %y ; break}
    bind image_pane <ButtonRelease> {qcme_mouse_handler %W buttonrelease %b %s %x %y ; break}
    bind image_pane <B1-Motion> {qcme_mouse_handler %W motion %b %s %x %y ; break}
    bind image_pane <B2-Motion> {qcme_mouse_handler %W motion %b %s %x %y ; break}
    bind image_pane <B3-Motion> {qcme_mouse_handler %W motion %b %s %x %y ; break}
    bind image_pane <KeyPress> {qcme_keypress_handler %W keypress %K %A %s; break}
}





msg "...done loading qwidgets.tcl"



