package provide qtk 1.0

#tkColorDialog
msg "Loading show-widget-doc.tcl..."

set show_documentation_widget_args \
	[list -justify left -borderwidth 0 -highlightthickness 0 -highlightbackground black]

set show_documentation_fallback_to_widget_name 0

proc cancel_delayed_show_widget_documentation {} {
    global delayed_show_widget_documentation_after_handle
    if {[info exists delayed_show_widget_documentation_after_handle] == 1} {
# puts [list after cancel $delayed_show_widget_documentation_after_handle]
	after cancel $delayed_show_widget_documentation_after_handle
	unset delayed_show_widget_documentation_after_handle
    }
}
 
set show_widget_documentation_delay_ms 600

proc delayed_show_widget_documentation {w} {
    global show_widget_documentation_delay_ms
    global delayed_show_widget_documentation_after_handle
    cancel_delayed_show_widget_documentation
#    remove_doc_widget $w
    set delayed_show_widget_documentation_after_handle \
	    [after $show_widget_documentation_delay_ms [list show_documentation $w]]
}


bind show_widget_doc <Enter> { delayed_show_widget_documentation %W } 
bind show_widget_doc <Leave> { remove_doc_widget %W } 

proc add_widget_doc {w string} {
    putprop $w widget_documentation $string
}


proc get_show_documentation_widget {w} {
    if {[global_boundp doc_widget]} {return [get_global doc_widget]}

    toplevel .doc_widget_frame
    wm overrideredirect .doc_widget_frame 1
    wm withdraw .doc_widget_frame
    set docw [eval {label .doc_widget_frame.doc_widget} \
	    [get_global show_documentation_widget_args]]
    pack $docw
    putprop $docw doc_widget_p 1
    set_global doc_widget $docw
}

proc place_documentation_widget {docw w x y} {
    set rootx [expr [winfo rootx $w] + $x]
    set rooty [expr [winfo rooty $w] + $y]
    set doc_frame [winfo toplevel $docw]
    wm geometry $doc_frame "+${rootx}+${rooty}"
    wm deiconify $doc_frame
    raise $doc_frame
    update
}

proc show_documentation {w} { 
#    global widget_documentation
    global show_documentation_fallback_to_widget_name
    set docw [get_show_documentation_widget $w]
    if {$docw == $w} { return }
    if {! [getp $w widget_documentation string]} { 
	if {$show_documentation_fallback_to_widget_name} {
	    set string $w
	} else { return }
    }
    set wheight [winfo height $w]
    $docw configure -text $string
    place_documentation_widget $docw $w 0 $wheight
}   

proc remove_doc_widget {w} {
    cancel_delayed_show_widget_documentation
    set docw [get_show_documentation_widget $w]
    set doc_frame [winfo toplevel $docw]
    wm withdraw $doc_frame
}


msg "...done loading show-widget-doc.tcl"
