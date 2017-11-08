package provide qtk 1.0

msg "Loading group-control.tcl..."
proc grid_info {w key} {
    if {[getp $w grid$key val]} { return $val}
    array set grid_info [grid info $w]
#    puts [list grid_info $w $key $grid_info($key)]
    return [putprop $w grid$key $grid_info($key)]
}

proc set_grid_row_weight {w wt} {
    grid rowconfigure [winfo parent $w] [grid_info $w -row] -weight $wt
}

proc set_grid_row_minheight {w minheight} {
    grid rowconfigure [winfo parent $w] [grid_info $w -row] -minsize $minheight 
}

proc sum_weights {parent} {
    set sumwt 0
    foreach w [grid slaves $parent] {
	if [getp $w widget_weight wt] {
	    incr sumwt $wt
    }   }
    return $sumwt
}  

proc sum_heights {widgets} {
    set sumheight 0
    foreach w $widgets {
	if {[winfo ismapped $w] && [getp $w doc_widget_p docp]==0 } {
	    incr sumheight [winfo height $w]
    }   }
    return $sumheight
}

# Fri Jan 30 1998 -- this stuff is broken -- all use of wm geometry screws
# things up including the toplevel resize handles.
proc restore_toplevel_size {w} {
    if {[getp $w previous_height previous_height]} {
	if {[string compare [winfo toplevel  $w] $w] == 0} {
	    # must be a toplevel widget
	    wm geometry $w [winfo width $w]x$previous_height
}   }   }

# Fri Jan 30 1998 -- this stuff is broken -- all use of wm geometry screws
# things up including the toplevel resize handles.
proc shrinkwrap_toplevel {w} {
    if {[string compare [winfo toplevel  $w] $w] != 0} {return }
    # must be a toplevel widget
#    $w configure -height [sum_heights $w]
    wm geometry $w [winfo width $w]x[sum_heights [winfo children $w]]
}

proc restore_toplevel_size {w} {
    if {[getp $w previous_height previous_height]} {
	if {[string compare [winfo toplevel  $w] $w] == 0} {
	    # must be a toplevel widget
#	    wm geometry $w [winfo width $w]x$previous_height
}   }   }


proc shrinkwrap_toplevel {w} {
    if {[string compare [winfo toplevel  $w] $w] != 0} {return }
    # must be a toplevel widget
#    $w configure -height [sum_heights $w]
#    wm geometry $w [winfo width $w]x[sum_heights [winfo children $w]]
}

# Bugs:  If minsize is to be used, unmap_group must set minsize to 0
#        and map_group must restore minsize.

proc unmap_group {gw} {
    if {[getp $gw group_widgets gwl]} {
	set parent [winfo parent $gw]
	putprop $parent previous_height [sum_heights [winfo children $parent]]
#	grid propagate $parent 0
	foreach wn $gwl {
	    set w [widget_named $parent $wn]
	    if  {[string compare [winfo parent $w] $parent] } {
		# handled scrolled widgets
		set w [winfo parent $w]
	    }
	    set_grid_row_weight $w 0
	    set_grid_row_minheight $w 0
	    grid remove $w
	    if {[winfo exists ${w}_grip] } {set grip_frm $w}

	    if {[getp $w label_widget lw]} {
		grid remove $lw
	}   }
	if {[sum_weights $parent] == 0} {
	    shrinkwrap_toplevel $parent
	}	
	if {[info exists grip_frm]} {
	    manage_group_grip $grip_frm
	}
}   }
	

proc map_group {gw} {
    if {[getp $gw group_widgets gwl]} {
	set parent [winfo parent $gw]
	restore_toplevel_size $parent
	foreach wn $gwl {
	    set w [widget_named $parent $wn]
	    if  {[string compare [winfo parent $w] $parent] } {
		# handled scrolled widgets
		set w [winfo parent $w]
	    }
	    grid $w
	    if {[winfo exists ${w}_grip] } {set grip_frm $w}
	    
	    if [getp $w widget_weight wt] {
		set_grid_row_weight $w $wt
	    } else {
#		set_grid_row_weight $w 0
	    }
	    if [getp $w widget_minheight minheight] {
		set_grid_row_minheight $w $minheight
	    }
	    if {[getp $w label_widget lw]} {
		grid $lw
	}   }

	if {[info exists grip_frm]} {
	    update
	    manage_group_grip $grip_frm
	}
}   }

proc manage_group_grip {frame} {
    if {[winfo exists ${frame}_grip]} {
	manage_frame_grips [winfo parent $frame] 0
}   } 

proc toggle_group_visibility {gw} {
#    puts [list toggle_group_visibility $gw [widget_value $gw]]
    if {[widget_value_0 $gw]} {
	map_group $gw
    } else {
	unmap_group $gw
}   } 

proc qgroupbutton {w args} {
    array set arg $args
    set gwl {} ; arefp_remove arg -group_widgets gwl
    if {! [arefp arg -initial_value val]} {
	set arg(-initial_value) 1
    }
    set w [eval {labelled_button checkbutton $w -indicatoron false -anchor w} [array get arg]]
    putprop $w group_widgets $gwl
    $w configure -command [list toggle_group_visibility $w]
    return $w
}

proc group_control_widgets {frm} {
    set gcw {}
    foreach w [winfo children $frm] {
	if {[getp $w group_widgets gwl]} {
	    lappend gcw $w
	}
    }
    return $gcw
}
	
proc initialize_group_control_state {frm} {
    if { $frm != "" } {
	foreach gcw [group_control_widgets $frm] {
	    if { $gcw != "" } {
		toggle_group_visibility $gcw
	    }
	}
    }
}


proc compute_grid_column0_max_width {frm} {
    update
    # must call update in order for grid widths to be defined
    set maxwidth 0
    foreach w [winfo children $frm] {
	if {[winfo class $w] == "Frame"} {
	    set col1_slaves [grid slaves $w -column 1]
	    if {$col1_slaves != {}} {
		set wid [winfo x [nth 0 $col1_slaves]] 
		if {$wid > $maxwidth} {set maxwidth $wid}
    }   }   }
    return $maxwidth
} 

proc set_grid_column0_width {frm width} {
    foreach w [winfo children $frm] {
	if {[winfo class $w] == "Frame" && [grid slaves $w -column 1]!={}} {
	    grid columnconfigure $w 0 -minsize $width 
    }   }
    return $width
} 

proc compute_grid_column0_max_width {frm} {
    update
    # must call update in order for grid widths to be defined
    set maxwidth 0
    foreach w [winfo children $frm] {
	set col1_slaves [grid slaves $w -column 1]
	if {$col1_slaves != {} } {
	    set wid [winfo x [nth 0 $col1_slaves]] 
	    if {$wid > $maxwidth} {set maxwidth $wid}
    }   }   
    return $maxwidth
} 

proc set_grid_column0_width {frm width} {
    foreach w [winfo children $frm] {
	if {[grid slaves $w -column 1] != {} } {
	    grid columnconfigure $w 0 -minsize $width 
    }   }
    return $width
} 

proc grid_uniform_column0_width {frm} {
    set_grid_column0_width $frm [compute_grid_column0_max_width $frm]
}

msg "...done loading group-control.tcl"
