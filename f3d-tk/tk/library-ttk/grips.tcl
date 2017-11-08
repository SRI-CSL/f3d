package provide qtk 1.0

msg "Loading grips.tcl..."
# grip is placed at bottom of frame which is resized using the grip
# grip is managed by place manager
proc qgrip {frame args} {
    array set arg $args
    set group_control_widgetp [arefp_remove arg -button button]
    set height 0; arefp_remove arg -height height
    set gripargs {}; arefp_remove arg -gripargs gripargs
    set parent [winfo parent $frame]
    array set griparg $gripargs
    set grip_width 6; arefp_remove arg -width grip_width
    set grip_height 6; arefp_remove arg -height grip_height
    set grip [qframe ${frame}_grip -width $grip_width -height $grip_height -bg black \
	    -cursor crosshair]
    set frame_frame [winfo parent $frame]	
    set grips [getprop $frame_frame grips]
    lappend grips $grip $frame
    putprop $frame_frame grips $grips
    putprop $grip grip_p 1
    bind $grip <ButtonPress-1> [list start_grip_drag $grip $frame %Y]
    bind $grip <ButtonRelease-1> [list stop_grip_drag $grip $frame]
    bind $grip <B1-Motion> [list move_grip $grip %Y]
#    bind $grip <Double-1> [list toggle_grip_state $grip $frame]
    bind $grip  <ButtonPress-3> [list toggle_grip_state $grip $frame]
    bind $frame <Configure> [list manage_grips $frame_frame ]

    if {$group_control_widgetp} {
	putprop $grip group_control_widget $button
	$button configure -command [list toggle_grip_state $grip $frame]
    }
    return $grip
}

proc initialize_grips {frame} {
    #update
    foreach {grip subframe} [getprop $frame grips] {
	if {[getp $grip group_control_widget gcw] && [winfo ismapped $gcw]} {
	    # unmap all of the "group" button widgets 
	    grid remove $gcw
    }   }
    # Update is needed before setting grips_are_managed, otherwise manage_grips
    # will cause thrashing for each grip in the frame.
    update
    foreach {grip subframe} [getprop $frame grips] {
	place_grip $grip $subframe
    }
    putprop $frame grips_are_managed 1
}

proc initialize_grips2 {frame visible_grips} {
    #update
    foreach {grip subframe} $visible_grips {
	if {[getp $grip group_control_widget gcw] && [winfo ismapped $gcw]} {
	    # unmap all of the "group" button widgets 
	    grid remove $gcw
    }   }
    foreach {grip subframe} [getprop $frame grips] {
	if {[getp $grip group_control_widget gcw] && [winfo ismapped $gcw]} {
	    # set subframe heights to 1 ("iconify" the subframe)
	    toggle_grip_state $grip $subframe
    }   }
    # Update is needed before setting grips_are_managed, otherwise manage_grips
    # will cause thrashing for each grip in the frame.
    update
    foreach {grip subframe} $visible_grips {
	place_grip $grip $subframe
    }
    putprop $frame grips_are_managed 1
}

set grip_x_position .01

proc place_grip {grip frame} {
    set parent [winfo parent $grip]
	set gripy [expr [winfo y $frame] \
		      + [winfo height $frame] \
		      - [winfo height $grip] ]
    place $grip -in $parent -relx [get_global grip_x_position] -y $gripy
    raise $grip
}

# called from frame <Configure> binding
proc manage_grips {frame} {
    if {[getp $frame grips_are_managed]} {
	update
	foreach {grip subframe} [getprop $frame grips] {
	    update_grip_state $grip $subframe
}   }   }


proc update_grip_state {grip frame} {
    if {![getp $grip group_control_widget gcw]} {
	place_grip $grip $frame
    } elseif {[winfo height $frame] > 1} {
	grid remove $gcw
	place_grip $grip $frame
    } else {
	place forget $grip
	grid $gcw
}   }

proc start_grip_drag {grip frame rooty} {  
    if {[grid propagate $frame] == 1} {
	grid propagate $frame false
    }
    set_global drag_start_y [expr $rooty - [winfo y $grip]]
}

proc move_grip {grip rooty} {
    set gripy [expr $rooty - [get_global drag_start_y]]
    place $grip -relx [get_global grip_x_position] -y $gripy
    }

proc stop_grip_drag {grip frame} {
    set height [expr [winfo y      $grip] \
	           + [winfo height $grip] \
		   - [winfo y      $frame]]
    if {$height < 1} {set height 1}
    $frame configure -height $height
    unset_global drag_start_y
} 




proc toggle_grip_state {grip frame} {
    if {[grid propagate $frame] == 1} {
	grid propagate $frame false
    }
#    if {![winfo ismapped $frame]} {puts [list toggle_grip_state $frame NOT MAPPED]}

    if {![winfo ismapped $frame] || [winfo height $frame] < 2} {
	$frame configure -height [getprop $frame previous_height]
#	puts [list toggle_grip_state $frame -height [getprop $frame previous_height]]
    } else {
	putprop $frame previous_height [winfo height $frame]
	$frame configure -height 1
}   }

proc toggle_grip_state {grip frame} {
    if {[grid propagate $frame] == 1} {
	grid propagate $frame false
    }
#    if {![winfo ismapped $frame]} {puts [list toggle_grip_state $frame NOT MAPPED]}

    if {[winfo height $frame] < 2} {
	$frame configure -height [getprop $frame previous_height]
    } else {
	putprop $frame previous_height [winfo height $frame]
	$frame configure -height 1
}   }

msg "...done loading grips.tcl"
