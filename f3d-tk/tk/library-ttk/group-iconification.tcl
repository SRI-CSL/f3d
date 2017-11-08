package provide qtk 1.0

msg "Loading group-iconification.tcl..."
# Bugs and limitations:
# Need to write some C code to get the window stacking order from X11
# so that windows are correctly stacked when deiconified.


proc group_leader_iconified {w} {
    set withdrawn_slaves {}
    foreach w2 [getprop $w slaved_frames] {
	set state [wm state $w2]
	if {$state == "normal" || $state == "iconic"} {
	    lappend withdrawn_slaves $w2 $state
	    putprop $w2 old_geometry [wm geometry $w2]
	    if {$state == "iconic"} {wm deiconify $w2}
	    wm withdraw $w2
    }   }
    putprop $w withdrawn_slaves $withdrawn_slaves
}   

proc group_leader_deiconified {w} {
    foreach {w2 state} [getprop $w withdrawn_slaves] {
	
	if {$state == "normal"} {
	    wm deiconify  $w2
	} else {
	    wm iconify $w2
	}   
	if {[getp $w2 old_geometry geom]} {
	    wm geometry $w2 $geom
}   }   }

# both toplevel and leader should be toplevel widgets.
proc set_group_iconification {toplevel {leader "."}} {
    if {![getp $leader slaved_frames slaves]} {
	bind $leader <Unmap> {group_leader_iconified %W}
	bind $leader <Map> {group_leader_deiconified %W}
	set slaves {}
    }
    lappend slaves $toplevel
    putprop $leader slaved_frames $slaves
}


proc toplevel_widgets_with_state {state} {
    set toplevels {}
    foreach w [winfo children  .] {
	if {[winfo class $w] == "Toplevel" \
		&& [string compare [wm state $w] $state] == 0} {
	    lappend toplevels $w
    }   }
    return $toplevels
}
	    
msg "...done loading group-iconification.tcl"
