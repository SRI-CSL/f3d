package provide qtk 1.0


msg "Loading layout.tcl..."

proc non_slaved_children {frame} {
    set children [widget_children $frame]
    foreach w [widget_children $frame] {
	if {[getp $w label_widget lw]} {
	    set children [ldelete $children $lw]
	}
	if {[getp $w doc_widget_p] || [getp $w grip_p docp]} {
	    set children [ldelete $children $w]
	}
    }   
    return $children
}

# args is options to grid command for each child
proc qgrid_2_column {frame args} {
    set children [non_slaved_children $frame]
#    puts "children: $children"
    set row 0
    foreach w $children {
	if {[getp $w label_widget lw]} {
	    eval {grid $lw -row $row -column 0 -sticky e} $args
	    eval {grid $w -row $row -column 1 -sticky news} $args
	} else {
	    eval {grid $w -row $row -column 0 -columnspan 2 -sticky news} $args
	}
	set wt 0 ; getp $w widget_weight wt
	set minheight 0 ; getp $w widget_minheight minheight
	grid rowconfigure $frame $row -weight $wt -minsize $minheight
	incr row
    }
    grid columnconfigure $frame 1 -weight 1
    #manage_frame_grip $frame
}

proc qgrid_1_column {frame args} {
    set children [non_slaved_children $frame]
    set row 0
    foreach w $children {
	if {[getp $w label_widget lw]} {
	    eval {grid $lw -row $row -column 0 -sticky w} $args
	    incr row
	}
	eval {grid $w -row $row -column 0 -sticky news} $args
	set wt 0 ; getp $w widget_weight wt
	set minheight 0 ; getp $w widget_minheight minheight
	grid rowconfigure $frame $row -weight $wt -minsize $minheight
#	if {[getp $w widget_minheight minheight]} {
#	    grid rowconfigure $frame $row -minsize $minheight
#	}
	incr row
    }
    grid columnconfigure $frame 0 -weight 1
#    tkwait visibility $frame
#    foreach w $children {
#	tkwait visibility $w
#    }
    #manage_frame_grip $frame
}

proc qgrid_1_row {frame args} {
    set children [non_slaved_children $frame]
    set col 0
    foreach w $children {
	eval {grid $w -column $col -row 0 -sticky news} $args
	set wt 0 ; getp $w widget_weight wt
	set minwidth 0 ; getp $w widget_minwidth minwidth
	grid columnconfigure $frame $col -weight $wt -minsize $minwidth
#	if {[getp $w widget_minheight minheight]} {
#	    grid rowconfigure $frame $row -minsize $minheight
#	}
	incr col
    }
    grid rowconfigure $frame 0 -weight 1
}

#  *********************  LAYOUT UTILS  *********************

proc pack_children {frame args} {
    if {$args == ""} {
	set args {-side top}
    }
    foreach w [widget_children $frame] {
# is this test needed?
	if { [getp $w nopack packval] == 0 } {
	    eval {pack $w} $args
    }   }
}
msg "...done loading layout.tcl"
