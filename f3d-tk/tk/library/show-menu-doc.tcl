package provide qtk 1.0

msg "Loading show-menu-doc.tcl..."
proc enable_menu_doc {menu} {
    bind $menu <<MenuSelect>> {delayed_show_menu_documentation %W}
}

proc remove_menu_doc_widget {w} {
    cancel_delayed_show_widget_documentation
    place forget [get_show_documentation_widget $w]
    place forget [get_show_documentation_widget [winfo parent $w]]  
}

proc delayed_show_menu_documentation {w} {
    global show_widget_documentation_delay_ms
    global delayed_show_widget_documentation_after_handle
    cancel_delayed_show_widget_documentation
    remove_menu_doc_widget $w
    set delayed_show_widget_documentation_after_handle \
	    [after $show_widget_documentation_delay_ms \
	    [list show_menu_documentation $w]]
}

# This version places the doc widget under the selected menu entry.
# Has problems:  width of doc string limited to width of menu
proc show_menu_documentation {w} {
    global widget_documentation
    if [catch {$w entrycget active -label} label] { return }
    set key $w,$label
    if {! [getp $key widget_documentation string]} { set string $key}
    set wheight 30 ;# need to figure out font height
    set ypos [expr [$w yposition active] + $wheight]
     if {[$w index active] == [$w index end]} {
	set topw [winfo toplevel [winfo parent  $w]]
	set docw [get_show_documentation_widget $topw]
	if {$docw == $w} { return }
	$docw configure -text $string
	place $docw -in $topw -x [expr [winfo rootx $w] - [winfo rootx $topw]] \
		-y [expr [winfo rooty $w] - [winfo rooty $topw] + $ypos]
    } else {
	set docw [get_show_documentation_widget $w]
	if {$docw == $w} { return }
	$docw configure -text $string
	place $docw -in $w -x 0 -y $ypos
}   }

# This version places the doc widget under the selected menu entry.
# Has problems:  width of doc string limited to width of menu
proc show_menu_documentation {w} {
    global widget_documentation
    if [catch {$w entrycget active -label} label] { return }
    set key $w,$label
    if {! [getp $key widget_documentation string]} { set string $key}
    set wheight 30 ;# need to figure out font height
    set ypos [expr [$w yposition active] + $wheight]
     if {[$w index active] == [$w index end]} {
	set topw [winfo toplevel [winfo parent  $w]]
	set docw [get_show_documentation_widget $topw]
	if {$docw == $w} { return }
	$docw configure -text $string
	place $docw -in $topw -x [expr [winfo rootx $w] - [winfo rootx $topw]] \
		-y [expr [winfo rooty $w] - [winfo rooty $topw] + $ypos]
    } else {
	set docw [get_show_documentation_widget $w]
	if {$docw == $w} { return }
	$docw configure -text $string
	place $docw -in $w -x 0 -y $ypos
}   }

proc show_menu_documentation {w} {
    global widget_documentation
    if [catch {$w entrycget active -label} label] { return }
    set key $w,$label
    if {! [getp $key widget_documentation string]} { set string $key}
    set topw [winfo toplevel [winfo parent  $w]]
    set docw [get_show_documentation_widget $topw]
# place docw above menu
#    set ypos [expr [winfo rooty $w] - [winfo rooty $topw] - [winfo height $docw]]
# place docw below menu
    set ypos [expr [winfo rooty $w] - [winfo rooty $topw] + [winfo height $w] ]
    $docw configure -text $string
    place $docw -in $topw \
	    -x [expr [winfo rootx $w] - [winfo rootx $topw]] \
	    -y $ypos
    update
    set off [expr [winfo rootx $docw] + [winfo width $docw] \
	    - [winfo rootx $topw] - [winfo width $topw]]
    if { $off >= 0 } {
	if { [winfo width $docw] <= [winfo width $topw]} {
#           puts [list show_menu_documentation -x [expr -$off]]
	    place $docw -in $topw \
		    -x [expr [winfo width $topw] - [winfo width $docw]] -y $ypos
}   }   }


# new version Mon Feb  2 1998

if {1} {
 
proc show_menu_documentation {w} {
    global widget_documentation
    set index [$w index active] 
    if {$index == "none"} { return }
    if [catch {$w entrycget active -label} label] { return }
    set key $w,$label
    if {! [getp $key widget_documentation string]} { 
#	set string $key
	set string {}
    }
    set topw [winfo toplevel [winfo parent  $w]]
    set docw [get_show_documentation_widget $topw]
    if {$index == [$w index last]} {
	set ypos [winfo height $w] 
    } else {
	incr index
	set ypos [$w yposition $index]
    }
    if {$string != {}} {
	$docw configure -text $string
	place_documentation_widget $docw $w 0 $ypos
    }
}      
   

proc add_menu_widget_doc {w label string} {
    set key $w,$label
    putprop $key widget_documentation $string
}

proc remove_menu_doc_widget {w} {
    cancel_delayed_show_widget_documentation
    remove_doc_widget [get_show_documentation_widget $w]
}

}
# end if block

msg "...done loading show-menu-doc.tcl"
