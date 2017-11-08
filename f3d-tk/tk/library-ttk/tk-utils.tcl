package provide qtk 1.0

msg "Loading tk-utils.tcl..."
proc widget_children {w} {
    return [winfo children $w] 
}

proc widget_name {w} {
    set p [string last . $w]
    return [string range $w [expr $p+1] end]
}

proc widget_panel {w} {
    if {[getp $w widget_panel p]} {
	# should we return $p instead?
	return $w
    }
    if {[winfo class $w] == "Toplevel"} {
	return $w
    }    
    set parent [winfo parent $w]
    if {$parent == {}} {
	return $w
    } else {
	return [widget_panel $parent]
    }
}

proc find_widget_named {w args} {
    set names $args
    foreach name $names {
	if { $w == {}} {
	    break
	} else {
	    set w [find_widget_named_int $w $name ]
    }   }
    return $w
}

proc find_widget_named_int {w name0} {
    set nam [widget_name $w]
#    puts [list $w $name0 $nam]
    if { $nam == $name0 } {
	return $w
    }
    foreach w2 [widget_children $w] { 
	set c [find_widget_named_int $w2 $name0]
	if { $c != {}} {
	    return $c
	}
    }
    return {}
}

proc widget_named {w name} {
    if {[string range $name 0 0] == "."} {return $name}
    if {[getp $w $name w2]} { return $w2 }
    putprop $w $name [find_widget_named $w $name]
}

proc wn {w name} {
    return [widget_named $w $name]
}

# *********************  WIDGET STATE ACCESSORS  *********************


proc widget_text {w} {
    switch -exact [winfo class $w] {
	Toplevel {return [wm title $w]}
	Entry {return [$w get]}
	Button {return [$w cget -text]}
	Label {return [$w cget -text]}
	TEntry {return [$w get]}
	TButton {return [$w cget -text]}
	TLabel {return [$w cget -text]}
	default {return {}}
}   }

proc set_widget_text {w string} {
    switch -exact [winfo class $w] {
	Toplevel {[wm title $w $string]}
	Entry {[entry_text $w $string]}
	Button {[$w configure -text $string]}
	Label {[$w configure -text $string]}
	TEntry {[entry_text $w $string]}
	TButton {[$w configure -text $string]}
	TLabel {[$w configure -text $string]}
	default {{}}
    }
    $string
}

proc entry_text {w {newval 0}} {
    if {$newval == 0} {
	return[$w get]
    } else {
	$w delete 0 end
	$w insert 0 $newval
}   }

proc entry_text {w args} {
    if {$args == {}} {
	return[$w get]
    } else {
	$w delete 0 end
	$w insert 0 [nth 0 $args]
}   }

proc text_text {w args} {
    if {$args == {}} {
	return[$w get 1.0 end]
    } else {
	$w delete 1.0 end
	$w insert 1.0 [nth 0 $args]
}   }

proc entry_value {w} {
    set string [$w get]
    if {[getp $w format format]} {	
	if {[scan $string $format val]} {
	    return $val
	} else {
	    $w config -background #ea7680
	    return $string
	}   
    } else {
	return $string
}   }

proc set_entry_value {w value} {
    if {[getp $w oformat format] || [getp $w format format]} {
	entry_text $w [format $format $value]
    } else {
	entry_text $w $value
    }   
    $w xview 0
  
}   

# This should use qwidget_class
proc widget_value_0 {w} {
    switch -exact [qwidget_class $w] {
	qEntry {return [entry_value $w]} 
	qText {return [$w get 1.0 end]}
	qMenubutton {return [get_global [$w cget -textvariable]]}
	qCheckbutton -
	qRadiobutton {return [get_global [$w cget -variable]]}
	qLabel {return [$w cget -text]}
	qListbox {return [listbox_selected_items $w]}
        qRadiogroup {getp $w variable var; return [get_global $var]}
	default {return {}}
}   }


#	Menubutton {return [get_global [$w cget -textvariable]]}
#       Menubutton {return [get_global [widget_name $w]]}

proc widget_value_0 {w} {
    switch -exact [winfo class $w] {
	TEntry -
	Entry {return [entry_value $w]} 
	Text {return [$w get 1.0 end]}
	TCheckbutton -
	TRadiobutton -
	Checkbutton -
	Radiobutton {return [get_global [$w cget -variable]]}
	TLabel -
	Label {return [$w cget -text]}
	Listbox {return [listbox_selected_items $w]}
	TScale -
	Scale {return [get_global [$w cget -variable]]}
        default {
	    switch -exact [qwidget_class $w] {
		qRadiogroup {getp $w variable var; return [get_global $var]}
		qMenubutton {return [get_global [getprop $w textvariable]]}
		qTMenubutton {return [get_global [getprop $w textvariable]]}
		return {}}
    }   }   }

# This returns both the value and the class so class dependent translations
# can be done
proc widget_value {w} {
    return [list [widget_value_0 $w] [qwidget_class $w] ]
 }

# This should use qwidget_class
proc set_widget_value {w val} {
    switch -exact [qwidget_class $w] {
	qEntry {return [set_entry_value $w $val]}
	qText {return [text_text $w $val]}
	qMenubutton {return [set_global [$w cget -textvariable] $val]}
	qCheckbutton -
	qRadiobutton {return [set_global [$w cget -variable] $val]}
	qLabel {return [$w configure -text $val]}
	qListbox  {return [set_listbox_selected_items $w $value]}
	qRadiogroup {getp $w variable var; return [set_global $var $val]}
	default {return {}}
}   }

# qRadiogroup {getp $w variable var; return [set_global $var $val]}
# qMenubutton {return [set_global [getprop $w textvariable] $val]}
proc set_widget_value {w val} {
    switch -exact [winfo class $w] {
	Entry {return [set_entry_value $w $val]}
	Text {return [text_text $w $val]}
	Checkbutton -
	Radiobutton {return [set_global [$w cget -variable] $val]}
	Label {return [$w configure -text $val]}
	Listbox  {return [set_listbox_selected_items $w $val]}
	Scale {return  [set_global [$w cget -variable] $val]}
	default {
	    switch -exact [qwidget_class $w] {
		qRadiogroup {return [set_global $var $val]}
		qMenu -
		qOptionMenu -
		qMenubutton {set_optionmenu_selected_item $w $val}
		return {}}
    }   }   }

proc set_widget_value {w val} {
    switch -exact [winfo class $w] {
	TEntry -
	Entry {return [set_entry_value $w $val]}
	Text {return [text_text $w $val]}
	TCheckbutton -
	Checkbutton -
	TRadiobutton -
	Radiobutton {return [set_global [$w cget -variable] $val]}
	TLabel -
	Label {return [$w configure -text $val]}
	Listbox  {return [set_listbox_selected_items $w $val]}
	TScale -
	Scale {return  [set_global [$w cget -variable] $val]}
	default {
	    switch -exact [qwidget_class $w] {
		qRadiogroup {return [set_global $var $val]}
		qMenu -
		qOptionMenu -
		qMenubutton {set_optionmenu_selected_item $w $val}
		return {}}
    }   }   }

# This returns a list of strings for the selected items
proc listbox_selected_items {w} {
    set items {}
    foreach i [$w curselection] {
	lappend items [$w get $i]
    }
    return $items
}

# This returns a list of indices for the selected items
proc listbox_selected_item_indices {w} {
    set indices {}
    foreach i [$w curselection] {
	lappend indices $i
    }
    return $indices
}

# indices are integers
proc set_listbox_selected_items {w indices} {
    $w selection clear 0 end
    foreach i $indices {
	$w selection set $i
    }
}

proc listbox_items {w} {
    return [$w get 0 end]
}

proc set_listbox_items {w strings} {
    $w delete 0 end
    eval {$w insert 0} $strings
}

proc set_widget_items {w strings} {
    switch -exact [qwidget_class $w] {
	qListbox {set_listbox_items $w $strings}
	qMenu -
	qOptionMenu -
	qMenubutton -
	qRadiogroup {
	    set_optionmenu_items $w $strings {}
#	    set_optionmenu_selected_item $w 0; # select first item 
}   }   }

proc widget_selected_item {w} {
    switch -exact [qwidget_class $w] {
	qListbox {return [listbox_selected_items $w]}
	qMenu -
	qOptionMenu -
	qMenubutton -
	qRadiogroup {return [get_optionmenu_selected_item $w]}
}   }   

proc string_linepos {string line} {
    set pos 0
    for {set i 0} {$i < $line} {incr i} {
	set nextpos [string first "\n" $string]
	incr nextpos
	incr pos $nextpos
	set string [string range $string $nextpos end]
    }
    return $pos
}

# return the character index in the text widget string at pixel x,y 
proc text_widget_xy_stringpos {w x y} {
    set line_char [$w index @$x,$y]
    scan $line_char {%d.%d} line char
    incr line -1
    return [expr [string_linepos [widget_value $w] $line] + $char]
}

proc widget_screen_parameters {w} {
    return [list [winfo width $w] [winfo height $w] \
		[winfo rootx $w] [winfo rooty $w] \
		[winfo screenwidth $w] [winfo screenheight $w]]
}

if {0} {
text .t; pack .t
bind .t <1> {puts [text_widget_xy_stringpos %W %x %y]} 
}

msg "...done loading tk-utils.tcl"
