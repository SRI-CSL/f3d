package provide qtk 1.0

# *********************  MENU BUILDERS  *********************

# *tk-menu-args-hack* = NIL

# Avoid this for large numbers of menu items -- list ops are slow
# Each item is either: label, (name label), or (button_type . args)
# By default, button_type is command executes {qcme_callback $m menubutton name}
# If the item is a list of greater than 2 elements, it is button_type, one of 
# command, radio, checkbutton, cascade, or separator, followed by button args,
# which must explicitly specify the command to be executed.
# cascade entries should be of form:
# {cascade -label label -items itemlist -tearoff boolean}
# cascade entries can also be of form
# {cascade -label label -menu menu}, where menu is the name of a child of $m,
# a menu that must be built separately.
proc add_menu_items {m items buttonargs} {
    set itemindex 0
    foreach item $items {
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n == 1 && $elt0 == "-separator"} {
	    $m add separator
	} elseif {$n <= 2} {
	    set label $elt0
	    if {$n == 2} {
		set label [nth 1 $item]
	    }
	    set cmd [list qcme_callback $m menubutton $elt0]
	    #puts [list $m add command -label $label -command $cmd $buttonargs]
	    eval {$m add command -label $label -command $cmd} $buttonargs
	} elseif {$elt0 == "cascade"} {
	    # item is {buttontype . args}
	    set buttontype $elt0
	    array set cascadeargs [lrange $item 1 end]
	    set submenuitems {}; arefp_remove cascadeargs -items submenuitems
	    if {[llength $submenuitems] > 0} {
		set tearoff 0; arefp_remove cascadeargs -tearoff tearoff
		set submenu $m.$itemindex
		eval {$m add cascade -menu $submenu} [array get cascadeargs]  
		qmenu $submenu -items $submenuitems -tearoff $tearoff
	    } else {
		set menuname {}; arefp_remove cascadeargs -menu menuname
		if { [llength $menuname] == 0} {
#		    puts {cascade item without -menu or -item specified}
		}
		set menuname [merge_widget_pathname $m $menuname]
		#puts [list $m add $buttontype -menu $menuname [array get cascadeargs]]
		eval {$m add $buttontype -menu $menuname} [array get cascadeargs]
	    }
	    unset cascadeargs	
	} else {
	    eval {$m add} $item
	}
	incr itemindex
    }
    return $m
} 

# *tk-menu-args-hack* = T

# Avoid this for large numbers of menu items -- list ops are slow
# Each item is either: label, (name label), or (name label . args)
# By default, button_type is command executes {qcme_callback $m menubutton name}
# If the item is a list of greater than 2 elements, it is button_type, one of 
# command, radio, checkbutton, cascade, or separator, followed by button args,
# which must explicitly specify the command to be executed.
# cascade entries should be of form:
# {0 label -button_type cascade -items itemlist -tearoff boolean}
# cascade entries can also be of form
# {0 label -button_type cascade -menu menu}, where menu is the name of a child of $m,
# a menu that must be built separately.
proc add_menu_items {m items buttonargs} {
    set itemindex 0
    set has_menu_doc 0
    foreach item $items {
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n == 1 && $elt0 == "-separator"} {
	    $m add separator
	} elseif {$n <= 2} {
	    set label $elt0
	    if {$n == 2} {
		set label [nth 1 $item]
	    }
	    # elt0 is value to return to freedius
	    set cmd [list qcme_callback $m menubutton $elt0]
	    #puts [list $m add command -label $label -command $cmd $buttonargs]
	    eval {$m add command -label $label -command $cmd} $buttonargs
	} else {
	    # item is {name label button_type . args}
	    set label [nth 1 $item]
	    set value [nth 0 $item]
	    array set opts [nthcdr 2 $item]
	    set buttontype command; arefp_remove opts -button_type buttontype
	    if {$buttontype == "cascade"} {
		# item is (value label cascade . args)
		if {[arefp_remove opts -items submenuitems] > 0} {
		    set tearoff 0; arefp_remove opts -tearoff tearoff
		    set submenu $m.$itemindex
		    eval {$m add cascade -label $label -menu $submenu} [array get opts]  
		    qmenu $submenu -items $submenuitems -tearoff $tearoff
		} else {
		    set menuname {}; arefp_remove opts -menu menuname
		    if { [llength $menuname] == 0} {
			# puts {cascade item without -menu or -item specified}
		    }
		    set menuname [merge_widget_pathname $m $menuname]
		    #puts [list $m add $buttontype -menu $menuname [array get opts]]
		    eval {$m add cascade -label $label -menu $menuname} [array get opts]
		}
	    } else {
		# item is (value label buttontype . args)
		set cmd [list qcme_callback $m menubutton $value]
		arefp_remove opts -command cmd
		arefp_remove opts -text label
		if {[arefp_remove opts -documentation doc]} {
		    add_menu_widget_doc $m $label $doc
		    set has_menu_doc 1
		}
		eval {$m add $buttontype -label $label -command $cmd} [array get opts]
	    }
	    unset opts
	}
	incr itemindex
    }
    if {$has_menu_doc} {
	enable_menu_doc $m
    }
    return $m
} 
 
proc qmenu {m args} {
    array set arg $args
    set items {} ; arefp_remove arg -items items
    set buttonargs {} ; arefp_remove arg -buttonargs buttonargs
    eval {menu $m} [array get arg]
    add_menu_items $m $items $buttonargs
    return $m
}

# Weirdness in TK:  The menu under a MENUBUTTON must a direct descendent of the
# menubutton.  However, the menu under a CASCADE button do not have to be
# a direct descendent.  
proc qpulldownmenu {mb0 args} {
    array set arg $args
    set items {} ; arefp_remove arg -items items
    set buttonargs {} ; arefp_remove arg -buttonargs buttonargs
    set tearoff 1; arefp_remove arg -tearoff tearoff
#    set mb [eval {qmenubutton $mb0 -activebackground yellow} [array get arg]]
    set mb [eval {qmenubutton $mb0} [array get arg]]
    set menu $mb.[widget_name ${mb0}]_menu
    menu $menu -tearoff $tearoff
    $mb configure -menu $menu 
    add_menu_items $menu $items $buttonargs
    # Not clear whether we should return the menubutton or the menu?
    # We can get the menu from the menubutton using cget $mb -menu
    # We can get the menubutton from the menu using winfo parent $m
    # To be consistent with all of the other menu creators, we return the menu
    # return $menu
    return $mb
}

# This is a pulldown option menu of radio buttons
# items is list of entries of form: label or {name label}
# When only label is specified, items are identified by their position in the list.
# FIXME:  tcl sucks.  It does not distinguish a list of 2 strings of one words each 
# from a string of 2 words.  Thus {Foo Bar} == "Foo Bar".  
proc qoptionmenu {w0 args} {
    array set arg $args
    set items {}     ; arefp_remove arg -items items    
    set buttonargs {}; arefp_remove arg -buttonargs buttonargs
    set initp [arefp_remove arg -initial_value initial_value]
    set var_p [arefp_remove arg -variable varName]
# qmenubutton used to have -borderwidth 2 -highlightthickness 2 
# should we rename qmenubutton to qoptionbutton?
    set w [eval {qmenubutton0 $w0 -indicatoron 1 \
	    -relief raised -anchor c -direction flush} [array get arg]]
    set w0 [widget_name $w0]
    set menu $w.${w0}_menu
    $w configure -menu $menu
    if {! $var_p} {set varName $w0}
    #    if {! $var_p} {set varName $w }
    # use the full menubutton name, not just the last component
    upvar #0 $varName var
    # default to first item
    if {![info exists var]} {
	if {$initp} {
	    set var $initial_value
	} else {
	    # By default select first item
	    set item [nth 0 $items]
	    if { [llength $item] == 1} {
#		set var $item
		set var 0
		$w configure -text [nth 0 $item]
	    } else { 
		set var [nth 0 $item]
		$w configure -text [nth 1 $item]
	    }
	}
    }   
    # problems here -- must use -textvariable meaning that is holds the text
    # to put into the menubutton, not the name of the selected item.
    #$w configure -textvariable $varName 
    putprop $w textvariable $varName
    menu $menu -tearoff 0
    set_optionmenu_items $w $items $buttonargs
# what should this return?  Does it really matter?
#    return $menu
    return $w
    
}


# items are of the form:  label
#                         (name label)
#                         (name label . options)

proc set_optionmenu_items {w items buttonargs} {
#    puts "set_optionmenu_items $w $items"
    set w0 [widget_name $w]
    set menu $w.${w0}_menu
    $menu delete 0 end
    set maxwidth 0
    set index 0
    set varName [getprop $w textvariable]
    set label {}
    foreach item $items {
	set options {}
        if {[llength $item] < 3} {
	    if {[llength $item] == 2} {
		set label [nth 1 $item]; 
		set name [nth 0 $item]
	    } else {
		set label [nth 0 $item]; 
		set name $index
	    }
	    eval {$menu add radiobutton -label $label -indicatoron false \
		      -value $name -variable $varName \
		      -command [list qoptionmenu_callback3 $w $varName $name -text $label]} \
		$buttonargs
	    if {[string length $label] > $maxwidth} { 
		set maxwidth [string length $label] 
	}
	} else {
	    set label [nth 1 $item]
	    set name [nth 0 $item]
	    set options [nthcdr 2 $item]
	    array set opts $options
	    set class radiobutton; arefp_remove opts -class class
	    if {[arefp_remove opts -bitmap bitmap]} {
		eval {$menu add $class -style Toolbutton \
			  -value $name -variable $varName \
			  -image $bitmap \
			  -command [list qoptionmenu_callback3 $w $varName $name -image $bitmap]} \
		    [array get opts]
		if {[image width $bitmap] > $maxwidth} {
		    set maxwidth [image width $bitmap]
		}
	    } else {
		arefp_remove opts -label label
		eval {$menu add $class -style Toolbutton \
			  -value $name -variable $varName \
			  -label $label \
			  -command [concat [list qoptionmenu_callback3 $w $varName $name -text $label] \
					[array get opts]]} \
		    [array get opts]
		if {[string length $label] > $maxwidth} { 
		    set maxwidth [string length $label] 
		}
	    }
	}
	
	incr index
    }
    $w configure -width $maxwidth 
}

proc get_optionmenu_menu {w} {
    set w0 [widget_name $w]
    set menu $w.${w0}_menu
    return $menu
}

proc get_optionmenu_item_index {w id} {
    set w0 [widget_name $w]
    set menu $w.${w0}_menu
    set n [$menu index end]
    for {set i 0} {$i <= $n} {incr i} {
	if {[string compare [$menu entrycget $i -value] $id] == 0} {
	    return $i
    }   }
    return {}
}

proc get_optionmenu_item_text {w id} {
    set w0 [widget_name $w]
    set menu $w.${w0}_menu
    set n [$menu index end]
    for {set i 0} {$i <= $n} {incr i} {
	if {[string compare [$menu entrycget $i -value] $id] == 0} {
	    return [$menu entrycget $i -label]
    }   }
    return {}
}

proc set_optionmenu_selected_item {w id} {
    set i [get_optionmenu_item_index $w $id]
    set menu [get_optionmenu_menu $w]
    set background [$menu entrycget $i -background]
    set image [$menu entrycget $i -image]
    set text [$menu entrycget $i -label]
    $w configure -text $text
    $w configure -image $image
    if {$background != {}} {
	$w configure -background $background
    } else {
	$w configure -background [nth 3 [$w configure -background]]
    }
    set_global [getprop $w textvariable] $id
}

proc get_optionmenu_selected_item {w} {
    return [get_global [getprop $w textvariable]]
}


proc qoptionmenu_callback3 {w varName name args} {
    set_optionmenu_selected_item $w $name
    qcme_callback $w optionbutton $varName $name
}

if {0} { 
# Each item is one of:
#   name label
#   label     (name defaults to $w0.$b, where $b is button-index)
#   widget_class widget_name . rest
proc do_radiobuttons {w items button_class buttonargs} {
#    puts [list do_radiobuttons $items $buttonargs]
    array set arg $buttonargs
    arefp arg -variable varName
    set w0 [widget_name $w]
    set b 0
    foreach item $items {
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n == 2} {
	    # item is {name label}
	    set name $elt0
	    set label [nth 1 $item]
	    eval {labelled_button $button_class $w.$name -text [nth 1 $item] \
		    -value $name } $buttonargs
	} elseif {$n == 1} {
	    # item is {label}
	    set name ${w0}_$b
	    set label $elt0 
	    eval {labelled_button $button_class $w.$name -text $label -value $name } $buttonargs
	} else {
	    # item is {type name . buttonargs}
	    set name [nth 1 $item]
#	    eval {labelled_button $elt0 $w.$name -variable $varName} [nthcdr 2 $item]
	    eval {labelled_button $elt0 $w.$name} [nthcdr 2 $item]
	}
	incr b
    }
}

}

# Each item is one of:
#   name label . button-args
#   label     (name defaults to $w0.$b, where $b is button-index)
proc do_radiobuttons {w items button_class buttonargs} {
#    puts [list do_radiobuttons $items $buttonargs]
    array set arg $buttonargs
    arefp arg -variable varName
    set w0 [widget_name $w]
    set b 0
    foreach item $items {
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n >= 2} {
	    # item is {name label}
	    set butcls $button_class
	    set butargs $buttonargs
	    set name $elt0
	    set label [nth 1 $item]
	    if {$n > 2} {
		# item is {name label . buttonargs}
		array set opts $buttonargs
		array set opts [nthcdr 2 $item]
		arefp_remove opts -button_type butcls
		if {! [arefp opts -indicatoron indicator]} {aset opts -style Toolbutton}
		set butargs [array get opts]
	    }
	    eval {labelled_button $butcls $w.$name -text $label \
		    -value $name -command [list qcme_callback $w button]} $butargs
	} elseif {$n == 1} {
	    # item is {label}
	    set name ${w0}_$b
	    set label $elt0
	    eval {labelled_button $button_class $w.$name -text $label -value $name \
                  -command [list qcme_callback $w button]} $buttonargs
	} else { 
	    # NEVER GET HERE
	    # item is {type name . buttonargs}
	    set name [nth 1 $item]
#	    eval {labelled_button $elt0 $w.$name -variable $varName} [nthcdr 2 $item]
	    eval {labelled_button $elt0 $w.$name} [nthcdr 2 $item]
	}
	incr b
    }
}

proc get_default_radiogroup_value {w items} {
    set w0 [widget_name $w]
    set item [nth 0 $items]
    set n [llength $item]
    set default [nth 0 $item]
    if {$n == 1} {
	set default ${w0}_0
    } elseif {$n > 2} {
	array set arg [nthcdr 2 $item]
	arefp arg -value default
    }
    return $default
}
 
# New version Mon Jan 26 1998 with generalized itemlist
# This is a horizontal layout of related radio buttons
# Args are -items -buttonargs -variable -initial_value
# Each item in items is either 
# 1. A string - the button -text argument, widget-name is index in radiogroup
# 2. The list {widget-name string} 
# 3. The list {button-class widget-name . button-args}
proc qradiogroup {w0 args} {
    array set arg $args
    set items {}; arefp_remove arg -items items
    set buttonargs {} ; arefp_remove arg -buttonargs buttonargs
    array set butargs $buttonargs
    if {! [arefp butargs -indicatoron indicator]} {aset butargs -style Toolbutton}
    set var_p [arefp_remove arg -variable varName]
    set initp [arefp_remove arg -initial_value initial_value]
    set w [eval {qframe $w0} [array get arg]]
    if {! $var_p} {set varName $w}
#    lappend buttonargs -variable $varName
    aset butargs -variable $varName
    putprop $w variable $varName
    putprop $w qwidget_class qRadiogroup
    upvar #0 $varName var
    if {![info exists var]} {
	if {$initp} {
	    set_global $varName $initial_value
	} else { 
	    set_global $varName [get_default_radiogroup_value $w $items]
#	    puts [list qradiogroup $varName = [get_global $varName]]
	}
    }   
    do_radiobuttons $w $items qradiobutton [array get butargs]
    pack_children $w -side left
    return $w
}


# items are:  button_label, (item_name button_label) , or (button_class item_name . args)
proc do_menu_itemlist {w items button_class buttonargs} {
#    puts [list do_menu_itemlist $items $buttonargs]
    set w0 [widget_name $w]
    set b 0
    foreach item $items {
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n == 2} {
	    # item is {name label}
	    set name $elt0
	    set label [nth 1 $item]
	    eval {labelled_button $button_class $w.$name -text $label } $buttonargs
	} elseif {$n == 1} {
	    # item is {label}
	    set name ${w0}_$b
	    set label $elt0
	    eval {labelled_button $button_class $w.$name -text $label } $buttonargs
	} else {
	    # item is {button_class name . buttonargs}
	    set name [nth 1 $item]
	    eval {labelled_button $elt0 $w.$name} [nthcdr 2 $item]
	}
	incr b
    }
}

# items are:  button_label, (item_name button_label) , or (item_name button_label . args)
proc do_menu_itemlist {w items button_class buttonargs} {
#    puts [list do_menu_itemlist $items $buttonargs]
    set w0 [widget_name $w]
    set b 0
    foreach item $items {
#	puts "do_menu_itemlist $item"
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n == 2} {
	    # item is {name label}
	    set name $elt0
	    set label [nth 1 $item]
	    eval {labelled_button $button_class $w.$name -text $label } $buttonargs
	} elseif {$n == 1} {
	    # item is {label}
	    set name ${w0}_$b
	    set label $elt0
	    eval {labelled_button $button_class $w.$name -text $label } $buttonargs
	} else {
	    # item is {name label . buttonargs}
	    array set opts [nthcdr 2 $item]
	    set button_class2 $button_class
	    arefp_remove opts -button_type button_class2
	    set name $elt0
	    set label [nth 1 $item]
	    if {[string compare $button_class2 "qpulldownmenu"] == 0} {
		eval {qpulldownmenu $w.$name -text $label} [array get opts]
	    } else {
		eval {labelled_button $button_class2 $w.$name -text $label} [array get opts]
	    }
	    unset opts
	}
	incr b
    }
}


# New version Mon Jan 26 1998 with generalized itemlist
# Tk does not support menu widgets with horizontal layout.
# Must create a frame with bottons packed left to right.

proc qbuttonlist {w0 args} {
    array set arg $args
    set buttonargs {} ; arefp_remove arg -buttonargs buttonargs
    set items {}; arefp_remove arg -items items
    set w [eval {qframe $w0} [array get arg]]
    do_menu_itemlist $w $items qbutton $buttonargs
#    pack_children $w -side left -fill both  -ipadx 6 -ipady 6
#    pack_children $w -side left -fill both -padx 0 -pady 0
    pack_children $w -side left -fill both
    return $w
} 


# Clone of the above, but creates pulldown menus instead of buttons:
#
# "On that day, many hacks were forged..."
#
proc do_pulldown_itemlist {w items button_class buttonargs} {
#    puts [list do_pulldown_itemlist $items $buttonargs]
    set w0 [widget_name $w]
    set b 0
    foreach item $items {
	set n [llength $item]
	set elt0 [nth 0 $item]
	if {$n == 2} {
	    # item is {name label}
	    set name $elt0
	    set label [nth 1 $item]
	    eval {labelled_widget qpulldownmenu $w.$name -text $label } $buttonargs
	} elseif {$n == 1} {
	    # item is {label}
	    set name ${w0}_$b
	    set label $elt0
	    eval {labelled_widget qpulldownmenu $w.$name -text $label } $buttonargs
	} else {
	    # item is {type name . buttonargs}
	    set name [nth 1 $item]
	    eval {labelled_widget qpulldownmenu $w.$name} [nthcdr 2 $item]
	}
	incr b
    }
}

proc qmenulist {w0 args} {
    array set arg $args
    set buttonargs {} ; arefp_remove arg -buttonargs buttonargs
    set items {}; arefp_remove arg -items items
    set w [eval {qframe $w0} [array get arg]]
    do_pulldown_itemlist $w $items qpulldownmenu $buttonargs
    pack_children $w -side left
    return $w
} 



bind listbox_after <ButtonRelease-1> {qcme_callback %W activate}

# The lines in a listbox are not widgets.
# The command [$w cursorselection] returns a list of indices of the selected items
# Use [$w get $i] to get the i-th string in the listbox
proc qScroll_Set {scrollbar cmd offset size} {
    if {$offset != 0.0 || $size != 1.0} {
	eval $cmd
    }
    $scrollbar set $offset $size
}


proc qlistbox {w args} {
    array set arg $args
    set items {}; arefp_remove arg -items items
    set scroll_p [arefp_remove arg -scroll scroll]
    # override default: -exportselection 1
    if {! [arefp arg -exportselection export]} {set export 0}
    if {$scroll_p} {
	set w [eval {labelled_widget listbox $w -exportselection $export -minheight 30 -scroll $scroll } [array get arg]]
    } else {
	set w [eval {labelled_widget listbox $w -exportselection $export -minheight 30  } [array get arg]]
    }
    if {[llength $items] > 0} {
	eval {$w insert 0} $items
    }
    bindtags $w [concat [bindtags $w] listbox_after]
    return $w
}


# This is a pulldown listbox
# The lines in a listbox are not widgets.
# The command [$w cursorselection] returns a list of indices of the selected items
# Use [$w get $i] to get the i-th string in the listbox
proc qoptionmenu2 {w args} {
    array set arg $args
    set items {}; arefp_remove arg -items items
    set initp [arefp_remove arg -initial_value initial_value]
    set var_p [arefp_remove arg -variable varName]
    set w [eval {qmenubutton0 $w0 -indicatoron 1 \
	    -relief raised -anchor c -direction flush} [array get arg]]
    set w0 [widget_name $w0]
    set w [eval {labelled_widget listbox $w} [array get arg]]
    eval {$w insert 0} $items
    bindtags $w [concat [bindtags $w] listbox_after]
    return $w
}

if {1} {


proc qoptionmenu {w0 args} {
    array set arg $args
    set items {}     ; arefp_remove arg -items items    
    set buttonargs {}; arefp_remove arg -buttonargs buttonargs
    set initp [arefp_remove arg -initial_value initial_value]
    set var_p [arefp_remove arg -variable varName]
# qmenubutton used to have -borderwidth 2 -highlightthickness 2 
# should we rename qmenubutton to qoptionbutton?
    set w [eval {qmenubutton $w0  -style Toolbutton } [array get arg]]
    set w0 [widget_name $w0]
    set menu $w.${w0}_menu
    $w configure -menu $menu
    if {! $var_p} {set varName $w0}
    #    if {! $var_p} {set varName $w }
    # use the full menubutton name, not just the last component
    upvar #0 $varName var
    # default to first item
    if {![info exists var]} {
	if {$initp} {
	    set var $initial_value
	} else {
	    # By default select first item
	    set item [nth 0 $items]
	    if { [llength $item] == 1} {
#		set var $item
		set var 0
		$w configure -text [nth 0 $item]
	    } else { 
		set var [nth 0 $item]
		$w configure -text [nth 1 $item]
	    }
	}
    }   
    # problems here -- must use -textvariable meaning that is holds the text
    # to put into the menubutton, not the name of the selected item.
    #$w configure -textvariable $varName 
    putprop $w textvariable $varName
    menu $menu -tearoff 0
    set_optionmenu_items $w $items $buttonargs
# what should this return?  Does it really matter?
#    return $menu
    return $w
}

proc qoptionmenu {w0 args} {
    array set arg $args
    set items {}     ; arefp_remove arg -items items    
    set buttonargs {}; arefp_remove arg -buttonargs buttonargs
    set initp [arefp_remove arg -initial_value initial_value]
    set var_p [arefp_remove arg -variable varName]
    # qmenubutton used to have -borderwidth 2 -highlightthickness 2 
    # should we rename qmenubutton to qoptionbutton?
    set w [eval {qmenubutton $w0  -style Toolbutton } [array get arg]]
    set w0 [widget_name $w0]
    set menu $w.${w0}_menu
    $w configure -menu $menu
    if {! $var_p} {set varName $w0}
    #    if {! $var_p} {set varName $w }
    # use the full menubutton name, not just the last component
    upvar #0 $varName var
    # default to first item
    # By default select first item
    set item [nth 0 $items]
    if {$initp} {
	set var $initial_value
	foreach item $items {
	    if {[llength $item] == 1} {
		if {$item == $initial_value} {break}
	    } else {
	    if {[nth 0 $item] == $initial_value} {break}
	    }
	}
    }  
    if { [llength $item] == 1} {
	#		set var $item
	set var 0
	$w configure -text [nth 0 $item]
    } else { 
	set var [nth 0 $item]
	$w configure -text [nth 1 $item]
    }
    
    # problems here -- must use -textvariable meaning that is holds the text
    # to put into the menubutton, not the name of the selected item.
    #$w configure -textvariable $varName 
    putprop $w textvariable $varName
    menu $menu -tearoff 0
    set_optionmenu_items $w $items $buttonargs
    # what should this return?  Does it really matter?
    #    return $menu
    return $w
}

# This is a pulldown listbox
# The lines in a listbox are not widgets.
# The command [$w cursorselection] returns a list of indices of the selected items
# Use [$w get $i] to get the i-th string in the listbox
proc qoptionmenu2 {w args} {
    array set arg $args
    set items {}; arefp_remove arg -items items
    set initp [arefp_remove arg -initial_value initial_value]
    set var_p [arefp_remove arg -variable varName]
    set w [eval {qmenubutton $w0 -style Toolbutton } [array get arg]]
    set w0 [widget_name $w0]
    set w [eval {labelled_widget listbox $w} [array get arg]]
    eval {$w insert 0} $items
    bindtags $w [concat [bindtags $w] listbox_after]
    return $w
}

proc set_optionmenu_selected_item {w id} {
    set i [get_optionmenu_item_index $w $id]
    set menu [get_optionmenu_menu $w]
    set background [$menu entrycget $i -background]
    set image [$menu entrycget $i -image]
    set text [$menu entrycget $i -label]
    $w configure -text $text
    $w configure -image $image
    if {0} {
	if {$background != {}} {
	    $w configure -background $background
	} else {
	    $w configure -background [nth 3 [$w configure -background]]
	}
    }
    set_global [getprop $w textvariable] $id
}

}