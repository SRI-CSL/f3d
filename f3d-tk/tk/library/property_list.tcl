
package provide qtk 1.0

msg "Loading property_list.tcl..."

#pkg_mkIndex ~/cp/tk property_list.tcl

set gensym_counter 0

proc gensym {} {
    global gensym_counter
    incr gensym_counter
    set string GENSYM
    return [append string $gensym_counter]
}

# HIDE THE UGLYNESS W.R.T. TCL ARRAYS 

proc htget {htname ind {lose 0}} {
    upvar #0 $htname ht
    if  {! [info exists ht($ind)]} { return $lose }
    return $ht($ind)
}

proc htgetp {htname ind varname} {
    upvar #0 $htname ht
    upvar 1 $varname result
    if  {! [info exists ht($ind)]} { 
	return 0
    } else {
	set result $ht($ind)
	return 1
    }
}

proc htset {htname ind val} {
    upvar #0 $htname ht
    set ht($ind) $val
    return $val
}

proc htremove {htname ind } {
    upvar #0 $htname ht
    if  {! [info exists ht($ind)]} { return 0 }
    unset ht($ind)
    return 1
}

# OBJECT ORIENTED PROPERTY LISTS
# Each obj has its own hash table 
# This makes it trivial to delete all entires associated with obj,
# which is particularly important when destroying widgets.

proc getprop {obj ind {lose {}}} {
    global get_prop_ht
    if {! [info exists get_prop_ht($obj)]} { return $lose }
    htget $get_prop_ht($obj) $ind $lose
}

# this version doesn't work -- tcl sucks
proc getp {obj ind varname} {
    upvar 1 $varname result
    global get_prop_ht
    if {! [info exists get_prop_ht($obj)]} { 
	return 0
    } else {
	htgetp $get_prop_ht($obj) $ind $varname
}   }

proc getp {obj ind varname} {
    upvar 1 $varname result
    global get_prop_ht
    if {! [info exists get_prop_ht($obj)]} { 
	return 0
    } else {
	if {[htgetp $get_prop_ht($obj) $ind res]} {
	    set result $res
	    return 1
	} else {
	    return 0
}   }   }

proc getp {obj ind {varname {}}} {
    global get_prop_ht
    if {! [info exists get_prop_ht($obj)]} { 
	return 0
    } else {
	if {[htgetp $get_prop_ht($obj) $ind res]} {
	    if {$varname != {}} {
		upvar 1 $varname result
		set result $res
	    }
	    return 1
	} else {
	    return 0
}   }   }

proc putprop {obj ind val} {
    global get_prop_ht
    if {! [info exists get_prop_ht($obj)]} { 
	set htname [gensym]
	set get_prop_ht($obj) $htname
    } else {
	set htname $get_prop_ht($obj)
    }
    return [htset $htname $ind $val]
}

proc remprop {obj ind} {
    global get_prop_ht
    if {! [info exists get_prop_ht($obj)]} { return 0 }
    return [htremove $get_prop_ht($obj) $ind]
}

proc remprop_all {obj} {
    htremove get_prop_ht $obj
}


if {0} {
    global get_prop_ht
    unset get_prop_ht 
    put_prop foo bar foobar
    get_prop foo bar
    rem_prop foo bar
}

msg "...done loading property_list.tcl"
