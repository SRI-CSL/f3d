package provide qtk 1.0

# *********************  LIST UTILITIES  *********************
msg "Loading tcl-utils.tcl..."

proc ldelete {list elt} {
    set i [lsearch -exact $list $elt]
    if {$i >= 0} {
	return [lreplace $list $i $i]
    } else {
    return $list
    }
}

proc nth {i list} {
    return [lindex $list $i]
}

proc nthcdr {i list} {
    return [lrange $list $i end]
}


proc string_append {args} {
    set string {}
    eval {append string} $args
    return $string
}

proc get_global {var} {
    upvar #0 $var x
    return ${x}
}

proc set_global {var val} {
    upvar #0 $var x
    set x $val
}

proc unset_global {var} {
    upvar #0 $var x
    catch {unset x}
}

proc global_boundp {var} {
    upvar #0 $var x
    info exists x
}

proc arefp {arrname key varname} {
    upvar 1 $arrname array
    upvar 1 $varname result
    if {[info exists array($key)]} {
	set result $array($key)
	return 1
    } else {
	return 0
    }
}
proc arefp_remove {arrname key varname} {
    upvar 1 $arrname array
    upvar 1 $varname result
    if {[info exists array($key)]} {
	set result $array($key)
	unset array($key)
	return 1
    } else {
	return 0
    }
}

proc aset {arrname key val} {
    upvar 1 $arrname array
    if {! [info exists array($key)]} {
	array set array [list $key $val]
    } else {
	set array($key) $val
    }
}

proc print_plist  {plist} {
    foreach {key val} $plist {
#	puts "$key = $val"
}   }


proc print_array {arrname} {
    upvar 1 $arrname arr
    print_plist [array get $arr]
}

proc print_list {list} {
    foreach val $list {
#	puts $val
}   }

msg "...done loading tcl-utils.tcl"
