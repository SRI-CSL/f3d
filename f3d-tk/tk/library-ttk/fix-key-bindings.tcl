
package provide qtk 1.0

msg "Loading fix-key-bindings.tcl..."

proc fix_key_bindings {} {
    # Fix the bindings for <Delete> to delete backwards like <BackSpace>
    foreach class [list Entry Text ] {
#	puts [list bind $class <Delete> [bind $class <BackSpace>]]
	bind $class <Delete> [bind $class <BackSpace>]
    }

    # Fix the bindings for Alt key to be same as Meta Key for Text and Entry widgets
    foreach class [list Entry Text ] {
	foreach seq [list <Meta-b> <Meta-d> <Meta-f> <Meta-BackSpace> <Meta-Delete> \
		           <Meta-less> <Meta-greater> ] {
	    if [regexp {<Meta-(.*)>} $seq match prefix] {
#		puts [list bind $class <Alt-$prefix> [bind $class $seq]]
		bind $class <Alt-$prefix> [bind $class $seq]
	    }
	}
    }
}


msg "...done loading fix-key-bindings.tcl"
