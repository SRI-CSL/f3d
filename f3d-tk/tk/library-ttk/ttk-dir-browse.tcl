
# from /usr/local/lib/tk8.5/demos/tree.tcl
#

package require Tk
package require Ttk

proc positionWindow w {
    wm geometry $w +300+300
}

proc ttk-dir-browse {w title {dir [pwd]}} {
    toplevel $w
    wm title $w $title
    wm iconname $w "tree"
    positionWindow $w

    ## See Code / Dismiss
#    pack [addSeeDismiss $w.seeDismiss $w] -side bottom -fill x

    ## Create the tree and set it up
    ttk::treeview $w.tree -columns {fullpath type size} -displaycolumns {size} \
	-yscroll "$w.vsb set" -xscroll "$w.hsb set"
    if {[tk windowingsystem] ne "aqua"} {
	ttk::scrollbar $w.vsb -orient vertical -command "$w.tree yview"
	ttk::scrollbar $w.hsb -orient horizontal -command "$w.tree xview"
    } else {
	scrollbar $w.vsb -orient vertical -command "$w.tree yview"
	scrollbar $w.hsb -orient horizontal -command "$w.tree xview"
    }
    $w.tree heading \#0 -text "Directory Structure"
    $w.tree heading size -text "File Size"
    $w.tree column size -stretch 0 -width 70

    
    populateRoots $w.tree $dir
    bind $w.tree <<TreeviewOpen>> {populateTree %W [%W focus]}
    bind $w.tree <<TreeviewSelect>> {ttk-dir-browse-select %W [%W focus]}

    bind $w <Destroy> {destroy_qwidget_callback %W}

    ## Arrange the tree and its scrollbars in the toplevel
    lower [ttk::frame $w.dummy]
    pack $w.dummy -fill both -expand 1
    grid $w.tree $w.vsb -sticky nsew -in $w.dummy
    grid $w.hsb -sticky nsew -in $w.dummy
    grid columnconfigure $w.dummy 0 -weight 1
    grid rowconfigure $w.dummy 0 -weight 1
}

proc ttk-dir-browse-select {tree node} {
    if {[$tree set $node type] eq "file"} {
	msg "ttk-dir-browse-select $tree [$tree set $node fullpath] [$tree set $node type] "
	msg [list qcme_callback $tree tix_dir_browse_activate [$tree set $node fullpath]]
	qcme_callback $tree tix_dir_browse_activate [$tree set $node fullpath]
    }
}


## Code to populate the roots of the tree (can be more than one on Windows)
proc populateRoots {tree roots} {
    foreach dir [lsort -dictionary $roots] {
	populateTree $tree [$tree insert {} end -text $dir \
				-values [list $dir directory]]
    }
}

proc tix_dir_browser_set_directory {w dir} {
    $w.tree delete
    populateRoots $w.tree $dir
}

proc tix_dir_browser_add_directory {w dir} {
    populateRoots $w.tree $dir
}

## Code to populate a node of the tree
proc populateTree {tree node} {
    if {[$tree set $node type] ne "directory"} {
	return
    }
    set path [$tree set $node fullpath]
    $tree delete [$tree children $node]
    foreach f [lsort -dictionary [glob -nocomplain -dir $path *]] {
	set type [file type $f]
	if {$type eq "link"} {
	    set type [link_resolved_file_type $f]
	}
	set id [$tree insert $node end -text [file tail $f] \
		    -values [list $f $type]]
	if {$type eq "directory"} {
	    ## Make it so that this node is openable
	    $tree insert $id 0 -text dummy ;# a dummy
	    $tree item $id -text [file tail $f]/
	    
	} elseif {$type eq "file"} {
	    set size [file size $f]
	    ## Format the file size nicely
	    if {$size >= 1024*1024*1024} {
		set size [format %.1f\ GB [expr {$size/1024/1024/1024.}]]
	    } elseif {$size >= 1024*1024} {
		set size [format %.1f\ MB [expr {$size/1024/1024.}]]
	    } elseif {$size >= 1024} {
		set size [format %.1f\ kB [expr {$size/1024.}]]
	    } else {
		append size " bytes"
	    }
	    $tree set $id size $size
	} 
    }
    
    # Stop this code from rerunning on the current node
    $tree set $node type processedDirectory
}


proc resolve_links {path} {
    return [file dirname [file normalize $path/__dummy__]]
}

proc link_resolved_file_type {path} {
    return [file type [resolve_links $path]]
}



package provide ttkDirBrowse 1.0
