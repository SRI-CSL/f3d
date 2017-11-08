package provide tixDirBrowse 1.0

global running_pkg_mkIndex

#if {[info exists running_pkg_mkIndex] == 0 || $running_pkg_mkIndex == 0} {

package require Tix 8.4

#
#	$Id: tix-dir-browse.tcl,v 1.1.2.1 2007/04/24 16:10:10 quam Exp $
#
# Tix Demostration Program
#
# This sample program is structured in such a way so that it can be
# executed from the Tix demo program "widget": it must have a
# procedure called "RunSample". It should also have the "if" statment
# at the end of this file so that it can be run as a standalone
# program using tixwish.

# Demonstrates the use of DirTree with the TList 
#

proc tix-dir-browse {w title {dir [pwd]}} {
    qtoplevel $w -class "Cme" -title $title
    set top [frame $w.f -bd 1 -relief raised]
    #set box [tixButtonBox $w.b -bd 1 -relief raised] 
    #pack $box -side bottom -fill both
    pack $top -side top -fill both -expand yes

    # Create the Paned Window to contain the dirtree and scrolled tlist
    #
    set p [tixPanedWindow $top.p -orient horizontal]
    pack $p -expand yes -fill both -padx 4 -pady 4

    set p1 [$p add pane1 -expand 1]
    set p2 [$p add pane2 -expand 1]

    $p1 config -relief flat
    $p2 config -relief flat

    # Create a DirTree
    #
    tixDirTree $p1.dirtree -value $dir -options {
	hlist.width 30
    }
    set hlist [$p1.dirtree subwidget hlist]
    
    pack $p1.dirtree -expand yes -fill both -padx 4 -pady 4


    # Create a TList
    # NOTE: we set the width of the tlist to 60 characters, since we'll have
    #       quite a few files to display
    #
    tixScrolledTList $p2.st -options {
	tlist.orient vertical
	tlist.selectMode single
	tlist.width 30
	tlist.height 15
    }

    pack $p2.st -expand yes -fill both -padx 4 -pady 4

    set tlist [$p2.st subwidget tlist]
    
#    $tlist configure -command "load_image_activate $p1.dirtree $tlist"
    $tlist configure -command "tix_dir_browse_activate $p1.dirtree $tlist"

    # setup the callbacks: when the user selects a directory, we'll display
    # its content in the tlist widget
    $p1.dirtree config \
	-browsecmd "TList:listdir $tlist" \
	-command "TList:listdir $tlist"

    #bind $tlist <Double-ButtonPress-1> {qcme_callback %W load_image [%W index @%x,%y]}
    bind $top <Unmap> {qcme_callback %W unmap}
    # List the directory now
    #
    TList:listdir $tlist $dir

    # Create the buttons
    #
    #$box add ok     -text Done     -command "wm iconify $w" -width 6
    #$box add cancel -text Cancel -command "wm iconify $w" -width 6
}

if {$tix_version == "8.4"} {
# This is needed for tix8.4
proc tixDirTree:AddAncestors {w dir} {
    upvar #0 $w data
    uplevel #0 set TRANSPARENT_GIF_COLOR [$data(w:hlist) cget -bg]

    set path ""
    set parent ""
    foreach name [tixFileSplit $dir] {
	set path [tixSubFolder $path $name]
	if {![$data(w:hlist) info exists $path]} {
            tixDirTree:AddToList $w $path [tix getimage openfold]
	}
	set parent $path
    }
}

# This is needed for tix8.4
proc tixFileSplit {intName} {

    set l ""
    foreach n [split $intName /] {
	if {$n == ""} {
	    continue
	}
	if {$n == "."} {
	    continue
	}

	lappend l $n
    }
    

    while {1} {
	set idx [lsearch $l ".."]
	if {$idx == -1} {
	    break;
	}
	set l [lreplace $l [expr $idx -1] $idx]
    }


    if {[string index $intName 0] == "/"} {
	return [concat "/" $l]
    } else {
	return $l
    }
}

# This is needed for tix8.4
proc tixSubFolder {parent sub} {
    if {$parent == ""} {
	return $sub
    }
    if {$parent == "/"} {
	return /$sub
    } else {
	return $parent/$sub
    }
}

proc tixFileDisplayName {intName} {
    if {$intName == "/"} {
	return "/"
    } else {
	return [file tail $intName]
    }
}

}
# end if tix8.4

proc tix_dir_browser_set_directory {w dir} {
    # this next might be done better
    set dirtree $w.f.p.pane1.dirtree
    set hlist [$dirtree subwidget hlist]
    set tlist [$w.f.p.pane2.st subwidget tlist]
    $dirtree configure -value $dir
    $hlist delete all
    tixDirTree:AddAncestors $dirtree $dir
    TList:listdir $tlist $dir
    $hlist selection clear
    $hlist anchor clear
    catch {$hlist selection set $dir}
}

proc tix_dir_browser_clear {w} {
    # this next might be done better
    set dirtree $w.f.p.pane1.dirtree
    set hlist [$dirtree subwidget hlist]
    set tlist [$w.f.p.pane2.st subwidget tlist]
    $hlist delete all
}

# change name to add_tix_dir_browser_directory
proc tix_dir_browser_add_directory {w dir} {
    # this next might be done better
    set dirtree $w.f.p.pane1.dirtree
    tixDirTree:AddAncestors $dirtree $dir
}

proc tix_dir_browse_activate {w0 w index} {
#    puts [$w0 cget -value];    puts [$w entrycget $index -text]
    qcme_callback $w tix_dir_browse_activate [format "%s/%s" [$w0 cget  -value] [$w entrycget $index -text]]
}



proc TList:listdir {w dir} {
#    puts "TList:listdir $dir"
    $w delete 0 end
    set showdirs 1
    set appPWD [pwd]

    if [catch {cd $dir} err] {
	# The user has entered an invalid directory
	# %% todo: prompt error, go back to last succeed directory
#	puts "TList:listdir cannot cd to $dir"
	cd $appPWD
	return
    }

    if {$showdirs} {
	foreach fname [lsort [glob -nocomplain *]] {
	    if [file isdirectory $fname] {
		set image [tix getimage folder]
	    } else {
		continue
	    }
	    
	    $w insert end -itemtype imagetext \
		    -text $fname -image $image
    }   }

    foreach fname [lsort [glob -nocomplain *]] {
	if [file isdirectory $fname] {
	    continue
	} elseif [string match *.c $fname] {
	    set image [tix getimage srcfile]
	} elseif [string match *.h $fname] {
	    set image [tix getimage srcfile]
	} elseif [string match *.tcl $fname] {
	    set image [tix getimage file]
	} elseif [string match *.o $fname] {
	    set image [tix getimage file]
	} else {
	    set image [tix getimage textfile]
	}

	$w insert end -itemtype imagetext \
	    -text $fname -image $image
    }

    cd $appPWD
}

