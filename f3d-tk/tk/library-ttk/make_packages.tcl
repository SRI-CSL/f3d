# LD_LIBRARY_PATH must be set correctly to find shared libraries 

# (tk::tcl-cmd `(source ,(namestring (truename "$FREEDIUS/tk/library-ttk/make_packages.tcl"))))
# source make_packages.tcl

package require Tix 8.4


if {1} {
pkg_mkIndex -load Tix -verbose [format "%s/tk/library8.5" $env(FREEDIUS)] \
    themes.tcl qwidgets.tcl layout.tcl \
   property_list.tcl tk-utils.tcl tcl-utils.tcl fix-key-bindings.tcl \
   show-widget-doc.tcl show-menu-doc.tcl group-control.tcl composite-widgets.tcl \
   grips.tcl group-iconification.tcl \
    tix-dir-browse.tcl 
} else {

pkg_mkIndex [format "%s/tk/library8.5" $env(FREEDIUS)] tix-dir-browse.tcl 

}


# (tk::tcl-cmd '(info exists "running_pkg_mkIndex"))
# (tk::tcl-cmd '(list "$running_pkg_mkIndex"))