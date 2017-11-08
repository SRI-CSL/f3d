# LD_LIBRARY_PATH must be set correctly to find shared libraries
# In fcme do (tk::tcl-cmd `(source ,(namestring (truename "$FREEDIUS/tk/library/make_packages.tcl"))))

package require Tix 8.4

pkg_mkIndex -load Tix -verbose [format "%s/tk/library" $env(FREEDIUS)] qwidgets.tcl layout.tcl \
   property_list.tcl tk-utils.tcl tcl-utils.tcl fix-key-bindings.tcl \
   show-widget-doc.tcl show-menu-doc.tcl group-control.tcl composite-widgets.tcl \
   grips.tcl group-iconification.tcl \
   tix-dir-browse.tcl 

# toglwin.tcl glxwin.tcl 

#incomplete    tkfbox-hacks.tcl

