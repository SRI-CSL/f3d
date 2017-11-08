package provide qtk 1.0

# style layout TMenubutton

msg "Loading themes.tcl"

if {$tk_version < 8.5} {
    set auto_path [concat $auto_path "/usr/local/lib"]
    package require tile
    package provide tile::theme::freedius 1.0
    namespace eval tile {namespace export style setTheme }
    namespace import -force tile::style
    namespace eval ttk {
	namespace import -force ::default ::classic ::alt ::winnative ::xpnative ::aqua 
    }
    #	puts [list namespace [namespace current]]
    namespace eval ttk { 
	namespace import -force ::tile::setTheme 
	namespace export setTheme
    }
} else {
    package require Tk 8.5
    package provide ttk::theme::freedius 1.0
    namespace import -force ::ttk::style
}

namespace eval ttk::theme::freedius {

    variable FREEDIUS_default_background

    #set FREEDIUS_default_background #accaae
    set FREEDIUS_default_background LightSkyBlue3
    set FREEDIUS_default_background #d9d9d9
    
    # Create freedius theme if it doesn't already exist
    catch {style theme create freedius -parent alt}
    
    style theme settings freedius {
	
	# begin copy of alt theme
	variable colors
	array set colors [list \
			      -frame 		$FREEDIUS_default_background \
			      -darker 	"#c3c3c3" \
			      -activebg 	"#ececec" \
			      -disabledfg	"#a3a3a3" \
			      -selectbg	"#4a6984" \
			      -selectfg	"#ffffff" \
			      -tooltips "#fab2be"]
	
	
	
	style configure "." \
	    -background 	$colors(-frame) \
	    -foreground 	black \
	    -troughcolor	$colors(-darker) \
	    -selectbackground 	$colors(-selectbg) \
	    -selectforeground 	$colors(-selectfg) \
	    -font 		TkDefaultFont \
	    ;
	
	style map "." -background \
	    [list disabled $colors(-frame)  active $colors(-activebg)] ;
	style map "." -foreground [list disabled $colors(-disabledfg)] ;
	style map "." -embossed [list disabled 1]
	
	style configure TButton \
	    -width -11 -padding "1 1" -relief raised -shiftrelief 1 \
	    -highlightthickness 1 -highlightcolor $colors(-frame)
	
	style map TButton -relief {
	    {pressed !disabled} 	sunken
	    {active !disabled} 	raised
	} -highlightcolor {alternate black}
	
	style configure TCheckbutton -indicatorcolor "#ffffff" -padding 2
	style configure TRadiobutton -indicatorcolor "#ffffff" -padding 2
	style map TCheckbutton -indicatorcolor \
	    [list  disabled $colors(-frame)  pressed $colors(-frame)]
	style map TRadiobutton -indicatorcolor \
	    [list  disabled $colors(-frame)  pressed $colors(-frame)]
	
	style configure TMenubutton -width -11 -padding "3 3" -relief raised
	
	style configure TEntry -padding 1
	style map TEntry -fieldbackground \
	    [list readonly $colors(-frame) disabled $colors(-frame)]
	style configure TCombobox -padding 1
	style map TCombobox -fieldbackground \
	    [list readonly $colors(-frame) disabled $colors(-frame)]
	
#	style configure Toolbutton -relief flat -padding 2 -borderwidth 0
	style configure Toolbutton -relief flat -padding 2
	style map Toolbutton -relief \
	    {disabled flat selected sunken pressed sunken active raised}
	style map Toolbutton -background \
	    [list pressed $colors(-darker)  active $colors(-activebg)]
	
	style configure TScrollbar -relief raised
	
	style configure TLabelframe -relief groove -borderwidth 2
	
	style configure TNotebook -tabmargins {2 2 1 0}
	style configure TNotebook.Tab \
	    -padding {4 2} -background $colors(-darker)
	style map TNotebook.Tab \
	    -background [list selected $colors(-frame)] \
	    -expand [list selected {2 2 1 0}] \
	    ;
	
	style configure TScale \
	    -groovewidth 4 -troughrelief sunken \
	    -sliderwidth raised -borderwidth 2
	style configure TProgressbar \
	    -background $colors(-selectbg) -borderwidth 0
	
# end copy of alt theme
# mods start here (except for change of colors)
	
	style configure "." \
	    -borderwidth 	1 \
	    -foreground 	black \
	    #-font               "sans-serif 10 bold" \
	    -font               "helvetica 10" \
	    -selectborderwidth	1 \
	    -insertwidth 	1 \
	    -indicatordiameter	10 \
	    ;
	# I do not understand why the next is needed
	style configure TLabel -font "helvetica 10"

	style configure TButton -padding 0 -width {} -relief raised -shiftrelief 1
	style configure TCheckbutton -indicatorcolor "#ffffff" -padding 0
	style configure TRadiobutton -indicatorcolor "#ffffff" -padding 1
	if {0} {
	    # old version
	    style configure TMenubutton -padding "3 1" -width {} -relief raised -anchor w	  
	} else {
	    # New version, eliminates the 
	    style configure TMenubutton -padding "5 1" -borderwidth 0 -width {} \
		-relief raised -anchor center	  
#		-relief raised -anchor w	  
	    style layout TMenubutton {
		TMenubutton.border -children {
		    TMenubutton.padding -children {
			TMenubutton.label
		    }
		}
	    }
	}
	
	style configure Toolbutton -padding 2 -relief ridge
	style map Toolbutton -relief \
	    {disabled flat selected sunken pressed sunken active raised}   
	# $colors(-frame) = "#d9d9d9"
	style map Toolbutton -background \
	    [list pressed $colors(-darker)  selected #ffffc0 active $colors(-activebg) ]
	
	style configure Groupbutton -relief flat -padding 2
	style map Groupbutton -relief \
	    {disabled flat selected sunken pressed sunken active raised}
	style map Groupbutton -background \
	    [list pressed $colors(-darker) selected $colors(-darker) active $colors(-activebg)]
	style layout Groupbutton {
	    Groupbutton.border -children {
		Groupbutton.padding -children {
		    Groupbutton.label
		}
	    }
	}
	style configure TEntry -relief  flat -padding 2 -fieldbackground $colors(-frame)
	#style configure TEntry -relief  flat -padding 2 -fieldbackground white
	
	# layout without the Entry.field element which draws the Entry with -relief sunken
	style layout Flat.TEntry {
	    Entry.padding -sticky nswe -children {
		Entry.textarea -sticky nswe
	    }
	}
	
	
	style configure Flat.TEntry  -padding 2 -fieldbackground $colors(-frame)
	
	#option add Qcme.ControlPanel*background $colors(-frame)
	option add *background $colors(-frame)
	option add Qcme.doc_widget_frame.doc_widget.background $colors(-tooltips)
    }
}

ttk::setTheme freedius
