package provide qtk 1.0

style theme settings  alt {
	style configure "." \
	    -borderwidth 	1 \
	    -foreground 	black \
            #-font               "sans-serif 10 bold" \
	    -font               "helvetica 10" \
	    -selectborderwidth	1 \
	    -insertwidth 	1 \
	    -indicatordiameter	10 \
	    ;
    set colors(-frame) #accaae 
    style configure TButton -padding 0 -width -9 -relief raised -shiftrelief 1
    style configure TCheckbutton -indicatorcolor "#ffffff" -padding 0
    style configure TRadiobutton -indicatorcolor "#ffffff" -padding 0
    #style configure TMenubutton -width -11 -padding 1 -relief raised -anchor w
    style configure TMenubutton -padding "3 1" -width {} -relief raised -anchor w	
    style configure Toolbutton -padding 2 -relief ridge
    style map Toolbutton -relief \
	    {disabled flat selected sunken pressed sunken active raised}   
   # $colors(-frame) = "#d9d9d9"
    style map Toolbutton -background \
	    [list pressed $colors(-darker)  selected #d9ffd9 active $colors(-activebg) ]

    style configure TEntry -relief  flat -padding 2 -fieldbackground $colors(-frame)
    #style configure TEntry -relief  flat -padding 2 -fieldbackground white

    # layout without the Entry.field element which draws the Entry with -relief sunken
    style layout Flat.TEntry {
	Entry.padding -sticky nswe -children {
	    Entry.textarea -sticky nswe
	}
    }
    style configure Flat.TEntry  -padding 2 -fieldbackground $colors(-frame)


    option add Qcme.ControlPanel*background $colors(-frame)

}

#ttk::setTheme alt
