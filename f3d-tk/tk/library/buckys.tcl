
proc update_buckys {window state mask onoff} {
#    puts "update_buckys $mask $onoff"
    qcme_lisp_callback $window update_buckys [list $state $mask $onoff]
}


bind Togl <Key-Alt_L> {update_buckys %W %s alt 1}
bind Togl <Key-Control_L> {update_buckys %W %s control 1}
bind Togl <Key-Meta_L> {update_buckys %W %s meta 1}
bind Togl <Key-Shift_L> {update_buckys %W %s shift 1}

bind Togl <KeyRelease-Alt_L> {update_buckys %W %s alt 0}
bind Togl <KeyRelease-Control_L> {update_buckys %W %s control 0}
bind Togl <KeyRelease-Shift_L> {update_buckys %W %s shift 0}

bind Togl <Enter> {focus %W
                   update_buckys %W %s enter 0}

bind Togl <Leave> {update_buckys %W 0 leave 0}

# <KeyRelease-Meta_L> never fires:
bind Togl <KeyRelease-Meta_L> {update_buckys %W %s meta 0}

# Something is weird with tk: doesn't detect <KeyRelease-Meta_L>.  keycode is right, but keysym is
# unknown, ie %K in event is ??

bind Togl <KeyRelease> {
#    puts "<KeyRelease> %%k=%k %%K=%K %%A=%A"
    if {%k ==  37} {
	update_buckys %W %s meta 0
    }    
    if {%k ==  64} {
	update_buckys %W %s control 0
    }    
    if {%k ==  50} {
	update_buckys %W %s shift 0
    }    
}


bind Togl <KeyPress> {
#    puts "<KeyPress> %%k=%k %%K=%K %%A=%A"
      
    if {%k ==  64} {
	update_buckys %W %s control 1
    }    
    if {%k ==  50} {
	update_buckys %W %s shift 1
    }    
}

