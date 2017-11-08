/*
 * tkMacOSXSubwindows.c --
 *
 *	Implements subwindows for the macintosh version of Tk.
 *
 * Copyright (c) 1995-1997 Sun Microsystems, Inc.
 * Copyright 2001-2009, Apple Inc.
 * Copyright (c) 2006-2009 Daniel A. Steffen <das@users.sourceforge.net>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkMacOSXSubwindows.c,v 1.31 2009/07/06 20:29:21 dkf Exp $
 */

#include "tkMacOSXPrivate.h"
#include "tkMacOSXDebug.h"
#include "tkMacOSXWm.h"

/*
 *----------------------------------------------------------------------
 *
 * TkMacOSXDrawableView --
 *
 *	This function returns the NSView for a given X drawable.
 *
 * Results:
 *	A NSView* or nil.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

NSView*
TkMacOSXDrawableView(
    MacDrawable *macWin)
{
    NSView *result = nil;

    if (!macWin) {
	result = nil;
    } else if (!macWin->toplevel) {
	result = macWin->view;
    } else if (!(macWin->toplevel->flags & TK_EMBEDDED)) {
	result = macWin->toplevel->view;
    } else {
	TkWindow *contWinPtr = TkpGetOtherWindow(macWin->toplevel->winPtr);
	if (contWinPtr) {
	    result = TkMacOSXDrawableView(contWinPtr->privatePtr);
	}
    }
    return result;
}
