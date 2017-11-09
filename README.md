# f3d

The f3d system (a.k.a. FREEDIUS) is a portable, open-source version of
software developed by SRI under the DARPA Image Understanding Program.
This program was funded in the 1980s-1990s to push the state of the
art in softcopy satellite imagery analysis.  At various times,
predecessors to f3d went by the names "Image-Calc", "CME" and "IUE",
all of which represented progress toward integrated 2d and 3d analysis
of image data from arbitrary sensor sources.

To some extent, this software has been supplanted by libraries such as
OpenCV, and utilities like Google Earth.  Nonetheless, it captures
work done at SRI that long predates these efforts, and in some
respects, may still offer something novel.

The code is largely written in Lisp, with a core set of C++ libraries.
Supported Lisps are Allegro, CMUCL, and SBCL.  Rendering is
implemented using OpenGL.  The C++ code should automatically build on
loading into Lisp.  The code has been build and run on Linux, Windows
7, and Mac OS X.  On the Mac, you're best off using X11 as opposed to
Cocoa.

# Installing:

Checklist - Download these and you should be ready to go - on Linux,
most of these are available through apt-get.  On Macs, try 'brew' or
the websites:

o Quicklisp at http://www.quicklisp.org/

o Steel Bank Common Lisp ver. 1.3.8 at https://sourceforge.net/projects/sbcl/files/sbcl/

o CMake at https://cmake.org/

o Emacs at https://www.gnu.org/software/emacs/

o SLIME at https://common-lisp.net/project/slime/

o Tcl/Tk 8.6


f3d is quicklisp-compatible (see http://www.quicklisp.org/).  Once you
have a quicklisp/local-projects directory, you can git clone f3d.git
there.

SBCL (Steel Bank Common Lisp at http://www.sbcl.org/) is an
open-source Common Lisp that is supported on all the platforms that
f3d runs on.  As of this writing, you should use version 1.3.8 for
maximum compatibility with other things.  In theory, f3d will also
work on Allegro Common Lisp and CMU Common Lisp, but these haven't been
tested in a while.

Dependencies: f3d depends on OpenGL and Tcl/Tk for graphics and GUI
operations.  Tcl/Tk version 8.6 is probably best.

You are encouraged to use Emacs (https://www.gnu.org/software/emacs/)
and SLIME (https://common-lisp.net/project/slime/) to interact with f3d.

CMake (https://cmake.org/) is used to build the C libraries, so be
sure to download that.

If quicklisp is installed, you can start Lisp and type the following
at the '*' prompt (or in the *inferior-lisp* buffer, if using Emacs):

(ql:quickload :f3d-tk)

This is how you load the full f3d system.  Upon the first loading, it
will compile itself.  KNOWN BUG: If you're using SLIME, do NOT start
f3d from the SLIME window (the '>' prompt) or it will hang.

Once it has been loaded, at the '*' prompt (or in the *inferior-lisp*
window), do this:

(start-cme)

and you should see a blue menu.


# Running

The core C++ libraries for f3d are written portably, so they should
compile on each of Windows, Linux, and MacOSX.  