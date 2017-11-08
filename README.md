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
implemented using OpenGL.  At one point, the code could be built under
Linux, MacOSX, and Windows.  Some bit rot has set in, so this
paragraph will be updated when we're able to clean things up.
