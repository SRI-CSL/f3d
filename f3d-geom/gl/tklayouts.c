#include <tk-private/tkInt.h>

// Compile with: 
// gcc -o tklayouts -DMAC_OSX_TK -framework Tk -I /System/Library/Frameworks/Tk.framework/Headers -I /System/Library/Frameworks/Tcl.framework/Headers tklayouts.c

struct TkWindowPrivate {
    TkWindow *winPtr;        /* Ptr to tk window or NULL if Pixmap */
    void *view;
    int context;
    int xOff;            /* X offset from toplevel window */
    int yOff;            /* Y offset from toplevel window */
    int size;
    void* visRgn;        /* Visible region of window */
    void* aboveVisRgn;    /* Visible region of window & its children */
    void* drawRgn;        /* Clipped drawing region */
    int referenceCount;        /* Don't delete toplevel until children are
                 * gone. */
    struct TkWindowPrivate *toplevel;
                /* Pointer to the toplevel datastruct. */
    int flags;            /* Various state see defines below. */
};

typedef struct TkWindowPrivate MacDrawable;

#if 0
struct TkWindowPrivate {
    void *winPtr;        /* Ptr to tk window or NULL if Pixmap */
    NSView *view;
    CGContextRef context;
    int xOff;            /* X offset from toplevel window */
    int yOff;            /* Y offset from toplevel window */
    CGSize size;
    HIShapeRef visRgn;        /* Visible region of window */
    HIShapeRef aboveVisRgn;    /* Visible region of window & its children */
    HIShapeRef drawRgn;        /* Clipped drawing region */
    int referenceCount;        /* Don't delete toplevel until children are
                 * gone. */
    struct TkWindowPrivate *toplevel;
                /* Pointer to the toplevel datastruct. */
    int flags;            /* Various state see defines below. */
};
#endif
/*
 * Change this to emit the declarations that will allow us to thread the above structure, rather than just the offsets.
 */

int
main()
{
  printf("/*  Offsets for threading Tk windows on Mac OS X: */\n");
  
  struct TkWindowPrivate a;
  TkWindow b;

  addr1 = &(b.privatePtr);
  addr2 = &b;
  printf("#define TKP_TKWIN_OFFSET  %lu\n\n", addr1 - addr2);
  printf("typedef struct TkWindowProxy {\n");
  printf("    char misc1[%lu];\n", addr1-addr2);
  printf("    struct MacDrawable *privatePtr;\n");
  printf("} TkWindowProxy;\n\n\n");
  char* addr1 = &(a.toplevel);
  char* addr2 = &(a.winPtr);
  
  printf("#define TKP_TOPLEVEL_OFFSET %lu\n\n", addr1 - addr2);
  printf("typedef struct MacDrawable {\n");
  printf("    TkWindowProxy *winPtr;\n");
  printf("    NSView *view;\n");
  printf("    CGContextRef context;\n");
  printf("    int xOff;                  /* X offset from toplevel window */\n");
  printf("    int yOff;                  /* Y offset from toplevel window */\n");
  printf("    CGSize size;\n");
  printf("    HIShapeRef visRgn;         /* Visible region of window */\n");
  printf("    HIShapeRef aboveVisRgn;    /* Visible region of window & its children */\n");
  printf("    HIShapeRef drawRgn;        /* Clipped drawing region */\n");
  printf("    int referenceCount;        /* Don't delete toplevel until children are\n");
  printf("                                * gone. */\n");
  printf("    struct MacDrawable *toplevel;\n");
  printf("                               /* Pointer to the toplevel datastruct. */\n");
  printf("    int flags;                 /* Various state see defines below. */\n");
  printf("} MacDrawable;\n\n\n");


}
