#include "namespace.h"


/* #ifndef  __STDC__ */
#if !defined(__STDC__) && !defined(_WIN32)
#define IDENT(x) x
#define CAT(a,b) IDENT(a)b
#define MKVAR(name) int FREEDIUS_GLOBAL(CAT(C_,name)) = (int)name;

#else

#define CAT(a,b) a##b
#define MKVAR(name) extern int C_##name; int FREEDIUS_GLOBAL(C_##name) = (int)name;

#endif
