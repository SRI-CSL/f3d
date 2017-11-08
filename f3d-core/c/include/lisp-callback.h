#ifndef __lisp_callback_h
#define __lisp_callback_h

//#include "namespace.h"

//BEGIN_NAMESPACE_FREEDIUS


/**********************************************************************************
Allegro and CMUCL have rather different restrictions w.r.t. callbacks:


In Allegro, the callback function can be moved by the garbage collector, thus
   the function address must be computed using lisp_call_address(callback_index)
   each time the callback is invoked.

In CMUCL (prior to Helmut_Eller's callback mechanism)

In CMUCL using Helmut_Eller's callback mechanism, the callback "trampoline"
   is stored in C malloc space, and thus does not move during garbage collection.
   Therefore, we can either 1) do exactly the same as is done in Allegro, or 2) we
   can store the address of the callback "trampoline".

**************************************************************************************/

// This enables the use of the Helmut Eller callback machinery.

#if defined(HAVE_ALLEGRO)

// New version for compatibility with CMUCL version

typedef void * (LISP_CALL_ADDRESS_FN)(int callback_index);

// This isn't used in Allegro 
// #define c2l_int(n) (n)

// The only difference between the C++ and C versions of these macros is
// that extern "C" is wrapped around some stuff in the C++ versions.

#if defined(__cplusplus)
#define MAYBE_EXTERN(form) extern "C" {form}
#else // !__cplusplus
#define MAYBE_EXTERN(form) form
#endif

// CAT(name,_lca) is the address of the Allegro lisp_call_address function.
// CAT(name,_callback_index) is the unique index for this callback
// Both are set by REGISTER-CALLBACK-NOW in $FREEDIUS/lisp/allegro/run-lcl.lisp

#define DEFCALLBACK(type, name, args, argnames)\
MAYBE_EXTERN(int CAT(name,_callback_index);)  \
MAYBE_EXTERN(LISP_CALL_ADDRESS_FN *CAT(name,_lca);)\
typedef type (CAT(name,_fn)) args;\
type name args{ \
 CAT(name,_fn)* CAT(name,_p) = (CAT(name,_fn) *) ((*CAT(name,_lca))(CAT(name,_callback_index)));\
 return ((* CAT(name,_p))argnames);\
}

#define DEFVOIDCALLBACK(type, name, args, argnames)\
MAYBE_EXTERN(int CAT(name,_callback_index);)\
MAYBE_EXTERN(LISP_CALL_ADDRESS_FN *CAT(name,_lca);)\
typedef type (CAT(name,_fn)) args;\
type name args{\
 CAT(name,_fn)* CAT(name,_p) = (CAT(name,_fn) *) ((*CAT(name,_lca))(CAT(name,_callback_index)));\
 (* CAT(name,_p))argnames;\
}

#elif defined(HAVE_CMUCL) || defined(HAVE_SBCL)

#if defined(QUAM_CMUCL_CALLBACKS)

/*
For CMUCL prior to the use of Helmut Eller's callback mechanism, 
all of the definitions are defined in the mechanically generated files:
$FREEDIUS/c/lisptk/lisptk-cmucl-callbacks.c and $FREEDIUS/c/misc/cmucl-lisp-callbacks.c

The following macros provide forward declarations and a few typedefs.
*/

#define DEFCALLBACK(type, name, args, argnames) \
type name args; // forward decl

#define DEFVOIDCALLBACK(type, name, args, argnames) \
type name args; // forward decl

#else // !defined(QUAM_CMUCL_CALLBACKS)

// Helmut Eller Callbacks

#if defined(__cplusplus)
#define MAYBE_EXTERN(form) extern "C" {form}
#else // !__cplusplus
#define MAYBE_EXTERN(form) form
#endif

/*
Here we generate C functions that directly call the trampoline function
using the callback_address which must be initialized from Lisp.
*/

#define DEFCALLBACK(type, name, args, argnames)\
typedef type (CAT(name,_fn)) args;\
MAYBE_EXTERN(void* CAT(name,_callback_address);)  \
type name args{ \
 CAT(name,_fn)* CAT(name,_p) = (CAT(name,_fn) *) CAT(name,_callback_address);\
 return ((* CAT(name,_p))argnames);\
}

#define DEFVOIDCALLBACK(type, name, args, argnames)\
typedef type (CAT(name,_fn)) args;\
MAYBE_EXTERN(CAT(name,_fn)* CAT(name,_callback_address);)  \
type name args{\
 (* CAT(name,_callback_address))argnames;\
}

#endif // !defined(QUAM_CMUCL_CALLBACKS) , ie Helmut Eller Callbacks

// Convert a C int to a Lisp fixnum
#define c2l_int(n) (((n)<<2))

// This should move to a Lisp dependent header file.
typedef unsigned long lispobj;

typedef lispobj (funcall1) (lispobj lisp_fn, int callback_index);

#else // UNKNOWN LISP
#error UNKNOWN LISP TYPE
#endif

//END_NAMESPACE_FREEDIUS

#endif /* ! __lisp_callback_h */

