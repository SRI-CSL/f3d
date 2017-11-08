#ifndef	__Object_h
#define __Object_h


#include "misc.h"
#include <stdlib.h>  // stdlib.h is needed to get size_t definition.
#include <stdio.h>

#include <typeinfo>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS


/* **********************************************

  Fri Jan  4 2002

  Flushing the use of this meta-object stuff.

  Trying to use typeid and type_info machinery.

 **********************************************  */

class Object {
 public:
#if 0
  void* operator new(size_t size);    
  void operator delete(void *obj);    
#endif
#if 1 // Mon Aug  2 2004:  eliminate the need for calls to typeid.
  // Christopher Connolly is having problems with gcc 3.3 for Panther.
  // className isn't really used anywhere.
  virtual const char *className() {
    return typeid(*this).name();
  }
#endif
  Object(){}
};

/* **********************************************

  c++ is a total crock w.r.t. polymorphism.  I am finding it impossible
  to get dynamic typing to really work.  In gdb, trying to do:

          p *((<type> *) obj)

  when <type> is the actual (dynamic) type of obj doesn't print the correct
  contents of the slots that are in <type> but not in the class statically
  declared for obj.  gdb shows me the slots, but the values are bogus.

 **********************************************  */

class Class :public Object {
 public:
  static Class *clas;
  Class (string name, string superClass_name):Object() {}
};

#define setClass(className, superClassname)


END_NAMESPACE_FREEDIUS

#endif /* ! __Object_h */
