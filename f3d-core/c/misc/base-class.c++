// Is there any reason for this file?

#ifndef _WIN32
#include <strings.h>
#endif

#include "misc.h"
#include "base-class.h"
#include <stdio.h>

#include "namespace.h"

// In gcc-4.0 these operators cannot be declared in a namespace

//BEGIN_NAMESPACE_FREEDIUS

/*
LHQ - Fri Jun 13 2003

  EXPECTING NEW INSTANCES TO AUTOMATICALLY HAVE ZEROED SLOTS CANNOT WORK FOR
  STACK ALLOCATED OBJECTS.  

  The initializer of every object must properly initialize every slot.

  Weird - there appear to be a problem with calloc and namespaces.
  For some reason, the slots of gl_tex_page_queue_entry are not getting zeroed 
  by new when the FREEDIUS namespace is used.  The problem went away when 
  gl_tex_page_queue_entry::gl_tex_page_queue_entry initialized the slots.


*/

// This allows setting GDB breakpoints
void * calloc_debug(size_t nmemb, size_t size)
{
  void *ptr = calloc(nmemb,  size);
  return ptr;
}

void operator delete(void *obj){
  FREEDIUS::xxfree(obj); // why?  
}

void* operator new(size_t size){
  return calloc_debug(size, 1);
}

// G++ new[] does not zero the array, whereas Irix CC new[] zeros the elements.

#if defined(__GNUG__)

void operator delete[](void *obj){
  FREEDIUS::xxfree(obj);
}

void* operator new[](size_t size){
  return calloc_debug(size, 1);
}
#endif //defined(__GNUG__)

//END_NAMESPACE_FREEDIUS
