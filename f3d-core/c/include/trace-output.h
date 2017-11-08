#ifndef __trace_output_h_
#define __trace_output_h_

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

  /* extern char trace_buffer[];
     extern int debug_trace_level; */

#error This file is deprecated...

void tprintf(int, char*, ...);

#ifdef __cplusplus
}
#endif

 END_NAMESPACE_FREEDIUS

#endif // !__trace_output_h_
