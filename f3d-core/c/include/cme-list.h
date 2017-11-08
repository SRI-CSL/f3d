#ifndef	__list_h
#define __list_h


#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS


typedef void *LISTELEM;

typedef struct pointer_list_struct {
  LISTELEM car;
  struct pointer_list_struct *cdr;
} Pointer_List;

typedef Pointer_List *POINTER_LIST;
typedef Pointer_List *INT_LIST; /* This is really bogus  */
typedef const char* PROPKEY;

/* Linux allegro libacl503.so has cons and nconc defined as a globals.  Since these functions are
   now defined in C++, there is name-mangling, so there will not be global conflicts.
*/
POINTER_LIST cons (LISTELEM car, POINTER_LIST cdr);
POINTER_LIST ncons (LISTELEM car);
POINTER_LIST nreverse (POINTER_LIST list);
POINTER_LIST listx (LISTELEM arg0, ...); /* BE SURE LAST ARG IS NULL */
POINTER_LIST push (LISTELEM elem, POINTER_LIST *p);
POINTER_LIST pushnew (LISTELEM elem, POINTER_LIST *p);
POINTER_LIST last (POINTER_LIST l);
POINTER_LIST nconc (POINTER_LIST l1, POINTER_LIST l2);

POINTER_LIST assoc (POINTER_LIST l, PROPKEY key);
POINTER_LIST member (PROPKEY key, POINTER_LIST l);
int memberp (PROPKEY key, POINTER_LIST l);
int memq (PROPKEY key, POINTER_LIST l);

/* PROPERTY LISTS */
LISTELEM get_prop (POINTER_LIST l, PROPKEY key, LISTELEM def=0);
int get_int_prop (POINTER_LIST l, PROPKEY key, int def=0);
float get_float_prop (POINTER_LIST l, PROPKEY key, float def=0);
void put_prop (POINTER_LIST *l, PROPKEY key, LISTELEM value);
void put_int_prop (POINTER_LIST *l, PROPKEY key,int value);
void put_float_prop (POINTER_LIST *l, PROPKEY key, float value);


END_NAMESPACE_FREEDIUS

#endif /* ! __list_h */ 

