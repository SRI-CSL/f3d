#include <stdlib.h>
#include <stdarg.h>
#ifndef _WIN32
#include <strings.h>
#endif
#include <mymalloc.h>
#include "cme-list.h"
#include "misc.h"
#include "cme-error.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

//#define NIL 0 is defined in misc.h and is adequate.
//#define NIL ((POINTER_LIST) 0)

/* this sucks -- specialized to string compare only */
int equal (PROPKEY key1, PROPKEY key2) {
  return(strcmp(key1, key2) ==0);
}

POINTER_LIST cons (LISTELEM car, POINTER_LIST cdr) {

  POINTER_LIST cell = XALLOC(Pointer_List, 1);

  if (! cell) error("cons: galloc failed\n");
  cell->car = car;
  cell->cdr = cdr;
  return(cell);
}

POINTER_LIST ncons (LISTELEM car) {
  return cons(car, NIL);
}


POINTER_LIST nreverse (POINTER_LIST list) {

  POINTER_LIST prev = NIL;
  POINTER_LIST next;
  while (list) {
    next = list->cdr;
    list->cdr = prev;
    prev = list;
    list = next;
  }
  return(prev);
}

#if 0

POINTER_LIST
listx (char *arg0, ...)
{
  va_list elems;
  POINTER_LIST list = cons(arg0, NIL);
  char *arg1;
  cerror("listx breakpoint:");
  va_start(elems, arg0);
  while (arg1 = va_arg(elems, char *)) 
    list=cons(arg1, list);
  return(nreverse(list));
}

#else

POINTER_LIST
listx (LISTELEM arg0, ...)
{
  va_list elems;
  POINTER_LIST list = cons(arg0, NIL);
  LISTELEM arg1;
  va_start(elems, arg0);
  while (arg1 = va_arg(elems, char *))
    list=cons(arg1, list);
  return(nreverse(list));
}
#endif

int verify_property_list (POINTER_LIST l)
{
  while (l) {
    if (l->cdr == 0) return 0; // odd number of elements
    l=l->cdr->cdr;
  }
  return 1;
}

POINTER_LIST
get_prop_slot (POINTER_LIST l, PROPKEY key)
{
  while (l) {
    if (equal(key, (PROPKEY)l->car))
      return (l);
    l=l->cdr->cdr;
  }
  return(NIL);
}

LISTELEM 
get_prop (POINTER_LIST l, PROPKEY key, LISTELEM defalt)
{
  while (l) {
    if (equal(key, (PROPKEY)l->car))
      return(l->cdr->car);
    l=l->cdr->cdr;
  }
  return(defalt);
}

float cast_int_to_float (int n)
{
  float *x = (float *) &n;
  return *x;
}

int cast_float_to_int (float x)
{
  int *n = (int *) &x;
  return *n;
}

float cast_long_to_float (long n)
{
  float *x = (float *) &n;
  return *x;
}

long cast_float_to_long (float x)
{
  long *n = (long *) &x;
  return *n;
}

int
get_int_prop (POINTER_LIST l, PROPKEY key, int defalt)
{
  return (long)(get_prop(l, key, (LISTELEM) defalt));
}

float
get_float_prop (POINTER_LIST l, PROPKEY key, float defalt)
{
  return cast_long_to_float((long)(get_prop(l, key, (LISTELEM) cast_float_to_long(defalt))));
}

void
put_prop (POINTER_LIST *l, PROPKEY key, LISTELEM value)
{
  POINTER_LIST found = get_prop_slot(*l, key);
  if (found)
    found->cdr->car = value;
  else *l = cons((void *)key, cons(value, *l));
}
  
void
put_float_prop (POINTER_LIST *l, PROPKEY key, float value)
{
  put_prop(l, key, (LISTELEM) cast_float_to_long(value));
}
  
void
put_int_prop (POINTER_LIST *l, PROPKEY key, int value)
{
  put_prop(l, key, (LISTELEM) value);
}
 
POINTER_LIST
assoc (POINTER_LIST l, PROPKEY key)
{
  while (l) {
    if (equal(key, (PROPKEY)((POINTER_LIST) (l->car))->car))
      return (POINTER_LIST)(l->car);
    l=l->cdr;
  }
  return(NIL);
}
 


POINTER_LIST
member (PROPKEY key, POINTER_LIST l)
{
  while (l) {
    if (equal(key, (PROPKEY)l->car))
      return (l);
    l=l->cdr;
  }
  return(NIL);
}

int
memberp (PROPKEY key, POINTER_LIST l)
{
  while (l) {
    if (equal(key, (PROPKEY)l->car))
      return (1);
    l=l->cdr;
  }
  return(0);
}

int
memq (PROPKEY key, POINTER_LIST l)
{
  while (l) {
    if (key == l->car)
      return (1);
    l=l->cdr;
  }
  return(0);
}
 
POINTER_LIST
push (LISTELEM elem, POINTER_LIST *p)
{return(* p = cons(elem, *p));
}


POINTER_LIST
pushnew (PROPKEY elem, POINTER_LIST *p)
{
  if (! member(elem, *p))
    *p = cons((void *)elem, *p);
  return(*p);
}


POINTER_LIST
last (POINTER_LIST l)
{
  POINTER_LIST prev;
  if (! l)
    return l;
  prev=l;
  while (l = l->cdr)
    prev = l;
  return(prev);
}

POINTER_LIST
nconc (POINTER_LIST l1, POINTER_LIST l2)
{
  POINTER_LIST last1;
  if (! l1)
    return(l2);
  else if (! l2)
    return(l1);
  else if (last1 = last(l1))
    last1->cdr = l2;
  return(l1);
}

END_NAMESPACE_FREEDIUS
