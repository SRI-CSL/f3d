#ifndef __namespace_h
#define __namespace_h


#define FREEDIUS_NAMESPACE FREEDIUS

#if defined(FREEDIUS_NAMESPACE)

#define BEGIN_NAMESPACE_FREEDIUS namespace FREEDIUS_NAMESPACE {
#define END_NAMESPACE_FREEDIUS }

#else

#define BEGIN_NAMESPACE_FREEDIUS
#define END_NAMESPACE_FREEDIUS

#endif

#if !defined(__STDC__) && !defined(_WIN32)
#define IDENT(x) x
#define CAT(a,b) IDENT(a)b
#else
#define CAT(a,b) a##b
#endif

//#define FREEDIUS_PREFIX FREEDIUS_
//#define FREEDIUS_GLOBAL(name) CAT(FREEDIUS_PREFIX,name)

#define FREEDIUS_GLOBAL(name) FREEDIUS_##name

#endif /* ! __namespace_h */ 

