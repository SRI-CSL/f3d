// -*- Mode: C++; c-basic-offset: 2 -*-

// $Id: io.c++,v 1.9.4.2 2008/01/09 06:47:18 connolly Exp $

// Rename this file to expand-pathname.c++?
// expand_pathname is the only function used from this file



#include "misc.h"

#include <stdio.h>
#include <fcntl.h>
#include <string.h>

#if !defined(_WIN32) && !defined(_MSC_VER)
#include <sys/types.h>
#endif

#include <sys/stat.h>

/* need to extend these definitions to include shell pathname expansion 
 */

#include "cme-error.h"
#include <string.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#ifdef _WIN32

extern char *expand_pathname (char *pathname)
{
  // FILE *popen(char*,char*);
  
  if (strchr(pathname, '$') == NULL) return pathname;
  {
    FILE *file;
    int ch;
    int n = 1000;
    char line[1000];
    fprintf(stderr,"Sorry, Windows cannot do pathname expansion for %s.\n", pathname);
    /* Need to translate pathnames some other way. */
    return strdup(pathname);
  } 
}

#else

#ifdef __GNUC__
extern char* expand_pathname (char *pathname)
{
  
  if (strchr(pathname, '$') == NULL) return pathname;
  {
    FILE *file;
    int ch;
    int n = 1000;
    char line[1000];
    char command[strlen(pathname)+20];
    /*  sprintf(command, "sh -c 'echo %s'", argv[1]);  sh doesn't do tilde expansion */
    sprintf(command, "csh -c \"echo %s\"", pathname);
    file = popen(command, "r");
    if (file == 0) {
      error ("expand_pathname could not create pipe.");
    }
    fgets(line, n, file);
    pclose(file);
    line[strlen(line)-1] = 0;  /* stomp out trailing NEWLINE */
    return strdup(line);
  }
} 
#else
extern char *expand_pathname (char *pathname)
{
	FILE *popen(char*,char*);
	char *strdup(char*);
  
  if (strchr(pathname, '$') == NULL) return pathname;
  {
    FILE *file;
    int ch;
    int n = 1000;
    char line[1000];
    char *command = XALLOC(char, strlen(pathname)+20);
    /*  sprintf(command, "sh -c 'echo %s'", argv[1]);  sh doesn't do tilde expansion */
    sprintf(command, "csh -c \"echo %s\"", pathname);
    file = popen(command, "r");
    if (file == 0) {
      error ("expand_pathname could not create pipe.");
    }
    fgets(line, n, file);
    pclose(file);
    line[strlen(line)-1] = 0;  /* stomp out trailing NEWLINE */
    free(command);
    return strdup(line);
  } 
}
#endif /* GNU_C */
#endif

// The rest of this file is unused

#if 0 // unused

int file_stat_mode (char *path)
{
  struct stat buf;
  if (stat(path, &buf) ==0 )
    return buf.st_mode;
  else return 0;
} 



#ifdef _WIN32

/* Links are not really supported in Windoze.  There is a similar
   animal, but it's not clear that we would want to use it: */

int file_lstat_mode (char *path)
{
  struct stat buf;
  if (stat(path, &buf) ==0 )
    return buf.st_mode;
  else return 0;
} 

int directory_p (int mode)
{return (mode & S_IFDIR);
}

int symbolic_link_p (int mode)
{return (0);
}

int file_lstat_length (char *path)
{struct stat buf;
 if (stat(path, &buf) ==0 )
   return buf.st_size;
 else return -1;
} 
#else

int file_lstat_mode (char *path)
{
  struct stat buf;
  if (lstat(path, &buf) ==0 )
    return buf.st_mode;
  else return 0;
} 

int directory_p (int mode)
{return (S_ISDIR(mode));
}

int symbolic_link_p (int mode)
{return (S_ISLNK(mode));
}

int file_lstat_length (char *path)
{struct stat buf;
 if (lstat(path, &buf) ==0 )
   return buf.st_size;
 else return -1;
} 
#endif



extern "C" {

  // This is temporary.  The Fedora cross-compile environment has an
  // MSVCRT version that inhibits the declaration of this struct -
  // need to figure out why:

#ifdef MINGW
struct __stat64
{
    _dev_t st_dev;
    _ino_t st_ino;
    _mode_t st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    __int64 st_size;
    __time64_t st_atime;
    __time64_t st_mtime;
    __time64_t st_ctime;
};
#endif


#if defined(_WIN32)
__int64 file_length (char *path)
{
#ifdef MINGW
  _CRTIMP int __cdecl __MINGW_NOTHROW _stat64 (const char*, struct __stat64*);
#endif

  struct __stat64 buf;
  if (_stat64 (path, &buf) < 0)
    return 0;
  return buf.st_size;
}
#else
off_t file_length (char *path)
{
  struct stat buf;
  if (stat (path, &buf) <0)
    return 0;
  return buf.st_size;
}
#endif


FILE* unix_stderr (void) {
  return stderr;
}

} // end extern "C"

#endif // unused


END_NAMESPACE_FREEDIUS
