#include <stdio.h>
#include <errno.h>
#include "misc.h"

#ifndef _WIN32
#include <sys/wait.h> 
#include <unistd.h>
#endif

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

// FIXME -- USE A CALLBACK TO LISP FOR IMPLEMENTING RUN-PROGRAM

#ifdef _WIN32

int
run_program(char **argv, char **envp, int waitp)
{
  int pid;
  int status;

  fprintf(stderr, "run_program: not implemented in Windoze.\n");
  fprintf(stderr, "run_program: exec failure (%s)\n", argv[0]);

  return(-1);
}

#else

int
run_program(char **argv, char **envp, int waitp)
{
  int pid;
  int status;

  pid = fork();
  if (pid== 0) {
    execve(argv[0], argv, envp);
    fprintf(stderr, "run_program: exec failure (%s) = %d\n", argv[0], errno);
    return(errno);
  } else if (waitp) {
    waitpid(pid, &status, 0);

    /*fprintf(stderr, "run_program done: status = %d\n", status);*/
    return(status);
  }
  else return(pid);
}


extern char **environ;

char **environ = 0;


#endif

int 
run_program_wait(char **argv)
{
  return(run_program(argv, environ, 1));
} 


#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#endif
#include <stdio.h>

#include <stdlib.h>

char *qcmebindir_path = 0;

extern "C" {
  
void FREEDIUS_GLOBAL(set_qcmebindir_path)(const char *path)
{ 
  qcmebindir_path = strdup(path);
}

} // end extern "C" 
    
char *
qcmebindir (void)
{
  tprintf(3, "qcmebindir_path = %s\n", qcmebindir_path);
  if (! qcmebindir_path)
    qcmebindir_path = getenv("QCMEBINDIR");
  if (! qcmebindir_path)
    error("qcmebindir_path not set");
  return(qcmebindir_path);
}


char *
QCME_executable_path(const char* program_name)
{
  char *bindir = qcmebindir();
  char *path = XALLOC(char, strlen(bindir)+strlen(program_name)+6);

  sprintf(path,"%s/%s", bindir, program_name);
  return(path);
}

#include "cme-error.h"


#if defined(_WIN32) || defined(LINUX) || defined(DARWIN)
/* WindOZe version of mkfile does a direct write: */

#define MAXSIZE 5000000

extern int mkfile(char *path, unsigned int size, int units) {
  int blocksize, bytesize, nbytes, n, pad, i, fd;
  char *buf;
  
  if ((fd = read_open_error_ok(path)) > 0) {
    close(fd); 
    fprintf(stderr, "mkfile %s: file already exists.\n", path);
    return(-1); /* already exists -- error  */
  } else {
  	tprintf(4,"mkfile: size =  %d;  file =  %s\n", size, path);
    if (units == 'b') bytesize = size;
    else if (units == 'k') bytesize = size*1024;
    else if (units == 'm') bytesize = size*1024*1024;
    else bytesize = size;
    /*  printf("bytesize = %d\n", bytesize); */

    if (bytesize < MAXSIZE) blocksize = bytesize;
    blocksize = MAXSIZE;

    pad = bytesize % blocksize;
    n = bytesize / blocksize;

    /*  printf("%d blocks of size %d.  Pad = %d\n", n, blocksize, pad); */

    buf = XALLOC(char, blocksize);
    memset(buf, 0, blocksize);

    fd = creat(path, 0777);
  
    for (i = 0; i < n; i++) write(fd, buf, blocksize);
    if (pad > 0) write(fd, buf, pad);
    close(fd);

    free(buf);
  
    return 0;
  }
}  

#else // ! windoze
/* units is a character 'b' 'k' 'm' or 0 */

extern int
mkfile(char *path, unsigned int size, int units)
{
  int fd;
  if ((fd = read_open_error_ok(path)) > 0) {
    close(fd); 
    fprintf(stderr, "mkfile %s: file already exists.\n", path);
    return(-1); /* already exists -- error  */
  } else {
    char *argv[4];
	char buf[512];
    char size_string[16];
    char *bindir = getenv("QCMEBINDIR");
    char *mkfile_path;
    int status;

    if (! bindir) error("environment variable QCMEBINDIR is not set");
    mkfile_path = QCME_executable_path("mkfile");

    /*    fprintf(stderr, "mkfile_path=%s\n", mkfile_path);*/

    switch (units) {
    case 'k': sprintf(size_string, "%dk", size); break;
    case 'm': sprintf(size_string, "%dm", size); break;
    default: sprintf(size_string, "%d", size); break;
    }

    /*   argv[0] = "/home/nansy1/quam/bin/mkfile";*/
    argv[0] = mkfile_path;
    argv[1] = size_string;
    argv[2] = path;
    argv[3] = (char *) 0;

    status = run_program_wait(argv);

    if (status) 
      fprintf(stderr, "mkfile %s: error code = %d.\n", mkfile_path, status);
    // free(mkfile_path);
    return(status);
  }
}

#endif // ! windoze


extern int
copy_file(const char *from, const char *to)
{
  char *argv[4];
  int status;
  char *cp_path = QCME_executable_path("cp");
  tprintf(3, "copy_file %s %s %s\n", cp_path, from, to);
  argv[0] = cp_path;
  argv[1] = (char *)from;
  argv[2] = (char *)to;
  argv[3] = (char *) 0;
  status = run_program_wait(argv);
  fprintf(stderr, "copy_file %s to %s\n", from, to);

  if (status) 
    fprintf(stderr, "copy_file failed: error code = %d.\n", status);
  free(cp_path);
  return(status);
}

extern "C" {
int cmkfile(char *path, unsigned int size, int units) {
  return mkfile(path, size, units);
}
} // end extern "C" 

END_NAMESPACE_FREEDIUS
