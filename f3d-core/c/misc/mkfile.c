/* Copyright (C) 2001 by Christopher Connolly, SRI International */

/* Hack version of mkfile for use on linux and windoze.  Creates files
   of specified length, filled with zeros.  You must compile this
   separately and place it in the appropriate bin directory:

   This will work fine on most systems:
   "cc -o mkfile mkfile.c" 

   "mv mkfile $GEOFEX/arch/linux/bin"
*/

/* #include <sys/types.h>
#include <sys/stat.h> */
#include <fcntl.h>

#define MAXSIZE 5000000

int make_file(char *path, unsigned long size, int units) {
  int blocksize, bytesize, nbytes, n, pad, i, fd;
  char *buf;

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

  buf = (char*) malloc(blocksize);
  memset(buf, 0, blocksize);

  fd = creat(path, 0777);
  
  for (i = 0; i < n; i++) write(fd, buf, blocksize);
  if (pad > 0) write(fd, buf, pad);
  close(fd);

  free(buf);
  
  return n;
}  


main(int argc, char **argv) {
  long size, slen;
  int units;

  if (argc != 3) printf("Error -- this version of mkfile only takes two args, the file size + filename.\n");
  else {
    slen = strlen(argv[1]);
    if (isdigit(argv[1][slen-1])) units = 'b';
    else {
      units = argv[1][slen-1];
      argv[1][slen-1] = 0;
    }
    size = atol(argv[1]);
    make_file(argv[2], size, units);
  }
}

			
