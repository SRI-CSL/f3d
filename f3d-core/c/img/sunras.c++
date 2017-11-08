#include "array-image.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>


#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#include <unistd.h>
#endif
#include <stdio.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS


#define sun_rasterfile_RAS_MAGIC 0x59a66a95

int read_number_from_big_endian_byte_array (char *arr, int n)
{
  int num=0;
  int radix=256;
  int k=1;
  int i;
  for (i=n-1; i>=0; i--, k*=radix)
    num += k*arr[i];
  return(num);
}

// THIS DOES NOT WORK FOR COLOR IMAGES
image * read_sunras_image (char *path)
{
  int fd;
  int buf[8];

  fd=read_open(path);
  if (! fd) return((image *) NULL);
  
  fullread(fd, (char*) buf, 32);
  
  {
    int magic=buf[0];
    int xdim=buf[1];
    int ydim=buf[2];
    int bps=buf[3];
    //    int raslen=buf[4];
    //int rastype=buf[5];
    //int maptype=buf[6];
    int maplen=buf[7];
    unsigned char *bufr = XALLOC(unsigned char, xdim);
    int *ibuf= XALLOC(int, xdim);
    int bytes_per_line = (bps*xdim)>>3;
    int x, y;

    if (magic != sun_rasterfile_RAS_MAGIC) {
      fprintf(stderr, "read_sunras_image: file %s is not a SUN rasterfile\n", path);
      return((image *) 0);
      }
    if (maplen > 0) {
      char *map= XALLOC(char, maplen);
      fullread(fd, map, maplen);
      free(map); // throw it away
    }
    image *img = make_array_image(xdim, ydim,  image_element_type_from_size(bps), 1, xdim, ydim);
    fprintf(stderr, "read_sunras_image:  xdim=%d, ydim=%d, bps=%d\n", xdim, ydim, bps);
    for (y=ydim-1; y>=0; y--) {
      fullread(fd, (char*) bufr, bytes_per_line);
      //     if (y == 512) {int x; for(x=0; x<xdim; x++) printf("%d ", bufr[x]);}
      for(x=0; x<xdim; x++) ibuf[x]=bufr[x];
      img->putline(ibuf, 0, y);
    }
    free(ibuf); free(bufr);
    return(img);
  }
}

END_NAMESPACE_FREEDIUS
