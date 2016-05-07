
#include "hs_mmap_at.h"

unsigned char *hs_mmap_at(size_t desiredDest, size_t len, int fd)
{
  //      void *result = mmap(desiredDest, len, PROT_READ, MAP_SHARED, fd, 0);
      void *result = mmap(desiredDest, len,
			  PROT_WRITE | PROT_READ,
			  MAP_FIXED | MAP_PRIVATE,
			  fd, 0);  
      if (result == MAP_FAILED)
            return (unsigned char *)0;
      else
            return (unsigned char *)result;
}
