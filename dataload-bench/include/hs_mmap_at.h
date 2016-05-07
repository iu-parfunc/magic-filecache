

#include <sys/types.h>
#include <sys/mman.h>

unsigned char *hs_mmap_at(size_t desiredDest, size_t len, int fd);
