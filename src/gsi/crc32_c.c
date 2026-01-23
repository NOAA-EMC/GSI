#include <stdio.h>
#include <string.h>
#include <zlib.h>
 
uLong digest_c(char * message) {
  return crc32(0, (const void*)message, strlen(message));
}
