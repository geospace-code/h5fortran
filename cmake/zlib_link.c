#include <zlib.h>
int main(void) {
  Bytef src[1] = {0};
  Bytef dest[100];
  uLongf dest_len = sizeof(dest);
  compress(dest, &dest_len, src, sizeof(src));
  return 0;
}
