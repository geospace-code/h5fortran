#include <stdio.h>
#include "example3.h"


int main(void) {

long x = 321;
long y;

write_int32(&x);

read_int32(&y);

if (x != y) {
  fprintf(stderr, "ERROR: read/write mismatch value. Expected %ld but got %ld", x, y);
  return 1;
}

printf("OK: example 3");

return 0;
}
