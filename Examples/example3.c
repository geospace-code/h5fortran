#include <stdio.h>
#include "fortran_interface.h"



int main(void) {

long x = 321;
long y;

char filename[256] = "h5fortran_example3.h5\0";

write_int32(filename, &x);

read_int32(filename, &y);

if (x != y) {
  fprintf(stderr, "ERROR: read/write mismatch value. Expected %ld but got %ld", x, y);
  return 1;
}

printf("OK: example 3");

return 0;
}
