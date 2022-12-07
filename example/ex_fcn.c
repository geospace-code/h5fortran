#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "fortran_interface.h"



int main(void) {

int32_t x = 321, y;

char filename[256] = "h5fortran_example3.h5";
char varname[64] = "/x_c";

write_int32(filename, varname, &x);

read_int32(filename, varname, &y);

if (x != y) {
  fprintf(stderr, "ERROR: read/write mismatch value. Expected %d but got %d", x, y);
  return 1;
}

printf("OK: example 3");

return 0;
}
