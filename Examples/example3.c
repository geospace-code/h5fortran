#include <stdio.h>
#include <stdint.h>
#include "fortran_interface.h"



int main(void) {

int_least32_t x = 321;
int_least32_t y;

char filename[256] = "h5fortran_example3.h5";

write_int32(filename, "/x_c", &x);

read_int32(filename, "/x_c", &y);

if (x != y) {
  fprintf(stderr, "ERROR: read/write mismatch value. Expected %d but got %d", x, y);
  return 1;
}

printf("OK: example 3");

return 0;
}
