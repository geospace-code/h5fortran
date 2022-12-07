#include <iostream>
#include <cstdlib>
#include <cstdint>
#include "fortran_interface.h"


int main(void) {

int32_t x = 321, y;

char filename[256] = "h5fortran_example4.h5";
char varname[64] = "/x_cpp";

write_int32(filename, varname, &x);

read_int32(filename, varname, &y);

if (x != y) {
  std::cerr << "ERROR: read/write mismatch value. Expected " << x << " but got " << y << std::endl;
  return EXIT_FAILURE;
}

std::cout << "OK: example 4" << std::endl;

return EXIT_SUCCESS;
}
