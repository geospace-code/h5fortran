#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

int main(void){

if(H5open() != 0){
  fprintf(stderr, "H5open() failed");
  return EXIT_FAILURE;
}

if(H5F_ACC_RDONLY == H5F_ACC_TRUNC || H5F_ACC_RDONLY == H5F_ACC_RDWR){
  fprintf(stderr, "H5F_ACC_RDONLY, H5F_ACC_TRUNC, H5F_ACC_RDWR are not all distinct");
  return EXIT_FAILURE;
}

if(H5close() != 0){
  fprintf(stderr, "H5close() failed");
  return EXIT_FAILURE;
}
printf("OK: HDF5 C type check");
return EXIT_SUCCESS;
}
