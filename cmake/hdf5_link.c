  #include "hdf5.h"
  int main(void){
  if(H5open() != 0) return 1;
  if(H5F_ACC_RDONLY == H5F_ACC_TRUNC || H5F_ACC_RDONLY == H5F_ACC_RDWR) return 1;
  if(H5close() != 0) return 1;
  return 0;
  }
