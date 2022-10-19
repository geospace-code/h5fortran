submodule (h5fortran:attr_smod) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aread_f, H5Aget_type_f, &
H5Tget_class_f, H5Tget_native_type_f, H5Tget_size_f, H5Tget_strpad_f, &
H5T_DIR_ASCEND_F, &
H5S_ALL_F

implicit none

contains

module procedure readattr_scalar
include 'attr_read.inc'
end procedure

module procedure readattr_1d
include 'attr_read.inc'
end procedure

module procedure readattr_2d
include 'attr_read.inc'
end procedure

module procedure readattr_3d
include 'attr_read.inc'
end procedure

module procedure readattr_4d
include 'attr_read.inc'
end procedure

module procedure readattr_5d
include 'attr_read.inc'
end procedure

module procedure readattr_6d
include 'attr_read.inc'
end procedure

module procedure readattr_7d
include 'attr_read.inc'
end procedure

module procedure lt0readattr

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr(obj_name, attr, A)
call h%close()

end procedure lt0readattr


module procedure lt1readattr

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr(obj_name, attr, A)
call h%close()

end procedure lt1readattr


end submodule attr_read
