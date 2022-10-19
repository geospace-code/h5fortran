submodule (h5fortran:attr_smod) attr_write

use hdf5, only: H5Awrite_f

implicit none

contains

module procedure writeattr_scalar
include 'attr_write.inc'
end procedure

module procedure writeattr_1d
include 'attr_write.inc'
end procedure

module procedure writeattr_2d
include 'attr_write.inc'
end procedure

module procedure writeattr_3d
include 'attr_write.inc'
end procedure

module procedure writeattr_4d
include 'attr_write.inc'
end procedure

module procedure writeattr_5d
include 'attr_write.inc'
end procedure

module procedure writeattr_6d
include 'attr_write.inc'
end procedure

module procedure writeattr_7d
include 'attr_write.inc'
end procedure


module procedure lt0writeattr

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr(obj_name, attr, A)
call h%close()

end procedure lt0writeattr


module procedure lt1writeattr

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr(obj_name, attr, A)
call h%close()

end procedure lt1writeattr


end submodule attr_write
