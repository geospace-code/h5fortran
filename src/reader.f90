submodule (h5fortran:hdf5_read) hdf5_reader

use hdf5, only : H5Dread_f

implicit none

contains


module procedure h5read_1d
include "reader.inc"
end procedure

module procedure h5read_2d
include "reader.inc"
end procedure

module procedure h5read_3d
include "reader.inc"
end procedure

module procedure h5read_4d
include "reader.inc"
end procedure

module procedure h5read_5d
include "reader.inc"
end procedure

module procedure h5read_6d
include "reader.inc"
end procedure

module procedure h5read_7d
include "reader.inc"
end procedure

end submodule hdf5_reader
