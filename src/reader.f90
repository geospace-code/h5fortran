submodule (h5fortran:hdf5_read) hdf5_reader
!! This submodule is for reading 1-D..7-D data

use hdf5, only : h5dread_f, h5sclose_f

implicit none (type, external)

contains


module procedure h5read_1d
include "reader_template.inc"
end procedure h5read_1d

module procedure h5read_2d
include "reader_template.inc"
end procedure h5read_2d

module procedure h5read_3d
include "reader_template.inc"
end procedure h5read_3d

module procedure h5read_4d
include "reader_template.inc"
end procedure h5read_4d

module procedure h5read_5d
include "reader_template.inc"
end procedure h5read_5d

module procedure h5read_6d
include "reader_template.inc"
end procedure h5read_6d

module procedure h5read_7d
include "reader_template.inc"
end procedure h5read_7d

end submodule hdf5_reader
