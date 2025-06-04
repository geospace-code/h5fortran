submodule (h5fortran:write) writer

use hdf5, only: H5Dwrite_f

implicit none

contains

module procedure h5write_1d
include "writer.inc"
end procedure

module procedure h5write_2d
include "writer.inc"
end procedure

module procedure h5write_3d
include "writer.inc"
end procedure

module procedure h5write_4d
include "writer.inc"
end procedure

module procedure h5write_5d
include "writer.inc"
end procedure

module procedure h5write_6d
include "writer.inc"
end procedure

module procedure h5write_7d
include "writer.inc"
end procedure

end submodule writer
