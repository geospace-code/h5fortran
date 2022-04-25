submodule (h5fortran:hdf5_read) hdf5_reader
!! This submodule is for reading 0-D..7-D data

use hdf5, only : h5dread_f

implicit none (type, external)

contains


module procedure h5read_1d
@reader_template@
end procedure h5read_1d

module procedure h5read_2d
@reader_template@
end procedure h5read_2d

module procedure h5read_3d
@reader_template@
end procedure h5read_3d

module procedure h5read_4d
@reader_template@
end procedure h5read_4d

module procedure h5read_5d
@reader_template@
end procedure h5read_5d

module procedure h5read_6d
@reader_template@
end procedure h5read_6d

module procedure h5read_7d
@reader_template@
end procedure h5read_7d

end submodule hdf5_reader
