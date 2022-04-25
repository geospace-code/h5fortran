submodule (h5fortran:write) writer

use hdf5, only: h5dwrite_f

implicit none (type, external)

contains

module procedure h5write_1d
@writer_template@
end procedure h5write_1d

module procedure h5write_2d
@writer_template@
end procedure h5write_2d

module procedure h5write_3d
@writer_template@
end procedure h5write_3d

module procedure h5write_4d
@writer_template@
end procedure h5write_4d

module procedure h5write_5d
@writer_template@
end procedure h5write_5d

module procedure h5write_6d
@writer_template@
end procedure h5write_6d

module procedure h5write_7d
@writer_template@
end procedure h5write_7d

end submodule writer
