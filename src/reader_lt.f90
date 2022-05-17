submodule (h5fortran:hdf5_read) reader_lt

implicit none (type, external)

contains

module procedure h5exist

type(hdf5_file) :: h

call h%open(filename, action='r')
h5exist = h%exist(dname)
call h%close()

end procedure h5exist


module procedure lt0read
include "reader_lt_template.inc"
end procedure lt0read

module procedure lt1read
include "reader_lt_template.inc"
end procedure lt1read

module procedure lt2read
include "reader_lt_template.inc"
end procedure lt2read

module procedure lt3read
include "reader_lt_template.inc"
end procedure lt3read

module procedure lt4read
include "reader_lt_template.inc"
end procedure lt4read

module procedure lt5read
include "reader_lt_template.inc"
end procedure lt5read

module procedure lt6read
include "reader_lt_template.inc"
end procedure lt6read

module procedure lt7read
include "reader_lt_template.inc"
end procedure lt7read

end submodule reader_lt
