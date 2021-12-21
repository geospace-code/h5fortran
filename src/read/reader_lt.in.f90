submodule (h5fortran:read) reader_lt

implicit none (type, external)

contains

module procedure h5exist

type(hdf5_file) :: h

call h%open(filename, action='r')
h5exist = h%exist(dname)
call h%close()

end procedure h5exist


module procedure lt0read
@reader_lt_template@
end procedure lt0read

module procedure lt1read
@reader_lt_template@
end procedure lt1read

module procedure lt2read
@reader_lt_template@
end procedure lt2read

module procedure lt3read
@reader_lt_template@
end procedure lt3read

module procedure lt4read
@reader_lt_template@
end procedure lt4read

module procedure lt5read
@reader_lt_template@
end procedure lt5read

module procedure lt6read
@reader_lt_template@
end procedure lt6read

module procedure lt7read
@reader_lt_template@
end procedure lt7read

end submodule reader_lt
