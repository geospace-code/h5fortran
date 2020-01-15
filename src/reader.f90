!! This submodule is for reading 0-D..7-D data
submodule (h5fortran:read) reader

use hdf5, only: HSIZE_T, SIZE_T, H5_REAL_KIND, H5_INTEGER_KIND, H5KIND_TO_TYPE
use H5LT, only: h5ltpath_valid_f

implicit none
contains


module procedure hdf_read_scalar

integer(HSIZE_T) :: dims(rank(value))
logical :: exists

select type (value)
type is (character(*))
  call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
  if (.not.exists) then
    write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
    ierr = -1
    return
  endif
  block
    !! Need to use "buf" variable, even intent(inout) doesn't help without
    !! separate "buf" variable
    character(len(value)) :: buf
    call h5ltread_dataset_string_f(self%lid, dname, buf, ierr)
    if (ierr /= 0)  then
      write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
      return
    endif
    value = buf
  end block
  return
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_scalar


module procedure hdf_read_1d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_1d


module procedure hdf_read_2d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_2d


module procedure hdf_read_3d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_3d


module procedure hdf_read_4d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_4d


module procedure hdf_read_5d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_5d


module procedure hdf_read_6d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_6d


module procedure hdf_read_7d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (real(real32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
type is (integer(int32))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
type is (integer(int64))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_7d


module procedure lt0read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (character(*))
  call h%read(dname, value, ier)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt0read


module procedure lt1read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt1read


module procedure lt2read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt2read


module procedure lt3read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt3read


module procedure lt4read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt4read


module procedure lt5read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt5read


module procedure lt6read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt6read


module procedure lt7read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')
if (check(ier, 'ERROR: open ' // dname // ' read_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int64))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' read_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt7read

end submodule reader
