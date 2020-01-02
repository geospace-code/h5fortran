!! This submodule is for reading 0-D..7-D data
submodule (hdf5_interface:read) reader

use H5LT, only: HSIZE_T, SIZE_T, H5_REAL_KIND, H5_INTEGER_KIND, H5KIND_TO_TYPE, h5ltpath_valid_f
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


end submodule reader
