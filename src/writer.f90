submodule (h5fortran:write) writer
!! This submodule is for writing 0-D..7-D data

use hdf5, only: H5_REAL_KIND, H5_INTEGER_KIND, H5S_SCALAR_F, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_write_scalar

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T), allocatable :: dims(:)
integer :: ier

allocate(dims(0))

select type (value)
type is (character(*))
  call h5ltmake_dataset_string_f(self%lid, dname, value, ier)
  if (present(ierr)) ierr = ier
  if (ier /= 0) then
    write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
    if (present(ierr)) return
    error stop
  endif
  return
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_scalar


module procedure hdf_write_1d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_1d


module procedure hdf_write_2d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_2d


module procedure hdf_write_3d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_3d


module procedure hdf_write_4d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_4d


module procedure hdf_write_5d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_5d


module procedure hdf_write_6d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_6d


module procedure hdf_write_7d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, dtype, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_7d


module procedure lt0write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (character(*))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt0write


module procedure lt1write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt1write


module procedure lt2write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt2write


module procedure lt3write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt3write


module procedure lt4write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt4write


module procedure lt5write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt5write


module procedure lt6write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt6write


module procedure lt7write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt7write

end submodule writer
