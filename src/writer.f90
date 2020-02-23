!! This submodule is for writing 0-D..7-D data
submodule (h5fortran:write) writer

use hdf5, only: H5_REAL_KIND, H5_INTEGER_KIND, H5S_SCALAR_F, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_write_scalar

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T), allocatable :: dims(:)
allocate(dims(0))

select type (value)
type is (character(*))
  call h5ltmake_dataset_string_f(self%lid, dname, value, ierr)
  if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_scalar


module procedure hdf_write_1d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_1d


module procedure hdf_write_2d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_2d


module procedure hdf_write_3d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_3d


module procedure hdf_write_4d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_4d


module procedure hdf_write_5d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_5d


module procedure hdf_write_6d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_6d


module procedure hdf_write_7d

integer(HID_T)  :: dtype, sid, did
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)
  call hdf_setup_write(self,dname,dtype,dims,sid,did, ierr, chunk_size)
  if (ierr /= 0) return
  call h5dwrite_f(did, dtype, value, dims, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

call hdf_wrapup(did, sid, ierr)
if (check(ierr, 'ERROR: ' // dname // ' write ' // self%filename))  return

end procedure hdf_write_7d


module procedure lt0write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (character(*))
  call h%write(dname, value, ier)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)

type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt0write


module procedure lt1write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)

type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt1write


module procedure lt2write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)

type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt2write


module procedure lt3write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)

type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt3write


module procedure lt4write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)
type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt4write


module procedure lt5write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)
type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt5write


module procedure lt6write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)

type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt6write


module procedure lt7write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (check(ier, 'ERROR: open ' // dname // ' write_lt ' // filename))  return

select type (value)
type is (real(real64))
  call h%write(dname, value, ier)
type is (real(real32))
  call h%write(dname, value, ier)
type is (integer(int32))
  call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename))  return

call h%finalize(ier)
if (check(ier, 'ERROR: close ' // dname // ' write_lt ' // filename))  return

if(present(ierr)) ierr = ier

end procedure lt7write

end submodule writer
