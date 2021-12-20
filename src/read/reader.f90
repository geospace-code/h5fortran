submodule (h5fortran:read) reader
!! This submodule is for reading 0-D..7-D data

use, intrinsic :: iso_c_binding, only : c_null_char
use hdf5, only : h5dread_f
use h5lt, only : h5ltread_dataset_string_f

implicit none (type, external)

contains


module procedure h5read_scalar

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: ds_id, native_dtype
integer :: ier

logical :: vector_scalar

real(real32) :: buf_r32(1)
real(real64) :: buf_r64(1)
integer(int32) :: buf_i32(1)
integer(int64) :: buf_i64(1)

call hdf_rank_check(self, dname, rank(value), vector_scalar)
if(vector_scalar) then
  select type(value)
  type is (real(real32))
    call h5read_1d(self, dname, buf_r32)
    value = buf_r32(1)
  type is (real(real64))
    call h5read_1d(self, dname, buf_r64)
    value = buf_r64(1)
  type is (integer(int32))
    call h5read_1d(self, dname, buf_i32)
    value = buf_i32(1)
  type is (integer(int64))
    call h5read_1d(self, dname, buf_i64)
    value = buf_i64(1)
  class default
    error stop "h5fortran:read:vector_scalar: unknown memory variable type" // dname
  end select
  return
endif

call h5dopen_f(self%lid, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(native_dtype == H5T_NATIVE_DOUBLE .or. native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier)
  class default
    error stop 'h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER .or. native_dtype == H5T_STD_I64LE) then
  select type(value)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier)
  type is (integer(int64))
    call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier)
  class default
    error stop 'h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
elseif(native_dtype == H5T_NATIVE_CHARACTER) then
  select type(value)
  type is (character(*))
    block
    character(len(value)) :: buf_char
    integer :: i
    call h5ltread_dataset_string_f(self%lid, dname, buf_char, ier)
    i = index(buf_char, c_null_char) - 1
    if (i == -1) i = len_trim(buf_char)
    value = buf_char(:i)
    end block
  class default
    error stop "h5fortran:read: character disk dataset " // dname // " needs character memory variable"
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call h5dclose_f(ds_id, ier)
if (ier/=0) error stop 'h5fortran:reader: error closing dataset ' // dname // ' in ' // self%filename

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) &
  error stop 'h5fortran:read: error closing ' // dname // ' in ' // self%filename

end procedure h5read_scalar



module procedure h5read_1d

real(real32), allocatable :: buf_r32(:)
real(real64), allocatable :: buf_r64(:)
integer(int32), allocatable :: buf_i32(:)
integer(int64), allocatable :: buf_i64(:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  type is (integer(int64))
    allocate(buf_r64(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  type is (integer(int64))
    allocate(buf_r32(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  type is (integer(int64))
    allocate(buf_i32(size(value, 1)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = int(buf_i32, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_STD_I64LE) then
  select type(value)
  type is (real(real64))
    allocate(buf_i64(size(value, 1)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = real(buf_i64, real64)
  type is (real(real32))
    allocate(buf_i64(size(value, 1)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = real(buf_i64, real32)
  type is (integer(int32))
    allocate(buf_i64(size(value, 1)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = int(buf_i64, int32)
  type is (integer(int64))
    call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure h5read_1d


module procedure hdf_read_2d

real(real32), allocatable :: buf_r32(:,:)
real(real64), allocatable :: buf_r64(:,:)
integer(int32), allocatable :: buf_i32(:,:)
integer(int64), allocatable :: buf_i64(:,:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  type is (integer(int64))
    allocate(buf_r64(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  type is (integer(int64))
    allocate(buf_r32(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  type is (integer(int64))
    allocate(buf_i32(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = int(buf_i32, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_STD_I64LE) then
  select type(value)
  type is (real(real64))
    allocate(buf_i64(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = real(buf_i64, real64)
  type is (real(real32))
    allocate(buf_i64(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = real(buf_i64, real32)
  type is (integer(int32))
    allocate(buf_i64(size(value, 1), size(value, 2)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = int(buf_i64, int32)
  type is (integer(int64))
    call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_2d


module procedure hdf_read_3d

real(real32), allocatable :: buf_r32(:,:,:)
real(real64), allocatable :: buf_r64(:,:,:)
integer(int32), allocatable :: buf_i32(:,:,:)
integer(int64), allocatable :: buf_i64(:,:,:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  type is (integer(int64))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  type is (integer(int64))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  type is (integer(int64))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = int(buf_i32, int64)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_STD_I64LE) then
  select type(value)
  type is (real(real64))
    allocate(buf_i64(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = real(buf_i64, real64)
  type is (real(real32))
    allocate(buf_i64(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = real(buf_i64, real32)
  type is (integer(int32))
    allocate(buf_i64(size(value, 1), size(value, 2), size(value, 3)))
    call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier, mem_space_id, space_id)
    value = int(buf_i64, int32)
  type is (integer(int64))
    call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_3d


module procedure hdf_read_4d

real(real32), allocatable :: buf_r32(:,:,:,:)
real(real64), allocatable :: buf_r64(:,:,:,:)
integer(int32), allocatable :: buf_i32(:,:,:,:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_4d


module procedure hdf_read_5d

real(real32), allocatable :: buf_r32(:,:,:,:,:)
real(real64), allocatable :: buf_r64(:,:,:,:,:)
integer(int32), allocatable :: buf_i32(:,:,:,:,:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_5d


module procedure hdf_read_6d

real(real32), allocatable :: buf_r32(:,:,:,:,:,:)
real(real64), allocatable :: buf_r64(:,:,:,:,:,:)
integer(int32), allocatable :: buf_i32(:,:,:,:,:,:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5), size(value, 6)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5), size(value, 6)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5), size(value, 6)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5), size(value, 6)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5), size(value, 6)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5), size(value, 6)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_6d


module procedure hdf_read_7d

real(real32), allocatable :: buf_r32(:,:,:,:,:,:,:)
real(real64), allocatable :: buf_r64(:,:,:,:,:,:,:)
integer(int32), allocatable :: buf_i32(:,:,:,:,:,:,:)

integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

ds_id = 0 !< sentinel
space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  if (present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, ds_id, space_id, mem_space_id, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, ds_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
!! select case doesn't allow H5T_*
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5),size(value, 6),size(value, 7)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = real(buf_r64, real32)
  type is (integer(int32))
    allocate(buf_r64(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5),size(value, 6),size(value, 7)))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier, mem_space_id, space_id)
    value = int(buf_r64, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5),size(value, 6),size(value, 7)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = real(buf_r32, real64)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  type is (integer(int32))
    allocate(buf_r32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5),size(value, 6),size(value, 7)))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier, mem_space_id, space_id)
    value = int(buf_r32, int32)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  select type(value)
  type is (real(real64))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5),size(value, 6),size(value, 7)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real64)
  type is (real(real32))
    allocate(buf_i32(size(value, 1), size(value, 2), size(value, 3), size(value, 4), size(value, 5),size(value, 6),size(value, 7)))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier, mem_space_id, space_id)
    value = real(buf_i32, real32)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_7d


end submodule reader
