submodule (h5fortran:read) reader
!! This submodule is for reading 0-D..7-D data

use hdf5, only : h5dread_f
use h5lt, only : h5ltread_dataset_string_f

implicit none (type, external)

contains


module procedure hdf_read_scalar_char

integer :: ier
character(len(value)) :: buf

if(.not.self%is_open) error stop 'h5fortran:reader: file handle is not open'

if (.not.self%exist(dname)) error stop 'h5fortran:reader: ' // dname // ' does not exist in ' // self%filename

call h5ltread_dataset_string_f(self%lid, dname, buf, ier)
value = buf

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_scalar_char


module procedure hdf_read_scalar_r32

real(real64) :: buf_r64
integer(int32) :: buf_i32
integer(int64) :: buf_i64

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: ds_id, space_id, native_dtype
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:reader: file handle is not open'

space_id = 0

if (.not.self%exist(dname)) error stop 'h5fortran:reader: ' // dname // ' does not exist in ' // self%filename

call h5dopen_f(self%lid, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
  !! select case doesn't allow H5T_*
  call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier)
  value = real(buf_r64, real32)
elseif(native_dtype == H5T_NATIVE_REAL) then
  call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier)
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier)
  value = real(buf_i32, real32)
elseif(native_dtype == H5T_STD_I64LE) then
  call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier)
  value = real(buf_i64, real32)
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_scalar_r32


module procedure hdf_read_scalar_r64

real(real32) :: buf_r32
integer(int32) :: buf_i32
integer(int64) :: buf_i64

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: ds_id, space_id, native_dtype
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:reader: file handle is not open'

space_id = 0

if (.not.self%exist(dname)) error stop 'h5fortran:reader: ' // dname // ' does not exist in ' // self%filename

call h5dopen_f(self%lid, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
  call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier)
elseif(native_dtype == H5T_NATIVE_REAL) then
  call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier)
  value = real(buf_r32, real64)
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier)
  value = real(buf_i32, real64)
elseif(native_dtype == H5T_STD_I64LE) then
  call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier)
  value = real(buf_i64, real64)
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_scalar_r64


module procedure hdf_read_scalar_i32

real(real32) :: buf_r32
real(real64) :: buf_r64
integer(int64) :: buf_i64

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: ds_id, space_id, native_dtype
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:reader: file handle is not open'

space_id = 0

if (.not.self%exist(dname)) error stop 'h5fortran:reader: ' // dname // ' does not exist in ' // self%filename

call h5dopen_f(self%lid, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
  call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier)
  value = int(buf_r64, int32)
elseif(native_dtype == H5T_NATIVE_REAL) then
  call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier)
  value = int(buf_r32, int32)
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier)
elseif(native_dtype == H5T_STD_I64LE) then
  call h5dread_f(ds_id, H5T_STD_I64LE, buf_i64, dims, ier)
  value = int(buf_i64, int32)
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_scalar_i32


module procedure hdf_read_scalar_i64

real(real32) :: buf_r32
real(real64) :: buf_r64
integer(int32) :: buf_i32

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: ds_id, space_id, native_dtype
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:reader: file handle is not open'

space_id = 0

if (.not.self%exist(dname)) error stop 'h5fortran:reader: ' // dname // ' does not exist in ' // self%filename

call h5dopen_f(self%lid, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
if(native_dtype == H5T_NATIVE_DOUBLE) then
  call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, buf_r64, dims, ier)
  value = int(buf_r64, int64)
elseif(native_dtype == H5T_NATIVE_REAL) then
  call h5dread_f(ds_id, H5T_NATIVE_REAL, buf_r32, dims, ier)
  value = int(buf_r32, int64)
elseif(native_dtype == H5T_NATIVE_INTEGER) then
  call h5dread_f(ds_id, H5T_NATIVE_INTEGER, buf_i32, dims, ier)
  value = int(buf_i32, int64)
elseif(native_dtype == H5T_STD_I64LE) then
  call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier)
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_scalar_i64


module procedure hdf_read_1d

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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_1d


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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

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
dims = shape(value)

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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
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
    error stop 'unknown variable type'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(ds_id, space_id, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_7d


end submodule reader
