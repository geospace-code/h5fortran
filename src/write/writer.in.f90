submodule (h5fortran:write) writer
!! This submodule is for writing 0-D..7-D data
use hdf5, only: h5dwrite_f

implicit none (type, external)

contains


module procedure h5write_scalar

integer(HID_T)  :: filespace_id, dset_id
integer(HSIZE_T) :: dims(0)
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open'

select type (value)
type is (real(real32))
  call hdf_create(self, dname, H5T_NATIVE_REAL, dims, filespace_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_REAL, value, dims, ier)
type is (real(real64))
  call hdf_create(self, dname, H5T_NATIVE_DOUBLE, dims, filespace_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (integer(int32))
  call hdf_create(self, dname, H5T_NATIVE_INTEGER, dims, filespace_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, value, dims, ier)
type is (integer(int64))
  call hdf_create(self, dname, H5T_STD_I64LE, dims, filespace_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_STD_I64LE, value, dims, ier)
type is (character(*))
  call h5ltmake_dataset_string_f(self%lid, dname, value, ier)
class default
  error stop "h5fortran:write: unknown type"
end select

if (ier /= 0) error stop 'h5fortran:write: could not write ' // dname // ' to ' // self%filename

select type (value)
type is (character(*))
class default
  call hdf_wrapup(dset_id, filespace_id)
end select

end procedure h5write_scalar


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
