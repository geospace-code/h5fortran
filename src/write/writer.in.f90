submodule (h5fortran:write) writer
!! This submodule is for writing 0-D..7-D data
use hdf5, only: h5dwrite_f, H5T_STD_I64LE

implicit none (type, external)

contains


module procedure hdf_write_scalar

integer(HID_T)  :: sid, did
integer(HSIZE_T), allocatable :: dims(:)
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open'

allocate(dims(0))

select type (value)
type is (real(real32))
  call hdf_create(self, dname, H5T_NATIVE_REAL, dims, sid, did, compact=compact)
  call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (real(real64))
  call hdf_create(self, dname, H5T_NATIVE_DOUBLE, dims, sid, did, compact=compact)
  call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (integer(int32))
  call hdf_create(self, dname, H5T_NATIVE_INTEGER, dims, sid, did, compact=compact)
  call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
type is (integer(int64))
  call hdf_create(self, dname, H5T_STD_I64LE, dims, sid, did, compact=compact)
  call h5dwrite_f(did, H5T_STD_I64LE, value, dims, ier)
type is (character(*))
  call h5ltmake_dataset_string_f(self%lid, dname, value, ier)
class default
  error stop "h5fortran:write: unknown type"
end select

if (ier /= 0) error stop 'h5fortran:write: could not write ' // dname // ' to ' // self%filename

select type (value)
type is (character(*))
class default
  call hdf_wrapup(did, sid)
end select

end procedure hdf_write_scalar


module procedure hdf_write_1d_r32
@writer_template_r32@
end procedure hdf_write_1d_r32

module procedure hdf_write_1d_r64
@writer_template_r64@
end procedure hdf_write_1d_r64

module procedure hdf_write_1d_i32
@writer_template_i32@
end procedure hdf_write_1d_i32

module procedure hdf_write_1d_i64
@writer_template_i64@
end procedure hdf_write_1d_i64


module procedure hdf_write_2d_r32
@writer_template_r32@
end procedure hdf_write_2d_r32

module procedure hdf_write_2d_r64
@writer_template_r64@
end procedure hdf_write_2d_r64

module procedure hdf_write_2d_i32
@writer_template_i32@
end procedure hdf_write_2d_i32

module procedure hdf_write_2d_i64
@writer_template_i64@
end procedure hdf_write_2d_i64


module procedure hdf_write_3d_r32
@writer_template_r32@
end procedure hdf_write_3d_r32

module procedure hdf_write_3d_r64
@writer_template_r64@
end procedure hdf_write_3d_r64

module procedure hdf_write_3d_i32
@writer_template_i32@
end procedure hdf_write_3d_i32

module procedure hdf_write_3d_i64
@writer_template_i64@
end procedure hdf_write_3d_i64


module procedure hdf_write_4d_r32
@writer_template_r32@
end procedure hdf_write_4d_r32

module procedure hdf_write_4d_r64
@writer_template_r64@
end procedure hdf_write_4d_r64

module procedure hdf_write_4d_i32
@writer_template_i32@
end procedure hdf_write_4d_i32

module procedure hdf_write_4d_i64
@writer_template_i64@
end procedure hdf_write_4d_i64


module procedure hdf_write_5d_r32
@writer_template_r32@
end procedure hdf_write_5d_r32

module procedure hdf_write_5d_r64
@writer_template_r64@
end procedure hdf_write_5d_r64

module procedure hdf_write_5d_i32
@writer_template_i32@
end procedure hdf_write_5d_i32

module procedure hdf_write_5d_i64
@writer_template_i64@
end procedure hdf_write_5d_i64


module procedure hdf_write_6d_r32
@writer_template_r32@
end procedure hdf_write_6d_r32

module procedure hdf_write_6d_r64
@writer_template_r64@
end procedure hdf_write_6d_r64

module procedure hdf_write_6d_i32
@writer_template_i32@
end procedure hdf_write_6d_i32

module procedure hdf_write_6d_i64
@writer_template_i64@
end procedure hdf_write_6d_i64


module procedure hdf_write_7d_r32
@writer_template_r32@
end procedure hdf_write_7d_r32

module procedure hdf_write_7d_r64
@writer_template_r64@
end procedure hdf_write_7d_r64

module procedure hdf_write_7d_i32
@writer_template_i32@
end procedure hdf_write_7d_i32

module procedure hdf_write_7d_i64
@writer_template_i64@
end procedure hdf_write_7d_i64
end submodule writer
