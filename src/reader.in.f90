submodule (h5fortran:read) reader
!! This submodule is for reading 0-D..7-D data

use hdf5, only : h5dread_f
use h5lt, only : h5ltread_dataset_string_f

implicit none (type, external)

contains


module procedure hdf_read_scalar

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:reader: file handle is not open'

ier = 0
sid = 0

if (.not.self%exist(dname)) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ier = -1
endif

if(ier == 0) then
call h5dopen_f(self%lid, dname, did, ier)

if (ier == 0) then
select type (value)
type is (character(*))
  call hdf_wrapup(did, sid, ier)  !< FIXME: till character is treated same as other types
  block
    character(len(value)) :: buf
    call h5ltread_dataset_string_f(self%lid, dname, buf, ier)
    value = buf
  end block
  return
type is (real(real64))
  call h5dread_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call h5dread_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call h5dread_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  ier = 6
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)
endif

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop

end procedure hdf_read_scalar


module procedure hdf_read_1d
@reader_template@
end procedure hdf_read_1d


module procedure hdf_read_2d
@reader_template@
end procedure hdf_read_2d


module procedure hdf_read_3d
@reader_template@
end procedure hdf_read_3d


module procedure hdf_read_4d
@reader_template@
end procedure hdf_read_4d


module procedure hdf_read_5d
@reader_template@
end procedure hdf_read_5d


module procedure hdf_read_6d
@reader_template@
end procedure hdf_read_6d


module procedure hdf_read_7d
@reader_template@
end procedure hdf_read_7d


end submodule reader
