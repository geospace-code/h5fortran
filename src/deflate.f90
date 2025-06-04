submodule (h5fortran:hdf5_read) h5f_deflate

use hdf5, only : H5Z_FILTER_DEFLATE_F

implicit none

contains

module procedure hdf_get_chunk

integer :: ier, drank
integer(HID_T) :: dapl, dset_id, space_id
integer(HSIZE_T) :: cs(size(chunk_size))

cs = -1

if (.not.self%exist(dname)) error stop 'ERROR:h5fortran:get_chunk: ' // dname // ' does not exist in ' // self%filename

if(self%is_chunked(dname)) then
  call H5Dopen_f(self%file_id, dname, dset_id, ier)
  call estop(ier, "get_chunk:H5Dopen", self%filename, dname)

  call H5Dget_space_f(dset_id, space_id, ier)
  call estop(ier, "get_chunk:H5Dget_space", self%filename, dname)
  call H5Sget_simple_extent_ndims_f(space_id, drank, ier)
  call estop(ier, "get_chunk:H5Sget_simple_extent_ndims", self%filename, dname)
  call H5Sclose_f(space_id, ier)
  call estop(ier, "get_chunk:H5Sclose", self%filename, dname)

  call h5dget_create_plist_f(dset_id, dapl, ier)
  call estop(ier, "get_chunk:H5Dget_create_plist", self%filename, dname)

  call h5dclose_f(dset_id, ier)
  call estop(ier, "get_chunk:H5Dclose", self%filename, dname)

  call h5pget_chunk_f(dapl, drank, cs, ier)
  if (ier /= drank) error stop 'ERROR:h5fortran:get_chunk:h5pget_chunk ' // dname // ' ' // self%filename
  !! yes ier == drank is success for this call

  call h5pclose_f(dapl, ier)
  call estop(ier, "get_chunk:H5Pclose", self%filename, dname)
endif

select type (chunk_size)
type is (integer(HSIZE_T))
  chunk_size = cs
type is (integer(int32))
  chunk_size = int(cs)
class default
  error stop 'ERROR:h5fortran:get_chunk: unknown type for chunk_size'
end select

end procedure hdf_get_chunk


module procedure get_deflate
!! h5pget_filter_f doesn't work collectively, will crash on h5fclose_f
!! if(mpi_id==0) with mpi_bcast does not work, same crash.
!! better to use H5Pall_filters_avail_f when mpi=.true.

integer :: i, j, ier
integer :: flags !< bit pattern
integer(HID_T) :: dcpl, dset_id
integer(SIZE_T) :: Naux
integer :: Aux(8)  !< arbitrary length
integer :: Nf, filter_id
character(32) :: filter_name

logical :: debug = .false.


get_deflate = .false.

Naux = size(Aux, kind=SIZE_T)

if(.not.self%exist(dname)) error stop "ERROR:h5fortran:get_deflate: " // dname // " does not exist: " // self%filename
call H5Dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, "get_deflate:H5Dopen", self%filename, dname)

call h5dget_create_plist_f(dset_id, dcpl, ier)
call estop(ier, "get_deflate:H5Dget_create_plist", self%filename, dname)

call H5Dclose_f(dset_id, ier)
call estop(ier, "get_deflate:H5Dclose", self%filename, dname)

call h5pget_nfilters_f(dcpl, Nf, ier)
call estop(ier, "get_deflate:H5Pget_nfilters", self%filename, dname)

filters: do i = 1, Nf
  filter_name = ""

  call h5pget_filter_f(dcpl, i, &
  flags, &
  Naux, Aux, &
  len(filter_name, SIZE_T), filter_name, &
  filter_id, ier)
  call estop(ier, "get_deflate:H5Pget_filter", self%filename, dname)
  if(filter_id < 0) write(stderr,'(a,i0)') "ERROR:h5fortran:get_deflate:h5pget_filter: index error " // dname, i

  if (debug) then
    j = index(filter_name, c_null_char)
    if(j>0) print *, "TRACE:get_filter: filter name: ", filter_name(:j-1)
  endif

  get_deflate = filter_id == H5Z_FILTER_DEFLATE_F
  if(get_deflate) exit filters

end do filters

call h5pclose_f(dcpl, ier)

end procedure get_deflate


end submodule h5f_deflate
