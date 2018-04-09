!> hdf5_interface
module hdf5_interface

  use H5LT

  implicit none

  public :: hdf5_file, toLower

  private

  type :: hdf5_file

    character(256)  :: filename  ! FIXME: character, allocatable gave only single (first) character with gfortran 7.2
    integer(HID_T) :: lid   !< location identifier
    integer(HID_T) :: gid    !< group identifier
    integer(HID_T) :: glid   !< group location identifier
    integer :: comp_lvl = 0 !< compression level (1-9)  0: disable compression
    integer(HSIZE_T) :: chunk_size(3) = [64,64,1]  !< chunk size per dimension
    logical :: verbose=.false.


  contains
    !> initialize HDF5 file
    procedure :: initialize => hdf_initialize, finalize => hdf_finalize

    !> open and close hdf5 group
    procedure :: open => hdf_open_group, close => hdf_close_group

    !> add group or dataset integer/real 0-3d
    generic   :: add => hdf_add_group, hdf_add_int, hdf_add_int1d, hdf_add_int2d, hdf_add_int3d, hdf_add_real, hdf_add_real1d,&
                        hdf_add_real2d, hdf_add_real3d, hdf_add_string

    !> get dataset integer/real 0-3d
    generic   :: get => hdf_get_int, hdf_get_int1d, hdf_get_int2d, hdf_get_int3d,&
                        hdf_get_real, hdf_get_real1d, hdf_get_real2d, hdf_get_real3d,&
                        hdf_get_string

    
    !> private methods
    procedure,private :: hdf_add_group, hdf_add_int, hdf_get_int,hdf_add_int1d, hdf_get_int1d, hdf_add_int2d, hdf_get_int2d, &
      hdf_add_int3d, hdf_get_int3d, hdf_add_real, hdf_get_real, hdf_add_real1d, hdf_get_real1d, hdf_add_real2d, hdf_get_real2d, &
      hdf_add_real3d, hdf_get_real3d, hdf_add_string, hdf_get_string
      
  end type hdf5_file

contains
!=============================================================================
subroutine hdf_initialize(self,filename,status,action,comp_lvl,chunk_size)
  !< Opens hdf5 file

  class(hdf5_file), intent(inout)    :: self
  character(*), intent(in)           :: filename
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: action
  integer, intent(in), optional      :: comp_lvl
  integer, intent(in), optional      :: chunk_size(3)

  character(:), allocatable :: lstatus, laction
  integer :: ierr
  
  self%filename = filename
  
  if (present(comp_lvl)) self%comp_lvl = comp_lvl
  if (present(chunk_size)) self%chunk_size = chunk_size

  !> Initialize FORTRAN interface.
  call h5open_f(ierr)
  if (ierr /= 0) error stop 'Error: HDF5 library initialize Failed!'

  lstatus = 'old'  ! not merge() due to unequal character length
  if(present(status)) lstatus = toLower(status)

  laction = 'rw'  ! not merge() due to unequal character length
  if(present(action)) laction = toLower(action)


  select case(lstatus)
    case ('old')
      select case(laction)
        case('read','r')  !> Open an existing file.
          call h5fopen_f(filename,H5F_ACC_RDONLY_F,self%lid,ierr)
        case('write','readwrite','w','rw')
          call h5fopen_f(filename,H5F_ACC_RDWR_F,self%lid,ierr)
        case default
          error stop 'Error: Unsupported action ->'// laction
        endselect
    case('new','replace')
      call h5fcreate_f(filename,H5F_ACC_TRUNC_F,self%lid,ierr)
    case default
      error stop 'Error: Unsupported status ->'// lstatus
  endselect
  
  if (ierr /= 0) error stop 'Error: HDF5 open/create failed: '//filename
  
end subroutine hdf_initialize
!=============================================================================
subroutine hdf_finalize(self)
  class(hdf5_file), intent(in) :: self

  integer :: ierr

  !> close hdf5 file
  call h5fclose_f(self%lid, ierr)

  !>  Close FORTRAN interface.
  call h5close_f(ierr)
  
  if (ierr /= 0) error stop 'Error: HDF5 finalization: '//self%filename

end subroutine hdf_finalize
!=============================================================================
subroutine hdf_add_group(self, gname)

  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: gname    !< relative path to group

  integer(HID_T)  :: gid

  integer :: ierr, sp, ep, sl
  logical :: gexist

  sl = len(gname)
  sp = 1
  ep = 0

  do
     ep = index(gname(sp+1:sl), "/")

     ! no subgroup found
     if (ep == 0) exit

     ! check subgroup exists
     sp = sp + ep
     call h5lexists_f(self%lid, gname(1:sp-1), gexist, ierr)
     if (ierr /= 0) error stop 'problem finding group '//gname

     if(.not.gexist) then
       call h5gcreate_f(self%lid, gname(1:sp-1), gid, ierr)
       if (ierr /= 0) error stop 'problem creating group '//gname
       
       call h5gclose_f(gid, ierr)
       if (ierr /= 0) error stop 'problem closing group '//gname
     endif
  end do

end subroutine hdf_add_group
!=============================================================================
subroutine hdf_open_group(self, gname)
  class(hdf5_file), intent(inout) :: self
  character(*), intent(in)        :: gname

  integer :: ierr

  call h5gopen_f(self%lid, gname, self%gid, ierr)
  if (ierr /= 0) error stop 'problem opening group '//gname
  
  self%glid = self%lid
  self%lid  = self%gid

end subroutine hdf_open_group
!=============================================================================
subroutine hdf_close_group(self)
  class(hdf5_file), intent(inout) :: self

  integer :: ierr

  call h5gclose_f(self%gid, ierr)
  if (ierr /= 0) error stop 'problem closing group '//self%filename
  
  self%lid = self%glid

end subroutine hdf_close_group
!=============================================================================
subroutine hdf_set_deflate(self, dims, pid)
  class(hdf5_file), intent(in) :: self
  integer(HSIZE_T), intent(in) :: dims(:)
  integer(HID_T), intent(out) :: pid

  integer :: ierr, ndims, i
  integer(HSIZE_T), allocatable :: chunk_size(:)
  
  ndims = size(dims)
  allocate(chunk_size(ndims))
  
  do concurrent (i=1:ndims)
    chunk_size(i) = min(self%chunk_size(i), dims(i))
  enddo
  
  if (self%verbose) print *,'dims: ',dims,'chunk size: ',chunk_size
 
 
  call h5pcreate_f(H5P_DATASET_CREATE_F, pid, ierr)
  if (ierr /= 0) error stop 'error creating property '//self%filename
  
  call h5pset_chunk_f(pid, ndims, chunk_size, ierr)
  if (ierr /= 0) error stop 'error setting chunk '//self%filename
  
  call h5pset_shuffle_f(pid, ierr)
  if (ierr /= 0) error stop 'error enabling Shuffle '//self%filename
   
  call h5pset_deflate_f(pid, self%comp_lvl, ierr)
  if (ierr /= 0) error stop 'error enabling Deflate compression '//self%filename

end subroutine hdf_set_deflate  
!===================================
subroutine hdf_add_int(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  integer, intent(in)      :: value
  character(*), intent(in), optional :: attr, attrval

  integer(HID_T) :: sid,did
  integer         :: ierr

  call self%add(dname)
  
  ! HDF5 >= 1.10
  !call h5ltmake_dataset_f(self%lid, dname, &
  !  rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_INTEGER_KIND), value, ierr)
  !if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
      
  !  HDF5 1.8 compatbility below:
  !> create dataspace
  call h5screate_f(H5S_SCALAR_F, sid, ierr)
  if (ierr /= 0) error stop 'error create dataspace '//dname//' write '//self%filename

  !> create dataset
  call h5dcreate_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), sid, did, ierr)
  if (ierr /= 0) error stop 'error create dataet '//dname//' write '//self%filename

  !> write dataset
  call h5dwrite_f(did, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, int(shape(value),HSIZE_T), ierr)
  if (ierr /= 0) error stop 'error write dataspace '//dname//' write '//self%filename

  !> close space and dataset
  call h5dclose_f(did, ierr)
  call h5sclose_f(sid, ierr)
  if (ierr /= 0) error stop 'error close dataspace '//dname//' write '//self%filename
  
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename
  
end subroutine hdf_add_int
!=============================================================================
subroutine hdf_add_int1d(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  integer, intent(in)      :: value(:)
  character(*), intent(in), optional :: attr, attrval

  integer         :: ierr

  call self%add(dname)

  call h5ltmake_dataset_f(self%lid, dname, &
    rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_INTEGER_KIND), value, ierr)
  if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename

end subroutine hdf_add_int1d
!=============================================================================
subroutine hdf_add_int2d(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  integer, intent(in)      :: value(:,:)
  character(*), intent(in), optional :: attr, attrval

  integer         :: ierr
  integer(HID_T)  :: pid, sid, did, dtype
  integer(HSIZE_T) :: dims(rank(value))


  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

  call self%add(dname)

  
  if (self%comp_lvl < 1) then
      call h5ltmake_dataset_f(self%lid, dname, rank(value), dims, dtype, value, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  else
    call hdf_set_deflate(self, dims, pid)

    call h5screate_simple_f(rank(value), dims, sid, ierr)
    if (ierr /= 0) error stop 'error on dataspace '//dname//' '//self%filename
    
    call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr, pid)
    if (ierr /= 0) error stop 'error on dataset '//dname//' '//self%filename
    
    call h5dwrite_f(did, dtype, value, dims, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
    
    call h5sclose_f(sid, ierr)
    call h5pclose_f(pid, ierr)
    call h5dclose_f(did, ierr)
    if (ierr /= 0) error stop 'error on closing dataset '//dname//' write '//self%filename
  endif
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename

end subroutine hdf_add_int2d
!=============================================================================
subroutine hdf_add_int3d(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  integer, intent(in)      :: value(:,:,:)
  character(*), intent(in), optional :: attr, attrval

  integer         :: ierr
  integer(HID_T)  :: pid, sid, did, dtype
  integer(HSIZE_T) :: dims(rank(value))


  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

  call self%add(dname)

  
  if (self%comp_lvl < 1) then
      call h5ltmake_dataset_f(self%lid, dname, rank(value), dims, dtype, value, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  else
    call hdf_set_deflate(self, dims, pid)

    call h5screate_simple_f(rank(value), dims, sid, ierr)
    if (ierr /= 0) error stop 'error on dataspace '//dname//' '//self%filename
    
    call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr, pid)
    if (ierr /= 0) error stop 'error on dataset '//dname//' '//self%filename
    
    call h5dwrite_f(did, dtype, value, dims, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
    
    call h5sclose_f(sid, ierr)
    call h5pclose_f(pid, ierr)
    call h5dclose_f(did, ierr)
    if (ierr /= 0) error stop 'error on closing dataset '//dname//' write '//self%filename
  endif
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename

end subroutine hdf_add_int3d
!=============================================================================
subroutine hdf_add_real(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  real, intent(in)      :: value
  character(*), intent(in), optional :: attr, attrval

  integer(HID_T) :: sid,did
  integer         :: ierr

  call self%add(dname)

  ! HDF5 >= 1.10 
  !call h5ltmake_dataset_f(self%lid, dname, &
 !   rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
  !if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  
  ! HDF5 1.8 compatbility below:
  !> create dataspace
  call h5screate_f(H5S_SCALAR_F, sid, ierr)
  if (ierr /= 0) error stop 'error create dataspace '//dname//' write '//self%filename

  !> create dataset
  call h5dcreate_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), sid, did, ierr)
  if (ierr /= 0) error stop 'error create dataset '//dname//' write '//self%filename

  !> write dataset
  call h5dwrite_f(did, h5kind_to_type(kind(value),H5_REAL_KIND), value, int(shape(value),HSIZE_T), ierr)
  if (ierr /= 0) error stop 'error write dataset '//dname//' write '//self%filename

  !> close space and dataset
  call h5dclose_f(did, ierr)
  call h5sclose_f(sid, ierr)
  if (ierr /= 0) error stop 'error close dataspace '//dname//' write '//self%filename
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename


end subroutine hdf_add_real
!=============================================================================
subroutine hdf_add_real1d(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  real, intent(in)      :: value(:)
  character(*), intent(in), optional :: attr, attrval

  integer         :: ierr

  call self%add(dname)

  call h5ltmake_dataset_f(self%lid, dname, &
    rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
  if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename

end subroutine hdf_add_real1d
!=============================================================================
subroutine hdf_add_real2d(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  real, intent(in)      :: value(:,:)
  character(*), intent(in), optional :: attr, attrval

  integer         :: ierr
  integer(HID_T)  :: pid, sid, did, dtype
  integer(HSIZE_T) :: dims(rank(value))


  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call self%add(dname)

  if (self%comp_lvl < 1) then
    call h5ltmake_dataset_f(self%lid, dname, rank(value), dims, dtype, value, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  else
    call hdf_set_deflate(self, dims, pid)

    call h5screate_simple_f(rank(value), dims, sid, ierr)
    if (ierr /= 0) error stop 'error on dataspace '//dname//' '//self%filename
    
    call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr, pid)
    if (ierr /= 0) error stop 'error on dataset '//dname//' '//self%filename
    
    call h5dwrite_f(did, dtype, value, dims, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
    
    call h5sclose_f(sid, ierr)
    call h5pclose_f(pid, ierr)
    call h5dclose_f(did, ierr)
    if (ierr /= 0) error stop 'error on closing dataset '//dname//' write '//self%filename
  endif
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename
 

end subroutine hdf_add_real2d


subroutine hdf_add_real3d(self,dname,value,attr,attrval)
  class(hdf5_file), intent(in) :: self
  character(*), intent(in) :: dname
  real, intent(in)      :: value(:,:,:)
  character(*), intent(in), optional :: attr, attrval

  integer         :: ierr
  integer(HID_T)  :: pid, sid, did, dtype
  integer(HSIZE_T) :: dims(rank(value))


  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call self%add(dname)

  if (self%comp_lvl < 1) then
    call h5ltmake_dataset_f(self%lid, dname, &
      rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
  else
    call hdf_set_deflate(self, dims, pid)

    call h5screate_simple_f(rank(value), dims, sid, ierr)
    if (ierr /= 0) error stop 'error on dataspace '//dname//' '//self%filename
    
    call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr, pid)
    if (ierr /= 0) error stop 'error on dataset '//dname//' '//self%filename
    
    call h5dwrite_f(did, dtype, value, dims, ierr)
    if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename
    
    call h5sclose_f(sid, ierr)
    call h5pclose_f(pid, ierr)
    call h5dclose_f(did, ierr)
    if (ierr /= 0) error stop 'error on closing dataset '//dname//' write '//self%filename
  endif
  
  if (present(attr)) call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
  if (ierr /= 0) error stop 'problem writing attribute '//attr//' to '//dname//' file '//self%filename
  
end subroutine hdf_add_real3d


subroutine hdf_add_string(self,dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname, value
  integer :: ierr

  call h5ltmake_dataset_string_f(self%lid, dname, value, ierr)
  if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename


end subroutine hdf_add_string

!====== READ =====================================

subroutine hdf_get_string(self,dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  character(*), intent(out)        :: value
  
  integer :: ierr

  call h5ltread_dataset_string_f(self%lid, dname, value, ierr)
  if (ierr /= 0) error stop 'error on dataset '//dname//' read '//self%filename


end subroutine hdf_get_string

subroutine hdf_get_int(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  integer, intent(out)             :: value

  integer(HID_T)  :: did
  integer :: ierr

  ! open dataset
  call h5dopen_f(self%lid, dname, did, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  ! read dataset
  call h5dread_f(did, h5kind_to_type(kind(value),H5_INTEGER_KIND), value,int(shape(value),HSIZE_T), ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

  ! close dataset
  call h5dclose_f(did, ierr)
  if (ierr /= 0) error stop 'error close dataset '//dname//' read '//self%filename

end subroutine hdf_get_int
!=============================================================================
subroutine hdf_get_int1d(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  integer, intent(out),allocatable :: value(:)

  integer(SIZE_T) :: dims(1), dsize
  integer :: ierr, dtype

  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  allocate(value(dims(1)))

  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end subroutine hdf_get_int1d
!=============================================================================
subroutine hdf_get_int2d(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  integer, intent(out),allocatable :: value(:,:)

  integer(SIZE_T) :: dims(2),dsize
  integer :: ierr, dtype

  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  allocate(value(dims(1),dims(2)))

  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end subroutine hdf_get_int2d
!=============================================================================
subroutine hdf_get_int3d(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  integer, intent(out),allocatable :: value(:,:,:)

  integer(SIZE_T) :: dims(3),dsize
  integer :: ierr, dtype

  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  allocate(value(dims(1),dims(2),dims(3)))

  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end subroutine hdf_get_int3d
!=============================================================================
subroutine hdf_get_real(self, dname, value)

  class(hdf5_file), intent(in)  :: self
  character(*), intent(in)      :: dname
  real, intent(out)             :: value

  integer(HID_T)  :: set_id
  integer :: ierr

  ! open dataset
  call h5dopen_f(self%lid, dname, set_id, ierr)

  ! read dataset
  call h5dread_f(set_id, h5kind_to_type(kind(value),H5_REAL_KIND), value,int(shape(value),HSIZE_T), ierr)

  ! close dataset
  call h5dclose_f(set_id, ierr)

end subroutine hdf_get_real
!=============================================================================
subroutine hdf_get_real1d(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  real, intent(out),allocatable :: value(:)

  integer(SIZE_T) :: dims(1),dsize
  integer :: ierr, dtype

  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  allocate(value(dims(1)))

  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename
  
end subroutine hdf_get_real1d
!=============================================================================
subroutine hdf_get_real2d(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  real, intent(out),allocatable :: value(:,:)

  integer(SIZE_T) :: dims(2),dsize
  integer :: ierr, dtype

  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  allocate(value(dims(1),dims(2)))

  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end subroutine hdf_get_real2d
!=============================================================================
subroutine hdf_get_real3d(self, dname, value)

  class(hdf5_file), intent(in)     :: self
  character(*), intent(in)         :: dname
  real, intent(out),allocatable :: value(:,:,:)

  integer(SIZE_T) :: dims(3),dsize
  integer :: ierr, dtype

  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
  if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

  allocate(value(dims(1),dims(2),dims(3)))

  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
  if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end subroutine hdf_get_real3d

!----- Helper functions

elemental function toLower(str)
! can be trivially extended to non-ASCII
  character(*), intent(in) :: str
  character(len(str)) :: toLower
  character(*), parameter :: lower="abcdefghijklmnopqrstuvwxyz", &
                             upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  integer :: i,j

  toLower = str

  do concurrent (i = 1:len(str))
    j = index(upper,str(i:i))
    if (j > 0) toLower(i:i) = lower(j:j)
  end do

end function toLower

end module hdf5_interface
