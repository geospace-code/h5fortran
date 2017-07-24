!> hdf5_interface
module hdf5_interface
  use HDF5
  use H5LT
  implicit none

  public :: hdf5_file

  private

  type :: hdf5_file

    integer(HID_T) :: lid    !< location identifier
    integer(HID_T) :: gid    !< group identifier
    integer(HID_T) :: glid   !< group location identifier


  contains
    !> initialize hfd5 file
    procedure :: initialize => hdf_initialize
    procedure :: finalize   => hdf_finalize

    !> open and close hdf5 group
    procedure :: open       => hdf_open_group
    procedure :: close      => hdf_close_group

    !> add group or dataset integer/real 0-3d
    generic   :: add => hdf_add_group,&
                        hdf_add_int,&
                        hdf_add_int1d,&
                        hdf_add_int2d,&
                        hdf_add_int3d,&
                        hdf_add_real,&
                        hdf_add_real1d,&
                        hdf_add_real2d,&
                        hdf_add_real3d

    !> get dataset integer/real 0-3d
    generic   :: get => hdf_get_int,&
                        hdf_get_int1d,&
                        hdf_get_int2d,&
                        hdf_get_int3d,&
                        hdf_get_real,&
                        hdf_get_real1d,&
                        hdf_get_real2d,&
                        hdf_get_real3d


    !> private methods
    procedure,private :: hdf_add_group
    procedure,private :: hdf_add_int
    procedure,private :: hdf_get_int
    procedure,private :: hdf_add_int1d
    procedure,private :: hdf_get_int1d
    procedure,private :: hdf_add_int2d
    procedure,private :: hdf_get_int2d
    procedure,private :: hdf_add_int3d
    procedure,private :: hdf_get_int3d
    procedure,private :: hdf_add_real
    procedure,private :: hdf_get_real
    procedure,private :: hdf_add_real1d
    procedure,private :: hdf_get_real1d
    procedure,private :: hdf_add_real2d
    procedure,private :: hdf_get_real2d
    procedure,private :: hdf_add_real3d
    procedure,private :: hdf_get_real3d
  end type hdf5_file

contains
  !=============================================================================
  subroutine hdf_initialize(self,file,status,action)
    !< Opens hdf5 file

    class(hdf5_file), intent(inout)    :: self
    character(*), intent(in)           :: file
    character(*), intent(in), optional :: status
    character(*), intent(in), optional :: action

    character(:), allocatable :: lstatus, laction
    integer :: ierr

    !> Initialize FORTRAN interface.
    call h5open_f(ierr)
    if (ierr /= 0 ) error stop 'Error: HDF5 file initialize Failed!'

    lstatus = 'NEW'
    if(present(status)) lstatus = status

    laction = 'READWRITE'
    if(present(action)) laction = action

    if(lstatus == 'OLD') then
      if(laction == 'READ') then
        !> Open an existing file.
        call h5fopen_f(file,H5F_ACC_RDONLY_F,self%lid,ierr)
      elseif(laction == 'WRITE' .or. laction == 'READWRITE') then
        call h5fopen_f(file,H5F_ACC_RDWR_F,self%lid,ierr)
      else
        error stop 'Error: Unsupported action ->'//laction
      endif
    elseif(lstatus == 'NEW') then
      call h5fcreate_f(file,H5F_ACC_TRUNC_F,self%lid,ierr)
    elseif(status == 'REPLACE') then
      call system("rm -f "//file)
      call h5fcreate_f(file,H5F_ACC_TRUNC_F,self%lid,ierr)
    else
      error stop 'Error: Unsupported status ->'//status
    endif

  end subroutine hdf_initialize
  !=============================================================================
  subroutine hdf_finalize(self)
    class(hdf5_file), intent(in) :: self

    integer :: ierr

    !> close hdf5 file
    call h5fclose_f(self%lid, ierr)

    !>  Close FORTRAN interface.
    call h5close_f(ierr)

  end subroutine hdf_finalize
  !=============================================================================
  subroutine hdf_add_group(self, name)

    class(hdf5_file), intent(in) :: self
    character(len=*), intent(in) :: name    !< relative path to group

    integer(HID_T)  :: gid

    integer :: ierr, sp, ep, sl
    logical :: exist

    sl = len(name)
    sp = 1
    ep = 0

    do

       ep = index(name(sp+1:sl), "/")

       ! no subgroup found
       if (ep == 0) exit

       ! check subgroup exists
       sp = sp + ep
       call h5lexists_f(self%lid, name(1:sp-1), exist, ierr)

       if(.not.exist) then
         call h5gcreate_f(self%lid, name(1:sp-1), gid, ierr)
         call h5gclose_f(gid, ierr)
       endif

    end do

  end subroutine hdf_add_group
  !=============================================================================
  subroutine hdf_open_group(self,name)
    class(hdf5_file), intent(inout) :: self
    character(*), intent(in)        :: name

    integer :: ierr

    call h5gopen_f(self%lid, name, self%gid, ierr)
    self%glid = self%lid
    self%lid  = self%gid

  end subroutine hdf_open_group
  !=============================================================================
  subroutine hdf_close_group(self)
    class(hdf5_file), intent(inout) :: self

    integer :: ierr

    call h5gclose_f(self%gid, ierr)
    self%lid = self%glid

  end subroutine hdf_close_group
  !=============================================================================
  subroutine hdf_add_int(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    integer, intent(in)      :: value

    integer(HSIZE_T):: dim(1)
    integer(HID_T)  :: sid,did
    integer         :: ierr

    dim  = (/0/)

    call self%add(name)

    !> create dataspace
    call h5screate_f(H5S_SCALAR_F, sid, ierr)

    !> create dataset
    call h5dcreate_f(self%lid, name, H5T_NATIVE_INTEGER, sid, did, ierr)

    !> write dataset
    call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dim, ierr)

    !> close space and dataset
    call h5dclose_f(did, ierr)

    call h5sclose_f(sid, ierr)


  end subroutine hdf_add_int
  !=============================================================================
  subroutine hdf_add_int1d(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    integer, intent(in)      :: value(:)

    integer         :: rank
    integer(HSIZE_T):: dim(1)
    integer         :: ierr

    rank = 1
    dim  = size(value)

    call self%add(name)


    call h5ltmake_dataset_f(self%lid, name, rank, dim, H5T_NATIVE_INTEGER, value, ierr)

  end subroutine hdf_add_int1d
  !=============================================================================
  subroutine hdf_add_int2d(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    integer, intent(in)      :: value(:,:)

    integer         :: rank
    integer(HSIZE_T):: dim(2)
    integer         :: ierr

    rank = 2
    dim  = shape(value)

    call self%add(name)

    call h5ltmake_dataset_f(self%lid, name, rank, dim, H5T_NATIVE_INTEGER, value, ierr)

  end subroutine hdf_add_int2d
  !=============================================================================
  subroutine hdf_add_int3d(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    integer, intent(in)      :: value(:,:,:)

    integer         :: rank
    integer(HSIZE_T):: dim(3)
    integer         :: ierr

    rank = 3
    dim  = shape(value)

    call self%add(name)

    call h5ltmake_dataset_f(self%lid, name, rank, dim, H5T_NATIVE_INTEGER, value, ierr)

  end subroutine hdf_add_int3d
  !=============================================================================
  subroutine hdf_add_real(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    real, intent(in)      :: value

    integer(HSIZE_T):: dim(1)
    integer(HID_T)  :: sid,did
    integer         :: ierr

    dim  = (/0/)

    call self%add(name)

    !> create dataspace
    call h5screate_f(H5S_SCALAR_F, sid, ierr)

    !> create dataset
    call h5dcreate_f(self%lid, name, H5T_NATIVE_DOUBLE, sid, did, ierr)

    !> write dataset
    call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dim, ierr)

    !> close space and dataset
    call h5dclose_f(did, ierr)

    call h5sclose_f(sid, ierr)


  end subroutine hdf_add_real
  !=============================================================================
  subroutine hdf_add_real1d(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    real, intent(in)      :: value(:)

    integer         :: rank
    integer(HSIZE_T):: dim(1)
    integer         :: ierr

    rank = 1
    dim  = size(value)

    call self%add(name)


    call h5ltmake_dataset_f(self%lid, name, rank, dim, H5T_NATIVE_INTEGER, value, ierr)

  end subroutine hdf_add_real1d
  !=============================================================================
  subroutine hdf_add_real2d(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    real, intent(in)      :: value(:,:)

    integer         :: rank
    integer(HSIZE_T):: dim(2)
    integer         :: ierr

    rank = 2
    dim  = shape(value)

    call self%add(name)

    call h5ltmake_dataset_f(self%lid, name, rank, dim, H5T_NATIVE_INTEGER, value, ierr)

  end subroutine hdf_add_real2d
  !=============================================================================
  subroutine hdf_add_real3d(self,name,value)
    class(hdf5_file), intent(in) :: self
    character(*), intent(in) :: name
    real, intent(in)      :: value(:,:,:)

    integer         :: rank
    integer(HSIZE_T):: dim(3)
    integer         :: ierr

    rank = 3
    dim  = shape(value)

    call self%add(name)

    call h5ltmake_dataset_f(self%lid, name, rank, dim, H5T_NATIVE_INTEGER, value, ierr)

  end subroutine hdf_add_real3d
  !=============================================================================
  subroutine hdf_get_int(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    integer, intent(out)             :: value

    integer(SIZE_T) :: dim(1)
    integer(HID_T)  :: set_id
    integer :: ierr

    dim = (/0/)
    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)

  end subroutine hdf_get_int
  !=============================================================================
  subroutine hdf_get_int1d(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    integer, intent(out),allocatable :: value(:)

    integer :: rank
    integer(SIZE_T) :: dim(1),maxdim(1)
    integer(HID_T)  :: set_id,space_id
    integer :: ierr

    rank = 1
    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! get dataspace
    call h5dget_space_f(set_id, space_id, ierr)

    ! get dims
    call h5sget_simple_extent_dims_f(space_id, dim, maxdim, ierr)

    allocate(value(dim(1)))

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)

  end subroutine hdf_get_int1d
  !=============================================================================
  subroutine hdf_get_int2d(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    integer, intent(out),allocatable :: value(:,:)

    integer :: rank
    integer(SIZE_T) :: dim(2),maxdim(2)
    integer(HID_T)  :: set_id, space_id
    integer :: ierr

    rank = 2

    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! get dataspace
    call h5dget_space_f(set_id, space_id, ierr)

    ! get dims
    call h5sget_simple_extent_dims_f(space_id, dim, maxdim, ierr)

    allocate(value(dim(1),dim(2)))

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)


  end subroutine hdf_get_int2d
  !=============================================================================
  subroutine hdf_get_int3d(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    integer, intent(out),allocatable :: value(:,:,:)

    integer :: rank
    integer(SIZE_T) :: dim(3),maxdim(3)
    integer(HID_T)  :: set_id, space_id
    integer :: ierr

    rank = 3

    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! get dataspace
    call h5dget_space_f(set_id, space_id, ierr)

    ! get dims
    call h5sget_simple_extent_dims_f(space_id, dim, maxdim, ierr)

    allocate(value(dim(1),dim(2),dim(3)))

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)


  end subroutine hdf_get_int3d
  !=============================================================================
  subroutine hdf_get_real(self, name, value)

    class(hdf5_file), intent(in)  :: self
    character(*), intent(in)      :: name
    real, intent(out)             :: value

    integer(SIZE_T) :: dim(1)
    integer(HID_T)  :: set_id
    integer :: ierr

    dim = (/0/)
    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_DOUBLE, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)

  end subroutine hdf_get_real
  !=============================================================================
  subroutine hdf_get_real1d(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    real, intent(out),allocatable :: value(:)

    integer :: rank
    integer(SIZE_T) :: dim(1),maxdim(1)
    integer(HID_T)  :: set_id,space_id
    integer :: ierr

    rank = 1
    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! get dataspace
    call h5dget_space_f(set_id, space_id, ierr)

    ! get dims
    call h5sget_simple_extent_dims_f(space_id, dim, maxdim, ierr)

    allocate(value(dim(1)))

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)

  end subroutine hdf_get_real1d
  !=============================================================================
  subroutine hdf_get_real2d(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    real, intent(out),allocatable :: value(:,:)

    integer :: rank
    integer(SIZE_T) :: dim(2),maxdim(2)
    integer(HID_T)  :: set_id, space_id
    integer :: ierr

    rank = 2

    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! get dataspace
    call h5dget_space_f(set_id, space_id, ierr)

    ! get dims
    call h5sget_simple_extent_dims_f(space_id, dim, maxdim, ierr)

    allocate(value(dim(1),dim(2)))

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)


  end subroutine hdf_get_real2d
  !=============================================================================
  subroutine hdf_get_real3d(self, name, value)

    class(hdf5_file), intent(in)     :: self
    character(*), intent(in)         :: name
    real, intent(out),allocatable :: value(:,:,:)

    integer :: rank
    integer(SIZE_T) :: dim(3),maxdim(3)
    integer(HID_T)  :: set_id, space_id
    integer :: ierr

    rank = 3

    ! open dataset
    call h5dopen_f(self%lid, name, set_id, ierr)

    ! get dataspace
    call h5dget_space_f(set_id, space_id, ierr)

    ! get dims
    call h5sget_simple_extent_dims_f(space_id, dim, maxdim, ierr)

    allocate(value(dim(1),dim(2),dim(3)))

    ! read dataset
    call h5dread_f(set_id, H5T_NATIVE_INTEGER, value,dim, ierr)

    ! close dataset
    call h5dclose_f(set_id, ierr)


  end subroutine hdf_get_real3d

end module hdf5_interface
