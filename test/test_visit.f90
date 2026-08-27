program test_visit
  use, intrinsic :: iso_fortran_env, only: real64
  use h5fortran
  use hdf5
  implicit none

  type(hdf5_file) :: h
  character(*), parameter :: filename='test_visit.h5'
  integer :: i

  i = 0

  ! Create a sample HDF5 file
  call h%open(filename, "w")

  call h%create_group("/group1")
  call h%create_group("/group1/group2")
  call h%write("/dataset1", 1.0_real64)
  call h%write("/group1/dataset2", 2.0_real64)

  call h%close()

  ! Reopen the file for testing
  call h%open(filename, "r")

  ! visit the root group
  print*, "test_visit: visiting root group"
  call h%visit("/", my_callback)

  print*, "test_visit: visiting /group1"
  ! visit a subgroup
  call h%visit("/group1", my_callback)

  call h%close()

  print*, "test_visit: found ", i, " objects"
  if (i /= 8) error stop "test_visit: expected 8 objects"

contains

  ! Define a callback subroutine
  subroutine my_callback(group_name, object_name, object_type)
    character(len=*), intent(in) :: group_name, object_name, object_type
    print *, "test_visit: at group ", trim(group_name), ' we found ', trim(object_name), ' that is a ', trim(object_type)
    i = i + 1
  end subroutine my_callback

end program test_visit
