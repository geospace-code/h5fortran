program test_iterate
  use, intrinsic :: iso_fortran_env, only: real64
  use h5fortran, only: hdf5_file
  implicit none

  type(hdf5_file) :: h
  character(*), parameter :: filename='test_iterate.h5'
  integer :: i = 0

  ! Create a sample HDF5 file
  call h%open(filename, "w")

  call h%create_group("/group1")
  call h%create_group("/group1/group2")
  call h%write("/dataset1", 1.0_real64)
  call h%write("/group1/dataset2", 2.0_real64)

  call h%close()

  ! Reopen the file for testing
  call h%open(filename, "r")

  ! iterate the root group
  print*, "test_iterate: iterating root group"
  call h%iterate("/", my_callback)

  print*, "test_iterate: iterating /group1"
  ! iterate a subgroup
  call h%iterate("/group1", my_callback)

  call h%close()

  print '(a,i0,a)', "test_iterate: found ", i, " objects"
  if (i /= 4) error stop "test_iterate: expected 4 objects"

contains

  ! Define a callback subroutine
  subroutine my_callback(group_name, object_name, object_type)
    character(*), intent(in) :: group_name, object_name, object_type
    print '(6a)', "test_iterate: at group ", trim(group_name), ' we found ', trim(object_name), ' which is a ', trim(object_type)
    i = i + 1
  end subroutine my_callback

end program test_iterate
