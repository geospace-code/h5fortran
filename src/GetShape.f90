program HDF5_shapes
!! This program shows how HDF5 dimension orders are distinct in different langauges
use hdf5_interface, only: hdf5_file,hsize_t
use, intrinsic:: iso_fortran_env, only: real64, stdout=>output_unit
implicit none

type(hdf5_file) :: h5f
character(1024) :: argv
character(:), allocatable :: fn, dname, cmd
integer(HSIZE_T), allocatable :: dims(:)
integer :: ierr

if (command_argument_count() /= 2) error stop "filename dset_name"

call get_command_argument(1, argv)
fn = trim(argv)

call get_command_argument(2, argv)
dname = trim(argv)


call h5f%initialize(fn, status='old', action='r')


call h5f%shape(dname, dims)

print '(/,A,100I8)', 'Fortran dims: ',dims

call h5F%finalize()

!> use HDF5 tool to see their view
write(stdout,'(/,A)', advance='no') 'H5LS: '
call execute_command_line('h5ls '//fn//' | grep '//dname)

!> Python
call execute_command_line('which python > /dev/null', exitstat=ierr)
if (ierr==0) then
  write(stdout,'(/,A)', advance='no') 'Python h5py: '
   cmd = "import h5py; f = h5py.File('"//fn//"','r'); print(f['"//dname//"'].shape)"
  call execute_command_line('python -c "'//cmd//'"', exitstat=ierr)
endif
if (ierr/=0) print *,'Python HDF5 not available. '

!> Matlab
call execute_command_line('which matlab > /dev/null', exitstat=ierr)
if (ierr==0) then
  write(stdout,'(/,A)', advance='no') 'Matlab dims: '
   cmd="i=h5info('"//fn//"','/"//dname//"'); disp(i.Dataspace.Size); pause(0.1); exit"
  call execute_command_line('matlab -nojvm -r "'//cmd//'" | tail -n3', wait=.true., exitstat=ierr)
endif
if (ierr/=0) print *,'Matlab HDF5 not available. '


end program
