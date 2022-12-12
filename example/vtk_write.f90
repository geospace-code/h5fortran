!! example of VTK HDF5 file format
!! this is a de facto HDF5 file template using specific group names and dataset hierarchy.
!! ParaView can also read these VTK HDF5 files.
!!
!! References:
!! https://kitware.github.io/vtk-examples/site/VTKFileFormats/#hdf-file-formats
!! https://www.kitware.com/vtk-hdf-reader/
!! https://gitlab.kitware.com/danlipsa/vtkxml-to-vtkhdf
!! https://gitlab.kitware.com/vtk/vtk/-/blob/master/IO/HDF/Testing/Cxx/TestHDFReader.cxx

program vtk_write

use h5fortran, only : hdf5_file

implicit none

character(1024) :: filename
integer :: ierr

!> these are just to avoid typos, you can type out full name each time
character(7), parameter :: vtkhdf = "/VTKHDF"
character(17), parameter :: pointdata = vtkhdf // "/PointData"

type(hdf5_file) :: h

real, allocatable :: Iterations(:,:,:), IterationsGradient(:,:,:,:)

call get_command_argument(1, filename, status=ierr)
if(ierr /= 0) error stop "please give filename to write"

call h % open(filename, 'w')

call h % create_group(vtkhdf)
call h % writeattr(vtkhdf ,"Version", [1, 0])

!> ImageData example
call h % writeattr(vtkhdf, "Direction", [1, 0, 0, 0, 1, 0, 0, 0, 1])
call h % writeattr(vtkhdf, "Origin", [-1.75, -1.25, 0.])
call h % writeattr(vtkhdf, "Spacing", [0.131579, 0.125, 0.0952381])
call h % writeattr(vtkhdf, "WholeExtent", [0, 19, 0, 20, 0, 21])
call h % writeattr(vtkhdf, "Type", "ImageData")
call h % create_group(pointdata)
call h % writeattr(pointdata, "Scalars", "IterationsGradient")

allocate(Iterations(20, 21, 22))
call random_number(Iterations)
call h % write(pointdata // "/Iterations", Iterations)
deallocate(Iterations)

! allocate(IterationsGradient(3, 20, 21, 22))
! call h % write(pointdata // "/IterationsGradient", IterationsGradient)
! deallocate(IterationsGradient)

call h % close()


end program
