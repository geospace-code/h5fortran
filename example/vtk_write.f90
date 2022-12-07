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

type(hdf5_file) :: h

call get_command_argument(1, filename, status=ierr)
if(ierr /= 0) error stop "please give filename to write"

call h % open(filename, 'w')

call h % write("/VTKHDF/Version", [1, 0])

call h % close()


end program
