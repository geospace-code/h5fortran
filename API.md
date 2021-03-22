# h5fortran API

This document provides a listing of h5fortran `public` scoped user-facing procedures and methods with a summary of their parameters.

All examples assume:

```fortran
use h5fortran, only: hdf5_file

type(hdf5_file) :: h
```

## Open / close HDF5 file reference

More than one HDF5 file can be open in a program, by declaring unique file handle (variable) like:

```fortran
type(hdf5_file) :: h1, h2, h3
```

```fortran
call h%initialize(filename,ierr, status,action,comp_lvl,verbose,debug)
!! Opens hdf5 file

character(*), intent(in) :: filename
integer, intent(out), optional :: ierr  !< 0 if OK
character(*), intent(in), optional :: status  !< old, unknown, new, replace, scratch
character(*), intent(in), optional :: action  !< read, write, readwrite
integer, intent(in), optional      :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
logical, intent(in), optional      :: verbose, debug
```

```fortran
call h%finalize(ierr, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

integer, intent(out), optional :: ierr
logical, intent(in), optional :: close_hdf5_interface
```

To avoid memory leaks or corrupted files, always "finalize" all hDF5 files before STOPping the Fortran program.


```fortran
call h%flush(ierr)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
integer, intent(out), optional :: ierr
```
