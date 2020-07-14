module pathlib
!! vendored from Michael Hirsch's Fortran pathlib

implicit none (type, external)

contains

function get_tempdir()

character(1024) :: argv
integer :: L, i
character(:), allocatable :: get_tempdir

call get_environment_variable("TMP", argv, L, i)
if (L==0 .or. i/=0) call get_environment_variable("TEMP", argv, L, i)
if (L==0 .or. i/=0) call get_environment_variable("TMPDIR", argv, L, i)
if (L==0 .or. i/=0) argv = "/tmp"

get_tempdir = trim(argv)

end function get_tempdir


logical function unlink(filename)
!! deletes file in Fortran standard manner.
character(*), intent(in) :: filename
integer :: i, u

open(newunit=u, file=filename, iostat=i)
close(u, status='delete', iostat=i)

inquire(file=filename, exist=unlink)

end function unlink

end module pathlib
