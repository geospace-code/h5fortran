submodule (h5fortran) pathlib
!! vendored from Michael Hirsch's Fortran pathlib

implicit none (type, external)

contains

module procedure get_tempdir

character(1024) :: argv
integer :: L, i

call get_environment_variable("TMP", argv, L, i)
if (L==0 .or. i/=0) call get_environment_variable("TEMP", argv, L, i)
if (L==0 .or. i/=0) call get_environment_variable("TMPDIR", argv, L, i)
if (L==0 .or. i/=0) argv = "/tmp"

get_tempdir = trim(argv)

end procedure get_tempdir


module procedure is_absolute_path
!! heuristic to determine if is absolute path

is_absolute_path = .false.

if(@is_windows@) then
  if (lge(path(1:1), 'A') .and. lle(path(1:1), 'z') .and. path(2:2) == ':') is_absolute_path = .true.
else
  if(path(1:1) == '/') is_absolute_path=.true.
endif

end procedure is_absolute_path


module procedure std_unlink
!! deletes file in Fortran standard manner.
integer :: i, u

open(newunit=u, file=filename, iostat=i)
close(u, status='delete', iostat=i)

inquire(file=filename, exist=std_unlink)

end procedure std_unlink

end submodule pathlib
