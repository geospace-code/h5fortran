c = configuration_data()
is_windows = os == 'windows' ? '.true.' : '.false.'
c.set('is_windows', is_windows)
configure_file(
  input : 'pathlib.in.f90',
  output : 'pathlib.f90',
  configuration : c)

c = configuration_data()
c.set('reader_lt_template',
'''
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old', action='r')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  ier = 6
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname)) then
  if (present(ierr)) return
  error stop
endif
''')
configure_file(
  input : 'reader_lt.in.f90',
  output : 'reader_lt.f90',
  configuration : c)

c = configuration_data()
c.set('writer_lt_template',
'''
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  ier = 6
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname)) then
  if (present(ierr)) return
  error stop
endif
''')
configure_file(
  input : 'writer_lt.in.f90',
  output : 'writer_lt.f90',
  configuration : c)

hdf5_src = files('interface.f90',
  'read.f90', 'reader.f90', meson.current_build_dir() / 'reader_lt.f90',
  'write.f90', 'writer.f90', meson.current_build_dir() / 'writer_lt.f90',
  'string_utils.f90', meson.current_build_dir() / 'pathlib.f90')
