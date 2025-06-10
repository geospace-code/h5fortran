submodule (h5fortran) iterate_smod
  use hdf5
  implicit none

  interface
    subroutine user_callback_interface(group_name, object_name, object_type)
      character(len=*), intent(in) :: group_name
        !! The name of the group being traversed.
      character(len=*), intent(in) :: object_name
        !! The name of the object encountered.
      character(len=*), intent(in) :: object_type
        !!A short description such as "group", "dataset",
        !!                            "datatype", or "other"
    end subroutine
  end interface

contains

  module procedure hdf_iterate
    use, intrinsic :: iso_c_binding, only: c_funptr, C_NULL_PTR, c_int
    implicit none
    integer(hid_t) :: group_id
    integer(c_int) :: status
    integer(hsize_t) :: idx
    type(c_funptr) :: funptr
    type(c_ptr) :: op_data_ptr
    integer(c_int) :: return_value
    procedure(user_callback_interface), pointer :: user_callback => null()

    ! Fill the iteration data with the user’s group name and callback.
    user_callback => callback

    ! Open the group.
    call H5Gopen_f(self%file_id, trim(group_name), group_id, status)
    call estop(status, "hdf_iterate:H5Gopen_f", self%filename, "Error opening group: " // trim(group_name))

    idx = 0
    op_data_ptr = C_NULL_PTR
    ! Get the C function pointer for our internal callback.
    funptr = c_funloc(internal_iterate_callback)

    ! Call H5Literate_f to iterate over the group.
    call H5Literate_f(group_id, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, idx, &
                      funptr, op_data_ptr, return_value, status)
    call estop(status, "hdf_iterate:H5Literate_f", self%filename, "Error during iteration of group: " // trim(group_name))

    ! Close the group and file.
    call H5Gclose_f(group_id, status)

  contains

    integer(c_int) function internal_iterate_callback(grp_id, name, info, op_data) bind(C)
      !!  internal_iterate_callback:
      !!
      !!  This is the callback procedure that will be passed to H5Literate_f.
      !!  It matches HDF5’s expected signature (using bind(C)) and is called
      !!  for each object in the group.
      !!
      !!  It extracts the object name from the provided character array,
      !!  calls H5Oget_info_by_name_f to determine the object type, and then
      !!  calls the user's callback with the high-level parameters.
      use ISO_C_BINDING, only: c_int, c_long, c_ptr, c_null_char
      implicit none
      integer(c_long), value        :: grp_id
      character(kind=c_char, len=1) :: name(0:255)
      type(h5l_info_t)              :: info
      type(c_ptr)                   :: op_data

      integer :: status, i, len
      type(H5O_info_t) :: infobuf
      character(len=256) :: name_string
      character(:), allocatable :: object_type

      ! FIXME - This is a workaround for the Fortran unused variable warning/error.
      if (info % type == info % type) continue
      if (c_associated(op_data)) continue

      ! Build a Fortran string from the character array.
      do i = 0, 255
        len = i
        if (name(i) == c_null_char) exit
        name_string(i+1:i+1) = name(i)(1:1)
      end do

      ! Retrieve object info using the object name.
      call H5Oget_info_by_name_f(grp_id, name_string(1:len), infobuf, status)
      if (status /= 0) then
        internal_iterate_callback = status
        return
      end if

      if(infobuf % type == H5O_TYPE_GROUP_F)then
        object_type = "group"
      else if(infobuf % type == H5O_TYPE_DATASET_F)then
        object_type = "dataset"
      else if(infobuf % type == H5O_TYPE_NAMED_DATATYPE_F)then
        object_type = "datatype"
      else
        object_type = "other"
      endif

      ! Call the user’s callback procedure.
      call user_callback(group_name, name_string(1:len), object_type)

      internal_iterate_callback = 0  ! Indicate success.
    end function internal_iterate_callback

  end procedure hdf_iterate

end submodule
