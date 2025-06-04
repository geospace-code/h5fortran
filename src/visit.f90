submodule (h5fortran:hdf5_read) visit_smod
  use, intrinsic :: iso_c_binding, only : c_long, c_char, c_funloc, c_int, c_funptr, c_null_ptr, C_associated
  use hdf5, only : h5l_info_t, h5o_info_t, H5O_TYPE_DATASET_F, H5O_TYPE_GROUP_F, &
                   H5O_TYPE_NAMED_DATATYPE_F, H5O_TYPE_UNKNOWN_F, &
                   H5_INDEX_NAME_F, H5_ITER_NATIVE_F, &
                   H5Gopen_f, H5Gclose_f, H5Oget_info_by_name_f, &
                   H5Ovisit_f

  implicit none

  interface
    subroutine user_callback_interface(group_name, object_name, object_type)
      character(*), intent(in) :: group_name
        !! The name of the group being traversed.
      character(*), intent(in) :: object_name
        !! The name of the object encountered.
      character(*), intent(in) :: object_type
        !!A short description such as "group", "dataset",
        !!                            "datatype", or "other"
    end subroutine
  end interface

  type :: visit_data_t
     procedure(user_callback_interface), nopass, pointer :: callback => null()
  end type visit_data_t

contains

  module procedure hdf_visit
    integer(hid_t) :: group_id
    integer(c_int) :: status
    integer(hsize_t) :: idx
    type(c_funptr) :: funptr
    type(c_ptr) :: op_data_ptr
    integer(c_int) :: return_value

    type(visit_data_t) :: data

    ! Fill the iteration data with the user’s group name and callback.
    data % callback => callback

    ! Open the group.
    call H5Gopen_f(self%file_id, trim(group_name), group_id, status)
    call estop(status, "hdf_visit:H5Gopen_f", self%filename, "Error opening group: " // trim(group_name))

    idx = 0
    op_data_ptr = C_NULL_PTR
    ! Get the C function pointer for our internal callback.
    funptr = c_funloc(internal_visit_callback)

    ! Call H5Lvisit_f to visit over the group.
    call H5Ovisit_f(group_id, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, &
                      funptr, op_data_ptr, return_value, status)
    call estop(status, "hdf_visit:H5Lvisit_f", self%filename, "Error during iteration of group: " // trim(group_name))

    ! Close the group and file.
    call H5Gclose_f(group_id, status)

  contains

    integer(c_int) function internal_visit_callback(grp_id, name, info, op_data) bind(C)
      !!  internal_visit_callback:
      !!
      !!  This is the callback procedure that will be passed to H5Lvisit_f.
      !!  It matches HDF5’s expected signature (using bind(C)) and is called
      !!  for each object in the group.
      !!
      !!  It extracts the object name from the provided character array,
      !!  calls H5Oget_info_by_name_f to determine the object type, and then
      !!  calls the user's callback with the high-level parameters.
      integer(c_long), intent(in), value        :: grp_id
      character(1, kind=c_char), intent(in) :: name(0:255)
      type(h5l_info_t), intent(in)              :: info
      type(c_ptr), intent(in)                   :: op_data

      integer :: i, ierr
      type(H5O_info_t) :: infobuf
      character(256) :: name_string
      character(:), allocatable :: object_type

      ! avoid unused argument warning
      if (C_associated(op_data) .and. info % corder == 0) i = 0

      ! Build a Fortran string from the character array.
      do i = 0, 255
        if (name(i) == c_null_char) exit
        name_string(i+1:i+1) = name(i)(1:1)
      end do

      ! Retrieve object info using the object name.
      call H5Oget_info_by_name_f(grp_id, name_string(:i), infobuf, ierr)
      if (ierr /= 0) then
        internal_visit_callback = ierr
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
      call data % callback(group_name, name_string(:i), object_type)

      internal_visit_callback = 0  ! Indicate success.
    end function internal_visit_callback

  end procedure hdf_visit

end submodule
