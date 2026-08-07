submodule (h5fortran) iterate_smod
  use hdf5
  use, intrinsic :: iso_c_binding
  implicit none

  interface
    subroutine user_callback_interface(group_name, object_name, object_type)
      character(len=*), intent(in) :: group_name
      character(len=*), intent(in) :: object_name
      character(len=*), intent(in) :: object_type
    end subroutine
  end interface

contains

  module procedure hdf_iterate
    implicit none
    integer(hid_t) :: group_id
    integer(c_int) :: status
    integer(hsize_t) :: idx
    type(c_funptr) :: funptr
    type(c_ptr) :: op_data_ptr
    integer(c_int) :: return_value
    procedure(user_callback_interface), pointer :: user_callback => null()

    user_callback => callback

    call H5Gopen_f(self%file_id, trim(group_name), group_id, status)
    call estop(status, "hdf_iterate:H5Gopen_f", self%filename, "Error opening group: " // trim(group_name))

    idx = 0_hsize_t
    op_data_ptr = C_NULL_PTR
    funptr = c_funloc(internal_iterate_callback)

    call H5Literate_f(group_id, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, idx, funptr, op_data_ptr, return_value, status)
    call estop(status, "hdf_iterate:H5Literate_f", self%filename, "Error during iteration of group: " // trim(group_name))
    if (return_value < 0) then
      call estop(1_c_int, "hdf_iterate:H5Literate", self%filename, "Error during iteration of group: " // trim(group_name))
    end if

    call H5Gclose_f(group_id, status)

  contains

    integer(c_int) function internal_iterate_callback(grp_id, name, info, op_data) bind(C)
      implicit none
      integer(c_intptr_t), value :: grp_id
      character(kind=c_char), intent(in) :: name(*)
      type(h5l_info_t), intent(in) :: info
      type(c_ptr), value :: op_data

      integer :: i, ln, obj_status
      integer(hid_t) :: loc_id
      type(h5o_info_t) :: obj_info
      character(len=256) :: name_str
      character(:), allocatable :: object_type

      if (info % corder == info % corder) continue
      if (c_associated(op_data)) continue

      ln = 0
      do i = 1, 256
        if (name(i) == c_null_char) exit
        name_str(i:i) = name(i)
        ln = i
      end do

      loc_id = int(grp_id, hid_t)
      call H5Oget_info_by_name_f(loc_id, name_str(1:ln), obj_info, obj_status)
      if (obj_status == 0) then
        if (obj_info % type == H5O_TYPE_GROUP_F) then
          object_type = "group"
        else if (obj_info % type == H5O_TYPE_DATASET_F) then
          object_type = "dataset"
        else if (obj_info % type == H5O_TYPE_NAMED_DATATYPE_F) then
          object_type = "datatype"
        else
          object_type = "other"
        end if
      else
        object_type = "other"
      end if

      call user_callback(group_name, name_str(1:ln), object_type)

      internal_iterate_callback = 0
    end function internal_iterate_callback

  end procedure hdf_iterate

end submodule
