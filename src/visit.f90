submodule (h5fortran) visit_smod
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

  module procedure hdf_visit
    implicit none
    integer(hid_t) :: group_id
    integer(c_int) :: status
    type(c_funptr) :: funptr
    type(c_ptr) :: op_data_ptr
    integer(c_int) :: return_value
    procedure(user_callback_interface), pointer :: user_callback => null()

    user_callback => callback

    call H5Gopen_f(self%file_id, trim(group_name), group_id, status)
    call estop(status, "hdf_visit:H5Gopen_f", self%filename, "Error opening group: " // trim(group_name))

    op_data_ptr = C_NULL_PTR
    funptr = c_funloc(internal_visit_callback)

    call H5Ovisit_f(group_id, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, funptr, op_data_ptr, return_value, status)
    call estop(status, "hdf_visit:H5Ovisit_f", self%filename, "Error during visit of group: " // trim(group_name))
    if (return_value < 0) then
      call estop(1_c_int, "hdf_visit:H5Ovisit", self%filename, "Error during visit of group: " // trim(group_name))
    end if

    call H5Gclose_f(group_id, status)

  contains

    integer(c_int) function internal_visit_callback(grp_id, name, info, op_data) bind(C)
      implicit none
      integer(c_intptr_t), value :: grp_id
      character(kind=c_char), intent(in) :: name(*)
      type(h5o_info_t), intent(in) :: info
      type(c_ptr), value :: op_data

      character(len=256) :: name_str
      character(:), allocatable :: object_type
      integer :: i, ln

      if (grp_id /= 0_c_intptr_t) continue
      if (c_associated(op_data)) continue

      ln = 0
      do i = 1, 256
        if (name(i) == c_null_char) exit
        name_str(i:i) = name(i)
        ln = i
      end do

      if (info % type == H5O_TYPE_GROUP_F) then
        object_type = "group"
      else if (info % type == H5O_TYPE_DATASET_F) then
        object_type = "dataset"
      else if (info % type == H5O_TYPE_NAMED_DATATYPE_F) then
        object_type = "datatype"
      else
        object_type = "other"
      end if

      call user_callback(group_name, name_str(1:ln), object_type)

      internal_visit_callback = 0
    end function internal_visit_callback

  end procedure hdf_visit

end submodule
