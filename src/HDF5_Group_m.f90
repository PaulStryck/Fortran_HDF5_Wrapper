module HDF5_Group_m
  use HDF5_AbstractTypes_m, only: HDF5_Node, check
  use hdf5

  implicit none
  private

  type, extends(HDF5_Node), public :: HDF5_Group
    integer(hid_t), private :: open_p_loc_id = -1

    contains
      procedure :: flush_upstream
      procedure :: flush_downstream

      procedure :: loc_id_open => node_loc_id_open
      procedure :: loc_id_close => node_loc_id_close
  end type HDF5_Group

  contains
    subroutine flush_upstream(this)
      class(HDF5_Group), intent(inout) :: this

      integer(hid_t) :: loc_id
      integer :: ierr

      call this%loc_id_open(loc_id, ierr)
      call check("open parent from group", ierr)

      call this%loc_id_close(loc_id, ierr)
      call check("close parent from group", ierr)
    end subroutine

    subroutine flush_downstream(this)
      class(HDF5_Group), intent(inout) :: this

      integer :: i

      if(.not.allocated(this%children)) then
        call this%flush_upstream()
        return
      else if(size(this%children) .eq. 0) then
        call this%flush_upstream()
        return
      end if

      do i = 1, size(this%children)
        call this%children(i)%child%flush_downstream()
      end do
    end subroutine


    subroutine node_loc_id_open(this, loc_id, ierr)
      class(HDF5_Group), intent(inout) :: this
      integer(hid_t), intent(inout) :: loc_id
      integer, intent(out) :: ierr

      integer(hid_t) :: p_loc_id
      logical :: exists = .false.

      call this%parent%loc_id_open(p_loc_id, ierr)

      call h5lexists_f(p_loc_id, this%nm, exists, ierr)
      call check("h5lexists_f", ierr)

      if(exists) then
        call h5gopen_f(p_loc_id, this%nm, loc_id, ierr)
        call check("h5gopen_f", ierr)
      else
        call h5gcreate_f(p_loc_id, this%nm, loc_id, ierr)
        call check("h5gcreate_f", ierr)
      end if

      this%open_p_loc_id = p_loc_id
    end subroutine

    subroutine node_loc_id_close(this, loc_id, ierr)
      class(HDF5_Group), intent(inout) :: this
      integer(hid_t), intent(inout) :: loc_id
      integer, intent(out) :: ierr

      call h5gclose_f(loc_id, ierr)
      call check("h5gclose_f", ierr)

      call this%parent%loc_id_close(this%open_p_loc_id, ierr)
      call check("close parent loc_id of group", ierr)

      this%open_p_loc_id = -1
    end subroutine

end module HDF5_Group_m
