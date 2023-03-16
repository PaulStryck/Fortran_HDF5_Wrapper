module HDF5_File_m
  use HDF5_AbstractTypes_m, only: HDF5_Node, check
  use hdf5

  implicit none
  private

  type, extends(HDF5_Node), public :: HDF5_File
    character(len=:), allocatable :: filename

    contains
      procedure :: flush_upstream
      procedure :: flush_downstream
      procedure :: attach_to

      procedure :: loc_id_open => node_loc_id_open
      procedure :: loc_id_close => node_loc_id_close

      procedure :: init => hdf5_file_init
      procedure :: check_tree => hdf5_file_check_tree
  end type

  contains
    !===============
    !Override HDF5_Node abstract functions
    !
    subroutine flush_upstream(this)
      class(HDF5_File), intent(inout) :: this

    end subroutine

    subroutine flush_downstream(this)
      class(HDF5_File), intent(inout) :: this

      integer :: i

      do i = 1, size(this%children)
        call this%children(i)%child%flush_downstream()
      end do
    end subroutine

    subroutine attach_to(this, parent)
      class(HDF5_File), intent(inout), target :: this
      class(HDF5_Node), pointer :: parent

      ! error, this is root
    end subroutine


    subroutine node_loc_id_open(this, loc_id, ierr)
      class(HDF5_File), intent(inout) :: this
      integer(hid_t), intent(inout) :: loc_id
      integer, intent(out) :: ierr

      integer(hid_t) :: p_loc_id
      logical :: exists = .false.


      call h5open_f(ierr);
      call check("h5open_f", ierr)

      call h5fopen_f(this%filename, H5F_ACC_RDWR_F, loc_id, ierr)
      call check("h5fopen_f", ierr)
    end subroutine

    subroutine node_loc_id_close(this, loc_id, ierr)
      class(HDF5_File), intent(inout) :: this
      integer(hid_t), intent(inout) :: loc_id
      integer, intent(out) :: ierr

      call H5Fclose_f(loc_id, ierr)
      call check("h5fclose_f", ierr)
    end subroutine


    !=================
    ! HDF5_File Functions
    !
    subroutine hdf5_file_init(this, path, nm)
      class(HDF5_File), intent(out) :: this
      character(len=*), intent(in) :: path, nm

      integer(hid_t) :: f_id, cp_id
      integer :: ierr

      this%nm = nm
      this%filename = path // nm
      this%is_root = .true.

      ! create file on dist
      call h5open_f(ierr); call check("h5open_f", ierr)

      call h5fcreate_f(this%filename, H5F_ACC_TRUNC_F, f_id, ierr); call check("h5fcreate_f", ierr)

      call h5pcreate_f(H5P_DATASET_CREATE_F, cp_id, ierr); call check("h5pcreate_f", ierr)

      call h5pclose_f(cp_id, ierr); call check("h5pclose_f", ierr)

      call h5fclose_f(f_id, ierr); call check("h5fclose_f", ierr)
    end subroutine

    subroutine hdf5_file_check_tree(this)
      class(HDF5_File), intent(inout) :: this
    end subroutine
end module HDF5_File_m
