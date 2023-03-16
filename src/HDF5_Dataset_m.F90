module HDF5_Dataset_m
  use Buffer_m, only: Buffer
  use HDF5_AbstractTypes_m, only: HDF5_Node, HDF5_Node_Child, HDF5_Leaf, check
  use hdf5

  implicit none
  private


  type, extends(HDF5_Leaf), public :: HDF5_Dataset
    integer(hid_t) :: type_id
    integer :: rank
    integer(hsize_t), allocatable :: chunks(:)
    integer(hsize_t), allocatable :: max_dims(:)

    contains
      procedure :: attach => node_attach
      procedure :: flush_upstream
      procedure :: flush_downstream
      procedure :: flush_upstream_wdata
  end type HDF5_Dataset

  contains
    subroutine node_attach(this, child)
      class(HDF5_Dataset), intent(inout) :: this
      class(HDF5_Node), pointer :: child

      integer :: r

      select type(child)
      class is ( Buffer )
        r = size(child%dims)
        this%rank = r+1
        this%max_dims = [child%dims, H5S_UNLIMITED_F]
        this%chunks = [integer(hsize_t) :: this%max_dims(1:r), child%t_max]
      end select

      if(.not. allocated(this%children)) then
        allocate(this%children(0), source=HDF5_Node_Child(NULL()))
      end if

      this%children = [this%children, HDF5_Node_Child(child)]
    end subroutine

    subroutine flush_upstream(this)
      class(HDF5_Dataset), intent(inout) :: this

    end subroutine

    subroutine flush_downstream(this)
      class(HDF5_Dataset), intent(inout) :: this

      ! this is the ultimate leaf in the hdf5 tree
      !   further leaves are data sources
      ! thus go upstream from here
      integer :: i

      if(.not.allocated(this%children)) then
        call this%flush_upstream()
        return
      else if(size(this%children).eq.0) then
        call this%flush_upstream()
        return
      end if

      do i=1, size(this%children)
        call this%children(i)%child%flush_downstream()
      end do
    end subroutine

    subroutine flush_upstream_wdata(this, buf)
      class(HDF5_Dataset), intent(inout) :: this
      real(8) :: buf(..)

      integer(hid_t) :: loc_id, type_id, s_id, ds_id, cp_id
      integer :: ierr
      logical :: exists = .false.

      type_id = H5T_NATIVE_REAL

      call this%parent%loc_id_open(loc_id, ierr)

      call h5lexists_f(loc_id, this%nm, exists, ierr)
      call check("h5lexists_f", ierr)

      if(exists) then

      else
        call h5screate_simple_f(this%rank, this%dims, s_id, ierr)
        call check("h5screate_simple_f", ierr)

        call h5dcreate_f(loc_id, this%nm, type_id, s_id, ds_id, &
          ierr)!, dcpl_id=cp_id)
        call check("h5dcreate_f", ierr)


        call h5dclose_f(ds_id, ierr)
        call check("h5dclose_f", ierr)

        call h5sclose_f(s_id, ierr)
        call check("h5sclose_f", ierr)
      end if

      call this%parent%loc_id_close(loc_id, ierr)

    end subroutine

end module HDF5_Dataset_m
