module HDF5_AbstractTypes_m
  use hdf5
  implicit none
  private

  type, public :: HDF5_Node_Child
    class(HDF5_Node), pointer :: child
  end type

  type, abstract, public :: HDF5_Node
    character(len=:), allocatable :: nm
    logical :: is_root = .false.
    class(HDF5_Node), pointer :: parent
    type(HDF5_Node_Child), allocatable :: children(:)

    contains
      procedure, pass(this) :: attach_to => node_attach_to
      procedure, pass(this) :: attach    => node_attach
      procedure, pass(this) :: loc_id_open => node_loc_id_open
      procedure, pass(this) :: loc_id_close => node_loc_id_close

      procedure(FLUSH_UPSTREAM),   deferred, pass(this) :: flush_upstream
      procedure(FLUSH_DOWNSTREAM), deferred, pass(this) :: flush_downstream
  end type

  type, abstract, extends(HDF5_Node), public :: HDF5_Leaf
    integer(hsize_t), allocatable :: dims(:)

    contains
      procedure(FLUSH_UPSTREAM_WDATA), deferred, pass(this) :: flush_upstream_wdata
  end type

  abstract interface
  subroutine FLUSH_UPSTREAM(this)
  import HDF5_Node
    class(HDF5_Node), intent(inout) :: this
  end subroutine
  end interface


  abstract interface
  subroutine FLUSH_DOWNSTREAM(this)
  import HDF5_Node
    class(HDF5_Node), intent(inout) :: this
  end subroutine
  end interface

  abstract interface
  subroutine FLUSH_UPSTREAM_WDATA(this, buf)
  import HDF5_Leaf
    class(HDF5_Leaf), intent(inout) :: this
    real(8) :: buf(..)
  end subroutine
  end interface


  abstract interface
  subroutine ATTACH(this, parent)
  import HDF5_Node
    class(HDF5_Node), intent(inout) :: this
    class(HDF5_Node), pointer :: parent
  end subroutine
  end interface

  public check

  contains
    subroutine node_attach(this, child)
      class(HDF5_Node), intent(inout) :: this
      class(HDF5_Node), pointer :: child

      if(.not. allocated(this%children)) then
        allocate(this%children(0), source=HDF5_Node_Child(NULL()))
      end if
      this%children = [this%children, HDF5_Node_Child(child)]
    end subroutine

    subroutine node_attach_to(this, parent)
      class(HDF5_Node), intent(inout), target :: this
      class(HDF5_Node), pointer :: parent

      class(HDF5_Node), pointer :: ptr

      ptr => this
      this%parent => parent
      call this%parent%attach(ptr)
    end subroutine

    subroutine node_loc_id_open(this, loc_id, ierr)
      class(HDF5_Node), intent(inout) :: this
      integer(hid_t), intent(inout) :: loc_id
      integer, intent(out) :: ierr

      ierr = -1
      loc_id = -1
    end subroutine

    subroutine node_loc_id_close(this, loc_id, ierr)
      class(HDF5_Node), intent(inout) :: this
      integer(hid_t), intent(inout) :: loc_id
      integer, intent(out) :: ierr

      ierr = -1
      loc_id = -1
    end subroutine

    subroutine check(str, ierr)
      character(len=*), intent(in) :: str
      integer :: ierr

      if(ierr .eq. 0) return

      write(*,*) "error:", str
    end subroutine

end module HDF5_AbstractTypes_m
