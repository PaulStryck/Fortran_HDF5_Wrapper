module Buffer_m
  use hdf5, only: hsize_t
  use HDF5_AbstractTypes_m, only: HDF5_Node, HDF5_Leaf
  implicit none
  private

  type, extends(HDF5_Node), public :: Buffer
    class(HDF5_Leaf), pointer :: parent_recv => NULL()

    integer(hsize_t), allocatable :: dims(:)
    integer :: t_max
    integer, private :: m ! size of buffer
    integer, private :: t ! current position in buffer

    ! field names in buffer
    character(len=500), allocatable, private :: ptr_names(:)

    ! actual data
    real(8), allocatable, private :: dat(:,:)

    ! helper function to generate data vector from field names
    procedure(BUF_APPEND_CB), nopass, pointer :: append_cb => NULL()

    contains
      procedure :: flush_upstream
      procedure :: flush_downstream
      procedure :: init   => buffer_init
      procedure :: append => buffer_append
      procedure :: attach_to => node_attach_to
  end type Buffer

  abstract interface
    subroutine BUF_APPEND_CB(buf, ptr_names)
      real(8), intent(inout) :: buf(:)
      character(len=500) :: ptr_names(:)
    end subroutine
  end interface

  abstract interface
    subroutine BUF_SIZE_QUERY(m, ptr_names)
      integer, intent(out) :: m
      character(len=500) :: ptr_names(:)
    end subroutine
  end interface

  public BUF_APPEND_CB, BUF_SIZE_QUERY

  contains
    subroutine node_attach_to(this, parent)
      class(Buffer), intent(inout), target :: this
      class(HDF5_Node), pointer :: parent

      class(HDF5_Node), pointer :: ptr

      SELECT TYPE(parent)
      class is ( HDF5_Leaf )
        this%parent_recv => parent
      end select

      ptr => this
      this%parent => parent
      call this%parent%attach(ptr)

    end subroutine
    subroutine flush_upstream(this)
      class(Buffer), intent(inout) :: this

      ! buffer is an ultimate leaf, start going downstream

      call this%parent_recv%flush_upstream_wdata(this%dat(1:this%m, 1:this%t))
      this%t = 0

      ! not strictly necessary, but done for safety reasons
      this%dat(:,:) = 0.0d0
    end subroutine

    subroutine flush_downstream(this)
      class(Buffer), intent(inout) :: this

      write(*,*) "this is a leaf, going down"
      call this%flush_upstream()
    end subroutine


    subroutine buffer_init(this, &
        t_max, ptr_names, append_cb, size_query, &
        info)
      class(Buffer), intent(inout)        :: this

      integer, intent(in)                 :: t_max
      character(len=500), intent(in)      :: ptr_names(:)

      procedure(BUF_APPEND_CB),  pointer, intent(in) :: append_cb
      procedure(BUF_SIZE_QUERY), pointer, intent(in) :: size_query

      integer, intent(out) :: info

      integer :: ierr

      info = 0

      this%t_max = t_max

      this%ptr_names = ptr_names
      call size_query(this%m, this%ptr_names)
      if(this%t_max .le. 0) info = -1
      if(size(this%ptr_names) .le. 0) info = -2
      if(this%m .le. 0) info = -3
      allocate(this%dat(this%m,this%t_max), source=0.0d0, stat=ierr)
      if(ierr /= 0) info = -4

      this%append_cb => append_cb

      this%t = 0
      this%dims = [this%m]
    end subroutine

    subroutine buffer_append(this)
      class(Buffer), intent(inout) :: this

      ! increment to point to next index with defined overflow
      !   after t_max
      this%t = MOD(this%t, this%t_max) + 1

      call this%append_cb(this%dat(:, this%t), this%ptr_names)

      ! check if buffer full
      if(this%t == this%t_max) call this%flush_upstream()
    end subroutine

end module Buffer_m
