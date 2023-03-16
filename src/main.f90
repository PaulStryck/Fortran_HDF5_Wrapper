program main
  use hdf5, only: H5T_NATIVE_DOUBLE
  use HDF5_AbstractTypes_m, only: HDF5_Node
  use HDF5_File_m, only: HDF5_File
  use HDF5_Group_m, only: HDF5_Group
  use Compress_m, only: Compress
  use SVD_m, only: SVD
  use Buffer_m, only: Buffer, BUF_APPEND_CB, BUF_SIZE_QUERY

  implicit none

  class(HDF5_File),  allocatable, target :: h5_file
  class(HDF5_Group), allocatable, target :: h5_grp
  class(Compress),   allocatable, target :: comp1
  class(Compress),   allocatable, target :: comp2
  class(SVD),        allocatable, target :: svd1

  type(buffer) :: buf1, buf2, buf3

  procedure(BUF_APPEND_CB), pointer :: append_ptr => NULL()
  procedure(BUF_SIZE_QUERY), pointer :: size_q_ptr => NULL()
  class(HDF5_Node), pointer :: ptr => NULL()

  integer :: ier


  allocate(HDF5_File  :: h5_file)
  allocate(HDF5_Group :: h5_grp)
  allocate(Compress   :: comp1)
  allocate(Compress   :: comp2)
  allocate(SVD        :: svd1)


  ! Create pointers to callback functions
  append_ptr => append
  size_q_ptr => size_q


  call h5_file%create("./", "foo.h5")

  ptr => h5_file
  h5_grp%nm = "G1"
  call h5_grp%attach_to(ptr)

  !comp 1 + provider
  ptr => h5_grp
  comp1%nm = "Comp1"
  comp1%settings = "H5Z_compression"
  comp1%type_id = H5T_NATIVE_DOUBLE
  call comp1%attach_to(ptr)

  ! init and add buf 1
  ptr => comp1
  call buf1%init(2, [character(len=500) :: "foo", "bar"], &
    append_ptr, size_q_ptr, ier)
  call buf1%attach_to(ptr)


  ! comp 2 + provider
  ptr => h5_grp
  comp2%nm = "Comp2"
  comp2%settings = "H5Z_compression"
  comp2%type_id = H5T_NATIVE_DOUBLE
  call comp2%attach_to(ptr)

  ! init and add buf 2
  ptr => comp2
  call buf2%init(5, [character(len=500) :: "foo", "bar"], &
    append_ptr, size_q_ptr, ier)
  call buf2%attach_to(ptr)


  ! ptr => h5_file
  ! svd1%nm = "SVD Group"
  ! call svd1%attach_to(ptr)

  ! ! init and add buf 2
  ! ptr => comp2
  ! call buf3%init(5, "test", [character(len=500) :: "foo", "bar"], &
  !   append_ptr, size_q_ptr, ier)
  ! call buf3%attach_to(ptr)

  nullify(ptr)

  call h5_file%check_tree()
  call h5_file%flush_downstream() ! initial flush to create file and all links


  call buf1%append()
  call buf1%append()
  call buf1%append()
  call buf1%append()
  call buf1%append()
  ! call buf1%append()
  ! call buf1%append()
  ! call buf1%append()
  ! call buf1%append()
  ! call buf1%append()
  ! call buf1%append()
  ! call buf1%append()



  contains
    subroutine append(buf, ptr_names)
      real(8), intent(inout) :: buf(:)
      character(len=500) :: ptr_names(:)
      integer :: i = 0

      i = i+1
      buf(:) = real(i, 8)

    end subroutine

    subroutine size_q(m, ptr_names)
      integer, intent(out) :: m
      character(len=500) :: ptr_names(:)

      m = 2

    end subroutine
end program main
