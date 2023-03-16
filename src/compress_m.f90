module Compress_m
  use hdf5
  use HDF5_AbstractTypes_m, only: HDF5_Node, check
  use HDF5_Dataset_m, only: HDF5_Dataset

  implicit none
  private

  type, extends(HDF5_Dataset), public :: Compress
    character(len=:), allocatable :: settings
    integer(hsize_t), allocatable :: chunks(:)
    integer(hsize_t), allocatable :: max_dims(:)

    contains
      procedure :: flush_upstream
      procedure :: flush_upstream_wdata

  end type Compress
  contains
    subroutine flush_upstream(this)
      class(Compress), intent(inout) :: this

      write(*,*) "error"
    end subroutine

    subroutine flush_upstream_wdata(this, buf)
      class(Compress), intent(inout) :: this
      real(8) :: buf(..)

      integer(hid_t) :: loc_id, type_id, s_id, ms_id, ds_id, cp_id
      integer :: ierr
      logical :: exists = .false.

      integer(hsize_t), allocatable :: buf_dims(:)
      integer(hsize_t), allocatable :: qdims(:), qmdims(:), offset(:)
      integer :: buf_rank, qrank

      integer :: i

      buf_rank = rank(buf)
      buf_dims = shape(buf)
      this%max_dims = [buf_dims(1:buf_rank-1), H5S_UNLIMITED_F]
      this%chunks   = [int(buf_dims(1:buf_rank-1), 4), 100]

      type_id = H5T_NATIVE_REAL
      call this%parent%loc_id_open(loc_id, ierr)

      call h5lexists_f(loc_id, this%nm, exists, ierr)
      call check("h5lexists_f", ierr)

      if(exists) then
        ! open existing
        call h5dopen_f(loc_id, this%nm, ds_id, ierr)
        call check("h5dopen_f", ierr)

        call h5dget_space_f(ds_id, s_id, ierr)
        call check("h5dget_space_f", ierr)
      else
        ! create new
        ! Create properties
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, cp_id, ierr)
        CALL check("h5pcreate_f", ierr)

        ! enable chunking
        CALL h5pset_chunk_f(cp_id, 2, this%chunks, ierr)
        CALL check("h5pset_chunk_f", ierr)

        call h5screate_simple_f(buf_rank, buf_dims, s_id, ierr, this%max_dims)
        call check("h5screate_simple_f", ierr)

        call h5dcreate_f(loc_id, this%nm, type_id, s_id, ds_id, &
          ierr, dcpl_id=cp_id)
        call check("h5dcreate_f", ierr)
      end if

      call h5sget_simple_extent_ndims_f(s_id, qrank, ierr)
      call check("h5sget_simple_extent_ndims_f", ierr)

      allocate(qdims(qrank), qmdims(qrank), source=int(0, hsize_t))
      allocate(offset(qrank), source=int(0, hsize_t))
      call h5sget_simple_extent_dims_f(s_id, qdims, qmdims, ierr)
      !call check("h5sget_simple_extent_dims", ierr)

      if(qrank .ne. buf_rank) write(*,*) "error"
      do i=1, qrank-1
        if(qdims(i) .ne. buf_dims(i)) write(*,*) "error"
      end do
      offset(qrank) = qdims(qrank)
      qdims(qrank) = qdims(qrank) + buf_dims(qrank)
      call h5dset_extent_f(ds_id, qdims, ierr)
      call check("h5dset_extent_f", ierr)

      call h5dget_space_f(ds_id, s_id, ierr)
      call check("h5dget_space_f", ierr)

      call h5screate_simple_f (qrank, buf_dims, ms_id, ierr)
      call check("h5screate_simple_f", ierr)

      CALL h5sselect_hyperslab_f(s_id, H5S_SELECT_SET_F, &
           offset, buf_dims, ierr)
      CALL check("h5sselect_hyperslab_f", ierr)

      select rank(buf)
        rank(0)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(1)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(2)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(3)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(4)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(5)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(6)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
        rank(7)
          CALL H5dwrite_f(ds_id, H5T_NATIVE_DOUBLE, buf, buf_dims, ierr, ms_id, s_id)
      end select
      CALL check("h5dwrite_f", ierr)

      deallocate(qdims)
      deallocate(qmdims)
      deallocate(offset)


      call h5sclose_f(ms_id, ierr); call check("h5sclose_f", ierr)
      call h5sclose_f(s_id, ierr); call check("h5sclose_f", ierr)
      call h5dclose_f(ds_id, ierr); call check("h5dclose_f", ierr)

      call this%parent%loc_id_close(loc_id, ierr)
    end subroutine

end module Compress_m


! module compress
!   use store
!   use hdf5
!   use HDF5_Dataset, only: h5_Dataset

!   implicit none
!   private

!   type, extends(Storage), public :: Compressc
!     type(h5_Dataset), allocatable :: datasets(:)

!     logical, private :: initialized = .false.

!     contains
!       procedure :: flush_data
!       procedure :: announce_data
!   end type Compressc

!   contains
!     subroutine flush_data(this, buf, nm)
!       class(Compressc), intent(inout) :: this
!       real(8) :: buf(:,:)
!       character(len=*), intent(in) :: nm

!       write(*,*) "flush"

!     end subroutine


!     subroutine announce_data(this, nm, dims)
!       class(Compressc), intent(inout)   :: this
!       character(len=*), intent(in) :: nm
!       integer, intent(in) :: dims(:)
!     end subroutine

!     subroutine compress_init_file(this)
!       class(Compressc), intent(in) :: this

!       integer(hid_t) :: f_id, cp_id
!       integer :: ier

!       call h5open_f(ier); call check("h5open_f", ier)

!       call h5fcreate_f(this%filename, H5F_ACC_TRUNC_F, f_id, ier); call check("h5fcreate_f", ier)

!       call h5pcreate_f(H5P_DATASET_CREATE_F, cp_id, ier); call check("h5pcreate_f", ier)

!       call h5pclose_f(cp_id, ier); call check("h5pclose_f", ier)

!       call h5fclose_f(f_id, ier); call check("h5fclose_f", ier)
!     end subroutine


!     subroutine check(str, ier)
!       character(len=*), intent(in) :: str
!       integer, intent(in) :: ier

!       if(ier/= 0) then
!         write(*,*) "error: ", str
!       end if
!     end subroutine

! end module compress
