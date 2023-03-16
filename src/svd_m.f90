module Svd_m
  use HDF5_Group_m, only: HDF5_Group

  implicit none
  private

  type, extends(HDF5_Group), public :: SVD

  end type
  ! use store
  ! implicit none
  ! private

  ! type, extends(Storage), public :: SVDc
  !   contains
  !     procedure :: flush_data
  !     procedure :: announce_data
  ! end type SVDc

  ! contains
  !   subroutine flush_data(this, buf, nm)
  !     class(SVDc), intent(inout) :: this
  !     real(8) :: buf(:,:)
  !     character(len=*), intent(in) :: nm
  !   end subroutine

  !   subroutine announce_data(this, nm)
  !     class(SVDc), intent(inout)   :: this
  !     character(len=*), intent(in) :: nm
  !   end subroutine
end module Svd_m
