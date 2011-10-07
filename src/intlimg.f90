module intlimgmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlimgmod    module for intlimg 
!   prgmmr:
!
! abstract: module for intlimg 
!
! program history log:
!
! subroutines included:
!   sub intlimg
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intlimg


contains

subroutine intlimg(rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimg
!   prgmmr: zhu           org: np23                date: 2011-02-20
!
! abstract: limit negative gust as a weak constraint
!
! program history log:
!   2011-02-20  zhu
!
!   input argument list:
!     sg       - increment in grid space
!
!   output argument list:
!     rg       - results from limiting operator                 
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nsig,lat1,lon1
  use jfunc, only: factg,ggues
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,k,ier,istatus
  real(r_kind) gust
  real(r_kind),pointer,dimension(:,:) :: sg
  real(r_kind),pointer,dimension(:,:) :: rg

  if (factg==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'gust',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'gust',rg,istatus);ier=istatus+ier
  if(ier/=0)return
 
  do j = 2,lon1+1
     do i = 2,lat1+1
        gust = ggues(i,j) + sg(i,j)
           
!       Lower constraint limit
        if (gust < zero) then
           rg(i,j) = rg(i,j) + factg*gust/(ggues(i,j)*ggues(i,j))
        end if
     end do
  end do
  
  return
end subroutine intlimg

end module intlimgmod
