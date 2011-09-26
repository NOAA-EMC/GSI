module intlimvmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlimvmod    module for intlimv 
!   prgmmr:
!
! abstract: module for intlimv 
!
! program history log:
!
! subroutines included:
!   sub intlimv
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
PUBLIC intlimv


contains

subroutine intlimv(rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimv
!   prgmmr: zhu           org: np23                date: 2011-02-20
!
! abstract: limit negative vis as a weak constraint
!
! program history log:
!   2011-02-20  zhu
!
!   input argument list:
!     sv       - increment in grid space
!
!   output argument list:
!     rv       - results from limiting operator                 
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
  use jfunc, only: factv,vgues
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,k,ier,istatus
  real(r_kind) vis
  real(r_kind),pointer,dimension(:,:) :: sv
  real(r_kind),pointer,dimension(:,:) :: rv

  if (factv==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vis',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vis',rv,istatus);ier=istatus+ier
  if(ier/=0)return
 
  do j = 2,lon1+1
     do i = 2,lat1+1
        vis = vgues(i,j) + sv(i,j)
           
!       Lower constraint limit
        if (vis < zero) then
           rv(i,j) = rv(i,j) + factv*vis/(vgues(i,j)*vgues(i,j))
        end if
     end do
  end do
  
  return
end subroutine intlimv

end module intlimvmod
