module intlimqmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlimqmod    module for intlimq and its tangent linear intlimq_tl
!   prgmmr:
!
! abstract: module for intlimq and its tangent linear intlimq_tl
!
! program history log:
!   2005-05-11  Yanqiu zhu - wrap intlimq and its tangent linear intlimq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2005-11-22  Wu - return if factq's = zero
!   2008-11-26  Todling - remove intlimq_tl
!   2009-08-13  lueken - update documentation
!   2010-05-13  todling - change interface
!
! subroutines included:
!   sub intlimq
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
PUBLIC intlimq


contains

subroutine intlimq(rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimq
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: limit negative q as a weak constraint
!
! program history log:
!   1996-11-19  derber
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-03-15  kleist, d., derber, j., treadon, r., use negative q only
!   2004-06-02  kleist, add penalty for excess moisture
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2007-02-13  derber - modify to use rh rather than q
!   2008-06-02  safford - rm unused vars
!   2010-05-13  todling - update to use gsi_bundle
!
!   input argument list:
!     sq       - increment in grid space
!
!   output argument list:
!     rq       - results from limiting operator                 
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
  use jfunc, only: factqmin,factqmax,qgues,qsatg
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,k,ier,istatus
  real(r_kind) q
  real(r_kind),pointer,dimension(:,:,:) :: sq
  real(r_kind),pointer,dimension(:,:,:) :: rq

  if (factqmin==zero .and. factqmax==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q',sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',rq,istatus);ier=istatus+ier
  if(ier/=0)return
 
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
           q = qgues(i,j,k) + sq(i,j,k)
           
!          Lower constraint limit
           if (q < zero) then
              rq(i,j,k) = rq(i,j,k) + factqmin*q/(qsatg(i,j,k)*qsatg(i,j,k))

!          Upper constraint limit
           else if (q > qsatg(i,j,k)) then
              rq(i,j,k) = rq(i,j,k) + factqmax*(q-qsatg(i,j,k))/(qsatg(i,j,k)*qsatg(i,j,k))
           
           end if
        end do
     end do
  end do
  
  return
end subroutine intlimq

end module intlimqmod
