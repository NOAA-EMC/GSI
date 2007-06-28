module intlimqmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimqmod    module for intlimq and its tangent linear intlimq_tl
!
! abstract: module for intlimq and its tangent linear intlimq_tl
!
! program history log:
!   2005-05-11  Yanqiu zhu - wrap intlimq and its tangent linear intlimq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2005-11-22  Wu - return if factq's = zero
!

implicit none

PRIVATE
PUBLIC intlimq,intlimq_tl


contains

subroutine intlimq(rq,sq)
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
  use jfunc, only: factqmin,factqmax,qsatg,qgues,qsinv2
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sq
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: rq

! Declare local variables
  integer(i_kind) i,j,k
! real(r_kind) penmin,penmax
  real(r_kind) term,q

  if (factqmin==zero .and. factqmax==zero) return
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
           q = qgues(i,j,k) + sq(i,j,k)
           
!          Lower constraint limit
           if (q<zero) then
              term=factqmin*q*qsinv2(i,j,k)
              rq(i,j,k) = rq(i,j,k) + term
!             penmin=penmin+term*q

!          Upper constraint limit
           else if (q>qsatg(i,j,k)) then
              term=factqmax*(q-qsatg(i,j,k))*qsinv2(i,j,k)
              rq(i,j,k) = rq(i,j,k) + term
!             penmax=penmax+term*q
           endif
           
        end do
     end do
  end do
  
  return
end subroutine intlimq


subroutine intlimq_tl(rq,sq,rq_tl,sq_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimq_tl
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-03-17
!
! abstract: the tangent linear of the operator that limits negative q as a weak constraint
!
! program history log:
!   2005-03-17  yanqiu zhu - tangent linear of intlimq
!
!   input argument list:
!     sq       - increment in grid space
!     sq_tl     - tangent linear increment in grid space
!
!   output argument list:
!     rq       - results from limiting operator                 
!     rq_tl     - tangent linear results from limiting operator                 
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
  use jfunc, only: factqmin,factqmax,qsatg,qgues,qsinv2
  use jfunc_tl, only: qgues_tl
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sq
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sq_tl
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: rq
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: rq_tl

! Declare local variables
  integer(i_kind) i,j,k
! real(r_kind) penmin,penmax
  real(r_kind) term,q
  real(r_kind) term_tl,q_tl

  if (factqmin==zero .and. factqmax==zero) return
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
           q = qgues(i,j,k) + sq(i,j,k)
           q_tl = qgues_tl(i,j,k) + sq_tl(i,j,k)
           
!          Lower constraint limit
           if (q<zero) then
              term=factqmin*q*qsinv2(i,j,k)
              rq(i,j,k) = rq(i,j,k) + term
              term_tl=factqmin*q_tl*qsinv2(i,j,k)
              rq_tl(i,j,k) = rq_tl(i,j,k) + term_tl
!             penmin=penmin+term*q

!          Upper constraint limit
           else if (q>qsatg(i,j,k)) then
              term=factqmax*(q-qsatg(i,j,k))*qsinv2(i,j,k)
              rq(i,j,k) = rq(i,j,k) + term
              term_tl=factqmax*q_tl*qsinv2(i,j,k)
              rq_tl(i,j,k) = rq_tl(i,j,k) + term_tl
!             penmax=penmax+term*q
           endif
           
        end do
     end do
  end do
  
  return
end subroutine intlimq_tl

end module intlimqmod
