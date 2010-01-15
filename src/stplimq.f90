module stplimqmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stplimqmod    module for stplimq and its tangent linear stplimq_tl
!  pgrmmr:
!
! abstract: module for stplimq and its tangent linear stplimq_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stplimq and its tangent linear stplimq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2005-11-22  Wu - return in factq's are zero
!   2008-12-02  Todling - remove stpqlimq_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stplimq
!   sub stplimq_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stplimq

contains
 
subroutine stplimq(rq,sq,sges,outmin,outmax,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimq     calculate penalty and stepsize for limit of q 
!   prgmmr: todling           org: np23                date: 2007-12-02
!
! abstract: interface to the actual stplimq
!
! program history log:
!   2008-12-02  todling - adapted interface for state vector convenience
!   2009-08-12  lueken  - update documentation
!
!   input argument list:
!    sges
!    rq,sq
!
!   output argument list:
!    outmin,outmax
!
! attributes:
!   language: f90
!   machine:
!
!$$$
  use kinds, only: r_kind,r_quad,i_kind
  use constants, only: ione
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nstep
  real(r_kind),dimension(max(ione,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(ione,nstep)),intent(  out) :: outmin,outmax
  real(r_kind),dimension(lat2*lon2*nsig) ,intent(in   ) :: rq,sq

  call stplimq_(rq,sq,sges,outmin,outmax,nstep)

end subroutine stplimq
 
subroutine stplimq_(rq,sq,sges,outmin,outmax,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimq     calculate penalty and stepsize for limit of q 
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   1996-11-19  derber
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-03-15  kleist, d., derber, j., treadon, r., use negative q only
!   2004-06-02  kleist, add penalty for excess moisture
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - modify for openMP
!   2006-09-18  derber - modify output b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-08-14  derber  - optimize
!
!   input argument list:
!     rq       - search direction                               
!     sq       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     outmin(1:nstep)  - current penalty for negative q sges(1:nstep)
!     outmax(1:nstep)  - current penalty for excess q sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: izero,ione,zero,two,one,half,zero_quad
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factqmin,factqmax,qgues,qsatg
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: nstep
  real(r_kind),dimension(max(ione,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(ione,nstep)),intent(  out) :: outmin,outmax
  real(r_kind),dimension(lat2,lon2,nsig) ,intent(in   ) :: rq,sq

! Declare local variables
  integer(i_kind) i,j,k,kk
  real(r_kind) q
  real(r_kind),dimension(nstep):: qx
  
  outmin=zero_quad; outmax=zero_quad

  if (factqmin==zero .and. factqmax==zero) return

! Loop over interior of subdomain          
  if(nstep > izero)then
     do k = 1,nsig
        do j = 2,lon1+ione
           do i = 2,lat1+ione

!             Values for q using stepsizes
              q  = qgues(i,j,k) + sq(i,j,k)
              do kk=1,nstep
                 qx(kk) = q + sges(kk)*rq(i,j,k)
                 if(qx(kk) < zero)then
                    outmin(kk)=outmin(kk)+factqmin*qx(kk)*qx(kk)/(qsatg(i,j,k)*qsatg(i,j,k))
                 else
                    if(qx(kk) > qsatg(i,j,k))then
                       outmax(kk)=outmax(kk)+factqmax*(qx(kk)-qsatg(i,j,k))*(qx(kk)-qsatg(i,j,k))/(qsatg(i,j,k)*qsatg(i,j,k))
                    end if
                 end if
              end do
           end do
        end do
     end do
  else
     do k = 1,nsig
        do j = 2,lon1+ione
           do i = 2,lat1+ione

!             Values for q using stepsizes
              q  = qgues(i,j,k) 
              if(q < zero)then
                 outmin(1)=outmin(1)+factqmin*q*q/(qsatg(i,j,k)*qsatg(i,j,k))
              else
                 if(q > qsatg(i,j,k))then
                    outmax(1)=outmax(1)+factqmax*(q-qsatg(i,j,k))*(q-qsatg(i,j,k))/(qsatg(i,j,k)*qsatg(i,j,k))
                 end if
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     outmin(kk)=outmin(kk)-outmin(1)
     outmax(kk)=outmax(kk)-outmax(1)
  end do
  return
end subroutine stplimq_

end module stplimqmod
