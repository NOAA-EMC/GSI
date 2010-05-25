module intjcpdrymod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intjcpdrymod    module for intlimq and its tangent linear intlimq_tl
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
!
! subroutines included:
!   sub intjcpdry
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
PUBLIC intjcpdry


contains

 subroutine intjcpdry(rq,rc,rp,sq,sc,sp,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intjcpdry   adjoint for mean dry ps conservation
!   prgmmr: kleist           org: np23                date: 2009-07-07
!
! abstract: calculate stepsize contribution and penalty for limiting changes to dry mass
!
! program history log:
!   2009-07-07  kleist
!
!   input argument list:
!     rq       - q search direction
!     rc       - cloud water search direction
!     rp       - surface pressure search direction
!     sq       - q increment
!     sc       - cloud water increment
!     sp       - increment in grid space
!     mype     - integer PE
!
!   output argument list:
!     rq       - q search direction
!     rc       - cloud water search direction
!     rp       - surface pressure search direction
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_prsi,ntguessig
  use jcmod, only: bamp_jcpdry
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: sq,sc
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: rq,rc
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: sp
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: rp
  integer(i_kind)                       ,intent(in   ) :: mype

! Declare local variables
  real(r_kind),dimension(lat2,lon2):: sqint,rqint
  real(r_kind) spave,rpave,sqave,rqave,sdmass,rdmass
  integer(i_kind) i,j,k,it
  
  sqint=zero ; rqint=zero
  it=ntguessig

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           sqint(i,j)=sqint(i,j) + ( (sq(i,j,k)+sc(i,j,k))* &
               (ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) )
        end do
     end do
  end do

! First, use MPI to get global mean increment
  call global_mean(sp,spave,mype)
  call global_mean(sqint,sqave,mype)

! Subtract out water to get incremental dry ps
  sdmass=spave-sqave

  rdmass=bamp_jcpdry*sdmass

  rpave=rdmass
  rqave=-rdmass
  call global_mean_ad(rp,rpave,mype)
  call global_mean_ad(rqint,rqave,mype)

  do k=1,nsig
     do j=2,lon2-1
        do i=2,lat2-1
           rq(i,j,k)=rq(i,j,k) + rqint(i,j)*(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))
           rc(i,j,k)=rc(i,j,k) + rqint(i,j)*(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))
        end do
     end do
  end do

  return
end subroutine intjcpdry

end module intjcpdrymod
