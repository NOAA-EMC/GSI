 subroutine stpjcpdry(rq,rc,rp,sq,sc,sp,mype,pen,b,c)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjcpdry   penalty and stp size for mean dry ps conservation
!   prgmmr: kleist           org: np23                date: 2009-07-07
!
! abstract: calculate stepsize contribution and penalty for limiting changes to global
!           mean dry ps increment
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
!     pen      - current penalty for mean dry pressure constraint
!     b        - contribution to numerator
!     c        - contribution to denomenator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero,two,one,half,grav,zero_quad
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use guess_grids, only:  ges_prsi,ntguessig
  use jcmod, only: bamp_jcpdry
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: rq,sq,rc,sc
  real(r_kind),dimension(lat2,lon2),intent(in):: rp,sp
  real(r_quad),intent(out):: pen,b,c
  integer(i_kind),intent(in):: mype

! Declare local variables
  real(r_kind),dimension(lat2,lon2):: sqint,rqint
  real(r_kind) spave,rpave,sqave,rqave,sdmass,rdmass
  integer(i_kind) i,j,k,it

  pen=zero_quad ; b=zero_quad ; c=zero_quad
  sqint=zero ; rqint=zero
  it=ntguessig

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        sqint(i,j)=sqint(i,j) + ( (sq(i,j,k)+sc(i,j,k))* &
             (ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) )
        rqint(i,j)=rqint(i,j) + ( (rq(i,j,k)+rc(i,j,k))* &
             (ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) ) 
      end do
    end do
  end do

! First, use MPI to get global mean increment
  call global_mean(sp,spave,mype)
  call global_mean(rp,rpave,mype)
  call global_mean(sqint,sqave,mype)
  call global_mean(rqint,rqave,mype)

! Subtract out water to get incremental dry mass
  sdmass=spave-sqave
  rdmass=rpave-rqave

! Now penalize non-zero global mean dry ps increment
! Notice there will only be a contribution from PE=0

  if (mype==0) then
    pen = bamp_jcpdry*sdmass*sdmass
    b  = -bamp_jcpdry*rdmass*sdmass
    c  = bamp_jcpdry*rdmass*rdmass
  end if

  return
end subroutine stpjcpdry
