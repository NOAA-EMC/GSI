module stpjcpdrymod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpjcpdrymod    module for stpps and its tangent linear stpps_tl
!  prgmmr:
!
! abstract: module for stpps and its tangent linear stpps_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpps and its tangent linear stpps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpps_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stpjcpdry
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpjcpdry

contains

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
  use kinds, only: r_quad,r_kind,i_kind
  use constants, only: izero,ione,zero,zero_quad
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only:  ges_prsi,ntguessig
  use jcmod, only: bamp_jcpdry
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: rq,sq,rc,sc
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: rp,sp
  real(r_quad)                          ,intent(  out) :: pen,b,c
  integer(i_kind)                       ,intent(in   ) :: mype

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
                (ges_prsi(i,j,k,it)-ges_prsi(i,j,k+ione,it)) )
           rqint(i,j)=rqint(i,j) + ( (rq(i,j,k)+rc(i,j,k))* &
                (ges_prsi(i,j,k,it)-ges_prsi(i,j,k+ione,it)) ) 
        end do
     end do
  end do

! First, use MPI to get global mean increment
  call global_mean(sp,spave,mype)
  call global_mean(rp,rpave,mype)
  call global_mean(sqint,sqave,mype)
  call global_mean(rqint,rqave,mype)

  if (mype==izero) then
!    Subtract out water to get incremental dry mass
     sdmass=spave-sqave
     rdmass=rpave-rqave

!    Now penalize non-zero global mean dry ps increment
!    Notice there will only be a contribution from PE=0

     pen = bamp_jcpdry*sdmass*sdmass
     b  = -bamp_jcpdry*rdmass*sdmass
     c  = bamp_jcpdry*rdmass*rdmass
  end if

  return
end subroutine stpjcpdry

end module stpjcpdrymod
