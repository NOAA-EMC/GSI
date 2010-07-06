module stpjcpdrymod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpjcpdrymod    module for stpjcpdry
!  prgmmr:
!
! abstract: module for stpjcpdry
!
! program history log:
!   2009-07-07  kleist
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
!   2010-05-25  derber - modify to decrease number of communications
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
  use constants, only: zero,zero_quad,one_quad,two_quad
  use gridmod, only: lat2,lon2,nsig,wgtlats,nlon,istart
  use guess_grids, only:  ges_prsi,ntguessig
  use mpl_allreducemod, only: mpl_allreduce
  use jcmod, only: bamp_jcpdry
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: rq,sq,rc,sc
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: rp,sp
  real(r_quad)                          ,intent(  out) :: pen,b,c
  integer(i_kind)                       ,intent(in   ) :: mype

! Declare local variables
  real(r_quad),dimension(2):: dmass
  real(r_quad) :: rcon,con
  integer(i_kind) i,j,k,it,mm1,ii

  pen=zero_quad ; b=zero_quad ; c=zero_quad
  it=ntguessig

  dmass=zero_quad
  rcon=one_quad/(two_quad*float(nlon))
  mm1=mype+1

! Calculate mean surface pressure contribution in subdomain
  do j=2,lon2-1
    do i=2,lat2-1
      ii=istart(mm1)+i-2
      con=wgtlats(ii)*rcon
      dmass(1)=dmass(1)+sp(i,j)*con
      dmass(2)=dmass(2)+rp(i,j)*con
    end do
  end do
! Remove water to get incremental dry ps
  do k=1,nsig
     do j=2,lon2-1
        do i=2,lat2-1
           ii=istart(mm1)+i-2
           con=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))*wgtlats(ii)*rcon
           dmass(1)=dmass(1) - (sq(i,j,k)+sc(i,j,k))*con
           dmass(2)=dmass(2) - (rq(i,j,k)+rc(i,j,k))*con
        end do
     end do
  end do

  call mpl_allreduce(2,dmass)

  if (mype==0) then

!    Now penalize non-zero global mean dry ps increment
!    Notice there will only be a contribution from PE=0

     pen = bamp_jcpdry*dmass(1)*dmass(1)
     b  = -bamp_jcpdry*dmass(2)*dmass(1)
     c  =  bamp_jcpdry*dmass(2)*dmass(2)
  end if

  return
end subroutine stpjcpdry

end module stpjcpdrymod
