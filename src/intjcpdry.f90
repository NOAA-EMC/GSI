module intjcpdrymod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intjcpdrymod    module for intjcpdry
!   prgmmr:
!
! abstract: module for intjcpdry
!
! program history log:
!   2009-07-07  kleist
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
!   2010-05-25  derber - modify to minimize number of communications
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
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero_quad,one_quad,two_quad
  use gridmod, only: lat2,lon2,nsig,wgtlats,nlon,istart
  use guess_grids, only: ges_prsi,ntguessig
  use mpl_allreducemod, only: mpl_allreduce
  use jcmod, only: bamp_jcpdry
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: sq,sc
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: rq,rc
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: sp
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: rp
  integer(i_kind)                       ,intent(in   ) :: mype

! Declare local variables
  real(r_quad),dimension(1) :: dmass
  real(r_quad) rcon,con
  integer(i_kind) i,j,k,it,ii,mm1
  
  it=ntguessig
  dmass(1)=zero_quad
  rcon=one_quad/(two_quad*float(nlon))
  mm1=mype+1

! Calculate mean surface pressure contribution in subdomain
  do j=2,lon2-1
    do i=2,lat2-1
      ii=istart(mm1)+i-2
      dmass(1)=dmass(1)+sp(i,j)*wgtlats(ii)
    end do
  end do
! Remove water to get incremental dry ps
  do k=1,nsig
     do j=2,lon2-1
        do i=2,lat2-1
           ii=istart(mm1)+i-2
           con = (ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))*wgtlats(ii)
           dmass(1)=dmass(1) - (sq(i,j,k)+sc(i,j,k))* con
        end do
     end do
  end do

! First, use MPI to get global mean increment
  call mpl_allreduce(1,dmass)

  dmass(1)=bamp_jcpdry*dmass(1)*rcon*rcon

! Calculate mean surface pressure contribution in subdomain
  do j=2,lon2-1
    do i=2,lat2-1
      ii=istart(mm1)+i-2
      rp(i,j)=rp(i,j)+dmass(1)*wgtlats(ii)
    end do
  end do
! Remove water to get incremental dry ps
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ii=istart(mm1)+i-2
           con = dmass(1)*(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it))*wgtlats(ii)
           rq(i,j,k)=rq(i,j,k) - con
           rc(i,j,k)=rc(i,j,k) - con
        end do
     end do
  end do

  return
end subroutine intjcpdry

end module intjcpdrymod
