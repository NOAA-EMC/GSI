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
!   2010-05-13  todling - update interface
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

 subroutine intjcpdry(rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intjcpdry   adjoint for mean dry ps conservation
!   prgmmr: kleist           org: np23                date: 2009-07-07
!
! abstract: calculate stepsize contribution and penalty for limiting changes to dry mass
!
! program history log:
!   2009-07-07  kleist
!   2010-05-13  todling - update to use gsi_bundle
!   2010-05-25  derber - modify to minimize number of communications
!   2010-08-18     hu  - added qpvals= to mpl_allreduce call
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
  use kinds, only: r_quad,r_kind,i_kind
  use mpimod, only: mype
  use constants, only: zero,zero_quad,one_quad,two_quad
  use gridmod, only: lat2,lon2,nsig,wgtlats,nlon,istart
  use guess_grids, only: ges_prsi,ntguessig
  use mpl_allreducemod, only: mpl_allreduce
  use jcmod, only: bamp_jcpdry
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

! Declare local variables
  real(r_quad),dimension(1) :: dmass 
  real(r_quad) rcon,con
  integer(i_kind) i,j,k,it,ii,mm1,ier,istatus
  real(r_kind),pointer,dimension(:,:,:) :: sq,sc
  real(r_kind),pointer,dimension(:,:,:) :: rq,rc
  real(r_kind),pointer,dimension(:,:)   :: sp
  real(r_kind),pointer,dimension(:,:)   :: rp
  
  it=ntguessig

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q' ,sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'cw',sc,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'ps',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q' ,rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'cw',rc,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'ps',rp,istatus);ier=istatus+ier
  if(ier/=0)return

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
  call mpl_allreduce(1,qpvals=dmass)

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
