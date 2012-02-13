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
!   2010-05-13  todling - uniform interface across stp routines
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

 subroutine stpjcpdry(rval,sval,pen,b,c)
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
!   2010-05-13  todling - update to use gsi_bundle
!   2010-05-25  derber  - modify to decrease number of communications
!   2010-08-18  hu      - add qpvals= to mpl_allreduce call
!   2011-11-01  eliu    - add handling for ql & qi increments and search directions 
!
!   input argument list:
!     rq       - q search direction
!     rc       - cloud water search direction
!     rp       - surface pressure search direction                  
!     sq       - q increment
!     sc       - cloud water increment
!     sp       - increment in grid space
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
  use mpimod, only: mype
  use gridmod, only: lat2,lon2,nsig,wgtlats,nlon,istart
  use guess_grids, only:  ges_prsi,ntguessig
  use mpl_allreducemod, only: mpl_allreduce
  use jcmod, only: bamp_jcpdry
  use gsi_bundlemod, only: assignment(=)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod,  only: gsi_metguess_get

  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(in   ) :: rval
  real(r_quad)    ,intent(  out) :: pen,b,c

! Declare local variables
  real(r_quad),dimension(2):: dmass
  real(r_quad) :: rcon,con
  integer(i_kind) i,j,k,it,mm1,ii,ier,icw,iql,iqi,istatus
  real(r_kind),pointer,dimension(:,:,:) :: rq,sq,rc,sc,rql,rqi,sql,sqi
  real(r_kind),pointer,dimension(:,:)   :: rp,sp

  pen=zero_quad ; b=zero_quad ; c=zero_quad
  it=ntguessig

! Retrieve pointers
! Simply return if any pointer not found
  ier=0; icw=0; iql=0; iqi=0     
  call gsi_bundlegetpointer(sval,'q' ,sq, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'cw',sc, istatus);icw=istatus+icw
  call gsi_bundlegetpointer(sval,'ql',sql,istatus);iql=istatus+iql
  call gsi_bundlegetpointer(sval,'qi',sqi,istatus);iqi=istatus+iqi
  call gsi_bundlegetpointer(sval,'ps',sp, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q' ,rq, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'cw',rc, istatus);icw=istatus+icw
  call gsi_bundlegetpointer(rval,'ql',rql,istatus);iql=istatus+iql
  call gsi_bundlegetpointer(rval,'qi',rqi,istatus);iqi=istatus+iqi
  call gsi_bundlegetpointer(rval,'ps',rp, istatus);ier=istatus+ier
  if (mype==0) write(6,*)'stpjcpdry: checking ier+icw*(iql+iql)=', ier+icw*(iql+iql)
  if(ier+icw*(iql+iql)/=0)return

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
           if(icw==0)then
              dmass(1)=dmass(1) - (sq(i,j,k)+sc(i,j,k))*con
              dmass(2)=dmass(2) - (rq(i,j,k)+rc(i,j,k))*con
           else
              dmass(1)=dmass(1) - (sq(i,j,k)+sql(i,j,k)+sqi(i,j,k))*con
              dmass(2)=dmass(2) - (rq(i,j,k)+rql(i,j,k)+rqi(i,j,k))*con
           endif
        end do
     end do
  end do

  call mpl_allreduce(2,qpvals=dmass)

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
