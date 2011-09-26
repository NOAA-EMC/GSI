module stplimvmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stplimvmod    module for stplimv
!  pgrmmr:
!
! abstract: module for stplimv 
!
! program history log:
!   2011-02-23  zhu
!
! subroutines included:
!   sub stplimv
!   sub stplimv_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stplimv

contains
 
subroutine stplimv(rval,sval,sges,out,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimv     calculate penalty and stepsize for limit of q 
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   2011-02-23  zhu
!
!   input argument list:
!     rg       - search direction                               
!     sg       - increment in grid space
!     sges     - step size estimates (4)
!     nstep    - number of step size estimates if == 0 then just do outer loop
!
!   output argument list:
!     out(1:nstep)  - current penalty for negative vis sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero,two,one,half,zero_quad
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factv,vgues
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(  out) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) i,j,k,kk,ier,istatus
  real(r_kind) vis,vx
  real(r_kind),pointer,dimension(:,:) :: rg,sg
  
  out=zero_quad

  if (factv==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vis',sg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vis',rg,istatus);ier=istatus+ier
  if(ier/=0)return

! Loop over interior of subdomain          
  if(nstep > 0)then
     do j = 2,lon1+1
        do i = 2,lat1+1

!          Values for vis using stepsizes
           vis  = vgues(i,j) + sg(i,j)
           do kk=1,nstep
              vx = vis + sges(kk)*rg(i,j)
              if(vx < zero)then
                 out(kk)=out(kk)+factv*vx*vx/(vgues(i,j)*vgues(i,j))
              end if
           end do
        end do
     end do
  end if

  do kk=2,nstep
     out(kk)=out(kk)-out(1)
  end do
  return
end subroutine stplimv

end module stplimvmod
