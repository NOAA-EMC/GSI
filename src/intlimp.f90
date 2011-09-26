module intlimpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlimpmod    module for intlimp 
!   prgmmr:
!
! abstract: module for intlimp 
!
! program history log:
!
! subroutines included:
!   sub intlimp
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
PUBLIC intlimp


contains

subroutine intlimp(rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimp
!   prgmmr: zhu           org: np23                date: 2011-02-20
!
! abstract: limit negative pblh as a weak constraint
!
! program history log:
!   2011-02-20  zhu
!
!   input argument list:
!     sp       - increment in grid space
!
!   output argument list:
!     rp       - results from limiting operator                 
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
  use jfunc, only: factp,pgues
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,k,ier,istatus
  real(r_kind) pblh
  real(r_kind),pointer,dimension(:,:) :: sp
  real(r_kind),pointer,dimension(:,:) :: rp

  if (factp==zero) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'pblh',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'pblh',rp,istatus);ier=istatus+ier
  if(ier/=0)return
 
  do j = 2,lon1+1
     do i = 2,lat1+1
        pblh = pgues(i,j) + sp(i,j)
           
!       Lower constraint limit
        if (pblh < zero) then
           rp(i,j) = rp(i,j) + factp*pblh/(pgues(i,j)*pgues(i,j))
        end if
     end do
  end do
  
  return
end subroutine intlimp

end module intlimpmod
