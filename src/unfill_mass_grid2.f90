subroutine unfill_mass_grid2t(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2t        opposite of fill_mass_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2t. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  The result is added to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use gridmod, only: itotsub,iglobal,ltosi,ltosj

  implicit none

  integer(i_kind), intent(in)  :: nx,ny
  real(r_single), intent(in)   :: gout(itotsub)
  real(r_single), intent(inout):: gin(nx,ny)
  
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

  do i=1,iglobal
     b(ltosj(i),ltosi(i))=gout(i)
  end do

! Mass grids--just copy
  do j=1,ny
     do i=1,nx
        gin(i,j)=b(i,j)+gin(i,j)
     end do
  end do
  
end subroutine unfill_mass_grid2t

subroutine unfill_mass_grid2u(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2u       opposite of fill_mass_grid2u
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2u. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  This routine interpolates in the x direction to the
!           C grid u points and adds result to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: half
  use gridmod, only: itotsub,iglobal,ltosi,ltosj

  implicit none

  integer(i_kind), intent(in)  :: nx,ny
  real(r_single), intent(in)   :: gout(itotsub)
  real(r_single), intent(inout):: gin(nx+1,ny)
  
  real(r_single) b(nx,ny)
  integer(i_kind) i,i0,im,j

  do i=1,iglobal
     b(ltosj(i),ltosi(i))=gout(i)
  end do

  do j=1,ny
     do i=1,nx+1
        im=max(1,i-1)
        i0=min(nx,i)
        gin(i,j)=gin(i,j)+half*(b(im,j)+b(i0,j))
     end do
  end do
  
end subroutine unfill_mass_grid2u

subroutine unfill_mass_grid2v(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2v       opposite of fill_mass_grid2v
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2v. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  This routine interpolates in y to the C grid v component
!           and adds the result to the preexisting contents of gout.
!
! program history log:
!   2004-07-16  parrish
!
!   input argument list:
!     gout     - input A-grid (reorganized for distibution to local domains)
!     gin      - preexisting input values to be added to on C-grid
!     nx,ny    - input grid dimensions
!
!   output argument list:
!     gin      - output result on C grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: half
  use gridmod, only: itotsub,iglobal,ltosi,ltosj

  implicit none

  integer(i_kind), intent(in)  :: nx,ny
  real(r_single), intent(in)   :: gout(itotsub)
  real(r_single), intent(inout):: gin(nx,ny+1)
  
  real(r_single) b(nx,ny)
  integer(i_kind) i,j,j0,jm

  do i=1,iglobal
     b(ltosj(i),ltosi(i))=gout(i)
  end do

  do j=1,ny+1
     jm=max(1,j-1)
     j0=min(ny,j)
     do i=1,nx
        gin(i,j)=gin(i,j)+half*(b(i,jm)+b(i,j0))
     end do
  end do
  
end subroutine unfill_mass_grid2v
