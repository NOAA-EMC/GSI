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
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid: 
!                        Here input grid is larger than output grid.
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
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

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
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
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
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx+1,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,i0,im,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

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
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
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
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny+1)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j,j0,jm

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif

  do j=1,ny+1
     jm=max(1,j-1)
     j0=min(ny,j)
     do i=1,nx
        gin(i,j)=gin(i,j)+half*(b(i,jm)+b(i,j0))
     end do
  end do
  
end subroutine unfill_mass_grid2v

subroutine unfill_mass_grid2tmap(gout,nx,ny,gin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2t        opposite of fill_mass_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_mass_grid2t. 
!           The input field is an analyis increment on an unstaggered
!           A grid.  The result is added to the preexisting contents of gout.
!           This is for discontinuity field like tten
!
! program history log:
!   2004-07-16  parrish
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
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
  use mod_wrfmass_to_a, only: wrfmass_map_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_map_a_to_h4(ba,b)
  endif

! Mass grids--just copy
  do j=1,ny
     do i=1,nx
        gin(i,j)=b(i,j)+gin(i,j)
     end do
  end do
  
end subroutine unfill_mass_grid2tmap

subroutine unfill_mass_grid2t_ldmk(gout,nx,ny,gin,landmask)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_mass_grid2t        opposite of fill_mass_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is the same as unfill_mass_grid2t, 
!           but only add analysis increment over land. 
!
! program history log:
!   2004-07-16  parrish
!   2014-03-12  Hu       Code for GSI analysis on grid larger than background grid:
!                        Here input grid is larger than output grid.
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
  use mod_wrfmass_to_a, only: wrfmass_a_to_h4
  use gridmod, only: nlon, nlat

  implicit none

  integer(i_kind), intent(in   ) :: nx,ny
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  real(r_single) , intent(in)    :: landmask(nx,ny)
  
  real(r_single) ba(nlon,nlat)
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

  do i=1,iglobal
     ba(ltosj(i),ltosi(i))=gout(i)
  end do

  if(nlon == nx .and. nlat == ny) then
     b=ba
  else
     call wrfmass_a_to_h4(ba,b)
  endif
! only add analysis increment over land
  if(nlon == nx .and. nlat == ny) then
! do nothing
  else
     if(maxval(landmask) > 1.01 .or. minval(landmask) < -0.01) then
       write(*,*) 'bad landmask, do not use landmask filter soil nudging field'
     else
        do j=1,ny
           do i=1,nx
              if(landmask(i,j) < 0.1)  b(i,j)=0.0_r_single 
           end do
        end do
     endif
  endif
! Mass grids--just copy
  do j=1,ny
     do i=1,nx
        gin(i,j)=b(i,j)+gin(i,j)
     end do
  end do
  
end subroutine unfill_mass_grid2t_ldmk

