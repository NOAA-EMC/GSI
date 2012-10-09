subroutine fill_mass_grid2t(gin,nx,ny,gout,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_mass_grid2t    move wrf mass core c-grid to a-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered C grid used 
!           by the wrf mass core.  This is for mass points, for which the
!           output grid is a direct copy of the input grid.  The final 
!           output grid is rearranged for transfer to subdomains.
!
! program history log:
!   2004-07-15  parrish
!
!   input argument list:
!     gin      - input C grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!
!   output argument list
!     gout     - output A-grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: ione,zero
  use gridmod, only: iglobal, itotsub, ltosi, ltosj, ltosi_s, ltosj_s

  implicit none

  integer(i_kind),intent(in   ) :: nx,ny,iorder
  real(r_single) ,intent(in   ) :: gin(nx,ny)
  real(r_single) ,intent(  out) :: gout(itotsub)
  
  real(r_single) b(nx,ny)
  integer(i_kind) i,j

!---------------------------mass grids--just copy
  do j=1,ny
     do i=1,nx
        b(i,j)=gin(i,j)
     end do
  end do
  
! Reorganize for eventual distribution to local domains
  do i=1,itotsub
     gout(i)=zero
  end do
  if(iorder==ione)then
     do i=1,itotsub
        gout(i)=b(ltosj_s(i),ltosi_s(i))
     end do
  else
     do i=1,iglobal
        gout(i)=b(ltosj(i),ltosi(i))
     end do
  endif
  
end subroutine fill_mass_grid2t


subroutine fill_mass_grid2u(gin,nx,ny,gout,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_mass_grid2u   move wrf mass core c-grid to a-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered C grid used 
!           by the wrf mass core.  This interpolates u-grid points in x 
!           to the location of mass grid points.
!
! program history log:
!   2004-07-15  parrish
!
!   input argument list:
!     gin      - input C grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!
!            INDEXING FOR C-GRID (u-points relative to h-points
!
!
!       ^    3         u  h  u  h  u  h  u  h  u
!       |
!           
!
!       y    2         u  h  u  h  u  h  u  h  u
!
!          
!
!        h,u 1         u  h  u  h  u  h  u  h  u 
!
!                                       (note extra u point in x direction)
!
!                u     1     2     3
!                h        1     2     3
!
!                           x -->
!
!   output argument list
!     gout     - output A-grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: ione,half,zero
  use gridmod, only: iglobal, itotsub, ltosi, ltosj, ltosi_s, ltosj_s

  implicit none

  integer(i_kind),intent(in   ) :: nx,ny,iorder
  real(r_single) ,intent(in   ) :: gin(nx+ione,ny)
  real(r_single) ,intent(  out) :: gout(itotsub)
  
  real(r_single) b(nx,ny)
  integer(i_kind) i,ip,j

  do j=1,ny
     do i=1,nx
        ip=i+ione
        b(i,j)=half*(gin(i,j)+gin(ip,j))
     end do
  end do

! Reorganize for eventual distribution to local domains
  do i=1,itotsub
     gout(i)=zero
  end do
  if(iorder==ione)then
     do i=1,itotsub
        gout(i)=b(ltosj_s(i),ltosi_s(i))
     end do
  else
     do i=1,iglobal
        gout(i)=b(ltosj(i),ltosi(i))
     end do
  endif

end subroutine fill_mass_grid2u


subroutine fill_mass_grid2v(gin,nx,ny,gout,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_mass_grid2    move wrf mass core c-grid to a-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered C grid used 
!           by the wrf mass core.  For the mass points, the output grid 
!           is a direct copy of the input grid.  The u-grid (v-grid) is interpolated
!           in the x-direction (y-direction) to the mass grid points.
!
! program history log:
!   2004-07-15  parrish
!
!   input argument list:
!     gin      - input C grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!
!            INDEXING FOR C-GRID
!
!              4          v     v     v     v
!
!       ^    3            h     h     h     h
!       |
!              3          v     v     v     v
!
!       y    2            h     h     h     h
!
!              2          v     v     v     v
!
!        h   1            h     h     h     h
!
!         v    1          v     v     v     v
!
!                h,v      1     2     3
!                          (note extra v point in y direction)
!
!                           x -->
!
!   output argument list
!     gout     - output A-grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: ione, half, zero
  use gridmod, only: iglobal, itotsub, ltosi, ltosj, ltosi_s, ltosj_s

  implicit none

  integer(i_kind),intent(in   ) :: nx,ny,iorder
  real(r_single) ,intent(in   ) :: gin(nx,ny+ione)
  real(r_single) ,intent(  out) :: gout(itotsub)
  
  real(r_single) b(nx,ny)
  integer(i_kind) i,j,jp

  do j=1,ny
     jp=j+ione
     do i=1,nx
        b(i,j)=half*(gin(i,j)+gin(i,jp))
     end do
  end do

! Reorganize for eventual distribution to local domains
  do i=1,itotsub
     gout(i)=zero
  end do
  if(iorder==ione)then
     do i=1,itotsub
        gout(i)=b(ltosj_s(i),ltosi_s(i))
     end do
  else
     do i=1,iglobal
        gout(i)=b(ltosj(i),ltosi(i))
     end do
  endif
  
end subroutine fill_mass_grid2v
