#ifdef FCA_REF_MOD
module core_disp_types_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   core_disp_types_m
!  prgmmr: Nehrkorn
!
! abstract: Core displacement data structures
!
! program history log:
!
! subroutines included:
!
! functions included:
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use fp_types_m, only: fp
  implicit none
  private
  public :: fca_gridded_meta, fca_gridded_disp
  integer, public :: ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme !domain and memory limits
  integer, public :: its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe !tile and patch limits
  integer, parameter, public :: bilinear=1, bicubic=3
#endif

type :: fca_gridded_meta
     ! meta data for a gridded field
     integer :: nx, ny	! # x points, y points
     real(fp) :: dx, dy, x0, y0
     real(fp), dimension(:,:), allocatable :: lat2d, lon2d
end type fca_gridded_meta

type :: fca_gridded_disp
     type (fca_gridded_meta) :: meta		! meta data
     ! displacements <x, y>
     real(fp), allocatable, dimension(:,:) :: x_disp, y_disp
     real(fp), allocatable, dimension(:,:) :: dx_n, dy_n
     integer, allocatable, dimension(:,:) :: ix_orig, iy_orig
end type fca_gridded_disp

#ifdef FCA_REF_MOD
end module core_disp_types_m
#endif
