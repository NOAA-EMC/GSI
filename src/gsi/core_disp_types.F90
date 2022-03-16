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
  use kinds, only: r_kind, i_kind
  implicit none
  private
  public :: fca_gridded_meta, fca_gridded_disp
  integer(i_kind), public :: ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme !domain and memory limits
  integer(i_kind), public :: its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe !tile and patch limits
  integer(i_kind), parameter, public :: bilinear=1, bicubic=3
#endif

type :: fca_gridded_meta
     ! meta data for a gridded field
     integer(i_kind) :: nx, ny	! # x points, y points
     real(r_kind) :: dx, dy, x0, y0
     real(r_kind), dimension(:,:), allocatable :: lat2d, lon2d
end type fca_gridded_meta

type :: fca_gridded_disp
     type (fca_gridded_meta) :: meta		! meta data
     ! displacements <x, y>
     real(r_kind), allocatable, dimension(:,:) :: x_disp, y_disp
     real(r_kind), allocatable, dimension(:,:) :: dx_n, dy_n
     integer(i_kind), allocatable, dimension(:,:) :: ix_orig, iy_orig
end type fca_gridded_disp

#ifdef FCA_REF_MOD
end module core_disp_types_m
#endif
