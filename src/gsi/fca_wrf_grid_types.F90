#ifdef FCA_REF_MOD
module fca_wrf_grid_types_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   fca_wrf_grid_types_m
!  prgmmr: Nehrkorn
!
! abstract: derived type definition for fca_wrf_grid
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
  public :: fca_wrf_grid
#else
#define TRACE_USE
#endif

type :: fca_wrf_grid
     ! component fields
     ! Note: in current implementation, all except XLAT, XLONG are used
     real(fp) :: ptop
     real(fp), allocatable, dimension(:,:,:) :: P, PB, PH, PHB, T, U, V, W, PH_NL
     real(fp), allocatable, dimension(:,:) ::  MU, MUB, HGT, PSFC, XLAT, XLONG
     real(fp), allocatable, dimension(:) :: ZNU, ZNW, C1H, C2H, C3H, C4H, C3F, C4F
     real(fp), allocatable, dimension(:,:,:,:) :: MOIST
end type fca_wrf_grid

#ifdef FCA_REF_MOD
end module fca_wrf_grid_types_m
#endif


