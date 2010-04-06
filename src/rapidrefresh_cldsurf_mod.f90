module rapidrefresh_cldsurf_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:  rapid refresh module
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2008-06-04
!
! abstract: 
!      This module contains definition and initialization of variables for RR
!
! program history log:
!   2008-06-03 Hu           initial build
!   2010-03-29 Hu           change to fit the trunk version
! 
! Subroutines Included:
!   sub init_rapidrefresh_cldsurf  - initialize RR related variables to default values
!
! Variable Definitions:
!   def l_cloud_analysis    - namelist logical for cloud analysis (=true) 
!   def dfi_radar_latent_heat_time_period - DFI forward integration window in minutes
!
! attributes:
!   language: f90
!   machine:  linux cluster (wjet)
!
!$$$ end documentation block

  use kinds, only: r_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_rapidrefresh_cldsurf
  public :: l_cloud_analysis 
  public :: dfi_radar_latent_heat_time_period

  logical l_cloud_analysis
  real(r_kind)  dfi_radar_latent_heat_time_period


contains

  subroutine init_rapidrefresh_cldsurf
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  init_rapidrefresh_cldsurf
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2008-06-04
!
! abstract:  set defaults for RR related variables
!
! program history log:
!   2008-06-03  Hu        initial build for cloud analysis
!   2010-03-29  Hu        change names to init_rapidrefresh_cldsurf
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  liunx cluster (Wjet)
!
!$$$
    implicit none

!   Set logical flag
    l_cloud_analysis = .false.   ! .true. = turn on GSD cloud analysis
    dfi_radar_latent_heat_time_period = 30.0_r_kind   ! in minutes

    return
  end subroutine init_rapidrefresh_cldsurf

end module rapidrefresh_cldsurf_mod
