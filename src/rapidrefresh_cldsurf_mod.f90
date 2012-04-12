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
!   def metar_impact_radius - impact radius for METAR cloud observation
!   def metar_impact_radius_lowCloud - impact radius for METAR cloud observation
!                                      that indicate low cloud base
!   def l_gsd_terrain_match_surfTobs - namelist logical for GSD terrain
!                                       match for  surface temperature observation
!   def l_sfcobserror_ramp_t  - namelist logical for adjusting surface temperature observation error
!   def l_sfcobserror_ramp_q  - namelist logical for adjusting surface moisture observation error
!   def l_PBL_pseudo_SurfobsT - namelist logical for producing pseudo-obs in PBL 
!                                       layer based on surface obs T
!   def l_PBL_pseudo_SurfobsQ - namelist logical for producing pseudo-obs in PBL 
!                                       layer based on surface obs Q
!   def l_PBL_pseudo_SurfobsUV - namelist logical for producing pseudo-obs in PBL 
!                                       layer based on surface obs UV
!   def pblH_ration - percent of the PBL height within which to add 
!                                       pseudo-obs (default:0.75)
!   def pps_press_incr - pressure increase for each additional pseudo-obs 
!                                       on top of previous level (default:30hPa)
!   def l_gsd_limit_ocean_q      - namelist logical for doing GSD limitation of Q over ocean
!   def l_pw_hgt_adjust      - namelist logical for doing precipitable water (PW) height adjustment
!                                       based on obs vs. model height
!   def l_limit_pw_innov     - namelist logical for limiting size of PW innovation
!   def max_innov_pct        - namelist real for limit size of PW innovation to percent
!                                       of background value (value = 0 to 1)
!   def l_cleanSnow_WarmTs   - namelist logical for doing GSD limitation of using
!                                       retrieved snow over warn area (Ts > 5C)
!   def l_conserve_thetaV    - namelist logical for conserving thetaV during moisture
!                                       adjustment in cloud analysis
!   def r_cleanSnow_WarmTs_threshold - namelist threshold for using retrieved snow over warn area
!
!   def i_conserve_thetaV_iternum    - namelist iteration number for conserving 
!                                           thetaV during moisture adjustment
!   def l_cld_bld            - namelist logical for GOES cloud building
!   def cld_bld_hgt          - namelist real for height limit, below which you build clouds
!                                       (default = 1200 meters)
!
! attributes:
!   language: f90
!   machine:  linux cluster (wjet)
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_rapidrefresh_cldsurf
  public :: l_cloud_analysis 
  public :: dfi_radar_latent_heat_time_period
  public :: metar_impact_radius
  public :: metar_impact_radius_lowCloud
  public :: l_gsd_terrain_match_surfTobs
  public :: l_sfcobserror_ramp_t
  public :: l_sfcobserror_ramp_q
  public :: l_PBL_pseudo_SurfobsT
  public :: l_PBL_pseudo_SurfobsQ
  public :: l_PBL_pseudo_SurfobsUV
  public :: pblH_ration
  public :: pps_press_incr
  public :: l_gsd_limit_ocean_q
  public :: l_pw_hgt_adjust
  public :: l_limit_pw_innov
  public :: max_innov_pct
  public :: l_cleanSnow_WarmTs
  public :: l_conserve_thetaV
  public :: r_cleanSnow_WarmTs_threshold
  public :: i_conserve_thetaV_iternum
  public :: l_cld_bld
  public :: cld_bld_hgt

  logical l_cloud_analysis
  real(r_kind)  dfi_radar_latent_heat_time_period
  real(r_kind)  metar_impact_radius
  real(r_kind)  metar_impact_radius_lowCloud
  logical l_gsd_terrain_match_surfTobs
  logical l_sfcobserror_ramp_t
  logical l_sfcobserror_ramp_q
  logical l_PBL_pseudo_SurfobsT
  logical l_PBL_pseudo_SurfobsQ
  logical l_PBL_pseudo_SurfobsUV
  logical l_gsd_limit_ocean_q
  real(r_kind)  pblH_ration
  real(r_kind)  pps_press_incr
  logical l_pw_hgt_adjust
  logical l_limit_pw_innov
  real(r_kind) max_innov_pct
  logical l_cleanSnow_WarmTs
  logical l_conserve_thetaV
  real(r_kind)    r_cleanSnow_WarmTs_threshold
  integer(i_kind) i_conserve_thetaV_iternum
  logical l_cld_bld
  real(r_kind) cld_bld_hgt

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
!   2011--5-04  Todling   inquire MetGuess for presence of hyrometeors & set default
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
    use kinds, only: i_kind 
    use gsi_metguess_mod, only: gsi_metguess_get
    implicit none
    integer(i_kind) ivar,i,ier
    logical have_hmeteor(5)
    character(len=2),parameter :: hydrometeors(5) = (/ 'qi', &
                                                       'ql', &
                                                       'qr', &
                                                       'qs', &
                                                       'qg'  &
                                                       /)

!   Set logical flag
    dfi_radar_latent_heat_time_period = 30.0_r_kind   ! in minutes
    metar_impact_radius = 10.0_r_kind                 ! in grid
    metar_impact_radius_lowCloud = 4.0_r_kind         ! in grid
    l_gsd_terrain_match_surfTobs = .false.            ! .true. = turn on GSD terrain 
                                                      !          match for  surface
                                                      !          temperature observation

!   Figure out if hydrometeors are available in guess, if so, do cloud adjustment as default
    if(size(hydrometeors) == 0)then
       l_cloud_analysis = .false.
    else
      do i=1,size(hydrometeors)
         call gsi_metguess_get ('var::'//trim(hydrometeors(i)),ivar,ier)
         have_hmeteor(i) = (ivar>0)
      enddo
      l_cloud_analysis = all(have_hmeteor)
    end if

    l_sfcobserror_ramp_t  = .false.  ! .true. = turn on GSD surface temperature observation error adjustment
    l_sfcobserror_ramp_q  = .false.  ! .true. = turn on GSD surface moisture observation error adjustment
    l_PBL_pseudo_SurfobsT  = .false.                  ! .true. = turn on PBL pseudo-obs T
    l_PBL_pseudo_SurfobsQ  = .false.                  ! .true. = turn on PBL pseudo-obs Q
    l_PBL_pseudo_SurfobsUV = .false.                  ! .true. = turn on PBL pseudo-obs UV
    pblH_ration = 0.75_r_kind                         ! in percent
    pps_press_incr = 30.0_r_kind                      ! in hPa
    l_gsd_limit_ocean_q    = .false.                      ! .true. = turn on limitation of Q over ocean
    l_pw_hgt_adjust    = .false.                      ! .true. = turn on PW obs height adjustment
    l_limit_pw_innov   = .false.                      ! .true. = turn on limit for size of PW innovation
    max_innov_pct      = 0.1_r_kind                   ! in percent of background PW
    l_cleanSnow_WarmTs = .false.                      ! .true. = turn on limitation of using snow when Ts>5
    l_conserve_thetaV  = .false.                      ! .true. = turn on conserving thetaV
    r_cleanSnow_WarmTs_threshold = 8.0_r_kind         ! Ts threshold for cleaning snow
    i_conserve_thetaV_iternum = 3                     ! iteration number for conserving thetaV
    l_cld_bld          = .false.                      ! .true. = turn on GOES cloud building
    cld_bld_hgt        = 1200.0_r_kind                ! Height (meters) below which to build clouds

    return
  end subroutine init_rapidrefresh_cldsurf

end module rapidrefresh_cldsurf_mod
