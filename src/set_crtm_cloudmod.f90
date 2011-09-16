module set_crtm_cloudmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   set_crtm_cloudmod
!  prgmmr: todling          org: gmao                date: 2011-06-01
!
! abstract: module providing interface to set-crtm-cloud procedures
!
! program history log:
!   2011-06-01  todling
!
! subroutines included:
!   sub Set_CRTM_Cloud
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

private

public Set_CRTM_Cloud

interface Set_CRTM_Cloud
  subroutine Set_CRTM_Cloud_ ( km, nac, cloud_name, icmask, nc, cloud_cont, dp, tp, pr, qh, cloud)
  use kinds, only: i_kind,r_kind
  use CRTM_Cloud_Define, only: CRTM_Cloud_type
  implicit none
  integer(i_kind) , intent(in)    :: km                ! number of levels
  integer(i_kind) , intent(in)    :: nac               ! number of actual clouds
  character(len=*), intent(in)    :: cloud_name(nac)   ! [nac]   Model cloud names: qi, ql, etc.
  logical,          intent(in)    :: icmask            ! mask determining where to consider clouds
  integer(i_kind) , intent(in)    :: nc                ! number of clouds
  real(r_kind),     intent(in)    :: cloud_cont(km,nc) ! [km,nc] aerosol concentration (Kg/m2)
  real(r_kind),     intent(in)    :: dp(km)            ! [km]    
  real(r_kind),     intent(in)    :: tp(km)            ! [km]   atmospheric temperature (K)
  real(r_kind),     intent(in)    :: pr(km)            ! [km]   atmospheric pressure  
  real(r_kind),     intent(in)    :: qh(km)            ! [km]   specific humidity
  type(CRTM_Cloud_type), intent(inout) :: cloud(nc)    ! [nc]   CRTM Cloud object
  end subroutine Set_CRTM_Cloud_
end interface

end module set_crtm_cloudmod
