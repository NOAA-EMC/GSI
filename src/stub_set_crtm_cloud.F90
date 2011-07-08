!-----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: set_CRTM_Cloud --- Set CRTM Cloud (FORTRAN-77 Interface)
!
! !INTERFACE:
!
subroutine Set_CRTM_Cloud ( km, nac, cloud_name, icmask, nc, cloud_cont, dp, tp, pr, qh, cloud)

! USES:

  use kinds, only: i_kind,r_kind
  use CRTM_Cloud_Define, only: CRTM_Cloud_type
  use crtm_cloud, only: SetCloud

implicit none

! !ARGUMENTS:

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

! !DESCRIPTION: Set the CRTM Cloud object given model cloud properties.
!
! !REVISION HISTORY:
!
! 14May2011  Todling  Initial version, FORTRAN-77 interface for GSI.
!
!EOP
!-----------------------------------------------------------------------------
 
  call setCloud (cloud_name, icmask, cloud_cont, dp, tp, pr, qh, cloud)

end subroutine Set_CRTM_Cloud

