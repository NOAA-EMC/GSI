subroutine Set_CRTM_Aerosol ( km, na, aero_name, aero_conc, rh, aerosol)

! USES:

  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use CRTM_Aerosol_Define, only: CRTM_Aerosol_type

  implicit none

! !ARGUMENTS:

  integer(i_kind) , intent(in)    :: km                ! number of levels
  integer(i_kind) , intent(in)    :: na                ! number of aerosols
  character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names: du0001, etc.
  real(r_kind),     intent(in)    :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
  real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humdity [0,1]

  type(CRTM_Aerosol_type), intent(inout) :: aerosol(na)! [na]   CRTM Aerosol object

! !DESCRIPTION: Set the CRTM Aerosol object given GOCART aerosol properties.
!               This version based on the GEOS-5 implementation of GOCART.
!
! !REVISION HISTORY:
!
! 23feb2011  da Silva  Initial version, FORTRAN-77 interface for GSI.
! 01aug2011  Lueken    Replaced F90 with f90 (no machine logic)
!
!EOP
!-----------------------------------------------------------------------------

  if(mype==0) then 
     print*, 'Stub Set_CRTM_Aerosol: Code should not be here (STUB)'
     print*, 'Stub Set_CRTM_Aerosol: You are likely doing something wrong'
  endif
  call stop2(999)
  
end subroutine Set_CRTM_Aerosol
