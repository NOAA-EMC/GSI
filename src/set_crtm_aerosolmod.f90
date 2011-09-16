module set_crtm_aerosolmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   set_crtm_aerosolmod
!  prgmmr: todling          org: gmao                date: 2011-06-01
!
! abstract: module providing interface to set-crtm-aerosol procedures
!
! program history log:
!   2011-06-01  todling
!
! subroutines included:
!   sub Set_CRTM_Aerosol_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

private

public Set_CRTM_Aerosol

interface Set_CRTM_Aerosol
  subroutine Set_CRTM_Aerosol_ ( km, na, aero_name, aero_conc, rh, aerosol)
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use CRTM_Aerosol_Define, only: CRTM_Aerosol_type
  implicit none
  integer(i_kind) , intent(in)    :: km                ! number of levels
  integer(i_kind) , intent(in)    :: na                ! number of aerosols
  character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names: du0001, etc.
  real(r_kind),     intent(in)    :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
  real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humdity [0,1]
  type(CRTM_Aerosol_type), intent(inout) :: aerosol(na)! [na]   CRTM Aerosol object
  end subroutine Set_CRTM_Aerosol_
end interface

end module set_crtm_aerosolmod
