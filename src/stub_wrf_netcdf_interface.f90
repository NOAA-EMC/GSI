!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  wrf_netcdf_interface
!
! !DESCRIPTION: This stub provides the default interfaces to the 
!               WRF netcdf capability in GSI.
!
! !REVISION HISTORY:
!
!  08Jun2016 Mahajan - Initial code
!
!EOP
!-------------------------------------------------------------------------
module convert_netcdf_mod
use abstract_convert_netcdf_mod 
  type, extends(abstract_convert_netcdf_class) :: convert_netcdf_class
  contains
    procedure, nopass :: convert_netcdf_mass => convert_netcdf_mass_dummy
    procedure, nopass :: convert_netcdf_nmm  => convert_netcdf_nmm_dummy
    procedure, nopass :: update_netcdf_mass  => update_netcdf_mass_dummy
    procedure, nopass :: update_netcdf_nmm   => update_netcdf_nmm_dummy
  end type convert_netcdf_class
contains
  subroutine convert_netcdf_mass_dummy
    
    implicit none
  end subroutine convert_netcdf_mass_dummy
  
  subroutine convert_netcdf_nmm_dummy(update_pint,ctph0,stph0,tlm0,guess)
    use kinds, only: r_single,i_kind,r_kind
    implicit none
    logical     ,intent(in   ) :: guess
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  
  end subroutine convert_netcdf_nmm_dummy
  
  subroutine update_netcdf_mass_dummy
  
    implicit none
  
  end subroutine update_netcdf_mass_dummy
  
  subroutine update_netcdf_nmm_dummy
  
    implicit none
  
  end subroutine update_netcdf_nmm_dummy
end module convert_netcdf_mod
