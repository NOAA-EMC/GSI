module abstract_convert_netcdf_mod 
  type, abstract :: abstract_convert_netcdf_class
    contains
      procedure(convert_netcdf_mass), deferred, nopass :: convert_netcdf_mass 
      procedure(convert_netcdf_nmm), deferred, nopass :: convert_netcdf_nmm  
      procedure(update_netcdf_mass), deferred, nopass :: update_netcdf_mass 
      procedure(update_netcdf_nmm), deferred, nopass :: update_netcdf_nmm  
  end type abstract_convert_netcdf_class

  abstract interface
  subroutine convert_netcdf_mass
    import abstract_convert_netcdf_class 
    implicit none
  end subroutine convert_netcdf_mass
  end interface

  abstract interface
  subroutine update_netcdf_nmm
    import abstract_convert_netcdf_class 
    implicit none
  
  end subroutine update_netcdf_nmm
  end interface

  abstract interface
  subroutine update_netcdf_mass
    import abstract_convert_netcdf_class 
    implicit none
  
  end subroutine update_netcdf_mass
  end interface

  abstract interface
  subroutine convert_netcdf_nmm(update_pint,ctph0,stph0,tlm0,guess)
    use kinds, only: r_single,i_kind,r_kind
    import abstract_convert_netcdf_class 
    implicit none
    logical     ,intent(in   ) :: guess
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  
  end subroutine convert_netcdf_nmm
  end interface
end module abstract_convert_netcdf_mod


