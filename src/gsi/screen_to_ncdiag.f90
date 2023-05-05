module screen_to_ncdiag
   implicit none
   private
   public:: screen_to_single_nc_diag_metadata

   !Provides a subroutine to screen quantities for NaNs before converting doubles
   !to singles and sending them to nc_diag_metadata. This prevents floating
   !invalids.

  contains

  subroutine screen_to_single_nc_diag_metadata(vname, var)
     use constants, only: r_missing
     use kinds, only: r_double, r_kind
     use nc_diag_write_mod, only: nc_diag_metadata

     !Screens double variables to be sent to nc_diag_metadata by checking for NaNs first
     !to prevent floating invalids (which are reported as 0)
     character(len=*), intent(in) :: vname
     real (r_double), intent(in) :: var

     if(isnan(var)) then
        call nc_diag_metadata(vname, sngl(real(r_missing)))
     else
        call nc_diag_metadata(vname, sngl(var))
     endif

  endsubroutine screen_to_single_nc_diag_metadata

endmodule screen_to_ncdiag
