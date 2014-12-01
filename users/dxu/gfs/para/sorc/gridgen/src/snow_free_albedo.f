 subroutine snow_free_albedo

 use program_setup, only         : snowfree_albedo_file

 implicit none

 character*3                    :: interp_mask
 character*2                    :: interp_type
 character*150                  :: output_file

 integer                        :: grib_scale_fac
 integer                        :: iunit_out

 real                           :: default_value

!-----------------------------------------------------------------------
! initialize some variables, then call interp driver.
!-----------------------------------------------------------------------

 if (len_trim(snowfree_albedo_file) == 0) return

 print*,"- INTERPOLATE SNOW FREE ALBEDO TO MODEL GRID"
 
 output_file    = "fort.46"   ! grib file of data on model grid.
 iunit_out      =  46         ! unit # of above.
 grib_scale_fac =  1          ! # decimal places (-1 same as input data) 
 default_value  =  30.        ! if interp routine can not find data
                              ! at a model grid point, set to this value.
 interp_type    = "xx"        ! let routine logic choose interp method
 interp_mask    = "lnd"       ! a land field

 call interp_to_mdl(snowfree_albedo_file, output_file, iunit_out, &
                    interp_type, default_value, grib_scale_fac,  &
                    interp_mask)

 return

 end subroutine snow_free_albedo
