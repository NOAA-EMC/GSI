 module soiltype_notile

 contains

 subroutine soiltype_notile_option

 use program_setup, only         : soiltype_notile_file

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

 if (len_trim(soiltype_notile_file) == 0) return

 print*,"- INTERPOLATE ZOBLER SOIL TYPE TO MODEL GRID"
 
 output_file    = "fort.34"   ! grib file of data on model grid.
 iunit_out      = 34          ! unit # of above.
 grib_scale_fac =  0          ! # decimal places (-1 same as input data) 
 default_value  =  2.         ! if interp routine can not find data
                              ! at a model grid point, set to this value.
 interp_type    = "nn"        ! use nearest neighbor
 interp_mask    = "lnd"       ! a land field

 call interp_to_mdl(soiltype_notile_file, output_file, iunit_out, &
                    interp_type, default_value, grib_scale_fac, &
                    interp_mask)

 return

 end subroutine soiltype_notile_option

 end module soiltype_notile
