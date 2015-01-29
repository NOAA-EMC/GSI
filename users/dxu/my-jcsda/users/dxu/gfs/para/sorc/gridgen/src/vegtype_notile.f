 module vegtype_notile

 contains

 subroutine vegtype_notile_option

 use program_setup, only         : vegtype_notile_file

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

 if (len_trim(vegtype_notile_file) == 0) return

 print*,"- INTERPOLATE DORMAN/SELLERS VEGETATION TYPE DATA TO MODEL GRID"
 
 output_file    = "fort.33"   ! grib file of data on model grid.
 iunit_out      = 33          ! unit # of above.
 grib_scale_fac =  0          ! # decimal places (-1 same as input data) 
 default_value  =  7.         ! if interp routine can not find data
                              ! at a model grid point, set to this value.
 interp_type    = "nn"        ! use nearest neighbor
 interp_mask    = "lnd"       ! a land field

 call interp_to_mdl(vegtype_notile_file, output_file, iunit_out, &
                    interp_type, default_value, grib_scale_fac, &
                    interp_mask)

 return

 end subroutine vegtype_notile_option

 end module vegtype_notile

