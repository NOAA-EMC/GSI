 program gridgen_sfc

 use program_setup, only   : setup,   &
                             setup_cleanup

 use calc_latlons,  only   : calc_latlons_mdl, &
                             calc_latlons_cleanup 
 
 use lsmask_orog,  only    : lsmask_orog_driver, &
                             lsmask_orog_cleanup

 use grib_latlons, only    : grib_latlons_mdl

 use gribit, only          : init_pds_gds

 use soiltype_notile, only : soiltype_notile_option

 use vegtype_notile, only  : vegtype_notile_option

 implicit none

 include 'mpif.h'

 integer            :: ierr, myrank

 call w3tagb('GRIDGEN_SFC',2005,0136,0000,'NP2')

 call mpi_init(ierr)

 call mpi_comm_rank(mpi_comm_world, myrank, ierr)

!-----------------------------------------------------------------------
! get user-specified options and grid information.
!-----------------------------------------------------------------------

 call setup

!-----------------------------------------------------------------------
! calc lat/lons on the model grid
!-----------------------------------------------------------------------

 call calc_latlons_mdl

!-----------------------------------------------------------------------
! set up grid header information for gribbing model data.
!-----------------------------------------------------------------------

 call init_pds_gds

!-----------------------------------------------------------------------
! get land sea mask and orography.
!-----------------------------------------------------------------------

 call lsmask_orog_driver(myrank)

 call leaf_area_index

!-----------------------------------------------------------------------
! interpolate hi-res soil and vegetation types using tiling option.
!-----------------------------------------------------------------------

 call soil_vegtype_tile

 if (myrank /=0) goto 888

!-----------------------------------------------------------------------
! interpolate soil type using a no tiling option. routine used
! for interpolating a coarse res database in grib format.
!-----------------------------------------------------------------------

 call soiltype_notile_option

!-----------------------------------------------------------------------
! interpolate veg types using no tiling option.  routine used
! for interpolating a coarse res database in grib format.
!-----------------------------------------------------------------------

 call vegtype_notile_option
 
!-----------------------------------------------------------------------
! interpolate glacier data
!-----------------------------------------------------------------------

 call glacier

!-----------------------------------------------------------------------
! interpolate climo sea ice data
!-----------------------------------------------------------------------

 call seaice

!-----------------------------------------------------------------------
! interpolate roughness data.
!-----------------------------------------------------------------------

 call roughness

!-----------------------------------------------------------------------
! interpolate greenness fraction.
!-----------------------------------------------------------------------

 call green 

!-----------------------------------------------------------------------
! interpolate maximum snow albedo.
!-----------------------------------------------------------------------

 call max_snow_albedo 

!-----------------------------------------------------------------------
! interpolate snow free albedo.
!-----------------------------------------------------------------------

 call snow_free_albedo

!-----------------------------------------------------------------------
! interpolate slope type.
!-----------------------------------------------------------------------
 
 call slope_type

!-----------------------------------------------------------------------
! interpolate climo soil moisture.
!-----------------------------------------------------------------------
 
 call soilm

!-----------------------------------------------------------------------
! interpolate soil substrate temperature.
!-----------------------------------------------------------------------

 call soil_substrate 

!-----------------------------------------------------------------------
! interpolate sst climatology.
!-----------------------------------------------------------------------

 call sst_climo 

!-----------------------------------------------------------------------
! interpolate snow climatology.
!-----------------------------------------------------------------------

 call snow_climo 

!-----------------------------------------------------------------------
! grib lat/lons
!-----------------------------------------------------------------------

 call grib_latlons_mdl

!-----------------------------------------------------------------------
! free up memory.
!-----------------------------------------------------------------------

888 continue
 call lsmask_orog_cleanup
 call calc_latlons_cleanup
 call setup_cleanup

 print*,"************************************"
 print*,"** NORMAL TERMINATION FOR TASK:", myrank,"**"
 print*,"************************************"

 call w3tage('GRIDGEN_SFC')

 call mpi_finalize(ierr)

 stop

 end program gridgen_sfc
