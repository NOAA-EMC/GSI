 subroutine roughness

 use gribit, only                : kpds_mdl,           &
                                   kgds_mdl

 use lsmask_orog, only           : lsmask,             &
                                   num_orog_tiles,     &
                                   orog_tiles_elev,    &
                                   orog_tiles_prcnt,   &
                                   lbms_lnd_mdl,       &
                                   orog_stnd_dev

 use program_setup, only         : roughness_file,     &
                                   imdl,               &
                                   jmdl,               &
                                   lonsperlat_mdl,     &
                                   thinned

!cggg use vegtype_tile, only          : dominate_veg_cat

 use read_write_utils, only      : full_to_thin

 implicit none

 character*3                    :: interp_mask
 character*2                    :: interp_type
 character*150                  :: output_file

 integer                        :: grib_scale_fac
 integer                        :: i, j
 integer                        :: iret
 integer                        :: iunit_out
 integer                        :: kgds(200)
 integer                        :: kpds(200)
 integer                        :: tile

 real                           :: avg_elev
 real                           :: default_value
 real                           :: fcm
 real                           :: sum
 real, allocatable              :: z0(:,:)
 real                           :: z0land
 real                           :: z0veg(24)  ! in meters

! must use usgs vegetation types.

 data z0veg / 1.00,  0.07,  0.07,  0.07,  0.07,  0.15, &
              0.08,  0.03,  0.05,  0.86,  0.80,  0.85, &
              2.65,  1.09,  0.80,  0.001, 0.04,  0.05, &
              0.01,  0.04,  0.06,  0.05,  0.03,  0.001 /

!-----------------------------------------------------------------------
! initialize some variables, then call interp driver.
!-----------------------------------------------------------------------

 if (len_trim(roughness_file) == 0) return

 output_file    = "fort.35"   ! grib file of data on model grid.
 iunit_out      =  35         ! unit # of above.

 if (trim(roughness_file) == "eta" .or. trim(roughness_file) == "nmm") then

   allocate (z0(imdl,jmdl))
   z0 = 0.0
  
   if (trim(roughness_file) == "eta") then

     print*,"- CALCULATE ROUGHNESS LENGTH AS IN THE ETA MODEL"

     z0land = 0.10
     fcm    = 1.0e-4   ! eta uses 1e-5, but it uses surface geopotential
                       ! whereas i use terrain height.
     do j = 1, jmdl
       do i = 1, imdl

         if (lsmask(i,j) > 0.0) then

           sum = 0.0

           do tile = 1, num_orog_tiles(i,j)
             sum = sum + orog_tiles_prcnt(i,j,tile) *    &
                         orog_tiles_elev(i,j,tile)
           enddo
 
           avg_elev = sum / num_orog_tiles(i,j)

           z0(i,j) = (avg_elev*fcm) + z0land

         end if

       enddo
     enddo

   else if (trim(roughness_file) == "nmm") then

     print*,"- CALCULATE ROUGHNESS LENGTH AS IN THE NMM MODEL"

!cggg     if (.not. allocated(dominate_veg_cat)) then
       print*,"- ROUGHNESS LENGTH CALCULATION REMOVED."
       stop
!cggg     end if

!     do j = 1, jmdl
!       do i = 1, imdl
!         if (lsmask(i,j) > 0.0) then
!           sum = 0.0
!           do tile = 1, num_orog_tiles(i,j)
!             sum = sum + orog_tiles_prcnt(i,j,tile) *  &
!                         orog_tiles_elev(i,j,tile)
!           enddo
!           avg_elev = sum / num_orog_tiles(i,j)
!           z0(i,j) = 0.1 + (max(avg_elev*9.81, 0.0)*0.0003) +  &
!                     (orog_stnd_dev(i,j) * 0.01) +  &
!                      z0veg(dominate_veg_cat(i,j))
!         end if
!       enddo
!     enddo

   end if

   print*,"- OPEN: ", trim(output_file)

   call baopenw(iunit_out, output_file, iret)

   if (iret /= 0) then
     print*,'- BAD OPEN, IRET IS ', iret
     call abort
   end if

   kpds = kpds_mdl
   kgds = kgds_mdl

   kpds(5) = 83  ! roughness length in meters
   kpds(22) = 3  ! scaling factor

   print*,"- WRITE: ", trim(output_file)

   if (thinned) then  ! for global grids, fill in grid.
     call full_to_thin(z0, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (iunit_out, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
               z0, iret)

   if (iret /= 0) then
     print*,'- BAD WRITE OF FILE:', trim(output_file), ' IRET IS ', iret
     call abort
   end if

   deallocate(z0)

   return

 end if

!-----------------------------------------------------------------------
! if the nmm or eta option was not chosen, interpolate an
! external roughness database to model grid.
!-----------------------------------------------------------------------
 
 print*,"- INTERPOLATE ROUGHNESS LENGTH DATA TO MODEL GRID"

 grib_scale_fac =  3          ! # decimal places (-1 same as input data) 
 default_value  =  .10        ! if interp routine can not find data
                              ! at a model grid point, set to this value.
 interp_type    = "xx"        ! let routine logic choose interp method
 interp_mask    = "lnd"       ! a land field

 call interp_to_mdl(roughness_file, output_file, iunit_out, &
                    interp_type, default_value, grib_scale_fac, &
                    interp_mask)

 return

 end subroutine roughness
