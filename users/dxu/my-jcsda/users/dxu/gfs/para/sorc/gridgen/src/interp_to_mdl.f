 subroutine interp_to_mdl(input_file, output_file, iunit_out,  &
                          interp_type, default_value, grib_scale_fac, &
                          interp_mask)

!---------------------------------------------------------------
! this routine reads a grib file, interpolates the contents
! to the model grid, then writes it to a grib file.
!---------------------------------------------------------------

 use program_setup, only        : resol_mdl,  &
                                  imdl,       &
                                  jmdl,       &
                                  thinned,    &
                                  lonsperlat_mdl,  &
                                  domain_type,   &
                                  dx_mdl,     &
                                  dy_mdl,     &
                                  centlat_mdl,  &
                                  centlon_mdl 

 use lsmask_orog,  only         : lsmask,        &
                                  wtrmask,       &
                                  lbms_lnd_mdl,  &
                                  lbms_wtr_mdl

 use calc_latlons,  only        : lat_mdl,       &
                                  lon_mdl

 use gribit,        only        : kgds_mdl,      &
                                  kpds_mdl

 use interp_utils,  only        : interp_aavg,   &
                                  interp_bilinear, &
                                  interp_aavg_egrid

 use read_write_utils, only     : full_to_thin

 implicit none

 character*150, intent(in)     :: input_file
 character*3,   intent(in)     :: interp_mask
 character*2,   intent(in)     :: interp_type
 character*150, intent(in)     :: output_file

 integer                       :: bit1, bit2
 integer                       :: bitmap
 integer                       :: gds
 integer, intent(in)           :: grib_scale_fac
 integer                       :: isrc, jsrc
 integer                       :: iret
 integer, parameter            :: iunit_src = 40
 integer, intent(in)           :: iunit_out
 integer                       :: jgds(200)
 integer                       :: jpds(200)
 integer                       :: kgds(200)
 integer                       :: kpds(200)
 integer                       :: lskip
 integer, parameter            :: lugi = 0  ! grib index file unit number - not used
 integer                       :: message_num
 integer                       :: numbytes
 integer                       :: numpts
 integer                       :: search_rad

 logical                       :: bilinear
 logical*1, allocatable        :: lbms_mdl(:,:)
 logical*1, allocatable        :: lbms_src(:,:)

 real, allocatable             :: data_mdl(:,:)
 real, allocatable             :: data_src(:,:)
 real, intent(in)              :: default_value
 real                          :: dx_src
 real                          :: dy_src
 real                          :: lat_11_src  ! lat of point 1,1 on src grid
 real                          :: lon_11_src  ! lon of point 1,1 on src grid
 real, allocatable             :: lsmask_mdl(:,:)
 real, parameter               :: water_value = -999

!-----------------------------------------------------------------------
! execution starts here.
!-----------------------------------------------------------------------

 print*,"- OPEN INPUT FILE ", trim(input_file)
 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   call abort
 end if

 print*,'- OPEN OUTPUT FILE ', trim(output_file)
 call baopenw(iunit_out, output_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   call abort
 end if

!-----------------------------------------------------------------------
! read every record in the grib file, interpolate the data to the
! model grid, then grib the interpolated data.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! most fields are land related.  however, some fields, like
! climo sst, will be interpolated to water points.
!-----------------------------------------------------------------------

 allocate (lbms_mdl(imdl,jmdl))
 allocate (lsmask_mdl(imdl,jmdl))

 if (interp_mask == 'wtr') then
   lbms_mdl   = lbms_wtr_mdl
   lsmask_mdl = wtrmask
 elseif (interp_mask == 'lnd') then
   lbms_mdl   = lbms_lnd_mdl
   lsmask_mdl = lsmask
 end if

!-----------------------------------------------------------------------
! set lskip to read from beginning AND clean out index file.
! don't use a zero if you are reading multiple files
! using the same unit number.
! note: lskip get incremented by one after the call to getgb.
!-----------------------------------------------------------------------

 lskip = -1 

 MAIN : do 

   jgds    = -1
   jpds    = -1
   kgds    = -1
   kpds    = -1

   print*,"- GET GRIB HEADER FOR RECORD ", (max(lskip,0)+1)

   call getgbh(iunit_src, lugi, lskip, jpds, jgds, numbytes,  &
               numpts, message_num, kpds, kgds, iret)

!-----------------------------------------------------------------------
! when the end of file is reached, a status code of 99 is returned.
! there are other status codes as well, so abort for those.
!-----------------------------------------------------------------------
  
   if (iret == 99) exit MAIN

   if (iret /= 0) then
     print*,'- BAD READ OF GRIB HEADER. IRET IS ', iret
     call abort
   end if

!-----------------------------------------------------------------------
! source data must have a grid description section.
! source grid must be lat/lon data.  interp routines can't handle
! other projections right now.
!-----------------------------------------------------------------------

   gds = kpds(4) / 128

   if (gds /= 1) then
     print*,"- SOURCE DATA DOES NOT HAVE GRID DESCRIPTION SECTION."
     stop
   elseif (kgds(1) /= 0) then
     print*,"- SOURCE DATA MUST BE LAT/LON GRID."
     stop
   end if

!-----------------------------------------------------------------------
! grid dimensions of source grid.
!-----------------------------------------------------------------------

   isrc = kgds(2)
   jsrc = kgds(3)

!-----------------------------------------------------------------------
! degrib source data.
!-----------------------------------------------------------------------

   allocate(data_src(isrc,jsrc))
   allocate(lbms_src(isrc,jsrc))

   print*,"- DEGRIB DATA "

   call getgb(iunit_src, lugi, (isrc*jsrc), lskip, jpds, jgds, &
              numpts, lskip, kpds, kgds, lbms_src, data_src, iret)

   if (iret /= 0) then
     print*,"- BAD DEGRIB OF DATA. IRET IS ", iret
     call abort
   end if

!-----------------------------------------------------------------------
! determine lat/lon of source grid's corner point.
! determine grid resolution in the "x" and "y" directions.
! these are used in the interpolation routines.
!-----------------------------------------------------------------------

   lat_11_src = float(kgds(4))  / 1000.0
   lon_11_src = float(kgds(5))  / 1000.0
   dx_src     = float(kgds(9))  / 1000.0
   dy_src     = float(kgds(10)) / 1000.0

!-----------------------------------------------------------------------
! for categorical data, the user should specify a nearest neighbor
! interpolation.  otherwise, let the program determine the interpolaton
! type based on the resolutions of the source and model grids.
!-----------------------------------------------------------------------

   if (interp_type == "nn") then
     search_rad = 0
     bilinear   = .false.
     print*,"- WILL USE NEAREST NEIGHBOR INTERPOLATION"
   else
     if ( resol_mdl <= dx_src ) then
       print*,'- WILL USE BI-LINEAR INTERPOLATION '
       bilinear = .true.
     else
       search_rad = int ( (resol_mdl * 0.5) / dx_src )
       if (search_rad == 0) print*,'- WILL USE NEAREST NEIGHBOR INTERPOLATION'
       if (search_rad >0)   print*,'- WILL TAKE AREA AVERAGE OF SOURCE DATA ', search_rad
       bilinear   = .false.
     end if
   end if

!-----------------------------------------------------------------------
! determine scanning mode flags and adjust sign of grid 
! resolution so interp routines calc the lat/lon on the source
! grid correctly.
!-----------------------------------------------------------------------

   bit1 = kgds(11) / 128
   if (bit1 == 1) dx_src = -dx_src
   bit2 = mod(kgds(11),128) / 64
   if (bit2 == 0) dy_src = -dy_src

   allocate (data_mdl(imdl,jmdl))
   data_mdl    = 0.0 

!------------------------------------------------------------------------
! check for the presence of a bit map section.
! (pds section: bit 2, octet 8).  if no bitmap section, can't tell
! what is land or water on the source grid.  this is unfortunate.
! if a bitmap was used, set all source data points to the water value
! so the interpolation routines ignore these points.
!------------------------------------------------------------------------

   bitmap = mod(kpds(4),128) / 64
   
   if (bitmap == 1) then
     where (.not.lbms_src) data_src = water_value
   end if
  
   if (bilinear) then

     call interp_bilinear (imdl, jmdl, lat_mdl, lon_mdl, lsmask_mdl,  &
                           dx_src, dy_src, lat_11_src, lon_11_src, &
                           default_value, water_value, &
                           data_mdl, data_src, isrc, jsrc)

   else

     if (trim(domain_type) == "egrid" .and. search_rad > 0) then

       call interp_aavg_egrid(imdl, jmdl, lat_mdl, lon_mdl, &
                              dx_mdl, dy_mdl, lsmask_mdl, &
                              dx_src, dy_src, lat_11_src, lon_11_src, &
                              centlat_mdl, centlon_mdl,   &
                              default_value, water_value,  &
                              data_mdl, data_src, isrc, jsrc)

     else

       call interp_aavg (imdl, jmdl, lat_mdl, lon_mdl, lsmask_mdl, &
                         dx_src, dy_src, lat_11_src, lon_11_src, search_rad, &
                         default_value, water_value, data_mdl, &
                         data_src, isrc, jsrc)

     end if

   end if

!----------------------------------------------------------------------
! output interpolated data to a grib file.  set kgds array for 
! model grid specs.  reuse the kgds array so that the orig data from
! the gribit module remains unchanged.  the data itself is described
! by its the pds section, so use that with the exception of the
! grid definition (use model's) and gds/bms flag (use model's) 
!----------------------------------------------------------------------

   kgds = kgds_mdl

   kpds(3) = kpds_mdl(3)  ! model grid definition
   kpds(4) = kpds_mdl(4)  ! gds/bms flag

   if (grib_scale_fac > -1) then
     kpds(22) = grib_scale_fac
   end if

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

   if (thinned) then
     call full_to_thin(data_mdl, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (iunit_out, (imdl*jmdl), kpds, kgds, lbms_mdl,  &
               data_mdl, iret)

   if (iret /= 0) then
     print*,"- GRIBBING OF DATA FAILED. IRET IS ", iret
     call abort
   end if

   deallocate (data_src)
   deallocate (data_mdl)
   deallocate (lbms_src)

 enddo MAIN

 call baclose(iunit_src, iret)
 call baclose(iunit_out, iret)

 deallocate (lbms_mdl)
 deallocate (lsmask_mdl)

 return

 end subroutine interp_to_mdl
 