 subroutine soil_substrate

 use gribit, only               : kpds_mdl, &
                                  kgds_mdl

 use program_setup, only        : max_orog_tiles, &
                                  resol_mdl, &
                                  imdl, &
                                  jmdl, &
                                  substrate_temp_file,  &
                                  thinned,   & 
                                  lonsperlat_mdl,  &
                                  domain_type

 use calc_latlons,  only        : lat_mdl, &
                                  lon_mdl

 use lsmask_orog,  only         : lsmask,    &
                                  lbms_lnd_mdl,  &
                                  num_orog_tiles, &
                                  orog_tiles_prcnt, &
                                  orog_tiles_elev,  &
                                  orog_tile_missing

 use interp_utils, only         : interp_aavg,     &
                                  interp_bilinear

 use read_write_utils, only     : full_to_thin

 implicit none

 real, parameter               :: hgt_1000mb   = 110.9
 real, parameter               :: lapse        = -6.5E-03

 character*150                 :: fngrib

 integer                       :: bit1, bit2
 integer                       :: bitmap
 integer                       :: count
 integer                       :: gds
 integer                       :: i, j, jj, jjj, n
 integer                       :: iret
 integer                       :: isrc
 integer                       :: istat
 integer, parameter            :: iunit_src = 40
 integer                       :: jgds_src(200)
 integer                       :: jpds_src(200)
 integer                       :: jsrc  
 integer                       :: kgds(200)
 integer                       :: kgds_src(200)
 integer                       :: kpds(200)
 integer                       :: kpds_src(200)
 integer                       :: lskip
 integer                       :: lugb
 integer, parameter            :: lugi = 0    ! grib index file unit number - not used
 integer                       :: message_num
 integer                       :: nearest_j
 integer                       :: numbytes
 integer                       :: numpts
 integer                       :: search_rad
 integer                       :: tile

 logical                       :: bilinear
 logical*1, allocatable        :: lbms_src(:,:)

 real, allocatable             :: dummy(:,:)
 real                          :: dx_src
 real                          :: dy_src
 real                          :: lat_11_src
 real                          :: lon_11_src
 real, allocatable             :: soilt_avg(:)
 real, allocatable             :: soilt_mdl(:,:)
 real, allocatable             :: soilt_src(:,:)
 real, allocatable             :: soilt_tiles(:,:,:)
 real                          :: sum
 real                          :: tavg
 real                          :: undefined_value
 real                          :: water_value
 real                          :: zlevel

!-----------------------------------------------------------------------
! execution starts here.
!-----------------------------------------------------------------------

 if (len_trim(substrate_temp_file) == 0) return

 print*,"- INTERPOLATE SUBSTRATE TEMPERATURE DATA TO MODEL GRID"

 print*,"- OPEN FILE ", trim(substrate_temp_file)
 call baopenr (iunit_src, substrate_temp_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   call abort
 end if

!-----------------------------------------------------------------------
! set lskip to read from beginning AND clean out index file.
! don't use a zero if you are reading multiple files
! using the same unit number.
! note: lskip get incremented by one after the call to getgb.
!-----------------------------------------------------------------------

 lskip    = -1
 jgds_src = -1
 jpds_src = -1
 kgds_src = -1
 kpds_src = -1

 print*,"- GET GRIB HEADER"

 call getgbh(iunit_src, lugi, lskip, jpds_src, jgds_src, numbytes,  &
             numpts, message_num, kpds_src, kgds_src, iret)

 if (iret /= 0) then
   print*,'- BAD READ OF GRIB HEADER. IRET IS ', iret
   call abort
 end if

!-----------------------------------------------------------------------
! source data must have a grid description section.
! source grid must be lat/lon data.  interp routines can't handle
! other projections right now.
!-----------------------------------------------------------------------

 gds = kpds_src(4) / 128

 if (gds /= 1) then
   print*,'source data does not have grid description section'
   stop
 elseif (kgds_src(1) /= 0) then
   print*,'source data must be lat/lon grid'
   stop
 end if

!-----------------------------------------------------------------------
! grid dimensions of source grid.
!-----------------------------------------------------------------------

 isrc = kgds_src(2)
 jsrc = kgds_src(3)

 allocate(soilt_src(isrc,jsrc))
 allocate(lbms_src(isrc,jsrc))

 call getgb(iunit_src, lugi, (isrc*jsrc), lskip, jpds_src, jgds_src, &
            numpts, lskip, kpds_src, kgds_src, lbms_src, soilt_src, iret)

 if (iret /= 0) then
   print*,"- BAD DEGRIB OF DATA. IRET IS ", iret
   call abort
 end if

!-----------------------------------------------------------------------
! determine lat/lon of source grid's corner point.
! determine grid resolution in the "x" and "y" directions.
! these are used in the interpolation routines.
!-----------------------------------------------------------------------

  lat_11_src = float(kgds_src(4))  / 1000.0
  lon_11_src = float(kgds_src(5))  / 1000.0
  dx_src     = float(kgds_src(9))  / 1000.0
  dy_src     = float(kgds_src(10)) / 1000.0

!-----------------------------------------------------------------------
! determine whether to do a bi-linear or area average interpolation
! approach based on the resolutions of the model and source grids.
!-----------------------------------------------------------------------

 if ( resol_mdl <= dx_src ) then
   print*,'- WILL USE BI-LINEAR INTERPOLATION '
   bilinear = .true.
 else
   search_rad = int ( (resol_mdl * 0.5) / dx_src )
   if (search_rad == 0) print*,'- WILL USE NEAREST NEIGHBOR INTERPOLATION'
   if (search_rad > 0)  print*,'- WILL TAKE AREA AVERAGE OF SOURCE DATA'
   bilinear   = .false.

!cggg if we ever get a high res dataset, will want to use the better
!cggg area averaging routine.

   if (search_rad > 0 .and. trim(domain_type) == "egrid") then
     print*,'use better area averaging routine for e grid'
     stop
   end if 

 end if

!-----------------------------------------------------------------------
! determine scanning mode flags and adjust sign of grid
! resolution so interp routines calc the lat/lon on the source
! grid correctly.
!-----------------------------------------------------------------------

 bit1 = kgds_src(11) / 128
 if (bit1 == 1) dx_src = -dx_src
 bit2 = mod(kgds_src(11),128) / 64
 if (bit2 == 0) dy_src = -dy_src

!------------------------------------------------------------------------
! check for the presence of a bit map section.
! (pds section: bit 2, octet 8).  if no bitmap section, can't tell
! what is land or water on the source grid.  this is unfortunate.
! so, set undefined and water values to be a value that forces the
! interpolation routines to consider all data.
!------------------------------------------------------------------------

 bitmap = mod(kpds_src(4),128) / 64

 water_value     = -999.
 undefined_value = -99.

 if (bitmap == 1) then
   where (.not.lbms_src) soilt_src = water_value
 end if

 deallocate (lbms_src)

!------------------------------------------------------------------------
! perform interpolation.
!------------------------------------------------------------------------

 allocate (soilt_mdl(imdl,jmdl))

 if (bilinear) then

   call interp_bilinear (imdl, jmdl, lat_mdl, lon_mdl, lsmask,  &
                         dx_src, dy_src, lat_11_src, lon_11_src, &
                         undefined_value, water_value, &
                         soilt_mdl, soilt_src, isrc, jsrc)

 else

   call interp_aavg (imdl, jmdl, lat_mdl, lon_mdl, lsmask, &
                     dx_src, dy_src, lat_11_src, lon_11_src, search_rad, &
                     undefined_value, water_value, soilt_mdl, &
                     soilt_src, isrc, jsrc)

 end if

!------------------------------------------------------------------------
! check for undefined points as indicated by the flag value. 
! replace with the latitude band average.  search adjacent bands
! if nearest band is undefined.
!------------------------------------------------------------------------

 allocate (soilt_avg(jsrc))  ! latitude band average

 do j = 1, jsrc

   sum   = 0.0
   count = 0

   do i = 1, isrc
     if (soilt_src(i,j) > water_value) then
       count = count + 1
       sum   = sum + soilt_src(i,j)
     end if
   enddo

   if (count > 0) then
     soilt_avg(j) = sum / float(count)
   else
     soilt_avg(j) = 0.0
   end if

 enddo

 deallocate (soilt_src)

 do j = 1, jmdl
   do i = 1, imdl

     UNDEFINED : if (soilt_mdl(i,j) < (undefined_value + 0.001)) then

       nearest_j = nint((lat_mdl(i,j) - lat_11_src) / dy_src + 1.0)
       nearest_j = max(nearest_j,1)
       nearest_j = min(nearest_j,jsrc)

       tavg = soilt_avg(nearest_j)

       if (tavg > 0.0) then

         soilt_mdl(i,j) = tavg

       else
 
         OUTER : do jj = 1, 5
         INNER : do n = -1, 1, 2

           jjj = nearest_j + (jj*n)

           if (jjj < 1 .or. jjj > jsrc) cycle INNER

           tavg = soilt_avg(jjj)

           if (tavg > 0.0) then
             soilt_mdl(i,j) = tavg
             exit OUTER 
           end if

         enddo INNER
         enddo OUTER 

         if (soilt_mdl(i,j) < (undefined_value + 0.001)) then
           print*,'Undefined substrate temperature at point ',i,j
           stop
         end if

       end if
       
     end if UNDEFINED

   enddo
 enddo

 deallocate (soilt_avg)

!-----------------------------------------------------------------------
! adjust substrate temperature for the model terrain height.  this
! code has only been tested with data gribbed as 1000 mb (air temperature)
! and data in the /nwprod/fix directory gribbed as a level below ground.  
! for both cases, set the level of the data in meters msl,
! then adjust the substrate temperature using the standard lapse rate.  
!
! NOTE: normally, data gribbed as a level below ground would imply
! there was some terrain adjustment to whatever source grid was used.
! however, visual inspection of the data in the /nwprod/fix directory
! indicated this data was NOT adjusted.  so for this case, setting
! zlevel to zero should be ok.  for other datasets it might be 
! dangerous.
!-----------------------------------------------------------------------

 if (kpds_src(6) == 100 .and. kpds_src(7) == 1000) then
   zlevel = hgt_1000mb
 else if (kpds_src(6) == 111) then
   zlevel = 0.0
 else
   print*,'Cannot define a level for source data '
   stop
 end if

 allocate (soilt_tiles(imdl,jmdl,max_orog_tiles))

 soilt_tiles = 0.0

 do j = 1, jmdl
   do i = 1, imdl

     if (lsmask(i,j) > 0.0) then

       do tile = 1, max_orog_tiles

         if (orog_tiles_elev(i,j,tile) > orog_tile_missing) then

           soilt_tiles(i,j,tile) = soilt_mdl(i,j) +    &
                (orog_tiles_elev(i,j,tile) - zlevel) * lapse

         end if

       enddo

     end if

   enddo
 enddo

 deallocate (soilt_mdl)

!-----------------------------------------------------------------------
! grib data.
!-----------------------------------------------------------------------

 kpds = kpds_src
 kgds = kgds_mdl

 lugb = 48
 fngrib = "./fort.48"
 call baopenw(lugb,fngrib,iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   call abort
 end if

 kpds(3)  = kpds_mdl(3)
 kpds(4)  = kpds_mdl(4)
 kpds(5)  =  85  ! grib parameter number 
 kpds(6)  = 111  ! level type - below ground level
 kpds(7)  = 800  ! 800 cm below ground level
 kpds(22) =  0   ! scaling factor

 allocate (dummy(imdl,jmdl))

 if (max_orog_tiles == 1) then ! just write out substrate record

   kpds(22) =  3

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

   dummy = soilt_tiles(:,:,1)

   if (thinned) then
     call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
               dummy, iret)

   if (iret /= 0) then
     print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
     call abort
   end if

 else   ! write out all tiles

   dummy = float(num_orog_tiles)

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

   if (thinned) then
     call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
               dummy, iret)

   if (iret /= 0) then
     print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
     call abort
   end if

!cggg increment parameter number for now.  need to resolve
!cggg how to do this properly.

   do tile = 1, max_orog_tiles

     kpds(5)  = kpds(5) + 1
     kpds(22) =  3

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

     dummy = soilt_tiles(:,:,tile)

     if (thinned) then
       call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
     end if

     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy, iret)

     if (iret /= 0) then
       print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
       call abort
     end if

     kpds(5)  = kpds(5) + 1
     kpds(22) =  2

     dummy = orog_tiles_prcnt(:,:,tile)

     if (thinned) then
       call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
     end if

     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy, iret)

     if (iret /= 0) then
       print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
       call abort
     end if

   enddo

 end if  ! multiple tile check

 call baclose(lugb, iret)

 deallocate (soilt_tiles)
 deallocate (dummy)

 return

 end subroutine soil_substrate