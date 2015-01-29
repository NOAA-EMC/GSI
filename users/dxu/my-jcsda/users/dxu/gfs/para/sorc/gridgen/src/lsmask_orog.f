 module lsmask_orog

 use program_setup, only            : max_orog_tiles,         &
                                      orog_bin_width,         &
                                      orog_tile_threshold,    &
                                      smooth,                 &
                                      num_smooth_passes,      &
                                      lsmask_tiles,           &
                                      lsmask_tile_threshold,  &
                                      domain_name,            &
                                      domain_type,            &
                                      imdl,                   &
                                      jmdl,                   &
                                      dx_mdl,                 &
                                      dy_mdl,                 &
                                      centlat_mdl,            &
                                      centlon_mdl,            &
                                      resol_mdl,              &
                                      remaining_tot_tiles,    &
                                      lsmask_file,            &
                                      orog_file,              &
                                      roughness_file,         &
                                      lonsperlat_mdl,         &
                                      thinned

 use calc_latlons, only             : lat_mdl,        &
                                      lon_mdl,        &
                                      lon_vpnts_mdl,  &
                                      lat_vpnts_mdl

 use gribit, only                   : kpds_mdl,      &
                                      kgds_mdl

 use read_write_utils, only         : full_to_thin

 integer*1, allocatable            :: num_orog_tiles(:,:)

 integer, parameter                :: orog_tile_missing = -999.0

 logical*1, allocatable            :: lbms_lnd_mdl(:,:)
 logical*1, allocatable            :: lbms_wtr_mdl(:,:)

 real, allocatable                 :: lsmask(:,:)   ! decimal % land 
 real, allocatable                 :: orog_tiles_elev(:,:,:)
 real, allocatable                 :: orog_tiles_prcnt(:,:,:)
 real, allocatable                 :: orog_stnd_dev(:,:)
 real, allocatable                 :: wtrmask(:,:)  ! 1.0 - lsmask

 contains

!-----------------------------------------------------------------------
! calc land/sea mask and orography.
!-----------------------------------------------------------------------

 subroutine lsmask_orog_driver(myrank)

 implicit none

 character*20                  :: fname

 integer                       :: cc
 integer                       :: count
 integer                       :: i, j, k
 integer                       :: ier
 integer                       :: ii
 integer, allocatable          :: isave(:)
 integer                       :: jj
 integer                       :: jgds(200),jpds(200)
 integer, allocatable          :: jsave(:)
 integer                       :: kgds(200)
 integer                       :: kpds(200)
 integer                       :: latbnd
 integer                       :: lskip
 integer                       :: lunout
 integer, allocatable          :: max_orog_tiles_grid(:,:)
 integer, intent(in)           :: myrank
 integer                       :: numpts
 integer                       :: tile

 logical*1                     :: lbms_src(imdl,jmdl)
 logical*1, allocatable        :: lbms(:,:)

 real, allocatable             :: dummy(:,:)
 real, allocatable             :: lsmask_rev(:,:)

 print*,"- CALCULATE LAND/SEA MASK AND OROGRAPHY"

 allocate (lsmask(imdl,jmdl))
 lsmask = 0.0

 allocate (wtrmask(imdl,jmdl))
 wtrmask = 0.0

 allocate (num_orog_tiles(imdl,jmdl))
 allocate (orog_stnd_dev(imdl,jmdl))
 allocate (orog_tiles_elev(imdl,jmdl,max_orog_tiles))
 allocate (orog_tiles_prcnt(imdl,jmdl,max_orog_tiles))

 num_orog_tiles   = 0
 orog_tiles_elev  = orog_tile_missing
 orog_tiles_prcnt = 0.
 orog_stnd_dev    = 0.

!-----------------------------------------------------------------------
! if user specifies a land mask and orog grib file in the namelist,
! read the data and use it to map the surface fields.  otherwise,
! let this program calculate the mask and orography.
!-----------------------------------------------------------------------

 USE_EXT: if ( (index(lsmask_file, ".grb ") > 0) .and.   &
               (index(orog_file, ".grb ") > 0)  ) then
   print*,"- GET TERRAIN AND LAND MASK FROM EXISTING FILES."
   print*,"- OPEN INPUT FILE ", trim(lsmask_file)
   call baopenr (99, lsmask_file, ier)
   if (ier /= 0) then
     print*,'- BAD OPEN, IRET IS ', ier
     call abort
   end if
   print*,"- DEGRIB DATA "
   jgds    = -1
   jpds    = -1
   jpds(5) = 81
   lskip   = -1
   call getgb(99, 0, (imdl*jmdl), -1, jpds, jgds, &
              numpts, lskip, kpds, kgds, lbms_src, lsmask, ier)
   if (ier /= 0) then
     print*,"- BAD DEGRIB OF DATA. IRET IS ", ier
     call abort
   end if
   call baclose(99, ier)
!-----------------------------------------------------------------------
!  will not use tiles since production data is not tiled.
!-----------------------------------------------------------------------
   num_orog_tiles   = 1  
   orog_tiles_prcnt(:,:,1) = 1.0
   print*,"- OPEN INPUT FILE ", trim(orog_file)
   call baopenr (99, orog_file, ier)
   if (ier /= 0) then
     print*,'- BAD OPEN, IRET IS ', ier
     call abort
   end if
   print*,"- DEGRIB TERRAIN HEIGHT DATA"
   jgds    = -1
   jpds    = -1
   jpds(5) =  8  ! param number of terrain height.
   lskip   = -1
   call getgb(99, 0, (imdl*jmdl), -1, jpds, jgds, &
              numpts, lskip, kpds, kgds, lbms_src, orog_tiles_elev(1,1,1), ier)
   if (ier /= 0) then
     print*,"- BAD DEGRIB OF DATA. IRET IS ", ier
     call abort
   end if
!-----------------------------------------------------------------------
! if the nmm formulation of roughess length is chosen, need to
! read in the standard deviation of terrain.
!-----------------------------------------------------------------------
   if (trim(roughness_file) == "nmm") then
     print*,"- DEGRIB TERRAIN HEIGHT STANDARD DEVIATION DATA"
     jgds    = -1
     jpds    = -1
     jpds(5) =  9   ! param number of std dev of terrain height
     lskip   = -1
     call getgb(99, 0, (imdl*jmdl), -1, jpds, jgds, &
                numpts, lskip, kpds, kgds, lbms_src, orog_stnd_dev, ier)
     if (ier /= 0) then
       print*,"- BAD DEGRIB OF DATA. IRET IS ", ier
       call abort
     end if
   end if
   call baclose(99, ier)

 else

!-----------------------------------------------------------------------
! compute the land mask and orography.
!-----------------------------------------------------------------------

  call calc_lsmask_orog(myrank)

!-----------------------------------------------------------------------
! test code to remove isolated inland lakes for nmm.
! note, don't do this with tiling.
!-----------------------------------------------------------------------

 LAKES_SMOOTH : if (domain_type == "egrid") then

   print*,"- REMOVE SMALL LAKES."
   if (max_orog_tiles > 1) then
     print*,'lake removal/smoothing code does not work with tiling'
     stop
   end if

   do j = 2, jmdl-1
     do i = 2, imdl-1
       if (mod(j,2) == 0) then                ! even row
         if (lsmask(i,j) == 0.0 .and.    &
             lsmask(i,j-1) == 1.0 .and.  &
             lsmask(i,j+1) == 1.0 .and.  &
             lsmask(i+1,j-1) == 1.0 .and. &
             lsmask(i+1,j+1) == 1.0 ) then
             print*,'remove isolated lake at ',i,j
             lsmask(i,j) = 1.0
         end if
       else                                   ! odd row
         if (lsmask(i,j) == 0.0 .and.    &
             lsmask(i-1,j-1) == 1.0 .and.  &
             lsmask(i-1,j+1) == 1.0 .and.  &
             lsmask(i,j-1) == 1.0 .and. &
             lsmask(i,j+1) == 1.0 ) then
             print*,'remove isolated lake at ',i,j
             lsmask(i,j) = 1.0
         end if
       end if
     enddo
   enddo

   if (smooth == 1) then   ! peak chopping

!----------------------------------------------------------------------
! smdhld utility requires an integer land mask as 0-land, 1-water.
! NOTE: does not work with tiling.  
!----------------------------------------------------------------------

     print*,"- SMOOTH TERRAIN BY PEAK CHOPPING."
     allocate(lsmask_rev(imdl,jmdl))
     lsmask_rev = 0.0
     where(lsmask == 0.0) lsmask_rev = 1.0
     call smdhld(imdl, jmdl, orog_tiles_elev(1,1,1), lsmask_rev, 12, num_smooth_passes)
     deallocate(lsmask_rev)

   elseif (smooth == 2) then

     print*,"- SMOOTH TERRAIN BY SMOOTHER/DESMOOTHER."
     call smth_desmth_egrid(orog_tiles_elev, lsmask, 1, imdl, 1, jmdl, 1, 1, num_smooth_passes, 1) 

     allocate(lsmask_rev(imdl,jmdl))
     lsmask_rev = 0.0
     where(lsmask == 0.0) lsmask_rev = 1.0
     call smdhld(imdl, jmdl, orog_tiles_elev(1,1,1), lsmask_rev, 12, 0)  ! boundaries only
     deallocate(lsmask_rev)

   endif ! smooth algorithm

! -4-point averaging of mountains along inner boundary-------
! taken from wrf si

   do i=1,imdl-1
     orog_tiles_elev(i,2,1)=0.25*(orog_tiles_elev(i,1,1) + orog_tiles_elev(i+1,1,1) +  &
                                  orog_tiles_elev(i,3,1) + orog_tiles_elev(i+1,3,1))
   enddo

   do i=1,imdl-1
     orog_tiles_elev(i,jmdl-1,1)=0.25*(orog_tiles_elev(i,jmdl-2,1) + & 
                               orog_tiles_elev(i+1,jmdl-2,1) +  &
                               orog_tiles_elev(i,jmdl,1) + orog_tiles_elev(i+1,jmdl,1))
   enddo

   do j=4,jmdl-3,2
      orog_tiles_elev(1,j,1)=0.25*(orog_tiles_elev(1,j-1,1)+orog_tiles_elev(2,j-1,1)+  &
                             orog_tiles_elev(1,j+1,1)+orog_tiles_elev(2,j+1,1))
   enddo

   do j=4,jmdl-3,2
      orog_tiles_elev(imdl,j,1)=0.25*(orog_tiles_elev(imdl-1,j-1,1)+orog_tiles_elev(imdl,j-1,1)+  &
                                orog_tiles_elev(imdl-1,j+1,1)+orog_tiles_elev(imdl,j+1,1))
   enddo

!----------------------------
!  remove waterfalls
!----------------------------

   do k = 1, 5
   do j = 2, jmdl-1
     do i = 2, imdl-1
       if (mod(j,2) == 0) then                ! even row
         if (lsmask(i,j) == 0.0) then   
           if(lsmask(i,j-1) == 0.0) orog_tiles_elev(i,j,1) = &
                                    min(orog_tiles_elev(i,j,1),orog_tiles_elev(i,j-1,1))
           if(lsmask(i,j+1) == 0.0) orog_tiles_elev(i,j,1) = &
                                    min(orog_tiles_elev(i,j,1),orog_tiles_elev(i,j+1,1))
           if(lsmask(i+1,j-1) == 0.0) orog_tiles_elev(i,j,1) = &
                                      min(orog_tiles_elev(i,j,1),orog_tiles_elev(i+1,j-1,1))
           if(lsmask(i+1,j+1) == 0.0) orog_tiles_elev(i,j,1) = &
                                      min(orog_tiles_elev(i,j,1),orog_tiles_elev(i+1,j+1,1))
         end if
       else                                   ! odd row
         if (lsmask(i,j) == 0.0) then
           if(lsmask(i-1,j-1) == 0.0) orog_tiles_elev(i,j,1) = &
                                      min(orog_tiles_elev(i,j,1),orog_tiles_elev(i-1,j-1,1))
           if(lsmask(i-1,j+1) == 0.0) orog_tiles_elev(i,j,1) = &
                                      min(orog_tiles_elev(i,j,1),orog_tiles_elev(i-1,j+1,1))
           if(lsmask(i,j-1) == 0.0) orog_tiles_elev(i,j,1) = &
                                    min(orog_tiles_elev(i,j,1),orog_tiles_elev(i,j-1,1))
           if(lsmask(i,j+1) == 0.0) orog_tiles_elev(i,j,1) = &
                                    min(orog_tiles_elev(i,j,1),orog_tiles_elev(i,j+1,1))
         end if
       end if
     enddo
   enddo
   enddo

 end if LAKES_SMOOTH

 end if USE_EXT

!-----------------------------------------------------------------------
! if running a global grid in thinned mode, fudge the land mask
! so that this program ignores the same points as the model.
!-----------------------------------------------------------------------

 if (thinned) then
   allocate (dummy(imdl,jmdl))
   dummy = lsmask
   call thin_mask(dummy, imdl, jmdl, lonsperlat_mdl, 0., lsmask)
   call thin_mask(dummy, imdl, jmdl, lonsperlat_mdl, 1., wtrmask)
   deallocate (dummy)
   wtrmask = 1.0 - wtrmask   ! set water to "1" to fool interpolation
                             ! routines to act at non-land points.
 else
   wtrmask = 1.0 - lsmask    ! set water to "1" to fool interpolation
                             ! routines to act at non-land points.
 end if

!-----------------------------------------------------------------------
! we restrict the total number of all land-related tile types
! (orog, veg, soil), so keep a running count of the remaining tiles.
!-----------------------------------------------------------------------

 do j = 1, jmdl
   do i = 1, imdl
     if (lsmask(i,j) > 0.0) then
       remaining_tot_tiles(i,j) = remaining_tot_tiles(i,j) / num_orog_tiles(i,j)
       remaining_tot_tiles(i,j) = max(remaining_tot_tiles(i,j),1)
     end if
   enddo
 enddo

!-----------------------------------------------------------------------
! grib data.
!-----------------------------------------------------------------------

 if (myrank == 0) then

 lunout = 56
 fname  = "fort.56"

 call baopenw(lunout, fname, ier)

 kpds = kpds_mdl
 kgds = kgds_mdl

 kpds(5)  = 81    ! parameter number for land/sea mask
 kpds(22) = 2     ! scaling factor
 kpds(4)  = 128   ! don't use bitmap

 allocate(lbms(imdl,jmdl))
 allocate(dummy(imdl,jmdl))

 dummy = lsmask

 if (thinned) then
   call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
 end if

 lbms     = .false.   ! don't use bitmap

 call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms,  &
             dummy, ier)

 if (ier /= 0) then
   print*,"- ERROR GRIBBING LAND/SEA MASK, IER IS ", ier
   stop
 end if

!-----------------------------------------------------------------------
! now grib orography.
!-----------------------------------------------------------------------

 lunout = 64
 fname  = "fort.64"

 call baopenw(lunout, fname, ier)

!-----------------------------------------------------------------------
! if one tile is selected, simply output the mean terrain.
!-----------------------------------------------------------------------

 MULTIPLE_TILES : if (max_orog_tiles == 1) then

   kpds(22) = 1  ! scaling factor
   kpds(5)  = 8  ! parameter number of terrain height
   dummy    = orog_tiles_elev(:,:,1)

   if (thinned) then
     call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
               dummy, ier)

   if (ier /= 0) then
     print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
     stop
   end if

!-----------------------------------------------------------------------
! output stnd dev of orog.  don't deallocate array.  it may be
! use later in the roughness length calculation.
!-----------------------------------------------------------------------

   kpds(5)  = 9
   dummy    = orog_stnd_dev

   if (thinned) then
     call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
               dummy, ier)

   if (ier /= 0) then
     print*,"- ERROR GRIBBING STND DEV OF OROG, IER IS ", ier
     stop
   end if

 else ! more than one tile

   kpds(5)  = 81  ! need to ask nco how to grib tiles.
   kpds(22) = 0

   dummy = float(num_orog_tiles)

   if (thinned) then
     call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms,  &
               dummy, ier)

   if (ier /= 0) then
     print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
     stop
   end if

   do tile = 1, max_orog_tiles

     kpds(22) = 1
     kpds(5)  = kpds(5) + 1
     dummy    = orog_tiles_elev(:,:,tile)

     if (thinned) then
       call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
     end if

     call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                 dummy, ier)

     if (ier /= 0) then
       print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
       stop
     end if

     kpds(22) = 3
     kpds(5)  = kpds(5) + 1
     dummy    = orog_tiles_prcnt(:,:,tile)

     if (thinned) then
       call full_to_thin(dummy, imdl, jmdl, lonsperlat_mdl)
     end if

     call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                 dummy, ier)

     if (ier /= 0) then
       print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
       stop
     end if

   enddo

 endif MULTIPLE_TILES

 deallocate (dummy)
 deallocate (lbms)

 call baclose(lunout, ier)

 end if  ! rank is zero

!-----------------------------------------------------------------------
! create a logical version of the land/sea mask for later use for
! gribbing fields that are only valid for land or water points.
!-----------------------------------------------------------------------

 allocate(dummy(imdl,jmdl))
 dummy = lsmask

 if (thinned) then
   call full_to_thin (dummy, imdl, jmdl, lonsperlat_mdl)
 end if

 allocate (lbms_lnd_mdl(imdl,jmdl))

 lbms_lnd_mdl = .false.

 do j = 1, jmdl
   do i = 1, imdl
     if (dummy(i,j) > 0.0) then
       lbms_lnd_mdl(i,j) = .true.
     end if
   enddo
 enddo

 dummy = wtrmask

 if (thinned) then
   call full_to_thin (dummy, imdl, jmdl, lonsperlat_mdl)
 end if

 allocate (lbms_wtr_mdl(imdl,jmdl))

 lbms_wtr_mdl = .false.

 do j = 1, jmdl
   do i = 1, imdl
     if (dummy(i,j) > 0.0) then
       lbms_wtr_mdl(i,j) = .true.
     end if
   enddo
 enddo

 deallocate (dummy)

 return

 end subroutine lsmask_orog_driver

!-----------------------------------------------------------------------
! determine mask and orog
!-----------------------------------------------------------------------

 subroutine calc_lsmask_orog(myrank)

 use ll2xy_utils, only    : ll2xy_egrid

 include 'mpif.h'

 integer                 :: calc_tiles
 integer, allocatable    :: count(:,:), count_ocean(:,:)
 integer, allocatable    :: count_all_tasks(:,:), count_ocean_all_tasks(:,:)
 integer, allocatable    :: count_tile(:,:,:), count_tile_all_tasks(:,:,:)
 integer, parameter      :: isrc=43200
 integer*2, allocatable  :: i_wrt_mdl_grid(:,:), j_wrt_mdl_grid(:,:)
 integer*1, allocatable  :: mask_src(:,:)
 integer                 :: iend, istart, jend, jend_task
 integer                 :: jstart, jstart_task, jsrc, jsrc_task, jsrc_task_approx
 integer                 :: i, ii, j, jj, nret
 integer                 :: ierr, iunit, nprocs
 integer, intent(in)     :: myrank
 integer                 :: itmp(1), jtmp(1), nearest_i, nearest_j
 integer, allocatable    :: num_tiles(:,:)
 integer*8               :: offset
 integer                 :: tile, total_tile
 integer*2, allocatable  :: topo_src(:,:)
 integer, parameter      :: water_flag=0  ! use 16 for usgs landuse data

 real, allocatable       :: bin_ranges(:,:,:,:)
 real                    :: bin_width
 real                    :: dlat_src, dlon_src, elev
 real                    :: lat_11_src, lon_11_src
 real, allocatable       :: lats_src(:),lons_src(:)
 real, allocatable       :: dum(:), ypts(:)
 real                    :: maxlat, maxlon, minlat, minlon
 real                    :: max_spread, percent
 real                    :: srclat(1), srclon(1)
 real, allocatable       :: sum_mask(:,:), sum_topo(:,:)
 real, allocatable       :: sum_tile(:,:,:), sum_tile_all_tasks(:,:,:)
 real, allocatable       :: sum_mask_all_tasks(:,:), sum_topo_all_tasks(:,:)
 real, allocatable       :: topo(:,:), topo_max(:,:), topo_min(:,:)
 real, allocatable       :: topo_max_all_tasks(:,:), topo_min_all_tasks(:,:)

 implicit none

 call mpi_comm_size(mpi_comm_world, nprocs, ierr)

!----------------------------------------------------------------------
! determine bounds of model grid within the 30-sec source data.
! use a 1-degree cushion.  terrain and mask source data are
! on the same grid.
!----------------------------------------------------------------------

 if (trim(domain_type) == "gaussian") then
   iend=isrc
   istart=1
   jstart=1
   jend=21600
 else  ! regional grids
   maxlat = min(maxval(lat_mdl) + 1.0,90.0)
   minlat = max(minval(lat_mdl) - 1.0,-90.0)
   maxlon = min(maxval(lon_mdl) + 1.0,180.0)
   minlon = max(minval(lon_mdl) - 1.0,-180.0)

   dlon_src   = 1.0 / 120.0
   dlat_src   = -(1.0 / 120.0)
   lon_11_src = -180.0 + (dlon_src*0.5)  ! lat point 1,1
   lat_11_src = 90.0 + (dlat_src*0.5)    ! lon point 1,1

! note the method for determining istart/iend are crude
! and will not be efficient for low latitude regional grids that
! cross the dateline.  but right now, we don't run many
! of those type of grids. may want to revisit this someday.

   iend        = int((maxlon - lon_11_src) / dlon_src + 1.0)
   istart      = nint((minlon - lon_11_src) / dlon_src + 1.0)
   jstart      = nint((maxlat - lat_11_src) / dlat_src + 1.0)
   jend        = nint((minlat - lat_11_src) / dlat_src + 1.0)
   jend        = min(jend, 21600)
 end if

 jsrc = jend-jstart+1

 jsrc_task_approx = jsrc/nprocs

 jstart_task = myrank*jsrc_task_approx + jstart
 jend_task   = jstart_task + jsrc_task_approx - 1

 if ( (myrank+1) == nprocs) then
   jend_task = jend
 endif

 jsrc_task = jend_task-jstart_task + 1

!----------------------------------------------------------------------
! open and read landuse and terrain data
!----------------------------------------------------------------------

 allocate (mask_src(isrc,jstart_task:jend_task))
 iunit=9
 print*,'- OPEN AND READ SOURCE FILE FOR MASK: ',trim(lsmask_file)
 call mpi_file_open(mpi_comm_world, lsmask_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'bad open'
   stop
 endif
 offset=isrc*(jstart_task-1)
 call mpi_file_read_at(iunit, offset, mask_src, isrc*jsrc_task, &
                       mpi_integer1, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'bad read'
   stop
 endif
 call mpi_file_close(iunit, ierr)

 print*,'- OPEN AND READ SOURCE FILE FOR OROGRAPHY: ',trim(orog_file)
 allocate (topo_src(isrc,jstart_task:jend_task))
 iunit=34
 call mpi_file_open(mpi_comm_world, orog_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'bad open'
   stop
 endif
 offset=2*isrc*(jstart_task-1)
 call mpi_file_read_at(iunit, offset, topo_src, isrc*jsrc_task, &
                       mpi_integer2, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'bad read'
   stop
 endif
 call mpi_file_close(iunit, ierr)

!----------------------------------------------------------------------
! for each source grid point, store the nearest i/j on model grid.
! done for efficiency.
!----------------------------------------------------------------------

 allocate (i_wrt_mdl_grid(isrc,jstart_task:jend_task))
 i_wrt_mdl_grid=-9999
 allocate (j_wrt_mdl_grid(isrc,jstart_task:jend_task))
 j_wrt_mdl_grid=-9999

 allocate(sum_mask(imdl,jmdl))
 sum_mask=0.
 allocate(count(imdl,jmdl))
 count=0
 allocate(sum_topo(imdl,jmdl))
 sum_topo=0.
 allocate(count_ocean(imdl,jmdl))
 count_ocean=0

 if (trim(domain_type) == "gaussian" .or.  &
     trim(domain_type) == "latlon") then  
   if (trim(domain_type) == "gaussian") then 
     allocate (lats_src(jstart_task:jend_task))
     allocate (lons_src(jstart_task:jend_task))
     lons_src = 0.0
     do j = jstart_task, jend_task
       lats_src(j) = lat_11_src + (j-1)*dlat_src
     enddo
     allocate (ypts(jstart_task:jend_task))
     allocate (dum(jstart_task:jend_task))
     call gdswiz(kgds_mdl,-1,jsrc_task,-999.9,dum,ypts,lons_src,lats_src, &
                 nret, 0, dum, dum)
     deallocate (dum, lons_src, lats_src)
   else  ! global grid
     allocate (ypts(jstart_task:jend_task))
     allocate (lats_src(1))
     do j = jstart_task, jend_task
       lats_src(1) = lat_11_src + (j-1)*dlat_src
       ypts(j) = (lat_mdl(1,1) - lats_src(1)) / dx_mdl + 1.0
     enddo
     deallocate(lats_src)
   endif

   do j = jstart_task,jend_task
     nearest_j = nint(ypts(j))
     do i = istart, iend
       srclon(1) = lon_11_src + (i-1)*dlon_src
       nearest_i = nint(srclon(1) / dx_mdl + 1.0)  ! assumes point 1 
                                                   ! centered on greenwich
       if (nearest_i > imdl) then
         nearest_i = nearest_i - imdl
       else if (nearest_i < 1) then
         nearest_i = nearest_i + imdl
       end if

       if (nearest_j >= 1 .and. nearest_j <= jmdl) then
         i_wrt_mdl_grid(i,j)=nearest_i
         j_wrt_mdl_grid(i,j)=nearest_j
         if (mask_src(i,j) /= water_flag) then
           sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
         end if
         if (topo_src(i,j) /= -9999) then ! non-ocean point
           sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                                 float(topo_src(i,j))
         endif
         if (topo_src(i,j) == -9999) then
           count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
         endif
         count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
       end if
     enddo
   enddo
   deallocate (ypts)
 else  ! e-grid
   do j = jstart_task,jend_task
     srclat(1) = lat_11_src + (j-1)*dlat_src
   do i = istart, iend
     srclon(1) = lon_11_src + (i-1)*dlon_src
     call ll2xy_egrid(srclat, srclon, imdl, jmdl, &
                      centlat_mdl, centlon_mdl,   &
                      -(dx_mdl), dy_mdl, &
                      1, 1, itmp, jtmp)
     if (itmp(1) >= 1 .and. itmp(1) <= imdl .and. &
         jtmp(1) >= 1 .and. jtmp(1) <= jmdl) then
       nearest_i = itmp(1)
       nearest_j = jtmp(1)
       i_wrt_mdl_grid(i,j)=nearest_i
       j_wrt_mdl_grid(i,j)=nearest_j
       if (mask_src(i,j) /= water_flag) then
         sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
       end if
       if (topo_src(i,j) /= -9999) then ! non-ocean point
         sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                               float(topo_src(i,j))
       endif
       if (topo_src(i,j) == -9999) then
         count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
       endif
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 endif
 deallocate(mask_src)

 allocate(count_all_tasks(imdl,jmdl))
 call mpi_allreduce(count, count_all_tasks, (imdl*jmdl), mpi_integer, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 deallocate(count)
 allocate(sum_mask_all_tasks(imdl,jmdl))
 call mpi_allreduce(sum_mask, sum_mask_all_tasks, (imdl*jmdl), mpi_double_precision, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 deallocate(sum_mask)
 allocate(sum_topo_all_tasks(imdl,jmdl))
 call mpi_allreduce(sum_topo, sum_topo_all_tasks, (imdl*jmdl), mpi_double_precision, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 allocate(count_ocean_all_tasks(imdl,jmdl))
 call mpi_allreduce(count_ocean, count_ocean_all_tasks, (imdl*jmdl), mpi_integer, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 deallocate(count_ocean)

 allocate(topo(imdl,jmdl))

 do j = 1, jmdl
  do i = 1, imdl
    lsmask(i,j)=sum_mask_all_tasks(i,j)/float(count_all_tasks(i,j))
    if (.not. lsmask_tiles) then
      lsmask(i,j) = nint(lsmask(i,j))
    else
      if (lsmask(i,j) >= (1.0-lsmask_tile_threshold)) then
        lsmask(i,j) = 1.0
      elseif (lsmask(i,j) <= lsmask_tile_threshold) then
        lsmask(i,j) = 0.0
      end if
    endif
    topo(i,j)=sum_topo_all_tasks(i,j)/float(count_all_tasks(i,j))
    if (lsmask(i,j) == 0.0) then
      if (count_ocean_all_tasks(i,j) > 0) topo(i,j) = 0.0
    end if
  enddo
 enddo

 deallocate(sum_mask_all_tasks, count_ocean_all_tasks)

 sum_topo=0.
 do j = jstart_task,jend_task
 do i = istart, iend
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= -9999 .and. jj /= -9999) then
     if (topo_src(i,j) /= -9999) then
       sum_topo(ii,jj)= sum_topo(ii,jj) + (topo(ii,jj) - topo_src(i,j))**2
     else  ! source grid is ocean
       sum_topo(ii,jj) = sum_topo(ii,jj) + topo(ii,jj)**2
     endif
   endif
 enddo
 enddo

 call mpi_allreduce(sum_topo, sum_topo_all_tasks, (imdl*jmdl), mpi_double_precision, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 deallocate(sum_topo)

 do j = 1, jmdl
  do i = 1, imdl
    if (lsmask(i,j) == 0.0) then
      orog_stnd_dev(i,j)=0.0
    else
      orog_stnd_dev(i,j) = sqrt( sum_topo_all_tasks(i,j)/float(count_all_tasks(i,j)) )
    endif
  enddo
 enddo
 deallocate(count_all_tasks, sum_topo_all_tasks)

 TILING: if (max_orog_tiles == 1) then
   print*,"- ONLY ONE OROGRAPHY TILE SELECTED"
   num_orog_tiles = 1
   orog_tiles_elev(:,:,1) = topo
   orog_tiles_prcnt(:,:,1) = 1.
   deallocate(topo, topo_src, i_wrt_mdl_grid, j_wrt_mdl_grid)
   return
 else   ! find orography tiles
   print*,"- MULTIPLE OROGRAPHY TILES SELECTED"
   allocate(topo_max(imdl,jmdl))
   allocate(topo_min(imdl,jmdl))
   topo_max=-99999.
   topo_min=99999.
   do j = jstart_task,jend_task
   do i = istart, iend
     ii = i_wrt_mdl_grid(i,j)
     jj = j_wrt_mdl_grid(i,j)
     if (ii /= -9999 .and. jj /= -9999) then
       if (topo_src(i,j) /= -9999) then
         topo_max(ii,jj) = max(topo_max(ii,jj),float(topo_src(i,j)))
         topo_min(ii,jj) = min(topo_min(ii,jj),float(topo_src(i,j)))
       else  ! source grid is ocean
         topo_max(ii,jj) = max(topo_max(ii,jj),0.0)
         topo_min(ii,jj) = min(topo_min(ii,jj),0.0)
       endif
     endif
   enddo
   enddo
   allocate(topo_max_all_tasks(imdl,jmdl))
   call mpi_allreduce(topo_max, topo_max_all_tasks, (imdl*jmdl), mpi_double_precision, &
                      mpi_max, 0, mpi_comm_world, ierr)
   deallocate(topo_max)
   allocate(topo_min_all_tasks(imdl,jmdl))
   call mpi_allreduce(topo_min, topo_min_all_tasks, (imdl*jmdl), mpi_double_precision, &
                      mpi_min, 0, mpi_comm_world, ierr)
   deallocate(topo_min)
! set up bin ranges
   allocate(num_tiles(imdl,jmdl))
   allocate(bin_ranges(imdl,jmdl,max_orog_tiles,2))
   num_tiles=1
   bin_ranges=0.
   do j=1,jmdl
     do i=1,imdl
       if(lsmask(i,j) > 0.0) then
!vic method
!         max_spread = max( (topo_max_all_tasks(i,j)-topo(i,j)),  &
!                           (topo(i,j)-topo_min_all_tasks(i,j)) )
!         calc_tiles = 2.0 * max_spread / orog_bin_width
         max_spread = topo_max_all_tasks(i,j)-topo_min_all_tasks(i,j)
         calc_tiles = max_spread / orog_bin_width
         if (calc_tiles > max_orog_tiles) then
           num_tiles(i,j) = max_orog_tiles
         else if (calc_tiles < 1) then
           num_tiles(i,j) = 1
         else
           num_tiles(i,j) = calc_tiles
         end if
!vic method
!         bin_width = 2.0 * max_spread / float(num_tiles(i,j))
!         do tile = 1, num_tiles(i,j)
!           bin_ranges(i,j,tile,1) = topo(i,j) - max_spread +       &
!                                 (bin_width * float((tile-1)))
!           bin_ranges(i,j,tile,2) = topo(i,j) - max_spread +       &
!                                 (bin_width * float(tile))
!         enddo
         bin_width = max_spread / float(num_tiles(i,j))
         do tile = 1, num_tiles(i,j)
           bin_ranges(i,j,tile,1) = topo_min_all_tasks(i,j) +       &
                                 (bin_width * float((tile-1)))
           bin_ranges(i,j,tile,2) = topo_min_all_tasks(i,j) +       &
                                 (bin_width * float(tile))
         enddo
         bin_ranges(i,j,1,1)              = bin_ranges(i,j,1,1) - 0.01
         bin_ranges(i,j,num_tiles(i,j),2) = bin_ranges(i,j,num_tiles(i,j),2) + 0.01
       endif
     enddo
   enddo
 end if TILING

 deallocate(topo_max_all_tasks, topo_min_all_tasks)

 allocate(sum_tile(imdl,jmdl,max_orog_tiles))
 allocate(count_tile(imdl,jmdl,max_orog_tiles))

 sum_tile=0.
 count_tile=0
 do j = jstart_task,jend_task
 do i = istart, iend
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= -9999 .and. jj /= -9999) then
     if (lsmask(ii,jj) > 0.0) then
       if (topo_src(i,j) /= -9999) then
         elev=float(topo_src(i,j))
       else
         elev=0.
       endif
       do tile = 1, num_tiles(ii,jj)
         if(elev >= bin_ranges(ii,jj,tile,1)   .and.   &
            elev <  bin_ranges(ii,jj,tile,2) ) then
            count_tile(ii,jj,tile) = count_tile(ii,jj,tile) + 1
            sum_tile(ii,jj,tile)  = sum_tile(ii,jj,tile) + elev
         end if
       enddo
     endif
   endif
 enddo
 enddo

 deallocate(topo_src)
 deallocate(bin_ranges)

 allocate(sum_tile_all_tasks(imdl,jmdl,max_orog_tiles))
 call mpi_allreduce(sum_tile, sum_tile_all_tasks, (imdl*jmdl*max_orog_tiles), &
                 mpi_double_precision, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 deallocate(sum_tile)
 allocate(count_tile_all_tasks(imdl,jmdl,max_orog_tiles))
 call mpi_allreduce(count_tile, count_tile_all_tasks, (imdl*jmdl*max_orog_tiles), &
                 mpi_integer, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 deallocate(count_tile)

!----------------------------------------------------------------------
! the user can select a percent threshold, below which he does
! not want a tile. merge the tiles below the threshold with an 
! adjacent bin.
!----------------------------------------------------------------------

 do j=1,jmdl
   do i=1,imdl
     if(lsmask(i,j) > 0.0) then
       total_tile=0
       do tile=1, num_tiles(i,j)
         total_tile = total_tile + count_tile_all_tasks(i,j,tile)
       enddo
       do tile = 1, (num_tiles(i,j)-1)
         percent= float(count_tile_all_tasks(i,j,tile)) / &
                  float(total_tile)
         if (percent > 0.0 .and. percent < orog_tile_threshold) then
           count_tile_all_tasks(i,j,tile+1) = count_tile_all_tasks(i,j,tile+1) + &
                                        count_tile_all_tasks(i,j,tile)
           count_tile_all_tasks(i,j,tile) = 0.0
           sum_tile_all_tasks(i,j,tile+1) = sum_tile_all_tasks(i,j,tile+1) + &
                                      sum_tile_all_tasks(i,j,tile)
           sum_tile_all_tasks(i,j,tile) = 0.0
         end if
       enddo
       do tile = num_tiles(i,j), 2, -1
         percent= float(count_tile_all_tasks(i,j,tile)) / &
                  float(total_tile)
         if (percent > 0.0 .and. percent < orog_tile_threshold) then
           count_tile_all_tasks(i,j,tile-1) = count_tile_all_tasks(i,j,tile-1) + &
                                        count_tile_all_tasks(i,j,tile)
           count_tile_all_tasks(i,j,tile) = 0.0
           sum_tile_all_tasks(i,j,tile-1) = sum_tile_all_tasks(i,j,tile-1) + &
                                      sum_tile_all_tasks(i,j,tile)
           sum_tile_all_tasks(i,j,tile) = 0.0
         end if
       enddo
     endif
   enddo
 enddo

 do j = 1, jmdl
   do i = 1, imdl
     if(lsmask(i,j) > 0.0) then
       total_tile=0
       do tile=1, num_tiles(i,j)
         total_tile = total_tile + count_tile_all_tasks(i,j,tile)
       enddo
       do tile=1, num_tiles(i,j)
         if (count_tile_all_tasks(i,j,tile) > 0) then
           num_orog_tiles(i,j) = num_orog_tiles(i,j) + 1
           orog_tiles_prcnt(i,j,num_orog_tiles(i,j))=  &
                                       float(count_tile_all_tasks(i,j,tile)) / &
                                       float(total_tile)
           orog_tiles_elev(i,j,num_orog_tiles(i,j))= sum_tile_all_tasks(i,j,tile) / &
                                       float(count_tile_all_tasks(i,j,tile))
         endif
       enddo
     else
       num_orog_tiles(i,j) = 1
       orog_tiles_prcnt(i,j,1) =  1.0
       orog_tiles_elev(i,j,1) = topo(i,j)
     endif
   enddo
 enddo

 deallocate(num_tiles, topo, sum_tile_all_tasks, count_tile_all_tasks)

 return

 end subroutine calc_lsmask_orog

!-----------------------------------------------------------------------
! free up memory
!-----------------------------------------------------------------------

 subroutine lsmask_orog_cleanup

 implicit none

 if (allocated(lsmask))           deallocate(lsmask)
 if (allocated(wtrmask))          deallocate(wtrmask)
 if (allocated(lbms_wtr_mdl))     deallocate(lbms_wtr_mdl)
 if (allocated(lbms_lnd_mdl))     deallocate(lbms_lnd_mdl)
 if (allocated(orog_tiles_elev))  deallocate(orog_tiles_elev)
 if (allocated(orog_tiles_prcnt)) deallocate(orog_tiles_prcnt)
 if (allocated(num_orog_tiles))   deallocate(num_orog_tiles)

 return 

 end subroutine lsmask_orog_cleanup

!-----------------------------------------------------------------------
! the gfs runs on the thinned or reduced grid.  mimic this behaviour
! by setting the land mask so that the interpolation routines ignore
! the same points the model does.
!
! based on gfs routine intlon.
!-----------------------------------------------------------------------

 subroutine thin_mask(fin, idim, jdim, lonsperlat, flag, fout)

 implicit none

 integer                     :: i, in, i2, j, jj
 integer, intent(in)         :: idim
 integer, intent(in)         :: jdim
 integer                     :: latd2
 integer, intent(in)         :: lonsperlat(jdim/2)
 integer                     :: m1, m2

 real, intent(in)            :: fin(idim,jdim)
 real, intent(in)            :: flag
 real, intent(out)           :: fout(idim,jdim)
 real                        :: r, x1

 latd2 = jdim / 2

 fout = flag  ! turn off interpolation routines at
              ! all points.  set to "0" for a land mask
              ! and to "1" for a water mask.

 do j = 1, jdim
   jj = j
   if (j .gt. latd2) jj = jdim - j + 1
   m2 = lonsperlat(jj)
   m1 = idim
   r  = float(m1) / float(m2)
   do i2=1,m2
     x1=(i2-1)*r
     in=mod(nint(x1),m1)+1
     fout(in,j) = fin(in,j)  ! add processing at these points.
   enddo
 enddo
 return

 end subroutine thin_mask

 end module lsmask_orog
