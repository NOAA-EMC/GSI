 subroutine soil_vegtype_tile 
 
 use program_setup, only         : soiltype_tile_file,    &
                                   max_soil_tiles,        &
                                   soil_tile_threshold,   &
                                   default_soil_category, &
                                   num_soil_groups,       &
                                   soil_groups,           &       
                                   vegtype_tile_file,     &
                                   max_veg_tiles,         &
                                   veg_tile_threshold,    &
                                   default_veg_category,  &
                                   num_veg_groups,        &
                                   veg_groups,            &
                                   max_num_categories

 implicit none

 integer                      :: kpds5

! call for soil type

 kpds5 = 224  
 call tile_driver(soiltype_tile_file, max_num_categories, &
                  max_soil_tiles,          &
                  soil_tile_threshold, default_soil_category, &
                  num_soil_groups, soil_groups, kpds5)

! call for veg type

 kpds5 = 225  
 call tile_driver(vegtype_tile_file, max_num_categories, &
                  max_veg_tiles,          &
                  veg_tile_threshold, default_veg_category, &
                  num_veg_groups, veg_groups, kpds5)

 return
 
 end subroutine soil_vegtype_tile

 subroutine tile_driver(source_file, max_num_categories,  &
                        max_tiles, tile_threshold, default_category, &
                        num_groups, groups_dum, kpds5)

 use gribit,        only        :  kpds_mdl, &
                                   kgds_mdl
 
 use program_setup, only        :  resol_mdl,   &
                                   imdl, &
                                   jmdl, &
                                   remaining_tot_tiles, &
                                   thinned,             &
                                   lonsperlat_mdl

 use lsmask_orog,  only         :  lsmask,       &
                                   lbms_lnd_mdl

 use read_write_utils, only     :  full_to_thin

 use calc_latlons, only         :  lat_mdl,lon_mdl

 include 'mpif.h'

 implicit none

 character*9                    :: fngrib
 character*150                  :: source_file

 integer                        :: cat, count_land
 integer, intent(in)            :: default_category
 integer, allocatable           :: dominate_cat(:)
 integer, intent(in)            :: groups_dum(max_num_categories)
 integer, allocatable           :: groups(:)
 integer*8, parameter           :: header_bytes = 48
 integer, allocatable           :: idum(:,:), idum_all(:,:)
 integer, allocatable           :: ijsave(:,:)
 integer*2, allocatable         :: isave(:), jsave(:)
 integer                        :: isrc, jsrc
 integer                        :: iret, iunit
 integer                        :: i, j, ij
 integer                        :: jstart_task, jend_task
 integer                        :: kgds(200)
 integer                        :: kpds(200)
 integer, intent(in)            :: kpds5
 integer                        :: lugb
 integer, intent(in)            :: max_num_categories
 integer, intent(in)            :: max_tiles
 integer, allocatable           :: max_tiles_grid(:)
 integer                        :: myrank, nprocs, num_bytes
 integer                        :: num_categories
 integer, intent(in)            :: num_groups
 integer, allocatable           :: num_tiles(:)        
 integer*8                      :: offset 
 integer, allocatable           :: remaining_tot_tiles_task(:)
 integer*1,allocatable          :: srcdat(:,:)
 integer                        :: water_category

 real                           :: dlon, dlat, deltalat
 real, allocatable              :: dummy(:,:)
 real, allocatable              :: dummy_all(:,:)
 real, allocatable              :: lat_mdl_task(:), lon_mdl_task(:)
 real                           :: lon_11, lat_11
 real                           :: maxlat, minlat
 real                           :: maxlat_task, minlat_task
 real*4, allocatable            :: prcnt_each_cat(:,:)                     
 real*4, allocatable            :: prcnt_each_group(:,:)                      
 real, intent(in)               :: tile_threshold

 type tile_data
   sequence
   integer :: category
   real*4  :: percent
 end type tile_data

 type(tile_data), allocatable   :: tiles(:,:)                      

 if (len_trim(source_file) == 0) return

 print*,"- INTERPOLATE SOURCE DATA TO MODEL GRID"

!-----------------------------------------------------------------------
! open high-res source data.  note: file must be opened on all tasks.
! note: source data must be a global lat/lon grid.  also, source file
! must be in the following common format:
!
! bytes 1-4   - i dimension of grid (integer*4)
! bytes 5-8   - j dimension of grid (integer*4)
! bytes 9-16  - n/s resolution in degrees (real*8)
! bytes 17-24 - e/w resolution in degrees (real*8)
! bytes 25-32 - longitude of pixel (1,1) (real*8)
! bytes 33-40 - latitude of pixel (1,1) (real*8)
! bytes 41-44 - water category (integer*4) - only one water cat allowed
! bytes 45-48 - number of categories (integer*4)
! bytes 49-...- the global data (integer*1)
!-----------------------------------------------------------------------

 print*,"- READ SOURCE DATA: ", trim(source_file)

 iunit = 15

 call mpi_file_open(mpi_comm_world, source_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IERR IS ', iret
   call mpi_abort(mpi_comm_world, 1, iret)
 endif

 offset = 0
 call mpi_file_read_at(iunit, offset, isrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 4
 call mpi_file_read_at(iunit, offset, jsrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 8
 call mpi_file_read_at(iunit, offset, dlon, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 16
 call mpi_file_read_at(iunit, offset, dlat, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 24
 call mpi_file_read_at(iunit, offset, lon_11, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 32
 call mpi_file_read_at(iunit, offset, lat_11, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 40
 call mpi_file_read_at(iunit, offset, water_category, 1, &
                        mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 offset = 44
 call mpi_file_read_at(iunit, offset, num_categories, 1, &
                        mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

!-----------------------------------------------------------------------
! sort model land points by latitude bands based on the max/min
! lat on the grid and the number of mpi tasks.
!-----------------------------------------------------------------------

 call mpi_comm_rank(mpi_comm_world, myrank, iret)
 call mpi_comm_size(mpi_comm_world, nprocs, iret)

 maxlat = -90.0
 minlat = 90.0
 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask(i,j) > 0.0) then
     maxlat = max(maxlat, lat_mdl(i,j))
     minlat = min(minlat, lat_mdl(i,j))
   endif
 enddo
 enddo

 deltalat= (maxlat - minlat) / float(nprocs)

 minlat_task = minlat + float(myrank)*deltalat
 maxlat_task = minlat + float(myrank+1)*deltalat

 if ( (myrank+1) == nprocs )  maxlat_task = maxlat + .01

 count_land = 0
 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask(i,j) > 0.0) then
     if (lat_mdl(i,j) >= minlat_task .and. & 
         lat_mdl(i,j) <= maxlat_task) then
       count_land = count_land + 1
     endif
   endif
 enddo
 enddo

!-----------------------------------------------------------------------
! for each latitude band of model land points, read the corresponding
! band of high-res data with a 2 degree cushion to the north and
! south.  2 degrees may not be enough for grids coarser than t62.
! although rare, there is the possibility a task has no land points.
! in that case, there is no reason to read the high-res data or
! do any processing.
!-----------------------------------------------------------------------

 if (count_land > 0) then

   if (dlat < 0.0) then
     jstart_task      = nint((maxlat_task + 2.0 - lat_11) / dlat + 1.0)
     jend_task        = nint((minlat_task - 2.0 - lat_11) / dlat + 1.0)
   else
     jstart_task      = nint((minlat_task - 2.0 - lat_11) / dlat + 1.0)
     jend_task        = nint((maxlat_task + 2.0 - lat_11) / dlat + 1.0)
   endif

   jstart_task = max(jstart_task,1)
   jend_task = min(jend_task, jsrc)

!cggg
   print*,'jstart/jend ',jstart_task,jend_task

   allocate (srcdat(isrc,jstart_task:jend_task))

   offset    = isrc*(jstart_task-1) + header_bytes
   num_bytes = isrc * ( jend_task - jstart_task + 1 )

   call mpi_file_read_at(iunit, offset, srcdat, num_bytes, &
                         mpi_integer1, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000

   allocate (lat_mdl_task(count_land))
   allocate (lon_mdl_task(count_land))
   allocate (ijsave(imdl,jmdl))
   allocate (isave(count_land))
   allocate (jsave(count_land))
   allocate (remaining_tot_tiles_task(count_land))

   isave = -999
   jsave = -999
   ijsave = -999

!-----------------------------------------------------------------------
!  save information about each model land point for each task.
!-----------------------------------------------------------------------

   count_land = 0
   do j = 1, jmdl
   do i = 1, imdl
     if (lsmask(i,j) > 0.0) then
       if (lat_mdl(i,j) >= minlat_task .and. & 
           lat_mdl(i,j) <= maxlat_task) then
         count_land = count_land + 1
         lat_mdl_task(count_land) = lat_mdl(i,j)
         lon_mdl_task(count_land) = lon_mdl(i,j)
         ijsave(i,j) = count_land
         isave(count_land) = i
         jsave(count_land) = j
         remaining_tot_tiles_task(count_land) = remaining_tot_tiles(i,j)
       endif
     endif
   enddo
   enddo

!-----------------------------------------------------------------------
!  call routine to determine the percentage of each soil type
!  and dominate type.
!-----------------------------------------------------------------------

   allocate (dominate_cat (count_land))
   dominate_cat     = 0

   allocate (prcnt_each_cat (count_land,num_categories))
   prcnt_each_cat   = 0.0

   allocate (prcnt_each_group (count_land,num_groups))
   prcnt_each_group = 0.0

   allocate (groups (num_categories))
   groups(1:num_categories)=groups_dum(1:num_categories)

   call interp_tiles(lon_11, lat_11, &
                    srcdat, isrc, dlon, dlat, &
                    num_categories, num_groups, &
                    water_category, default_category, &
                    tile_threshold, groups, &
                    prcnt_each_cat, prcnt_each_group,  &
                    dominate_cat, &
                    count_land, ijsave, isave, jsave, lat_mdl_task, &
                    lon_mdl_task, jstart_task, jend_task)

   deallocate (srcdat)
   deallocate (ijsave)
   deallocate (lat_mdl_task, lon_mdl_task)

!-----------------------------------------------------------------------
! now determine the soil tiles.  need to limit the tiles at
! each grid point to be no more than what the user specifies for 
! soil type.  also, constrain tiles to be no more than the
! running total of all tiles for all fields (variable remaining_tot_tiles).
!-----------------------------------------------------------------------

   allocate ( max_tiles_grid(count_land) )
 
   max_tiles_grid = max_tiles

   do ij = 1, count_land
     if (remaining_tot_tiles_task(ij) < max_tiles) then
        max_tiles_grid(ij) = remaining_tot_tiles_task(ij)
     endif
   enddo

!-----------------------------------------------------------------------
!  choose tiles.
!-----------------------------------------------------------------------

   allocate (tiles(count_land,max_tiles))
   tiles%percent    = 0.0
   tiles%category   = 0

   allocate (num_tiles (count_land))
   num_tiles        = 0

   call calc_tiles(count_land, dominate_cat, &
                 prcnt_each_cat, prcnt_each_group, &
                 num_groups, num_categories, &
                 groups, max_tiles, &
                 max_tiles_grid, remaining_tot_tiles_task, &
                 tiles, num_tiles)

   deallocate ( groups )
   deallocate ( max_tiles_grid )
   deallocate ( prcnt_each_group )

 endif  ! if count land > 0

!-----------------------------------------------------------------------
! recall: total tiles is multiplicative.  i.e., two veg tiles and
! three soil tiles is six total tiles.
!-----------------------------------------------------------------------

 allocate (idum(imdl,jmdl))

 idum = 0
 if (count_land > 0) then
   do ij = 1, count_land
     idum(isave(ij),jsave(ij)) = remaining_tot_tiles_task(ij)
   enddo
   deallocate (remaining_tot_tiles_task)
 end if  

 call mpi_allreduce(idum, remaining_tot_tiles, (imdl*jmdl), mpi_integer,  &
                    mpi_max, 0, mpi_comm_world, iret)

 deallocate (idum)
 
!-----------------------------------------------------------------------
! output tile data in grib format
!-----------------------------------------------------------------------

 kgds = kgds_mdl
 kpds = kpds_mdl

 if (kpds5 == 224) then
   lugb   = 22
   fngrib = "./fort.22"
 elseif (kpds5 == 225) then
   lugb   = 23
   fngrib = "./fort.23"
 endif

 if (myrank == 0) then
   call baopenw(lugb,fngrib,iret)
   if (iret /= 0) then
     print*,"BAD OPEN OF OUTPUT GRIB FILE: ", trim(fngrib), " IRET IS ", iret
     call mpi_abort(mpi_comm_world, 1, iret)
   end if
 end if

 allocate (idum(imdl,jmdl))
 allocate (idum_all(imdl,jmdl))
 allocate (dummy_all(imdl,jmdl))

 idum = 0
 if (count_land > 0) then
   do ij = 1, count_land
     idum(isave(ij),jsave(ij)) = dominate_cat(ij)
   enddo
   deallocate (dominate_cat)
 endif

 call mpi_reduce(idum, idum_all, (imdl*jmdl), mpi_integer,  &
                 mpi_max, 0, mpi_comm_world, iret)
 
!-----------------------------------------------------------------------
! dominate category.
!-----------------------------------------------------------------------

 if (myrank == 0) then

   kpds(22) = 0
   kpds(5)  = kpds5

   dummy_all = float(idum_all)

   if (thinned) then
     call full_to_thin(dummy_all, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
               dummy_all, iret)

   if (iret /= 0) then
     print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
     call mpi_abort(mpi_comm_world, 1, iret)
   end if

 end if

!-----------------------------------------------------------------------
! if the number of soil tiles chosen is greater than one, write
! out the tile information. otherwise close the file.
!-----------------------------------------------------------------------

 MULTIPLE_TILES : if (max_tiles > 1) then

   kpds(5)  = kpds(5) + 1
   kpds(22) = 0

   idum = 0
   if (count_land > 0) then
     do ij = 1, count_land
       idum(isave(ij),jsave(ij)) = num_tiles(ij)
     enddo
   end if

   call mpi_reduce(idum, idum_all, (imdl*jmdl), mpi_integer,  &
                   mpi_max, 0, mpi_comm_world, iret)

   if (myrank == 0) then

     dummy_all = float(idum_all)

     if (thinned) then
       call full_to_thin(dummy_all, imdl, jmdl, lonsperlat_mdl)
     end if

     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy_all, iret)

     if (iret /= 0) then
       print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if

   end if ! rank 0

!-----------------------------------------------------------------------
! each tile's category and percent coverage.
!-----------------------------------------------------------------------

   allocate (dummy(imdl,jmdl))

   do i = 1, max_tiles

     kpds(5)  = kpds(5) + 1
     kpds(22) = 0

     idum = 0
     if (count_land > 0) then
       do ij = 1, count_land
         idum(isave(ij),jsave(ij)) = tiles(ij,i)%category
       enddo
     endif

     call mpi_reduce(idum, idum_all, (imdl*jmdl), mpi_integer,  &
                     mpi_max, 0, mpi_comm_world, iret)

     if (myrank == 0) then

       dummy_all = float(idum_all)

       if (thinned) then
         call full_to_thin(dummy_all, imdl, jmdl, lonsperlat_mdl)
       end if

       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dummy_all, iret)

       if (iret /= 0) then
         print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
         call mpi_abort(mpi_comm_world, 1, iret)
       end if

    end if  ! rank 0

    kpds(5)  = kpds(5) + 1
    kpds(22) = 1

    dummy = 0.0
    if (count_land > 0) then
      do ij = 1, count_land
        dummy(isave(ij),jsave(ij)) = tiles(ij,i)%percent
      enddo
    endif

    call mpi_reduce(dummy, dummy_all, (imdl*jmdl), mpi_double_precision,  &
                    mpi_max, 0, mpi_comm_world, iret)

    if (myrank == 0) then

      if (thinned) then
       call full_to_thin(dummy_all, imdl, jmdl, lonsperlat_mdl)
      end if

      call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                  dummy_all, iret)

      if (iret /= 0) then
        print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
        call mpi_abort(mpi_comm_world, 1, iret)
      end if

   end if ! rank 0

 enddo

 if (allocated (tiles)) deallocate (tiles)

!-----------------------------------------------------------------------
! the percent of each category.
!-----------------------------------------------------------------------

 do cat = 1,num_categories

   kpds(5)  = kpds(5) + 1
   kpds(22) = 1

   dummy = 0.0
   if (count_land > 0) then
     do ij = 1, count_land
       dummy(isave(ij),jsave(ij)) = prcnt_each_cat(ij,cat)
     enddo
   endif

   call mpi_reduce(dummy, dummy_all, (imdl*jmdl), mpi_double_precision,  &
                   mpi_max, 0, mpi_comm_world, iret)

   if (myrank == 0) then

     if (thinned) then
       call full_to_thin(dummy_all, imdl, jmdl, lonsperlat_mdl)
     end if
 
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy_all, iret)

     if (iret /= 0) then
       print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if

   end if ! rank 0

 enddo

 deallocate (dummy)

 end if MULTIPLE_TILES

 if (myrank == 0)  call baclose(lugb, iret)
 call mpi_file_close(iunit, iret)

 deallocate (dummy_all)
 deallocate (idum, idum_all)

 if (allocated (isave))               deallocate (isave)
 if (allocated (jsave))               deallocate (jsave)

 if (allocated (prcnt_each_cat))  deallocate (prcnt_each_cat)
 if (allocated (num_tiles))       deallocate (num_tiles)

 return 

 9000 print*,'- BAD READ OF SOURCE FILE, IERR IS ', iret
 call mpi_abort(mpi_comm_world, 1, iret)

 end subroutine tile_driver
