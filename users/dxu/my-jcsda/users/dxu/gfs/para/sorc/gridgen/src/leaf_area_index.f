 subroutine leaf_area_index

 use calc_latlons, only             : lat_mdl,            &
                                      lon_mdl
 
 use lsmask_orog, only              : lsmask,             &
                                      lbms_lnd_mdl

 use program_setup, only            : domain_type,        &
                                      dx_mdl,             &
                                      dy_mdl,             &
                                      imdl,               &
                                      jmdl,               &
                                      centlat_mdl,        &
                                      centlon_mdl,        &
                                      leaf_area_idx_file, &
                                      thinned,            &
                                      lonsperlat_mdl

 use gribit, only                   : kgds_mdl,       &
                                      kpds_mdl

 use ll2xy_utils, only              : ll2xy_egrid

 use read_write_utils, only         : full_to_thin

 include 'mpif.h'

 implicit none

 character*9             :: output_file

 integer, allocatable    :: count(:,:), count_all_tasks(:,:)
 integer                 :: i, j, ii, jj, p
 integer, parameter      :: isrc=43200
 integer*2, allocatable  :: i_wrt_mdl_grid(:,:), j_wrt_mdl_grid(:,:)
 integer                 :: ierr, iunit, iunit_out, myrank, nprocs, nret
 integer                 :: is, ie, js, je
 integer                 :: iend, istart, jstart, jend
 integer                 :: jsrc, jsrc_task_approx
 integer                 :: jstart_task, jend_task, jsrc_task, num_bytes
 integer                 :: isearch, jsearch, iii, jjj, krad
 integer                 :: itmp(1), jtmp(1), nearest_i, nearest_j
 integer                 :: kgds(200), kpds(200), jgds(200), jpds(200)
 integer*1, allocatable  :: lai_src(:,:)
 integer*8               :: offset
 integer                 :: month(46), day(46)

 logical                 :: gfs

 real, parameter         :: dlon_src   = 1.0 / 120.0      
 real, parameter         :: dlat_src   = -(1.0 / 120.0)
 real, parameter         :: lon_11_src = -180.0 + (dlon_src*0.5) ! lon point (1,1)  
 real, parameter         :: lat_11_src = 90.0 + (dlat_src*0.5)   ! lat point (1,1)

 real, allocatable       :: dum(:), ypts(:)
 real, allocatable       :: lai(:,:)
 real, allocatable       :: lats_src(:),lons_src(:)
 real                    :: maxlat, maxlon, minlat, minlon
 real                    :: srclat(1), srclon(1)
 real, allocatable       :: sum_lai(:,:), sum_lai_all_tasks(:,:)

! there are 45 eight-day periods and 1 five-day period.  they are:
! 
! jan 1-8,    jan 9-16,    jan 17-24,    jan 25-feb 1,
! feb 2-9,    feb 10-17,   feb 18-25,    feb 26-mar 5,
! mar 6-13,   mar 14-21,   mar 22-29,    mar 30-apr 6,
! apr 7-14,   apr 15-22,   apr 23-30,    may 1-8,
! may 9-16,   may 17-24,   may 25-jun 1, jun 2-9,
! jun 10-17,  jun 18-25,   jun 26-jul 3, jul 4-11,
! jul 12-19,  jul 20-27,   jul 28-aug 4, aug 5-12,
! aug 13-20,  aug 21-28,   aug 29-sep 5, sep 6-13,
! sep 14-21,  sep 22-29,   sep 30-oct 7, oct 8-15,
! oct 16-23,  oct 24-31,   nov 1-8,      nov 9-16,
! nov 17-24,  nov 25-dec2, dec 3-10,     dec 11-18,
! dec 19-26,  dec 27-31
!

 data month /1,1,1,1,2,2,2,3,3,3,3,4,4,4,4,  &
             5,5,5,5,6,6,6,6,7,7,7,8,8,8,8,  &
             9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12/

 data day /5,13,21,29,6,14,22,2,10,18,26,3,11,19,27,   &
           4,13,21,29,6,14,22,30,8,16,24,1,9,17,25,    &
           2,10,18,26,4,12,20,28,5,13,21,29,7,15,23,29/  

 if (len_trim(leaf_area_idx_file) == 0) return

 print*,"- PROCESS LEAF AREA INDEX"

 call mpi_comm_rank(mpi_comm_world, myrank, ierr)
 call mpi_comm_size(mpi_comm_world, nprocs, ierr)

!----------------------------------------------------------------------
! determine bounds of model grid within the 30-sec source dat
!----------------------------------------------------------------------

! note, there does not appear to be any valid data north of
! 80 N or south of 60S.  so, only read in and process 
! rows 1200 thru 18000

 gfs = trim(domain_type) == "gaussian"

 if (gfs) then
   iend=isrc
   istart=1
   jstart=1200
   jend=18000
 else  ! regional grids
   maxlat = min(maxval(lat_mdl) + 1.0,90.0)    ! use a 1-deg cushion
   minlat = max(minval(lat_mdl) - 1.0,-90.0)
   maxlon = min(maxval(lon_mdl) + 1.0,180.0)
   minlon = max(minval(lon_mdl) - 1.0,-180.0)

! note the method for determining istart/iend are crude
! and will not be efficient for low latitude regional grids that
! cross the dateline.  but right now, we don't run many
! of those type of grids. may want to revisit this someday.

   iend        = int((maxlon - lon_11_src) / dlon_src + 1.0)
   istart      = nint((minlon - lon_11_src) / dlon_src + 1.0)
   jstart      = nint((maxlat - lat_11_src) / dlat_src + 1.0)
   jstart      = max(jstart, 1200)
   jend        = nint((minlat - lat_11_src) / dlat_src + 1.0)
   jend        = min(jend, 18000)
 end if

 jsrc = jend-jstart+1

 jsrc_task_approx = jsrc/nprocs

 jstart_task = myrank*jsrc_task_approx + jstart
 jend_task   = jstart_task + jsrc_task_approx - 1

 if ( (myrank+1) == nprocs) then
   jend_task = jend
 endif

 jsrc_task = jend_task-jstart_task + 1

 print*,'minlon/maxlon ',minlon,maxlon
 print*,'myrank/istart/iend/jstart_task,jend_task: ',   &
         myrank, istart, iend, jstart_task, jend_task

!----------------------------------------------------------------------
! for each source grid point, store the nearest i/j on model grid.
!----------------------------------------------------------------------

 allocate (i_wrt_mdl_grid(isrc,jstart_task:jend_task))
 i_wrt_mdl_grid=-9999
 allocate (j_wrt_mdl_grid(isrc,jstart_task:jend_task))
 j_wrt_mdl_grid=-9999

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
       i_wrt_mdl_grid(i,j)=itmp(1)
       j_wrt_mdl_grid(i,j)=jtmp(1)
     endif
   enddo
   enddo
 endif

 allocate (lai_src(isrc,jstart_task:jend_task))

 allocate(lai(imdl,jmdl))
 allocate(sum_lai(imdl,jmdl))
 allocate(count(imdl,jmdl))
 allocate(count_all_tasks(imdl,jmdl))
 allocate(sum_lai_all_tasks(imdl,jmdl))

! open grib file for interpolated data.

 if (myrank == 0) then
   output_file="./fort.73"
   iunit_out = 73
   print*,'- OPEN OUTPUT FILE ', trim(output_file)
   call baopenw(iunit_out, output_file, ierr)
   if (ierr /= 0) then
     print*,'- BAD OPEN, IERR IS ', ierr
     call mpi_abort(mpi_comm_world, 1, ierr)
   end if
 end if

 PERIOD : do p = 1, 1

 print*,'period is: ', p

 iunit=9

 print*,'- OPEN AND READ SOURCE FILE FOR LAI: ',trim(leaf_area_idx_file)
 call mpi_file_open(mpi_comm_world, leaf_area_idx_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'- BAD OPEN, IERR IS ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif

 offset=isrc*(jstart_task-1)
 num_bytes = isrc * ( jend_task - jstart_task + 1 )
 call mpi_file_read_at(iunit, offset, lai_src, num_bytes, &
                       mpi_integer1, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'- BAD READ, IERR IS ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 call mpi_file_close(iunit, ierr)

 sum_lai = 0.0
 count   = 0

 do j = jstart_task,jend_task
 do i = istart, iend
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= -9999 .and. jj /= -9999) then  ! is source data outside of  model grid?
! need to reduce the number of flags in raw data so i don't have
! to ask so many questions.
       if (lai_src(i,j) >= 0 .and. lai_src(i,j) <=70) then
         sum_lai(ii,jj) = sum_lai(ii,jj) + float(lai_src(i,j))*0.1
         count(ii,jj) = count(ii,jj) + 1       
       elseif (lai_src(i,j) == -3) then  ! barren
         count(ii,jj) = count(ii,jj) + 1       ! treat as zero
       elseif (lai_src(i,j) == -4) then  ! snow/ice
         count(ii,jj) = count(ii,jj) + 1       ! treat as zero
       elseif (lai_src(i,j) == -5) then  ! wetland
         count(ii,jj) = count(ii,jj) + 1       ! treat as zero
       elseif (lai_src(i,j) == -6) then  ! urban
         count(ii,jj) = count(ii,jj) + 1       ! treat as zero
       end if
   endif
 enddo
 enddo

 call mpi_reduce(count, count_all_tasks, (imdl*jmdl), mpi_integer, &
                 mpi_sum, 0, mpi_comm_world, ierr)
 call mpi_reduce(sum_lai, sum_lai_all_tasks, (imdl*jmdl),    &
                 mpi_double_precision, &
                 mpi_sum, 0, mpi_comm_world, ierr)

 if (myrank == 0) then

 do j = 1, jmdl
 ILOOP : do i = 1, imdl
   if (lsmask(i,j) > 0.0) then
     if (count_all_tasks(i,j) > 0) then
       lai(i,j) = sum_lai_all_tasks(i,j) / float(count_all_tasks(i,j))
     else
! no raw data over antarctica, or north of 80 N, so always set to zero?
       if (lat_mdl(i,j) <= -59.9 .or. lat_mdl(i,j) >= 79.9) then
         lai(i,j) = 0.0
       else
         SPIRAL_SEARCH : do krad = 1, 4
           is = i - krad
           ie = i + krad
           js = j - krad
           je = j + krad
           do jj = js, je
           do ii = is, ie
!  search only along outer square.
           if ((jj .eq. js) .or. (jj .eq. je) .or.   &
               (ii .eq. is) .or. (ii .eq. ie))  then
              if (jj > jmdl .or. jj < 1) then
                cycle
              else
                jjj = jj
              endif
              if (ii <= 0) then
                if (.not. gfs) cycle
                iii = imdl + ii
              else if (ii >= (imdl+1)) then
                if (.not. gfs) cycle
                iii = ii - imdl
              else
                iii = ii
              end if
              if (count_all_tasks(iii,jjj) > 0) then
                lai(i,j) = sum_lai_all_tasks(iii,jjj) / float(count_all_tasks(iii,jjj))
                print*,'- MISSING LAI AT: ',i,j,' SET TO ',iii,jjj,lai(i,j)
                cycle ILOOP
              end if
           endif   ! outer square
           enddo   ! ii
           enddo   ! jj
         enddo SPIRAL_SEARCH
         print*,'- SEARCH FAILED, USE DEFAULT AT: ',i,j,lat_mdl(i,j),lon_mdl(i,j), &
                 ii,jj
         lai(i,j) = 1.0   ! default value
       endif  ! if near pole
     endif  ! if count_task > 0 
   end if  ! if a land point
 enddo ILOOP
 enddo

! grib data

 kgds = kgds_mdl

 kpds=kpds_mdl

 kpds(5)  = 182 ! lai
 kpds(8)  = 1   ! 2001
 kpds(9)  = month(p)
 kpds(10) = day(p)
 kpds(11) = 0
 kpds(12) = 0
 kpds(13) = 2   ! fcst time unit - day
 kpds(14) = 0   ! analysis
 kpds(15) = 8   ! 8 day averages
 kpds(16) = 51  ! climo
 kpds(17) = 5
 kpds(21) = 21  ! century
 kpds(22) = 2   ! decimal scale factor

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

   if (thinned) then
     call full_to_thin(lai, imdl, jmdl, lonsperlat_mdl)
   end if

   call putgb (iunit_out, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,  &
               lai, ierr)

   if (ierr /= 0) then
     print*,"- GRIBBING OF DATA FAILED. IERR IS ", ierr
     call mpi_abort(mpi_comm_world, 1, ierr)
   end if

 end if

 call mpi_barrier(mpi_comm_world, ierr)   ! is this necessary?

 enddo PERIOD

 if (myrank == 0) then
   call baclose(iunit_out, ierr)
 endif

 deallocate (lai_src, lai, sum_lai, sum_lai_all_tasks, count, count_all_tasks)
 deallocate (i_wrt_mdl_grid, j_wrt_mdl_grid)

 return

 end subroutine leaf_area_index
