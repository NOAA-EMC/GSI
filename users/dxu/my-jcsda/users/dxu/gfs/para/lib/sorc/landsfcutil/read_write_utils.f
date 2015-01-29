 module read_write_utils

 type date
   sequence
   integer    :: year
   integer    :: month
   integer    :: day
 end type date

 contains

!----------------------------------------------------------------------
! the gfs operates on a thinned or reduced grid. (# grid points in
! x-direction decreases toward poles)  when the gfs outputs its data,
! if fills in the thinned points with nearest neighbor data.  this
! routine mimics that process.
!----------------------------------------------------------------------

 subroutine full_to_thin(dummy_full, imdl, jmdl, lonsperlat)

 implicit none

 integer                         :: dummy(imdl*jmdl)
 integer, intent(in)             :: imdl
 integer                         :: ijmdl
 integer, intent(in)             :: jmdl
 integer, intent(in)             :: lonsperlat(jmdl/2)

 real, intent(inout)             :: dummy_full(imdl*jmdl)
 real, allocatable               :: dummy_thinned(:)

 dummy = 0   ! not used

 ijmdl = sum(lonsperlat) * 2    ! number of points, thinned or reduced grid.

 allocate(dummy_thinned(ijmdl))

 call interpred(1,dummy,dummy_full,dummy_thinned,imdl,jmdl,ijmdl,lonsperlat)

 call uninterpred(1, dummy, dummy_thinned, dummy_full, &
                  imdl, jmdl, ijmdl, lonsperlat)

 deallocate (dummy_thinned)

 return

 end subroutine full_to_thin

!-----------------------------------------------------------------------
! time interpolate to a specific day given an array of
! bounding dates.
!-----------------------------------------------------------------------

 subroutine time_interp(data_climo_in, climo_dates, num_dates, &
                        ijmdl, curr_year, curr_month, curr_day, &
                        curr_hour, curr_minute, data_climo_out)

 implicit none

 integer, intent(in)       :: curr_year
 integer, intent(in)       :: curr_month
 integer, intent(in)       :: curr_day
 integer, intent(in)       :: curr_hour
 integer, intent(in)       :: curr_minute
 integer, intent(in)       :: ijmdl
 integer, intent(in)       :: num_dates
 integer                   :: idat(8)
 integer                   :: jdat(8)
 integer                   :: n
 real                      :: ndays           ! # days between date and
                                              ! 2nd bounding rec
 integer                   :: ndays_btwn_recs ! # days between bounding recs
 integer                   :: rec1
 integer                   :: rec2

 real, intent(in)          :: data_climo_in(ijmdl, num_dates)
 real, intent(out)         :: data_climo_out(ijmdl)
 real                      :: rinc(5)
 real                      :: wght1
 real                      :: wght2

 type (date), intent(in)  :: climo_dates(num_dates)

!-----------------------------------------------------------------------
! current date/time for this time step.
!-----------------------------------------------------------------------

 idat    = 0
 idat(1) = curr_year
 idat(2) = curr_month
 idat(3) = curr_day
 idat(5) = curr_hour
 idat(6) = curr_minute

!-----------------------------------------------------------------------
! find grib records that bound the current date.  routine returns
! the number of days between idat and jdat (rinc(1)).
!-----------------------------------------------------------------------

 SEARCH : do n = 1, num_dates

   jdat    = 0
   jdat(1) = curr_year              ! don't use year from climo data
   jdat(2) = climo_dates(n)%month
   jdat(3) = climo_dates(n)%day
   jdat(5) = 0                      ! assume data valid at 00z

   call w3difdat(jdat, idat, 3, rinc)

   if (rinc(3) > 0) exit SEARCH

 enddo SEARCH

 rec2 = n
 rec1 = n - 1

!-----------------------------------------------------------------------
! handle end of year.
!-----------------------------------------------------------------------

 if (rec1 == 0)           rec1 = num_dates
 if (rec2 == num_dates+1) rec2 = 1

 if (rec2 /= 1) then

!-----------------------------------------------------------------------
! num of minutes between current day and second bounding record was
! calculated above.  convert to decimal number of days.
!-----------------------------------------------------------------------

   ndays = rinc(3) / 1440.0

!-----------------------------------------------------------------------
!  now calculate number of days between bounding records.
!-----------------------------------------------------------------------

   idat    = 0
   idat(1) = curr_year
   idat(2) = climo_dates(rec1)%month
   idat(3) = climo_dates(rec1)%day

   jdat    = 0
   jdat(1) = curr_year
   jdat(2) = climo_dates(rec2)%month
   jdat(3) = climo_dates(rec2)%day

   call w3difdat(jdat, idat, 1, rinc)

   ndays_btwn_recs = rinc(1)

 else  ! rec2 = 1

!-----------------------------------------------------------------------
!  bounding records span two years.
!-----------------------------------------------------------------------

   jdat = 0
   idat = 0

   idat(1) = curr_year
   idat(2) = curr_month
   idat(3) = curr_day
   idat(5) = curr_hour
   idat(6) = curr_minute

!-----------------------------------------------------------------------
!  calc number of days between current day and second bounding
!  record.
!-----------------------------------------------------------------------

   if (curr_month <= climo_dates(rec2)%month) then
     jdat(1) = curr_year
   else
     jdat(1) = curr_year+1
   end if

   jdat(2) = climo_dates(rec2)%month
   jdat(3) = climo_dates(rec2)%day

   call w3difdat(jdat, idat, 3, rinc)

   ndays = rinc(3) / 1440.

!-----------------------------------------------------------------------
!  now calculate number of days between bounding months.
!-----------------------------------------------------------------------

   idat = 0
   jdat = 0

   if (curr_month <= climo_dates(rec2)%month) then
     idat(1) = curr_year-1
   else
     idat(1) = curr_year
   end if

   idat(2) = climo_dates(rec1)%month
   idat(3) = climo_dates(rec1)%day

   if (curr_month <= climo_dates(rec2)%month) then
     jdat(1) = curr_year
   else
     jdat(1) = curr_year+1
   end if
   jdat(2) = climo_dates(rec2)%month
   jdat(3) = climo_dates(rec2)%day

   call w3difdat(jdat, idat, 1, rinc)
   ndays_btwn_recs = rinc(1)

 end if

!-----------------------------------------------------------------------
!  calculate temporal weights for records 1 and 2.
!-----------------------------------------------------------------------

  wght1 = ndays / float(ndays_btwn_recs)
  wght2 = 1.0 - wght1

  data_climo_out = (wght1 * data_climo_in(:,rec1)) + (wght2 * data_climo_in(:,rec2))

  print*,'mon/day ',curr_month,curr_day,curr_hour,curr_minute, &
                    rec1, rec2, ndays, &
                    ndays_btwn_recs, wght1, wght2

 return

 end subroutine time_interp

!-----------------------------------------------------------------------
! given data on the full grid, pick off the points on the subdomain
! (useful for when running in parallel.)
!-----------------------------------------------------------------------

 subroutine thin_data(dummy_in, ipts, jpts, lonsperlat,   &
                      ijmdl, jmdl2, imdl, jmdl, dummy_out)
 implicit none

 integer                   :: ifull
 integer                   :: ij
 integer, intent(in)       :: ijmdl  ! # points on subdomain
 integer, intent(in)       :: imdl   ! # i points on full domain
 integer, intent(in)       :: ipts(ijmdl)
 integer                   :: jfull
 integer                   :: jj
 integer, intent(in)       :: jmdl   ! # j points on full domain
 integer, intent(in)       :: jmdl2
 integer, intent(in)       :: jpts(ijmdl)
 integer, intent(in)       :: lonsperlat(jmdl2)

 real, intent(in)          :: dummy_in(imdl,jmdl)
 real, intent(out)         :: dummy_out(ijmdl)
 real                      :: r
 real                      :: x1

 if (lonsperlat(1) == -9999) then  ! regional grid, where number of
                                   ! 'i' points is constant.

   do ij = 1, ijmdl

     dummy_out(ij) = dummy_in(ipts(ij),jpts(ij))

   enddo

 else ! global grid, where number of points can decrease toward poles.

   do ij = 1, ijmdl

     jfull = jpts(ij)

     jj    = jfull
     if (jfull > (jmdl2)) jj = jmdl - jfull + 1

     r     = float(imdl) / float(lonsperlat(jj))
     x1    = float((ipts(ij) - 1)) * r
     ifull = nint(x1) + 1

     dummy_out(ij) = dummy_in(ifull,jfull)

   enddo

 end if

 return

 end subroutine thin_data

!-----------------------------------------------------------------------
! degrib multiple records of a climo file and pick off the points
! on the subdomain of interest.
!-----------------------------------------------------------------------

 subroutine degrib_climo_thin(data_climo, dates, ijmdl, imdl, jmdl, &
                              jmdl2, ipts, jpts, lonsperlat, param_num, &
                              input_file, tot_num_recs, status, me)

 implicit none

 character*150, intent(in)            :: input_file

 integer, intent(in)                  :: ijmdl
 integer, intent(in)                  :: imdl
 integer, intent(in)                  :: ipts(ijmdl)
 integer                              :: iret
 integer, parameter                   :: iunit_src = 55
 integer                              :: jgds(200)
 integer, intent(in)                  :: jmdl
 integer, intent(in)                  :: jmdl2
 integer                              :: jpds(200)
 integer, intent(in)                  :: jpts(ijmdl)
 integer                              :: kgds(200)
 integer                              :: kpds(200)
 integer, intent(in)                  :: lonsperlat(jmdl2)
 integer                              :: lskip
 integer                              :: lugi
 integer, intent(in), optional        :: me     ! mpi task number
 integer                              :: num_pts
 integer, intent(in)                  :: param_num
 integer                              :: rec_num
 integer, intent(out)                 :: status
 integer, intent(in)                  :: tot_num_recs

 logical*1, allocatable               :: lbms(:,:)

 real, intent(out)                    :: data_climo(ijmdl, tot_num_recs)
 real, allocatable                    :: dummy2d(:,:)

 type (date), intent(out)             :: dates(tot_num_recs)

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------

 status = 0

 if (present(me)) then
   if (me == 0) print*,"- OPEN INPUT FILE ", trim(input_file)
 else
   print*,"- OPEN INPUT FILE ", trim(input_file)
 end if

 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   status = -1
   return
 end if

 allocate (dummy2d(imdl,jmdl))
 allocate (lbms(imdl,jmdl))

 lskip   = -1
 rec_num = 0
 lugi    = 0

 READ : do

   jgds    = -1
   jpds    = -1
   jpds(5) = param_num
   kgds    = -1
   kpds    = -1

   if (present(me)) then
     if (me == 0) print*,"- DEGRIB DATA "
   else
     print*,"- DEGRIB DATA "
   endif

   call getgb(iunit_src, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              num_pts, lskip, kpds, kgds, lbms, dummy2d, iret)

   if ((iret > 0) .and. (iret < 99)) then
     print*,'- BAD READ OF GRIB DATA. IRET IS ', iret
     status = -2
     return
   end if

   if (iret == 99 .or. rec_num == tot_num_recs) exit READ

   if (param_num == kpds(5)) then

     rec_num = rec_num + 1

     dates(rec_num)%day   = kpds(10)
     dates(rec_num)%month = kpds(9)
     dates(rec_num)%year  = ((kpds(21)-1) * 100) + kpds(8)

!     print*,rec_num,dates(rec_num)%year,dates(rec_num)%month,dates(rec_num)%day

     call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                    ijmdl, jmdl2, imdl, jmdl, data_climo(1,rec_num))

!     print*,maxval(data_climo(:,rec_num)),minval(data_climo(:,rec_num))

   end if

 enddo READ

 call baclose(iunit_src, iret)

 return

 end subroutine degrib_climo_thin

!-----------------------------------------------------------------------
! degrib multiple records of a climo file.
!-----------------------------------------------------------------------

 subroutine degrib_climo(data_climo, dates, ijmdl,      &
                         param_num, input_file,         &
                         tot_num_recs, status, me)

 implicit none

 character*150, intent(in)            :: input_file

 integer, intent(in)                  :: ijmdl
 integer                              :: iret
 integer, parameter                   :: iunit_src = 55
 integer                              :: jgds(200)
 integer                              :: jpds(200)
 integer                              :: kgds(200)
 integer                              :: kpds(200)
 integer                              :: lskip
 integer                              :: lugi
 integer, intent(in)                  :: me
 integer                              :: num_pts
 integer, intent(in)                  :: param_num
 integer                              :: rec_num
 integer, intent(out)                 :: status
 integer, intent(in)                  :: tot_num_recs
 
 logical*1, allocatable               :: lbms(:)

 real, intent(out)                    :: data_climo(ijmdl, tot_num_recs)
 real, allocatable                    :: dummy(:)

 type (date), intent(out)             :: dates(tot_num_recs)

!-----------------------------------------------------------------------
! read data.
!-----------------------------------------------------------------------

 status = 0

 if (me == 0) print*,"- OPEN INPUT FILE ", trim(input_file)
 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   status = -1
   return
 end if

 allocate (dummy(ijmdl))
 allocate (lbms(ijmdl))

 lskip   = -1
 rec_num = 0
 lugi    = 0

 READ : do

   jgds    = -1
   jpds    = -1
   jpds(5) = param_num
   kgds    = -1
   kpds    = -1

   if (me == 0) print*,"- DEGRIB DATA "

   call getgb(iunit_src, lugi, ijmdl, lskip, jpds, jgds, &
              num_pts, lskip, kpds, kgds, lbms, dummy, iret)

   if ((iret > 0) .and. (iret < 99)) then
     print*,'- BAD READ OF GRIB DATA. IRET IS ', iret
     status = -2
     return
   end if

   if (iret == 99 .or. rec_num == tot_num_recs) exit READ

   if (param_num == kpds(5)) then

     rec_num = rec_num + 1

     dates(rec_num)%day   = kpds(10)
     dates(rec_num)%month = kpds(9)
     dates(rec_num)%year  = ((kpds(21)-1) * 100) + kpds(8)

     data_climo(:,rec_num) = dummy

   end if

 enddo READ

 call baclose(iunit_src, iret)

 deallocate (dummy)
 deallocate (lbms)

 return

 end subroutine degrib_climo

!----------------------------------------------------------------
! inventory a climo grib file.
!----------------------------------------------------------------

 subroutine inventory (input_file, param_num, tot_num_recs, status, me)

 implicit none

 character*150, intent(in)            :: input_file

 integer                              :: iret
 integer, parameter                   :: iunit_src = 55
 integer                              :: jgds(200)
 integer                              :: jpds(200)
 integer                              :: kgds(200)
 integer                              :: kpds(200)
 integer                              :: lskip
 integer                              :: lugi
 integer, intent(in)                  :: me   ! mpi task number
 integer                              :: message_num
 integer                              :: num_bytes
 integer                              :: num_pts
 integer, intent(in)                  :: param_num
 integer, intent(out)                 :: status
 integer, intent(out)                 :: tot_num_recs

!-----------------------------------------------------------------------
! open input and output files.
!-----------------------------------------------------------------------

 status = 0

 if (me == 0) print*,"- OPEN INPUT FILE ", trim(input_file)
 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   status = -1
   return
 end if

!-----------------------------------------------------------------------
! inventory the source data.  assumes the following:
!
! 1) data is in grib format
! 2) multiple parameter types are allowed (ex: different types of
!    albedo), but they must be in consecutive records.
! 3) multiple time periods are allowed, but they must be in
!    chronological order.
! 4) each grib record is on the same grid.
!-----------------------------------------------------------------------

 lugi         = 0
 tot_num_recs = 0
 lskip        = -1

 INVENTORY_LOOP : do

   jgds    = -1
   jpds    = -1
   jpds(5) = param_num
   kgds    = -1
   kpds    = -1

   if (me == 0) print*,"- GET GRIB HEADER FOR RECORD ", (max(lskip,0)+1)

   call getgbh(iunit_src, lugi, lskip, jpds, jgds, num_bytes,  &
               num_pts, message_num, kpds, kgds, iret)

!-----------------------------------------------------------------------
!  when the end of file is reached, a status code of 99 is returned.
!  other status codes indicate a bad read.
!-----------------------------------------------------------------------

   if ((iret > 0) .and. (iret < 99)) then
     print*,'- BAD READ OF GRIB HEADER. IRET IS ', iret
     status = -2
     return
   end if

   if (iret == 99) exit INVENTORY_LOOP

!-----------------------------------------------------------------------
!  keep track of the number of records for this field.
!-----------------------------------------------------------------------

   if (param_num == kpds(5)) then
     tot_num_recs = tot_num_recs + 1
   end if

   lskip = message_num

 enddo INVENTORY_LOOP

 if (me == 0) &
 print*,'FILE CONTAINS ', tot_num_recs, ' RECORDS OF PARAMETER ', param_num

 call baclose(iunit_src, iret)

 return

 end subroutine inventory

!-----------------------------------------------------------------------
! write processed data to file (sfccycle format).
!-----------------------------------------------------------------------

!noah: add the following variables: greenfrc_max, greenfrc_min,
!noah: soilm_tot (previously soilm), soilm_liq, slope_type, mxsnow_alb,
!noah: sheleg, tprcp, srflag.

 subroutine write_sfc_file(greenfrc, greenfrc_max, greenfrc_min, &
                           soil_temp, soilm_tot, soilm_liq,      &
                           alvsf, alvwf, alnsf, alnwf,           &
                           substrate_temp, skin_temp,            &
                           slope_type, mxsnow_alb,               &
                           sheleg, snow_depth, z0, soil_type,    &
                           veg_type, canopy_mc,                  &
                           cv, cvb, cvt, facsf, facwf,           &
                           ffhh, ffmm, f10m, ustar,              &
                           tprcp, srflag, lsmask,                &
                           output_file, thinned,                 &
                           nsoil, imdl, jmdl, ijmdl, ijmdl_full, &
                           fcst_hour, cycle_year, cycle_month,   &
                           cycle_day, cycle_hour )

 implicit none

 character*8                   :: labfix(4)
 character*150, intent(in)     :: output_file

 integer, intent(in)           :: cycle_day
 integer, intent(in)           :: cycle_hour
 integer, intent(in)           :: cycle_month
 integer, intent(in)           :: cycle_year
 integer                       :: dummyifull(ijmdl_full)
 integer*4                     :: idate(4)
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: ijmdl_full
 integer, intent(in)           :: imdl
 integer                       :: istat
 integer, intent(in)           :: jmdl
 integer, intent(in)           :: lsmask(ijmdl)
 integer                       :: n
 integer, intent(in)           :: nsoil
!noah start
 integer, intent(in)           :: slope_type(ijmdl)
!noah end
 integer, intent(in)           :: soil_type(ijmdl)
 integer, intent(in)           :: veg_type(ijmdl)
 integer*4, parameter          :: version = 200004  ! change this for noah era?

!cggg
 integer :: lonsperlat(jmdl/2)


 logical, intent(in)           :: thinned

 real, intent(in)              :: alvsf(ijmdl)
 real, intent(in)              :: alvwf(ijmdl)
 real, intent(in)              :: alnsf(ijmdl)
 real, intent(in)              :: alnwf(ijmdl)
 real                          :: buff4(ijmdl_full,4)
 real, intent(in)              :: canopy_mc(ijmdl)
 real, intent(in)              :: cv(ijmdl)
 real, intent(in)              :: cvb(ijmdl)
 real, intent(in)              :: cvt(ijmdl)
 real                          :: dummyr(ijmdl)
 real                          :: dummyrfull(ijmdl_full)
 real                          :: dummyrfull2(ijmdl_full,nsoil)
 real, intent(in)              :: facsf(ijmdl)
 real, intent(in)              :: facwf(ijmdl)
 real, intent(in)              :: fcst_hour
 real, intent(in)              :: ffhh(ijmdl)
 real, intent(in)              :: ffmm(ijmdl)
 real, intent(in)              :: f10m(ijmdl)
 real, intent(in)              :: greenfrc(ijmdl)
!noah start
 real, intent(in)              :: greenfrc_max(ijmdl)
 real, intent(in)              :: greenfrc_min(ijmdl)
 real, intent(in)              :: mxsnow_alb(ijmdl)
 real, intent(in)              :: sheleg(ijmdl)
!noah end
 real, intent(in)              :: skin_temp(ijmdl)
 real, intent(in)              :: snow_depth(ijmdl)
 real, intent(in)              :: soil_temp(ijmdl,nsoil)
!noah start
 real, intent(in)              :: soilm_liq(ijmdl,nsoil)
 real, intent(in)              :: soilm_tot(ijmdl,nsoil)
 real, intent(in)              :: srflag(ijmdl)
!noah end
 real, intent(in)              :: substrate_temp(ijmdl)
!noah start
 real, intent(in)              :: tprcp(ijmdl)
!noah end
 real, intent(in)              :: ustar(ijmdl)
 real, intent(in)              :: z0(ijmdl)

 labfix=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)

 idate(1)   = cycle_hour       ! cycle
 idate(2)   = cycle_month      ! month
 idate(3)   = cycle_day        ! day
 idate(4)   = cycle_year

 print*,"- WRITE OUTPUT DATA TO FILE: ", trim(output_file)

 open (9, file=trim(output_file), form="unformatted")

 write(9, iostat=istat, err=9000) labfix

 write(9, iostat=istat, err=9000) fcst_hour, idate, imdl, jmdl, version, lonsperlat

 if (thinned) then

   call uninterpred(1, dummyifull, skin_temp, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull  ! skin temp

!noah start
   do n = 1, nsoil
     call uninterpred(1, dummyifull, soilm_tot(1,n), dummyrfull2(1,n), &
                      imdl, jmdl, ijmdl, lonsperlat)
   enddo
   write(9, iostat=istat, err=9000) dummyrfull2  ! soil moisture - total

   call uninterpred(1, dummyifull, sheleg, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! snow - liq equivalent
!noah end

   do n = 1, nsoil
     call uninterpred(1, dummyifull, soil_temp(1,n), dummyrfull2(1,n), &
                      imdl, jmdl, ijmdl, lonsperlat)
   enddo
   write(9, iostat=istat, err=9000) dummyrfull2  ! soil temp

   call uninterpred(1, dummyifull, substrate_temp, dummyrfull, &
                    imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! substrate t

   call uninterpred(1, dummyifull, z0, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! z0

   call uninterpred(1, dummyifull, cv, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! substrate t

   call uninterpred(1, dummyifull, cvb, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! substrate t

   call uninterpred(1, dummyifull, cvt, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull  ! substrate t

   call uninterpred(1, dummyifull, alvsf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,1) = dummyrfull
   call uninterpred(1, dummyifull, alvwf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,2) = dummyrfull
   call uninterpred(1, dummyifull, alnsf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,3) = dummyrfull
   call uninterpred(1, dummyifull, alnwf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,4) = dummyrfull
   write(9, iostat=istat, err=9000) buff4   ! 4 albedos

   dummyr = float(lsmask)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! land sea mask

   call uninterpred(1, dummyifull, greenfrc, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! greenness

   call uninterpred(1, dummyifull, canopy_mc, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! canopy mc

   call uninterpred(1, dummyifull, f10m, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! f10m

   dummyr = float(veg_type)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! veg type

   dummyr = float(soil_type)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! soil type

   call uninterpred(1, dummyifull, facsf, dummyrfull2(1,1), imdl, jmdl, &
                    ijmdl, lonsperlat)
   call uninterpred(1, dummyifull, facwf, dummyrfull2(1,2), imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull2  ! facsf and facwf

   call uninterpred(1, dummyifull, ustar, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! u*

   call uninterpred(1, dummyifull, ffmm, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! ffmm

   call uninterpred(1, dummyifull, ffhh, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! ffhh

!noah start
   call uninterpred(1, dummyifull, tprcp, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! tprcp

   call uninterpred(1, dummyifull, srflag, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! rain/snow flag

   call uninterpred(1, dummyifull, snow_depth, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! snow depth

   do n = 1, nsoil
     call uninterpred(1, dummyifull, soilm_liq(1,n), dummyrfull2(1,n), &
                      imdl, jmdl, ijmdl, lonsperlat)
   enddo
   write(9, iostat=istat, err=9000) dummyrfull2  ! soil moisture liquid

   call uninterpred(1, dummyifull, greenfrc_min, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! min greenness

   call uninterpred(1, dummyifull, greenfrc_max, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! max greenness

   dummyr = float(slope_type)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl, & 
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! slope type

   call uninterpred(1, dummyifull, mxsnow_alb, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! max snow albedo
!noah end

 else

   write(9, iostat=istat, err=9000) skin_temp
!noah start
   write(9, iostat=istat, err=9000) soilm_tot
   write(9, iostat=istat, err=9000) sheleg
!noah end
   write(9, iostat=istat, err=9000) soil_temp
   write(9, iostat=istat, err=9000) substrate_temp
   write(9, iostat=istat, err=9000) z0
   write(9, iostat=istat, err=9000) cv
   write(9, iostat=istat, err=9000) cvb
   write(9, iostat=istat, err=9000) cvt
   buff4(:,1) = alvsf
   buff4(:,2) = alvwf
   buff4(:,3) = alnsf
   buff4(:,4) = alnwf
   write(9, iostat=istat, err=9000) buff4   ! 4 albedoes
   write(9, iostat=istat, err=9000) float(lsmask)
   write(9, iostat=istat, err=9000) greenfrc
   write(9, iostat=istat, err=9000) canopy_mc
   write(9, iostat=istat, err=9000) f10m
   write(9, iostat=istat, err=9000) float(veg_type)
   write(9, iostat=istat, err=9000) float(soil_type)
   write(9, iostat=istat, err=9000) facsf, facwf
   write(9, iostat=istat, err=9000) ustar
   write(9, iostat=istat, err=9000) ffmm
   write(9, iostat=istat, err=9000) ffhh
!noah start
   write(9, iostat=istat, err=9000) tprcp
   write(9, iostat=istat, err=9000) srflag
   write(9, iostat=istat, err=9000) snow_depth
   write(9, iostat=istat, err=9000) soilm_liq
   write(9, iostat=istat, err=9000) greenfrc_min
   write(9, iostat=istat, err=9000) greenfrc_max
   write(9, iostat=istat, err=9000) float(slope_type)
   write(9, iostat=istat, err=9000) mxsnow_alb
!noah end


 end if
 close(9)

 return

 9000 print*,"** ERROR WRITING: ", trim(output_file)

 print*,"** ISTAT IS ", istat

 call abort

 stop

 end subroutine write_sfc_file

!-----------------------------------------------------------------------
! write processed data to file (sfccycle format). include new ice
! records
!-----------------------------------------------------------------------

!noah: add the following variables: greenfrc_max, greenfrc_min,
!noah: soilm_tot (previously soilm), soilm_liq, slope_type, mxsnow_alb,
!noah: sheleg, tprcp, srflag.

 subroutine write_sfc_fileice(greenfrc, greenfrc_max, greenfrc_min, &
                           soil_temp, soilm_tot, soilm_liq,      &
                           alvsf, alvwf, alnsf, alnwf,           &
                           substrate_temp, skin_temp,            &
                           slope_type, mxsnow_alb,               &
                           sheleg, snow_depth, z0, soil_type,    &
                           veg_type, canopy_mc,                  &
                           cv, cvb, cvt, facsf, facwf,           &
                           ffhh, ffmm, f10m, ustar,              &
                           tprcp, srflag, lsmask,                &
                           fice, hice,                           &
                           output_file, thinned,                 &
                           nsoil, imdl, jmdl, ijmdl, ijmdl_full, &
                           fcst_hour, cycle_year, cycle_month,   &
                           cycle_day, cycle_hour )

 implicit none

 character*8                   :: labfix(4)
 character*150, intent(in)     :: output_file

 integer, intent(in)           :: cycle_day
 integer, intent(in)           :: cycle_hour
 integer, intent(in)           :: cycle_month
 integer, intent(in)           :: cycle_year
 integer                       :: dummyifull(ijmdl_full)
 integer*4                     :: idate(4)
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: ijmdl_full
 integer, intent(in)           :: imdl
 integer                       :: istat
 integer, intent(in)           :: jmdl
 integer, intent(in)           :: lsmask(ijmdl)
 integer                       :: n
 integer, intent(in)           :: nsoil
!noah start
 integer, intent(in)           :: slope_type(ijmdl)
!noah end
 integer, intent(in)           :: soil_type(ijmdl)
 integer, intent(in)           :: veg_type(ijmdl)
 integer*4, parameter          :: version = 200004  ! change this for noah era?

!cggg
 integer :: lonsperlat(jmdl/2)


 logical, intent(in)           :: thinned

 real, intent(in)              :: alvsf(ijmdl)
 real, intent(in)              :: alvwf(ijmdl)
 real, intent(in)              :: alnsf(ijmdl)
 real, intent(in)              :: alnwf(ijmdl)
 real                          :: buff4(ijmdl_full,4)
 real, intent(in)              :: canopy_mc(ijmdl)
 real, intent(in)              :: cv(ijmdl)
 real, intent(in)              :: cvb(ijmdl)
 real, intent(in)              :: cvt(ijmdl)
 real                          :: dummyr(ijmdl)
 real                          :: dummyrfull(ijmdl_full)
 real                          :: dummyrfull2(ijmdl_full,nsoil)
 real, intent(in)              :: facsf(ijmdl)
 real, intent(in)              :: facwf(ijmdl)
 real, intent(in)              :: fcst_hour
 real, intent(in)              :: ffhh(ijmdl)
 real, intent(in)              :: ffmm(ijmdl)
!ice start
 real, intent(in)              :: fice(ijmdl)
!ice end
 real, intent(in)              :: f10m(ijmdl)
 real, intent(in)              :: greenfrc(ijmdl)
!noah start
 real, intent(in)              :: greenfrc_max(ijmdl)
 real, intent(in)              :: greenfrc_min(ijmdl)
!ice start
 real, intent(in)              :: hice(ijmdl)
!ice end
 real, intent(in)              :: mxsnow_alb(ijmdl)
 real, intent(in)              :: sheleg(ijmdl)
!noah end
 real, intent(in)              :: skin_temp(ijmdl)
 real, intent(in)              :: snow_depth(ijmdl)
 real, intent(in)              :: soil_temp(ijmdl,nsoil)
!noah start
 real, intent(in)              :: soilm_liq(ijmdl,nsoil)
 real, intent(in)              :: soilm_tot(ijmdl,nsoil)
 real, intent(in)              :: srflag(ijmdl)
!noah end
 real, intent(in)              :: substrate_temp(ijmdl)
!noah start
 real, intent(in)              :: tprcp(ijmdl)
!noah end
 real, intent(in)              :: ustar(ijmdl)
 real, intent(in)              :: z0(ijmdl)

 labfix=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)

 idate(1)   = cycle_hour       ! cycle
 idate(2)   = cycle_month      ! month
 idate(3)   = cycle_day        ! day
 idate(4)   = cycle_year

 print*,"- WRITE OUTPUT DATA TO FILE: ", trim(output_file)

 open (9, file=trim(output_file), form="unformatted")

 write(9, iostat=istat, err=9000) labfix

 write(9, iostat=istat, err=9000) fcst_hour, idate, imdl, jmdl, version, lonsperlat

 if (thinned) then

   call uninterpred(1, dummyifull, skin_temp, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull  ! skin temp

!noah start
   do n = 1, nsoil
     call uninterpred(1, dummyifull, soilm_tot(1,n), dummyrfull2(1,n), &
                      imdl, jmdl, ijmdl, lonsperlat)
   enddo
   write(9, iostat=istat, err=9000) dummyrfull2  ! soil moisture - total

   call uninterpred(1, dummyifull, sheleg, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! snow - liq equivalent
!noah end

   do n = 1, nsoil
     call uninterpred(1, dummyifull, soil_temp(1,n), dummyrfull2(1,n), &
                      imdl, jmdl, ijmdl, lonsperlat)
   enddo
   write(9, iostat=istat, err=9000) dummyrfull2  ! soil temp

   call uninterpred(1, dummyifull, substrate_temp, dummyrfull, &
                    imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! substrate t

   call uninterpred(1, dummyifull, z0, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! z0

   call uninterpred(1, dummyifull, cv, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! substrate t

   call uninterpred(1, dummyifull, cvb, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! substrate t

   call uninterpred(1, dummyifull, cvt, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull  ! substrate t

   call uninterpred(1, dummyifull, alvsf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,1) = dummyrfull
   call uninterpred(1, dummyifull, alvwf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,2) = dummyrfull
   call uninterpred(1, dummyifull, alnsf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,3) = dummyrfull
   call uninterpred(1, dummyifull, alnwf, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   buff4(:,4) = dummyrfull
   write(9, iostat=istat, err=9000) buff4   ! 4 albedos

   dummyr = float(lsmask)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! land sea mask

   call uninterpred(1, dummyifull, greenfrc, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! greenness

   call uninterpred(1, dummyifull, canopy_mc, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! canopy mc

   call uninterpred(1, dummyifull, f10m, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! f10m

   dummyr = float(veg_type)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! veg type

   dummyr = float(soil_type)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! soil type

   call uninterpred(1, dummyifull, facsf, dummyrfull2(1,1), imdl, jmdl, &
                    ijmdl, lonsperlat)
   call uninterpred(1, dummyifull, facwf, dummyrfull2(1,2), imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull2  ! facsf and facwf

   call uninterpred(1, dummyifull, ustar, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! u*

   call uninterpred(1, dummyifull, ffmm, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! ffmm

   call uninterpred(1, dummyifull, ffhh, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! ffhh

!ice start
   call uninterpred(1, dummyifull, hice, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! hice

   call uninterpred(1, dummyifull, fice, dummyrfull, imdl, jmdl, ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! fice
!ice end

!noah start
   call uninterpred(1, dummyifull, tprcp, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! tprcp

   call uninterpred(1, dummyifull, srflag, dummyrfull, imdl, jmdl,  &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! rain/snow flag

   call uninterpred(1, dummyifull, snow_depth, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! snow depth

   do n = 1, nsoil
     call uninterpred(1, dummyifull, soilm_liq(1,n), dummyrfull2(1,n), &
                      imdl, jmdl, ijmdl, lonsperlat)
   enddo
   write(9, iostat=istat, err=9000) dummyrfull2  ! soil moisture liquid

   call uninterpred(1, dummyifull, greenfrc_min, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! min greenness

   call uninterpred(1, dummyifull, greenfrc_max, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! max greenness

   dummyr = float(slope_type)
   call uninterpred(1, dummyifull, dummyr, dummyrfull, imdl, jmdl, & 
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! slope type

   call uninterpred(1, dummyifull, mxsnow_alb, dummyrfull, imdl, jmdl, &
                    ijmdl, lonsperlat)
   write(9, iostat=istat, err=9000) dummyrfull   ! max snow albedo
!noah end

 else

   write(9, iostat=istat, err=9000) skin_temp
!noah start
   write(9, iostat=istat, err=9000) soilm_tot
   write(9, iostat=istat, err=9000) sheleg
!noah end
   write(9, iostat=istat, err=9000) soil_temp
   write(9, iostat=istat, err=9000) substrate_temp
   write(9, iostat=istat, err=9000) z0
   write(9, iostat=istat, err=9000) cv
   write(9, iostat=istat, err=9000) cvb
   write(9, iostat=istat, err=9000) cvt
   buff4(:,1) = alvsf
   buff4(:,2) = alvwf
   buff4(:,3) = alnsf
   buff4(:,4) = alnwf
   write(9, iostat=istat, err=9000) buff4   ! 4 albedoes
   write(9, iostat=istat, err=9000) float(lsmask)
   write(9, iostat=istat, err=9000) greenfrc
   write(9, iostat=istat, err=9000) canopy_mc
   write(9, iostat=istat, err=9000) f10m
   write(9, iostat=istat, err=9000) float(veg_type)
   write(9, iostat=istat, err=9000) float(soil_type)
   write(9, iostat=istat, err=9000) facsf, facwf
   write(9, iostat=istat, err=9000) ustar
   write(9, iostat=istat, err=9000) ffmm
   write(9, iostat=istat, err=9000) ffhh
!ice start
   write(9, iostat=istat, err=9000) hice
   write(9, iostat=istat, err=9000) fice
!ice end
!noah start
   write(9, iostat=istat, err=9000) tprcp
   write(9, iostat=istat, err=9000) srflag
   write(9, iostat=istat, err=9000) snow_depth
   write(9, iostat=istat, err=9000) soilm_liq
   write(9, iostat=istat, err=9000) greenfrc_min
   write(9, iostat=istat, err=9000) greenfrc_max
   write(9, iostat=istat, err=9000) float(slope_type)
   write(9, iostat=istat, err=9000) mxsnow_alb
!noah end

 end if
 close(9)

 return

 9000 print*,"** ERROR WRITING: ", trim(output_file)

 print*,"** ISTAT IS ", istat

 call abort

 stop

 end subroutine write_sfc_fileice

!-----------------------------------------------------------------------
! a clunky routine that allows one to read a single field from
! a sfccycle formatted file.
!-----------------------------------------------------------------------

 subroutine read_sfccycle_fmt(first_guess_file, field, ijmdl, nsoil, &
                              buff1, buffsoil)

 implicit none

 character*5,   intent(in)   :: field
 character*150, intent(in)   :: first_guess_file
 character*8                 :: labfix(4)

 integer, intent(in)         :: ijmdl
 integer                     :: imdl_fg
 integer                     :: idate(4), version
 integer                     :: istat
 integer                     :: jmdl_fg
 integer, intent(in)         :: nsoil

 real*4                      :: yhour
 real*4, intent(out)         :: buff1(ijmdl)
 real*4                      :: buff1_1(ijmdl)
 real*4, intent(out)         :: buffsoil(ijmdl,nsoil)
 real*4                      :: buff4(ijmdl,4)

 print*,'- READ FILE: ',trim(first_guess_file)
 print*,'- GET ', trim(field)

 open(9, file=trim(first_guess_file), form="unformatted", status='old', &
         iostat=istat, err=9100)

 read(9, iostat=istat, err=9000) labfix

 read(9, iostat=istat, err=9000) yhour, idate, imdl_fg, jmdl_fg, version

 print*,'- FILE DATE/TIME: ',yhour, idate

!-----------------------------------------------------------------------
! model grid dimensions are determined from land/sea mask file in
! module program_setup.  if they don't match those from the first
! guess file, something is wrong.
!----------------------------------------------------------------------

!cggg do i want to keep this stuff??
! if ( (imdl_fg /= imdl)  .or. (jmdl_fg /= jmdl)) then
!   print*,"** GRID DIMENSIONS OF FILE GRID: ", imdl_fg, jmdl_fg
!   print*,"** DO NOT MATCH DIMENSIONS OF LAND/SEA MASK FILE: ", imdl, jmdl
!   call abort
! end if

 print*,'- FILE GRID DIMENSIONS: ', imdl_fg, jmdl_fg

 read(9, iostat=istat, err=9000) buff1    ! skin temp

 if (field == 'skint') then
   close(9)
   return
 end if

!noah start
 read(9, iostat=istat, err=9000) buffsoil   ! soil moisture-total

 if (field == 'smtot') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! liq equivalent snow

 if (field == 'sneqv') then
   close(9)
   return
 end if
!noah end

 read(9, iostat=istat, err=9000) buffsoil   ! soil temp

 if (field == 'sltmp') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! substrate t

 if (field == 'tg3') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! z0

 if (field == 'z0') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! cv

 if (field == 'cv') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! cvb

 if (field == 'cvb') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! cvt

 if (field == 'cvt') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff4   ! 4 albedoes

 if (field == 'alvsf') then
   buff1 = buff4(:,1)
   close(9)
   return
 elseif (field == 'alvwf') then
   buff1 = buff4(:,2)
   close(9)
   return
 elseif (field == 'alnsf') then
   buff1 = buff4(:,3)
   close(9)
   return
 elseif (field == 'alnwf') then
   buff1 = buff4(:,4)
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! land/sea mask

 if (field == 'ice') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! greenness

 if (field == 'green') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! canopy mc

 if (field == 'canmc') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! f10m

 if (field == 'f10m') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! vegtype

 if (field == 'vtype') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! soil type

 if (field == 'stype') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1, buff1_1   ! facsf and facwf

 if (field == 'facsf') then
   close(9)
   return
 end if

 if (field == 'facwf') then
   buff1 = buff1_1
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! ustar

 if (field == 'ustar') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! ffmm

 if (field == 'ffmm') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! ffhh

 if (field == 'ffhh') then
   close(9)
   return
 endif

!noah start
 read(9, iostat=istat, err=9000) buff1   ! tprcp

 if (field == 'tprcp') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! srflag

 if (field == 'srflg') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! snow depth

 if (field == 'snowd') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buffsoil   ! soil moisture-liquid

 if (field == 'smliq') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! min greenness

 if (field == 'mingn') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! max greenness

 if (field == 'maxgn') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! slope index

 if (field == 'slope') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! max snow albedo
!noah end

 if (field == 'mxsna') then
   close(9)
   return
 endif

 close(9)

 return

9000 continue
 print*,"- ** ERROR READING FIRST GUESS DATA **"
 print*,"- ** ISTAT IS ", istat
 call abort

9100 continue
 print*,"- ** ERROR OPENING FIRST GUESS DATA **"
 print*,"- ** ISTAT IS ", istat
 call abort

 end subroutine read_sfccycle_fmt

!-----------------------------------------------------------------------
! a clunky routine that allows one to read a single field from
! a sfccycle formatted file.
!-----------------------------------------------------------------------

 subroutine read_sfccycle_fmtice(first_guess_file, field, ijmdl, nsoil, &
                              buff1, buffsoil)

 implicit none

 character*5,   intent(in)   :: field
 character*150, intent(in)   :: first_guess_file
 character*8                 :: labfix(4)

 integer, intent(in)         :: ijmdl
 integer                     :: imdl_fg
 integer                     :: idate(4), version
 integer                     :: istat
 integer                     :: jmdl_fg
 integer, intent(in)         :: nsoil

 real*4                      :: yhour
 real*4, intent(out)         :: buff1(ijmdl)
 real*4                      :: buff1_1(ijmdl)
 real*4, intent(out)         :: buffsoil(ijmdl,nsoil)
 real*4                      :: buff4(ijmdl,4)

 print*,'- READ FILE: ',trim(first_guess_file)
 print*,'- GET ', trim(field)

 open(9, file=trim(first_guess_file), form="unformatted", status='old', &
         iostat=istat, err=9100)

 read(9, iostat=istat, err=9000) labfix

 read(9, iostat=istat, err=9000) yhour, idate, imdl_fg, jmdl_fg, version

 print*,'- FILE DATE/TIME: ',yhour, idate

 print*,'- FILE GRID DIMENSIONS: ', imdl_fg, jmdl_fg

 read(9, iostat=istat, err=9000) buff1    ! skin temp

 if (field == 'skint') then
   close(9)
   return
 end if

!noah start
 read(9, iostat=istat, err=9000) buffsoil   ! soil moisture-total

 if (field == 'smtot') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! liq equivalent snow

 if (field == 'sneqv') then
   close(9)
   return
 end if
!noah end

 read(9, iostat=istat, err=9000) buffsoil   ! soil temp

 if (field == 'sltmp') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! substrate t

 if (field == 'tg3') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! z0

 if (field == 'z0') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! cv

 if (field == 'cv') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! cvb

 if (field == 'cvb') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! cvt

 if (field == 'cvt') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff4   ! 4 albedoes

 if (field == 'alvsf') then
   buff1 = buff4(:,1)
   close(9)
   return
 elseif (field == 'alvwf') then
   buff1 = buff4(:,2)
   close(9)
   return
 elseif (field == 'alnsf') then
   buff1 = buff4(:,3)
   close(9)
   return
 elseif (field == 'alnwf') then
   buff1 = buff4(:,4)
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! land/sea mask

 if (field == 'ice') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! greenness

 if (field == 'green') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! canopy mc

 if (field == 'canmc') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! f10m

 if (field == 'f10m') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! vegtype

 if (field == 'vtype') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! soil type

 if (field == 'stype') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1, buff1_1   ! facsf and facwf

 if (field == 'facsf') then
   close(9)
   return
 end if

 if (field == 'facwf') then
   buff1 = buff1_1
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! ustar

 if (field == 'ustar') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! ffmm

 if (field == 'ffmm') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! ffhh

 if (field == 'ffhh') then
   close(9)
   return
 endif

!ice start
 read(9, iostat=istat, err=9000) buff1   ! hice

 if (field == 'hice') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! fice

 if (field == 'fice') then
   close(9)
   return
 endif

!noah start
 read(9, iostat=istat, err=9000) buff1   ! tprcp

 if (field == 'tprcp') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! srflag

 if (field == 'srflg') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! snow depth

 if (field == 'snowd') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buffsoil   ! soil moisture-liquid

 if (field == 'smliq') then
   close(9)
   return
 end if

 read(9, iostat=istat, err=9000) buff1   ! min greenness

 if (field == 'mingn') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! max greenness

 if (field == 'maxgn') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! slope index

 if (field == 'slope') then
   close(9)
   return
 endif

 read(9, iostat=istat, err=9000) buff1   ! max snow albedo
!noah end

 if (field == 'mxsna') then
   close(9)
   return
 endif

 close(9)

 return

9000 continue
 print*,"- ** ERROR READING FIRST GUESS DATA **"
 print*,"- ** ISTAT IS ", istat
 call abort

9100 continue
 print*,"- ** ERROR OPENING FIRST GUESS DATA **"
 print*,"- ** ISTAT IS ", istat
 call abort

 end subroutine read_sfccycle_fmtice

!-----------------------------------------------------------------------
! generic routine to read grib data.
!-----------------------------------------------------------------------

 subroutine read_grib_data(grib_file, parm_num, dummy, ijmdl, istatus, me)

 implicit none

 character*150, intent(in)     :: grib_file

 integer, intent(in)           :: ijmdl
 integer                       :: iret
 integer, intent(out)          :: istatus
 integer, parameter            :: iunit = 13  ! grib file unit number
 integer                       :: jgds(200)
 integer                       :: jpds(200)
 integer                       :: lgrib
 integer                       :: lskip
 integer, parameter            :: lugi = 0    ! grib index file unit number - not used
 integer                       :: kgds(200)
 integer                       :: kpds(200)
 integer, intent(in), optional :: me          ! used to limit print with mpi
 integer                       :: numbytes
 integer                       :: numpts
 integer, intent(in)           :: parm_num

 logical*1, allocatable        :: lbms(:)

 real, intent(out)             :: dummy(ijmdl)

 istatus = 0

 if (present(me)) then
   if (me == 0)  print*,"- OPEN FILE ", trim(grib_file)
 else
   print*,"- OPEN FILE ", trim(grib_file)
 end if

 call baopenr (iunit, grib_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN OF FILE, IRET IS ', iret
   istatus = -1
   return
 end if

!-----------------------------------------------------------------------
! tell degribber what to look for.
!-----------------------------------------------------------------------

 lskip    = -1
 jpds     = -1
 jgds     = -1
 jpds(5)  = parm_num
 kpds     = jpds
 kgds     = jgds

 if (present(me)) then
   if (me == 0) print*,"- DEGRIB DATA."
 else
   print*,"- DEGRIB DATA."
 end if

 allocate(lbms(ijmdl))

 call getgb(iunit, lugi, ijmdl, lskip, jpds, jgds, &
            numpts, lskip, kpds, kgds, lbms, dummy, iret)

 if (iret /= 0) then
   print*,"- BAD DEGRIB OF DATA. IRET IS ", iret
   istatus = -2
   return
 end if

 call baclose(iunit,iret)

 deallocate (lbms)

 return

 end subroutine read_grib_data

!-----------------------------------------------------------------------
! fills out full grid using thinned grid data.  use an iord of
! "1" to use a nearest neighbor approach.
!-----------------------------------------------------------------------

 subroutine uninterpred(iord,kmsk,fi,f,lonl,latd,len,lonsperlat)

 implicit none

 integer, intent(in)               :: len
 integer, intent(in)               :: iord
 integer, intent(in)               :: lonl
 integer, intent(in)               :: latd
 integer, intent(in)               :: lonsperlat(latd/2)
 integer, intent(in)               :: kmsk(lonl*latd)
 integer                           :: j,lons,jj,latd2,ii,i

 real, intent(in)                  :: fi(len)
 real, intent(out)                 :: f(lonl,latd)

 latd2 = latd / 2
 ii    = 1

 do j=1,latd

   jj = j
   if (j .gt. latd2) jj = latd - j + 1
   lons=lonsperlat(jj)

   if(lons.ne.lonl) then
     call intlon(iord,1,1,lons,lonl,kmsk(ii),fi(ii),f(1,j))
   else
     do i=1,lonl
       f(i,j)  = fi(ii+i-1)
     enddo
   endif

   ii = ii + lons

 enddo

 end subroutine uninterpred

 subroutine interpred(iord,kmsk,f,fi,lonl,latd,ijmdl,lonsperlat)

 implicit none

 integer,intent(in)             :: iord
 integer,intent(in)             :: ijmdl
 integer,intent(in)             :: lonl
 integer,intent(in)             :: latd
 integer,intent(in)             :: lonsperlat(latd/2)
 integer,intent(in)             :: kmsk(lonl,latd)
 integer                        :: j,lons,jj,latd2,ii,i

 real,intent(in)                :: f(lonl,latd)
 real,intent(out)               :: fi(ijmdl)

 latd2 = latd / 2
 ii    = 1
 do j=1,latd
   jj = j
   if (j .gt. latd2) jj = latd - j + 1

   lons=lonsperlat(jj)
   if(lons.ne.lonl) then
      call intlon(iord,1,1,lonl,lons,kmsk(1,j),f(1,j),fi(ii))
   else
      do i=1,lonl
        fi(ii+i-1)  = f(i,j)
      enddo
   endif
   ii = ii + lons
 enddo

 end subroutine interpred

 subroutine intlon(iord,imon,imsk,m1,m2,k1,f1,f2)

 implicit none

 integer,intent(in)        :: iord,imon,imsk,m1,m2
 integer,intent(in)        :: k1(m1)
 integer                   :: i2,in,il,ir

 real,intent(in)           :: f1(m1)
 real,intent(out)          :: f2(m2)
 real                      :: r,x1

 r=real(m1)/real(m2)
 do i2=1,m2
   x1=(i2-1)*r
   il=int(x1)+1
   ir=mod(il,m1)+1
   if(iord.eq.2.and.(imsk.eq.0.or.k1(il).eq.k1(ir))) then
     f2(i2)=f1(il)*(il-x1)+f1(ir)*(x1-il+1)
   else
     in=mod(nint(x1),m1)+1
     f2(i2)=f1(in)
   endif
 enddo

 end subroutine intlon

!-----------------------------------------------------------------------
! get time based on model cycle and forecast hour.
!-----------------------------------------------------------------------

 subroutine new_time(cycle_year, cycle_month, &
                     cycle_day,  cycle_hour,  &
                     fcst_hour, curr_year, curr_month, &
                     curr_day, curr_hour, curr_minute, &
                     curr_date)

 implicit none

 character*10, intent(out)  :: curr_date

 integer, intent(out)       :: curr_day
 integer, intent(out)       :: curr_hour
 integer, intent(out)       :: curr_minute
 integer, intent(out)       :: curr_month
 integer, intent(out)       :: curr_year
 integer, intent(in)        :: cycle_day
 integer, intent(in)        :: cycle_hour
 integer, intent(in)        :: cycle_month
 integer, intent(in)        :: cycle_year
 integer                    :: idat(8)
 integer                    :: jdat(8)

 real, intent(in)           :: fcst_hour
 real                       :: fcst_minute
 real                       :: rinc(5)

 rinc = 0
 idat = 0
 jdat = 0

 fcst_minute = nint((fcst_hour - int(fcst_hour)) * 60.0)

 rinc(2) = int(fcst_hour)
 rinc(3) = fcst_minute

 idat(1) = cycle_year
 idat(2) = cycle_month
 idat(3) = cycle_day
 idat(5) = cycle_hour

 call w3movdat(rinc, idat, jdat)

 curr_year  = jdat(1)
 curr_month = jdat(2)
 curr_day   = jdat(3)
 curr_hour  = jdat(5)
 curr_minute= jdat(6)

 write(curr_date(1:4),  "(i4)")   curr_year
 write(curr_date(5:6),  "(i2.2)") curr_month
 write(curr_date(7:8),  "(i2.2)") curr_day
 write(curr_date(9:10), "(i2.2)") curr_hour

 return

 end subroutine new_time

 end module read_write_utils