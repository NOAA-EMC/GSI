SUBROUTINE write_gmi_bufr_1cr(prefix, version, nscan, npixel, nchan,        &
                          year, mon, day, hour, minu, seco,                 &
                          lat, lon, Quality,                     &
                          scLat, scLon, scAlt,                              &
                          satAzimuthAngle, solarAzimuthAngle, solarZenAngle, incidenceAngle,  &
                          Tb,                              &
                          GranuleNumber, NumberScansBeforeGranule,NumberScansGranule,    &
                          NumberScansAfterGranule, sunGlintAngle1c,  RFIflag,MissingData,&    
                          need_date,need_syn, &
                          exitCode)
! 08/20/2015  J.Jin-Yelena     Add NEW ARGUMENT - RFIflag.

! 12-10-2012  J.Jin      Modified from Santha's code.
!                        output to tmi.%y4%m2%d2.t%h2z.bufr.
! 01-26-2014  J.Jin     (1) Re-orgnize the bufr table and the output procedure.
!                       Now the bufr file has a MG containing TMI general information
!			and channel information at the beginning, and other MGs containing
!			scans which has 208 SB for the pixcels.
!			(2) Added a check of the existing bufr file to which obs are being 
!			appended. If these data are in the existing bufr file, these obs
!			are not written out. Only new obs are appended to the bufr file.
! 03-00-2014  E.Jones   Modifications for GMI
!
! 03-25-2014  J.Jin     Modify the check process. Now it checks time (seconds from 00:00z Jan 01, 2013) 
!                       in the existing bufr in order to avoid overlap output.
!                       Merger all subroutines for L1B_S1, L1B_S2, L1C_S1 and L1C_S2 into 
!                       one subroutine through adding an input variable prefix.
!                       clean up. 
! 03-27-2014  J.Jin     Separate subroutines for 1B and 1C. Couldn't get all integer inputs 
!                       correctly when they in one subroutine.
!
! 06-11-2014  ejones    modified for GMI L1CR data.
! 06-24-2014  J.Jin     revise outputs according to a new BUFR table.
! 10/10/2014  Yelena    Add 'need_date,need_syn'  as Input subroutine ymd_thhz.f90
!                       --> to write Bufr  only for needed date,needed synoptic
!                        and get back  tak(2) , means:
!                       tak(i) =  0 , do not take file "i" 
!                       tak(i) =  1 ,  take file "i"

! 11-06-2014  J.Jin     Revise the saving of data version. V03A which was saved as V3, but
!                       now is saved as V3.01.

! 20-08-2015  J.Jin     Merge Erin Jones's code to write out RFIflag.
! 27-10-2015  J.Jin     Write satellite altitude (HMSL) values in unit meter. It is in     km in 1C-R HDF files,
!                       but should be saved in meter in BUFR files. (Note, it is in me    ter in 1B HDF files.)

!---------------------------------------------------------------------------
  IMPLICIT NONE
  EXTERNAL                     :: ymdhms2tim13

  INTEGER, PARAMETER           :: ngs=2
! INTEGER,   INTENT(IN)        :: version, nscan, npixel, nchan ! npixello, nchanlo, nchanhi

! 11/06/2014 J. Jin 
  INTEGER,   INTENT(IN)        :: nscan, npixel, nchan ! npixello, nchanlo, nchanhi
  REAL*4,    INTENT(IN)        :: version

  INTEGER*4,   INTENT(IN)      :: year(nscan)
  INTEGER*4,   INTENT(IN)      :: mon(nscan), day(nscan),      &
                                  hour(nscan), minu(nscan), seco(nscan)
  INTEGER*4, INTENT(IN)        :: Quality(nchan, npixel, nscan),sunGlintAngle1c(ngs,npixel,nscan)

!    20-08-2015  J.Jin
!  ADDED  RFIflag(nchan, npixel, nscan)
  INTEGER*4, INTENT(IN)        :: RFIflag(nchan, npixel, nscan)
                                  
  REAL*4,    INTENT(IN)        :: lat(npixel,nscan), lon(npixel,nscan),     &
                                  satAzimuthAngle(ngs, npixel,nscan),            &
                                  solarAzimuthAngle(ngs, npixel,nscan),          &
                                  solarZenAngle(ngs, npixel,nscan),              &
                                  incidenceAngle(ngs, npixel,nscan)
  INTEGER, INTENT(IN)          :: GranuleNumber,   NumberScansBeforeGranule,   &
                                   NumberScansGranule,  NumberScansAfterGranule, &
                                   MissingData
                             
  REAL*4,    INTENT(IN)        :: scLon(nscan), scLat(nscan), scAlt(nscan)

  REAL*4,    INTENT(IN)        :: Tb(nchan, npixel, nscan)
  character(len=80),INTENT(IN) :: prefix
  integer, INTENT(IN)          :: need_date,need_syn


  INTEGER,   INTENT(OUT)       :: exitCode

  INTEGER                      :: unit_table = 20
  INTEGER                      :: unit_out   = 10


  INTEGER                      :: nObs, nBad, nGood
  INTEGER*8                    :: iScan, iPixel, iChan, iDate,              &
                                  IRET, iBad, iGood

  REAL                         :: XTEMP = 999


  INTEGER                      :: npxllo, npxl
  CHARACTER (LEN = 8)          :: subset    = 'NC021204'
  CHARACTER (LEN = 8)          :: subset0    = 'NC021200'
  CHARACTER (LEN = 80)          :: subset_ex
  CHARACTER(len=80)            ::  scaninfo, gmiinfo ! scan message
  REAL*8                       ::  scaninfo_v(11)    ! scan message
  REAL*8                       ::  gmiinfo_v(5)

  INTEGER                      ::  nch
! REAL*8                       ::  gmichq(nchan,nscan), gmichq_v(nchan)  ! qulity for chanels


! 10/03/2015
!      20-08-2015  J.Jin
!  Added gmi_rfi(nchan    )
  REAL*8                       ::  gmichq(nchan,nscan), gmichq_v(nchan), gmi_rfi(nchan)  ! qulity for chanels


  REAL*8                       ::  geoloc(2), tmbr_v(nchan),iang_v(ngs),sga_v(ngs)
  real*8                       ::  fov(nchan), gmi_ina(ngs)

  character(len=80)            :: outfile, bufrtbfile, frmt
  character(len=13)            :: ymdthrz(2)
  integer                      :: id_oa(2), id_ob(2), nfile, iScan_x1, iScan_x2
  integer                      :: tak(2)
  logical                      :: ex, ex_obit, new_scan

  real*8                       :: orbn_v, scan_v, jdate
  integer*8, parameter         :: nsc_max = 50000 ! a large number of scans in the obit in an existing bufr file.
  integer                      :: ireadmg, ireadsb, img, nsb, nsc, scan_old0(nsc_max) 
  integer, allocatable         ::  scan_old(:) 
  REAl*8                       :: ymdhms(6) 
  integer*8                    :: tim13_old0(nsc_max),tim13
  integer*8, allocatable       :: tim13_old(:)

   INTEGER, PARAMETER          ::  nch1=9,nch2=4,nch3=13
!   real*8                      :: SCCF1(nch1),SCBW1(nch1),ANPO1(nch1),CHNM1(nch1)
!   real*8                      :: SCCF2(nch2),SCBW2(nch2),ANPO2(nch2),CHNM2(nch2)
!   real*8, allocatable         :: SCCF(:),SCBW(:),ANPO(:),CHNM(:)
   real*8                      :: SCCF(13),SCBW(13),ANPO(13),CHNM(13)
   real*4                       :: scAlt_meter(nscan)


   !GHz
!   data SCCF1 /10.65,10.65, 18.70,18.70, 23.80, 36.50,36.50, 89.00,89.00/
   !MHz
!   data SCBW1 /100,100, 200,200, 400, 1000,1000, 6000,6000/
!   data ANPO1 /1,0, 1,0, 1, 1,0, 1,0/   ! (1, vertical;  0, horizontal) polarization
!   data CHNM1 /1,2,3,4,5,6,7,8,9/
!   data SCCF2 /166.0,166.0, 183.31, 183.31/
!   data SCBW2 /3400, 3400, 2000, 2000/
!   data ANPO2 /1, 0, 1, 1/   ! (1, vertical;  0, horizontal) polarization
!   data CHNM2 /10,11,12,13/
   data SCCF /10.65,10.65,18.70,18.70,23.80,36.50,36.50,89.00,89.00,166.0,166.0,183.31,183.31/
   !MHz
   data SCBW /100,100, 200,200,400,1000,1000,6000,6000,3400,3400,2000,2000/
   data ANPO /1,0, 1,0, 1, 1,0, 1,0, 1, 0, 1, 1/   ! (1, vertical;  0, horizontal) polarization
   data CHNM /1,2,3,4,5,6,7,8,9,10,11,12,13/
!   nch=nchan
!   allocate( SCCF(nch),SCBW(nch),ANPO(nch),CHNM(nch) )
!   if(nch==9) then   
!     bufrtbfile = 'GMI_bufr_table_S1'
!     SCCF = SCCF1
!     SCBW = SCBW1
!     ANPO = ANPO1
!     CHNM = CHNM1
!   elseif(nch==4) then
!     bufrtbfile = 'GMI_bufr_table_S2'
!     SCCF = SCCF2
!     SCBW = SCBW2
!     ANPO = ANPO2
!     CHNM = CHNM2
!   elseif(nch==13) then

   bufrtbfile = 'GMI_bufr_table_1CR'
!     SCCF = SCCF1cr
!     SCBW = SCBW1cr
!     ANPO = ANPO1cr
!     CHNM = CHNM1cr
!   endif 
   !change unit: => Hz
   SCCF = SCCF*1e+9
   SCBW = SCBW*1e+6
   nch=13
!

! 
!    20-08-2015  J.Jin
!    In meter ( not km )

   scAlt_meter=scAlt*1000        ! unit change, km => meter
!---------------------------------------------------------------------------

!---------------------------------------------------------------------------
! Initialize
!---------------------------------------------------------------------------
  nObs = 0;    nBad = 0;     nGood = 0

       gmiinfo='SAID SIID OGCE GSES SACV'
       gmiinfo_v(1) = 288  ! SAID -satellite id
       gmiinfo_v(2) = 519  ! SIID -Satellite instruments
       gmiinfo_v(3) = 173  ! OGCE -implies that NASA is the originator of this bufr data
       gmiinfo_v(4) = 0    ! GSES -no sub-center
       gmiinfo_v(5) = version ! SACV - version of source GMI data

       scaninfo='ORBN SLNM SCLON SCLAT HMSL &
                 YEAR MNTH DAYS HOUR MINU SECO'
       scaninfo_v( :) = XTEMP                       ! missing data. (JJJ) 999 exceeds the limit for some data 
                                                    ! (i.e., TMBR*(10^2): 0-2^(16-1). Therefore, TMBR is the default missing data 1e+12 in bufr).
       print*,  'Writing data into a bufr file.' 
!---------------------------------------------------------------------------
     iScan_x1 = NumberScansBeforeGranule+1
     iScan_x2 = nscan-NumberScansAfterGranule
     print*, ' iScan_x1, iScan_x2',  iScan_x1, iScan_x2
     print*, ' Fist scan ddhhmmss', day(iScan_x1), hour(iScan_x1), minu(iScan_x1),seco(iScan_x1)
     print*, ' Last scan ddhhmmss', day(iScan_x2), hour(iScan_x2), minu(iScan_x2),seco(iScan_x2)

!        stop
     call  ymd_thhz(nscan, iScan_x1, iScan_x2, &
                         year, mon, day, hour, &
                          need_date,need_syn, &
                   ymdthrz, id_oa, id_ob,tak)
     print*, 'id_oa = ', id_oa
     print*, 'id_ob = ', id_ob
     print*, 'ymdthrz = ', ymdthrz
     print*, 'tak = ', tak

!        stop

     bufrfile: do nfile=1, 2                   ! no more than 2 files for one orbit.
     print*, '================================================================'
! YELENA    tak(nfile) =0  means  file is not for need_date or need_syn
!            we skip it
        if (id_oa(nfile)< 0.or.tak(nfile).eq.0) cycle bufrfile 
        outfile = trim(prefix) // ymdthrz(nfile) // '.bufr'
!---------------------------------------------------------------------------
  OPEN(unit_table, FILE = bufrtbfile, ACTION = 'read')
! Check the status of the output file.
       ! check if the file exists.
       inquire(file=outfile,exist=ex)
       if (ex) then
       !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          ! check the existing bufr file.
          ex_obit=.FALSE.
          OPEN(unit_out, FILE = outfile, status='old', FORM='unformatted')
          call openbf(unit_out,'IN',unit_out)
          call datelen(10)
          img =1
          nsb = 0
          nsc = 1
          msg_report: do while (ireadmg(unit_out, subset_ex,jdate) == 0)
            sb_report: do while (ireadsb(unit_out) == 0)
             if(img==1) exit sb_report
             call ufbint(unit_out, orbn_v, 1, 1, iret, 'ORBN')
             if(abs(orbn_v - GranuleNumber) < 1e-10) then
               ex_obit=.TRUE.
               call ufbint(unit_out, scan_v, 1, 1, iret, 'SLNM')  !scan number
               call ufbint(unit_out, ymdhms, 6, 1, iret, 'YEAR MNTH DAYS HOUR MINU SECO')
                    call ymdhms2tim13(tim13,ymdhms)
               nsb=nsb +1
               if(nsb ==1) then
                 scan_old0(nsc) = int(scan_v)
                 tim13_old0(nsc) = tim13
               endif
                 if (int(scan_v) /= scan_old0(nsc) )then
                    nsc = nsc + 1
                    scan_old0(nsc) = int(scan_v)
                    call ymdhms2tim13(tim13,ymdhms)
                    tim13_old0(nsc) = tim13
                 endif
              endif
              exit sb_report  ! only check the fist pixel for the scan number.
            enddo sb_report
            img = img + 1
          enddo msg_report
          call closbf(unit_out)
          close(unit_out)
          if( nsc > nsc_max ) then
            print*, 'existing obit, nsc=',nsc
            print*, 'Therefore, need increase nsc_max in sub wr_gmi_bufr.'
            print*, 'ERROR.Program stopped at sub wr_gmi_bufr.'
!  J.Jin  11/06/2014  
            stop
          endif
          new_scan=.TRUE.
          if(ex_obit) then 
            if( allocated(scan_old) ) deallocate(scan_old)
            if( allocated(tim13_old) ) deallocate(tim13_old)
            allocate(scan_old(nsc))
            allocate(tim13_old(nsc))
            scan_old(1:nsc) = scan_old0(1:nsc)
            tim13_old(1:nsc) = tim13_old0(1:nsc)
                 new_scan=.false.
            isck_loop: DO iScan = id_oa(nfile), id_ob(nfile)
              !nsb = iScan-NumberScansBeforeGranule
              !if( minval(abs(scan_old - nsb)) > 0 ) then
              !  new_scan=.TRUE.       !a new obs is not in the existing file.
              !  exit isck_loop
              !endif
              ymdhms(1) = REAL( year(iScan))          ! YEAR
              ymdhms(2) = REAL( mon (iScan))          ! MONTH
              ymdhms(3) = REAL( day (iScan))          ! DAY
              ymdhms(4) = REAL( hour(iScan))          ! HOUR
              ymdhms(5) = REAL( minu(iScan))          ! Minute
              ymdhms(6) = REAL( seco(iScan))          ! Second
              call ymdhms2tim13(tim13,ymdhms)
              if( minval(abs(tim13_old - tim13)) > 0 ) then
                new_scan=.TRUE.       !a new obs is not in the existing file.
                exit isck_loop
              endif
            ENDDO isck_loop
            if(.not. new_scan) then
              write(*,*) 'OBS data for orib ', GranuleNumber, ' and scans between' 
              write(*,*) id_oa(nfile), 'and ', id_ob(nfile), 'are already in file '
              write(*,*) trim(outfile)
              write(*,*) 'Therefore, OBS data are not written in the bufr file.'
              cycle bufrfile 
            endif
          endif ! ex_obit
          !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

          if(new_scan) then 
            print *,'Append to the output file ',trim(outfile)
            OPEN(unit_out, FILE = outfile, status='old', FORM='unformatted')
          endif
       else
          print *,'Create new an output file ',trim(outfile)
          OPEN(unit_out, FILE = outfile, ACTION = 'write', FORM='unformatted')
       endif

       ! Specify date format: YYYYMMDDHH
       CALL datelen(10) 

       ! Connect bufrtable, bufr file to bufr lib
       !---------------------------------------------------------------------------
            if (ex) then
               CALL openbf(unit_out, 'APN', unit_table)                                  
            else
               CALL openbf(unit_out, 'OUT', unit_table)                                  
            endif
       ! ..........................................................................
       print*, '  output scans: ', id_oa(nfile), id_ob(nfile)
       print*, '  start ddhhmmss', day(id_oa(nfile)), hour(id_oa(nfile)), minu(id_oa(nfile)),seco(id_oa(nfile))
       print*, '  end   ddhhmmss', day(id_ob(nfile)), hour(id_ob(nfile)), minu(id_ob(nfile)),seco(id_ob(nfile))
       DO iScan = id_oa(nfile), id_ob(nfile)             !  write whole or a part of the obit
          nsb = iScan-NumberScansBeforeGranule
          scaninfo_v( 1) = GranuleNumber               ! ORBN -Orbit number-- has to be extracted from hdf file name
          scaninfo_v( 2) = nsb                         ! SLNM -Scan line number-- 
!          scaninfo_v( 3) = REAL( geoQuality (iScan))   ! NGQI  -Geolocation quality
          scaninfo_v( 3) = scLon(iScan)                ! SCLON -geodedic longitude of the spacecraft, scLon in HDF file.
          scaninfo_v( 4) = scLat(iScan)                ! SCLAT -geodedic latitude of the spacecraft, scLat in HDF file.
!         scaninfo_v( 5) = scAlt(iScan)                ! HMSL -Height or altitude- comes from scAlt in HDF file

!    20-08-2015  J.Jin
!   In meter ( not km)
           scaninfo_v( 5) = scAlt_meter(iScan)          ! HMSL -Height or altitude- comes from scAlt_meter in HDF file

          ! YYMMDD
          scaninfo_v( 6) = REAL( year(iScan))          ! YEAR
          scaninfo_v( 7) = REAL( mon (iScan))          ! MONTH
          scaninfo_v( 8) = REAL( day (iScan))          ! DAY
          ! HHMMSS
          scaninfo_v( 9) = REAL( hour(iScan))          ! HOUR
          scaninfo_v(10) = REAL( minu(iScan))          ! Minute
          scaninfo_v(11) = REAL( seco(iScan))          ! Second
          iDate = year(iScan)*1000000+mon(iScan)*10000+day(iScan)*100+hour(iScan)
          call  ymdhms2tim13(tim13,scaninfo_v(6:11))
          !if( ex .and. ex_obit .and. minval(abs(scan_old - nsb)) == 0 .and. &
          !    minval(abs(tim13_old - tim13)) == 0 ) cycle    ! not to write out this iScan
          if( ex .and. ex_obit .and.  &
             minval(abs(tim13_old - tim13)) == 0 ) cycle    ! not to write out this iScan


          if( iScan == id_oa(nfile) ) then 
            if (.not. ex) then  ! only write out in a new bufr file.
                 call openmb(unit_out,subset0,iDate)
                   call ufbint(unit_out,gmiinfo_v,5,1,iret, gmiinfo)
                   call ufbrep(unit_out,CHNM,1,nch,iret,'CHNM')
                   call ufbrep(unit_out,SCCF,1, nch, iret, 'SCCF')
                   call ufbrep(unit_out,SCBW,1, nch, iret, 'SCBW')
                   call ufbrep(unit_out,ANPO,1, nch, iret, 'ANPO')
                   CALL WRITSB(unit_out)                                             ! write the above data subset to the current message type
                CALL CLOSMG(unit_out)
            endif
          endif

          call openmb(unit_out,subset,iDate)
          do iPixel = 1, nPixel
            call ufbint(unit_out,scaninfo_v(1:11),11,1,iret, scaninfo)
                 !call ufbint(unit_out,geoQuality(iScan),1,1,iret, 'NGQI')
!                 print *, 'GMISQ, NGQI', scaninfo_v( 3),  geoQuality(iScan)
                 ! for the pixels in each scan.
            geoloc(1) = lat(iPixel, iScan)
            geoloc(2) = lon(iPixel, iScan)
            fov(1:nch)=iPixel
               ! for TMBR, SAZA, SGA
               tmbr_v(1:nchan) = Tb(:, iPixel, iScan)
               gmichq_v(:) = real(Quality(:, iPixel, iScan))

!     20-08-2015  J.Jin
               gmi_rfi(:) = real(RFIflag(:, iPixel, iScan))
     
               call ufbint(unit_out,geoloc,2,1,iret,'CLATH CLONH')
               call ufbint(unit_out,fov(1),1,1,iret,'FOVN')
               call ufbrep(unit_out,CHNM,1,nch,iret,'CHNM')
               call ufbrep(unit_out,gmichq_v, 1, nch, iret, 'GMICHQ')

!     20-08-2015  J.Jin
                call ufbrep(unit_out,gmi_rfi, 1, nch, iret, 'GMIRFI')

               call ufbrep(unit_out,tmbr_v, 1, nch, iret, 'TMBR')

               iang_v(1:ngs) = incidenceAngle(:, iPixel, iScan)
               call ufbrep(unit_out,iang_v, 1, ngs, iret, 'SAZA')
               gmi_ina(1:ngs) = satAzimuthAngle(:,iPixel,iScan)
               call ufbrep(unit_out,gmi_ina,1, ngs, iret,'SAMA')
               gmi_ina(1:ngs) = solarAzimuthAngle(:,iPixel,iScan)
               call ufbrep(unit_out,gmi_ina,1, ngs, iret,'SMA')
               gmi_ina(1:ngs) = solarZenAngle(:,iPixel,iScan)
               call ufbrep(unit_out,gmi_ina,1, ngs, iret,'SZA')
               sga_v(1:ngs)  = sunGlintAngle1c(:, iPixel, iScan)
               call ufbrep(unit_out,sga_v,  1, ngs, iret, 'SGA')

               CALL WRITSB(unit_out)       ! write the above data subset to the current message type
          enddo
          CALL CLOSMG(unit_out)
       ! end of the pixels
       END DO   ! iScan = 1, nscan
! ..........................................................................

! Close bufr file
!---------------------------------------------------------------------------
  CALL closbf(unit_out)

  CLOSE(unit_table)
  CLOSE(unit_out)

  exitCode = IRET

!---------------------------------------------------------------------------
  enddo  bufrfile
  !deallocate( SCCF,SCBW,ANPO,CHNM )
  print*, '================================================================'
  print*, 'done'
END SUBROUTINE write_gmi_bufr_1cr
!
