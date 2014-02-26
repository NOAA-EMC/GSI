!===============================================================================
! Name:       fm_mtsa.f90
!
!
! Type:         Main Program
!
!
! Description:
!       Performs footprint matching for MT SAPHIR (L1A2)
!       Parameter FMthin controls whether averaging (0), or thinning (1) is done
!
! Modules needed:
!       - Consts
!       - misc
!       - IO_MeasurData
!       - IO_Misc
!       - ErrorHandling
!
!
! History:
!       03-12-2013      Chris Grassotti    Original Code
!
!===============================================================================

program fm_mtsa

  USE Consts
  USE misc  
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling

  IMPLICIT NONE

  !---- INTRINSIC functions used in this module
  INTRINSIC :: ABS,ASSOCIATED,MAXVAL,MINVAL,NINT,RESHAPE,SUM,TRIM,ANY,COUNT

  INTEGER,            PARAMETER  :: ExpdSaphirfov=130
  INTEGER,            PARAMETER  :: nfovs_hr=130,nfovs_lr=65,nfovs_cr=26
  INTEGER,            PARAMETER  :: FMthin=1 ! 0: FOV averaging, 1: FOV thinning
  INTEGER,            PARAMETER  :: nqc_shared=4  ! the first 4 qc are the same: missing,validity,qac,geoQuality
  INTEGER,            PARAMETER  :: iu_Check=40,np=11
  REAL,               PARAMETER  :: epsilonFm = 0.01
  INTEGER,            PARAMETER  :: FMautoORmanual = 1 !0:Automatic, 1:Manual
  INTEGER,            PARAMETER  :: maxMSallowed=40 !ms
  REAL,               PARAMETER  :: mxdlatAllowed=0.5,mxdangAllowed=0.9

  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: sdrFiles
  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: FMsdrFiles
  REAL,               DIMENSION(:),    POINTER     :: Cfreq_1,Cfreq_2
  INTEGER,            DIMENSION(:),    POINTER     :: pol_1,pol_2,pol
  REAL,               DIMENSION(:),    ALLOCATABLE :: angleFM,Cfreq
  REAL,               DIMENSION(:,:),    ALLOCATABLE :: lat_1,lon_1,angle_1,RelAziAngle_1,SolZenAngle_1
  REAL,               DIMENSION(:,:),    ALLOCATABLE :: lat_2,lon_2,angle_2,RelAziAngle_2,SolZenAngle_2
  REAL,               DIMENSION(:,:,:),  ALLOCATABLE :: tb_1, tb_2
  INTEGER,            DIMENSION(:,:),    ALLOCATABLE :: qc_1, qc_2
  REAL,               DIMENSION(:,:,:),ALLOCATABLE :: FMerr
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: ScanLineInROI
  REAL       :: sum_tmp=0.0, lon_fm1=0.0, lon_fm2=0.0, lon_tmp=0.0  

  REAL,               DIMENSION(:,:),  ALLOCATABLE :: tb
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat,lon,angle,RelAziAngle,SolZenAngle
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: qc
  INTEGER                                          :: node
  INTEGER    :: nAvg,iAvg,ifov_fm

  CHARACTER(LEN=2)    :: ext
  CHARACTER(LEN=250)  :: fileFMSDR
  INTEGER    :: iu_sdr_1,iu_sdr_2,iu_FMsdr
  INTEGER    :: nfilesLR,nfilesHR,nFiles,iFile,nChan,nqc,nFovs,nScanL,nBatch
  INTEGER    :: nScanL_1,nFovs_1,nqc_1,nchan_1,nScanL_2,nFovs_2,nqc_2,nchan_2,nScanL_1_0
  INTEGER    :: iscanline,i,ifov,ichan,node_1,node_2,nScanLines,jfov
  INTEGER    :: scanDAY_1,scanYear_1,scanDAY_2,scanYear_2,Day_hr,Yr_hr,dDay,dYr
  INTEGER    :: scanUTC_1,scanUTC_2,UTC_hr,dUTC,dUTC0
  INTEGER    :: scanDAY,scanYear,scanUTC,iu_listLR,iu_listHR
  REAL       :: lat_hr,lon_hr,ang_hr,dLat,dLon,dAng,xVec(9)
  REAL       :: ALPH(3,3),Tbar,DELTAT,Tmax,Tmin
  INTEGER    :: WRITE_HEADER_DONE=0
  INTEGER    :: nProcessScanLines=0,iProcessScanLine=0
  INTEGER    :: nCountScanLinesROI=0 ! record individual file's number of scans falling in ROI
  INTEGER    :: TotalScanLinesROI=0  ! record total number of scans of all files falling in ROI

  !---Namelist data 
  CHARACTER(LEN=250) :: sdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathFMSDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: logFile=DEFAULT_VALUE_STR4
  INTEGER            :: FMresol=DEFAULT_VALUE_INT
  INTEGER            :: GeogrLimits=DEFAULT_VALUE_INT
  REAL               :: minLat=DEFAULT_VALUE_REAL
  REAL               :: maxLat=DEFAULT_VALUE_REAL
  REAL               :: minLon=DEFAULT_VALUE_REAL
  REAL               :: maxLon=DEFAULT_VALUE_REAL

  NAMELIST /ContrlFM/sdrfileList,pathFMSDR,&
       FMresol,GeogrLimits,minLat,maxLat,minLon,maxLon,LogFile

  !---Read control-data from namelist
  READ(*,NML=ContrlFM)

  !---Prepare Log file
  CALL OpenLogFile(Logfile)

  !---Read the file names of HR SDRs
  call ReadList(iu_listLR,trim(sdrfileList),sdrFiles,nFiles,FMsdrFiles,pathFMSDR,'FMSDR')

  IF( nfiles .lt. 1 ) CALL ErrHandl(ErrorType,Err_NoFilesFound,'HIGH') 

  !-------------------------------------------------------------------------------
  !   Add extension (CR, HR or LR) depending on the resolution chosen)
  !-------------------------------------------------------------------------------
  print *,'adding extension to output file names'
  DO ifile=1,nFiles
     print *,'ifile, sdrFiles(ifile): ',ifile, sdrFiles(ifile)
     !    call replace_path_string(sdrFiles(ifile), pathFMSDR, 'SDR', 'FMSDR', fileFMSDR)
     call replace_path_string_mt(sdrFiles(ifile), pathFMSDR, '_','SDR', 'FMSDR', fileFMSDR)
     print *,'FMresol:',FMresol
     print *,'fileFMSDR:',trim(fileFMSDR)
     IF( FMresol .eq. -1 ) THEN
        FMsdrFiles(ifile) = trim(fileFMSDR)//'.CR'
     ELSE IF( FMresol .eq. 0 ) THEN
        FMsdrFiles(ifile) = trim(fileFMSDR)//'.LR'
     ELSE IF( FMresol .eq. 1 ) THEN
        FMsdrFiles(ifile) = trim(fileFMSDR)//'.HR'
     ELSE
        CALL ErrHandl(ErrorType,Err_DifferFromExpect,'iFMType not supported')
     ENDIF
     print *,'FMsdrFiles(ifile): ',trim(FMsdrFiles(ifile))
  ENDDO

  !-------------------------------------------------------------------------------
  !   Begin FM process
  !-------------------------------------------------------------------------------
  TotalScanLinesROI=0

  print *,'begin files loop'
  FilesLoop: DO ifile=1,nFiles

     WRITE_HEADER_DONE=0
     write(*,*)
     write(*,*) 'ifile=', ifile
     write(*,'(A)') 'HR file='//TRIM(sdrFiles(ifile))

     !---Open/Read SDR header
     CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr_1,nScanL_1,nFovs_1,nqc_1,nchan_1,CFreq_1,pol_1)

     !----Consistency checks of nfov
     IF( nFovs_1  .ne. ExpdSaphirfov ) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'HR nFOV')

     nScanL = nScanL_1
     nChan  = nChan_1

     !---- set nqc = input nqc plus one from this fm process
     nqc = nqc_1 + 1
     nBatch = 1

     !----FM at CR resolution
     IF( FMresol .eq. -1 ) THEN  
        nFovs = nFovs_cr
        !----FM at LR resolution
     ELSE IF( FMresol .eq. 0 ) THEN
        nFovs = nFovs_lr
        !----FM at HR resolution
     ELSE IF( FMresol .eq. 1 ) THEN
        nFovs = nfovs_hr
     ELSE
        CALL ErrHandl(ErrorType,Err_DifferFromExpect,'iFMType not supported') 
     ENDIF
     nAvg=1
     iAvg=1

     !----Allocate memory for vectors/arrays
     ALLOCATE(lat(nFovs),lon(nFovs),angle(nFovs),angleFM(nchan),tb(nFovs,nchan),qc(nqc),&
          RelAziAngle(nFovs),SolZenAngle(nFovs),&
          lat_1(nFovs_1,nAvg),lon_1(nFovs_1,nAvg),angle_1(nFovs_1,nAvg),tb_1(nFovs_1,nchan_1,nAvg),qc_1(nqc_1,nAvg),&
          Cfreq(nChan),pol(nChan),FMerr(nScanL,nFovs,np), &
          RelAziAngle_1(nFovs_1,nAvg),SolZenAngle_1(nFovs_1,nAvg))

     Cfreq(1:nChan) = (/Cfreq_1(1:nchan_1)/)
     pol(1:nChan)   = (/pol_1(1:nchan_1)/)

     nScanLines = nScanL

     !-------------------------------------------------------------------------------------
     !   Loop over scanlines to determine whether scanline falls in Region of Interest(ROI)
     !-------------------------------------------------------------------------------------
     ALLOCATE(ScanLineInROI(nScanLines))
     ScanLineInROI(:) = 0
     nProcessScanLines = 0
     print *,'begin first scanline loop'
     ScanLLoopFlag: DO iscanLine=1,nScanLines

        !----Read SDR scanline content
        Do iavg=1,nAvg
           CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1(:,iAvg),nchan_1,nFovs_1,&
                angle_1(:,iAvg),tb_1(:,:,iAvg),lat_1(:,iAvg),lon_1(:,iAvg),node_1,&
                scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1(:,iAvg),SolZenAngle_1(:,iAvg))
           ! Convert lon from 0 -> 360 to -180 -> 180
           DO ifov=1,nFovs_1
              if(lon_1(ifov,iavg) .gt. 180.)then 
                 lon_1(ifov,iavg)=lon_1(ifov,iavg)-360.
              endif
           ENDDO
        Enddo

        DO ifov=1,nFovs
           !---When footprint matching is done at high resolution
           IF( FMresol .eq. 1 ) THEN
              !---- No lat/lon averaging: simply copy original HR lat/lon into output arrays
              lat(ifov) =  lat_1(ifov,1)
              lon(ifov) =  lon_1(ifov,1)

              !---When footprint matching is done at low resolution
              !   FMthin=0: Average FOV values         
           ELSE IF( FMresol .eq. 0 .and. FMthin .eq. 0) THEN
              !            CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ low resolution)') 
              ifov_fm                    = int((ifov-1)*2.)+1
              lat(ifov)                  = SUM(lat_1(ifov_fm:(ifov_fm+1),1))/2.
              !---To fix longitude jump problem
              if( ABS( lon_1(ifov_fm,1)-lon_1(ifov_fm+1,1) ) .le. 180 ) then
                 lon(ifov)                = SUM(lon_1(ifov_fm:(ifov_fm+1),1))/2.
              else
                 if( lon_1(ifov_fm,1) .lt. 0 ) then
                    lon_fm1 = lon_1(ifov_fm,1) + 360.
                 else
                    lon_fm1 = lon_1(ifov_fm,1)
                 endif
                 if( lon_1(ifov_fm+1,1) .lt. 0 ) then
                    lon_fm2 = lon_1(ifov_fm+1,1) + 360.
                 else
                    lon_fm2 = lon_1(ifov_fm+1,1)
                 endif

                 lon_tmp = ( lon_fm1 + lon_fm2 ) * 0.5
                 if( lon_tmp .gt. 180. ) then
                    lon(ifov) = lon_tmp- 360.
                 else
                    lon(ifov) = lon_tmp
                 endif
              endif
              !---When footprint matching is done at low resolution
              !   FMthin=1: Thin (subsample) FOV values         
           ELSE IF( FMresol .eq. 0 .and. FMthin .eq. 1) THEN
              !            CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ low resolution)') 
              ifov_fm                    = int((ifov-1)*2.)+1
              lat(ifov)                  = lat_1(ifov_fm,1)
              lon(ifov)                  = lon_1(ifov_fm,1)

              !---When footprint matching is done at coarse resolution
              !   FMthin=0: Average FOV values         
           ELSE IF( FMresol .eq.-1 .and. FMthin .eq. 0) THEN
              !            CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ coarse resolution)') 
              ifov_fm                    = int((ifov-1)*5.)+1
              lat(ifov)                  = lat_1(ifov_fm+2,1)
              lon(ifov)                  = lon_1(ifov_fm+2,1)

              !---When footprint matching is done at coarse resolution
              !   FMthin=1: Thin (subsample) FOV values         
           ELSE IF( FMresol .eq.-1 .and. FMthin .eq. 1) THEN
              !            CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ coarse resolution)') 
              ifov_fm                    = int((ifov-1)*5.)+1
              lat(ifov)                  = lat_1(ifov_fm,1)
              lon(ifov)                  = lon_1(ifov_fm,1)
           ENDIF

           ! sanity check for lat/lon (debugging)
           if(abs(lon(ifov)) .gt. 180.)then
              print *,'WARNING: computed lon: iscanLine,lon(ifov)=',iscanLine,lon(ifov)
              print *,'Scan line should be qc flagged in next scanline processing section'
              !                 CYCLE ScanLLoopFlag 
              !                 stop
           endif
           if(abs(lat(ifov)) .gt. 90.)then
              print *,'WARNING: computed lat: iscanLine,lat(ifov)=',iscanLine,lat(ifov)
              print *,'Scan line should be qc flagged in next scanline processing section'
           endif
        ENDDO
        IF( GeogrLimits .eq. 0 ) THEN
           ScanLineInROI(iscanLine) = 1
           nProcessScanLines = nProcessScanLines + 1
        ELSE IF( GeogrLimits .eq. 1 ) THEN
           GeoLoop: DO ifov=1,nFovs
              IF( lat(ifov) .ge. minLat .and. lat(ifov) .le. maxLat .and. &
                   lon(ifov) .ge. minLon .and. lon(ifov) .le. maxLon) THEN
                 ScanLineInROI(iscanLine) = 1
                 nProcessScanLines = nProcessScanLines + 1
                 exit GeoLoop
              ENDIF
           ENDDO GeoLoop
        ELSE
           CALL ErrHandl(ErrorType,Err_OptionNotSupported,'(in FM)') 
        ENDIF
     ENDDO ScanLLoopFlag

     !---Close SDR file
     close(iu_sdr_1)
     print *,'finished first scanline loop'

     !---- if nCountScanLinesROI < 1, no scanline falls in ROI, exit program ----
     nCountScanLinesROI = COUNT( ScanLineInROI .eq. 1 )
     if( nCountScanLinesROI .lt. 1 ) then
        CALL ErrHandl(WarningType,Warn_NotCoverROI,'orbit: '//trim(FMsdrFiles(ifile)) )
        DEALLOCATE(lat,lon,angle,angleFM,tb,qc,&
             RelAziAngle,SolZenAngle,&
             lat_1,lon_1,angle_1,tb_1,qc_1,&
             Cfreq,pol,FMerr, &
             RelAziAngle_1,SolZenAngle_1)
        DEALLOCATE(Cfreq_1,pol_1)
        !      DEALLOCATE(angleFM,tb,qc,lat_1,lon_1,angle_1,tb_1,qc_1,&
        !         Cfreq,pol,FMerr,RelAziAngle_1,SolZenAngle_1)
        DEALLOCATE(ScanLineInROI)

        CYCLE FilesLoop
        !CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'No FMSDR is generated!' )
     endif
     TotalScanLinesROI = TotalScanLinesROI + nCountScanLinesROI

     !----Rewind file and re-open header
     CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr_1,nScanL_1,nFovs_1,nqc_1,nchan_1,CFreq_1,pol_1)

     !----Open/Write FM-SDR header
     FmErr = -99

     !------------------------------------------------------------------------------
     !   Loop over scanlines
     !------------------------------------------------------------------------------
     print *,'begin second scanline loop'
     iProcessScanLine=0
     ScanLLoop: DO iscanLine=1,nScanLines
        Do iavg=1,nAvg
           !---Read SDR scanline content
           CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1(:,iAvg),nchan_1,nFovs_1,angle_1(:,iAvg),&
                tb_1(:,:,iAvg),lat_1(:,iAvg),lon_1(:,iAvg),node_1,scanUTC_1,scanDAY_1,scanYear_1,&
                RelAziAngle_1(:,iAvg),SolZenAngle_1(:,iAvg))

           ! Convert lon from 0 -> 360 to -180 -> 180
           DO ifov=1,nFovs_1
              if(lon_1(ifov,iavg) .gt. 180.)then 
                 lon_1(ifov,iavg)=lon_1(ifov,iavg)-360.
              endif
           ENDDO
        Enddo
        IF(ScanLineInROI(iscanLine) .EQ. 0 ) CYCLE ScanLLoop
        iProcessScanLine = iProcessScanLine+1

        !---Flag Output QC if any scanline affected by a non-0 QC
        qc(1:nqc)                                   = 0
        IF( ANY(qc_1(1:nqc_1,:) .ne. 0) ) qc(1) = 1
        ! QC check for bad lat/lon info
        IF( ANY(lat_1(1:nFovs_1,1:nAvg) .gt. 90.) .or. ANY(lat_1(1:nFovs_1,1:nAvg) .lt. -90.))then
           qc(1) = 1
           print *,'Bad lat_1 in current scanline: Setting qc(1)=1: iScanLine,lat_1(1:nFovs_1,1:nAvg)=',&
                iScanLine,lat_1(1:nFovs_1,1:nAvg)
        ENDIF
        IF( ANY(lon_1(1:nFovs_1,1:nAvg) .gt. 180.) .or. ANY(lat_1(1:nFovs_1,1:nAvg) .lt. -180.))then
           qc(1) = 1
           print *,'Bad lon_1 in current scanline: Setting qc(1)=1: iScanLine,lon_1(1:nFovs_1,1:nAvg)=',&
                iScanLine,lon_1(1:nFovs_1,1:nAvg)
        ENDIF

        !---- Perform time and position quality checks then FM if scanline not already flagged
        tb = -999.0
        IF( qc(1) .eq. 0 ) THEN 
           FOVsLoop: DO ifov=1,nFovs
              !---High resolution         
              IF( FMresol .eq. 1 ) THEN  !----FM @ high resolution
                 !                 CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ High resolution)') 
                 !--- direct copy - no averaging
                 tb(ifov,1:nchan_1) =  tb_1(ifov,1:nchan_1,1)
                 lat(ifov) =  lat_1(ifov,1)
                 lon(ifov) =  lon_1(ifov,1)

                 Angle(ifov)       = Angle_1(iFov,1)
                 RelAziAngle(ifov) = RelAziAngle_1(ifov,1)
                 SolZenAngle(ifov) = SolZenAngle_1(ifov,1)
                 node              = node_1

                 !---Low resolution, FMthin=0: Average FOV values         
              ELSE IF( FMresol .eq. 0 .and. FMthin .eq. 0) THEN  !----FM @ Low resolution
                 !                 CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ Low resolution)') 
                 !---Perform footprint-matching : TB averaging every 2 original FOVs
                 ifov_fm                    = int((ifov-1)*2.)+1
                 lat(ifov)                  = SUM(lat_1(ifov_fm:(ifov_fm+1),1))/2.
                 DO ichan=1,nchan_1
                    tb(iFov,ichan)          = SUM(tb_1(ifov_fm:ifov_fm+1,ichan,1:nAvg))/(nAvg*2.)
                 ENDDO

                 !---To fix longitude jump problem
                 if( ABS( lon_1(ifov_fm,1)-lon_1(ifov_fm+1,1) ) .le. 180 ) then
                    lon(ifov)                = SUM(lon_1(ifov_fm:(ifov_fm+1),1))/2.
                 else
                    if( lon_1(ifov_fm,1) .lt. 0 ) then
                       lon_fm1 = lon_1(ifov_fm,1) + 360.
                    else
                       lon_fm1 = lon_1(ifov_fm,1)
                    endif
                    if( lon_1(ifov_fm+1,1) .lt. 0 ) then
                       lon_fm2 = lon_1(ifov_fm+1,1) + 360.
                    else
                       lon_fm2 = lon_1(ifov_fm+1,1)
                    endif

                    lon_tmp = ( lon_fm1 + lon_fm2 ) * 0.5
                    if( lon_tmp .gt. 180. ) then
                       lon(ifov) = lon_tmp- 360.
                    else
                       lon(ifov) = lon_tmp
                    endif
                 endif

                 Angle(ifov)       = SUM(Angle_1(ifov_fm:ifov_fm+1,1:nAvg))/(nAvg*2.)
                 RelAziAngle(ifov) = SUM(RelAziAngle_1(ifov_fm:ifov_fm+1,1:nAvg))/(nAvg*2.)
                 SolZenAngle(ifov) = SUM(SolZenAngle_1(ifov_fm:ifov_fm+1,1:nAvg))/(nAvg*2.)
                 node              = node_1

                 !---Low resolution, FMthin=1: Thin (subsample) FOV values
              ELSE IF( FMresol .eq. 0 .and. FMthin .eq. 1) THEN  !----FM @ Low resolution
                 !                 CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ Low resolution)') 
                 !---Perform footprint-matching : Select one TB from every 2 original FOVs
                 ifov_fm                    = int((ifov-1)*2.)+1
                 lat(ifov)                  = lat_1(ifov_fm,1)
                 lon(ifov)                  = lon_1(ifov_fm,1)
                 DO ichan=1,nchan_1
                    tb(iFov,ichan)          = tb_1(ifov_fm,ichan,1)
                 ENDDO

                 Angle(ifov)       = Angle_1(ifov_fm,1)
                 RelAziAngle(ifov) = RelAziAngle_1(ifov_fm,1)
                 SolZenAngle(ifov) = SolZenAngle_1(ifov_fm,1)
                 node              = node_1

                 !---Coarse resolution, FMthin=0: Average FOV values         
              ELSE IF( FMresol .eq. -1 .and. FMthin .eq. 0) THEN   !----FM @ coarse resolution
                 !                 CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ Coarse resolution)') 
                 !---Perform footprint-matching : TB averaging every 5 original FOVs
                 !               print *,'After2.1: ifov,nqc,qc(1:nqc)=',ifov,nqc,qc(1:nqc)

                 ifov_fm                    = int((ifov-1)*5.)+1
                 lat(ifov)                  = lat_1(ifov_fm+2,1)
                 lon(ifov)                  = lon_1(ifov_fm+2,1)
                 DO ichan=1,nchan_1
                    tb(iFov,ichan)          = SUM(tb_1(ifov_fm:ifov_fm+4,ichan,1:nAvg))/(nAvg*5.)
                 ENDDO

                 Angle(ifov)       = SUM(Angle_1(ifov_fm:ifov_fm+4,1:nAvg))/(nAvg*5.)
                 RelAziAngle(ifov) = SUM(RelAziAngle_1(ifov_fm:ifov_fm+4,1:nAvg))/(nAvg*5.)
                 SolZenAngle(ifov) = SUM(SolZenAngle_1(ifov_fm:ifov_fm+4,1:nAvg))/(nAvg*5.)
                 node              = node_1

                 !---Coarse resolution, FMthin=1: Thin (subsample) FOV values         
              ELSE IF( FMresol .eq. -1 .and. FMthin .eq. 1) THEN   !----FM @ coarse resolution
                 !                 CALL ErrHandl(ErrorType,Err_NotImplemented,'(SAPHIR FM @ Coarse resolution)') 
                 !---Perform footprint-matching : Select one TB from every 5 original FOVs
                 !               print *,'After2.1: ifov,nqc,qc(1:nqc)=',ifov,nqc,qc(1:nqc)

                 ifov_fm                    = int((ifov-1)*5.)+1
                 lat(ifov)                  = lat_1(ifov_fm,1)
                 lon(ifov)                  = lon_1(ifov_fm,1)
                 DO ichan=1,nchan_1
                    tb(iFov,ichan)          = tb_1(ifov_fm,ichan,1)
                 ENDDO

                 Angle(ifov)       = Angle_1(ifov_fm,1)
                 RelAziAngle(ifov) = RelAziAngle_1(ifov_fm,1)
                 SolZenAngle(ifov) = SolZenAngle_1(ifov_fm,1)
                 node              = node_1
              ENDIF
           ENDDO FOVsLoop
        ENDIF

        !---Write out FM-SDR
        scanDAY  = scanDAY_1
        scanYear = scanYear_1
        scanUTC  = scanUTC_1

        IF (ScanLineInROI(iscanLine) .eq. 1) THEN

           !---if not write header yet, write header
           IF( WRITE_HEADER_DONE .EQ. 0 ) THEN
               CALL WriteHdrMeasurmts(FMsdrFiles(ifile),iu_FMsdr,nProcessScanLines*nFovs,&
                   nqc,nchan,nFovs,CFreq,pol,nProcessScanLines)
              WRITE_HEADER_DONE = 1
              write(*,'(A)') 'FM file='//TRIM(FMsdrFiles(ifile))
           ENDIF

           !----write contents
           IF( FMresol .eq. -1 ) THEN
              DO ifov=1,nFovs
                 angleFM(1:nchan) = angle(ifov)
                 CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan),lat(ifov),&
                      lon(ifov),node,scanUTC/1000.,scanDAY,scanYear,ifov,iscanLine,&
                      RelAziAngle(ifov),SolZenAngle(ifov))
              ENDDO
           ELSE IF( FMresol .eq. 0 ) THEN
              DO ifov=1,nFovs
                 angleFM(1:nchan) = angle(ifov)
                 CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan),lat(ifov),&
                      lon(ifov),node,scanUTC/1000.,scanDAY,scanYear,ifov,iscanLine,&
                      RelAziAngle(ifov),SolZenAngle(ifov))
              ENDDO
           ELSE IF( FMresol .eq. 1 ) THEN
              DO ifov=1,nFovs
                 angleFM(1:nchan) = angle(ifov)
                 CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan),lat(ifov),&
                      lon(ifov),node,scanUTC/1000.,scanDAY,scanYear,ifov,iscanLine,&
                      RelAziAngle(ifov),SolZenAngle(ifov))
              ENDDO
           ENDIF
        ENDIF  !--- ENDIF (ScanLineInROI)
     ENDDO ScanLLoop

     print *,'end second scanline loop'
     DEALLOCATE(angleFM,angle,lat,lon,tb,qc,&
          RelAziAngle,SolZenAngle,&
          lat_1,lon_1,angle_1,tb_1,qc_1,&
          Cfreq,pol,FMerr,RelAziAngle_1,SolZenAngle_1)

     !---Close SDR file
     CLOSE(iu_sdr_1)

     !---Close FM-SDR file
     IF( WRITE_HEADER_DONE .EQ. 1 ) CLOSE(iu_FMsdr)
     DEALLOCATE(ScanLineInROI)

  ENDDO FilesLoop
  print *,'end files loop'

  If(ASSOCIATED(CFreq_1)) DEALLOCATE(Cfreq_1)
  If(ASSOCIATED(pol_1))   DEALLOCATE(pol_1)
  DEALLOCATE(sdrFiles,FMsdrFiles)

  !---- if no FMSDR generated, return an error code ---- 
  if( TotalScanLinesROI .lt. 1 ) then
     CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'(in MT SAPHIR FM)' )
  endif

  CALL CloseLogFile()

end program fm_mtsa
