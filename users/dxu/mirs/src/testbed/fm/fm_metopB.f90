!===============================================================
! Name:       footprintMatching
!
!
! Type:         Main Program
!
!
! Description:
!       Performs footprint matching of the two MW
!       instruments onboard NOAA-18, namely AMSUA & MHS
!
! Modules needed:
!       - IO_MeasurData
!       - misc
!       - ErrorHandling
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

program fm_metopB
  USE IO_MeasurData
  USE IO_Misc
  USE misc  
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used in this module
  INTRINSIC :: ABS,ASSOCIATED,MAXVAL,MINVAL,NINT,RESHAPE,SUM,TRIM,ANY,COUNT

  INTEGER,            PARAMETER  :: ExpdAMSUAfov=30,ExpdMHSfov=90
  INTEGER,            PARAMETER  :: iu_Check=40,np=11
  REAL,               PARAMETER  :: epsilonFm = 1.0E+09 ! changed from epsilonFm = 0.01 to prevent HRR processing
  INTEGER,            PARAMETER  :: FMautoORmanual = 1 !0:Automatic, 1:Manual
  INTEGER,            PARAMETER  :: maxMSallowed=100 !ms
  REAL,               PARAMETER  :: mxdlatAllowed=0.5,mxdangAllowed=0.9

  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: sdrFilesAMSUA,sdrFilesMHS
  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: FMsdrFiles
  REAL,               DIMENSION(:),    POINTER     :: Cfreq_1,Cfreq_2
  INTEGER,            DIMENSION(:),    POINTER     :: pol_1,pol_2
  REAL,               DIMENSION(:),    ALLOCATABLE :: angleFM,Cfreq
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat_1,lon_1,angle_1,RelAziAngle_1,SolZenAngle_1
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: lat_2,lon_2,angle_2,RelAziAngle_2,SolZenAngle_2
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: tb_1
  REAL,               DIMENSION(:,:,:),ALLOCATABLE :: tb,tb_2
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: qc,qc_1,pol
  INTEGER,            DIMENSION(:,:),  ALLOCATABLE :: qc_2
  REAL,               DIMENSION(:,:,:),ALLOCATABLE :: FMerr
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: ScanLineInROI
  CHARACTER(LEN=2)  :: ext
  INTEGER    :: iu_sdr_1,iu_sdr_2,iu_FMsdr,nScanL_mhs_2skip0,nScanL_amsua_2skip0
  INTEGER    :: nScanL_mhs_2skip_requested,nScanL_amsua_2skip_requested
  INTEGER    :: nfilesAMSUA,nfilesMHS,nFiles,iFile,nChan,nqc,nFovs,nScanL,nBatch
  INTEGER    :: nScanL_1,nFovs_1,nqc_1,nchan_1,nScanL_2,nFovs_2,nqc_2,nchan_2,nScanL_1_0
  INTEGER    :: iscanline,i,ifov,ichan,node_1,node_2(3),nScanLines,jscanline
  INTEGER    :: scanDAY_1,scanYear_1,scanDAY_2(3),scanYear_2(3),Day_mhs,Yr_mhs,dDay,dYr
  INTEGER    :: scanUTC_1,scanUTC_2(3),UTC_mhs,dUTC,dUTC0
  INTEGER    :: scanDAY,scanYear,scanUTC,iu_listAMSUA,iu_listMHS
  REAL       :: lat_mhs,lon_mhs,ang_mhs,dLat,dLon,dAng,xVec(9)
  REAL       :: ALPH(3,3),Tbar,DELTAT,Tmax,Tmin
  INTEGER    :: WRITE_HEADER_DONE=0
  INTEGER    :: nProcessScanLines=0
  INTEGER    :: nCountScanLinesROI=0 ! record individual file's number of scans falling in ROI
  INTEGER    :: TotalScanLinesROI=0  ! record total number of scans of all files falling in ROI
  INTEGER    :: dUTC_min=99999,nScanL_amsua_2skip0_min=0,nScanL_mhs_2skip0_min=0 

  !---Namelist data 
  CHARACTER(LEN=250) :: sdrfileList_amsua=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: sdrfileList_mhs=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathFMSDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: prefCheck=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: LogFile=DEFAULT_VALUE_STR4
  INTEGER            :: iOutpFM_accur=DEFAULT_VALUE_INT
  INTEGER            :: iFMtype=DEFAULT_VALUE_INT
  INTEGER            :: nScanL_mhs_2skip=DEFAULT_VALUE_INT
  INTEGER            :: nScanL_amsua_2skip=DEFAULT_VALUE_INT
  INTEGER            :: ScanLindx_mhs_TimeColloc=DEFAULT_VALUE_INT
  INTEGER            :: norbits2process=DEFAULT_VALUE_INT
  INTEGER            :: GeogrLimits=DEFAULT_VALUE_INT
  REAL               :: minLat=DEFAULT_VALUE_REAL
  REAL               :: maxLat=DEFAULT_VALUE_REAL
  REAL               :: minLon=DEFAULT_VALUE_REAL
  REAL               :: maxLon=DEFAULT_VALUE_REAL
  
  NAMELIST /ContrlFM/sdrfileList_amsua,sdrfileList_mhs,pathFMSDR,iOutpFM_accur,&
       prefCheck,iFMtype,nScanL_amsua_2skip,nScanL_mhs_2skip,ScanLindx_mhs_TimeColloc,&
       norbits2process,LogFile,GeogrLimits,minLat,maxLat,minLon,maxLon
  !---Read control-data from namelist
  READ(*,NML=ContrlFM)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of AMSUA SDRs
  call ReadList(iu_listAMSUA,trim(sdrfileList_amsua),sdrFilesAMSUA,nFilesAMSUA,&
       FMsdrFiles,pathFMSDR,'FMSDR_')
  DEALLOCATE(FMsdrFiles)
  IF (nfilesAMSUA .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'AMSUA') 
  !---Read the file names of MHS SDRs
  call ReadList(iu_listMHS,trim(sdrfileList_mhs),sdrFilesMHS,nFilesMHS,&
       FMsdrFiles,pathFMSDR,'FMSDR_')
  IF (nfilesMHS .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'MHS')
  !-------------------------------------------------------------------------------
  !   Consistency checks
  !-------------------------------------------------------------------------------
  IF (nfilesAMSUA.ne.nfilesMHS) CALL ErrHandl(ErrorType,Err_InconsNumber,'of files AMSUA/MHS ')
  nFiles=minval((/nfilesAMSUA,norbits2process/))
  !-------------------------------------------------------------------------------
  !   Add extension (HR or LR) depending on the resolution chosen)
  !-------------------------------------------------------------------------------
  DO ifile=1,nFiles
    IF (iFMtype.eq.0) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.LR'
    IF (iFMtype.eq.1) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.HR'
  ENDDO
  !-------------------------------------------------------------------------------
  !   Begin FM process
  !-------------------------------------------------------------------------------
  nScanL_mhs_2skip_requested   = nScanL_mhs_2skip
  nScanL_amsua_2skip_requested = nScanL_amsua_2skip
  TotalScanLinesROI=0
  FilesLoop: DO ifile=1,nFiles
    WRITE_HEADER_DONE=0
    write(*,*)
    write(*,'(A)')'Input AMSUA file='//trim(sdrFilesAMSUA(ifile))
    write(*,'(A)')'Input   MHS file='//trim(sdrFilesMHS(ifile))
    !---Open/Read AMSU/A SDR header
    CALL ReadRadHdrScanLMode(sdrFilesAMSUA(ifile),iu_sdr_1,nScanL_1,nFovs_1, &
         nqc_1,nchan_1,CFreq_1,pol_1)
    write(*,*)'AMSUA scan=',nScanL_1, ', AMSUA nfov=', nFovs_1, ', AMSUA nprf=', nScanL_1*nFovs_1	 
    !---Open/Read MHS SDR header
    CALL ReadRadHdrScanLMode(sdrFilesMHS(ifile),iu_sdr_2,nScanL_2,nFovs_2,   &
         nqc_2,nchan_2,CFreq_2,pol_2)
    write(*,*)'MHS   scan=',nScanL_2, ', MHS   nfov=', nFovs_2, ', MHS   nprf=', nScanL_2*nFovs_2	 
    !---Consistency checks
    IF (nFovs_1.ne.ExpdAMSUAfov) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'AMSUA nFOV') 
    IF (nFovs_2.ne.ExpdMHSfov)   CALL ErrHandl(ErrorType,Err_DifferFromExpect,'MHS nFOV') 
    !---Allocate memory for vectors/arrays
    nChan           = nChan_1+nChan_2
    nqc             = nqc_1+nqc_2+1
    !---FM at AMSU A resolution
    IF (iFMtype .eq. 0) THEN
       nScanL          = minval((/nScanL_1,nint(nScanL_2/3.)/))
       nFovs           = nFovs_1
       nBatch          = 1
       !----test
       nScanL=nScanL-1
       !--------
       nscanL_1        = nScanL
    ENDIF
    !---FM at MHS resolution
    IF (iFMtype .eq. 1) THEN
       nScanL          = minval((/(nScanL_1*3),(nint(nScanL_2/3.)*3)/))
       nFovs           = nFovs_2
       nBatch          = 3
       !----test
       nScanL=nScanL-3
       !--------
       nscanL_1        = nScanL/3
    ENDIF
    ALLOCATE(angleFM(nchan),tb(nFovs,nchan,nBatch),qc(nqc),&
         lat_1(nFovs_1),lon_1(nFovs_1),angle_1(nFovs_1),tb_1(nFovs_1,nchan_1),qc_1(nqc_1),    &
         lat_2(nFovs_2,3),lon_2(nFovs_2,3),angle_2(nFovs_2,3),tb_2(nFovs_2,nchan_2,3),        &
         qc_2(nqc_2,3),Cfreq(nChan),pol(nChan),FMerr(nScanL,nFovs,np),                        &
         RelAziAngle_1(nFovs_1),SolZenAngle_1(nFovs_1), &
         RelAziAngle_2(nFovs_2,3),SolZenAngle_2(nFovs_2,3))
    Cfreq(1:nChan)  = (/Cfreq_1(1:nchan_1),Cfreq_2(1:nchan_2)/)
    pol(1:nChan)    = (/pol_1(1:nchan_1),pol_2(1:nchan_2)/)

    !---Determine how much scanlines to skip to time-synchronize AMSUA and MHS
    nScanL_amsua_2skip0 = 0
    dUTC                = 9999
    dUTC_min            = 99999
    !---Always skip first scanline of AMSU-A
    CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
         tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
    nScanL_amsua_2skip0 = nScanL_amsua_2skip0+1
    !---Loop over scanlines
    AMSUALoop: DO WHILE (nScanL_amsua_2skip0 .le. nScanL_1-1) 
       CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
       nScanL_amsua_2skip0 = nScanL_amsua_2skip0+1
       nScanL_mhs_2skip0   = 0
       MHSLoop: DO WHILE (nScanL_mhs_2skip0 .le. nScanL_2-1) 
          CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2(:,1),nchan_2,nFovs_2,angle_2(:,1),&
               tb_2(:,:,1),lat_2(:,1),lon_2(:,1),node_2(1),scanUTC_2(1),scanDAY_2(1),     &
               scanYear_2(1),RelAziAngle_2(:,1),SolZenAngle_2(:,1))
          IF (scanDAY_2(1).ne.scanDAY_1) scanUTC_2(1)=scanUTC_2(1)-86400000
          nScanL_mhs_2skip0=nScanL_mhs_2skip0+1
          dUTC=abs(scanUTC_1-scanUTC_2(1))
          dLat=abs(Lat_1(1)-Lat_2(1,1))
          IF (dUTC .lt. dUTC_min) THEN
            dUTC_min = dUTC
            nScanL_amsua_2skip0_min = nScanL_amsua_2skip0
            nScanL_mhs_2skip0_min = nScanL_mhs_2skip0 
          ENDIF
          IF (dUTC .le. maxMSallowed) EXIT AMSUALoop
       ENDDO MHSLoop
       close(iu_sdr_2)
       CALL ReadRadHdrScanLMode(sdrFilesMHS(ifile),iu_sdr_2,nScanL_2,nFovs_2,   &
            nqc_2,nchan_2,CFreq_2,pol_2)
    ENDDO AMSUALoop
    close(iu_sdr_1)
    close(iu_sdr_2)

    nScanL_amsua_2skip0 = nScanL_amsua_2skip0_min
    nScanL_mhs_2skip0   = nScanL_mhs_2skip0_min

    !---Rewind files and re-open their headers and skip the computed number of scanlines 
    CALL ReadRadHdrScanLMode(sdrFilesAMSUA(ifile),iu_sdr_1,nScanL_1_0,nFovs_1,nqc_1,nchan_1,CFreq_1,pol_1)
    CALL ReadRadHdrScanLMode(sdrFilesMHS(ifile),iu_sdr_2,nScanL_2,nFovs_2,nqc_2,nchan_2,CFreq_2,pol_2)

    !-----------------------------------------------------------------
    !     Manual Shift of scanlines
    !-----------------------------------------------------------------
    IF (FMautoORmanual .eq. 1) THEN
       nScanL_amsua_2skip = nScanL_amsua_2skip0 + nScanL_amsua_2skip_requested
       nScanL_mhs_2skip   = nScanL_mhs_2skip0   + nScanL_mhs_2skip_requested
    ENDIF

    !-----------------------------------------------------------------
    !     Automatic Shift of scanlines (based on time synchronzation only)
    !-----------------------------------------------------------------
    IF (FMautoORmanual .eq. 0) THEN 
       nScanL_amsua_2skip = nScanL_amsua_2skip0 
       nScanL_mhs_2skip   = nScanL_mhs_2skip0  
    ENDIF

    nScanL=nScanL-maxval((/nint(nScanL_mhs_2skip/3.),nScanL_amsua_2skip/))-1

    !---shift MHS by X scan lines 
    DO i=1,nScanL_mhs_2skip
       CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2(:,1),nchan_2,nFovs_2,angle_2(:,1),&
            tb_2(:,:,1),lat_2(:,1),lon_2(:,1),node_2(1),scanUTC_2(1),scanDAY_2(1),     &
            scanYear_2(1),RelAziAngle_2(:,1),SolZenAngle_2(:,1))
       IF (scanDAY_2(1).ne.scanDAY_1) scanUTC_2(1)=scanUTC_2(1)-86400000
    ENDDO
    !---shift AMSUA by X scan lines
    DO i=1,nScanL_amsua_2skip
       CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
    ENDDO

    !---Open/Write FM-SDR header
    FmErr      = -99
    !---This is AMUSA scan lines !!!
    nScanLines = nScanL_1-maxval((/nint(nScanL_mhs_2skip/3.),nScanL_amsua_2skip/))-1

    !------------------------------------------------------------------------------
    !   Loop over scanlines to determine whether scanline falls in Region of Interest
    !------------------------------------------------------------------------------
    ALLOCATE(ScanLineInROI(nScanLines))
    ScanLineInROI(:) = 0
    nProcessScanLines = 0
    ScanLLoopFlag: DO iscanLine=1,nScanLines  ! loop over AMSUA scan lines
       !---Read AMSUA SDR scanline content
       CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
       !---Read 3 MHS SDR consecutive scanlines content
       DO i=1,3
          CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2(:,i),nchan_2,nFovs_2,angle_2(:,i),&
               tb_2(:,:,i),lat_2(:,i),lon_2(:,i),node_2(i),scanUTC_2(i),scanDAY_2(i),     &
               scanYear_2(i),RelAziAngle_2(:,i),SolZenAngle_2(:,i))
       ENDDO
      
      IF (GeogrLimits .eq. 0) THEN
          ScanLineInROI(iscanLine) = 1
          if( iFMtype .eq. 0 ) then
            nProcessScanLines = nProcessScanLines + 1
	  else if( iFMtype .eq. 1 ) then
            nProcessScanLines = nProcessScanLines + 3
	  endif
      ELSE IF (GeogrLimits .eq. 1  .and. iFMtype .eq. 0) THEN
          GeoLoop1: DO ifov=1,nFovs
              IF( lat_1(ifov) .ge. minLat .and. lat_1(ifov) .le. maxLat .and. &
                  lon_1(ifov) .ge. minLon .and. lon_1(ifov) .le. maxLon) THEN
                  ScanLineInROI(iscanLine) = 1
                  nProcessScanLines = nProcessScanLines + 1
                  exit GeoLoop1
              ENDIF
          ENDDO GeoLoop1
      ELSE IF (GeogrLimits .eq. 1  .and. iFMtype .eq. 1) THEN
          DO i=1,nBatch
          GeoLoop2: DO ifov=1,nFovs
              IF( lat_2(ifov,i) .ge. minLat .and. lat_2(ifov,i) .le. maxLat .and. &
                  lon_2(ifov,i) .ge. minLon .and. lon_2(ifov,i) .le. maxLon) THEN
                  ScanLineInROI(iscanLine) = 1 
                  nProcessScanLines = nProcessScanLines + 1
                  exit GeoLoop2
              ENDIF
          ENDDO GeoLoop2
          ENDDO
      ELSE
          CALL ErrHandl(ErrorType,Err_OptionNotSupported,'(in FM)') 
      ENDIF
    ENDDO ScanLLoopFlag
    close(iu_sdr_1)
    close(iu_sdr_2)

    !---- if nCountScanLinesROI < 1, no scanline falls in ROI, exit program ----
    nCountScanLinesROI = COUNT( ScanLineInROI .eq. 1 )
    if( nCountScanLinesROI .lt. 1 ) then
      !print *, 'No scanline found in the ROI!'
      !print *, 'No FMSDR data is generated'
      CALL ErrHandl(WarningType,Warn_NotCoverROI,'orbit: '//trim(FMsdrFiles(ifile)) )

      DEALLOCATE(angleFM,tb,qc,lat_1,lon_1,angle_1,tb_1,qc_1,lat_2,&
         lon_2,angle_2,tb_2,qc_2,Cfreq,pol,FMerr,&
         RelAziAngle_1,SolZenAngle_1,RelAziAngle_2,SolZenAngle_2)

      DEALLOCATE(ScanLineInROI)
     
      CYCLE FilesLoop
      !CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'No FMSDR is generated!' )
    endif 
    TotalScanLinesROI = TotalScanLinesROI + nCountScanLinesROI


    !---Rewind files and re-open their headers and skip the computed number of scanlines 
    CALL ReadRadHdrScanLMode(sdrFilesAMSUA(ifile),iu_sdr_1,nScanL_1_0,nFovs_1,nqc_1,nchan_1,CFreq_1,pol_1)
    CALL ReadRadHdrScanLMode(sdrFilesMHS(ifile),iu_sdr_2,nScanL_2,nFovs_2,nqc_2,nchan_2,CFreq_2,pol_2)

    !-----------------------------------------------------------------
    !     Manual Shift of scanlines
    !-----------------------------------------------------------------
    IF (FMautoORmanual .eq. 1) THEN
       nScanL_amsua_2skip = nScanL_amsua_2skip0 + nScanL_amsua_2skip_requested
       nScanL_mhs_2skip   = nScanL_mhs_2skip0   + nScanL_mhs_2skip_requested
    ENDIF

    !-----------------------------------------------------------------
    !     Automatic Shift of scanlines (based on time synchronzation only)
    !-----------------------------------------------------------------
    IF (FMautoORmanual .eq. 0) THEN 
       nScanL_amsua_2skip = nScanL_amsua_2skip0 
       nScanL_mhs_2skip   = nScanL_mhs_2skip0  
    ENDIF

    nScanL=nScanL-maxval((/nint(nScanL_mhs_2skip/3.),nScanL_amsua_2skip/))-1

    !---shift MHS by X scan lines 
    DO i=1,nScanL_mhs_2skip
       CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2(:,1),nchan_2,nFovs_2,angle_2(:,1),&
            tb_2(:,:,1),lat_2(:,1),lon_2(:,1),node_2(1),scanUTC_2(1),scanDAY_2(1),     &
            scanYear_2(1),RelAziAngle_2(:,1),SolZenAngle_2(:,1))
       IF (scanDAY_2(1).ne.scanDAY_1) scanUTC_2(1)=scanUTC_2(1)-86400000
    ENDDO
    !---shift AMSUA by X scan lines
    DO i=1,nScanL_amsua_2skip
       CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
    ENDDO

    !---Open/Write FM-SDR header
    FmErr      = -99
    nScanLines = nScanL_1-maxval((/nint(nScanL_mhs_2skip/3.),nScanL_amsua_2skip/))-1

    !------------------------------------------------------------------------------
    !   Loop over scanlines
    !------------------------------------------------------------------------------
    jscanline=0
    ScanLLoop: DO iscanLine=1,nScanLines
       !---Read AMSUA SDR scanline content
       CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
       !---Read 3 MHS SDR consecutive scanlines content
       DO i=1,3
          CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2(:,i),nchan_2,nFovs_2,angle_2(:,i),&
               tb_2(:,:,i),lat_2(:,i),lon_2(:,i),node_2(i),scanUTC_2(i),scanDAY_2(i),     &
               scanYear_2(i),RelAziAngle_2(:,i),SolZenAngle_2(:,i))
          IF (scanDAY_2(i).ne.scanDAY_1) scanUTC_2(i)=scanUTC_2(i)-86400000
          IF (scanUTC_2(i) .lt. 0) scanUTC_2(i)=scanUTC_2(i)+86400000
       ENDDO

       !---Find out which is the MHS scanline that synchs with AMSUA (on the fly), by default, 1
       ScanLindx_mhs_TimeColloc=1
       dUTC0=999999
       DO i=1,3
          UTC_mhs = scanUTC_2(i)
          Day_mhs = scanDAY_2(i)
          Yr_mhs  = scanYear_2(i)
          dUTC    = ABS(UTC_mhs-scanUTC_1)
          IF (dUTC0 .gt. dUTC) THEN
             ScanLindx_mhs_TimeColloc=i
             dUTC0=dUTC
          ENDIF
       ENDDO
       !---Flag Output QC if any scanline affected by a non-0 QC
       qc(1:nqc)                   = 0
       qc(1+1:nqc_1+1)             = qc_1(1:nqc_1)
       DO i=1,nqc_2
          qc(nqc_1+1+i)            = SUM(qc_2(i,:))
       ENDDO

       !---- only set 1 if both AMSUA and MHS have channels bad
       IF (ANY(qc(2:nqc_1+1) .ne. 0) .and. ANY(qc(nqc_1+2:nqc_1+nqc_2+1) .ne. 0)) qc(1)=1

       !---Take the time of the user-driven scanline
       UTC_mhs= scanUTC_2(ScanLindx_mhs_TimeColloc)
       Day_mhs= scanDAY_2(ScanLindx_mhs_TimeColloc)
       Yr_mhs = scanYear_2(ScanLindx_mhs_TimeColloc)

       !---Compute Time/day/Yr difference
       dUTC   = ABS(UTC_mhs-scanUTC_1)
       dDay   = ABS(Day_mhs-scanDAY_1)
       dYr    = ABS(Yr_mhs-scanYear_1)
       !---Perform time and position quality checks then FM if scanline not already flagged
       tb =-99
       IF (qc(1) .eq. 0) THEN 
          !---Check time-collocation 
          IF (dUTC .gt. maxMSallowed) THEN
             print *, 'Scan#',iscanLine,' dUTC:',dUTC,' MxAllowed:',maxMSallowed,&
                  UTC_mhs,scanUTC_1,'===',scanUTC_2(1:3)
             CALL ErrHandl(WarningType,Warn_TimeIncons,'Please check SDR files.')
             CYCLE ScanLLoop
          ENDIF
          !---Check Lat/Lon collocation and Angle similarity
          FOVsLoop: DO ifov=1,nFovs_1
             !---Average MHS lat/lon/Angle
             Lat_mhs = SUM(lat_2((ifov-1)*3+1:(ifov-1)*3+3,1:3))/9.
             Lon_mhs = SUM(lon_2((ifov-1)*3+1:(ifov-1)*3+3,1:3))/9.
             Ang_mhs = SUM(angle_2((ifov-1)*3+1:(ifov-1)*3+3,1:3))/9.
             !---Take MHS lat/lon/Angle from the 2nd scanline/2nd position (footprint center)
             Lat_mhs = lat_2((ifov-1)*3+2,2)
             Lon_mhs = lon_2((ifov-1)*3+2,2)
             Ang_mhs = angle_2((ifov-1)*3+2,2)
             !---Compute Difference in Lat/Lon/Angle
             dlat    = abs(Lat_mhs-lat_1(ifov))
             dlon    = abs(Lon_mhs-lon_1(ifov))
             dAng    = abs(Ang_mhs-angle_1(ifov))
             dLat    = minval((/dLat,  90.-dLat /))
             !---Check Lat/Lon/Angle-collocation
             IF (dLat .ge. mxdlatAllowed) THEN
                CALL ErrHandl(WarningType,Warn_LatIncons,'Please check SDR files.')
                CYCLE ScanLLoop
             ENDIF
             IF (dAng .ge. mxdAngAllowed) THEN
                CALL ErrHandl(WarningType,Warn_AngleIncons,'Please check SDR files.')
                CYCLE ScanLLoop
             ENDIF
             !----FM @ AMSUA spatial resolution
             IF (iFMtype .eq. 0) THEN
                !---Perform footprint-matching (for now, simple averaging of 3x3 MHS )
                tb(iFov,1:nchan_1,1)        = tb_1(iFov,1:nchan_1)
                DO ichan=1,nchan_2
                   tb(iFov,nchan_1+ichan,1) = SUM(tb_2((ifov-1)*3+1:(ifov-1)*3+3,ichan,1:3))/9.
                ENDDO
             ENDIF
             !----FM @ MHS spatial resolution
             IF (iFMtype .eq. 1) THEN
                !---Perform footprint-matching (for now, consider AMSUA valid atthe 3x3 MHS footprints)
                DO i=1,3
                   DO ichan=1,nchan_2
                      !----Compute weighting factors to adjust low resolution TBs (based on 89 GHz)
                      IF (ichan.eq.1) THEN
                         Tbar = SUM(tb_2((ifov-1)*3+1:(ifov-1)*3+3,ichan,1:3))/9.
                         Tmax = maxval(tb_2((ifov-1)*3+1:(ifov-1)*3+3,ichan,1:3))
                         Tmin = minval(tb_2((ifov-1)*3+1:(ifov-1)*3+3,ichan,1:3))
                         DELTAT =Tmax-Tmin
                         IF (DELTAT.le.epsilonFm) THEN 
                            ALPH=0.
                         ELSE
                            ALPH(1:3,i)=(tb_2((iFov-1)*3+1:(iFov-1)*3+3,ichan,i) - tbar ) / deltat
                         ENDIF
                      ENDIF
                      !----store high-resolution MHS TBs
                      tb((iFov-1)*3+1:(iFov-1)*3+3,nchan_1+ichan,i) = tb_2((iFov-1)*3+1:(iFov-1)*3+3,ichan,i)
                   ENDDO
                   !---Adjust low-resolution TBs with weighting fcts (but only those that are 89-like sensitive)
                   DO ichan=1,nchan_1
                      IF (ichan.le.3 .or. ichan.eq.15) THEN
                         tb((iFov-1)*3+1:(iFov-1)*3+3,ichan,i) = tb_1(iFov,ichan)+(DELTAT)*ALPH(1:3,i)
                      ELSE
                         tb((iFov-1)*3+1:(iFov-1)*3+3,ichan,i) = tb_1(iFov,ichan)
                      ENDIF
                   ENDDO
                ENDDO
             ENDIF
             !---Metric for assessing FM accuracy is DeltaTB @89.
             !----FM @ AMSUA spatial resolution
             IF (iFMtype .eq. 0) THEN
                Xvec(1:9)                = RESHAPE(tb_2((ifov-1)*3+1:(ifov-1)*3+3,1,1:3),(/9/))
                FMerr(iscanLine,iFov,1)  = tb(iFov,nchan_1,1)
                FMerr(iscanLine,iFov,2)  = tb(iFov,nchan_1+1,1)
                FMerr(iscanLine,iFov,3)  = Stdev(Xvec)
                FMerr(iscanLine,iFov,4)  = qc(1)
                FMerr(iscanLine,iFov,5)  = dLat
                FMerr(iscanLine,iFov,6)  = dLon
                FMerr(iscanLine,iFov,7)  = dAng
                FMerr(iscanLine,iFov,8)  = Lat_mhs
                FMerr(iscanLine,iFov,9)  = Lon_mhs
                FMerr(iscanLine,iFov,10) = node_1
                FMerr(iscanLine,iFov,11) = Day_MHS
             ENDIF
             !----FM @ MHS spatial resolution
             IF (iFMtype .eq. 1) THEN
                Xvec(1:9)                  = RESHAPE(tb_2((ifov-1)*3+1:(ifov-1)*3+3,1,1:3),(/9/))
                DO i=1,3
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,1)  = tb((iFov-1)*3+1:(iFov-1)*3+3,nchan_1,i)
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,2)  = tb((iFov-1)*3+1:(iFov-1)*3+3,nchan_1+1,i)
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,3)  = Stdev(Xvec)
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,4)  = qc(1)
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,5)  = dLat
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,6)  = dLon
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,7)  = dAng
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,8)  = lat_2((ifov-1)*3+1:(ifov-1)*3+3,i)
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,9)  = Lon_2((ifov-1)*3+1:(ifov-1)*3+3,i)
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,10) = node_1
                   FMerr((iscanLine-1)*3+i,(iFov-1)*3+1:(iFov-1)*3+3,11) = Day_MHS
                ENDDO
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
	   write(*,'(A)')'Output FM file='//trim(FMsdrFiles(ifile))
	   write(*,*)'FM scan =',nProcessScanLines
	   write(*,*)'FM nfov =',nFovs
	   write(*,*)'FM nprf =',nProcessScanLines*nFovs
           CALL WriteHdrMeasurmts(FMsdrFiles(ifile),iu_FMsdr,nProcessScanLines*nFovs,&
                nqc,nchan,nFovs,CFreq,pol,nProcessScanLines)
           WRITE_HEADER_DONE = 1
         ENDIF
         !---write contents
         IF (iFMtype .eq. 0) THEN
           DO ifov=1,nFovs
             angleFM(1:nchan) = angle_1(ifov)
             CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan,1),lat_1(ifov),&
                  lon_1(ifov),node_1,scanUTC_1/1000.,scanDAY,scanYear,ifov,iscanLine,&
                  RelAziAngle_1(ifov),SolZenAngle_1(ifov))
           ENDDO
	 ELSE IF (iFMtype .eq. 1) THEN
             DO i=1,nBatch
	     jscanline=jscanline+1
             DO ifov=1,nFovs
                  angleFM(1:nchan) = angle_2(ifov,i)
                  CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan,i),lat_2(ifov,i),&
                       lon_2(ifov,i),node_2(i),scanUTC_2(i)/1000.,scanDAY_2(i),scanYear_2(i),ifov,jscanLine,&
                       RelAziAngle_2(ifov,i),SolZenAngle_2(ifov,i))
             ENDDO
             ENDDO
         ENDIF
       ENDIF  !--- ENDIF (ScanLineInROI)

    ENDDO ScanLLoop
    !---Output FM-accuracy metric
    IF (iOutpFM_accur .eq. 1) THEN
       write(ext,'(i2.2)') ifile
       open(iu_check,file=trim(prefCheck)//ext,status='unknown',form='formatted',ACCESS='SEQUENTIAL')
       write(iu_check,'(3i10)') nScanL,nFovs,np
       write(iu_check,'(11f12.5)') FMerr(1:nScanL,1:nFovs,1:np)
       close(iu_check)
    ENDIF     
    DEALLOCATE(angleFM,tb,qc,lat_1,lon_1,angle_1,tb_1,qc_1,lat_2,&
         lon_2,angle_2,tb_2,qc_2,Cfreq,pol,FMerr,&
         RelAziAngle_1,SolZenAngle_1,RelAziAngle_2,SolZenAngle_2)
    !---Close SDR file
    CLOSE(iu_sdr_1)
    CLOSE(iu_sdr_2)
    !---Close FM-SDR file
    IF( WRITE_HEADER_DONE .EQ. 1 ) CLOSE(iu_FMsdr)
    DEALLOCATE(ScanLineInROI)
  ENDDO FilesLoop

  If(ASSOCIATED(CFreq_1)) DEALLOCATE(Cfreq_1)
  If(ASSOCIATED(pol_1))   DEALLOCATE(pol_1)
  If(ASSOCIATED(CFreq_2)) DEALLOCATE(Cfreq_2)
  If(ASSOCIATED(pol_2))   DEALLOCATE(pol_2)
  DEALLOCATE(sdrFilesAMSUA,sdrFilesMHS,FMsdrFiles)
  
  !---- if no FMSDR generated, return an error code ---- 
  if( TotalScanLinesROI .lt. 1 ) then
    !print *, 'No FMSDR data is generated.'
    CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'(in POES FM)' )
  endif

  CALL CloseLogFile()

end program fm_metopB
