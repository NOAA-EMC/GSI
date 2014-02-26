!===============================================================================
! Name:       fm_gcomw1.f90
!
!
! Type:         Main Program
!
!
! Description:
!       Performs footprint matching high/low resolution channels to 3 possible
!       resolutions ( high, low, coarse )
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
!       11-12-2012      Wanchun Chen    Original Coder
!
!===============================================================================

program fm_gcomw1
  
  USE Consts
  USE misc  
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  
  IMPLICIT NONE
  
  !---- INTRINSIC functions used in this module
  INTRINSIC :: ABS,ASSOCIATED,MAXVAL,MINVAL,NINT,RESHAPE,SUM,TRIM,ANY,COUNT

  INTEGER,            PARAMETER  :: ExpdLRfov=243,ExpdHRfov=486,ExpdCRfov=27 ! 243/9=27
  INTEGER,            PARAMETER  :: iu_Check=40,np=11
  REAL,               PARAMETER  :: epsilonFm = 0.01
  INTEGER,            PARAMETER  :: FMautoORmanual = 1 !0:Automatic, 1:Manual
  INTEGER,            PARAMETER  :: maxMSallowed=40 !ms
  REAL,               PARAMETER  :: mxdlatAllowed=0.5,mxdangAllowed=0.9

  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: sdrFilesLr,sdrFilesHr
  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: FMsdrFiles
  REAL,               DIMENSION(:),    POINTER     :: Cfreq_1,Cfreq_2
  INTEGER,            DIMENSION(:),    POINTER     :: pol_1,pol_2,pol
  REAL,               DIMENSION(:),    ALLOCATABLE :: angleFM,Cfreq
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat_1,lon_1,angle_1,RelAziAngle_1,SolZenAngle_1
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat_2,lon_2,angle_2,RelAziAngle_2,SolZenAngle_2
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: tb_1, tb_2, tb
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: qc_1, qc_2, qc
  REAL,               DIMENSION(:,:,:),ALLOCATABLE :: FMerr
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: ScanLineInROI
  
  REAL,DIMENSION(ExpdCRfov) :: lat_0,lon_0,RelAziAngle_0,SolZenAngle_0
  INTEGER    :: node_0
  
  CHARACTER(LEN=2)    :: ext
  CHARACTER(LEN=250)  :: fileFMSDR
  INTEGER    :: iu_sdr_1,iu_sdr_2,iu_FMsdr,nScanL_hr_2skip0,nScanL_lr_2skip0
  INTEGER    :: nScanL_hr_2skip_requested,nScanL_lr_2skip_requested
  INTEGER    :: nfilesLR,nfilesHR,nFiles,iFile,nChan,nqc,nFovs,nScanL,nBatch
  INTEGER    :: nScanL_1,nFovs_1,nqc_1,nchan_1,nScanL_2,nFovs_2,nqc_2,nchan_2,nScanL_1_0
  INTEGER    :: iscanline,i,ifov,ichan,node_1,node_2,nScanLines,jfov
  INTEGER    :: scanDAY_1,scanYear_1,scanDAY_2,scanYear_2,Day_hr,Yr_hr,dDay,dYr
  INTEGER    :: scanUTC_1,scanUTC_2,UTC_hr,dUTC,dUTC0
  INTEGER    :: scanDAY,scanYear,scanUTC,iu_listLR,iu_listHR
  REAL       :: lat_hr,lon_hr,ang_hr,dLat,dLon,dAng,xVec(9)
  REAL       :: ALPH(3,3),Tbar,DELTAT,Tmax,Tmin
  INTEGER    :: WRITE_HEADER_DONE=0
  INTEGER    :: nProcessScanLines=0
  INTEGER    :: nCountScanLinesROI=0 ! record individual file's number of scans falling in ROI
  INTEGER    :: TotalScanLinesROI=0  ! record total number of scans of all files falling in ROI
  
  !---Namelist data 
  CHARACTER(LEN=250) :: sdrfileList_lr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: sdrfileList_hr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathFMSDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: prefCheck=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: logFile=DEFAULT_VALUE_STR4
  INTEGER            :: iOutpFM_accur=DEFAULT_VALUE_INT
  INTEGER            :: iFMtype=DEFAULT_VALUE_INT
  INTEGER            :: nScanL_hr_2skip=DEFAULT_VALUE_INT
  INTEGER            :: nScanL_lr_2skip=DEFAULT_VALUE_INT
  INTEGER            :: ScanLindx_hr_TimeColloc=DEFAULT_VALUE_INT
  INTEGER            :: norbits2process=DEFAULT_VALUE_INT
  INTEGER            :: GeogrLimits=DEFAULT_VALUE_INT
  REAL               :: minLat=DEFAULT_VALUE_REAL
  REAL               :: maxLat=DEFAULT_VALUE_REAL
  REAL               :: minLon=DEFAULT_VALUE_REAL
  REAL               :: maxLon=DEFAULT_VALUE_REAL
 
  NAMELIST /ContrlFM/sdrfileList_lr,sdrfileList_hr,pathFMSDR,iOutpFM_accur,&
       prefCheck,iFMtype,nScanL_lr_2skip,nScanL_hr_2skip,ScanLindx_hr_TimeColloc,&
       norbits2process,logFile,GeogrLimits,minLat,maxLat,minLon,maxLon
       
  !---Read control-data from namelist
  READ(*,NML=ContrlFM)
 
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
 
  !---Read the file names of LR SDRs
  call ReadList(iu_listLR,trim(sdrfileList_lr),sdrFilesLr,nFilesLR,FMsdrFiles,pathFMSDR,'FMSDR')
  DEALLOCATE(FMsdrFiles)
  
  IF( nfilesLR .lt. 1 ) CALL ErrHandl(ErrorType,Err_NoFilesFound,'LOW') 
  !---Read the file names of HR SDRs
  call ReadList(iu_listHR,trim(sdrfileList_hr),sdrFilesHr,nFilesHR,FMsdrFiles,pathFMSDR,'FMSDR')
  IF( nfilesHR .lt. 1 ) CALL ErrHandl(ErrorType,Err_NoFilesFound,'HIGH')
  
  !-------------------------------------------------------------------------------
  !   Consistency checks
  !-------------------------------------------------------------------------------
  IF( nfilesLR .ne. nfilesHR ) CALL ErrHandl(ErrorType,Err_InconsNumber,'of files Low/High ')
  nFiles = minval((/nfilesLR,norbits2process/))
  
  !-------------------------------------------------------------------------------
  !   Add extension (CR, HR or LR) depending on the resolution chosen)
  !-------------------------------------------------------------------------------
  DO ifile=1,nFiles
    call replace_path_string_gcomw1(sdrFilesLr(ifile), pathFMSDR, 'SDR_LR_', 'FMSDR_', fileFMSDR)
    IF( iFMtype .eq. -1 ) THEN
      FMsdrFiles(ifile) = trim(fileFMSDR)//'.CR'
    ELSE IF( iFMtype .eq. 0 ) THEN
      FMsdrFiles(ifile) = trim(fileFMSDR)//'.LR'
    ELSE IF( iFMtype .eq. 1 ) THEN
      FMsdrFiles(ifile) = trim(fileFMSDR)//'.HR'
    ELSE
      CALL ErrHandl(ErrorType,Err_DifferFromExpect,'iFMType not supported')
    ENDIF
  ENDDO
  
  !-------------------------------------------------------------------------------
  !   Begin FM process
  !-------------------------------------------------------------------------------
  nScanL_hr_2skip_requested = nScanL_hr_2skip
  nScanL_lr_2skip_requested = nScanL_lr_2skip
  TotalScanLinesROI=0
  
  FilesLoop: DO ifile=1,nFiles
  
    WRITE_HEADER_DONE=0
    write(*,*)
    write(*,*) 'ifile=', ifile
    write(*,'(A)') 'LR file='//TRIM(sdrFilesLr(ifile))
    write(*,'(A)') 'HR file='//TRIM(sdrFilesHr(ifile))
    
    !----Open/Read LOW Resolution SDR header
    CALL ReadRadHdrScanLMode(sdrFilesLr(ifile),iu_sdr_1,nScanL_1,nFovs_1,nqc_1,nchan_1,CFreq_1,pol_1)
    !---Open/Read HIGH Resolution SDR header
    CALL ReadRadHdrScanLMode(sdrFilesHr(ifile),iu_sdr_2,nScanL_2,nFovs_2,nqc_2,nchan_2,CFreq_2,pol_2)
    
    !----Consistency checks of nfov
    IF( nFovs_1  .ne. ExpdLRfov ) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'LOW nFOV')
    IF( nFovs_2  .ne. ExpdHRfov ) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'HIGH nFOV')
    IF( nScanL_1 .ne. nScanL_2  ) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'Low Scans != High Scans')
    
    nScanL = nScanL_1
    nChan  = nChan_1+nChan_2
    
    nqc = nqc_1
   
    nBatch = 1
    
    !----FM at CR resolution
    IF( iFMtype .eq. -1 ) THEN  
      nFovs = ExpdCRfov
    !----FM at LR resolution
    ELSE IF( iFMtype .eq. 0 ) THEN
      nFovs = nFovs_1
    !----FM at HR resolution
    ELSE IF( iFMtype .eq. 1 ) THEN
      nFovs = nFovs_2
    ELSE
      CALL ErrHandl(ErrorType,Err_DifferFromExpect,'iFMType not supported') 
    ENDIF
    
    !----Allocate memory for vectors/arrays
    ALLOCATE(angleFM(nchan),tb(nFovs,nchan),qc(nqc),&
         lat_1(nFovs_1),lon_1(nFovs_1),angle_1(nFovs_1),tb_1(nFovs_1,nchan_1),qc_1(nqc_1),&
         lat_2(nFovs_2),lon_2(nFovs_2),angle_2(nFovs_2),tb_2(nFovs_2,nchan_2),qc_2(nqc_2),&
	 Cfreq(nChan),pol(nChan),FMerr(nScanL,nFovs,np), &
         RelAziAngle_1(nFovs_1),SolZenAngle_1(nFovs_1),  &
         RelAziAngle_2(nFovs_2),SolZenAngle_2(nFovs_2))
    
    Cfreq(1:nChan) = (/Cfreq_1(1:nchan_1),Cfreq_2(1:nchan_2)/)
    pol(1:nChan)   = (/pol_1(1:nchan_1),pol_2(1:nchan_2)/)

    !---- scanlines to skip to time-synchronize LOW and HIGH, they are alway 0's for TRMM TMI
    nScanL_lr_2skip0 = 0
    nScanL_hr_2skip0 = 0
    
    nScanLines = nScanL
    
    !-------------------------------------------------------------------------------------
    !   Loop over scanlines to determine whether scanline falls in Region of Interest(ROI)
    !-------------------------------------------------------------------------------------
    ALLOCATE(ScanLineInROI(nScanLines))
    ScanLineInROI(:) = 0
    nProcessScanLines = 0
    ScanLLoopFlag: DO iscanLine=1,nScanLines
    
      !----Read LOW SDR scanline content
      CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
    
      !----Read HIGH SDR consecutive scanlines content
      CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2,nchan_2,nFovs_2,angle_2,&
            tb_2,lat_2,lon_2,node_2,scanUTC_2,scanDAY_2,scanYear_2,RelAziAngle_2,SolZenAngle_2)
      
      !----Get coarse resolution lat/lon, we take the average of 2 center ones ( risk of lon jump? )
      !----lat/lon we don't use 4 points average, we only use 2 center points
      do ifov=1,ExpdCRfov
        lat_0(ifov) =  lat_1(9*(ifov-1)+1)
        lon_0(ifov) =  lon_1(9*(ifov-1)+1)
      enddo
      
      IF( GeogrLimits .eq. 0 ) THEN
          ScanLineInROI(iscanLine) = 1
          nProcessScanLines = nProcessScanLines + 1
      ELSE IF( GeogrLimits .eq. 1  .and. iFMtype .eq. -1 ) THEN
          GeoLoop0: DO ifov=1,nFovs
              IF( lat_0(ifov) .ge. minLat .and. lat_0(ifov) .le. maxLat .and. &
                  lon_0(ifov) .ge. minLon .and. lon_0(ifov) .le. maxLon) THEN
                  ScanLineInROI(iscanLine) = 1
                  nProcessScanLines = nProcessScanLines + 1
                  exit GeoLoop0
              ENDIF
          ENDDO GeoLoop0
      ELSE IF( GeogrLimits .eq. 1  .and. iFMtype .eq. 0 ) THEN
          GeoLoop1: DO ifov=1,nFovs
              IF( lat_1(ifov) .ge. minLat .and. lat_1(ifov) .le. maxLat .and. &
                  lon_1(ifov) .ge. minLon .and. lon_1(ifov) .le. maxLon) THEN
                  ScanLineInROI(iscanLine) = 1
                  nProcessScanLines = nProcessScanLines + 1
                  exit GeoLoop1
              ENDIF
          ENDDO GeoLoop1
      ELSE IF( GeogrLimits .eq. 1  .and. iFMtype .eq. 1 ) THEN
          GeoLoop2: DO ifov=1,nFovs
              IF( lat_2(ifov) .ge. minLat .and. lat_2(ifov) .le. maxLat .and. &
                  lon_2(ifov) .ge. minLon .and. lon_2(ifov) .le. maxLon) THEN
                  ScanLineInROI(iscanLine) = 1 
                  nProcessScanLines = nProcessScanLines + 1
                  exit GeoLoop2
              ENDIF
          ENDDO GeoLoop2
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
    
       
    !----Rewind files and re-open their headers
    CALL ReadRadHdrScanLMode(sdrFilesLr(ifile),iu_sdr_1,nScanL_1,nFovs_1,nqc_1,nchan_1,CFreq_1,pol_1)
    CALL ReadRadHdrScanLMode(sdrFilesHr(ifile),iu_sdr_2,nScanL_2,nFovs_2,nqc_2,nchan_2,CFreq_2,pol_2)

    !----Open/Write FM-SDR header
    FmErr = -99
    
    !------------------------------------------------------------------------------
    !   Loop over scanlines
    !------------------------------------------------------------------------------
    ScanLLoop: DO iscanLine=1,nScanLines
    
      !---Read LOW resolution scanline content
      CALL ReadRadMeasScanLMode(iu_sdr_1,nqc_1,qc_1,nchan_1,nFovs_1,angle_1,&
            tb_1,lat_1,lon_1,node_1,scanUTC_1,scanDAY_1,scanYear_1,RelAziAngle_1,SolZenAngle_1)
       
      !---Read HIGH resolution SDR consecutive scanlines content
      CALL ReadRadMeasScanLMode(iu_sdr_2,nqc_2,qc_2,nchan_2,nFovs_2,angle_2,&
            tb_2,lat_2,lon_2,node_2,scanUTC_2,scanDAY_2,scanYear_2,RelAziAngle_2,SolZenAngle_2)
       
      !----Get coarse resolution lat/lon parameters
      do ifov=1,ExpdCRfov
        lat_0(ifov) =  lat_1(9*(ifov-1)+1)
        lon_0(ifov) =  lon_1(9*(ifov-1)+1)
	RelAziAngle_0(ifov) = RelAziAngle_1(9*(ifov-1)+1)
	SolZenAngle_0(ifov) = SolZenAngle_1(9*(ifov-1)+1)
      enddo


      IF( scanDAY_2 .ne. scanDAY_1 ) scanUTC_2 = scanUTC_2-86400000
      IF( scanUTC_2 .lt. 0	   ) scanUTC_2 = scanUTC_2+86400000

      !----Flag Output QC if any scanline affected by a non-0 QC
      qc(1:nqc)       = 0

      !---- only set 1 if both Low and high have channels bad
      !IF( ANY(qc(2:nqc_1+1) .ne. 0) .and. ANY(qc(nqc_1+2:nqc) .ne. 0) ) qc(1) = 1

      !---- Perform time and position quality checks then FM if scanline not already flagged
      tb = -999.0

      qc(1) = 0
      IF( qc(1) .eq. 0 ) THEN 

	 IF( iFMtype .eq. -1 ) THEN           !----FM @ coarse resolution
	   FOVsLoop0: DO ifov=1,nFovs
             !---get low resolution channel part into coarse resolution
             tb(ifov,1:nchan_1) = tb_1(9*(ifov-1)+1,1:nchan_1)
             !---get high resolution channel part into coarse resolution
             DO ichan=1,nchan_2
                tb(ifov,nchan_1+ichan) = tb_2(18*(ifov-1)+1,ichan)
             ENDDO
	   ENDDO FOVsLoop0

	 ELSE
           FOVsLoop: DO ifov=1,nFovs_1

	     IF( iFMtype .eq. 0 ) THEN        !----FM @ Low resolution
        	!---Perform footprint-matching ( high 1,3,5,... --> low 1,2,3,... )
        	tb(iFov,1:nchan_1) = tb_1(iFov,1:nchan_1)
        	DO ichan=1,nchan_2
            	   tb(ifov,nchan_1+ichan) = tb_2(iFov*2-1,ichan)
        	ENDDO

	     ELSE IF( iFMtype .eq. 1 ) THEN   !----FM @ high resolution

		!---- get high resolution part
		DO ichan=1,nchan_2
	          tb(ifov*2-1,nchan_1+ichan) = tb_2(ifov*2-1,ichan)
	          tb(ifov*2,  nchan_1+ichan) = tb_2(ifov*2,  ichan)
		ENDDO

        	!---- get low resolution part
		do ichan = 1, nchan_1
	          tb(ifov*2-1,ichan) = tb_1(ifov,ichan)
	          tb(ifov*2,  ichan) = tb_1(ifov,ichan)
		enddo

             ENDIF

           ENDDO FOVsLoop
         ENDIF
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
        IF( iFMtype .eq. -1 ) THEN
          DO ifov=1,nFovs
            angleFM(1:nchan) = angle_1(9*(ifov-1)+1)
	    node_0 = node_1
            CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan),lat_0(ifov),&
                 lon_0(ifov),node_0,scanUTC/1000.,scanDAY,scanYear,ifov,iscanLine,&
                 RelAziAngle_0(ifov),SolZenAngle_0(ifov))
          ENDDO
        ELSE IF( iFMtype .eq. 0 ) THEN
          DO ifov=1,nFovs
            angleFM(1:nchan) = angle_1(ifov)
            CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan),lat_1(ifov),&
                 lon_1(ifov),node_1,scanUTC_1/1000.,scanDAY,scanYear,ifov,iscanLine,&
                 RelAziAngle_1(ifov),SolZenAngle_1(ifov))
          ENDDO
	ELSE IF( iFMtype .eq. 1 ) THEN
          DO ifov=1,nFovs
            angleFM(1:nchan) = angle_2(ifov)
            CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan),lat_2(ifov),&
                 lon_2(ifov),node_2,scanUTC_2/1000.,scanDAY_2,scanYear_2,ifov,iscanLine,&
                 RelAziAngle_2(ifov),SolZenAngle_2(ifov))
          ENDDO
        ENDIF

      ENDIF  !--- ENDIF (ScanLineInROI)

    ENDDO ScanLLoop

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
  DEALLOCATE(sdrFilesLr,sdrFilesHr,FMsdrFiles)
  
  !---- if no FMSDR generated, return an error code ---- 
  if( TotalScanLinesROI .lt. 1 ) then
    !print *, 'No FMSDR data is generated.'
    CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'(in GCOMW1 FM)' )
  endif

  CALL CloseLogFile()

  contains

  SUBROUTINE replace_path_string_gcomw1(strin, str_append, str_old, str_new, strout)

    !---- input variables ---- 
    character(len=*) :: strin
    character(len=*) :: str_append
    character(len=*) :: str_old
    character(len=*) :: str_new

    !---- output variable ----
    character(len=*) :: strout

    !---- local variables ----
    integer :: indx = 0, indx_cut = 0
    integer :: len_effective = 0
    character(len=128) :: str_tmp
    integer :: len_old

    indx = INDEX (strin, '/', BACK = .TRUE.)
    len_effective = LEN_TRIM(strin)
    len_old = LEN_TRIM(str_old)

    !---- if no slash inside input strin ---
    if( indx .lt. 1 ) then
      indx_cut = INDEX (strin, TRIM(str_old))
      indx_cut = indx_cut+len_old
      strout = TRIM(str_append)//TRIM(str_new)//TRIM(strin(indx_cut:len_effective))
    else
      str_tmp = strin(indx+1:len_effective)
      indx_cut = INDEX (str_tmp,TRIM(str_old))
      indx_cut = indx_cut+len_old 
      len_effective = LEN_TRIM(str_tmp)
      strout = TRIM(str_append)//TRIM(str_new)//str_tmp(indx_cut:len_effective)
    endif
    return
  END SUBROUTINE replace_path_string_gcomw1

end program fm_gcomw1
