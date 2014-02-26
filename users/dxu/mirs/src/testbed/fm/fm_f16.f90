!===============================================================
! Name:      fm_f16
!
!
! Type:         Main Program
!
!
! Description:
!       Program that performs footprint matching of the MW
!       instrument data of SSMIS onboard F-16.
!       The data come at different footprint sizes:
!       IMG, ENV, LAS and UAS.
!       The present code has the option of doing the FM
!       at the highest or lowest resolutions (not implemented yet).
!
! Modules needed:
!       - IO_MeasurData
!       - misc
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

program fm_f16
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ALL,ANY,INT,SUM,TRIM,ABS,COUNT
  
  INTEGER,            PARAMETER  :: ExpdSSMISimgfov=180,ExpdSSMISenvfov=90
  INTEGER,            PARAMETER  :: ExpdSSMISlasfov=60,ExpdSSMISuasfov=30

  CHARACTER(LEN=250), DIMENSION(:),     POINTER     :: sdrFilesSSMISimg,sdrFilesSSMISenv
  CHARACTER(LEN=250), DIMENSION(:),     POINTER     :: sdrFilesSSMISlas,sdrFilesSSMISuas
  CHARACTER(LEN=250), DIMENSION(:),     POINTER     :: FMsdrFiles
  REAL,               DIMENSION(:),     POINTER     :: Cfreq_img,Cfreq_env
  REAL,               DIMENSION(:),     POINTER     :: Cfreq_las,Cfreq_uas,Cfreq
  INTEGER,            DIMENSION(:),     POINTER     :: pol,pol_img,pol_env,pol_las,pol_uas
  INTEGER,            DIMENSION(:),     ALLOCATABLE :: ChanReorder

  REAL,               DIMENSION(:,:),   ALLOCATABLE :: lat_img,lon_img,angle_img,RelAziAngle_img,SolZenAngle_img
  REAL,               DIMENSION(:,:),   ALLOCATABLE :: lat_env,lon_env,angle_env,RelAziAngle_env,SolZenAngle_env
  REAL,               DIMENSION(:,:),   ALLOCATABLE :: lat_las,lon_las,angle_las,RelAziAngle_las,SolZenAngle_las
  REAL,               DIMENSION(:,:),   ALLOCATABLE :: lat_uas,lon_uas,angle_uas,RelAziAngle_uas,SolZenAngle_uas
  REAL,               DIMENSION(:,:,:), ALLOCATABLE :: tb_img,tb_env,tb_las,tb_uas
  INTEGER,            DIMENSION(:,:),   ALLOCATABLE :: qc_img,qc_env,qc_las,qc_uas

  REAL,               DIMENSION(:),     ALLOCATABLE :: lat,lon,angle,RelAziAngle,SolZenAngle,angleFM
  REAL,               DIMENSION(:,:),   ALLOCATABLE :: tb
  INTEGER,            DIMENSION(:),     ALLOCATABLE :: qc
  INTEGER,            DIMENSION(:),     ALLOCATABLE :: ScanLineInROI

  INTEGER    :: iu_sdr_img,iu_sdr_env,iu_sdr_las,iu_sdr_uas,iu_FMsdr
  INTEGER    :: nfilesIMG,nfilesENV,nfilesLAS,nfilesUAS,nFiles,iFile
  INTEGER    :: nScanL_img,nscanL_env,nscanL_las,nscanL_uas,iscanline,nScanL
  INTEGER    :: nFovs_img,nFovs_env,nFovs_las,nFovs_uas,ifov,nFovs,nPts
  INTEGER    :: ifov_img,ifov_env,ifov_las,ifov_uas
  INTEGER    :: nqc_img,nqc_env,nqc_las,nqc_uas,nqc
  INTEGER    :: nchan_img,nchan_env,nchan_las,nchan_uas,nChan,ichan,nchanTot
  INTEGER    :: node_img,node_env,node_las,node_uas,node
  INTEGER    :: scanDAY_img,scanDAY_env,scanDAY_las,scanDAY_uas
  INTEGER    :: scanYear_img,scanYear_env,scanYear_las,scanYear_uas
  INTEGER    :: scanUTC_img,scanUTC_env,scanUTC_las,scanUTC_uas
  INTEGER    :: scanDAY,scanYear,scanUTC
  INTEGER    :: nAvg4ENV=ExpdSSMISimgfov/ExpdSSMISenvfov,nAvg4LAS=ExpdSSMISimgfov/ExpdSSMISlasfov
  INTEGER    :: nAvg4UAS=ExpdSSMISimgfov/ExpdSSMISuasfov,nAvg4IMG=ExpdSSMISimgfov/ExpdSSMISimgfov
  INTEGER    :: nAvg,iAvg
  INTEGER    :: iu_list_img,iu_list_env,iu_list_las,iu_list_uas
  INTEGER    :: nProcessScanLines=0,iProcessScanLine=0
  REAL       :: sum_tmp=0.0, lon_env1=0.0, lon_env2=0.0, lon_tmp=0.0  
  INTEGER    :: nCountScanLinesROI=0 ! record individual file's number of scans falling in ROI
  INTEGER    :: TotalScanLinesROI=0  ! record total number of scans of all files falling in ROI

  !---Namelist data 
  CHARACTER(LEN=250) :: sdrfileList_img=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: sdrfileList_env=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: sdrfileList_las=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: sdrfileList_uas=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathFMSDR=DEFAULT_VALUE_STR4
  INTEGER            :: FMresol=DEFAULT_VALUE_INT
  INTEGER            :: GeogrLimits=DEFAULT_VALUE_INT
  REAL               :: minLat=DEFAULT_VALUE_REAL
  REAL               :: maxLat=DEFAULT_VALUE_REAL
  REAL               :: minLon=DEFAULT_VALUE_REAL
  REAL               :: maxLon=DEFAULT_VALUE_REAL
  CHARACTER(LEN=250) :: logfile=DEFAULT_VALUE_STR4

  NAMELIST /ContrlFM/sdrfileList_img,sdrfileList_env,sdrfileList_las,sdrfileList_uas,pathFMSDR,FMresol,&
            GeogrLimits,minLat,maxLat,minLon,maxLon,logfile

  !---Read control-data from namelist
  READ(*,NML=ContrlFM)

  !---Prepare Log file
  CALL OpenLogFile(logfile)

  !---Read the file names of SSMI/S SDRs
  call ReadList(iu_list_img,trim(sdrfileList_img),sdrFilesSSMISimg,nFilesIMG,  &
       FMsdrFiles,pathFMSDR,'FMSDR_')
  DEALLOCATE(FMsdrFiles)
  IF (nfilesIMG .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(SSMI/S IMG)') 
  call ReadList(iu_list_env,trim(sdrfileList_env),sdrFilesSSMISenv,nFilesENV,  &
       FMsdrFiles,pathFMSDR,'FMSDR_')
  DEALLOCATE(FMsdrFiles)
  IF (nfilesENV .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(SSMI/S ENV)') 
  call ReadList(iu_list_las,trim(sdrfileList_las),sdrFilesSSMISlas,nFilesLAS,  &
       FMsdrFiles,pathFMSDR,'FMSDR_')
  DEALLOCATE(FMsdrFiles)
  IF (nfilesLAS .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(SSMI/S LAS)') 
  call ReadList(iu_list_uas,trim(sdrfileList_uas),sdrFilesSSMISuas,nFilesUAS,  &
       FMsdrFiles,pathFMSDR,'FMSDR_')
  IF (nfilesUAS .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(SSMI/S UAS)') 
  !-------------------------------------------------------------------------------
  !   Consistency checks
  !-------------------------------------------------------------------------------
  IF (nfilesIMG.ne.nfilesENV) CALL ErrHandl(ErrorType,Err_InconsNumber,'of SDR channels IMG/ENV')
  IF (nfilesIMG.ne.nfilesLAS) CALL ErrHandl(ErrorType,Err_InconsNumber,'of SDR channels IMG/LAS')
  IF (nfilesIMG.ne.nfilesUAS) CALL ErrHandl(ErrorType,Err_InconsNumber,'of SDR channels IMG/UAS')
  nFiles=nfilesIMG
  !-------------------------------------------------------------------------------
  !   Add extension (HR or LR) depending on the resolution chosen)
  !-------------------------------------------------------------------------------
  DO ifile=1,nFiles
      IF( FMresol .eq. 0 ) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.UAS'
      IF( FMresol .eq. 1 ) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.LAS'
      IF( FMresol .eq. 2 ) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.ENV'
      IF( FMresol .eq. 3 ) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.IMG'
  ENDDO
  !-------------------------------------------------------------------------------
  !   Begin FM process
  !-------------------------------------------------------------------------------
  TotalScanLinesROI=0
  FilesLoop: DO ifile=1,nFiles
      write(*,'(A,I2,A)') '========== File#',ifile,' ==============='
      write(*,'(A)') sdrFilesSSMISimg(ifile)
      write(*,'(A)') sdrFilesSSMISenv(ifile)
      write(*,'(A)') sdrFilesSSMISlas(ifile)
      write(*,'(A)') sdrFilesSSMISuas(ifile)
      !---Open/Read SSMIS IMG/ENV/LAS/UAS SDR headers
      CALL ReadRadHdrScanLMode(sdrFilesSSMISimg(ifile),iu_sdr_img,nScanL_img,nFovs_img, &
           nqc_img,nchan_img,CFreq_img,pol_img)
      CALL ReadRadHdrScanLMode(sdrFilesSSMISenv(ifile),iu_sdr_env,nScanL_env,nFovs_env, &
           nqc_env,nchan_env,CFreq_env,pol_env)
      CALL ReadRadHdrScanLMode(sdrFilesSSMISlas(ifile),iu_sdr_las,nScanL_las,nFovs_las, &
           nqc_las,nchan_las,CFreq_las,pol_las)
      CALL ReadRadHdrScanLMode(sdrFilesSSMISuas(ifile),iu_sdr_uas,nScanL_uas,nFovs_uas, &
           nqc_uas,nchan_uas,CFreq_uas,pol_uas)
      !---Consistency checks
      IF (nFovs_img.ne.ExpdSSMISimgfov) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'(SSMIS IMG nFOV)') 
      IF (nFovs_env.ne.ExpdSSMISenvfov) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'(SSMIS ENV nFOV)') 
      IF (nFovs_las.ne.ExpdSSMISlasfov) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'(SSMIS LAS nFOV)')
      IF (nFovs_uas.ne.ExpdSSMISuasfov) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'(SSMIS UAS nFOV)')
      IF (nScanL_img.ne.nScanL_env) CALL ErrHandl(ErrorType,Err_InconsNumber,' of scanlines IMG/ENV')   
      IF (nScanL_img.ne.nScanL_las) CALL ErrHandl(ErrorType,Err_InconsNumber,' of scanlines IMG/LAS') 
      IF (nScanL_img.ne.nScanL_uas) CALL ErrHandl(ErrorType,Err_InconsNumber,' of scanlines IMG/UAS')
      !---Allocate memory for vectors/arrays
      nChan           = nChan_img+nChan_env+nChan_las+nChan_uas
      nqc             = nqc_img+nqc_env+nqc_las+nqc_uas
      IF( FMresol .eq. 0 ) THEN !FM process is done @ the UAS resolution (lowest)
         nFovs   = nFovs_uas 
         nAvg    = nAvg4UAS
      ELSE IF( FMresol .eq. 1 ) THEN !FM process is done @ the LAS resolution
         nFovs   = nFovs_las 
         nAvg    = nAvg4LAS
      ELSE IF( FMresol .eq. 2 ) THEN !FM process is done @ the ENV resolution
         nFovs   = nFovs_env 
         nAvg    = nAvg4ENV
      ELSE IF( FMresol .eq. 3 ) THEN !FM process is done @ the IMG resolution (highest)
         nFovs   = nFovs_img 
         nAvg    = nAvg4IMG
      ENDIF
      nScanL  = nScanL_img/nAvg
      ALLOCATE(Cfreq(nChan),pol(nChan),lat(nFovs),lon(nFovs),angle(nFovs),angleFM(nchan),         &
          tb(nFovs,nchan),qc(nqc),RelAziAngle(nFovs),SolZenAngle(nFovs),                          &
          lat_img(nFovs_img,nAvg),lon_img(nFovs_img,nAvg),angle_img(nFovs_img,nAvg),              &
          tb_img(nFovs_img,nchan_img,nAvg),qc_img(nqc_img,nAvg),                                  &
          lat_env(nFovs_env,nAvg),lon_env(nFovs_env,nAvg),angle_env(nFovs_env,nAvg),              &
          tb_env(nFovs_env,nchan_env,nAvg),qc_env(nqc_env,nAvg),                                  &
          lat_las(nFovs_las,nAvg),lon_las(nFovs_las,nAvg),angle_las(nFovs_las,nAvg),              &
          tb_las(nFovs_las,nchan_las,nAvg),qc_las(nqc_las,nAvg),                                  &
          lat_uas(nFovs_uas,nAvg),lon_uas(nFovs_uas,nAvg),angle_uas(nFovs_uas,nAvg),              &
          tb_uas(nFovs_uas,nchan_uas,nAvg),qc_uas(nqc_uas,nAvg),ChanReorder(nChan),               &
          RelAziAngle_img(nFovs_img,nAvg),SolZenAngle_img(nFovs_img,nAvg),                        &
          RelAziAngle_env(nFovs_env,nAvg),SolZenAngle_env(nFovs_env,nAvg),                        &
          RelAziAngle_las(nFovs_las,nAvg),SolZenAngle_las(nFovs_las,nAvg),                        &
          RelAziAngle_uas(nFovs_uas,nAvg),SolZenAngle_uas(nFovs_uas,nAvg))
      !---The reordering of the channels to fit the original SSMIS order
      Cfreq(1:nChan)  = (/Cfreq_img(1:nchan_img),Cfreq_env(1:nchan_env),&
           Cfreq_las(1:nchan_las),Cfreq_uas(1:nchan_uas)/)
      pol(1:nChan)    = (/pol_img(1:nchan_img),pol_env(1:nchan_env),&
           pol_las(1:nchan_las),pol_uas(1:nchan_uas)/)
      ChanReorder(1:nchan) = (/12,13,14,15,16,17,18,1,2,3,4,7,8,9,10,11,5,6,20,21,22,23,24,19/)
      

      !---Loop over scanlines to flag whether the scanline need to process or not
      ALLOCATE(ScanLineInROI(nScanL))
      ScanLineInROI(:) = 0
      nProcessScanLines=0
      ScanLLoopFlag: DO iscanLine=1,nScanL
        !---Read SSMIS SDR scanline content
        Do iavg=1,nAvg
           CALL ReadRadMeasScanLMode(iu_sdr_img,nqc_img,qc_img(:,iAvg),nchan_img,nFovs_img, &
                angle_img(:,iAvg),tb_img(:,:,iAvg),lat_img(:,iAvg),lon_img(:,iAvg),node_img,&
                scanUTC_img,scanDAY_img,scanYear_img,RelAziAngle_img(:,iAvg),SolZenAngle_img(:,iAvg))
           CALL ReadRadMeasScanLMode(iu_sdr_env,nqc_env,qc_env(:,iAvg),nchan_env,nFovs_env, &
                angle_env(:,iAvg),tb_env(:,:,iAvg),lat_env(:,iAvg),lon_env(:,iAvg),node_env,&
                scanUTC_env,scanDAY_env,scanYear_env,RelAziAngle_env(:,iAvg),SolZenAngle_env(:,iAvg))
           CALL ReadRadMeasScanLMode(iu_sdr_las,nqc_las,qc_las(:,iAvg),nchan_las,nFovs_las, &
                angle_las(:,iAvg),tb_las(:,:,iAvg),lat_las(:,iAvg),lon_las(:,iAvg),node_las,&
                scanUTC_las,scanDAY_las,scanYear_las,RelAziAngle_las(:,iAvg),SolZenAngle_las(:,iAvg))
           CALL ReadRadMeasScanLMode(iu_sdr_uas,nqc_uas,qc_uas(:,iAvg),nchan_uas,nFovs_uas, &
                angle_uas(:,iAvg),tb_uas(:,:,iAvg),lat_uas(:,iAvg),lon_uas(:,iAvg),node_uas,&
                scanUTC_uas,scanDAY_uas,scanYear_uas,RelAziAngle_uas(:,iAvg),SolZenAngle_uas(:,iAvg))
        Enddo

        DO ifov=1,nFovs
           !---When footprint matching is done at IMG resolution
           IF( FMresol .eq. 3 ) THEN
              lat(ifov)                  = lat_img(ifov,1)
              lon(ifov)                  = lon_img(ifov,1)
           !---When footprint matching is done at ENV resolution
           ELSE IF( FMresol .eq. 2 ) THEN
              CALL ErrHandl(ErrorType,Err_NotImplemented,'(SSMIS/S FM @ ENV resolution)') 
           !---When footprint matching is done at LAS resolution
           ELSE IF( FMresol .eq. 1 ) THEN
              lat(ifov)                  = SUM(lat_las(ifov,1:nAvg))/(nAvg*1.)
              lon(ifov)                  = SUM(lon_las(ifov,1:nAvg))/(nAvg*1.)
           !---When footprint matching is done at UAS resolution
           ELSE IF( FMresol .eq. 0 ) THEN
              ifov_env                   = int((ifov-1)*3.)+1
              !---Use the ENV (37GHz) geo-location
              lat(ifov)                  = SUM(lat_env(ifov_env+1,3:4))/2.
              !---To fix longitude jump problem
              if( ABS( lon_env(ifov_env+1,3)-lon_env(ifov_env+1,4) ) .le. 180 ) then
                lon(ifov)                = SUM(lon_env(ifov_env+1,3:4))/2.
              else
                if( lon_env(ifov_env+1,3) .lt. 0 ) then
                  lon_env1 = lon_env(ifov_env+1,3) + 360.
                else
                  lon_env1 = lon_env(ifov_env+1,3)
                endif

                if( lon_env(ifov_env+1,4) .lt. 0 ) then
                  lon_env2 = lon_env(ifov_env+1,4) + 360.
                else
                  lon_env2 = lon_env(ifov_env+1,4)
                endif

                lon_tmp = ( lon_env1 + lon_env2 ) * 0.5
                if( lon_tmp .gt. 180. ) then
                  lon(ifov) = lon_tmp - 360.
                else
                  lon(ifov) = lon_tmp
                endif   
              endif
           ENDIF
        ENDDO

        IF (GeogrLimits .eq. 0) THEN
            ScanLineInROI(iscanLine) = 1
            nProcessScanLines = nProcessScanLines + 1
        ELSE IF (GeogrLimits .eq. 1) THEN
            GeoLoop: DO ifov=1,nFovs
              IF( lat(ifov).ge.minLat .and. lat(ifov).le.maxLat .and. &
                  lon(ifov).ge.minLon .and. lon(ifov).le.maxLon) THEN
                      ScanLineInROI(iscanLine) = 1
                      nProcessScanLines = nProcessScanLines + 1
                      EXIT GeoLoop
              ENDIF
            ENDDO GeoLoop
        ELSE
            CALL ErrHandl(ErrorType,Err_OptionNotSupported,'(in SSMI/S FM)') 
        ENDIF

      ENDDO ScanLLoopFlag
      !---Close SDR file
      CLOSE(iu_sdr_img)
      CLOSE(iu_sdr_env)
      CLOSE(iu_sdr_las)
      CLOSE(iu_sdr_uas)

      !---- if nCountScanLinesROI < 1, no scanline falls in ROI, exit program ----
      nCountScanLinesROI = COUNT( ScanLineInROI .eq. 1 )
      if( nCountScanLinesROI .lt. 1 ) then
        !print *, 'No scanline found in the ROI!'
        !print *, 'No FMSDR data is generated'
        CALL ErrHandl(WarningType,Warn_NotCoverROI, 'orbit: '//trim(FMsdrFiles(ifile)) )

        DEALLOCATE(lat,lon,angle,tb,qc,lat_img,lat_env,lat_las,lat_uas,lon_img,lon_env,lon_las,lon_uas,&
           angle_img,angle_env,angle_las,angle_uas,tb_img,tb_env,tb_las,tb_uas,qc_img,qc_env,qc_las,qc_uas,&
           Cfreq,pol,ChanReorder,angleFM,RelAziAngle,SolZenAngle,RelAziAngle_img,SolZenAngle_img,&
           RelAziAngle_env,SolZenAngle_env,RelAziAngle_las,SolZenAngle_las,RelAziAngle_uas,SolZenAngle_uas)

        DEALLOCATE(Cfreq_img,pol_img)
        DEALLOCATE(Cfreq_env,pol_env)
        DEALLOCATE(Cfreq_las,pol_las)
        DEALLOCATE(Cfreq_uas,pol_uas)
        DEALLOCATE(ScanLineInROI)

        CYCLE FilesLoop
        !CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'No FMSDR is generated!' )
      endif 
      TotalScanLinesROI = TotalScanLinesROI + nCountScanLinesROI

     
      !---Open/Read SSMIS IMG/ENV/LAS/UAS SDR headers
      CALL ReadRadHdrScanLMode(sdrFilesSSMISimg(ifile),iu_sdr_img,nScanL_img,nFovs_img, &
           nqc_img,nchan_img,CFreq_img,pol_img)
      CALL ReadRadHdrScanLMode(sdrFilesSSMISenv(ifile),iu_sdr_env,nScanL_env,nFovs_env, &
           nqc_env,nchan_env,CFreq_env,pol_env)
      CALL ReadRadHdrScanLMode(sdrFilesSSMISlas(ifile),iu_sdr_las,nScanL_las,nFovs_las, &
           nqc_las,nchan_las,CFreq_las,pol_las)
      CALL ReadRadHdrScanLMode(sdrFilesSSMISuas(ifile),iu_sdr_uas,nScanL_uas,nFovs_uas, &
           nqc_uas,nchan_uas,CFreq_uas,pol_uas)

      !---Loop over scanlines
      iProcessScanLine=0
      nPts = 0
      ScanLLoop: DO iscanLine=1,nScanL
        !---Read SSMIS SDR scanline content
        Do iavg=1,nAvg
           CALL ReadRadMeasScanLMode(iu_sdr_img,nqc_img,qc_img(:,iAvg),nchan_img,nFovs_img, &
                angle_img(:,iAvg),tb_img(:,:,iAvg),lat_img(:,iAvg),lon_img(:,iAvg),node_img,&
                scanUTC_img,scanDAY_img,scanYear_img,RelAziAngle_img(:,iAvg),SolZenAngle_img(:,iAvg))
           CALL ReadRadMeasScanLMode(iu_sdr_env,nqc_env,qc_env(:,iAvg),nchan_env,nFovs_env, &
                angle_env(:,iAvg),tb_env(:,:,iAvg),lat_env(:,iAvg),lon_env(:,iAvg),node_env,&
                scanUTC_env,scanDAY_env,scanYear_env,RelAziAngle_env(:,iAvg),SolZenAngle_env(:,iAvg))
           CALL ReadRadMeasScanLMode(iu_sdr_las,nqc_las,qc_las(:,iAvg),nchan_las,nFovs_las, &
                angle_las(:,iAvg),tb_las(:,:,iAvg),lat_las(:,iAvg),lon_las(:,iAvg),node_las,&
                scanUTC_las,scanDAY_las,scanYear_las,RelAziAngle_las(:,iAvg),SolZenAngle_las(:,iAvg))
           CALL ReadRadMeasScanLMode(iu_sdr_uas,nqc_uas,qc_uas(:,iAvg),nchan_uas,nFovs_uas, &
                angle_uas(:,iAvg),tb_uas(:,:,iAvg),lat_uas(:,iAvg),lon_uas(:,iAvg),node_uas,&
                scanUTC_uas,scanDAY_uas,scanYear_uas,RelAziAngle_uas(:,iAvg),SolZenAngle_uas(:,iAvg))
        Enddo
        IF(ScanLineInROI(iscanLine) .EQ. 0 ) CYCLE ScanLLoop
        iProcessScanLine = iProcessScanLine+1
        
        !---Flag Output QC if any scanline affected by a non-0 QC
        qc(1:nqc)                                = 0
        IF( ANY(qc_img(1:nqc_img,:) .ne. 0) ) qc(1) = 1
        IF( ANY(qc_env(1:nqc_env,:) .ne. 0) ) qc(1) = 1
        IF( ANY(qc_las(1:nqc_las,:) .ne. 0) ) qc(1) = 1
        IF( ANY(qc_uas(1:nqc_uas,:) .ne. 0) ) qc(1) = 1
        !---Perform time and position quality checks then FM if scanline not already flagged
        tb =-99
        IF (ALL(qc(1:nqc) .eq. 0)) THEN 
           DO ifov=1,nFovs
              !---When footprint matching is done at IMG resolution
              IF( FMresol .eq. 3 ) THEN
                 ifov_env = int((ifov-1)/2.)+1
                 ifov_las = int((ifov-1)/3.)+1
                 ifov_uas = int((ifov-1)/6.)+1
                 tb(iFov,1:nchan_img)       = tb_img(iFov,1:nchan_img,1)
                 nchanTot                   = nchan_img
                 DO ichan=1,nchan_env
                    tb(iFov,nchanTot+ichan) = tb_env(ifov_env,ichan,1)
                 ENDDO
                 nchanTot                   = nchan_img + nchan_env
                 DO ichan=1,nchan_las
                    tb(iFov,nchanTot+ichan) = tb_las(ifov_las,ichan,1)
                 ENDDO
                 nchanTot                   = nchan_img + nchan_env + nchan_las
                 DO ichan=1,nchan_uas
                    tb(iFov,nchanTot+ichan) = tb_uas(ifov_uas,ichan,1)
                 ENDDO
                 lat(ifov)                  = lat_img(iFov,1)
                 lon(ifov)                  = lon_img(iFov,1)
                 Angle(ifov)                = Angle_img(iFov,1)
                 RelAziAngle(ifov)          = RelAziAngle_img(iFov,1)
                 SolZenAngle(ifov)          = SolZenAngle_img(iFov,1)
                 node                       = node_img
              !---When footprint matching is done at ENV resolution
              ELSE IF( FMresol .eq. 2 ) THEN
                 CALL ErrHandl(ErrorType,Err_NotImplemented,'(SSMIS/S FM @ ENV resolution)') 
              !---When footprint matching is done at LAS resolution
              ELSE IF( FMresol .eq. 1 ) THEN
                 ifov_img                   = int((ifov-1)*3.)+1
                 ifov_env                   = int((ifov-1)*1.5)+1
                 ifov_uas                   = int((ifov-1)/2.)+1
                 DO ichan=1,nchan_img
                    tb(iFov,ichan)          = SUM(tb_img(ifov_img:ifov_img+2,ichan,1:nAvg))/(nAvg*3.)
                 ENDDO
                 nchanTot                   = nchan_img
                 DO ichan=1,nchan_env
                    tb(iFov,nchanTot+ichan) = SUM(tb_env(ifov_env:ifov_env+1,ichan,1:nAvg))/(nAvg*2.)
                 ENDDO
                 nchanTot                   = nchan_img + nchan_env
                 DO ichan=1,nchan_las
                    tb(iFov,nchanTot+ichan) = SUM(tb_las(ifov,ichan,1:nAvg))/3.
                 ENDDO
                 nchanTot                   = nchan_img + nchan_env + nchan_las
                 DO ichan=1,nchan_uas
                    tb(iFov,nchanTot+ichan) = SUM(tb_uas(ifov_uas,ichan,1:nAvg))/3.
                 ENDDO
                 lat(ifov)                  = SUM(lat_las(iFov,1:nAvg))/(nAvg*1.)
                 lon(ifov)                  = SUM(lon_las(iFov,1:nAvg))/(nAvg*1.)
                 Angle(ifov)                = SUM(Angle_las(iFov,1:nAvg))/(nAvg*1.)
                 RelAziAngle(ifov)          = SUM(RelAziAngle_las(iFov,1:nAvg))/(nAvg*1.)
                 SolZenAngle(ifov)          = SUM(SolZenAngle_las(iFov,1:nAvg))/(nAvg*1.)
                 node                       = node_las
              !---When footprint matching is done at UAS resolution
              ELSE IF( FMresol .eq. 0 ) THEN
                 ifov_img                   = int((ifov-1)*6.)+1
                 ifov_env                   = int((ifov-1)*3.)+1
                 ifov_las                   = int((ifov-1)*2.)+1
                 DO ichan=1,nchan_img
                    tb(iFov,ichan)          = SUM(tb_img(ifov_img:ifov_img+5,ichan,1:nAvg))/(nAvg*6.)
                 ENDDO
                 nchanTot                   = nchan_img
                 DO ichan=1,nchan_env
                    tb(iFov,nchanTot+ichan) = SUM(tb_env(ifov_env:ifov_env+2,ichan,1:nAvg))/(nAvg*3.)
                 ENDDO
                 nchanTot                   = nchan_img + nchan_env
                 DO ichan=1,nchan_las
                    tb(iFov,nchanTot+ichan) = SUM(tb_las(ifov_las:ifov_las+1,ichan,1:nAvg))/(nAvg*2.)
                 ENDDO
                 nchanTot                   = nchan_img + nchan_env + nchan_las
                 DO ichan=1,nchan_uas
                    tb(iFov,nchanTot+ichan) = SUM(tb_uas(ifov,ichan,1:nAvg))/(nAvg*1.)
                 ENDDO

                 !---Use the ENV (37GHz) geo-location
                 lat(ifov)                  = SUM(lat_env(ifov_env+1,3:4))/2.
                  !---To fix longitude jump problem
                 if( ABS( lon_env(ifov_env+1,3)-lon_env(ifov_env+1,4) ) .le. 180 ) then
                   lon(ifov)                = SUM(lon_env(ifov_env+1,3:4))/2.
                 else
                   if( lon_env(ifov_env+1,3) .lt. 0 ) then
                     lon_env1 = lon_env(ifov_env+1,3) + 360.
                   else
                     lon_env1 = lon_env(ifov_env+1,3)
                   endif

                   if( lon_env(ifov_env+1,4) .lt. 0 ) then
                     lon_env2 = lon_env(ifov_env+1,4) + 360.
                   else
                     lon_env2 = lon_env(ifov_env+1,4)
                   endif

                   lon_tmp = ( lon_env1 + lon_env2 ) * 0.5
                   if( lon_tmp .gt. 180. ) then
                     lon(ifov) = lon_tmp - 360.
                   else
                     lon(ifov) = lon_tmp
                   endif   
                 endif

                 Angle(ifov)                = SUM(Angle_uas(iFov,1:nAvg))/(nAvg*1.)
                 RelAziAngle(ifov)          = SUM(RelAziAngle_uas(iFov,1:nAvg))/(nAvg*1.)
                 SolZenAngle(ifov)          = SUM(SolZenAngle_uas(iFov,1:nAvg))/(nAvg*1.)
                 node                       = node_uas
              ENDIF
           ENDDO
        ENDIF
        
        !---Write out FM-SDR
        scanDAY  = scanDAY_img
        scanYear = scanYear_img
        scanUTC  = scanUTC_img

        !---If scanline has any point that fit within the geographic limits, output it
        IF (ScanLineInROI(iscanLine) .EQ. 1 ) THEN
           nPts = nPts + 1
           IF (nPts .eq. 1) THEN  
              !---Open/Write FM-SDR header
              CALL WriteHdrMeasurmts(FMsdrFiles(ifile),iu_FMsdr,nProcessScanLines*nFovs,nqc,&
                   nchan,nFovs,CFreq(ChanReorder(1:nchan)),pol(ChanReorder(1:nchan)),nProcessScanLines)
           ENDIF
           FOVsLoop: DO ifov=1,nFovs
              angleFM(1:nchan)=angle(ifov)
              CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,(ChanReorder(1:nchan))),&
                   lat(ifov),lon(ifov),node,scanUTC_img/1000.,scanDAY,scanYear,ifov,iProcessScanLine,&
                   RelAziAngle(ifov),SolZenAngle(ifov))
           ENDDO FOVsLoop
        ENDIF
      ENDDO ScanLLoop
      
      DEALLOCATE(lat,lon,angle,tb,qc,lat_img,lat_env,lat_las,lat_uas,lon_img,lon_env,lon_las,lon_uas,&
           angle_img,angle_env,angle_las,angle_uas,tb_img,tb_env,tb_las,tb_uas,qc_img,qc_env,qc_las,qc_uas,&
           Cfreq,pol,ChanReorder,angleFM,RelAziAngle,SolZenAngle,RelAziAngle_img,SolZenAngle_img,&
           RelAziAngle_env,SolZenAngle_env,RelAziAngle_las,SolZenAngle_las,RelAziAngle_uas,SolZenAngle_uas)
      !---Close SDR file
      CLOSE(iu_sdr_img)
      CLOSE(iu_sdr_env)
      CLOSE(iu_sdr_las)
      CLOSE(iu_sdr_uas)
      !---Close FM-SDR file
      CLOSE(iu_FMsdr)
      !---Release memory
      DEALLOCATE(Cfreq_img,pol_img)
      DEALLOCATE(Cfreq_env,pol_env)
      DEALLOCATE(Cfreq_las,pol_las)
      DEALLOCATE(Cfreq_uas,pol_uas)
      DEALLOCATE(ScanLineInROI)
  ENDDO FilesLoop

  !---Release memory
  DEALLOCATE(sdrFilesSSMISimg)
  DEALLOCATE(sdrFilesSSMISenv)
  DEALLOCATE(sdrFilesSSMISlas)
  DEALLOCATE(sdrFilesSSMISuas)
  DEALLOCATE(FMsdrFiles)
  
  !---- if no FMSDR generated, return an error code ---- 
  if( TotalScanLinesROI .lt. 1 ) then
    !print *, 'No FMSDR data is generated.'
    CALL ErrHandl(ErrorType,Err_NoFmsdrGenerated,'(in SSMIS FM)' )
  endif

  CALL CloseLogFile()

end program fm_f16
