!===============================================================
! Name:     fm_npp
!
!
! Type:     Main Program
!
!
! Description:
!       Transforms the SDRs into FM-SDRs for NPP ATMS.  Will 
!       either produce high resolution (native 96 fovs and all
!       scanlines) or low resolution (3x3 averaging)
!       
!
! Modules needed:
!       - IO_MeasurData
!       - misc
!       - ErrorHandling
!       - Consts
!       - IO_Misc
!       - utils
!
!
! History:
!       06-24-2009      Kevin Garrett @ IMSG,NOAA/NESDIS/STAR
!       04-13-2012      Modified to use AAPP as subroutine
!
!===============================================================

program fm_npp
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  USE Consts
  USE misc
  USE utils

  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: MINVAL,TRIM,SUM,ANY,ALL,INT

  !---Declarations
  INTEGER,            PARAMETER                    :: maxGranules2Use=3
  INTEGER,            PARAMETER                    :: nExpdFovs_hr=96
  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: sdrFiles,FMsdrFiles
  INTEGER                                          :: iu_sdr1,iu_sdr2,iu_sdr3,iu_fmsdr,iu_list
  INTEGER                                          :: iscanline,ifov,ifile,iavg,ichan,ifov_hr
  INTEGER                                          :: iGranuleOut,node
  INTEGER                                          :: nScanL,nScanLTot,nFovs,nqc,nchan,nPts,nGranules
  INTEGER                                          :: nFovs_hr
  INTEGER                                          :: nAvg,nfiles,allocate_status
  REAL,               DIMENSION(:),    POINTER     :: Cfreq
  INTEGER,            DIMENSION(:),    POINTER     :: pol,node_hr
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat,lon,angle,RelAziAngle,SolZenAngle
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: lat_hr,lon_hr,angle_hr,RelAziAngle_hr,SolZenAngle_hr
  REAL,               DIMENSION(:),    ALLOCATABLE :: angleFM
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: tb
  REAL,               DIMENSION(:,:,:),ALLOCATABLE :: tb_hr
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: qc
  INTEGER,            DIMENSION(:,:),  ALLOCATABLE :: qc_hr
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: scanUTC_hr,scanDAY_hr,scanYEAR_hr
  INTEGER                                          :: scanDAY,scanYear,scanUTC
  INTEGER                                          :: nProcessScanLines=0,iProcessScanLine 
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: ScanLineInROI
  INTEGER,            PARAMETER                    :: sensor_id=SENSOR_ID_NPP

  !---New Variables for AAPP
  INTEGER,            DIMENSION(22)                 :: nxaverage, nyaverage, QCdist,nChan2Average
  REAL,               DIMENSION(22)                 :: beam_width, ave_beam_width,cutoff
  REAL                                              :: sampling_distance
  REAL,               DIMENSION(:,:,:), ALLOCATABLE :: BT_Image, BT_Image_AAPP
  REAL,               DIMENSION(:,:),   ALLOCATABLE :: BT_2AAPP


  !---Namelist data 
  INTEGER            :: norbits2process=DEFAULT_VALUE_INT
  CHARACTER(LEN=250) :: sdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathFMSDR=DEFAULT_VALUE_STR4
  INTEGER            :: FMresol=DEFAULT_VALUE_INT
  CHARACTER(LEN=250) :: LogFile=DEFAULT_VALUE_STR4
  INTEGER            :: GeogrLimits=DEFAULT_VALUE_INT
  REAL               :: minLat=DEFAULT_VALUE_REAL
  REAL               :: maxLat=DEFAULT_VALUE_REAL
  REAL               :: minLon=DEFAULT_VALUE_REAL
  REAL               :: maxLon=DEFAULT_VALUE_REAL

  NAMELIST /ContrlFM/sdrfileList,pathFMSDR,FMresol,norbits2process,LogFile,GeogrLimits,minLat,maxLat,minLon,maxLon

  !---Read control-data from namelist
  READ(*,NML=ContrlFM)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of SDR data and build FMSDR files names
  call ReadList(iu_list,trim(sdrfileList),sdrFiles,nFiles,FMsdrFiles,pathFMSDR,'FMSDR_')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  nfiles=minval((/nfiles,norbits2process/))

  !---Determine resolution of output TBs
  DO ifile=1,nFiles
     IF (FMresol.eq.0) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.HR'
     IF (FMresol.eq.1) FMsdrFiles(ifile)=trim(FMsdrFiles(ifile))//'.HR'
  ENDDO

  !====================================================================  
  !---Define parameters for AAPP average (high resolution processing)
  !====================================================================  
  sampling_distance = 1.11
  nChan2Average    = 22

  !---Native channel beamwidth
  beam_width      = (/ 5.2, 5.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, &
        2.2, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1 /)
  !---Desired channel beamwidth
  ave_beam_width  = (/ 3.3, 3.3, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, &
        2.2, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1 /)
  !---Cutoff
  cutoff          = (/ 0.4, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
  !---Set nx/ny to 0 for FFT average, 3 for 3x3 simple averaging)
  nxaverage       = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  nyaverage       = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  !---Define number of points around missing value to flag as missing
  QCdist          = (/ 11, 11, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 /)

  !---------------------------------------------------------
  !  Loop over the SDR files
  !---------------------------------------------------------
  FilesLoop: DO ifile=1,nFiles
     !IF (ifile .eq. 1 .or. ifile .eq. nFiles) CYCLE

     !---Open/Read SDR header
     IF (ifile .gt. 1 .and. ifile .lt. nFiles) THEN
        print *, 'ifile',ifile
        print *, 'Input granule: ',sdrFiles(ifile)
        print *, 'Granule for first scanline...',sdrFiles(ifile-1)
        print *, 'Granule for last scanline....',sdrFiles(ifile+1)
        CALL ReadRadHdrScanLMode(sdrFiles(ifile-1),iu_sdr1,nScanL,nFovs_hr,nqc,nchan,CFreq,pol)
        CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr2,nScanL,nFovs_hr,nqc,nchan,CFreq,pol)
        CALL ReadRadHdrScanLMode(sdrFiles(ifile+1),iu_sdr3,nScanL,nFovs_hr,nqc,nchan,CFreq,pol)
     ELSE
        !---For first and last granules, or single granules
        print *, 'ifile',ifile
        print *, 'Input granule: ',sdrFiles(ifile)
        print *, 'Granule for first scanline...',sdrFiles(ifile)
        print *, 'Granule for last scanline....',sdrFiles(ifile)
        CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr1,nScanL,nFovs_hr,nqc,nchan,CFreq,pol)
     ENDIF

     print *,'Number of scan lines: ',nScanL
     
     !---Release memory
     DEALLOCATE(CFreq,pol)
     IF (nFovs_hr.ne.nExpdFovs_hr) CALL ErrHandl(ErrorType,Err_DifferFromExpect,'(ATMS nFOVs)') 
     
     IF (FMresol .eq. 0 .or. FMresol .eq. 1) THEN !FM process is done @ high resolution regardless
        nFovs     = nFovs_hr
        nGranules = 1
        IF (ifile .gt. 1 .or. ifile .lt. nFiles) nGranules = maxGranules2Use
     ENDIF
     !---Allocate memory for to-be-read vectors/arrays
     ALLOCATE( lat_hr(nFovs_hr,nGranules),lon_hr(nFovs_hr,nGranules),angle_hr(nFovs_hr,nGranules), &
          tb_hr(nFovs_hr,nchan,nGranules),qc_hr(nqc,nGranules),RelAziAngle_hr(nFovs_hr,nGranules), &
          SolZenAngle_hr(nFovs_hr,nGranules),node_hr(nGranules),scanDay_hr(nGranules),             &
          scanUTC_hr(nGranules),scanYear_hr(nGranules),angleFM(nchan),lat(nFovs),lon(nFovs),  &
          angle(nFovs),tb(nFovs,nchan),qc(nqc),RelAziAngle(nFovs),SolZenAngle(nFovs),&
          BT_Image(nFovs_hr,nScanL*nGranules,nchan),BT_2AAPP(nFovs_hr,nScanL*nGranules),&
          BT_Image_AAPP(nFovs_hr,nScanL*nGranules,nchan),STAT=allocate_status)
     IF (allocate_status .ne. 0) THEN
        CALL ErrHandl(ErrorType,Err_AllocMemPb,'(FM output arrays not allocated)')
     ENDIF
     !---Loop over high resolution scanlines to flag whether the scanline needs to be processed or not
     ALLOCATE(ScanLineInROI(nScanL),STAT=allocate_status)
     IF (allocate_status .ne. 0) THEN
        CALL ErrHandl(ErrorType,Err_AllocMemPb,'(ScanLineInROI not allocated in FM)')
     ENDIF
     ScanLineInROI = 0
     nProcessScanLines=0
     ScanLinesLoopFlag: DO iscanLine=1,nScanL
        !---Read SDR scanline content
        !---For middle granules
        IF (ifile .gt. 1 .and. ifile .lt. nFiles) THEN
           CALL ReadRadMeasScanLMode(iu_sdr1,nqc,qc_hr(:,1),nchan,nFovs_hr,      &
                angle_hr(:,1),tb_hr(:,:,1),lat_hr(:,1),lon_hr(:,1),    &
                node_hr(1),scanUTC_hr(1),scanDAY_hr(1),scanYear_hr(1), &
                RelAziAngle_hr(:,1),SolZenAngle_hr(:,1))
           CALL ReadRadMeasScanLMode(iu_sdr2,nqc,qc_hr(:,2),nchan,nFovs_hr,      &
                angle_hr(:,2),tb_hr(:,:,2),lat_hr(:,2),lon_hr(:,2),    &
                node_hr(2),scanUTC_hr(2),scanDAY_hr(2),scanYear_hr(2), &
                RelAziAngle_hr(:,2),SolZenAngle_hr(:,2))
           CALL ReadRadMeasScanLMode(iu_sdr3,nqc,qc_hr(:,3),nchan,nFovs_hr,      &
                angle_hr(:,3),tb_hr(:,:,3),lat_hr(:,3),lon_hr(:,3),    &
                node_hr(3),scanUTC_hr(3),scanDAY_hr(3),scanYear_hr(3), &
                RelAziAngle_hr(:,3),SolZenAngle_hr(:,3))
           !---Fill TB_Image with TBs to average (all 3 granules)
           DO ichan=1,nchan
              BT_Image(1:nFovs,iscanLine,ichan)=tb_hr(:,ichan,1)
              BT_Image(1:nFovs,nScanL+iscanLine,ichan)=tb_hr(:,ichan,2)
              BT_Image(1:nFovs,nScanL*2+iscanLine,ichan)=tb_hr(:,ichan,3)
           ENDDO
           !---Flag scenes in selected geographic area
           IF (GeogrLimits .eq. 0) THEN
              ScanLineInROI(iscanLine) = 1
              nProcessScanLines = nProcessScanLines + 1
           ELSE IF (GeogrLimits .eq. 1) THEN
              GeoLoop: DO ifov=1,nFovs
                 IF( lat_hr(ifov,2).ge.minLat .and. lat_hr(ifov,2).le.maxLat .and. &
                      lon_hr(ifov,2).ge.minLon .and. lon_hr(ifov,2).le.maxLon) THEN
                    ScanLineInROI(iscanLine) = 1
                    nProcessScanLines = nProcessScanLines + 1
                    EXIT GeoLoop
                 ENDIF
              ENDDO GeoLoop
           ELSE
              CALL ErrHandl(ErrorType,Err_OptionNotSupported,'(in NPP-ATMS FM)') 
           ENDIF
        ENDIF
        !---For a single granule (or first and last granules)
        IF (ifile .eq. 1 .or. ifile .eq. nFiles) THEN
           CALL ReadRadMeasScanLMode(iu_sdr1,nqc,qc_hr(:,1),nchan,nFovs_hr,      &
                angle_hr(:,1),tb_hr(:,:,1),lat_hr(:,1),lon_hr(:,1),    &
                node_hr(1),scanUTC_hr(1),scanDAY_hr(1),scanYear_hr(1), &
                RelAziAngle_hr(:,1),SolZenAngle_hr(:,1))

           !---Fill TB_Image with TBs to average (single granule)
           DO ichan=1,nchan
              BT_Image(1:nFovs,iscanLine,ichan)=tb_hr(:,ichan,1)
           ENDDO
           !---Flag scenes in selected geographic area
           IF (GeogrLimits .eq. 0) THEN
              ScanLineInROI(iscanLine) = 1
              nProcessScanLines = nProcessScanLines + 1
           ELSE IF (GeogrLimits .eq. 1) THEN
              GeoLoop2: DO ifov=1,nFovs
                 IF( lat_hr(ifov,1).ge.minLat .and. lat_hr(ifov,1).le.maxLat .and. &
                      lon_hr(ifov,1).ge.minLon .and. lon_hr(ifov,1).le.maxLon) THEN
                    ScanLineInROI(iscanLine) = 1
                    nProcessScanLines = nProcessScanLines + 1
                    EXIT GeoLoop2
                 ENDIF
              ENDDO GeoLoop2
           ELSE
              CALL ErrHandl(ErrorType,Err_OptionNotSupported,'(in NPP-ATMS FM)') 
           ENDIF
        ENDIF
     ENDDO ScanLinesLoopFlag
     CLOSE(iu_sdr1)
     IF (ifile .gt. 1 .and. ifile .lt. nFiles) THEN
        CLOSE(iu_sdr2)
        CLOSE(iu_sdr3)
     ENDIF

     IF (nProcessScanLines .eq. 0) THEN
        DEALLOCATE( lat_hr,lon_hr,angle_hr,tb_hr,qc_hr,RelAziAngle_hr,SolZenAngle_hr,node_hr,&
             scanUTC_hr,scanDay_hr,scanYear_hr,angleFM,lat,lon,angle,tb,qc,RelAziAngle,SolZenAngle,&
             BT_Image,BT_2AAPP,BT_Image_AAPP)
        DEALLOCATE(ScanLineInROI)
        CYCLE FilesLoop
     ENDIF

     !---Perform BT_Image averaging and write to BT_Image_AAPP for storage
     DO ichan=1,nchan
        BT_2AAPP=BT_Image(:,:,ichan)
        nScanLTot=nScanL
        IF (ifile .gt. 1 .and. ifile .lt. nFiles) nScanLTot = nScanL*3
        CALL Modify_Beamwidth( nFovs, nScanLTot, BT_2AAPP, sampling_distance, &
             beam_width(ichan), ave_beam_width(ichan), cutoff(ichan), nxaverage(ichan), nyaverage(ichan), &
             QCdist(ichan))
         DO iscanLine=1,nScanLTot
            BT_Image_AAPP(:,iscanLine,ichan)=BT_2AAPP(:,iscanLine)
         ENDDO
     ENDDO

     !---Re-open/Read SDR header and process file
     CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr2,nScanL,nFovs_hr,nqc,nchan,CFreq,pol)
     iProcessScanLine = 0
     npts = 0
     
     !================================================================
     !---When footprint matching is done at HIGH resolution (use AAPP)
     !================================================================
     IF (FMResol .eq. 0 .or. FMResol .eq. 1) THEN
        ScanLinesLoop_HR: DO iscanLine=1,nScanL
           iGranuleOut=1
           IF (ifile .gt. 1 .and. ifile .lt. nFiles) iGranuleOut=2
           !---Read SDR scanline content for output granule
           CALL ReadRadMeasScanLMode(iu_sdr2,nqc,qc_hr(:,iGranuleOut),nchan,nFovs_hr,      &
                angle_hr(:,iGranuleOut),tb_hr(:,:,iGranuleOut),lat_hr(:,iGranuleOut),lon_hr(:,iGranuleOut),    &
                node_hr(iGranuleOut),scanUTC_hr(iGranuleOut),scanDAY_hr(iGranuleOut),scanYear_hr(iGranuleOut), &
                RelAziAngle_hr(:,iGranuleOut),SolZenAngle_hr(:,iGranuleOut))         
           IF (ANY(tb_hr(:,:,iGranuleOut) .lt. 100)) THEN
              tb_hr(:,:,iGranuleOut)=-999.
              qc_hr(:,iGranuleOut)=1
           ENDIF

           !---If scanline has any point that fit within the geographic limits, output it
           IF (ScanLineInROI(iscanLine) .EQ. 0 ) CYCLE ScanLinesLoop_HR
           iProcessScanLine = iProcessScanLine + 1
          
           !---Flag Output QC if any scanline affected by a non-0 QC
           qc(1:nqc)                             = 0
           IF (ANY(qc_hr(1:nqc,iGranuleOut) .ne. 0)) qc(1) = 1
           !---Fill output arrays with FOV values
           DO ifov=1,nFovs  
              IF (ifile .gt. 1 .and. ifile .lt. nFiles) THEN
                 !---For middle granules
                 tb(iFov,1:nchan)        = BT_Image_AAPP(iFov,nScanL+iscanLine,1:nchan)
              ELSE
                 !---For single, first, or last granules
                 tb(iFov,1:nchan)        = BT_Image_AAPP(iFov,iscanLine,1:nchan)
              ENDIF
              lat(ifov)                  = lat_hr(iFov,iGranuleOut)
              lon(ifov)                  = lon_hr(iFov,iGranuleOut)
              Angle(ifov)                = Angle_hr(iFov,iGranuleOut)
              RelAziAngle(ifov)          = RelAziAngle_hr(iFov,iGranuleOut)
              SolZenAngle(ifov)          = SolZenAngle_hr(iFov,iGranuleOut)
              scanDAY                    = scanDAY_hr(iGranuleOut)
              scanYear                   = scanYear_hr(iGranuleOut)
              scanUTC                    = scanUTC_hr(iGranuleOut)
              node                       = node_hr(iGranuleOut)
           ENDDO
           !---Set Tb value to fill if QC fails
           IF (qc(1) .eq. 1) tb =-999.
           !---Write out FM-SDR
           !---If scanline has any point that fit within the geographic limits, output it
           nPts = nPts + 1
           IF (nPts .eq. 1) THEN  
              !---Open/Write FM-SDR header
              CALL WriteHdrMeasurmts(FMsdrFiles(ifile),iu_FMsdr,nProcessScanLines*nFovs,nqc,&
                   nchan,nFovs,CFreq,pol,nProcessScanLines)
           ENDIF
           FOVsLoop_HR: DO ifov=1,nFovs
              angleFM(1:nchan)=angle(ifov)
              CALL WriteMeasurmts(iu_FMsdr,nqc,qc,nchan,angleFM,tb(ifov,1:nchan), &
                   lat(ifov),lon(ifov),node,scanUTC/1000.,scanDAY,scanYear,ifov,  &
                   iProcessScanLine,RelAziAngle(ifov),SolZenAngle(ifov))
           ENDDO FOVsLoop_HR
        ENDDO ScanLinesLoop_HR
     ENDIF
     print *,'Total number of scan lines written: ', iProcessScanLine
     DEALLOCATE( lat_hr,lon_hr,angle_hr,tb_hr,qc_hr,RelAziAngle_hr,SolZenAngle_hr,node_hr,&
          scanUTC_hr,scanDay_hr,scanYear_hr,angleFM,lat,lon,angle,tb,qc,RelAziAngle,SolZenAngle)
     DEALLOCATE(BT_Image,BT_Image_AAPP,BT_2AAPP)
     DEALLOCATE(ScanLineInROI)
     !---Close SDR file
     CLOSE(iu_sdr2)
     !---Close FM-SDR file
     CLOSE(iu_FMsdr)
  ENDDO FilesLoop
  DEALLOCATE(sdrFiles)
  DEALLOCATE(FMsdrFiles)
  CALL CloseLogFile()

end program fm_npp
