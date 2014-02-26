!===============================================================
! Name:      footprintMatching
!
!
! Type:         Main Program
!
!
! Description:
!       Transforms the SDRs  into FM-SDRs.
!       No attempt to do footprint matching
!
!
! Modules needed:
!       - IO_MeasurData
!       - misc
!       - ErrorHandling
!
!
! History:
!       02-13-2009      Tiger.Yang @ NSMC @ IMSG,NOAA/NESDIS/STAR
!reference: based on the fm_aqua.f90 program
!===============================================================

program fm_fy3_mwri
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: MINVAL,MOD,TRIM
  
  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: sdrFiles,FMsdrFiles
  INTEGER                                          :: iu_sdr,iu_fmsdr,nfiles,ifile
  INTEGER                                          :: iscanline,ifov,node
  INTEGER                                          :: nScanL,nFovs,nqc,nchan
  REAL,               DIMENSION(:),    POINTER     :: Cfreq
  INTEGER,            DIMENSION(:),    POINTER     :: pol
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat,lon,angle,RelAziAngle,SolZenAngle
  REAL,               DIMENSION(:),    ALLOCATABLE :: angleFM
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: tb
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: qc
  INTEGER                                          :: scanDAY,scanYear
  INTEGER                                          :: scanUTC
  INTEGER                                          :: iu_list
  INTEGER                                          :: nProcessScanLines=0,iProcessScanLine 
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: ScanLineInROI
  INTEGER                                          :: WRITE_HEADER_DONE=0
  
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
  !---------------------------------------------------------
  !  Loop over the TDR files
  !---------------------------------------------------------
  FilesLoop: DO ifile=1,nFiles
      WRITE_HEADER_DONE=0
      
      print *, 'ifile',ifile,sdrFiles(ifile),FMsdrFiles(ifile)
      
      !---Open/Read SDR header
      CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr,nScanL,nFovs,nqc,nchan,CFreq,pol)
      !---Release memory
      DEALLOCATE(CFreq,pol)
     
      !---Allocate memory for to-be-read vectors/arrays
      ALLOCATE(lat(nFovs),lon(nFovs),angle(nFovs),tb(nFovs,nchan),qc(nqc),RelAziAngle(nFovs),SolZenAngle(nFovs),angleFM(nchan) )
     
      !---Loop over scanlines to flag whether the scanline need to process or not
      ALLOCATE(ScanLineInROI(nScanL))
      ScanLineInROI = 0
      nProcessScanLines=0
      
      ScanLinesLoopFlag: DO iscanLine=1,nScanL
         !---Read SDR scanline content
         CALL ReadRadMeasScanLMode(iu_sdr,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,&
              scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle)
         
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
            CALL ErrHandl(ErrorType,Err_OptionNotSupported,'(in AQUA-AMSRE FM)') 
        ENDIF
      ENDDO ScanLinesLoopFlag
      CLOSE(iu_sdr)
      
    
      !---Open/Read SDR header
      CALL ReadRadHdrScanLMode(sdrFiles(ifile),iu_sdr,nScanL,nFovs,nqc,nchan,CFreq,pol)
      iProcessScanLine = 0
      !---Loop over scanlines
      ScanLinesLoop: DO iscanLine=1,nScanL
         !---Read SDR scanline content
         CALL ReadRadMeasScanLMode(iu_sdr,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,&
              scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle)
         
        !---If scanline has any point that fit within the geographic limits, output it
        IF (ScanLineInROI(iscanLine) .EQ. 1 ) THEN
          iProcessScanLine = iProcessScanLine + 1
          IF( WRITE_HEADER_DONE .EQ. 0 ) THEN
              !---Open/Write FM-SDR header
              CALL WriteHdrMeasurmts(FMsdrFiles(ifile),iu_fmsdr,nProcessScanLines*nFovs,&
                   nqc,nchan,nFovs,CFreq,pol,nProcessScanLines)
              WRITE_HEADER_DONE = 1
              !---Release memory
              DEALLOCATE(CFreq,pol)
          ENDIF
 
          !---Write out FM-SDR
          FOVsLoop: Do ifov=1,nFovs
           qc(:)=0
          !  !if(mod(iscanLine,8)/=1) qc(:)=1 ! data thinning
          !  !if(mod(ifov,4)/=1) qc(:)=1      ! data thinning
          !  angleFM(1:nchan)=angle(ifov)
           CALL WriteMeasurmts(iu_fmsdr,nqc,qc,nchan,angle,tb(ifov,:),lat(ifov),&
                lon(ifov),node,scanUTC*1.0,scanDAY,scanYear,ifov,iProcessScanLine,&
                RelAziAngle(ifov),SolZenAngle(ifov))
          ENDDO FOVsLoop

        ENDIF
 
      ENDDO ScanLinesLoop
     
      DEALLOCATE(lat,lon,angle,tb,qc,RelAziAngle,SolZenAngle,angleFM)
      DEALLOCATE(ScanLineInROI)
      !---Close FM-SDR file
      IF( WRITE_HEADER_DONE .EQ. 1 ) CLOSE(iu_fmsdr)
      !---Close SDR file
      CLOSE(iu_sdr)
  ENDDO FilesLoop

  DEALLOCATE(sdrFiles)
  DEALLOCATE(FMsdrFiles)
  CALL CloseLogFile()

end program fm_fy3_mwri
