!===============================================================
! Name:      tdr2sdr 
!
!
! Type:         Main Program
!
!
! Description:
!       Transforms the TDRs  into SDRs.
!       This should be done by applying an antenna pattern correction.
!
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

program tdr2sdr
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used in this module
  INTRINSIC :: INT,MINVAL,TRIM

  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: tdrFiles,sdrFiles
  INTEGER                                          :: iu_tdr,iu_sdr,nfiles,ifile
  INTEGER                                          :: iscanline,node
  INTEGER                                          :: nScanL,nFovs,nqc,nchan
  REAL,               DIMENSION(:),    POINTER     :: Cfreq
  INTEGER,            DIMENSION(:),    POINTER     :: pol
  REAL,               DIMENSION(:),    ALLOCATABLE :: lat,lon,angle,RelAziAngle,SolZenAngle
  REAL,               DIMENSION(:,:),  ALLOCATABLE :: tb
  INTEGER,            DIMENSION(:),    ALLOCATABLE :: qc
  INTEGER(2)                                       :: scanDAY,scanYear
  INTEGER(4)                                       :: scanUTC
  INTEGER                                          :: Day, Year, UTC,iu_list

  !---Namelist data 
  INTEGER                                     :: tdrFormat=DEFAULT_VALUE_INT
  INTEGER                                     :: norbits2process=DEFAULT_VALUE_INT
  CHARACTER(LEN=250)                          :: tdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                          :: pathSDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                          :: AntennaPattFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                          :: LogFile=DEFAULT_VALUE_STR4
 
  NAMELIST /ContrlTDR2SDR/tdrfileList,tdrFormat,pathSDR,AntennaPattFile,norbits2process,LogFile

  !---Read control-data from namelist
  READ(*,NML=ContrlTDR2SDR)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !--Check
  !IF (tdrFormat.ne.1) CALL ErrHandl(ErrorType,Err_NotImplemented,'Only binary format is supported.')
  !---Read the file names of RDR data and build TDR files names
  call ReadList(iu_list,trim(tdrfileList),tdrFiles,nFiles,sdrFiles,pathSDR,'SDR_')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  nfiles=minval((/nfiles,norbits2process/))
  !---------------------------------------------------------
  !  Loop over the TDR files
  !---------------------------------------------------------
  FilesLoop: DO ifile=1,nFiles
      write(*,*) 'ifile=',ifile
      write(*,'(A)') 'TDR='//TRIM( tdrFiles(ifile) )
      write(*,'(A)') 'SDR='//TRIM( sdrFiles(ifile) )
      write(*,*)
      !---Open/Read TDR header
      IF (tdrFormat.eq.1) THEN 
         CALL ReadRadHdrScanLMode(tdrFiles(ifile),iu_tdr,nScanL,nFovs,nqc,nchan,CFreq,pol)
      ENDIF
      IF (tdrFormat.eq.0) THEN 
         CALL ReadRadHdrScanLMode_ascii(tdrFiles(ifile),iu_tdr,nScanL,nFovs,nqc,nchan,CFreq,pol)
      ENDIF
      !---Open/Write SDR header
      CALL WriteRadHdrScanLMode(sdrFiles(ifile),iu_sdr,nScanL,nFovs,nqc,nchan,CFreq,pol)
      !---Release memory
      DEALLOCATE(CFreq)
      DEALLOCATE(pol)
      !---Allocate memory for to-be-read vectors/arrays
      ALLOCATE(lat(nFovs),lon(nFovs),angle(nFovs),tb(nFovs,nchan),qc(nqc),RelAziAngle(nFovs),SolZenAngle(nFovs))
      !---Loop over scanlines
      ScanLinesLoop: DO iscanLine=1,nScanL
         !---Read TDR scanline content
         IF (tdrFormat.eq.1) THEN 
            CALL ReadRadMeasScanLMode(iu_tdr,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,&
                 UTC,DAY,Year,RelAziAngle,SolZenAngle)
         ENDIF
         IF (tdrFormat.eq.0) THEN 
            CALL ReadRadMeasScanLMode_ascii(iu_tdr,nqc,qc,nchan,nFovs,angle,tb,lat,lon,&
                 node,UTC,DAY,Year,RelAziAngle,SolZenAngle)
         ENDIF
         scanDAY  = Day
         scanYear = Year
         scanUTC  = UTC
         !---Process antenna pattern correction (place-holder)
         !---Write SDR scanline
         CALL WriteRadMeasScanLMode(iu_sdr,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,&
              int(scanUTC),int(scanDAY),int(scanYear),RelAziAngle,SolZenAngle)
      ENDDO ScanLinesLoop
      DEALLOCATE(lat,lon,angle,tb,qc,RelAziAngle,SolZenAngle)
      !---Close SDR file
      CLOSE(iu_sdr)
      !---Close TDR file
      CLOSE(iu_tdr)
  ENDDO FilesLoop

  DEALLOCATE(tdrFiles)
  DEALLOCATE(sdrFiles)
  CALL CloseLogFile()

end program tdr2sdr
