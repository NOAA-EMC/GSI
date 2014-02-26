!$Id: rdr2tdr_n18_mhs.f90 1987 2009-08-27 21:04:22Z kgarrett $
!===============================================================
! Name:       rdr2tdr_mhs
!
!
! Type:         Main Program
!
!
! Description:
!       Program to convert raw data of MHS into TDRs.
!       Based on program of Ninghai Sun / Fuzhong Weng : grid_noaan_v2.f90
!
! Modules needed:
!       - USE IO_InstrConfig
!       - USE IO_MeasurData
!       - USE IO_Noise
!       - USE misc
!       - USE ErrorHandling
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================
Program rdr2tdr_mhs
  USE IO_InstrConfig
  USE IO_MeasurData
  USE IO_Noise
  USE IO_Misc
  USE misc
  USE Consts
  USE ErrorHandling
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ABS,IBITS,INT,MINVAL,NINT,SUM,TRIM

  INTEGER,      PARAMETER             :: nch_m = 5, nfov_m = 90,nqc=5
  INTEGER,      PARAMETER             :: mxDeltaTim = 2800,nominalDeltaTim = 2667 !ms
  REAL,         PARAMETER             :: mxDeltaLat = 0.16   !degrees
  INTEGER,      PARAMETER             :: mxScan = 30000, nWarmView =4
  REAL,         PARAMETER             :: wn2ghz=29.98 !to convert wavenumber to freq
  !---Specify global varables
  INTEGER(2)                          :: num_scan,missScanLPred
  INTEGER(4)                          :: ierror,ichan,irec,nValid
  INTEGER                             :: ifov,nFiles,ifile,iu_Swath,midFov=nfov_m/2
  INTEGER                             :: nEffScanL,GuessNumbMissScanLin,imiss,scanLineQualFlg1
  INTEGER                             :: scanLineQualFlg2,scanLineQualFlg3,scanLineQualFlg4
  INTEGER                             :: i,j,k,nEffFiles,iView,iu_list,iu_mhs
  INTEGER(2)                          :: ScanLinIndex
  INTEGER(4)                          :: WarmTargCount
  REAL                                :: dlat,dtim,dDay
  !---Pointers and other allocatable arrays
  CHARACTER(LEN=250), DIMENSION(:),       POINTER     :: rdrFilesMHS,tdrFilesMHS
  REAL,               DIMENSION(:,:),     ALLOCATABLE :: LATtab,LONtab,SatZenAngTab,SolZenAngTab,RelAziAngTab,LATtabAllFiles
  REAL,               DIMENSION(:,:,:),   ALLOCATABLE :: TBtab
  INTEGER,            DIMENSION(:,:,:),   ALLOCATABLE :: qcScanLine
  INTEGER,            DIMENSION(:),       ALLOCATABLE :: NodeTab
  INTEGER(2),         DIMENSION(:),       ALLOCATABLE :: scanDAYtab,scanYearTab,nEffScanLvec
  INTEGER(4),         DIMENSION(:),       ALLOCATABLE :: scanUTCtab
  REAL(4),            DIMENSION(:,:,:,:), ALLOCATABLE :: warmTargetTb
  REAL,               DIMENSION(:),       ALLOCATABLE :: warmTargetVec
  REAL,               DIMENSION(:),       ALLOCATABLE :: NEDT
  REAL,               DIMENSION(:,:),     ALLOCATABLE :: NEDTperOrbit
  !---Dummy arrays for missing scanlines
  REAL,               DIMENSION(nfov_m)             :: scanDummy=DEFAULT_VALUE_REAL
  REAL,               DIMENSION(nfov_m,nch_m)       :: TBDummy=DEFAULT_VALUE_REAL
  INTEGER,            DIMENSION(nqc)                :: qcDummy=DEFAULT_VALUE_INT
  !---Single variables
  INTEGER(2)                           :: scanDAY,scanYear
  INTEGER(4)                           :: scanUTC
  INTEGER                              :: qcScan
  !---Specify arrays related to MHS
  CHARACTER(1),   DIMENSION(3072)      :: mhs_header, mhs_data
  INTEGER(4),     DIMENSION(nch_m*3)   :: rad2tb_b,calib_b
  INTEGER(4),     DIMENSION(nfov_m*2)  :: loc_b
  INTEGER(4),     DIMENSION(nfov_m*3)  :: ssl_b
  INTEGER(4)                           :: count_b(540)
  REAL(4),        DIMENSION(nch_m)     :: mhs_wvnum, mhs_const, mhs_slope
  REAL(4),        DIMENSION(nch_m)     :: mhs_a0, mhs_a1, mhs_a2,mhs_tb
  REAL(4),        DIMENSION(nch_m)     :: CentrFreq
  INTEGER,        DIMENSION(nch_m)     :: polar,computedOrNot
  REAL(4)                              :: mhs_lat,mhs_lon,mhs_sza,mhs_solza,mhs_raa
  INTEGER(2)                           :: sat_id
  TYPE(InstrConfig_type)               :: InstrConfig
  !---Big-Endian to Little-Endian variables
  CHARACTER(1), DIMENSION(4, nch_m*3)  :: rad2tb_bb, calib_bb
  CHARACTER(1), DIMENSION(4, nfov_m*2) :: loc_bb
  CHARACTER(1), DIMENSION(2, nfov_m*3) :: ssl_bb
  CHARACTER(1), DIMENSION(2, 540)      :: count_bb
  INTEGER                              :: nEffFilesValid
  
  !---Namelist data 
  CHARACTER(LEN=250)                  :: rdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                  :: pathTDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                  :: NEDTfile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                  :: InstrConfigFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                  :: WarmTargetFile=DEFAULT_VALUE_STR4
  INTEGER                             :: norbits2process=DEFAULT_VALUE_INT
  CHARACTER(LEN=250)                  :: LogFile=DEFAULT_VALUE_STR4
  
  NAMELIST /ContrlRDR2TDR/rdrfileList,pathTDR,NEDTfile,InstrConfigFile,WarmTargetFile,norbits2process,LogFile

  !-------------------------------------------------------------------------------------
  !   MHS general information from header/data record
  !-------------------------------------------------------------------------------------
  EQUIVALENCE (mhs_header(417), rad2tb_bb)     !central wavenumber, constant and slopes at 5 channels
  EQUIVALENCE (mhs_data(61),    calib_bb)      !calibration coeffs for each scan line
  EQUIVALENCE (mhs_data(213),   ssl_bb)        !sza, satellite zenith, local azimuth angles for 90 positions
  EQUIVALENCE (mhs_data(753),   loc_bb)        !earth location of 90 beam positions in each scan lines
  EQUIVALENCE (mhs_data(1481),  count_bb)      !data count at 90 positions for 5 MHS channels

  !-------------------------------------------------------------------------------------
  !   Read control-data from namelist
  !-------------------------------------------------------------------------------------
  READ(*,NML=ContrlRDR2TDR)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of MHS RDR data and build TDR files names
  call ReadList(iu_list,trim(rdrfileList),rdrFilesMHS,nFiles,tdrFilesMHS,pathTDR,'TDR_')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  nfiles=minval((/nfiles,norbits2process/))
  !----------------------------------------------------------------------------
  !   Load Instrumental configuration
  !----------------------------------------------------------------------------
  CALL ReadInstrConfig(InstrConfigFile,InstrConfig)
  !==================================================================================================
  !  Processing MHS data
  !==================================================================================================
  nEffFiles=0
  ALLOCATE(warmTargetTb(mxScan,nch_m,nWarmView,nFiles),NEDT(nch_m),NEDTperOrbit(nch_m,nFiles),   &
       warmTargetVec(nWarmView*mxScan*nFiles),nEffScanLvec(nFiles),LATtabAllFiles(mxScan,nFiles),&
       qcScanLine(nqc,mxScan,nfiles))
  MHS: DO ifile=1,nFiles
     print *, '================================================'
     !---Open MHS RDR file
     iu_mhs=get_lun()
     OPEN(iu_mhs, FILE=rdrFilesMHS(ifile), FORM='UNFORMATTED', ACCESS='DIRECT', RECL=3072,&
         STATUS='OLD', ACTION='READ', IOSTAT=ierror)
     IF (ierror /= 0) CALL ErrHandl(ErrorType,Err_FileNotFound,rdrFilesMHS(ifile))
     nEffFiles=nEffFiles+1
     !---Read in MHS header information
     READ(iu_mhs, REC=1, IOSTAT=ierror) mhs_header
     !---Big-Endian to Little-Endian Translation of header data
     num_scan = Translate2I2(mhs_header(133:134))
     DO ichan=1,nch_m*3
        rad2tb_b(ichan) = Translate2I4(rad2tb_bb(1:4,ichan))
     END DO
     sat_id        = Translate2I2(mhs_header(73:74))
     missScanLPred = Translate2I2(mhs_header(137:138))
     PRINT *, ' MHS number of scan lines = ', num_scan
     PRINT *, ' Reported Missing:',missScanLPred
     !---Decode the wavenumber and calibration coefficients
     DO ichan = 1, nch_m
        mhs_wvnum(ichan) = rad2tb_b((ichan-1)*3 + 1)*1.0E-06
        mhs_const(ichan) = rad2tb_b((ichan-1)*3 + 2)*1.0E-06
        mhs_slope(ichan) = rad2tb_b((ichan-1)*3 + 3)*1.0E-06
     END DO
     !---Allocate memory for arrays that will hold TBs, etc
     ALLOCATE(TBtab(num_scan,nfov_m,nch_m),LATtab(num_scan,nfov_m),   &
          LONtab(num_scan,nfov_m),SatZenAngTab(num_scan,nfov_m),      &
          NodeTab(num_scan),scanUTCtab(num_scan),scanDAYtab(num_scan),&
          scanYearTab(num_scan),RelAziAngTab(num_scan,nfov_m),        &
          SolZenAngTab(num_scan,nfov_m))
     !------------------------------------------------------------
     !     Loop over the number of scanlines
     !------------------------------------------------------------
     nEffScanL   = 0
     BSCAN_LINE: DO irec = 1, num_scan
        !---
        READ(iu_mhs, REC=irec+1, IOSTAT=ierror) mhs_data
        IF(ierror /= 0) THEN
           CALL ErrHandl(WarningType,Warn_BadScanline,'')
           CYCLE BSCAN_LINE
        ENDIF
        !----Get the quality flags
        scanLineQualFlg1 = Translate2I1(mhs_data(29)) !Scan line Quality flag [Additional Calibration problem code]
        scanLineQualFlg2 = Translate2I1(mhs_data(30)) !Scan line Quality flag [Time Pb code]
        scanLineQualFlg3 = Translate2I1(mhs_data(31)) !Scan line Quality flag [Calibration problem code]
        scanLineQualFlg4 = Translate2I1(mhs_data(32)) !Scan line Quality flag [Earth Location Pb code]
        qcScan = Translate2I4(mhs_data(25:28))
        ScanLinIndex                      = Translate2I2(mhs_data(1:2))
        !---Get Timing information
        scanYear  = Translate2I2(mhs_data(3:4))
        scanDAY   = Translate2I2(mhs_data(5:6))
        scanUTC   = Translate2I4(mhs_data(9:12))
        !----Increment the number of scanlines
        nEffScanL = nEffScanL+1
        nEffScanLvec(ifile) = nEffScanL
        !---Big-Endian to Little-Endian Translation
        DO ichan=1,nch_m*3
           calib_b(ichan) = Translate2I4(calib_bb(1:4,ichan))
        END DO
        DO ichan=1,nfov_m*3
           ssl_b(ichan) = Translate2I2(ssl_bb(1:2,ichan))
        END DO
        DO ichan=1,nfov_m*2
           loc_b(ichan) = Translate2I4(loc_bb(1:4,ichan))
        END DO
        DO ichan=1,540
           count_b(ichan) = Translate2I2(count_bb(1:2,ichan))
           IF( count_b(ichan) .LE. 0) count_b(ichan)=count_b(ichan)+65536 !conversion to unsigned
        END DO
        !---Decode calibration coeff. for 5 MHS Channels at each scan line
        DO ichan = 1, nch_m
           mhs_a2(ichan) = calib_b((ichan-1)*3 + 1)*1.0E-16
           mhs_a1(ichan) = calib_b((ichan-1)*3 + 2)*1.0E-10
           mhs_a0(ichan) = calib_b((ichan-1)*3 + 3)*1.0E-06
        END DO
        !---Check the asc/des nature of each scan
        NodeTab(nEffScanL)                = ibits(Translate2I2(mhs_data(13:14)),15,1)
        scanYearTab(nEffScanL)            = scanYear
        scanDAYtab(nEffScanL)             = scanDAY
        scanUTCtab(nEffScanL)             = scanUTC
        qcScanLine(1,nEffScanL,nEffFiles) = qcScan
        qcScanLine(2,nEffScanL,nEffFiles) = scanLineQualFlg1
        qcScanLine(3,nEffScanL,nEffFiles) = scanLineQualFlg2
        qcScanLine(4,nEffScanL,nEffFiles) = scanLineQualFlg3
        qcScanLine(5,nEffScanL,nEffFiles) = scanLineQualFlg4
        !------------------------------------------------------------
        !  Loop over the number of scan positions within the scanline
        !------------------------------------------------------------
        BFIELD: DO ifov = 1, nfov_m
           mhs_solza =  ssl_b((ifov-1)*3 + 1)*1.0E-2   !solar zenith angle
           mhs_sza   =  ssl_b((ifov-1)*3 + 2)*1.0E-2   !satellite zenith angle
           mhs_raa   =  ssl_b((ifov-1)*3 + 3)*1.0E-2   !relative azimuth angle
           IF (ifov < 46) mhs_sza = -mhs_sza
           mhs_lat =  loc_b((ifov-1)*2 + 1)*1.0E-4   !latitude
           mhs_lon =  loc_b((ifov-1)*2 + 2)*1.0E-4   !longitude
           LATtab(nEffScanL,ifov)       = mhs_lat
           LONtab(nEffScanL,ifov)       = mhs_lon
           SolZenAngTab(nEffScanL,ifov) = mhs_solza
           SatZenAngTab(nEffScanL,ifov) = mhs_sza
           RelAziAngTab(nEffScanL,ifov) = mhs_raa
           !------------------------------------------------------------
           !   Loop over the channels 
           !------------------------------------------------------------
           DO ichan = 1, nch_m
              !---Convert and calibrate scene brightness temperatures
              CALL ConvertNcalibrate(count_b((ifov-1)*6+ichan+1),mhs_a0(ichan),&
                   mhs_a1(ichan),mhs_a2(ichan),mhs_const(ichan),mhs_wvnum(ichan),&
                   mhs_slope(ichan),mhs_tb(ichan))
              TBtab(nEffScanL,ifov,ichan) = mhs_tb(ichan)
           END DO
        END DO BFIELD
        !------------------------------------------------------------
        ! Compute Warm Target TB (to later compute NEDT)
        !------------------------------------------------------------
        DO iView= 1,nWarmView
           DO ichan = 1, nch_m
              !---Convert and calibrate warm target count (1)
              WarmTargCount = Translate2I2(mhs_data(2617+(iView-1)*12+(ichan-1)*2+2:2617+(iView-1)*12+(ichan-1)*2+2+1))
              IF(WarmTargCount .LE. 0) THEN
                 WarmTargCount=WarmTargCount+65536 !conversion to unsigned
              ENDIF
              CALL ConvertNcalibrate(WarmTargCount,mhs_a0(ichan),mhs_a1(ichan),&
                   mhs_a2(ichan),mhs_const(ichan),mhs_wvnum(ichan),mhs_slope(ichan),&
                   warmTargetTb(nEffScanL,ichan,iView,nEffFiles))
           ENDDO
        END DO
     END DO BSCAN_LINE
     !------------------------------------------------------------
     !   Open output file and write header & data (in swath mode)
     !------------------------------------------------------------
     CentrFreq(1:nch_m) = mhs_wvnum(1:nch_m) * wn2ghz
     polar(1:nch_m)     = InstrConfig%Polarity(1:nch_m)
     CALL WriteRadHdrScanLMode(tdrFilesMHS(ifile),iu_Swath,nEffScanL,nfov_m,&
          nqc,nch_m,CentrFreq,polar)
     DO irec = 1, nEffScanL
        IF (irec .ge. 2) THEN
           !---Take care of duplicated records
           IF (scanUTCtab(irec).eq.scanUTCtab(irec-1)) THEN
              scanUTCtab(irec)=scanUTCtab(irec-1)+nominalDeltaTim
           ENDIF
           !---determine if missing scanlines exist
           dlat = abs(LATtab(irec,midFov)-LATtab(irec-1,midFov))
           dtim = abs(scanUTCtab(irec)-scanUTCtab(irec-1))
           dDay = abs(scanDAYtab(irec)-scanDAYtab(irec-1))
           !IF (dDay .ne. 0.0) dTim=abs(dTim-86400000.)
           IF ( ABS(dDay) .gt. 0.0 ) dTim=abs(dTim-86400000.)
           IF (dLat.gt.mxDeltaLat) CALL ErrHandl(WarningType,Warn_LatitudeGap,'')
           GuessNumbMissScanLin=0
           IF (dTim.gt.mxDeltaTim .and.  scanUTCtab(irec).ne. scanUTCtab(irec-1)) THEN
              CALL ErrHandl(WarningType,Warn_TimeGap,'')
              GuessNumbMissScanLin=nint(abs(dtim-nominalDeltaTim)/nominalDeltaTim)
           ENDIF
           if (abs(dtim-2667) .gt.4 .or. GuessNumbMissScanLin .ne.0) THEN
              print *, '====',dtim,GuessNumbMissScanLin,irec,ifile,dtim,nominalDeltaTim,scanUTCtab(irec),scanUTCtab(irec-1)
           ENDIF
           !---Fill potential missing scanlines with dummy values
           DO imiss=1,GuessNumbMissScanLin
              scanUTC  = scanUTCtab(irec-1)+imiss*nominalDeltaTim
              scanDAY  = scanDAYtab(irec-1)
              scanYear = scanYearTab(irec-1)
              CALL WriteRadMeasScanLMode(iu_Swath,nqc,qcDummy(:),nch_m,nfov_m,       &
                   scanDummy(1:nfov_m),TBDummy(1:nfov_m,1:nch_m),scanDummy(1:nfov_m),&
                   scanDummy(1:nfov_m),DEFAULT_VALUE_INT,scanUTC,int(scanDAY),int(scanYear),&
                   scanDummy(1:nfov_m),scanDummy(1:nfov_m))
           ENDDO
        ENDIF
        !---write scanline info data
        CALL WriteRadMeasScanLMode(iu_Swath,nqc,qcScanLine(:,irec,nEffFiles),nch_m,nfov_m,&
             SatZenAngTab(irec,1:nfov_m),TBtab(irec,1:nfov_m,1:nch_m),          &
             LATtab(irec,1:nfov_m),LONtab(irec,1:nfov_m),NodeTab(irec),         &
             scanUTCtab(irec),int(scanDAYtab(irec)),int(scanYearTab(irec)),     &
             RelAziAngTab(irec,1:nfov_m),SolZenAngTab(irec,1:nfov_m))
     ENDDO
     CLOSE(iu_mhs)
     CLOSE(iu_Swath)
     LatTabAllFiles(1:num_scan,ifile) = LATtab(1:num_scan,1)
     DEALLOCATE(TBtab,LATtab,LONtab,SatZenAngTab,NodeTab,scanUTCtab, &
          scanDAYtab,scanYearTab,RelAziAngTab,SolZenAngTab)
  END DO MHS
  !---If no effective scanline found, stop (no further processing possible/desired)
  IF (nEffScanL.eq.0) CALL ErrHandl(ErrorType,Err_NoGoodScanLineFound,'')
  !------------------------------------------------------------
  ! Compute NEDT by means of monitoring the Warm load target
  !------------------------------------------------------------
  computedOrNot=0
  ChanLoop: DO ichan = 1, nch_m
     nEffFilesValid = 0
     OrbitLoop: DO k=1,nEffFiles
        nValid =0
        ScanLinesLoop: Do i=1,nEffScanLvec(k)
           WarmViewLoop: DO j=1,nWarmView
              IF (warmTargetTb(i,ichan,j,k) .lt. 0.) EXIT WarmViewLoop
              IF (warmTargetTb(i,ichan,j,k) .ge. 0. .and. qcScanLine(1,i,k) .eq. 0 .and. &
                   qcScanLine(2,i,k) .eq. 0 .and. qcScanLine(3,i,k) .eq. 0 .and.          &
                   qcScanLine(4,i,k) .eq. 0 .and. qcScanLine(5,i,k) .eq. 0) THEN
                 nValid                = nValid+1
                 warmTargetVec(nValid) = warmTargetTb(i,ichan,j,k)
              ENDIF
           ENDDO WarmViewLoop
        ENDDO ScanLinesLoop
        !---After looping over all orbit scanlines, compute NEDT for that orbit (if possible)
        IF (nvalid .ge. 2) THEN
           nEffFilesValid        = nEffFilesValid+1
           NEDTperOrbit(ichan,nEffFilesValid) = Stdev(warmTargetVec(1:nValid))
        ENDIF
        IF (nvalid .lt. 2) THEN
           CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in MHS RDR conversion). No NEDT generated for this orbit.')
        ENDIF
     ENDDO OrbitLoop
     !---Test if we have at least one valid orbit to compute the NEDT on, otherwise fatal error message
     IF (nEffFilesValid .ge. 1) THEN
        NEDT(ichan) =SUM(NEDTperOrbit(ichan,1:nEffFilesValid))/nEffFilesValid
     ENDIF
     IF (nEffFilesValid .lt. 1) THEN
        OrbitLoop2: DO k=1,nEffFiles
           nValid =0
           ScanLinesLoop2: Do i=1,nEffScanLvec(k)
              WarmViewLoop2: DO j=1,nWarmView
                 IF (warmTargetTb(i,ichan,j,k) .lt. 0.) EXIT WarmViewLoop2
                 IF (warmTargetTb(i,ichan,j,k) .ge. 0. .and. qcScanLine(1,i,k) .eq. 0 .and. &
                      qcScanLine(2,i,k) .eq. 0 .and. qcScanLine(4,i,k) .eq. 0) THEN
                    nValid                = nValid+1
                    warmTargetVec(nValid) = warmTargetTb(i,ichan,j,k)
                 ENDIF
              ENDDO WarmViewLoop2
           ENDDO ScanLinesLoop2
           !---After looping over all orbit scanlines, compute NEDT for that orbit (if possible)
           IF (nvalid .ge. 2) THEN
              nEffFilesValid        = nEffFilesValid+1
              NEDTperOrbit(ichan,nEffFilesValid) = Stdev(warmTargetVec(1:nValid))
           ENDIF
           IF (nvalid .lt. 2) THEN
              CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in MHS RDR conversion). Still no NEDT generated for orbit.')
           ENDIF    
        ENDDO OrbitLoop2
        !---Test if we have at least one valid orbit to compute the NEDT on, otherwise exit program with fatal error
        IF (nEffFilesValid .ge. 1) THEN
           NEDT(ichan) =SUM(NEDTperOrbit(ichan,1:nEffFilesValid))/nEffFilesValid
        ENDIF
        IF (nEffFilesValid .lt. 1) THEN
           NEDT(ichan) = -9.999
           computedOrNot(ichan)=1
           CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in MHS RDR conversion). No NEDT value generated.')  
        ENDIF
        CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in MHS RDR). QC flag relaxed to allow noise computation') 
     ENDIF
  ENDDO ChanLoop
  !------------------------------------------------------------
  !   Open NEDT file and write NEDT data into it
  !------------------------------------------------------------
  CALL WriteNoise(NEDTFile,CentrFreq,nedt,computedOrNot,nch_m)     
  CALL WriteWarmTarget(WarmTargetFile,CentrFreq,warmTargetTb,nch_m,nEffFiles,nEffScanLvec,nWarmView)
  DEALLOCATE(rdrFilesMHS,tdrFilesMHS)     
  DEALLOCATE(InstrConfig%CentrFreq,InstrConfig%Polarity)
  DEALLOCATE(warmTargetTb,NEDT,NEDTperOrbit,warmTargetVec,nEffScanLvec,LATtabAllFiles,qcScanLine)
  CALL CloseLogFile()

CONTAINS


!===============================================================
! Name:         ConvertNcalibrate
!
!
! Type:         Subroutine
!
!
! Description:
!
!
! Arguments:
!
!      Name                       Type      Description
!      ---------------------------------------------------
!       - count_b                  I        Counts
!       - mhs_a0                   I        Calib Coeff  
!       - mhs_a1                   I        Calib Coeff 
!       - mhs_a2                   I        Calib Coeff 
!       - mhs_const                I        Calib Coeff 
!       - mhs_wvnum                I        Calib Coeff 
!       - mhs_slope                I        Calib Coeff
!       - mhs_tb                   O        Temperature
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ConvertNcalibrate(count_b,mhs_a0,mhs_a1,mhs_a2,mhs_const,mhs_wvnum,mhs_slope,mhs_tb)
    INTEGER(4) :: count_b
    REAL(4)    :: mhs_a0,mhs_a1,mhs_a2,mhs_const,mhs_wvnum,mhs_slope,mhs_tb,mhs_rad
    
    mhs_rad=(mhs_a0+mhs_a1*count_b+mhs_a2*count_b**2.)*1.0E-3
    IF (mhs_rad <= 0.0) THEN
       mhs_tb =-999.0
    ELSE
       mhs_tb =mhs_const+mhs_slope*plank(mhs_rad,mhs_wvnum)
    END IF
    RETURN
  END SUBROUTINE ConvertNcalibrate


END PROGRAM rdr2tdr_mhs

