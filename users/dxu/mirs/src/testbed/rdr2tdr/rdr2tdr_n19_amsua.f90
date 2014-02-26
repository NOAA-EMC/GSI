!$Id: rdr2tdr_n18_amsua.f90 1389 2008-07-14 15:51:08Z wchen $
!===============================================================
! Name:       rdr2tdr_amsua
!
!
! Type:       Main Program
!
!
! Description:
!        Program to convert raw data of AMSU into TDRs.
!        Based on program of Ninghai Sun / Fuzhong Weng : grid_noaan_v2.f90
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
Program rdr2tdr_amsua
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

  INTEGER,      PARAMETER             :: nch_a = 15, nfov_a = 30, nqc=5
  INTEGER,      PARAMETER             :: mxDeltaTim = 9000,nominalDeltaTim=8000 !ms
  REAL,         PARAMETER             :: mxDeltaLat = 0.6  !degrees
  REAL,         PARAMETER             :: wn2ghz=29.98 !to convert wavenumber to freq
  INTEGER,      PARAMETER             :: mxScan = 10000, nWarmView =2
  !---Specify global varables
  INTEGER(2)                          :: num_scan,missScanLPred
  INTEGER(4)                          :: ierror, ichan, irec,nValid
  INTEGER                             :: ifov,nFiles,ifile,iu_Swath,midFov=nfov_a/2
  INTEGER                             :: nEffScanL,GuessNumbMissScanLin,imiss
  INTEGER                             :: i,j,k,nEffFiles,iView,iu_list,iu_ama
  INTEGER(2)                          :: ScanLinIndex,counta_tmp
  REAL                                :: dlat,dtim,dDay
  !---Pointers and other allocatable arrays
  CHARACTER(LEN=250), DIMENSION(:),       POINTER     :: rdrFilesAmsuA,tdrFilesAmsuA
  REAL,               DIMENSION(:,:),     ALLOCATABLE :: LATtab,LONtab,SatZenAngTab,SolZenAngTab,RelAziAngTab
  REAL,               DIMENSION(:,:,:),   ALLOCATABLE :: TBtab
  INTEGER,            DIMENSION(:,:,:),   ALLOCATABLE :: qcScanLine
  INTEGER,            DIMENSION(:),       ALLOCATABLE :: NodeTab
  INTEGER(2),         DIMENSION(:),       ALLOCATABLE :: scanDAYtab,scanYearTab,nEffScanLvec
  INTEGER(4),         DIMENSION(:),       ALLOCATABLE :: scanUTCtab
  REAL(4),            DIMENSION(:,:,:,:), ALLOCATABLE :: warmTargetTb
  REAL,               DIMENSION(:),       ALLOCATABLE :: warmTargetVec
  REAL,               DIMENSION(:,:),     ALLOCATABLE :: NEDTperOrbit
  !---Dummy arrays for missing scanlines
  REAL,               DIMENSION(nfov_a)             :: scanDummy=DEFAULT_VALUE_REAL
  REAL,               DIMENSION(nfov_a,nch_a)       :: TBDummy=DEFAULT_VALUE_REAL
  INTEGER,            DIMENSION(nqc)                :: qcDummy=DEFAULT_VALUE_INT
  !---Single variables
  INTEGER(2)                          :: scanDAY,scanYear
  INTEGER(4)                          :: scanUTC
  INTEGER                             :: qcScan,iflg
  !---Specify arrays related to AMSU-A
  CHARACTER(1), DIMENSION(2560)       :: ama_header, ama_data
  INTEGER(4),   DIMENSION(nch_a*3)    :: rad2tb_a,calib_a
  INTEGER(4),   DIMENSION(nfov_a*2)   :: loc_a
  INTEGER(2),   DIMENSION(nfov_a*3)   :: ssl_a
  INTEGER(2)                          :: count_a1(510), count_a2(120)
  REAL(4),      DIMENSION(nch_a)      :: ama_wvnum, ama_const, ama_slope
  REAL(4),      DIMENSION(nch_a)      :: ama_a0, ama_a1, ama_a2,ama_tb
  REAL(4),      DIMENSION(nch_a)      :: CentrFreq
  REAL,         DIMENSION(nch_a)      :: NEDT
  INTEGER,      DIMENSION(nch_a)      :: polar,computedOrNot
  REAL(4)                             :: ama_lat,ama_lon,ama_sza,ama_solza,ama_raa
  INTEGER(2)                          :: sat_id
  TYPE(InstrConfig_type)              :: InstrConfig
  INTEGER                             :: scanLineQualFlg1,scanLineQualFlg2,scanLineQualFlg3,scanLineQualFlg4
  INTEGER(2),        DIMENSION(16)    :: scanLineCalibFlg
  !---Big-Endian to Little-Endian variables
  CHARACTER(1), DIMENSION(4,nch_a*3)  :: rad2tb_ab, calib_ab
  CHARACTER(1), DIMENSION(4)          :: radChar
  CHARACTER(1), DIMENSION(4,nfov_a*2) :: loc_ab
  CHARACTER(1), DIMENSION(2,nfov_a*3) :: ssl_ab
  CHARACTER(1), DIMENSION(2,510)      :: count_a1b
  CHARACTER(1), DIMENSION(2,120)      :: count_a2b
  INTEGER                             :: nEffFilesValid

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
  ! AMSU-A information from header/data record
  !-------------------------------------------------------------------------------------
  EQUIVALENCE (ama_header(689), rad2tb_ab) !central wavenumber, constant and slopes at 15 channels
  EQUIVALENCE (ama_data(81),  calib_ab)    !AMSU-A calibration coeffs for each scan line
  EQUIVALENCE (ama_data(473), ssl_ab)      !sza, satellite zenith, local azimuth angles for 30 pos
  EQUIVALENCE (ama_data(653), loc_ab)      !earth location of 30 beam positions in each scan lines
  EQUIVALENCE (ama_data(905),  count_a1b)  !count at 30 positions for 13 AMSU-A1 channel (3-15)
  EQUIVALENCE (ama_data(2193), count_a2b)  !data count at 30 positions for 2 AMSU-A2 channel (1-2)
  
  !-------------------------------------------------------------------------------------
  !   Read control-data from namelist
  !-------------------------------------------------------------------------------------
  READ(*,NML=ContrlRDR2TDR)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of AMSU-A RDR data and build TDR files names
  call ReadList(iu_list,trim(rdrfileList),rdrFilesAmsuA,nFiles,tdrFilesAmsuA,pathTDR,'TDR_')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  nfiles=minval((/nfiles,norbits2process/))
  !----------------------------------------------------------------------------
  !   Load Instrumental configuration
  !----------------------------------------------------------------------------
  CALL ReadInstrConfig(InstrConfigFile,InstrConfig)
  !=====================================================================================================
  ! Processing AMSU-A data
  !=====================================================================================================
  nEffFiles=0
  ALLOCATE(warmTargetTb(mxScan,nch_a,nWarmView,nFiles),qcScanLine(nqc,mxScan,nfiles),&
       warmTargetVec(nWarmView*mxScan*nFiles),nEffScanLvec(nFiles),NEDTperOrbit(nch_a,nFiles))
  qcScanLine =0 
  AMSUA: DO ifile=1,nFiles
     !---Open AMSU RDR file
     iu_ama=get_lun()
     OPEN(iu_ama, FILE=trim(rdrFilesAmsuA(ifile)), FORM='UNFORMATTED', ACCESS='DIRECT', RECL=640*4,&
          STATUS='OLD', ACTION='READ', IOSTAT=ierror)
     IF (ierror /= 0) CALL ErrHandl(ErrorType,Err_FileNotFound,rdrFilesAmsuA(ifile))
     nEffFiles=nEffFiles+1
     !---Read in amsu header information
     READ(iu_ama, REC=1, IOSTAT=ierror) ama_header
     !---Big-Endian to Little-Endian Translation of header data
     num_scan = Translate2I2(ama_header(145:146))
     DO ichan = 1, nch_a*3
        radChar(1:4) = rad2tb_ab(1:4,ichan)
        rad2tb_a(ichan) = Translate2I4(rad2tb_ab(1:4,ichan))
     END DO     
     sat_id        = Translate2I2(ama_header(73:74))
     missScanLPred = Translate2I2(ama_header(149:150))
     PRINT *, ' AMSU-A number of scan lines = ', num_scan,' File#',ifile
     PRINT *, ' Reported Missing:',missScanLPred
     !---Decode the wavenumbers and calibration coefficients
     DO ichan = 1, nch_a
        ama_wvnum(ichan) = rad2tb_a((ichan-1)*3 + 1)*1.0E-06
        ama_const(ichan) = rad2tb_a((ichan-1)*3 + 2)*1.0E-06
        ama_slope(ichan) = rad2tb_a((ichan-1)*3 + 3)*1.0E-06
     END DO
     !---Allocate memory for arrays that will hold TBs, etc
     ALLOCATE(TBtab(num_scan,nfov_a,nch_a),LATtab(num_scan,nfov_a),   &
          LONtab(num_scan,nfov_a),SatZenAngTab(num_scan,nfov_a),      &
          NodeTab(num_scan),scanUTCtab(num_scan),scanDAYtab(num_scan),&
          scanYearTab(num_scan),RelAziAngTab(num_scan,nfov_a),        &
          SolZenAngTab(num_scan,nfov_a))
     !------------------------------------------------------------
     !     Loop over the number of scanlines
     !------------------------------------------------------------
     nEffScanL   = 0
     ASCAN_LINE: DO irec = 1, num_scan
        !-----------------------------------
        READ(iu_ama, REC=irec+1, IOSTAT=ierror) ama_data
        IF(ierror /= 0) THEN
           CALL ErrHandl(WarningType,Warn_BadScanline,'')
           CYCLE ASCAN_LINE
        ENDIF
        !----Get the quality flags
        scanLineQualFlg1 = Translate2I1(ama_data(29)) !Scan line Quality flag [Additional Calibration problem code]
        scanLineQualFlg2 = Translate2I1(ama_data(30)) !Scan line Quality flag [Time Pb code]
        scanLineQualFlg3 = Translate2I1(ama_data(31)) !Scan line Quality flag [Calibration problem code]
        scanLineQualFlg4 = Translate2I1(ama_data(32)) !Scan line Quality flag [Earth Location Pb code]
        qcScan           = Translate2I4(ama_data(25:28))
        DO iflg=1,16
           scanLineCalibFlg(iflg) = Translate2I2(ama_data(33+(iflg-1)*2:33+(iflg-1)*2+1))
        ENDDO
        ScanLinIndex     = Translate2I2(ama_data(1:2))
        !---Get Timing information
        scanYear  = Translate2I2(ama_data(3:4))
        scanDAY   = Translate2I2(ama_data(5:6))
        scanUTC   = Translate2I4(ama_data(9:12))
        !----Get the number of scanlines
        nEffScanL = nEffScanL+1
        nEffScanLvec(ifile) = nEffScanL
        !---Big-Endian to Little-Endian Translation
        DO ichan = 1, nch_a*3
           calib_a(ichan) = Translate2I4(calib_ab(1:4,ichan))
        END DO
        DO ichan = 1, nfov_a*3
           ssl_a(ichan) = Translate2I2(ssl_ab(1:2,ichan))
        END DO
        DO ichan = 1, nfov_a*2
           loc_a(ichan) = Translate2I4(loc_ab(1:4,ichan))
        END DO
        DO ichan = 1, 510
           count_a1(ichan) = Translate2I2(count_a1b(1:2,ichan))
        END DO
        DO ichan = 1, 120
           count_a2(ichan) = Translate2I2(count_a2b(1:2,ichan))
        END DO
        scanYearTab(nEffScanL)            = scanYear
        scanDAYtab(nEffScanL)             = scanDAY
        scanUTCtab(nEffScanL)             = scanUTC
        qcScanLine(1,nEffScanL,nEffFiles) = qcScan
        qcScanLine(2,nEffScanL,nEffFiles) = scanLineQualFlg1
        qcScanLine(3,nEffScanL,nEffFiles) = scanLineQualFlg2
        qcScanLine(4,nEffScanL,nEffFiles) = scanLineQualFlg3
        qcScanLine(5,nEffScanL,nEffFiles) = scanLineQualFlg4
        ScanLinIndex                      = Translate2I2(ama_data(1:2))
        !---Decode calibration coeff. for 15 AMSU-A Channels at each scan line
        DO ichan = 1, nch_a
           ama_a2(ichan) = calib_a((ichan-1)*3 + 1)*1.0E-19
           ama_a1(ichan) = calib_a((ichan-1)*3 + 2)*1.0E-13
           ama_a0(ichan) = calib_a((ichan-1)*3 + 3)*1.0E-09
        END DO
        !---Check the asc/desc nature of each scan 
        NodeTab(nEffScanL)      = ibits(Translate2I2(ama_data(13:14)),15,1)
        !------------------------------------------------------------
        !  Loop over the number of scan positions within the scanline
        !------------------------------------------------------------
        AFIELD: DO ifov = 1, nfov_a
           ama_solza =  ssl_a((ifov-1)*3 + 1)*1.0E-2 !solar zenith angle
           ama_sza   =  ssl_a((ifov-1)*3 + 2)*1.0E-2 !satellite zenith angle
           ama_raa   =  ssl_a((ifov-1)*3 + 3)*1.0E-2 !relative azimuth angle
           IF (ifov < 16) ama_sza = -ama_sza
           ama_lat   =  loc_a((ifov-1)*2 + 1)*1.0E-4 !latitude
           ama_lon   =  loc_a((ifov-1)*2 + 2)*1.0E-4 !longitude
           LATtab(nEffScanL,ifov)       = ama_lat
           LONtab(nEffScanL,ifov)       = ama_lon
           SolZenAngTab(nEffScanL,ifov) = ama_solza
           SatZenAngTab(nEffScanL,ifov) = ama_sza
           RelAziAngTab(nEffScanL,ifov) = ama_raa
           !------------------------------------------------------------
           !   Loop over the channels 
           !------------------------------------------------------------
           DO ichan = 1, nch_a
              IF (ichan <= 2) THEN
                 counta_tmp = count_a2((ifov-1)*4+ichan+2)
              ELSE
                 counta_tmp = count_a1((ifov-1)*17+ichan+2)
              END IF
              !---Convert and calibrate scene brightness temperatures
              CALL ConvertNcalibrate(ichan,counta_tmp,ama_a0(ichan),ama_a1(ichan),&
                   ama_a2(ichan),ama_const(ichan),ama_wvnum(ichan),ama_slope(ichan),&
                   ama_tb(ichan))
              TBtab(nEffScanL,ifov,ichan) = ama_tb(ichan)
           END DO
        END DO AFIELD
        !------------------------------------------------------------
        ! Compute Warm Target TB (to later compute NEDT)
        !------------------------------------------------------------
        DO ichan = 1, nch_a
           DO iView= 1,nWarmView
              !---Convert and calibrate warm target count (1)
              IF (ichan <= 2) THEN
                 counta_tmp = Translate2I2(ama_data(2489+(iView-1)*4+&
                 (ichan-1)*nWarmView:2489+(iView-1)*4+(ichan-1)*nWarmView+1))
              ELSE
                 counta_tmp = Translate2I2(ama_data(2085+(iView-1)*26+&
                 (ichan-3)*nWarmView:2085+(iView-1)*26+(ichan-3)*nWarmView+1))
              END IF
              CALL ConvertNcalibrate(ichan,counta_tmp,ama_a0(ichan),ama_a1(ichan),ama_a2(ichan),ama_const(ichan),&
                   ama_wvnum(ichan),ama_slope(ichan),warmTargetTb(nEffScanL,ichan,iView,nEffFiles))
           ENDDO
        END DO
     END DO ASCAN_LINE
     !---If no effective scanline found, stop (no further processing possible/desired)
     IF (nEffScanL.eq.0) CALL ErrHandl(ErrorType,Err_NoGoodScanLineFound,'')
     !------------------------------------------------------------
     !   Open output file and write header & data (in swath mode)
     !------------------------------------------------------------
     CentrFreq(1:nch_a) = ama_wvnum(1:nch_a) * wn2ghz
     polar(1:nch_a)     = InstrConfig%Polarity(1:nch_a)
     CALL WriteRadHdrScanLMode(tdrFilesAmsuA(ifile),iu_Swath,nEffScanL,nfov_a,&
          nqc,nch_a,CentrFreq,polar)
     DO irec = 1, nEffScanL
        IF (irec .ge. 2) THEN
           !---determine if missing scanlines exist
           dlat=abs(LATtab(irec,midFov)-LATtab(irec-1,midFov))
           dtim = abs(scanUTCtab(irec)-scanUTCtab(irec-1))
           dDay = abs(scanDAYtab(irec)-scanDAYtab(irec-1))
           IF ( ABS(dDay) .gt. 0.0 ) dTim=abs(dTim-86400000.)
           IF (dLat.gt.mxDeltaLat) CALL ErrHandl(WarningType,Warn_LatitudeGap,'') 
           GuessNumbMissScanLin=0
           IF (dTim.gt.mxDeltaTim) THEN
              CALL ErrHandl(WarningType,Warn_TimeGap,'')
              GuessNumbMissScanLin=nint(abs(dtim-nominalDeltaTim)/nominalDeltaTim)
           ENDIF
           if (abs(dtim-8000).gt.4 .or. GuessNumbMissScanLin .ne.0) THEN
              print *, '====',irec,dtim,GuessNumbMissScanLin,scanUTCtab(irec),scanUTCtab(irec-1)
           ENDIF
           !---Fill potential missing scanlines with dummy values
           DO imiss=1,GuessNumbMissScanLin
              scanUTC  = scanUTCtab(irec-1)+imiss*nominalDeltaTim
              scanDAY  = scanDAYtab(irec-1)
              scanYear = scanYearTab(irec-1)
              CALL WriteRadMeasScanLMode(iu_Swath,nqc,qcDummy(:),nch_a,nfov_a,       &
                   scanDummy(1:nfov_a),TBDummy(1:nfov_a,1:nch_a),scanDummy(1:nfov_a),&
                   scanDummy(1:nfov_a),DEFAULT_VALUE_INT,scanUTC,int(scanDAY),int(scanYear),&
                   scanDummy(1:nfov_a),scanDummy(1:nfov_a))
           ENDDO
        ENDIF
        !---write scanline info data
        CALL WriteRadMeasScanLMode(iu_Swath,nqc,qcScanLine(:,irec,nEffFiles),nch_a,nfov_a,&
             SatZenAngTab(irec,1:nfov_a),TBtab(irec,1:nfov_a,1:nch_a),          &
             LATtab(irec,1:nfov_a),LONtab(irec,1:nfov_a),NodeTab(irec),         &
             scanUTCtab(irec),int(scanDAYtab(irec)),int(scanYearTab(irec)),     &
             RelAziAngTab(irec,1:nfov_a),SolZenAngTab(irec,1:nfov_a))
     ENDDO
     CLOSE(iu_ama)
     CLOSE(iu_Swath)
     DEALLOCATE(TBtab,LATtab,LONtab,SatZenAngTab,NodeTab,scanUTCtab, &
          scanDAYtab,scanYearTab,RelAziAngTab,SolZenAngTab)
  END DO AMSUA  
  !------------------------------------------------------------
  ! Compute NEDT by means of monitoring the Warm load target
  !------------------------------------------------------------
  computedOrNot=0
  ChanLoop: DO ichan = 1, nch_a
     nEffFilesValid = 0
     OrbitLoop: DO k=1,nEffFiles
        nValid =0
        ScanLinesLoop: Do i=1,nEffScanLvec(k)
           WarmViewLoop: DO j=1,nWarmView
              IF (warmTargetTb(i,ichan,j,k) .lt. 0.) EXIT WarmViewLoop
              IF (warmTargetTb(i,ichan,j,k) .ge. 0. .and. qcScanLine(1,i,k) .eq. 0 .and. &
                  qcScanLine(2,i,k) .eq. 0 .and. qcScanLine(3,i,k) .eq. 0 .and. &
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
           CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in AMSUA RDR conversion). No NEDT generated for this orbit.')
        ENDIF
     ENDDO OrbitLoop
     !---Test if we have at least one valid orbit to compute the NEDT on, otherwise relax QC flags and recompute
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
              CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in AMSUA RDR conversion). Still no NEDT generated for orbit.')
           ENDIF    
        ENDDO OrbitLoop2
        !---Test if we have at least one valid orbit to compute the NEDT on, otherwise exit program with fatal error
        IF (nEffFilesValid .ge. 1) THEN
           NEDT(ichan) =SUM(NEDTperOrbit(ichan,1:nEffFilesValid))/nEffFilesValid
        ENDIF
        IF (nEffFilesValid .lt. 1) THEN
           NEDT(ichan) = -9.999
           computedOrNot(ichan)=1
           CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in AMSUA RDR conversion). No NEDT value generated.')  
        ENDIF
        CALL ErrHandl(WarningType,Warn_NoValidWarmTarget,' (in AMSUA RDR). QC flag relaxed to allow noise computation') 
     ENDIF
  ENDDO ChanLoop
  !------------------------------------------------------------
  !   Open NEDT file and write NEDT data into it
  !------------------------------------------------------------
  CALL WriteNoise(NEDTFile,CentrFreq,nedt,computedOrNot,nch_a)     
  CALL WriteWarmTarget(WarmTargetFile,CentrFreq,warmTargetTb,nch_a,nEffFiles,nEffScanLvec,nWarmView)     
  DEALLOCATE(rdrFilesAmsuA,tdrFilesAmsuA)
  DEALLOCATE(InstrConfig%CentrFreq,InstrConfig%Polarity)
  DEALLOCATE(warmTargetTb,qcScanLine,warmTargetVec,nEffScanLvec,NEDTperOrbit)
  CALL CloseLogFile()

CONTAINS

!===============================================================
! Name:                ConvertNcalibrate
!
!
! Type:                Subroutine
!
!
! Description:  Converts counts into temperatures using calibration
!               Information.
!
!
! Arguments:
!
!            Name                  Type     Description
!      ---------------------------------------------------
!       - ichan                    I        Channel Index    
!       - counta_tmp               I        Counts
!       - ama_a0                   I        Calib Coeff  
!       - ama_a1                   I        Calib Coeff 
!       - ama_a2                   I        Calib Coeff 
!       - ama_const                I        Calib Coeff 
!       - ama_wvnum                I        Calib Coeff 
!       - ama_slope                I        Calib Coeff
!       - ama_tb                   O        Temperature
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
        
  SUBROUTINE ConvertNcalibrate(ichan,counta_tmp,ama_a0,ama_a1,&
       ama_a2,ama_const,ama_wvnum,ama_slope,ama_tb)
    INTEGER    :: ichan
    INTEGER(2) :: counta_tmp
    REAL(4)    :: ama_a0,ama_a1,ama_a2,ama_const,ama_wvnum,ama_slope,ama_tb,ama_rad
    
    ama_rad = (ama_a0+ama_a1*counta_tmp+ama_a2*counta_tmp*counta_tmp)*1.0E-3
    IF (ama_rad <= 0.0) THEN
       ama_tb = - 999.0
    ELSE
       ama_tb =ama_const+plank(ama_rad,ama_wvnum)*ama_slope
    END IF
    RETURN
  END SUBROUTINE ConvertNcalibrate

END PROGRAM rdr2tdr_amsua
