!$Id: fwd.f90 3265 2013-03-14 20:58:34Z kgarrett $
!===============================================================
! Name:       ForwardModel
!
!
! Type:         Main Program
!
!
! Description:
!
!       Main forward simulation program. Used for all sensors.
!
! Modules needed:
!       - CRTM_Module
!       - misc
!       - utils
!       - CntrlParams
!       - IO_InstrConfig
!       - IO_MeasurData
!       - IO_Scene
!       - QCchecking
!       - Consts
!       - Noise
!       - ErrorHandling
!       - FwdOperator
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       03-14-2013      Kevin Garrett, RTI @ NOAA/NESDIS/STAR
!                       - Updated interface to CRTM 2.1.1
!
!===============================================================

Program ForwardModel
  !---CRTM module
  USE CRTM_Module
  !---Utility functions
  USE misc
  USE utils
  USE CntrlParams
  USE IO_InstrConfig
  USE IO_MeasurData
  USE IO_Scene
  USE QCchecking
  USE Consts
  USE Noise
  USE ErrorHandling
  USE FwdOperator
  !---Everything explicitly declared
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ADJUSTL,COS,REAL,SNGL

  !----------------------------------------------------------------------------
  !   Declaration Section
  !----------------------------------------------------------------------------
  !---File units and local variables
  INTEGER                                      :: iprof,fID,FileOut
  INTEGER                                      :: nSensors
  CHARACTER(STRLEN), DIMENSION(:), ALLOCATABLE :: SensorID
  INTEGER                                      :: Error_Status, allocate_status
  !---Geophysical data and QC
  INTEGER                                  :: nqc,nPrf,nProfiles,nLay,nLev
  INTEGER                                  :: nLayEff,nLevEff,ierr
  INTEGER, DIMENSION(:), ALLOCATABLE       :: qc
  TYPE(Scene_type)                         :: Scene 
  REAL                                     :: CLW,RWP,IWP,SWP,GWP
  !---Instrument-related variables
  TYPE(InstrConfig_type)                   :: InstrConfig
  INTEGER                                  :: nchan,ichan
  REAL,    DIMENSION(:), ALLOCATABLE       :: NoiseErr,TB,angle

  !---CRTM-related variables/structures
  TYPE(CRTM_ChannelInfo_type), DIMENSION(:), ALLOCATABLE   :: ChannelInfo
  TYPE(CRTM_Atmosphere_type)                               :: Atmos(1)
  TYPE(CRTM_Surface_type)                                  :: Sfc(1)
  TYPE(CRTM_Options_type)                                  :: Options(1)
  TYPE(CRTM_RTSolution_type),  DIMENSION(:,:), ALLOCATABLE :: RTSolution

  !----------------------------------------------------------------------------
  !   Loading off-line databases and control parameters 
  !----------------------------------------------------------------------------
  CALL LoadCntrlParams_Fwd()
  CALL OpenLogFile(CntrlConfig_fwd%Logfile)
  IF (CntrlConfig_fwd%iAddDeviceNoise .eq. 1) THEN
      CALL LoadNoise(CntrlConfig_fwd%NoiseFile)
      CALL SetUpRandomGenSeed()
  ENDIF

  !----------------------------------------------------------------------------
  !   Initialize CRTM
  !----------------------------------------------------------------------------
  !---Get number of sensors on satellite and determine SensorID character string
  CALL GetSensorInfo(CntrlConfig_fwd%SensorID,SensorID,nSensors)

  ALLOCATE(ChannelInfo(nSensors))
  Error_Status = CRTM_Init(SensorID,ChannelInfo,File_Path=CntrlConfig_fwd%Coeff_Path)
  IF (Error_Status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Init Failed')
  nchan=SUM(ChannelInfo%n_Channels)
  !---Create Atmophere, Surface and Options structures
  CALL CRTM_Atmosphere_Create(Atmos,MAXLAYS,MAXABSORB,MAXCLOUDS,MAXAEROSOL)
  CALL CRTM_Surface_Create(Sfc,nchan)
  CALL CRTM_Options_Create(Options,nchan)
  !---Allocate and Initialize RTSolution structures
  ALLOCATE(RTSolution(nchan,1),STAT=allocate_status)
  IF (allocate_status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'RT Structure Allocation Error')
  CALL CRTM_RTSolution_Create(RTSolution,MAXLAYS)

  !Try calling subrouting which does above initialization in future 
!  CALL  Init_CRTM(SensorID,CntrlConfig_fwd%Coeff_Path,ChannelInfo,Atmos,Atmos_K,Sfc, &
!       Sfc_K,Options,RTSolution,RTSolution_K)

  !----------------------------------------------------------------------------
  !   Sanity Checks 
  !----------------------------------------------------------------------------
  IF(nchan.ne.CntrlConfig_fwd%nchan) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'nChan not consistent with CRTM')
  IF (CntrlConfig_fwd%iAddDeviceNoise .eq. 1) THEN
      IF(nchan.ne.noiseInfo%nChan) CALL ErrHandl(ErrorType,Err_NOISEneCNTRL,'nChan not consistent with NEDT File')
  ENDIF

  !----------------------------------------------------------------------------
  !   Load Instrumental configuration
  !----------------------------------------------------------------------------
  CALL ReadInstrConfig(CntrlConfig_fwd%InstrConfigFile,InstrConfig)
  ALLOCATE(NoiseErr(nchan),TB(nchan),angle(nchan))
  !----------------------------------------------------------------------------
  !   Open geophysical data file and load header
  !----------------------------------------------------------------------------
  CALL ReadHdrScene(fID,CntrlConfig_fwd%GeophInputFile,Scene,nPrf)
  !---Determine the number of profiles to be processed
  CALL GetNprofs2Process_fwd(CntrlConfig_fwd%nprofs2process,nPrf,nprofiles)
  !----------------------------------------------------------------------------
  !   Open Output File(s) and write Header/  Set Up QC
  !----------------------------------------------------------------------------
  nqc=1
  ALLOCATE(qc(nqc))
  qc(1:nqc) = 0
  CALL WriteHdrMeasurmts(CntrlConfig_fwd%OutputFile,FileOut,nProfiles,nqc,nchan,     &
       Scene%nPosScan,InstrConfig%CentrFreq,InstrConfig%Polarity,Scene%nScanLines)
  !----------------------------------------------------------------------------
  !   Loop over the profiles
  !----------------------------------------------------------------------------
  ProfLoop: DO iprof=1,nProfiles
      !---Read the Profiles data
      CALL ReadScene(fID,Scene,ierr)
      IF (ierr.ne.0) CYCLE ProfLoop
      qc(1)=0
      nLay = Scene%nLay
      nLev = Scene%nLev

      IF (CntrlConfig_fwd%iCldOffOrOn .eq. 0) THEN
         Scene%CLW     = 0
         Scene%GRAUPEL = 0
         Scene%ICE     = 0
         Scene%RAIN    = 0
         Scene%SNOW    = 0
      ENDIF

      !---Interfacing the Fwd simulator with the CRTM structure
      IF (ANY(Scene%Emiss .lt. 0) .or. Scene%Tskin .lt. 0 .or. Scene%qc(1) .ne. 0) THEN
         TB(1:nchan) = -999.
         qc(1)       = 1
      ELSE
         CALL InterfaceCRTM(Scene,ChannelInfo,Atmos,Sfc,Options,RTSolution,TB)
         !---Flag bad TB values
         IF (ANY(TB(1:nChan) .le. 0)) qc(1) = 2
         !---Flag the simulations done on non-convergent profiles
         IF (Scene%iTyp.eq.1 .and. Scene%ChiSq .gt. 1.) qc(1) = 2
      ENDIF
      !---Simulation of device noise
      IF (CntrlConfig_fwd%iAddDeviceNoise .eq. 1) THEN
        CALL GenerateNoiseErr(nchan,NoiseErr)
        CALL NoiseOnTopOfRad(nchan,tb,NoiseErr)
      ENDIF
      !---Output only those that were turned ON
      DO ichan=1,nchan
        IF(CntrlConfig_fwd%ChanSel(ichan).eq.0) TB(ichan)=DEFAULT_VALUE_REAL
      ENDDO
      !---Output simulation results
      angle(1:nchan) = Scene%angle
      CALL WriteMeasurmts(FileOut,nqc,qc,nChan,angle,TB,Scene%lat,Scene%lon,&
          Scene%node,Scene%scanUTC,Scene%scanDAY,Scene%scanYear,Scene%iscanPos,&
          Scene%iscanLine,Scene%RelAziAngle,Scene%SolZenAngle)
      !---Output monitoring Information
      IF (CntrlConfig_fwd%iPrintMonitor.eq.1) &
          WRITE(*,'(a10,i6,2i4,a7,40f10.3)')'Prof/Typ:',iprof,qc(1),Scene%iTypSfc,'   TBs:',TB(1:4)
  ENDDO ProfLoop
  !---Close Profiles File and Output File 
  CLOSE(fID)
  CLOSE(FileOut)
  
  !---Release memory allocated in LoadNoise of src/lib/noise/Noise.f90
  IF (CntrlConfig_fwd%iAddDeviceNoise .eq. 1) DEALLOCATE(noiseInfo%CentrFreq,noiseInfo%rms,noiseInfo%nedt)

  !---Release memory allocated in CRTM_Init
  Error_Status = CRTM_Destroy(ChannelInfo)
  CALL CRTM_RTSolution_Destroy(RTSolution)
  CALL CRTM_Atmosphere_Destroy(Atmos)
  CALL CRTM_Surface_Destroy(Sfc)
  CALL CRTM_Options_Destroy(Options)

  !--- Release memory allocated in ReadInstrConfig of src/lib/io/IO_InstrConfig.f90
  DEALLOCATE(InstrConfig%CentrFreq,InstrConfig%Polarity)
  DEALLOCATE(NoiseErr,TB,angle)
  DEALLOCATE(SensorID)
  CALL DestroyScene(Scene)
  DEALLOCATE(qc)

  !---Close log file
  CALL CloseLogFile()

END Program ForwardModel

