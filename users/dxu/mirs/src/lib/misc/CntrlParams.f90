!$Id: CntrlParams.f90 3284 2013-05-13 19:11:36Z chrisg $
!-----------------------------------------------------------------------------------------------
! Name:         CntrlParams
! 
! Type:         F90 module
!
! Description:
!       Module that contains the necessary routines and corresponding data
!       related to the controlling paramaters used in the 1dVAR/retrieval.
!       Extended to also handle controlling parameters of the fwd operator
!
! Modules needed:
!       - Consts
!
! Subroutines contained:
!       - LoadCntrlParams
!       - LoadCntrlParams_Fwd
!
! Data type included:
!       - CntrlConfig
!       - CntrlConfig_fwd
!       - CntrlConfig_type
!       - CntrlConfig_fwd_type
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE CntrlParams
  USE Consts
  IMPLICIT NONE
  PRIVATE
  !----Publicly available subroutine(s)
  PUBLIC :: LoadCntrlParams,LoadCntrlParams_Fwd
  !----Publicly available data/types
  PUBLIC :: CntrlConfig,CntrlConfig_fwd,CntrlConfig_type,CntrlConfig_fwd_type
  !----Global section of the module
  INTEGER, PARAMETER :: nLEN=256,mxCh=100,mxEDR=100,mxAtt=2
  !------This structure relates to the 1dvar retrieval code-------------
  TYPE :: CntrlConfig_type
    INTEGER                               :: AlgSN              !Algorithm Serial Number
    INTEGER                               :: iSens              !Only when measurement file is of type coloc
    INTEGER                               :: SensorID           !Sensor ID
    CHARACTER(LEN=nLEN)                   :: MeasurmtFile       !Measurement file
    CHARACTER(LEN=nLEN)                   :: BiasFile           !Bias file
    INTEGER                               :: nAttempts          !Number of retr. attempts to make 
    INTEGER                               :: ExternDataAvailab  !External data availability (0: No, 1:yes)
    CHARACTER(LEN=nLEN), DIMENSION(mxAtt) :: TuningFile         !Tuning file(s)
    CHARACTER(LEN=nLEN), DIMENSION(mxAtt) :: CovBkgFileAtm      !Atmospheric covariance/background file
    CHARACTER(LEN=nLEN), DIMENSION(mxAtt) :: CovBkgFileSfc      !Surface covariance/background file
    CHARACTER(LEN=nLEN), DIMENSION(mxAtt) :: FileModelErr       !Model errors file
    CHARACTER(LEN=nLEN)                   :: OutputFile         !Output file
    CHARACTER(LEN=nLEN)                   :: ExternDataFile     !External data file
    CHARACTER(LEN=nLEN)                   :: NoiseFile          !Noise file
    CHARACTER(LEN=nLEN)                   :: MonitorFile        !Monitoring file
    CHARACTER(LEN=nLEN)                   :: Topogr             !Topography file
    INTEGER                               :: PrintMonitor       !Flag to print monitoring on screen
    INTEGER                               :: nprofs2process     !Number of profiles requested to process
    INTEGER                               :: MonitorIterat      !Flag 2 print (in a file) data 4 iterat. monitoring 
    INTEGER                               :: GeogrLimits        !Which geographic limits flag
    REAL                                  :: minLat             !Minimum latitude of the lat/lon box
    REAL                                  :: maxLat             !Maximum latitude of the lat/lon box
    REAL                                  :: minLon             !Minimum longitude of the lat/lon box
    REAL                                  :: maxLon             !Maximum longitude of the lat/lon box
    INTEGER                               :: Pass               !Flag about which orbits to process (asc/des/both)
    !---CRTM-needed files
    CHARACTER(LEN=nLEN)                   :: Coeff_Path         !CRTM-based Coeff files path 
    !---LogFile 
    CHARACTER(LEN=nLEN)                   :: LogFile            !Output Log file
    INTEGER                               :: LEN                !Maximum dimension of the filenames (strings)
    INTEGER                               :: mxEDR              !Maximum number of EDRs used to declare vectors
    !--Ext BkgAtm File
    CHARACTER(LEN=nLEN)                   :: BkgAtmFile_Ext     !External atmospheric backgroud file
    INTEGER                               :: useBkgExt          !Flag to use the external atmospheric backgroud file
  END TYPE CntrlConfig_type

  !------This structure relates to the forward model code-------------
  TYPE :: CntrlConfig_fwd_type
    CHARACTER(LEN=nLEN)                   :: GeophInputFile     !geophysical data file
    INTEGER                               :: SensorID           !Sensor ID
    CHARACTER(LEN=nLEN)                   :: Coeff_Path         !CRTM-based Coeff files path 
    CHARACTER(LEN=nLEN)                   :: InstrConfigFile    !Instrument configuration file
    CHARACTER(LEN=nLEN)                   :: OutputFile         !Output file containing the simulations
    CHARACTER(LEN=nLEN)                   :: NoiseFile          !Noise data file (NEDT)
    CHARACTER(LEN=nLEN)                   :: LogFile            !Log file containing the warning/errors
    INTEGER                               :: nprofs2process     !Number of profiles requested to process
    INTEGER,             DIMENSION(mxCh)  :: ChanSel            !Selected Channels index vector 
    INTEGER                               :: nChan              !Number of channels
    INTEGER                               :: LEN                !Maximum dimension of the file names (strings)
    INTEGER                               :: mxChan             !Maximum number of channels used for declaration
    INTEGER                               :: iAddDeviceNoise    !Flag to add/not noise to the simulated TBs
    INTEGER                               :: iCldOffOrOn        !Flag to set hydrometeor profiles to 0 for simulation
    INTEGER                               :: iPrintMonitor      !Flag to print on screen a summary of the exec
  END TYPE CntrlConfig_fwd_type
  
  TYPE(CntrlConfig_type)     :: CntrlConfig
  TYPE(CntrlConfig_fwd_type) :: CntrlConfig_fwd
  
  SAVE CntrlConfig
  SAVE CntrlConfig_fwd
 
CONTAINS


!===============================================================
! Name:         LoadCntrlParams
!
!
! Type:         Subroutine
!
!
! Description:  Load the control parameters used by the 1dvar code
!               These are stored in the CntrlConfig structure and made
!               available in the PUBLIC section of this module.
!
! Arguments: None
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

  SUBROUTINE LoadCntrlParams()
    NAMELIST /ContrlP/CntrlConfig
    READ(*,NML=ContrlP)
    CntrlConfig%LEN   = nLEN
    CntrlConfig%mxEDR = mxEDR
  END SUBROUTINE LoadCntrlParams


!===============================================================
! Name:        LoadCntrlParams_Fwd
!
!
! Type:        Subroutine
!
!
! Description:  Load the control parameters used by the fwd model code
!               These are stored in the CntrlConfig_fwd structure and 
!               made available in the PUBLIC section of this module.
!
!
! Arguments: None
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

  SUBROUTINE LoadCntrlParams_Fwd()
    INTEGER :: nchan,ichan
    NAMELIST /ContrlP_fwd/CntrlConfig_fwd
    CntrlConfig_fwd%ChanSel = DEFAULT_VALUE_INT
    READ(*,NML=ContrlP_fwd)
    CntrlConfig_fwd%LEN     = nLEN
    CntrlConfig_fwd%mxChan  = mxCh
    !---Determination # of channels (from the channels selection vector)
    nChan =0
    ChanLoop: Do ichan=1,mxCh
       IF (CntrlConfig_fwd%ChanSel(ichan).eq.DEFAULT_VALUE_INT) CYCLE  ChanLoop
       nChan=nChan+1          
    ENDDO ChanLoop
    CntrlConfig_fwd%nChan = nChan
  END SUBROUTINE LoadCntrlParams_Fwd

END MODULE CntrlParams
