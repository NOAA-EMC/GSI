!$Id: FwdOperator.f90 3339 2013-10-09 17:23:35Z amims $
!-----------------------------------------------------------------------------------------------
! Name:         FwdOperator
! 
! Type:         F90 module
!
! Description:
!       Module that contains all subroutines needed for the interface with 
!       the forward operator.
!
! Modules needed:
!       - Consts
!       - utils
!       - misc
!       - ErrorHandling
!       - IO_Scene
!       - IO_MeasurData
!       - CRTM_Module
!       - CRTM_FastemX
!       - MWwaterCoeff_Define
!
! Public Subroutines contained:
!       - CRTM_Init
!       - InterfaceCRTM
!       - FwdOper
!       - OptionlAdjustK
!       - ComputeEmiss
!       - GetSensorInfo
!
! Private Subroutines contained:
!       - DetermnCldLays
!       - FillCRTMSfcStrc
!
! Data type included:
!       - none
!
! History:
!      2006         S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!      03-14-2013   Kevin Garrett, RTi @ NOAA/NESDIS/STAR
!                   -Interface with CRTM 2.1.1
!
!-----------------------------------------------------------------------------------------------

MODULE FwdOperator
  USE Consts
  USE utils
  USE misc
  USE ErrorHandling
  USE IO_Scene
  USE IO_MeasurData
  ! -- CRTM module
  USE CRTM_Module
  USE CRTM_FastemX     ,   ONLY: Compute_FastemX,iVar_type
  USE MWwaterCoeff_Define, ONLY: MWwaterCoeff_type

  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: Init_CRTM,InterfaceCRTM,FwdOper,ComputeEmiss,GetSensorInfo
  !---INTRINSIC functions used in this module
  INTRINSIC :: COUNT,PACK,SIZE,SUM,EXP,REAL,ALOG,TRIM,TRANSPOSE,ADJUSTL

CONTAINS



!=============================================================================
! Name:		Init_CRTM
!
!
! Type:		F90 Subroutine
!
!
! Description:  Initializes CRTM structures
!      
! Arguments:
!
!      Name		  Type (I/O)	 Description
!      ------------------------------------------------
!	- SensorID           I           Array of Sensor IDs
!       - Coeff_Path         I           Path to spectral coefficient files
!       - MAXLAYS            I           Maximum number of atmospheric layers
!       - MAXCLOUDS          I           Maximum number of cloud layers
!       - MAXAEROSOL         I           Maximum number of aerosol layers
!       - nAbsorb            I           Number of atmospheric absorbers
!       - ChannelInfo        O           CRTM ChannelInfo Structure
!       - Atmos              O           CRTM Atmosphere Structure
!       - Atmos_K            O           CRTM Atmosphere Structure for k-matrix
!       - Sfc                O           CRTM Surface Structure
!       - Sfc_K              O           CRTM Surface Structure for k-matrix
!       - Options            O           CRTM Structure for optional input
!       - RTSolution         O           CRTM Structure for fwd calculations
!       - RTSolution_K       O           CRTM Structure for k-matrix
!
!
! Modules needed:
!  CRTM_Module
!
!
! History:
!       01-16-2013    Kevin Garrett, RTi @ NOAA/NESDIS/STAR
!
!==============================================================================
  SUBROUTINE Init_CRTM(Sensor_ID,Coeff_Path,ChannelInfo,Atmos, &
       Atmos_K,Sfc,Sfc_K,Options,RTSolution,RTSolution_K)

    !---Input/Output Variables
    CHARACTER(LEN=*),            DIMENSION(:)                :: Sensor_ID
    CHARACTER(LEN=*)                                         :: Coeff_Path
    !---CRTM Data Types
    TYPE(CRTM_ChannelInfo_type), DIMENSION(:),   POINTER     :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:),   ALLOCATABLE :: Atmos
    TYPE(CRTM_Surface_type),     DIMENSION(:),   ALLOCATABLE :: Sfc
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:,:), ALLOCATABLE :: Atmos_K
    TYPE(CRTM_Surface_type),     DIMENSION(:,:), ALLOCATABLE :: Sfc_K
    TYPE(CRTM_Options_type),     DIMENSION(:),   ALLOCATABLE :: Options
    TYPE(CRTM_RTSolution_type),  DIMENSION(:,:), ALLOCATABLE :: RTSolution
    TYPE(CRTM_RTSolution_type),  DIMENSION(:,:), ALLOCATABLE :: RTSolution_K
    !---Local variables
    INTEGER,                     PARAMETER                   :: nProfiles=1
    INTEGER                                                  :: nchan
    INTEGER                                                  :: Error_Status,Allocate_Status
    INTEGER                                                  :: ichan
    !---Initialize CRTM Model
    ALLOCATE(ChannelInfo(SIZE(Sensor_ID)))
    Error_Status = CRTM_Init(Sensor_ID,ChannelInfo,File_Path=Coeff_Path)
    IF (Error_Status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Init Failed')
    nchan=SUM(ChannelInfo%n_channels)
    !---Allocate and Initialize Atmosphere, Surface and Options Structures
    !ALLOCATE(Atmos(nProfiles),Sfc(nProfiles),Options(nProfiles),Atmos_K(nchan,nProfiles), &
    !Sfc_K(nchan,nProfiles),STAT=allocate_status)
    !ALLOCATE(Atmos_K(nchan,nProfiles),Sfc_K(nchan,nProfiles),STAT=allocate_status)
    IF (allocate_status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Structures Allocation')
    CALL CRTM_Atmosphere_Create(Atmos,MAXLAYS,MAXABSORB,MAXCLOUDS,MAXAEROSOL)
    !CALL CRTM_Aerosol_Create(Aerosol(1),MAXLAYS)
    CALL CRTM_Surface_Create(Sfc,nchan)
    CALL CRTM_Options_Create(Options,nchan)
    DO ichan=1,nchan
       CALL CRTM_Atmosphere_Create(Atmos_K(ichan,1),MAXLAYS,MAXABSORB,MAXCLOUDS,MAXAEROSOL)
       CALL CRTM_Surface_Create(Sfc_K(ichan,1),nchan)
    ENDDO
    !---Allocate and Initialize RTSolution,RTSolution_K structures
    ALLOCATE(RTSolution(nchan,nProfiles),RTSolution_K(nchan,nProfiles),STAT=allocate_status)
    IF (allocate_status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'RT Allocation')
    CALL CRTM_RTSolution_Create(RTSolution,MAXLAYS)
    CALL CRTM_RTSolution_Create(RTSolution_K,MAXLAYS)
    RETURN
  END SUBROUTINE Init_CRTM

!=============================================================================
! Name:		InterfaceCRTM
!
!
! Type:		F90 Subroutine
!
!
! Description:  Initializes CRTM structures and fills with MIRS scene data.
!               Calls CRTM_Forward and returns the computed TBs. 
!      
! Arguments:
!
!      Name	     Type (I/O)   Description
!      ----------------------------------------------------------------------
!	- Scene         I         Scene structure containing profile and surface data
!	- ChannelInfo   I         CRTM structure containing the channels to simulate
!       - Atmos         I         CRTM structure for atmosphere
!       - Sfc           I         CRTM structure for surface
!       - Options       I         CRTM structure for optional input
!       - RTSolution    I         CRTM structure for RT simulation
!	- TB            O         Array of TBs to return
!
!
! Modules needed:
!  IO_Scene
!  Consts
!  CRTM_Module
!
!
! History:
!       01-16-2013    Kevin Garrett, RTi @ NOAA/NESDIS/STAR
!
!==============================================================================
  SUBROUTINE InterfaceCRTM(Scene,ChannelInfo,Atmos,Sfc,Options,RTSolution,TB)
    !---I/O Variables
    !-MIRS Data Types
    TYPE(Scene_type)                                   :: Scene
    !-CRTM Data Types
    TYPE(CRTM_ChannelInfo_type), DIMENSION(:)          :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:)          :: Atmos
    TYPE(CRTM_Surface_type),     DIMENSION(:)          :: Sfc 
    TYPE(CRTM_Options_type),     DIMENSION(:)          :: Options
    TYPE(CRTM_RTSolution_type),  DIMENSION(:,:)        :: RTSolution
    REAL,                        DIMENSION(:)          :: TB
    !---Local Variables/Parameters
    INTEGER                                     :: nchan,nCldLays
    INTEGER                                     :: Error_Status
    REAL,    DIMENSION(:), ALLOCATABLE          :: CLW,RAIN,SNOW,GRPL,ICE
    INTEGER, DIMENSION(MAXCLOUDS)               :: CldTypIdx
    !---Indexes
    INTEGER, DIMENSION(:), ALLOCATABLE          :: idx_LayEff,idx_LevEff,idx_cld
    INTEGER                                     :: i,iLay,nLayEff,nLevEff
    !---CRTM Related Variables
    TYPE(CRTM_Geometry_type)                    :: GeometryInfo(1)
    !---Varying cloud properties variables
    REAL                                        :: Re, rn,rn_2

    nchan = SUM(ChannelInfo%n_channels)
    !---Zero-out CRTM structures
    CALL CRTM_Atmosphere_Zero(Atmos)
    CALL CRTM_Surface_Zero(Sfc)
    !---Adjust scene p,t,w layers,nLay,nLev for valid layers only
    nLayEff  = COUNT(Scene%Temp_lay .gt. 0. .and. Scene%Pres_lev(1:Scene%nLay) .le. Scene%SfcPress)
    IF (nLayEff .le. 2) THEN
       Scene%qc(1) = 1
       RETURN
    ELSE
       nLevEff  = nLayEff + 1
       ALLOCATE(idx_LayEff(nLayEff),idx_LevEff(nLevEff))
       idx_LayEff = PACK( (/(i,i=1,SIZE(Scene%Temp_lay))/), & 
            (Scene%Temp_lay .gt. 0. .and. Scene%Pres_lev(1:Scene%nLay) .le. Scene%SfcPress))
       idx_LevEff(1:nLayEff) = idx_LayEff(1:nLayEff)
       idx_LevEff(nLevEff)   = idx_LayEff(nLayEff) + 1  
       DO iLay=1,Scene%nLay-1
          IF (Scene%Absorb_lay(iLay,Scene%ih2o) .lt. 0) Scene%Absorb_lay(iLay,Scene%ih2o) = 0.
       ENDDO
    ENDIF

    !----------------------------------------------------------------------------
    !---Fill CRTM_State structures to pass to forward model
    !----------------------------------------------------------------------------
    CALL CRTM_Atmosphere_SetLayers(Atmos,nLayEff)
    Atmos(1)%n_Layers                   = nLayEff
    Atmos(1)%n_Absorbers                = 2
    Atmos(1)%level_pressure(0:nLayEff)  = Scene%pres_lev(idx_LevEff(1:nLevEff))
    Atmos(1)%level_pressure(nLayEff)    = Scene%SfcPress
    Atmos(1)%pressure(1:nLayEff)        = Scene%Pres_lay(idx_LayEff(1:nLayEff))
    Atmos(1)%temperature(1:nLayEff)     = Scene%Temp_lay(idx_LayEff(1:nLayEff))
    Atmos(1)%absorber(1:nLayEff,1)      = Scene%Absorb_lay((idx_LayEff(1:nLayEff)),Scene%ih2o)
    IF (Scene%iO3 .gt. 0) THEN
       Atmos(1)%absorber(1:nLayEff,2)   = Scene%Absorb_lay((idx_LayEff(1:nLayEff)),Scene%iO3)
    ELSE
       Atmos(1)%absorber(1:nLayEff,2)   = 0.
    ENDIF
    !---Set absorber indices and units
    Atmos(1)%Absorber_ID(1)             = H2O_ID
    Atmos(1)%Absorber_ID(2)             = O3_ID
    Atmos(1)%Absorber_Units(1)          = MASS_MIXING_RATIO_UNITS   !(g/kg)
    Atmos(1)%Absorber_Units(2)          = VOLUME_MIXING_RATIO_UNITS !(ppmv)
    Atmos(1)%n_Aerosols                 = 0
    !---Determine Clouds; fill Cloud structures if simulation, zero it out if analytic emissivity
    IF (Atmos(1)%n_Clouds .eq. -999) THEN
      Atmos(1)%n_Clouds = 0
    ELSE
      CALL DetermnCldLays(Scene,Atmos,nLayEff,CldTypIdx)
    ENDIF
    !---Fill Surface and Options Structure
    CALL FillCRTMSfcStrc(Scene,Sfc)
    Sfc(1)%SensorData%Is_Allocated = .FALSE.

    !---Set Options
    Options(1)%n_Channels                 = nchan 
    Options(1)%Emissivity(1:nchan)        = Scene%Emiss
    Options(1)%Direct_Reflectivity        = 1.-Scene%Emiss
    Options(1)%Use_Emissivity             = .TRUE.     !-TRUE: set emissivity from external source
    Options(1)%Use_Direct_Reflectivity    = .TRUE.     !-TRUE: set reflectivity from external source
    !---Fill GeometryInfo Structure
    GeometryInfo%Longitude            = Scene%lon
    IF (Scene%lon .lt. 0) GeometryInfo%Longitude = abs(Scene%lon)+180
    GeometryInfo%Latitude             = Scene%lat
    GeometryInfo%Surface_Altitude     = 0.
    GeometryInfo%Year                 = Scene%scanYear
    !---Calculate day and month
    CALL day_month(Scene%scanYear,GeometryInfo(1)%Month,GeometryInfo(1)%Day,Scene%scanDay)
    GeometryInfo%iFOV                 = Scene%iScanPos
    GeometryInfo%Sensor_Zenith_Angle  = Scene%Angle
    !GeometryInfo%Sensor_Azimuth_Angle = 0
    !GeometryInfo%Source_Zenith_Angle  = 0.
    !GeometryInfo%Source_Azimuth_Angle = 0.
    !----------------------------------------------------------------------------
    !---Call CRTM Forward
    !----------------------------------------------------------------------------
    RTSolution%n_Layers = nLayEff
    Error_Status = CRTM_Forward(Atmos,Sfc,GeometryInfo,ChannelInfo,RTSolution, &
         Options=Options)
    IF (Error_Status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Forward Error')
    TB(1:nchan) = RTSolution(:,1)%Brightness_Temperature
    !---Deallocate arrays
    DEALLOCATE(idx_LayEff,idx_LevEff)
    CALL CRTM_Geometry_Destroy(GeometryInfo)

    RETURN 
  END SUBROUTINE InterfaceCRTM


!===============================================================
! Name:         FwdOper
!
!
! Type:         Subroutine
!
!
! Description:  Interfaces MIRS to CRTM. Performs the call to CRTM
!               as well as gets back its radiances/brightness 
!               temperatures and Jacobians which get adjusted 
!               according to how they are treated (natural space, 
!               logarithm space, etc).
!
!
! Arguments:
!
!        Name                      Type             Description
!      ---------------------------------------------------
!       - Scene                      I              Scene structure
!       - Y                          O              Simulated radiances
!       - K                          O              Jacobians (derivatives)
!       - RTmodel                    I              CRTM structure with geometry, etc
!       - ChannelInfo                I              CRTM structure with chennel info
!       - ParamLabel                 I              Geoph. Parameters labels vector
!       - ParamIndx                  I              Geoph. Parameters index vector
!       - ParamLength                I              Geoph. Parameters lengths vector
!       - iSpaceModeFlag             I              Geoph. Parameters mode vector (natural space, logarith, etc)
!       - nLayEff                    O              Effective number of layers
!       - dnWelling_rad              O              Downwelling radiances
!       - optDepNadir_lay            O              Nadir optical depths vector
!       - topSensPressT              I              Pressure above which no sensitivity to temp. exists
!       - topSensPressWV             I              Pressure above which no sensitivity to humid. exists
!
!
! Modules needed:
!       - DeterminNlayEff
!       - DeterminLayIndxOnTopPress
!       - OptionlAdjustK
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================
  SUBROUTINE FwdOper(Scene,Y,K,ChannelInfo,Atmos,Atmos_K,Sfc,Sfc_K,Options,RTSolution,     &
       RTSolution_K,ParamLabel,ParamIndx,ParamLength,iSpaceModeFlag,nLayEff,dnWelling_rad, &
       upWelling_rad,opt_lay,altitude,topSensPressT,topSensPressWV,logFile)

    !---Input/Output variables
    TYPE(Scene_type)                                     :: Scene
    CHARACTER(LEN=*), DIMENSION(:)                       :: ParamLabel
    INTEGER,          DIMENSION(:)                       :: ParamIndx,ParamLength
    INTEGER,          DIMENSION(:)                       :: iSpaceModeFlag
    INTEGER,                     INTENT(OUT)             :: nLayEff
    REAL                                                 :: topSensPressT,topSensPressWV,altitude
    CHARACTER(LEN=*)                                     :: logFile
    REAL,    DIMENSION(:),       INTENT(OUT)             :: dnWelling_rad,upWelling_rad
    REAL,    DIMENSION(:,:),     INTENT(OUT)             :: opt_lay
    REAL,    DIMENSION(:),       INTENT(OUT)             :: Y
    REAL,    DIMENSION(:,:),     INTENT(OUT)             :: K
    TYPE(CRTM_ChannelInfo_type), DIMENSION(:)            :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:)            :: Atmos
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:,:)          :: Atmos_K
    TYPE(CRTM_Surface_type),     DIMENSION(:)            :: Sfc
    TYPE(CRTM_Surface_type),     DIMENSION(:,:)          :: Sfc_K
    TYPE(CRTM_Options_type),     DIMENSION(:)            :: Options
    TYPE(CRTM_RTSolution_type),  DIMENSION(:,:)          :: RTSolution,RTSolution_K
    !---Local variables
    INTEGER                                          :: nchan,nG
    INTEGER                                          :: Error_Status,Allocate_Status
    INTEGER                                          :: ichan,iMode
    INTEGER                                          :: nLev,nLay,iEDR,nEDRs,iG
    INTEGER                                          :: iLayTopT,iLayTopWV
    INTEGER, DIMENSION(MAXCLOUDS)                    :: CldTypIdx
    REAL,    DIMENSION(SIZE(Y))                      :: SfcP_K
    REAL                                             :: SfcP
    REAL,    DIMENSION(SIZE(Scene%pres_lay))         :: Temp,WV,O3,CLW,RAIN,SNOW,GRPL,ICE
    REAL,    DIMENSION(SIZE(Scene%pres_lay),SIZE(Y)) :: Temp_K,WV_K,O3_K
    REAL,    DIMENSION(SIZE(Scene%pres_lay),SIZE(Y)) :: CLW_K,RAIN_K,SNOW_K,GRPL_K,ICE_K
    TYPE(CRTM_Geometry_type)                         :: GeometryInfo(1)
 
    !---Shorten variables
    nchan = SIZE(Y)
    nLev  = SIZE(Scene%Pres_lev)
    nLay  = SIZE(Scene%Pres_lay)
    nEDRs = SIZE(Paramindx)
    nLayEff = COUNT(Scene%Temp_lay .gt. 0 .and. Scene%Pres_lev(1:nLay) .le. Scene%SfcPress)
    !---Set previously allocated arrays to zero
    CALL CRTM_Atmosphere_Zero(Atmos)
    CALL CRTM_Atmosphere_Zero(Atmos_K)
    CALL CRTM_Surface_Zero(Sfc)
    CALL CRTM_Surface_Zero(Sfc_K)

    !---Determine the effective number of layers according to SfcPress
    !CALL DeterminNlayEff(Scene%nLay,Scene%pres_lay(1:Scene%nLay),Scene%SfcPress,nLayEff)
    CALL DeterminLayIndxOnTopPress(Scene%nLay,Scene%pres_lay(1:Scene%nLay),topSensPressT,iLayTopT)
    CALL DeterminLayIndxOnTopPress(Scene%nLay,Scene%pres_lay(1:Scene%nLay),topSensPressWV,iLayTopWV)
    !-------------------------------------------------------------------------------
    !---Define Atmosphere Parameters
    !-------------------------------------------------------------------------------
    CALL CRTM_Atmosphere_SetLayers(Atmos,nLayEff)
    CALL CRTM_Atmosphere_SetLayers(Atmos_K,nLayEff)
    Atmos(1)%n_Layers                  = nLayEff
    Atmos(1)%Level_Pressure(0:nLayEff) = Scene%Pres_lev(1:nLayEff+1)
    Atmos(1)%Level_Pressure(nLayEff)   = Scene%SfcPress
    Atmos(1)%Pressure(1:nLayEff)       = Scene%Pres_lay(1:nLayEff)
    Atmos(1)%Pressure(nLayeff)         = (Scene%Pres_lev(nLayEff)+Scene%SfcPress)/2
    Atmos(1)%Temperature(1:nLayEff)    = Scene%Temp_lay(1:nLayEff)
    Atmos(1)%Absorber(1:nLayEff,1)     = Scene%Absorb_lay(1:nLayEff,Scene%iH2O)
    Atmos(1)%Absorber(1:nLayEff,2)     = Scene%Absorb_lay(1:nLayEff,Scene%iO3)
    Atmos(1)%Absorber_ID(1)            = H2O_ID
    Atmos(1)%Absorber_ID(2)            = O3_ID
    Atmos(1)%Absorber_Units(1)         = MASS_MIXING_RATIO_UNITS 
    Atmos(1)%Absorber_Units(2)         = VOLUME_MIXING_RATIO_UNITS 
    Atmos(1)%n_Aerosols                = 0
    !---Set RT_Solution_K%Brightness_Temperature to 1 and Radiance to 0
    DO ichan=1,nchan
       RTSolution_K(ichan,1)%Brightness_Temperature = 1
       RTSolution_K(ichan,1)%Radiance               = 0
       RTSolution_K(ichan,1)%n_Layers               = nLayEff
    ENDDO
    RTSolution%n_Layers=nLayEff
    !---Fill Atmosphere%Cloud Structure
    CldTypIdx=0

    CALL DetermnCldLays(Scene,Atmos,nLayEff,CldTypIdx)
    !---Fille Surface Structure
    CALL FillCRTMSfcStrc(Scene,Sfc)
    Sfc(1)%SensorData%Is_Allocated = .FALSE.
    !---Set Options
    Options(1)%n_Channels                   = nChan
    Options(1)%Emissivity(1:nchan)          = Scene%Emiss
    Options(1)%Direct_Reflectivity(1:nchan) = 0.05
    Options(1)%Use_Emissivity               = .TRUE.
    Options(1)%Use_Direct_Reflectivity      = .TRUE.
    Options(1)%Include_Scattering           = .FALSE.

    IF (CldTypIdx(2) .gt. 0 .or. CldTypIdx(3) .gt. 0) &
         Options(1)%Include_Scattering = .TRUE.
    Options(1)%Use_n_Streams = .TRUE.
    Options(1)%n_Streams=2

    !---Fill GeometryInfo Structure
    GeometryInfo%Longitude            = Scene%lon
    IF (Scene%lon .lt. 0) GeometryInfo%Longitude = abs(Scene%lon)+180
    GeometryInfo%Latitude             = Scene%lat
    GeometryInfo%Surface_Altitude     = altitude
    GeometryInfo%Year                 = Scene%scanYear
    GeometryInfo%iFOV                 = Scene%iScanPos
    GeometryInfo%Sensor_Zenith_Angle  = Scene%Angle
    GeometryInfo%Sensor_Azimuth_Angle = Scene%RelAziAngle
    GeometryInfo%Source_Zenith_Angle  = 0.
    GeometryInfo%Source_Azimuth_Angle = 0.
    !-----------------------------------
    !---Compute RTSolution and K-matrix
    !-----------------------------------
    Error_Status = CRTM_K_Matrix( &
         Atmos         , &  ! FWD Input
         Sfc           , &  ! FWD Input
         RTSolution_K  , &  ! K   Input
         GeometryInfo  , &  ! Input
         ChannelInfo   , &  ! Input
         Atmos_K       , &  ! K   Output
         Sfc_K         , &  ! K   Output
         RTSolution    , &  ! FWD Output
         Options = Options ) 
    IF (Error_Status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM K-matrix Failed')
    Temp = Atmos(1)%temperature(:)
    WV   = Atmos(1)%absorber(:,1)
    O3   = Atmos(1)%absorber(:,2)
    SfcP = Atmos(1)%Level_Pressure(nLayEff)
    upWelling_rad(1:nchan)=RTSolution(1:nchan,1)%Up_Radiance
    dnWelling_rad(1:nchan)=RTSolution(1:nchan,1)%Down_Radiance
    IF (CldTypIdx(1) .gt. 0) CLW(:)  = Atmos(1)%Cloud(CldTypIdx(1))%Water_Content
    IF (CldTypIdx(2) .gt. 0) RAIN(:) = Atmos(1)%Cloud(CldTypIdx(2))%Water_Content
    IF (CldTypIdx(3) .gt. 0) GRPL(:) = Atmos(1)%Cloud(CldTypIdx(3))%Water_Content
    IF (CldTypIdx(4) .gt. 0) SNOW(:) = Atmos(1)%Cloud(CldTypIdx(4))%Water_Content
    IF (CldTypIdx(5) .gt. 0) ICE(:)  = Atmos(1)%Cloud(CldTypIdx(5))%Water_Content
    !---Transfer jacobians to arrays for processing
    DO iChan=1,nchan
       opt_lay(:,iChan)= RTSolution(iChan,1)%Layer_Optical_Depth
       Temp_K(:,iChan) = Atmos_K(iChan,1)%temperature
       WV_K(:,iChan)   = Atmos_K(iChan,1)%absorber(:,1)
       O3_K(:,iChan)   = Atmos_K(iChan,1)%absorber(:,2)
       SfcP_K(iChan)   = Atmos_K(iChan,1)%Level_Pressure(nLayeff)
       IF (CldTypIdx(1) .gt. 0) CLW_K(:,iChan)  = Atmos_K(iChan,1)%Cloud(CldTypIdx(1))%Water_Content
       IF (CldTypIdx(2) .gt. 0) RAIN_K(:,iChan) = Atmos_K(iChan,1)%Cloud(CldTypIdx(2))%Water_Content
       IF (CldTypIdx(3) .gt. 0) GRPL_K(:,iChan) = Atmos_K(iChan,1)%Cloud(CldTypIdx(3))%Water_Content
       IF (CldTypIdx(4) .gt. 0) SNOW_K(:,iChan) = Atmos_K(iChan,1)%Cloud(CldTypIdx(4))%Water_Content
       IF (CldTypIdx(5) .gt. 0) ICE_K(:,iChan)  = Atmos_K(iChan,1)%Cloud(CldTypIdx(5))%Water_Content
    ENDDO

    !---Put the CRTM-produced jacobians into K
    K = 0.
    DO iEDR=1,nEDRs
       iG    = ParamIndx(iEDR)
       nG    = ParamLength(iEDR)
       iMode = iSpaceModeFlag(iEDR)
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'TEMP')  THEN
          CALL OptionlAdjustK(iMode,Temp_K,nG,nchan,Temp)
          K(1:nchan,iG:iG+nG-1)     = transpose(Temp_K(1:nG,1:nchan))
          !---Disable Jacobians below surface and above topSensPressT
          K(1:nchan,iG+nLayeff:iG+nG-1) = 0.
          K(1:nchan,iG:iG+iLayTopT-1)   = 0.
       ENDIF
       !print *,'K Temp', K(1,iG:nG)
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'WVAP')  THEN
          CALL OptionlAdjustK(iMode,WV_K,nG,nchan,WV)
          K(1:nchan,iG:iG+nG-1)     = transpose(WV_K(1:nG,1:nchan))
          !---Disable Jacobians below surface and above topSensPressWV
          K(1:nchan,iG+nLayeff:iG+nG-1) = 0.
          K(1:nchan,iG:iG+iLayTopWV-1)  = 0.
       ENDIF
       !print *,'K WV',K(1,iG:nG)
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'OZON')  THEN
          CALL OptionlAdjustK(iMode,O3_K,nG,nchan,O3)
          K(1:nchan,iG:iG+nG-1)     = transpose(O3_K(1:nG,1:nchan))
          !---Disable Jacobians below surface
          K(1:nchan,iG+nLayeff:iG+nG-1)=0.
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'CLW')   THEN
          CALL OptionlAdjustK(iMode,CLW_K,nG,nchan,CLW)
          K(1:nchan,iG:iG+nG-1)     = transpose(CLW_K(1:nG,1:nchan))
          !---Disable Jacobians below surface
          K(1:nchan,iG+nLayeff:iG+nG-1)=0.
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'RAIN')  THEN
          CALL OptionlAdjustK(iMode,RAIN_K,nG,nchan,RAIN)
          K(1:nchan,iG:iG+nG-1)     = transpose(RAIN_K(1:nG,1:nchan))
          !---Disable Jacobians below surface
          K(1:nchan,iG+nLayeff:iG+nG-1)=0.
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'SNOW')  THEN
          CALL OptionlAdjustK(iMode,SNOW_K,nG,nchan,SNOW)
          K(1:nchan,iG:iG+nG-1)     = transpose(SNOW_K(1:nG,1:nchan))
          !---Disable Jacobians below surface
          K(1:nchan,iG+nLayeff:iG+nG-1)=0.
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'GRPL')  THEN
          CALL OptionlAdjustK(iMode,GRPL_K,nG,nchan,GRPL)
          K(1:nchan,iG:iG+nG-1)     = transpose(GRPL_K(1:nG,1:nchan))
          !---Disable Jacobians below surface
          K(1:nchan,iG+nLayeff:iG+nG-1)=0.
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'ICE')   THEN
          CALL OptionlAdjustK(iMode,ICE_K,nG,nchan,ICE)
          K(1:nchan,iG:iG+nG-1)     = transpose(ICE_K(1:nG,1:nchan))
          !---Disable Jacobians below surface
          K(1:nchan,iG+nLayeff:iG+nG-1)=0.
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'EMIS')  THEN
          IF (nG .ne. nchan) CALL ErrHandl(ErrorType,Err_InconsNchanAndNGemis,'') 
          IF (iMode .eq. 1) THEN
             RTSolution_K(:,1)%Surface_Emissivity = &
                  RTSolution_K(:,1)%Surface_Emissivity*RTSolution(:,1)%Surface_Emissivity  
             
          ENDIF
          IF (iMode .eq. 2) THEN
             RTSolution_K(:,1)%Surface_Emissivity = &
                  RTSolution_K(:,1)%Surface_Emissivity*RTSolution(:,1)%Surface_Emissivity* &
                  alog(real(RTSolution(:,1)%Surface_Emissivity))
          ENDIF
          DO ichan=1,nchan
             K(ichan,iG+ichan-1) =  RTSolution_K(ichan,1)%Surface_Emissivity
             !print *,'Em K',K(ichan,iG+ichan-1)
          ENDDO
         
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'WINDSP')THEN
          IF (nG .ne. 1) CALL ErrHandl(ErrorType,Err_InconsNGne1_ws,'')
          IF (iMode .eq. 1) THEN
             Sfc_K(1:nchan,1)%Wind_Speed=Sfc_K(1:nchan,1)%Wind_Speed*Sfc%Wind_Speed
          ENDIF
          IF (iMode .eq. 2) THEN
             Sfc_K(1:nchan,1)%Wind_Speed=Sfc_K(1:nchan,1)%Wind_Speed*Sfc%Wind_Speed* &
                  alog(real(Sfc%Wind_Speed))
          ENDIF
          K(1:nchan,iG) = Sfc_K(1:nchan,1)%Wind_Speed
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'TSKIN') THEN
          IF (nG .ne. 1) CALL ErrHandl(ErrorType,Err_InconsNGne1_tsk,'') 
          IF (Scene%iTypSfc .eq. OC_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Water_Temperature = Sfc_K(1:nchan,1)%Water_Temperature* &
                     Sfc%Water_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Water_Temperature = Sfc_K(1:nchan,1)%Water_Temperature* &
                     Sfc%Water_Temperature*alog(real(Sfc%Water_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Water_Temperature
          ENDIF
          IF (Scene%iTypSfc .eq. SEAICE_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Ice_Temperature = Sfc_K(1:nchan,1)%Ice_Temperature* &
                     Sfc%Ice_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Ice_Temperature = Sfc_K(1:nchan,1)%Ice_Temperature* &
                     Sfc%Ice_Temperature*alog(real(Sfc%Ice_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Ice_Temperature
          ENDIF
          IF (Scene%iTypSfc .eq. LD_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Land_Temperature = Sfc_K(1:nchan,1)%Land_Temperature* &
                     Sfc%Land_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Land_Temperature = Sfc_K(1:nchan,1)%Land_Temperature* &
                     Sfc%Land_Temperature*alog(real(Sfc%Land_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Land_Temperature
          ENDIF
          IF (Scene%iTypSfc .eq. SNOW_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Snow_Temperature = Sfc_K(1:nchan,1)%Snow_Temperature* &
                     Sfc%Snow_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Snow_Temperature = Sfc_K(1:nchan,1)%Snow_Temperature* &
                     Sfc%Snow_Temperature*alog(real(Sfc%Snow_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Snow_Temperature
          ENDIF
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'DELTAT') THEN
          IF (nG .ne. 1) CALL ErrHandl(ErrorType,Err_InconsNGne1_deltaT,'') 
          IF (Scene%iTypSfc .eq. OC_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Water_Temperature = Sfc_K(1:nchan,1)%Water_Temperature* &
                     Sfc%Water_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Water_Temperature = Sfc_K(1:nchan,1)%Water_Temperature* &
                     Sfc%Water_Temperature*alog(real(Sfc%Water_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Water_Temperature
          ENDIF
          IF (Scene%iTypSfc .eq. SEAICE_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Ice_Temperature = Sfc_K(1:nchan,1)%Ice_Temperature* &
                     Sfc%Ice_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Ice_Temperature = Sfc_K(1:nchan,1)%Ice_Temperature* &
                     Sfc%Ice_Temperature*alog(real(Sfc%Ice_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Ice_Temperature
          ENDIF
          IF (Scene%iTypSfc .eq. LD_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Land_Temperature = Sfc_K(1:nchan,1)%Land_Temperature* &
                     Sfc%Land_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Land_Temperature = Sfc_K(1:nchan,1)%Land_Temperature* &
                     Sfc%Land_Temperature*alog(real(Sfc%Land_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Land_Temperature
          ENDIF
          IF (Scene%iTypSfc .eq. SNOW_TYP) THEN
             IF (iMode .eq. 1) THEN
                Sfc_K(1:nchan,1)%Snow_Temperature = Sfc_K(1:nchan,1)%Snow_Temperature* &
                     Sfc%Snow_Temperature
             ENDIF
             IF (iMode .eq. 2) THEN
                Sfc_K(1:nchan,1)%Snow_Temperature = Sfc_K(1:nchan,1)%Snow_Temperature* &
                     Sfc%Snow_Temperature*alog(real(Sfc%Snow_Temperature))
             ENDIF
             K(1:nchan,iG) = Sfc_K(1:nchan,1)%Snow_Temperature
          ENDIF
       ENDIF
       IF(adjustl(trim(ParamLabel(iEDR))).eq.'SFCP') THEN
          IF (nG .ne. 1) CALL ErrHandl(ErrorType,Err_InconsNGne1_sfcP,'')  
          IF (iMode .eq. 1) THEN
             SfcP_K(1:nchan) = SfcP_K(1:nchan)*SfcP
          ENDIF
          IF (iMode .eq. 2) THEN
             SfcP_K(1:nchan) = SfcP_K(1:nchan)*SfcP*alog(real(SfcP))
          ENDIF
          K(1:nchan,iG) = SfcP_K(1:nchan)   
       ENDIF
    ENDDO
    !---Put the CRTM-produced TBs into the vector Y
    Y(1:nchan) = RTSolution(1:nchan,1)%Brightness_Temperature
    CALL CRTM_Geometry_Destroy(GeometryInfo)
    RETURN
  END SUBROUTINE FwdOper




!===============================================================
! Name:         OptionlAdjustK
!
!
! Type:         Subroutine
!
!
! Description:  Adjusts the Jacobians coming from CRTM according
!               to how they are treated in MIRS. 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iMode              I              Mode of treatment (0: natural, 1: log, 2:Log(-Log))
!       - xlay_K             I/O            Derivatives array
!       - nG                 I              # of parameters
!       - nchan              I              # of channels
!       - xlay               I              Vector of parameters
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

  SUBROUTINE OptionlAdjustK(iMode,xlay_K,nG,nchan,xlay)
    INTEGER                         :: iMode,nG,nchan
    REAL,            DIMENSION(:,:) :: xlay_K
    REAL,            DIMENSION(:)   :: xLay
    INTEGER                         :: i
    IF (iMode .eq. 1) THEN
       DO i=1,nG
          xlay_K(i,1:nchan)=xlay_K(i,1:nchan)*xlay(i)  !derivative wrt Log()
       ENDDO
    ENDIF
    IF (iMode .eq. 2) THEN
       DO i=1,nG
          xlay_K(i,1:nchan)=xlay_K(i,1:nchan)*xlay(i)*alog(real(xlay(i)))  !derivative wrt Log(-Log())
       ENDDO
    ENDIF
    RETURN
  END SUBROUTINE OptionlAdjustK

 !===============================================================
! Name:		ComputeEmiss
!
!
! Type:		Subroutine
!
!
! Description:  Computes the emissivity from a scene either by
!               Fastem model or analytically 
!
!
! Arguments:
!
!      Name		      Type   Description
!      ---------------------------------------------------
!	- Scene                 I    Scene structure
!	- Ym                    I    Meas structure
!       - tb                    I     TBs
!       - Atmos                 I    CRTM Atmos Structure
!       - Sfc                   I    CRTM Sfc Structure
!       - Options               I    CRTM Options structure
!       - RTSolution            I    CRTM RTSolution structure
!       - ChannelInfo           I    CRTM ChannelInfo structure
!       - MWwaterCoeff          I    Fastem Coefficients from CRTM shared variable
!       - Emiss                 O    Emissivity vector
!       - logFile               O    logFile
!
!
! Modules needed:
!       - None
!
!
! History:
!       02-07-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE ComputeEmiss(Scene,Ym,tb,Atmos,Sfc,Options,RTSolution,ChannelInfo,MWwaterCoeff,Emiss,logFile)

    !---Input/Output Variables
    REAL(fp_kind),         DIMENSION(:) :: Emiss
    TYPE(Scene_type),      INTENT(INOUT):: Scene
    TYPE(MeasurData_type), INTENT(IN)   :: Ym   
    REAL,                  DIMENSION(:) :: tb
    TYPE(MWwaterCoeff_type)             :: MWwaterCoeff 
    CHARACTER(LEN=*)                    :: logFile
    !---CRTM structures
    TYPE(CRTM_ChannelInfo_type), DIMENSION(:), POINTER :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:)          :: Atmos
    TYPE(CRTM_Surface_type),     DIMENSION(:)          :: Sfc
    TYPE(CRTM_Options_type),     DIMENSION(:)          :: Options
    TYPE(CRTM_RTSolution_type),  DIMENSION(:,:)        :: RTSolution
    TYPE(iVar_type)                                    :: iVar
    !---Local variables
    INTEGER                                        :: ichan,Error_Status
    REAL(fp)                                       :: trx
    REAL(4)                                        :: wn,upRad,dnRad
    REAL, DIMENSION(Ym%nChan)                      :: dnWelling_rad,upWelling_rad,tbrt
    REAL, DIMENSION(SIZE(Scene%pres_lay),Ym%nChan) :: optDepNadir_lay  
    REAL, DIMENSION(SIZE(Scene%pres_lay),Ym%nChan) :: trx_lay  
    INTEGER                                        :: ilay,ipol,nLayEff,nLevEff,i
    REAL                                           :: evert,ehorz
    REAL(fp), DIMENSION(4)                    :: Emiss4Stokes,Refl4Stokes
    REAL(fp), PARAMETER                       :: salinity=33.0 !ppt
    REAL(fp), PARAMETER                       :: azu=-999.0
    REAL(fp)                                  :: windsp,tskin,angle,freq

    !--Determine valid atmospheric layers to use for trx computation
    nLayEff = COUNT(Scene%Temp_lay .gt. 0. .and. Scene%Pres_lev(2:Scene%nLev) .le. Scene%SfcPress)
    IF (nLayEff .le. 2) THEN
      Scene%qc(1) = 1
      RETURN
    ENDIF
    !--Set Atmos cloud flag to signal analytic emissivity computation
    Atmos(1)%n_Clouds = -999
    !--Get radiance/optical depth profile from CRTM
    CALL InterfaceCRTM(Scene,ChannelInfo,Atmos,Sfc,Options,RTSolution,tbrt)
    !--Setup Fastem5 inputs
    tskin  = Scene%Tskin
    windsp = Scene%Windsp
    !---Derive the emissivity
    DO ichan=1,Ym%nchan
       !---Convert radiance from mW/m2 . st . cm-1 to K
       wn=((Ym%CentrFreq(ichan)*1000000000.)/SPEED_LIGHT)/100.
       upRad=RTSolution(ichan,1)%Up_Radiance/1000.
       dnRad=RTSolution(ichan,1)%Down_Radiance/1000.
       upWelling_rad(ichan)=plank(upRad,wn)
       dnWelling_rad(ichan)=plank(dnRad,wn)
       trx_lay(:,ichan)=0.
       DO ilay=1,nLayEff
          trx_lay(ilay,ichan)=exp(-(sum(RTSolution(ichan,1)%Layer_Optical_Depth(1:ilay)*Ym%secant_view(ichan))))
       ENDDO
       !---Compute transmittance
       optDepNadir_lay(:,ichan)=0.
       optDepNadir_lay(1:nLayEff,ichan)=RTSolution(ichan,1)%Layer_Optical_Depth(1:nLayEff)
       trx=exp(-(sum(optDepNadir_lay(:,ichan)*Ym%secant_view(ichan))))
       !---Compute emissivity over ocean using Fastem5 model
       IF (Scene%iTypSfc .eq. OC_TYP) THEN
          !--Setup Fastem5 inputs
          angle = Ym%angle(ichan)
          freq  = Ym%CentrFreq(ichan)
          CALL Compute_FastemX(MWwaterCoeff,freq,angle,tskin,salinity,windsp,ivar,Emiss4Stokes,Refl4Stokes,azu,trx)
          evert             = Emiss4Stokes(1)
          ehorz             = Emiss4Stokes(2)
          ipol              = Ym%polar(ichan)
          Emiss(ichan)      = composeEmiss(ipol,evert,ehorz,real(Angle)) 
       ENDIF
       !---Compute emissivity analytically over non-ocean
       IF (trx .gt.0.35 .and. (Scene%Tskin-dnWelling_rad(ichan)) .gt. 90. .and. Scene%iTypSfc .gt. 0) THEN
          Emiss(ichan)=&
               (((tb(ichan)-upWelling_rad(ichan))/(trx))- &
               dnWelling_rad(ichan))/(Scene%Tskin-dnWelling_rad(ichan))
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE ComputeEmiss

!===============================================================
! Name:		DetermnCldLays
!
!
! Type:		Subroutine
!
!
! Description:  Determines the number of clouds and type within
!                a scene and fills the CRTM Atmosphere%Cloud 
!                structure.
!
!
! Arguments:
!
!      Name		      Type   Description
!      ---------------------------------------------------
!	- Scene                 I    Scene structure
!	- Atmos                 I/O  CRTM Atmosphere structure
!       - nLayEff               I    Number of valid layers
!	- CldTypIdx(MAXCLOUDS)  I    Index identifying cloud type
!
!
! Modules needed:
!       - None
!
!
! History:
!       02-07-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE DetermnCldLays(Scene,Atmos,nLayEff,CldTypIdx)

    !---I/O Variables
    TYPE(Scene_type)                                  :: Scene
    TYPE(CRTM_Atmosphere_type), DIMENSION(:)          :: Atmos
    INTEGER                                           :: nLayEff
    INTEGER,                    DIMENSION(MAXCLOUDS)  :: CldTypIdx
    !---Local Variables
    INTEGER, DIMENSION(:), ALLOCATABLE :: idx_cld
    INTEGER, DIMENSION(:), ALLOCATABLE :: idx_layMiss
    INTEGER                            :: nClouds,nCldLays
    INTEGER                            :: nLayMiss
    INTEGER                            :: i
    REAL,    PARAMETER                 :: DEFAULT_MIN=0.0 !Minimum amount threshold to declare cloudy layer

    !-------------------------------------------------------------------------------
    !---Determine number of cloud layers and type
    !-------------------------------------------------------------------------------
    nCldLays=0
    nClouds=0
    !---Water cloud
    IF (nLayEff .lt. 1) return
    nCldLays = COUNT(Scene%CLW .gt. DEFAULT_MIN)
    IF (nCldLays .gt. 0) THEN
       nClouds=nClouds+1
       CldTypIdx(1) = nClouds
       ALLOCATE(idx_cld(nCldLays))
       idx_cld = PACK( (/(i,i=1,SIZE(Scene%CLW))/), (Scene%CLW .gt. DEFAULT_MIN)) 
       !Atmos(1)%Cloud(nClouds)%n_Layers                    = nLayEff !nCldLays
       Atmos(1)%Cloud(nClouds)%Type                        = WATER_CLOUD
       Atmos(1)%Cloud(nClouds)%Effective_Radius(idx_cld)   = 30          !micron
       Atmos(1)%Cloud(nClouds)%Effective_Variance(idx_cld) = 0.0         !micron^2 (not used)
       Atmos(1)%Cloud(nClouds)%Water_Content(idx_cld)      = Scene%Clw(idx_cld) !kg/m^2
       DEALLOCATE(idx_cld)
    ! Set any negative CLW values in CRTM structure to 0.
       nLayMiss = COUNT(Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)
       IF (nLayMiss .gt. 0) THEN
          ALLOCATE(idx_layMiss(nLayMiss))
          idx_layMiss = PACK( (/(i,i=1,SIZE(Atmos(1)%Cloud(nClouds)%Water_Content))/), &
               (Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)) 
          Atmos(1)%Cloud(nClouds)%Water_Content(idx_layMiss)      = 0.
          DEALLOCATE(idx_layMiss)
       ENDIF
    ENDIF
    !---Rain cloud
    nCldLays = COUNT(Scene%Rain .gt. DEFAULT_MIN)
    IF (nCldLays .gt. 0) THEN
       nClouds=nClouds+1
       CldTypIdx(2) = nClouds
       ALLOCATE(idx_cld(nCldLays))
       idx_cld = PACK( (/(i,i=1,SIZE(Scene%Rain))/), (Scene%Rain .gt. DEFAULT_MIN)) 
       !Atmos(1)%Cloud(nClouds)%n_Layers                    = nLayEff !nCldLays
       Atmos(1)%Cloud(nClouds)%Type                        = RAIN_CLOUD
       Atmos(1)%Cloud(nClouds)%Effective_Radius(idx_cld)   = 500          !micron
       Atmos(1)%Cloud(nClouds)%Effective_Variance(idx_cld) = 0.0          !micron^2 (not used)
       Atmos(1)%Cloud(nClouds)%Water_Content(idx_cld)      = Scene%Rain(idx_cld) !kg/m^2
       DEALLOCATE(idx_cld)
    ! Set any negative Rain values in CRTM structure to 0.
       nLayMiss = COUNT(Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)
       IF (nLayMiss .gt. 0) THEN
          ALLOCATE(idx_layMiss(nLayMiss))
          idx_layMiss = PACK( (/(i,i=1,SIZE(Atmos(1)%Cloud(nClouds)%Water_Content))/), &
               (Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)) 
          Atmos(1)%Cloud(nClouds)%Water_Content(idx_layMiss)      = 0.
          DEALLOCATE(idx_layMiss)
       ENDIF
    ENDIF
    !---Graupel cloud
    nCldLays = COUNT(Scene%Graupel .gt. DEFAULT_MIN)
    IF (nCldLays .gt. 0) THEN
       nClouds=nClouds+1
       CldTypIdx(3) = nClouds
       ALLOCATE(idx_cld(nCldLays))
       idx_cld = PACK( (/(i,i=1,SIZE(Scene%Graupel))/), (Scene%Graupel .gt. DEFAULT_MIN)) 
       !Atmos(1)%Cloud(nClouds)%n_Layers                    = nLayEff !nCldLays
       Atmos(1)%Cloud(nClouds)%Type                        = GRAUPEL_CLOUD
       Atmos(1)%Cloud(nClouds)%Effective_Radius(idx_cld)   = 500         !micron
       Atmos(1)%Cloud(nClouds)%Effective_Variance(idx_cld) = 0.0         !micron^2 (not used)
       Atmos(1)%Cloud(nClouds)%Water_Content(idx_cld)      = Scene%Graupel(idx_cld) !kg/m^2
       DEALLOCATE(idx_cld)
    ! Set any negative Graupel values in CRTM structure to 0.
       nLayMiss = COUNT(Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)
       IF (nLayMiss .gt. 0) THEN
          ALLOCATE(idx_layMiss(nLayMiss))
          idx_layMiss = PACK( (/(i,i=1,SIZE(Atmos(1)%Cloud(nClouds)%Water_Content))/), &
               (Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)) 
          Atmos(1)%Cloud(nClouds)%Water_Content(idx_layMiss)      = 0.
          DEALLOCATE(idx_layMiss)
       ENDIF
    ENDIF
    !---Snow cloud
    nCldLays = COUNT(Scene%Snow(1:nLayEff) .gt. DEFAULT_MIN)
    IF (nCldLays .gt. 0) THEN
       nClouds=nClouds+1
       CldTypIdx(4) = nClouds
       ALLOCATE(idx_cld(nCldLays))
       idx_cld = PACK( (/(i,i=1,SIZE(Scene%Snow(1:nLayEff)))/), (Scene%Snow(1:nLayEff) .gt. DEFAULT_MIN)) 
       Atmos(1)%Cloud(nClouds)%n_Layers                    = nLayEff
       Atmos(1)%Cloud(nClouds)%Type                        = SNOW_CLOUD
       Atmos(1)%Cloud(nClouds)%Effective_Radius(idx_cld)   = 300          !micron
       Atmos(1)%Cloud(nClouds)%Effective_Variance(idx_cld) = 0.0          !micron^2 (not used)
       Atmos(1)%Cloud(nClouds)%Water_Content(idx_cld)      = Scene%Snow(idx_cld) !kg/m^2
       DEALLOCATE(idx_cld)
    ! Set any negative Snow values in CRTM structure to 0.
       nLayMiss = COUNT(Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)
       IF (nLayMiss .gt. 0) THEN
          ALLOCATE(idx_layMiss(nLayMiss))
          idx_layMiss = PACK( (/(i,i=1,SIZE(Atmos(1)%Cloud(nClouds)%Water_Content))/), &
               (Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)) 
          Atmos(1)%Cloud(nClouds)%Water_Content(idx_layMiss)      = 0.
          DEALLOCATE(idx_layMiss)
       ENDIF
    ENDIF
    !---Ice cloud
    nCldLays = COUNT(Scene%Ice(1:nLayEff) .gt. DEFAULT_MIN)
    IF (nCldLays .gt. 0) THEN
       nClouds=nClouds+1
       CldTypIdx(5) = nClouds
       ALLOCATE(idx_cld(nCldLays))
       idx_cld = PACK( (/(i,i=1,SIZE(Scene%Ice(1:nLayEff)))/), (Scene%Ice(1:nLayEff) .gt. DEFAULT_MIN)) 
       Atmos(1)%Cloud(nClouds)%n_Layers                    = nLayEff
       Atmos(1)%Cloud(nClouds)%Type                        = ICE_CLOUD
       Atmos(1)%Cloud(nClouds)%Effective_Radius(idx_cld)   = 30.0         !micron
       Atmos(1)%Cloud(nClouds)%Effective_Variance(idx_cld) = 0.0          !micron^2 (not used)
       Atmos(1)%Cloud(nClouds)%Water_Content(idx_cld)      = Scene%Ice(idx_cld) !kg/m^2
       DEALLOCATE(idx_cld)
    ! Set any negative Ice values in CRTM structure to 0.
       nLayMiss = COUNT(Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)
       IF (nLayMiss .gt. 0) THEN
          ALLOCATE(idx_layMiss(nLayMiss))
          idx_layMiss = PACK( (/(i,i=1,SIZE(Atmos(1)%Cloud(nClouds)%Water_Content))/), &
               (Atmos(1)%Cloud(nClouds)%Water_Content .lt. 0.)) 
          Atmos(1)%Cloud(nClouds)%Water_Content(idx_layMiss)      = 0.
          DEALLOCATE(idx_layMiss)
       ENDIF
    ENDIF
    Atmos(1)%n_Clouds = nClouds
    RETURN
  END SUBROUTINE DetermnCldLays


!===============================================================
! Name:		FillCRTMSfcStrc
!
!
! Type:		Subroutine
!
!
! Description:  Transfers the Scene surface properties to the
!               CTRM Sfc structure 
!
!
! Arguments:
!
!      Name		      Type   Description
!      ---------------------------------------------------
!	- Scene                 I    Scene structure
!	- Sfc                  I/O   Sfc structure
!
!
! Modules needed:
!       - None
!
!
! History:
!       02-07-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE FillCRTMSfcStrc(Scene,Sfc)

    !---I/O Variables
    TYPE(Scene_type),                      INTENT(IN)     :: Scene
    TYPE(CRTM_Surface_type), DIMENSION(:), INTENT(IN OUT) :: Sfc
    !---Local Variables
    INTEGER          :: nchan
    REAL             :: rn

    Sfc%Wind_Speed   = Scene%WindSp
    IF (Scene%iTypSfc .eq. OC_TYP) THEN
       Sfc%Water_Coverage      = 1
       Sfc%Ice_Coverage        = 0
       Sfc%Land_Coverage       = 0
       Sfc%Snow_Coverage       = 0
       Sfc%Water_Temperature   = Scene%Tskin
       Sfc%Salinity            = 33.0
       !Sfc%Wind_Direction      = Scene%WindDir
    ENDIF
    IF (Scene%iTypSfc .eq. SEAICE_TYP) THEN
       Sfc%Water_Coverage      = 0
       Sfc%Ice_Coverage        = 1
       Sfc%Land_Coverage       = 0
       Sfc%Snow_Coverage       = 0
       Sfc%Ice_Temperature     = Scene%Tskin
       !Sfc%Ice_Type            = DEFAULT_ICE_TYPE
       !Sfc%Ice_Thickness       = DEFAULT_ICE_THICKNESS
       !Sfc%Ice_Density         = DEFAULT_ICE_DENSITY
       !Sfc%Ice_Roughness       = DEFAULT_ICE_ROUGHNESS
    ENDIF
    IF (Scene%iTypSfc .eq. LD_TYP) THEN
       Sfc%Water_Coverage      = 0
       Sfc%Ice_Coverage        = 0
       Sfc%Land_Coverage       = 1
       Sfc%Snow_Coverage       = 0
       Sfc%Land_Temperature    = Scene%Tskin
       Sfc%Soil_Temperature    = Scene%Tskin
       !---Randomize or fix surface properties
       !rn=1
       !call random_number(rn)
       !Sfc%Soil_Moisture_Content = 0.2*rn
       !call random_number(rn)
       !Sfc%Vegetation_Fraction   = 0.5*rn
       !call random_number(rn)
       !Sfc%Canopy_Water_Content  = 0.2*rn
    ENDIF
    IF (Scene%iTypSfc .eq. SNOW_TYP) THEN
       Sfc%Water_Coverage      = 0
       Sfc%Ice_Coverage        = 0
       Sfc%Land_Coverage       = 0
       Sfc%Snow_Coverage       = 1
       Sfc%Snow_Temperature    = Scene%Tskin
       !---Vary Snow parameters
       !call random_number(rn)
       !Sfc%Snow_Depth          = 10+(rn*490)
       !call random_number(rn)
       !Sfc%Snow_Grain_Size     = 0.1+(rn*0.7)
       !---Fix Snow Parameters
       !Sfc%Snow_Depth          = 500
       !Sfc%Snow_Grain_Size     = 0.5
       !Sfc%Snow_Density        = 0.25
    ENDIF
    IF (Scene%iTypSfc .eq. COAST_TYP) THEN
       !---Half Ocean/Half Land
       Sfc%Water_Coverage      = 0.5
       Sfc%Ice_Coverage        = 0
       Sfc%Land_Coverage       = 0.5
       Sfc%Snow_Coverage       = 0
       Sfc%Water_Temperature   = Scene%Tskin
       Sfc%Land_Temperature    = Scene%Tskin
       Sfc%Soil_Temperature    = Scene%Tskin
       Sfc%Wind_Direction      = Scene%WindDir
    ENDIF
    RETURN
  END SUBROUTINE FillCRTMSfcStrc

!===============================================================
! Name:         GetSensorInfo
!
!
! Type:         Subroutine
!
!
! Description:  Returns the SensorID character array and nSensors
!               to determine coefficient file names used in 
!               CRTM
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - INT_SensorID       I            Integer string sensor ID
!       - CHAR_SensorID      O            Character string sensor ID
!       - nSensors           O            # of sensors on satellite
!
!
! Modules needed:
!       - Consts
!
!
! History:
!       01-16-2012      Kevin Garrett, RTi @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE GetSensorInfo(INT_SensorID, CHAR_SensorID, nSensors)
    !---Input/Output Variables
    INTEGER, INTENT(IN)                                          :: INT_SensorID
    CHARACTER(STRLEN), DIMENSION(:), ALLOCATABLE, INTENT(IN OUT) :: CHAR_SensorID
    INTEGER, INTENT(OUT)                                         :: nSensors

    !---Determine SensorID and nSensors
    IF (INT_SensorID .eq. SENSOR_ID_N18) THEN
       nSensors = 2
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'amsua_n18', &
            'mhs_n18  ' /)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_METOPA) THEN
       nSensors = 2
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'amsua_metop-a', &
            'mhs_metop-a  ' /)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_METOPB) THEN
       nSensors = 2
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'amsua_metop-b', &
            'mhs_metop-b  ' /)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_F16) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'ssmis_f16'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_N19) THEN
       nSensors = 2
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'amsua_n19', &
            'mhs_n19  ' /)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_F18) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'ssmis_f18'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_NPP) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'atms_npp'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_AMSRE) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'amsre_aqua'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_FY3RI) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'mwri_fy3a'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_TRMM) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'tmi_trmm'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_GPM) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'gmi_gpm'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_F17) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'ssmis_f17'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_MTMA) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'madras_meghat'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_MTSA) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'saphir_meghat'/)
    ENDIF
    IF (INT_SensorID .eq. SENSOR_ID_GCOMW1) THEN
       nSensors = 1
       ALLOCATE(CHAR_SensorID(nSensors))
       CHAR_SensorID = (/'amsr2_gcom-w1'/)
    ENDIF

  END SUBROUTINE GetSensorInfo


END MODULE FwdOperator
