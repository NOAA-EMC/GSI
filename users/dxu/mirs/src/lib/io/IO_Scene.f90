!$Id: IO_Scene.f90 2782 2011-11-18 20:48:59Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:         IO_Scene
! 
! Type:         F90 module
!
! Description:
!       This module is dedicated to the I/O of the scene data.
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!
! Subroutines contained:
!       - set2defaultNonOfficialproducts
!       - setSceneEDRs2default
!       - InitHdrScene
!       - SetUpScene
!       - WriteHdrScene
!       - WriteScene
!       - TransfScene2Xg
!       - ReadHdrScene
!       - ReadScene
!       - FindIndxStr
!       - PrintScene
!       - TransfXg2Scene
!       - PutTogetherXg
!       - SetUpIndex
!       - TransfData2Scene
!       - AffectScene
!       - AdjustHdrScene
!       - AdjustExtProfsPressGrid
!       - PutIntoXg
!       - AdjustEmiss
!       - DestroyScene
!       - FillSceneNegValues
!
! Data type included:
!       - Scene_type
!
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_Scene
  USE misc
  USE Consts
  USE utils
  USE ErrorHandling
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: InitHdrScene,SetUpScene,WriteHdrScene,WriteScene,TransfScene2Xg,getSceneNprf
  PUBLIC :: ReadHdrScene,ReadScene,FindIndxStr,PrintScene,TransfXg2Scene
  PUBLIC :: PutTogetherXg,SetUpIndex,TransfData2Scene,AffectScene
  PUBLIC :: AdjustHdrScene,AdjustExtProfsPressGrid,PutIntoXg,generateRandomCld,DestroyScene
  PUBLIC :: set2defaultNonOfficialproducts,setSceneEDRs2default,FillSceneNegValues
  !---Publicly available data/type definitions
  PUBLIC :: Scene_type
  !---Declaration sections
  TYPE   :: Scene_type
    INTEGER                                    :: iTyp       !0->Scene, 1->retrieved scene (with ChiSq, etc)
    INTEGER                                    :: AlgSN      !Algorithm Serial number (svn). For algorithm-based scenes)
    INTEGER                                    :: ProfIndx   !Profile Index 
    !---Atmospheric class / Surface class
    INTEGER                                    :: iTypAtm    !Atmospheric type ID
    CHARACTER(LEN=20)                          :: DescTypAtm !Label of the atmospheric class
    INTEGER                                    :: iTypSfc    !Surface type ID
    CHARACTER(LEN=20)                          :: DescTypSfc !Label of the surface class
    !---Atmosph Profile-related information
    INTEGER                                    :: Nlev       !Number of levels 
    INTEGER                                    :: Nlay       !Number of layers
    INTEGER                                    :: Nabsorb    !Number of absorbents
    INTEGER,           DIMENSION(:),   POINTER :: AbsorbID   !IDS of the absorbents
    REAL,              DIMENSION(:),   POINTER :: Pres_lev   !Level pressure grid
    REAL,              DIMENSION(:),   POINTER :: Pres_lay   !Layer pressure grid
    REAL,              DIMENSION(:),   POINTER :: Temp_lay   !Layer temperature profile
    REAL,              DIMENSION(:,:), POINTER :: Absorb_lay !Layer aborbents profiles
    INTEGER                                    :: iH2O       !Index of H2O (within absorb vector)
    INTEGER                                    :: iO3        !Index of O3 within absorb vector
    !---Positioning Data
    REAL                                       :: lat        !Latitude
    REAL                                       :: lon        !Longitude
    INTEGER                                    :: node       !=0->ASC, =1->DESC
    INTEGER                                    :: scanDAY    !Day 
    INTEGER                                    :: scanYear   !Year
    REAL                                       :: scanUTC    !UTC time
    INTEGER                                    :: iscanPos   !Scan position 
    INTEGER                                    :: iScanLine  !Scan line Index 
    INTEGER                                    :: nPosScan   !Number of scan positions within scanline
    INTEGER                                    :: nScanLines !Number of scanlines within orbit (some might be missing)
    !---Hydrometeors-related information
    INTEGER                                    :: nParmCLW   !#Parameters defining cloud
    INTEGER                                    :: nParmRain  !#Parameters defining rain
    INTEGER                                    :: nParmSnow  !#Parameters defining Snow
    INTEGER                                    :: nParmIce   !#Parameters defining Ice
    INTEGER                                    :: nParmGrpl  !#Parameters defining Graupel
    REAL,              DIMENSION(:),   POINTER :: Clw        !Cloud amount vector
    REAL,              DIMENSION(:),   POINTER :: Rain       !Rain amount vector
    REAL,              DIMENSION(:),   POINTER :: Snow       !Snow amount vector
    REAL,              DIMENSION(:),   POINTER :: Ice        !Ice amount vector
    REAL,              DIMENSION(:),   POINTER :: Graupel    !Graupel amount vector
    !---Emiss-spectra -related information
    INTEGER                                    :: Nchan      !Number of channels
    REAL,              DIMENSION(:),   POINTER :: CentrFreq  !Central frequencies
    INTEGER,           DIMENSION(:),   POINTER :: polarity   !Polarizations
    REAL                                       :: Angle      !Angle
    REAL                                       :: RelAziAngle!Relative Azimuth Angle
    REAL                                       :: SolZenAngle!Solar Zenith Angle
    REAL,              DIMENSION(:),   POINTER :: Emiss      !Emissivities vector
    REAL,              DIMENSION(:),   POINTER :: Refl       !Reflectivities vector
    !---Surface-level parameters
    REAL                                       :: WindSp     !Wind speed
    REAL                                       :: WindDir    !Wind vector
    REAL                                       :: WindU      !U-direction wind speed
    REAL                                       :: WindV      !V-direction wind speed
    REAL                                       :: Tskin      !Skin temperature
    REAL                                       :: DeltaT     !Delta-Temperature
    REAL                                       :: SfcPress   !Surface pressure
    !---Additional information (used for GDAS,ECMWF, etc)
    REAL                                       :: swe        !Snow Water Equivalent
    REAL                                       :: snowdepth  !Snow Depth
    REAL                                       :: soilMoist  !Soil Moisture
    !---QC info 
    INTEGER                                    :: nqc        !#Elements in QC
    INTEGER(2),        DIMENSION(:),   POINTER :: qc         !QC vector
    !---Convergence/Measurements items (when the scene is a retrieved one)
    INTEGER                                    :: nAttempt   !Number of attempts performed for retrieval
    INTEGER                                    :: nIter      !Number of iterations
    REAL                                       :: ChiSq      !Convergence metric
    REAL,              DIMENSION(:),   POINTER :: YFwd       !Last forward simulated TBs in retr.
    INTEGER,           DIMENSION(:),   POINTER :: ChanSel    !Channels selection used in retr.
    REAL,              DIMENSION(:),   POINTER :: Ym         !Measured TBs used for retrieval (uncorrected)
    REAL,              DIMENSION(:),   POINTER :: YmCorr     !Measured TBs used for retrieval (corrected)
    !---Dummy placeholder variables
    INTEGER                                    :: dummy      !dummy variable (placeholder)
  END TYPE Scene_type
  !---HITRAN-based IDS of molecules
  INTEGER, PARAMETER :: H2OID = 1, O3ID = 3
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: SUM,ADJUSTL,SIZE,TRIM,ALOG,MAXVAL,EXP,RANDOM_NUMBER,MINVAL
  
  
CONTAINS

!===============================================================
! Name:         set2defaultNonOfficialproducts
!
!
! Type:         Subroutine
!
!
! Description:  set to default values all non-officially delivered products
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene              I/O              Input scene structure
!
! Modules needed:
!       - Consts
!
!
! History:
!       06-12-2008      Sid Ahmed Boukabara NOAA/NESDIS/ORA
!
!===============================================================
  SUBROUTINE set2defaultNonOfficialproducts(Scene)
    TYPE(Scene_type)      :: Scene

    !---Some non-retrieved products
    Scene%WindSp      = DEFAULT_VALUE_REAL
    Scene%WindDir     = DEFAULT_VALUE_REAL
    Scene%WindU       = DEFAULT_VALUE_REAL
    Scene%WindV       = DEFAULT_VALUE_REAL
    Scene%DeltaT      = DEFAULT_VALUE_REAL
    Scene%swe         = DEFAULT_VALUE_REAL
    Scene%snowdepth   = DEFAULT_VALUE_REAL
    Scene%soilMoist   = DEFAULT_VALUE_REAL
    !---Set Non-Official products to -999
    IF (Scene%iTypSfc .ne. OC_TYP) THEN
       Scene%CLW(1:Scene%Nlay) = DEFAULT_VALUE_REAL
       !Scene%Temp_lay(1:Scene%Nlay) = DEFAULT_VALUE_REAL
    ENDIF
  END SUBROUTINE set2defaultNonOfficialproducts


!===============================================================
! Name:         setSceneEDRs2default
!
!
! Type:         Subroutine
!
!
! Description:  set EDRs to default if called
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene              I/O              Input scene structure
!
! Modules needed:
!       - Consts
!
!
! History:
!       09-30-2008      Kevin Garrett IMSG@NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE setSceneEDRs2default(Scene)
    TYPE(Scene_type)      :: Scene

    !---Set these parameters to default values
    Scene%Temp_Lay(:)     = DEFAULT_VALUE_REAL
    Scene%Absorb_Lay(:,:) = DEFAULT_VALUE_REAL
    Scene%CLW(:)          = DEFAULT_VALUE_REAL
    Scene%Rain(:)         = DEFAULT_VALUE_REAL
    Scene%Snow(:)         = DEFAULT_VALUE_REAL
    Scene%Ice(:)          = DEFAULT_VALUE_REAL
    Scene%Graupel(:)      = DEFAULT_VALUE_REAL
    Scene%Emiss(:)        = DEFAULT_VALUE_REAL
    Scene%Refl(:)         = DEFAULT_VALUE_REAL

    Scene%Tskin       = DEFAULT_VALUE_REAL
    Scene%SfcPress    = DEFAULT_VALUE_REAL
    Scene%WindSp      = DEFAULT_VALUE_REAL
    Scene%WindDir     = DEFAULT_VALUE_REAL
    Scene%WindU       = DEFAULT_VALUE_REAL
    Scene%WindV       = DEFAULT_VALUE_REAL
    Scene%DeltaT      = DEFAULT_VALUE_REAL
    Scene%swe         = DEFAULT_VALUE_REAL
    Scene%snowdepth   = DEFAULT_VALUE_REAL
    Scene%soilMoist   = DEFAULT_VALUE_REAL

  END SUBROUTINE setSceneEDRs2default


!===============================================================
! Name:         generateRandomCld
!
!
! Type:         Subroutine
!
!
! Description:  Generate Random cloud information 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene              I/O              Input scene structure
!       - maxCLW             I                Max cloud amount to be generated
!       - maxCLWtopP         I                Max cloud top pressure
!       - maxCLWthickn       I                Max cloud thickness
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================
  Subroutine generateRandomCld(Scene,maxCLW,maxCLWtopP,maxCLWthickn)
    TYPE(Scene_type)      :: Scene
    REAL                  :: maxCLW,maxCLWtopP,maxCLWthickn
    REAL                  :: X,CLW,CldThick,CldTop,CldBottom,UnitClw,ActualThick
    INTEGER               :: iParam
    CALL RANDOM_NUMBER(HARVEST=X)
    CLW         = X*maxCLW
    CALL RANDOM_NUMBER(HARVEST=X)
    CldThick    = X*maxCLWthickn
    CALL RANDOM_NUMBER(HARVEST=X)
    CldTop      = X*maxCLWtopP
    CldBottom   = minval((/CldTop+CldThick,Scene%SfcPress/))
    ActualThick = CldBottom-CldTop
    IF (ActualThick .le. 0.) THEN
       print *, 'Warning: cloud geometry not correct. Please check. No cloud simulated.'
       RETURN
    ENDIF
    UnitClw   = CLW/ActualThick
    Do iParam=1,Scene%nParmCLW
       IF (Scene%Pres_lay(iParam) .ge. CldTop .and. Scene%Pres_lay(iParam) .le. CldBottom) THEN
          Scene%clw(iParam) = UnitClw*(Scene%Pres_lev(iParam+1)-Scene%Pres_lev(iParam))
       ENDIF
    ENDDO

  END Subroutine generateRandomCld


!===============================================================
! Name:         AffectScene
!
!
! Type:         Subroutine
!
!
! Description:  Affects one scene structure to another, element 
!               by element (see description of scene at top section
!               of module). 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SceneIn            I              Input scene structure
!       - SceneOut           I/O            Output scene structure
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

  SUBROUTINE AffectScene(SceneIn,SceneOut)
    TYPE(Scene_type)      :: SceneIn,SceneOut
    !----The general variables
    SceneOut%iTyp                          = SceneIn%iTyp
    SceneOut%AlgSN                         = SceneIn%AlgSN
    SceneOut%ProfIndx                      = SceneIn%ProfIndx
    SceneOut%iTypAtm                       = SceneIn%iTypAtm
    SceneOut%DescTypAtm                    = SceneIn%DescTypAtm
    SceneOut%iTypSfc                       = SceneIn%iTypSfc
    SceneOut%DescTypSfc                    = SceneIn%DescTypSfc
    SceneOut%nLev                          = SceneIn%nLev
    SceneOut%nLay                          = SceneIn%nLay
    SceneOut%nAbsorb                       = SceneIn%nAbsorb
    SceneOut%iH2O                          = SceneIn%iH2O
    SceneOut%iO3                           = SceneIn%iO3
    SceneOut%lat                           = SceneIn%lat
    SceneOut%lon                           = SceneIn%lon
    SceneOut%node                          = SceneIn%node
    SceneOut%scanDay                       = SceneIn%scanDay
    SceneOut%scanYear                      = SceneIn%scanYear
    SceneOut%scanUTC                       = SceneIn%scanUTC
    SceneOut%nPosScan                      = SceneIn%nPosScan
    SceneOut%nScanLines                    = SceneIn%nScanLines
    SceneOut%nParmCLW                      = SceneIn%nParmCLW
    SceneOut%nParmRain                     = SceneIn%nParmRain
    SceneOut%nParmSnow                     = SceneIn%nParmSnow
    SceneOut%nParmIce                      = SceneIn%nParmIce
    SceneOut%nParmGrpl                     = SceneIn%nParmGrpl  
    SceneOut%nChan                         = SceneIn%nChan
    SceneOut%Angle                         = SceneIn%Angle
    SceneOut%RelAziAngle                   = SceneIn%RelAziAngle
    SceneOut%SolZenAngle                   = SceneIn%SolZenAngle
    SceneOut%WindSp                        = SceneIn%WindSp
    SceneOut%WindDir                       = SceneIn%WindDir
    SceneOut%Tskin                         = SceneIn%Tskin
    SceneOut%DeltaT                        = SceneIn%DeltaT
    SceneOut%SfcPress                      = SceneIn%SfcPress
    SceneOut%SnowDepth                     = SceneIn%SnowDepth
    SceneOut%nqc                           = SceneIn%nqc
    SceneOut%nAttempt                      = SceneIn%nAttempt
    SceneOut%nIter                         = SceneIn%nIter
    SceneOut%ChiSq                         = SceneIn%ChiSq

    SceneOut%AbsorbID(1:SceneOut%nAbsorb)  = SceneIn%AbsorbID(1:SceneIn%nAbsorb)
    SceneOut%pres_lev(1:SceneOut%nLev)     = SceneIn%pres_lev(1:SceneIn%nLev) 
    SceneOut%pres_lay(1:SceneOut%nLay)     = SceneIn%pres_lay(1:SceneIn%nLay)
    SceneOut%temp_lay(1:SceneOut%nLay)     = SceneIn%temp_lay(1:SceneIn%nLay)
    SceneOut%Absorb_lay(1:SceneOut%nLay,1:SceneOut%nAbsorb) = SceneIn%Absorb_lay(1:SceneIn%nLay,1:SceneIn%nAbsorb)
    SceneOut%clw(1:SceneOut%nParmCLW)      = SceneIn%clw(1:SceneIn%nParmCLW)
    SceneOut%Rain(1:SceneOut%nParmRain)    = SceneIn%Rain(1:SceneIn%nParmRain)
    SceneOut%Snow(1:SceneOut%nParmSnow)    = SceneIn%Snow(1:SceneIn%nParmSnow)
    SceneOut%Ice(1:SceneOut%nParmIce)      = SceneIn%Ice(1:SceneIn%nParmIce)
    SceneOut%Graupel(1:SceneOut%nParmGrpl) = SceneIn%Graupel(1:SceneIn%nParmGrpl)
    SceneOut%CentrFreq(1:SceneOut%nchan)   = SceneIn%CentrFreq(1:SceneIn%nchan)
    SceneOut%polarity(1:SceneOut%nchan)    = SceneIn%polarity(1:SceneIn%nchan)
    SceneOut%Emiss(1:SceneOut%nchan)       = SceneIn%Emiss(1:SceneIn%nchan)
    SceneOut%Refl(1:SceneOut%nchan)        = SceneIn%Refl(1:SceneIn%nchan)
    SceneOut%qc(1:SceneOut%nqc)            = SceneIn%qc(1:SceneIn%nqc)   
    SceneOut%YFwd(1:SceneOut%nchan)        = SceneIn%YFwd(1:SceneIn%nchan)
    SceneOut%ChanSel(1:SceneOut%nchan)     = SceneIn%ChanSel(1:SceneIn%nchan)
    SceneOut%Ym(1:SceneOut%nchan)          = SceneIn%Ym(1:SceneIn%nchan)
    SceneOut%YmCorr(1:SceneOut%nchan)      = SceneIn%YmCorr(1:SceneIn%nchan)
    RETURN
  END SUBROUTINE AffectScene



!===============================================================
! Name:         AdjustHdrScene
!
!
! Type:         Subroutine
!
!
! Description:  Adjusts a scene header with a new pressure grid
!               (for both levels and layers)
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - Scene_Ext                 I              Scene structure to be adjusted
!       - Scene_ExtAdjusted         I/O            Adjusted scene structure
!       - CovNlay                   I              New number of layers (for adjusted)
!       - CovPresLay                I              New lay-based press grid
!       - CovPresLev                I              New lev-based press grid
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

  SUBROUTINE AdjustHdrScene(Scene_Ext,Scene_ExtAdjusted,CovNlay,&
       CovPresLay,CovPresLev)
    REAL,    DIMENSION(:) :: CovPresLay,CovPresLev
    INTEGER               :: CovNlay
    TYPE(Scene_type)      :: Scene_Ext,Scene_ExtAdjusted
    !---Allocate
    ALLOCATE(Scene_ExtAdjusted%AbsorbID(Scene_Ext%nAbsorb), &
         Scene_ExtAdjusted%CentrFreq(Scene_Ext%nchan),      &
         Scene_ExtAdjusted%polarity(Scene_Ext%nchan),       &
         Scene_ExtAdjusted%Absorb_lay(CovNlay,Scene_Ext%nAbsorb),                                   &
         Scene_ExtAdjusted%pres_lay(CovNlay),             Scene_ExtAdjusted%Temp_lay(CovNlay),      &
         Scene_ExtAdjusted%Clw(CovNlay),                  Scene_ExtAdjusted%Rain(CovNlay),          &
         Scene_ExtAdjusted%Snow(CovNlay),                 Scene_ExtAdjusted%Ice(CovNlay),           &
         Scene_ExtAdjusted%Graupel(CovNlay),              Scene_ExtAdjusted%Emiss(Scene_Ext%nchan), &
         Scene_ExtAdjusted%Refl(Scene_Ext%nchan),         Scene_ExtAdjusted%pres_lev(CovNlay+1),    &
         Scene_ExtAdjusted%qc(Scene_Ext%nqc),             Scene_ExtAdjusted%YFwd(Scene_Ext%nchan),  &
         Scene_ExtAdjusted%ChanSel(Scene_Ext%nchan),      Scene_ExtAdjusted%Ym(Scene_Ext%nchan),    &
         Scene_ExtAdjusted%YmCorr(Scene_Ext%nchan))
    !----The adjusted structure is by default identical to the external structure
    Scene_ExtAdjusted%AlgSN                         = Scene_Ext%AlgSN
    Scene_ExtAdjusted%iTyp                          = Scene_Ext%iTyp
    Scene_ExtAdjusted%nChan                         = Scene_Ext%nChan
    Scene_ExtAdjusted%nPosScan                      = Scene_Ext%nPosScan
    Scene_ExtAdjusted%nScanLines                    = Scene_Ext%nScanLines
    Scene_ExtAdjusted%nAbsorb                       = Scene_Ext%nAbsorb
    Scene_ExtAdjusted%AbsorbID(1:Scene_Ext%nAbsorb) = Scene_Ext%AbsorbID(1:Scene_Ext%nAbsorb)
    Scene_ExtAdjusted%CentrFreq(1:Scene_Ext%nchan)  = Scene_Ext%CentrFreq(1:Scene_Ext%nchan)
    Scene_ExtAdjusted%polarity(1:Scene_Ext%nchan)   = Scene_Ext%polarity(1:Scene_Ext%nchan)
    Scene_ExtAdjusted%nqc                           = Scene_Ext%nqc
    Scene_ExtAdjusted%iH2O                          = Scene_Ext%iH2O
    Scene_ExtAdjusted%iO3                           = Scene_Ext%iO3
    !----Adjust the external scene to fit the covariance dimensions
    Scene_ExtAdjusted%nLay        = CovNlay
    Scene_ExtAdjusted%nLev        = CovNlay+1
    Scene_ExtAdjusted%nParmCLW    = CovNlay
    Scene_ExtAdjusted%nParmRain   = CovNlay
    Scene_ExtAdjusted%nParmSnow   = CovNlay
    Scene_ExtAdjusted%nParmIce    = CovNlay
    Scene_ExtAdjusted%nParmGrpl   = CovNlay
    !----Adjust the external scene to fit the covariance press grid
    Scene_ExtAdjusted%pres_lay(1:CovNlay)   = CovPresLay(1:CovNlay)
    Scene_ExtAdjusted%pres_lev(1:CovNlay+1) = CovPresLev(1:CovNlay+1)
    RETURN
  END SUBROUTINE AdjustHdrScene


!===============================================================
! Name:         AdjustExtProfsPressGrid
!
!
! Type:         Subroutine
!
!
! Description:  Generate a scene structure that is on a different
!               pressure grid (for the profiles) than the original
!               scene structure. Interpolation is performed 
!               to the original profiles and affacted to the adjusted
!               scene structure.
!
! Note: The adjustment is peformed only if the number of layers 
!       between the structures is different. If it is the same, it is
!       assumed that they have the same pressure grid (obviously, this
!       is not true all the time). This needs to be updated in the future.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene_Ext           I            Original scene structure
!       - Scene_ExtAdjusted   I/O          Adjusted scene structure 
!                                          based on a different 
!                                          pres grid contained in header
!
!
! Modules needed:
!       - AffectScene
!       - LINT
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE AdjustExtProfsPressGrid(Scene_Ext,Scene_ExtAdjusted)
    TYPE(Scene_type)      :: Scene_Ext,Scene_ExtAdjusted
    CALL AffectScene(Scene_Ext,Scene_ExtAdjusted)
    RETURN
  END SUBROUTINE AdjustExtProfsPressGrid


!===============================================================
! Name:         TransfData2Scene
!
!
! Type:         Subroutine
!
!
! Description:  Transfers some data into the scene structure.
!               These data relate all to convergence.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nAttempt           I           # of attempts made for retrieval
!       - nIter              I           # iterations realized in retrieval
!       - ChiSq              I           Final Chi Square obtained
!       - Scene              I/O         Scene structure
!       - Y                  I           Last forward simulations in retrvl
!       - nchan              I           Number of channels
!       - ChanSel            I           Channels selection used for retrvl
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

  SUBROUTINE TransfData2Scene(nAttempt,nIter,ChiSq,Scene,Y,nchan,ChanSel,Ym,Ymcorr)
    REAL                  :: ChiSq
    INTEGER               :: nAttempt,nIter,nchan
    TYPE(Scene_type)      :: Scene
    REAL,    DIMENSION(:) :: Y,Ym,Ymcorr
    INTEGER, DIMENSION(:) :: ChanSel
    Scene%nAttempt          = nAttempt
    Scene%nIter             = nIter
    Scene%ChiSq             = ChiSq
    Scene%YFwd(1:nchan)     = Y(1:nchan)
    Scene%ChanSel(1:nchan)  = ChanSel(1:nchan)
    Scene%Ym(1:nchan)       = Ym(1:nchan)
    Scene%YmCorr(1:nchan)   = Ymcorr(1:nchan)
    RETURN
  END SUBROUTINE TransfData2Scene



!===============================================================
! Name:         TransfXg2Scene
!
!
! Type:         Subroutine
!
!
! Description:  Transfers data from a geophysical vector into a 
!               scene structure
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Xg                 I             Geophys vector
!       - Scene              I/O           Scene structure
!       - Label              I             Labels of EDRs in Xg, to transfer
!       - Indx               I             Indexes of EDRs in Xg to transfer
!       - ParamLength        I             Lengths of params in Xg.
!       - iSpaceMode         I             Way EDRs are treated (log, natur., etc)
!
!
! Modules needed:
!       - GetFromXg
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE TransfXg2Scene(Xg,Scene,Label,Indx,ParamLength,iSpaceMode)
    TYPE(Scene_type)               :: Scene    
    REAL,             DIMENSION(:) :: Xg
    CHARACTER(LEN=*), DIMENSION(:) :: Label
    INTEGER,          DIMENSION(:) :: Indx,ParamLength,iSpaceMode
    !---Local variables
    INTEGER                        :: nG,nEDRs,i,iG,iMode
    REAL                           :: Tair
    nEDRs = size(Indx)
    DO i=1,nEDRs
       iG    = Indx(i)
       nG    = ParamLength(i)
       iMode = iSpaceMode(i)
       IF(adjustl(trim(Label(i))).eq.'TEMP') THEN
          CALL GetFromXg(Xg,iG,nG,Scene%Temp_lay,iMode)
          Tair = Xg(iG+nG-1)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'WVAP') CALL GetFromXg(Xg,iG,nG,Scene%Absorb_lay(1:nG,1),iMode)
       IF(adjustl(trim(Label(i))).eq.'OZON') CALL GetFromXg(Xg,iG,nG,Scene%Absorb_lay(1:nG,2),iMode)
       IF(adjustl(trim(Label(i))).eq.'CLW')  CALL GetFromXg(Xg,iG,nG,Scene%CLW(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'RAIN') CALL GetFromXg(Xg,iG,nG,Scene%Rain(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'SNOW') CALL GetFromXg(Xg,iG,nG,Scene%Snow(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'ICE')  CALL GetFromXg(Xg,iG,nG,Scene%Ice(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'GRPL') CALL GetFromXg(Xg,iG,nG,Scene%Graupel(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'EMIS') CALL GetFromXg(Xg,iG,nG,Scene%Emiss(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'REFL') CALL GetFromXg(Xg,iG,nG,Scene%Refl(1:nG),iMode)
       IF(adjustl(trim(Label(i))).eq.'WINDSP')THEN
          IF (iMode .eq. 0) Scene%Windsp = Xg(iG)
          IF (iMode .eq. 1) Scene%Windsp = exp(Xg(iG))
          IF (iMode .eq. 2) Scene%Windsp = exp(-exp(Xg(iG)))
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'TSKIN') THEN
          IF (iMode .eq. 0) Scene%Tskin  = Xg(iG)
          IF (iMode .eq. 1) Scene%Tskin  = exp(Xg(iG))
          IF (iMode .eq. 2) Scene%Tskin  = exp(-exp(Xg(iG)))
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'DELTAT') THEN
          IF (iMode .eq. 0) Scene%DeltaT  = Tair + Xg(iG)
          IF (iMode .eq. 1) Scene%DeltaT  = Tair + exp(Xg(iG))
          IF (iMode .eq. 2) Scene%DeltaT  = Tair + exp(-exp(Xg(iG)))
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'SFCP') THEN
          IF (iMode .eq. 0) Scene%SfcPress  = Xg(iG)
          IF (iMode .eq. 1) Scene%SfcPress  = exp(Xg(iG))
          IF (iMode .eq. 2) Scene%SfcPress  = exp(-exp(Xg(iG)))
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE TransfXg2Scene

!===============================================================
! Name:         GetFromXg
!
!
! Type:         Subroutine
!
!
! Description:  Depending on how an EDR is stored (in log, 
!               natural space, etc, depending on iMode), this 
!               subroutines the EDR values fro ma vector Xg. 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Xg                 I             Geophysical state vector
!       - iG                 I             Index of EDR to extract
!       - nG                 I             Number params for that EDR
!       - xLay               O             Extracted EDR vector
!       - iMode              I             Determines how EDR is stored
!                                          in Xg
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

  SUBROUTINE GetFromXg(Xg,iG,nG,xLay,iMode)
    REAL,    DIMENSION(:) :: Xg,xLay
    INTEGER               :: iG,nG,iMode
    IF (iMode .eq. 0) xlay(1:nG)  = Xg(iG:iG+nG-1)
    IF (iMode .eq. 1) xlay(1:nG)  = exp(Xg(iG:iG+nG-1))
    IF (iMode .eq. 2) xlay(1:nG)  = exp(-exp(Xg(iG:iG+nG-1)))
    RETURN
  END SUBROUTINE GetFromXg


!===============================================================
! Name:         PutIntoXg
!
!
! Type:         Subroutine
!
!
! Description:  Does opposite of GetFromXg. It puts an EDR vector
!               into an Xg state vector. It puts it in either
!               normal space, log space, etc depending on iMode
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Xg                 I/O           Geophysical state vector
!       - iG                 I             Index of EDR to put
!       - nG                 I             Number params for that EDR
!       - xLay               O             EDR vector to insert
!       - iMode              I             Determines how EDR is stored
!                                          in Xg
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE PutIntoXg(Xg,iG,nG,xLay,iMode)
    REAL,    DIMENSION(:) :: Xg,xLay
    INTEGER               :: iG,nG,iMode,i
    IF (iMode .eq. 0) Xg(iG:iG+nG-1) = xlay(1:nG)
    IF (iMode .eq. 1) THEN
       DO i=1,nG
          Xg(iG+i-1) = alog(maxval( (/xlay(i),0.00000001/) ))
       ENDDO
    ENDIF
    IF (iMode .eq. 2) THEN
       DO i=1,nG
          Xg(iG+i-1) = alog(-alog(maxval( (/xlay(i),0.00000001/) )))
       ENDDO
    ENDIF
    RETURN
  END SUBROUTINE PutIntoXg


!===============================================================
! Name:         TransfScene2Xg
!
!
! Type:         Subroutine
!
!
! Description:  Transfers certain elements of the scene 
!               structure in to a state vector Xg.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Xg                 I/O         Geophysical state vector
!       - Scene              I           Scene structure (see def. in top)
!       - Label              I           Labels of EDRs to transfer over
!       - Indx               I           Indexes of EDRs (within Xg)
!       - ParamLength        I           Lengths of EDRs to transfer
!       - iSpaceMode         I           Way EDRs are treated (Log,etc)
!
!
! Modules needed:
!       - PutIntoXg
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE TransfScene2Xg(Xg,Scene,Label,Indx,ParamLength,iSpaceMode)
    TYPE(Scene_type)               :: Scene    
    REAL,             DIMENSION(:) :: Xg
    CHARACTER(LEN=*), DIMENSION(:) :: Label
    INTEGER,          DIMENSION(:) :: Indx,ParamLength,iSpaceMode
    !---Local variables
    INTEGER                        :: nG,nEDRs,i,iG,imode
    nEDRs = size(Indx)
    DO i=1,nEDRs
       iG    = Indx(i)
       nG    = ParamLength(i)
       iMode = iSpaceMode(i)
       IF(adjustl(trim(Label(i))).eq.'TEMP') THEN
          IF (nG.ne.Scene%nLay) CALL ErrHandl(ErrorType,Err_InconsSiz,' for TEMP (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%Temp_lay(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'WVAP') THEN
          IF (nG.ne.Scene%nLay) CALL ErrHandl(ErrorType,Err_InconsSiz,' for WVAP (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%Absorb_lay(1:nG,1),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'OZON') THEN
          IF (nG.ne.Scene%nLay) CALL ErrHandl(ErrorType,Err_InconsSiz,' for OZON (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%Absorb_lay(1:nG,2),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'CLW')  THEN
          IF (nG.ne.Scene%nParmCLW) CALL ErrHandl(ErrorType,Err_InconsSiz,' for CLW (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%CLW(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'RAIN') THEN
          IF (nG.ne.Scene%nParmRain) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Rain (Scene & Xg)')  
          CALL PutIntoXg(Xg,iG,nG,Scene%Rain(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'SNOW') THEN
          IF (nG.ne.Scene%nParmSnow) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Snow (Scene & Xg)')
          CALL PutIntoXg(Xg,iG,nG,Scene%Snow(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'ICE')  THEN
          IF (nG.ne.Scene%nParmIce) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Ice (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%Ice(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'GRPL') THEN
          IF (nG.ne.Scene%nParmGrpl) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Grpl (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%Graupel(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'EMIS') THEN
          IF (nG.ne.Scene%nchan) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Emiss (Scene & Xg)')  
          CALL PutIntoXg(Xg,iG,nG,Scene%Emiss(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'REFL') THEN
          IF (nG.ne.Scene%nchan) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Refl (Scene & Xg)') 
          CALL PutIntoXg(Xg,iG,nG,Scene%Refl(1:nG),iMode)
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'WINDSP')THEN
          IF (nG.ne.1) CALL ErrHandl(ErrorType,Err_InconsSiz,' for Windsp (Scene & Xg)') 
          IF (iMode .eq. 0) Xg(iG) = Scene%Windsp
          IF (iMode .eq. 1) Xg(iG) = alog(Scene%Windsp)
          IF (iMode .eq. 2) Xg(iG) = alog(-alog(Scene%Windsp))
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'TSKIN') THEN
          IF (nG.ne.1)  CALL ErrHandl(ErrorType,Err_InconsSiz,' for Tskin (Scene & Xg)') 
          IF (iMode .eq. 0) Xg(iG) = Scene%Tskin
          IF (iMode .eq. 1) Xg(iG) = alog(Scene%Tskin)
          IF (iMode .eq. 2) Xg(iG) = alog(-alog(Scene%Tskin))
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'DELTAT')THEN
          IF (nG.ne.1) CALL ErrHandl(ErrorType,Err_InconsSiz,' for deltaT (Scene & Xg)')  
          IF (iMode .eq. 0) Xg(iG) = Scene%Tskin - Scene%Temp_lay(Scene%nLay)
          IF (iMode .eq. 1) Xg(iG) = alog(Scene%Tskin - Scene%Temp_lay(Scene%nLay))
          IF (iMode .eq. 2) Xg(iG) = alog(-alog(Scene%Tskin - Scene%Temp_lay(Scene%nLay)))
       ENDIF
       IF(adjustl(trim(Label(i))).eq.'SFCP')THEN
          IF (nG.ne.1) CALL ErrHandl(ErrorType,Err_InconsSiz,' for SfcP (Scene & Xg)') 
          IF (iMode .eq. 0) Xg(iG) = Scene%SfcPress
          IF (iMode .eq. 1) Xg(iG) = alog(Scene%SfcPress)
          IF (iMode .eq. 2) Xg(iG) = alog(-alog(Scene%SfcPress))
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE TransfScene2Xg


!===============================================================
! Name:         PrintScene
!
!
! Type:         Subroutine
!
!
! Description:  Log-prints the scene contents 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene              I            Scene structure (see 
!                                         definition in top 
!                                         section of module)
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

  SUBROUTINE PrintScene(Scene)
    TYPE(Scene_type)                :: Scene    
    REAL                            :: xtpw1,iclw,rwp,iwp,gwp,swp
    print *, 'Algorithm serial number:',Scene%AlgSN
    print *, 'TypAtm:',Scene%iTypAtm,Scene%DescTypAtm
    print *, 'TypSfc:',Scene%iTypSfc,Scene%DescTypSfc
    print *, 'Nlev/Nlay:',Scene%Nlev,Scene%nLay
    print *, 'Nabsorb:',Scene%Nabsorb
    print *, 'AborbIDs:',Scene%AbsorbID
    print *, 'H2O Indx:',Scene%iH2O
    !print *, 'O3 Indx:',Scene%iO3
    print *, 'Pres(Lev):',Scene%Pres_lev
    print *, 'Pres(Lay):',Scene%Pres_lay
    print *, 'Temp(Lay):',Scene%Temp_lay
    print *, 'nPar(CLW/Rain/Snow):',Scene%nParmCLW,Scene%nParmRain,Scene%nParmSnow
    print *, 'nPar(Ice/Grpl):',Scene%nParmIce,Scene%nParmGrpl
    print *, 'Absorbers(Lay) H2O:',Scene%Absorb_lay(:,Scene%iH2O)
    !print *, 'Absorbers(Lay) O3:',Scene%Absorb_lay(:,Scene%iO3)
    print *, 'CLW:',Scene%Clw
    print *, 'Rain:',Scene%Rain
    !print *, 'Snow:',Scene%Snow
    !print *, 'Ice:',Scene%Ice
    print *, 'Graupel:',Scene%Graupel
    call ComputeTPW(Scene%pres_lev(1:Scene%nLev),&
         Scene%SfcPress,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2O),xtpw1)
    iclw=ColumIntegr(Scene%nParmCLW,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%clw(1:Scene%nParmCLW))
    gwp=ColumIntegr(Scene%nParmGrpl,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%graupel(1:Scene%nParmGrpl))
    rwp=ColumIntegr(Scene%nParmRain,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%rain(1:Scene%nParmRain))
    swp=ColumIntegr(Scene%nParmSnow,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%snow(1:Scene%nParmSnow))
    iwp=ColumIntegr(Scene%nParmIce,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%ice(1:Scene%nParmIce))
    print *, 'TPW:',xtpw1
    print *, 'CLW:',iClw
    print *, 'Rain:',rwp
    print *, 'Snow:',swp
    print *, 'Ice:',iwp
    print *, 'Graupel:',gwp
    print *, 'nChan:',Scene%nchan
    print *, 'Freq:',Scene%CentrFreq
    print *, 'Polar:',Scene%polarity
    print *, 'Angle:',Scene%Angle
    print *, 'Rel-Azi Angle:',Scene%RelAziAngle
    print *, 'Solar Zenith Angle:',Scene%SolZenAngle
    print *, 'Emiss:',Scene%Emiss
    print *, 'Refl:',Scene%Refl
    print *, 'WindSp:',Scene%WindSp
    print *, 'WindDir:',Scene%WindDir
    print *, 'Tskin:',Scene%Tskin
    print *, 'SnowDepth:',Scene%snowdepth
    print *, 'SfcPres:',Scene%SfcPress
    print *, '----------------END of profile---------------'
    RETURN
  END SUBROUTINE PrintScene

!===============================================================
! Name:         InitHdrScene
!
!
! Type:         Subroutine
!
!
! Description:  Initialize the header of the scene structure 
!               with general variables like frequency, 
!               polarization, layers pressure grid, etc. It also 
!               performs the memory allocation for the arrays.
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - nLev                      I             # of levels
!       - nLay                      I             # of layers
!       - nChan                     I             # of channels
!       - CentrFreq                 I             Central Frequencies vector
!       - polarity                  I             Polarization vector
!       - pres_lev                  I             Level-pressure grid
!       - pres_lay                  I             Layer-pressure grid
!       - Scene                     I/O           Scene structure
!       - nParmCLW                  I             # parameters describing cloud
!       - nParmRain                 I             # parameters describing Rain
!       - nParmSnow                 I             # parameters describing Snow
!       - nParmIce                  I             # parameters describing Ice
!       - nParmGrpl                 I             # parameters describing Graupel
!       - nAbsorb                   I             # of absorbents
!       - AbsorbID                  I             IDs of absorbents 
!       - nqc                       I             # of integers used for QC
!       - iTyp                      I             Type of file: retrieval of truth
!       - nPosScan                  I             # of scan positions within scanline
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

  SUBROUTINE InitHdrScene(nLev,nLay,nChan,CentrFreq,polarity,pres_lev,&
       pres_lay,Scene,nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl,&
       nAbsorb,AbsorbID,nqc,iTyp,nPosScan,nScanLines,AlgSN)
    INTEGER                         :: nLev,nLay,nchan,nAbsorb,nqc
    INTEGER                         :: nPosScan,nScanLines
    REAL,              DIMENSION(:) :: CentrFreq
    REAL,              DIMENSION(:) :: pres_lay,pres_lev
    INTEGER,           DIMENSION(:) :: polarity,AbsorbID
    TYPE(Scene_type)                :: Scene
    INTEGER                         :: nParmCLW,nParmRain,nParmSnow
    INTEGER                         :: nParmIce,nParmGrpl     
    INTEGER                         :: iTyp !=0->Simple Scene, =1->Retrieved Scene 
    INTEGER                         :: AlgSN
    !---Initialize the Scene structure (individual values)
    Scene%iH2O                = 1
    Scene%iO3                 = 2
    Scene%iTyp                = iTyp
    Scene%AlgSN               = AlgSN
    Scene%nLev                = nLev 
    Scene%nLay                = nLay 
    Scene%nChan               = nChan 
    Scene%nPosScan            = nPosScan
    Scene%nScanLines          = nScanLines
    Scene%nAbsorb             = nAbsorb
    Scene%nParmCLW            = nParmCLW
    Scene%nParmRain           = nParmRain
    Scene%nParmSnow           = nParmSnow
    Scene%nParmIce            = nParmIce
    Scene%nParmGrpl           = nParmGrpl
    !---Initialize the Scene structure 
    ALLOCATE(Scene%AbsorbID(Scene%Nabsorb),Scene%pres_lev(nLev),      &
         Scene%pres_lay(nLay),Scene%Temp_lay(nLay),                   &
         Scene%Absorb_lay(nLay,Scene%nAbsorb),Scene%CentrFreq(nChan), &
         Scene%polarity(nChan),Scene%Emiss(nchan),Scene%Refl(nchan),  &
         Scene%qc(nqc),Scene%YFwd(nchan),Scene%ChanSel(nchan),        &
         Scene%Ym(nchan),Scene%YmCorr(nchan))
    ALLOCATE(Scene%Clw(nParmCLW),Scene%Rain(nParmRain),Scene%Snow(nParmSnow))
    ALLOCATE(Scene%Ice(nParmIce),Scene%Graupel(nParmGrpl))
    Scene%AbsorbID(1:Scene%nAbsorb) = AbsorbID(1:nAbsorb)
    Scene%pres_lay(1:nLay)          = pres_lay(1:nLay)
    Scene%pres_lev(1:nLev)          = pres_lev(1:nLev)
    Scene%CentrFreq(1:nchan)        = CentrFreq(1:nchan)
    Scene%polarity(1:nchan)         = polarity(1:nchan)
    Scene%nqc                       = nqc
    RETURN
  END SUBROUTINE InitHdrScene



!===============================================================
! Name:         ReadHdrScene
!
!
! Type:         Subroutine
!
!
! Description:  Reads the header from a geophysical Scene file.
!               It also performs memory allocation for arrays 
!               needed later on.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 O              Unit number after opening
!       - InputFile          I              Name of the input file
!       - Scene              I/O            Structure containing geoph data
!       - nprf               O              Number of profiles in file
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

  SUBROUTINE ReadHdrScene(iu,InputFile,Scene,nprf)
    CHARACTER(LEN=*)      :: InputFile
    INTEGER               :: iu,nPrf
    TYPE(Scene_type)      :: Scene
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=InputFile,form='unformatted')
    READ(iu)  Scene%iTyp,Scene%AlgSN!iTyp: =0->Simple Scene, =1->Retrieved Scene 
    READ(iu)  nprf
    READ(iu)  Scene%nLay
    READ(iu)  Scene%nLev
    READ(iu)  Scene%nChan
    READ(iu)  Scene%nPosScan
    READ(iu)  Scene%nScanLines
    READ(iu)  Scene%nAbsorb
    READ(iu)  Scene%nParmCLW
    READ(iu)  Scene%nParmRain
    READ(iu)  Scene%nParmSnow
    READ(iu)  Scene%nParmIce
    READ(iu)  Scene%nParmGrpl
    ALLOCATE(Scene%AbsorbID(1:Scene%nAbsorb),Scene%CentrFreq(1:Scene%nchan),&
         Scene%polarity(1:Scene%nchan))
    READ(iu)  Scene%AbsorbID(1:Scene%nAbsorb)
    READ(iu)  Scene%CentrFreq(1:Scene%nchan)
    READ(iu)  Scene%polarity(1:Scene%nchan)
    READ(iu)  Scene%nqc
    !---Find the indexes for H2O and O3 gases
    Scene%iH2O = FindIndx(Scene%AbsorbID,H2OID)
    Scene%iO3  = FindIndx(Scene%AbsorbID,O3ID)
    !---Allocate the arrays for future reading
    ALLOCATE(Scene%pres_lay(1:Scene%nLay),Scene%Temp_lay(1:Scene%nLay), &
         Scene%Absorb_lay(1:Scene%nLay,1:Scene%nAbsorb),                &
         Scene%Clw(1:Scene%nParmCLW),Scene%Rain(1:Scene%nParmRain),     &
         Scene%Snow(1:Scene%nParmSnow),Scene%Ice(1:Scene%nParmIce),     &
         Scene%Graupel(1:Scene%nParmGrpl),Scene%Emiss(1:Scene%nchan),   &
         Scene%Refl(1:Scene%nchan),Scene%pres_lev(1:Scene%nLev),        &
         Scene%qc(1:Scene%nqc),Scene%YFwd(1:Scene%nchan),               &
         Scene%ChanSel(1:Scene%nchan),Scene%Ym(1:Scene%nchan),          &
         Scene%YmCorr(1:Scene%nchan))
    RETURN
  END SUBROUTINE ReadHdrScene




!===============================================================
! Name:         getSceneNprf
!
!
! Type:         Subroutine
!
!
! Description:  get number of profiles in the Scene file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - InputFile          I              Name of the input Scene file
!       - nprf               O              Number of profiles in file
!
!
! Modules needed:
!       - get_lun
!
!
! History:
!       06-29-2011      Wanchun Chen,       DELL Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE getSceneNprf(InputFile,nprf)
    CHARACTER(LEN=*)      :: InputFile
    INTEGER               :: iu,nprf,ityp,algSn
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=InputFile,form='unformatted')
    READ(iu) ityp,algSn
    READ(iu) nprf
    CLOSE(iu)
    RETURN
  END SUBROUTINE getSceneNprf


!===============================================================
! Name:          FindIndx
!
!
! Type:          Function
!
!
! Description:  Within a vector containing the IDs of absorbents,
!               this function will find the index corresponding
!               to a specified ID.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - AbsorbID          I              Vector of absorbents IDs
!       - ID                I              ID of the absorbent which 
!                                          we want to find if it is 
!                                          within AbsorbID.
!       - FindIndx          O              Index of ID within AbsorbID
!                                          (=-1 if not in there at all)
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

  INTEGER FUNCTION FindIndx(AbsorbID,ID)
    INTEGER, DIMENSION(:) :: AbsorbID
    INTEGER               :: ID
    !---Local variables
    INTEGER               :: iId,nIDs
    nIDs = SIZE(AbsorbID)
    FindIndx = -1
    DO iId=1,nIDs
       IF (ID .eq. AbsorbID(iId)) THEN
          FindIndx = iId
       ENDIF
    ENDDO
    IF (FindIndx .eq. -1) CALL ErrHandl(WarningType,Warn_NoMolecIndxFound,'') 
    RETURN
  END FUNCTION FindIndx


!===============================================================
! Name:         FindIndxStr
!
!
! Type:         Function
!
!
! Description:  Same thing as FindIndx except that the selection
!               is based on the string labels, not the numerical
!               IDs: Find the index corresponding to an absorbent label
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - DescVec           I              List of labels
!       - Desc              I              Label for which we search index.
!       - FindIndxStr       O              Index of Desc within DescVec
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

  INTEGER FUNCTION FindIndxStr(DescVec,Desc)
    CHARACTER(LEN=*), DIMENSION(:) :: DescVec
    CHARACTER(LEn=*)               :: Desc
    !---Local variables
    INTEGER               :: i,n
    n = SIZE(DescVec)
    FindIndxStr = -1
    DO i=1,n
       IF (adjustl(trim(Desc)) .eq. adjustl(trim(DescVec(i)))) FindIndxStr = i
    ENDDO
    IF (FindIndxStr .eq. -1) CALL ErrHandl(ErrorType,Err_NoMatching,'EDR string label found')
    RETURN
  END FUNCTION FindIndxStr


!===============================================================
! Name:         WriteHdrScene
!
!
! Type:         Subroutine
!
!
! Description:  Writes header of scene in to file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 O             Unit number
!       - OutputFile         I             Name of output file
!       - Scene              I             Structure whose header 
!                                          is to be written out
!       - nprf               I             Number of profiles to write
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

  SUBROUTINE WriteHdrScene(iu,OutputFile,Scene,nprf)
    CHARACTER(LEN=*)      :: OutputFile
    INTEGER               :: iu,nPrf
    TYPE(Scene_type)      :: Scene
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=OutputFile,form='unformatted')
    WRITE(iu) Scene%iTyp,Scene%AlgSN
    WRITE(iu) nprf
    WRITE(iu) Scene%nLay
    WRITE(iu) Scene%nLev
    WRITE(iu) Scene%nChan
    WRITE(iu) Scene%nPosScan
    WRITE(iu) Scene%nScanLines
    WRITE(iu) Scene%nAbsorb
    WRITE(iu) Scene%nParmCLW
    WRITE(iu) Scene%nParmRain
    WRITE(iu) Scene%nParmSnow
    WRITE(iu) Scene%nParmIce
    WRITE(iu) Scene%nParmGrpl
    WRITE(iu) Scene%AbsorbID(1:Scene%nAbsorb)
    !---Commented out because the presure is assumed to vary prof/prof like the angle
    !WRITE(iu) Scene%pres_lay(1:Scene%nLay)
    !-------------
    WRITE(iu) Scene%CentrFreq(1:Scene%nchan)
    WRITE(iu) Scene%polarity(1:Scene%nchan)
    WRITE(iu) Scene%nqc
    RETURN
  END SUBROUTINE WriteHdrScene



!===============================================================
! Name:         WriteScene
!
!
! Type:         Subroutine
!
!
! Description:  Writes the scene content (not header) into a file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                I               Unit number
!       - Scene             I               Scene structure whose
!                                           content is to be written out
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

  SUBROUTINE WriteScene(iu,Scene)
    TYPE(Scene_type)   :: Scene
    INTEGER            :: iu

    WRITE(iu) Scene%ProfIndx
    !---Atmospheric constituents
    WRITE(iu) Scene%pres_lay(1:Scene%nLay)
    WRITE(iu) Scene%pres_lev(1:Scene%nLev)
    WRITE(iu) Scene%Temp_lay(1:Scene%nLay)
    WRITE(iu) Scene%Absorb_lay(1:Scene%nLay,1:Scene%nAbsorb)
    !---hydrometeors
    WRITE(iu) Scene%Clw(1:Scene%nParmCLW)
    WRITE(iu) Scene%Rain(1:Scene%nParmRain)
    WRITE(iu) Scene%Graupel(1:Scene%nParmGrpl)
    !---Emissivity/reflectivity vectors
    WRITE(iu) Scene%Emiss(1:Scene%nchan)
    !---Surface-level paremeters
    WRITE(iu) Scene%angle,Scene%WindSp,Scene%Tskin,Scene%SfcPress,Scene%iTypSfc,Scene%WindU,Scene%WindV,&
         Scene%RelAziAngle,Scene%SolZenAngle,Scene%SnowDepth
    !---QC variables 
    WRITE(iu) Scene%qc(1:Scene%nqc)   
    !---Positioning variables
    WRITE(iu) Scene%lat,Scene%lon,Scene%node,Scene%scanUTC,Scene%scanYear,Scene%scanDay,Scene%iscanPos,Scene%iscanLine
    !---In case the scene is a retrieved one
    IF (Scene%iTyp .eq. 1) THEN
       WRITE(iu) Scene%nAttempt,Scene%nIter,Scene%ChiSq
       WRITE(iu) Scene%YFwd(1:Scene%nchan)
       WRITE(iu) Scene%ChanSel(1:Scene%nchan)
       WRITE(iu) Scene%Ym(1:Scene%nchan)
       WRITE(iu) Scene%YmCorr(1:Scene%nchan)
    ENDIF
    RETURN
  END SUBROUTINE WriteScene


!===============================================================
! Name:         ReadScene
!
!
! Type:         Subroutine
!
!
! Description:  Reads content of a geophysical scene file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 I              Unit number
!       - Scene              I/O            Structure to be filled 
!                                           with content of file
!       - ierr               O              Error index when reading
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

  SUBROUTINE ReadScene(iu,Scene,ierr)
    TYPE(Scene_type)   :: Scene
    INTEGER            :: iu,ierr
    ierr=0
    READ(iu,iostat=ierr,end=10) Scene%ProfIndx
    IF (ierr.ne.0) THEN
       ierr=Warn_readInvalid
       CALL ErrHandl(WarningType,Warn_readInvalid,'Scene invalid.')
       RETURN
    ENDIF
    !---Atmospheric constituents
    READ(iu,err=20) Scene%pres_lay(1:Scene%nLay)
    READ(iu,err=20) Scene%pres_lev(1:Scene%nLev)
    READ(iu,err=20) Scene%Temp_lay(1:Scene%nLay)
    READ(iu,err=20) Scene%Absorb_lay(1:Scene%nLay,1:Scene%nAbsorb)
    !---hydrometeors
    READ(iu,err=20) Scene%Clw(1:Scene%nParmCLW)
    READ(iu,err=20) Scene%Rain(1:Scene%nParmRain)
    READ(iu,err=20) Scene%Graupel(1:Scene%nParmGrpl)
    Scene%Snow(1:Scene%nParmSnow) =0.
    Scene%Ice(1:Scene%nParmIce)   =0.
    !---Emissivity/reflectivity vectors
    READ(iu,err=20) Scene%Emiss(1:Scene%nchan)
    !---Surface-level paremeters
    READ(iu,err=20) Scene%angle,Scene%WindSp,Scene%Tskin,Scene%SfcPress,Scene%iTypSfc,Scene%WindU,Scene%WindV,&
         Scene%RelAziAngle,Scene%SolZenAngle,Scene%SnowDepth
    !---QC variables 
    READ(iu,err=20) Scene%qc(1:Scene%nqc)   
    !---Positioning variables
    READ(iu,err=20) Scene%lat,Scene%lon,Scene%node,Scene%scanUTC,Scene%scanYear,Scene%scanDay,Scene%iscanPos,Scene%iscanLine 
    !---In case the scene is a retrieved one
    IF (Scene%iTyp .eq. 1) THEN
       READ(iu,err=20) Scene%nAttempt,Scene%nIter,Scene%ChiSq
       READ(iu,err=20) Scene%YFwd(1:Scene%nchan)
       READ(iu,err=20) Scene%ChanSel(1:Scene%nchan)
       READ(iu,err=20) Scene%Ym(1:Scene%nchan)
       READ(iu,err=20) Scene%YmCorr(1:Scene%nchan)
    ENDIF
    RETURN
10  CONTINUE
    ierr=Warn_EndOfFile
    CALL ErrHandl(WarningType,Warn_EndOfFile,'Scene') 
    RETURN
20  ierr=Warn_readInvalid
    CALL ErrHandl(WarningType,Warn_readInvalid,'(ReadScene)')
    RETURN
  END SUBROUTINE ReadScene


!===============================================================
! Name:         SetUpScene
!
!
! Type:         Subroutine
!
!
! Description:  This subroutine basically sets up the scene 
!               structure with elements coming from the inputs.
!               This is to be done before writing the scene into
!               a file.
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - pres_lev                   I           Level press grid
!       - pres_lay                   I           Layer press grid
!       - angle                      I           Viewing angle
!       - RelAziangle                I           Relative Azimuth angle
!       - SolZenAngle                I           Solar Zenith angle
!       - Scene                      I/O         Structure to be set
!       - iSfcTyp                    I           Surface type
!       - iProf                      I           Profile number
!       - lat                        I           Latitude
!       - lon                        I           Longitude
!       - node                       I           Ascending/descending mode
!       - scanDAY                    I           Day of the measurement
!       - scanYear                   I           Year of the measurement
!       - scanUTC                    I           UTC time of the measurement
!       - iscanPos                   I           Scan position of measurem.
!       - qc                         I           QC vector (if any)
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

  SUBROUTINE SetUpScene(pres_lev,pres_lay,angle,Scene,iSfcTyp,iProf,lat,lon,node,&
       scanDAY,scanYear,scanUTC,iscanPos,iscanLine,RelAziAngle,SolZenAngle)
    REAL,              DIMENSION(:) :: pres_lev,pres_lay
    REAL                            :: angle,RelAziAngle,SolZenAngle
    REAL                            :: lat,lon,scanUTC
    TYPE(Scene_type)                :: Scene
    INTEGER                         :: iProf,iSfcTyp,node,scanDay,scanYear,iscanPos,iscanLine

    Scene%iH2O                          = 1
    Scene%iO3                           = 2
    Scene%ProfIndx                      = iProf
    Scene%pres_lev(1:Scene%nLev)        = pres_lev(1:Scene%nLev)
    Scene%pres_lay(1:Scene%nLay)        = pres_lay(1:Scene%nLay)
    Scene%angle                         = angle
    Scene%RelAziAngle                   = RelAziAngle
    Scene%SolZenAngle                   = SolZenAngle
    Scene%iTypSfc                       = iSfcTyp
    Scene%lat                           = lat
    Scene%lon                           = lon
    Scene%node                          = node
    Scene%scanDay                       = scanDay
    Scene%scanYear                      = scanYear
    Scene%scanUTC                       = scanUTC
    Scene%iscanPos                      = iscanPos
    Scene%iscanLine                     = iscanLine
    RETURN
  END SUBROUTINE SetUpScene


!===============================================================
! Name:         PutTogetherXg
!
!
! Type:         Subroutine
!
!
! Description:  This subroutine puts together the Xg vector, from
!               independent pieces of geophysical parameters, 
!               following a certain predefined order defined by the
!               subroutine SetUpIndex
!
!
! Arguments:
!
!          Name                    Type          Description
!      ---------------------------------------------------
!       - Xg                         O           Consolidated State vector
!       - layer_t                    I           Layer temperature profile
!       - layer_w                    I           Layer humidity profile
!       - layer_o3                   I           Layer ozone profile
!       - layer_Clw                  I           Layer cloud amount profile
!       - layer_Rain                 I           Layer rain profile
!       - layer_Snow                 I           Layer snow profile
!       - layer_Ice                  I           Layer ice profile
!       - layer_gh                   I           Layer graupel profile
!       - iProf                      I           profile index
!       - Emiss                      I           Emissivity spectrum
!       - Refl                       I           reflectivity spectrum
!       - SfcWind_s                  I           Surface wind speed
!       - tskin                      I           Skin temperature
!       - EDR_Desc                   I           EDRs description labels
!       - EDR_Indx                   I           EDRs indexes (within Xg)
!       - EDR_Length                 I           EDRs lengthes 
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

  SUBROUTINE PutTogetherXg(Xg,layer_t,layer_w,layer_o3,layer_Clw,layer_Rain, &
       layer_Snow,layer_Ice,layer_gh,iProf,Emiss,Refl,SfcWind_s,tskin,&
       EDR_Desc,EDR_Indx,EDR_Length)
    
    INTEGER                        :: iProf
    REAL,             DIMENSION(:) :: Xg,layer_t,layer_w,layer_o3,layer_Clw,layer_Rain
    REAL,             DIMENSION(:) :: layer_Snow,layer_Ice,layer_gh,Emiss,Refl
    REAL                           :: SfcWind_s,tskin
    INTEGER,          DIMENSION(:) :: EDR_Indx,EDR_Length
    CHARACTER(LEN=*), DIMENSION(:) :: EDR_Desc
    
    Xg(EDR_Indx(1):EDR_Indx(1)+EDR_Length(1)-1)    = layer_t
    Xg(EDR_Indx(2):EDR_Indx(2)+EDR_Length(2)-1)    = layer_w
    Xg(EDR_Indx(3):EDR_Indx(3)+EDR_Length(3)-1)    = layer_o3
    Xg(EDR_Indx(4):EDR_Indx(4)+EDR_Length(4)-1)    = layer_clw
    Xg(EDR_Indx(5):EDR_Indx(5)+EDR_Length(5)-1)    = layer_rain
    Xg(EDR_Indx(6):EDR_Indx(6)+EDR_Length(6)-1)    = layer_snow
    Xg(EDR_Indx(7):EDR_Indx(7)+EDR_Length(7)-1)    = layer_ice
    Xg(EDR_Indx(8):EDR_Indx(8)+EDR_Length(8)-1)    = layer_gh
    Xg(EDR_Indx(9):EDR_Indx(9)+EDR_Length(9)-1)    = Emiss
    Xg(EDR_Indx(10):EDR_Indx(10)+EDR_Length(10)-1) = Refl
    Xg(EDR_Indx(11):EDR_Indx(11)+EDR_Length(11)-1) = (/SfcWind_s/)
    Xg(EDR_Indx(12):EDR_Indx(12)+EDR_Length(12)-1) = (/Tskin/)
    RETURN
  END SUBROUTINE PutTogetherXg


!===============================================================
! Name:         SetUpIndex
!
!
! Type:         Subroutine
!
!
! Description:  This subroutine sets up the order to the EDRs 
!               within the state vector.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nLay               I              Number of layers
!       - nchan              I              Number of channels
!       - EDR_Desc           O              EDRs description labels
!       - EDR_Indx           O              EDRs indexes (within Xg)
!       - EDR_Length         O              EDRs lengthes 
!       - nEDRs              I              Number of EDRs
!       - nG                 O              Number of total parameters 
!
!
! Modules needed:
!       - DeterminIndx
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE SetUpIndex(nLay,nchan,EDR_Desc,EDR_Indx,EDR_Length,nEDRs,nG)
    INTEGER                        :: nLay,nEDRs,nchan,nG
    INTEGER, DIMENSION(:)          :: EDR_Indx,EDR_Length
    CHARACTER(LEN=*), DIMENSION(:) :: EDR_Desc
    nEDRs= 12 !T+WV+O3+CLW+RAIN+SNOW+ICE+GRPL+EMIS+REFL+WINDSP+TSKIN
    EDR_Desc(1)  ='TEMP'
    EDR_Desc(2)  ='WVAP'
    EDR_Desc(3)  ='OZON'
    EDR_Desc(4)  ='CLW'
    EDR_Desc(5)  ='RAIN'
    EDR_Desc(6)  ='SNOW'
    EDR_Desc(7)  ='ICE'
    EDR_Desc(8)  ='GRPL'
    EDR_Desc(9)  ='EMIS'
    EDR_Desc(10) ='REFL'
    EDR_Desc(11) ='WINDSP'
    EDR_Desc(12) ='TSKIN'

    EDR_Length(1)  =nLay
    EDR_Length(2)  =nLay
    EDR_Length(3)  =nLay
    EDR_Length(4)  =nLay
    EDR_Length(5)  =nLay
    EDR_Length(6)  =nLay
    EDR_Length(7)  =nLay
    EDR_Length(8)  =nLay
    EDR_Length(9)  =nchan
    EDR_Length(10) =nchan
    EDR_Length(11) =1
    EDR_Length(12) =1
    call DeterminIndx(nEDRs,EDR_Length,EDR_Indx)
    nG=SUM(EDR_Length(1:nEDRs))
    RETURN
  END SUBROUTINE SetUpIndex

!===============================================================
! Name:         DeterminIndx
!
!
! Type:         Subroutine
!
!
! Description:  From the lengths, determine the indexes of the 
!               EDRs within a consolidated vector Xg.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nEDRs              I              Number of EDRs
!       - EDRlgth            I              Vector of lengths of each EDR
!       - EDRindx            O              Vector of indexes (within Xg)
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

  SUBROUTINE DeterminIndx(nEDRs,EDRlgth,EDRindx)
    INTEGER, DIMENSION(:) :: EDRlgth,EDRindx
    INTEGER               :: nEDRs
    !---Local variable(s)
    INTEGER               :: i
    EDRindx(1) = 1
    DO i=2,nEDRs
       EDRindx(i)=EDRindx(i-1)+EDRlgth(i-1)
    ENDDO
    RETURN
  END SUBROUTINE DeterminIndx


!===============================================================
! Name:         DestroyScene
!
!
! Type:         Subroutine
!
!
! Description:  Deallocate scene
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene              I/O            Structure to be deallocated
!
!
! Modules needed:
!       - None
!
!
! History:
!       07-22-2008      Wanchun @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE DestroyScene(Scene)
    TYPE(Scene_type)   :: Scene
    DEALLOCATE(Scene%AbsorbID, Scene%CentrFreq,  &
               Scene%polarity, Scene%Absorb_lay, &
               Scene%pres_lay, Scene%Temp_lay,   &
               Scene%Clw,      Scene%Rain,       &
               Scene%Snow,     Scene%Ice,        &
               Scene%Graupel,  Scene%Emiss,      &
               Scene%Refl,     Scene%pres_lev,   &
               Scene%qc,       Scene%YFwd,       &
               Scene%ChanSel,  Scene%Ym,         &
               Scene%YmCorr)
  END SUBROUTINE DestroyScene


!===============================================================
! Name:        FillSceneNegValues
!
!
! Type:        Subroutine
!
!
! Description:  Check Scene profile variables for negative values
!               and fill with profile values at top and/or bottom 
!               with lowest/heighest layer with non-negative values, respectively
!
! Arguments:
!
!           Name            Type           Description
!      ---------------------------------------------------
!       - SceneIn            I             Original Scene
!       - SceneOut           O             Output Scene with missing values replaced
!
!
! Limitation: Will not search for and fill negative values in interior of profile
!             (i.e. layers bounded at top and bottom by non-negative values)
!             Also, assumes that SceneIn has already been copied into SceneOut
!             to initialize before calling.
!
! Modules needed:
!       - None
!
!
! History:
!       09-16-2011      C.Grassotti, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE FillSceneNegValues(SceneIn, SceneOut)

    TYPE(Scene_type)              :: SceneIn, SceneOut

    INTEGER                       :: iLay, iTopLay, iBotLay

    !---Search the following scene profile variables:
    !   Temp_Lay (temperature)
    !   Absorb_Lay (water vapor)
    !   CLW (cloud liquid water)
    !   Rain (rain water)
    !   Graupel (frozen water)

    !---Temp_Lay (temperature)
    IF((ANY(SceneIn%Temp_lay(1:SceneIn%nLay) .le. 0.))) then
       !---Check top of profile
       call DeterminTopLayerFill(SceneIn%Temp_Lay,SceneIn%nLay,iTopLay,0)
       IF(iTopLay .ne. 1)then
          DO iLay=1,iTopLay-1
             SceneOut%Temp_Lay(iLay)=SceneOut%Temp_Lay(iTopLay)
          ENDDO
       ENDIF
       !---Check bottom of profile
       call DeterminBotLayerFill(SceneIn%Temp_Lay,SceneIn%nLay,iBotLay,0)
       IF(iBotLay .ne. 1)then
!       TprofLoop: do iLay=SceneIn%nLay,iBotLay+1,-1
!          if(SceneIn%Temp_Lay(iLay) .gt. 0.)then
!             SceneOut%Temp_Lay(iLay)=SceneOut%Temp_Lay(iLay-1)
!
!          endif
!       enddo TprofLoop
          do iLay=SceneIn%nLay,iBotLay+1,-1
             SceneOut%Temp_Lay(iLay)=SceneOut%Temp_Lay(iBotLay)
          enddo
       ENDIF
          

    ENDIF
    !---Recheck output scene for any remaining negative values
    IF((ANY(SceneOut%Temp_lay(1:SceneIn%nLay) .le. 0.))) then
       Call DeterminTopLayerFill(SceneOut%Temp_Lay,SceneOut%nLay,iTopLay,0)
       Call DeterminBotLayerFill(SceneOut%Temp_Lay,SceneOut%nLay,iBotLay,0)
!       CALL ErrHandl(WarningType,Err_NegValues,'(Scene file Temp)')
       print *,'WARNING: Negative values in Scene File Temp after filling top and/or bottom.'// &
               ' top and bottom layer values: ', iTopLay,iBotLay
    ENDIF


    !---Absorb_Lay (water vapor)
    IF((ANY(SceneIn%Absorb_lay(1:SceneIn%nLay,SceneIn%iH2O) .lt. 0.))) then
       !---Check top of profile
       call DeterminTopLayerFill(SceneIn%Absorb_lay(:,SceneIn%iH2O),SceneIn%nLay,iTopLay,1)
       IF(iTopLay .ne. 1)then
          DO iLay=1,iTopLay-1
             SceneOut%Absorb_lay(iLay,SceneIn%iH2O)=SceneOut%Absorb_lay(iTopLay,SceneIn%iH2O)
          ENDDO
       ENDIF
       !---Check bottom of profile
       call DeterminBotLayerFill(SceneIn%Absorb_lay(:,SceneIn%iH2O),SceneIn%nLay,iBotLay,1)
       IF(iBotLay .ne. 1)then
          do iLay=SceneIn%nLay,iBotLay+1,-1
             SceneOut%Absorb_lay(iLay,SceneIn%iH2O)=SceneOut%Absorb_lay(iBotLay,SceneIn%iH2O)
          enddo
       ENDIF
          

    ENDIF
    !---Recheck output scene for any remaining negative values
    IF((ANY(SceneOut%Absorb_lay(1:SceneOut%nLay,SceneIn%iH2O) .lt. 0.))) then
       Call DeterminTopLayerFill(SceneOut%Absorb_lay(:,SceneIn%iH2O),SceneOut%nLay,iTopLay,1)
       Call DeterminBotLayerFill(SceneOut%Absorb_lay(:,SceneIn%iH2O),SceneOut%nLay,iBotLay,1)
       print *,'WARNING: Negative values in Scene File Water Vapor after filling top and/or bottom.'// &
               ' top and bottom layer values: ',iTopLay,iBotLay
    ENDIF

    !---CLW
    IF((ANY(SceneIn%CLW(1:SceneIn%nLay) .lt. 0.))) then
       !---Check top of profile
       call DeterminTopLayerFill(SceneIn%CLW,SceneIn%nLay,iTopLay,1)
       IF(iTopLay .ne. 1)then
          DO iLay=1,iTopLay-1
             SceneOut%CLW(iLay)=SceneOut%CLW(iTopLay)
          ENDDO
       ENDIF
       !---Check bottom of profile
       call DeterminBotLayerFill(SceneIn%CLW,SceneIn%nLay,iBotLay,1)
       IF(iBotLay .ne. 1)then
          do iLay=SceneIn%nLay,iBotLay+1,-1
             SceneOut%CLW(iLay)=SceneOut%CLW(iBotLay)
          enddo
       ENDIF
          

    ENDIF
    !---Recheck output scene for any remaining negative values
    IF((ANY(SceneOut%CLW(1:SceneOut%nLay) .lt. 0.))) then
       Call DeterminTopLayerFill(SceneOut%CLW,SceneOut%nLay,iTopLay,1)
       Call DeterminBotLayerFill(SceneOut%CLW,SceneOut%nLay,iBotLay,1)
       print *,'WARNING: Negative values in Scene File CLW after filling top and/or bottom.'// &
               ' top and bottom layer values: ',iTopLay,iBotLay
    ENDIF

     !---Rain
    IF((ANY(SceneIn%Rain(1:SceneIn%nLay) .lt. 0.))) then
       !---Check top of profile
       call DeterminTopLayerFill(SceneIn%Rain,SceneIn%nLay,iTopLay,1)
       IF(iTopLay .ne. 1)then
          DO iLay=1,iTopLay-1
             SceneOut%Rain(iLay)=SceneOut%Rain(iTopLay)
          ENDDO
       ENDIF
       !---Check bottom of profile
       call DeterminBotLayerFill(SceneIn%Rain,SceneIn%nLay,iBotLay,1)
       IF(iBotLay .ne. 1)then
          do iLay=SceneIn%nLay,iBotLay+1,-1
             SceneOut%Rain(iLay)=SceneOut%Rain(iBotLay)
          enddo
       ENDIF
          

    ENDIF
    !---Recheck output scene for any remaining negative values
    IF((ANY(SceneOut%Rain(1:SceneOut%nLay) .lt. 0.))) then
       Call DeterminTopLayerFill(SceneOut%Rain,SceneOut%nLay,iTopLay,1)
       Call DeterminBotLayerFill(SceneOut%Rain,SceneOut%nLay,iBotLay,1)
       print *,'WARNING: Negative values in Scene File Rain after filling top and/or bottom.'// &
               ' top and bottom layer values: ',iTopLay,iBotLay
    ENDIF
  
     !---Graupel
    IF((ANY(SceneIn%Graupel(1:SceneIn%nLay) .lt. 0.))) then
       !---Check top of profile
       call DeterminTopLayerFill(SceneIn%Graupel,SceneIn%nLay,iTopLay,1)
       IF(iTopLay .ne. 1)then
          DO iLay=1,iTopLay-1
             SceneOut%Graupel(iLay)=SceneOut%Graupel(iTopLay)
          ENDDO
       ENDIF
       !---Check bottom of profile
       call DeterminBotLayerFill(SceneIn%Graupel,SceneIn%nLay,iBotLay,1)
       IF(iBotLay .ne. 1)then
          do iLay=SceneIn%nLay,iBotLay+1,-1
             SceneOut%Graupel(iLay)=SceneOut%Graupel(iBotLay)
          enddo
       ENDIF
          

    ENDIF
    !---Recheck output scene for any remaining negative values
    IF((ANY(SceneOut%Graupel(1:SceneOut%nLay) .lt. 0.))) then
       Call DeterminTopLayerFill(SceneOut%Graupel,SceneOut%nLay,iTopLay,1)
       Call DeterminBotLayerFill(SceneOut%Graupel,SceneOut%nLay,iBotLay,1)
       print *,'WARNING: Negative values in Scene File Graupel after filling top and/or bottom.'// &
               ' top and bottom layer values: ',iTopLay,iBotLay
    ENDIF

    RETURN



  END SUBROUTINE FillSceneNegValues




END MODULE IO_Scene
