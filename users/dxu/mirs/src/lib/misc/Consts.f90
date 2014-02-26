!$Id: Consts.f90 3340 2013-10-09 17:24:40Z amims $
!-----------------------------------------------------------------------------------------------
! Name:         Consts
! 
! Type:         F90 module
!
! Description:
!       Module that contains the many constants needed across applications.
!
! Modules needed:
!       - none
!
! Subroutines contained:
!       - Set2Default
!
! Data type included:
!       - STANDARD_GRAVITY
!       - SPEED_LIGHT
!       - MAXIM_SKIN_TEMP   
!       - MINIM_SKIN_TEMP   
!       - MAXIM_TPW   
!       - MINIM_TPW   
!       - MAXIM_ATM_TEMP    
!       - MINIM_ATM_TEMP    
!       - MAXIM_ATM_HUMID   
!       - MINIM_ATM_HUMID   
!       - DEFAULT_VALUE_INT 
!       - DEFAULT_VALUE_REAL
!       - DEFAULT_VALUE_STR4
!       - LabelColocFile    
!       - MW_H2O   
!       - MW_O3    
!       - MW_DRYAIR   
!       - DryAirConst   
!       - AVOGADRO_CONSTANT 
!       - PI  
!       - earthrad   
!       - satheight   
!       - OC_TYP   
!       - SEAICE_TYP   
!       - LD_TYP   
!       - SNOW_TYP   
!       - COAST_TYP   
!       - SPECULAR_SURFACE  
!       - LAMBERTIAN_SURFACE
!       - MAP_TYP_2_CRTM    
!       - MAP_CRTM_2_TYP
!       - MAXLAYS
!       - MAXCLOUDS
!       - MAXAEROSOL
!       - MAXABSORB
!       - mr2ppmv_o3
!
! History:
!    2006  S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE Consts
  IMPLICIT NONE
  !----Publicly available constants
  !-------------------------------------------------------------------------------------------------------------|
  !                                      Variable          |  Value        !  Unit     |   Description          |
  !-------------------------------------------------------------------------------------------------------------|
  REAL,                    PARAMETER :: STANDARD_GRAVITY   =  9.80665      !  m/s2     | Gravity acceleration   |
  REAL,                    PARAMETER :: SPEED_LIGHT        =  2.99792458e+8!  m/s      | Speed of light         |
  REAL,                    PARAMETER :: MAXIM_SKIN_TEMP    =  350.         !  Kelvin   | Max Valid Sfc Temper.  |
  REAL,                    PARAMETER :: MINIM_SKIN_TEMP    =  180.         !  Kelvin   | Min Valid Sfc Temper.  |
  REAL,                    PARAMETER :: MAXIM_TPW          =  100.         !  mm       | Max Valid TPW          |
  REAL,                    PARAMETER :: MINIM_TPW          =  0.           !  mm       | Min Valid TPW          |
  REAL,                    PARAMETER :: MAXIM_ATM_TEMP     =  350.         !  Kelvin   | Max Valid Atm Temper.  |
  REAL,                    PARAMETER :: MINIM_ATM_TEMP     =  100.         !  Kelvin   | Min Valid Atm Temper.  |
  REAL,                    PARAMETER :: MAXIM_ATM_HUMID    =  40.          !  g/Kg     | Max Valid Atm humd mr. |
  REAL,                    PARAMETER :: MINIM_ATM_HUMID    =  0.           !  g/Kg     | Min Valid Atm humd mr. |
  REAL,                    PARAMETER :: MAX_CLD4CLR        =  0.05         !  mm       | Max Cld Amt Clrsky     |
  INTEGER,                 PARAMETER :: DEFAULT_VALUE_INT  = -999          !   -       | Default integer value  |
  REAL,                    PARAMETER :: DEFAULT_VALUE_REAL = -999.         !   -       | Default real value     |
  CHARACTER(LEN=4),        PARAMETER :: DEFAULT_VALUE_STR4 = 'XXXX'        !   -       | Default string4 value  |
  CHARACTER(LEN=10),       PARAMETER :: LabelColocFile     = 'Coloc_File'  !   -       | Label of coloc files   |
  REAL,                    PARAMETER :: MW_H2O             = 18.01528      !  g/mol    | Molecul Weight of H2O  |
  REAL,                    PARAMETER :: MW_O3              = 47.99820      !  g/mol    | Molecul Weight of O3   |
  REAL,                    PARAMETER :: MW_DRYAIR          = 28.9648       !  g/mol    | Molec Weight of Dry Air|  
  REAL,                    PARAMETER :: DryAirConst        = 287.04        !  J/(kg.K) | Dry Air constant       |
  REAL,                    PARAMETER :: AVOGADRO_CONSTANT  = 6.02214199E+23!  mole^-1  | Avogadro constant      |
  REAL,                    PARAMETER :: PI                 = 3.14159       !   -       | Pi constant            |
  REAL,                    PARAMETER :: earthrad           = 6371.0        !  Km       | Earth Radius           |
  REAL,                    PARAMETER :: satheight          = 833.0         !  Km       | Satellite height       |
  REAL,                    PARAMETER :: epsilon            = 0.000001      !   -       | Epsilon (4 comparison) |
  REAL,                    PARAMETER :: epsilonLoose       = 0.02          !   -       | Epsilon (4 loose comp.)|
  INTEGER,                 PARAMETER :: SENSOR_ID_N18      = 1             !   -       | Sensor ID for N18      |
  INTEGER,                 PARAMETER :: SENSOR_ID_METOPA   = 2             !   -       | Sensor ID for Metop-A  |
  INTEGER,                 PARAMETER :: SENSOR_ID_F16      = 3             !   -       | Sensor ID for DMSP-F16 |
  INTEGER,                 PARAMETER :: SENSOR_ID_N19      = 4             !   -       | Sensor ID for N19      |
  INTEGER,                 PARAMETER :: SENSOR_ID_F18      = 5             !   -       | Sensor ID for DMSP-F18 |
  INTEGER,                 PARAMETER :: SENSOR_ID_NPP      = 6             !   -       | Sensor ID for NPP ATMS |
  INTEGER,                 PARAMETER :: SENSOR_ID_AMSRE    = 7             !   -       | Sensor ID for AMSRE    |
  INTEGER,                 PARAMETER :: SENSOR_ID_FY3RI    = 8             !   -       | Sensor ID for FY3RI    |
  INTEGER,                 PARAMETER :: SENSOR_ID_TRMM     = 9             !   -       | Sensor ID for TRMM/TMI |
  INTEGER,                 PARAMETER :: SENSOR_ID_GPM      = 10            !   -       | Sensor ID for GPM/GMI  |
  INTEGER,                 PARAMETER :: SENSOR_ID_TRMM2A12 = 11            !   -       | Sensor ID for TRMM_2A12|
  INTEGER,                 PARAMETER :: SENSOR_ID_MTMA     = 12            !   -       | Sensor ID for MEGHA-TROPIQUES/MADRAS|
  INTEGER,                 PARAMETER :: SENSOR_ID_MTSA     = 13            !   -       | Sensor ID for MEGHA-TROPIQUES/SAPHIR|
  INTEGER,                 PARAMETER :: SENSOR_ID_METOPB   = 14            !   -       | Sensor ID for Metop-B  | 
  INTEGER,                 PARAMETER :: SENSOR_ID_GCOMW1   = 15            !   -       | Sensor ID for GCOMW1/AMSR2| 
  INTEGER,                 PARAMETER :: SENSOR_ID_F17      = 18            !   -       | Sensor ID for DMSP-F17 |
  CHARACTER(LEN=3),        PARAMETER :: SATID_N18          = 'n18'         !   -       / String ID for N18      /
  CHARACTER(LEN=6),        PARAMETER :: SATID_METOPA       = 'metopA'      !   -       / String ID for METOPA   /
  CHARACTER(LEN=3),        PARAMETER :: SATID_F16          = 'f16'         !   -       / String ID for F16      /
  CHARACTER(LEN=3),        PARAMETER :: SATID_N19          = 'n19'         !   -       / String ID for N19      /
  CHARACTER(LEN=3),        PARAMETER :: SATID_F18          = 'f18'         !   -       / String ID for F18      /
  CHARACTER(LEN=3),        PARAMETER :: SATID_NPP          = 'npp'         !   -       / String ID for NPP      /
  CHARACTER(LEN=5),        PARAMETER :: SATID_AMSRE        = 'aqua'        !   -       / String ID for AMSRE    /
  CHARACTER(LEN=5),        PARAMETER :: SATID_FY3RI        = 'fy3ri'       !   -       / String ID for FY3RI    /
  CHARACTER(LEN=4),        PARAMETER :: SATID_TRMM         = 'trmm'        !   -       / String ID for TRMM/TMI /  
  CHARACTER(LEN=3),        PARAMETER :: SATID_GPM          = 'gpm'         !   -       / String ID for GPM/GMI  /
  CHARACTER(LEN=8),        PARAMETER :: SATID_TRMM2A12     = 'trmm2a12'    !   -       / String ID for TRMM_2A12/
  CHARACTER(LEN=4),        PARAMETER :: SATID_MTMA         = 'mtma'        !   -       / String ID for MEGHA-TROPIQUES/MADRAS/
  CHARACTER(LEN=4),        PARAMETER :: SATID_MTSA         = 'mtsa'        !   -       / String ID for MEGHA-TROPIQUES/SAPHIR/
  CHARACTER(LEN=6),        PARAMETER :: SATID_METOPB       = 'metopB'      !   -       / String ID for METOPB   /
  CHARACTER(LEN=6),        PARAMETER :: SATID_GCOMW1       = 'gcomw1'      !   -       / String ID for GCOMW1/AMSR2/
  CHARACTER(LEN=3),        PARAMETER :: SATID_F17          = 'f17'         !   -       / String ID for F17      /
  INTEGER,                 PARAMETER :: OC_TYP             = 0             !   -       | Ocean surface type     |
  INTEGER,                 PARAMETER :: SEAICE_TYP         = 1             !   -       | Sea-Ice surface type   |
  INTEGER,                 PARAMETER :: LD_TYP             = 2             !   -       | Land surface type      |
  INTEGER,                 PARAMETER :: SNOW_TYP           = 3             !   -       | Snow surface type      |
  INTEGER,                 PARAMETER :: Desert_TYP         = 4             !   -       | Desert surface type    |
  INTEGER,                 PARAMETER :: COAST_TYP          = 6             !   -       | Coast surface type     |
  INTEGER,                 PARAMETER :: SPECULAR_SURFACE   = 1             !   -       | Specular Sfc Type-CRTM |
  INTEGER,                 PARAMETER :: LAMBERTIAN_SURFACE = 0             !   -       | Non-Specular Sfc -CRTM |
  INTEGER, DIMENSION(0:6), PARAMETER :: MAP_TYP_2_CRTM     = (/1,24,11,3,-1,-1,21/)    ! Map Typ conv. 2 CRTM   |
  INTEGER, DIMENSION(24),  PARAMETER :: MAP_CRTM_2_TYP     = (/0,3,1,2,2,2,2,2,2,2,2,2,2,2,&
                                                               2,2,2,2,2,2,2,2,2,2/)   ! CRTM type->convention  |
  INTEGER,                 PARAMETER :: MAXLAYS            = 100           !   -       | CRTM Max # of layers   |
  INTEGER,                 PARAMETER :: MAXCLOUDS          = 5             !   -       | CRTM Max # of cld lays |
  INTEGER,                 PARAMETER :: MAXAEROSOL         = 1             !   -       | CRTM Max # aeros lays  |
  INTEGER,                 PARAMETER :: MAXABSORB          = 2             !   -       | CRTM Max # absorbers   |
  !--------------------------------------------------------------------------------------------
  !  Dependent constants
  !--------------------------------------------------------------------------------------------  
  REAL,    PARAMETER :: mr2ppmv_o3         = 1000.0*MW_DRYAIR / MW_O3
  !--------------------------------------------------------------------------------------------

CONTAINS

!===============================================================
! Name:         Set2Default
!
!
! Type:         Subroutine
!
!
! Description:  Sets to the default values certain integer, real
!               single values as well s vectors. These default values 
!               defined in the global section of the module.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - vec_Int            O              Integer vector to set to default values
!       - vec_Real           O              Real vector to set to default values
!       - singl_Int          O              Single integer variable to set
!       - singl_Real         O              Single real variable to set
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

  SUBROUTINE Set2Default(vec_Int,vec_Real,singl_Int,singl_Real)
    INTRINSIC                       :: present
    REAL, DIMENSION(:), OPTIONAL    :: vec_Real
    INTEGER, DIMENSION(:), OPTIONAL :: vec_Int
    INTEGER, OPTIONAL               :: singl_Int
    REAL, OPTIONAL                  :: singl_Real
    IF (present(vec_Int))    vec_Int(:)  = DEFAULT_VALUE_INT
    IF (present(vec_Real))   vec_Real(:) = DEFAULT_VALUE_REAL
    IF (present(singl_Int))  singl_Int   = DEFAULT_VALUE_INT
    IF (present(singl_Real)) singl_Real  = DEFAULT_VALUE_REAL
    RETURN
  END SUBROUTINE Set2Default


!===============================================================
! Name:         Set2DefaultQC
!
!
! Type:         Subroutine
!
!
! Description:  Sets to the default QC values certain integer, real
!               single values as well as vectors. These default values 
!               defined in the global section of the module.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - vec_Int            O              Integer vector to set to default values
!       - vec_Real           O              Real vector to set to default values
!       - singl_Int          O              Single integer variable to set
!       - singl_Real         O              Single real variable to set
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-02-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE Set2DefaultQC(vec_Int,vec_Real,singl_Int,singl_Real)
    INTRINSIC                            :: present
    REAL,          DIMENSION(:),OPTIONAL :: vec_Real
    INTEGER(2),    DIMENSION(:),OPTIONAL :: vec_Int
    INTEGER,                    OPTIONAL :: singl_Int
    REAL,                       OPTIONAL :: singl_Real
    IF (present(vec_Int))    vec_Int(:)  = DEFAULT_VALUE_INT
    IF (present(vec_Real))   vec_Real(:) = DEFAULT_VALUE_REAL
    IF (present(singl_Int))  singl_Int   = DEFAULT_VALUE_INT
    IF (present(singl_Real)) singl_Real  = DEFAULT_VALUE_REAL
    RETURN
  END SUBROUTINE Set2DefaultQC


END MODULE Consts
