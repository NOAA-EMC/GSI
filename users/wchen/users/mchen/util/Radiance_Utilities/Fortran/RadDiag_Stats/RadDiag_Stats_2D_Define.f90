!
! RadDiag_Stats_2D_Define
!
! Module defining the RadDiag_Stats structure
! and containing routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Mar-2006
!                       paul.vandelst@noaa.gov (RadDiag_Stats_2D_Define)
! 
! 13 July 2010  Version for lat-long plots started (using RadDiag_Stats_Define as a template).
!                                                                      Andrew Collard
!
MODULE RadDiag_Stats_2D_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: sp=>Single
  USE Message_Handler      , ONLY: FAILURE, SUCCESS, INFORMATION, Display_Message
  ! Disable implicit typing
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: INVALID_FOV 
  PUBLIC :: N_VARIABLES  
  PUBLIC :: IBC   
  PUBLIC :: INBC  
  PUBLIC :: VNSL
  ! Datatypes
  PUBLIC :: RadDiag_Stats_2D_type
  ! Procedures
  PUBLIC :: RadDiag_Stats_2D_Associated
  PUBLIC :: RadDiag_Stats_2D_Destroy
  PUBLIC :: RadDiag_Stats_2D_Create
  PUBLIC :: RadDiag_Stats_2D_Inspect
  PUBLIC :: RadDiag_Stats_2D_ValidRelease
  PUBLIC :: RadDiag_Stats_2D_Info
  PUBLIC :: RadDiag_Stats_2D_DefineVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: INVALID_FOV = -1
  INTEGER, PARAMETER :: N_VARIABLES = 2
  INTEGER, PARAMETER :: IBC    = 1
  INTEGER, PARAMETER :: INBC   = 2
  CHARACTER(*), PARAMETER :: VARIABLENAMES(N_VARIABLES) = &
    (/ 'Obs-Calc dTb [Bias Corrected]          ', &
       'Obs-Calc dTb [NOT Bias Corrected]      ' /)
  INTEGER, PARAMETER :: VNSL = 80
  ! Literal constants
  REAL, PARAMETER :: ZERO = 0.0_sp
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: RadDiag_Stats_2D_Define.f90 8661 2010-07-13 21:21:17Z andrew.collard@noaa.gov $'
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: RADDIAG_STATS_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: RADDIAG_STATS_VERSION = 1  ! This is just the data version for the release.
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! Sensor Id string length
  INTEGER, PARAMETER :: SL = 20
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  
  
  ! --------------------
  ! Structure definition
  ! --------------------
  !:tdoc+:
  TYPE :: RadDiag_Stats_2D_type
    ! Release and version information
    INTEGER :: Release = RADDIAG_STATS_RELEASE
    INTEGER :: Version = RADDIAG_STATS_VERSION
    ! Dimensions
    INTEGER :: n_Channels    = 0  ! C
    INTEGER :: n_Longitudes  = 0  ! L
    INTEGER :: n_Latitudes   = 0  ! M
    INTEGER :: n_Variables   = 0  ! N
  ! Sensor Ids
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ! The variable names
    CHARACTER(VNSL), ALLOCATABLE :: VariableNames(:)  ! N
    ! The channel numbers
    INTEGER,  ALLOCATABLE :: Channel(:)  ! C
    ! The latitudes and longtitudes
    INTEGER,  ALLOCATABLE :: Longitude(:)  ! L
    INTEGER,  ALLOCATABLE :: Latitude(:)   ! M
    ! Radiance Statistics
    REAL(sp), ALLOCATABLE :: Mean(:,:,:,:)    ! N x C x L x M
    REAL(sp), ALLOCATABLE :: StdDev(:,:,:,:)  ! N x C x L x M
    INTEGER,  ALLOCATABLE :: nSamples(:,:,:)  ! C x L x M
  END TYPE RadDiag_Stats_2D_type
  !:tdoc-:


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RadDiag_Stats_2D_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a RadDiag_Stats object.
!
! CALLING SEQUENCE:
!       Status = RadDiag_Stats_Associated( RadDiag_Stats )
!
! OBJECTS:
!       RadDiag_Stats:   RadDiag_Stats structure which is to have its
!                        member's status tested.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Stats_2D_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the RadDiag_Stats members.
!                .TRUE.  - if ANY of the RadDiag_Stats allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the RadDiag_Stats allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input RadDiag_Stats argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION RadDiag_Stats_2D_Associated( RadDiag_Stats ) RESULT( Status )
    ! Arguments
    TYPE(RadDiag_Stats_2D_type), INTENT(IN) :: RadDiag_Stats
    ! Function result
    LOGICAL :: Status

    ! Test the structure members
    Status = &
      ALLOCATED( RadDiag_Stats%VariableNames       ) .OR. &
      ALLOCATED( RadDiag_Stats%Channel             ) .OR. &
      ALLOCATED( RadDiag_Stats%Latitude            ) .OR. &
      ALLOCATED( RadDiag_Stats%Longitude           ) .OR. &
      ALLOCATED( RadDiag_Stats%Mean                ) .OR. &
      ALLOCATED( RadDiag_Stats%StdDev              ) .OR. &
      ALLOCATED( RadDiag_Stats%nSamples            )

  END FUNCTION RadDiag_Stats_2D_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize RadDiag_Stats objects.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_Destroy( RadDiag_Stats )
!
! OBJECTS:
!       RadDiag_Stats:  Re-initialized RadDiag_Stats structure.
!                       UNITS:      N/A
!                       TYPE:       RadDiag_Stats_2D_type
!                       DIMENSION:  Scalar OR any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Stats_2D_Destroy( RadDiag_Stats )
    TYPE(RadDiag_Stats_2D_type), INTENT(OUT) :: RadDiag_Stats
  END SUBROUTINE RadDiag_Stats_2D_Destroy
  


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the RadDiag_Stats object.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_Create( RadDiag_Stats, &
!                                  n_Channels   , &
!                                  n_Latitudes  , &
!                                  n_Longitudes )
!
! OBJECTS:
!       RadDiag_Stats:   RadDiag_Stats structure.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Stats_2D_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:      Channel dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!       n_Latitudes:     Latitude dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!       n_Longitudes:    Longitude dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Stats_2D_Create( &
    RadDiag_Stats, &  ! Output
    n_Channels   , &  ! Input
    n_Latitudes  , &  ! Input
    n_Longitudes )    ! Input
    ! Arguments
    TYPE(RadDiag_Stats_2D_type), INTENT(OUT) :: RadDiag_Stats
    INTEGER,                     INTENT(IN)  :: n_Channels
    INTEGER,                     INTENT(IN)  :: n_Latitudes
    INTEGER,                     INTENT(IN)  :: n_Longitudes
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels   < 1 .OR. &
         n_Latitudes  < 1 .OR. &
         n_Longitudes < 1      ) RETURN

    ! Perform the allocation
    ALLOCATE( &
      RadDiag_Stats%VariableNames(N_VARIABLES), &
      RadDiag_Stats%Channel(n_Channels), &
      RadDiag_Stats%Latitude(n_Latitudes), &
      RadDiag_Stats%Longitude(n_Longitudes), &
      RadDiag_Stats%Mean(N_VARIABLES,n_Channels,n_Longitudes,n_Latitudes), &
      RadDiag_Stats%StdDev(N_VARIABLES,n_Channels,n_Longitudes,n_Latitudes), &
      RadDiag_Stats%nSamples(n_Channels,n_Longitudes,n_Latitudes), &
      STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    RadDiag_Stats%n_Channels   = n_Channels
    RadDiag_Stats%n_Latitudes  = n_Latitudes
    RadDiag_Stats%n_Longitudes = n_Longitudes
    RadDiag_Stats%n_Variables  = N_VARIABLES
    ! ...Arrays
    RadDiag_Stats%VariableNames       = VARIABLENAMES
    RadDiag_Stats%Channel             = 0
    RadDiag_Stats%Latitude            = ZERO
    RadDiag_Stats%Longitude           = ZERO
    RadDiag_Stats%Mean                = ZERO
    RadDiag_Stats%StdDev              = ZERO
    RadDiag_Stats%nSamples            = 0
  END SUBROUTINE RadDiag_Stats_2D_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a RadDiag_Stats object to stdout.
!       CUrrently only limited information is output.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_Inspect( rds )
!
! INPUTS:
!       rds:    RadDiag_Stats object to display.
!               UNITS:      N/A
!               TYPE:       RadDiag_Stats_2D_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_Stats_2D_Inspect( rds )
    TYPE(RadDiag_Stats_2D_type), INTENT(IN) :: rds
    INTEGER :: i, j

    WRITE(*, '(1x,"RADDIAG_STATS OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Channels  :",1x,i0)') rds%n_Channels  
    WRITE(*, '(3x,"n_Latitudes :",1x,i0)') rds%n_Latitudes      
    WRITE(*, '(3x,"n_Longitudes:",1x,i0)') rds%n_Longitudes  
    WRITE(*, '(3x,"n_Variables :",1x,i0)') rds%n_Variables 
    IF ( .NOT. RadDiag_Stats_2D_Associated(rds) ) RETURN
    ! Variable names
    WRITE(*, '(3x,"VariableNames :")')
    DO i = 1, rds%n_Variables
      WRITE(*, '(7x,i2,") ",a)') i, rds%VariableNames(i)
    END DO
    ! Channel number information
    WRITE(*, '(3x,"Channel:")') 
    WRITE(*, '(10(1x,i5,:))') rds%Channel
    ! Latitudes
    WRITE(*, '(3x,"Latitudes:")')
    DO j = 1, rds%n_Latitudes
        WRITE(*, '(5(1x,es13.6,:))') rds%Latitude(j)
    END DO
    ! Longitudes
    WRITE(*, '(3x,"Longitudes:")')
    DO j = 1, rds%n_Longitudes
        WRITE(*, '(5(1x,es13.6,:))') rds%Longitude(j)
    END DO
       
  END SUBROUTINE RadDiag_Stats_2D_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_ValidRelease
!
! PURPOSE:
!       Function to check the RadDiag_Stats Release value.
!
! CALLING SEQUENCE:
!       IsValid = RadDiag_Stats_ValidRelease( RadDiag_Stats )
!
! INPUTS:
!       RadDiag_Stats: RadDiag_Stats structure for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYEP(RadDiag_Stats_2D_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION RadDiag_Stats_2D_ValidRelease( RadDiag_Stats ) RESULT( IsValid )
    ! Arguments
    TYPE(RadDiag_Stats_2D_type), INTENT(IN)  :: RadDiag_Stats
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_2D_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.

    ! Check release is not too old
    IF ( RadDiag_Stats%Release < RADDIAG_STATS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A RadDiag_Stats data update is needed. ", &
                  &"RadDiag_Stats release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  RadDiag_Stats%Release, RADDIAG_STATS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( RadDiag_Stats%Release > RADDIAG_STATS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A RadDiag_Stats software update is needed. ", &
                  &"RadDiag_Stats release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  RadDiag_Stats%Release, RADDIAG_STATS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION RadDiag_Stats_2D_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the data structure.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_Info( RadDiag_Stats, Info )
!
! INPUTS:
!       RadDiag_Stats: Filled RadDiag_Stats structure.
!                      UNITS:      N/A
!                      TYPE:       RadDiag_Stats_2D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed RadDiag_Stats data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_Stats_2D_Info( RadDiag_Stats, Info )
    ! Arguments
    TYPE(RadDiag_Stats_2D_type), INTENT(IN)  :: RadDiag_Stats
    CHARACTER(*)            , INTENT(OUT) :: Info
    ! Local parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
   WRITE( Long_String, &
          '(a,1x,"RadDiag_Stats RELEASE.VERSION: ",i2,".",i2.2,2x,&
          &"N_CHANNELS=",i0,2x,&
          &"N_LATITUDES=",i0,2x,&
          &"N_LONGITUDES=",i0,2x,&
          &"N_VARIABLE=",i0 )' ) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          RadDiag_Stats%Release, RadDiag_Stats%Version, &
          RadDiag_Stats%n_Channels  , &
          RadDiag_Stats%n_Latitudes      , &
          RadDiag_Stats%n_Longitudes     , &
          RadDiag_Stats%n_Variables 
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE RadDiag_Stats_2D_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_2D_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_DefineVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_Stats_2D_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE RadDiag_Stats_2D_DefineVersion

END MODULE RadDiag_Stats_2D_Define
