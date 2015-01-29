!
! RadDiag_Stats_Define
!
! Module defining the RadDiag_Stats structure
! and containing routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Mar-2006
!                       paul.vandelst@noaa.gov
! 
! 18 June 2010  Replace scan_data and time_data with scan_mean, scan_stddev,
!                  time_mean and time_stddev.    A. Collard.
!  1 July 2010  Add channel-only stats (i.e., not split into scan pos or time)
!                                                A. Collard.
!
MODULE RadDiag_Stats_Define

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
  PUBLIC :: ISCAN 
  PUBLIC :: ICONST
  PUBLIC :: IANGLE
  PUBLIC :: ILPSR 
  PUBLIC :: ILPSR2
  PUBLIC :: ICLW
  PUBLIC :: VNSL
  ! Datatypes
  PUBLIC :: RadDiag_Stats_type
  ! Procedures
  PUBLIC :: RadDiag_Stats_Associated
  PUBLIC :: RadDiag_Stats_Destroy
  PUBLIC :: RadDiag_Stats_Create
  PUBLIC :: RadDiag_Stats_Inspect
  PUBLIC :: RadDiag_Stats_ValidRelease
  PUBLIC :: RadDiag_Stats_Info
  PUBLIC :: RadDiag_Stats_DefineVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: INVALID_FOV = -1
  INTEGER, PARAMETER :: N_VARIABLES = 8
  INTEGER, PARAMETER :: IBC    = 1
  INTEGER, PARAMETER :: INBC   = 2
  INTEGER, PARAMETER :: ISCAN  = 3
  INTEGER, PARAMETER :: ICONST = 4
  INTEGER, PARAMETER :: IANGLE = 5
  INTEGER, PARAMETER :: ILPSR  = 6
  INTEGER, PARAMETER :: ILPSR2 = 7
  INTEGER, PARAMETER :: ICLW   = 8
  CHARACTER(*), PARAMETER :: VARIABLENAMES(N_VARIABLES) = &
    (/ 'Obs-Calc dTb [Bias Corrected]          ', &
       'Obs-Calc dTb [NOT Bias Corrected]      ', &
       'SatBias Angle term                     ', &
       'SatBias AirMass Constant term          ', &
       'SatBias AirMass Angle term             ', &
       'SatBias AirMass Lapse Rate term        ', &
       'SatBias AirMass (Lapse Late)^2 term    ', &
       'SatBias AirMass Cloud Liquid Water term' /)
  INTEGER, PARAMETER :: VNSL = 80
  ! Literal constants
  REAL, PARAMETER :: ZERO = 0.0_sp
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
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
  TYPE :: RadDiag_Stats_type
    ! Release and version information
    INTEGER :: Release = RADDIAG_STATS_RELEASE
    INTEGER :: Version = RADDIAG_STATS_VERSION
    ! Dimensions
    INTEGER :: n_Predictors = 0  ! I
    INTEGER :: n_Channels   = 0  ! L
    INTEGER :: n_FOVs       = 0  ! Is
    INTEGER :: n_Times      = 0  ! It
    INTEGER :: n_Variables  = 0  ! N
    ! Sensor Ids
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ! The variable names
    CHARACTER(VNSL), ALLOCATABLE :: VariableNames(:)  ! N
    ! The channel numbers
    INTEGER,  ALLOCATABLE :: Channel(:)  ! L
    ! The Air Mass bias correction coefficients
    REAL(sp), ALLOCATABLE :: AirMassCoefficients(:,:,:)  ! I x L x It
    ! Scan position statistics
    INTEGER,  ALLOCATABLE :: FOV(:)              ! Is
    REAL(sp), ALLOCATABLE :: scan_Mean(:,:,:)    ! N x L x Is
    REAL(sp), ALLOCATABLE :: scan_StdDev(:,:,:)  ! N x L x Is
    INTEGER,  ALLOCATABLE :: scan_nSamples(:,:)  ! L x Is
    ! Time series statistics
    INTEGER,  ALLOCATABLE :: DateTime(:)         ! It
    REAL(sp), ALLOCATABLE :: time_Mean(:,:,:)    ! N x L x It
    REAL(sp), ALLOCATABLE :: time_StdDev(:,:,:)  ! N x L x It
    INTEGER,  ALLOCATABLE :: time_nSamples(:,:)  ! L x It
    ! Channel only statistics
    REAL(sp), ALLOCATABLE :: Mean(:,:)           ! N x L 
    REAL(sp), ALLOCATABLE :: StdDev(:,:)         ! N x L 
    INTEGER,  ALLOCATABLE :: nSamples(:)         ! L 
  END TYPE RadDiag_Stats_type
  !:tdoc-:


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RadDiag_Stats_Associated
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
!                        TYPE:       RadDiag_Stats_type
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

  FUNCTION RadDiag_Stats_Associated( RadDiag_Stats ) RESULT( Status )
    ! Arguments
    TYPE(RadDiag_Stats_type), INTENT(IN) :: RadDiag_Stats
    ! Function result
    LOGICAL :: Status

    ! Test the structure members
    Status = &
      ALLOCATED( RadDiag_Stats%VariableNames       ) .OR. &
      ALLOCATED( RadDiag_Stats%Channel             ) .OR. &
      ALLOCATED( RadDiag_Stats%AirMassCoefficients ) .OR. &
      ALLOCATED( RadDiag_Stats%FOV                 ) .OR. &
      ALLOCATED( RadDiag_Stats%scan_Mean           ) .OR. &
      ALLOCATED( RadDiag_Stats%scan_StdDev         ) .OR. &
      ALLOCATED( RadDiag_Stats%scan_nSamples       ) .OR. &
      ALLOCATED( RadDiag_Stats%DateTime            ) .OR. &
      ALLOCATED( RadDiag_Stats%time_Mean           ) .OR. &
      ALLOCATED( RadDiag_Stats%time_StdDev         ) .OR. &
      ALLOCATED( RadDiag_Stats%time_nSamples       ) .OR. &
      ALLOCATED( RadDiag_Stats%Mean                ) .OR. &
      ALLOCATED( RadDiag_Stats%StdDev              ) .OR. &
      ALLOCATED( RadDiag_Stats%nSamples            )

  END FUNCTION RadDiag_Stats_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_Destroy
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
!                       TYPE:       RadDiag_Stats_type
!                       DIMENSION:  Scalar OR any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Stats_Destroy( RadDiag_Stats )
    TYPE(RadDiag_Stats_type), INTENT(OUT) :: RadDiag_Stats
  END SUBROUTINE RadDiag_Stats_Destroy
  


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the RadDiag_Stats object.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_Create( RadDiag_Stats, &
!                                  n_Predictors , &
!                                  n_Channels   , &
!                                  n_FOVs       , &
!                                  n_Times        )
!
! OBJECTS:
!       RadDiag_Stats:   RadDiag_Stats structure.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Stats_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Predictors:    Predictor dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!       n_Channels:      Channel dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!       n_FOVs:          Field-of-view dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!       n_Times:         Time dimension of RadDiag_Stats structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Stats object
!                        ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Stats_Create( &
    RadDiag_Stats, &  ! Output
    n_Predictors , &  ! Input
    n_Channels   , &  ! Input
    n_FOVs       , &  ! Input
    n_Times        )  ! Input
    ! Arguments
    TYPE(RadDiag_Stats_type), INTENT(OUT) :: RadDiag_Stats
    INTEGER,                  INTENT(IN)  :: n_Predictors
    INTEGER,                  INTENT(IN)  :: n_Channels
    INTEGER,                  INTENT(IN)  :: n_FOVs
    INTEGER,                  INTENT(IN)  :: n_Times
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Predictors < 1 .OR. &
         n_Channels   < 1 .OR. &
         n_FOVs       < 1 .OR. &
         n_Times      < 1      ) RETURN

    ! Perform the allocation
    ALLOCATE( &
      RadDiag_Stats%VariableNames(N_VARIABLES), &
      RadDiag_Stats%Channel(n_Channels), &
      RadDiag_Stats%AirMassCoefficients(n_Predictors,n_Channels,n_Times), &
      RadDiag_Stats%FOV(n_FOVs), &
      RadDiag_Stats%scan_Mean(N_VARIABLES,n_Channels,n_FOVs), &
      RadDiag_Stats%scan_StdDev(N_VARIABLES,n_Channels,n_FOVs), &
      RadDiag_Stats%scan_nSamples(n_Channels,n_FOVs), &
      RadDiag_Stats%DateTime(n_Times), &
      RadDiag_Stats%time_Mean(N_VARIABLES,n_Channels,n_Times), &
      RadDiag_Stats%time_StdDev(N_VARIABLES,n_Channels,n_Times), &
      RadDiag_Stats%time_nSamples(n_Channels,n_Times), &
      RadDiag_Stats%Mean(N_VARIABLES,n_Channels), &
      RadDiag_Stats%StdDev(N_VARIABLES,n_Channels), &
      RadDiag_Stats%nSamples(n_Channels), &
      STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    RadDiag_Stats%n_Predictors = n_Predictors
    RadDiag_Stats%n_Channels   = n_Channels
    RadDiag_Stats%n_FOVs       = n_FOVs
    RadDiag_Stats%n_Times      = n_Times
    RadDiag_Stats%n_Variables  = N_VARIABLES
    ! ...Arrays
    RadDiag_Stats%VariableNames       = VARIABLENAMES
    RadDiag_Stats%AirMassCoefficients = ZERO
    RadDiag_Stats%Channel             = 0
    RadDiag_Stats%FOV                 = 0
    RadDiag_Stats%scan_Mean           = ZERO
    RadDiag_Stats%scan_StdDev         = ZERO
    RadDiag_Stats%scan_nSamples       = ZERO
    RadDiag_Stats%DateTime            = ZERO
    RadDiag_Stats%time_Mean           = ZERO
    RadDiag_Stats%time_StdDev         = ZERO
    RadDiag_Stats%time_nSamples       = ZERO
    RadDiag_Stats%Mean                = ZERO
    RadDiag_Stats%StdDev              = ZERO
    RadDiag_Stats%nSamples            = ZERO

  END SUBROUTINE RadDiag_Stats_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a RadDiag_Stats object to stdout.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Stats_Inspect( rds )
!
! INPUTS:
!       rds:    RadDiag_Stats object to display.
!               UNITS:      N/A
!               TYPE:       RadDiag_Stats_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_Stats_Inspect( rds )
    TYPE(RadDiag_Stats_type), INTENT(IN) :: rds
    INTEGER :: i, j

    WRITE(*, '(1x,"RADDIAG_STATS OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Predictors:",1x,i0)') rds%n_Predictors
    WRITE(*, '(3x,"n_Channels  :",1x,i0)') rds%n_Channels  
    WRITE(*, '(3x,"n_FOVs      :",1x,i0)') rds%n_FOVs      
    WRITE(*, '(3x,"n_Times     :",1x,i0)') rds%n_Times     
    WRITE(*, '(3x,"n_Variables :",1x,i0)') rds%n_Variables 
    IF ( .NOT. RadDiag_Stats_Associated(rds) ) RETURN
    ! Variable names
    WRITE(*, '(3x,"VariableNames :")')
    DO i = 1, rds%n_Variables
      WRITE(*, '(7x,i2,") ",a)') i, rds%VariableNames(i)
    END DO
    ! Channel number information
    WRITE(*, '(3x,"Channel:")') 
    WRITE(*, '(10(1x,i5,:))') rds%Channel
    ! Air mass bias correction coefficients
    WRITE(*, '(3x,"AirMassCoefficients:")')
    DO j = 1, rds%n_Times
      WRITE(*, '(5x,"Date/Time: ",i0)') rds%DateTime(j) 
      DO i = 1, rds%n_Channels
        WRITE(*, '(7x,"Channel: ",i0)') rds%Channel(i) 
        WRITE(*, '(5(1x,es13.6,:))') rds%AirMassCoefficients(:,i,j)
      END DO
    END DO
    ! Scan position statistics
    WRITE(*, '(3x,"Scan position means:")')
    DO j = 1, rds%n_FOVs
      WRITE(*, '(5x,"FOV: ",i0)') rds%FOV(j) 
      DO i = 1, rds%n_Channels
        WRITE(*, '(7x,"Channel: ",i0,"; n_Samples: ",i0)') rds%Channel(i), rds%scan_nSamples(i,j)
        WRITE(*, '(5(1x,es13.6,:))') rds%scan_Mean(:,i,j)
      END DO
    END DO
    WRITE(*, '(3x,"Scan position standard deviations:")')
    DO j = 1, rds%n_FOVs
      WRITE(*, '(5x,"FOV: ",i0)') rds%FOV(j) 
      DO i = 1, rds%n_Channels
        WRITE(*, '(7x,"Channel: ",i0,"; n_Samples: ",i0)') rds%Channel(i), rds%scan_nSamples(i,j)
        WRITE(*, '(5(1x,es13.6,:))') rds%scan_StdDev(:,i,j)
      END DO
    END DO
    ! Time series statistics
    WRITE(*, '(3x,"Time series Means:")')
    DO j = 1, rds%n_Times
      WRITE(*, '(5x,"Date/Time: ",i0)') rds%DateTime(j) 
      DO i = 1, rds%n_Channels
        WRITE(*, '(7x,"Channel: ",i0,"; n_Samples: ",i0)') rds%Channel(i), rds%time_nSamples(i,j)
        WRITE(*, '(5(1x,es13.6,:))') rds%time_Mean(:,i,j)
      END DO
    END DO
    WRITE(*, '(3x,"Time series Standard Deviations:")')
    DO j = 1, rds%n_Times
      WRITE(*, '(5x,"Date/Time: ",i0)') rds%DateTime(j) 
      DO i = 1, rds%n_Channels
        WRITE(*, '(7x,"Channel: ",i0,"; n_Samples: ",i0)') rds%Channel(i), rds%time_nSamples(i,j)
        WRITE(*, '(5(1x,es13.6,:))') rds%time_StdDev(:,i,j)
      END DO
    END DO
    ! Channel only statistics
    WRITE(*, '(3x,"Channel Means:")')
    WRITE(*, '(5x,"Date/Time: ",i0)') rds%DateTime(j) 
    DO i = 1, rds%n_Channels
       WRITE(*, '(7x,"Channel: ",i0,"; n_Samples: ",i0)') rds%Channel(i), rds%nSamples(i)
       WRITE(*, '(5(1x,es13.6,:))') rds%Mean(:,i)
    END DO
    WRITE(*, '(3x,"Channel Standard Deviations:")')
    WRITE(*, '(5x,"Date/Time: ",i0)') rds%DateTime(j) 
    DO i = 1, rds%n_Channels
       WRITE(*, '(7x,"Channel: ",i0,"; n_Samples: ",i0)') rds%Channel(i), rds%nSamples(i)
       WRITE(*, '(5(1x,es13.6,:))') rds%StdDev(:,i)
    END DO
        
  END SUBROUTINE RadDiag_Stats_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_ValidRelease
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
!                      TYPE:       TYEP(RadDiag_Stats_type)
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

  FUNCTION RadDiag_Stats_ValidRelease( RadDiag_Stats ) RESULT( IsValid )
    ! Arguments
    TYPE(RadDiag_Stats_type), INTENT(IN)  :: RadDiag_Stats
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'RadDiag_Stats_ValidRelease'
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

  END FUNCTION RadDiag_Stats_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_Info
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
!                      TYPE:       RadDiag_Stats_type
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

  SUBROUTINE RadDiag_Stats_Info( RadDiag_Stats, Info )
    ! Arguments
    TYPE(RadDiag_Stats_type), INTENT(IN)  :: RadDiag_Stats
    CHARACTER(*)            , INTENT(OUT) :: Info
    ! Local parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
   WRITE( Long_String, &
          '(a,1x,"RadDiag_Stats RELEASE.VERSION: ",i2,".",i2.2,2x,&
          &"N_PREDICTORS=",i0,2x,&
          &"N_CHANNELS=",i0,2x,&
          &"N_FOVS=",i0,2x,&
          &"N_TIMES=",i0,2x,&
          &"N_VARIABLE=",i0 )' ) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          RadDiag_Stats%Release, RadDiag_Stats%Version, &
          RadDiag_Stats%n_Predictors, &
          RadDiag_Stats%n_Channels  , &
          RadDiag_Stats%n_FOVs      , &
          RadDiag_Stats%n_Times     , &
          RadDiag_Stats%n_Variables 
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE RadDiag_Stats_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Stats_DefineVersion
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

  SUBROUTINE RadDiag_Stats_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE RadDiag_Stats_DefineVersion

END MODULE RadDiag_Stats_Define
