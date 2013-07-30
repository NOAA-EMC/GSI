!
! RadDiag_Hdr_Define
!
! Module defining the RadDiag header structures
! and containing routines to manipulate them
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Mar-2006
!                       paul.vandelst@noaa.gov
!

MODULE RadDiag_Hdr_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,      ONLY: sp=>Single
  USE Message_Handler, ONLY: FAILURE, SUCCESS, INFORMATION, Display_Message
  ! Disable implicit typing
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: RadDiag_Hdr_Scalar_type
  PUBLIC :: RadDiag_Hdr_Channel_type
  PUBLIC :: RadDiag_Hdr_type
  ! Module subprograms
  PUBLIC :: RadDiag_Hdr_Associated
  PUBLIC :: RadDiag_Hdr_Destroy
  PUBLIC :: RadDiag_Hdr_Create
  PUBLIC :: RadDiag_Hdr_Inspect
  PUBLIC :: RadDiag_Hdr_DefineVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Literal constants
  REAL, PARAMETER :: ZERO = 0.0_sp
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'


  ! ---------------------------
  ! Header structure definition
  ! ---------------------------
  ! Scalar part of header
  TYPE :: RadDiag_Hdr_Scalar_type
    CHARACTER(20) :: isis    = ' '  ! sat and sensor type
    CHARACTER(10) :: id      = ' '  ! sat type
    CHARACTER(10) :: obstype = ' '  ! observation type
    INTEGER       :: jiter   = 0    ! outer loop counter
    INTEGER       :: nchan   = 0    ! number of channels in the sensor
    INTEGER       :: npred   = 0    ! number of updating bias correction predictors
    INTEGER       :: idate   = 0    ! time (yyyymmddhh)
    INTEGER       :: ireal   = 0    ! # of real elements in the fix part of a data record
    INTEGER       :: ipchan  = 0    ! # of elements for each channel except for bias correction terms
    INTEGER       :: iextra  = 0    ! # of extra elements for each channel
    INTEGER       :: jextra  = 0    ! # of extra elements
  END TYPE RadDiag_Hdr_Scalar_type

  ! Channel dependent part of header
  TYPE :: RadDiag_Hdr_Channel_type
    REAL(sp) :: freq     = ZERO  ! frequency (Hz)
    REAL(sp) :: polar    = ZERO  ! polarization
    REAL(sp) :: wave     = ZERO  ! wave number (cm^-1)
    REAL(sp) :: varch    = ZERO  ! error variance (or SD error?)
    REAL(sp) :: tlapmean = ZERO  ! mean lapse rate
    INTEGER  :: iuse     = -1    ! use flag
    INTEGER  :: nuchan   = -1    ! sensor relative channel number
    INTEGER  :: iochan   = -1    ! satinfo relative channel number
  END TYPE RadDiag_Hdr_Channel_type

  ! The complete header
  TYPE :: RadDiag_Hdr_type
    INTEGER :: n_Channels = 0  ! Structure dimensions
    TYPE(RadDiag_Hdr_Scalar_type) :: Scalar
    TYPE(RadDiag_Hdr_Channel_type), ALLOCATABLE :: Channel(:)
  END TYPE RadDiag_Hdr_type


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RadDiag_Hdr_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a RadDiag_Hdr object.
!
! CALLING SEQUENCE:
!       Status = RadDiag_Hdr_Associated( RadDiag_Hdr )
!
! OBJECTS:
!       RadDiag_Hdr:     RadDiag_Hdr structure which is to have its
!                        member's status tested.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Hdr_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the RadDiag_Hdr members.
!                .TRUE.  - if ANY of the RadDiag_Hdr allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the RadDiag_Hdr allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input RadDiag_Hdr argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION RadDiag_Hdr_Associated( RadDiag_Hdr ) RESULT( Status )
    ! Arguments
    TYPE(RadDiag_Hdr_type), INTENT(IN) :: RadDiag_Hdr
    ! Function result
    LOGICAL :: Status

    ! Test the structure members
    Status = ALLOCATED( RadDiag_Hdr%Channel )

  END FUNCTION RadDiag_Hdr_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Hdr_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize RadDiag_Hdr objects.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Hdr_Destroy( RadDiag_Hdr )
!
! OBJECTS:
!       RadDiag_Hdr:    Re-initialized RadDiag_Hdr structure.
!                       UNITS:      N/A
!                       TYPE:       RadDiag_Hdr_type
!                       DIMENSION:  Scalar OR any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Hdr_Destroy( RadDiag_Hdr )
    TYPE(RadDiag_Hdr_type), INTENT(OUT) :: RadDiag_Hdr
  END SUBROUTINE RadDiag_Hdr_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Hdr_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the RadDiag_Hdr object.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Hdr_Create( RadDiag_Hdr, n_Channels )
!
! OBJECTS:
!       RadDiag_Hdr:     RadDiag_Hdr structure.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Hdr_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:      Channel dimension of RadDiag_Hdr structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Hdr object
!                        ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Hdr_Create( &
    RadDiag_Hdr, &  ! Output
    n_Channels   )  ! Input
    ! Arguments
    TYPE(RadDiag_Hdr_type), INTENT(OUT) :: RadDiag_Hdr
    INTEGER,                INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( RadDiag_Hdr%Channel(n_Channels), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise dimensions
    RadDiag_Hdr%n_Channels = n_Channels

  END SUBROUTINE RadDiag_Hdr_Create
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Hdr_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a RadDiag_Hdr object to stdout.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Hdr_Inspect( rdh )
!
! INPUTS:
!       rdh:    RadDiag_Hdr object to display.
!               UNITS:      N/A
!               TYPE:       RadDiag_Hdr_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_Hdr_Inspect( rdh )
    TYPE(RadDiag_Hdr_type), INTENT(IN) :: rdh
    INTEGER :: i
    WRITE(*, '(1x,"RADDIAG_HDR OBJECT")')
    ! Scalar object
    CALL RadDiag_Hdr_Scalar_Inspect( rdh%Scalar )
    ! Channel object(s)
    IF ( .NOT. RadDiag_Hdr_Associated(rdh) ) RETURN
    DO i = 1, rdh%n_Channels
      WRITE(*, '(3x,"Channel index: ",i0)') i
      CALL RadDiag_Hdr_Channel_Inspect( rdh%Channel(i) )
    END DO
  END SUBROUTINE RadDiag_Hdr_Inspect
  
  SUBROUTINE RadDiag_Hdr_Scalar_Inspect( rdhs )
    TYPE(RadDiag_Hdr_Scalar_type), INTENT(IN) :: rdhs
    WRITE(*, '(3x,"Scalar Component")')
    WRITE(*, '(5x,"isis    :",1x,a)') TRIM(rdhs%isis)    
    WRITE(*, '(5x,"id      :",1x,a)') TRIM(rdhs%id)      
    WRITE(*, '(5x,"obstype :",1x,a)') TRIM(rdhs%obstype) 
    WRITE(*, '(5x,"jiter  :",1x,i0)') rdhs%jiter 
    WRITE(*, '(5x,"nchan  :",1x,i0)') rdhs%nchan 
    WRITE(*, '(5x,"npred  :",1x,i0)') rdhs%npred 
    WRITE(*, '(5x,"idate  :",1x,i0)') rdhs%idate 
    WRITE(*, '(5x,"ireal  :",1x,i0)') rdhs%ireal 
    WRITE(*, '(5x,"ipchan :",1x,i0)') rdhs%ipchan
    WRITE(*, '(5x,"iextra :",1x,i0)') rdhs%iextra
    WRITE(*, '(5x,"jextra :",1x,i0)') rdhs%jextra
  END SUBROUTINE RadDiag_Hdr_Scalar_Inspect
  
  SUBROUTINE RadDiag_Hdr_Channel_Inspect( rdhc )
    TYPE(RadDiag_Hdr_Channel_type), INTENT(IN) :: rdhc
    WRITE(*, '(3x,"Channel Component")')
    WRITE(*, '(5x,"freq     :",1x,es13.6)') rdhc%freq     
    WRITE(*, '(5x,"polar    :",1x,es13.6)') rdhc%polar    
    WRITE(*, '(5x,"wave     :",1x,es13.6)') rdhc%wave     
    WRITE(*, '(5x,"varch    :",1x,es13.6)') rdhc%varch    
    WRITE(*, '(5x,"tlapmean :",1x,es13.6)') rdhc%tlapmean
    WRITE(*, '(5x,"iuse     :",1x,i0)') rdhc%iuse   
    WRITE(*, '(5x,"nuchan   :",1x,i0)') rdhc%nuchan 
    WRITE(*, '(5x,"iochan   :",1x,i0)') rdhc%iochan 
  END SUBROUTINE RadDiag_Hdr_Channel_Inspect
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Hdr_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Hdr_DefineVersion( Id )
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

  SUBROUTINE RadDiag_Hdr_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE RadDiag_Hdr_DefineVersion

END MODULE RadDiag_Hdr_Define
