!
! RadDiag_Data_Define
!
! Module defining the RadDiag data structure
! and containing routines to manipulate them
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Mar-2006
!                       paul.vandelst@noaa.gov
!

MODULE RadDiag_Data_Define

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
  PRIVATE
  ! Parameters
  PUBLIC :: RADDIAG_N_FPELEMENTS
  PUBLIC :: RADDIAG_N_CHELEMENTS
  PUBLIC :: RADDIAG_N_PRELEMENTS
  ! Datatypes
  PUBLIC :: RadDiag_Data_Scalar_type
  PUBLIC :: RadDiag_Data_Channel_type
  PUBLIC :: RadDiag_Data_type
  ! Procedures
  PUBLIC :: RadDiag_Data_Associated
  PUBLIC :: RadDiag_Data_Destroy
  PUBLIC :: RadDiag_Data_Create
  PUBLIC :: RadDiag_Data_Inspect
  PUBLIC :: RadDiag_Data_DefineVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: RADDIAG_N_FPELEMENTS = 26 ! Number of floating point elements
  INTEGER, PARAMETER :: RADDIAG_N_CHELEMENTS = 7  ! Number of channel elements
  INTEGER, PARAMETER :: RADDIAG_N_PRELEMENTS = 5  ! Number of bias correction terms
  ! Literal constants
  REAL, PARAMETER :: ZERO = 0.0_sp
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'


  ! -------------------------
  ! Data structure definition
  ! -------------------------
  ! Scalar part of data
  TYPE :: RadDiag_Data_Scalar_type
    REAL(sp) :: lat        = ZERO  ! latitude (deg)
    REAL(sp) :: lon        = ZERO  ! longitude (deg)
    REAL(sp) :: zsges      = ZERO  ! guess elevation at obs location (m)
    REAL(sp) :: obstime    = ZERO  ! observation time relative to analysis
    REAL(sp) :: senscn_pos = ZERO  ! sensor scan position (integer)
    REAL(sp) :: satzen_ang = ZERO  ! satellite zenith angle (deg)
    REAL(sp) :: satazm_ang = ZERO  ! satellite azimuth angle (deg)
    REAL(sp) :: solzen_ang = ZERO  ! solar zenith angle (deg)
    REAL(sp) :: solazm_ang = ZERO  ! solar azimumth angle (deg)
    REAL(sp) :: sungln_ang = ZERO  ! sun glint angle (deg)
    REAL(sp) :: water_frac = ZERO  ! fractional coverage by water
    REAL(sp) :: land_frac  = ZERO  ! fractional coverage by land
    REAL(sp) :: ice_frac   = ZERO  ! fractional coverage by ice
    REAL(sp) :: snow_frac  = ZERO  ! fractional coverage by snow
    REAL(sp) :: water_temp = ZERO  ! surface temperature over water (K)
    REAL(sp) :: land_temp  = ZERO  ! surface temperature over land (K)
    REAL(sp) :: ice_temp   = ZERO  ! surface temperature over ice (K)
    REAL(sp) :: snow_temp  = ZERO  ! surface temperature over snow (K)
    REAL(sp) :: soil_temp  = ZERO  ! soil temperature (K)
    REAL(sp) :: soil_mois  = ZERO  ! soil moisture 
    REAL(sp) :: land_type  = ZERO  ! land type (integer)
    REAL(sp) :: veg_frac   = ZERO  ! vegetation fraction
    REAL(sp) :: snow_depth = ZERO  ! snow depth
    REAL(sp) :: sfc_wndspd = ZERO  ! surface wind speed
    REAL(sp) :: qcdiag1    = ZERO  ! ir=cloud fraction, mw=cloud liquid water
    REAL(sp) :: qcdiag2    = ZERO  ! ir=cloud top pressure, mw=total column water
  END TYPE RadDiag_Data_Scalar_type

  ! Channel dependent part of data
  TYPE :: RadDiag_Data_Channel_type
    REAL(sp) :: tbobs  = ZERO  ! Tb (obs) (K)
    REAL(sp) :: omgbc  = ZERO  ! Tb_(obs) - Tb_(simulated w/ bc)  (K)
    REAL(sp) :: omgnbc = ZERO  ! Tb_(obs) - Tb_(simulated_w/o bc) (K)
    REAL(sp) :: errinv = ZERO  ! inverse error (K**(-1))
    REAL(sp) :: qcmark = ZERO  ! quality control mark
    REAL(sp) :: emiss  = ZERO  ! surface emissivity
    REAL(sp) :: tlap   = ZERO  ! temperature lapse rate
    REAL(sp) :: bifix  = ZERO  ! fixed angle dependent bias
    REAL(sp) :: bilap  = ZERO  ! lapse rate bias correction term
    REAL(sp) :: bilap2 = ZERO  ! square lapse rate bias correction term
    REAL(sp) :: bicons = ZERO  ! constant bias correction term
    REAL(sp) :: biang  = ZERO  ! scan angle bias correction term
    REAL(sp) :: biclw  = ZERO  ! CLW bias correction term
  END TYPE RadDiag_Data_Channel_type

  ! The complete data structure
  TYPE :: RadDiag_Data_type
    INTEGER :: n_Channels  = 0  ! Structure dimensions
    TYPE(RadDiag_Data_Scalar_type) :: Scalar
    TYPE(RadDiag_Data_Channel_type), ALLOCATABLE :: Channel(:)
  END TYPE RadDiag_Data_type


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RadDiag_Data_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a RadDiag_Data object.
!
! CALLING SEQUENCE:
!       Status = RadDiag_Data_Associated( RadDiag_Data )
!
! OBJECTS:
!       RadDiag_Data:    RadDiag_Data structure which is to have its
!                        member's status tested.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Data_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the RadDiag_Data members.
!                .TRUE.  - if ANY of the RadDiag_Data allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the RadDiag_Data allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input RadDiag_Data argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION RadDiag_Data_Associated( RadDiag_Data ) RESULT( Status )
    ! Arguments
    TYPE(RadDiag_Data_type), INTENT(IN) :: RadDiag_Data
    ! Function result
    LOGICAL :: Status

    ! Test the structure members
    Status = ALLOCATED( RadDiag_Data%Channel )

  END FUNCTION RadDiag_Data_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Data_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize RadDiag_Data objects.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Data_Destroy( RadDiag_Data )
!
! OBJECTS:
!       RadDiag_Data:    Re-initialized RadDiag_Data structure.
!                       UNITS:      N/A
!                       TYPE:       RadDiag_Data_type
!                       DIMENSION:  Scalar OR any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Data_Destroy( RadDiag_Data )
    TYPE(RadDiag_Data_type), INTENT(OUT) :: RadDiag_Data
  END SUBROUTINE RadDiag_Data_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Data_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the RadDiag_Data object.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Data_Create( RadDiag_Data, n_Channels )
!
! OBJECTS:
!       RadDiag_Data:     RadDiag_Data structure.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Data_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:      Channel dimension of RadDiag_Data structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Conformable with RadDiag_Data object
!                        ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RadDiag_Data_Create( &
    RadDiag_Data, &  ! Output
    n_Channels   )  ! Input
    ! Arguments
    TYPE(RadDiag_Data_type), INTENT(OUT) :: RadDiag_Data
    INTEGER,                INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( RadDiag_Data%Channel(n_Channels), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise dimensions
    RadDiag_Data%n_Channels = n_Channels

  END SUBROUTINE RadDiag_Data_Create
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Data_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a RadDiag_Data object to stdout.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Data_Inspect( rdd )
!
! INPUTS:
!       rdd:    RadDiag_Data object to display.
!               UNITS:      N/A
!               TYPE:       RadDiag_Data_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_Data_Inspect( rdd )
    TYPE(RadDiag_Data_type), INTENT(IN) :: rdd
    INTEGER :: i
    WRITE(*, '(1x,"RadDiag_Data OBJECT")')
    ! Scalar object
    CALL RadDiag_Data_Scalar_Inspect( rdd%Scalar )
    ! Channel object(s)
    IF ( .NOT. RadDiag_Data_Associated(rdd) ) RETURN
    DO i = 1, rdd%n_Channels
      WRITE(*, '(3x,"Channel index: ",i0)') i
      CALL RadDiag_Data_Channel_Inspect( rdd%Channel(i) )
    END DO
  END SUBROUTINE RadDiag_Data_Inspect
  
  SUBROUTINE RadDiag_Data_Scalar_Inspect( rdds )
    TYPE(RadDiag_Data_Scalar_type), INTENT(IN) :: rdds
    WRITE(*, '(3x,"Scalar Component")')
    WRITE(*, '(5x,"lat        :",1x,es13.6)') rdds%lat        
    WRITE(*, '(5x,"lon        :",1x,es13.6)') rdds%lon        
    WRITE(*, '(5x,"zsges      :",1x,es13.6)') rdds%zsges      
    WRITE(*, '(5x,"obstime    :",1x,es13.6)') rdds%obstime    
    WRITE(*, '(5x,"senscn_pos :",1x,es13.6)') rdds%senscn_pos
    WRITE(*, '(5x,"satzen_ang :",1x,es13.6)') rdds%satzen_ang 
    WRITE(*, '(5x,"satazm_ang :",1x,es13.6)') rdds%satazm_ang 
    WRITE(*, '(5x,"solzen_ang :",1x,es13.6)') rdds%solzen_ang 
    WRITE(*, '(5x,"solazm_ang :",1x,es13.6)') rdds%solazm_ang 
    WRITE(*, '(5x,"sungln_ang :",1x,es13.6)') rdds%sungln_ang
    WRITE(*, '(5x,"water_frac :",1x,es13.6)') rdds%water_frac 
    WRITE(*, '(5x,"land_frac  :",1x,es13.6)') rdds%land_frac  
    WRITE(*, '(5x,"ice_frac   :",1x,es13.6)') rdds%ice_frac   
    WRITE(*, '(5x,"snow_frac  :",1x,es13.6)') rdds%snow_frac  
    WRITE(*, '(5x,"water_temp :",1x,es13.6)') rdds%water_temp
    WRITE(*, '(5x,"land_temp  :",1x,es13.6)') rdds%land_temp  
    WRITE(*, '(5x,"ice_temp   :",1x,es13.6)') rdds%ice_temp   
    WRITE(*, '(5x,"snow_temp  :",1x,es13.6)') rdds%snow_temp  
    WRITE(*, '(5x,"soil_temp  :",1x,es13.6)') rdds%soil_temp  
    WRITE(*, '(5x,"soil_mois  :",1x,es13.6)') rdds%soil_mois 
    WRITE(*, '(5x,"land_type  :",1x,es13.6)') rdds%land_type  
    WRITE(*, '(5x,"veg_frac   :",1x,es13.6)') rdds%veg_frac   
    WRITE(*, '(5x,"snow_depth :",1x,es13.6)') rdds%snow_depth 
    WRITE(*, '(5x,"sfc_wndspd :",1x,es13.6)') rdds%sfc_wndspd 
    WRITE(*, '(5x,"qcdiag1    :",1x,es13.6)') rdds%qcdiag1   
    WRITE(*, '(5x,"qcdiag2    :",1x,es13.6)') rdds%qcdiag2    
  END SUBROUTINE RadDiag_Data_Scalar_Inspect
  
  SUBROUTINE RadDiag_Data_Channel_Inspect( rddc )
    TYPE(RadDiag_Data_Channel_type), INTENT(IN) :: rddc
    WRITE(*, '(3x,"Channel Component")')
    WRITE(*, '(5x,"tbobs  :",1x,es13.6)') rddc%tbobs  
    WRITE(*, '(5x,"omgbc  :",1x,es13.6)') rddc%omgbc  
    WRITE(*, '(5x,"omgnbc :",1x,es13.6)') rddc%omgnbc 
    WRITE(*, '(5x,"errinv :",1x,es13.6)') rddc%errinv 
    WRITE(*, '(5x,"qcmark :",1x,es13.6)') rddc%qcmark
    WRITE(*, '(5x,"emiss  :",1x,es13.6)') rddc%emiss  
    WRITE(*, '(5x,"tlap   :",1x,es13.6)') rddc%tlap   
    WRITE(*, '(5x,"bifix  :",1x,es13.6)') rddc%bifix  
    WRITE(*, '(5x,"bilap  :",1x,es13.6)') rddc%bilap  
    WRITE(*, '(5x,"bilap2 :",1x,es13.6)') rddc%bilap2
    WRITE(*, '(5x,"bicons :",1x,es13.6)') rddc%bicons 
    WRITE(*, '(5x,"biang  :",1x,es13.6)') rddc%biang  
    WRITE(*, '(5x,"biclw  :",1x,es13.6)') rddc%biclw  
  END SUBROUTINE RadDiag_Data_Channel_Inspect
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_Data_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL RadDiag_Data_DefineVersion( Id )
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

  SUBROUTINE RadDiag_Data_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE RadDiag_Data_DefineVersion

END MODULE RadDiag_Data_Define
