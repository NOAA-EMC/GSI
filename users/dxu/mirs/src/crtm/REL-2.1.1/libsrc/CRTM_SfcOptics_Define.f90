!
! CRTM_SfcOptics_Define
!
! Module defining the CRTM SfcOptics structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       02-Apr-2004
!

MODULE CRTM_SfcOptics_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message,warning
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE CRTM_Parameters      , ONLY: ZERO, ONE, SET, NOT_SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_SfcOptics_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: CRTM_SfcOptics_Associated
  PUBLIC :: CRTM_SfcOptics_Destroy
  PUBLIC :: CRTM_SfcOptics_Create
  PUBLIC :: CRTM_SfcOptics_Inspect
  PUBLIC :: CRTM_SfcOptics_DefineVersion
  PUBLIC :: CRTM_SfcOptics_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_SfcOptics_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_SfcOptics_Define.f90 22454 2012-11-16 23:39:18Z paul.vandelst@noaa.gov $'


  ! -----------------------------------
  ! Surface optics data type definition
  ! -----------------------------------
  TYPE :: CRTM_SfcOptics_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Angles = 0 ! I
    INTEGER :: n_Stokes = 0 ! Ls
    ! Flag for SfcOptics computation
    LOGICAL :: Compute = .TRUE.

    ! MW Water SfcOptics options
    LOGICAL  :: Use_New_MWSSEM = .TRUE.    ! Flag for MW Water SfcOptics algorithm switch
    REAL(fp) :: Azimuth_Angle  = 999.9_fp  ! Relative azimuth angle

    ! Index of the satellite view angle in the angle arrays
    INTEGER  :: Index_Sat_Ang = 1
    ! The counter for the m'th component of the Fourier exapnsion of
    ! the radiance for azimuth angle
    INTEGER  :: mth_Azi = 0
    ! The weighted mean surface temperature
    REAL(fp) :: Surface_Temperature = ZERO

    ! The stream angles and weights
    REAL(fp), ALLOCATABLE :: Angle(:)                 ! I
    REAL(fp), ALLOCATABLE :: Weight(:)                ! I
    ! The emissivities and reflectivities
    REAL(fp), ALLOCATABLE :: Emissivity(:,:)          ! I x Ls
    REAL(fp), ALLOCATABLE :: Reflectivity(:,:,:,:)    ! I x Ls x I x Ls
    REAL(fp), ALLOCATABLE :: Direct_Reflectivity(:,:) ! I x Ls
  END TYPE CRTM_SfcOptics_type

  ! Some notes regarding the above definition:
  !
  ! 1) The physical meaning of Reflectivity(:,:,:,:) is the following:
  !
  !    Given a pair of polarization indices, ip and rp, for the incident and
  !    reflected radiances respectively, assuming there are no cross contributions
  !    from incident radiation with different polarization, Reflectivity(:, rp, :, ip)
  !    is defined as a reflectivity matrix with
  !
  !      Reflectivity(:, rp, :, ip) = 0 ;  if rp /= ip
  !
  !    and
  !
  !      I(angle_r, p) = SUM( Reflectivity(angle_r, p, :, p) * I(:, p)), if rp=ip=p
  !
  !    where I(angle_r, p) is the reflected radiance at zenith angle with index angle_r,
  !    and I(:, p) are the incident radiances and the summation is over the number of
  !    incident angles.  Thus, if BRDF(angle_r, p, angle_in, p) is the bidirectional
  !    reflectivity distribution function, then
  !
  !       Reflectivity(angle_r, p, angle_in, p) = &
  !             BRDF(angle_r, p, angle_in, p)*cos(angle_in)*w(angle_in)
  !
  !    where w(angle_in) is the quadrature weight.
  !
  !    A SPECIAL CASE
  !    --------------
  !    For a Lambertian surface, if only one angle is given, then
  !
  !        I_r = Reflectivity(1, rp, 1, ip) * I_diff
  !
  !    where I_r is the reflected radiance, constant at all angles, I_diff
  !    is the incident radiance at the diffusivity angle.
  !
  !
  ! 2) Regarding the Direct_Reflectivity(:,:) component,
  !
  !    If I(angle_r, p) is the reflected radiance at the zenith angle with index
  !    angle_r and F_direct(angle_in) is the direct incident irradiance at the surface,
  !    then Direct_Reflectivity(angle_r, p) is defined as
  !
  !      I(angle_r, p) = Direct_Reflectivity(angle_r, p) * cos(angle_in) * F_direct(angle_in)
  !


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM SfcOptics object.
!
! CALLING SEQUENCE:
!       Status = CRTM_SfcOptics_Associated( SfcOptics )
!
! OBJECTS:
!       SfcOptics:   SfcOptics structure which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SfcOptics_type
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating the
!                    status of the SfcOptics members.
!                      .TRUE.  - if the array components are allocated.
!                      .FALSE. - if the array components are not allocated.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input SfcOptics argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Associated( SfcOptics ) RESULT( Status )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: SfcOptics
    LOGICAL :: Status
    Status = SfcOptics%Is_Allocated
  END FUNCTION CRTM_SfcOptics_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM SfcOptics objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Destroy( SfcOptics )
!
! OBJECTS:
!       SfcOptics:    Re-initialized SfcOptics structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SfcOptics_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SfcOptics_Destroy( SfcOptics )
    TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: SfcOptics
    SfcOptics%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_SfcOptics_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM SfcOptics object.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Create( SfcOptics, n_Layers )
!
! OBJECTS:
!       SfcOptics:        SfcOptics structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SfcOptics_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of layers for which there is SfcOptics data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as SfcOptics object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SfcOptics_Create( &
    SfcOptics, &
    n_Angles , &
    n_Stokes   )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: SfcOptics
    INTEGER,                   INTENT(IN)  :: n_Angles
    INTEGER,                   INTENT(IN)  :: n_Stokes
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Angles < 1 .OR. n_Stokes < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( SfcOptics%Angle( n_Angles ), &
              SfcOptics%Weight( n_Angles ), &
              SfcOptics%Emissivity( n_Angles, n_Stokes ), &
              SfcOptics%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
              SfcOptics%Direct_Reflectivity( n_Angles, n_Stokes ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    SfcOptics%n_Angles = n_Angles
    SfcOptics%n_Stokes = n_Stokes
    ! ...Arrays
    SfcOptics%Angle               = ZERO
    SfcOptics%Weight              = ZERO
    SfcOptics%Emissivity          = ZERO
    SfcOptics%Reflectivity        = ZERO
    SfcOptics%Direct_Reflectivity = ZERO

    ! Set allocation indicator
    SfcOptics%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_SfcOptics_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM SfcOptics object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Inspect( SfcOptics )
!
! INPUTS:
!       SfcOptics:     CRTM SfcOptics object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SfcOptics_Inspect( SfcOptics )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: SfcOptics

    WRITE(*, '(1x,"SfcOptics OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Angles   :",1x,i0)') SfcOptics%n_Angles
    WRITE(*, '(3x,"n_Stokes   :",1x,i0)') SfcOptics%n_Stokes
    ! Display components
    WRITE(*, '(3x,"Compute flag              :",1x,l1)') SfcOptics%Compute
    WRITE(*, '(3x,"Use_New_MWSSEM flag       :",1x,l1)') SfcOptics%Use_New_MWSSEM
    WRITE(*, '(3x,"Satellite view angle index:",1x,i0)') SfcOptics%Index_Sat_Ang
    WRITE(*, '(3x,"Azimuth Fourier component :",1x,i0)') SfcOptics%mth_Azi
    WRITE(*, '(3x,"Weighted mean Tsfc        :",1x,es13.6)') SfcOptics%Surface_Temperature
    IF ( .NOT. CRTM_SfcOptics_Associated(SfcOptics) ) RETURN
    WRITE(*, '(3x,"Angle :")')
    WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Angle
    WRITE(*, '(3x,"Weight :")')
    WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Weight
    WRITE(*, '(3x,"Emissivity :")')
    WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Emissivity
    WRITE(*, '(3x,"Reflectivity :")')
    WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Reflectivity
    WRITE(*, '(3x,"Direct_Reflectivity :")')
    WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Direct_Reflectivity
  END SUBROUTINE CRTM_SfcOptics_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_DefineVersion( Id )
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

  SUBROUTINE CRTM_SfcOptics_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_SfcOptics_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_SfcOptics_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_SfcOptics objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_SfcOptics_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM SfcOptics objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figure to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: x, y
    INTEGER,         OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( (.NOT. CRTM_SfcOptics_Associated(x)) .OR. &
         (.NOT. CRTM_SfcOptics_Associated(y)) ) RETURN

    ! Check scalars
    IF ( (x%n_Angles /= y%n_Angles) .OR. &
         (x%n_Stokes /= y%n_Stokes) ) RETURN

    ! Check arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Angle              ,y%Angle              ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Weight             ,y%Weight             ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Emissivity         ,y%Emissivity         ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Reflectivity       ,y%Reflectivity       ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Direct_Reflectivity,y%Direct_Reflectivity,n))) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_SfcOptics_Compare


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SfcOptics_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_SfcOptics objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_SfcOptics_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM SfcOptics objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( (.NOT. CRTM_SfcOptics_Associated(x)) .OR. &
         (.NOT. CRTM_SfcOptics_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalars
    IF ( (x%n_Angles /= y%n_Angles) .OR. &
         (x%n_Stokes /= y%n_Stokes) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Angle               .EqualTo. y%Angle              ) .AND. &
         ALL(x%Weight              .EqualTo. y%Weight             ) .AND. &
         ALL(x%Emissivity          .EqualTo. y%Emissivity         ) .AND. &
         ALL(x%Reflectivity        .EqualTo. y%Reflectivity       ) .AND. &
         ALL(x%Direct_Reflectivity .EqualTo. y%Direct_Reflectivity)       ) &
      is_equal = .TRUE.

  END FUNCTION CRTM_SfcOptics_Equal


!!----------------------------------------------------------------------------------
!!
!! NAME:
!!       CRTM_Clear_SfcOptics
!!
!! PURPOSE:
!!       Subroutine to clear the scalar members of a CRTM_SfcOptics structure.
!!
!! CALLING SEQUENCE:
!!       CALL CRTM_Clear_SfcOptics( SfcOptics ) ! Output
!!
!! OUTPUTS:
!!       SfcOptics:   CRTM_SfcOptics structure for which the scalar members have
!!                    been cleared.
!!                    UNITS:      N/A
!!                    TYPE:       CRTM_SfcOptics_type
!!                    DIMENSION:  Scalar
!!                    ATTRIBUTES: INTENT(IN OUT)
!!
!! COMMENTS:
!!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined upon
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!!                       paul.vandelst@ssec.wisc.edu
!!
!!----------------------------------------------------------------------------------
!
!  SUBROUTINE CRTM_Clear_SfcOptics( SfcOptics )
!    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
!    SfcOptics%n_Angles = 0
!    SfcOptics%n_Stokes = 0
!    SfcOptics%Compute_Switch = SET
!    SfcOptics%Index_Sat_Ang = 1
!    SfcOptics%Surface_Temperature = ZERO
!  END SUBROUTINE CRTM_Clear_SfcOptics
!
!
!
!
!
!!################################################################################
!!################################################################################
!!##                                                                            ##
!!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!!##                                                                            ##
!!################################################################################
!!################################################################################
!
!!--------------------------------------------------------------------------------
!!
!! NAME:
!!       CRTM_Associated_SfcOptics
!!
!! PURPOSE:
!!       Function to test the association status of the pointer members of a
!!       SfcOptics structure.
!!
!! CALLING SEQUENCE:
!!       Association_Status = CRTM_Associated_SfcOptics( SfcOptics,          &  ! Input
!!                                                       ANY_Test = Any_Test )  ! Optional input
!!
!! INPUTS:
!!       SfcOptics:           CRTM_SfcOptics structure which is to have its pointer
!!                            member's association status tested.
!!                            UNITS:      N/A
!!                            TYPE:       CRTM_SfcOptics_type
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUTS:
!!       ANY_Test:            Set this argument to test if ANY of the
!!                            CRTM_SfcOptics structure pointer members are associated.
!!                            The default is to test if ALL the pointer members
!!                            are associated.
!!                            If ANY_Test = 0, test if ALL the pointer members
!!                                             are associated.  (DEFAULT)
!!                               ANY_Test = 1, test if ANY of the pointer members
!!                                             are associated.
!!                            UNITS:      N/A
!!                            TYPE:       INTEGER
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Association_Status:  The return value is a logical value indicating the
!!                            association status of the CRTM_SfcOptics pointer members.
!!                            .TRUE.  - if ALL the CRTM_SfcOptics pointer members are
!!                                      associated, or if the ANY_Test argument
!!                                      is set and ANY of the CRTM_SfcOptics pointer
!!                                      members are associated.
!!                            .FALSE. - some or all of the CRTM_SfcOptics pointer
!!                                      members are NOT associated.
!!                            UNITS:      N/A
!!                            TYPE:       LOGICAL
!!                            DIMENSION:  Scalar
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!!                       paul.vandelst@ssec.wisc.edu
!!
!!--------------------------------------------------------------------------------
!
!  FUNCTION CRTM_Associated_SfcOptics( SfcOptics, & ! Input
!                                      ANY_Test ) & ! Optional input
!                                    RESULT( Association_Status )
!    ! Arguments
!    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: SfcOptics
!    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
!    ! Function result
!    LOGICAL :: Association_Status
!    ! Local variables
!    LOGICAL :: ALL_Test
!
!
!    ! ------
!    ! Set up
!    ! ------
!    ! Default is to test ALL the pointer members
!    ! for a true association status....
!    ALL_Test = .TRUE.
!    ! ...unless the ANY_Test argument is set.
!    IF ( PRESENT( ANY_Test ) ) THEN
!      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
!    END IF
!
!
!    ! ---------------------------
!    ! Test the association status
!    ! ---------------------------
!    Association_Status = .FALSE.
!    IF ( ALL_Test ) THEN
!      IF ( ASSOCIATED( SfcOptics%Angle               ) .AND. &
!           ASSOCIATED( SfcOptics%Weight              ) .AND. &
!           ASSOCIATED( SfcOptics%Emissivity          ) .AND. &
!           ASSOCIATED( SfcOptics%Reflectivity        ) .AND. &
!           ASSOCIATED( SfcOptics%Direct_Reflectivity )       ) THEN
!        Association_Status = .TRUE.
!      END IF
!    ELSE
!      IF ( ASSOCIATED( SfcOptics%Angle               ) .OR. &
!           ASSOCIATED( SfcOptics%Weight              ) .OR. &
!           ASSOCIATED( SfcOptics%Emissivity          ) .OR. &
!           ASSOCIATED( SfcOptics%Reflectivity        ) .OR. &
!           ASSOCIATED( SfcOptics%Direct_Reflectivity )      ) THEN
!        Association_Status = .TRUE.
!      END IF
!    END IF
!  END FUNCTION CRTM_Associated_SfcOptics
!
!
!!--------------------------------------------------------------------------------
!!
!! NAME:
!!       CRTM_Destroy_SfcOptics
!!
!! PURPOSE:
!!       Function to re-initialize the scalar and pointer members of
!!       a CRTM_SfcOptics data structure.
!!
!! CALLING SEQUENCE:
!!       Error_Status = CRTM_Destroy_SfcOptics( SfcOptics,                &  ! Output
!!                                              RCS_Id = RCS_Id,          &  ! Revision control
!!                                              Message_Log = Message_Log )  ! Error messaging
!!
!! OPTIONAL INPUTS:
!!       Message_Log:  Character string specifying a filename in which any
!!                     messages will be logged. If not specified, or if an
!!                     error occurs opening the log file, the default action
!!                     is to output messages to standard output.
!!                     UNITS:      N/A
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OUTPUTS:
!!       SfcOptics:    Re-initialized CRTM_SfcOptics structure.
!!                     UNITS:      N/A
!!                     TYPE:       CRTM_SfcOptics_type
!!                     DIMENSION:  Scalar OR Rank-1 array
!!                     ATTRIBUTES: INTENT(IN OUT)
!!
!! OPTIONAL OUTPUTS:
!!       RCS_Id:       Character string containing the Revision Control
!!                     System Id field for the module.
!!                     UNITS:      N/A
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status: The return value is an integer defining the error status.
!!                     The error codes are defined in the Message_Handler module.
!!                     If == SUCCESS the structure re-initialisation was successful
!!                        == FAILURE - an error occurred, or
!!                                   - the structure internal allocation counter
!!                                     is not equal to zero (0) upon exiting this
!!                                     function. This value is incremented and
!!                                     decremented for every structure allocation
!!                                     and deallocation respectively.
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!
!! COMMENTS:
!!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined upon
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!!                       paul.vandelst@ssec.wisc.edu
!!
!!--------------------------------------------------------------------------------
!
!  FUNCTION CRTM_Destroy_SfcOptics( SfcOptics,    &  ! Output
!                                   No_Clear,     &  ! Optional input
!                                   RCS_Id,       &  ! Revision control
!                                   Message_Log ) &  ! Error messaging
!                                 RESULT( Error_Status )
!    ! Arguments
!    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
!    INTEGER,         OPTIONAL, INTENT(IN)     :: No_Clear
!    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_SfcOptics'
!    ! Local variables
!    CHARACTER( 256 ) :: Message
!    LOGICAL :: Clear
!    INTEGER :: Allocate_Status
!
!
!    ! ------
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_VERSION_ID
!
!    ! Default is to clear scalar members...
!    Clear = .TRUE.
!    ! ....unless the No_Clear argument is set
!    IF ( PRESENT( No_Clear ) ) THEN
!      IF ( No_Clear == SET ) Clear = .FALSE.
!    END IF
!
!
!    ! -----------------------------
!    ! Initialise the scalar members
!    ! -----------------------------
!    IF ( Clear ) CALL CRTM_Clear_SfcOptics( SfcOptics )
!
!
!    ! -----------------------------------------------------
!    ! If ALL pointer members are NOT associated, do nothing
!    ! -----------------------------------------------------
!    IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics ) ) RETURN
!
!
!    ! ------------------------------
!    ! Deallocate the pointer members
!    ! ------------------------------
!
!    ! Deallocate the CRTM_SfcOptics Angle
!    IF ( ASSOCIATED( SfcOptics%Angle ) ) THEN
!      DEALLOCATE( SfcOptics%Angle, STAT = Allocate_Status )
!      IF ( Allocate_Status /= 0 ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Angle ", &
!                          &"member. STAT = ", i5 )' ) &
!                        Allocate_Status
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!      END IF
!    END IF
!
!    ! Deallocate the CRTM_SfcOptics Weight
!    IF ( ASSOCIATED( SfcOptics%Weight ) ) THEN
!      DEALLOCATE( SfcOptics%Weight, STAT = Allocate_Status )
!      IF ( Allocate_Status /= 0 ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Weight ", &
!                          &"member. STAT = ", i5 )' ) &
!                        Allocate_Status
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!      END IF
!    END IF
!
!    ! Deallocate the CRTM_SfcOptics Emissivity
!    IF ( ASSOCIATED( SfcOptics%Emissivity ) ) THEN
!      DEALLOCATE( SfcOptics%Emissivity, STAT = Allocate_Status )
!      IF ( Allocate_Status /= 0 ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Emissivity ", &
!                          &"member. STAT = ", i5 )' ) &
!                        Allocate_Status
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!      END IF
!    END IF
!
!    ! Deallocate the CRTM_SfcOptics Reflectivity
!    IF ( ASSOCIATED( SfcOptics%Reflectivity ) ) THEN
!      DEALLOCATE( SfcOptics%Reflectivity, STAT = Allocate_Status )
!      IF ( Allocate_Status /= 0 ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Reflectivity ", &
!                          &"member. STAT = ", i5 )' ) &
!                        Allocate_Status
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!      END IF
!    END IF
!
!    ! Deallocate the CRTM_SfcOptics Direct_Reflectivity
!    IF ( ASSOCIATED( SfcOptics%Direct_Reflectivity ) ) THEN
!      DEALLOCATE( SfcOptics%Direct_Reflectivity, STAT = Allocate_Status )
!      IF ( Allocate_Status /= 0 ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Direct_Reflectivity ", &
!                          &"member. STAT = ", i5 )' ) &
!                        Allocate_Status
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!      END IF
!    END IF
!
!
!    ! -------------------------------------
!    ! Decrement and test allocation counter
!    ! -------------------------------------
!
!    SfcOptics%n_Allocates = SfcOptics%n_Allocates - 1
!    IF ( SfcOptics%n_Allocates /= 0 ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
!                      SfcOptics%n_Allocates
!      CALL Display_Message( ROUTINE_NAME,    &
!                            TRIM( Message ), &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!    END IF
!
!  END FUNCTION CRTM_Destroy_SfcOptics
!
!
!!--------------------------------------------------------------------------------
!!
!! NAME:
!!       CRTM_Allocate_SfcOptics
!!
!! PURPOSE:
!!       Function to allocate the pointer members of the CRTM_SfcOptics
!!       data structure.
!!
!! CALLING SEQUENCE:
!!       Error_Status = CRTM_Allocate_SfcOptics( n_Angles,                 &  ! Input
!!                                               n_Stokes,                 &  ! Input
!!                                               SfcOptics,                &  ! Output
!!                                               RCS_Id = RCS_Id,          &  ! Revision control
!!                                               Message_Log = Message_Log )  ! Error messaging
!!
!! INPUTS:
!!       n_Angles:            Number of angles for which surface optical
!!                            data are represented.
!!                            Must be > 0
!!                            UNITS:      N/A
!!                            TYPE:       INTEGER
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(IN)
!!
!!       n_Stokes:            Number of Stokes parameters used to represent the
!!                            propagating radiation.
!!                            Must be > 0
!!                            UNITS:      N/A
!!                            TYPE:       INTEGER
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUTS:
!!       Message_Log:         Character string specifying a filename in which any
!!                            messages will be logged. If not specified, or if an
!!                            error occurs opening the log file, the default action
!!                            is to output messages to standard output.
!!                            UNITS:      N/A
!!                            TYPE:       CHARACTER(*)
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OUTPUTS:
!!       SfcOptics:           CRTM_SfcOptics structure with allocated pointer members
!!                            UNITS:      N/A
!!                            TYPE:       CRTM_SfcOptics_type
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(IN OUT)
!!
!!
!! OPTIONAL OUTPUTS:
!!       RCS_Id:              Character string containing the Revision Control
!!                            System Id field for the module.
!!                            UNITS:      N/A
!!                            TYPE:       CHARACTER(*)
!!                            DIMENSION:  Scalar
!!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:        The return value is an integer defining the error status.
!!                            The error codes are defined in the Message_Handler module.
!!                            If == SUCCESS the structure re-initialisation was successful
!!                               == FAILURE - an error occurred, or
!!                                          - the structure internal allocation counter
!!                                            is not equal to one (1) upon exiting this
!!                                            function. This value is incremented and
!!                                            decremented for every structure allocation
!!                                            and deallocation respectively.
!!                            UNITS:      N/A
!!                            TYPE:       INTEGER
!!                            DIMENSION:  Scalar
!!
!! COMMENTS:
!!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined upon
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!!                       paul.vandelst@ssec.wisc.edu
!!
!!--------------------------------------------------------------------------------
!
!  FUNCTION CRTM_Allocate_SfcOptics( n_Angles,     &  ! Input
!                                    n_Stokes,     &  ! Input
!                                    SfcOptics,    &  ! Output
!                                    RCS_Id,       &  ! Revision control
!                                    Message_Log ) &  ! Error messaging
!                                  RESULT( Error_Status )
!    ! Arguments
!    INTEGER,                   INTENT(IN)     :: n_Angles
!    INTEGER,                   INTENT(IN)     :: n_Stokes
!    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
!    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_SfcOptics'
!    ! Local variables
!    CHARACTER( 256 ) :: Message
!    INTEGER :: Allocate_Status
!
!
!    ! ------
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_VERSION_ID
!
!    ! Dimensions
!    IF ( n_Angles < 1 ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Input n_Angles must be > 0.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    IF ( n_Stokes < 1 ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Input n_Stokes must be > 0.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!
!    ! Check if ANY pointers are already associated
!    ! If they are, deallocate them but leave scalars.
!    IF ( CRTM_Associated_SfcOptics( SfcOptics, ANY_Test = SET ) ) THEN
!      Error_Status = CRTM_Destroy_SfcOptics( SfcOptics, &
!                                             No_Clear = SET, &
!                                             Message_Log = Message_Log )
!      IF ( Error_Status /= SUCCESS ) THEN
!        CALL Display_Message( ROUTINE_NAME,    &
!                              'Error deallocating CRTM_SfcOptics pointer members.', &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!        RETURN
!      END IF
!    END IF
!
!
!    ! ----------------------
!    ! Allocate the structure
!    ! ----------------------
!    ALLOCATE( SfcOptics%Angle( n_Angles ), SfcOptics%Weight( n_Angles ), &
!              SfcOptics%Emissivity( n_Angles, n_Stokes ), &
!              SfcOptics%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
!              SfcOptics%Direct_Reflectivity( n_Angles, n_Stokes ), &
!              STAT = Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "Error allocating SfcOptics data arrays. STAT = ", i5 )' ) &
!                      Allocate_Status
!      CALL Display_Message( ROUTINE_NAME,    &
!                            TRIM( Message ), &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!
!    ! ------------------------------------------
!    ! Assign dimensions and initialise variables
!    ! ------------------------------------------
!    SfcOptics%n_Angles = n_Angles
!    SfcOptics%n_Stokes = n_Stokes
!    SfcOptics%Angle               = ZERO
!    SfcOptics%Weight              = ZERO
!    SfcOptics%Emissivity          = ZERO
!    SfcOptics%Reflectivity        = ZERO
!    SfcOptics%Direct_Reflectivity = ZERO
!
!
!    ! -----------------------------------------
!    ! Increment and test the allocation counter
!    ! -----------------------------------------
!    SfcOptics%n_Allocates = SfcOptics%n_Allocates + 1
!    IF ( SfcOptics%n_Allocates /= 1 ) THEN
!      Error_Status = WARNING
!      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
!                      SfcOptics%n_Allocates
!      CALL Display_Message( ROUTINE_NAME,    &
!                            TRIM( Message ), &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!    END IF
!  END FUNCTION CRTM_Allocate_SfcOptics
!
!
!!--------------------------------------------------------------------------------
!!
!! NAME:
!!       CRTM_Assign_SfcOptics
!!
!! PURPOSE:
!!       Function to copy valid CRTM_SfcOptics structures.
!!
!! CALLING SEQUENCE:
!!       Error_Status = CRTM_Assign_SfcOptics( SfcOptics_in,             &  ! Input
!!                                             SfcOptics_out,            &  ! Output
!!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!!                                             Message_Log = Message_Log )  ! Error messaging
!!
!! INPUTS:
!!       SfcOptics_in:    CRTM_SfcOptics structure which is to be copied.
!!                        UNITS:      N/A
!!                        TYPE:       CRTM_SfcOptics_type
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUTS:
!!       Message_Log:     Character string specifying a filename in which any
!!                        messages will be logged. If not specified, or if an
!!                        error occurs opening the log file, the default action
!!                        is to output messages to standard output.
!!                        UNITS:      N/A
!!                        TYPE:       CHARACTER(*)
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OUTPUTS:
!!       SfcOptics_out:   Copy of the input structure, CRTM_SfcOptics_in.
!!                        UNITS:      N/A
!!                        TYPE:       CRTM_SfcOptics_type
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(IN OUT)
!!
!!
!! OPTIONAL OUTPUTS:
!!       RCS_Id:          Character string containing the Revision Control
!!                        System Id field for the module.
!!                        UNITS:      N/A
!!                        TYPE:       CHARACTER(*)
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:    The return value is an integer defining the error status.
!!                        The error codes are defined in the Message_Handler module.
!!                        If == SUCCESS the structure assignment was successful
!!                           == FAILURE an error occurred
!!                        UNITS:      N/A
!!                        TYPE:       INTEGER
!!                        DIMENSION:  Scalar
!!
!! COMMENTS:
!!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined upon
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!!                       paul.vandelst@ssec.wisc.edu
!!
!!--------------------------------------------------------------------------------
!
!  FUNCTION CRTM_Assign_SfcOptics( SfcOptics_in,  &  ! Input
!                                  SfcOptics_out, &  ! Output
!                                  RCS_Id,        &  ! Revision control
!                                  Message_Log )  &  ! Error messaging
!                                RESULT( Error_Status )
!    ! Arguments
!    TYPE(CRTM_SfcOptics_type), INTENT(IN)     :: SfcOptics_in
!    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_out
!    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_SfcOptics'
!
!
!    ! ------
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_VERSION_ID
!
!
!    ! ----------------------------------------------
!    ! ALL *input* pointers must be associated.
!    !
!    ! If this test succeeds, then some or all of the
!    ! input pointers are NOT associated, so destroy
!    ! the output structure and return.
!    ! ----------------------------------------------
!    IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics_In ) ) THEN
!      Error_Status = CRTM_Destroy_SfcOptics( SfcOptics_Out, &
!                                             Message_Log = Message_Log )
!      IF ( Error_Status /= SUCCESS ) THEN
!        CALL Display_Message( ROUTINE_NAME,    &
!                              'Error deallocating output CRTM_SfcOptics pointer members.', &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!      END IF
!      RETURN
!    END IF
!
!
!    ! ----------------------
!    ! Allocate the structure
!    ! ----------------------
!    Error_Status = CRTM_Allocate_SfcOptics( SfcOptics_in%n_Angles, &
!                                            SfcOptics_in%n_Stokes, &
!                                            SfcOptics_out, &
!                                            Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error allocating output SfcOptics arrays.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!
!    ! ------------------
!    ! Assign scalar data
!    ! ------------------
!    SfcOptics_out%Compute_Switch      = SfcOptics_in%Compute_Switch
!    SfcOptics_out%Index_Sat_Ang       = SfcOptics_in%Index_Sat_Ang
!    SfcOptics_out%Surface_Temperature = SfcOptics_in%Surface_Temperature
!
!
!    ! -----------------
!    ! Assign array data
!    ! -----------------
!    SfcOptics_out%Angle               = SfcOptics_in%Angle
!    SfcOptics_out%Weight              = SfcOptics_in%Weight
!    SfcOptics_out%Emissivity          = SfcOptics_in%Emissivity
!    SfcOptics_out%Reflectivity        = SfcOptics_in%Reflectivity
!    SfcOptics_out%Direct_Reflectivity = SfcOptics_in%Direct_Reflectivity
!  END FUNCTION CRTM_Assign_SfcOptics

END MODULE CRTM_SfcOptics_Define
