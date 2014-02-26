!
! CRTM_IRSSEM
!
! Module containing function to invoke the CRTM Infrared
! Sea Surface Emissivity Model (IRSSEM).
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jun-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_IRSSEM

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,         ONLY: fp
  USE Message_Handler,    ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,    ONLY: ZERO
  USE CRTM_Interpolation, ONLY: NPTS, &
                                LPoly, &
                                LPoly_type, &
                                Clear_LPoly, &
                                Find_Index, &
                                Interp_3D, &
                                LPoly_TL, &
                                Interp_3D_TL, &
                                LPoly_AD, &
                                Interp_3D_AD
  USE CRTM_IRwaterCoeff,  ONLY: IRwaterC
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Derived type
  PUBLIC :: iVar_type
  ! Procedures
  PUBLIC :: CRTM_Compute_IRSSEM
  PUBLIC :: CRTM_Compute_IRSSEM_TL
  PUBLIC :: CRTM_Compute_IRSSEM_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_IRSSEM.f90 22454 2012-11-16 23:39:18Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -------------------------------
  ! Structure definition to hold
  ! forward interpolating variables
  ! across fwd, tl and adjoint
  ! -------------------------------
  ! The interpolation routine structure
  TYPE :: Einterp_type
    ! The dimensions
    INTEGER :: n_Angles = 0
    INTEGER :: n_Pts    = 0
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! The interpolating polynomials
    TYPE(LPoly_type), ALLOCATABLE :: wlp(:)  ! Angle
    TYPE(LPoly_type)              :: xlp     ! Frequency
    TYPE(LPoly_type)              :: ylp     ! Wind Speed
    ! The LUT interpolation indices
    INTEGER, ALLOCATABLE :: i1(:), i2(:)     ! Angle
    INTEGER              :: j1    , j2       ! Frequency
    INTEGER              :: k1    , k2       ! Wind Speed
    ! The LUT interpolation boundary check
    LOGICAL, ALLOCATABLE :: a_outbound(:)    ! Angle
    LOGICAL              :: f_outbound       ! Frequency
    LOGICAL              :: v_outbound       ! Wind Speed
    ! The interpolation input
    REAL(fp), ALLOCATABLE :: a_int(:)        ! Angle
    REAL(fp)              :: f_int           ! Frequency
    REAL(fp)              :: v_int           ! Wind Speed
    ! The data to be interpolated
    REAL(fp), ALLOCATABLE :: a(:,:)          ! Angle
    REAL(fp), ALLOCATABLE :: f(:)            ! Frequency
    REAL(fp), ALLOCATABLE :: v(:)            ! Wind Speed
  END TYPE Einterp_type

  ! The main internal variable structure
  TYPE :: iVar_type
    PRIVATE
    ! The interpolation data
    TYPE(Einterp_type) :: ei
  END TYPE iVar_type


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
!       CRTM_Compute_IRSSEM
!
! PURPOSE:
!       Function to compute the CRTM infrared sea surface emissivity (IRSSE)
!       for input wind speed, frequency, and angles.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM( Wind_Speed, &  ! Input
!                                           Frequency,  &  ! Input
!                                           Angle,      &  ! Input
!                                           Emissivity, &  ! Output
!                                           iVar        )  ! Internal Variable Output
!
! INPUTS:
!       Wind_Speed:     Wind speed.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Frequency:      Infrared frequency.
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Angle:          Surface zenith angle.
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Emissivity:     Sea surface emissivities for the
!                       requested wind speed, frequency, and angles.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Same as input ANGLE argument.
!                       ATTRIBUTES: INTENT(OUT)
!
!       iVar:           Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_IRSSEM module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the emissivity computation was successful.
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM( &
    Wind_Speed, &  ! Input
    Frequency , &  ! Input
    Angle     , &  ! Input
    Emissivity, &  ! Output
    iVar      ) &  ! Internal variable output
  RESULT( Error_Status )
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Wind_Speed     ! v
    REAL(fp)       , INTENT(IN)  :: Frequency      ! f
    REAL(fp)       , INTENT(IN)  :: Angle(:)       ! a
    REAL(fp)       , INTENT(OUT) :: Emissivity(:)
    TYPE(iVar_type), INTENT(OUT) :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_Angles, i

    ! Set up
    Error_Status = SUCCESS
    ! ...Check dimensions
    n_Angles = SIZE(Angle)
    IF ( SIZE(Emissivity) /= n_Angles ) THEN
      Error_Status = FAILURE
      msg = 'Input Angle and output Emissivity array dimensions inconsistent.'
      CALL Display_Message( ROUTINE_NAME, msg, Error_Status )
      RETURN
    END IF
    ! ...Allocate interpolation variable structure
    CALL Einterp_Create( iVar%ei, NPTS, n_Angles )
    IF ( .NOT. Einterp_Associated( iVar%ei ) ) THEN
      Error_Status = FAILURE
      msg = 'Error allocating interpolation variable structure.'
      CALL Display_Message( ROUTINE_NAME, msg, Error_Status )
      RETURN
    END IF


    ! Compute the wind speed interpolating polynomial
    ! ...Find the LUT indices and check if input is out of bounds
    iVar%ei%v_int = Wind_Speed
    CALL find_index(IRwaterC%Wind_Speed, iVar%ei%v_int, iVar%ei%k1, iVar%ei%k2, iVar%ei%v_outbound)
    iVar%ei%v = IRwaterC%Wind_Speed(iVar%ei%k1:iVar%ei%k2)
    ! ...Compute the polynomial
    CALL LPoly( iVar%ei%v    , & ! Input
                iVar%ei%v_int, & ! Input
                iVar%ei%ylp    ) ! Output


    ! Compute the frequency interpolating polynomial
    ! ...Find the LUT indices and check if input is out of bounds
    iVar%ei%f_int = Frequency
    CALL find_index(IRwaterC%Frequency, iVar%ei%f_int, iVar%ei%j1, iVar%ei%j2, iVar%ei%f_outbound)
    iVar%ei%f = IRwaterC%Frequency(iVar%ei%j1:iVar%ei%j2)
    ! ...Compute the polynomial
    CALL LPoly( iVar%ei%f    , & ! Input
                iVar%ei%f_int, & ! Input
                iVar%ei%xlp    ) ! Output


    ! Compute the angle interpolating polynomials
    DO i = 1, n_Angles

      ! ...Find the LUT indices and check if input is out of bounds
      iVar%ei%a_int(i) = ABS(Angle(i))
      CALL find_index(IRwaterC%Angle, iVar%ei%a_int(i), iVar%ei%i1(i), iVar%ei%i2(i), iVar%ei%a_outbound(i))
      iVar%ei%a(:,i) = IRwaterC%Angle(iVar%ei%i1(i):iVar%ei%i2(i))
      ! ...Compute the polynomial
      CALL LPoly( iVar%ei%a(:,i)  , & ! Input
                  iVar%ei%a_int(i), & ! Input
                  iVar%ei%wlp(i)    ) ! Output


      ! Compute the interpolated emissivity
      CALL Interp_3D( IRwaterC%Emissivity( iVar%ei%i1(i):iVar%ei%i2(i), &
                                           iVar%ei%j1   :iVar%ei%j2   , &
                                           iVar%ei%k1   :iVar%ei%k2     ), & ! Input
                      iVar%ei%wlp(i), & ! Input
                      iVar%ei%xlp   , & ! Input
                      iVar%ei%ylp   , & ! Input
                      Emissivity(i)   ) ! Output

    END DO

  END FUNCTION CRTM_Compute_IRSSEM


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_IRSSEM_TL
!
! PURPOSE:
!       Function to compute the tangent-linear CRTM infrared sea surface
!       emissivity (IRSSE) for input wind speed, frequency, and angles.
!
!       This function must be called *after* the forward model function,
!       CRTM_Compute_IRSSEM, has been called. The forward model function
!       populates the internal variable structure argument, iVar.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM_TL( Wind_Speed_TL, &  ! Input
!                                              Emissivity_TL, &  ! Output
!                                              iVar           )  ! Internal variable input
! INPUTS:
!       Wind_Speed_TL:  The tangent-linear wind speed.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       iVar:           Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_IRSSEM module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Emissivity_TL:  Tangent-linear sea surface emissivity.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the emissivity computation was successful.
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM_TL( &
    Wind_Speed_TL, &  ! Input
    Emissivity_TL, &  ! Output
    iVar         ) &  ! Internal Variable
  RESULT ( Error_Status )
    ! Arguments
    REAL(fp),        INTENT(IN)  :: Wind_Speed_TL
    REAL(fp),        INTENT(OUT) :: Emissivity_TL(:)
    TYPE(iVar_type), INTENT(IN)  :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM_TL'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: i
    REAL(fp) :: v_TL(NPTS)
    REAL(fp) :: e_TL(NPTS,NPTS,NPTS)
    TYPE(LPoly_Type) :: ylp_TL, xlp_TL, wlp_TL

    ! Set up
    Error_Status = SUCCESS
    ! ...Check internal variable allocation
    IF ( .NOT. Einterp_Associated( iVar%ei ) ) THEN
      Error_Status = FAILURE
      msg = 'Internal structure ei is not allocated'
      CALL Display_Message( ROUTINE_NAME, msg, Error_Status )
      RETURN
    END IF
    ! ...Check dimensions
    IF ( SIZE( Emissivity_TL ) /= iVar%ei%n_Angles ) THEN
      Error_Status = FAILURE
      msg = 'Input Emissivity_TL array dimensions inconsistent with number of angles.'
      CALL Display_Message( ROUTINE_NAME, msg, Error_Status )
      RETURN
    END IF
    ! ...No TL if wind speed is out of bounds
    IF ( iVar%ei%v_outbound ) THEN
      Emissivity_TL = ZERO
      RETURN
    END IF
    ! ...Initialise local TL variables
    v_TL = ZERO
    e_TL = ZERO
    CALL Clear_LPoly(wlp_TL)
    CALL Clear_LPoly(xlp_TL)


    ! Calculate the TL interpolating
    ! polynomials for wind speed
    CALL LPoly_TL( iVar%ei%v, iVar%ei%v_int, & ! FWD Input
                   iVar%ei%ylp,              & ! FWD Input
                   v_TL, Wind_Speed_TL,      & ! TL  Input
                   ylp_TL                    ) ! TL  Output


    ! Begin loop over angles
    DO i = 1, iVar%ei%n_Angles

      ! Perform interpolation
      CALL interp_3D_TL(IRwaterC%Emissivity(iVar%ei%i1(i):iVar%ei%i2(i), &
                                            iVar%ei%j1   :iVar%ei%j2   , &
                                            iVar%ei%k1   :iVar%ei%k2     ), & ! FWD Emissivity input
                        iVar%ei%wlp(i), & ! FWD polynomial input
                        iVar%ei%xlp   , & ! FWD polynomial input
                        iVar%ei%ylp   , & ! FWD polynomial input
                        e_TL, wlp_TL, xlp_TL, ylp_TL, & ! TL input
                        Emissivity_TL(i)              ) ! Output

    END DO

  END FUNCTION CRTM_Compute_IRSSEM_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_IRSSEM_AD
!
! PURPOSE:
!       Function to compute the adjoint of the CRTM infrared sea surface
!       emissivity (IRSSE) for input wind speed, frequency, and angles.
!
!       This function must be called *after* the forward model function,
!       CRTM_Compute_IRSSEM, has been called. The forward model function
!       populates the internal variable structure argument, iVar.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM_AD( Emissivity_AD, &  ! Input
!                                              Wind_Speed_AD, &  ! Output
!                                              iVar           )  ! Internal Variable Input
!
! INPUTS:
!       Emissivity_AD:  Adjoint sea surface emissivity.
!                       *** SET TO ZERO ON EXIT ***
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:           Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_IRSSEM module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Wind_Speed_AD:  Adjoint wind speed.
!                       *** MUST HAVE VALUE ON ENTRY ***
!                       UNITS:      per metres per second, (m.s^-1)^-1
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was successful.
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM_AD( &
    Emissivity_AD, & ! Input
    Wind_Speed_AD, & ! Output
    iVar         ) & ! Internal Variable Input
  RESULT ( Error_Status )
    ! Arguments
    REAL(fp),        INTENT(IN OUT) :: Emissivity_AD(:)
    REAL(fp),        INTENT(IN OUT) :: Wind_Speed_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM_AD'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: i
    REAL(fp) :: e_AD(NPTS,NPTS,NPTS)
    REAL(fp) :: v_AD(NPTS)
    TYPE(LPoly_Type) :: wlp_AD, xlp_AD, ylp_AD

    ! Set Up
    Error_Status = SUCCESS
    e_AD = ZERO
    v_AD = ZERO
    ! ...Check internal variable allocation
    IF ( .NOT. Einterp_Associated( iVar%ei ) ) THEN
      Error_Status = FAILURE
      msg = 'Internal structure ei is not allocated'
      CALL Display_Message( ROUTINE_NAME, msg, Error_Status )
      RETURN
    END IF
    ! ...Check dimensions
    IF ( SIZE(Emissivity_AD) /= iVar%ei%n_Angles ) THEN
      Error_Status = FAILURE
      msg = 'Input Emissivity_AD array dimensions inconsistent with number of angles.'
      CALL Display_Message( ROUTINE_NAME, msg, Error_Status )
      RETURN
    END IF
    ! ...No AD if wind speed is out of bounds
    IF ( iVar%ei%v_outbound ) THEN
      Wind_Speed_AD = ZERO
      RETURN
    END IF
    ! ...Initialize local variables
    CALL Clear_LPoly(wlp_AD)
    CALL Clear_LPoly(xlp_AD)
    CALL Clear_LPoly(ylp_AD)

    ! Loop over emissivity calculation angles
    DO i = 1, iVar%ei%n_Angles

      ! Get the adjoint interpoalting polynomial for wind speed
      CALL Interp_3D_AD(IRwaterC%Emissivity( iVar%ei%i1(i):iVar%ei%i2(i), &
                                             iVar%ei%j1   :iVar%ei%j2   , &
                                             iVar%ei%k1   :iVar%ei%k2     ), & ! FWD Input
                        iVar%ei%wlp(i)  , & ! FWD Input
                        iVar%ei%xlp     , & ! FWD Input
                        iVar%ei%ylp     , & ! FWD Input
                        Emissivity_AD(i), & ! AD Input
                        e_AD, wlp_AD, xlp_AD, ylp_AD ) ! AD Output

      ! Set adjoint emissivity to zero
      Emissivity_AD(i) = ZERO

    END DO

    ! Compute the wind speed adjoint
    CALL Lpoly_AD(iVar%ei%v    , & ! FWD Input
                  iVar%ei%v_int, & ! FWD Input
                  iVar%ei%ylp  , & ! FWD Input
                  ylp_AD       , & ! AD  Input
                  v_AD         , & ! AD  Output
                  Wind_Speed_AD  ) ! AD  Output

  END FUNCTION CRTM_Compute_IRSSEM_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! ----------------------------------------------
  ! Procedures to manipulate the Einterp structure
  ! ----------------------------------------------
  ELEMENTAL FUNCTION Einterp_Associated( ei ) RESULT( Status )
    TYPE(Einterp_type), INTENT(IN) :: ei
    LOGICAL :: Status
    Status = ei%Is_Allocated
  END FUNCTION Einterp_Associated

  ELEMENTAL SUBROUTINE Einterp_Destroy( ei )
    TYPE(Einterp_type), INTENT(OUT) :: ei
    ei%n_Angles = 0
    ei%n_Pts    = 0
    ei%Is_Allocated = .FALSE.
  END SUBROUTINE Einterp_Destroy

  ELEMENTAL SUBROUTINE Einterp_Create( ei, n_Pts, n_Angles )
    TYPE(Einterp_type), INTENT(OUT) :: ei
    INTEGER,            INTENT(IN)  :: n_Pts
    INTEGER,            INTENT(IN)  :: n_Angles
    INTEGER :: alloc_stat
    IF ( n_Pts < 1 .OR. n_Angles < 1 ) RETURN
    ALLOCATE( ei%wlp(n_Angles)       , &
              ei%i1(n_Angles)        , &
              ei%i2(n_Angles)        , &
              ei%a_outbound(n_Angles), &
              ei%a_int(n_Angles)     , &
              ei%a(n_Pts,n_Angles)   , &
              ei%f(n_Pts)            , &
              ei%v(n_Pts)            , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN
    ei%n_Angles = n_Angles
    ei%n_Pts    = n_Pts
    ei%Is_Allocated = .TRUE.
  END SUBROUTINE Einterp_Create

END MODULE CRTM_IRSSEM
