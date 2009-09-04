!
! AntCorr_Application
!
! Module containing routines to apply/remove antenna corrections to/from 
! supported microwave sensor observations.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Aug-2008
!                       paul.vandelst@noaa.gov
!

MODULE AntCorr_Application

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: Display_Message, FAILURE
  USE AntCorr_Define   , ONLY: AntCorr_type, &
                               Associated_AntCorr, &
                               Destroy_AntCorr, &
                               Allocate_AntCorr, &
                               Assign_AntCorr, &
                               Equal_AntCorr, &
                               Info_AntCorr, &
                               CheckRelease_AntCorr
  USE AntCorr_Binary_IO, ONLY: Inquire_AntCorr_Binary, &
                               Read_AntCorr_Binary, &
                               Write_AntCorr_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Inherited procedures from definition module
  ! -------------------------------------------
  ! The AntCorr structure definition
  PUBLIC :: AntCorr_type
  ! The AntCorr structure methods
  PUBLIC :: Associated_AntCorr
  PUBLIC :: Destroy_AntCorr
  PUBLIC :: Allocate_AntCorr
  PUBLIC :: Assign_AntCorr
  PUBLIC :: Equal_AntCorr
  PUBLIC :: Info_AntCorr
  PUBLIC :: CheckRelease_AntCorr
  ! Inherited procedures from I/O modules
  ! -------------------------------------
  ! The Binary I/O functions
  PUBLIC :: Inquire_AntCorr_Binary
  PUBLIC :: Read_AntCorr_Binary
  PUBLIC :: Write_AntCorr_Binary
  ! Module procedures
  ! -----------------
  PUBLIC :: Remove_AntCorr
  PUBLIC :: Apply_AntCorr


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id: antcorr_application.f90,v 1.1 2008/12/11 22:26:27 jguo Exp $'

  ! Invalid result
  REAL(fp), PARAMETER :: INVALID = -1.0_fp
    
  ! Cosmic background temperature. Taken from
  ! Mather,J.C. et. al., 1999, "Calibrator Design for the COBE
  !    Far-Infrared Absolute Spectrophotometer (FIRAS)"
  !    Astrophysical Journal, vol 512, pp 511-520
  REAL(fp), PARAMETER :: TSPACE = 2.7253_fp
  

CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Remove_AntCorr
!
! PURPOSE:
!       Subroutine to remove an antenna correction to microwave instrument
!       brightness temperatures, Tb, to produce antenna temperatures, Ta.
!
! CALLING SEQUENCE:
!       CALL Remove_AntCorr( AC  , &  ! Input
!                            iFOV, &  ! Input
!                            T     )  ! In/Output
!
! INPUT ARGUMENTS:
!       AC:             Structure containing the antenna correction coefficients
!                       for the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AntCorr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       iFOV:           The FOV index for a scanline of the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       T:              On input, this argument contains the brightness
!                       temperatures for the sensor channels.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       T:              On output, this argument contains the antenna
!                       temperatures for the sensor channels.
!                       If an error occurs, the return values are all -1.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The temperature array argument, T, is modified.
!
! PROCEDURE:
!       For every FOV and channel, the brightness temperature, Tb, is converted
!       to antenna temperature, Ta, using,
!
!         Ta = Ae.Tb + Ap.Tb + As.Ts
!
!       where Ae == antenna efficiency for the Earth view
!             Ap == antenna efficiency for satellite platform view
!             As == antenna efficiency for cold space view
!             Ts == cosmic background temperature.
!
!       Note that the observed earth view brightness temperature is used as a
!       proxy for the platform temperature for the (Ap.Tb) component since
!       there is no measurement of the platform temperature in-flight.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Remove_AntCorr( AC  , &  ! Input
                             iFOV, &  ! Input
                             T     )  ! In/Output
    implicit none

    ! Arguments
    TYPE(AntCorr_type), INTENT(IN)     :: AC
    INTEGER           , INTENT(IN)     :: iFOV
    REAL(fp)          , INTENT(IN OUT) :: T(:)
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Remove_AntCorr'
    ! Local variables
    INTEGER :: l
    ! Check input
    IF ( iFOV < 1 .OR. iFOV > AC%n_FOVS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Input iFOV inconsistent with AC data', FAILURE )
      T = INVALID
      RETURN
    END IF
    IF ( SIZE(T) /= AC%n_Channels ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Size of T() inconsistent with AC data', FAILURE )
      T = INVALID
      RETURN
    END IF
    ! Compute the antenna temperature
    DO l = 1, AC%n_Channels
      T(l) = AC%A_earth(iFOV,l)*T(l) + AC%A_platform(iFOV,l)*T(l) + AC%A_space(iFOV,l)*TSPACE
    END DO
  END SUBROUTINE Remove_AntCorr


!--------------------------------------------------------------------------------
!
! NAME:
!       Apply_AntCorr
!
! PURPOSE:
!       Subroutine to apply an antenna correction to microwave instrument
!       antenna temperatures, Ta, to produce brightness temperatures, Tb.
!
! CALLING SEQUENCE:
!       CALL Apply_AntCorr( AC  , &  ! Input
!                           iFOV, &  ! Input
!                           T     )  ! In/Output
!
! INPUT ARGUMENTS:
!       AC:             Structure containing the antenna correction coefficients
!                       for the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AntCorr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       iFOV:           The FOV index for a scanline of the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       T:              On input, this argument contains the antenna temperatures
!                       for the sensor channels.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       T:              On output, this argument contains the brightness
!                       temperatures for the sensor channels.
!                       If an error occurs, the return values are all -1.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The temperature array argument, T, is modified.
!
! PROCEDURE:
!       For every FOV and channel, the antenna temperature, Ta, is converted
!       to brightness temperature, Tb, using,
!
!               Ta - As.Ts
!         Tb = ------------
!                Ae + Ap
!
!       where Ae == antenna efficiency for the Earth view
!             Ap == antenna efficiency for satellite platform view
!             As == antenna efficiency for cold space view
!             Ts == cosmic background temperature.
!
!       Note that the earth view brightness temperature is used as a proxy for
!       the platform temperature since there is no measurement of the platform
!       temperature in-flight.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Apply_AntCorr( AC  , &  ! Input
                            iFOV, &  ! Input
                            T     )  ! In/Output
    implicit none

    ! Arguments
    TYPE(AntCorr_type), INTENT(IN)     :: AC
    INTEGER           , INTENT(IN)     :: iFOV
    REAL(fp)          , INTENT(IN OUT) :: T(:)
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Apply_AntCorr'
    ! Local variables
    INTEGER :: l
    ! Check input
    IF ( iFOV < 1 .OR. iFOV > AC%n_FOVS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Input iFOV inconsistent with AC data', FAILURE )
      T = INVALID
      RETURN
    END IF
    IF ( SIZE(T) /= AC%n_Channels ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Size of T() inconsistent with AC data', FAILURE )
      T = INVALID
      RETURN
    END IF
    ! Compute the brightness temperature
    DO l = 1, AC%n_Channels
      T(l) = (T(l) - AC%A_space(iFOV,l)*TSPACE)/(AC%A_earth(iFOV,l)+AC%A_platform(iFOV,l))
    END DO
  END SUBROUTINE Apply_AntCorr
  
END MODULE AntCorr_Application
