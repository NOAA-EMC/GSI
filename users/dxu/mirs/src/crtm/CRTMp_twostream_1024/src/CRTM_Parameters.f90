!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Parameters
!
! PURPOSE:
!       Module of parameter definitions for the CRTM.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Parameters
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds
!                            of variable types.
!
! CONTAINS:
!       CRTM_Set_Max_n_Channels:    Subroutine to set the protected variable
!                                   MAX_N_CHANNELS value in the CRTM_Parameters
!                                   module. This should *only* be done during
!                                   the CRTM initialisation.
!
!       CRTM_Reset_Max_n_Channels:  Subroutine to reset the protected variable
!                                   MAX_N_CHANNELS value in the CRTM_Parameters
!                                   module to an invalid value. This should
!                                   *only* be done during the CRTM destruction.
!
!       CRTM_Get_Max_n_Channels:    Subroutine to GET the protected variable
!                                   MAX_N_CHANNELS value stored in the
!                                   CRTM_Parameters module.
!
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP/EMC 31-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!
!
!  Copyright (C) 2000 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE CRTM_Parameters


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind


  ! ------------------
  ! Default visibility
  ! ------------------

  ! -- Everything PRIVATE by default
  PRIVATE

  ! -- The MAX_N_CHANNELS methods
  PUBLIC :: CRTM_Set_Max_n_Channels
  PUBLIC :: CRTM_Reset_Max_n_Channels
  PUBLIC :: CRTM_Get_Max_n_Channels



  !#----------------------------------------------------------------------------#
  !#                    -- ALGORITHM INDEPENDENT PARAMETERS --                  #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------------------
  ! Number of channels (for ALL satellites - really the number
  ! of satellites x number of channels USED per satellite)
  !
  ! This is also the number of lines in the satellite 
  ! information (satinfo) file used in the NCEP GDAS.
  !
  ! The number of channels that can be used is determined,
  ! and SET, during the model initialisation.
  !
  ! In this module it is a protected variable in that it can
  ! only be set, reset, or retrieved via the MAX_N_CHANNELS
  ! methods.
  ! ----------------------------------------------------------

  ! -- Accessed via SET_MAX_N_CHANNELS, RESET_MAX_N_CHANNELS,
  ! -- and GET_MAX_N_CHANNELS routines
  INTEGER, PRIVATE, PARAMETER :: RESET_VALUE = -1
  INTEGER, PRIVATE, SAVE      :: MAX_N_CHANNELS = RESET_VALUE


  ! -----------------------------------------------------
  ! The maximum number of atmospheric profiles and layers
  ! accepted. These values are arbitrary. Nothing magical
  ! -----------------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_PROFILES = 128
  INTEGER, PUBLIC, PARAMETER :: MAX_N_LAYERS   = 100


  ! -----------------
  ! Literal constants
  ! -----------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: ZERO          =  0.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE           =  1.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: TWO           =  2.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: THREE         =  3.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: FOUR          =  4.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: FIVE          =  5.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: TEN           = 10.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_25      =  0.25_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_5       =  0.5_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_75      =  0.75_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONEpointFIVE  =  1.5_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: Smallvalue  = 0.00001_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: earthrad = 6371.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: satheight = 833.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: Diffuse_Factor = 1.7320508_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: Cosmic_Temperature = 2.7_fp_kind
  ! -----------------------------
  ! Numerical precision/tolerance
  ! -----------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: TOLERANCE = EPSILON( ONE )


  ! ---------------------------------------------
  ! Constant to allow degrees->radians conversion
  ! ---------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: PI = 3.141592653589793238462643383279_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp_kind


  ! -------------------------
  ! Direction flags
  !   DOWN == From TOA to SFC
  !   UP   == From SFC to TOA
  ! -------------------------

  INTEGER, PUBLIC, PARAMETER :: DOWN = 0
  INTEGER, PUBLIC, PARAMETER :: UP   = 1


  ! ------------------------
  ! Invalid sensor ID values
  ! ------------------------

  INTEGER, PUBLIC, PARAMETER :: INVALID_NCEP_SENSOR_ID   = -1
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  



  !#----------------------------------------------------------------------------#
  !#                       -- AtmAbsorption PARAMETERS --                       #
  !#----------------------------------------------------------------------------#

  ! -------------------------------------
  ! Absorbers in the gas absorption model
  ! -------------------------------------

  ! -- The total number
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBERS = 3

  ! -- The indexing order of the absorbers
  INTEGER, PUBLIC, PARAMETER :: WET_ABSORBER_INDEX = 1
  INTEGER, PUBLIC, PARAMETER :: DRY_ABSORBER_INDEX = 2
  INTEGER, PUBLIC, PARAMETER :: OZO_ABSORBER_INDEX = 3

  ! -- The absorber index and name arrays
  INTEGER, PUBLIC, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: &
    ABSORBER_INDEX = (/ WET_ABSORBER_INDEX, &
                        DRY_ABSORBER_INDEX, &
                        OZO_ABSORBER_INDEX /)

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: &
    ABSORBER_NAME = (/ 'wet', &
                       'dry', &
                       'ozo' /)


  ! --------------------------------------
  ! Predictors in the gas absorption model
  ! --------------------------------------

  ! -- Standard predictors are absorber independent
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STANDARD_PREDICTORS   = 11

  ! -- Integrated predictors are defined for EACH absoreber
  INTEGER, PUBLIC, PARAMETER :: MAX_N_INTEGRATED_PREDICTORS = 6

  ! -- The total number of predictors
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS = MAX_N_STANDARD_PREDICTORS + &
                                                   ( MAX_N_ABSORBERS * MAX_N_INTEGRATED_PREDICTORS )

  ! -- The number selected from the total to be
  ! -- used in the gas absorption algorithm
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS_USED = 6


  ! ----------------------------------------------
  ! Maximum number of polynomial orders for
  ! reconstructing the gas absorption coefficients
  ! ----------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_ORDERS = 10


  ! ----------------------------------------------
  ! The minimum absorber amount allowed based upon
  ! the smallest representable numbers.
  ! This value is equivalent to TINY(ONE)**0.25
  ! ----------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: MINIMUM_ABSORBER_AMOUNT = TEN**(-RANGE(ONE)/4)


  ! ---------------------------------------
  ! Numerical limits for the gas absorption
  ! coefficient reconstruction
  ! ---------------------------------------

  ! -- Numerical limits based on precision
!  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_EXP = 36.0436_fp_kind   ! ABS( LOG( TOLERANCE ) )
!  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_LOG = 4.5e+15_fp_kind   ! EXP( LIMIT_EXP )

  ! -- Numerical limits based on experiment.
  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_EXP = 20.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_LOG = 4.8e+08_fp_kind   ! EXP( LIMIT_EXP )


  ! ---------------------------------------
  ! Top-Of-Atmosphere (TOA) pressure in hPa
  ! ---------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: TOA_PRESSURE = 0.005_fp_kind


  ! -------------------------------------------------------
  ! Reciprocal gravity (scaled by 100 for use with pressure
  ! in hPa) used in computing integrated absorber amounts
  ! -------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: RECIPROCAL_GRAVITY = ONE / 980.665_fp_kind



  !#----------------------------------------------------------------------------#
  !#                       -- GeometryInfo PARAMETERS --                        #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------
  ! Limits on sensor angles.
  ! - Azimuth angles that are multiples
  !   of 2pi are not accepted.
  ! -----------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SENSOR_SCAN_ANGLE    = 65.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SENSOR_ZENITH_ANGLE  = 65.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SENSOR_AZIMUTH_ANGLE = 360.0_fp_kind


  ! -------------------------------------------------
  ! Limits on source angles.
  ! - The maximum source zenith angle should
  !   be determined by the maximum angle secant
  !   used in generating the gas absorption model
  !   coefficients, i.e. a secant of 2.25 => 63.6deg.
  !   Users have requested the Value be 85deg which
  !   has a secant of ~11.47.
  ! - Azimuth angles that are multiples
  !   of 2pi are not accepted.
  ! -------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SOURCE_ZENITH_ANGLE  = 85.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SECANT_SOURCE_ZENITH = 11.473711738554476_fp_kind

  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SOURCE_AZIMUTH_ANGLE = 360.0_fp_kind


  ! ----------------------------------------
  ! Default diffusivity angle and secant
  ! ACOS( 3/5 ) in degrees is (~53.13)
  ! Used to approximate the downwelling flux
  ! ----------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: DIFFUSIVITY_ANGLE  = 53.130102354156_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: DIFFUSIVITY_RADIAN = 0.927295218002_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: SECANT_DIFFUSIVITY = FIVE / THREE


  ! -----------------------------------------------------------
  ! Maximum flux angle definitions. Determined by the maximum
  ! angle secant used in generating the gas absorption model
  ! coefficients, i.e. a secant of 2.25 => 63.6deg. If the user
  ! inputs a value larger than this for the Flux_Zenith_Angle,
  ! the diffusivity angles are used instead
  ! -----------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_FLUX_ZENITH_ANGLE  = 63.612200038757_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SECANT_FLUX_ZENITH = 2.25_fp_kind



  !#----------------------------------------------------------------------------#
  !#            -- CloudScatter, RTSolution, AtmOptics PARAMETERS --            #
! This will be further broken down when it becomes more clear exactly where they are used.
  !#----------------------------------------------------------------------------#

  REAL( fp_kind ), PUBLIC, PARAMETER :: WATER_CONTENT_THRESHOLD = 0.000001_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: OPTICAL_DEPTH_THRESHOLD = 0.000001_fp_kind

  REAL( fp_kind ), PUBLIC, PARAMETER :: BS_THRESHOLD  = 0.000001_fp_kind  ! Was SCATTERING_ALBEDO_THRESHOLD
  REAL( fp_kind ), PUBLIC, PARAMETER :: SCATTERING_ALBEDO_THRESHOLD  = 0.000001_fp_kind  ! Eventually replace this with BS_THRESHOLD


  INTEGER, PUBLIC, PARAMETER :: MAX_N_LEGENDRE_TERMS = 20
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PHASE_ELEMENTS = 1
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STREAMS = 20 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ANGLES = MAX_N_STREAMS + 1 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STOKES = 4
  
  LOGICAL, PUBLIC, PARAMETER :: HGPHASE = .FALSE.
                                                                                                        


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Set_Max_n_Channels
! 
! PURPOSE:
!       Subroutine to set the protected variable MAX_N_CHANNELS value in the
!       CRTM_Parameters module. This should *only* be done during the CRTM
!       initialisation.
!
! CATEGORY:
!       CRTM : Parameters
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Set_Max_n_Channels( Value )
!
! INPUT ARGUMENTS:
!       Value:        The maximum number of channels.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       This subroutines changes the value of the MAX_N_CHANNELS pseudo-parameter
!       in the CRTM_Parameters module.
!
! RESTRICTIONS:
!       None.
!       
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Set_Max_n_Channels( Value )
    INTEGER, INTENT( IN ) :: Value
    MAX_N_CHANNELS = Value
  END SUBROUTINE CRTM_Set_Max_n_Channels


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Reset_Max_n_Channels
! 
! PURPOSE:
!       Subroutine to reset the protected variable MAX_N_CHANNELS value in the
!       CRTM_Parameters module to an invalid value. This should *only* be done
!       during the CRTM destruction.
!
! CATEGORY:
!       CRTM : Parameters
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Reset_Max_n_Channels()
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       This subroutines changes the value of the MAX_N_CHANNELS pseudo-parameter
!       in the CRTM_Parameters module.
!
! RESTRICTIONS:
!       None.
!       
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Reset_Max_n_Channels()
    MAX_N_CHANNELS = RESET_VALUE
  END SUBROUTINE CRTM_Reset_Max_n_Channels


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Get_Max_n_Channels
! 
! PURPOSE:
!       Subroutine to GET the protected variable MAX_N_CHANNELS value stored
!       in the CRTM_Parameters module.
!
! CATEGORY:
!       CRTM : Parameters
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Get_Max_n_Channels( Value, Is_Set )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Value:        The maximum number of channels.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Is_Set:       Logical flag for determining whether or not the
!                     maximum number of channels has been set.
!                     If == .TRUE.  the MAX_N_CHANNELS protected variable is
!                                   set to a valid value.
!                        == .FALSE. the MAX_N_CHANNELS protected variable
!                                   value is invalid
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Get_Max_n_Channels( Value, Is_Set )
    INTEGER,           INTENT( OUT ) :: Value
    LOGICAL, OPTIONAL, INTENT( OUT ) :: Is_Set
    Value = MAX_N_CHANNELS
    IF ( PRESENT( Is_Set ) ) THEN
      IF ( Value /= RESET_VALUE ) THEN
        Is_Set = .TRUE.
      ELSE
        Is_Set = .FALSE.
      END IF
    END IF
  END SUBROUTINE CRTM_Get_Max_n_Channels

END MODULE CRTM_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Parameters.f90,v 2.6.2.1 2005/08/16 19:02:08 qliu Exp $
!
! $Date: 2005/08/16 19:02:08 $
!
! $Revision: 2.6.2.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Parameters.f90,v $
! Revision 2.6.2.1  2005/08/16 19:02:08  qliu
! - Added literal constant and HGPhase
!
! Revision 2.6  2005/08/15 16:57:30  paulv
! - Removed DIFFUSE_FACTOR parameter.
!
! Revision 2.5  2005/08/15 15:13:17  yhan
! - Temporarily added back the SCATTERING_ALBEDO_THRESHOLD parameter for
!   AtmOptics testing.
!
! Revision 2.4  2005/08/04 20:33:13  paulv
! - Renamed parameter SCATTERING_ALBEDO_THRESHOLD to BS_THRESHOLD. Change was
!   introduced to accomodate changes in the CRTM_AtmOptics module.
!
! Revision 2.3  2005/08/04 18:58:33  paulv
! - Added parameters for use in the CloudScatter, RTSolution, and AtmOptics
!   modules.
!
! Revision 2.2  2005/07/15 16:44:20  paulv
! - Added some CloudScatter, RTSolution, and AtmOptics parameters.
!
! Revision 2.1  2005/06/29 01:18:19  paulv
! - Modifed for use with updated CRTM_GeometryInfo structure.
!
! Revision 2.0  2004/11/05 16:15:45  paulv
! - New version. Tidied up parameter definitions; better documentation.
! - Changed Get, Set, and Reset routines for the protected variable
!   MAX_N_CHANNELS to have the prefix "CRTM_".
! - Added header documentation for the contained subprograms.
!
! Revision 1.4  2004/06/24 19:01:19  paulv
! - Corrected a declaration error for the ABSORBER_INDEX parameter array.
!   It is an INTEGER array but was declared as CHARACTER(*)! Sheesh.
!
! Revision 1.3  2004/06/18 20:15:40  paulv
! - Conact email change.
!
! Revision 1.2  2004/06/04 19:39:12  paulv
! - Added information about the absorbers. For OPTRAN, these are different
!   from the HITRAN specification.
!
! Revision 1.1  2004/05/28 20:05:31  paulv
! Initial checkin.
!
!
!
!
