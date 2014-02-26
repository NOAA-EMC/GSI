module absorber_profile
!$$$  module documentation block
!                .      .    .                                       .
! module:   absorber_profile
!   prgmmr: van delst        org: np20                date: 2000-08-01
!
! abstract:  Module containing routines to compute the integrated 
!            absorber profiles
!
! module history log:
!   2000-08-01  van delst, paul
!   2000-08-24  van delst - initial check in
!   2000-08-31  van delst - add documentation delimiters; update 
!                           documentation headers
!   2000-11-09  van delst - add tangent linear form of the absorber 
!                           profile routines
!   2001-03-26  van delst - numerous changes (see end of this file)
!   2001-05-29  van delst - cosmetic changes; other changes (see end 
!                           of this file)
!   2001-06-05  van delst - change adjoint routine slightly to make 
!                           adjoint calcs a bit clearer when looking 
!                           at the tangent-linear code; correct bug 
!                           in TOA layer pressure_AD calculation
!   2001-08-01  van delst - removed use of module ABSORBER_SPACE and 
!                           replaced it with 
!             USE transmittance_coefficients, ONLY : absorber_space_levels
!                           to reflect changes in code
!   2001-08-16  van delst - update documentation
!   2001-08-31  van delst - alter method of searching for bracketing 
!                           absorber space layers in 
!                           FIND_ABSORBER_LAYER_INDEX
!   2001-09-25  van delst - change the calculation of the bracketing 
!                           absorber space layer in sbroutine 
!                           FIND_ABSORBER_LAYER_INDEX
!   2002-10-21  van delst - Synchronisation with repository for 
!                           algorithm upgrade
!   2003-02-04  van delst - new release
!   2004-06-18  treadon   - add NCEP doc block
!
!
! Subroutines Included:
!   Compute_Absorber_Amount    - compute the integrated absorber profiles.
!                                Currently the absorbers are:
!                                   - Water vapor
!                                   - Dry/fixed gases
!                                   - Ozone
!   Compute_Absorber_Amount_TL - compute the tangent-linear form of the 
!                                integrated absorber profiles
!   Compute_Absorber_Amount_AD - compute the adjoint of the integrated 
!                                absorber profiles
!
! Functions Included:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
!------------------------------------------------------------------------------
!M+
! NAME:
!       absorber_profile
!
! PURPOSE:
!       Module containing routines to compute the integrated absorber profiles.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE absorber_profile
!
! OUTPUTS:
!       None.
!
! MODULES:
!       type_kinds:      Module containing data type kind definitions.
!
!       parameters:      Module containing parameter definitions for the
!                        RT model.
!                        USEs: TYPE_KINDS module
!
! CONTAINS:
!       Compute_Absorber_Amount:     PUBLIC subroutine to compute the integrated
!                                    absorber profiles. Currently the absorbers
!                                    are:
!                                      - Water vapor
!                                      - Dry/fixed gases
!                                      - Ozone
!
!       Compute_Absorber_Amount_TL:  PUBLIC subroutine to compute the tangent-
!                                    linear form of the integrated absorber 
!                                    profiles.
!
!       Compute_Absorber_Amount_AD:  PUBLIC subroutine to compute the adjoint of
!                                    the integrated absorber profiles.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP 01-Aug-2000
!                       pvandelst@ncep.noaa.gov
!
!       Adapted from code written by: Thomas J.Kleespies
!                                     NOAA/NESDIS/ORA
!                                     tkleespies@nesdis.noaa.gov
!
!  Copyright (C) 2000 Thomas Kleespies, Paul van Delst
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

!MODULE absorber_profile


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE type_kinds, ONLY : fp_kind
  USE CRTM_Parameters
!  USE transmittance_coefficients, ONLY : alpha, &
!                                         absorber_space_levels


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! ----------------------------------
  ! Explicit visibility of subprograms
  ! ----------------------------------

  PUBLIC :: Compute_Absorber_Amount
  PUBLIC :: Compute_Absorber_Amount_TL
  PUBLIC :: Compute_Absorber_Amount_AD
!  PUBLIC :: find_absorber_layer_index


CONTAINS



!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Absorber_Amount
!
! PURPOSE:
!       Subroutine to compute the integrated profiles for all the
!       absorbers. Currently the number of absorbers are:
!         - Water vapor
!         - Dry/fixed gases (pressure == absorber amount)
!         - Ozone
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Absorber_Amount( pressure,    &  ! Input,  K
!                                     water_vapor, &  ! Input,  K
!                                     ozone,       &  ! Input,  K
!                                     absorber     )  ! Output, 0:K x J
!
! INPUT ARGUMENTS:
!       pressure:     Profile LEVEL pressure array. The array must be ordered
!                     from the top of the atmosphere down. The first value
!                     is the pressure of the lower level of the first layer.
!                     UNITS:      hPa
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  K
!                     ATTRIBUTES: INTENT( IN )
!
!       water_vapor:  Profile LAYER water vapor mixing ratio array. The array
!                     must be ordered from the top of the atmosphere down.
!                     UNITS:      g/kg
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  K
!                     ATTRIBUTES: INTENT( IN )
!
!       ozone:        Profile LAYER ozone mixing ratio array. The array
!                     must be ordered from the top of the atmosphere down.
!                     UNITS:      ppmv
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  K
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       absorber:     Profile LEVEL integrated absorber amount array. The
!                     vertical coordinate of the array is ordered from the
!                     top of the atmosphere down. Note that the value is
!                     the total integrated value to the lower level of each
!                     layer
!                     UNITS:      Varies with absorber
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  0:K x J
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The function calculates and accumulates the integrated absorber
!       amount for the wet, dry, and ozo absorbers through k atmospheric
!       layers.
!
!       For water vapor and ozone, the following is used,
!
!                     __ k
!                 1  \
!         A(k) = ---  >  q(i).dP(i)
!                 g  /__
!                      i=1
!
!       where dP(i) = pressure(i) - pressure(i-1)
!             q(i)  = layer water vapor or ozone amount.
!
!       For the dry gases, the layer lower level pressure is used as a
!       proxy integrated absorber amount,
!
!         A(k) = pressure(k)
!
!       And,
!
!         A_WET(0) = ZERO
!         A_DRY(0) = TOA_PRESSURE
!         A_OZO(0) = ZERO
!
!       where TOA_PRESSURE is the lowest pressure used in the LBL calculations.
!
!       The routine loop over layers, k, with each absorber amount calculated
!       independently (like looping over absorber, j). This is opposite
!       to what is recommended (since the arrays are dimensioned [k,j]) but 
!       later use of the arrays require the [k,j] ordering where the absorber
!       loop over j is the OUTSIDE loop (see COMPUTE_TRANSMITTANCE()).
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Absorber_Amount( Pressure,    &  ! Input,  K
                                      Water_Vapor, &  ! Input,  K
                                      Ozone,       &  ! Input,  K
                                      Absorber     )  ! Output, 0:K x J


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Pressure     ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Water_Vapor  ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Ozone        ! Input,  K

    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: Absorber     ! Output, 0:K x J


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: dP

    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE 0'TH LEVEL AMOUNTS --                     #
    !#                                                                          #
    !# This is done so that layer differences and averages can be calculated    #
    !# simply in the predictor and transmittance routines.                      #
    !#--------------------------------------------------------------------------#

    Absorber( 0, 1 ) = ZERO          ! Wet
    Absorber( 0, 2 ) = TOA_PRESSURE  ! Dry
    Absorber( 0, 3 ) = ZERO          ! Ozo



    !#--------------------------------------------------------------------------#
    !#                   -- NADIR LEVEL ABSORBER PROFILES --                    #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! First layer. The reason this isn't in the 
    ! loop below is due to the dP calculation.
    ! -----------------------------------------

    dP = Pressure(1) - TOA_PRESSURE

    Absorber( 1, 1 ) = RECIPROCAL_GRAVITY * dP * Water_Vapor( 1 )
    Absorber( 1, 2 ) = Pressure( 1 )
    Absorber( 1, 3 ) = RECIPROCAL_GRAVITY * dP * Ozone( 1 )


    ! --------------------------------
    ! Loop over layers, TOA - 1 -> SFC
    ! --------------------------------

    k_layer_loop: DO k = 2, SIZE( Pressure )

      dP = Pressure( k ) - Pressure( k-1 )

      Absorber( k, 1 ) = Absorber( k-1, 1 ) + ( RECIPROCAL_GRAVITY * dP * Water_Vapor( k ) )
      Absorber( k, 2 ) = Pressure( k )
      Absorber( k, 3 ) = Absorber( k-1, 3 ) + ( RECIPROCAL_GRAVITY * dP * Ozone( k ) )

    END DO k_layer_loop

  END SUBROUTINE Compute_Absorber_Amount





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Absorber_Amount_TL
!
! PURPOSE:
!       PUBLIC subroutine to compute the tangent linear form of the integrated
!       profiles for all the absorbers. Currently the number of absorbers
!       are:
!         - Water vapor
!         - Dry/fixed gases (pressure == absorber amount)
!         - Ozone
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Absorber_Amount_TL( &
!                                        ! -- Forward input
!                                        pressure,       &  ! Input, K
!                                        water_vapor,    &  ! Input, K
!                                        ozone,          &  ! Input, K
!
!                                        ! -- Tangent-linear input
!                                        pressure_TL,    &  ! Input, K
!                                        water_vapor_TL, &  ! Input, K
!                                        ozone_TL,       &  ! Input, K
!
!                                        ! -- Tangent-linear output
!                                        absorber_TL     )  ! Output, 0:K x J
!
! INPUT ARGUMENTS:
!       pressure:        Profile LEVEL pressure array.
!                        UNITS:      hPa
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       water_vapor:     Profile LAYER water vapor mixing ratio array.
!                        UNITS:      g/kg
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       ozone:           Profile LAYER ozone mixing ratio array.
!                        UNITS:      ppmv
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       pressure_TL:     Profile LEVEL tangent-linear pressure array,
!                        i.e. the pressure perturbation.
!                        UNITS:      hPa
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K, number of levels - 1
!                        ATTRIBUTES: INTENT( IN )
!
!       water_vapor_TL:  Profile LAYER tangent-linear water vapor mixing
!                        ratio array, i.e. the water vapor mixing
!                        ratio perturbation.
!                        UNITS:      g/kg
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       ozone_TL:        Profile LAYER tangent-linear ozone mixing ratio
!                        array i.e. the ozone mixing ratio perturbation.
!                        UNITS:      ppmv
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       absorber_TL:     Profile LEVEL tangent-linear average integrated 
!                        absorber amount array.
!                        UNITS:      Varies with absorber.
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  0:K x J
!                        ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The function calculates and accumulates the tangent-linear form
!       of the integrated absorber amount for the wet, dry, and ozo
!       absorbers through k atmospheric layers.
!
!       For water vapor and ozone, the following is used,
!
!                        __ k
!                    1  \
!         A_TL(k) = ---  >  [ q_TL(i).dP(i) + q(i).dP_TL(i) ]
!                    g  /__
!                          i=1
!
!       where dP(i)    = pressure(i) - pressure(i-1)
!             dP_TL(i) = pressure_TL(i) - pressure_TL(i-1)
!             q(i)     = layer water vapor or ozone amount.
!             q_TL(i) = tangent-linear layer water vapor or ozone amount.
!
!       For the dry gases, pressure is used as a proxy absorber amount,
!
!         A_TL(k) = pressure_TL(k)
!
!       The routine loop over layers, k, with each absorber amount calculated
!       independently (like looping over absorber, j). This is opposite
!       to what is recommended (since the arrays are dimensioned [k,j]) but 
!       later use of the arrays require the [k,j] ordering where the absorber
!       loop over j is the OUTSIDE loop (see COMPUTE_TRANSMITTANCE_TL()).
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Absorber_Amount_TL( &
                                         ! -- Forward input
                                         Pressure,       &  ! Input,  K
                                         Water_Vapor,    &  ! Input,  K
                                         Ozone,          &  ! Input,  K

                                         ! -- Tangent-linear input
                                         Pressure_TL,    &  ! Input,  K
                                         Water_Vapor_TL, &  ! Input,  K
                                         Ozone_TL,       &  ! Input,  K

                                         ! -- Tangent-linear output
                                         Absorber_TL     )  ! Output, 0:K x J


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Pressure        ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Water_Vapor     ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Ozone           ! Input,  K

    ! -- Tangent-linear input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Pressure_TL     ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Water_Vapor_TL  ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Ozone_TL        ! Input,  K

    ! -- Tangent-linear output
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: Absorber_TL     ! Output, 0:K x J



    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: dP
    REAL( fp_kind ) :: dP_TL



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE 0'TH LEVEL AMOUNTS --                     #
    !#                                                                          #
    !# This is done so that layer differences and averages can be calculated    #
    !# simply in the predictor and transmittance routines.                      #
    !#--------------------------------------------------------------------------#

    absorber_TL( 0, : ) = ZERO



    !#--------------------------------------------------------------------------#
    !#           -- NADIR LEVEL TANGENT-LINEAR ABSORBER PROFILES --             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! First layer. The reason this isn't in the 
    ! loop below is due to the dP calculation.
    ! -----------------------------------------

    dP =    Pressure(1) - TOA_PRESSURE
    dP_TL = Pressure_TL(1)

    Absorber_TL( 1, 1 ) = RECIPROCAL_GRAVITY * ( ( dP    * Water_Vapor_TL( 1 ) ) + &
                                                 ( dP_TL * Water_Vapor( 1 )    )   )
    Absorber_TL( 1, 2 ) = Pressure_TL(1)
    Absorber_TL( 1, 3 ) = RECIPROCAL_GRAVITY * ( ( dP    * Ozone_TL( 1 ) ) + &
                                                 ( dP_TL * Ozone( 1 )    )   )


    ! --------------------------------
    ! Loop over layers, TOA - 1 -> SFC
    ! --------------------------------

    k_layer_loop: DO k = 2, SIZE( pressure )

      ! -- Layer pressure difference
      dP    = Pressure( k )    - Pressure( k-1 )
      dP_TL = Pressure_TL( k ) - Pressure_TL( k-1 )

      ! -- Integrated absorber amounts
      Absorber_TL( k, 1 ) = Absorber_TL( k-1, 1 ) + &
                            ( RECIPROCAL_GRAVITY * ( ( dP    * Water_Vapor_TL( k ) ) + &
                                                     ( dP_TL * Water_Vapor( k )    )   ) )
      Absorber_TL( k, 2 ) = Pressure_TL( k )

      Absorber_TL( k, 3 ) = Absorber_TL( k-1, 3 ) + &
                            ( RECIPROCAL_GRAVITY * ( ( dP    * Ozone_TL( k ) ) + &
                                                     ( dP_TL * Ozone( k )    )   ) )

    END DO k_layer_loop

  END SUBROUTINE Compute_Absorber_Amount_TL





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Absorber_Amount_AD
!
! PURPOSE:
!       PUBLIC subroutine to compute the adjoint of the integrated
!       profiles for all the absorbers. Currently the number of absorbers
!       are:
!         - Water vapor
!         - Dry/fixed gases (pressure == absorber amount)
!         - Ozone
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Absorber_Amount_AD( &
!                                        ! -- Forward input
!                                        pressure,       &  ! Input, K
!                                        water_vapor,    &  ! Input, K
!                                        ozone,          &  ! Input, K
!
!                                        ! -- Adjoint input
!                                        absorber_AD,    &  ! In/Output, 0:K x J
!
!                                        ! -- Adjoint output
!                                        pressure_AD,    &  ! In/Output, K
!                                        water_vapor_AD, &  ! In/Output, K
!                                        ozone_AD        )  ! In/Output, K
!
! INPUT ARGUMENTS:
!       pressure:        Profile LEVEL pressure array.
!                        UNITS:      hPa
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       water_vapor:     Profile LAYER water vapor mixing ratio array.
!                        UNITS:      g/kg
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       ozone:           Profile LAYER ozone mixing ratio array.
!                        UNITS:      ppmv
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN )
!
!       absorber_AD:     Profile LEVEL adjoint average integrated 
!                        absorber amount array.
!                        ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                        UNITS:      Varies with absorber
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  0:K x J
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       pressure_AD:     Profile LAYER adjoint pressure array.
!                        UNITS:      hPa
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       water_vapor_AD:  Profile LAYER adjoint water vapor array.
!                        UNITS:      g/kg
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       ozone_AD:        Profile LAYER adjoint ozone water vapor array.
!                        UNITS:      ppmv
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  K
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       The input argument ABSORBER_AD is set to zero on output.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The function calculates the adjoints of the integrated path length
!       through k atmospheric layers. Given the tangent-linear expression,
!
!                        __ k
!                    1  \
!         u_TL(k) = ---  >  [ q_TL(i).dP(i) + q(i).dP_TL(i) ]
!                    g  /__
!                          i=1
!
!       for the profile inputs (water vapor and ozone), the adjoints are
!       given by,
!
!                              1
!         dP_AD     = dP_AD + --- q(k).u_AD(k)
!                              g
!
!                                1
!         q_AD(k)   = q_AD(k) + --- dP.u_AD(k)
!                                g
!
!
!         u_AD(k-1) = u_AD(k-1) + u_AD(k)
!
!         u_AD(k)   = 0.0
!
!       looping over the layer index, k, from the surface (max.) to the
!       top of the atmosphere. For the dry absorber component, which uses
!       pressure as the absorber amount proxy, the tangent-linear expression
!       is simply,
!
!         u_TL(k) = p_TL(k)
!
!       The adjoint is thus,
!
!         p_AD(k) = p_AD(k) + u_AD(k)
!
!         u_AD(k) = 0.0
!
!       The actual execution of the above expressions in the code are done to
!       prevent recalculation of the the same quantities in individual loops.
!
!       Typically, the adjoint quantities that correspond to INPUTS in the
!       forward model (pressure_AD, water_vapor_AD, and ozone_AD) are set to
!       0.0 before entering this routine, and the adjoint quantities 
!       corresponding to OUTPUTS in the forward model (absorber_AD) are set to
!       1.0 before entering this routine. This will return the gradient vector
!       of the output variable with respect to the input variable (partial
!       derivative.) E.g. if the forward problem is defined as,
!
!
!         u = f(q,p)
!
!       then the TL form would be,
!
!                 df          df
!         u_TL = ---- q_TL + ---- p_TL   (NOTE: the df/dX is partial)
!                 dq          dP
!
!       If u_AD = 1.0 and q_AD,p_AD = 0.0 on input, then on output,
!
!                 df            df
!         q_AD = ---- , p_AD = ---- , and u_AD = 0.0
!                 dq            dP 
! 
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE  Compute_Absorber_Amount_AD( &
                                          ! -- Forward input
                                          Pressure,       &  ! Input, K
                                          Water_Vapor,    &  ! Input, K
                                          Ozone,          &  ! Input, K

                                          ! -- Adjoint input
                                          Absorber_AD,    &  ! In/Output, 0:K x J

                                          ! -- Adjoint output
                                          Pressure_AD,    &  ! In/Output, K
                                          Water_Vapor_AD, &  ! In/Output, K
                                          Ozone_AD        )  ! In/Output, K



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Pressure        ! Input, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Water_Vapor     ! Input, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Ozone           ! Input, K

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN OUT ) :: Absorber_AD     ! In/Output, 0:K x J

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Pressure_AD     ! In/Output, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Water_Vapor_AD  ! In/Output, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Ozone_AD        ! In/Output, K


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: dP
    REAL( fp_kind ) :: dP_AD



    !#--------------------------------------------------------------------------#
    !#                     -- CALCULATE ADJOINT VALUES --                       #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------
    ! Loop over atmospheric layers, SFC -> TOA-1
    ! ------------------------------------------

    k_layer_loop: DO k = SIZE( Pressure ), 2, -1

      ! -- Layer pressure difference
      dP = Pressure( k ) - Pressure( k-1 )

      ! -- Ozone amount adjoint
      Ozone_AD( k ) = Ozone_AD( k ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( k, 3 ) )

      ! -- Pressure adjoint
      Pressure_AD( k ) = Pressure_AD( k ) + Absorber_AD( k, 2 )

      ! -- Water vapor amount adjoint
      Water_Vapor_AD( k ) = Water_Vapor_AD( k ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( k, 1 ) )

      ! -- Layer pressure difference adjoint
      dP_AD = RECIPROCAL_GRAVITY * ( ( Water_Vapor( k ) * Absorber_AD( k, 1 ) ) + &
                                     ( Ozone( k )       * Absorber_AD( k, 3 ) )   )
      Pressure_AD( k )   = Pressure_AD( k )   + dP_AD
      Pressure_AD( k-1 ) = Pressure_AD( k-1 ) - dP_AD

      ! -- Previous layer absorber amounts
      Absorber_AD( k-1, 3 ) = Absorber_AD( k-1, 3 ) + Absorber_AD( k, 3 )
      Absorber_AD( k, 3 ) = ZERO

      Absorber_AD( k, 2 ) = ZERO

      Absorber_AD( k-1, 1 ) = Absorber_AD( k-1, 1 ) + Absorber_AD( k, 1 )
      Absorber_AD( k, 1 ) = ZERO

    END DO k_layer_loop


    ! --------------------------------
    ! First layer adjoint calculations
    ! --------------------------------

    ! -- First layer dP
    dP = Pressure(1) - TOA_PRESSURE

    ! -- Adjoint of first layer OZONE absorber amount calculation
    dP_AD         = RECIPROCAL_GRAVITY * Ozone( 1 ) * Absorber_AD( 1, 3 )
    Ozone_AD( 1 ) = Ozone_AD( 1 ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( 1, 3 ) )
    Absorber_AD( 1, 3 ) = ZERO

    ! -- Adjoint of first layer DRY GAS absorber amount calculation
    Pressure_AD( 1 ) = Pressure_AD( 1 ) + Absorber_AD( 1, 2 )
    Absorber_AD( 1, 2 ) = ZERO

    ! -- Adjoint of first layer WATER VAPOR absorber amount calculation
    dP_AD               = dP_AD + ( RECIPROCAL_GRAVITY * Water_Vapor( 1 ) * Absorber_AD( 1, 1 ) )
    Water_Vapor_AD( 1 ) = Water_Vapor_AD( 1 ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( 1, 1 ) )
    Absorber_AD( 1, 1 ) = ZERO

    ! -- Adjoint of pressure difference calculation
    Pressure_AD( 1 ) = Pressure_AD( 1 ) + dP_AD
    dP_AD = ZERO


  END SUBROUTINE Compute_Absorber_Amount_AD

END MODULE absorber_profile


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: absorber_profile.f90,v 1.4 2004/07/02 15:18:29 treadon Exp $
!
! $Date: 2004/07/02 15:18:29 $
!
! $Revision: 1.4 $
!
! $Name: ncep-gsi-2004_06 $
!
! $State: Exp $
!
! $Log: absorber_profile.f90,v $
! Revision 1.4  2004/07/02 15:18:29  treadon
! add NCEP docblock
!
! Revision 1.3  2004/05/06 15:52:40  treadon
! modify !Name: comment near end of file
!
! Revision 1.2  2004/02/18 16:14:02  treadon
! unified GSI
!
! Revision 2.0  2003/02/04 21:13:05  paulv
! - New release.
!
! Revision 1.12  2002/10/21 20:14:40  paulv
! - Synchronisation with repository for algorithm upgrade. Incomplete.
!
! Revision 1.11  2001/09/25 15:51:29  paulv
! - Changed the calculation of the bracketing absorber space layer in
!   sbroutine FIND_ABSORBER_LAYER_INDEX from
!     MIN( ka, MAX_N_ABSORBERS_LAYERS )
!   to
!     MAX( MIN( ka, MAX_N_ABSORBERS_LAYERS ), 1 )
!   so as to avoid the result ever being zero - which could happen before if
!   adjacent layers of the input absorber profile were zero.
!
! Revision 1.10  2001/08/31 20:41:18  paulv
! - Altered method of searching for bracketing absorber space layers in
!   FIND_ABSORBER_LAYER_INDEX. Previosuly a trickle down search was performed.
!   Now the actual corresponding layer is calculated using the exponential
!   factor used in generating the absorber space.
!
! Revision 1.9  2001/08/16 16:30:38  paulv
! - Updated documentation.
!
! Revision 1.8  2001/08/01 16:36:34  paulv
! - Removed use of module ABSORBER_SPACE and replaced it with
!     USE transmittance_coefficients, ONLY : absorber_space_levels
!   to reflect changes in code. The absorber space levels are no longer
!   calculated during model initialisation, but are precalculated and stored
!   in the transmittance coefficient data file.
!
! Revision 1.7  2001/06/05 21:18:10  paulv
! - Changed adjoint routine slightly to make adjoint calcs a bit clearer
!   when looking at the tangent-linear code.
! - Corrected bug in TOA layer pressure_AD calculation.
!
! Revision 1.6  2001/05/29 17:32:51  paulv
! - Some cosmetic changes
! - Removed subtraction of the TOA_PRESSURE parameter from the DRY absorber
!   calculation. This was causing the upper level channels to produce
!   spurious numbers in the forward calculation.
! - Added the  FIND_ABSORBER_LAYER_INDEX routine. Removed it from the FORWARD_MODEL
!   module. It seemed more appropriate in this one.
! - Using pressure array data directly in first layer calcs rather than
!   dp variable.
!
! Revision 1.5  2001/03/26 18:45:59  paulv
! - Now use TYPE_KINDS module parameter FP_KIND to set the floating point
!   data type.
! - Module parameter RECIPROCAL_GRAVITY moved to PARAMETERS module.
! - ONLY clause used in USE PARAMETERS statement. Only parameters available
!   in ABSORBER_PROFILE module are ZERO, TOA_PRESSURE, and RECIPROCAL_GRAVITY.
! - Output ABSORBER argument is now dimensioned as 0:K. This eliminates the
!   need for using an ABSORBER_KM1 variable in other routines that use the
!   ABSORBER array variable where the layer loop always goes from 1 -> n_layers.
! - Removed output arguments of AVE_ABSORBER and DELTA_ABSORBER due to the
!   ABSORBER dimension change to 0:K. Calculating the average and layer
!   difference absorber amounts in other routines can now be done simply
!   by averaging or differencing ABOSRBER(K) and ABSORBER(K-1) even for
!   layer #1.
! - Integration of absorber amount for the TOA layer is done OUTSIDE of the
!   layer loop. This avoids the need for a PRESSURE_KM1 variable since
!   pressure is dimensioned 1:K.
! - Layer loop, thus, goes from 2 -> n_layers.
! - Changed order of argument list in COMPUTE_PREDICTORS_TL and its
!   associated routines. All forward arguments are listed followed by
!   the tangent-linear arguments rather than interspersing them as before.
! - Added adjoint routine COMPUTE_ABSORBER_AMOUNT_AD.
!
! Revision 1.4  2001/03/26 18:30:54  paulv
! - Removed integrate_absorber_profile and integrate_absorber_profile_tl
!   functions. Integration is now done in-line in the main routines.
!
! Revision 1.3  2000/11/09 20:29:40  paulv
! - Added tangent linear form of the absorber profile routines.
!
! Revision 1.2  2000/08/31 19:36:31  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.1  2000/08/24 13:11:27  paulv
! Initial checkin.
!
!
!
!
