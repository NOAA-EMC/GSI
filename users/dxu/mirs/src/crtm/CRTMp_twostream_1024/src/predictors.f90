module predictors
!$$$  module documentation block
!                .      .    .                                       .
! module:   predictors
!   prgmmr: van delst        org: np20                date: 2000-07-11
!
! abstract:  Module containing routines to compute the predictor 
!            profiles for the transmittance calculation
!
! module history log:
!   2000-07-11  van delst, paul
!   2000-08-08  van delst - initial check in
!   2000-08-21  van delst - standard and integrated predictor sets 
!                           calculated in separate functions; predictor
!                           values saved in linear store; wrapper function
!                           "compute_predictors" added
!   2000-08-24  van delst - add optional NO_STANDARD argument
!   2000-08-31  van delst - add documentation delimiters; update 
!                           documentation headers
!   2000-11-09  van delst - add tangent linear form of routines in this
!                           module; change names of PRIVATE routines used
!                           to compute predictors; initial setup in place 
!                           for dynamic allocation of predictor arrays
!   2001-01-24  van delst - latest test version
!   2001-04-03  van delst - comment out shared predictor data arrays; remove
!                           reference to profile number; correct bug in 1st
!                           and 2nd order predictor calculation
!   2001-05-04  van delst - numerous changes (see description at end of this file)
!   2001-05-29  van delst - update documentation
!   2001-07-12  van delst - numerous changes (see description at end of this file)
!   2001-08-16  van delst - update documentation
!   2001-10-01  van delst - add "Name" to RCS keyword list
!   2003-02-04  van delst - new release
!   2003-02-05  van delst - correct bug in standard predictor adjoint
!   2003-06-30  van delst - remove use of pointers; pass actual array slices
!   2003-07-08  van delst - fix bugs, numerous changes in speed up code
!   2004-06-21  treadon   - add NCEP doc block
!
! Subroutines Included:
!   Compute_Predictors    - PUBLIC subroutine to calculate the predictor profiles
!   Compute_Predictors_TL - PUBLIC subroutine to calculate the tangent-linear
!                           predictor profiles
!   Compute_Predictors_AD - PUBLIC subroutine to calculate the adjoint of the
!                           predictor profiles
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
!       Predictors
!
! PURPOSE:
!       Module containing routines to compute the predictor profiles for the
!       transmittance calculation.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE Predictors
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:      Module containing data type kind definitions.
!
!       Parameters:      Module containing parameter definitions for the
!                        RT model.
!                        USEs: TYPE_KINDS module
!
!       Error_Handler:   Module to define error codes and handle error
!                        conditions.
!                        USEs: FILE_UTILITY module
!
! CONTAINS:
!       Compute_Predictors:             PUBLIC subroutine to calculate the
!                                       predictor profiles.
!
!       Compute_Predictors_TL:          PUBLIC subroutine to calculate the 
!                                       tangent-linear predictor profiles.
!
!       Compute_Predictors_AD:          PUBLIC subroutine to calculate the
!                                       adjoint of the predictor profiles.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!
!       Updated predictor algorithm by Yoshihiko Tahara
!                                      UCAR/JMA visiting scientent
!                                      NOAA/NCEP/EMC
!                                      Yoshihiko.Tahara@noaa.gov
!
!  Copyright (C) 2000, 2002 Paul van Delst, Yoshihiko Tahara
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

!MODULE Predictors


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind
  USE CRTM_Parameters
  USE Error_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Compute_Predictors
  PUBLIC :: Compute_Predictors_TL
  PUBLIC :: Compute_Predictors_AD


CONTAINS



!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Predictors
!
! PURPOSE:
!       PUBLIC routine to calculate the forward transmittance model predictors.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Predictors ( Pressure,    &  ! Input,  K
!                                 Temperature, &  ! Input,  K
!                                 Water_Vapor, &  ! Input,  K
!                                 Absorber,    &  ! Input,  0:K x J
!
!                                 Predictor,   &  ! Output, I x K
!
!                                 no_standard  )  ! Optional input
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer average pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer average temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor:      Profile layer average water vapor mixing ratio array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber:         Profile LEVEL integrated absorber amount array.
!                         UNITS:      Varies with absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  0:K x J
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       no_standard:      If present, the standard predictors are not calculated.
!                         This prevents recalculation of the standard Predictors
!                         is only the view angle has changed - which only affects
!                         the integrated predictors.
!                         UNITS:      None
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:        Profile layer predictors array.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  I x K
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Compute_Std_Predictors:   PRIVATE function to compute the standard
!                                 (Absorber independent) predictor set.
!
!       Compute_Int_Predictors:   PRIVATE function to compute the Absorber
!                                 integrated predictor set.
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
!       The predictors used in the transmittance model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    water vapor    dry gases        ozone
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors ( Pressure,    &  ! Input,  K
                                  Temperature, &  ! Input,  K
                                  Water_Vapor, &  ! Input,  K
                                  Absorber,    &  ! Input,  0:K x J

                                  Predictor,   &  ! Output, I x K

                                  no_standard  )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )            :: Pressure      ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )            :: Temperature   ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )            :: Water_Vapor   ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )            :: Absorber      ! 0:K x J

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT )           :: Predictor     ! I x K

    INTEGER,                             INTENT( IN ),  OPTIONAL :: no_standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i1, i2, j

    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE THE STANDARD PREDICTORS IF NEEDED --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( no_standard ) ) THEN

      CALL Compute_Std_Predictors( Pressure,      &
                                   Temperature,   &
                                   Water_Vapor,   &
                                   Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) )

    END IF


    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, SIZE( Absorber, DIM = 2 )

      ! -- Determine indices of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Compute the predictors for the current absorber
      CALL Compute_Int_Predictors( Pressure, &
                                   Temperature, &
                                   Absorber( 0:, j ), &
                                   Predictor( i1:i2, : ) )

    END DO j_Absorber_loop

  END SUBROUTINE Compute_Predictors



!--------------------------------------------------------------------------------
!P+
! NAME:
!       Compute_Std_Predictors
!
! PURPOSE:
!       PRIVATE function to compute the standard, absorber independent
!       predictor set for the forward transmittance model.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Std_Predictors( Pressure,    &
!                                    Temperature, &
!                                    Water_Vapor, &
!                                    Predictor    )
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer average pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer average temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor:      Profile layer average water vapor mixing ratio array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor:        Array containing the calculated standard predictor set.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Istd x K
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       The standard predictors are the following:
!
!         1) Temperature, T
!         2) Pressure, P
!         3) T^2
!         4) P^2
!         5) T.P
!         6) T^2.P
!         7) T.P^2
!         8) T^2.P^2
!         9) P^0.25
!        10) Water vapor mixing ratio, W
!        11)  W
!            ---
!            T^2
!P-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Std_Predictors( p,        &  ! Input,  K
                                     t,        &  ! Input,  K
                                     w,        &  ! Input,  K
                                     Predictor )  ! Output, Istd x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: p          ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: t          ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: w          ! K

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: Predictor  ! Istd x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Std_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: p2
    REAL( fp_kind ) :: t2



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE STANDARD PREDICTOR SET --                #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( p )


      ! -- Precalculate the squared terms
      p2 = p( k ) * p( k )
      t2 = t( k ) * t( k )

      ! -- Calculate and assign the Absorber independent Predictors
      Predictor(  1, k ) = t( k )
      Predictor(  2, k ) = p( k )
      Predictor(  3, k ) = t2
      Predictor(  4, k ) = p2
      Predictor(  5, k ) = t( k ) * p( k )
      Predictor(  6, k ) = t2     * p( k )
      Predictor(  7, k ) = t( k ) * p2
      Predictor(  8, k ) = t2     * p2
      Predictor(  9, k ) = p( k )**POINT_25
      Predictor( 10, k ) = w( k )
      Predictor( 11, k ) = w( k ) / t2

    END DO k_Layer_loop

  END SUBROUTINE Compute_Std_Predictors






!--------------------------------------------------------------------------------
!P+
! NAME:
!       Compute_Int_Predictors
!
! PURPOSE:
!       PRIVATE function to compute the integrated, absorber dependent
!       predictor set for the forward transmittance model.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Int_Predictors( Pressure,    &
!                                    Temperature, &
!                                    Absorber,    &
!                                    Predictor    )
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer average pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer average temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber:         Profile LEVEL integrated absorber amount array.
!                         UNITS:      Varies with absorber
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (0:K levels)
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor:        Array containing the calculated integrated Predictor set
!                         for the passed Absorber.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Iint x K
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       The integrated predictors consist of six that are repeated for every
!       absorber:
!
!         1) T*
!         2) P*
!         3) T**
!         4) P**
!         5) T***
!         6) P***
!
!       In the equations below, the "X" quantities can be replaced with temperature,
!       T, or pressure, P, to determine the required predictor:
!
!                           __ k
!                    1     \ 
!         X*(k) = --------  >  ( X(i) + X(i-1) ) ( A(i) - A(i-1) )
!                  c.A(k)  /__
!                             i=1
!
!                            __ k
!                     1     \
!        X**(k) = ---------  >  ( X(i)A(i) + X(i-1)A(i-1) ) ( A(i) - A(i-1) )
!                  c.A^2(k) /__
!                              i=1
!
!                            __ k
!                     1     \ 
!       X***(k) = ---------  >  ( X(i)A^2(i) + X(i-1)A^2(i-1) ) ( A(i) - A(i-1) )
!                  c.A^3(k) /__
!                              i=1
!
!       To accomodate input layer values (i.e. layer average values) from the
!       NCEP GDAS, the predictor formulations were modified to:
!
!                   __ k                        __k-1
!                  \                           \ 
!                   >  X(i)( A(i)-A(i-1) )      >  X(i)( A(i)-A(i-1) )
!                  /__                         /__ 
!                     i=1                         i=1
!         X*(k) = ------------------------- + -------------------------
!                            2.A(k)                    2.A(k-1)
!
!                   __ k
!                  \ 
!                   >  X(i)( A(i) + A(i-1) ) ( A(i) - A(i-1) )
!                  /__
!                     i=1
!        X**(k) = --------------------------------------------- +
!                                       2.A^2(k) 
!                   
!                   __k-1
!                  \ 
!                   >  X(i)( A(i) + A(i-1) ) ( A(i) - A(i-1) )
!                  /__
!                     i=1
!                 ---------------------------------------------
!                                       2.A^2(k-1)
!
!                      __ k
!                     \ 
!                 3 .  >  X(i)( A^2(i) + A^2(i-1) ) ( A(i) - A(i-1) )
!                     /__
!                        i=1
!       X***(k) = ---------------------------------------------------- +
!                                       4.A^3(k)                    
!
!                      __k-1
!                     \ 
!                 3 .  >  X(i)( A^2(i) + A^2(i-1) ) ( A(i) - A(i-1) )
!                     /__
!                        i=1
!                 ----------------------------------------------------
!                                       4.A^3(k-1)
!
!       Thus the transmittance model coefficients calculated using the LEVEL
!       predictor formulation are used with the predictors constructed above 
!       with layer values.
!P-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Int_Predictors( Pressure,    &  ! Input,  K
                                     Temperature, &  ! Input,  K
                                     Absorber,    &  ! Input,  0:K
                                     Predictor    )  ! Output, Iint x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Pressure     ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Temperature  ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: Absorber     ! 0:K

    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor    ! Iint x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Int_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Layers
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_Absorber
    REAL( fp_kind ) :: Factor_1
    REAL( fp_kind ) :: Factor_2
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Absorber_3

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2

    ! -- Intermediate summation array. Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor, DIM=1 ) ) :: s

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor, DIM=1 ), 0:SIZE( Pressure ) ) :: x



    !#--------------------------------------------------------------------------#
    !#                  -- DETERMINE THE NUMBER PREDICTORS --                   #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                         -- INITIALISE VALUES --                          #
    !#--------------------------------------------------------------------------#

    Absorber_2( 0 ) = Absorber(0) * Absorber(0)

    s( : )    = ZERO
    x( :, 0 ) = ZERO



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE INTEGRATED PREDICTOR SET --               #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( Pressure )


      ! -----------------------------------------
      ! Calculate Absorber multiplicative Factors
      ! -----------------------------------------

      Absorber_2( k ) = Absorber( k ) * Absorber( k )

      d_Absorber = Absorber( k ) - Absorber( k-1 )                      ! For * terms
      Factor_1   = ( Absorber( k )   + Absorber( k-1 )   ) * d_Absorber ! For ** terms
      Factor_2   = ( Absorber_2( k ) + Absorber_2( k-1 ) ) * d_Absorber ! For *** terms


      ! -------------------------------
      ! Calculate the intermediate sums
      ! -------------------------------

      s( 1 ) = s( 1 ) + ( Temperature( k ) * d_Absorber )  ! T*
      s( 2 ) = s( 2 ) + ( Pressure( k )    * d_Absorber )  ! P*

      s( 3 ) = s( 3 ) + ( Temperature( k ) * Factor_1 )    ! T**
      s( 4 ) = s( 4 ) + ( Pressure( k )    * Factor_1 )    ! P**

      s( 5 ) = s( 5 ) + ( Temperature( k ) * Factor_2 )    ! T***
      s( 6 ) = s( 6 ) + ( Pressure( k )    * Factor_2 )    ! P***


      ! -------------------------------------------------------
      ! Calculate the normalising factors for the integrated
      ! predictors. Note that the checks below, the IF tests to
      ! determine if the absorber products are represenatble
      ! are to minimise the number of calcs. I.e if Inverse_1
      ! is toast because Absorber(k) is too small there's no
      ! need to check any further.
      ! -------------------------------------------------------

      ! -- Is Inverse_1 representable?
      Inverse_1_check: IF ( Absorber( k ) > TOLERANCE ) THEN

        Inverse_1 = ONE / Absorber( k )

        ! -- Is Inverse_2 representable
        Inverse_2_check: IF ( Absorber_2( k ) > TOLERANCE ) THEN

          Inverse_2  = Inverse_1 * Inverse_1
          Absorber_3 = Absorber( k ) * Absorber_2( k )

          ! -- Is Inverse_3 representable
          Inverse_3_check: IF ( Absorber_3 > TOLERANCE ) THEN

            Inverse_3 = Inverse_2 * Inverse_1

          ELSE ! Inverse_3_check

            Inverse_3 = ZERO

          END IF Inverse_3_check

        ELSE ! Inverse_2_check

          Inverse_2 = ZERO
          Inverse_3 = ZERO

        END IF Inverse_2_check

      ELSE ! Inverse_1_check

        Inverse_1 = ZERO
        Inverse_2 = ZERO
        Inverse_3 = ZERO

      END IF Inverse_1_check


      ! ---------------------------------------------
      ! Scale and normalise the integrated Predictors
      ! ---------------------------------------------

      x( 1, k ) = POINT_5  * s( 1 ) * Inverse_1  ! T*
      x( 2, k ) = POINT_5  * s( 2 ) * Inverse_1  ! P*

      x( 3, k ) = POINT_5  * s( 3 ) * Inverse_2  ! T**
      x( 4, k ) = POINT_5  * s( 4 ) * Inverse_2  ! P**

      x( 5, k ) = POINT_75 * s( 5 ) * Inverse_3  ! T***
      x( 6, k ) = POINT_75 * s( 6 ) * Inverse_3  ! P***


      ! ----------------------------
      ! Sum Predictors across Layers
      ! ----------------------------

      DO i = 1, n_Predictors
        Predictor( i, k ) = x( i, k ) + x( i, k-1 )
      END DO

    END DO k_Layer_loop

  END SUBROUTINE Compute_Int_Predictors






!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Predictors_TL
!
! PURPOSE:
!       PUBLIC routine to calculate the tangent-linear transmittance model
!       predictors.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Predictors_TL ( Pressure,       &  ! Input,  K
!                                    Temperature,    &  ! Input,  K
!                                    Water_Vapor,    &  ! Input,  K
!                                    Absorber,       &  ! Input,  0:K x J
!
!                                    Pressure_TL,    &  ! Input,  K
!                                    Temperature_TL, &  ! Input,  K
!                                    Water_Vapor_TL, &  ! Input,  K
!                                    Absorber_TL,    &  ! Input,  0:K x J
!
!                                    Predictor_TL,   &  ! Output, I x K
!
!                                    no_standard     )  ! Optional input
!
! INPUT ARGUMENTS:
!       Pressure:            Profile payer average pressure array.
!                            UNITS:      hectoPascals (hPa)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Temperature:         Profile layer average temperature array.
!                            UNITS:      Kelvin (K)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor:         Profile layer average water vapor mixing ratio array.
!                            UNITS:      mixing ratio (g/kg)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Absorber:            Profile LEVEL integrated absorber amount array.
!                            UNITS:      Varies with absorber
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Pressure_TL:         Profile tangent-linear pressure array, i.e. the
!                            pressure perturbation.
!                            UNITS:      hectoPascals (hPa)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Temperature_TL:      Profile tangent-linear layer average temperature array
!                            i.e. the temperature perturbation.
!                            UNITS:      Kelvin (K)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor_TL:      Profile tangent-linear water vapor mixing
!                            ratio array, i.e. the water vapor mixing
!                            ratio perturbation.
!                            UNITS:      mixing ratio (g/kg)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
!       Absorber_TL:         Profile LEVEL integrated tangent-linear 
!                            absorber amount array.
!                            UNITS:      Varies with Absorber
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (K layers)
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       no_standard:         If present, the standard predictors are not calculated.
!                            This prevents recalculation of the standard predictors
!                            is only the view angle has changed - which only affects
!                            the integrated predictors.
!                            UNITS:      None
!                            TYPE:       Integer
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:        Profile layer tangent-linear predictors array.
!                            UNITS:      Varies with predictor type.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Iint x K
!                            ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Compute_Std_Predictors_TL:   PRIVATE function to compute the tangent-
!                                    linear form of the standard (absorber
!                                    independent) predictor set.
!
!       Compute_Int_Predictors_TL:   PRIVATE function to compute the tangent-
!                                    linear form of the absorber integrated
!                                    predictor set.
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
!       The predictors used in the transmittance model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    water vapor    dry gases        ozone
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_TL ( Pressure,       &  ! Input,  K
                                     Temperature,    &  ! Input,  K
                                     Water_Vapor,    &  ! Input,  K
                                     Absorber,       &  ! Input,  0:K x J

                                     Pressure_TL,    &  ! Input,  K
                                     Temperature_TL, &  ! Input,  K
                                     Water_Vapor_TL, &  ! Input,  K
                                     Absorber_TL,    &  ! Input,  0:K x J

                                     Predictor_TL,   &  ! Output, I x K

                                     no_standard     )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Water_Vapor     ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )           :: Absorber        ! 0:K x J

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Pressure_TL     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Temperature_TL  ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Water_Vapor_TL  ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )           :: Absorber_TL     ! 0:K x J

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT )          :: Predictor_TL    ! I x K

    INTEGER,                             INTENT( IN ), OPTIONAL :: no_standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#     -- CALCULATE THE TANGENT-LINEAR STANDARD PREDICTORS IF NEEDED --     #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( no_standard ) ) THEN

      CALL Compute_Std_Predictors_TL( Pressure,         &
                                      Temperature,      &
                                      Water_Vapor,      &

                                      Pressure_TL,      &
                                      Temperature_TL,   &
                                      Water_Vapor_TL,   &

                                      Predictor_TL( 1:MAX_N_STANDARD_PREDICTORS, : ) )

    END IF


                                                   
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, SIZE( Absorber_TL, DIM = 2 )

      ! -- Determine indices of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Calculate tangent-linear predictors for current absorber
      CALL Compute_Int_Predictors_TL( Pressure,             &
                                      Temperature,          &
                                      Absorber( 0:, j ),    &

                                      Pressure_TL,          &
                                      Temperature_TL,       &
                                      Absorber_TL( 0:, j ), &

                                      Predictor_TL( i1:i2, : ) )

    END DO j_Absorber_loop

  END SUBROUTINE Compute_Predictors_TL





!--------------------------------------------------------------------------------
!P+
! NAME:
!       Compute_Std_Predictors_TL
!
! PURPOSE:
!       PRIVATE function to compute the standard, Aasorber independent
!       predictor set for the tangent-linear transmittance model.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Std_Predictors_TL( Pressure,       &  ! Input,  K
!                                       Temperature,    &  ! Input,  K
!                                       Water_Vapor,    &  ! Input,  K
!
!                                       Pressure_TL,    &  ! Input,  K
!                                       Temperature_TL, &  ! Input,  K
!                                       Water_Vapor_TL, &  ! Input,  K
!
!                                       Predictors_TL   )  ! Output, Istd x K
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer average pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer average temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor:      Profile layer average water vapor mixing ratio array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Pressure_TL:      Profile layer tangent-linear pressure array, i.e. the
!                         pressure perturbation.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature_TL:   Profile layer tangent-linear temperature array
!                         i.e. the temperature perturbation.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor_TL:   Profile layer tangent-linear water vapor mixing
!                         ratio array, i.e. the water vapor mixing
!                         ratio perturbation.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:     Array containing the calculated tangent-linear
!                         standard predictor set.
!                         UNITS:      Varies depending on Predictor index.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Istd x K
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       The standard Predictors are the following:
!
!          1) Temperature, T
!          2) Pressure, P
!          3) T^2
!          4) P^2
!          5) T.P
!          6) T^2.P
!          7) T.P^2
!          8) T^2.P^2
!          9) P^0.25
!         10) Water vapor mixing ratio, W
!         11)  W
!             ---
!             T^2
!
!       Thus the tangent-linear form of these are
!
!          1) dT
!          2) dP
!          3) 2T.dT
!          4) 2P.dP
!          5) P.dT + T.dP
!          6) 2TP.dT + T^2.dP
!          7) 2TP.dP + P^2.dT
!          8) 2T(P^2).dT + 2(T^2)P.dP
!          9) 0.25*P^(-0.75).dP
!         10) dW
!         11)  1  (         2.W.dT  )
!             ---.( dW  -  -------- )
!             T^2 (            T    )
!
!
!P-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Std_Predictors_TL( p,           &  ! Input,  K
                                        t,           &  ! Input,  K
                                        w,           &  ! Input,  K

                                        p_TL,        &  ! Input,  K
                                        t_TL,        &  ! Input,  K
                                        w_TL,        &  ! Input,  K

                                        Predictor_TL )  ! Output, Istd x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: p              ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: t             ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: w             ! Input,  K

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: p_TL          ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: t_TL          ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: w_TL          ! Input,  K

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: Predictor_TL  ! Output, Istd x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Std_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: p2, p2_TL
    REAL( fp_kind ) :: t2, t2_TL



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE TANGENT-LINEAR STANDARD PREDICTOR SET --         #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( p )


      ! -- Precalculate the squared terms
      p2 = p( k ) * p( k )
      t2 = t( k ) * t( k )

      ! -- Tangent-linear of squared terms
      p2_TL = TWO * p( k ) * p_TL( k )
      t2_TL = TWO * t( k ) * t_TL( k )
      
      ! -- Calculate and assign the Absorber independent Predictors
      Predictor_TL(  1, k ) = t_TL( k )
      Predictor_TL(  2, k ) = p_TL( k )
      Predictor_TL(  3, k ) = t2_TL
      Predictor_TL(  4, k ) = p2_TL
      Predictor_TL(  5, k ) = ( t( k ) * p_TL( k ) ) + ( p( k ) * t_TL( k ) )
      Predictor_TL(  6, k ) = ( p( k ) * t2_TL     ) + ( t2     * p_TL( k ) )
      Predictor_TL(  7, k ) = ( t( k ) * p2_TL     ) + ( p2     * t_TL( k ) )
      Predictor_TL(  8, k ) = ( t2     * p2_TL     ) + ( p2     * t2_TL     )
      Predictor_TL(  9, k ) = POINT_25 * (p(k)**(-POINT_75)) * p_TL(k)
      Predictor_TL( 10, k ) = w_TL( k )
      Predictor_TL( 11, k ) = ( w_TL(k) - ( w(k) * t2_TL / t2 ) ) / t2

    END DO k_Layer_loop

  END SUBROUTINE Compute_Std_Predictors_TL





!--------------------------------------------------------------------------------
!P+
! NAME:
!       Compute_Int_Predictors_TL
!
! PURPOSE:
!       PRIVATE function to compute the integrated, absorber dependent
!       predictor set for the tangent-linear transmittance model.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Int_Predictors_TL( Pressure,       &  ! Input, K
!                                       Temperature,    &  ! Input, K
!                                       Absorber,       &  ! Input, 0:K
!
!                                       Pressure_TL,    &  ! Input, K
!                                       Temperature_TL, &  ! Input, K
!                                       Absorber_TL,    &  ! Input, 0:K
!
!                                       Predictor_TL    )  ! Input, Iint x K
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber_TL:      Profile LEVEL integrated absorber amount array.
!                         UNITS:      Varies with absorber
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Pressure_TL:      Profile layer tangent-linear pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature_TL:   Profile layer tangent-linear temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber_TL:      Profile LEVEL tangent-linear integrated absorber
!                         amount array.
!                         UNITS:      Varies with absorber
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:     Array containing the calculated tangent-linear
!                         integrated predictor set for every absorber.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Iint x K
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       The integrated predictors consist of six that are repeated for every
!       absorber:
!
!         1) T*
!         2) P*
!         3) T**
!         4) P**
!         5) T***
!         6) P***
!
!       In the equations below, the "X" quantities can be replaced with temperature,
!       T, of pressure, P, to determine the required predictor, and A is the LEVEL
!       absorber amount:
!
!                           __ k
!                    1     \ 
!         X*(k) = --------  >  ( X(i) + X(i-1) ) ( A(i) - A(i-1) )
!                  c.A(k)  /__
!                             i=1
!
!                            __ k
!                     1     \
!        X**(k) = ---------  >  ( X(i)A(i) + X(i-1)A(i-1) ) ( A(i) - A(i-1) )
!                  c.A^2(k) /__
!                              i=1
!
!                            __ k
!                     1     \ 
!       X***(k) = ---------  >  ( X(i)A^2(i) + X(i-1)A^2(i-1) ) ( A(i) - A(i-1) )
!                  c.A^3(k) /__
!                              i=1
!
!       To accomodate input layer values (i.e. layer average values) from the
!       NCEP GDAS, the predictor formulations were modified to:
!
!                   __ k                        __k-1
!                  \                           \ 
!                   >  X(i)( A(i)-A(i-1) )      >  X(i)( A(i)-A(i-1) )
!                  /__                         /__ 
!                     i=1                         i=1
!         X*(k) = ------------------------- + -------------------------
!                            2.A(k)                    2.A(k-1)
!
!                   __ k
!                  \ 
!                   >  X(i)( A(i) + A(i-1) ) ( A(i) - A(i-1) )
!                  /__
!                     i=1
!        X**(k) = --------------------------------------------- +
!                                       2.A^2(k) 
!                   
!                   __k-1
!                  \ 
!                   >  X(i)( A(i) + A(i-1) ) ( A(i) - A(i-1) )
!                  /__
!                     i=1
!                 ---------------------------------------------
!                                       2.A^2(k-1)
!
!                      __ k
!                     \ 
!                 3 .  >  X(i)( A^2(i) + A^2(i-1) ) ( A(i) - A(i-1) )
!                     /__
!                        i=1
!       X***(k) = ---------------------------------------------------- +
!                                       4.A^3(k)                    
!
!                      __k-1
!                     \ 
!                 3 .  >  X(i)( A^2(i) + A^2(i-1) ) ( A(i) - A(i-1) )
!                     /__
!                        i=1
!                 ----------------------------------------------------
!                                       4.A^3(k-1)
!
!
!       The tangent-linear form of these layer predictor formulations are:
!
!                    __ k                                             
!                   \                                                 
!                    >  dX(i)( A(i)-A(i-1) ) + X(i)( dA(i)-dA(i-1) )  
!                   /__                                               
!                      i=1                                            
!         dX*(k) = -------------------------------------------------- -
!                                       2.A(k)                        
!
!                           __ k
!                          \
!                   dA(k) . >  X(i)( A(i)-A(i-1) )
!                          /__
!                             i=1
!                  -------------------------------- +
!                               2.A^2(k)
!                 
!                    __k-1                                              
!                   \                                                   
!                    >  dX(i)( A(i)-A(i-1) ) + X(i)( dA(i)-dA(i-1) )    
!                   /__                                                 
!                      i=1                                              
!                  -------------------------------------------------- - 
!                                      2.A(k-1)                         
!
!                             __k-1
!                            \
!                   dA(k-1) . >  X(i)( A(i)-A(i-1) )
!                            /__
!                               i=1
!                  ----------------------------------
!                              2.A^2(k-1)
!
!                 
!                 
!                    __ k                                                                                                       
!                   \                                                                                                           
!                    >  [dX(i)( A(i)+A(i-1) ) + X(i)( dA(i)+dA(i-1) )]( A(i)-A(i-1) ) + X(i)( A(i)+A(i-1) )( dA(i)-dA(i-1) )    
!                   /__                                                                                                         
!                      i=1                                                                                                      
!        dX**(k) = ---------------------------------------------------------------------------------------------------------- - 
!                                                                  2.A^2(k)                                                     
!
!                           __ k
!                          \
!                   dA(k) . >  X(i)( A(i)+A(i-1) )( A(i)-A(i-1) )
!                          /__
!                             i=1
!                  ----------------------------------------------- +
!                                      A^3(k)
!
!                    __k-1                                                                                                      
!                   \                                                                                                           
!                    >  [dX(i)( A(i)+A(i-1) ) + X(i)( dA(i)+dA(i-1) )]( A(i)-A(i-1) ) + X(i)( A(i)+A(i-1) )( dA(i)-dA(i-1) )    
!                   /__                                                                                                         
!                      i=1                                                                                                      
!                  ---------------------------------------------------------------------------------------------------------- - 
!                                                                 2.A^2(k-1)                                                    
!
!                             __k-1
!                            \
!                   dA(k-1) . >  X(i)( A(i)+A(i-1) )( A(i)-A(i-1) )
!                            /__
!                               i=1
!                  -------------------------------------------------
!                                      A^3(k-1)
!
!
!                 
!                      __ k                                                                                                       
!                     \            2    2                 2     2                                2    2                           
!                  3 . >  [dX(i)( A(i)+A(i-1) ) + X(i)( dA(i)+dA(i-1) )]( A(i)-A(i-1) ) + X(i)( A(i)+A(i-1) )( dA(i)-dA(i-1) )    
!                     /__                                                                                                         
!                        i=1                                                                                                      
!       dX***(k) = ------------------------------------------------------------------------------------------------------------ - 
!                                                                  4.A^3(k)                                                       
!
!                            __ k
!                           \          2    2
!                  9.dA(k) . >  X(i)( A(i)+A(i-1) )( A(i)-A(i-1) )
!                           /__
!                              i=1
!                  ------------------------------------------------ +
!                                      4.A^4(k)
!
!                      __k-1                                                                                                      
!                     \            2    2                 2     2                                2    2                           
!                  3 . >  [dX(i)( A(i)+A(i-1) ) + X(i)( dA(i)+dA(i-1) )]( A(i)-A(i-1) ) + X(i)( A(i)+A(i-1) )( dA(i)-dA(i-1) )    
!                     /__                                                                                                         
!                        i=1                                                                                                      
!                  ------------------------------------------------------------------------------------------------------------ - 
!                                                                 4.A^3(k-1)                                                      
!
!                            __k-1
!                           \          2    2
!                  9.dA(k) . >  X(i)( A(i)+A(i-1) )( A(i)-A(i-1) )
!                           /__
!                              i=1
!                  ------------------------------------------------
!                                      4.A^4(k-1)
!
!
!       Thus the transmittance model coefficients calculated using the LEVEL
!       predictor formulation are used with the tangent-linear predictors
!       constructed above with layer values.
!
!P-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Int_Predictors_TL( Pressure,       &  ! Input,  K
                                        Temperature,    &  ! Input,  K
                                        Absorber,       &  ! Input,  0:K

                                        Pressure_TL,    &  ! Input,  K
                                        Temperature_TL, &  ! Input,  K
                                        Absorber_TL,    &  ! Input,  0:K

                                        Predictor_TL    )  ! Output, Iint x K
 


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: Absorber        ! 0:K

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Pressure_TL     ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Temperature_TL  ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: Absorber_TL     ! 0:K

    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor_TL    ! Iint x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Int_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_Absorber
    REAL( fp_kind ) :: d_Absorber_TL
    REAL( fp_kind ) :: Factor_1
    REAL( fp_kind ) :: Factor_1_TL
    REAL( fp_kind ) :: Factor_2
    REAL( fp_kind ) :: Factor_2_TL
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Inverse_4
    REAL( fp_kind ) :: Absorber_3
    REAL( fp_kind ) :: Absorber_4
    REAL( fp_kind ) :: Inverse_1_TL
    REAL( fp_kind ) :: Inverse_2_TL
    REAL( fp_kind ) :: Inverse_3_TL

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2_TL

    ! -- Intermediate summation arrays. Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ) ) :: s
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ) ) :: s_TL

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ), 0:SIZE( Pressure ) ) :: x_TL



    !#--------------------------------------------------------------------------#
    !#          -- DETERMINE THE NUMBER OF LAYERS AND PREDICTORS --             #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_TL, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                         -- INITIALISE VALUES --                          #
    !#--------------------------------------------------------------------------#

    Absorber_2( 0 )    = Absorber(0) * Absorber(0)
    Absorber_2_TL( 0 ) = TWO*Absorber(0) * Absorber_TL(0)

    s( : )       = ZERO
    s_TL( : )    = ZERO
    x_TL( :, 0 ) = ZERO



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE INTEGRATED PREDICTOR SET --               #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( Pressure )


      ! --------------------------------
      ! Calculate multiplicative Factors
      ! --------------------------------

      Absorber_2( k )    = Absorber( k ) * Absorber( k )
      Absorber_2_TL( k ) = TWO * Absorber(k) * Absorber_TL(k)

      ! -- For the * terms
      d_Absorber    = Absorber( k )    - Absorber( k-1 )
      d_Absorber_TL = Absorber_TL( k ) - Absorber_TL( k-1 )

      ! -- For the ** terms
      Factor_1    = ( Absorber( k ) + Absorber( k-1 ) ) * d_Absorber
      Factor_1_TL = ( ( Absorber( k )    + Absorber( k-1 )    ) * d_Absorber_TL ) + &
                    ( ( Absorber_TL( k ) + Absorber_TL( k-1 ) ) * d_Absorber    )

      ! -- For the *** terms       
      Factor_2    = ( Absorber_2( k ) + Absorber_2( k-1 ) ) * d_Absorber
      Factor_2_TL = ( ( Absorber_2( k )    + Absorber_2( k-1 )    ) * d_Absorber_TL ) + &
                    ( ( Absorber_2_TL( k ) + Absorber_2_TL( k-1 ) ) * d_Absorber )


      ! -------------------------------
      ! Calculate the intermediate sums
      ! -------------------------------

      ! -- T*
      s( 1 )    = s( 1 )    + ( Temperature( k )    * d_Absorber    )     ! Forward Predictor
      s_TL( 1 ) = s_TL( 1 ) + ( Temperature_TL( k ) * d_Absorber    ) + &
                              ( Temperature( k )    * d_Absorber_TL )

      ! -- P*
      s( 2 )    = s( 2 )    + ( Pressure( k )       * d_Absorber    )     ! Forward Predictor
      s_TL( 2 ) = s_TL( 2 ) + ( Pressure_TL( k )    * d_Absorber    ) + &
                              ( Pressure( k )       * d_Absorber_TL )

      ! -- T**
      s( 3 )    = s( 3 )    + ( Temperature( k )    * Factor_1    )       ! Forward Predictor
      s_TL( 3 ) = s_TL( 3 ) + ( Temperature_TL( k ) * Factor_1    ) + &
                              ( Temperature( k )    * Factor_1_TL )

      ! -- P**
      s( 4 )    = s( 4 )    + ( Pressure( k )       * Factor_1    )       ! Forward Predictor
      s_TL( 4 ) = s_TL( 4 ) + ( Pressure_TL( k )    * Factor_1    ) + &
                              ( Pressure( k )       * Factor_1_TL )

      ! -- T***
      s( 5 )    = s( 5 )    + ( Temperature( k )    * Factor_2    )       ! Forward Predictor
      s_TL( 5 ) = s_TL( 5 ) + ( Temperature_TL( k ) * Factor_2    ) + &
                              ( Temperature( k )    * Factor_2_TL )

      ! -- P***
      s( 6 )    = s( 6 )    + ( Pressure( k )       * Factor_2    )       ! Forward Predictor
      s_TL( 6 ) = s_TL( 6 ) + ( Pressure_TL( k )    * Factor_2    ) + &
                              ( Pressure( k )       * Factor_2_TL )


      ! ------------------------------------------------------
      ! Calculate the normalising factors for the integrated
      ! tangent-linear predictors. Note that the checks below,
      ! the IF tests to determine if the absorber products are
      ! represenatble are to minimise the number of calcs. I.e
      ! if Inverse_1 is toast because Absorber(k) is too small
      ! there's no need to check any further.
      ! ------------------------------------------------------

      ! -- Is Inverse_1 representable?
      Inverse_1_check: IF ( Absorber( k ) > TOLERANCE ) THEN

        Inverse_1 = ONE / Absorber( k )

        ! -- Is Inverse_2 representable
        Inverse_2_check: IF ( Absorber_2( k ) > TOLERANCE ) THEN

          Inverse_2    =  Inverse_1 * Inverse_1
          Inverse_1_TL = -Inverse_2 * Absorber_TL( k )
          Absorber_3   =  Absorber( k ) * Absorber_2( k )

          ! -- Is Inverse_3 representable
          Inverse_3_check: IF ( Absorber_3 > TOLERANCE ) THEN

            Inverse_3    =  Inverse_2 * Inverse_1
            Inverse_2_TL = -Inverse_3 * Absorber_TL( k ) * TWO
            Absorber_4   =  Absorber( k ) * Absorber_3

            ! -- Is Inverse_4 represenatble?
            Inverse_4_check: IF ( Absorber_4 > TOLERANCE ) THEN

              Inverse_4    =  Inverse_3 * Inverse_1
              Inverse_3_TL = -Inverse_4 * Absorber_TL( k ) * THREE

            ELSE ! Inverse_4_check

              Inverse_3_TL = ZERO

            END IF Inverse_4_check

          ELSE ! Inverse_3_check

            Inverse_3 = ZERO

            Inverse_2_TL = ZERO
            Inverse_3_TL = ZERO

          END IF Inverse_3_check

        ELSE ! Inverse_2_check

          Inverse_2 = ZERO
          Inverse_3 = ZERO

          Inverse_1_TL = ZERO
          Inverse_2_TL = ZERO
          Inverse_3_TL = ZERO

        END IF Inverse_2_check

      ELSE ! Inverse_1_check

        Inverse_1 = ZERO
        Inverse_2 = ZERO
        Inverse_3 = ZERO

        Inverse_1_TL = ZERO
        Inverse_2_TL = ZERO
        Inverse_3_TL = ZERO

      END IF Inverse_1_check


      ! ------------------------------------------------------------
      ! Scale and normalise the tangent-linear integrated Predictors
      ! ------------------------------------------------------------

      ! -- T*
      x_TL( 1, k ) = POINT_5  * ( ( s_TL( 1 ) * Inverse_1    ) + &
                                  ( s( 1 )    * Inverse_1_TL ) )

      ! -- P*
      x_TL( 2, k ) = POINT_5  * ( ( s_TL( 2 ) * Inverse_1    ) + &
                                  ( s( 2 )    * Inverse_1_TL ) )

      ! -- T**
      x_TL( 3, k ) = POINT_5  * ( ( s_TL( 3 ) * Inverse_2    ) + &
                                  ( s( 3 )    * Inverse_2_TL ) )

      ! -- P**
      x_TL( 4, k ) = POINT_5  * ( ( s_TL( 4 ) * Inverse_2    ) + &
                                  ( s( 4 )    * Inverse_2_TL ) )

      ! -- T***
      x_TL( 5, k ) = POINT_75 * ( ( s_TL( 5 ) * Inverse_3    ) + &
                                  ( s( 5 )    * Inverse_3_TL ) )

      ! -- P***
      x_TL( 6, k ) = POINT_75 * ( ( s_TL( 6 ) * Inverse_3    ) + &
                                  ( s( 6 )    * Inverse_3_TL ) )


      ! ----------------------------
      ! Sum Predictors across Layers
      ! ----------------------------

      DO i = 1, n_Predictors
        Predictor_TL( i, k ) = x_TL( i, k ) + x_TL( i, k - 1 )
      END DO

    END DO k_Layer_loop

  END SUBROUTINE Compute_Int_Predictors_TL




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Predictors_AD
!
! PURPOSE:
!       PUBLIC routine to calculate the adjoint transmittance model
!       predictors.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Predictors_AD ( &
!                                    ! -- Forward input
!                                    Pressure,       &  ! Input,  K
!                                    Temperature,    &  ! Input,  K
!                                    Water_Vapor,    &  ! Input,  K
!                                    Absorber,       &  ! Input,  0:K x J
!
!                                    ! -- Adjoint input
!                                    Predictor_AD,   &  ! In/Output, I x K
!
!                                    ! -- Adjoint output
!                                    Pressure_AD,    &  ! In/Output,  K
!                                    Temperature_AD, &  ! In/Output,  K
!                                    Water_Vapor_AD, &  ! In/Output,  K
!                                    Absorber_AD,    &  ! In/Output,  0:K x J
!
!                                    ! -- Optional input
!                                    no_standard     )  ! Optional input
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer average pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer average temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor:      Profile layer average water vapor mixing ratio array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber:         Profile LEVEL integrated absorber amount array.
!                         UNITS:      Varies with absorber
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor_AD:     Profile layer predictor adjoint array.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  I x K
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       no_standard:      If present, the standard predictors are not calculated.
!                         This prevents recalculation of the standard predictors
!                         is only the view angle has changed - which only affects
!                         the integrated predictors.
!                         UNITS:      None
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Pressure_AD:      Profile layer adjoint pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Temperature_AD:   Profile layer adjoint temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Water_Vapor_AD:   Profile layer adjoint water vapor array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Absorber_AD:      Profile LEVEL adjoint integrated absorber
!                         amount array.
!                         UNITS:      Varies with absorber
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (0:K levels)
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Compute_Std_Predictors_AD:   PRIVATE function to compute the adjoint
!                                    form of the standard (absorber
!                                    independent) predictor set.
!
!       Compute_Int_Predictors_AD:   PRIVATE function to compute the adjoint
!                                    form of the absorber integrated
!                                    predictor set.
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
! PROCEDURE:
!       The predictors used in the transmittance model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    water vapor    dry gases        ozone
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_AD ( &
                                     ! -- Forward input
                                     Pressure,       &  ! Input, K
                                     Temperature,    &  ! Input, K
                                     Water_Vapor,    &  ! Input, K
                                     Absorber,       &  ! Input, 0:K x J

                                     ! -- Adjoint input
                                     Predictor_AD,   &  ! Input, I x K

                                     ! -- Adjoint output
                                     Pressure_AD,    &  ! In/Output, K
                                     Temperature_AD, &  ! In/Output, K
                                     Water_Vapor_AD, &  ! In/Output, K
                                     Absorber_AD,    &  ! In/Output, 0:K x J

                                     ! -- Optional input
                                     no_standard     )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Water_Vapor     ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )     :: Absorber        ! 0:K x J

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )     :: Predictor_AD    ! I x K

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Pressure_AD     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Temperature_AD  ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Water_Vapor_AD  ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN OUT ) :: Absorber_AD     ! 0:K x J

    ! -- Optional input
    INTEGER,         OPTIONAL,           INTENT( IN )     :: no_standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#         -- CALCULATE THE ADJOINT OF THE INTEGRATED PREDICTORS --         #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, SIZE( Absorber, DIM = 2 )

      ! -- Determine indices of current Absorber Predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Compute the predictor adjoints for the current absorber
      CALL Compute_Int_Predictors_AD( &
                                      ! -- Forward input
                                      Pressure,                 &  ! Input,  K
                                      Temperature,              &  ! Input,  K
                                      Absorber( 0:, j ),        &  ! Input,  0:K

                                      ! -- Adjoint input
                                      Predictor_AD( i1:i2, : ), &  ! Input,  Iint x K
                                                              
                                      ! -- Adjoint output
                                      Pressure_AD,              &  ! In/Output,  K
                                      Temperature_AD,           &  ! In/Output,  K
                                      Absorber_AD( 0:, j )      )  ! In/Output,  0:K
                                                              
    END DO j_Absorber_loop                                    




    !#--------------------------------------------------------------------------#
    !#     -- CALCULATE THE ADJOINT OF THE STANDARD PREDICTORS IF NEEDED --     #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( no_standard ) ) THEN

      ! -- Compute the predictor adjoints
      CALL Compute_Std_Predictors_AD( &
                                      ! -- Forward input
                                      Pressure,          &  ! Input,  K
                                      Temperature,       &  ! Input,  K
                                      Water_Vapor,       &  ! Input,  K

                                      ! -- Adjoint input
                                      Predictor_AD( 1:MAX_N_STANDARD_PREDICTORS, : ), &  ! Input, Istd x K

                                      ! -- Adjoint output
                                      Pressure_AD,       &  ! In/Output,  K
                                      Temperature_AD,    &  ! In/Output,  K
                                      Water_Vapor_AD     )  ! In/Output,  K

    END IF

  END SUBROUTINE Compute_Predictors_AD





!--------------------------------------------------------------------------------
!P+
! NAME:
!       Compute_Std_Predictors_AD
!
! PURPOSE:
!       PRIVATE function to compute the standard, absorber independent
!       predictor set for the adjoint transmittance model.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Std_Predictors_AD( &
!                                       ! -- Forward input
!                                       Pressure,       &  ! Input,  K
!                                       Temperature,    &  ! Input,  K
!                                       Water_Vapor,    &  ! Input,  K
!
!                                       ! -- Adjoint input
!                                       Predictor_AD,   &  ! Input, Istd x K
!
!                                       ! -- Adjoint output
!                                       Pressure_AD,    &  ! In/Output,  K
!                                       Temperature_AD, &  ! In/Output,  K
!                                       Water_Vapor_AD  )  ! In/Output,  K
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer average pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer average temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor:      Profile layer average water vapor mixing ratio array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor_AD:     Adjoint of the layer predictor arrays.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Iint x K
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Pressure_AD:      Profile layer adjoint pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Temperature_AD:   Profile layer adjoint temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Water_Vapor_AD:   Profile layer adjoint water vapor array.
!                         UNITS:      mixing ratio (g/kg)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The standard Predictors are the following:
!
!         pred(1)  = T, temperature
!         pred(2)  = P, pressure
!         pred(3)  = T^2
!         pred(4)  = P^2
!         pred(5)  = T.P
!         pred(6)  = T^2.P
!         pred(7)  = T.P^2
!         pred(8)  = T^2.P^2
!         pred(9)  = P^0.25
!         pred(10) = W, water vapor mixing ratio
!         pred(11) =  W
!                    ---
!                    T^2
!
!       The tangent-linear form of these are
!
!         dpred(1)  = dT
!         dpred(2)  = dP
!         dpred(3)  = 2T.dT
!         dpred(4)  = 2P.dP
!         dpred(5)  = P.dT + T.dP
!         dpred(6)  = 2TP.dT + T^2.dP
!         dpred(7)  = 2TP.dP + P^2.dT
!         dpred(8)  = 2T(P^2).dT + 2(T^2)P.dP
!         dpred(9)  = 0.25*P^(-0.75).dP
!         dpred(10) = dW
!         dpred(11) =  1  (         2.W.dT  )
!                     ---.( dW  -  -------- )
!                     T^2 (            T    )
!
!       Thus, the adjoint of the Predictors are
!
!         d#T = 2T( P^2.d#pred(8) + P.d#pred(6) + d#pred(3) ) + 
!                   P^2.d#pred(7) + P.d#pred(5) + d#pred(1)   -
!                 2W
!               -----.d#pred(11)
!                T^3
!
!         d#P = 2P( T^2.d#pred(8) + T.d#pred(7) + d#pred(4) ) + 
!                   T^2.d#pred(6) + T.d#pred(5) + d#pred(2)   +
!               0.25*P^(-0.75).d#pred(9)
!
!                             1
!         d#W = d#pred(9) + -----.d#pred(11)
!                            T^2
!
!       where the "#"ed terms represent the adjoints.
!
!P-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Std_Predictors_AD( &
                                        ! -- Forward input
                                        p,            &  ! Input,  K
                                        t,            &  ! Input,  K
                                        w,            &  ! Input,  K

                                        ! -- Adjoint input
                                        Predictor_AD, &  ! Input,  Istd x K

                                        ! -- Adjoint output
                                        p_AD,         &  ! In/Output,  K
                                        t_AD,         &  ! In/Output,  K
                                        w_AD          )  ! In/Output,  K




    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: p             ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: t             ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: w             ! Input,  K

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )     :: Predictor_AD  ! Input,  Istd x K

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: p_AD          ! In/Output,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: t_AD          ! In/Output,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: w_AD          ! In/Output,  K



    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Std_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_Predictors
    INTEGER :: k

    REAL( fp_kind ) :: p2, p2_AD
    REAL( fp_kind ) :: t2, t2_AD
    REAL( fp_kind ) :: t4



    !#--------------------------------------------------------------------------#
    !#              -- Determine the number of Predictors --                    #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_AD, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#        -- Calculate the adjoints of the standard Predictor set --        #
    !#                                                                          #
    !# Don't have to loop backwards here as this is a parallel loop.            #
    !#                                                                          #
    !# Pressure and Temperature squared adjoint terms are not zeroed out every  #
    !# loop iteration as they are local to each iteration and can be simply     #
    !# re-assigned.                                                             #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( p )


      ! -- Precalculate the squared terms
      p2 = p( k ) * p( k )
      t2 = t( k ) * t( k )
      t4 = t2 * t2

      ! -- Pressure squared adjoint
      p2_AD =          Predictor_AD( 4, k )   + &   ! Predictor #4, P^2
              ( t(k) * Predictor_AD( 7, k ) ) + &   ! Predictor #7, T.P^2
              ( t2   * Predictor_AD( 8, k ) )       ! Predictor #8, T^2.P^2

      ! -- Temperature squared adjoint
      t2_AD =          Predictor_AD( 3, k )         + &  ! Predictor #3, T^2
              ( p(k) * Predictor_AD( 6, k ) )       + &  ! Predictor #6, T^2.P
              ( p2   * Predictor_AD( 8, k ) )       + &  ! Predictor #8, T^2.P^2
              (-w(k) * Predictor_AD( 11, k ) / t4 )      ! Predictor #11, W/T^2

      ! -- Water vapor adjoint
      w_AD( k ) = w_AD( k ) +   Predictor_AD( 10, k ) + &     ! Predictor #10, W
                              ( Predictor_AD( 11, k ) / t2 )  ! Predictor #11, W/T^2

      ! -- Temperature adjoint
      t_AD( k ) = t_AD( k ) + &
                  ( TWO * t(k) * t2_AD )  + &          ! T^2 term
                           Predictor_AD( 1, k )   + &  ! Predictor #1, T
                  ( p(k) * Predictor_AD( 5, k ) ) + &  ! Predictor #5, T.P
                  ( p2   * Predictor_AD( 7, k ) )      ! Predictor #7, T.P^2

      ! -- Pressure adjoint
      p_AD( k ) = p_AD( k ) + &
                  ( TWO * p(k) * p2_AD ) + &                                 ! P^2 term
                           Predictor_AD( 2, k )   + &                        ! Predictor #2, P
                  ( t(k) * Predictor_AD( 5, k ) ) + &                        ! Predictor #5, T.P
                  ( t2   * Predictor_AD( 6, k ) ) + &                        ! Predictor #6, T^2.P
                  ( POINT_25 * (p(k)**(-POINT_75)) * Predictor_AD( 9, k ) )  ! Predictor #9, P^1/4

    END DO k_Layer_loop

  END SUBROUTINE Compute_Std_Predictors_AD





!--------------------------------------------------------------------------------
!P+
! NAME:
!       Compute_Int_Predictors_AD
!
! PURPOSE:
!       PRIVATE function to compute the integrated, absorber dependent predictor
!       set for the adjoint transmittance model.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL Compute_Int_Predictors_AD( &
!                                       ! -- Forward input
!                                       Pressure,       &  ! Input,  K
!                                       Temperature,    &  ! Input,  K
!                                       Absorber,       &  ! Input,  0:K
!
!                                       ! -- Adjoint input
!                                       Predictor_AD,   &  ! Input,  Iint x K
!
!                                       ! -- Adjoint output
!                                       Pressure_AD,    &  ! In/Output,  K
!                                       Temperature_AD, &  ! In/Output,  K
!                                       Absorber_AD     )  ! In/Output,  0:K
!
! INPUT ARGUMENTS:
!       Pressure:         Profile layer pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Profile layer temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber:         Profile LEVEL integrated absorber amount array.
!                         UNITS:      Varies with absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (0:K levels)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor_AD:     Adjoint of the layer predictor arrays.
!                         UNITS:      Varies with predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Iint x K
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Pressure_AD:      Profile layer adjoint pressure array.
!                         UNITS:      hectoPascals (hPa)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Temperature_AD:   Profile layer adjoint temperature array.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K layers)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Absorber_AD:      Profile LEVEL adjoint integrated absorber
!                         amount array.
!                         UNITS:      Varies with absorber
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (0:K levels)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:


!
!P-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Int_Predictors_AD( &
                                        ! -- Forward input
                                        Pressure,       &  ! Input,  K
                                        Temperature,    &  ! Input,  K
                                        Absorber,       &  ! Input,  0:K

                                        ! -- Adjoint input
                                        Predictor_AD,   &  ! Input,  Iint x K

                                        ! -- Adjoint output
                                        Pressure_AD,    &  ! In/Output,  K
                                        Temperature_AD, &  ! In/Output,  K
                                        Absorber_AD     )  ! In/Output,  0:K

 


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )     :: Absorber        ! 0:K

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )     :: Predictor_AD    ! Iint x K

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN OUT ) :: Pressure_AD     ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN OUT ) :: Temperature_AD  ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN OUT ) :: Absorber_AD     ! 0:K



    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Int_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2_AD

    ! -- Multiplicative Factors, K
    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: d_Absorber
    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: Factor_1
    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: Factor_2

    ! -- Intermediate summation arrays, Iint x 0:K and Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ), 0:SIZE( Pressure ) ) :: s
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ) ) :: s_AD

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ), 0:SIZE( Pressure ) ) :: x_AD

    ! -- Scalars
    INTEGER :: i, n_Predictors
    INTEGER :: k, n_Layers

    REAL( fp_kind ) :: d_Absorber_AD
    REAL( fp_kind ) :: Factor_1_AD
    REAL( fp_kind ) :: Factor_2_AD

    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Inverse_4
    REAL( fp_kind ) :: Absorber_3
    REAL( fp_kind ) :: Absorber_4
    REAL( fp_kind ) :: Inverse_1_AD
    REAL( fp_kind ) :: Inverse_2_AD
    REAL( fp_kind ) :: Inverse_3_AD
    REAL( fp_kind ) :: Multiplier
    REAL( fp_kind ) :: Add_Factor



    !#--------------------------------------------------------------------------#
    !#                       -- ASSIGN THE DIMENSIONS --                        #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_AD, DIM=1 )
    n_Layers     = SIZE( Pressure )



    !#--------------------------------------------------------------------------#
    !#          -- RECALCULATE THE INTERMEDIATE FORWARD MODEL SUMS --           #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Initialise top level of arrays
    ! ------------------------------

    Absorber_2( 0 ) = Absorber(0) * Absorber(0)
    s( :, 0 ) = ZERO


    ! ----------------
    ! Loop over Layers
    ! ----------------

    k_Layer_loop_forward: DO k = 1, n_Layers


      ! -----------------------------------------
      ! Calculate Absorber multiplicative Factors
      ! and save for adjoint calculation.
      ! -----------------------------------------

      Absorber_2( k ) = Absorber( k ) * Absorber( k )

      d_Absorber( k ) = Absorber( k ) - Absorber( k-1 )                           ! For * terms
      Factor_1( k )   = ( Absorber( k )   + Absorber( k-1 )   ) * d_Absorber( k ) ! For ** terms
      Factor_2( k )   = ( Absorber_2( k ) + Absorber_2( k-1 ) ) * d_Absorber( k ) ! For *** terms


      ! ----------------------------------------
      ! Calculate and save the intermediate sums
      ! ----------------------------------------

      s( 1, k ) = s( 1, k-1 ) + ( Temperature( k ) * d_Absorber( k ) )  ! T*
      s( 2, k ) = s( 2, k-1 ) + ( Pressure( k )    * d_Absorber( k ) )  ! P*

      s( 3, k ) = s( 3, k-1 ) + ( Temperature( k ) * Factor_1( k ) )    ! T**
      s( 4, k ) = s( 4, k-1 ) + ( Pressure( k )    * Factor_1( k ) )    ! P**

      s( 5, k ) = s( 5, k-1 ) + ( Temperature( k ) * Factor_2( k ) )    ! T***
      s( 6, k ) = s( 6, k-1 ) + ( Pressure( k )    * Factor_2( k ) )    ! P***

    END DO k_Layer_loop_forward



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE LOCAL ADJOINT VARIABLES --                  #
    !#--------------------------------------------------------------------------#

    x_AD( :, n_Layers )       = ZERO
    s_AD( : )                 = ZERO
    Absorber_2_AD( n_Layers ) = ZERO



    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE THE INTEGRATED PREDICTOR ADJOINTS --             #
    !#--------------------------------------------------------------------------#


    ! -----------------------------------
    ! Here loop order does matter as this
    ! is a sequential loop
    ! -----------------------------------

    k_Layer_loop_adjoint: DO k = n_Layers, 1, -1



      ! -------------------------------------------------------
      ! Calculate the normalising factors for the integrated
      ! predictors. Note that the checks below, the IF tests to
      ! determine if the absorber products are represenatble
      ! are to minimise the number of calcs. I.e if Inverse_1
      ! is toast because Absorber(k) is too small there's no
      ! need to check any further.
      ! -------------------------------------------------------

      ! -- Is Inverse_1 representable?
      Inverse_1_check: IF ( Absorber( k ) > TOLERANCE ) THEN

        Inverse_1 = ONE / Absorber( k )

        ! -- Is Inverse_2 representable
        Inverse_2_check: IF ( Absorber_2( k ) > TOLERANCE ) THEN

          Inverse_2  = Inverse_1 * Inverse_1
          Absorber_3 = Absorber( k ) * Absorber_2( k )
         
          ! -- Is Inverse_3 representable
          Inverse_3_check: IF ( Absorber_3 > TOLERANCE ) THEN

            Inverse_3  = Inverse_2 * Inverse_1
            Absorber_4 = Absorber( k ) * Absorber_3

            ! -- Is Inverse_4 represenatble?
            Inverse_4_check: IF ( Absorber_4 > TOLERANCE ) THEN

              Inverse_4 = Inverse_3 * Inverse_1

            ELSE ! Inverse_4_check

              Inverse_4 = ZERO

            END IF Inverse_4_check

          ELSE ! Inverse_3_check

            Inverse_3 = ZERO
            Inverse_4 = ZERO

          END IF Inverse_3_check

        ELSE ! Inverse_2_check

          Inverse_2 = ZERO
          Inverse_3 = ZERO
          Inverse_4 = ZERO

        END IF Inverse_2_check

      ELSE ! Inverse_1_check

        Inverse_1 = ZERO
        Inverse_2 = ZERO
        Inverse_3 = ZERO
        Inverse_4 = ZERO

      END IF Inverse_1_check


      ! --------------------------------------------
      ! Adjoint of Predictor summation across Layers
      ! --------------------------------------------


      DO i = 1, n_Predictors

        x_AD( i, k )   = x_AD( i, k )   + Predictor_AD( i, k )
        x_AD( i, k-1 ) = Predictor_AD( i, k )

      END DO


      ! --------------------------------------------------------------
      ! Adjoint of the LEVEL integrated predictors intermediate sums
      !
      ! Note that the adjoint variables Inverse_X_AD are local to this
      ! loop iteration so they are simply assigned when they are first
      ! used.
      ! --------------------------------------------------------------

      ! -- P* and T*, Predictor indices #2 and 1
      ! -- Simply assign a value for Inverse_1_AD
      Multiplier   = POINT_5 * Inverse_1
      s_AD( 1 )    = s_AD( 1 ) + ( Multiplier * x_AD( 1, k ) )
      s_AD( 2 )    = s_AD( 2 ) + ( Multiplier * x_AD( 2, k ) )
      Inverse_1_AD = POINT_5 * ( ( s( 1, k ) * x_AD( 1, k ) ) + &
                                 ( s( 2, k ) * x_AD( 2, k ) ) )

      ! -- P** and T**, Predictor indices #4 and 3
      Multiplier   = POINT_5 * Inverse_2
      s_AD( 3 )    = s_AD( 3 ) + ( Multiplier * x_AD( 3, k ) )
      s_AD( 4 )    = s_AD( 4 ) + ( Multiplier * x_AD( 4, k ) )
      Inverse_2_AD = POINT_5 * ( ( s( 3, k ) * x_AD( 3, k ) ) + &
                                 ( s( 4, k ) * x_AD( 4, k ) ) )

      ! -- P*** and T***, Predictor indices #6 and 5
      Multiplier   = POINT_75 * Inverse_3
      s_AD( 5 )    = s_AD( 5 ) + ( Multiplier * x_AD( 5, k ) )
      s_AD( 6 )    = s_AD( 6 ) + ( Multiplier * x_AD( 6, k ) )
      Inverse_3_AD = POINT_75 * ( ( s( 5, k ) * x_AD( 5, k ) ) + &
                                  ( s( 6, k ) * x_AD( 6, k ) ) )

      ! -- Adjoint of Inverse terms. Note that the Inverse_X_AD
      ! -- terms are *not* zeroed out as they are re-assigned values
      ! -- each loop iteration above.
      Absorber_AD( k ) = Absorber_AD( k ) - (         Inverse_2 * Inverse_1_AD ) - &
                                            ( TWO *   Inverse_3 * Inverse_2_AD ) - &
                                            ( THREE * Inverse_4 * Inverse_3_AD )


      ! ---------------------------------
      ! Pressure and temperature adjoints
      ! ---------------------------------

      ! -- Pressure
      Pressure_AD( k ) = Pressure_AD( k ) + ( d_Absorber( k ) * s_AD( 2 ) ) + &  ! P*
                                            ( Factor_1( k )   * s_AD( 4 ) ) + &  ! P**
                                            ( Factor_2( k )   * s_AD( 6 ) )      ! P***


      ! -- Temperature
      Temperature_AD( k ) = Temperature_AD( k ) + ( d_Absorber( k ) * s_AD( 1 ) ) + &  ! T*
                                                  ( Factor_1( k )   * s_AD( 3 ) ) + &  ! T**
                                                  ( Factor_2( k )   * s_AD( 5 ) )      ! T***


      ! --------------------------------------------------
      ! Adjoint of the absorber amount
      !
      ! Note that the adjoint variables Factor_X_AD and
      ! d_Absorber_AD are local to this loop iteration
      ! so they are simply assigned when they are first
      ! used.
      !
      ! Note there are no
      !   s_AD() = 0
      ! because all the tangent-linear forms are
      !   s_TL() = s_TL() + (...)
      ! summing from the previous Layer.
      !
      ! Note that the Factor_X_AD and d_Absorber_AD
      ! terms are *not* zeroed out as they are re-assigned
      ! values each loop iteration.
      ! --------------------------------------------------

      ! -- Multiplicative factors
      Factor_1_AD = ( Temperature( k ) * s_AD( 3 ) ) + &
                    ( Pressure( k )    * s_AD( 4 ) )

      Factor_2_AD = ( Temperature( k ) * s_AD( 5 ) ) + &
                    ( Pressure( k )    * s_AD( 6 ) )

      ! -- Adjoint of Absorber_2(). Note that Absorber_2_AD() is a LOCAL adjoint
      ! -- variable, so the initialisation of Absorber_2_AD( k-1 ) here for
      ! -- each "k-1" is o.k. rather than
      ! --   Absorber_2_AD( k-1 ) = Absorber_2_AD( k-1 ) + ( d_Absorber( k ) * Factor_2_AD )
      ! --   Absorber_2_AD(  k  ) = Absorber_2_AD(  k  ) + ( d_Absorber( k ) * Factor_2_AD )
      ! -- since only Absorber_2_AD( n_Layers ) is initialised outside the
      ! -- current layer loop.
      Absorber_2_AD( k-1 ) = d_Absorber( k ) * Factor_2_AD
      Absorber_2_AD(  k  ) = Absorber_2_AD(  k  ) + Absorber_2_AD( k-1 )

      ! -- Adjoint of Absorber(). Here, since Absorber_AD() is NOT a local adjoint
      ! -- variable, we can't use the same form as for Absorber_2_AD() above.
      d_Absorber_AD        = ( Temperature( k ) * s_AD( 1 ) ) + &
                             ( Pressure( k )    * s_AD( 2 ) ) + &
                             ( ( Absorber( k )   + Absorber( k-1 )   ) * Factor_1_AD ) + &
                             ( ( Absorber_2( k ) + Absorber_2( k-1 ) ) * Factor_2_AD )

      Add_Factor = d_Absorber( k ) * Factor_1_AD
      Absorber_AD( k-1 ) = Absorber_AD( k-1 ) + Add_Factor - d_Absorber_AD
      Absorber_AD(  k  ) = Absorber_AD(  k  ) + Add_Factor + d_Absorber_AD + &
                                                ( TWO * Absorber(k) * Absorber_2_AD(k) )
      Absorber_2_AD( k ) = ZERO

    END DO k_Layer_loop_adjoint

    ! -- Adjoint of level 0 Absorber
    Absorber_AD( 0 )   = Absorber_AD( 0 ) + ( TWO * Absorber(0) * Absorber_2_AD(0) )
    Absorber_2_AD( 0 ) = ZERO

  END SUBROUTINE Compute_Int_Predictors_AD

END MODULE predictors


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: predictors.f90,v 1.4 2004/07/02 15:27:12 treadon Exp $
!
! $Date: 2004/07/02 15:27:12 $
!
! $Revision: 1.4 $
!
! $Name: ncep-gsi-2004_06 $
!
! $State: Exp $
!
! $Log: predictors.f90,v $
! Revision 1.4  2004/07/02 15:27:12  treadon
! add NCEP docblock
!
! Revision 1.3  2004/05/06 15:55:33  treadon
! modify !Name: comment near end of file
!
! Revision 1.2  2004/02/18 16:03:43  treadon
! implemented unified GSI
!
! Revision 3.3  2003/07/08 19:18:24  paulv
! - Fixed bug in initialising the Absorber_2(0) element (and it's TL and AD).
!   In the past, all 0'th level values were initialised to zero, but this is
!   no longer the case so
!       Absorber_2( 0 ) = ZERO
!     is changed to
!       Absorber_2( 0 ) = Absorber(0) * Absorber(0)
!   and
!       Absorber_2_TL( 0 ) = TWO*Absorber(0) * Absorber_TL(0)
! - The TL and AD form of Absorber_2 is now used in the integrated predictor
!   routines to ensure that they are consistent with the FWD routine.
! - Many changes made in an attempt to speed up the predictor routines:
!     Compute_Int_Predictors_AD():
!     ----------------------------
!     o All the arrays are now accessed in ascending index order. Previously
!       they were accessed in descending order based solely on the TL transpose
!       equations
!     o Predictor_AD argument is now INTENT( IN ) rather than INTENT( IN OUT )
!       as they are no longer zeroed in the routines. Not zeroing out the
!       Predictor_AD values after they were used saved time.
!     o The local forward and adjoint variables are now being initialised only as
!       necessary. Before the initialisations looked like:
!         s( :, 0: )    = ZERO
!         x_AD( :, 0: ) = ZERO
!       and now the following is done:
!         s( :, 0 )           = ZERO
!         x_AD( :, n_Layers ) = ZERO
!       This saves a lot of time as the entire profile does not have to be
!       zeroed.
!     o The initialisation for x_AD() works because rather than doing
!         x_AD( i, k-1 ) = x_AD( i, k-1 ) + Predictor_AD( i, k )
!       in the predictor adjoint summation loop, the following is done:
!         x_AD( i, k-1 ) = Predictor_AD( i, k )
!       since the "k-1"th x_AD value is only accumulated in the next k-iteration.
!     o Added scalar variables to hold common multiplier and addition values
!       for the intermediate sum adjoint calculation.
!     o Rearranged the adjoint calculation for the multiplicative factors to
!       do as much as possible in a single assignment to an array value.
!     Compute_Std_Predictors_AD():
!     ----------------------------
!     o All the arrays are now accessed in ascending index order. Previously
!       they were accessed in descending order based solely on the TL transpose
!       equations. This sped up this routine by around 15% (depending on how
!       the code is profiled.)
!
! Revision 3.2  2003/06/30 16:33:28  paulv
! - Removed use of pointers in aliasing the predictor arrays for the standard
!   and integrated computations. Now passing the actual array slices.
! - Updated documentation.
!
! Revision 3.1  2003/02/05 15:05:30  paulv
! - Corrected bug in standard predictor adjoint calculation.
!
! Revision 3.0  2003/02/04 21:14:56  paulv
! - New release.
!
! Revision 2.11  2001/10/01 20:28:47  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 2.10  2001/08/16 16:45:05  paulv
! - Updated documentation
!
! Revision 2.9  2001/07/12 18:22:33  paulv
! - Added more robust code for calculating the powers of the inverse absorber
!   amount. The squares, cubes and fourth powers of absorber amounts were
!   causing floating point underflows which, when used in a denominator were
!   greatly inflating any precision errors. So, the solution I adopted was,
!   prior to each inverse calculation, to check the value of the absorber
!   quantity (e.g. absorber**2, absorber**3, or absorber**4). If they are less
!   than a tolerance value (defined using the EPSILON intrinsic) then their
!   inverse is set to zero - as are any higher power inverses. This prevents
!   too small values from being used.
!   This is mostly a problem for water vapor near at the top of the defined
!   atmosphere, although some low ozone values can be found (rarely).
! - Changed all loop initialisation to vector expressions, e.g. from
!     DO i = 1, n_predictors
!       s( i )       = ZERO
!       s_TL( i )    = ZERO
!       x_TL( i, 0 ) = ZERO
!     END DO
!   to
!     s( : )       = ZERO
!     s_TL( : )    = ZERO
!     x_TL( :, 0 ) = ZERO
! - Corrected bug in definition of the intermediate summation array in
!   COMPUTE_INT_PREDICTORS_AD from
!     REAL( fp_kind ), DIMENSION( SIZE( predictor_AD, DIM=1 ), SIZE( pressure ) ) :: s
!   to
!     REAL( fp_kind ), DIMENSION( SIZE( predictor_AD, DIM=1 ), 0:SIZE( pressure ) ) :: s
! - Added initialisation of absorber_2( 0 )
!     absorber_2( 0 ) = ZERO
!   in COMPUTE_INT_PREDICTORS_AD.
! - Corrected bug in intermediate summation loop. The sums were being calculated
!   like:
!     DO k = 1, n_layers
!       s( i, k ) = s( i, k ) + .......
!   rather than:
!     DO k = 1, n_layers
!       s( i, k ) = s( i, k-1 ) + .......
!   hence the need for the redefintion of s(K) to s(0:K).
!
! Revision 2.8  2001/05/29 17:51:34  paulv
! - Corrected some documentation errors.
!
! Revision 2.7  2001/05/04 14:39:43  paulv
! - Removed shared predictor arrays from module.
! - Now use TYPE_KINDS module parameter FP_KIND to set the floating point
!   data type.
! - Added adjoint form of routines to module. Use of NO_STANDARD optional
!   keyword in COMPUTE_PREDICTORS_AD has not yet been looked into.
! - Changed names of PRIVATE integrated predictor routines from
!   COMPUTE_STARD_PREDICTORS to COMPUTE_INT_PREDICTORS. The difference between
!   "stard" and "std" is small enough to cause mild confusion when scanning
!   the code.
! - Changed references to parameter maximums to input array sizes, e.g.
!     j_absorber_loop: DO j = 1, MAX_N_ABSORBERS
!   becomes
!     j_absorber_loop: DO j = 1, SIZE( absorber, DIM = 2 )
! - Shortened variable names in standard predictor routines, e.g. p instead
!   of pressure. These calcs are simple enough that short names are clear.
! - ABSORBER arrays are now dimensioned as 0:K. This eliminates the
!   need for using an ABSORBER_KM1 variable in computing the absorber layer
!   difference, D_ABSORBER, and average, AVE_ABSORBER where the layer loop
!   always goes from 1 -> n_layers.
! - Simplified COMPUTE_INT_PREDICTORS_TL. This is a combination of the
!   change in the absorber array dimensioning and just thinking about it
!   for a while.
! - Updated header documentation. Adjoint routines not yet fully documented.
!
! Revision 2.6  2001/04/03 20:02:56  paulv
! - Commented out shared predictor data arrays. Predictor arrays are now
!   passed arguments.
! - Removed reference to profile number. Calls to routines are now a simgle
!   profile passed per call.
! - Removed planned allocation of predictor arrays.
! - Correted bug in 1st and 2nd order predictor calculation.
!
! Revision 2.5  2001/01/24 20:14:21  paulv
! - Latest test versions.
!
! Revision 2.4  2000/11/09 20:36:07  paulv
! - Added tangent linear form of routines in this module.
! - Changed names of PRIVATE routines used to compute predictors. The
!   appearance of "standard" and "integrated" in routine names have been
!   replaced with "std" and "stard" respectively.
! - Initial setup in place for dynamic allocation of predictor arrays. These
!   arrays are still statically allocated with parameterised dimensions.
!
! Revision 2.3  2000/08/31 19:36:33  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 2.2  2000/08/24 16:43:29  paulv
! - Added optional NO_STANDARD argument to COMPUTE_PREDICTORS subprogram.
!   Using this argument prevents the standard predictors (which are angle
!   independent) from being recalculated when only the path angle has changed
!   in the calling procedure.
! - Updated module and subprogram documentation.
!
! Revision 2.1  2000/08/21 21:03:16  paulv
! - Standard and integrated predictor sets calculated in separate
!   functions.
! - Predictor values saved in linear store. Simplifies application of
!   predictors when calculating absorption coefficients.
! - Wrapper function "compute_predictors" added to simplify predictor
!   calculation.
!
! Revision 1.1  2000/08/08 16:34:17  paulv
! Initial checkin
!
!
!
