module radiance
!$$$  module documentation block
!                .      .    .                                       .
!   2004-10-26  Quanhua Liu, Yong Han, Paul van delst, and Fuzhong Weng
!
!   Two-stream radiance calculation
!
!   Part of code ia adopted from van delst

! module:   radiance
!   prgmmr: van delst        org: np20                date: 2000-07-11
!
! abstract:  RT model radiance module.
!
! module history log:
!   2000-08-01  van delst, paul
!   2002-08-21  van delst - initial check in
!   2000-08-24  van delst - replate "regular" reflectivity for reflected
!                           downwelling thermal with the isotropic 
!                           reflectivity in the COMPUTE_RADIANCE subprogram;
!                           update documentation
!   2000-08-31  van delst - add documentation delimiters; update 
!                           documentation headers
!   2000-11-09  van delst - add solar term, downwelling flux and surface
!                           reflectivity made optional arguments
!   2001-01-24  van delst - latest test version
!   2001-05-29  van delst - include tangent-linear and adjoint routines;
!                           make downwelling flux and surface reflectivity
!                           required arguments; alter calculation of layer
!                           contributions
!   2001-08-01  van delst - correct bug in COMPUTE_RADIANCE; remove
!                           initialization and zeroing of adjoint of
!                           surface emission term
!   2001-08-08  van delst - remove sensor Planck function routines
!   2001-08-16  van delst - update documentation
!   2001-10-01  van delst - add "Name" to RCS keyword list
!   2003-05-02  van delst - spectral coefficient data for the cosmic 
!                           background radiance and solar irradiance is 
!                           now via the SpcCoeff structure
!   2004-06-21  treadon   - add NCEP doc block
!
! Subroutines Included:
!   compute_radiance    - PUBLIC subroutine to calculate the channel TOA
!                         radiance and brightness temperature
!   compute_radiance_TL - PUBLIC subroutine to calculate the tangent-
!                         linear TOA radiance and brightness temperature
!   compute_radiance_AD - PUBLIC subroutine to calculate the adjoint of
!                         the TOA radiance and brightness temperature
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
!       Radiance
!
! PURPOSE:
!       RT model radiance module
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE radiance
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:              Module containing data type kind definitions.
!
!       Parameters:              Module containing parameter definitions for
!                                the RT model.
!
!       Spectral_Coefficients:   Module containing the RT model spectral
!                                coefficients.
!
!       Sensor_Planck_Routines:  Module containing all the forward, tangent-
!                                linear, and adjoint Planck radiance and
!                                temperature subroutines. 
!
! CONTAINS:
!       compute_radiance:        PUBLIC subroutine to calculate the channel TOA
!                                radiance and brightness temperature.
!
!       compute_radiance_TL:     PUBLIC subroutine to calculate the tangent-
!                                linear TOA radiance and brightness temperature.
!
!       compute_radiance_AD:     PUBLIC subroutine to calculate the adjoint of
!                                the TOA radiance and brightness temperature.
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

!MODULE Radiance


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind
  USE Error_Handler
  USE CRTM_Parameters
  USE CRTM_SpcCoeff
!  USE Spectral_Coefficients
  USE Sensor_Planck_Routines
  USE RT_Twostream

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC  :: compute_radiance
  PUBLIC  :: compute_radiance_TL
  PUBLIC  :: compute_radiance_AD


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       compute_radiance
!
! PURPOSE:
!       PUBLIC subroutine to calculate the TOA radiance and brightness temperature.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL compute_radiance( temperature,           &  ! Input, K
!
!                              surface_temperature,   &  ! Input, scalar
!                              surface_emissivity,    &  ! Input, scalar
!                              surface_reflectivity,  &  ! Input, scalar
!
!                              tau,                   &  ! Input, K
!                              single_w0,              &  ! Input, K
!                              assy_g0,             &  ! Input, scalar
!
!                              secant_solar_angle,    &  ! Input, scalar
!                              valid_solar,           &  ! Input, scalar
!                              channel_index,         &  ! Input, scalar
!
!                              layer_radiance,        &  ! Output, K
!                              downwelling_radiance,  &  ! Output, scalar
!                              upwelling_radiance,    &  ! Output, scalar
!
!                              brightness_temperature )  ! Output, scalar
!
!
! INPUT ARGUMENTS:
!       temperature:             Profile LAYER average temperature array.
!                                UNITS:      Kelvin
!                                TYPE:       Float
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
!       surface_temperature:     Surface boundary temperature.
!                                UNITS:      Kelvin
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       surface_emissivity:      Surface boundary emissivity
!                                UNITS:      None
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       surface_reflectivity:    Surface boundary reflectivity
!                                UNITS:      None
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       tau:                     Layer-to-space transmittance profile for
!                                a particular satellite view angle.
!                                UNITS:      None
!                                TYPE:       Real
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
!       single_w0:                Layer-to-surface transmittance profile for
!                                either the diffuse approximation angle (IR)
!                                or the satellite view angle (MW). The latter
!                                assumes specular reflectivity.
!                                UNITS:      None
!                                TYPE:       Real
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
!       assy_g0:               Total space-to-surface transmittance at the
!                                solar zenith angle.
!                                UNITS:      None
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       secant_solar_angle:      Secant of the solar zenith angle corresponding
!                                to that used in calculating the total solar
!                                transmittance.
!                                UNITS:      None
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       valid_solar:             Flag indicating if the solar component should
!                                be included.
!                                If = 0, no solar (if sensor channel frequency
!                                        is less than a preset cutoff or if solar
!                                        zenith angle is greater than its preset
!                                         cutoff.)
!                                   = 1, include solar
!                                UNITS:      None.
!                                TYPE:       Integer
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       channel_index:           Channel index id. This is a unique index
!                                to a (supported) sensor channel.
!                                UNITS:      None
!                                TYPE:       Integer
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       layer_radiance:          Channel Planck radiance for every input layer.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       Real
!                                DIMENSION:  K.
!                                ATTRIBUTES: INTENT( OUT )
!
!       downwelling_radiance:    Channel radiance at surface due to downwelling
!                                flux and solar components.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( OUT )
!
!       upwelling_radiance:      Channel TOA radiance simulating the satellite
!                                sensor measurement. This is composed of the
!                                reflected downwelling propagated through the
!                                atmosphere as well as the upwelling only component.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( OUT )
!
!       brightness_temperature:  Channel TOA brightness temperature.
!                                UNITS:      Kelvin
!                                TYPE:       Real
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       sensor_planck_radiance:    Function to compute the Planck radiance
!                                  for a specified channel given the temperature.
!                                  SOURCE: SENSOR_PLANCK_ROUTINES module
!
!       sensor_planck_temperature: Function to compute the Planck temperature
!                                  for a specified channel given the radiance.
!                                  SOURCE: SENSOR_PLANCK_ROUTINES module
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
!       The downwelling radiance is first initialised to the space emissisio
!       boundary term using precalculated cosmic background radiances,
!
!         R_down = CBR * single_w0(1)
!
!       where the emissivity of space is implicitly assumed to be 1.0 and
!       single_w0(1) is the space-to-ground transmittance.
!
!       The contributions of all the layers EXCEPT THE SURFACE LAYER to the
!       downwelling flux is accumulated,
!
!                            __K-1
!                           \
!         R_down = R_down +  >  B(k) * dsingle_w0(k)
!                           /__
!                              k=1
!
!       The surface layer contribution is then added explicitly,
!
!         R_down = R_down + ( B(K) * ( 1 - single_w0(K) ) )
!
!       to avoid exceeding the arrays bounds of single_w0 
!       (i.e. single_w0(K+1) == 1.0 ) or requiring single_w0 to be
!       dimensioned 0:K.
!
!       The solar term is then added if required,
!
!         R_down = R_down + ( solar_irradiance * assy_g0 * COS(solar_theta) )
!
!       The downwelling radiance is then reflected off the surface, added
!       to the surface emission term, propagated upwards through the atmosphere
!       and used to initialise the upwelling radiance term,
!
!         R_up = ( ( e_sfc * B_sfc ) + ( r_sfc * R_down ) ) * tau(K)
!
!       The contributions of all the layers EXCEPT THE TOP LAYER to the
!       upwelling radiance is accumulated,
!
!                        __ 2
!                       \
!         R_up = R_up +  >  B(k) * dtau(k)
!                       /__
!                          k=K
!
!       The top layer contribution is then added explicitly,
!
!         R_up = R_up + ( B(1) * ( 1 - tau(1) ) )
!
!       to avoid exceeding the arrays bounds of tau (i.e. tau(0) == 1.0 )
!       or requiring tau to be dimensioned 0:K.
!
!       The final upwelling radiance is then converted to a brightness
!       temperature.
!
!S-      
!--------------------------------------------------------------------------------


  SUBROUTINE compute_radiance( Surface_Type,          &  ! Surface type
                             secant_view_angle,       &
                               temperature,           &  ! Input, K      
                               surface_temperature,   &  ! Input, scalar 
                               surface_emissivity,    &  ! Input, scalar 
                               surface_reflectivity,  &  ! Input, scalar 
                               opt_lay,                   &  ! Input, K      
                               alb_lay,              &  ! Input, K      
                               g_lay,             &  ! Input, scalar 

                               secant_solar_angle,    &  ! Input, scalar 
                               valid_solar,           &  ! Input, scalar 
                               channel_index,         &  ! Input, scalar 

                               Planck_layer,        &  ! Output, K     
                               Planck_surface,        &  ! Output, scalar
                               downwelling_radiance,  &  ! Output, scalar
                               upwelling_radiance,    &  ! Output, scalar
                               brightness_temperature )  ! Output, scalar

    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT( IN )  :: Surface_Type
    REAL( fp_kind ), INTENT( IN )  :: secant_view_angle 
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: temperature

    REAL( fp_kind ),                 INTENT( IN )  :: surface_temperature
    REAL( fp_kind ),                 INTENT( IN )  :: surface_emissivity
    REAL( fp_kind ),                 INTENT( IN )  :: surface_reflectivity

    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: opt_lay 
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: alb_lay 
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: g_lay 

    REAL( fp_kind ),                 INTENT( IN )  :: secant_solar_angle
    INTEGER,                         INTENT( IN )  :: valid_solar
    INTEGER,                         INTENT( IN )  :: channel_index

    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Planck_layer
    REAL( fp_kind ), INTENT( OUT ) :: Planck_surface
    REAL( fp_kind ),                 INTENT( OUT ) :: downwelling_radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: upwelling_radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: brightness_temperature


    ! ----------------
    ! Local parameters
    ! ----------------

    REAL( fp_kind ) :: u ,radiance
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Radiance'

    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n_layers
    INTEGER :: i, l, Error_Status

    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC SIZE

    !#--------------------------------------------------------------------------#
    !#                   -- Determine array dimensions --                       #
    !#--------------------------------------------------------------------------#
    u = ONE/secant_view_angle
    n_layers = SIZE( temperature )

    !#--------------------------------------------------------------------------#
    !#              -- Calculate the downwelling thermal flux --                #
    !#--------------------------------------------------------------------------#

    ! -- Assign the channel index to a short name
    l = channel_index

    ! --------------------------------------------
    ! Initialise the downwelling radiance to the
    ! space emission boundary term reaching the
    ! surface. Thhe cosmic background radiance is
    ! zero for infrared channels and precalculated
    ! for microwave channels. The emissivity of
    ! space is assumed to be 1.0.
    !
    ! Cosmic background data from the
    ! SPECTRAL_COEFFICIENTS module
    ! --------------------------------------------

    !downwelling_radiance = SC%Cosmic_Background_Radiance( l ) 


    ! --------------------------------
    ! Loop over layers from TOA->SFC-1
    ! --------------------------------

    k_down_layer_loop: DO k = 1, n_layers

      ! -- Calculate the Planck layer radiance
      CALL sensor_planck_radiance( l,                  &  ! Input
                                   temperature( k ),   &  ! Input
                                   Planck_layer( k ) )  ! Output

    END DO k_down_layer_loop

    !#--------------------------------------------------------------------------#
    !#   -- Reflect the downwelling radiance and add the surface emission --    #
    !#   -- and use it to initialise the upwelling radiance               --    #
    !#--------------------------------------------------------------------------#

    ! -- Calculate the surface term
    CALL sensor_planck_radiance( l,                   &  ! Input
                                 surface_temperature, &  ! Input
                                 Planck_surface   )  ! Output

    CALL RT_Solution(Surface_Type, u, alb_lay, g_lay, opt_lay, &
                         Planck_layer, Planck_surface, SC%Cosmic_Background_Radiance( l ), &
                         surface_emissivity, radiance,   &
                         downwelling_radiance,upwelling_radiance)

    !#--------------------------------------------------------------------------#
    !#           -- Convert the radiances to brightness temperatures --         #
    !#--------------------------------------------------------------------------#


    CALL sensor_planck_temperature( l,                     &  ! Input
                                    radiance,              &  ! Input
                                    brightness_temperature )  ! Output
    
    CALL sensor_planck_temperature(l,downwelling_radiance,downwelling_radiance)
    CALL sensor_planck_temperature(l,upwelling_radiance,upwelling_radiance)

    Error_Status = SUCCESS

  END SUBROUTINE compute_radiance


!--------------------------------------------------------------------------------
!S+
! NAME:
!       compute_radiance_TL
!
! PURPOSE:
!       PUBLIC subroutine to calculate the tangent-linear TOA radiance and
!       brightness temperature.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       CALL compute_radiance_TL( &
!
!                                 ! -- Forward inputs
!                                 temperature,              &  ! Input, K
!
!                                 surface_temperature,      &  ! Input, scalar
!                                 surface_emissivity,       &  ! Input, scalar
!                                 surface_reflectivity,     &  ! Input, scalar
!
!                                 tau,                      &  ! Input, K
!                                 single_w0,                 &  ! Input, K
!                                 assy_g0,                &  ! Input, scalar
!
!                                 layer_radiance,           &  ! Input, K
!                                 downwelling_radiance,     &  ! Input, scalar
!                                 upwelling_radiance,       &  ! Input, scalar
!
!                                 ! -- Tangent-linear inputs
!                                 temperature_TL,           &  ! Input, K
!
!                                 surface_temperature_TL,   &  ! Input, scalar
!                                 surface_emissivity_TL,    &  ! Input, scalar
!                                 surface_reflectivity_TL,  &  ! Input, scalar
!
!                                 tau_TL,                   &  ! Input, K
!                                 single_w0_TL,              &  ! Input, K
!                                 assy_g0_TL,             &  ! Input, scalar
!
!                                 ! -- Other inputs
!                                 secant_solar_angle,       &  ! Input, scalar
!                                 valid_solar,              &  ! Input, scalar
!                                 channel_index,            &  ! Input, scalar
!
!                                 ! -- Tangent-linear outputs
!                                 layer_radiance_TL,        &  ! Output, K
!                                 downwelling_radiance_TL,  &  ! Output, scalar
!                                 upwelling_radiance_TL,    &  ! Output, scalar
!
!                                 brightness_temperature_TL )  ! Output, scalar
!
!
! INPUT ARGUMENTS:
!       temperature:               Profile LAYER average temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       Float
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN )
!
!       surface_temperature:       Surface boundary temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       surface_emissivity:        Surface boundary emissivity
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       surface_reflectivity:      Surface boundary reflectivity
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       tau:                       Layer-to-space transmittance profile for
!                                  a particular satellite view angle.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       single_w0:                  Layer-to-surface transmittance profile for
!                                  either the diffuse approximation angle (IR)
!                                  or the satellite view angle (MW). The latter
!                                  assumes specular reflectivity.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       assy_g0:                 Total space-to-surface transmittance at the
!                                  solar zenith angle.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       layer_radiance:            Channel Planck radiance for every layer.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       Real
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       downwelling_radiance:      Channel radiance at surface due to downwelling
!                                  flux and solar components.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       upwelling_radiance:        Channel TOA radiance simulating the satellite
!                                  sensor measurement. This is composed of the
!                                  reflected downwelling propagated through the
!                                  atmosphere as well as the upwelling only component.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       temperature_TL:            Tangent-linear temperature profile.
!                                  UNITS:      Kelvin
!                                  TYPE:       Float
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       surface_temperature_TL:    Tangent-linear surface boundary temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       surface_emissivity_TL:     Tangent-linear surface boundary emissivity
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       surface_reflectivity_TL:   Tangent-linear surface boundary reflectivity
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       tau_TL:                    Tangent-linear layer-to-space transmittance
!                                  profile.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       single_w0_TL:               Tangent-linear layer-to-surface flux transmittance
!                                  profile.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       assy_g0_TL:              Tangent-linear total space-to-surface solar
!                                  transmittance.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       secant_solar_angle:        Secant of the solar zenith angle corresponding
!                                  to that used in calculating the total solar
!                                  transmittance.
!                                  UNITS:      None
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       valid_solar:               Flag indicating if the solar component should
!                                  be included.
!                                  If = 0, no solar (if sensor channel frequency
!                                          is less than a preset cutoff or if solar
!                                          zenith angle is greater than its preset
!                                           cutoff.)
!                                     = 1, include solar
!                                  UNITS:      None.
!                                  TYPE:       Integer
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       channel_index:             Channel index id. This is a unique index
!                                  to a (supported) sensor channel.
!                                  UNITS:      None
!                                  TYPE:       Integer
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       layer_radiance_TL:         Tangent-linear channel Planck radiance for
!                                  every layer.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       Real
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( OUT )
!
!       downwelling_radiance_TL:   Tangent-linear channel radiance at surface
!                                  due to downwelling flux and solar components.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( OUT )
!
!       upwelling_radiance_TL:     Tangent-linear channel TOA radiance.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( OUT )
!
!       brightness_temperature_TL: Tangent-linear channel TOA brightness
!                                  temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       Real
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       sensor_planck_radiance_TL:    Function to compute the tangent-linear
!                                     Planck radiance.
!                                     SOURCE: SENSOR_PLANCK_ROUTINES module
!
!       sensor_planck_temperature_TL: Function to compute the tangent-linear
!                                     Planck temperature.
!                                     SOURCE: SENSOR_PLANCK_ROUTINES module
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
!       The downwelling radiance is first initialised to the space emission
!       boundary term using precalculated cosmic background radiances,
!
!         R_down_TL = CBR * single_w0_TL(1)
!
!       and the emissivity of space is implicitly assumed to be 1.0 and
!       single_w0_TL(1) is the tangent-linear form of the space-to-ground
!       transmittance.
!
!       The contributions of all the layers EXCEPT THE SURFACE LAYER to the
!       downwelling flux is accumulated,
!
!                                  __K-1
!                                 \
!         R_down_TL = R_down_TL +  >  ( B(k) * dsingle_w0_TL(k) ) + ( B_TL(k) * dsingle_w0(k) )
!                                 /__
!                                    k=1
!
!       The surface layer contribution is then added explicitly,
!
!         R_down_TL = R_down_TL + ( B(K) * ( -single_w0_TL(K) ) ) + ( B_TL(K) * ( 1 - single_w0(K) ) )
!
!       to avoid exceeding the arrays bounds of single_w0 and single_w0_TL
!       (i.e. single_w0(K+1) == 1.0 ) or requiring them to be
!       dimensioned 0:K.
!
!       The solar term is then added if required,
!
!         R_down_TL = R_down_TL + ( solar_irradiance * assy_g0_TL * COS(solar_theta) )
!
!       The downwelling radiance is then reflected off the surface, added
!       to the surface emission term, propagated upwards through the atmosphere
!       and used to initialise the upwelling radiance term,
!
!         R_up_TL = ( e_sfc    * B_sfc_TL  * tau(K) ) + &
!                   ( e_sfc_TL * B_sfc     * tau(K) ) + &
!                   ( r_sfc    * R_down_TL * tau(K) ) + &
!                   ( r_sfc_TL * R_down    * tau(K) ) + &
!                   ( ( ( e_sfc * B_sfc ) + ( r_sfc * R_down ) ) * tau_TL(K) )
!
!       The contributions of all the layers EXCEPT THE TOP LAYER to the
!       upwelling radiance is accumulated,
!
!                              __ 2
!                             \
!         R_up_TL = R_up_TL +  >  ( B(k) * dtau_TL(k) ) + ( B_TL(k) * dtau(k) )
!                             /__
!                                k=K
!
!       The top layer contribution is then added explicitly,
!
!         R_up_TL = R_up_TL + ( B(1) * ( -tau_TL(1) ) ) + ( B_TL(1) * ( 1 - tau(1) ) )
!
!       to avoid exceeding the arrays bounds of tau (i.e. tau(0) == 1.0 )
!       or tau_TL or requiring them to be dimensioned 0:K.
!
!       The final tangent-linear upwelling radiance is then converted to a
!       tangent-linear  brightness temperature.
!
!S-      
!--------------------------------------------------------------------------------


  SUBROUTINE compute_radiance_TL( Surface_Type,          &  ! Surface type
                                secant_view_angle,       &
                                  ! -- Forward inputs
                                  temperature,              &  ! Input, K
                                  surface_temperature,      &  ! Input, scalar
                                  surface_emissivity,       &  ! Input, scalar
                                  surface_reflectivity,     &  ! Input, scalar
                                  opt_lay,                  &  ! Input, K
                                  alb_lay,                  &  ! Input, K
                                  g_lay,                    &  ! Input, scalar

                                  Planck_layer,             &  ! Input, K
                                  downwelling_radiance,     &  ! Input, scalar
                                  upwelling_radiance,       &  ! Input, scalar
                                  brightness_temperature,   &  ! Output, scalar
                                  Planck_surface,           &
                                  ! -- Tangent-linear inputs
                                  temperature_TL,           &  ! Input, K

                                  surface_temperature_TL,   &  ! Input, scalar
                                  surface_emissivity_TL,    &  ! Input, scalar
                                  surface_reflectivity_TL,  &  ! Input, scalar

                                  opt_lay_TL,               &  ! Input, K
                                  alb_lay_TL,               &  ! Input, K
                                  g_lay_TL,                 &  ! Input, scalar

                                  ! -- Other inputs
                                  secant_solar_angle,       &  ! Input, scalar
                                  valid_solar,              &  ! Input, scalar
                                  channel_index,            &  ! Input, scalar

                                  ! -- Tangent-linear outputs
                                  Planck_layer_TL,        &  ! Output, K
                                  Planck_surface_TL,           &
                                  downwelling_radiance_TL,  &  ! Output, scalar
                                  upwelling_radiance_TL,    &  ! Output, scalar
                                  brightness_temperature_TL )  ! Output, scalar

    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT( IN )  :: Surface_Type
    REAL( fp_kind ), INTENT( IN )  :: secant_view_angle
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: temperature
                                                                                                            
    REAL( fp_kind ),                 INTENT( IN )  :: surface_temperature
    REAL( fp_kind ),                 INTENT( IN )  :: surface_emissivity
    REAL( fp_kind ),                 INTENT( IN )  :: surface_reflectivity
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: opt_lay
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: alb_lay
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: g_lay
                                                                                                            
    REAL( fp_kind ),                 INTENT( IN )  :: secant_solar_angle
    INTEGER,                         INTENT( IN )  :: valid_solar
    INTEGER,                         INTENT( IN )  :: channel_index
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Planck_layer
    REAL( fp_kind ), INTENT( OUT ) :: Planck_surface
    REAL( fp_kind ),                 INTENT( OUT ) :: downwelling_radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: upwelling_radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: brightness_temperature

    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: temperature_TL
                                                                                                            
    REAL( fp_kind ),                 INTENT( IN OUT )  :: surface_temperature_TL
    REAL( fp_kind ),                 INTENT( IN OUT )  :: surface_emissivity_TL
    REAL( fp_kind ),                 INTENT( IN OUT )  :: surface_reflectivity_TL
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: opt_lay_TL
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: alb_lay_TL
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: g_lay_TL
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Planck_layer_TL
    REAL( fp_kind ), INTENT( OUT ) :: Planck_surface_TL
    REAL( fp_kind ),                 INTENT( IN OUT ) :: downwelling_radiance_TL
    REAL( fp_kind ),                 INTENT( OUT ) :: upwelling_radiance_TL
    REAL( fp_kind ),                 INTENT( OUT ) :: brightness_temperature_TL
    ! ----------------
    ! Local parameters
    ! ----------------
                                                                                                            
    REAL( fp_kind ) :: u
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Radiance_TL'
                                                                                                            
    ! ---------------
    ! Local variables
    ! ---------------
                                                                                                            
    INTEGER :: k, n_layers
    INTEGER :: i, l, Error_Status
                                                                                                            
    ! ----------
    ! Intrinsics
    ! ----------
                                                                                                            
    INTRINSIC SIZE
                                                                                                            
    !#--------------------------------------------------------------------------#
    !#                   -- Determine array dimensions --                       #
    !#--------------------------------------------------------------------------#
    u = ONE/secant_view_angle
    n_layers = SIZE( temperature )
                                                                                                            
    !#--------------------------------------------------------------------------#
    !#              -- Calculate the downwelling thermal flux --                #
    !#--------------------------------------------------------------------------#
                                                                                                            
    ! -- Assign the channel index to a short name
    l = channel_index
                                                                                                            
    ! --------------------------------------------
    ! Initialise the downwelling radiance to the
    ! space emission boundary term reaching the
    ! surface. Thhe cosmic background radiance is
    ! zero for infrared channels and precalculated
    ! for microwave channels. The emissivity of
    ! space is assumed to be 1.0.
    !
    ! Cosmic background data from the
    ! SPECTRAL_COEFFICIENTS module
    ! --------------------------------------------
                                                                                                            
    downwelling_radiance = SC%Cosmic_Background_Radiance( l )
                                                                                                            
    ! --------------------------------
    ! Loop over layers from TOA->SFC-1
    ! --------------------------------
                                                                                                            
    k_down_layer_loop: DO k = 1, n_layers
                                                                                                            
      ! -- Calculate the Planck layer radiance
      CALL sensor_planck_radiance( l,                  &  ! Input
                                   temperature( k ),   &  ! Input
                                   Planck_layer( k ) )  ! Output

      CALL sensor_planck_radiance_TL( l,                     &  ! Input
                                      temperature( k ),      &  ! Input
                                      temperature_TL( k ),   &  ! Input
                                      Planck_layer_TL( k ) )  ! Output
    END DO k_down_layer_loop
                                                                                                            
                                                                                                            
                                                                                                            
    !#--------------------------------------------------------------------------#
    !#   -- Reflect the downwelling radiance and add the surface emission --    #
    !#   -- and use it to initialise the upwelling radiance               --    #
    !#--------------------------------------------------------------------------#
                                                                                                            
    ! -- Calculate the surface term
    CALL sensor_planck_radiance( l,                   &  ! Input
                                 surface_temperature, &  ! Input
                                 Planck_surface   )  ! Output
                                                                                                            
    CALL sensor_planck_radiance_TL( l,                   &  ! Input
                                 surface_temperature, &  ! Input
                                 surface_temperature_TL, &  ! Input
                                 Planck_surface_TL   )  ! Output
                                                                   
                                         
    CALL RT_Solution(Surface_Type, u, alb_lay, g_lay, opt_lay, &
                         Planck_layer, Planck_surface, downwelling_radiance, &
                         surface_emissivity, upwelling_radiance)

    CALL RT_Solution_TL(SUrface_Type, u, alb_lay, g_lay, opt_lay, &
                         Planck_layer, Planck_surface, downwelling_radiance, &
                         surface_emissivity, &
                         ! --- Tangent-liner input
                         alb_lay_TL, g_lay_TL, opt_lay_TL, &
                         Planck_layer_TL, Planck_surface_TL, &
                         surface_emissivity_TL, &
                         ! --- Tangent-liner output
                         upwelling_radiance_TL)

                                                                                                            
    !#--------------------------------------------------------------------------#
    !#           -- Convert the radiances to brightness temperatures --         #
    !#--------------------------------------------------------------------------#
                                                                                                            
    CALL sensor_planck_temperature( l,                     &  ! Input
                                    upwelling_radiance,    &  ! Input
                                    brightness_temperature )  ! Output
                                                     
    CALL sensor_planck_temperature_TL( l,                        &  ! Input
                                       upwelling_radiance,       &  ! Input
                                       upwelling_radiance_TL,    &  ! Input
                                       brightness_temperature_TL )  ! Output
                                                       
    Error_Status = SUCCESS


  END SUBROUTINE compute_radiance_TL


  SUBROUTINE compute_radiance_AD( Surface_Type,          &  ! Surface type
                               secant_view_angle,       &
                               ! -- Forward inputs
                               temperature,              &  ! Input, K
                               surface_temperature,      &  ! Input, scalar
                               surface_emissivity,       &  ! Input, scalar
                               surface_reflectivity,     &  ! Input, scalar
                               opt_lay,                  &  ! Input, K
                               alb_lay,                  &  ! Input, K
                               g_lay,                    &  ! Input, scalar
                               Planck_layer,             &  ! Input, K
                               Planck_surface,           &
                               downwelling_radiance,     &  ! Input, scalar
                               upwelling_radiance,       &  ! Input, scalar
                               brightness_temperature,   &  ! Output, scalar
                                  ! -- Tangent-linear inputs
                               Planck_layer_AD,          &
                               Planck_surface_AD,        &
                               downwelling_radiance_AD,  &
                               upwelling_radiance_AD,    &
                               brightness_temperature_AD, &  ! In/Output, scalar
                                ! -- Other input
                                secant_solar_angle,            &  ! Input, scalar
                                valid_solar,                   &  ! Input, scalar
                                channel_index,            &  ! Input, scalar
                                ! -- K-matrix output
                                temperature_K,             &  ! In/Output, K
                                surface_temperature_K,    &  ! In/Output, scalar
                                surface_emissivity_AD,     &  ! In/Output, scalar
                                surface_reflectivity_AD,   &  ! In/Output, scalar
                                opt_lay_AD,          &   ! Output, K
                                    alb_lay_AD,           &   ! OutInput, K
                                      g_lay_AD )              ! Output, scalar

    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT( IN )  :: Surface_Type
    REAL( fp_kind ), INTENT( IN )  :: secant_view_angle
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: temperature
                                                                                                            
    REAL( fp_kind ),                 INTENT( IN )  :: surface_temperature
    REAL( fp_kind ),                 INTENT( IN )  :: surface_emissivity
    REAL( fp_kind ),                 INTENT( IN )  :: surface_reflectivity
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: opt_lay
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: alb_lay
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: g_lay
                                                                                                            
    REAL( fp_kind ),                 INTENT( IN )  :: secant_solar_angle
    INTEGER,                         INTENT( IN )  :: valid_solar
    INTEGER,                         INTENT( IN )  :: channel_index
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Planck_layer
    REAL( fp_kind ), INTENT( IN OUT ) :: Planck_surface
    REAL( fp_kind ),                 INTENT( IN OUT ) :: downwelling_radiance
    REAL( fp_kind ),                 INTENT( IN OUT ) :: upwelling_radiance
    REAL( fp_kind ),                 INTENT( IN OUT ) :: brightness_temperature

    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: temperature_K
                                                                                                            
    REAL( fp_kind ),                 INTENT( IN OUT )  :: surface_temperature_K
    REAL( fp_kind ),                 INTENT( IN OUT )  :: surface_emissivity_AD
    REAL( fp_kind ),                 INTENT( IN OUT )  :: surface_reflectivity_AD
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: opt_lay_AD
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: alb_lay_AD
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT )  :: g_lay_AD
                                                                                                            
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Planck_layer_AD
    REAL( fp_kind ), INTENT( OUT ) :: Planck_surface_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: downwelling_radiance_AD
    REAL( fp_kind ),                 INTENT( OUT ) :: upwelling_radiance_AD
    REAL( fp_kind ),                 INTENT( IN ) :: brightness_temperature_AD
    ! ----------------
    ! Local parameters
    ! ----------------
                                                                                                            
    REAL( fp_kind ) :: u
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Radiance_AD'
                                                                                                            
    ! ---------------
    ! Local variables
    ! ---------------
                                                                                                            
    INTEGER :: k, n_layers
    INTEGER :: i, l, Error_Status
                                                                                                            
    ! ----------
    ! Intrinsics
    ! ----------
                                                                                                            
    INTRINSIC SIZE
                                                                                                            
    !#--------------------------------------------------------------------------#
    !#                   -- Determine array dimensions --                       #
    !#--------------------------------------------------------------------------#
    u = ONE/secant_view_angle
    n_layers = SIZE( temperature )
                                                                                                            
    !#--------------------------------------------------------------------------#
    !#              -- Calculate the downwelling thermal flux --                #
    !#--------------------------------------------------------------------------#
                                                                                                            
    ! -- Assign the channel index to a short name
    l = channel_index
                                                                                                            
    ! --------------------------------------------
    ! Initialise the downwelling radiance to the
    ! space emission boundary term reaching the
    ! surface. Thhe cosmic background radiance is
    ! zero for infrared channels and precalculated
    ! for microwave channels. The emissivity of
    ! space is assumed to be 1.0.
    !
    ! Cosmic background data from the
    ! SPECTRAL_COEFFICIENTS module
    ! --------------------------------------------
                                                                                                            
    !downwelling_radiance = SC%Cosmic_Background_Radiance( l )

    CALL sensor_planck_temperature_AD( l,                        &  ! Input
                                       upwelling_radiance,       &  ! Input
                                       brightness_temperature_AD, &  ! Intput
                                       upwelling_radiance_AD )      ! Output

    ! ------------------------------------------------------------------
    !   Initializing 
    ! ------------------------------------------------------------------
         alb_lay_AD = ZERO
         g_lay_AD = ZERO
         opt_lay_AD = ZERO
         Planck_layer_AD = ZERO
         Planck_surface_AD = ZERO
         surface_emissivity_AD = ZERO

   CALL RT_Solution_AD(Surface_Type, u, alb_lay, g_lay, opt_lay, &
                         Planck_layer, Planck_surface, SC%Cosmic_Background_Radiance( l ), &
                         surface_emissivity, &
                     !--- Adjoint input
                         upwelling_radiance_AD, &

                         ! --- Tangent-liner input
                         alb_lay_AD, g_lay_AD, opt_lay_AD, &
                         Planck_layer_AD, Planck_surface_AD, &
                         surface_emissivity_AD )

!       upwelling_radiance = 0.001 * alb_lay(46) * opt_lay(46) * g_lay(46)
!!      opt_lay_AD(46) = 0.001 * alb_lay(46) * upwelling_radiance_AD*g_lay(46)
!!      alb_lay_AD(46) = 0.001 * upwelling_radiance_AD * opt_lay(46)*g_lay(46)
!!       g_lay_AD(46) = 0.001 * upwelling_radiance_AD * alb_lay(46) *opt_lay(46)

   CALL sensor_planck_radiance_AD( l,                   &  ! Input
                                 surface_temperature, &    ! Input
                                 Planck_surface_AD, &      ! Input
                                 surface_temperature_K )  ! Input


   ! --------------------------------
    ! Loop over layers from TOA->SFC-1
    ! --------------------------------
    k_down_layer_loop: DO k = n_layers, 1, -1

      CALL sensor_planck_radiance_AD( l,                     &  ! Input
                                      temperature( k ),      &  ! Input
                                      Planck_layer_AD( k ),  &  ! Intput
                                      temperature_K( k ) )     ! Input
    END DO k_down_layer_loop

   !
    Error_Status = SUCCESS

  END SUBROUTINE compute_radiance_AD

END MODULE radiance


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: radiance.f90,v 1.4 2004/07/02 15:28:50 treadon Exp $
!
! $Date: 2004/07/02 15:28:50 $
!
! $Revision: 1.4 $
!
! $Name: ncep-gsi-2004_06 $
!
! $State: Exp $
!
! $Log: radiance.f90,v $
! Revision 1.4  2004/07/02 15:28:50  treadon
! add NCEP docblock
!
! Revision 1.3  2004/05/06 15:55:34  treadon
! modify !Name: comment near end of file
!
! Revision 1.2  2004/02/18 16:01:37  treadon
! implemented unified GSI
!
! Revision 2.6  2003/05/02 18:14:03  paulv
! - Spectral coefficient data for the cosmic background radiance and solar
!   irradiance is now via the SpcCoeff structure.
!
! Revision 2.5  2001/10/01 20:28:47  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 2.4  2001/08/16 17:11:57  paulv
! - Updated documentation
!
! Revision 2.3  2001/08/08 20:02:12  paulv
! - Removed sensor Planck function routines and placed them in their own
!   module, SENSOR_PLANCK_ROUTINES. Some routines were required for other
!   uses so their PRIVATE subprogram status wasn't amenable to code-sharing.
! - Updated header documentation.
!
! Revision 2.2  2001/08/01 16:58:32  paulv
! - Corrected bug in COMPUTE_RADIANCE() function. The initialisation
!   statement of the downwelling radiance was,
!      downwelling_radiance = cosmic_background_radiance( l )
!   and has been changed to,
!      downwelling_radiance = cosmic_background_radiance( l ) * single_w0( 1 )
!   i.e. the transmission of the space emission term to the surface. This
!   was a holdover from earlier versions of the functions when the transmittances
!   were calculated and passed as *layer* rather than layer-to-surface
!   transmittances.
! - Removed initialisation and zeroing of the adjoint of the surface emission
!   term SURFACE_B_AD. This is used in only one place so there is no need to
!   do,
!     surface_B_AD = ZERO
!     surface_B_AD = surface_B_AD + &
!                    ( tau( n_layers ) * surface_emissivity * upwelling_radiance_AD )
!     ....use surface_B_AD...
!     surface_B_AD = ZERO
!   when,
!     surface_B_AD = ( tau( n_layers ) * surface_emissivity * upwelling_radiance_AD )
!   will do.
! - Updated documentation.
!
! Revision 2.1  2001/05/29 18:05:29  paulv
! - All tangent-linear and adjoint routines included.
! - No more optional arguments of downwelling flux and surface reflectivity -
!   they are expected.
! - Altered the method of calculating the layer contributions. Changed code
!   from:
!     layer_radiance(k) = (1-tau)*B(T) + tau*layer_radiance(k-1)
!   to:
!     layer_radiance(k) = B(T) * dtau
!
! Revision 1.5  2001/01/24 20:14:21  paulv
! - Latest test versions.
!
! Revision 1.4  2000/11/09 20:46:07  paulv
! - Added solar term.
! - Downwelling flux transmittance term is now an optional argument.
!   If not specified, the layer radiances are calculated during the
!   upwelling radiance integration.
! - Surface reflectivity is an optional argument. If not specified the
!   surface emissivity is used to generate an isotropic reflectivity.
!
! Revision 1.3  2000/08/31 19:36:33  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.2  2000/08/24 15:48:34  paulv
! - Replaced "regular" reflectivity for reflected downwelling thermal with
!   the isotropic reflectivity in the COMPUTE_RADIANCE subprogram.
! - Updated module and subprogram documentation.
!
! Revision 1.1  2000/08/21 20:59:34  paulv
! Initial checkin.
!
!
!
!
