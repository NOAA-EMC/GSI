module forward_model
!$$$  module documentation block
!                .      .    .                                       .
!   2004-10-26  Quanhua Liu, Yong Han, Paul van delst, and Fuzhong Weng
!
!   Two-stream radiative transfer forward model
!
! 
! module:   absorber_profile
!   prgmmr: van delst        org: np20                date: 2000-07-15
!
! abstract:  Module containing the NCEP RT forward model function.
!
! module history log:
!   2000-07-15  van delst, paul
!   2000-08-24  van delst - numerous change; update documentation
!   2000-08-31  van delst - add documentation delimiters; update 
!                           documentation headers
!   2000-11-09  van delst - make radiance function call consistent with 
!                           changes to that module (see radiance.f90)
!   2001-01-24  van delst - latest test version
!   2001-05-29  van delst - use TYPE_KINDS module; add ABSORBER_SPACE 
!                           and PREDICTORS module; additional changes
!   2001-07-12  van delst - comment out informational message output 
!                           at start of function
!   2001-08-01  van delst - remove USE of ABSORBER_SPACE module; ONLY 
!                           clauses added to other USE statements; add
!                           COMPUTE_RTM function; update input argument
!                           checking
!   2001-08-16  van delst - update documentation; change way channel
!                           dimension is obtained
!   2001-08-31  van delst - remove input data checks from COMPUTE_RTM;
!                           add check for negative profile and surface 
!                           data in FORWARD_RTM; changes related to
!                           maximum solar angle
!   2001-09-04  van delst - update documentation
!   2001-09-28  van delst - overload COMPUTE_RTM and FORWARD_RTM; put
!                           N_INPUT_PROFILES back in COMPUTE_RTM and
!                           FORWARD_RTM; change SURFACE_TEMPERATURE
!                           check; add "Name" to RCS keyword list
!   2001-11-07  van delst - add check for negative number of channels;
!                           add profile loop CYCLE statement; change
!                           logical IF test for number of input profiles
!   2003-05-02  van delst - update for use with new transmittance algorithm 
!                           and spectral coefficients module
!   2004-06-21  treadon   - add NCEP doc block
!
! Subroutines Included:
!
! Functions Included:
!   compute_rtm - PUBLIC function that calculates the forward model
!                 top-of-atmosphere (TOA) radiances and brightness
!                 temperatures for an input atmospheric profile set and
!                 user specified satellites/channels.
!
!                 This function is simply a wrapper around the FORWARD
!                 model so that the user doesn't have to declare the
!                 absorber/predictor/etc arrays in the calling routine.
!
!   forward_rtm - PUBLIC function that calculates top-of-atmosphere (TOA)
!                 radiances and brightness temperatures for user specified
!                 profiles and satellites/channels.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
!------------------------------------------------------------------------------
!M+
! NAME:
!       forward_model
!
! PURPOSE:
!       Module containing the NCEP RT forward model function.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE forward_model
!
! OUTPUTS:
!       None.
!
! MODULES:
!       type_kinds:            Module to define kind types for variable declaration.
!
!       error_handler:         Module to define error codes and handle error conditions
!
!       parameters:            Module containing parameter definitions for the
!                              RT model.
!
!       spectral_coefficients: Module containing the RT model spectral coefficients.
!
!       absorber_profile:      Module containing routines for generating the absorber
!                              profiles.
!
!       predictors:            Module containing routines for generating the predictor
!                              profiles.
!
!       transmittance:         Module containing transmittance calculation routines.
!
!       radiance:              Module containing radiance calculation routines.
!
! CONTAINS:
!       compute_rtm:           PUBLIC function that calculates the forward model 
!                              top-of-atmosphere (TOA) radiances and brightness 
!                              temperatures for an input atmospheric profile set and
!                              user specified satellites/channels.
!
!                              This function is simply a wrapper around the FORWARD
!                              model so that the user doesn't have to declare the
!                              absorber/predictor/etc arrays in the calling routine.
!
!       forward_rtm:           PUBLIC function that calculates top-of-atmosphere (TOA)
!                              radiances and brightness temperatures for user specified
!                              profiles and satellites/channels.
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
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP 15-July-2000
!                       pvandelst@ncep.noaa.gov
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

!MODULE forward_model

  ! ------------
  ! Module usage
  ! ------------

  USE type_kinds,            ONLY : fp_kind
  USE error_handler
  USE CRTM_Parameters
  USE CRTM_SpcCoeff
  USE mw_cloud_opt
  USE Surface_Emissivity_Model
  USE CRTM_Fastem3
!  USE spectral_coefficients

  USE absorber_profile,      ONLY : compute_absorber_amount
  USE predictors,            ONLY : compute_predictors
  USE transmittance,         ONLY : compute_transmittance
  USE radiance,              ONLY : compute_radiance

  USE RT_Twostream_Variable
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: compute_rtm
  PUBLIC :: forward_rtm


CONTAINS


  FUNCTION compute_rtm( &
             ! -- Forward inputs
             level_p, layer_p, layer_t, layer_w, layer_o, &  ! Input, K
             layer_clw, layer_rain, layer_snow, layer_gh, layer_ice, &
             surface_temperature,                         &  ! Input, Scalar
             Surface_Type, &
             Surface_Wind_s, Surface_Wind_d, &

             ! -- Other inputs
             secant_view_angle,                           &  ! Input, Scalar
             secant_solar_angle,                          &  ! Input, Scalar
             n_channels,                                  &  ! Input, Scalar
             channel_index,                               &  ! Input, L
             PolStatus,                                   &  ! Input, L

             upwelling_radiance,                          &  ! Output, L
             brightness_temperature,                      &  ! Output, L

             ! -- Optional inputs
             surface_emissivity_i,                          &  ! Input, L
             surface_reflectivity_i,                        &  ! Input, L
             message_log )                                &

           RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: level_p                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_p                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_t                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_w                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_o                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_clw                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_rain                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_snow                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_gh                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: layer_ice                 ! K

    REAL( fp_kind ),                    INTENT( IN )  :: surface_temperature     ! Scalar
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( INOUT ) :: surface_emissivity_i   ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( INOUT ) :: surface_reflectivity_i ! L

    ! -- Other inputs
    REAL( fp_kind ),                    INTENT( IN )  :: secant_view_angle       ! Scalar
    REAL( fp_kind ),                    INTENT( IN )  :: secant_solar_angle      ! Scalar
    INTEGER,                            INTENT( IN )  :: n_channels              ! Scalar
    INTEGER,         DIMENSION( : ),    INTENT( IN )  :: channel_index           ! L
    INTEGER,         DIMENSION( : ),    INTENT( IN )  :: PolStatus           ! L

    REAL( fp_kind ), DIMENSION( : ),    INTENT( OUT ) :: upwelling_radiance      ! L
    REAL( fp_kind ), DIMENSION( : ),    INTENT( OUT ) :: brightness_temperature  ! L

    ! -- Optional input. Note that N_INPUT_PROFILES is not used in this
    ! -- function. It is included here so if a user specifies it by mistake
    ! -- for rank-1 profile input the code won't (hopefully) fall in a heap.
    CHARACTER( * ), OPTIONAL,           INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'COMPUTE_RTM_RANK1'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Array for integrated absorber amounts, 0:K x J
    REAL( fp_kind ), DIMENSION( 0:SIZE( layer_p, DIM = 1 ), &
                                  MAX_N_ABSORBERS           ) :: absorber

    ! -- Arrays for predictors, Imax x K
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( layer_p, DIM = 1 )  ) :: tau_predictor

    ! -- Array for layer Planck radiance term, K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: layer_radiance  ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) ) :: Planck_surface  ! L
      
    ! -- Forward outputs
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: opt_lay   ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: alb_lay   ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: g_lay   ! K x L

    ! -- Array for downwelling radiance (flux + solar), L
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) ) :: downwelling_radiance

     INTEGER :: Surface_Type
     REAL( fp_kind ) :: Surface_Wind_s, Surface_Wind_d


    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC ADJUSTL, &
              MAXVAL,  &
              SIZE,    &
              TRIM


    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FORWARD RADIANCES AND TEMPERATURES --             #
    !#--------------------------------------------------------------------------#

    error_status = forward_rtm( &
                     ! -- Forward inputs
                     level_p, layer_p, layer_t, layer_w, layer_o,  &  ! Input,  K
                     layer_clw, layer_rain, layer_snow, layer_gh, layer_ice, &
                     surface_temperature,                          &  ! Input,  Scalar
                     Surface_Type,                                 &
                     Surface_Wind_s, Surface_Wind_d,               &

                     ! -- Other inputs
                     secant_view_angle,                            &  ! Input,  Scalar
                     secant_solar_angle,                           &  ! Input,  Scalar
                     n_channels,                                   &  ! Input,  Scalar
                     channel_index,                                &  ! Input,  L
             Polstatus,                                   &  ! Input,  L

                     ! -- Outputs
                     absorber,                                     &  ! Output, 0:K x J
                     tau_predictor,                                &  ! Output, Imax x K
                     opt_lay,                                      &  ! Output, K x L
                     alb_lay,                                      &  ! Output, K x L
                     g_lay,                                        &  ! Output, K x L
                     layer_radiance,                               &  ! Output, K x L
                     Planck_surface,                               &  ! Output, L
                     downwelling_radiance,                         &  ! Output, L
                     upwelling_radiance,                           &  ! Output, L
                     brightness_temperature,                       &  ! Output, L
                     surface_emissivity_i = surface_emissivity_i,      &  ! Input,  L
                     surface_reflectivity_i = surface_reflectivity_i,  &  ! Input,  L
                     message_log = message_log )


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    IF ( error_status /= SUCCESS ) THEN

      CALL display_message( ROUTINE_NAME, &
                            'Error occured in FORWARD_RTM_RANK1', &
                            error_status, &
                            message_log = message_log )
      RETURN

    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS


  END FUNCTION compute_rtm


  FUNCTION forward_rtm( &

             ! -- Inputs
             level_p, layer_p, layer_t, layer_w, layer_o, &  ! Input, K
             layer_clw, layer_rain, layer_snow, layer_gh, layer_ice, &
             surface_temperature,                         &  ! Input,  Scalar
             Surface_Type, &
             Surface_Wind_s, Surface_Wind_d, &

             secant_view_angle,                           &  ! Input,  Scalar
             secant_solar_angle,                          &  ! Input,  Scalar
             n_channels,                                  &  ! Input,  Scalar
             channel_index,                               &  ! Input,  L
             Polstatus,                                   &  ! Input,  L

             ! -- Outputs
             absorber,                                    &  ! Output, 0:K x J
             tau_predictor,                               &  ! Output, Imax x K
             opt_lay,                                      &  ! Output, K x L
             alb_lay,                                      &  ! Output, K x L
             g_lay,                                        &  ! Output, K x L

             layer_radiance,                              &  ! Output, K x L
             Planck_surface,                              &  ! Output, L
             downwelling_radiance,                        &  ! Output, L
             upwelling_radiance,                          &  ! Output, L
             brightness_temperature,                      &  ! Output, L

             ! -- Optional inputs
             surface_emissivity_i,                          &  ! Input,  L
             surface_reflectivity_i,                        &  ! Input,  L
             message_log )                                &

           RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
     INTEGER :: Surface_Type
     REAL( fp_kind ) :: Surface_Wind_s, Surface_Wind_d
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: level_p                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_p                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_t                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_w                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_o                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_clw                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_rain                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_snow                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_gh                 ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: layer_ice                 ! K

    REAL( fp_kind ),                     INTENT( IN )  :: surface_temperature     ! Scalar
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( INOUT )  :: surface_emissivity_i      ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( INOUT )  :: surface_reflectivity_i    ! L

    REAL( fp_kind ),                     INTENT( IN )  :: secant_view_angle       ! Scalar
    REAL( fp_kind ),                     INTENT( IN )  :: secant_solar_angle      ! Scalar
    INTEGER,                             INTENT( IN )  :: n_channels              ! Scalar
    INTEGER,         DIMENSION( : ),     INTENT( IN )  :: channel_index           ! L
    INTEGER,         DIMENSION( : ),     INTENT( IN )  :: PolStatus               ! L

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: absorber                ! 0:K x J

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: tau_predictor           ! Imax x K

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: opt_lay                ! K x L
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: alb_lay                ! K x L
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: g_lay                  ! K x L

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: layer_radiance          ! K x L
    REAL( fp_kind ), DIMENSION( : ),     INTENT( OUT ) :: Planck_surface          ! K x L
    REAL( fp_kind ), DIMENSION( : ),     INTENT( OUT ) :: downwelling_radiance    ! L
    REAL( fp_kind ), DIMENSION( : ),     INTENT( OUT ) :: upwelling_radiance      ! L

    REAL( fp_kind ), DIMENSION( : ),     INTENT( OUT ) :: brightness_temperature  ! L

    ! -- Optional input. Note that N_INPUT_PROFILES is not used in this
    ! -- function. It is specified so if a user specifies it by mistake
    ! -- for rank-1 profile input the code won't (hopefully) fall in a heap.
    CHARACTER( * ), OPTIONAL,            INTENT( IN )  :: message_log
    

    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'FORWARD_RTM'

    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Scalars
    CHARACTER( 100 ) :: message
    CHARACTER( 10 )  :: value_in, value_allowed

    INTEGER :: n_layers   ! Layer dimension
    INTEGER :: l          ! Channel loop/index variables
    INTEGER :: i
    INTEGER :: valid_solar

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: is_set

    ! -- Arrays for integrated absorber amounts.
    REAL( fp_kind ), DIMENSION( 0:SIZE( absorber, DIM = 1 )-1, &
                                  SIZE( absorber, DIM = 2 )    ) :: tau_absorber
    REAL( fp_kind ), DIMENSION( n_channels ) :: f0
    REAL( fp_kind ) :: view_angle_in_degree
    REAL( fp_kind ), DIMENSION (SIZE( layer_p) ) :: Ext0, Scat0, g0
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) )  :: surface_emissivity      ! L
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) )  :: surface_reflectivity    ! L

    REAL( fp_kind ) :: junk1
    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC ADJUSTL, &
              ANY,     &
              COS,     &
              MAXVAL,  &
              SIZE,    &
              TRIM

    !#--------------------------------------------------------------------------#
    !#           -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --               #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Check the number of channels - if zero
    ! then simply RETURN.
    ! --------------------------------------

    IF ( n_channels == 0 ) RETURN

  !  surface emissivity
      DO l = 1, n_channels
       f0(l) = SC%Frequency(channel_Index( l ))
      ENDDO

      view_angle_in_degree = ACOS (ONE/secant_view_angle) / DEGREES_TO_RADIANS

     IF( .NOT. PRESENT( surface_emissivity_i ) ) THEN
      call EMISS_MW(n_channels,n_channels,channel_Index,PolStatus,f0, &
      Surface_Type,view_angle_in_degree,Surface_Wind_s, &
      Surface_Temperature,surface_emissivity,surface_reflectivity)

     ELSE
      surface_emissivity = surface_emissivity_i
      surface_reflectivity = surface_reflectivity_i
     END IF
    ! ------------------
    ! Get the dimensions
    ! ------------------

    n_layers = SIZE( layer_p )
    ! -----------------------------------
    ! Perform a simple check on the input
    ! data for negative values
    ! -----------------------------------

    ! -- Profile data
    IF ( ANY( level_p < ZERO ) .OR. &
         ANY( layer_p < ZERO ) .OR. &
         ANY( layer_t < ZERO ) .OR. &
         ANY( layer_w < ZERO ) .OR. &
         ANY( layer_o < ZERO )      ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Negative values found in input profile data.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    ! -- Surface properties
    IF (      surface_temperature  < ZERO   .OR. &
         ANY( surface_emissivity   < ZERO ) .OR. &
         ANY( surface_reflectivity < ZERO )      ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Negative values found in surface properties (Tsfc,esfc,rsfc).', &
                            error_status, &
                            message_log = message_log )
      print *, surface_temperature,surface_emissivity(1:n_channels),surface_reflectivity(1:n_channels)
      RETURN
    END IF

    ! -- Number of channels
    IF ( n_channels < 0 ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Negative number of channels passed..', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#           -- CALCULATE THE PROFILE GENERIC ABSORBER AMOUNTS --           #
    !#--------------------------------------------------------------------------#

    CALL compute_absorber_amount( level_p( : ),     &  ! Input,  K
                                  layer_w( : ),     &  ! Input,  K
                                  layer_o( : ),     &  ! Input,  K

                                  absorber( 0:, : ) )  ! Output, 0:K x J



    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE PREDICTORS FOR THE UPWELLING TRANSMITTANCE --      #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------------
    ! Modify absorber quantities by the angle secant
    ! Could put a loop here but here's hoping the compiler
    ! recognises this as a group of loops over layer.
    ! ------------------------------------------------------

    tau_absorber( 0:, : ) = secant_view_angle * absorber( 0:, : )


    ! -----------------------------------------------------
    ! Calculate the predictors for the satellite view angle
    ! -----------------------------------------------------

    CALL compute_predictors( layer_p( : ),          &  ! Input,  K
                             layer_t( : ),          &  ! Input,  K
                             layer_w( : ),          &  ! Input,  K
                             tau_absorber( 0:, : ), &  ! Input,  0:K x J

                             tau_predictor( :, : )  )  ! Output, I x K

    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE PREDICTORS FOR THE FLUX TRANSMITTANCE --         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Have any INFRARED channels been specified for
    ! the current profile? (Microwave channels are
    ! flagged as == 1, so IR == 0).
    !
    ! For microwave channels the downwelling flux
    ! transmission is assumed == upwelling view
    ! angle transmission.
    ! ---------------------------------------------

    !#--------------------------------------------------------------------------#
    !#                           -- CHANNEL LOOP --                             #
    !#--------------------------------------------------------------------------#


    l_channel_loop: DO l = 1, n_channels

      ! --------------------------------------------------
      ! Calculate the current channel layer transmittances
      ! for the satellite view angle
      ! --------------------------------------------------

      CALL compute_transmittance( tau_absorber( 0:, : ),   &   ! Input, 0:K x J
                                  tau_predictor( :, : ),   &   ! Input, I x K
                                  channel_index( l ),      &   ! Input, scalar
                                  UP,                      &   ! Input, scalar
                                  opt_lay( :, l )          )   ! Output, K
        ! -- This gives the identical result as a separate call
        ! -- but without the extra computational burden.

      ! converting optical depth at nadir
           opt_lay(:, l) = opt_lay( :, l)/secant_view_angle 

      ! ---------------------------------------
      ! Calculate cloud optical properties 
      ! ---------------------------------------

       junk1 = ZERO
      do i = 1, n_Layers
      error_status = mw_compute_cloud_opt(SC%Frequency(channel_Index( l )), &
           layer_t(i), layer_clw(i), &
         layer_rain(i), layer_snow(i), layer_gh(i), Layer_ice(i), Ext0(i), Scat0(i), g0(i))
         if(Scat0(i) > TOLERANCE) then
           g_lay(i,l) = g0(i)/Scat0(i)
         else
           g_lay(i,l) = ZERO
         endif

       junk1 = junk1 + opt_lay(i,l)

         opt_lay(i,l) = opt_lay(i,l) + Ext0(i)
         if( opt_lay(i,l) > TOLERANCE ) then
          alb_lay(i,l) = Scat0(i)/opt_lay(i,l)
         else
          alb_lay(i,l) = ZERO
         endif
      enddo

        junk1 = exp(-junk1)

       !write(6, '(I5, F12.5)') l, junk1
      ! ---------------------------------------
      ! Calculate the profile/channel radiances
      ! ---------------------------------------

      CALL compute_radiance( Surface_Type,               &  ! Surface type
                             secant_view_angle,          &
                             layer_t( : ),               &  ! Input, K
                             surface_temperature,        &  ! Input, scalar
                             surface_emissivity( l ),    &  ! Input, scalar
                             surface_reflectivity( l ),  &  ! Input, scalar
                             opt_lay(      :, l ),           &  ! Input, K
                             alb_lay( :, l ),           &  ! Input, K
                             g_lay( :, l ),   &  ! Input, scalar
                             secant_solar_angle,         &  ! Input, scalar
                             valid_solar,                &  ! Input, scalar
                             channel_index( l ),         &  ! Input, scalar
                             layer_radiance( :, l ),     &  ! Output, K
                             Planck_surface( l ),             &  ! Output, scalar
                             downwelling_radiance( l ),  &  ! Output, scalar
                             upwelling_radiance( l ),    &  ! Output, scalar
                             brightness_temperature( l ) )  ! Output, scalar

    END DO l_channel_loop



    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION forward_rtm

END MODULE forward_model

!
