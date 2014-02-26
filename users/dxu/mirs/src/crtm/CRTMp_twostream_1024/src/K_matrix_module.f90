!
! --------------------------------------------------------------------------------------------
!     radiative derivatives (K-matrix )
!
!   Developed by
!     Quanhua Liu, Yong Han, and Paul van delst
!     2004-10-26
! --------------------------------------------------------------------------------------------

 MODULE K_matrix_module
!
      USE type_kinds,            ONLY : fp_kind
      USE error_handler
      USE CRTM_Parameters
      USE CRTM_ChannelInfo_Define
  USE radiance, ONLY : compute_radiance, compute_radiance_AD
  USE CRTM_SpcCoeff
  USE mw_cloud_opt
!  USE spectral_coefficients

  USE Surface_Emissivity_Model
  USE CRTM_Fastem3
  USE absorber_profile,      ONLY : compute_absorber_amount, compute_absorber_amount_AD
  USE predictors,            ONLY : compute_predictors, compute_predictors_AD
  USE transmittance,         ONLY : compute_transmittance, compute_transmittance_AD
  USE radiance,              ONLY : compute_radiance
  USE RT_Twostream_Variable

! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE
  ! ------------
  ! Visibilities
  ! ------------

    PUBLIC :: kmatrix

 CONTAINS

      FUNCTION kmatrix(level_p, layer_p, layer_t, layer_w, layer_o,  &  ! Input, K
             layer_clw, layer_rain, layer_snow, layer_gh, layer_ice, &
             surface_temperature,                                   &  ! Input, Scalar
             Surface_Type, &
             Surface_Wind_s, Surface_Wind_d, &
            ! -- Other inputs
             secant_view_angle,                                     &  ! Input, Scalar
             secant_solar_angle,                                    &  ! Input, Scalar
             n_channels,                                            &  ! Input, Scalar
             channel_index,                                         &  ! Input, L
             PolStatus,                                             &  ! Input, L
             upwelling_radiance,                                    &  ! Input, L
             downwelling_radiance,                                  &  ! Input, L
             opt_lay,                                               &
             brightness_temperature,                                &  ! Input, L
             ! -- K-matrix outputs
             level_p_K, layer_p_K, layer_t_K, layer_w_K, layer_o_K, &  ! In/Output, K x L
             layer_clw_K, layer_rain_K, layer_snow_K, layer_gh_K, layer_ice_K, &                                                                                                   
             surface_temperature_K,                                 &  ! In/Output, L
             Surface_Wind_s_K,                                      &
             ChannelInfo, &
             ! -- Optional inputs
             surface_emissivity_i,                                  &  ! Input, L
             surface_reflectivity_i,                                &  ! Input, L
             surface_emissivity_K,                                  &  ! In/Output, L
             surface_reflectivity_K,                                &  ! In/Output, L
             message_log )                                          &
           RESULT ( error_status )

     ! -----------------------
     ! Disable implicit typing
     ! -----------------------
      IMPLICIT NONE
  TYPE( CRTM_ChannelInfo_type ), INTENT( IN )  :: ChannelInfo
    ! -- Forward inputs
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: level_p                    ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_p                    ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_t                    ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_w                    ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_o                    ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_clw                  ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_rain                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_snow                 ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_gh                   ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: layer_ice                  ! K
                                                                                                 
    REAL( fp_kind ),                    INTENT( IN )     :: surface_temperature        ! Scalar
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( INOUT ) :: surface_emissivity_i   ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( INOUT ) :: surface_reflectivity_i ! L
                                                                                                 
    ! -- Other inputs
    REAL( fp_kind ),                    INTENT( IN )     :: secant_view_angle          ! Scalar
    REAL( fp_kind ),                    INTENT( IN )     :: secant_solar_angle         ! Scalar
    INTEGER,                            INTENT( IN )     :: n_channels                 ! Scalar
    INTEGER,         DIMENSION( : ),    INTENT( IN )     :: channel_index              ! L
    INTEGER,         DIMENSION( : ),    INTENT( IN )  :: PolStatus           ! 
    ! -- Forward outputs

    REAL( fp_kind ), DIMENSION( : ),    INTENT( OUT )    :: upwelling_radiance         ! L
    REAL( fp_kind ), DIMENSION( : ),    INTENT( OUT )    :: brightness_temperature     ! L
                                                                                                 
    ! -- K-matrix outputs
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN OUT ) :: level_p_K                  ! K x L
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN OUT ) :: layer_p_K                  ! K x L
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN OUT ) :: layer_t_K                  ! K x L
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN OUT ) :: layer_w_K                  ! K x L
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN OUT ) :: layer_o_K                  ! K x L
    REAL( fp_kind ), DIMENSION( :, : ),    INTENT( IN OUT ) :: layer_clw_K                  ! K
    REAL( fp_kind ), DIMENSION( :, : ),    INTENT( IN OUT ) :: layer_rain_K                 ! K
    REAL( fp_kind ), DIMENSION( :, : ),    INTENT( IN OUT ) :: layer_snow_K                 ! K
    REAL( fp_kind ), DIMENSION( :, : ),    INTENT( IN OUT ) :: layer_gh_K                   ! K
    REAL( fp_kind ), DIMENSION( :, : ),    INTENT( IN OUT ) :: layer_ice_K                  ! K
                                                                                                 
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN OUT ) :: surface_temperature_K      ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),  INTENT( IN OUT ) :: surface_emissivity_K       ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),  INTENT( IN OUT ) :: surface_reflectivity_K     ! L
    REAL( fp_kind ),DIMENSION(SIZE(upwelling_radiance)) :: surface_emissivity_KK, surface_reflectivity_KK  

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Wind_s_K     ! L

    ! -- Optional input. Note that N_INPUT_PROFILES is not used in this
    ! -- function. It is included here so if a user specifies it by mistake
    ! -- for rank-1 profile input the code won't (hopefully) fall in a heap.
    CHARACTER(256), OPTIONAL,           INTENT( IN )     :: message_log

    REAL( fp_kind ), DIMENSION( SIZE(upwelling_radiance) ) :: upwelling_radiance_K       ! L
    REAL( fp_kind ), DIMENSION( SIZE(upwelling_radiance) ) :: brightness_temperature_K   ! L
                                                                                                 
    ! -- Array for integrated absorber amounts, 0:K x J
    REAL( fp_kind ), DIMENSION( 0:SIZE( layer_p, DIM = 1 ), &
                                  MAX_N_ABSORBERS           ) :: absorber
                                                                                                 
    ! -- Arrays for predictors, Imax x K
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( layer_p, DIM = 1 )  ) :: tau_predictor  
                                                                                                 
    ! -- Array for forward and K-matrix layer Planck radiance term, K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: layer_radiance,  &
                                                                layer_radiance_K

    REAL( fp_kind ), DIMENSION( n_channels ) :: f0
    REAL( fp_kind ), DIMENSION (SIZE( layer_p) ) :: Ext0, Scat0, g0
    REAL( fp_kind ), DIMENSION (SIZE( layer_p) ) :: Ext0_AD, Scat0_AD, g0_AD

    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) ) :: Planck_Surface,Planck_Surface_K
                
    ! -- Array for forward and K-matrix downwelling radiance (flux + solar), L
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) ) :: downwelling_radiance,  &
                                                                downwelling_radiance_K

    ! -- Forward outputs
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: opt_lay   ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: alb_lay   ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: g_lay   ! K x L

    ! -- Forward outputs
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: opt_lay_AD   ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: alb_lay_AD   ! K x L
    REAL( fp_kind ), DIMENSION( SIZE( layer_p, DIM = 1 ),  &
                                SIZE( upwelling_radiance ) ) :: g_lay_AD   ! K x L

     ! ---------------
     ! Function result
     ! ---------------
     INTEGER :: error_status
     ! ----------------
     ! Local parameters
     ! ----------------
     CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Kmatrix'
     INTEGER, INTENT( IN )  :: Surface_Type
     REAL( fp_kind ) :: Surface_Wind_s, Surface_Wind_d
     INTEGER ::  n_layers, l, i, j, valid_solar
     REAL( fp_kind ) :: view_angle_in_degree
    ! -- Arrays for integrated absorber amounts.
     REAL( fp_kind ), DIMENSION( 0:SIZE( absorber, DIM = 1 )-1, &
                                  SIZE( absorber, DIM = 2 )    ) :: tau_absorber
     REAL( fp_kind ), DIMENSION( 0:SIZE( absorber, DIM = 1 )-1, &
                    SIZE( absorber, DIM = 2 )    ) :: tau_absorber_K, absorber_K
    ! -- Arrays for predictors, Imax x K
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( layer_p, DIM = 1 )  ) :: tau_predictor_K 
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) )  :: surface_emissivity      ! L
    REAL( fp_kind ), DIMENSION( SIZE( upwelling_radiance ) )  :: surface_reflectivity    ! L



    !print *, '@@@@@@@@@@@@@@@@@@@@'
    !print *, '@1-LevP@',size(level_p),'==',level_p
    !print *, '@1-LayP@',size(layer_p),'==',layer_p
    !print *, '@2-T@',size(layer_t),'==',layer_t
    !print *, '@2-Q@',size(layer_w),'==',layer_w
    !print *, '@2-O3@',size(layer_o),'==',layer_o
    !print *, '@3-clw@',size(layer_clw),'==',layer_clw
    !print *, '@3-rain@',size(layer_rain),'==',layer_rain
    !print *, '@4-sn@',size(layer_snow),'==',layer_snow
    !print *, '@4-gh@',size(layer_gh),'==',layer_gh
    !print *, '@4-ice@',size(layer_ice),'==',layer_ice
    !print *, '@5@',surface_temperature,Surface_Type,Surface_Wind_s, Surface_Wind_d
    !print *, '@6@',secant_view_angle,secant_solar_angle,n_channels,channel_index
    !print *, '@7@',PolStatus,upwelling_radiance
    !print *, '@8@',brightness_temperature
    !print *, '@9-levp-K@',shape(level_p_K),'==',level_p_K
    !print *, '@9-layp-K@',shape(layer_p_K),'==',layer_p_K
    !print *, '@10-layT-K@',shape(layer_t_K),'==',layer_t_K
    !print *, '@10-layQ-K@',shape(layer_w_K),'==',layer_w_K
    !print *, '@11@',shape(layer_o_K),'==',layer_o_K
    !print *, '@12@',shape(layer_clw_K),'==',layer_clw_K
    !print *, '@13@',shape(layer_rain_K),'==',layer_rain_K
    !print *, '@14@',shape(layer_snow_K),'==',layer_snow_K
    !print *, '@15@',shape(layer_gh_K),'==',layer_gh_K
    !print *, '@16@',shape(layer_ice_K),'==',layer_ice_K      
    !print *, '@17@',surface_temperature_K,Surface_Wind_s_K
    !!print *, '@@',ChannelInfo
    !print *, '@18@',shape(surface_emissivity_i),'==',surface_emissivity_i
    !print *, '@19@',shape(surface_reflectivity_i),'==',surface_reflectivity_i
    !print *, '@20@',shape(surface_emissivity_K),'==',surface_emissivity_K
    !print *, '@21@',shape(surface_reflectivity_K),'==',surface_reflectivity_K



      level_p_K(:, :) = ZERO
      layer_p_K(:, :) = ZERO
      layer_t_K(:, :) = ZERO
      layer_w_K(:, :) = ZERO
      layer_o_K(:, :) = ZERO
      layer_clw_K( :,: ) = ZERO
      layer_rain_K( :,: ) = ZERO
      layer_snow_K( :,: ) = ZERO
      layer_gh_K( :,: ) = ZERO
      Layer_ice_K( :,: ) = ZERO
      Surface_Wind_s_K = ZERO
      surface_temperature_K( : ) = ZERO
      downwelling_radiance_K = ZERO
      upwelling_radiance_K = ZERO



      ! ----------------------------------------------------
      ! Set the "this is a channel influenced by solar" flag
      ! ----------------------------------------------------
                                                                                                            
      valid_solar = 0

    ! ----------------------------------------------------------------- !
    !                                                                   !
    !    RT Forward Part
    !                                                                   !
    ! ----------------------------------------------------------------- !

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
       print *,' error '
       print *,minval(level_p),minval(layer_p),minval(layer_w),minval(layer_t),minval(layer_o)
!       pause

      RETURN
    END IF
    ! -- Surface properties
    IF (      surface_temperature  < ZERO   .OR. &
         ANY( surface_emissivity   < ZERO ) .OR. &
         ANY( surface_reflectivity < ZERO )      ) THEN
      error_status = FAILURE
      print *, 'TSKIN:',surface_temperature,' Em/Rf:',minval(surface_emissivity),minloc(surface_emissivity),&
           minval(surface_reflectivity),minloc(surface_reflectivity)
      CALL display_message( ROUTINE_NAME, &
                            'Negative values found in surface properties (Tsfc,esfc,rsfc).', &
                            error_status, &
                            message_log = message_log )
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

      do i = 1, n_Layers
      error_status = mw_compute_cloud_opt(SC%Frequency(channel_Index( l )), &
           layer_t(i), layer_clw(i), &
         layer_rain(i), layer_snow(i), layer_gh(i), Layer_ice(i), Ext0(i), Scat0(i), g0(i))

         opt_lay(i,l) = opt_lay(i,l) + Ext0(i)
         if(Scat0(i) > TOLERANCE .and. opt_lay(i,l) > TOLERANCE) then
           g_lay(i,l) = g0(i)/Scat0(i)
          alb_lay(i,l) = Scat0(i)/opt_lay(i,l)
         else
           g_lay(i,l) = ZERO
          alb_lay(i,l) = ZERO
         endif

      enddo
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

    ! ----------------------------------------------------------------- !
    !                                                                   !
    !    RT Adjoint Part
    !                                                                   !
    ! ----------------------------------------------------------------- !
         !print *,l,brightness_temperature( l )

      ! -- Absorber arrays, 0:K x J
      absorber_K( 0:, : )           = ZERO
      tau_absorber_K( 0:, : )       = ZERO
                                                                                                            
      ! -- Predictor arrays, Imax x K
      tau_predictor_K( :, : )       = ZERO
         brightness_temperature_K( l ) = ONE

      ! ------------------------------------------------
      ! Calculate the adjoint of the current channel TOA
      ! radiance or brightness temperature
      ! ------------------------------------------------
      CALL compute_radiance_AD(Surface_Type,                   &  ! Surface type
                                secant_view_angle,             & 
                                ! -- Forward input
                                layer_t( : ),                  &  ! Input, K
                                surface_temperature,           &  ! Input, scalar
                                surface_emissivity( l ),       &  ! Input, scalar
                                surface_reflectivity( l ),     &  ! Input, scalar
                                opt_lay(      :, l ),          &  ! Input, K
                                    alb_lay( :, l ),           &  ! Input, K
                                      g_lay( :, l ),           &  ! Input, scalar
                                layer_radiance( :, l ),        &  ! Input, K
                                Planck_surface( l ),           &  ! Input, scalar
                                downwelling_radiance( l ),     &  ! Input, scalar
                                upwelling_radiance( l ),       &  ! Input, scalar
                                brightness_temperature( l ),   &  ! Input, scalar
                                ! -- K-matrix input
                                layer_radiance_K( :, l ),      &  ! Output, K
                                Planck_Surface_K( l ),         &  ! Output
                                downwelling_radiance_K( l ),   &  ! Output, scalar
                                upwelling_radiance_K( l ),     &  ! Output, scalar
                                brightness_temperature_K( l ), &  ! In/Output, scalar
                                ! -- Other input
                                secant_solar_angle,            &  ! Input, scalar
                                valid_solar,                   &  ! Input, scalar
                                channel_index( l ),            &  ! Input, scalar
                                ! -- K-matrix output
                                layer_t_K( :, l ),             &  ! Output, K
                                surface_temperature_K( l ),    &  ! Output, scalar
                                surface_emissivity_KK( l ),     &  ! Output, scalar
                                surface_reflectivity_KK( l ),   &  ! Output, scalar
                                opt_lay_AD(      :, l ),       &   ! Output, K
                                    alb_lay_AD( :, l ),        &   ! OutInput, K
                                      g_lay_AD( :, l ) )           ! Output, scalar

      !
      ! ---------------------------------------
      ! Calculate cloud optical properties
      ! ---------------------------------------

         Scat0_AD( : ) = ZERO
         Ext0_AD( : ) = ZERO
         g0_AD( : ) = ZERO

      do i =  n_Layers, 1, -1

         if( Scat0(i) > TOLERANCE .and. opt_lay(i,l) > TOLERANCE ) then
          opt_lay_AD(i,l) = opt_lay_AD(i,l) -alb_lay(i,l)/opt_lay(i,l) *  alb_lay_AD(i,l) 
          Scat0_AD(i) = alb_lay_AD(i,l)/opt_lay(i,l)
           Scat0_AD(i) = Scat0_AD(i)-g_lay(i,l)/Scat0(i) * g_lay_AD(i,l)
           g0_AD(i) = g_lay_AD(i,l)/Scat0(i)
         else
          alb_lay_AD(i,l) = ZERO
           g_lay_AD(i,l) = ZERO
         endif

         Ext0_AD(i) = opt_lay_AD(i,l)

      error_status = mw_compute_cloud_opt_AD(SC%Frequency(channel_Index( l )), &
          layer_t(i), layer_clw(i), layer_rain(i), layer_snow(i), layer_gh(i), Layer_ice(i), &
          Ext0(i), Scat0(i), g0(i),  Ext0_AD(i), Scat0_AD(i), g0_AD(i), &
      layer_t_K(i,l),layer_clw_K(i,l),layer_rain_K(i,l),layer_snow_K(i,l),layer_gh_K(i,l),Layer_ice_K(i,l) )

      end do 

      ! converting optical depth at nadir
           opt_lay_AD(:, l) = opt_lay_AD( :, l)/secant_view_angle
                                                                                                           
      ! -----------------------------------------------------
      ! Calculate the adjoint of the upwelling transmittances
      ! for the satellite view angle
      ! -----------------------------------------------------
                                                                                                            
      CALL compute_transmittance_AD( &
                                     ! -- Forward input
                                     tau_absorber( 0:, : ),   &   ! Input, 0:K x J
                                     tau_predictor( :, : ),   &   ! Input, I x K
                                     opt_lay( :, l ),             &   ! Input, K
                                                                                                            
                                     ! -- K-matrix input
                                     opt_lay_AD( :, l ),           &   ! In/Output, K
                                                                                                            
                                     ! -- Other input
                                     channel_index( l ),      &   ! Input, scalar
                                     UP,                      &   ! Input, scalar
                                                                                                            
                                     ! -- K-matrix output
                                     tau_absorber_K( 0:, : ), &   ! In/Output, 0:K x J
                                     tau_predictor_K( :, : )  )   ! In/Output, I x K
      ! --------------------------------------
      ! K-matrix adjoint of all the predictors
      ! --------------------------------------
                                                                                                            
      CALL compute_predictors_AD( &
                                  ! -- Forward input
                                  layer_p( : ),            &  ! Input,  K
                                  layer_t( : ),            &  ! Input,  K
                                  layer_w( : ),            &  ! Input,  K
                                  tau_absorber( 0:, : ),   &  ! Input,  0:K x J
                                                                                                            
                                  ! -- K-matrix input
                                  tau_predictor_K( :, : ), &  ! In/Output, I x K
                                                                                                            
                                  ! -- K-matrix output
                                  layer_p_K( :, l ),       &  ! In/Output,  K
                                  layer_t_K( :, l ),       &  ! In/Output,  K
                                  layer_w_K( :, l ),       &  ! In/Output,  K
                                  tau_absorber_K( 0:, : )  )  ! In/Output,  0:K x J
                                                                                                            
     ! ----------------------------------------------------------
      ! K-matrix adjoint of the nadir absorber amount modification
      ! ----------------------------------------------------------

      absorber_K( 0:, : ) = absorber_K( 0:, : ) + &
                               ( secant_view_angle * tau_absorber_K( 0:, : ) )
           
      !#------------------------------------------------------------------------#
      !#             -- CALCULATE THE ABSORBER K-MATRIX ADJOINTS --             #
      !#------------------------------------------------------------------------#
                                                                                                            
      CALL compute_absorber_amount_AD( &
                                       ! -- Forward input
                                       level_p( : ),        &  ! Input,  K
                                       layer_w( : ),        &  ! Input,  K
                                       layer_o( : ),        &  ! Input,  K
                                                                                                            
                                       ! -- K-matrix input
                                       absorber_K( 0:, : ), &  ! In/Output, 0:K x J
                                                                                                            
                                       ! -- K-matrix output
                                       level_p_K( :, l ),   &  ! In/Ouput,  K
                                       layer_w_K( :, l ),   &  ! In/Ouput,  K
                                       layer_o_K( :, l )    )  ! In/Ouput,  K

    END DO l_channel_loop


    IF( .NOT. PRESENT( surface_emissivity_i ) ) THEN
       call EMISS_MW_AD(n_channels,n_channels,channel_Index,PolStatus,f0, &
            Surface_Type,view_angle_in_degree,Surface_Wind_s, &
            Surface_Temperature,surface_emissivity,surface_reflectivity, &
            surface_emissivity_KK,Surface_Wind_s_K,surface_temperature_K)
    ELSE
       surface_emissivity_K = surface_emissivity_KK
       surface_reflectivity_K = surface_reflectivity_KK
       !---test
       IF (Surface_Type .ge. 1) THEN 
          call EMISS_MW_AD(n_channels,n_channels,channel_Index,PolStatus,f0, &
               Surface_Type,view_angle_in_degree,Surface_Wind_s, &
               Surface_Temperature,surface_emissivity,surface_reflectivity, &
               surface_emissivity_K,Surface_Wind_s_K,surface_temperature_K)
       ENDIF
    ENDIF
    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#
                                                                                                                              
    error_status = SUCCESS
        



    !print *, 'TB:',brightness_temperature(1:n_channels)
    !print *, 'JacEmiss:',surface_emissivity_K
    !print *, 'JacRefl:',surface_reflectivity_K 
    !print *, 'JacSfc:',surface_temperature_K
    !do i=1,size(layer_p)
    !   !write(*,'(a3,1x,i4,5f12.6)') '=JinCRTM=',i,layer_clw_K(i,1:n_channels)
    !   print *, '=JinCRTM=',i,layer_clw_K(i,1:n_channels)
    !   do j=1,n_channels
    !      print *,j,layer_clw_K(i,j)
    !   enddo
    !enddo
    !stop

   END FUNCTION kmatrix

 END MODULE K_matrix_module
