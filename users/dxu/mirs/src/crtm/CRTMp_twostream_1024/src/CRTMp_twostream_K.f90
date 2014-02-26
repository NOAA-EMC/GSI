!
        Program CRTMp_twostream 
!------------------------------------------------------------------------------
!P+
! NAME:
!       CRTMp_twostream 
!
!    This CRTMp_twostream is currently used inside NESDIS. Official CRTM
!    will be released in future.
!
! 
!    Functions: The code can be used to compute current/histotical
!               satellite measurements, such as AMSU, SSMIS, ....
!
!    Methodology: forward and adjoint
!                 emission/scattering radiative transfer model based on
!                 an improved two-stream approximation, ocean and land
!                 surface emissivity model.
!
! MODULES:
!
!    CRTM_Module:     Community Radiative Transfer Model module (NESDIS VERSION).
!
! -- Utility modules
!    Type_Kinds:      pre-defined abbreviation for data type (e.g. fp_kind -> REAL*8 ) 
!    Error_Handler
!
! CONTAINS:
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!
! CREATION HISTORY:
!
!    Version         beta test version 0       September 8, 2003 
!                       Quanhua Liu,    Quanhua.Liu@noaa.gov
!                       Fuzhong Weng,   Fuzhong.Weng@noaa.gov
!
!    Version         beta test version 1        October 26, 2004 
!
!       Written by:     Quanhua Liu,    Quanhua.Liu@noaa.gov
!                       Yong Han,       Yong.Han@noaa.gov
!                       Paul van Delst  paul.vandelst@ssec.wisc.edu
!                       Fuzhong Weng,   Fuzhong.Weng@noaa.gov
!
!  Copyright (C) 2005
!
!  Reference:
!
!    Liu, Q. and F. Weng, 2002: A microwave polarimetric two-stream radiative 
!         transfer model, J. Atmos. Sci., 59, pp. 2396-2402, 2002.
!
!    Weng, F., B. Yan, and N. C. Grody, 2001: A microwave land emissivity model, 
!         J. Geophys. Res., 106, 20115- 20123.
!
!    English S. J., 1999: A fast generic millimetre-wave ocean emissivity model. Tech. Proc. 
!         10th International TOVS Study Conference,? Eds. J. Le Marshall and J.D. Jasper,
!         Published by Bureau of Meteorology Research Centre, pp. 178-183.
!
!    McMillin, L., L. J. Crone, M. D. Goldberg, and T. J. Kleespies, 1995: Atmospheric 
!         transmittance of an absorbing gas. 4. OPTRAN: a computationally fast and 
!         accurate transmittance model for absorbing gases with fixed and variable
!         mixing ratios at variable viewing angles, Appl. Opt., 34, pp. 6269-6274.
!
! ------------------------------------------------------------------------------
!        
   ! -- CRTM module

      USE CRTM_Module
        ! -- Utility modules
      USE Type_Kinds
      USE Error_Handler
      USE Binary_File_Utility
      USE Profile_Init
      USE RTmodel_Init
      USE forward_model
      USE CRTM_SpcCoeff
!
      IMPLICIT NONE
!
     ! ---------
     ! Variables
     ! ---------
     CHARACTER( 256 ) :: SpcCoeff_File
     CHARACTER( 256 ) :: TauCoeff_File
     CHARACTER( 256 ) :: ScatterCoeff_File
     CHARACTER( 256 ) :: Profile_Input 
     CHARACTER( 256 ) :: ROUTINE_NAME = ' CRTMp_twostream '
     INTEGER :: Error_Status, number_of_sensor
     INTEGER :: i, n_Layers, FileID
     INTEGER :: ssmis_polstatus(24), amsu_polstatus(20)
     DATA ssmis_polstatus/7*4, 5*5, 2*4, 5, 4, 5, 4, 6*1/
     DATA amsu_polstatus/4*2, 2*3, 2, 7*3, 6*2/ 

    TYPE( CRTM_ChannelInfo_type )  :: ChannelInfo
    TYPE( Atmosphere_Surface_State_type ) :: Start_State 
    TYPE( RT_model_type ) :: RTmodel

      REAL( fp_kind ), DIMENSION( 40,20 ) :: level_p_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_p_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_t_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_w_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_o_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_clw_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_rain_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_snow_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_gh_K
      REAL( fp_kind ), DIMENSION( 40,20 ) :: layer_ice_K

      REAL( fp_kind ), DIMENSION( 20 )    :: surface_temperature_K
      REAL( fp_kind ), DIMENSION( 20 ):: Surface_Wind_s_K

!
      call crtm_init_channelinfo( channelinfo )
!
      !#----------------------------------------------------------------------------#
      !#                     -- GET THE COEFFICIENT FILENAMES --                    #
      !#----------------------------------------------------------------------------#
      WRITE( *, FMT     = '( /5x, "Enter the SpcCoeff filename: " )', &
                ADVANCE = 'NO' )
      READ( *, '( a )' ) SpcCoeff_File
      SpcCoeff_File = ADJUSTL( SpcCoeff_File )
      WRITE( *, FMT     = '(  5x, "Enter the TauCoeff filename: " )', &
                ADVANCE = 'NO' )
      READ( *, '( a )' ) TauCoeff_File
      TauCoeff_File = ADJUSTL( TauCoeff_File )
      WRITE( *, FMT     = '( /5x, "Enter the ScatterCoeff filename: " )', &
                ADVANCE = 'NO' )
      READ( *, '( a )' ) ScatterCoeff_File
      ScatterCoeff_File = ADJUSTL( ScatterCoeff_File )
      WRITE( *, FMT     = '( /5x, "Enter the profile filename: " )', &
                ADVANCE = 'NO' )
      READ( *, '( a )' ) Profile_Input 
      Profile_Input = ADJUSTL( Profile_Input )

      !#----------------------------------------------------------------------------#
      !#                     -- Inilization for RT model  --                        #
      !#----------------------------------------------------------------------------#
      !

      Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File, &
                            TauCoeff_File     = TauCoeff_File, &
                            ScatterCoeff_File = ScatterCoeff_File )
      IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( ROUTINE_NAME, &
                           'Error initializing CRTM', &
                            Error_Status)
       STOP
      END IF

!
     !#----------------------------------------------------------------------------#
     !#                          -- PRINT OUT INSTRUMENTAL INFO --                 #
     !#----------------------------------------------------------------------------#
      CALL Print_ChannelInfo( ChannelInfo )
      Error_Status = RTmodel_Initial(ChannelInfo%n_Channels, RTmodel)
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                            'Error in RTmodel initial ', &
                            Error_Status )
      Error_Status = FAILURE
      STOP 
      END IF
!  secant of SSMIS angle at 55 degree
             RTmodel%secant_view_angle = 1.74344671  
             RTmodel%secant_solar_angle = 0.0 
             RTmodel%channel_index = ChannelInfo%Channel_Index
      IF(ChannelInfo%n_Channels == 24 ) THEN
!  SSMIS
      RTmodel%PolStatus = ssmis_polstatus
      ELSE IF(ChannelInfo%n_Channels == 20 ) THEN
!  AMSU A+B
      RTmodel%PolStatus = amsu_polstatus
      ELSE
! User must provide Polstatus
      print *,' You have to provide Polstatus '
      STOP
      ENDIF 
!
!     READ Profile
      Error_Status = Open_Text_File( TRIM( Profile_Input ), &
                                     FileID )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Profile_Input ), &
                            Error_Status )
      Error_Status = FAILURE
      STOP 
      END IF
      READ(FileID, *) n_Layers
      write(*, *) n_Layers
      READ(FileID, *) Start_State%Surface_Type,Start_State%Surface_Wind_s, &
          Start_State%Surface_Temperature,Start_State%Surface_Pressure
      write(*, *) Start_State%Surface_Type,Start_State%Surface_Wind_s, &
          Start_State%Surface_Temperature,Start_State%Surface_Pressure
!
      !#----------------------------------------------------------------------------#
      !#                     -- Allocate Arrays for profile  --                     #
      !#----------------------------------------------------------------------------#

       Error_Status = Profile_Initial( n_Layers, Start_State) 
       IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( ROUTINE_NAME,'Error in Profile_Init ', &
                            Error_Status )
       STOP
       END IF

      DO i = 1, n_Layers
      READ(FileID, *) Start_State%level_p(i),Start_State%layer_p(i), &
                      Start_State%layer_t(i),Start_State%layer_w(i), &
                      Start_State%layer_clw(i),Start_State%layer_rain(i), &
                      Start_State%layer_snow(i),Start_State%layer_ice(i), &
                      Start_State%layer_gh(i)
      write(*,*) Start_State%level_p(i),Start_State%layer_p(i), &
                      Start_State%layer_t(i),Start_State%layer_w(i), &
                      Start_State%layer_clw(i),Start_State%layer_rain(i), &
                      Start_State%layer_snow(i),Start_State%layer_ice(i), &
                      Start_State%layer_gh(i)
      ENDDO
!
!   calling forward model
! ozone is not used here.
     Start_State%layer_o3 = 0.0
!    Error_Status = compute_rtm( &
     Error_Status = kmatrix( &
             ! -- Forward inputs
             Start_State%level_p, Start_State%layer_p, Start_State%layer_t, &
             Start_State%layer_w, Start_State%layer_o3, & 
             Start_State%layer_clw, Start_State%layer_rain, &
             Start_State%layer_snow, Start_State%layer_gh, Start_State%layer_ice, &
             Start_State%surface_temperature, Start_State%Surface_Type, &
             Start_State%Surface_Wind_s, Start_State%Surface_Wind_d, &
             ! -- Other inputs
             RTmodel%secant_view_angle,                           &  
             RTmodel%secant_solar_angle,                          & 
             RTmodel%n_channels,                                  & 
             RTmodel%channel_index,                               & 
             RTmodel%PolStatus,                                   & 
             RTmodel%upwelling_radiance,                          & 
             RTmodel%brightness_temperature,                      &
             level_p_K, layer_p_K, layer_t_K, layer_w_K, layer_o_K, &  ! In/Output, K x L
             layer_clw_K, layer_rain_K, layer_snow_K, layer_gh_K, layer_ice_K, &
             surface_temperature_K,                                 &  ! In/Output, L
             Surface_Wind_s_K,                                      &
             ChannelInfo)

!
      write(6,511)
 511  format(1x,' ch. index    frequency     BT ')
      DO i = 1, RTmodel%n_channels
      write(6,'(I10,2f12.3)') RTmodel%channel_index(i), &
      SC%Frequency(RTmodel%channel_Index( i )), &
      RTmodel%brightness_temperature(i)
      ENDDO

      CLOSE(FileID)
!
     !#----------------------------------------------------------------------------#
     !#                     -- INITIALISE THE RETRIEVAL --                         #
     !#     READ a fixed background. It may be replaced by desired data.           #
     !#----------------------------------------------------------------------------#

!
   CONTAINS

  SUBROUTINE Print_ChannelInfo( ChannelInfo )
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN ) :: ChannelInfo
    INTEGER :: l
                                                                                                  
    WRITE( *, '( /5x, "Number of channels indexed: ", i5 )' ) ChannelInfo%n_Channels
    WRITE( *, '(  /2x, "Channel         Sensor             NCEP          WMO           WMO     Channel", &
                 &/2x, " Index        Descriptor         Sensor ID   Satellite ID   Sensor ID   Number", &
                 &/2x, "------------------------------------------------------------------------------" )' )
    DO l = 1, ChannelInfo%n_Channels
      WRITE( *, '( 2x, 2x, i4, 2x, ">", a, "<", 5x, i3, 11x, i3, 11x, i3, 7x, i4 )' ) &
                ChannelInfo%Channel_Index( l ), &
                ChannelInfo%Sensor_Descriptor( l ), &
                ChannelInfo%NCEP_Sensor_ID( l ), &
                ChannelInfo%WMO_Satellite_ID( l ), &
                ChannelInfo%WMO_Sensor_ID( l ), &
                ChannelInfo%Sensor_Channel( l )
    END DO
                                                                                                  
  END SUBROUTINE Print_ChannelInfo

  END Program CRTMp_twostream 
!
