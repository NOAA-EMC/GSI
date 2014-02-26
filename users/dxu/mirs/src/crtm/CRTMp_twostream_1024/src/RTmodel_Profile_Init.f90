!
MODULE RTmodel_Profile_Init
  ! ----------
  ! Module use
  ! ----------
  USE Type_Kinds
  USE Error_Handler
  USE CRTM_Parameters
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
                                                                                              
  IMPLICIT NONE
                                                                                              
                                                                                              
  ! ------------
  ! Visibilities
  ! ------------
                                                                                              
  ! -- Everything private by default
  PRIVATE

  PUBLIC :: RTmodel_Profile_Initial

  TYPE, PUBLIC :: Kmatrix_type
    ! -- Dimension values
    INTEGER :: n_Layers     ! K dimension
    INTEGER :: n_channels
    INTEGER :: Max_channels 
    ! -- Profile LEVEL pressure and LAYER quantities
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: level_p    
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_p   
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_t  
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_w    
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_o3  
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_clw 
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_rain 
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_snow 
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_gh  
    REAL( fp_kind ), POINTER, DIMENSION( :,: ) :: layer_ice 
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: surface_temperature    
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Surface_Wind_s    
!
  END TYPE Kmatrix_type 


  CONTAINS


     FUNCTION RTmodel_Profile_Initial(   n_Layers,       & !  INPUT, number of atmos. layers 
                                     Max_channels,       & !  INPUT, number of channels
                                Start_State) RESULT( Error_Status )  ! state variable structure

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  INTEGER, INTENT( IN ) :: n_Layers, Max_channels
  CHARACTER( * ), PARAMETER :: ROUTINE_NAME  = 'Profile_Initial'
  INTEGER :: Error_Status,FileID, i, j, nl
  
    TYPE( Kmatrix_type ),  INTENT( OUT ) :: Start_State 
  ! ---------
  ! Variables
  ! ---------
!
  !  define a structure for Start point or first guess 
   Error_Status = Allocate_State ( n_Layers, Max_channels, Start_State) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocate from Start_State structure ', &
                            Error_Status)
    RETURN 
    END IF

  END FUNCTION RTmodel_Profile_Initial
                                                     
!
  FUNCTION Allocate_State ( n_Layers,     &      ! INPUT
                       M, &
              Background)  RESULT( Error_Status )   ! Output a stucture
    !
     INTEGER, INTENT( IN ) :: n_Layers, M
     CHARACTER( 256 ) :: Message
     INTEGER :: Allocate_Status, Error_Status
    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( Kmatrix_type ),  INTENT( OUT ) :: Background

          Error_Status = SUCCESS

    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

        NULLIFY( Background%level_p, Background%layer_p,  &
          Background%layer_t, Background%layer_w, &
          Background%layer_o3, Background%layer_clw, &
          Background%layer_snow, Background%layer_gh, &
          Background%layer_ice, Background%layer_rain, &
          Background%surface_temperature, Background%Surface_Wind_s )

    IF( ASSOCIATED( Background%level_p ) ) THEN
        DEALLOCATE( Background%level_p, Background%layer_p,  &
          Background%layer_t, Background%layer_w, &
          Background%layer_o3, Background%layer_clw, &
          Background%layer_snow, Background%layer_gh, &
          Background%layer_ice, Background%layer_rain, &
          Background%surface_temperature, Background%Surface_Wind_s )

        ALLOCATE(Background%level_p(n_Layers,M), Background%layer_p(n_Layers,M),  &
          Background%layer_t(n_Layers,M), Background%layer_w(n_Layers,M),         &
          Background%layer_o3(n_Layers,M), Background%layer_clw(n_Layers,M),       &
          Background%layer_snow(n_Layers,M), Background%layer_gh(n_Layers,M),     &
          Background%layer_ice(n_Layers,M), Background%layer_rain(n_Layers,M),    &
          Background%surface_temperature(M), Background%Surface_Wind_s(M), &
          STAT = Allocate_Status )

    ELSE
        ALLOCATE(Background%level_p(n_Layers,M), Background%layer_p(n_Layers,M),  &
          Background%layer_t(n_Layers,M), Background%layer_w(n_Layers,M),         &
          Background%layer_o3(n_Layers,M), Background%layer_clw(n_Layers,M),       &
          Background%layer_snow(n_Layers,M), Background%layer_gh(n_Layers,M),     &
          Background%layer_ice(n_Layers,M), Background%layer_rain(n_Layers,M),    &
          Background%surface_temperature(M), Background%Surface_Wind_s(M), &
          STAT = Allocate_Status )
    END IF

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Background data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
    END IF
!
    Background%n_Layers = n_Layers
    Background%n_channels = M 
  END FUNCTION Allocate_State
!
 END MODULE RTmodel_Profile_Init 

