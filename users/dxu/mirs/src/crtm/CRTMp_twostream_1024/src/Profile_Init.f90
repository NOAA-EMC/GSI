!
MODULE Profile_Init
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

  PUBLIC :: Profile_Initial

  TYPE, PUBLIC :: Atmosphere_Surface_State_type
    CHARACTER( 256 ) :: BGtype
    ! -- Dimension values
    INTEGER :: n_Layers     ! K dimension
    INTEGER :: Surface_Type 
    ! -- Profile LEVEL pressure and LAYER quantities
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: level_p        ! K+1
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_p        ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_t        ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_w        ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_o3       ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_clw      ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_rain     ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_snow     ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_gh       ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: layer_ice      ! K
    REAL( fp_kind ) :: Air_Temperature, Specific_Humidity
    REAL( fp_kind ) ::  Surface_Wind_s, Surface_Wind_d, surface_temperature           
    REAL( fp_kind ) ::  Surface_pressure, Surface_rainrate, total_clw
    REAL( fp_kind ) ::  Ice_Content, Rain_Water           
!
  END TYPE Atmosphere_Surface_State_type 


  CONTAINS


     FUNCTION Profile_Initial(   n_Layers,       & !  INPUT, number of atmos. layers 
                                Start_State) RESULT( Error_Status )  ! state variable structure

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  INTEGER, INTENT( IN ) :: n_Layers
  CHARACTER( * ), PARAMETER :: ROUTINE_NAME  = 'Profile_Initial'
  INTEGER :: Error_Status,FileID, i, j, nl
  
    TYPE( Atmosphere_Surface_State_type ),  INTENT( OUT ) :: Start_State 
  ! ---------
  ! Variables
  ! ---------
!
  !  define a structure for Start point or first guess 
   Error_Status = Allocate_State ( n_Layers, Start_State) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocate from Start_State structure ', &
                            Error_Status)
    RETURN 
    END IF

  END FUNCTION Profile_Initial
                                                     
!
  FUNCTION Allocate_State ( n_Layers,     &      ! INPUT
              Background)  RESULT( Error_Status )   ! Output a stucture
    !
     INTEGER, INTENT( IN ) :: n_Layers
     CHARACTER( 256 ) :: Message
     INTEGER :: Allocate_Status, Error_Status
    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( Atmosphere_Surface_State_type ),  INTENT( OUT ) :: Background

          Error_Status = SUCCESS

    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

        NULLIFY( Background%level_p, Background%layer_p,  &
          Background%layer_t, Background%layer_w, &
          Background%layer_o3, Background%layer_clw, &
          Background%layer_snow, Background%layer_gh, &
          Background%layer_ice, Background%layer_rain)

    IF( ASSOCIATED( Background%level_p ) ) THEN
        DEALLOCATE( Background%level_p, Background%layer_p,  &
          Background%layer_t, Background%layer_w, &
          Background%layer_o3, Background%layer_clw, &
          Background%layer_snow, Background%layer_gh, &
          Background%layer_ice, Background%layer_rain)

        ALLOCATE(Background%level_p(n_Layers), Background%layer_p(n_Layers),  &
          Background%layer_t(n_Layers), Background%layer_w(n_Layers),         &
          Background%layer_o3(n_Layers), Background%layer_clw(n_Layers),       &
          Background%layer_snow(n_Layers), Background%layer_gh(n_Layers),     &
          Background%layer_ice(n_Layers), Background%layer_rain(n_Layers),    &
          STAT = Allocate_Status )

    ELSE
        ALLOCATE(Background%level_p(n_Layers), Background%layer_p(n_Layers),  &
          Background%layer_t(n_Layers), Background%layer_w(n_Layers),         &
          Background%layer_o3(n_Layers), Background%layer_clw(n_Layers),       &
          Background%layer_snow(n_Layers), Background%layer_gh(n_Layers),     &
          Background%layer_ice(n_Layers), Background%layer_rain(n_Layers),    &
          STAT = Allocate_Status )
    END IF

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Background data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
    END IF
!
    Background%n_Layers = n_Layers
    
  END FUNCTION Allocate_State
!
 END MODULE Profile_Init 

