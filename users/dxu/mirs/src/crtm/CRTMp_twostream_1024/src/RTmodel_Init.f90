!
MODULE RTmodel_Init
  ! ----------
  ! Module use
  ! ----------
  USE Type_Kinds
  USE Error_Handler
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
                                                                                              
  IMPLICIT NONE
                                                                                              
                                                                                              
  ! ------------
  ! Visibilities
  ! ------------
                                                                                              
  ! -- Everything private by default
  PRIVATE

  PUBLIC :: RTmodel_Initial

  TYPE, PUBLIC :: RT_model_type
    ! -- Dimension values
    INTEGER :: n_channels   
    INTEGER :: Max_channels 
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: upwelling_radiance  ! K
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: brightness_temperature  ! K
    INTEGER, DIMENSION( : ), POINTER :: channel_index      ! K
    INTEGER, DIMENSION( : ), POINTER :: PolStatus      ! K
    REAL( fp_kind ) :: secant_view_angle,secant_solar_angle
!
  END TYPE RT_model_type


  CONTAINS


     FUNCTION RTmodel_Initial(   Max_channels,       & !  INPUT, number of channels 
                                RTmodel) RESULT( Error_Status )  ! state variable structure

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  INTEGER, INTENT( IN ) :: Max_channels
  CHARACTER( * ), PARAMETER :: ROUTINE_NAME  = 'RTmodel_Initial'
  INTEGER :: Error_Status
  
    TYPE( RT_model_type ),  INTENT( OUT ) :: RTmodel 
  ! ---------
  ! Variables
  ! ---------
!
   Error_Status = Allocate_State ( Max_channels, RTmodel) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocate from RTmodel structure ', &
                            Error_Status)
    RETURN 
    END IF

  END FUNCTION RTmodel_Initial
                                                     
!
  FUNCTION Allocate_State ( Max_channels,     &      ! INPUT
              RTmodel)  RESULT( Error_Status )   ! Output a stucture
    !
     INTEGER, INTENT( IN ) :: Max_channels 
     CHARACTER( 256 ) :: Message
     INTEGER :: Allocate_Status, Error_Status
    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( RT_model_type ),  INTENT( OUT ) :: RTmodel 

          Error_Status = SUCCESS

    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

        NULLIFY( RTmodel%upwelling_radiance, RTmodel%brightness_temperature,  &
          RTmodel%channel_index, RTmodel%PolStatus)

    IF( ASSOCIATED( RTmodel%channel_index ) ) THEN
        DEALLOCATE( RTmodel%upwelling_radiance, RTmodel%brightness_temperature,  &
          RTmodel%channel_index, RTmodel%PolStatus)

        ALLOCATE(RTmodel%upwelling_radiance(Max_channels), &
                 RTmodel%brightness_temperature(Max_channels), &
                 RTmodel%channel_index(Max_channels), &
                 RTmodel%PolStatus(Max_channels), STAT = Allocate_Status )
    ELSE
        ALLOCATE(RTmodel%upwelling_radiance(Max_channels), &
                 RTmodel%brightness_temperature(Max_channels), &
                 RTmodel%channel_index(Max_channels), &
                 RTmodel%PolStatus(Max_channels), STAT = Allocate_Status )
    END IF

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RTmodel data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
    END IF
!
    RTmodel%n_channels = Max_channels 
    
  END FUNCTION Allocate_State
!
 END MODULE RTmodel_Init 

