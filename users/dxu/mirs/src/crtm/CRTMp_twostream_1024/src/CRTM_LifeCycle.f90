!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_LifeCycle
!
! PURPOSE:
!       Module containing CRTM life cycle functions to initialize and destroy
!       the CRTM space.
!
! CATEGORY:
!       CRTM
!
! CALLING SEQUENCE:
!       USE CRTM_LifeCycle
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Error_Handler:     Module to define error codes and handle
!                          error conditions.
!                          USEs: FILE_UTILITY module
!
!       CRTM_SpcCoeff:     Module containing the shared CRTM spectral
!                          coefficients (SpcCoeff) and their load/destruction
!                          routines.
!                          USEs: TYPE_KINDS module
!                                ERROR_HANDLER module
!                                SPCCOEFF_DEFINE module
!                                SPCCOEFF_BINARY_IO module
!                                CRTM_PARAMETERS module
!
!       CRTM_TauCoeff:     Module containing the shared CRTM gas absorption
!                          coefficients (TauCoeff) and their load/destruction
!                          routines.
!                          USEs: TYPE_KINDS module
!                                ERROR_HANDLER module
!                                TAUCOEFF_DEFINE module
!                                TAUCOEFF_BINARY_IO module
!                                CRTM_PARAMETERS module
!
!       CRTM_ScatterCoeff: Module containing the shared CRTM scattering
!                          coefficients (ScatterCoeff) and their load/destruction
!                          routines.
!                          USEs: TYPE_KINDS module
!                                ERROR_HANDLER module
!                                SCATTERCOEFF_DEFINE module
!                                SCATTERCOEFF_BINARY_IO module
!                                CRTM_PARAMETERS module
!
!
! CONTAINS:
!       CRTM_Init:         Function to initialise the CRTM.
!
!       CRTM_Destroy:      Function to destroy the CRTM space.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       Various shared data structures are allocated and filled with data
!       from file.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

MODULE CRTM_LifeCycle


  ! ----------
  ! Module use
  ! ----------

  USE Error_handler

  USE CRTM_SpcCoeff
  USE CRTM_TauCoeff

  USE CRTM_ChannelInfo_Define
  USE CRTM_ChannelInfo


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Init
  PUBLIC :: CRTM_Destroy


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_LifeCycle.f90,v 1.5 2004/07/01 20:52:51 paulv Exp $'


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Init
!
! PURPOSE:
!       Function to initialise the CRTM.
!
! CATEGORY:
!       CRTM
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Init( ChannelInfo,                           &  ! Output
!                                 SpcCoeff_File     = SpcCoeff_File,     &  ! Optional input
!                                 TauCoeff_File     = TauCoeff_File,     &  ! Optional input
!                                 ScatterCoeff_File = ScatterCoeff_File, &  ! Optional input
!                                 File_Path         = File_Path,         &  ! Optional input
!                                 Quiet             = Quiet,             &  ! Optional input
!                                 Process_ID        = Process_ID,        &  ! Optional input
!                                 Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                 RCS_Id            = RCS_Id,            &  ! Revision control
!                                 Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       SpcCoeff_File:      Name of the CRTM Binary format SpcCoeff file
!                           containing the spectral coefficient data. If not
!                           specified, "SpcCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       TauCoeff_File:      Name of the CRTM Binary format TauCoeff file
!                           containing the gas absorption coefficient data. If not
!                           specified, "TauCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       ScatterCoeff_File:  Name of the CRTM Binary format ScatterCoeff file
!                           containing the scattering coefficient data. If not
!                           specified, "ScatterCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       File_Path:          Character string specifying a file path for the
!                           input data files. If not specified, the current
!                           directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to the screen.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the CRTM initialisation was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
!       CRTM_Load_SpcCoeff:      Function to load the SpcCoeff spectral
!                                coefficient data into the public data
!                                structure SC. 
!                                SOURCE: CRTM_SPCCOEFF module
!
!       CRTM_Load_TauCoeff:      Function to load the TauCoeff gas absorption
!                                coefficient data into the public data
!                                structure TC. 
!                                SOURCE: CRTM_TAUCOEFF module
!
!       CRTM_Load_ScatterCoeff:  Function to load the ScatterCoeff spectral
!                                coefficient data into the public data
!                                structure SC. 
!                                SOURCE: CRTM_SCATTERCOEFF module
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       All public data arrays accessed by this module and its dependencies
!       are overwritten.
!
! RESTRICTIONS:
!       If specified, the length of the combined file path and filename strings
!       cannot exceed 512 characters.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Init( ChannelInfo,       &  ! Output

                      SpcCoeff_File,     &  ! Optional input
                      TauCoeff_file,     &  ! Optional input
                      ScatterCoeff_File, &  ! Optional input
                      File_Path,         &  ! Optional input

                      Sensor_Descriptor, &  ! Optional input
                      NCEP_Sensor_ID,    &  ! Optional input
                      Sensor_Channel,    &  ! Optional input

                      Quiet,             &  ! Optional input
                      Process_ID,        &  ! Optional input
                      Output_Process_ID, &  ! Optional input

                      RCS_Id,            &  ! Revision control
                      Message_Log )      &  ! Error messaging
                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------
      USE mw_cloud_opt

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),            INTENT( OUT ) :: ChannelInfo

    ! -- Optional input
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: SpcCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: TauCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: ScatterCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: File_Path

    CHARACTER( * ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: Sensor_Descriptor
    INTEGER,        OPTIONAL, DIMENSION( : ), INTENT( IN )  :: NCEP_Sensor_ID
    INTEGER,        OPTIONAL, DIMENSION( : ), INTENT( IN )  :: Sensor_Channel

    INTEGER,        OPTIONAL,                 INTENT( IN )  :: Quiet
    INTEGER,        OPTIONAL,                 INTENT( IN )  :: Process_ID
    INTEGER,        OPTIONAL,                 INTENT( IN )  :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Init'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Default_SpcCoeff_File
    CHARACTER( 512 ) :: Default_TauCoeff_File
    CHARACTER( 512 ) :: Default_ScatterCoeff_File

    INTEGER :: l



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK THE OPTIONAL FILE NAME/PATH ARGUMENTS --            #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Specify the default filenames
    ! -----------------------------

    Default_SpcCoeff_File     = 'SpcCoeff.bin'
    Default_TauCoeff_File     = 'TauCoeff.bin'
    Default_ScatterCoeff_File = 'ScatterCoeff.bin'


    ! -------------------------------
    ! Were other filenames specified?
    ! -------------------------------

    IF ( PRESENT( SpcCoeff_File ) ) &
      Default_SpcCoeff_File = TRIM( ADJUSTL( SpcCoeff_File ) )

    IF ( PRESENT( TauCoeff_File ) ) &
      Default_TauCoeff_File = TRIM( ADJUSTL( TauCoeff_File ) )

    IF ( PRESENT( ScatterCoeff_File ) ) &
      Default_ScatterCoeff_File = TRIM( ADJUSTL( ScatterCoeff_File ) )


    ! ---------------------
    ! Was a path specified?
    ! ---------------------

    IF ( PRESENT( File_Path ) ) THEN
      Default_SpcCoeff_File     = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_SpcCoeff_File )
      Default_TauCoeff_File     = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_TauCoeff_File )
      Default_ScatterCoeff_File = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_ScatterCoeff_File )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- LOAD THE SPECTRAL COEFFICIENTS --                #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_SpcCoeff( TRIM( Default_SpcCoeff_File ), &
                                       Quiet             = Quiet, &
                                       Process_ID        = Process_ID, &
                                       Output_Process_ID = Output_Process_ID, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading SpcCoeff data from '//&
                            TRIM( Default_SpcCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- LOAD THE GAS ABSORPTION COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_TauCoeff( TRIM( Default_TauCoeff_File ), &
                                       Quiet             = Quiet, &
                                       Process_ID        = Process_ID, &
                                       Output_Process_ID = Output_Process_ID, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading TauCoeff data from '//&
                            TRIM( Default_TauCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#        -- TEST THE CONGRUENCY OF THE SpcCoeff and TauCoeff DATA --       #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! The channel dimension must be the same
    ! --------------------------------------

    IF ( SC%n_Channels /= TC%n_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff and TauCoeff data have different channel dimensions.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------------------
    ! The sensor IDs and channel numbers must be the same
    ! ---------------------------------------------------

    IF ( ANY( (SC%NCEP_Sensor_ID   - TC%NCEP_Sensor_ID)   /= 0 ) .OR. &
         ANY( (SC%WMO_Satellite_ID - TC%WMO_Satellite_ID) /= 0 ) .OR. &
         ANY( (SC%WMO_Sensor_ID    - TC%WMO_Sensor_ID)    /= 0 ) .OR. &
         ANY( (SC%Sensor_Channel   - TC%Sensor_Channel)   /= 0 )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff and TauCoeff data Sensor ID/channel mismatch.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- LOAD THE SCATTERING COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#

    Error_Status = mw_read_cloud_opt(TRIM( Default_ScatterCoeff_File ) )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading ScatterCoeff data from '//&
                            TRIM( Default_ScatterCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#                     -- LOAD THE ChannelInfo STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Using the Sensor_Descriptor string
    ! ----------------------------------

    IF ( PRESENT( Sensor_Descriptor ) .AND. &
         PRESENT( Sensor_Channel    )       ) THEN
      ! ---------------------------------
      ! Get the requested channel indices
      ! ---------------------------------

      Error_Status = CRTM_Index_ChannelInfo( SC%Sensor_Descriptor, &
                                             SC%Sensor_Channel, &
                                             Sensor_Descriptor, &
                                             Sensor_Channel, &
                                             ChannelInfo, &
                                             Message_Log = Message_Log )

      IF ( Error_Status  /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error indexing ChannelInfo', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



    ! ------------------------------
    ! Using the NCEP_Sensor_ID array
    ! ------------------------------

    ELSE IF ( PRESENT( NCEP_Sensor_ID ) .AND. &
              PRESENT( Sensor_Channel )       ) THEN

      ! ---------------------------------
      ! Get the requested channel indices
      ! ---------------------------------

      Error_Status = CRTM_Index_ChannelInfo( SC%NCEP_Sensor_ID, &
                                             SC%Sensor_Channel, &
                                             NCEP_Sensor_ID, &
                                             Sensor_Channel, &
                                             ChannelInfo, &
                                             Message_Log = Message_Log )

      IF ( Error_Status  /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error indexing ChannelInfo', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



    ! -------------------------------------
    ! Default action is to use ALL channels
    ! -------------------------------------

    ELSE


      ! ----------------------------------
      ! Allocate the ChannelInfo structure
      ! ----------------------------------

      Error_Status = CRTM_Allocate_ChannelInfo( SC%n_Channels, &
                                                ChannelInfo, &
                                                Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Allocation of ChannelInfo(2) structure failed.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------------------
      ! Fill the Channel_Index component
      ! --------------------------------

      ChannelInfo%Channel_Index = (/ ( l, l = 1, SC%n_Channels ) /)

    END IF



    ! ------------------------------------------
    ! Fill the rest of the ChannelInfo structure
    ! ------------------------------------------

    ChannelInfo%Sensor_Descriptor = SC%Sensor_Descriptor( ChannelInfo%Channel_Index )
    ChannelInfo%NCEP_Sensor_ID    = SC%NCEP_Sensor_ID( ChannelInfo%Channel_Index )
    ChannelInfo%WMO_Satellite_ID  = SC%WMO_Satellite_ID( ChannelInfo%Channel_Index )
    ChannelInfo%WMO_Sensor_ID     = SC%WMO_Sensor_ID( ChannelInfo%Channel_Index )
    ChannelInfo%Sensor_Channel    = SC%Sensor_Channel( ChannelInfo%Channel_Index )

  END FUNCTION CRTM_Init





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy
!
! PURPOSE:
!       Function to deallocate all the shared data arrays allocated and
!       populated during the CRTM initialization.
!
! CATEGORY:
!       CRTM
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy( Process_ID  = Process_ID, &  ! Optional input
!                                    RCS_Id      = RCS_Id,     &  ! Revision control
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:   Set this argument to the MPI process ID that this
!                     function call is running under. This value is used
!                     solely for controlling message output. If MPI is not
!                     being used, ignore this argument.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     ERROR_HANDLER module.
!                     If == SUCCESS the CRTM deallocations were successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
!       CRTM_Destroy_SpcCoeff:       Function to deallocate the public data
!                                    structure SC containing the CRTM SpcCoeff
!                                    spectral coefficient data.
!                                    SOURCE: CRTM_SPCCOEFF module
!
!       CRTM_Destroy_TauCoeff:       Function to deallocate the public data
!                                    structure TC containing the CRTM TauCoeff
!                                    gas absorption coefficient data.
!                                    SOURCE: CRTM_TAUCOEFF module
!
!       CRTM_Destroy_ScatterCoeff:   Function to deallocate the public data
!                                    structure ScatC containing the CRTM
!                                    ScatterCoeff scattering coefficient data.
!                                    SOURCE: CRTM_SCATTERCOEFF module
!
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       All CRTM shared data arrays and structures are deallocated.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy( ChannelInfo,  &  ! In/Output
                         Process_ID,   &  ! Optional input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- In/Output
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN OUT ) :: ChannelInfo
    ! -- Optional input
    INTEGER,        OPTIONAL,      INTENT( IN )     :: Process_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,      INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,      INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DESTROY THE ChannelInfo STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ChannelInfo structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- DEALLOCATE THE GAS ABSORPTION COEFFICIENT STRUCTURE --         #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_TauCoeff( Process_ID  = Process_ID, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared TauCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DEALLOCATE THE SPECTRAL COEFFICIENT STRUCTURE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_SpcCoeff( Process_ID  = Process_ID, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared SpcCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy

END MODULE CRTM_LifeCycle


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_LifeCycle.f90,v 1.5 2004/07/01 20:52:51 paulv Exp $
!
! $Date: 2004/07/01 20:52:51 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_LifeCycle.f90,v $
! Revision 1.5  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.4  2004/06/24 19:03:14  paulv
! - Added code to initialize and destroy the ScatterCoeff structure.
! - Added code to check the congruency of the SpcCoeff and TauCoeff data
!   structures. This will probably change in the future but currently
!   they must agree.
!
! Revision 1.3  2004/06/15 21:59:51  paulv
! - Added optional NCEP_Sensor_ID argument to Init() function.
! - Corrected misnaming of ChannelInfo indexing function calls.
!
! Revision 1.2  2004/06/15 21:44:31  paulv
! - Added calls to ChannelInfo indexing functions in the Init() function.
! - Deallocation of ChannelInfo added to Destroy() function.
!
! Revision 1.1  2004/05/21 20:36:55  paulv
! Initial checkin.
!
!
!
