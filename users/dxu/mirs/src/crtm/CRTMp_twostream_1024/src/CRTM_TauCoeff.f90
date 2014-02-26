!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_TauCoeff
!
! PURPOSE:
!       Module containing the shared CRTM absorption coefficients (TauCoeff)
!       and their load/destruction routines. 
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_TauCoeff
!
! PUBLIC DATA:
!       TC:   Data structure containing the absorption coefficient data
!             UNITS:      N/A
!             TYPE:       TauCoeff_type
!             DIMENSION:  Scalar
!             ATTRIBUTES: PUBLIC, SAVE
!
! MODULES:
!       Type_Kinds:           Module containing data type kind definitions.
!
!       Error_Handler:        Module to define simple error codes and
!                             handle error conditions
!                             USEs: FILE_UTILITY module
!
!       TauCoeff_Define:      Module defining the TauCoeff data structure and
!                             containing routines to manipulate it.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   COMPUTE_FLOAT_NUMBERS module
!
!       TauCoeff_Binary_IO:   Module containing routines to read and write
!                             binary format TauCoeff files.
!                             USEs: TYPE_KINDS module
!                                   FILE_UTILITY module
!                                   ERROR_HANDLER module
!                                   TAUCOEFF_DEFINE module
!                                   COEFFICIENT_UTILITY module
!
!       CRTM_Parameters:      Module of parameter definitions for the CRTM.
!                             USEs: TYPE_KINDS module
!
! CONTAINS:
!       CRTM_Load_TauCoeff:      Function to load the TauCoeff data
!                                into the module public data structure TC.
!
!       CRTM_Destroy_TauCoeff:   Function to deallocate the module public data
!                                structure TC.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure TC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2003, 2004 Paul van Delst
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

MODULE CRTM_TauCoeff


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Error_Handler

  USE TauCoeff_Define
  USE TauCoeff_Binary_IO

  USE CRTM_Parameters


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Load_TauCoeff
  PUBLIC :: CRTM_Destroy_TauCoeff


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_TauCoeff.f90,v 1.5 2004/11/04 21:54:34 paulv Exp $'


  !#----------------------------------------------------------------------------#
  !#                 -- THE SHARED TauCoeff DATA STRUCTURE --                   #
  !#                                                                            #
  !# Note that the SAVE attribute is specified to ensure that the data is       #
  !# retained even when this module is not being directly accessed.             #
  !#----------------------------------------------------------------------------#

  TYPE( TauCoeff_type ), SAVE, PUBLIC :: TC





CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Load_TauCoeff
!
! PURPOSE:
!       Function to load the TauCoeff transmittance coefficient data into
!       the public data structure TC.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_TauCoeff( TauCoeff_File,                         &  ! Input
!                                          Quiet             = Quiet,             &  ! Optional input
!                                          Process_ID        = Process_ID,        &  ! Optional input
!                                          Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                          Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff_File:      Name of the CRTM Binary format TauCoeff file
!                           containing the transmittance coefficient data.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the TauCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in differs
!                                         from that stored in the CRTM_Parameters
!                                         module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
!       Read_TauCoeff_Binary:     Function to read the CRTM Binary format
!                                 TauCoeff data file.
!                                 SOURCE: TAUCOEFF_BINARY_IO module
!
!       CRTM_Get_Max_n_Channels:  Routine to get the protected variable
!                                 MAX_N_CHANNELS value in the CRTM_Parameters
!                                 module.
!                                 SOURCE: CRTM_PARAMETERS module
!
!       CRTM_Set_Max_n_Channels:  Routine to set the protected variable
!                                 MAX_N_CHANNELS value in the CRTM_Parameters
!                                 module.
!                                 SOURCE: CRTM_PARAMETERS module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure TC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_TauCoeff( TauCoeff_File,     &  ! Input
                               Quiet,             &  ! Optional input
                               Process_ID,        &  ! Optional input
                               Output_Process_ID, &  ! Optional input
                               Message_Log )      &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),           INTENT( IN )  :: TauCoeff_File

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN )  :: Output_Process_ID

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_TauCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: Process_ID_Tag

    ! -- Maximum channels protected variable
    INTEGER :: Max_n_Channels
    LOGICAL :: Is_Set



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Process ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- READ THE TAUCOEFF DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_TauCoeff_Binary( TRIM( TauCoeff_File ), &  ! Input
                                         TC,                    &  ! Output
                                         Quiet             = Quiet, &
                                         Process_ID        = Process_ID, &
                                         Output_Process_ID = Output_Process_ID, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading TauCoeff data from '//&
                            TRIM( TauCoeff_File )//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#               -- SET THE PROTECT VARIABLE MAX_N_CHANNELS --              #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the current value, if any
    ! -----------------------------

    CALL CRTM_Get_Max_n_Channels( Max_n_Channels, Is_Set )


    ! ------------------------------------
    ! Has the number of channels been set?
    ! ------------------------------------

    IF ( Is_Set ) THEN

      ! -- Yes. Check the value      
      IF ( Max_n_Channels /= TC%n_Channels ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "MAX_N_CHANNELS already set to different value, ", i4, ", ", &
                          &"than defined in transmittance coefficient file, ", i4, &
                          &". Overwriting" )' ) &
                        Max_n_Channels, TC%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CALL CRTM_Set_Max_n_Channels( TC%n_Channels )
      END IF

    ELSE

      ! -- No. Set the value
      CALL CRTM_Set_Max_n_Channels( TC%n_Channels )

    END IF

  END FUNCTION CRTM_Load_TauCoeff 



!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_TauCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure TC containing
!       the CRTM TauCoeff transmittance coefficient data.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_TauCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         ERROR_HANDLER module.
!                         If == SUCCESS the deallocation of the public TC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
!       Destroy_TauCoeff:           Function to deallocate TauCoeff data
!                                   structures.
!                                   SOURCE: TAUCOEFF_DEFINE module
!
!       CRTM_Reset_Max_n_Channels:  Routine to reset the protected variable
!                                   MAX_N_CHANNELS value in the CRTM_Parameters
!                                   module to an invalid value.
!                                   SOURCE: CRTM_PARAMETERS module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure TC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_TauCoeff( Process_ID,   &  ! Optional input
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_TauCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Process_ID_Tag



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Process ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- DESTROY THE STRUCTURE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_TauCoeff( TC, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred deallocating the public TauCoeff structure'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- RESET THE PROTECTED VARIABLE MAX_N_CHANNELS --              #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Reset_Max_n_Channels()

  END FUNCTION CRTM_Destroy_TauCoeff

END MODULE CRTM_TauCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_TauCoeff.f90,v 1.5 2004/11/04 21:54:34 paulv Exp $
!
! $Date: 2004/11/04 21:54:34 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_TauCoeff.f90,v $
! Revision 1.5  2004/11/04 21:54:34  paulv
! - Renamed calls to CRTM_Parameters routines that set the protected variable
!   MAX_N_CHANNELS to have the "CRTM_" prefix.
! - Updated header documentation.
!
! Revision 1.4  2004/08/06 19:20:45  paulv
! - Altered initialization call to have the same form as other CRTM structure
!   initializations, but without the "CRTM_" prefix.
!
! Revision 1.3  2004/06/18 20:16:29  paulv
! - Changed documentation to emphasise coefficients are for absorption
!   rather than transmittance.
!
! Revision 1.2  2004/05/21 20:36:13  paulv
! - Updated routine descriptions. Cosmetic change only.
!
! Revision 1.1  2004/05/19 21:36:37  paulv
! Initial checkin.
!
!
!
