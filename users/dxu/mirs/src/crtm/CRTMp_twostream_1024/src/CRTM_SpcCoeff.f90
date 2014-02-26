!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_SpcCoeff
!
! PURPOSE:
!       Module containing the shared CRTM spectral coefficients (SpcCoeff)
!       and their load/destruction routines. 
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_SpcCoeff
!
! PUBLIC DATA:
!       SC:   Data structure containing the SpcCoeff data
!             UNITS:      N/A
!             TYPE:       SpcCoeff_Sensor_type
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
!       SpcCoeff_Define:      Module defining the SpcCoeff data structure and
!                             containing routines to manipulate it.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   COMPUTE_FLOAT_NUMBERS module
!
!       SpcCoeff_Binary_IO:   Module containing routines to read and write
!                             binary format SpcCoeff files.
!                             USEs: TYPE_KINDS module
!                                   FILE_UTILITY module
!                                   ERROR_HANDLER module
!                                   SPCCOEFF_DEFINE module
!                                   BINARY_UTILITY module
!
!       CRTM_Parameters:      Module of parameter definitions for the CRTM.
!                             USEs: TYPE_KINDS module
!
! CONTAINS:
!       CRTM_Load_SpcCoeff:      Function to load the SpcCoeff data
!                                into the module public data structure SC.
!
!       CRTM_Destroy_SpcCoeff:   Function to deallocate the module public data
!                                structure SC.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure SC.
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

MODULE CRTM_SpcCoeff


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Error_Handler

  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO

  USE CRTM_Parameters



  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibilities
  ! ------------------

  ! -- Everything private by default
  PRIVATE
  
  ! -- Public routines in this module
  PUBLIC :: CRTM_Load_SpcCoeff
  PUBLIC :: CRTM_Destroy_SpcCoeff

  ! -- Sensor type module parameters inherited from SpcCoeff_Define
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR
  PUBLIC :: VISIBLE_SENSOR
  PUBLIC :: SENSOR_TYPE_NAME
  
  ! -- Polarisation flag parameters inherited from SpcCoeff_Define
  PUBLIC :: N_POLARIZATION_TYPES   
  PUBLIC :: INVALID_POLARIZATION   
  PUBLIC :: UNPOLARIZED            
  PUBLIC :: INTENSITY              
  PUBLIC :: FIRST_STOKES_COMPONENT 
  PUBLIC :: SECOND_STOKES_COMPONENT
  PUBLIC :: THIRD_STOKES_COMPONENT 
  PUBLIC :: FOURTH_STOKES_COMPONENT
  PUBLIC :: VL_POLARIZATION        
  PUBLIC :: HL_POLARIZATION        
  PUBLIC :: plus45L_POLARIZATION   
  PUBLIC :: minus45L_POLARIZATION  
  PUBLIC :: VL_MIXED_POLARIZATION  
  PUBLIC :: HL_MIXED_POLARIZATION  
  PUBLIC :: RC_POLARIZATION        
  PUBLIC :: LC_POLARIZATION        
  PUBLIC :: POLARIZATION_TYPE_NAME


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_SpcCoeff.f90,v 1.4.2.2 2005/08/16 19:56:01 qliu Exp $'



  !#----------------------------------------------------------------------------#
  !#                 -- THE SHARED SpcCoeff DATA STRUCTURE --                   #
  !#                                                                            #
  !# Note that the SAVE attribute is specified to ensure that the data is       #
  !# retained even when this module is not being directly accessed.             #
  !#----------------------------------------------------------------------------#

  TYPE( SpcCoeff_Sensor_type ), SAVE, PUBLIC :: SC





CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Load_SpcCoeff
!
! PURPOSE:
!       Function to load the SpcCoeff spectral coefficient data into
!       the public data structure SC.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_SpcCoeff( SpcCoeff_File,                         &  ! Input
!                                          Quiet             = Quiet,             &  ! Optional input
!                                          Process_ID        = Process_ID,        &  ! Optional input
!                                          Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                          Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff_File:      Name of the CRTM Binary format SpcCoeff file
!                           containing the spectral coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
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
!                           If == SUCCESS the SpcCoeff data load was successful
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
!       Read_SpcCoeff_Binary:     Function to read the CRTM Binary format
!                                 SpcCoeff data file.
!                                 SOURCE: SPCCOEFF_BINARY_IO module
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
!       This function modifies the contents of the public data structure SC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_SpcCoeff( SpcCoeff_File,     &  ! Input
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

    CHARACTER( * ),           INTENT( IN )  :: SpcCoeff_File

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN )  :: Output_Process_ID

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_SpcCoeff'


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
    !#                   -- READ THE SpcCoeff DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_SpcCoeff_Binary( TRIM( SpcCoeff_File ), &  ! Input
                                         SC,                    &  ! Output
                                         Quiet             = Quiet, &
                                         Process_ID        = Process_ID, &
                                         Output_Process_ID = Output_Process_ID, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading SpcCoeff data from '//&
                            TRIM( SpcCoeff_File )//&
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
      IF ( Max_n_Channels /= SC%n_Channels ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "MAX_N_CHANNELS already set to different value, ", i4, ", ", &
                          &"than defined in spectral coefficient file, ", i4, &
                          &". Overwriting" )' ) &
                        Max_n_Channels, SC%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CALL CRTM_Set_Max_n_Channels( SC%n_Channels )
      END IF

    ELSE

      ! -- No. Set the value
      CALL CRTM_Set_Max_n_Channels( SC%n_Channels )

    END IF

  END FUNCTION CRTM_Load_SpcCoeff




!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_SpcCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure SC containing
!       the CRTM SpcCoeff spectral coefficient data.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_SpcCoeff( Process_ID  = Process_ID, &  ! Optional input
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
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
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
!                         If == SUCCESS the deallocation of the public SC data
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
!       Destroy_SpcCoeff:           Function to deallocate SpcCoeff data
!                                   structures.
!                                   SOURCE: SPCCOEFF_DEFINE module
!
!       CRTM_Reset_Max_n_Channels:  Routine to reset the protected variable
!                                   MAX_N_CHANNELS value in the CRTM_Parameters
!                                   module to an invalid value.
!                                   SOURCE: CRTM_PARAMETERS module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure SC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_SpcCoeff( Process_ID,   &  ! Optional input
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_SpcCoeff'


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

    Error_Status = Destroy_SpcCoeff( SC, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred deallocating the public SpcCoeff structure'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- RESET THE PROTECTED VARIABLE MAX_N_CHANNELS --              #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Reset_Max_n_Channels()

  END FUNCTION CRTM_Destroy_SpcCoeff

END MODULE CRTM_SpcCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_SpcCoeff.f90,v 1.4.2.2 2005/08/16 19:56:01 qliu Exp $
!
! $Date: 2005/08/16 19:56:01 $
!
! $Revision: 1.4.2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_SpcCoeff.f90,v $
! Revision 1.4.2.2  2005/08/16 19:56:01  qliu
! - Added sensor type and polarisation parameters to PUBLIC list.
!
! Revision 1.4.2.1  2005/07/12 14:47:01  paulv
! - Updated older generic type, SpcCoeff_type, to SpcCoeff_Sensor_type for
!   the CRTM_Sensor branch of the CRTM.
!
! Revision 1.4  2004/11/04 21:54:34  paulv
! - Renamed calls to CRTM_Parameters routines that set the protected variable
!   MAX_N_CHANNELS to have the "CRTM_" prefix.
! - Updated header documentation.
!
! Revision 1.3  2004/08/06 19:20:45  paulv
! - Altered initialization call to have the same form as other CRTM structure
!   initializations, but without the "CRTM_" prefix.
!
! Revision 1.2  2004/05/21 20:36:27  paulv
! - Updated routine descriptions. Cosmetic change only.
!
! Revision 1.1  2004/05/19 21:36:29  paulv
! Initial checkin.
!
!
!
