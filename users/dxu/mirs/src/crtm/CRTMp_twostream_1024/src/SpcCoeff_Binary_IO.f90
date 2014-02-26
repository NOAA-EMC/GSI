!------------------------------------------------------------------------------
!M+
! NAME:
!       SpcCoeff_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write Binary format
!       SpcCoeff files.
!       
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SpcCoeff_Binary_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Error_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Binary_File_Utility:   Module containing utility routines for "Coeff" 
!                              datafiles in Binary format.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
!       SpcCoeff_Define:       Module defining the SpcCoeff data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       Inquire_SpcCoeff_Binary: Function to inquire a binary format
!                                SpcCoeff file.
!
!       Read_SpcCoeff_Binary:    Function to read a binary format
!                                SpcCoeff file.
!
!       Write_SpcCoeff_Binary:   Function to write a binary format
!                                SpcCoeff file.
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
!       User specified Binary format SpcCoeff data files for both
!       input and output.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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

MODULE SpcCoeff_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE Binary_File_Utility

  USE SpcCoeff_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_SpcCoeff_Binary
  PUBLIC :: Write_SpcCoeff_Binary
  PUBLIC :: Read_SpcCoeff_Binary


  ! -------------------
  ! Procedure overloads
  ! -------------------

  INTERFACE Write_SpcCoeff_Binary
    MODULE PROCEDURE Write_Spectral
    MODULE PROCEDURE Write_Sensor
  END INTERFACE Write_SpcCoeff_Binary

  INTERFACE Read_SpcCoeff_Binary
    MODULE PROCEDURE Read_Spectral
    MODULE PROCEDURE Read_Sensor
  END INTERFACE Read_SpcCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: SpcCoeff_Binary_IO.f90,v 6.2 2005/07/05 23:57:06 paulv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_SpcCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary format SpcCoeff file.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_SpcCoeff_Binary( &
!                        Filename,                                      &  ! Input
!                        n_Channels            = n_Channels,            &  ! Optional Output
!                        n_Nodes               = n_Nodes,               &  ! Optional output
!                        Max_Channels_per_Node = Max_Channels_per_Node, &  ! Optional output
!                        Release               = Release,               &  ! Optional Output
!                        Version               = Version,               &  ! Optional Output
!                        RCS_Id                = RCS_Id,                &  ! Revision control
!                        Message_Log           = Message_Log            )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:              Character string specifying the name of an SpcCoeff
!                              format data file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:           Character string specifying a filename in which any
!                              messages will be logged. If not specified, or if an
!                              error occurs opening the log file, the default action
!                              is to output messages to standard output.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Channels:            The number of channels dimension of the 
!                              SpcCoeff data.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Nodes:               The number of nodes dimension of the
!                              SpcCoeff data.
!                              ** NOTE:  This argument is ignored if
!                              the SpcCoeff data file contains only
!                              sensor data. 
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Max_Channels_per_Node: The maximum number of channels per
!                              node dimension of the SpcCoeff data.
!                              ** NOTE:  This argument is ignored if
!                              the SpcCoeff data file contains only
!                              sensor data. 
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Release:               The coefficient file release number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Version:               The coefficient file version number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:                Character string containing the Revision Control
!                              System Id field for the module.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error status.
!                              The error codes are defined in the ERROR_HANDLER module.
!                              If == SUCCESS the Binary file inquiry was successful
!                                 == FAILURE an unrecoverable error occurred.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format Coeff
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_SpcCoeff_Binary( Filename,              &  ! Input
                                    n_Channels,            &  ! Optional Output
                                    n_Nodes,               &  ! Optional output
                                    Max_Channels_per_Node, &  ! Optional output
                                    Release,               &  ! Optional Output
                                    Version,               &  ! Optional Output
                                    RCS_Id,                &  ! Revision control
                                    Message_Log )          &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional Output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Channels
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Nodes
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Max_Channels_per_Node
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Release
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Version

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_SpcCoeff_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: File_Type
    INTEGER( Long ) :: File_Release
    INTEGER( Long ) :: File_Version
    INTEGER( Long ) :: File_n_Channels
    INTEGER( Long ) :: File_n_Nodes
    INTEGER( Long ) :: File_Max_Channels_per_Node
 



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- OPEN THE BINARY FORMAT SpcCoeff DATA FILE --             #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening SpcCoeff file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_Release, &
                                       File_Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading SpcCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------------
    ! Read the file type information
    ! ------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading SpcCoeff file type from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------------
    ! Read the channel dimension
    ! --------------------------

    READ( FileID, IOSTAT = IO_Status ) File_n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Channels dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The other spectral dimensions
    ! -----------------------------

    Spectral_Dimensions: IF ( File_Type == SPECTRAL_FILE_TYPE ) THEN

      ! -- The number of nodes
      READ( FileID, IOSTAT = IO_Status ) File_n_Nodes

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading n_Nodes dimension from ", a, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! -- The maximum number of channels per node
      READ( FileID, IOSTAT = IO_Status ) File_Max_Channels_per_Node

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading Max_Channels_per_Nodes dimension from ", a, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END IF Spectral_Dimensions



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE RETURN ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( PRESENT( n_Channels ) ) THEN
      n_Channels = File_n_Channels
    END IF


    IF ( PRESENT( n_Nodes ) .AND.        &
         File_Type == SPECTRAL_FILE_TYPE ) THEN
      n_Nodes = File_n_Nodes
    END IF

    IF ( PRESENT( Max_Channels_per_Node ) .AND. &
         File_Type == SPECTRAL_FILE_TYPE        ) THEN
      Max_Channels_per_Node = File_Max_Channels_per_Node
    END IF


    ! --------------
    ! Ancillary info
    ! --------------

    IF ( PRESENT( Release ) ) THEN
      Release = File_Release
    END IF


    IF ( PRESENT( Version ) ) THEN
      Version = File_Version
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Inquire_SpcCoeff_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_SpcCoeff_Binary
!
! PURPOSE:
!       Function to read Binary format SpcCoeff files.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SpcCoeff_Binary( Filename,                              &  ! Input
!                                            SpcCoeff,                              &  ! Output
!                                            Quiet             = Quiet,             &  ! Optional input
!                                            Process_ID        = Process_ID,        &  ! Optional input
!                                            Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                            RCS_Id            = RCS_Id,            &  ! Revision control
!                                            Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the
!                           input binary format SpcCoeff data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress Information messages
!                           being printed to standard output (or the Message
!                           log file if the Message_Log optional argument is
!                           used.) By default, Information messages are printed.
!                           If QUIET = 0, Information messages are OUTPUT.
!                              QUIET = 1, Information messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn Message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all Information messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the Information messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:           Structure to contain the spectral coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       SpcCoeff_Sensor_type
!                                         OR
!                                       SpcCoeff_Spectral_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format Coeff
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Check_SpcCoeff_Release:  Function to check the Release value of
!                                the SpcCoeff data.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Allocate_SpcCoeff:       Function to allocate the pointer members
!                                of the SpcCoeff structure.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Count_SpcCoeff_Sensors:  Subroutine to count the number of
!                                different satellite/sensors in the
!                                SpcCoeff structure and set the
!                                n_Sensors field.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Version_SpcCoeff:        Subroutine to return a string containing
!                                version and dimension information about
!                                a SpcCoeff data structure.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Sensor( Filename,          &  ! Input
                        SpcCoeff,          &  ! Output
                        Quiet,             &  ! Optional input
                        Process_ID,        &  ! Optional input
                        Output_Process_ID, &  ! Optional input
                        RCS_Id,            &  ! Revision control
                        Message_Log )      &  ! Error messaging
                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),               INTENT( IN )     :: Filename

    ! -- Output
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: Quiet
    INTEGER,            OPTIONAL, INTENT( IN )     :: Process_ID
    INTEGER,            OPTIONAL, INTENT( IN )     :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_Binary(Sensor)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    CHARACTER( 128 ) :: Process_ID_Tag
    INTEGER :: FileID
    INTEGER( Long ) :: File_Type
    INTEGER( Long ) :: n_Channels
    INTEGER( Long ) :: Sensor_Descriptor_StrLen
    INTEGER( Long ) :: n_Items, n
    INTEGER( Long ), DIMENSION( N_SPCCOEFF_SENSOR_ITEMS ) :: Data_Type
 



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.

    ! -- ....unless....
    IF ( PRESENT( Quiet ) ) THEN
      ! -- ....the QUIET keyword is set.
      IF ( Quiet == SET ) Noisy = .FALSE.
    ELSE
      ! -- ....the Process_ID is not selected for output
      IF ( PRESENT( Process_ID ) .AND. PRESENT( Output_Process_ID ) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF


    ! -----------------------------------
    ! Create a process ID Message tag for
    ! WARNING and FAILURE Messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE SpcCoeff DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the release/version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Release, &
                                       SpcCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading SpcCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Read and check the file type information
    ! ----------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading SpcCoeff file type from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    IF ( File_Type /= SENSOR_FILE_TYPE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff file '//TRIM( Filename )//&
                            'is not a Sensor type' , &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading dimension data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Read the sensor descriptor string length
    ! ----------------------------------------

    READ( FileID, IOSTAT = IO_Status ) Sensor_Descriptor_StrLen

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Descriptor_StrLen from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the string length is consistent
    IF ( Sensor_Descriptor_StrLen /= SpcCoeff%Sensor_Descriptor_StrLen ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor descriptor string length ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), &
                      Sensor_Descriptor_StrLen, &
                      SpcCoeff%Sensor_Descriptor_StrLen
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------------------        
    ! Read the number of data items
    ! -----------------------------

    READ( FileID, IOSTAT = IO_Status ) n_Items

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading the number of data items from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the number of data items is correct
    IF ( n_Items /= N_SPCCOEFF_SENSOR_ITEMS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of data items in ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), n_Items, N_SPCCOEFF_SENSOR_ITEMS
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the data types
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) Data_Type
    
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading the data items types from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the data items types are correct
    DO n = 1, n_Items

      IF ( Data_Type( n ) /= SPCCOEFF_SENSOR_DATA_TYPE( n ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Invalid type for data item #", i2, &
                          &", ", a, ", in ", a )' ) &
                        n, TRIM( SPCCOEFF_SENSOR_DATA_NAME( n ) ), TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO



    !#--------------------------------------------------------------------------#
    !#                 -- ALLOCATE THE SpcCoeff STRUCTURE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      SpcCoeff, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating SpcCoeff structure.'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE DATA ITEMS --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The Sensor_Descriptor
    ! ---------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Descriptor

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Descriptor SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------
    ! The Sensor_Type
    ! ---------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Type SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_Sensor_ID
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%NCEP_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading NCEP_Sensor_ID SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_Satellite_ID
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Satellite_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading WMO_Satellite_ID SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_Sensor_ID
    ! -----------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading WMO_Sensor_ID SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The Sensor_Channel
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Channel SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------
    ! The Frequency
    ! -------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Frequency SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------
    ! The Wavenumber
    ! --------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Wavenumber

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Wavenumber SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------
    ! The Planck_C1
    ! -------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Planck_C1 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------
    ! The Planck_C2
    ! -------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Planck_C2 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------
    ! The Band_C1
    ! -----------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Band_C1 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------
    ! The Band_C2
    ! -----------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Band_C2 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------
    ! The Polarization
    ! ----------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Polarization

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Polarization SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The Cosmic_Background_Radiance
    ! ------------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Cosmic_Background_Radiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Cosmic_Background_Radiance SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The Is_Solar_Channel
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Is_Solar_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Is_Solar_Channel SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The Solar_Irradiance
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Solar_Irradiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Solar_Irradiance SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COUNT THE NUMBER OF SENSORS --                      #
    !#--------------------------------------------------------------------------#

    CALL Count_SpcCoeff_Sensors( SpcCoeff, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_Sensor


  FUNCTION Read_Spectral( Filename,          &  ! Input
                          SpcCoeff,          &  ! Output
                          Quiet,             &  ! Optional input
                          Process_ID,        &  ! Optional input
                          Output_Process_ID, &  ! Optional input
                          RCS_Id,            &  ! Revision control
                          Message_Log )      &  ! Error messaging
                        RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                 INTENT( IN )     :: Filename

    ! -- Output
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )     :: Quiet
    INTEGER,              OPTIONAL, INTENT( IN )     :: Process_ID
    INTEGER,              OPTIONAL, INTENT( IN )     :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_Binary(Spectral)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    CHARACTER( 128 ) :: Process_ID_Tag
    INTEGER :: FileID
    INTEGER( Long ) :: File_Type
    INTEGER( Long ) :: n_Channels
    INTEGER( Long ) :: n_Nodes
    INTEGER( Long ) :: Max_Channels_per_Node
    INTEGER( Long ) :: Sensor_Descriptor_StrLen
    INTEGER( Long ) :: n_Items, n
    INTEGER( Long ), DIMENSION( N_SPCCOEFF_SPECTRAL_ITEMS ) :: Data_Type
 



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.

    ! -- ....unless....
    IF ( PRESENT( Quiet ) ) THEN
      ! -- ....the QUIET keyword is set.
      IF ( Quiet == SET ) Noisy = .FALSE.
    ELSE
      ! -- ....the Process_ID is not selected for output
      IF ( PRESENT( Process_ID ) .AND. PRESENT( Output_Process_ID ) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF


    ! -----------------------------------
    ! Create a process ID Message tag for
    ! WARNING and FAILURE Messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE SpcCoeff DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the release/version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Release, &
                                       SpcCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading SpcCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Read and check the file type information
    ! ----------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading SpcCoeff file type from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    IF ( File_Type /= SPECTRAL_FILE_TYPE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff file '//TRIM( Filename )//&
                            'is not a Spectral type' , &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) n_Channels, &
                                       n_Nodes, &
                                       Max_Channels_per_Node

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading dimension data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Read the sensor descriptor string length
    ! ----------------------------------------

    READ( FileID, IOSTAT = IO_Status ) Sensor_Descriptor_StrLen

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Descriptor_StrLen from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the string length is consistent
    IF ( Sensor_Descriptor_StrLen /= SpcCoeff%Sensor_Descriptor_StrLen ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor descriptor string length ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), &
                      Sensor_Descriptor_StrLen, &
                      SpcCoeff%Sensor_Descriptor_StrLen
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------------------        
    ! Read the number of data items
    ! -----------------------------

    READ( FileID, IOSTAT = IO_Status ) n_Items

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading the number of data items from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the number of data items is correct
    IF ( n_Items /= N_SPCCOEFF_SPECTRAL_ITEMS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of data items in ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), n_Items, N_SPCCOEFF_SPECTRAL_ITEMS
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the data types
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) Data_Type
    
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading the data items types from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the data items types are correct
    DO n = 1, n_Items

      IF ( Data_Type( n ) /= SPCCOEFF_SPECTRAL_DATA_TYPE( n ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Invalid type for data item #", i2, &
                          &", ", a, ", in ", a )' ) &
                        n, TRIM( SPCCOEFF_SPECTRAL_DATA_NAME( n ) ), TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO



    !#--------------------------------------------------------------------------#
    !#                 -- ALLOCATE THE SpcCoeff STRUCTURE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      n_Nodes, &
                                      Max_Channels_per_Node, &
                                      SpcCoeff, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating SpcCoeff structure.'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE DATA ITEMS --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The Sensor_Descriptor
    ! ---------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Descriptor

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Descriptor SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------
    ! The Sensor_Type
    ! ---------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Type SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_Sensor_ID
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%NCEP_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading NCEP_Sensor_ID SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_Satellite_ID
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Satellite_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading WMO_Satellite_ID SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_Sensor_ID
    ! -----------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading WMO_Sensor_ID SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The Sensor_Channel
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Channel SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------
    ! The channel Frequency
    ! ---------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Frequency SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------
    ! The channel Wavenumber
    ! ----------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Wavenumber

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Wavenumber SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------
    ! The channel Planck_C1
    ! ---------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Planck_C1 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------
    ! The channel Planck_C2
    ! ---------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Planck_C2 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------
    ! The Band_C1
    ! -----------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Band_C1 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------
    ! The Band_C2
    ! -----------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Band_C2 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------
    ! The channel Polarization
    ! ------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Polarization

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Polarization SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The Is_Solar_Channel
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Is_Solar_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Is_Solar_Channel SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The Solar_Irradiance
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Solar_Irradiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Solar_Irradiance SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------------
    ! The MW_and_IR_Channel_Index
    ! ---------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%MW_and_IR_Channel_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading MW_and_IR_Channel_Index SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------------
    ! The n_Channels_per_Node
    ! -----------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%n_Channels_per_Node

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Channels_per_Node SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------------
    ! The Channel_Node_Map
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Channel_Node_Map

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Channel_Node_Map SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------
    ! The MW_and_IR_Node_Index
    ! ------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%MW_and_IR_Node_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading MW_and_IR_Node_Index SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The node Frequency
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Node_Frequency SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! The node Wavenumber
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Wavenumber

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Node_Wavenumber SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The node Planck_C1
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Planck_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Node_Planck_C1 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------
    ! The node Planck_C2
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Planck_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Node_Planck_C2 SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The Cosmic_Background_Radiance
    ! ------------------------------

    READ( FileID, IOSTAT = IO_Status ) SpcCoeff%Cosmic_Background_Radiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Cosmic_Background_Radiance SpcCoeff data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COUNT THE NUMBER OF SENSORS --                      #
    !#--------------------------------------------------------------------------#

    CALL Count_SpcCoeff_Sensors( SpcCoeff, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_Spectral



!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_SpcCoeff_Binary
!
! PURPOSE:
!       Function to write Binary format SpcCoeff files.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SpcCoeff_Binary( Filename,                 &  ! Input
!                                             SpcCoeff,                 &  ! Input
!                                             Quiet       = Quiet,      &  ! Optional input
!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     SpcCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       SpcCoeff:     Structure containing the spectral coefficient data.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_Sensor_type
!                                   OR
!                                 SpcCoeff_Spectral_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input SpcCoeff structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:     Function to test the association status
!                                of the pointer members of a SpcCoeff
!                                structure.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Check_SpcCoeff_Release:  Function to check the Release value of
!                                the SpcCoeff data.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Open_Binary_File:        Function to open Binary format Coeff
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Version_SpcCoeff:        Subroutine to return a string containing
!                                version and dimension information about
!                                a SpcCoeff data structure.
!                                SOURCE: SPCCOEFF_DEFINE module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Sensor( Filename,     &  ! Input
                         SpcCoeff,     &  ! Input
                         Quiet,        &  ! Optional input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),               INTENT( IN )  :: Filename
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )  :: SpcCoeff

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_Binary(Sensor)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Check the SpcCoeff structure Release
    ! ------------------------------------

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the SpcCoeff structure dimensions
    ! ---------------------------------------

    IF ( SpcCoeff%n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Channel dimension of SpcCoeff structure is < or = 0.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- OPEN THE SPECTRAL COEFFICIENT DATA FILE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Write the release/version information
    ! -------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Release, &
                                        SpcCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing SpcCoeff file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------------
    ! Write the file type
    ! -------------------

    WRITE( FileID, IOSTAT = IO_Status ) SENSOR_FILE_TYPE

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing SpcCoeff file type to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! Write the dimensions
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing n_Channels dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------------------------------------
    ! Write the sensor descriptor string length
    ! -----------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Descriptor_StrLen

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Descriptor_StrLen to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------------------------
    ! Write the number of data items per channel
    ! ------------------------------------------
   
    WRITE( FileID, IOSTAT = IO_Status ) N_SPCCOEFF_SENSOR_ITEMS

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the number of data items to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------------------
    ! Write the data item types
    ! -------------------------
   
    WRITE( FileID, IOSTAT = IO_Status ) SPCCOEFF_SENSOR_DATA_TYPE

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the data items types to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE DATA ITEMS --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The Sensor_Descriptor
    ! ---------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Descriptor

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Descriptor SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ---------------
    ! The Sensor_Type
    ! ---------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Type SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_Sensor_ID
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%NCEP_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing NCEP_Sensor_ID SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The WMO_Satellite_ID
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Satellite_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing WMO_Satellite_ID SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------------
    ! The WMO_Sensor_ID
    ! -----------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing WMO_Sensor_ID SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The Sensor_Channel
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Channel SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------
    ! The Frequency
    ! -------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Frequency SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------
    ! The Wavenumber
    ! --------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Wavenumber

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Wavenumber SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------
    ! The Planck_C1
    ! -------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Planck_C1 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------
    ! The Planck_C2
    ! -------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Planck_C2 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------
    ! The Band_C1
    ! -----------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Band_C1 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------
    ! The Band_C2
    ! -----------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Band_C2 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ----------------
    ! The Polarization
    ! ----------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Polarization

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Polarization SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------------
    ! The Cosmic_Background_Radiance
    ! ------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Cosmic_Background_Radiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Cosmic_Background_Radiance SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The Is_Solar_Channel
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Is_Solar_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Is_Solar_Channel SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The Solar_Irradiance
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Solar_Irradiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Solar_Irradiance SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Sensor


  FUNCTION Write_Spectral( Filename,     &  ! Input
                           SpcCoeff,     &  ! Input
                           Quiet,        &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                 INTENT( IN )  :: Filename
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )  :: SpcCoeff

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_Binary(Spectral)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Check the SpcCoeff structure Release
    ! ------------------------------------

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the SpcCoeff structure dimensions
    ! ---------------------------------------

    IF ( SpcCoeff%n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Channel dimension of SpcCoeff structure is < or = 0.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- OPEN THE SPECTRAL COEFFICIENT DATA FILE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Write the release/version information
    ! -------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Release, &
                                        SpcCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing SpcCoeff file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------------
    ! Write the file type
    ! -------------------

    WRITE( FileID, IOSTAT = IO_Status ) SPECTRAL_FILE_TYPE

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing SpcCoeff file type to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! Write the dimensions
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%n_Channels, &
                                        SpcCoeff%n_Nodes, &
                                        SpcCoeff%Max_Channels_per_Node

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing dimension data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------------------------------------
    ! Write the sensor descriptor string length
    ! -----------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Descriptor_StrLen

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Descriptor_StrLen to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------------
    ! Write the number of data items
    ! ------------------------------
   
    WRITE( FileID, IOSTAT = IO_Status ) N_SPCCOEFF_SPECTRAL_ITEMS

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the number of data items to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------------------
    ! Write the data item types
    ! -------------------------
   
    WRITE( FileID, IOSTAT = IO_Status ) SPCCOEFF_SPECTRAL_DATA_TYPE

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the data items types to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE DATA ITEMS --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The Sensor_Descriptor
    ! ---------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Descriptor

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Descriptor SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ---------------
    ! The Sensor_Type
    ! ---------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Type

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Type SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_Sensor_ID
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%NCEP_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing NCEP_Sensor_ID SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The WMO_Satellite_ID
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Satellite_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing WMO_Satellite_ID SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------------
    ! The WMO_Sensor_ID
    ! -----------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%WMO_Sensor_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing WMO_Sensor_ID SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The Sensor_Channel
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Sensor_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sensor_Channel SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ---------------------
    ! The channel Frequency
    ! ---------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Frequency SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ----------------------
    ! The channel Wavenumber
    ! ----------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Wavenumber

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Wavenumber SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ---------------------
    ! The channel Planck_C1
    ! ---------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Planck_C1 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ---------------------
    ! The channel Planck_C2
    ! ---------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Planck_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Planck_C2 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------
    ! The Band_C1
    ! -----------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Band_C1 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------
    ! The Band_C2
    ! -----------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Band_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Band_C2 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------
    ! The channel Polarization
    ! ------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Polarization

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Polarization SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The Is_Solar_Channel
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Is_Solar_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Is_Solar_Channel SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The Solar_Irradiance
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Solar_Irradiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Solar_Irradiance SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ---------------------------
    ! The MW_and_IR_Channel_Index
    ! ---------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%MW_and_IR_Channel_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing MW_and_IR_Channel_Index SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -----------------------
    ! The n_Channels_per_Node
    ! -----------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%n_Channels_per_Node

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing n_Channels_per_Node SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! The Channel_Node_Map
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Channel_Node_Map

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Channel_Node_Map SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------
    ! The MW_and_IR_Node_Index
    ! ------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%MW_and_IR_Node_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing MW_and_IR_Node_Index SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The node Frequency
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Node_Frequency SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------------
    ! The node Wavenumber
    ! -------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Wavenumber

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Node_Wavenumber SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The node Planck_C1
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Planck_C1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Node_Planck_C1 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------
    ! The node Planck_C2
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Node_Planck_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Node_Planck_C2 SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------------
    ! The Cosmic_Background_Radiance
    ! ------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) SpcCoeff%Cosmic_Background_Radiance

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Cosmic_Background_Radiance SpcCoeff data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Spectral

END MODULE SpcCoeff_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SpcCoeff_Binary_IO.f90,v 6.2 2005/07/05 23:57:06 paulv Exp $
!
! $Date: 2005/07/05 23:57:06 $
!
! $Revision: 6.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SpcCoeff_Binary_IO.f90,v $
! Revision 6.2  2005/07/05 23:57:06  paulv
! - Corrected numerous minor bugs involving misnamed variables/parameters
!   and use of non-existent arguments (cut-n-paste error).
!
! Revision 6.1  2005/07/05 21:38:04  paulv
! - Added some additional components to the Spectral SpcCoeff structure
!   definition.
! - Moved some node-based components back to being channel-based in the
!   Spectral SpcCoeff structure definition.
! - Still untested.
!
! Revision 6.0  2005/07/05 11:39:34  paulv
! - Major update. There are now two SpcCoeff data structures; a Sensor based
!   one (the old, regular SpcCoeff) and a Spectral based one (for use with the
!   monomchromatic OSS code). Definition and I/O modules have been modified
!   accordingly. Untested.
!
! Revision 5.1  2005/04/01 18:01:03  paulv
! - Removed all n_Stokes variable declarations.
!
! Revision 5.0  2005/03/31 21:26:38  paulv
! - Updated for use with new SpcCoeff_Define module. Untested.
!
! Revision 4.6  2005/02/18 23:14:48  paulv
! - Corrected header documentation.
!
! Revision 4.5  2004/08/31 21:10:42  paulv
! - Minor documentation updates only.
!
! Revision 4.4  2004/08/23 14:45:18  paulv
! - Upgraded to Fortran95.
! - Added structure association test to the Write() function.
! - Changed INTENT of SpcCoeff structure in Read() function from OUT to
!   IN OUT. Necessary to prevent memory leaks.
! - Updated header documentation.
!
! Revision 4.3  2004/07/08 23:44:14  paulv
! - Replaced Coeff_Binary_Utility with Binary_File_Utility module.
!
! Revision 4.2  2004/06/25 22:31:32  paulv
! - Minor header documentation update.
!
! Revision 4.1  2004/06/25 21:35:22  paulv
! - Corrected a type declaration bug in the Read() function.
!
! Revision 4.0  2004/06/25 19:46:05  paulv
! - Dummy checkin to update version number branch to 4.X.
!
! Revision 3.1  2004/06/25 19:41:13  paulv
! - Upgraded definition and I/O of polarization component from a scalar
!   value to a full Stokes vector.
!
! Revision 3.0  2004/05/17 17:40:49  paulv
! - Added Sensor_Descriptor component to SpcCoeff structure. Modified the
!   netCDF and Binary I/O modules to handle the new component. New SpcCoeff
!   release number is 3.
!
! Revision 1.11  2004/03/09 17:24:37  paulv
! - Mostly cosmetic changes. Some fixes to eliminate possibilities of
!   exceeding string lengths.
!
! Revision 1.10  2003/11/25 21:07:01  paulv
! - Cosmetic documentation changes only.
!
! Revision 1.9  2003/11/13 19:35:46  paulv
! - Updated header documentation.
!
! Revision 1.8  2003/10/24 18:36:41  paulv
! - Replaced COEFFICIENT_UTILITY module with COEFF_BINARY_UTILITY module.
!
! Revision 1.7  2003/10/24 18:18:10  paulv
! - Code category changed from
!     NCEP RTM : Coefficients : SpcCoeff
!   to
!     Instrument Information : SpcCoeff
!
! Revision 1.6  2003/06/19 21:38:07  paulv
! - Now using the Check_SpcCoeff_Release() function to determine if a passed
!   SpcCoeff structure can be read/written.
!
! Revision 1.5  2003/04/15 20:36:28  paulv
! - Removed SpcCoeff association status check from Write() function.
!
! Revision 1.4  2003/02/13 17:36:49  paulv
! - Modified all calls that pass filename character strings to pass the TRIM()
!   version of the argument.
!
! Revision 1.3  2003/02/10 22:53:15  paulv
! - Minor cosmetic changes.
!
! Revision 1.2  2002/12/26 17:38:42  paulv
! - Many changes to use the new SpcCoeff structure and new routines from
!   SpcCoeff_Define module.
!
! Revision 1.1  2002/12/23 18:32:09  paulv
! Initial checkin. Currently, just a copy of the original SpcCoeff_IO module
! but with renamed components. Untested.
!
!
!
