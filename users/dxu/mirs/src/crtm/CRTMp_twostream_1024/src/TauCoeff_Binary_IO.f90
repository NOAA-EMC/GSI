!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write Binary format
!       TauCoeff files.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_Binary_IO
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
!       Binary_File_Utility:   Module containing utility routines for "Binary" 
!                              format datafiles.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
!       TauCoeff_Define:       Module defining the TauCoeff data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       Inquire_TauCoeff_Binary: Function to inquire a Binary format
!                                TauCoeff file.
!
!       Read_TauCoeff_Binary:    Function to read a Binary format
!                                TauCoeff file.
!
!       Write_TauCoeff_Binary:   Function to write a Binary format
!                                TauCoeff file.
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
!       User specified Binary format TauCoeff data files for both
!       input and output.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either Version 2
!  of the License, or (at your option) any later Version.
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

MODULE TauCoeff_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE Binary_File_Utility

  USE TauCoeff_Define



  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_TauCoeff_Binary
  PUBLIC :: Read_TauCoeff_Binary
  PUBLIC :: Write_TauCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: TauCoeff_Binary_IO.f90,v 5.6 2005/02/07 18:15:01 paulv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_TauCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary format TauCoeff file.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_TauCoeff_Binary( Filename,                    &  ! Input
!                                               n_Orders     = n_Orders,     &  ! Optional output
!                                               n_Predictors = n_Predictors, &  ! Optional output
!                                               n_Absorbers  = n_Absorbers,  &  ! Optional output
!                                               n_Channels   = n_Channels,   &  ! Optional output
!                                               Release      = Release,      &  ! Optional Output
!                                               Version      = Version,      &  ! Optional Output
!                                               RCS_Id       = RCS_Id,       &  ! Revision control
!                                               Message_Log  = Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           TauCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Orders:           The maximum polynomial order used to reconstruct the
!                           transmittance coefficients.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Orders, where the
!                                 0'th term is the offset. Therefore the actual
!                                 number of array elements along this dimension
!                                 is n_Orders+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Predictors:       The number of predictor functions used in generating
!                           the TauCoeff data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Absorbers:        The number of absorbers dimension of the TauCoeff data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the TauCoeff data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Release:            The TauCoeff data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Version:            The TauCoeff data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output Messages
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

  FUNCTION Inquire_TauCoeff_Binary( Filename,     &  ! Input
                                    n_Orders,     &  ! Optional output
                                    n_Predictors, &  ! Optional output
                                    n_Absorbers,  &  ! Optional output
                                    n_Channels,   &  ! Optional output
                                    Release,      &  ! Optional Output
                                    Version,      &  ! Optional Output
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
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Orders
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Predictors
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Absorbers
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Channels
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_TauCoeff_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: File_Release
    INTEGER( Long ) :: File_Version
    INTEGER( Long ) :: File_n_Orders
    INTEGER( Long ) :: File_n_Predictors
    INTEGER( Long ) :: File_n_Absorbers
    INTEGER( Long ) :: File_n_Channels
 



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
    !#             -- OPEN THE BINARY FORMAT TauCoeff DATA FILE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening TauCoeff file '//TRIM( Filename ), &
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
      WRITE( Message, '( "Error reading TauCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) File_n_Orders , &
                                       File_n_Predictors, &
                                       File_n_Absorbers, &
                                       File_n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
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
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE RETURN ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( PRESENT( n_Orders ) ) THEN
      n_Orders = File_n_Orders
    END IF

    IF ( PRESENT( n_Predictors ) ) THEN
      n_Predictors = File_n_Predictors
    END IF

    IF ( PRESENT( n_Absorbers ) ) THEN
      n_Absorbers = File_n_Absorbers
    END IF

    IF ( PRESENT( n_Channels ) ) THEN
      n_Channels = File_n_Channels
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

  END FUNCTION Inquire_TauCoeff_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_TauCoeff_Binary
!
! PURPOSE:
!       Function to read Binary format TauCoeff files.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_TauCoeff_Binary( Filename,                              &  ! Input
!                                            TauCoeff,                              &  ! Output
!                                            Quiet             = Quiet,             &  ! Optional input
!                                            Process_ID        = Process_ID,        &  ! Optional input
!                                            Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                            RCS_Id            = RCS_Id,            &  ! Revision control
!                                            Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format TauCoeff data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
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
!                           UNITS:      N/A
!                           TYPE:       Integer
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       Integer
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       Integer
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff:           Structure containing the transmittance coefficient data
!                           read from the file.
!                           UNITS:      N/A
!                           TYPE:       TauCoeff_type
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
!                              == FAILURE an unrecoverable read error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Check_TauCoeff_Release:  Function to check the Release value of
!                                the TauCoeff data.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Allocate_TauCoeff:       Function to allocate the pointer members
!                                of the TauCoeff structure.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Count_TauCoeff_Sensors:  Subroutine to count the number of
!                                different satellite/sensors in the
!                                TauCoeff structure and set the
!                                n_Sensors field.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Version_TauCoeff:        Subroutine to return a string containing
!                                version and dimension information about
!                                a TauCoeff data structure.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Display_Message:         Subroutine to output Messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_TauCoeff_Binary( Filename,          &  ! Input
                                 TauCoeff,          &  ! Output
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
    CHARACTER( * ),           INTENT( IN )     :: Filename

    ! -- Output
    TYPE( TauCoeff_type ),    INTENT( IN OUT ) :: TauCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )     :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN )     :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    CHARACTER( 128 ) :: Process_ID_Tag
    INTEGER :: FileID
    INTEGER( Long ) :: n_Orders
    INTEGER( Long ) :: n_Predictors
    INTEGER( Long ) :: n_Absorbers
    INTEGER( Long ) :: n_Channels
    INTEGER( Long ) :: Sensor_Descriptor_StrLen
    INTEGER( Long ) :: n_items, n
    INTEGER( Long ), DIMENSION( N_TAUCOEFF_ITEMS ) :: Data_Type
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
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
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
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
    !#                    -- OPEN THE TauCoeff DATA FILE --                     #
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
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Release, &
                                       TauCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading TauCoeff file Release/Version values from ", a, &
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

    Error_Status = Check_TauCoeff_Release( TauCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'TauCoeff Release check failed for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) n_Orders, &
                                       n_Predictors, &
                                       n_Absorbers, &
                                       n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
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
    IF ( Sensor_Descriptor_StrLen /= TauCoeff%Sensor_Descriptor_StrLen ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor descriptor string length ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), &
                      Sensor_Descriptor_StrLen, &
                      TauCoeff%Sensor_Descriptor_StrLen
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
    IF ( n_Items /= N_TAUCOEFF_ITEMS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of data items in ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), n_Items, N_TAUCOEFF_ITEMS
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

      IF ( Data_Type( n ) /= TAUCOEFF_DATA_TYPE( n ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Invalid type for data item #", i2, &
                          &", ", a, ", in ", a )' ) &
                        n, TRIM( TAUCOEFF_DATA_NAME( n ) ), TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO



    !#--------------------------------------------------------------------------#
    !#                  -- ALLOCATE THE TauCoeff STRUCTURE --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_TauCoeff( n_Orders, &
                                      n_Predictors, &
                                      n_Absorbers, &
                                      n_Channels, &
                                      TauCoeff, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating TauCoeff structure.'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE SENSOR ID DATA --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Descriptor, &
                                       TauCoeff%NCEP_Sensor_ID, &
                                       TauCoeff%WMO_Satellite_ID, &
                                       TauCoeff%WMO_Sensor_ID, &
                                       TauCoeff%Sensor_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading sensor ID data from ", a, &
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
    !#                      -- READ THE ABSORBER ID DATA --                     #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Absorber_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading absorber ID data from ", a, &
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
    !#                         -- READ THE ALPHA DATA --                        #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Alpha, &
                                       TauCoeff%Alpha_C1, &
                                       TauCoeff%Alpha_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Alpha data from ", a, &
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
    !#                       -- READ THE ORDER INDICES --                       #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Order_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading order indices from ", a, &
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
    !#                     -- READ THE PREDICTOR INDICES --                     #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Predictor_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading predictor indices from ", a, &
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
    !#                -- READ THE GAS ABSORPTION COEFFICIENTS --                #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%C

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading gas absorption coefficients from ", a, &
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

    CLOSE( FileID, STATUS = 'KEEP',   &
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

    CALL Count_TauCoeff_Sensors( TauCoeff, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_TauCoeff( TauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_TauCoeff_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_TauCoeff_Binary
!
! PURPOSE:
!       Function to write Binary format TauCoeff files.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauCoeff_Binary( Filename,                 &   ! Input
!                                             TauCoeff,                 &   ! Input
!                                             Quiet       = Quiet,      &   ! Optional input
!                                             RCS_Id      = RCS_Id,     &   ! Revision control
!                                             Message_Log = Message_Log )   ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     TauCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       TauCoeff:     Structure containing the gas absorption coefficient data.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information Messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information Messages are printed.
!                     If QUIET = 0, information Messages are OUTPUT.
!                        QUIET = 1, information Messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
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
!                        == FAILURE - the input TauCoeff structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff:     Function to test the association status
!                                of the pointer members of a TauCoeff
!                                structure.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Check_TauCoeff_Status:   Function to check the association status
!                                of the TauCoeff structure pointer members.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Version_TauCoeff:        Subroutine to return a string containing
!                                version and dimension information about
!                                a TauCoeff data structure.
!                                SOURCE: TAUCOEFF_DEFINE module
!
!       Display_Message:         Subroutine to output Messages
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
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_TauCoeff_Binary( Filename,     &  ! Input
                                  TauCoeff,     &  ! Input
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
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( TauCoeff_type ),    INTENT( IN )  :: TauCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_Binary'
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
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
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

    IF ( .NOT. Associated_TauCoeff( TauCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Check the TauCoeff structure Release
    ! ------------------------------------

    Error_Status = Check_TauCoeff_Release( TauCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'TauCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the TauCoeff structure dimensions
    ! ---------------------------------------

    IF ( TauCoeff%n_Orders     < 1 .OR. &
         TauCoeff%n_Predictors < 1 .OR. &
         TauCoeff%n_Absorbers  < 1 .OR. &
         TauCoeff%n_Channels   < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'One or more dimensions of TauCoeff structure are < or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- OPEN THE GAS ABSORPTION COEFFICIENT DATA FILE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     For_Output  = SET,        &
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
    ! Write the Release/Version information
    ! -------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Release, &
                                        TauCoeff%Version 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing TauCoeff file Release/Version values to ", a, &
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

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%n_Orders, &
                                        TauCoeff%n_Predictors, &
                                        TauCoeff%n_Absorbers, &
                                        TauCoeff%n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing data dimensions to ", a, ". IOSTAT = ", i5 )' ) &
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

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Descriptor_StrLen

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
   
    WRITE( FileID, IOSTAT = IO_Status ) N_TAUCOEFF_ITEMS

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
   
    WRITE( FileID, IOSTAT = IO_Status ) TAUCOEFF_DATA_TYPE

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the data item types to ", a, &
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
    !#                      -- WRITE THE SENSOR ID DATA --                      #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Descriptor, &
                                        TauCoeff%NCEP_Sensor_ID, &
                                        TauCoeff%WMO_Satellite_ID, &
                                        TauCoeff%WMO_Sensor_ID, &
                                        TauCoeff%Sensor_Channel

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing sensor ID data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE ABSORBER ID DATA --                     #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Absorber_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing absorber ID data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE ALPHA DATA --                        #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Alpha, &
                                        TauCoeff%Alpha_C1, &
                                        TauCoeff%Alpha_C2

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Alpha data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE ORDER INDICES --                       #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Order_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing order indices to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE PREDICTOR INDICES --                     #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Predictor_Index

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing predictor indices to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                -- WRITE THE GAS ABSORPTION COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%C

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing gas absorption coefficients to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
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
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_TauCoeff( TauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_TauCoeff_Binary

END MODULE TauCoeff_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: TauCoeff_Binary_IO.f90,v 5.6 2005/02/07 18:15:01 paulv Exp $
!
! $Date: 2005/02/07 18:15:01 $
!
! $Revision: 5.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: TauCoeff_Binary_IO.f90,v $
! Revision 5.6  2005/02/07 18:15:01  paulv
! - Category change only.
!
! Revision 5.5  2004/09/09 20:29:22  paulv
! - Updated documentation.
!
! Revision 5.4  2004/08/20 01:08:31  paulv
! - Upgraded to Fortran95.
! - Removed all Initialize_TauCoeff() calls.
! - Added TauCoeff association check in Write_TauCoeff_Binary() function. If
!   the input structure is empty, an error is issued.
! - Changed the INTENT of the output TauCoeff argument in the Read_TauCoeff_Binary()
!   function from OUT to IN OUT. Necessary to prevent memory leaks.
!
! Revision 5.3  2004/07/09 13:36:48  paulv
! - Stuffed up the checkin of v5.2 changes - overwrote v5.1 changes. So, redid
!   the v5.1 changes:
! - Removed declarations of unused variables.
! - Changed all references to transmittance coefficients to gas absorption
!   coefficients.
!
! Revision 5.2  2004/07/08 18:30:11  paulv
! - Replaced Coeff_Binary_Utility module with more generic Binary_File_Utility
!
! Revision 5.0  2004/05/17 21:02:06  paulv
! - Added Sensor_Descriptor component to TauCoeff structure. Modified the
!   netCDF and Binary I/O modules to handle the new component. New TauCoeff
!   release number is 5.
!
! Revision 4.0  2004/02/17 21:56:32  paulv
! - New Release.
! - Revision number of this module sync'd with the TauCoeff_Define module.
! - Added I/O for ORDER_INDEX component of TauCoeff structure.
! - Changes to Predictor_Index and C coefficients array I/O to sync with
!   dimension changes in TauCoeff_Define module.
!
! Revision 1.5  2003/11/25 19:38:39  paulv
! - Replaced COEFFICIENT_UTILITY module with COEFF_BINARY_UTILITY module.
! - Updated header documentation.
!
! Revision 1.4  2003/06/19 21:37:48  paulv
! - Now using the Check_TauCoeff_Release() function to determine if a passed
!   TauCoeff structure can be read/written.
!
! Revision 1.3  2003/03/28 14:45:02  paulv
! - Removed calls to the Check_TauCoeff_Status routine. This function in the
!   TauCoeff_Define module has been replaced with the PRIVATE function
!   Associated_TauCoeff. The assumption is now that the user will take the
!   required care to ensure a valid structure is passed to the I/O routines.
!
! Revision 1.2  2003/01/02 22:11:08  paulv
! - Corrected bug with incorrect filename variable.
!
! Revision 1.1  2003/01/02 20:46:31  paulv
! - Many changes to use the new TauCoeff structure and new routines from
!   TauCoeff_Define module.
!
!
!
!
