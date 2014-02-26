!--------------------------------------------------------------------------------
!M+
! NAME:
!       Binary_File_Utility
!
! PURPOSE:
!       Module for utility routines for "Binary" datafiles (unformatted,
!       sequential) that conform to the required format.
!
! CATEGORY:
!       Utility
!
! CALLING SEQUENCE:
!       USE Binary_File_Utility
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:             Module containing data type kind definitions.
!
!       File_Utility:           Module containing generic file utility routines
!
!       Error_Handler:          Module to define error codes and handle error
!                               conditions.
!                               USEs: FILE_UTILITY module
!
!       Endian_Utility:         Module containing functions to byte-swap intrinsic
!                               data types.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       PUBLIC routines
!       ---------------
!
!         Open_Binary_File:     Function to open the sequential access
!                               unformatted files.
!
!       PRIVATE routines
!       ----------------
!
!         Check_Binary_File:    Function to determine if the Binary
!                               file is in the correct format, endian-wise.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000,2003,2004 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE Binary_File_Utility


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE Endian_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Open_Binary_File
  PUBLIC :: Open_Text_File

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- File header magic number used to
  ! -- check for the correct byte order
  INTEGER( Long ), PRIVATE, PARAMETER :: MAGIC_NUMBER = 123456789_Long

  ! -- Keyword argument set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Check_Binary_File
!
! PURPOSE:
!       Function to determine if the unformatted Binary file is in the correct
!       byte order.
!
! CALLING SEQUENCE:
!       Error_Status = Check_Binary_File( Filename                  &  ! Input
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Name of the Binary file to check.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the ERROR_HANDLER module. Values returned by
!                         this function are:
!                           SUCCESS == file check was successful
!                           FAILURE == - error occurred reading a file record,
!                                      - 8- and/or 32-bit integers not supported.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Swap_Endian:      Function to swap the byte order of an input
!                         Long integer
!                         
!       Display_Message:  Subroutine to output Messages
!                         SOURCE: Error_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-July-2000
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Check_Binary_File( Filename,  &
                              Message_Log ) &
                            RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),           INTENT( IN ) :: Filename
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Check_Binary_File'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: FileID
    INTEGER :: IO_Status

    INTEGER( Long ) :: Magic_Number_Read
    INTEGER( Long ) :: Magic_Number_Swapped



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#  -- CHECK THAT THE CURRENT COMPILATION SUPPORTS 4-BYTE INTEGER TYPES --  #
    !#--------------------------------------------------------------------------#

    IF ( BIT_SIZE( 1_Long ) /= 32 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            '32-bit integers not supported. '//&
                            'Unable to determine endian-ness', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- OPEN THE FILE FIR DIRECT ACCESS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a free unit number
    ! ----------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------
    ! Open the file as direct access
    ! ------------------------------

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD', &
                  ACTION = 'READ', &
                  ACCESS = 'DIRECT', &
                  FORM   = 'UNFORMATTED', &
                  RECL   = n_Bytes_Long, &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE MAGIC NUMBER --                       #
    !#--------------------------------------------------------------------------#

    READ( FileID, REC = 2, IOSTAT = IO_Status ) Magic_Number_Read

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading file magic number. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CLOSE THE FILE --                          #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID )



    !#--------------------------------------------------------------------------#
    !#                      -- COMPARE THE MAGIC NUMBERS --                     #
    !#--------------------------------------------------------------------------#

    IF ( Magic_Number_Read /= MAGIC_NUMBER ) THEN


      ! ---------------------------
      ! Set the return error status
      ! ---------------------------

      Error_Status = FAILURE


      ! --------------------------
      ! Byte swap the magic number
      ! --------------------------

      Magic_Number_Swapped = Swap_Endian( Magic_Number_Read )


      ! ----------------------------
      ! Check the magic number again
      ! ----------------------------

      IF ( Magic_Number_Swapped /= MAGIC_NUMBER ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Unrecognised file format. Invalid magic number.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ---------------------------------------------------------
      ! If we get here then the data does need to be byte-swapped
      ! ---------------------------------------------------------

      CALL Display_Message( ROUTINE_NAME, &
                            'Data file needs to be byte-swapped.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    END IF

  END FUNCTION Check_Binary_File





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Open_Binary_File
!
! PURPOSE:
!       Function to open the unformatted, sequential access Binary files
!
! CALLING SEQUENCE:
!       Error_Status = Open_Binary_File( Filename,                 &  ! Input
!                                        FileID,                   &  ! Output
!                                        For_Output = For_Output,  &  ! Optional input
!                                        No_Check   = No_Check,    &  ! Optional input
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Name of the Binary file to open.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       For_Output:       Set this optional argument to open a new file for
!                         writing. Default action is to open an existing file
!                         for read access. Note, if the file already exists and
!                         it is opened with this keyword set, the file is
!                         overwritten.
!                         If == 0, existing file is opened for READ access (DEFAULT)
!                                  ACTION='READ', STATUS='OLD'
!                            == 1, new file is opened for WRITE access.
!                                  ACTION='WRITE', STATUS='REPLACE'
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       No_Check:         Set this optional argument to suppress the byte-order
!                         check made on an existing file by NOT reading the file
!                         header magic number.  Default action is to check the
!                         file. This argument is ignored if the FOR_OUTPUT 
!                         optional argument is set.
!                         If == 0, existing file magic number is read and the
!                                  byte order is checked (DEFAULT)
!                            == 1, magic number is *NOT* read from file and
!                                  checked for validity.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       FileID:           File unit number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the ERROR_HANDLER module. Values returned by
!                         this function are:
!                           SUCCESS == file open was successful
!                           FAILURE == - error occurred during file open,
!                                      - error occurred during file check.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!      Check_Binary_File:       Function to determine if the unformatted
!                               Binary file is in the correct byte order.
!
!      File_Exists:             Function to determine if a named file exists.
!                               SOURCE: File_Utility module
!
!      Get_Lun:                 Function to return a free logical unit number
!                               for file access.
!                               SOURCE: File_Utility module
!
!      Display_Message:         Subroutine to output Messages
!                               SOURCE: Error_Handler module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       File open ACCESS and FORM specifiers are set to 'SEQUENTIAL' and
!       'UNFORMATTED' respectively.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-July-2000
!                       paul.vandelst@ssec.wisc.edu
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION Open_Binary_File( Filename,     &  ! Input
                             FileID,       &  ! Output
                             For_Output,   &  ! Optional input
                             No_Check,     &  ! Optional input
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: FileID

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: For_Output
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_Check

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_Binary_File'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: File_Check
    LOGICAL :: File_Input

    INTEGER :: IO_Status
    INTEGER( Long ) :: Magic_Number_Read
    CHARACTER( 7 ) :: File_Status
    CHARACTER( 5 ) :: File_Action



    !#--------------------------------------------------------------------------#
    !#                   -- SET SUCCESSFUL RETURN STATUS --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! File byte order check
    ! ---------------------

    ! -- Default action is to check the file...
    File_Check = .TRUE.

    ! -- Unless the No_Check argument is set
    IF ( PRESENT( No_Check ) ) THEN
      IF ( No_Check == SET ) File_Check = .FALSE.
    END IF


    ! ------------------------------
    ! Is file to be read or written?
    ! ------------------------------

    ! -- Default action is to READ file
    File_Input = .TRUE.

    ! -- ...unless the For_Output keyword is set
    IF ( PRESENT( For_Output ) ) THEN
      IF ( For_Output == SET ) THEN
        File_Input = .FALSE.
        File_Check = .FALSE.
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK DATA FILE EXISTENCE --                     #
    !#--------------------------------------------------------------------------#

    IF ( File_Input ) THEN


      ! -------------------------------
      ! File is to be READ. If the file
      ! does not exist, return an error
      ! -------------------------------

      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAMe, &
                              'File '//TRIM( Filename )//' not found.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Set OPEN keywords for READING
      File_Status = 'OLD'
      File_Action = 'READ'


    ELSE


      ! ------------------------------------
      ! File is to be WRITTEN. If the file
      ! does exist, output a warning message
      ! ------------------------------------

      IF ( File_Exists( TRIM( Filename ) ) ) THEN
        CALL Display_Message( ROUTINE_NAMe, &
                              'File '//TRIM( Filename )//' will be overwritten.', &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

      ! -- Set OPEN keywords for WRITING
      File_Status = 'REPLACE'
      File_Action = 'WRITE'

    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK THE DATA FILE IF REQUIRED --                 #
    !#--------------------------------------------------------------------------#

    IF ( File_Check ) THEN

      Error_Status = Check_Binary_File( TRIM( Filename ), &
                                        Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error checking ", a )' ) &
                        TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- OPEN THE DATA FILE --                          #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a free unit number
    ! ----------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = TRIM( File_Status ), &
                  ACTION = TRIM( File_Action ), &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'UNFORMATTED', &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- SKIP PAST, OR WRITE THE MAGIC NUMBER --               #
    !#--------------------------------------------------------------------------#

    IF ( File_Input ) THEN

      READ( FileID, IOSTAT = IO_Status ) Magic_Number_Read

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading magic number from ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    ELSE

      WRITE( FileID, IOSTAT = IO_Status ) MAGIC_NUMBER

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing magic number to ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END IF
    
  END FUNCTION Open_Binary_File

!--------------------------------------------------------------------------------
  FUNCTION Open_Text_File( Filename,     &  ! Input
                             FileID,       &  ! Output
                             For_Output,   &  ! Optional input
                             No_Check,     &  ! Optional input
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS -- #
    !#--------------------------------------------------------------------------#
    ! ---------
    ! Arguments
    ! ---------
    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    ! -- Output
    INTEGER,                  INTENT( OUT ) :: FileID
    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: For_Output
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_Check
    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log
    ! ---------------
    ! Function result
    ! ---------------
    INTEGER :: Error_Status
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_Text_File'
    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER( 256 ) :: Message
    LOGICAL :: File_Check
    LOGICAL :: File_Input
    INTEGER :: IO_Status
    CHARACTER( 7 ) :: File_Status
    CHARACTER( 5 ) :: File_Action
    !#--------------------------------------------------------------------------#
    !#                   -- SET SUCCESSFUL RETURN STATUS -- #
    !#--------------------------------------------------------------------------#
    Error_Status = SUCCESS
    ! ------------------------------
    ! Is file to be read or written?
    ! ------------------------------
    ! -- Default action is to READ file
    File_Input = .TRUE.
    ! -- ...unless the For_Output keyword is set
    IF ( PRESENT( For_Output ) ) THEN
      IF ( For_Output == SET ) THEN
       File_Input = .FALSE.
        File_Check = .FALSE.
      END IF
    END IF
    !#--------------------------------------------------------------------------#
    !#                      -- CHECK DATA FILE EXISTENCE -- #
    !#--------------------------------------------------------------------------#
    IF ( File_Input ) THEN
      ! -------------------------------
      ! File is to be READ. If the file
      ! does not exist, return an error
      ! -------------------------------
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAMe, &
                              'File '//TRIM( Filename )//' not found.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
      ! -- Set OPEN keywords for READING
      File_Status = 'OLD'
      File_Action = 'READ'
    ELSE
      ! ------------------------------------
      ! File is to be WRITTEN. If the file
      ! does exist, output a warning message
      ! ------------------------------------
      IF ( File_Exists( TRIM( Filename ) ) ) THEN
        CALL Display_Message( ROUTINE_NAMe, &
                              'File '//TRIM( Filename )//' will be overwritten.', &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF
      ! -- Set OPEN keywords for WRITING
      File_Status = 'REPLACE'
      File_Action = 'WRITE'
    END IF
    !#--------------------------------------------------------------------------#
    !#                        -- OPEN THE DATA FILE -- #
    !#--------------------------------------------------------------------------#
    ! ----------------------
    ! Get a free unit number
    ! ----------------------
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    ! -------------
    ! Open the file
    ! -------------
    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = TRIM( File_Status ), &
                  ACTION = TRIM( File_Action ), &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
  END FUNCTION Open_Text_File


END MODULE Binary_File_Utility


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: Binary_File_Utility.f90,v 2.4 2004/08/27 19:49:43 paulv Exp $
!
! $Date: 2004/08/27 19:49:43 $
!
! $Revision: 2.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Binary_File_Utility.f90,v $
! Revision 2.4  2004/08/27 19:49:43  paulv
! - Corrected a brain-dead bug in how an input file's magic number is checked.
!   The file is now opened as direct access for the check to allow the magic
!   number to be read regardless of the endian-ness of the file.
!
! Revision 2.3  2004/08/20 01:13:22  paulv
! - Replaced local Swap_Endian() function with a call to the function of the
!   same name in the Endian_Utility module.
!
! Revision 2.2  2004/08/11 20:39:25  paulv
! - Updated header documentation.
!
! Revision 2.1  2004/07/01 17:55:11  paulv
! - Copy of Coeff_Binary_Utility and renamed.
!
! Revision 2.0  2003/10/22 18:57:49  paulv
! - New version for non-RTM Coeff files.
!
! Revision 1.11  2003/05/16 18:20:15  paulv
! - Altered logic in Open() function. Using logical variables rather than
!   checking integer values.
! - Removed module wide message variable. Using local variables instead.
!
! Revision 1.10  2002/07/24 14:54:39  paulv
! - Added use of TRIM intrinsic when using coefficient_file filename variable.
!
! Revision 1.9  2001/10/01 20:28:46  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.8  2001/08/16 16:39:54  paulv
! - Updated documentation
!
! Revision 1.7  2001/08/09 20:38:15  paulv
! - Changed magic number visibility attribute to PRIVATE. Ahhh....
! - Moved all the spectral and transmittance coefficient data type and name
!   definitions into their respective modules. Another ahhh.....
! - Added optional FOR_OUTPUT argument to OPEN_COEFFICIENT_FILE function
!   so that the same function can be used to open coefficient files for
!   writing. It also means the magic number write can be done in this module
!   totally encapsulating that functionality in this module only. Double ahhh....
!
! Revision 1.6  2001/08/01 16:43:05  paulv
! - Updated the definitions of data items and types in the transmittance
!   coefficient data file to reflect changes in code. The absorber space
!   levels are no longer calculated during model initialisation, but are
!   precalculated and stored in the transmittance coefficient data file.
!
! Revision 1.5  2001/07/12 16:58:18  paulv
! - Added USE of TYPE_KINDS module at top of this module. Previously it was
!   USEd only in the CHECK_COEFFICIENT_FILE() function.
! - Data file magic number definition now defined at top of module rather
!   than in the CHECK_COEFFICIENT_FILE() function.
! - Definitions for the number, type, and names of items in the transmittance
!   and spectral coefficient files moved from the TRANSMITTANCE_COEFFICIENTS
!   and SPECTRAL COEFFICIENTS module to this one. This was done to allow this
!   module to be used in both reading and writing/reformatting the coefficient
!   data files.
! - Module-wide error Message character string defined.
! - Added NO_CHECK optional argument to the OPEN_COEFFICIENT_FILE() function.
!   This was done to allow the function to be used to open the old format
!   coefficient files for reformatting by not performing a magic number check.
!
! Revision 1.4  2000/08/31 19:36:31  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.3  2000/08/24 15:22:10  paulv
! - File access changed from DIRECT to SEQUENTIAL. Record length argument
!   no longer required by OPEN_COEFFICIENT_FILE and CHECK_COEFFICIENT_FILE
!   subprograms.
! - INQUIRE statement in OPEN_COEFFICIENT_FILE that checks for existence
!   of the file replaced by function FILE_EXISTS in module FILE_UTILITY.
! - CHECK_COEFFICIENT_FILE used to return a WARNING status if either 8- or
!   32-bit integers were not supported. This condition now returns a
!   FAILURE status as the magic number would not be read so any subsequent
!   attempt to read data would either fail or return junk.
! - The name of the SWAP_ENDIAN_FOURBYTE_INTEGER subprogram was changed to
!   SWAP_ENDIAN_LONG_INTEGER to remove any indication of how many bytes are
!   expected for this data type *apart* from the definition of
!   N_BYTES_FOR_LONG_KIND in the TYPE_KINDS module.
! - Updated module and subprogram documentation.
!
! Revision 1.2  2000/08/08 17:05:45  paulv
! Cosmetic changes to highlight parameters in the source by making them
! uppercase.
!
! Revision 1.1  2000/07/12 16:08:10  paulv
! Initial checked in version
!
!
!

