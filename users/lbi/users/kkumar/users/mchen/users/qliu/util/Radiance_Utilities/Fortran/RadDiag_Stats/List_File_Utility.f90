!------------------------------------------------------------------------------
!M+
! NAME:
!       List_File_Utility
!
! PURPOSE:
!       Module containing routines for reading list files, i.e. ASCII files
!       that contain lists of character or integer data, one item per line.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE List_File_Utility
!
! MODULES:
!       Type_Kinds:               Module containing definitions for kinds
!                                 of variable types.
!
!       File_Utility:             Module containing generic file utility routines
!
!       Message_Handler:        Module to define simple error/exit codes
!                                 and output messages.
!                                 USEs: FILE_UTILITY module
!
! CONTAINS:
!       PUBLIC routines
!       ---------------
!
!       Read_List_File:           Function to read a list file and populate 
!                                 a List File structure.
!
!       Get_List_Size:            Fucntion to return the size of the List File
!                                 structure.
!
!       Get_List_Entry:           Function to return requested entries in a 
!                                 populated List File structure.
!
!       PRIVATE routines
!       ----------------
!       Clear_List:               Subroutine to clear the scalar members of a
!                                 List_File structure.
!
!       Associated_List:          Function to test the association status of
!                                 the pointer members of a List File structure.
!
!       Destroy_List:             Function to re-initialize the scalar and
!                                 pointer members of List File data structures.
!
!       Allocate_List:            Function to allocate the pointer members of
!                                 the List File data structure.
!
!       Open_List_File:           Function to open a list file for reading.
!
!       Count_List_File_Entries:  Function to count the number of entries in a
!                                 list file
!
! DERIVED TYPES:
!       All derived types defined in this module are declared PUBLIC but 
!       with PRIVATE components. The public routines above must be used to
!       access the contents of the structures.
!
!       Character_List_File_type:   Definition of the List File structure for
!                                   character data. Fields are:
!
!         n_Entries:                     Number of list file entries.
!                                        UNITS:      N/A
!                                        TYPE:       INTEGER
!                                        DIMENSION:  Scalar
!
!         Entry:                         Array of list file entries.
!                                        UNITS:      N/A
!                                        TYPE:       CHARACTER(5000)
!                                        DIMENSION:  Rank-1 (n_Entries)
!                                        ATTRIBUTES: POINTER
!
!
!       Integer_List_File_type:     Definition of the List File structure for
!                                   integer data. Fields are:
!
!         n_Entries:                     Number of list file entries.
!                                        UNITS:      N/A
!                                        TYPE:       INTEGER
!                                        DIMENSION:  Scalar
!
!         Entry:                         Array of list file entries.
!                                        UNITS:      N/A
!                                        TYPE:       INTEGER
!                                        DIMENSION:  Rank-1 (n_Entries)
!                                        ATTRIBUTES: POINTER
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
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Feb-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003, 2004 Paul van Delst
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


MODULE List_File_Utility


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Read_List_File
  PUBLIC :: Get_List_Size
  PUBLIC :: Get_List_Entry


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Read_List_File
    MODULE PROCEDURE Read_Character_List_File
    MODULE PROCEDURE Read_Integer_List_File
  END INTERFACE Read_List_File

  INTERFACE Get_List_Size
    MODULE PROCEDURE Get_Character_List_Size
    MODULE PROCEDURE Get_Integer_List_Size
  END INTERFACE Get_List_Size

  INTERFACE Get_List_Entry
    MODULE PROCEDURE Get_Character_List_Entry
    MODULE PROCEDURE Get_Integer_List_Entry
  END INTERFACE Get_List_Entry


  ! -----------------
  ! Module parameters
  ! -----------------

  CHARACTER( * ), PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id$'

  INTEGER, PRIVATE, PARAMETER :: SET = 1
  INTEGER, PRIVATE, PARAMETER :: STRLEN = 5000


  ! -------------------------
  ! Derived type declarations
  ! -------------------------

  TYPE, PUBLIC :: Character_List_File_type
    PRIVATE
    INTEGER :: n_Allocates = 0
    INTEGER :: String_Length = STRLEN
    INTEGER :: n_Entries = 0
    CHARACTER( STRLEN ), DIMENSION(:), POINTER :: Entry => NULL()
  END TYPE Character_List_File_type

  TYPE, PUBLIC :: Integer_List_File_type
    PRIVATE
    INTEGER :: n_Allocates = 0
    INTEGER :: n_Entries = 0
    INTEGER, DIMENSION(:), POINTER :: Entry => NULL()
  END TYPE Integer_List_File_type


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_List
!
! PURPOSE:
!       Subroutine to clear the scalar members of a List_File structure.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_List( List ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       List:        File List structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       Character_List_File_type
!                                  OR
!                                Integer_List_File_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output List argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Character_List( List )
    TYPE( Character_List_File_type ), INTENT( IN OUT ) :: List
    List%n_Entries  = 0
  END SUBROUTINE Clear_Character_List

  SUBROUTINE Clear_Integer_List( List )
    TYPE( Integer_List_File_type ), INTENT( IN OUT ) :: List
    List%n_Entries  = 0
  END SUBROUTINE Clear_Integer_List





!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_List
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       List File structure.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_List( List,               &  ! Input
!                                             ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       List:                List structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       Character_List_File_type 
!                                          OR                     
!                                        Integer_List_File_type   
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            List structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the List pointer members.
!                            .TRUE.  - if ALL the List pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the List pointer
!                                      members are associated.
!                            .FALSE. - some or all of the List pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_Character_List( List,      & ! Input
                                      ANY_Test ) & ! Optional input
                                    RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Character_List_File_type ), INTENT( IN ) :: List

    ! -- Optional input
    INTEGER,                OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      ! -- Here the test will use .AND. when more members are added
      IF ( ASSOCIATED( List%Entry ) ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      ! -- Here the test will use .OR. when more members are added
      IF ( ASSOCIATED( List%Entry ) ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Character_List

  FUNCTION Associated_Integer_List( List,      & ! Input
                                      ANY_Test ) & ! Optional input
                                    RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Integer_List_File_type ), INTENT( IN ) :: List

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      ! -- Here the test will use .AND. when more members are added
      IF ( ASSOCIATED( List%Entry ) ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      ! -- Here the test will use .OR. when more members are added
      IF ( ASSOCIATED( List%Entry ) ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Integer_List





!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_List
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of List File
!       data structures.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_List( List,                     &  ! Output
!                                    RCS_Id = RCS_Id,          &  ! Revision control
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       List:         Re-initialized List structure.
!                     UNITS:      N/A
!                     TYPE:       Character_List_File_type 
!                                   OR                     
!                                 Integer_List_File_type   
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output List argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Character_List( List,         &  ! Output
                                   No_Clear,     &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( Character_List_File_type ), INTENT( IN OUT ) :: List

    ! -- Optional input
    INTEGER,                OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),         OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),         OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_List(Character)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_Character_List( List )


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Entry list
    IF ( ASSOCIATED( List%Entry ) ) THEN

      DEALLOCATE( List%Entry, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating List Entry ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    List%n_Allocates = List%n_Allocates - 1

    IF ( List%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      List%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Character_List

  FUNCTION Destroy_Integer_List( List,         &  ! Output
                                   No_Clear,     &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( Integer_List_File_type ), INTENT( IN OUT ) :: List

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_List(Integer)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_Integer_List( List )


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Entry list
    IF ( ASSOCIATED( List%Entry ) ) THEN

      DEALLOCATE( List%Entry, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating List Entry ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    List%n_Allocates = List%n_Allocates - 1

    IF ( List%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      List%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Integer_List





!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_List
! 
! PURPOSE:
!       Function to allocate the pointer members of the List File
!       data structure.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_List( n_Entries,                &  ! Input
!                                     List,                     &  ! Output
!                                     RCS_Id      = RCS_Id,     &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Entries:    Required dimension of List File structure pointer
!                     members.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       List:     List structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       Character_List_File_type 
!                                   OR                     
!                                 Integer_List_File_type   
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure pointer allocations were
!                                   successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_List:      Function to test the association status of the
!                             pointer members of a List structure.
!
!       Destroy_List:         Function to re-initialize the scalar and pointer
!                             members of List data structures.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output List argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Character_List( n_Entries,    &  ! Input
                                    List,         &  ! Output
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                          INTENT( IN )     :: n_Entries

    ! -- Output
    TYPE( Character_List_File_type ), INTENT( IN OUT ) :: List

    ! -- Revision control
    CHARACTER( * ),         OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),         OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_List(Character)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Spectral dimension
    ! ------------------

    IF ( n_Entries < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Entries must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_Character_List( List, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_Character_List( List, &
                                             No_Clear = SET, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating List pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( List%Entry( n_Entries ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating List data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    List%n_Entries = n_Entries



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    List%n_Allocates = List%n_Allocates + 1

    IF ( List%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      List%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Character_List

  FUNCTION Allocate_Integer_List( n_Entries,    &  ! Input
                                  List,         &  ! Output
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                        INTENT( IN )     :: n_Entries

    ! -- Output
    TYPE( Integer_List_File_type ), INTENT( IN OUT ) :: List

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_List(Integer)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Spectral dimension
    ! ------------------

    IF ( n_Entries < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Entries must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_Integer_List( List, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_Integer_List( List, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating List pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( List%Entry( n_Entries ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating List data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    List%n_Entries = n_Entries



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    List%n_Allocates = List%n_Allocates + 1

    IF ( List%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      List%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Integer_List





!------------------------------------------------------------------------------
!
! NAME:
!       Open_List_File
!
! PURPOSE:
!       Function to open a list file for reading.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Open_List_File( List_Filename,            &  ! Input
!                                      List_FileID,              &  ! Output
!                                      RCS_Id      = RCS_Id,     &  ! Optional output
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       List_Filename:   Character string specifying the name of the list file
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       List_FileID:     List file logical unit number.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the Message_Handler module. Values returned by
!                        this function are:
!                          SUCCESS == file open was successful
!                          FAILURE == an error occurred opening the file.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:  Subroutine to output Messages
!                        SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Feb-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Open_List_File( List_Filename, &  ! Input
                           List_FileID,   &  ! Output
                           RCS_Id,        &  ! Optional output
                           Message_Log )  &  ! Error messaging
                         RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: List_Filename

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: List_FileID

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_List_File'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: Lun



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK ARGUMENTS --                           #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( .NOT. File_Exists( TRIM( List_Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAMe, &
                            'List file '//TRIM( List_Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- OPEN THE LIST FILE --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a file unit number
    ! ----------------------

    Lun = Get_Lun()

    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining unit number for list file '//TRIM( List_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( Lun, FILE   = TRIM( List_Filename ), &
               STATUS = 'OLD', &
               ACCESS = 'SEQUENTIAL', &
               FORM   = 'FORMATTED', &
               ACTION = 'READ', &
               IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening list file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( List_Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    

    !#--------------------------------------------------------------------------#
    !#                                -- DONE --                                #
    !#--------------------------------------------------------------------------#

    List_FileID = Lun

  END FUNCTION Open_List_File





!------------------------------------------------------------------------------
!
! NAME:
!       Count_List_File_Entries
!
! PURPOSE:
!       Function to count the number of entries in a list file
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Count_List_File_Entries( List_Filename,            &  ! Input
!                                               n_Entries,                &  ! Output
!                                               RCS_Id      = RCS_Id,     &  ! Optional output
!                                               Message_Log = Message_Log ) ! Error messaging
!
! INPUT ARGUMENTS:
!       List_Filename:   Character string specifying the name of the list file
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       n_Entries:       The number of entries in the list file.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the Message_Handler module. Values returned by
!                        this function are:
!                          SUCCESS == the list file entry count was successful.
!                          FAILURE == an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output Messages
!                          SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Feb-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Count_List_File_Entries( List_Filename, &  ! Input
                                    n_Entries,     &  ! Output
                                    RCS_Id,        &  ! Optional output
                                    Message_Log )  &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: List_Filename

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: n_Entries

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Count_List_File_Entries'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( STRLEN ) :: Line_Buffer
    INTEGER :: IO_Status
    INTEGER :: List_FileID
    INTEGER :: Line_Read_Count
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK ARGUMENTS --                           #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- OPEN THE LIST FILE --                        #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_List_File( List_Filename, &
                                   List_FileID, &
                                   Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAMe, &
                            'Error opening list file '//TRIM( List_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- COUNT THE ENTRIES --                         #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Initialise counter
    ! ------------------

    Line_Read_Count = 0
    n = 0


    ! ---------------
    ! Begin open loop
    ! ---------------

    Count_Entries_Loop: DO

      ! -- Increment line read count
      Line_Read_Count = Line_Read_Count + 1

      ! -- Read a line of the file
      READ( List_FileID, FMT    = '( a )',  &
                         IOSTAT = IO_Status ) Line_Buffer

      ! -- Check for an error
      IF ( IO_Status > 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading list file at line #", i5, ". IOSTAT = ", i5 )' ) &
                        Line_Read_Count, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( List_FileID )
        RETURN
      END IF

      ! -- Check for end-of-file
      IF ( IO_Status < 0 ) THEN
        CLOSE( List_FileID )
        EXIT Count_Entries_Loop
      END IF

      ! -- Update entry counter if this is NOT a comment or blank line
      IF ( Line_Buffer(1:1) /= '!' .AND. LEN_TRIM(Line_Buffer) /= 0 ) THEN
        n = n + 1
      END IF

    END DO Count_Entries_Loop


    ! ----------------------------
    ! Assign the final entry count
    ! ----------------------------

    n_Entries = n


  END FUNCTION Count_List_File_Entries






!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_List_File
!
! PURPOSE:
!       Function to read a list file.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_List_File( List_Filename,           &  ! Input
!                                      List,                     &  ! Output
!                                      RCS_Id      = RCS_Id,     &  ! Optional output
!                                      Message_Log = Message_Log ) ! Error messaging
!
! INPUT ARGUMENTS:
!       List_Filename:  Character string specifying the name of the list file
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       Messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output Messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       List:           List file structure containing the list file entries
!                       on exit.
!                       UNITS:      N/A
!                       TYPE:       Character_List_File_type 
!                                     OR                     
!                                   Integer_List_File_type   
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the Message_Handler module. Values returned by
!                        this function are:
!                          SUCCESS == the list file read was successful.
!                          FAILURE == an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:   Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       - Maximum line length that can be read is 5000 characters. If there
!         is any data beyond this limit, the line is considered blank.
!
!       - Any line in list file with a "!" character in the first column
!         is treated as a comment line and is not read. Similarly for
!         blank lines.
!
! COMMENTS:
!       Note the INTENT on the output List argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Character_List_File( List_Filename, &  ! Input
                                     List,          &  ! Output
                                     RCS_Id,        &  ! Optional output
                                     Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                   INTENT( IN )     :: List_Filename

    ! -- Output
    TYPE( Character_List_File_type ), INTENT( IN OUT ) :: List

    ! -- Optional output
    CHARACTER( * ),         OPTIONAL, INTENT( OUT )    :: RCS_Id
  
    ! -- Error handler Message log
    CHARACTER( * ),         OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_List_File(Character)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( STRLEN ) :: Line_Buffer
    INTEGER :: IO_Status
    INTEGER :: List_FileID
    INTEGER :: Line_Read_Count
    INTEGER :: n, n_Entries



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK ARGUMENTS --                           #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- ALLOCATE THE OUTPUT List ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Count the number of list file entries
    ! -------------------------------------

    Error_Status = Count_List_File_Entries( TRIM( List_Filename ), &
                                            n_Entries, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error counting entries in the list file '//TRIM( List_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------
    ! Allocate the list structure
    ! ---------------------------

    Error_Status  = Allocate_Character_List( n_Entries, &
                                             List, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating list structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- OPEN THE LIST FILE FOR READING --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_List_File( List_Filename, &
                                   List_FileID, &
                                   Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening list file '//TRIM( List_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE ENTRIES --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Initialise counters
    ! -------------------

    Line_Read_Count = 0
    n = 0


    ! ---------------
    ! Begin open loop
    ! ---------------

    Read_Entries_Loop: DO

      ! -- Increment line read count
      Line_Read_Count = Line_Read_Count + 1

      ! -- Read a line of the file
      Line_Buffer = ' '
      READ( List_FileID, FMT    = '( a )',  &
                         IOSTAT = IO_Status ) Line_Buffer

      ! -- Check for an error
      IF ( IO_Status > 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading list file at line #", i5, ". IOSTAT = ", i5 )' ) &
                        Line_Read_Count, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        Error_Status = Destroy_Character_List( List )
        CLOSE( List_FileID )
        RETURN
      END IF

      ! -- Check for end-of-file
      IF ( IO_Status < 0 ) THEN
        CLOSE( List_FileID )
        EXIT Read_Entries_Loop
      END IF

      ! -- Update entry counter and list if this is NOT a comment or blank line
      IF ( Line_Buffer(1:1) /= '!' .AND. LEN_TRIM(Line_Buffer) /= 0 ) THEN

        n = n + 1

        ! -- Too many list entries!?
        IF ( n > n_Entries ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Number of list entries, ", i5, &
                            &", is greater than the size of the list structure, ", i5, "." )' ) &
                          n, n_Entries
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          Error_Status = Destroy_Character_List( List )
          CLOSE( List_FileID )
          RETURN
        END IF

        ! -- Assign the entry
        List%Entry( n ) = TRIM( ADJUSTL( Line_Buffer ) )

      END IF

    END DO Read_Entries_Loop

  END FUNCTION Read_Character_List_File

  FUNCTION Read_Integer_List_File( List_Filename, &  ! Input
                                     List,          &  ! Output
                                     RCS_Id,        &  ! Optional output
                                     Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                 INTENT( IN )     :: List_Filename

    ! -- Output
    TYPE( Integer_List_File_type ), INTENT( IN OUT ) :: List

    ! -- Optional output
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id
  
    ! -- Error handler Message log
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_List_File(Integer)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( STRLEN ) :: Line_Buffer
    INTEGER :: IO_Status
    INTEGER :: List_FileID
    INTEGER :: Line_Read_Count
    INTEGER :: n, n_Entries



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK ARGUMENTS --                           #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- ALLOCATE THE OUTPUT List ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Count the number of list file entries
    ! -------------------------------------

    Error_Status = Count_List_File_Entries( TRIM( List_Filename ), &
                                            n_Entries, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error counting entries in the list file '//TRIM( List_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------
    ! Allocate the list structure
    ! ---------------------------

    Error_Status  = Allocate_Integer_List( n_Entries, &
                                           List, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating list structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- OPEN THE LIST FILE FOR READING --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_List_File( List_Filename, &
                                   List_FileID, &
                                   Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening list file '//TRIM( List_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE ENTRIES --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Initialise counters
    ! -------------------

    Line_Read_Count = 0
    n = 0


    ! ---------------
    ! Begin open loop
    ! ---------------

    Read_Entries_Loop: DO

      ! -- Increment line read count
      Line_Read_Count = Line_Read_Count + 1

      ! -- Read a line of the file
      Line_Buffer = ' '
      READ( List_FileID, FMT    = '( a )',  &
                         IOSTAT = IO_Status ) Line_Buffer

      ! -- Check for an error
      IF ( IO_Status > 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading list file at line #", i5, ". IOSTAT = ", i5 )' ) &
                        Line_Read_Count, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        Error_Status = Destroy_Integer_List( List )
        CLOSE( List_FileID )
        RETURN
      END IF

      ! -- Check for end-of-file
      IF ( IO_Status < 0 ) THEN
        CLOSE( List_FileID )
        EXIT Read_Entries_Loop
      END IF

      ! -- Update entry counter and list if this is NOT a comment or blank line
      IF ( Line_Buffer(1:1) /= '!' .AND. LEN_TRIM(Line_Buffer) /= 0 ) THEN

        n = n + 1

        ! -- Too many list entries!?
        IF ( n > n_Entries ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Number of list entries, ", i5, &
                            &", is greater than the size of the list structure, ", i5, "." )' ) &
                          n, n_Entries
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          Error_Status = Destroy_Integer_List( List )
          CLOSE( List_FileID )
          RETURN
        END IF

        ! -- Assign the entry
        Line_Buffer = ADJUSTL( Line_Buffer )
        READ( Line_Buffer, '( i10 )' ) List%Entry( n )

      END IF

    END DO Read_Entries_Loop

  END FUNCTION Read_Integer_List_File




!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_List_Size
!
! PURPOSE:
!       Function to return the size fo a List File structure
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       n_Entries = Get_List_Size( List )
!
! INPUT ARGUMENTS:
!       List:           List file structure.
!                       UNITS:      N/A
!                       TYPE:       Character_List_File_type 
!                                     OR                     
!                                   Integer_List_File_type   
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       n_Entries:       The number of entries in the list file structure.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!      None.
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Get_Character_List_Size( List ) RESULT ( n_Entries )
    TYPE( Character_List_File_type ), INTENT( IN ) :: List
    INTEGER :: n_Entries

    n_Entries = List%n_Entries

  END FUNCTION Get_Character_List_Size

  FUNCTION Get_Integer_List_Size( List ) RESULT ( n_Entries )
    TYPE( Integer_List_File_type ), INTENT( IN ) :: List
    INTEGER :: n_Entries

    n_Entries = List%n_Entries

  END FUNCTION Get_Integer_List_Size





!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_List_Entry
!
! PURPOSE:
!       Function to return entries from a list file structure.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Get_List_Entry( List,                     &  ! Input
!                                      n,                        &  ! Input
!                                      Entry,                    &  ! Output
!                                      RCS_Id      = RCS_Id,     &  ! Optional output
!                                      Message_Log = Message_Log ) ! Error messaging
!
! INPUT ARGUMENTS:
!       List:           List file structure.
!                       UNITS:      N/A
!                       TYPE:       Character_List_File_type 
!                                     OR                     
!                                   Integer_List_File_type   
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       n:              The list file structure entry to retrieve
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       Messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output Messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Entry:          The retrieved list file entry.
!                       UNITS:      N/A
!                       TYPE:       Depends on List input argument.
!                                     If:   List  is TYPE(Character_List_File_type)
!                                     Then: Entry is CHARACTER(*)
!                                       OR                     
!                                     If:   List  is TYPE(Integer_List_File_type)
!                                     Then: Entry is INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the Message_Handler module. Values returned by
!                        this function are:
!                          SUCCESS == the list structure retrieval was successful.
!                          FAILURE == an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Get_Character_List_Entry( List, n,       &  ! Input
                                     Entry,         &  ! Output
                                     RCS_Id,        &  ! Optional output
                                     Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Character_List_File_type ), INTENT( IN )  :: List
    INTEGER,                          INTENT( IN )  :: n

    ! -- Output
    CHARACTER( * ),                   INTENT( OUT ) :: Entry

    ! -- Optional output
    CHARACTER( * ),         OPTIONAL, INTENT( OUT ) :: RCS_Id
  
    ! -- Error handler Message log
    CHARACTER( * ),         OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_List_Entry(Character)'



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK ARGUMENTS --                           #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! -----------------------------
    ! Is the requested entry valid?
    ! -----------------------------

    IF ( n < 1 .OR. n > List%n_Entries ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid entry number argument, n, specified.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE REQUESTED ENTRY --                    #
    !#--------------------------------------------------------------------------#

    Entry = List%Entry( n )

  END FUNCTION Get_Character_List_Entry

  FUNCTION Get_Integer_List_Entry( List, n,       &  ! Input
                                   Entry,         &  ! Output
                                   RCS_Id,        &  ! Optional output
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Integer_List_File_type ), INTENT( IN )  :: List
    INTEGER,                        INTENT( IN )  :: n

    ! -- Output
    INTEGER,                        INTENT( OUT ) :: Entry

    ! -- Optional output
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id
  
    ! -- Error handler Message log
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_List_Entry(Integer)'



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK ARGUMENTS --                           #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! -----------------------------
    ! Is the requested entry valid?
    ! -----------------------------

    IF ( n < 1 .OR. n > List%n_Entries ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid entry number argument, n, specified.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE REQUESTED ENTRY --                    #
    !#--------------------------------------------------------------------------#

    Entry = List%Entry( n )

  END FUNCTION Get_Integer_List_Entry

END MODULE List_File_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id$
!
! $Date: 2006/07/26 21:37:50 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: List_File_Utility.f90,v $
! Revision 2.3  2006/07/26 21:37:50  wd20pd
! Additional replacement of "Error_Handler" string with "Message_Handler"
! in documentaiton blocks.
!
! Revision 2.2  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 2.1  2006/03/29 21:02:19  paulv
! - Replaced Message_Handler module with Message_Handler module.
!
! Revision 2.0  2004/08/24 16:33:07  paulv
! - Totally revised to use structures to read the list file data.
!
! Revision 1.3  2004/07/01 17:40:10  paulv
! - Added function to get integer list file entries.
! - Overloaded the character and integer functions.
!
! Revision 1.2  2003/02/12 20:30:13  paulv
! - Added documentation.
!
! Revision 1.1  2003/02/07 19:32:40  paulv
! Initial checkin.
!
!
!
