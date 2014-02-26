!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_ChannelInfo_Define
!
! PURPOSE:
!       Module defining the CRTM ChannelInfo data structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE CRTM_ChannelInfo_Define
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds
!                            of variable types.
!
!       Error_Handler:       Module to define simple error codes and
!                            handle error conditions
!                            USEs: FILE_UTILITY module
!
! CONTAINS:
!       CRTM_Init_ChannelInfo:       Subroutine to initialize a ChannelInfo
!                                    structure.
!
!       CRTM_Destroy_ChannelInfo:    Function to re-initialize a ChannelInfo
!                                    structure.
!
!       CRTM_Allocate_ChannelInfo:   Function to allocate the pointer members
!                                    of an ChannelInfo structure.
!
!       CRTM_Assign_ChannelInfo:     Function to copy a valid ChannelInfo
!                                    structure.
!
! DERIVED TYPES:
!       CRTM_ChannelInfo_type:  Definition of the public CRTM_ChannelInfo data
!                               structure. Fields are,
!
!         n_Channels:        Total number of channels.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Sensor_Descriptor: A character string containing a description of the 
!                            satellite and sensor sensor name, e.g. hirs3_n17.
!                            UNITS:      None
!                            TYPE:       CHARACTER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!         NCEP_Sensor_ID:    The NCEP/EMC "in-house" value used to distinguish
!                            between different sensor/platform combinations.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!         WMO_Satellite_ID:  The WMO Satellite ID number taken from Common
!                            Code Table C-5 in documentation at
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!         WMO_Sensor_ID:     The WMO Sensor ID number taken from Common
!                            Code Table C-8 in documentation at
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!         Sensor_Channel:    The channel numbers for the sensor channels
!                            in the data structure.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!         Channel_Index:     The index of the ChannelInfo channels in the
!                            the array of channels loaded during the CRTM
!                            initialisation.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_ChannelInfo_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user initialize,
!       destroy, allocate, assign, and concatenate the structure
!       using only the routines in this module where possible to
!       eliminate -- or at least minimise -- the possibility of 
!       memory leakage since most of the structure members are
!       pointers.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
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

MODULE CRTM_ChannelInfo_Define


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

  ! -- Definition functions
  PUBLIC :: CRTM_Init_ChannelInfo
  PUBLIC :: CRTM_Destroy_ChannelInfo
  PUBLIC :: CRTM_Allocate_ChannelInfo
  PUBLIC :: CRTM_Assign_ChannelInfo


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_ChannelInfo_Define.f90,v 1.5 2004/06/29 20:09:04 paulv Exp $'

  ! -- ChannelInfo scalar invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Descriptor string length
  INTEGER, PRIVATE, PARAMETER :: SL = 20

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1



  ! -------------------------------
  ! ChannelInfo data type definition
  ! -------------------------------

  TYPE, PUBLIC :: CRTM_ChannelInfo_type
    INTEGER :: n_Allocates

    INTEGER :: n_Channels
    INTEGER :: Sensor_Descriptor_StrLen
    CHARACTER( SL ), DIMENSION( : ), POINTER :: Sensor_Descriptor
    INTEGER,         DIMENSION( : ), POINTER :: NCEP_Sensor_ID
    INTEGER,         DIMENSION( : ), POINTER :: WMO_Satellite_ID
    INTEGER,         DIMENSION( : ), POINTER :: WMO_Sensor_ID
    INTEGER,         DIMENSION( : ), POINTER :: Sensor_Channel
    INTEGER,         DIMENSION( : ), POINTER :: Channel_Index
  END TYPE CRTM_ChannelInfo_type


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ASSOCIATED, &
            TRIM


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Clear_ChannelInfo
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM ChannelInfo structure.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_ChannelInfo( ChannelInfo) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  ChannelInfo structure for which the scalar members have
!                     been cleared.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_ChannelInfo( ChannelInfo )

    TYPE( CRTM_ChannelInfo_type ), INTENT( OUT ) :: ChannelInfo

    ChannelInfo%n_Channels = 0
    ChannelInfo%Sensor_Descriptor_StrLen = SL

  END SUBROUTINE CRTM_Clear_ChannelInfo





!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Associated_ChannelInfo
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_ChannelInfo structure.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_ChannelInfo( ChannelInfo,        &  ! Input
!                                                         ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       ChannelInfo: ChannelInfo structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_ChannelInfo_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    ChannelInfo structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ChannelInfo pointer
!                            members.
!                            .TRUE.  - if ALL the ChannelInfo pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the ChannelInfo
!                                      pointer members are associated.
!                            .FALSE. - some or all of the ChannelInfo pointer
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
!       This function tests the association status of the CRTM_ChannelInfo
!       structure pointer members. Therefore this function must only be
!       called after the input CRTM_ChannelInfo structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_ChannelInfo( ChannelInfo, & ! Input
                                        ANY_Test )   & ! Optional input
                                      RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN ) :: ChannelInfo

    ! -- Optional input
    INTEGER,             OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( ChannelInfo%Sensor_Descriptor ) .AND. &
           ASSOCIATED( ChannelInfo%NCEP_Sensor_ID    ) .AND. &
           ASSOCIATED( ChannelInfo%WMO_Satellite_ID  ) .AND. &
           ASSOCIATED( ChannelInfo%WMO_Sensor_ID     ) .AND. &
           ASSOCIATED( ChannelInfo%Sensor_Channel    ) .AND. &
           ASSOCIATED( ChannelInfo%Channel_Index     )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ChannelInfo%Sensor_Descriptor ) .OR. &
           ASSOCIATED( ChannelInfo%NCEP_Sensor_ID    ) .OR. &
           ASSOCIATED( ChannelInfo%WMO_Satellite_ID  ) .OR. &
           ASSOCIATED( ChannelInfo%WMO_Sensor_ID     ) .OR. &
           ASSOCIATED( ChannelInfo%Sensor_Channel    ) .OR. &
           ASSOCIATED( ChannelInfo%Channel_Index     )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_ChannelInfo





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Init_ChannelInfo
! 
! PURPOSE:
!       Function to initialize the scalar and pointer members of a CRTM
!       ChannelInfo data structures.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL CRTM_Init_ChannelInfo( ChannelInfo,    &  ! Output
!                                   RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  Initialized ChannelInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This subroutine nullifies the ChannelInfo structure pointer members.
!       Therefore, this function should *only* be called to initialise
!       ChannelInfo structures before their *first* use. Subsequent
!       re-initialisations should be done using the Destroy_ChannelInfo()
!       function.
!       
! PROCEDURE:
!       The scalar structure members are set to an "invalid" value and the 
!       pointer members are nullified.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Init_ChannelInfo( ChannelInfo, &  ! Output
                                    RCS_Id       )  ! Revision control )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    TYPE( CRTM_ChannelInfo_type ), INTENT( OUT ) :: ChannelInfo

    CHARACTER( * ),      OPTIONAL, INTENT( OUT ) :: RCS_Id



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! Initialise the allocation counter. This
    ! is only done for initialisation, not in
    ! the Clear() function
    ! ---------------------------------------

    ChannelInfo%n_Allocates = 0


    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL CRTM_Clear_ChannelInfo( ChannelInfo )


    ! ---------------------------
    ! Nullify the pointer members
    ! ---------------------------

    NULLIFY( ChannelInfo%Sensor_Descriptor, &
             ChannelInfo%NCEP_Sensor_ID, &
             ChannelInfo%WMO_Satellite_ID, &
             ChannelInfo%WMO_Sensor_ID, &
             ChannelInfo%Sensor_Channel, &
             ChannelInfo%Channel_Index )

  END SUBROUTINE CRTM_Init_ChannelInfo





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_ChannelInfo
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       ChannelInfo data structures.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo,              &  ! Output
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
! 
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  Re-initialized ChannelInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar or Rank-1
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
!                     The error codes are defined in the ERROR_HANDLER module.
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
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the ChannelInfo structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the ChannelInfo structure has been initialised via the
!       CRTM_Init_ChannelInfo() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the ChannelInfo structure pointer
!       members will be undefined until they are initialised (via the
!       CRTM_Init_ChannelInfo() subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_ChannelInfo( ChannelInfo,  &  ! Output
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
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),      OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_ChannelInfo'


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
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL CRTM_Clear_ChannelInfo( ChannelInfo )


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the ChannelInfo Sensor_Descriptor member
    IF ( ASSOCIATED( ChannelInfo%Sensor_Descriptor ) ) THEN

      DEALLOCATE( ChannelInfo%Sensor_Descriptor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_ChannelInfo Sensor_Descriptor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the ChannelInfo NCEP_Sensor_ID member
    IF ( ASSOCIATED( ChannelInfo%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( ChannelInfo%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_ChannelInfo NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the ChannelInfo WMO_Satellite_ID member
    IF ( ASSOCIATED( ChannelInfo%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( ChannelInfo%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_ChannelInfo WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the ChannelInfo WMO_Sensor_ID member
    IF ( ASSOCIATED( ChannelInfo%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( ChannelInfo%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_ChannelInfo WMO_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the ChannelInfo Sensor_Channel member
    IF ( ASSOCIATED( ChannelInfo%Sensor_Channel ) ) THEN

      DEALLOCATE( ChannelInfo%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_ChannelInfo Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the ChannelInfo Channel_Index member
    IF ( ASSOCIATED( ChannelInfo%Channel_Index ) ) THEN

      DEALLOCATE( ChannelInfo%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_ChannelInfo Channel_Index ", &
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

    ChannelInfo%n_Allocates = ChannelInfo%n_Allocates - 1

    IF ( ChannelInfo%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ChannelInfo%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_ChannelInfo





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_ChannelInfo
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM ChannelInfo
!       data structure.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_ChannelInfo( n_Channels,               &  ! Input
!                                                 ChannelInfo,              &  ! Output
!                                                 RCS_Id = RCS_Id,          &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   The number of channels in the ChannelInfo structure.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  ChannelInfo structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
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
!                     The error codes are defined in the ERROR_HANDLER module.
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
!       CRTM_Associated_ChannelInfo:  Function to test the association status
!                                     of the pointer members of a CRTM
!                                     ChannelInfo structure.
!
!       Display_Message:              Subroutine to output messages
!                                     SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the ChannelInfo structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the ChannelInfo structure has been initialised via the
!       CRTM_Init_ChannelInfo() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the ChannelInfo structure pointer
!       members will be undefined until they are initialised (via the 
!       CRTM_Init_ChannelInfo() subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_ChannelInfo( n_Channels,   &  ! Input
                                      ChannelInfo,  &  ! Output
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
    INTEGER,                       INTENT( IN )  :: n_Channels

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ), INTENT( OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),      OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_ChannelInfo'


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

    ! ----------------------
    ! The number of channels
    ! ----------------------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! Check if ANY pointers are already associated
    ! --------------------------------------------

    IF ( CRTM_Associated_ChannelInfo( ChannelInfo, ANY_Test = SET ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'One or more CRTM_ChannelInfo pointer members are associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( ChannelInfo%Sensor_Descriptor( n_Channels ), &
              ChannelInfo%NCEP_Sensor_ID( n_Channels ), &
              ChannelInfo%WMO_Satellite_ID( n_Channels ), &
              ChannelInfo%WMO_Sensor_ID( n_Channels ), &
              ChannelInfo%Sensor_Channel( n_Channels ), &
              ChannelInfo%Channel_Index( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_ChannelInfo data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE n_Channels DIMENSION --                 #
    !#--------------------------------------------------------------------------#

    ChannelInfo%n_Channels = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ChannelInfo%n_Allocates = ChannelInfo%n_Allocates + 1

    IF ( ChannelInfo%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ChannelInfo%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_ChannelInfo





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_ChannelInfo
!
! PURPOSE:
!       Function to copy valid CRTM ChannelInfo structures.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_ChannelInfo( ChannelInfo_in,           &  ! Input
!                                               ChannelInfo_out,          &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ChannelInfo_in:  ChannelInfo structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo_out: Copy of the input structure, ChannelInfo_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_ChannelInfo:  Function to test the association status
!                                     of the pointer members of a CRTM
!                                     ChannelInfo structure.
!
!
!       CRTM_Allocate_ChannelInfo:    Function to allocate the pointer members
!                                     of a CRTM ChannelInfo structure.
!
!       Display_Message:              Subroutine to output Messages
!                                     SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function allocates the output ChannelInfo structure pointer members.
!       Therefore this function should *only* be called *after* the output
!       ChannelInfo structure has been initialised via the CRTM_Init_ChannelInfo()
!       subroutine or re-initialised via the CRTM_Destroy_ChannelInfo() function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_ChannelInfo( ChannelInfo_in,  &  ! Input
                                    ChannelInfo_out, &  ! Output
                                    RCS_Id,          &  ! Revision control
                                    Message_Log )    &  ! Error messaging
                                  RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN )  :: ChannelInfo_in

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ), INTENT( OUT ) :: ChannelInfo_out

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),      OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_ChannelInfo'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. CRTM_Associated_ChannelInfo( ChannelInfo_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CRTM_ChannelInfo pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! ANY *output* pointers must NOT be associated
    ! --------------------------------------------

    IF ( CRTM_Associated_ChannelInfo( ChannelInfo_Out, ANY_Test = SET ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all OUTPUT CRTM_ChannelInfo pointer '//&
                            'members are associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = CRTM_Allocate_ChannelInfo( ChannelInfo_in%n_Channels, &
                                              ChannelInfo_out, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output CRTM_ChannelInfo arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    ChannelInfo_out%Sensor_Descriptor = ChannelInfo_in%Sensor_Descriptor
    ChannelInfo_out%NCEP_Sensor_ID    = ChannelInfo_in%NCEP_Sensor_ID
    ChannelInfo_out%WMO_Sensor_ID     = ChannelInfo_in%WMO_Sensor_ID
    ChannelInfo_out%WMO_Satellite_ID  = ChannelInfo_in%WMO_Satellite_ID
    ChannelInfo_out%Sensor_Channel    = ChannelInfo_in%Sensor_Channel
    ChannelInfo_out%Channel_Index     = ChannelInfo_in%Channel_Index

  END FUNCTION CRTM_Assign_ChannelInfo

END MODULE CRTM_ChannelInfo_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_ChannelInfo_Define.f90,v 1.5 2004/06/29 20:09:04 paulv Exp $
!
! $Date: 2004/06/29 20:09:04 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_ChannelInfo_Define.f90,v $
! Revision 1.5  2004/06/29 20:09:04  paulv
! - Separated the definition and application code into separate modules.
!
! Revision 1.4  2004/06/24 18:59:39  paulv
! - Removed code that triggered an error in the indexing function if the
!   user inputs more channels than are available.
!
! Revision 1.3  2004/06/15 21:58:39  paulv
! - Corrected some minor indexing and declaration bugs.
!
! Revision 1.2  2004/06/15 21:43:41  paulv
! - Added indexing functions.
! - Renamed module from CRTM_ChannelInfo_Define.
!
! Revision 1.1  2004/05/19 19:55:18  paulv
! Initial checkin.
!
!
!
!
