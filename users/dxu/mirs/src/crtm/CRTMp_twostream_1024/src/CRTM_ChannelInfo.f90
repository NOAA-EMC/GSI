!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_ChannelInfo
!
! PURPOSE:
!       Module containgin routines to populate the CRTM ChannelInfo structure.
!       
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE CRTM_ChannelInfo
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_ChannelInfo_Define:    Module defining the CRTM ChannelInfo
!                                   data structure and containing routines
!                                   to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Index_ChannelInfo:      Function to populate the Channel_Index
!                                      component of a ChannelInfo structure.
!
!       PUBLIC INHERITED subprograms
!       ----------------------------
!         CRTM_Init_ChannelInfo:       Subroutine to initialize a ChannelInfo
!                                      structure.
!
!         CRTM_Destroy_ChannelInfo:    Function to re-initialize a ChannelInfo
!                                      structure.
!
!         CRTM_Allocate_ChannelInfo:   Function to allocate the pointer members
!                                      of an ChannelInfo structure.
!
!         CRTM_Assign_ChannelInfo:     Function to copy a valid ChannelInfo
!                                      structure.
!
!       PRIVATE subprograms
!       -------------------
!         None.
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

MODULE CRTM_ChannelInfo


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler

  USE CRTM_ChannelInfo_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
!  PRIVATE

  ! -- CRTM_ChannelInfo structure data type
  ! -- in the CRTM_ChannelInfo_Define module
  PUBLIC :: CRTM_ChannelInfo_type

  ! -- CRTM_ChannelInfo structure routines inherited
  ! -- from the CRTM_ChannelInfo_Define module
  ! -- Definition functions
  PUBLIC :: CRTM_Init_ChannelInfo
  PUBLIC :: CRTM_Destroy_ChannelInfo
  PUBLIC :: CRTM_Allocate_ChannelInfo
  PUBLIC :: CRTM_Assign_ChannelInfo

  ! -- This modules public routines
  PUBLIC :: CRTM_Index_ChannelInfo


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Index_ChannelInfo
    MODULE PROCEDURE IndexChannelInfo_DESC
    MODULE PROCEDURE IndexChannelInfo_NCEPID
  END INTERFACE ! CRTM_Index_ChannelInfo


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_ChannelInfo.f90,v 1.6 2004/07/01 20:52:51 paulv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################




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
!       CRTM_Index_ChannelInfo
!
! PURPOSE:
!       Function to populate the CRTM ChannelInfo structure Channel_Index
!       component
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_IndexChannelInfo( ChannelInfo_in,           &  ! Input
!                                             ChannelInfo_out,          &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
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
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
!
!
!       CRTM_Allocate_ChannelInfo:    Function to allocate the pointer members
!                                     of a CRTM ChannelInfo structure.
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION IndexChannelInfo_DESC( Master_Sensor_Descriptor, &  ! Input
                                  Master_Sensor_Channel,    &  ! Input
                                  User_Sensor_Descriptor,   &  ! Input
                                  User_Sensor_Channel,      &  ! Input
                                  ChannelInfo,              &  ! Output
                                  RCS_Id,                   &  ! Revision control
                                  Message_Log )             &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )  :: Master_Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )  :: Master_Sensor_Channel
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )  :: User_Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )  :: User_Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_IndexChannelInfo'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, n
    INTEGER :: n_Master_Channels
    INTEGER :: n_User_Channels
    INTEGER :: n_Matched_Channels
    INTEGER, DIMENSION( SIZE( Master_Sensor_Channel ) ) :: Channel_Index
    INTEGER, DIMENSION( 1 ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- CHECK INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    ! -- Check that Master inputs are the same size
    n_Master_Channels = SIZE( Master_Sensor_Descriptor )
    IF ( SIZE( Master_Sensor_Channel ) /= n_Master_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Master_Sensor_Descriptor and Master_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs are the same size
    n_User_Channels = SIZE( User_Sensor_Descriptor )
    IF ( SIZE( User_Sensor_Channel ) /= n_User_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User_Sensor_Descriptor and User_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs aren't zero-sized
    IF ( n_User_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User arrays must have sizes > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE MATCHING INDICES --                      #
    !#--------------------------------------------------------------------------#

    n_Matched_Channels = 0

    DO l = 1, n_User_Channels

      ! -- Count the matchups
      n = COUNT( Master_Sensor_Descriptor == User_Sensor_Descriptor(l) .AND. &
                 Master_Sensor_Channel    == User_Sensor_Channel(l)          )
 
      ! -- If none, go to next channel
      IF ( n == 0 ) CYCLE

      ! -- If more than one, Master arrays are screwy
      IF ( n > 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Multiple entries in Master arrays for the same '//&
                              'User specified channel.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Increment channel match counter
      n_Matched_Channels = n_Matched_Channels + 1
 
      ! -- Get the matching index
      Idx = PACK( (/ ( n, n = 1, n_Master_Channels ) /), &
                  Master_Sensor_Descriptor == User_Sensor_Descriptor(l) .AND. &
                  Master_Sensor_Channel    == User_Sensor_Channel(l)          )

      ! -- Save it
      Channel_Index( n_Matched_Channels ) = Idx(1)

    END DO

    ! -- Were ANY matches found?
    IF ( n_Matched_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'No Master/User array data matches were found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Allocate the ChannelInfo structure
    ! ----------------------------------

    Error_Status = CRTM_Allocate_ChannelInfo( n_Matched_Channels, &
                                              ChannelInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocation of ChannelInfo(1) structure failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Save the channel indices
    ! ------------------------

    ChannelInfo%Channel_Index = Channel_Index( 1:n_Matched_Channels )


  END FUNCTION IndexChannelInfo_DESC

  FUNCTION IndexChannelInfo_NCEPID( Master_NCEP_Sensor_ID, &  ! Input
                                    Master_Sensor_Channel, &  ! Input
                                    User_NCEP_Sensor_ID,   &  ! Input
                                    User_Sensor_Channel,   &  ! Input
                                    ChannelInfo,           &  ! Output
                                    RCS_Id,                &  ! Revision control
                                    Message_Log )          &  ! Error messaging
                                  RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,        DIMENSION( : ), INTENT( IN )  :: Master_NCEP_Sensor_ID
    INTEGER,        DIMENSION( : ), INTENT( IN )  :: Master_Sensor_Channel
    INTEGER,        DIMENSION( : ), INTENT( IN )  :: User_NCEP_Sensor_ID
    INTEGER,        DIMENSION( : ), INTENT( IN )  :: User_Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_IndexChannelInfo'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, n
    INTEGER :: n_Master_Channels
    INTEGER :: n_User_Channels
    INTEGER :: n_Matched_Channels
    INTEGER, DIMENSION( SIZE( Master_Sensor_Channel ) ) :: Channel_Index
    INTEGER, DIMENSION( 1 ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- CHECK INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    ! -- Check that Master inputs are the same size
    n_Master_Channels = SIZE( Master_NCEP_Sensor_ID )
    IF ( SIZE( Master_Sensor_Channel ) /= n_Master_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Master_NCEP_Sensor_ID and Master_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs are the same size
    n_User_Channels = SIZE( User_NCEP_Sensor_ID )
    IF ( SIZE( User_Sensor_Channel ) /= n_User_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User_NCEP_Sensor_ID and User_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs aren't zero-sized
    IF ( n_User_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User arrays must have sizes > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE MATCHING INDICES --                      #
    !#--------------------------------------------------------------------------#

    n_Matched_Channels = 0

    DO l = 1, n_User_Channels

      ! -- Count the matchups
      n = COUNT( Master_NCEP_Sensor_ID == User_NCEP_Sensor_ID(l) .AND. &
                 Master_Sensor_Channel == User_Sensor_Channel(l)       )
 
      ! -- If none, go to next channel
      IF ( n == 0 ) CYCLE

      ! -- If more than one, Master arrays are screwy
      IF ( n > 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Multiple entries in Master arrays for the same '//&
                              'User specified channel.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Increment channel match counter
      n_Matched_Channels = n_Matched_Channels + 1
 
      ! -- Get the matching index
      Idx = PACK( (/ ( n, n = 1, n_Master_Channels ) /), &
                  Master_NCEP_Sensor_ID == User_NCEP_Sensor_ID(l) .AND. &
                  Master_Sensor_Channel == User_Sensor_Channel(l)       )

      ! -- Save it
      Channel_Index( n_Matched_Channels ) = Idx(1)

    END DO

    ! -- Were ANY matches found?
    IF ( n_Matched_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'No Master/User array data matches were found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Allocate the ChannelInfo structure
    ! ----------------------------------

    Error_Status = CRTM_Allocate_ChannelInfo( n_Matched_Channels, &
                                              ChannelInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocation of ChannelInfo(1) structure failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Save the channel indices
    ! ------------------------

    ChannelInfo%Channel_Index = Channel_Index( 1:n_Matched_Channels )


  END FUNCTION IndexChannelInfo_NCEPID

END MODULE CRTM_ChannelInfo


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_ChannelInfo.f90,v 1.6 2004/07/01 20:52:51 paulv Exp $
!
! $Date: 2004/07/01 20:52:51 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_ChannelInfo.f90,v $
! Revision 1.6  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
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
