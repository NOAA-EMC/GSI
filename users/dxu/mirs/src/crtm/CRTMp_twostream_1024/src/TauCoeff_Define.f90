!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_Define
!
! PURPOSE:
!       Module defining the TauCoeff data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               check comparisons on input floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       Associated_TauCoeff:           Function to test the association status
!                                      of the pointer members of a TauCoeff
!                                      structure.
!
!       Destroy_TauCoeff:              Function to re-initialize a TauCoeff
!                                      structure.
!
!       Allocate_TauCoeff:             Function to allocate the pointer members
!                                      of a TauCoeff structure.
!
!       Assign_TauCoeff:               Function to copy a valid TauCoeff structure.
!
!       Concatenate_Channel_TauCoeff:  Function to concatenate two TauCoeff
!                                      structures along the CHANNEL dimension.
!
!       Concatenate_Absorber_TauCoeff: Function to concatenate two TauCoeff
!                                      structures along the ABSORBER dimension.
!
!       Equal_TauCoeff:                Function to test if two TauCoeff
!                                      structures are equal.
!
!       Check_TauCoeff_Release:        Function to check the TauCoeff Release value.
!
!       Count_TauCoeff_Sensors:        Subroutine to count the number of
!                                      different satellites/sensors in the
!                                      TauCoeff data structure.
!
!       Version_TauCoeff:              Subroutine to return a string containing
!                                      version and dimension information about
!                                      the TauCoeff data structure.
!
! DERIVED TYPES:
!       TauCoeff_type:   Definition of the public TauCoeff data structure. Fields
!                        are...
!
!         Release:             Coefficient data file release number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Version:             Coefficient data file version number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Orders:            Maximum polynomial order used to reconstruct
!                              the regression coefficients.
!                              "Iorder" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Predictors:        Number of predictors used in the
!                              gas absorption regression.
!                              "Iuse" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Absorbers:         Number of gaseous absorbers.
!                              "J" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Total number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Sensors:           Number of different satellite/sensors in the
!                              data structure.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         Sensor_Descriptor:   String variable containing a short text
!                              description of the sensor and satellite.
!                              Descriptors are taken from the SensorInfo
!                              file prefix member. Examples are:
!                                - hirs3_n17
!                                - airs_aqua
!                                - ssmis_f16... etc
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( 20 )
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         NCEP_Sensor_ID:      An "in-house" value used at NOAA/NCEP/EMC 
!                              to identify a satellite/sensor combination.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         WMO_Satellite_ID:    The WMO code for identifying satellite
!                              platforms. Taken from the WMO common
!                              code tables at:
!                                http://www.wmo.ch/web/ddbs/Code-tables.html
!                              The Satellite ID is from Common Code
!                              table C-5, or code table 0 01 007 in BUFR
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         WMO_Sensor_ID:       The WMO code for identifying a satelite
!                              sensor. Taken from the WMO common
!                              code tables at:
!                                http://www.wmo.ch/web/ddbs/Code-tables.html
!                              The Sensor ID is from Common Code
!                              table C-8, or code table 0 02 019 in BUFR
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Sensor_Channel:      This is the sensor channel number associated
!                              with the data in the coefficient file. Helps
!                              in identifying channels where the numbers are
!                              not contiguous (e.g. AIRS).
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Absorber_ID:         Array containing a list of absorber ID
!                              values. Used to identify the individual
!                              or collective molecular species for which
!                              the gas absorption coefficients were
!                              generated.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  J (n_Absorbers)
!                              ATTRIBUTES: POINTER
!
!         Alpha:               Array containing the alpha values used to
!                              generate the absorber space levels.
!                              UNITS:      Absorber dependent.
!                              TYPE:       REAL( Double )
!                              DIMENSION:  J (n_Absorbers)
!                              ATTRIBUTES: POINTER
!
!         Alpha_C1:            First constant (slope) used in defining the
!                              Alpha to absorber space equation:
!                                A(k) = Alpha_C1 * EXP( Alpha * k )  +  Alpha_C2
!                              where k = layer variable
!                                    A = absorber amount
!                              UNITS:      Absorber dependent.
!                              TYPE:       REAL( Double )
!                              DIMENSION:  J (n_Absorbers)
!                              ATTRIBUTES: POINTER
!
!         Alpha_C2:            Second constant (offset) used in defining the
!                              Alpha to absorber space equation:
!                                A(k) = Alpha_C1 * EXP( Alpha * k )  +  Alpha_C2
!                              where k = layer variable
!                                    A = absorber amount
!                              UNITS:      Absorber dependent.
!                              TYPE:       REAL( Double )
!                              DIMENSION:  J (n_Absorbers)
!                              ATTRIBUTES: POINTER
!
!         Order_Index:         Array containing the polynomial orders to use
!                              in reconstructing the gas absorption model
!                              regression coefficients.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  0:Iuse x J x L
!                              ATTRIBUTES: POINTER
!
!         Predictor_Index:     Array containing the predictor indices
!                              used to identify which predictors to use
!                              in the gas absorption model.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  0:Iuse x J x L
!                              ATTRIBUTES: POINTER
!
!         C:                   Array containing the gas absorption
!                              model coefficients.
!                              UNITS:      Variable
!                              TYPE:       REAL( Double )
!                              DIMENSION:  0:Iorder x 0:Iuse x J x L
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the TauCoeff_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
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

MODULE TauCoeff_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler
  USE Compare_Float_Numbers


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the TauCoeff structure
  PUBLIC :: Associated_TauCoeff
  PUBLIC :: Destroy_TauCoeff
  PUBLIC :: Allocate_TauCoeff
  PUBLIC :: Assign_TauCoeff
  PUBLIC :: Concatenate_Channel_TauCoeff
  PUBLIC :: Concatenate_Absorber_TauCoeff
  PUBLIC :: Equal_TauCoeff
  PUBLIC :: Check_TauCoeff_Release
  PUBLIC :: Count_TauCoeff_Sensors
  PUBLIC :: Version_TauCoeff


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: TauCoeff_Define.f90,v 5.9 2005/02/07 18:15:01 paulv Exp $'

  ! -- TauCoeff valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Sensor descriptor component string length
  INTEGER, PRIVATE, PARAMETER :: DL = 20

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: TAUCOEFF_RELEASE = 5  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: TAUCOEFF_VERSION = 3  ! This is just the data version.


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- Number of TauCoeff data items
  INTEGER( Long ), PUBLIC, PARAMETER :: N_TAUCOEFF_ITEMS = 12_Long

  ! -- Internal data type descriptors for the TauCoeff data
  !    5 = Double (i.e. 8-byte float)
  !    4 = Single (i.e. 4-byte float)
  !    3 = Long   (i.e. 4-byte integer)
  INTEGER( Long ), PUBLIC, PARAMETER, &
                   DIMENSION( N_TAUCOEFF_ITEMS ) :: TAUCOEFF_DATA_TYPE = &
                                                       (/ 7_Long, &  ! Sensor_Descriptor
                                                          3_Long, &  ! NCEP_Sensor_ID
                                                          3_Long, &  ! WMO_Satellite_ID
                                                          3_Long, &  ! WMO_Sensor_ID
                                                          3_Long, &  ! Sensor_Channel
                                                          3_Long, &  ! Absorber_ID
                                                          5_Long, &  ! Alpha
                                                          5_Long, &  ! Alpha_C1
                                                          5_Long, &  ! Alpha_C2
                                                          3_Long, &  ! Order_Index
                                                          3_Long, &  ! Predictor_Index
                                                          5_Long /)  ! C

  ! -- Names of the data items (for error processing)
  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( N_TAUCOEFF_ITEMS ) :: TAUCOEFF_DATA_NAME = &
                                                      (/ 'Sensor_Descriptor', &
                                                         'NCEP_Sensor_ID   ', &
                                                         'WMO_Satellite_ID ', &
                                                         'WMO_Sensor_ID    ', &
                                                         'Sensor_Channel   ', &
                                                         'Absorber_ID      ', &
                                                         'Alpha            ', &
                                                         'Alpha_C1         ', &
                                                         'Alpha_C2         ', &
                                                         'Order_Index      ', &
                                                         'Predictor_Index  ', &
                                                         'Tau_Coefficients ' /)


  ! ------------------------------
  ! TauCoeff data type definition
  ! ------------------------------

  TYPE, PUBLIC :: TauCoeff_type
    INTEGER :: n_Allocates = 0

    ! -- Release and version information
    INTEGER( Long ) :: Release = TAUCOEFF_RELEASE
    INTEGER( Long ) :: Version = TAUCOEFF_VERSION

    ! -- Array dimensions
    INTEGER( Long ) :: n_Orders     = 0    ! Iorder
    INTEGER( Long ) :: n_Predictors = 0    ! Iuse
    INTEGER( Long ) :: n_Absorbers  = 0    ! J
    INTEGER( Long ) :: n_Channels   = 0    ! L

    INTEGER( Long ) :: Sensor_Descriptor_StrLen = DL

    ! -- Number of different satellite/sensor combinations
    INTEGER( Long ) :: n_Sensors = 0

    ! -- The satellite and sensor IDs
    CHARACTER( DL ), POINTER, DIMENSION( : ) :: Sensor_Descriptor => NULL() ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: NCEP_Sensor_ID    => NULL() ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: WMO_Satellite_ID  => NULL() ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: WMO_Sensor_ID     => NULL() ! L

    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Sensor_Channel => NULL()    ! L

    ! -- The absorber ID
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Absorber_ID => NULL()    ! J

    ! -- The absorber space function values
    REAL( Double ),  POINTER, DIMENSION( : ) :: Alpha    => NULL()       ! J
    REAL( Double ),  POINTER, DIMENSION( : ) :: Alpha_C1 => NULL()       ! J
    REAL( Double ),  POINTER, DIMENSION( : ) :: Alpha_C2 => NULL()       ! J

    ! -- The polynomial order index array.
    ! -- This array identifies the order of the polynomial used to
    ! -- reconstruct the regression coefficients. For each predictor
    ! -- (Iuse), each absorber (J) and each channel (L) a different
    ! -- order of polynomial can be specified.
    INTEGER( Long ), POINTER, DIMENSION( :, :, : ) :: Order_Index => NULL()  ! 0:Iuse x J x L

    ! -- The predictor index array.
    ! -- This array identifies which subset (Iuse) of the total number
    ! -- number of predictors are used to compute the absorption coefficient
    ! -- for absorber (J) and each channel (L). If Predictor_Index(0,:,:) is
    ! -- less than 0, this is an indication that there is NO absorption for
    ! -- the selected absorber in the current channel.
    INTEGER( Long ), POINTER, DIMENSION( :, :, : )    :: Predictor_Index => NULL()  ! 0:Iuse x J x L

    ! -- The array of coefficients
    REAL( Double ),  POINTER, DIMENSION( :, :, :, : ) :: C => NULL() ! 0:Iorder x 0:Iuse x J x L
  END TYPE TauCoeff_type


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
!       Clear_TauCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a TauCoeff structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_TauCoeff( TauCoeff ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       TauCoeff:    TauCoeff structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_type
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
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_TauCoeff( TauCoeff )

    TYPE( TauCoeff_type ), INTENT( IN OUT ) :: TauCoeff

    TauCoeff%n_Orders     = 0
    TauCoeff%n_Predictors = 0
    TauCoeff%n_Absorbers  = 0
    TauCoeff%n_Channels   = 0

    TauCoeff%Sensor_Descriptor_StrLen = DL

    TauCoeff%n_Sensors = 0

  END SUBROUTINE Clear_TauCoeff





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
!       Associated_TauCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       TauCoeff structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_TauCoeff( TauCoeff,           &  ! Input
!                                                 ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       TauCoeff:    TauCoeff structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    TauCoeff structure pointer members are associated.
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
!                            association status of the TauCoeff pointer members.
!                            .TRUE.  - if ALL the TauCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the TauCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the TauCoeff pointer
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
!       This function tests the association status of the TauCoeff
!       structure pointer members. Therefore this function must only
!       be called after the input TauCoeff structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_TauCoeff( TauCoeff,  & ! Input
                                ANY_Test ) & ! Optional input
                              RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_type ), INTENT( IN ) :: TauCoeff

    ! -- Optional input
    INTEGER,     OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( TauCoeff%Sensor_Descriptor ) .AND. &
           ASSOCIATED( TauCoeff%NCEP_Sensor_ID    ) .AND. &
           ASSOCIATED( TauCoeff%WMO_Satellite_ID  ) .AND. &
           ASSOCIATED( TauCoeff%WMO_Sensor_ID     ) .AND. &
           ASSOCIATED( TauCoeff%Sensor_Channel    ) .AND. &
           ASSOCIATED( TauCoeff%Absorber_ID       ) .AND. &
           ASSOCIATED( TauCoeff%Alpha             ) .AND. &
           ASSOCIATED( TauCoeff%Alpha_C1          ) .AND. &
           ASSOCIATED( TauCoeff%Alpha_C2          ) .AND. &
           ASSOCIATED( TauCoeff%Order_Index       ) .AND. &
           ASSOCIATED( TauCoeff%Predictor_Index   ) .AND. &
           ASSOCIATED( TauCoeff%C                 )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( TauCoeff%Sensor_Descriptor ) .OR. &
           ASSOCIATED( TauCoeff%NCEP_Sensor_ID    ) .OR. &
           ASSOCIATED( TauCoeff%WMO_Satellite_ID  ) .OR. &
           ASSOCIATED( TauCoeff%WMO_Sensor_ID     ) .OR. &
           ASSOCIATED( TauCoeff%Sensor_Channel    ) .OR. &
           ASSOCIATED( TauCoeff%Absorber_ID       ) .OR. &
           ASSOCIATED( TauCoeff%Alpha             ) .OR. &
           ASSOCIATED( TauCoeff%Alpha_C1          ) .OR. &
           ASSOCIATED( TauCoeff%Alpha_C2          ) .OR. &
           ASSOCIATED( TauCoeff%Order_Index       ) .OR. &
           ASSOCIATED( TauCoeff%Predictor_Index   ) .OR. &
           ASSOCIATED( TauCoeff%C                 )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_TauCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_TauCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of TauCoeff
!       data structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_TauCoeff( TauCoeff,                 &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff:     Re-initialized TauCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
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
!       Associated_TauCoeff:  Function to test the association status of the
!                             pointer members of a TauCoeff structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_TauCoeff( TauCoeff,     &  ! Output
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
    TYPE( TauCoeff_type ),    INTENT( IN OUT ) :: TauCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message
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
    !#                     -- PERFORM RE-INITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_TauCoeff( TauCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Sensor Descriptor
    IF ( ASSOCIATED( TauCoeff%Sensor_Descriptor ) ) THEN

      DEALLOCATE( TauCoeff%Sensor_Descriptor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Sensor_Descriptor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the NCEP Sensor ID
    IF ( ASSOCIATED( TauCoeff%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( TauCoeff%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the WMO Satellite ID
    IF ( ASSOCIATED( TauCoeff%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( TauCoeff%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the WMO Sensor ID
    IF ( ASSOCIATED( TauCoeff%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( TauCoeff%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff WMO_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( TauCoeff%Sensor_Channel ) ) THEN

      DEALLOCATE( TauCoeff%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Absorber_ID
    IF ( ASSOCIATED( TauCoeff%Absorber_ID ) ) THEN

      DEALLOCATE( TauCoeff%Absorber_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Absorber_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Alpha
    IF ( ASSOCIATED( TauCoeff%Alpha ) ) THEN

      DEALLOCATE( TauCoeff%Alpha, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Alpha ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Alpha_C1
    IF ( ASSOCIATED( TauCoeff%Alpha_C1 ) ) THEN

      DEALLOCATE( TauCoeff%Alpha_C1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Alpha_C1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Alpha_C2
    IF ( ASSOCIATED( TauCoeff%Alpha_C2 ) ) THEN

      DEALLOCATE( TauCoeff%Alpha_C2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Alpha_C2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Order_Index
    IF ( ASSOCIATED( TauCoeff%Order_Index ) ) THEN

      DEALLOCATE( TauCoeff%Order_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Order_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Predictor_Index
    IF ( ASSOCIATED( TauCoeff%Predictor_Index ) ) THEN

      DEALLOCATE( TauCoeff%Predictor_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff Predictor_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate coefficients
    IF ( ASSOCIATED( TauCoeff%C ) ) THEN

      DEALLOCATE( TauCoeff%C, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff coefficients ", &
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

    TauCoeff%n_Allocates = TauCoeff%n_Allocates - 1

    IF ( TauCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_TauCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_TauCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the TauCoeff
!       data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_TauCoeff( n_Orders,                  &  ! Input
!                                         n_Predictors,              &  ! Input
!                                         n_Absorbers,               &  ! Input
!                                         n_Channels,                &  ! Input
!                                         TauCoeff,                  &  ! Output
!                                         RCS_Id      = RCS_Id,      &  ! Revision control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Orders:     Maximum polynomial order used to reconstruct
!                     the prediction coefficients.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Predictors: Maximum number of predictors dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Absorbers:  Number of absorbers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Channels:   Number of channels dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in
!                     which any messages will be logged. If not
!                     specified, or if an error occurs opening
!                     the log file, the default action is to
!                     output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff:     TauCoeff structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
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
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff:  Function to test the association status of the
!                             pointer members of a TauCoeff structure.
!
!       Destroy_TauCoeff:     Function to re-initialize the scalar and pointer
!                             members of TauCoeff data structures.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_TauCoeff( n_Orders,     &  ! Input
                              n_Predictors, &  ! Input
                              n_Absorbers,  &  ! Input
                              n_Channels,   &  ! Input
                              TauCoeff,     &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Orders
    INTEGER,                  INTENT( IN )     :: n_Predictors
    INTEGER,                  INTENT( IN )     :: n_Absorbers
    INTEGER,                  INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( TauCoeff_type ),    INTENT( IN OUT ) :: TauCoeff

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

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

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Orders     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Channels   < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_TauCoeff( TauCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_TauCoeff( TauCoeff, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating TauCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( TauCoeff%Sensor_Descriptor( n_Channels ), &
              TauCoeff%NCEP_Sensor_ID( n_Channels ), &
              TauCoeff%WMO_Satellite_ID( n_Channels ), &
              TauCoeff%WMO_Sensor_ID( n_Channels ), &
              TauCoeff%Sensor_Channel( n_Channels ), &
              TauCoeff%Absorber_ID( n_Absorbers ), &
              TauCoeff%Alpha( n_Absorbers ), &
              TauCoeff%Alpha_c1( n_Absorbers ), &
              TauCoeff%Alpha_c2( n_Absorbers ), &
              TauCoeff%Order_Index( 0:n_Predictors, n_Absorbers, n_Channels ), &
              TauCoeff%Predictor_Index( 0:n_Predictors, n_Absorbers, n_Channels ), &
              TauCoeff%C( 0:n_Orders, 0:n_Predictors, n_Absorbers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    TauCoeff%n_Orders     = n_Orders
    TauCoeff%n_Predictors = n_Predictors
    TauCoeff%n_Absorbers  = n_Absorbers
    TauCoeff%n_Channels   = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    TauCoeff%n_Allocates = TauCoeff%n_Allocates + 1

    IF ( TauCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_TauCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_TauCoeff
!
! PURPOSE:
!       Function to copy valid TauCoeff structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_TauCoeff( TauCoeff_in,              &  ! Input
!                                       TauCoeff_out,             &  ! Output
!                                       RCS_Id      = RCS_Id,     &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff_in:   TauCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff_out:  Copy of the input structure, TauCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff:  Function to test the association status of the
!                             pointer members of a TauCoeff structure.
!
!       Allocate_TauCoeff:    Function to allocate the pointer members of
!                             the TauCoeff data structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_TauCoeff( TauCoeff_in,   &  ! Input
                            TauCoeff_out,  &  ! Output
                            RCS_Id,        &  ! Revision control
                            Message_Log )  &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_type ),    INTENT( IN )     :: TauCoeff_in

    ! -- Output
    TYPE( TauCoeff_type ),    INTENT( IN OUT ) :: TauCoeff_out

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff'



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

    IF ( .NOT. Associated_TauCoeff( TauCoeff_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    TauCoeff_out%Release = TauCoeff_in%Release
    TauCoeff_out%Version = TauCoeff_in%Version

    TauCoeff_out%n_Sensors = TauCoeff_in%n_Sensors


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_TauCoeff( TauCoeff_in%n_Orders, &
                                      TauCoeff_in%n_Predictors, &
                                      TauCoeff_in%n_Absorbers, &
                                      TauCoeff_in%n_Channels, &
                                      TauCoeff_out, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output TauCoeff arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    TauCoeff_out%Sensor_Descriptor = TauCoeff_in%Sensor_Descriptor
    TauCoeff_out%NCEP_Sensor_ID    = TauCoeff_in%NCEP_Sensor_ID
    TauCoeff_out%WMO_Satellite_ID  = TauCoeff_in%WMO_Satellite_ID
    TauCoeff_out%WMO_Sensor_ID     = TauCoeff_in%WMO_Sensor_ID
    TauCoeff_out%Sensor_Channel    = TauCoeff_in%Sensor_Channel

    TauCoeff_out%Absorber_ID = TauCoeff_in%Absorber_ID

    TauCoeff_out%Alpha    = TauCoeff_in%Alpha
    TauCoeff_out%Alpha_C1 = TauCoeff_in%Alpha_C1
    TauCoeff_out%Alpha_C2 = TauCoeff_in%Alpha_C2

    TauCoeff_out%Order_Index     = TauCoeff_in%Order_Index
    TauCoeff_out%Predictor_Index = TauCoeff_in%Predictor_Index
    TauCoeff_out%C               = TauCoeff_in%C

  END FUNCTION Assign_TauCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Concatenate_Channel_TauCoeff
!
! PURPOSE:
!       Function to concatenate two valid TauCoeff structures along
!       the channel dimension.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Channel_TauCoeff( TauCoeff1,                &  ! Input/Output
!                                                    TauCoeff2,                &  ! Input
!                                                    RCS_Id      = RCS_Id,     &  ! Revision control
!                                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff1:     First TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       TauCoeff2:     Second TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff1:     The concatenated TauCoeff structure. The order of
!                      concatenation is TauCoeff1,TauCoeff2 along the 
!                      channel dimension.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred, or
!                         == WARNING - the version numbers of the TauCoeff structure
!                                      data are different.
!                                    - the destruction of a temporary, local TauCoeff
!                                      structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Init_TauCoeff:           Function to initialize the scalar and pointer
!                                members of TauCoeff data structures.
!
!       Associated_TauCoeff:     Function to test the association status of the
!                                pointer members of a TauCoeff structure.
!
!       Assign_TauCoeff:         Function to copy valid TauCoeff data structures.
!
!       Destroy_TauCoeff:        Function to re-initialize the scalar and pointer
!                                members of TauCoeff data structures.
!
!       Allocate_TauCoeff:       Function to allocate the pointer members of
!                                the TauCoeff data structure.
!
!       Count_TauCoeff_Sensors:  Subroutine to count the number of different
!                                satellite/sensors in the TauCoeff structure
!                                and set the n_Sensors field.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The input TauCoeff1 argument contains the concatenated structure
!       data (in character-speak: TauCoeff1//TauCoeff2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input TauCoeff1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Concatenate_Channel_TauCoeff( TauCoeff1,     &  ! Input/Output
                                         TauCoeff2,     &  ! Input
                                         RCS_Id,        &  ! Revision control
                                         Message_Log )  &  ! Error messaging
                                       RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( TauCoeff_type ),    INTENT( IN OUT )  :: TauCoeff1
    TYPE( TauCoeff_type ),    INTENT( IN )      :: TauCoeff2

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )     :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )      :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_Channel_TauCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_Channels, l1, l2

    TYPE( TauCoeff_type ) :: TauCoeff_Tmp



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The first structure
    ! -------------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- COMPARE THE INPUT TauCoeff RELEASE AND VERSION --           #
    !#--------------------------------------------------------------------------#

    ! -------
    ! Release
    ! -------

    IF ( TauCoeff1%Release /= TauCoeff2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff Release values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------
    ! Version
    ! -------

    IF ( TauCoeff1%Version /= TauCoeff2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff Version values are different.', &
                            WARNING, &
                            Message_Log = Message_Log )

    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK THE NON-CHANNEL DIMENSIONS --                  #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff1%n_Orders     /= TauCoeff2%n_Orders     .OR. &
         TauCoeff1%n_Predictors /= TauCoeff2%n_Predictors .OR. &
         TauCoeff1%n_Absorbers  /= TauCoeff2%n_Absorbers       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-channel TauCoeff dimensions are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- COPY FIRST INPUT TauCoeff STRUCTURE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_TauCoeff( TauCoeff1, TauCoeff_Tmp, &
                                    Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying TauCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT TauCoeff STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_TauCoeff( TauCoeff1, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    ! -- Set the total number of channels
    n_Channels = TauCoeff_Tmp%n_Channels + TauCoeff2%n_Channels

    ! -- Perform the allocation
    Error_Status = Allocate_TauCoeff( TauCoeff_Tmp%n_Orders, &
                                      TauCoeff_Tmp%n_Predictors, &
                                      TauCoeff_Tmp%n_Absorbers, &
                                      n_Channels, &
                                      TauCoeff1, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating TauCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE CONCATENATION --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Assign the non-channel array data
    ! ---------------------------------

    TauCoeff1%Version = MAX( TauCoeff_Tmp%Version, TauCoeff2%Version )

    TauCoeff1%Absorber_ID = TauCoeff_Tmp%Absorber_ID

    TauCoeff1%Alpha    = TauCoeff_Tmp%Alpha
    TauCoeff1%Alpha_C1 = TauCoeff_Tmp%Alpha_C1
    TauCoeff1%Alpha_C2 = TauCoeff_Tmp%Alpha_C2


    ! ------------------------------
    ! Concatenate channel array data
    ! ------------------------------

    ! -- The first part
    l1 = 1
    l2 = TauCoeff_Tmp%n_Channels

    TauCoeff1%Sensor_Descriptor(l1:l2)   = TauCoeff_Tmp%Sensor_Descriptor
    TauCoeff1%NCEP_Sensor_ID(l1:l2)      = TauCoeff_Tmp%NCEP_Sensor_ID
    TauCoeff1%WMO_Satellite_ID(l1:l2)    = TauCoeff_Tmp%WMO_Satellite_ID
    TauCoeff1%WMO_Sensor_ID(l1:l2)       = TauCoeff_Tmp%WMO_Sensor_ID
    TauCoeff1%Sensor_Channel(l1:l2)      = TauCoeff_Tmp%Sensor_Channel

    TauCoeff1%Order_Index(:,:,l1:l2)     = TauCoeff_Tmp%Order_Index
    TauCoeff1%Predictor_Index(:,:,l1:l2) = TauCoeff_Tmp%Predictor_Index
    TauCoeff1%C(:,:,:,l1:l2)             = TauCoeff_Tmp%C

    ! -- The second part
    l1 = l2 + 1
    l2 = n_Channels

    TauCoeff1%Sensor_Descriptor(l1:l2)   = TauCoeff2%Sensor_Descriptor
    TauCoeff1%NCEP_Sensor_ID(l1:l2)      = TauCoeff2%NCEP_Sensor_ID
    TauCoeff1%WMO_Satellite_ID(l1:l2)    = TauCoeff2%WMO_Satellite_ID
    TauCoeff1%WMO_Sensor_ID(l1:l2)       = TauCoeff2%WMO_Sensor_ID
    TauCoeff1%Sensor_Channel(l1:l2)      = TauCoeff2%Sensor_Channel

    TauCoeff1%Order_Index(:,:,l1:l2)     = TauCoeff2%Order_Index
    TauCoeff1%Predictor_Index(:,:,l1:l2) = TauCoeff2%Predictor_Index
    TauCoeff1%C(:,:,:,l1:l2)             = TauCoeff2%C



    !#--------------------------------------------------------------------------#
    !#                    -- COUNT THE NUMBER OF SENSORS --                     #
    !#--------------------------------------------------------------------------#

    CALL Count_TauCoeff_Sensors( TauCoeff1, Use_WMO_Id = SET )



    !#--------------------------------------------------------------------------#
    !#             -- DEALLOCATE THE TEMPORARY TauCoeff STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_TauCoeff( TauCoeff_Tmp, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauCoeff_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_Channel_TauCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Concatenate_Absorber_TauCoeff
!
! PURPOSE:
!       Function to concatenate two valid TauCoeff structures along
!       the absorber dimension.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Absorber_TauCoeff( TauCoeff1,                &  ! Input/Output
!                                                     TauCoeff2,                &  ! Input
!                                                     RCS_Id      = RCS_Id,     &  ! Revision control
!                                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff1:     First TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       TauCoeff2:     Second TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff1:     The concatenated TauCoeff structure. The order of
!                      concatenation is TauCoeff1,TauCoeff2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred, or
!                         == WARNING - the version numbers of the TauCoeff structure
!                                      data are different.
!                                    - the destruction of a temporary, local TauCoeff
!                                      structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Init_TauCoeff:           Function to initialize the scalar and pointer
!                                members of TauCoeff data structures.
!
!       Associated_TauCoeff:     Function to test the association status of the
!                                pointer members of a TauCoeff structure.
!
!       Assign_TauCoeff:         Function to copy valid TauCoeff data structures.
!
!       Destroy_TauCoeff:        Function to re-initialize the scalar and pointer
!                                members of TauCoeff data structures.
!
!       Allocate_TauCoeff:       Function to allocate the pointer members of
!                                the TauCoeff data structure.
!
!       Count_TauCoeff_Sensors:  Subroutine to count the number of different
!                                satellite/sensors in the TauCoeff structure
!                                and set the n_Sensors field.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The input TauCoeff1 argument contains the concatenated structure
!       data (in character-speak: TauCoeff1//TauCoeff2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input TauCoeff1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Concatenate_Absorber_TauCoeff( TauCoeff1,     &  ! Input/Output
                                          TauCoeff2,     &  ! Input
                                          RCS_Id,        &  ! Revision control
                                          Message_Log )  &  ! Error messaging
                                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( TauCoeff_type ),    INTENT( IN OUT )  :: TauCoeff1
    TYPE( TauCoeff_type ),    INTENT( IN )      :: TauCoeff2

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )     :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )      :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_Absorber_TauCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_Absorbers, j1, j2

    TYPE( TauCoeff_type ) :: TauCoeff_Tmp



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The first structure
    ! -------------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- COMPARE THE INPUT TauCoeff RELEASE AND VERSION --           #
    !#--------------------------------------------------------------------------#

    ! -------
    ! Release
    ! -------

    IF ( TauCoeff1%Release /= TauCoeff2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff Release values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------
    ! Version
    ! -------

    IF ( TauCoeff1%Version /= TauCoeff2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff Version values are different.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK THE INPUT STRUCTURE CONTENTS --                  #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! The non-absorber dimensions
    ! ---------------------------

    IF ( TauCoeff1%n_Orders     /= TauCoeff2%n_Orders     .OR. &
         TauCoeff1%n_Predictors /= TauCoeff2%n_Predictors .OR. &
         TauCoeff1%n_Channels   /= TauCoeff2%n_Channels        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-absorber TauCoeff dimensions are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! The ID and channel values
    ! -------------------------

    IF ( ANY( ( TauCoeff1%NCEP_Sensor_ID   - TauCoeff2%NCEP_Sensor_ID   ) /= 0 ) .OR. &
         ANY( ( TauCoeff1%WMO_Satellite_ID - TauCoeff2%WMO_Satellite_ID ) /= 0 ) .OR. &
         ANY( ( TauCoeff1%WMO_Sensor_ID    - TauCoeff2%WMO_Sensor_ID    ) /= 0 ) .OR. &
         ANY( ( TauCoeff1%Sensor_Channel   - TauCoeff2%Sensor_Channel   ) /= 0 )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauCoeff sensor ID and channel values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- COPY FIRST INPUT TauCoeff STRUCTURE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_TauCoeff( TauCoeff1, TauCoeff_Tmp, &
                                    Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying TauCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT TauCoeff STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_TauCoeff( TauCoeff1, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    ! -- Set the total number of absorbers
    n_Absorbers = TauCoeff_Tmp%n_Absorbers + TauCoeff2%n_Absorbers

    ! -- Perform the allocation
    Error_Status = Allocate_TauCoeff( TauCoeff_Tmp%n_Orders, &
                                      TauCoeff_Tmp%n_Predictors, &
                                      n_Absorbers, &
                                      TauCoeff_Tmp%n_Channels, &
                                      TauCoeff1, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating TauCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE CONCATENATION --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Assign the non-absorber array data
    ! ---------------------------------

    TauCoeff1%Version = MAX( TauCoeff_Tmp%Version, TauCoeff2%Version )

    TauCoeff1%Sensor_Descriptor = TauCoeff_Tmp%Sensor_Descriptor
    TauCoeff1%NCEP_Sensor_ID    = TauCoeff_Tmp%NCEP_Sensor_ID
    TauCoeff1%WMO_Satellite_ID  = TauCoeff_Tmp%WMO_Satellite_ID
    TauCoeff1%WMO_Sensor_ID     = TauCoeff_Tmp%WMO_Sensor_ID
    TauCoeff1%Sensor_Channel    = TauCoeff_Tmp%Sensor_Channel


    ! -------------------------------
    ! Concatenate absorber array data
    ! -------------------------------

    ! -- The first part
    j1 = 1
    j2 = TauCoeff_Tmp%n_Absorbers

    TauCoeff1%Absorber_ID(j1:j2) = TauCoeff_Tmp%Absorber_ID

    TauCoeff1%Alpha(j1:j2)    = TauCoeff_Tmp%Alpha
    TauCoeff1%Alpha_C1(j1:j2) = TauCoeff_Tmp%Alpha_C1
    TauCoeff1%Alpha_C2(j1:j2) = TauCoeff_Tmp%Alpha_C2

    TauCoeff1%Order_Index(:,j1:j2,:)     = TauCoeff_Tmp%Order_Index
    TauCoeff1%Predictor_Index(:,j1:j2,:) = TauCoeff_Tmp%Predictor_Index
    TauCoeff1%C(:,:,j1:j2,:)             = TauCoeff_Tmp%C

    ! -- The second part
    j1 = j2 + 1
    j2 = n_Absorbers

    TauCoeff1%Absorber_ID(j1:j2) = TauCoeff2%Absorber_ID

    TauCoeff1%Alpha(j1:j2)    = TauCoeff2%Alpha
    TauCoeff1%Alpha_C1(j1:j2) = TauCoeff2%Alpha_C1
    TauCoeff1%Alpha_C2(j1:j2) = TauCoeff2%Alpha_C2

    TauCoeff1%Order_Index(:,j1:j2,:)     = TauCoeff2%Order_Index
    TauCoeff1%Predictor_Index(:,j1:j2,:) = TauCoeff2%Predictor_Index
    TauCoeff1%C(:,:,j1:j2,:)             = TauCoeff2%C



    !#--------------------------------------------------------------------------#
    !#             -- DEALLOCATE THE TEMPORARY TauCoeff STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_TauCoeff( TauCoeff_Tmp, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauCoeff_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_Absorber_TauCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Equal_TauCoeff
!
! PURPOSE:
!       Function to test if two TauCoeff structures are equal.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_TauCoeff( TauCoeff_LHS,             &  ! Input
!                                      TauCoeff_RHS,             &  ! Input
!                                      ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                      Check_All   = Check_All,  &  ! Optional input
!                                      RCS_Id      = RCS_Id,     &  ! Optional output
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff_LHS:  TauCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( TauCoeff_LHS == TauCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       TauCoeff_RHS:  TauCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( TauCoeff_LHS == TauCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the *floating point*
!                      channel data of the TauCoeff structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in TauCoeff structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      Note: Setting this argument has no effect if, for
!                            example, the structure dimensions are different,
!                            or the sensor ids/channels are different, or the
!                            absorber ids are different, etc. 
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Compare_Float:        Function to compare floating point scalars and
!                             arrays with adjustible precision tolerance.
!                             SOURCE: COMPARE_FLOAT_NUMBERS module.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Congruency of the structure data is a prerequisite of equality.
!       That is, the *order* of the data is important. For example, if
!       two structures contain the same absorber information, but in a
!       different order, the structures are not considered equal. 
! 
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Equal_TauCoeff( TauCoeff_LHS, &  ! Input
                           TauCoeff_RHS, &  ! Input
                           ULP_Scale,    &  ! Optional input
                           Check_All,    &  ! Optional input
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
    TYPE( TauCoeff_type ),    INTENT( IN )  :: TauCoeff_LHS
    TYPE( TauCoeff_type ),    INTENT( IN )  :: TauCoeff_RHS

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: ULP_Scale
    INTEGER,        OPTIONAL, INTENT( IN )  :: Check_All

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_TauCoeff'
    CHARACTER( * ), PARAMETER :: INT_FORMAT = 'i4'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: l, j



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#                   -- CHECK THE OPTIONAL ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Test the ULP_Scale argument
    ! ---------------------------

    ! -- Default precision is a single unit in last place
    ULP = 1

    ! -- ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF


    ! ---------------------------
    ! Test the Check_All argument
    ! ---------------------------

    ! -- Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! -- ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! The LHS structure
    ! -----------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_TauCoeff( TauCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK SCALAR MEMBERS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Release/Version info
    ! --------------------

    IF ( ( TauCoeff_LHS%Release /= TauCoeff_RHS%Release ) .OR. &
         ( TauCoeff_LHS%Version /= TauCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      TauCoeff_LHS%Release, TauCoeff_LHS%Version, &
                      TauCoeff_RHS%Release, TauCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------
    ! Dimensions
    ! ----------

    ! -- Order of the polynomial
    IF ( TauCoeff_LHS%n_Orders /= TauCoeff_RHS%n_Orders ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Orders dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      TauCoeff_LHS%n_Orders, TauCoeff_RHS%n_Orders
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of predictors
    IF ( TauCoeff_LHS%n_Predictors /= TauCoeff_RHS%n_Predictors ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Predictors dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      TauCoeff_LHS%n_Predictors, TauCoeff_RHS%n_Predictors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of Absorbers
    IF ( TauCoeff_LHS%n_Absorbers /= TauCoeff_RHS%n_Absorbers ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Absorbers dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      TauCoeff_LHS%n_Absorbers, TauCoeff_RHS%n_Absorbers
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of channels
    IF ( TauCoeff_LHS%n_Channels /= TauCoeff_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Channels dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      TauCoeff_LHS%n_Channels, TauCoeff_RHS%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------
    ! n_Sensors value
    ! ---------------

    IF ( TauCoeff_LHS%n_Sensors /= TauCoeff_RHS%n_Sensors ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Sensors values are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      TauCoeff_LHS%n_Sensors, TauCoeff_RHS%n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- CHECK ID POINTER MEMBERS BY CHANNEL --                #
    !#                                                                          #
    !# Each structure member is tested separately. It's a bit of a brain dead   #
    !# way to do it, but easiest to implement since the data types differ.      #
    !# Also, each channel is tested explicitly, rather than using the ANY       #
    !# or ALL intrinsic functions, since I wanted to highlight the actual       #
    !# channel index where any difference occured so it would be very easy to   #
    !# track down the location of the difference.                               #
    !#--------------------------------------------------------------------------#

    l_Channel_Loop: DO l = 1, TauCoeff_RHS%n_Channels


      ! ---------------------
      ! The Sensor Descriptor
      ! ---------------------

      IF ( TauCoeff_LHS%Sensor_Descriptor(l) /= TauCoeff_RHS%Sensor_Descriptor(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Descriptor values are different, ", &
                          &a, " vs. ", a, ",  for channel index # ", i4 )' ) &
                        TRIM( TauCoeff_LHS%Sensor_Descriptor(l) ), &
                        TRIM( TauCoeff_RHS%Sensor_Descriptor(l) ), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------
      ! The NCEP sensor ID
      ! ------------------

      IF ( TauCoeff_LHS%NCEP_Sensor_ID(l) /= TauCoeff_RHS%NCEP_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "NCEP_Sensor_ID values are different, ", &
                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
                        TauCoeff_LHS%NCEP_Sensor_ID(l), &
                        TauCoeff_RHS%NCEP_Sensor_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------
      ! The WMO Satellite ID
      ! --------------------

      IF ( TauCoeff_LHS%WMO_Satellite_ID(l) /= TauCoeff_RHS%WMO_Satellite_ID(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "WMO_Satellite_ID values are different, ", &
                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
                        TauCoeff_LHS%WMO_Satellite_ID(l), &
                        TauCoeff_RHS%WMO_Satellite_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! -----------------
      ! The WMO Sensor ID
      ! -----------------

      IF ( TauCoeff_LHS%WMO_Sensor_ID(l) /= TauCoeff_RHS%WMO_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "WMO_Sensor_ID values are different, ", &
                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
                        TauCoeff_LHS%WMO_Sensor_ID(l), &
                        TauCoeff_RHS%WMO_Sensor_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------------
      ! The sensor channel numbers
      ! --------------------------

      IF ( TauCoeff_LHS%Sensor_Channel(l) /= TauCoeff_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Sensor_Channel values are different, ", &
                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
                        TauCoeff_LHS%Sensor_Channel(l), &
                        TauCoeff_RHS%Sensor_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF

    END DO l_Channel_Loop



    !#--------------------------------------------------------------------------#
    !#            -- CHECK ABSORBER DIMENSIONED POINTER MEMBERS --              #
    !#--------------------------------------------------------------------------#

    j_Absorber_Loop: DO j = 1, TauCoeff_RHS%n_Absorbers


      ! ---------------------
      ! The Absorber_ID value
      ! ---------------------

      IF ( TauCoeff_LHS%Absorber_ID(j) /= TauCoeff_RHS%Absorber_ID(j) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Absorber_ID values are different, ", &
                          &i3, " vs. ", i3, ",  for absorber # ", i4 )' ) &
                        TauCoeff_LHS%Absorber_ID(j), &
                        TauCoeff_RHS%Absorber_ID(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ---------------
      ! The Alpha value
      ! ---------------

      IF ( .NOT. Compare_Float( TauCoeff_LHS%Alpha(j), &
                                TauCoeff_RHS%Alpha(j), &
                                ULP = ULP              ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Alpha values are different, ", &
                          &es13.6, " vs. ", es13.6, ",  for absorber # ", i4 )' ) &
                        TauCoeff_LHS%Alpha(j), &
                        TauCoeff_RHS%Alpha(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------
      ! The Alpha_C1 value
      ! ------------------

      IF ( .NOT. Compare_Float( TauCoeff_LHS%Alpha_C1(j), &
                                TauCoeff_RHS%Alpha_C1(j), &
                                ULP = ULP                 ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Alpha_C1 values are different, ", &
                          &es13.6, " vs. ", es13.6, ",  for absorber # ", i4 )' ) &
                        TauCoeff_LHS%Alpha_C1(j), &
                        TauCoeff_RHS%Alpha_C1(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------
      ! The Alpha_C2 value
      ! ------------------

      IF ( .NOT. Compare_Float( TauCoeff_LHS%Alpha_C2(j), &
                                TauCoeff_RHS%Alpha_C2(j), &
                                ULP = ULP                 ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Alpha_C2 values are different, ", &
                          &es13.6, " vs. ", es13.6, ",  for absorber # ", i4 )' ) &
                        TauCoeff_LHS%Alpha_C2(j), &
                        TauCoeff_RHS%Alpha_C2(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF

    END DO j_Absorber_Loop



    !#--------------------------------------------------------------------------#
    !#   -- CHECK INDEX AND COEFFICIENT POINTER MEMBERS BY CHANNEL/ABSORBER --  #
    !#--------------------------------------------------------------------------#

    jl_Channel_Loop: DO l = 1, TauCoeff_RHS%n_Channels

      jl_Absorber_Loop: DO j = 1, TauCoeff_RHS%n_Absorbers


        ! -----------------
        ! The order indices
        ! -----------------

        IF ( ANY( TauCoeff_LHS%Order_Index(:,j,l) /= TauCoeff_RHS%Order_Index(:,j,l) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Order_Index values are different for absorber # ", i2, &
                            &", channel # ", i4 )' ) &
                          j, l
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF


        ! ---------------------
        ! The predictor indices
        ! ---------------------

        IF ( ANY( TauCoeff_LHS%Predictor_Index(:,j,l) /= TauCoeff_RHS%Predictor_Index(:,j,l) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Predictor_Index values are different for absorber # ", i2, &
                            &", channel # ", i4 )' ) &
                          j, l
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF


        ! -------------------------------
        ! The gas absorption coefficients
        ! -------------------------------

        IF ( ANY( .NOT. Compare_Float( TauCoeff_LHS%C(:,:,j,l), &
                                       TauCoeff_RHS%C(:,:,j,l), &
                                       ULP = ULP                ) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Gas absorption coefficient values are different for absorber # ", i2, &
                            &", channel # ", i4 )' ) &
                          j, l
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF

      END DO jl_Absorber_Loop

    END DO jl_Channel_Loop

  END FUNCTION Equal_TauCoeff





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Check_TauCoeff_Release
!
! PURPOSE:
!       Function to check the TauCoeff Release value.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Check_TauCoeff_Release( TauCoeff,                 &  ! Input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff:      TauCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  FUNCTION Check_TauCoeff_Release( TauCoeff,     &  ! Input
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
    TYPE( TauCoeff_type ),    INTENT( IN )  :: TauCoeff

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_TauCoeff_Release'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#               -- CHECK THAT THE RELEASE IS NOT TOO OLD --                #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff%Release < TAUCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff data update is needed. ", &
                        &"TauCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff%Release, TAUCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff%Release > TAUCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff software update is needed. ", &
                        &"TauCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff%Release, TAUCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_TauCoeff_Release





!------------------------------------------------------------------------------
!S+
! NAME:
!       Count_TauCoeff_Sensors
!
! PURPOSE:
!       Subroutine to count the number of different satellite/sensors in the
!       TauCoeff structure and set the n_Sensors field.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Count_TauCoeff_Sensors( TauCoeff,                &  ! In/Output
!                                    Use_WMO_ID = Use_WMO_ID, &  ! Optional input
!                                    RCS_Id = RCS_Id          )  ! Optional output
!
! INPUT ARGUMENTS:
!       TauCoeff_in:   Filled TauCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Use_WMO_ID:    Set this argument to use the WMO satellite and sensor
!                      IDs in the TauCoeff structure to count the number of
!                      different sensors. By default, the NCEP sensor ID is
!                      used.
!                      If = 0, use NCEP sensor ID (default)
!                         = 1, use WMO satellite/sensor ID
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       The N_SENSORS field of the input TauCoeff structure is modified.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Count_TauCoeff_Sensors( TauCoeff,   &  ! In/Output
                                     Use_WMO_ID, &  ! Optional input
                                     RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_type ),    INTENT( INOUT ) :: TauCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )    :: Use_WMO_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )   :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Use_NCEP_ID
    INTEGER :: l, j, n

    INTEGER, DIMENSION( TauCoeff%n_Channels ) :: idx



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    Use_NCEP_ID = .TRUE.
    IF ( PRESENT( Use_WMO_ID ) ) THEN
      IF ( Use_WMO_ID == SET ) Use_NCEP_ID = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALISE INVALID RESULT VALUE --                  #
    !#--------------------------------------------------------------------------#

    TauCoeff%n_Sensors = INVALID



    !#--------------------------------------------------------------------------#
    !#                     -- COUNT THE DIFFERENT SENSORS --                    #
    !#--------------------------------------------------------------------------#

    ID_Type: IF ( Use_NCEP_ID ) THEN


      !#------------------------------------------------------------------------#
      !#                     -- USING THE NCEP SENSOR ID --                     #
      !#------------------------------------------------------------------------#

      ! --------------------
      ! Check the data array
      ! --------------------

      ! -- Check that the pointer member is associated
      IF ( .NOT. ASSOCIATED( TauCoeff%NCEP_Sensor_ID ) ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( TauCoeff%NCEP_Sensor_ID == INVALID ) ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      TauCoeff%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      DO l = 2, TauCoeff%n_Channels

        ! -- Only increment sensor count if the current channel's
        ! -- value has not been previously encountered
        IF ( ALL( TauCoeff%NCEP_Sensor_ID(1:l-1) /= TauCoeff%NCEP_Sensor_ID(l) ) ) THEN
          TauCoeff%n_Sensors = TauCoeff%n_Sensors + 1
        END IF

      END DO

    ELSE ! Use WMO ID


      !#------------------------------------------------------------------------#
      !#                       -- USING THE WMO IDs --                          #
      !#------------------------------------------------------------------------#

      ! ---------------------
      ! Check the data arrays
      ! ---------------------

      ! -- Check that the pointer members are associated
      IF ( .NOT. ASSOCIATED( TauCoeff%WMO_Satellite_ID ) .OR. &
           .NOT. ASSOCIATED( TauCoeff%WMO_Sensor_ID    )      ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( TauCoeff%WMO_Satellite_ID == INVALID ) .OR. &
           ANY( TauCoeff%WMO_Sensor_ID    == INVALID )      ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      TauCoeff%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      l_Channel_Loop: DO l = 2, TauCoeff%n_Channels


        ! ------------------------------------------
        ! Count the number of channels with the SAME
        ! WMO SENSOR ID as the current channel
        ! ------------------------------------------

        n = COUNT( TauCoeff%WMO_Sensor_ID(1:l-1) == TauCoeff%WMO_Sensor_ID(l) )


        ! ----------------------------------------------
        ! How many channels have the same WMO SENSOR ID?
        ! ----------------------------------------------

        IF ( n == 0 ) THEN

          ! -- None. Increment the sensor count
          TauCoeff%n_Sensors = TauCoeff%n_Sensors + 1

        ELSE

          ! -- Some channels have the same SENSOR ID.
          ! -- Now get those corresponding array indices
          idx(1:n) = PACK( (/ ( j, j=1,l-1 ) /), &
                           TauCoeff%WMO_Sensor_ID(1:l-1) == TauCoeff%WMO_Sensor_ID(l) )

          ! -- If ALL of the previous channels' SATELLITE ID
          ! -- values are different from the current channel,
          ! -- then we have a different sensor so increment
          ! -- the sensor count.
          IF ( ALL( TauCoeff%WMO_Satellite_ID(idx(1:n)) /= TauCoeff%WMO_Satellite_ID(l) ) ) THEN
            TauCoeff%n_Sensors = TauCoeff%n_Sensors + 1
          END IF

        END IF

      END DO l_Channel_Loop

    END IF ID_Type

  END SUBROUTINE Count_TauCoeff_Sensors





!------------------------------------------------------------------------------
!S+
! NAME:
!       Version_TauCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the TauCoeff data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Version_TauCoeff( TauCoeff,       &  ! Input
!                              Version_Info,   &  ! Output
!                              RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       TauCoeff:      Filled TauCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed TauCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Version_TauCoeff( TauCoeff,     &  ! Input
                               Version_Info, &  ! Output
                               RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_type ),    INTENT( IN )  :: TauCoeff

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ----------
    ! Parameters
    ! ----------

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FILL THE VERSION INFO STRING --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, '( a,1x,"TauCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_ORDERS=",i2,2x,&
                           &"N_PREDICTORS=",i2,2x,&
                           &"N_ABSORBERS=",i2,2x,&
                           &"N_CHANNELS=",i4,2x,&
                           &"N_SENSORS=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TauCoeff%Release, TauCoeff%Version, &
                         TauCoeff%n_Orders, &
                         TauCoeff%n_Predictors, &
                         TauCoeff%n_Absorbers, &
                         TauCoeff%n_Channels, &
                         TauCoeff%n_Sensors


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_TauCoeff

END MODULE TauCoeff_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: TauCoeff_Define.f90,v 5.9 2005/02/07 18:15:01 paulv Exp $
!
! $Date: 2005/02/07 18:15:01 $
!
! $Revision: 5.9 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: TauCoeff_Define.f90,v $
! Revision 5.9  2005/02/07 18:15:01  paulv
! - Category change only.
!
! Revision 5.8  2004/09/27 19:03:13  paulv
! - Concatenation function now uses the newest version number for the result.
!
! Revision 5.7  2004/09/09 21:22:23  paulv
! - Added optional ULP_Scale argument to Equal() function.
!
! Revision 5.6  2004/09/09 20:26:38  paulv
! - Added association test in destroy function. If no pointer components
!   are associated, the function returns without modifying the n_Allocates
!   counter. Thus, calling the destroy function on an already destroyed
!   structure want flag an error.
! - Updated header documentation.
!
! Revision 5.5  2004/08/20 18:11:33  paulv
! - Removed warning message in Allocate() function when the structure contains
!   associated pointer components. Now the structure destruction is done silently.
!
! Revision 5.4  2004/08/20 15:03:51  paulv
! - File had conflicts on merge. Dunno how that happened. Fixed.
!
! Revision 5.3  2004/08/20 00:53:57  paulv
! - Upgraded to Fortran95.
! - Structure initialization now done in the derived type definition.
! - Initialize_TauCoeff() routine, and all references to it have been
!   removed.
! - Made the Associated_TauCoeff() function public.
! - Changed the INTENT of the output TauCoeff structure in the Clear_TauCoeff()
!   routine from OUT to IN OUT. Necessary to prevent memory leaks.
! - Changed the INTENT of the output TauCoeff structure in the Destroy_TauCoeff()
!   routine from OUT to IN OUT. Necessary to prevent memory leaks.
! - Added optional input argument No_Clear to Destroy_TauCoeff() function.
! - Changed the INTENT of the output TauCoeff structure in the Allocate_TauCoeff()
!   routine from OUT to IN OUT. Necessary to prevent memory leaks.
! - Added structure destruction call in allocate function in case the function
!   is called with an already allocated structure.
! - Changed the INTENT of the output TauCoeff_out structure in the Assign_TauCoeff()
!   routine from OUT to IN OUT. Necessary to prevent memory leaks.
! - Added optional input argument Check_All to Equal_TauCoeff() fuction.
! - Using a long internal string in the Version_TauCoeff() routine.
!
! Revision 5.2  2004/06/30 14:48:54  paulv
! - Corrected bug in the concatenation function along the absorber direction.
!   The order index component for the *second* structure was not being copied
!   correctly. See r4.1 log entry for info on correction of the *first* structure
!   copy.
! - Changed default version number from 1 to 3. This corresponds to the latest
!   TauCoeff data with a polynomial order cutoff of 5 for some IR and MW
!   channels/absorbers.
!
! Revision 5.1  2004/06/24 18:54:14  paulv
! - Corrected a bug in the concatenation by channel function.
! - Changed all references to transmittance coefficients to gas absorption
!   coefficients.
!
! Revision 5.0  2004/05/17 21:02:41  paulv
! - Added Sensor_Descriptor component to TauCoeff structure. Modified the
!   netCDF and Binary I/O modules to handle the new component. New TauCoeff
!   release number is 5.
!
! Revision 4.1  2004/03/08 15:56:36  paulv
! - Corrected bug in the concatenation function along the absorber direction.
!   The order index component was not being copied correctly.
!
! Revision 4.0  2004/02/17 21:51:05  paulv
! - New Release.
! - Added ORDER_INDEX to TauCoeff structure definition.
! - Changed index of:
!     o Predictor_Index from
!         0:Iuse x L x J
!       to
!         0:Iuse x J x L
!   and
!     o C coefficient array from
!         0:Iorder x 0:Iuse x L x J
!       to
!         0:Iorder x 0:Iuse x J x L
!   I did this because the previous order was not the most efficient way to
!   access the data inside the RTM transmittance module.
!
! Revision 3.11  2003/11/25 19:37:24  paulv
! - Updated header documentation
!
! Revision 3.10  2003/06/19 21:32:28  paulv
! - Added TAUCOEFF_RELEASE and TAUCOEFF_VERSION parameters to assign to TauCoeff
!   structure Release and Version members during initialisation.
! - Added optional VERSION argument to the Initialize_TauCoeff() function to
!   allow user to override default initialisation value.
! - Added Check_TauCoeff_Release() function.
!
! Revision 3.9  2003/04/14 19:53:56  paulv
! - Corrected bug in sensor counting routine when the WMO Ids were used to
!   identify separate sensors.
!
! Revision 3.8  2003/04/11 16:29:41  paulv
! - Added N_ALLOCATES member to the TauCoeff_type structure definition. This
!   value is set ONLY in the initialisation routine and is incremented in
!   the ALLOCATE_TAUCOEFF() function and decremented in the DESTROY_TAUCOEFF()
!   function. If the value of N_ALLOCATES upon exit from these routines is
!   not 1 and 0 respectively, an error is signaled. This extra check should
!   never fail due to the association tests and allocation tests performed,
!   but was added to test the code for spurious allocation/deallocations.
!
! Revision 3.7  2003/04/02 17:36:45  paulv
! - Added USE of Compare_Float_Numbers module. All floating point comparisons
!   in the Equal_TauCoeff() function are now done using the Compare_Float()
!   function.
!
! Revision 3.6  2003/03/28 19:17:37  paulv
! - Corrected bug in Concatenate_Absorber_TauCoeff() function. The second
!   structure was not being concatenated due to using the first structure
!   again! Sheesh.
!
! Revision 3.5  2003/03/28 14:41:50  paulv
! - Added TauCoeff structure concatenation functions:
!     Concatenate_Channel_TauCoeff  - concatenate structure along the channel dimension
!     Concatenate_Absorber_TauCoeff - concatenate structure along the absorber dimension
! - Removed Check_TauCoeff_Status routine and replaced it with the PRIVATE
!   Associated_TauCoeff function. All references to the former routine have been replaced
!   with the latter.
! - Updated documentation.
!
! Revision 3.4  2003/01/02 22:09:24  paulv
! - Corrected bug in Allocate() function - forgot to prefix the pointer members
!   with the structure name!
!
! Revision 3.3  2003/01/02 20:44:10  paulv
! - Removed absorber ID check function.
! - Corrected incorrect definition of Absorber_ID in the TauCoeff derived
!   type definition.
!
! Revision 3.2  2003/01/02 12:03:25  paulv
! - Added Absorber_ID member to the TauCoeff structure definition.
! - Added absorber list check function and valid value parameters.
!
! Revision 3.1  2002/12/30 16:03:52  paulv
! - New version of TauCoeff definition routine with altered members and
!   additional utility routines. Untested.
!
! Revision 2.1  2002/08/21 16:24:51  paulv
! - Updated code for new transmittance algorithm.
!
! Revision 1.2  2002/08/16 20:50:11  paulv
! - Corrected bug in TauCoeff TYPE specification.
!
! Revision 1.1  2002/08/13 20:06:03  paulv
! Initial checkin.
!
!
!
!
