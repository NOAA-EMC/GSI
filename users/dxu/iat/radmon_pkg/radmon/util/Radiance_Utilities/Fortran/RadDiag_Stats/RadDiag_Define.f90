!
! RadDiag_Define
!
! Container module for RadDiag objects
!

MODULE RadDiag_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE RadDiag_Hdr_Define , ONLY: RadDiag_Hdr_Scalar_type  , &
                                 RadDiag_Hdr_Channel_type , &
                                 RadDiag_Hdr_type         , &
                                 RadDiag_Hdr_Associated   , &
                                 RadDiag_Hdr_Destroy      , &
                                 RadDiag_Hdr_Create       , &
                                 RadDiag_Hdr_Inspect      , &
                                 RadDiag_Hdr_DefineVersion
  USE RadDiag_Data_Define, ONLY: RADDIAG_N_FPELEMENTS, &
                                 RADDIAG_N_CHELEMENTS, &
                                 RADDIAG_N_PRELEMENTS, &
                                 RadDiag_Data_Scalar_type  , &
                                 RadDiag_Data_Channel_type , &
                                 RadDiag_Data_type         , &
                                 RadDiag_Data_Associated   , &
                                 RadDiag_Data_Destroy      , &
                                 RadDiag_Data_Create       , &
                                 RadDiag_Data_Inspect      , &
                                 RadDiag_Data_DefineVersion
  ! Disable implicit typing
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  ! Everything private by default
  PRIVATE
  ! RadDiag_Hdr entities
  ! ...Datatypes
  PUBLIC :: RadDiag_Hdr_Scalar_type
  PUBLIC :: RadDiag_Hdr_Channel_type
  PUBLIC :: RadDiag_Hdr_type
  ! ...Procedures
  PUBLIC :: RadDiag_Hdr_Associated
  PUBLIC :: RadDiag_Hdr_Destroy
  PUBLIC :: RadDiag_Hdr_Create
  PUBLIC :: RadDiag_Hdr_Inspect
  PUBLIC :: RadDiag_Hdr_DefineVersion
  ! RadDiag_Data entities
  ! ...Parameters
  PUBLIC :: RADDIAG_N_FPELEMENTS
  PUBLIC :: RADDIAG_N_CHELEMENTS
  PUBLIC :: RADDIAG_N_PRELEMENTS
  ! ...Datatypes
  PUBLIC :: RadDiag_Data_Scalar_type
  PUBLIC :: RadDiag_Data_Channel_type
  PUBLIC :: RadDiag_Data_type
  ! ...Procedures
  PUBLIC :: RadDiag_Data_Associated
  PUBLIC :: RadDiag_Data_Destroy
  PUBLIC :: RadDiag_Data_Create
  PUBLIC :: RadDiag_Data_Inspect
  PUBLIC :: RadDiag_Data_DefineVersion
  ! RadDiag entities
  ! ...Procedures
  PUBLIC :: RadDiag_DefineVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RadDiag_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL RadDiag_DefineVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE RadDiag_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE RadDiag_DefineVersion

END MODULE RadDiag_Define
