!*****************************************************************************
! Header file to write GrADS GPV and station data. It includes the interface
! of utility functions written by C language.
!
! Created by Y.Tahara        Sep 2000
!*****************************************************************************


module GrADS_Hdr

  implicit none

  !--- Parameters ---

  integer(4),parameter :: GRADS_MAXLEN_FILENAME = 250
  integer(4),parameter :: GRADS_LEN_ID          = 8

!#define LEN_LEVELEM_NAME 10
! integer(4),parameter :: GRADS_LEN_LEVELEMNAME = LEN_LEVELEM_NAME
  integer(4),parameter :: GRADS_LEN_LEVELEMNAME = 10

  integer(4),parameter :: GRADS_MAXLEN_COMMENT  =  80
  integer(4),parameter :: GRADS_MAX_ELEMS       = 100
  integer(4),parameter :: GRADS_MAX_LEVELS      = 300

  real(4),parameter    :: GRADS_MISSING         = -32768.


end module GrADS_Hdr
