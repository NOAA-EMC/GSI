!-----------------------------------------------------------------------
      subroutine w3kind(kindreal,kindint)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3KIND        RETURN THE real kind and integer kind used 
!                           in w3 lib
!   AUTHOR: Jun Wang                         DATE: 11-06-24
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE REAL KIND AND THE INTEGER KIND
!           THAT THE W3 LIB IS COMPILED WITH.
!
! PROGRAM HISTORY LOG:
! 2011-06-24  Jun Wang
!
! USAGE:  CALL W3KIND(kindreal,kindint)
!
!   OUTPUT VARIABLES:
!     KINDREAL   INTEGER    KIND OF REAL NUMBER IN W3 LIB
!     KINDINTL   INTEGER    KIND OF INTEGER NUMBER IN W3 LIB
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      IMPLICIT NONE
!
      integer,intent(out) :: kindreal,kindint
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get real kind from a real number
      kindreal=kind(1.0)
      kindint=kind(1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

