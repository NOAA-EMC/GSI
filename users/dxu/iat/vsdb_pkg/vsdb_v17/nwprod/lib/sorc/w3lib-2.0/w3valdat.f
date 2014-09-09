!-----------------------------------------------------------------------
      logical function w3valdat(idat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3VALDAT       DETERMINE THE VALIDITY OF A DATE AND TIME
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS LOGICAL FUNCTION RETURNS TRUE IF THE INPUT IS A VALID
!   NCEP ABSOLUTE DATE AND TIME.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  ...=W3VALDAT(IDAT)
!
!   INPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     W3VALDAT   LOGICAL TRUE IF IDAT IS A VALID NCEP DATE AND TIME
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!     W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer idat(8)
      real rinc1(5),rinc2(5)
      integer jdat(8)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  essentially move the date and time by a zero time interval
!  and see if the same date and time is returned
      rinc1(1)=0
      rinc1(2:5)=idat(5:8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
!  the time zone is valid if it is in signed hhmm format
!  with hh between -23 and 23 and mm equal to 00 or 30
      jdat(4)=mod(idat(4)/100,24)*100+mod(mod(idat(4),100),60)/30*30
      jdat(5:8)=nint(rinc2(2:5))
      w3valdat=all(idat.eq.jdat)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
