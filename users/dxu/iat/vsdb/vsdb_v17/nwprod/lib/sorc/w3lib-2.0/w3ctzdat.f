!-----------------------------------------------------------------------
      subroutine w3ctzdat(ntz,idat,jdat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3CTZDAT       CHANGE THE TIME ZONE OF A DATE AND TIME
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM CONVERTS AN NCEP ABSOLUTE DATE AND TIME
!   TO ANOTHER TIME ZONE.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3CTZDAT(NTZ,IDAT,JDAT)
!
!   INPUT VARIABLES:
!     NTZ        INTEGER NEW TIME ZONE DIFFERENTIAL FROM UTC
!                IN SIGNED HH OR HHMM FORMAT
!                (IF NTZ IS INVALID, NO CHANGE IS MADE.)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
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
      integer idat(8),jdat(8)
      real rinc1(5),rinc2(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  determine if the input time zone is in valid hh or hhmm format
      if(ntz.gt.-24.and.ntz.lt.24) then
        itz=ntz*100
      elseif(ntz.eq.mod(ntz/100,24)*100+mod(mod(ntz,100),60)/30*30) then
        itz=ntz
      else
        itz=idat(4)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  determine new time of day, putting into reduced form
!  and possibly adjust the date as well
      rinc1(1)=0
      rinc1(2)=idat(5)+itz/100-idat(4)/100
      rinc1(3)=idat(6)+mod(itz,100)-mod(idat(4),100)
      rinc1(4)=idat(7)
      rinc1(5)=idat(8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
      jdat(4)=itz
      jdat(5:8)=nint(rinc2(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
