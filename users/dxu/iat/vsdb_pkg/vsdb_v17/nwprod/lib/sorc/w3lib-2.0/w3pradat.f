!-----------------------------------------------------------------------
      subroutine w3pradat(idat,cdat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3PRADAT       FORMAT A DATE AND TIME INTO CHARACTERS
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM FORMS VARIOUS CHARACTER STRINGS USEFUL
!   IN DESCRIBING AN NCEP ABSOLUTE DATE AND TIME.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3PRADAT(IDAT,CDAT)
!
!   INPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     CDAT       CHARACTER*10 (8) STRINGS DESCRIBING DATE AND TIME
!                (CDAT(1) IS THE NAME OF THE DAY OF THE WEEK;
!                 CDAT(2) IS THE NAME OF THE MONTH;
!                 CDAT(3) IS THE DAY OF MONTH, YEAR;
!                 CDAT(4) IS THE DATE IN YYYY-MM-DD FORMAT;
!                 CDAT(5) IS THE DATE IN YYYY.DOY FORMAT;
!                 CDAT(6) IS THE TIME IN HH:MM:SS FORMAT;
!                 CDAT(7) IS THE MILLISECONDS IN .XXX FORMAT;
!                 CDAT(8) IS THE TIME ZONE.)
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer idat(8)
      character*(*) cdat(8)
      character*10 ctmp(8)
      character*10 cmon(12)
      data cmon/'January   ','February  ','March     ',
     &          'April     ','May       ','June      ',
     &          'July      ','August    ','September ',
     &          'October   ','November  ','December  '/
      character*10 cdow(7)
      data cdow/'Sunday    ','Monday    ','Tuesday   ',
     &          'Wednesday ','Thursday  ','Friday    ',
     &          'Saturday  '/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get day of week and day of year, convert day of week and month
!  to english names, write other formats of date and time, and
!  write time zone differential in one of three ways.
      jldayn=iw3jdn(idat(1),idat(2),idat(3))
      call w3fs26(jldayn,jy,jm,jd,jdow,jdoy)
      ctmp(1)=cdow(jdow)
      ctmp(2)='********'
      if(idat(2).ge.1.and.idat(2).le.12) ctmp(2)=cmon(idat(2))
      write(ctmp(3),'(i2,", ",i4)') idat(3),idat(1)
      write(ctmp(4),'(i4,"-",i2.2,"-",i2.2)') idat(1),idat(2),idat(3)
      write(ctmp(5),'(i4,".",i3.3)') idat(1),jdoy
      write(ctmp(6),'(i2.2,":",i2.2,":",i2.2)') idat(5),idat(6),idat(7)
      write(ctmp(7),'(".",i3.3)') idat(8)
      if(idat(4).eq.0) then
        write(ctmp(8),'("UTC")')
      elseif(mod(idat(4),100).eq.0) then
        kh=idat(4)/100
        write(ctmp(8),'("UTC",sp,i3.2,"h")') kh
      else
        kh=idat(4)/100
        km=abs(mod(idat(4),100))
        write(ctmp(8),'("UTC",sp,i3.2,"h",ss,i2.2,"m")') kh,km
      endif
      cdat=ctmp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
