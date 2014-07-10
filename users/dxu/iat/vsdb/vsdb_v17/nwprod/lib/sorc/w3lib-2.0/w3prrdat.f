!-----------------------------------------------------------------------
      subroutine w3prrdat(rinc,cinc)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3PRRDAT       FORMAT A TIME INTERVAL INTO CHARACTERS
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM FORMS VARIOUS CHARACTER STRINGS USEFUL
!   IN DESCRIBING AN NCEP RELATIVE TIME INTERVAL.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3PRRDAT(RINC,CINC)
!
!   INPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!
!   OUTPUT VARIABLES:
!     CINC       CHARACTER*10 (8) STRINGS DESCRIBING TIME INTERVAL
!                (CINC(1) IS THE SIGNED INTEGER NUMBER OF DAYS;
!                 CINC(2) IS THE TIME IN HH:MM:SS FORMAT;
!                 CINC(3) IS THE MILLISECONDS IN .XXX FORMAT;
!                 CINC(4) IS THE SIGNED REAL NUMBER OF DAYS;
!                 CINC(5) IS THE SIGNED REAL NUMBER OF HOURS;
!                 CINC(6) IS THE SIGNED REAL NUMBER OF MINUTES;
!                 CINC(7) IS THE SIGNED REAL NUMBER OF SECONDS;
!                 CINC(8) IS THE SIGNED REAL NUMBER OF MILLISECONDS.)
!
! SUBPROGRAMS CALLED:
!     W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      real rinc(5)
      real rinc2(5)
      character*(*) cinc(8)
      character*10 ctmp(8)
      character*5 c
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  reduce date to second form but write sign only for days,
!  and write in units of days, hours, minutes, second and milliseconds.
      c='DHMSm'
      call w3reddat(0,rinc,rinc2)
      if(rinc2(1).ge.0) then
        write(ctmp(1),'("+",i6)') nint(rinc2(1))
      else
        write(ctmp(1),'("-",i6)') -nint(rinc2(1))
      endif
      write(ctmp(2),'(i2.2,":",i2.2,":",i2.2)') abs(nint(rinc2(2:4)))
      write(ctmp(3),'(".",i3.3)') abs(nint(rinc2(5)))
      do it=1,5
        call w3reddat(it,rinc,rinc2)
        write(ctmp(3+it),'(sp,g9.3,a1)') rinc2(it),c(it:it)
      enddo
      cinc=ctmp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
