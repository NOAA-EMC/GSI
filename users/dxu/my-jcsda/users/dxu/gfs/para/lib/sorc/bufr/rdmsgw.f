      SUBROUTINE RDMSGW(LUNIT,MESG,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDMSGW
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS SUBROUTINE READS THE NEXT BUFR MESSAGE FROM LOGICAL
C   UNIT LUNIT AS AN ARRAY OF INTEGER WORDS.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C 2009-03-23  D. KEYSER  -- CALL BORT IN CASE OF MESG OVERFLOW
C
C USAGE:    CALL RDMSGW (LUNIT, MESG, IRET)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     MESG     - *-WORD ARRAY CONTAINING BUFR MESSAGE READ FROM LUNIT
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = end-of-file encountered while reading
C                           from LUNIT
C                      -2 = I/O error encountered while reading
C                           from LUNIT
C
C   INPUT FILES:
C     UNIT "LUNIT" - BUFR FILE
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     ICHKSTR  LMSG
C    THIS ROUTINE IS CALLED BY: COPYBF   CPDXMM   DATEBF   DUMPBF
C                               MESGBC   MESGBF   POSAPN   POSAPX
C                               RDBFDX   READMG   UFBMEM
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      DIMENSION   MESG(*)

      CHARACTER*128 BORT_STR
      CHARACTER*8 SEC0
      CHARACTER*1 CEC0(8)

      EQUIVALENCE (SEC0,CEC0)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      SEC0 = ' '

C     Read Section 0 from the next message in the file.

      READ(LUNIT,END=100,ERR=200) SEC0

C     Confirm that the first 4 bytes contain 'BUFR' encoded in
C     CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',CEC0,4).NE.0) GOTO 200

C     Check the length of the next message to make sure it will fit
C     within the output array.

      LNMSG = LMSG(SEC0)
      IF(LNMSG*NBYTW.GT.MXMSGL) GOTO 900

C     Backspace and re-read Section 0 along with the rest of the
C     message.  This is important in case the ERR=200 exit is triggered
C     below, and in which case the calling subroutine may want to
C     attempt its own backspace and re-read of the entire message
C     (including Section 0) using subroutine RDMSGB.

      BACKSPACE (LUNIT,ERR=200)
      READ(LUNIT,END=100,ERR=200) (MESG(I),I=1,LNMSG)

C  EXITS
C  -----

      IRET = 0
      RETURN

100   IRET = -1
      RETURN

200   IRET = -2
      RETURN

900   WRITE(BORT_STR,'("BUFRLIB: RDMSGW - INPUT BUFR MESSAGE LENGTH (",
     . I6," BYTES) IS LARGER THAN LIMIT OF ",I6," BYTES")')
     . LNMSG*NBYTW,MXMSGL
      CALL BORT(BORT_STR)

      END
