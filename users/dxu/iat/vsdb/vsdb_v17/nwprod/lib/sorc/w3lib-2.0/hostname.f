      CHARACTER(15) FUNCTION HOSTNAME()
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  HOSTNAME      RETURN CURRENT HOSTNAME
C   PRGMMR: IREDELL          ORG: NP23        DATE:1998-06-04
C
C ABSTRACT: RETURN A 15-CHARACTER NAME OF THE CURRENT COMPUTER NODE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   2003-08-15  Gilbert - XLF 8.1 doesn't like calling gethostname()
C                         with a constant argument.  Put size of hostname
C                         in a variable and passed that to gethostname().
C
C USAGE:    ...=HOSTNAME()
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C     HOSTNAME - CHARACTER(15) HOSTNAME
C
C SUBPROGRAMS CALLED:
C   GETHOSTNAME  GET NAME OF CURRENT HOST
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  ORIGIN
C
C$$$
      integer :: nsize=15
      HOSTNAME='               '
      CALL GETHOSTNAME(HOSTNAME,nsize)
      END
