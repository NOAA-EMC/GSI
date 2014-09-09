       SUBROUTINE W3YMDH4 (IDATE,IYEAR,MONTH,IDAY,IHOUR,NN)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3YMDH4          4-BYTE DATE WORD UNPACKER AND PACKER
C   AUTHOR: Brill,K.F.       ORG: NP/22       DATE: 98-07-29
C
C ABSTRACT: OBTAINS THE COMPONENTS OF THE NMC DATE WORD (NCEP Y2K
C   COMPLIANT FORM), OR GIVEN ITS COMPONENTS, FORMS AN NMC TYPE DATE
C   WORD.  THE PACKING IS DONE USING BASE 32.
C
C   If the first byte of IDATE is less than 101, then the old
C   Office Note 84 packing is assumed.  A four-digit year is
C   always returned.  To pack the "old" way, pass in a 2-digit
C   year.
C
C   This program will work for the years ranging from A.D. 101
C   through 79359.
C
C   On unpacking, years less than or equal to 100 are returned
C   as follows:
C
C	0-50   2000--2050
C	51-100 1951--2000
C
C
C PROGRAM HISTORY LOG:
C   98-07-29  K.F.BRILL
C 1999-03-15  Gilbert      -   Removed Call to W3FS11 and put its
C                              processing inline.  W3FS11 was deleted
C                              from the W3LIB.
C
C USAGE:  CALL W3YMDH4 (IDATE, IYEAR, MONTH, IDAY, IHOUR, NN)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDATE  ARG LIST  LEFT 4 BYTES OF INTEGER 64 BIT WORD, OR CAN BE
C                      CHARACTER*1 IDATE(4) OR CHARACTER*4 IDATE.
C     IYEAR  ARG LIST  INTEGER   YEAR (4 DIGITS or 2 DIGITS for ON84)
C     MONTH  ARG LIST  INTEGER   MONTH
C     IDAY   ARG LIST  INTEGER   DAY
C     IHOUR  ARG LIST  INTEGER   HOUR
C     NN     ARG LIST  INTEGER   CODE:
C                     .EQ. 0 PACK IYEAR, MONTH, IDAY, IHOUR INTO IDATE
C                     .NE. 0 UNPACK IDATE INTO IYEAR, MONTH, IDAY, IHOUR
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDATE  ARG LIST  LEFT 4 BYTES OF INTEGER 64 BIT WORD, OR CAN BE
C                      CHARACTER*1 IDATE(4) OR CHARACTER*4 IDATE.
C     IYEAR  ARG LIST  INTEGER   YEAR (4 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH
C     IDAY   ARG LIST  INTEGER   DAY
C     IHOUR  ARG LIST  INTEGER   HOUR
C
C   SUBROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     CHAR					         	F90
C     MOVA2I							W3
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT90 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
      CHARACTER IDATE(4)
C
      IF (NN.NE.0) THEN
C
	ITEMP = MOVA2I(IDATE(1))
	IF ( ITEMP .lt. 101 ) THEN
            IYEAR  = MOVA2I(IDATE(1))
            MONTH  = MOVA2I(IDATE(2))
            IDAY   = MOVA2I(IDATE(3))
            IHOUR  = MOVA2I(IDATE(4))
	    IF(IYEAR.LE.100) IYEAR=2050-MOD(2050-IYEAR,100)
	    RETURN
	END IF
	ITEMP = ITEMP - 101
	ITEMP = ITEMP * 256 + MOVA2I(IDATE(2))
	ITEMP = ITEMP * 256 + MOVA2I(IDATE(3))
	ITEMP = ITEMP * 256 + MOVA2I(IDATE(4))
	IHOUR = MOD ( ITEMP, 32 )
	ITEMP = ITEMP / 32
	IDAY  = MOD ( ITEMP, 32 )
	ITEMP = ITEMP / 32
	MONTH = MOD ( ITEMP, 32 )
	IYEAR = ITEMP / 32
C
      ELSE
C
	ITEMP = IYEAR
	IF ( ITEMP .lt. 101 ) THEN
            IDATE(1) = CHAR(IYEAR)
            IDATE(2) = CHAR(MONTH)
            IDATE(3) = CHAR(IDAY)
            IDATE(4) = CHAR(IHOUR)
	    RETURN
	END IF
	ITEMP = ITEMP * 32 + MONTH
	ITEMP = ITEMP * 32 + IDAY
	ITEMP = ITEMP * 32 + IHOUR
C*
	IDATE(4)=CHAR(MOD(ITEMP,256))
	ITEMP = ITEMP / 256
	IDATE(3)=CHAR(MOD(ITEMP,256))
	ITEMP = ITEMP / 256
	IDATE(2)=CHAR(MOD(ITEMP,256))
	ITEMP = ITEMP / 256
	ITEMP = ITEMP + 101
	IDATE(1)=CHAR(ITEMP)
C
      ENDIF
C
      RETURN
      END
