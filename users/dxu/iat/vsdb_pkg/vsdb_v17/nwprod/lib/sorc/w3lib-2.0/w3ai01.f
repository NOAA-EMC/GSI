      SUBROUTINE W3AI01(PACK,REAL8,LABEL)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3AI01         UNPACK RECORD INTO IEEE F.P.
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 89-10-17
C
C ABSTRACT: UNPACKS A RECORD IN OFFICE NOTE 84 FORMAT AND CONVERT THE
C   PACKED DATA TO IEEE REAL FLOATING POINT NUMBERS. THE
C   OFFICE NOTE 84 DATA IS BIT FOR BIT THE SAME ON THE NAS-9050 AND
C   THE CRAY.
C
C PROGRAM HISTORY LOG:
C   89-10-20  R.E.JONES
C   90-02-02  R.E.JONES  CHANGE TO CRAY FUNCTION FOR INTEGER*2, F.P.
C   90-10-11  R.E.JONES  SPECIAL VERSION OF W3AI01 TO UNPACK RECORDS
C                        PACKED BY BIG VERSION OF W3AI00. WILL DO
C                        OLD AND NEW VERSION.
C   91-03-19  R.E.JONES  MAKE SPECIAL VERSION OF W3AI01 TO UNPACK
C                        BIG RECORDS THE OPERATIONAL VERSION.
C   93-06-10  R.E.JONES  INCREACE ARRAY SIZE TO 262144 WORDS.
C   98-03-10  B. VUONG   REMOVE THE CDIR$ INTEGER=64 DIRECTIVE
C   98-11-17  Gilbert    Changed to unpack into IEEE reals for the IBM SP
C
C USAGE:  CALL W3AI01 (PACK, REAL8, LABEL)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     PACK   ARG LIST  INTEGER ARRAY WITH DATA IN OFFICE NOTE 84
C                      FORMAT TO BE UNPACKED.  
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     REAL8  ARG LIST  REAL ARRAY OF N WORDS. WHERE N IS GIVEN IN
C                      WORD 6 OF PACK. WORD 6 OF PACK MUST
C                      CONTAIN CENTER AND SCALING VALUES.
C     LABEL  ARG LIST  SIX WORD INTEGER LABEL COPIED FROM PACK, 12
C                      OFFICE NOTE 84 32 BIT ID'S THAT ARE STORED INTO
C                      six 64-bit words.
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     Q9IE32                                                  W3LIB
C
C   REMARKS: LABEL AND PACK MAY BE EQUIVALENCED.
C
C ATTRIBUTES:
C   LANGUAGE: IBM XL FORTRAN
C   MACHINE:  IBM SP
C
C$$$
C
       REAL     REAL8(*)
C
       INTEGER(2)  ITEMP(262144)
       INTEGER(8)  LABEL(6)
       INTEGER(8)  PACK(*)
       INTEGER(8)  MASK16
       INTEGER(8)  MASK32
       integer(2) i2(4)
       real(4)  rtemp(2)
       integer(8)  ktemp,jtemp(65536)
       equivalence (ktemp,rtemp(1),i2(1))
       equivalence (itemp(1),jtemp(1))
C
       SAVE
C
       DATA  MASK16/X'000000000000FFFF'/
       DATA  MASK32/X'00000000FFFFFFFF'/
C
C      MOVE OFFICE NOTE 84 12 32 BIT ID'S INTO LABEL
C
       DO 10 I = 1,6
         LABEL(I) = PACK(I)
 10    CONTINUE
C
C GET WORD COUNT, AVERAGE VALUE, SCALING FACTOR, J, A , N.
C
         J   = IAND(LABEL(4),MASK16)
         IF (J.EQ.0) THEN
           J   = IAND(LABEL(6),MASK32)
           IF (J.EQ.0) THEN
             PRINT *,' W3AI01 ERROR, NUMBER OF WORDS IN GRID IS 0'
             RETURN
           ENDIF
           IF (J.GT.262144) THEN
             PRINT *,' W3AI01 ERROR, NUMBER OF WORDS IN GRID IS ',J
             PRINT *,' THERE IS A LIMIT OF 262144'
             RETURN
           ENDIF
         ENDIF
C
C        CONVERT IBM 32 BIT MEAN VALUE TO IEEE F.P. NUMBER
C
C         CALL USSCTC(LABEL(5),5,A,1)
         ktemp=LABEL(5)
         call q9ie32(rtemp(2),rtemp(1),1,istat)
         A=rtemp(1)
C
C        GET SCALING VALUE N, CAN BE NEGATIVE (INTEGER*2 TWO'S COMPL.)
C
C         CALL USICTC(LABEL(6),3,N,1,2)
         ktemp=LABEL(6)
         n=i2(2)
C
         TWON = 2.0 ** (N - 15)
C
C UNPACK, CONVERT TO REAL 64 BIT FLOATING POINT DATA
C
C         CALL USICTC(PACK(7),1,ITEMP,J,2)
         jtemp(1:65536)=pack(7:65542)
C
         DO 20 I = 1,J
           REAL8(I) = FLOAT(ITEMP(I)) * TWON + A
  20     CONTINUE
C
         RETURN
       END
