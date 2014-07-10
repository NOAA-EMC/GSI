      SUBROUTINE W3FP12(ID8, IFLAG, IDPDS, ICENT, ISCALE, IER)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FP12        CREATES THE PRODUCT DEFINITION SECTION
C   PRGMMR: MCCLEES          ORG: NMC421      DATE:92-01-14
C
C ABSTRACT: FORMATS THE PRODUCT DEFINITION SECTION ACCORDING TO THE
C   SPECIFICATIONS SET BY WMO. USING  O.N. 84 ID'S  (1ST 8 WORDS)
C   AS THE INPUT DATA.  NEW SUBROUTINE CORRESPONDS TO THE REVISION
C   #1 OF THE WMO GRIB STANDARDS MADE MARCH 15, 1991.
C
C PROGRAM HISTORY LOG:
C   91-07-30  MCCLEES,A.J. NEW SUBROUTINE WHICH FORMATS THE PDS
C                          SECTION FROM THE O.N. 84 ID'S FROM THE GRIB
C                          EDITION  1 DATED MARCH 15, 1991.
C
C   92-01-06  MCCLEES,A.J. DELETE PARAMATER 202 (ACCUMULATED EVAP)
C                          AND MAKE PARAMETER 57 (EVAPORATION) THE
C                          EQUIVALENT OF O.N.84 117.
C   92-11-02  R.E.JONES    CORRECTION AT SAME LEVEL AS W3FP12 IN
C                          V77W3LIB ON HDS  92-09-30
C   93-03-29  R.E.JONES    ADD SAVE STATEMENT
C   93-04-16  R.E.JONES    ADD 176, 177 LAT, LON TO TABLES
C   93-08-03  R.E.JONES    ADD 156 (CIN), 204 (DSWRF), 205 (DLWRF)
C                          211 (USWRF), 212 (ULWRF)  TO TABLES
C   95-02-07  R.E.JONES    CHANGE PDS BYTE 4, VERSION NUMBER TO 2.
C   95-07-14  R.E.JONES    CORRECTION FOR SFC LFT X
C   98-03-10  B. VUONG     REMOVE THE CDIR$ INTEGER=64 DIRECTIVE
C   98-12-21  Gilbert    Replaced Function ICHAR with mova2i.
C   99-02-15  B. FACEY     REPLACE W3FS04 WITH W3MOVDAT.
C 1999-03-15  Gilbert     Specified 8-byte integer array explicitly for ID8
C   99-03-22  B. FACEY     REMOVE THE DATE RECALCULATION FOR MEAN
C                          CHARTS.  THIS INCLUDES THE PREVIOUS
C                          CHANGE TO W3MOVDAT.
C
C USAGE:    CALL W3FP12  (ID8, IFLAG, IDPDS, ICENT, ISCALE, IER)
C   INPUT ARGUMENT LIST:
C     ID8      - FIRST 8 ID WORKDS (O.N.84) INTEGER*4
C     ICENT    - CENTURY, 2 DIGITS, FOR 1991 IT IS 20.
C     IFLAG    - INDICATION OF INCLUSION OR OMISSION OF GRID DEFINITION
C                AND/OR BIT MAP CODE        CHARACTER*1
C     ISCALE   - 10 SCALER INTEGER*4
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IDPDS    - GRIB PRODUCT DEFINITION SECTION  CHARACTER*1 (28)
C     IER      = 0 COMPLETED SMOOTHLY
C              = 1 INDICATOR PARAMETER N.A. TO GRIB
C              = 2 LEVEL INDICATOR N.A. TO GRIB
C              = 3 TIME RANGE N.A. TO GRIB NOTATION
C              = 4 LAYERS OR LEVELS N.A. TO GRIB
C   OUTPUT FILES:
C     FT06F001 - SELF-EXPLANATORY ERROR MESSAGES
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
C
        INTEGER     E1
        INTEGER     E2
        INTEGER     F1
        INTEGER     F2
        DATA        F1/0/, F2/0/
        INTEGER     HH         (163)
        INTEGER(8)  ID8        (  4)
        INTEGER(8)  IDWK       (  4)
        INTEGER(8)  MSK1,MSK2,MSK3,MSK4,MSK5,MSK6,MSK7
        INTEGER     ISIGN
        INTEGER     ISCALE
        INTEGER     ICENT
        INTEGER     LL         (163)
        INTEGER     L
        INTEGER     M
        INTEGER     N
        INTEGER     Q
        INTEGER     S1
        INTEGER     T
        DATA        T/0/
C
        CHARACTER*1 IDPDS      (28)
        CHARACTER*1 IFLAG
        CHARACTER*1 IHOLD      ( 8)
        CHARACTER*1 IPDS1      ( 8)
        CHARACTER*1 KDATE      ( 8)
        CHARACTER*1 LIDWK      (32)
C
        EQUIVALENCE (IDWK(1),LIDWK(1))
        EQUIVALENCE (L,IPDS1(1))
        EQUIVALENCE (NBYTES,IHOLD(1))
        EQUIVALENCE (JDATE,KDATE(1))
        REAL  RINC(5) 
        INTEGER  NDATE(8), MDATE(8)
C
        DATA  LL    /   8,   8,   9, 255, 255, 255,   1,   6, 255, 255,
     &                 16,  24,  19,  23,  20,  21,  17,  18, 255, 180,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                 55,  50,  48,  56,  49,  57,  80,  81,  71, 255,
     &                 40,  42,  72,  74,  73, 255, 255, 255, 255, 255,
     &                304, 305,  95,  88, 101,  89, 104, 255, 117, 255,
     &                 97,  98,  90, 105,  94, 255, 255,  93, 188, 255,
     &                255, 255, 255, 211, 255, 255, 255, 255, 255, 255,
     &                255, 384, 161, 255, 255, 169,  22, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 400, 389, 385, 388, 391, 386, 390, 402, 401,
     &                404, 403, 204, 255, 255, 255, 255, 255, 255, 255,
     &                255, 255, 195, 194, 255, 255, 255, 255, 255, 255,
     &                255, 255, 112, 116, 114, 255, 103,  52, 255, 255, 
     &                255, 255, 119, 157, 158, 159, 255, 176, 177, 392, 
     &                192, 190, 199, 216, 189, 193, 191, 210, 198, 255,
     &                255,   1, 255/
        DATA  HH    /   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,
     &                 11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
     &                 21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
     &                 31,  32,  33,  33,  34,  34,  35,  36,  37,  38,
     &                 39,  40,  41,  42,  43,  44,  45,  46,  47,  48,
     &                 49,  50,  51,  52,  53,  54,  55,  56,  57,  58,
     &                 59,  60,  61,  62,  63,  64,  65,  66,  67,  68,
     &                 69,  70,  71,  72,  73,  74,  75,  76,  77,  78,
     &                 79,  80,  81,  82,  83,  84,  85,  86,  87,  88,
     &                 89,  90,  91,  92,  93,  94,  95,  96,  97,  98,
     &                 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
     &                109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
     &                119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
     &                129, 130, 131, 132, 133, 134, 135, 136, 137, 150,
     &                151, 152, 156, 157, 158, 159, 175, 176, 177, 201,
     &                204, 205, 207, 208, 209, 211, 212, 213, 216, 218,
     &                220, 222, 255/
C       DATA  MSK1  /Z'00000FFF'/,
C    &        MSK2  /Z'0FFFFF00'/,
C    &        MSK3  /Z'0000007F'/,
C    &        MSK4  /Z'00000080'/,
C    &        MSK5  /Z'F0000000'/,
C    &        MSK6  /Z'00000200'/,
C    &        MSK7  /Z'000000FF'/
C    CHANGE HEX TO DECIMAL TO MAKE SUBROUTINE MORE PORTABLE
        DATA  MSK1  /4095/,
     &        MSK2  /268435200/,
     &        MSK3  /127/,
     &        MSK4  /128/,
     &        MSK5  /Z'00000000F0000000'/
     &        MSK6  /512/,
     &        MSK7  /255/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C$          1.0 INITIALIZATION - NO. OF ENTRIES IN INDCATOR PARM.
C$                             - NO. OF ENTRIES IN TYPE LEVEL
C
       IQ = 163
C
C$          1.1 COPY O.N. 84 ID'S INTO WORK SPACE
C
       DO 100 N = 1,4
         IDWK(N) = ID8(N)
 100   CONTINUE
C ---------------------------------------------------------------------
C             2.0 NO. OF OCTETS IN THE PDS IN THE FIRST 3
C$            2.1 SET CNTR ID, DATA TYPE, GRID DEF AND FLAG
C
       NBYTES    = 28
       IDPDS(1)  = IHOLD(6)
       IDPDS(2)  = IHOLD(7)
       IDPDS(3)  = IHOLD(8)
       IDPDS(4)  = CHAR(2)
       IDPDS(5)  = CHAR(7)
       IDPDS(6)  = LIDWK(30)
       JSCALE    = ISCALE
       IF (JSCALE.LT.0) THEN
         JSCALE    = -JSCALE
         IDPDS(27) = CHAR(128)
         IDPDS(28) = CHAR(JSCALE)
       ELSE
         IDPDS(27) = CHAR(0)
         IDPDS(28) = CHAR(JSCALE)
       END IF
C
         IF (LIDWK(30) .EQ. CHAR (69)) THEN
           IF (LIDWK(29) .EQ. CHAR(3)) THEN
             IDPDS(6) = CHAR(68)
           ELSE IF (LIDWK(29) .EQ. CHAR(4)) THEN
             IDPDS(6) = CHAR(69)
           ENDIF
         ENDIF
         IF (LIDWK(30) .EQ. CHAR (78)) THEN
           IF (LIDWK(29) .EQ. CHAR(3)) THEN
             IDPDS(6) = CHAR(77)
           ELSE IF (LIDWK(29) .EQ. CHAR(4)) THEN
             IDPDS(6) = CHAR(78)
           ENDIF
         ENDIF
       IDPDS(7)  = LIDWK(20)
         IF (LIDWK(20) .EQ. CHAR (26)) IDPDS(7) = CHAR(6)
       IDPDS(8)  = IFLAG
       IDPDS(24) = CHAR(0)
       IDPDS(26) = CHAR(0)
C---------------------------------------------------------------------
C
C$            3.0 FORM INDICATOR PARAMETER
C
       Q = ISHFT(IDWK(1),-52_8)
         DO 300 I = 1,IQ
           II = I
           IF (Q .EQ. LL(I)) GO TO 310
 300   CONTINUE
C
       IER = 1
       PRINT 320, IER, Q, ID8
 320   FORMAT (' W3FP12  (320) - IER = ',I2,', Q = ',I3,/,
     & ' OFFICE NOTE 84 PARAMETER N.A. IN GRIB',
     & /,1X,4(Z16,' '))
       RETURN
C
 310   I        = II
       S1       = IAND(ISHFT(IDWK(1),-40_8),MSK1)
       C1       = ISHFT(IAND(IDWK(1),MSK2),-8_8)
       ISIG1    = IAND(IDWK(1),MSK4)
       E1       = IAND(IDWK(1),MSK3)
       IF (ISIG1 .NE. 0) E1 = -E1
       M        = ISHFT(IAND(ISHFT(IDWK(2),-32_8),MSK5),-28_8)
       N        = ISHFT(IAND(IDWK(2),MSK5),-28_8)
       KS       = ISHFT(IAND(ISHFT(IDWK(3),-32_8),MSK6),-8_8)
       IF (M.NE.0) THEN
         C2    = ISHFT(IAND(IDWK(2),MSK2),-8_8)
         ISIG2 = IAND(IDWK(2),MSK4)
         E2    = IAND(IDWK(2),MSK3)
         IF (ISIG2 .NE. 0) E2 = -E2
       ENDIF
       IDPDS(9) = CHAR(HH(I))
C
C  N IS A SPECIAL TEST FOR WAVE HGTS, M AND KS ARE SPECIAL FOR
C  ACCUMULATED PRECIP
C
       IF (N .EQ. 5 .AND. Q .EQ. 1) THEN
         IDPDS(9) = CHAR (222)
       ENDIF
       IF (KS .EQ. 2) THEN
         IF (M .EQ. 0 .AND. Q .EQ. 8) THEN
           IDPDS(9) = CHAR (211)
         END IF
C
         IF (M .EQ. 0 .AND. Q .EQ. 1) THEN
           IDPDS(9) = CHAR (210)
         ENDIF
C
         IF (M .EQ. 1 .AND. Q .EQ. 1) THEN
           IER = 1
           PRINT 330, IER, ID8
 330       FORMAT (' W3FP12 (330) - IER =',I2,/,
     &     ' OFFICE NOTE 84 PARAMETER N.A. IN GRIB',
     &     /,1X,4(Z16,' '))
           RETURN
         ENDIF
       ENDIF
C
C$            4.0 DETERMINE IF LAYERS OR LEVEL AND FORM TYPE
C
C  ......... M  = THE M MARKER FROM O.N.84  CHECK ABOVE
C  ......... S1 = S1 TYPE OF SURFACE
C
       IF (M .EQ. 0) THEN
         IF (S1.EQ.0.AND.(Q.EQ.176.OR.Q.EQ.177)) THEN
           IDPDS(10) = CHAR(0)
           IDPDS(11) = CHAR(0)
           IDPDS(12) = CHAR(0)
C
         ELSE IF (S1 .EQ. 8) THEN
           IDPDS(10) = CHAR (100)
           L         = C1 * (10. ** E1) + .5
           IDPDS(11) = IPDS1(7)
           IDPDS(12) = IPDS1(8)
C
         ELSE IF (S1 .EQ. 1) THEN
           IDPDS(10) = CHAR (103)
           L         = C1 * (10. ** E1) + .5
           IDPDS(11) = IPDS1(7)
           IDPDS(12) = IPDS1(8)
C
         ELSE IF (S1 .EQ. 6) THEN
           IDPDS(10) = CHAR (105)
           L         = C1 * (10. ** E1) + .5
           IDPDS(11) = IPDS1(7)
           IDPDS(12) = IPDS1(8)
C
         ELSE IF (S1 .EQ. 7) THEN
           IDPDS(10) = CHAR (111)
C    CONVERT FROM METERS TO CENTIMETERS
           IF (ISIG1 .NE. 0) E1 = E1 + 2
           L         = C1 * (10. ** E1) + .5
           IDPDS(11) = IPDS1(7)
           IDPDS(12) = IPDS1(8)
C
         ELSE IF (S1.EQ.148 .OR. S1 .EQ. 144 .OR. S1 .EQ. 145) THEN
           IDPDS(10) = CHAR (107)
           L         = (C1 * (10. ** E1) * 10**4) + .5
           IDPDS(11) = IPDS1(7)
           IDPDS(12) = IPDS1(8)
C
         ELSE IF (S1 .EQ. 16) THEN
           L     = C1 * (10. ** E1) + .5
             IF (L .EQ. 273) THEN
               IDPDS(10) = CHAR (4)
               IDPDS(11) = CHAR (0)
               IDPDS(12) = CHAR (0)
             ELSE
               IER = 2
               PRINT 410, IER, S1, ID8
               RETURN
             ENDIF
C
         ELSE IF (S1 .EQ. 19) THEN
           L     = C1 * (10. ** E1) + .5
           IDPDS(10) = CHAR (113)
           IDPDS(11) = IPDS1(7)
           IDPDS(12) = IPDS1(8)
C
C   SET LEVEL AND PARAMETER FOR MSL PRESSURE
C
         ELSE IF (S1 .EQ. 128) THEN
           IF (Q.EQ.8) THEN
             IDPDS(9) = CHAR(2)
           END IF
           IDPDS(10) = CHAR (102)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 129) THEN
           IDPDS(10) = CHAR (1)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 130) THEN
           IDPDS(10) = CHAR (7)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 131) THEN
           IDPDS(10) = CHAR (6)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 133) THEN
           IDPDS(10) = CHAR (1)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 136) THEN
           IF (Q.EQ.8) THEN
             IF (T.EQ.2.AND.F1.EQ.0.AND.F2.EQ.3) THEN
               IDPDS(9) = CHAR (137)
             ELSE
             IDPDS(9) = CHAR (128)
           END IF
           END IF
           IDPDS(10) = CHAR (102)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 137) THEN
           IF (Q.EQ.8) THEN
             IDPDS(9) = CHAR (129)
           END IF
           IDPDS(10) = CHAR (102)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE IF (S1 .EQ. 138) THEN
           IF (Q.EQ.8) THEN
             IDPDS(9) = CHAR (130)
           END IF
           IDPDS(10) = CHAR (102)
           IDPDS(11) = CHAR (0)
           IDPDS(12) = CHAR (0)
C
         ELSE
           IER = 2
           PRINT 410, IER, S1, ID8
 410       FORMAT (' W3FP12 (410) - IER = ',I2,', S1 = ',I5,/,
     &     ' SURFACE TYPE N.A. IN GRIB',/,' ID8 = ',
     &     4(Z16,' '))
           RETURN
         ENDIF
C
       ELSE IF (M .EQ. 1) THEN
         IF ((S1 .EQ. 8) .AND. (Q .EQ. 1)) THEN
           IDPDS(9)  = CHAR(101)
           IDPDS(10) = CHAR(101)
           JJJ       = ((C1 * 10. ** E1) * .1) + .5
           IDPDS(11) = CHAR(JJJ)
           KKK       = ((C2 * 10. ** E2) * .1) + .5
           IDPDS(12) = CHAR(KKK)
         END IF
C
       ELSE IF (M .EQ. 2) THEN
         IF (S1 .EQ. 8) THEN
           IDPDS(10) = CHAR(101)
           JJJ       = ((C1 * 10. ** E1) * .1) + .5
           IDPDS(11) = CHAR(JJJ)
           KKK       = ((C2 * 10. ** E2) * .1) + .5
           IDPDS(12) = CHAR(KKK)
           IF (IDPDS(9) .EQ. CHAR(131)) IDPDS(12) = CHAR(100) 
C
         ELSE IF (S1 .EQ. 1) THEN
           IDPDS(10) = CHAR(104)
           JJJ       = ((C1 * 10. ** E1) * .1) + .5
           IDPDS(11) = CHAR(JJJ)
           KKK       = ((C2 * 10. ** E2) * .1) + .5
           IDPDS(12) = CHAR(KKK)
C
         ELSE IF (S1 .EQ. 6) THEN
           IDPDS(10) = CHAR(106)
           JJJ       = ((C1 * 10. ** E1) * .1) + .5
           IDPDS(11) = CHAR(JJJ)
           KKK       = ((C2 * 10. ** E2) * .1) + .5
           IDPDS(12) = CHAR(KKK)
C
         ELSE IF (S1.EQ.148 .OR. S1 .EQ. 144 .OR. S1 .EQ. 145) THEN
           IDPDS(10) = CHAR(108)
           JJJ       = ((C1 * 10. ** E1) * 10**2) + .5
           IDPDS(11) = CHAR(JJJ)
           KKK       = ((C2 * 10. ** E2) * 10**2) + .5
           IDPDS(12) = CHAR(KKK)
C
         ELSE
           IER = 2
           PRINT 420, IER, S1, ID8
 420       FORMAT (' W3FP12 (420) - IER = ',I2,', S1 = ',I5,/,
     &     ' SURFACE LAYERS N.A. IN GRIB',
     &     /,' ID8= ',4(Z16,' '))
           RETURN
         ENDIF
       ELSE IF (M .GT. 2) THEN
         IER = 4
         PRINT 500, IER, M, ID8
 500     FORMAT ('W3FP12 (500) - IER = ',I2,', M = ',/,
     &   ' THE M FROM O.N. 84 N.A. IN GRIB',
     &   /,' ID8 = ',4(Z16,' '))
         RETURN
       ENDIF
C
C$             6.0 DATE - YR.,MO,DA,& INITIAL HR AND CENTURY
C
       IDPDS(13) = LIDWK(25)
       IDPDS(14) = LIDWK(26)
       IDPDS(15) = LIDWK(27)
       IDPDS(16) = LIDWK(28)
       IDPDS(17) = CHAR(0)
       IDPDS(25) = CHAR(ICENT)
C---------------------------------------------------------------------
C
C$             OCTET (17) N.A. FROM O.N. 84 DATA
C
C$             7.0 INDICATOR OF TIME UNIT, TIME RANGE 1 AND 2, AND TIME
C                  RANGE FLAG
C
       T  = ISHFT((IAND(IDWK(1),MSK5)),-28_8)
       F1 = IAND(ISHFT(IDWK(1),-32_8),MSK7)
       F2 = IAND(ISHFT(IDWK(2),-32_8),MSK7)
       IF (T .EQ. 0) THEN
         IDPDS(18) = CHAR (1)
         IDPDS(19) = CHAR (F1)
         IDPDS(20) = CHAR (0)
         IDPDS(21) = CHAR (0)
         IDPDS(22) = CHAR (0)
         IDPDS(23) = CHAR (0)
C
       ELSE IF (T .EQ. 1) THEN
           PRINT 710, T, ID8
           IER = 3
           RETURN
C
       ELSE IF (T .EQ. 2) THEN
           IF (mova2i(IDPDS(9)).NE.137) THEN
             PRINT 710, T, ID8
             IER = 3
             RETURN
           END IF
C
       ELSE IF (T .EQ. 3) THEN
         IF (Q .EQ. 89 .OR. Q .EQ. 90 .OR. Q .EQ. 94
     &   .OR. Q .EQ. 105) THEN
C
           IDPDS(18) = CHAR (1)
C      CORRECTION FOR 00 HR FCST
           ITEMP     = F1 - F2
           IF (ITEMP.LT.0) ITEMP = 0
C          IDPDS(19) = CHAR (F1 - F2)
           IDPDS(19) = CHAR (ITEMP)
           IDPDS(20) = CHAR (F1)
           IDPDS(21) = CHAR (4)
           IDPDS(22) = CHAR (0)
           IDPDS(23) = CHAR (0)
C
         ELSE
           IDPDS(18) = CHAR (1)
C      CORRECTION FOR 00 HR FCST
           ITEMP     = F1 - F2
           IF (ITEMP.LT.0) ITEMP = 0
C          IDPDS(19) = CHAR (F1 - F2)
           IDPDS(19) = CHAR (ITEMP)
           IDPDS(20) = CHAR (F1)
           IDPDS(21) = CHAR (5)
           IDPDS(22) = CHAR (0)
           IDPDS(23) = CHAR (0)
         END IF
C
       ELSE IF (T .EQ. 4) THEN
C
         IF (F1 .EQ. 0 .AND. F2 .NE. 0) THEN
            IDPDS(18) = CHAR (4)
            IDPDS(19) = CHAR (0)
            IDPDS(20) = CHAR (1)
            IDPDS(21) = CHAR (124)
            L         = F2
            IDPDS(22) = IPDS1(7)
            IDPDS(23) = IPDS1(8)
C
         ELSE IF (F1 .NE. 0 .AND. F2 .EQ. 0) THEN
           IDPDS(18) = CHAR (2)
           IDPDS(19) = CHAR (0)
           IDPDS(20) = CHAR (1)
           IDPDS(21) = CHAR (124)
           L         = F1
           IDPDS(22) = IPDS1(7)
           IDPDS(23) = IPDS1(8)
C
         ENDIF
C
       ELSE IF (T .EQ. 5) THEN
         IDPDS(18) = CHAR (1)
C      CORRECTION FOR 00 HR FCST
         ITEMP     = F1 - F2
         IF (ITEMP.LT.0) ITEMP = 0
C        IDPDS(19) = CHAR (F1 - F2)
         IDPDS(19) = CHAR (ITEMP)
         IDPDS(20) = CHAR (F1)
         IDPDS(21) = CHAR (2)
         IDPDS(22) = CHAR (0)
         IDPDS(23) = CHAR (0)
C
       ELSE IF (T .EQ. 6) THEN
         JSIGN = IAND(ISHFT(IDWK(1),-32_8),MSK4)
         JSIGO = IAND(ISHFT(IDWK(2),-32_8),MSK4)
         F1   = IAND(ISHFT(IDWK(1),-32_8),MSK3)
         F2   = IAND(ISHFT(IDWK(2),-32_8),MSK3)
         IF (JSIGN .NE. 0) F1 = -F1
         IF (JSIGO .NE. 0) F2 = -F2
         IDPDS(18) = CHAR (1)
C****CALCULATE NEW DATE BASED ON THE BEGINNING OF THE DATA IN MEAN
C        INCR = (F1)
C        IF (INCR.LT.0) THEN
C           RINC=0
C           RINC(2)=INCR
C           PRINT *, 'INCR=',INCR
C           CALL W3FS04 (IDWK(4),JDATE,INCR,IERR)
C           IYR=ICHAR(LIDWK(25))
C           PRINT *, 'IYR = ', IYR
C           IF(IYR.LT.20)THEN
C             MDATE(1)=2000+IYR
C           ELSE
C             MDATE(1)=1900+IYR
C           ENDIF
C           MDATE(2) = ICHAR(LIDWK(26))
C           MDATE(3) = ICHAR(LIDWK(27))
C           MDATE(4) = ICHAR(LIDWK(28))
C           PRINT *, 'CHANGE DATE BY - ',  RINC(2)
C           CALL W3MOVDAT(RINC,MDATE,NDATE)
C           PRINT *,'NEW DATE =',NDATE(1),NDATE(2),NDATE(3),NDATE(5)
C           IYEAR = MOD(NDATE(1),100)
C           LIDWK(25) = CHAR(IYEAR)
C           LIDWK(26) = CHAR(NDATE(2))
C           LIDWK(27) = CHAR(NDATE(3))
C           LIDWK(28) = CHAR(NDATE(4))
C        END IF
         IDPDS(13) = LIDWK(25)
         IDPDS(14) = LIDWK(26)
         IDPDS(15) = LIDWK(27)
         IDPDS(16) = LIDWK(28)
         IF (F1.LT.0) THEN
           IDPDS(19) = CHAR (0)
           IDPDS(21) = CHAR (123)
         ELSE
           NF1 = F1 * 12
           IDPDS(19) = CHAR (NF1)
           IDPDS(21) = CHAR (113)
         END IF
         IDPDS(20) = CHAR (24)
C*****THE NUMBER OF CASES AVERAGED IS ASSUMING ONE TIME A DAY
C        L = (F2/2) + 1
C***THE ABOVE CALCULATION WOULD BE CORR. IF ID8(3) WERE CORR.
         L = (F2+1) / 2
         IDPDS(22) = IPDS1(7)
         IDPDS(23) = IPDS1(8)
C
       ELSE IF (T .EQ. 7) THEN
           PRINT 710, T, ID8
           IER = 3
           RETURN
C
       ELSE IF (T .EQ. 10) THEN
           PRINT 710, T, ID8
           IER = 3
           RETURN
C
 710   FORMAT (' W3FP12 (710) - NOT APPLICABLE (YET) TO GRIB. ',
     &         ', T = ',I2,/,
     &         ' O.N. 84 IDS ARE ',/,
     &         1X,4(Z16,' '))
C
       ENDIF
       IER = 0
       RETURN
       END
