       SUBROUTINE W3FI52(IDENT,CNST,IER)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FI52         COMPUTES SCALING CONSTANTS USED BY GRDPRT
C   AUTHOR: STACKPOLE,J.     ORG: W342       DATE: 85-12-03
C   AUTHOR: JONES,R.E.
C
C ABSTRACT: COMPUTES THE FOUR SCALING CONSTANTS USED BY GRDPRT, W3FP03,
C   OR W3FP05 FROM THE 1ST 5 IDENTIFIER WORDS IN OFFICE NOTE 84 FORMAT.
C
C PROGRAM HISTORY LOG:
C   80-06-15  J. STACKPOLE
C   85-12-03  R.E.JONES  MADE SUBROUTINE IN GENOUT INTO THIS SUBR.
C   89-07-07  R.E.JONES  CONVERT TO MICROSOFT FORTRAN 4.10
C   90-02-03  R.E.JONES  CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FI52(IDENT,CNST,IER)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDENT  ARG LIST  FIRST 5 ID'S IN OFFICE NOTE 84 FORMAT
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     CNST   ARG LIST  4 CONSTANT'S USED BY GRDPRT,W3FP05, OR W3FP03
C     IER    ARG LIST  0 = NORMAL RETURN
C                      1 = ID'S IN IDENT ARE NOT IN O.N. 84 FORMAT
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     W3FI33                                                  W3LIB
C
C ATTRIBUTES:
C   LANGUAGE: MICROSOFT FORTRAN 4.10 OPTIMIZING COMPILER
C   MACHINE:  IBM PC, AT, PS/2, 386, CLONES.
C
C$$$
C
CC       SET DEFAULT VALUES FOR NMC FIELDS  GRIDPRINTING
C
      REAL      CNST(4)
C
      INTEGER   IDENT(4)
      INTEGER   LABUNP(27)
      INTEGER   Q
C
C     UPACK 8 OFFICE NOTE 84 ID'S INTO 27 PARTS
C
      CALL W3FI33(IDENT,LABUNP)
C
      ITYPEQ = LABUNP(1)
      Q      = ITYPEQ
      ITYPES = LABUNP(2)
      ITYPEC = LABUNP(5)
      ISC    = LABUNP(6)
      IER    = 0
      XLVL   = ITYPEC
      IF (ISC) 10,30,20
C
   10 CONTINUE
        ISC = -ISC
C
C     DIVIDE BY WHOLE NUMBER RATHER THAN MULTIPLY BY FRACTION TO
C     TO AVOID ROUND OF ERROR
C
        XLVL = XLVL / (10.**ISC)
        GO TO 30
C
   20 CONTINUE
        XLVL = XLVL * (10.**ISC)
C
   30 CONTINUE
        ILVL = XLVL
        IF (Q.NE.1.AND.Q.NE.2)  GO TO 40
C
C***  GEOPOTENTIAL METERS ............
C
      CNST(3) = 60.
      IF  (ILVL .LT. 500)  CNST(3) = 120.
      IF  ((ITYPES .EQ. 129) .OR. (ITYPES .EQ. 130)) CNST(3) = 500.
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(4) = 0.
      IF  (CNST(3) .EQ. 500.)  CNST(4) = 2.
         RETURN
C
   40 CONTINUE
        IF (Q.NE.8) GO TO 50
C
C***  PRESSURE, MILLIBARS ...............
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 4.
      CNST(4) = 0.
C
C***   IF SFC OR TROPOPAUSE PRESSURE ..
C
      IF  ((ITYPES .EQ. 129) .OR. (ITYPES .EQ. 130))  CNST(3) = 25.
         RETURN
C
   50 CONTINUE
      DO 60 I = 16,21
         IF (Q.EQ.I)  GO TO 70
   60 CONTINUE
        GO TO 80
C
   70 CONTINUE
C
C*** TEMPERATURES (DEG K) CONVERT TO DEG C, EXCEPT FOR POTENTIAL TEMP.
C
      CNST(1) = -273.15
      CNST(2) = 1.
      CNST(3) = 5.
      CNST(4) = 0.
      IF  (ITYPEQ .EQ. 19)  CNST(1) = 0.
         RETURN
C
   80 CONTINUE
        IF (Q.NE.40)  GO TO 90
C
C***  VERTICAL VELOCITY (MB/SEC) TO MICROBARS/SEC
C***  SIGN CHANGED SUCH THAT POSITIVE VALUES INDICATE UPWARD MOTION.
C
      CNST(1) =  0.
      CNST(2) = -1.E3
      CNST(3) = 2.
      CNST(4) = 0.
         RETURN
C
   90 CONTINUE
        IF (Q.NE.41)  GO TO 100
C
C***  NET VERTICAL DISPLACEMENT  ...  MILLIBARS
C
      CNST(1) =  0.
      CNST(2) =  1.
      CNST(3) = 10.
      CNST(4) =  0.
         RETURN
C
  100 CONTINUE
      DO 110 I = 48,51
        IF (Q.EQ.I)  GO TO 120
  110 CONTINUE
         GO TO 130
C
  120 CONTINUE
C
C***   WIND SPEEDS   M/SEC
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 10.
      CNST(4) = 0.
         RETURN
C
  130 CONTINUE
        IF (Q.NE.52)  GO TO 140
C
C***  VERTICAL SPEED SHEAR(/ SEC)...   TO BE CONVERTED TO KNOTS/1000 FT
C
      CNST(1) = 0.
      CNST(2) = 592.086
      CNST(3) = 2.
      CNST(4) = 0.
         RETURN
C
  140 CONTINUE
        IF (Q.NE.53.AND.Q.NE.54)  GO TO 150
C
C***  DIVERGENT U AND V COMPONENTS   M/SEC
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 2.
      CNST(4) = 0.
         RETURN
C
  150 CONTINUE
        IF (Q.NE.72.AND.Q.NE.73)  GO TO 160
C
C***  VORTICITY (APPROX 10**-5)  TIMES 10**6 /SEC
C
      CNST(1) = 0.
      CNST(2) = 1.E6
      CNST(3) = 40.
      CNST(4) = 0.
         RETURN
C
  160 CONTINUE
        IF (Q.NE.74)  GO TO 170
C
C***  DIVERGENCE    (/SEC)       TIMES 10**6
C
      CNST(1) = 0.
      CNST(2) = 1.E6
      CNST(3) = 20.
      CNST(4) = 0.
         RETURN
C
  170 CONTINUE
        IF (Q.NE.80.AND.Q.NE.81)  GO TO 180
C
C***  STREAM FUNCTION OR VELOCITY POTENTIAL (M*M/SEC) CONVERTED TO M.
C***  CONVERT TO METERS.    (M*M/SEC  * FOG)
C
      CNST(1) = 0.
      CNST(2) = 1.03125E-4 / 9.8
      CNST(3) = 60.
      CNST(4) = 0.
      IF ((ILVL.LT.500) .AND. (ITYPEC .EQ. 0)) CNST(3) = 120.
         RETURN
C
  180 CONTINUE
        IF (Q.NE.88)  GO TO 190
C
C***  RELATIVE HUMIDITY  ...  PERCENT
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 10.
      CNST(4) = 0.
         RETURN
C
  190 CONTINUE
        IF (Q.NE.89)  GO TO 200
C
C***  PRECIPITABLE WATER (KG/M*M) OR .1 GRAM/CM*CM OR MILLIMETERS/CM*CM
C***   CHANGE TO CENTI-INCHES/CM*CM
C
      CNST(1) = 0.
      CNST(2) = 3.937
      CNST(3) = 5.
      CNST(4) = 0.
         RETURN
C
  200 CONTINUE
        IF (Q.NE.90)  GO TO 210
C
C***  ACCUMULATED PRECIPITATION (METERS)   TO CENTI-INCHES, AT 1/2 IN.
C
      CNST(1) = 0.
      CNST(2) = 3937.
      CNST(3) = 50.
      CNST(4) = 0.
         RETURN
C
  210 CONTINUE
        IF (Q.NE.91.AND.Q.NE.92)  GO TO 220
C
C***  PROBABILITY  ...  PERCENT
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 10.
      CNST(4) = 0.
         RETURN
C
  220 CONTINUE
        IF (Q.NE.93)  GO TO  230
C
C***  SNOW DEPTH (METERS)    TO INCHES, AT INTERVALS OF 6 INCHES
C
      CNST(1) = 0.
      CNST(2) = 39.37
      CNST(3) = 6.
      CNST(4) = 0.
         RETURN
C
  230 CONTINUE
        IF (Q.NE.112)  GO TO 240
C
C***  LIFTED INDEX  ..(DEG K)   TO DEG C.
C
      CNST(1) = -273.15
      CNST(2) = 1.
      CNST(3) = 2.
      CNST(4) = 0.
         RETURN
C
  240 CONTINUE
        IF (Q.NE.120.AND.Q.NE.121)  GO TO 250
C
C***  WAVE COMPONENT OF GEOPOTENTIAL   (GEOP M)
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 10.
      CNST(4) = 0.
         RETURN
C
  250 CONTINUE
        IF (Q.NE.160)  GO TO 260
C
C***  DRAG COEFFICIENT  DIMENSIONLESS       TIMES 10**5
C
      CNST(1) = 0.
      CNST(2) = 1.E5
      CNST(3) = 100.
      CNST(4) = 0.
         RETURN
C
  260 CONTINUE
        IF (Q.NE.161)  GO TO 270
C
C***  LAND/SEA   DIMENSIONLESS
C
      CNST(1) = 0.
      CNST(2) = 1.
      CNST(3) = 1.
      CNST(4) = .5
       RETURN
C
  270 CONTINUE
        IF (Q.NE.169)  GO TO 280
C
C           ALBIDO * 100.  (DIMENSIONLESS)
C
            CNST(1) = 0.
            CNST(2) = 100.
            CNST(3) = 5.
            CNST(4) = 0.
            RETURN
C
  280 CONTINUE
      IF  (ITYPEQ .EQ. 384)  GO TO 290
      IF  ((ITYPEQ .GE. 385) .AND. (ITYPEQ .LE. 387)) GO TO 300
C
C*** NONE OF THE ABOVE ....
C
        IER = 1
        RETURN
C
C***  OCEAN WATER TEMPERATURE  (DEGREES K)
C
  290 CONTINUE
        CNST(1) = 0.
        CNST(2) = 1.
        CNST(3) = 5.
        CNST(4) = 0.
        RETURN
C
C***  HEIGHT OF WIND DRIVEN OCEAN WAVES, SEA SWELLS, OR COMBINATION
C
  300 CONTINUE
        CNST(1) = 0.
        CNST(2) = 1.
        CNST(3) = 2.
        CNST(4) = 0.
      RETURN
      END
