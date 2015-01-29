       SUBROUTINE W3FT06(ALOLA,APOLA,W1,W2,LINEAR)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FT06         CONVERT (145,37) TO (65,65) S. HEMI. GRID
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 84-06-18
C
C ABSTRACT:  CONVERT A SOUTHERN HEMISPHERE 2.5 DEGREE LAT.,LON. 145 BY
C   37 GRID TO A POLAR STEREOGRAPHIC 65 BY 65 GRID. THE POLAR
C   STEREOGRAPHIC MAP PROJECTION IS TRUE AT 60 DEG. S. , THE MESH
C   LENGTH IS 381 KM. AND THE ORIENTION IS 260 DEG. W.(100E)  .
C
C PROGRAM HISTORY LOG:
C   84-06-18  R.E.JONES
C   91-07-30  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C   92-05-02  R.E.JONES   ADD SAVE
C
C USAGE:  CALL W3FT06(ALOLA,APOLA,W1,W2,LINEAR)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ALOLA  ARG LIST  145*37 DEG 2.5 LAT,LON GRID S. HEMI.
C                      5365 POINT GRID IS TYPE 30 OR 1E HEX O.N. 84
C     LINEAR ARG LIST  1 LINEAR INTERPOLATION , NE.1 BIQUADRATIC
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     APOLA  ARG LIST  65*65 GRID OF SOUTHERN HEMI.
C                      4225 POINT GRID IS TYPE 28 OR 1C HEX O.N. 84
C     W1     ARG LIST  65*65 SCRATCH FIELD
C     W2     ARG LIST  65*65 SCRATCH FIELD
C            FT06F001  ERROR MESSAGE
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     ASIN   ATAN2                                            SYSTEM
C
C   REMARKS:
C
C   1. W1 AND W2 ARE USED TO STORE SETS OF CONSTANTS WHICH ARE
C   REUSABLE FOR REPEATED CALLS TO THE SUBROUTINE. IF THEY ARE
C   OVER WRITTEN BY THE USER, A WARNING MESSAGE WILL BE PRINTED
C   AND W1 AND W2 WILL BE RECOMPUTED.
C
C   2. WIND COMPONENTS ARE NOT ROTATED TO THE 65*65 GRID ORIENTATION
C   AFTER INTERPOLATION. YOU MAY USE W3FC10 TO DO THIS.
C
C   3. THE GRID POINTS VALUES ON THE EQUATOR HAVE BEEN EXTRAPOLATED
C   OUTWARD TO ALL THE GRID POINTS OUTSIDE THE EQUATOR ON THE 65*65
C   GRID (ABOUT 1100 POINTS).
C
C   4. YOU SHOULD USE THE CRAY VECTORIZED VERION W3FT06V ON THE CRAY
C   IT HAS 3 PARAMETERS IN THE CALL, RUNS ABOUT TIMES FASTER, USES
C   MORE MEMORY.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864
C
C$$$
C
       REAL            ALOLA(145,37)
       REAL            APOLA(4225)
       REAL            ERAS(4)
       REAL            SAVEW1(10)
       REAL            SAVEW2(10)
       REAL            W1(4225)
       REAL            W2(4225)
C
       INTEGER         JY(4)
       INTEGER         OUT
C
       LOGICAL         LIN
C
       SAVE
C
       DATA  DEGPRD/57.2957795/
       DATA  EARTHR/6371.2/
       DATA  ISWT  /0/
       DATA  OUT   /6/
C
 4000  FORMAT ( 52H *** WARNING , W1 OR W2 SCRATCH FILES OVER WRITTEN ,,
     &          43H I WILL RESTORE THEM , BURNING UP CPU TIME,,
     &          14H IN W3FT06 ***)
C
         LIN = .FALSE.
         IF (LINEAR.EQ.1) LIN = .TRUE.
         IF  (ISWT.EQ.0)  GO TO  300
C
C        TEST TO SEE IF W1 OR W2 WAS WRITTEN OVER
C
         DO 100  KK=1,10
           IF (SAVEW1(KK).NE.W1(KK))  GO TO 200
           IF (SAVEW2(KK).NE.W2(KK))  GO TO 200
 100     CONTINUE
         GO TO 800
C
 200     CONTINUE
         WRITE (OUT,4000)
C
 300     CONTINUE
         DEG   = 2.5
         NN    = 0
         XMESH = 381.0
         GI2   = (1.86603*EARTHR) / XMESH
         GI2   = GI2 * GI2
C
C        DO LOOP 600 PUTS SUBROUTINE W3FB03 IN LINE
C
         DO  600 J=1,65
           XJ = J - 33
           XJ2 = XJ * XJ
           DO  600 I=1,65
             XI = I - 33
             R2 = XI*XI + XJ2
             IF  (R2.NE.0.0)  GO TO  400
             WLON = 0.0
             XLAT = -90.0
             GO TO  500
 400         CONTINUE
             XLONG = DEGPRD * ATAN2(XJ,XI)
             WLON = XLONG -10.0
             IF (WLON.LT.0.0)  WLON = WLON + 360.0
             XLAT = ASIN((GI2-R2)/(GI2+R2))*DEGPRD
             XLAT = -XLAT
 500         CONTINUE
             XLAT = XLAT + 90.0
             IF  (WLON.GT.360.0)  WLON = WLON - 360.0
             IF  (WLON.LT.0.0)    WLON = WLON + 360.0
             NN = NN + 1
             W1(NN) = ( 360.0 - WLON ) / DEG + 1.0
             W2(NN) = XLAT / DEG + 1.0
  600      CONTINUE
C
         DO 700  KK=1,10
           SAVEW1(KK)=W1(KK)
           SAVEW2(KK)=W2(KK)
 700     CONTINUE
C
           ISWT = 1
C
 800     CONTINUE
C
         DO  1900  KK=1,4225
           I = W1(KK)
           J = W2(KK)
           FI = I
           FJ = J
           XDELI = W1(KK) - FI
           XDELJ = W2(KK) - FJ
           IP1 = I + 1
           JY(3) = J + 1
           JY(2) = J
           IF (LIN)  GO TO  900
           IP2 = I + 2
           IM1 = I - 1
           JY(4) = J + 2
           JY(1) = J - 1
           XI2TM = XDELI*(XDELI-1.)*.25
           XJ2TM = XDELJ*(XDELJ-1.)*.25
 900     CONTINUE
         IF ((I.LT.2).OR.(J.LT.2)) GO TO 1000
         IF ((I.GT.142).OR.(J.GT.34)) GO TO  1000
C     QUADRATIC (LINEAR TOO) OK W/O FURTHER ADO SO GO TO 1500
         GO TO  1500
C
 1000    CONTINUE
         IF (I.EQ.1) GO TO 1100
         IF (I.EQ.144) GO TO 1200
         IP2 = I+2
         IM1 = I-1
         GO TO  1300
C
 1100    CONTINUE
         IP2 = 3
         IM1 = 144
         GO TO  1300
C
 1200    CONTINUE
         IP2 = 2
         IM1 = 143
C
 1300    CONTINUE
         IP1 = I + 1
         IF (LIN)  GO TO  1400
         IF ((J.LT.2).OR.(J.GE.36))  XJ2TM=0.
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
         IF (IP2.LT.1) IP2 = 1
         IF (IM1.LT.1) IM1 = 1
         IF (IP2.GT.145) IP2 = 145
         IF (IM1.GT.145) IM1 = 145
C
 1400    CONTINUE
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
         IF (I.LT.1) I = 1
         IF (IP1.LT.1) IP1 = 1
         IF (I.GT.145) I = 145
         IF (IP1.GT.145) IP1 = 145
C
 1500    CONTINUE
C
         IF (.NOT.LIN)  GO TO  1700
C
C        LINEAR INTERPOLATION
C
          DO 1600 K = 2,3
           J1 = JY(K)
           IF  (J1.LT.1)  J1=1
           IF  (J1.GT.37) J1=37
           ERAS(K) = (ALOLA(IP1,J1) - ALOLA(I,J1)) * XDELI + ALOLA(I,J1)
 1600    CONTINUE
C
         APOLA(KK) = ERAS(2) + (ERAS(3) - ERAS(2)) * XDELJ
         GO TO  1900
C
 1700    CONTINUE
C
C        QUADRATIC INTERPOLATION
C
         DO 1800  K = 1,4
           J1 = JY(K)
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
           IF (J1.LT.1) J1=1
           IF (J1.GT.37) J1=37
           ERAS(K)=(ALOLA(IP1,J1)-ALOLA(I,J1))*XDELI+ALOLA(I,J1)+
     &             (ALOLA(IM1,J1)-ALOLA(I,J1)-ALOLA(IP1,J1)+
     &              ALOLA(IP2,J1))*XI2TM
 1800    CONTINUE
C
         APOLA(KK) = ERAS(2)+(ERAS(3)-ERAS(2))*XDELJ+(ERAS(1)-
     &               ERAS(2)-ERAS(3)+ERAS(4))*XJ2TM
C
 1900    CONTINUE
C
C        SET POLE POINT, WMO STANDARD FOR U OR V
C
         APOLA(2113) = ALOLA(73,1)
C
         RETURN
       END
