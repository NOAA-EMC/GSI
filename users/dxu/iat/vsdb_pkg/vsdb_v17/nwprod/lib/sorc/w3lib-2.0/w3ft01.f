       SUBROUTINE W3FT01(STI,STJ,FLD,HI,II,JJ,NCYCLK,LIN)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FT01         INTERPOLATE VALUES IN A DATA FIELD
C   AUTHOR: MCDONELL, J.     ORG: W345       DATE: 84-06-27
C   UPDATE: JONES,R.E.       ORG: W342       DATE: 87-03-19
C
C ABSTRACT: FOR A GIVEN GRID COORDINATE IN A DATA ARRAY, ESTIMATES
C   A DATA VALUE FOR THAT POINT USING EITHER A LINEAR OR QUADRATIC
C   INTERPOLATION METHOD.
C
C PROGRAM HISTORY LOG:
C   84-06-27  J.MCDONELL
C   89-11-01  R.E.JONES   CHANGE TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FT01 (STI, STJ, FLD, HI, II, JJ, NCYCLK, LIN)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     STI    ARG LIST  REAL*4 I GRID COORDINATE OF THE POINT FOR WHICH
C                      AN INTERPOLATED VALUE IS DESIRED
C     STJ    ARG LIST  REAL*4 J GRID COORDINATE OF THE POINT FOR WHICH
C                      AN INTERPOLATED VALUE IS DESIRED
C     FLD    ARG LIST  REAL*4 SIZE(II,JJ) DATA FIELD
C     II     ARG LIST  INTEGER*4 NUMBER OF COLUMNS IN 'FLD'
C     JJ     ARG LIST  INTEGER*4 NUMBER OF ROWS IN 'FLD'
C     NCYCLK ARG LIST  INTEGER*4 CODE TO SPECIFY IF GRID IS CYCLIC OR
C                      NOT:
C                       = 0 NON-CYCLIC IN II, NON-CYCLIC IN JJ
C                       = 1 CYCLIC IN II, NON-CYCLIC IN JJ
C                       = 2 CYCLIC IN JJ, NON-CYCLIC IN II
C                       = 3 CYCLIC IN II, CYCLIC IN JJ
C     LIN    ARG LIST  INTEGER*4 CODE SPECIFYING INTERPOLATION METHOD:
C                       = 1 LINEAR INTERPOLATION
C                      .NE.1  QUADRATIC INTERPOLATION
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     HI     ARG LIST  REAL*4 DATA FIELD VALUE AT (STI,STJ) OBTAINED
C                      BY INTERPOLATION.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
      REAL    ERAS(4)
      REAL    FLD(II,JJ)
      REAL    JY(4)
C
      I     = STI
      J     = STJ
      FI    = I
      FJ    = J
      XDELI = STI - FI
      XDELJ = STJ - FJ
      IP2   = I + 2
      IM1   = I - 1
      IP1   = I + 1
      JY(4) = J + 2
      JY(1) = J - 1
      JY(3) = J + 1
      JY(2) = J
      XI2TM = 0.0
      XJ2TM = 0.0
      IF (LIN.NE.1) THEN
        XI2TM = XDELI * (XDELI - 1.0) * 0.25
        XJ2TM = XDELJ * (XDELJ - 1.0) * 0.25
      ENDIF
      IF ((I.LT.2).OR.(J.LT.2))       GO TO 10
      IF ((I.GT.II-3).OR.(J.GT.JJ-3)) GO TO 10
C
C     QUADRATIC (LINEAR TOO) OK W/O FURTHER ADO SO GO TO 170
C
      GO TO 170
C
   10 CONTINUE
        ICYCLK = 0
        JCYCLK = 0
        IF (NCYCLK) 20,120,20
C
   20 CONTINUE
        IF (NCYCLK / 2 .NE. 0) JCYCLK = 1
        IF (NCYCLK .NE. 2)     ICYCLK = 1
        IF (ICYCLK) 30,70,30
C
   30 CONTINUE
        IF (I.EQ.1)      GO TO 40
        IF (I.EQ.(II-1)) GO TO 50
        IP2 = I + 2
        IM1 = I - 1
        GO TO 60
C
   40 CONTINUE
        IP2 = 3
        IM1 = II - 1
        GO TO 60
C
   50 CONTINUE
        IP2 = 2
        IM1 = II - 2
C
   60 CONTINUE
        IP1 = I + 1
C
   70 CONTINUE
        IF (JCYCLK) 80,120,80
C
   80 CONTINUE
        IF (J.EQ.1)      GO TO 90
        IF (J.EQ.(JJ-1)) GO TO 100
        JY(4) = J + 2
        JY(1) = J - 1
        GO TO 110
C
   90 CONTINUE
        JY(4) = 3
        JY(1) = JJ - 1
        GO TO 110
C
  100 CONTINUE
        JY(4) = 2
        JY(1) = JJ - 2
C
  110 CONTINUE
        JY(3) = J + 1
        JY(2) = J
C
  120 CONTINUE
        IF (LIN.EQ.1) GO TO 160
        IF (ICYCLK) 140,130,140
C
  130 CONTINUE
        IF ((I.LT.2).OR.(I.GE.(II-1)))  XI2TM = 0.0
C
  140 CONTINUE
        IF (JCYCLK) 160,150,160
C
  150 CONTINUE
        IF ((J.LT.2).OR.(J.GE.(JJ-1)))  XJ2TM = 0.0
C
  160 CONTINUE
C
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
C
        IF (I.LT.1)   I   = 1
        IF (IP1.LT.1) IP1 = 1
        IF (IP2.LT.1) IP2 = 1
        IF (IM1.LT.1) IM1 = 1
C
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
C
        IF (I.GT.II)   I   = II
        IF (IP1.GT.II) IP1 = II
        IF (IP2.GT.II) IP2 = II
        IF (IM1.GT.II) IM1 = II
C
  170 CONTINUE
      DO 180 K = 1,4
        J1 = JY(K)
C
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
C
        IF (J1.LT.1)  J1 = 1
        IF (J1.GT.JJ) J1 = JJ
        ERAS(K) = (FLD(IP1,J1) - FLD(I,J1)) * XDELI + FLD(I,J1) +
     &  (FLD(IM1,J1) - FLD(I,J1) - FLD(IP1,J1) + FLD(IP2,J1)) * XI2TM
  180 CONTINUE
C
      HI = ERAS(2) + (ERAS(3) - ERAS(2)) * XDELJ + (ERAS(1) -
     &     ERAS(2) -  ERAS(3) + ERAS(4)) * XJ2TM
C
      RETURN
      END
