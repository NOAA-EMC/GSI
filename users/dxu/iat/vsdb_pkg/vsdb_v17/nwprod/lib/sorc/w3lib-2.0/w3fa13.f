       SUBROUTINE W3FA13(TRIGS,RCOS)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FA13         COMPUTES TRIG FUNCTIONS
C   AUTHOR: SELA, JOE        ORG: W323       DATE: 80-11-21
C
C ABSTRACT:  COMPUTES TRIG FUNCTIONS USED IN 2.5 BY 2.5 LAT,LON
C   MAPPING ROUTINES. W3FA13 MUST BE CALLED AT LEAST ONCE BEFORE
C   CALLS TO W3FT08,W3FT09,W3FT10,W3FT11.
C
C PROGRAM HISTORY LOG:
C   80-11-21  JOE SELA
C   84-06-01  R.E.JONES   CHANGE TO VS FORTRAN
C
C USAGE:  CALL W3FA13(TRIGS,RCOS)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     N/A
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     TRIGS  ARG LIST  216 TRIG VALUES, USED BY SUBROUTINE W3FA12.
C     RCOS   ARG LIST  37 COLATITUDES USED BY SUBROUTINES W3FT09,W3FT11
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     IABS  SIN COS                                           SYSTEM
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN 77
C   MACHINE:  CRAY Y-MP8/864
C
C$$$
C
       REAL            RCOS(*)
       REAL            TRIGS(*)
C
       SAVE
C
       DATA  PI    /3.14159265358979323846/
C
         N = 144
         MODE = 3
         DRAD = 2.5*PI/180.
C
         DO 100 LAT = 2,37
           ARG = (LAT-1)*DRAD
           RCOS(LAT) = 1./SIN(ARG)
 100     CONTINUE
C
         RCOS(1) = 77777.777
         IMODE = IABS(MODE)
         NN = N
         IF (IMODE.GT.1.AND.IMODE.LT.6) NN = N/2
         ANGLE = 0.0
         DEL = (PI+PI)/FLOAT(NN)
         L = NN+NN
C
         DO 200 I = 1,L,2
           TRIGS(I) = COS(ANGLE)
           TRIGS(I+1) = SIN(ANGLE)
           ANGLE = ANGLE+DEL
 200     CONTINUE
C
         IF (IMODE.EQ.1) RETURN
         IF (IMODE.EQ.8) RETURN
         ANGLE = 0.0
         DEL = 0.5*DEL
         NH = (NN+1)/2
         L = NH+NH
         LA = NN+NN
C
         DO 300 I = 1,L,2
           TRIGS(LA+I) = COS(ANGLE)
           TRIGS(LA+I+1) = SIN(ANGLE)
           ANGLE = ANGLE+DEL
 300     CONTINUE
C
         IF (IMODE.LE.3) RETURN
         DEL = 0.5*DEL
         ANGLE = DEL
         LA = LA+NN
C
         DO 400 I = 2,NN
           TRIGS(LA+I) = 2.0*SIN(ANGLE)
           ANGLE = ANGLE+DEL
 400     CONTINUE
C
         RETURN
       END
