       SUBROUTINE W3FA12(PLN,COLRAD,JCAP,EPS)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FA12         COMPUTES LEGENDRE POLYNOMIALS
C   AUTHOR: SELA,JOE         ORG: W323        DATE: 80-10-28
C
C ABSTRACT:  SUBROUTINE COMPUTES LEGENDRE POLYNOMIALS AT A
C   GIVEN LATITUDE.
C
C PROGRAM HISTORY LOG:
C   80-10-20  JOE SELA
C   84-06-01  R.E.JONES   CHANGE TO IBM VS FORTRAN
C   93-04-12  R.E.JONES   CHANGES FOR CRAY, DOUBLE PRECISION TO REAL
C
C USAGE:  CALL W3FA12 (PLN,COLRAD,JCAP,EPS)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     COLRAD ARG LIST  COLATITUDE IN RADIANS OF DESIRED POINT.
C     JCAP   ARG LIST  FOR RHOMBOIADAL TRUNCATION OF ZONAL WAVE
C     EPS    ARG LIST  COEFF. USED IN RECURSION EQUATION.
C                      DIMENSION OF EPS IS (JCAP+2)*(JCAP+1)
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     PLN    ARG LIST  REAL LOCATIONS CONTAIN LEGENDRE
C                      POLNOMIALS , SIZE IS (JCAP+2)*(JCAP+1)
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     COS   SIN   SNGL                                        SYSTEM
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864
C
C***
C
       REAL A
       REAL B
       REAL COLRAD
       REAL COS2
       REAL EPS(*)
       REAL FL
       REAL PROD
       REAL P1
       REAL P2
       REAL P3
       REAL SINLAT
       REAL PLN(*)
C
       SAVE
C
         SINLAT = COS(COLRAD)
         COS2   = 1.0 - SINLAT * SINLAT
         PROD   = 1.0
         A      = 1.0
         B      = 0.0
         JCAP1  = JCAP+1
         JCAP2  = JCAP+2
C
         DO 300 LL = 1,JCAP1
           L   = LL - 1
           FL  = L
           JLE = L * JCAP2
           IF (L.EQ.0) GO TO 100
             A = A + 2.0
             B = B + 2.0
             PROD = PROD * COS2 * A / B
 100       CONTINUE
           P1 = SQRT (0.5 * PROD)
           PLN(JLE+1) = P1
           P2 = SQRT(2.0 * FL + 3.0) * SINLAT * P1
           PLN(JLE+2) = P2
C
           DO 200 N = 3,JCAP2
             LINDEX = JLE + N
             P3 = (SINLAT*P2 - EPS(LINDEX-1)*P1)/EPS(LINDEX)
             PLN(LINDEX) = P3
             P1 = P2
             P2 = P3
200        CONTINUE
300      CONTINUE
         RETURN
C
       END
