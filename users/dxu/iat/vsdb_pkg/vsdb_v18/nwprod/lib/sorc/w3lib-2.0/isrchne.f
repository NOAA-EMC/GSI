      FUNCTION ISRCHNE(N,X,INCX,TARGET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ISRCHNE     Searches vector given a target
C   PRGMMR: gilbert          ORG: W/NP11    DATE: 99-02-11
C
C ABSTRACT: Searches a vector for the first element 
C           not equal to a target
C
C PROGRAM HISTORY LOG:
C   99-02-11  Gilbert
C
C USAGE:    index=ISRCHNE(n, x, incx, target)
C   INPUT ARGUMENT LIST:
C     n        - Number of elements to be searched
C     x        - Real or integer array of dimension (n-1) * |incx| + 1.
C                Array x contains the vector to be searched.
C     incx     - Increment between elements of the searched array.
C     target   - Value for which to search in the array.
C
C   OUTPUT VALUE
C     index  - Index of the first element equal or not equal to target.  If
C              target is not found, n+1 is returned.  If n <= 0, 0 is
C              returned.
C
C REMARKS: This code and documentation was taken directly from the 
C          man page for routine ISRCHNE on a CRAY UNICOS system.
C
C ATTRIBUTES:
C   LANGUAGE: Fortran
C
C$$$
      INTEGER X(*), TARGET
      J=1
      ISRCHNE=0
      IF(N.LE.0) RETURN
      IF(INCX.LT.0) J=1-(N-1)*INCX
      DO 100 I=1,N
         IF(X(J).NE.TARGET) GO TO 200
         J=J+INCX
  100 CONTINUE
  200 ISRCHNE=I
      RETURN
      END

