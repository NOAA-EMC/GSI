C-----------------------------------------------------------------------
C  SUBROUTINE SETTERP
C-----------------------------------------------------------------------
      SUBROUTINE setterp

C
      use gridef
      use grid3d
      use vrtfac
      INCLUDE 'parm.inc'
C

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      maxprs = 1200

C     COMPUTE TABLE FOR THE PRESSURE INTERPOLATION FACTORS
C     ----------------------------------------------------

      DO 30 i = 1, maxprs
        p = i
        DO 10 la = 1, kmax
          IF (p.ge.pgd(1,1,la)) GO TO 20
   10   CONTINUE
   20   IF (la.gt.kmax) THEN
          la = kmax
          lb = kmax
        ELSE IF (la.eq.1) THEN
          la = 1
          lb = 1
        ELSE
          lb = la - 1
        END IF
        IF (la.eq.lb) THEN
          wp = 0.
        ELSE
          pa = pgd(1,1,la)
          pb = pgd(1,1,lb)
          wp = log(p/pb) / log(pa/pb)
        END IF
        vrterp(i) = lb + wp
   30 CONTINUE

      RETURN
      END
