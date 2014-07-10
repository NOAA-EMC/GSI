C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE raddate(idate,dhour,bdate)

      DIMENSION mon(12)
      REAL*8 bdate

      DATA mon /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      icent = mod(idate/100000000,100)
      iy = mod(idate/1000000,100)
      im = mod(idate/10000,100)
      id = mod(idate/100,100)
      ihr = mod(idate,100) + dhour

      IF (mod(iy,4).eq.0) mon(2) = 29

   10 IF (ihr.lt.0) THEN
        ihr = ihr + 24
        id = id - 1
        IF (id.eq.0) THEN
          im = im - 1
          IF (im.eq.0) THEN
            im = 12
            iy = iy - 1
            IF (iy.lt.0) iy = 99
          END IF
          id = mon(im)
        END IF
        GO TO 10
      ELSE IF (ihr.ge.24) THEN
        ihr = ihr - 24
        id = id + 1
        IF (id.gt.mon(im)) THEN
          id = 1
          im = im + 1
          IF (im.gt.12) THEN
            im = 1
            iy = mod(iy+1,100)
          END IF
        END IF
        GO TO 10
      END IF

      bdate = icent*100000000 + iy*1000000 + im*10000 + 
     1      id*100 + ihr

      RETURN
      END
