      SUBROUTINE setlevel(ilv,namlevel,nchr)
      INCLUDE 'parm.inc'
      CHARACTER*24 namlevel, nam24
      CHARACTER*1 nam1(24)
      EQUIVALENCE (nam24,nam1(1))
      COMMON /layer/mode(maxlvl), iplevel(maxlvl,2)
      nam24 = namlevel
      IF (nam1(1).eq.'P') THEN
        IF (nchr.le.5) THEN
          READ (nam24(2:5),'(I4)') iplevel(ilv,1)
          mode(ilv) = 1
          PRINT *, ' ILV & MODE & IPLEVEL: ', ilv, mode(ilv), 
     +                iplevel(ilv,1)
        ELSE
          DO n = 2, nchr
            IF (nam1(n).eq.'-') nd = n
          END DO
          READ (nam24(2:nd-1),'(I4)') iplevel(ilv,1)
          READ (nam24(nd+1:nchr),'(I4)') iplevel(ilv,2)
          mode(ilv) = 2
          PRINT *, ' ILV & MODE & IPLEVEL1: ', ilv, mode(ilv), 
     +                iplevel(ilv,1)
          PRINT *, ' ILV & MODE & IPLEVEL2: ', ilv, mode(ilv), 
     +                iplevel(ilv,2)
        END IF
      ELSE IF (nam24(:3).eq.'SFC') THEN
        mode(ilv) = 3
      ELSE IF (nam24(:4).eq.'TROP') THEN
        mode(ilv) = 4
      END IF

      !add Sigma level, by Binbin Zhou
      IF (nam1(1).eq.'S') THEN
        IF (nchr.le.6) THEN
          READ (nam24(2:6),'(I5)') iplevel(ilv,1)
          mode(ilv) = 1
          PRINT *, ' ILV & MODE & IPLEVEL: ', ilv, mode(ilv),
     +                iplevel(ilv,1)
        ELSE
          DO n = 2, nchr
            IF (nam1(n).eq.'-') nd = n
          END DO
          READ (nam24(2:nd-1),'(I5)') iplevel(ilv,1)
          READ (nam24(nd+1:nchr),'(I5)') iplevel(ilv,2)
          mode(ilv) = 2
          PRINT *, ' ILV & MODE & IPLEVEL1: ', ilv, mode(ilv),
     +                iplevel(ilv,1)
          PRINT *, ' ILV & MODE & IPLEVEL2: ', ilv, mode(ilv),
     +                iplevel(ilv,2)
        END IF
      END IF

      RETURN
      END
