      SUBROUTINE setobtyp(iob,namvfyobs,nchr)
      INCLUDE 'parm.inc'
      CHARACTER*24 namvfyobs, nam24
      CHARACTER*1 nam1(24)
      EQUIVALENCE (nam24,nam1(1))
      EQUIVALENCE (nam1(1),nam8)
      CHARACTER*8 nam8, reqsub
      COMMON /obtyp/ mode(maxobs), reqsub(maxobs), iobtyp(maxobs)
      COMMON /obmrk/ iqmod(maxobs)
      nam24 = namvfyobs
      reqsub(iob) = nam8
      mode(iob) = 1
      iqmod(iob) = 1
      IF (nam1(7).eq.'/') THEN
        IF (nam1(8).eq.'K') THEN
          mode(iob) = 2
          READ (nam24(9:11),'(I3)') iobtyp(iob)
        ELSE IF (nam1(8).eq.'B') THEN
          iqmod(iob) = 2
        END IF
      END IF
      IF (mode(iob).eq.1.and.nchr.gt.6) PRINT *, ' BAD OBTYP'
      RETURN
      END
