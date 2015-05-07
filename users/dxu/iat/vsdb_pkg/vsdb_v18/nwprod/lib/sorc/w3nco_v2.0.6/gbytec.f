      SUBROUTINE GBYTEC(IN,IOUT,ISKIP,NBYTE)
      character*1 in(*)
      integer iout(*)
      CALL GBYTESC(IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END
