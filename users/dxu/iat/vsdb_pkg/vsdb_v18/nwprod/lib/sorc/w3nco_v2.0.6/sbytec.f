      SUBROUTINE SBYTEC(OUT,IN,ISKIP,NBYTE)
      character*1 out(*)
      integer in(*)
      CALL SBYTESC(OUT,IN,ISKIP,NBYTE,0,1)
      RETURN
      END
