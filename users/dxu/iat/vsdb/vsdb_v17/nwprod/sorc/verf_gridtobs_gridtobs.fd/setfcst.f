      SUBROUTINE setfcst(ifh,namfcst,nchr)
      INCLUDE 'parm.inc'
      CHARACTER*24 namfcst, nam24
      COMMON /fcsthr/ fhour(mxfcst)
      nam24 = namfcst
      READ (nam24(1:4),'(F4.0)') fhour(ifh)
      PRINT *, ' IFH & FORECAST HOUR: ', ifh, fhour(ifh)
      IF (nchr.gt.3) PRINT *, 
     +            ' FORECAST HOUR CURRENTLY LIMITED TO 3 CHAR'
      RETURN
      END
