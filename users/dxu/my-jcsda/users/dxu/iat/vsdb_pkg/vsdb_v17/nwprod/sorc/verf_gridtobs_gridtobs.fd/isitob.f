      FUNCTION isitob(subset,typ,iob)
      INCLUDE 'parm.inc'
      COMMON /obtyp/ mode(maxobs), reqsub(maxobs), iobtyp(maxobs)
      CHARACTER*8 subset, reqsub
      real*8 typ
      isitob = -1
      IF (reqsub(iob)(:6).eq.'ANYSFC') THEN
        IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP'.or.
     +      subset(:6).eq.'MSONET'.or.subset(:6).eq.'ADPUPA'.or.
     *      subset(:6).eq.'PROFLR') 
     +              isitob = 0
      ELSE IF (reqsub(iob)(:6).eq.'ANYAIR') THEN
        IF (subset(:6).eq.'AIRCAR'.or.subset(:6).eq.'AIRCFT') isitob = 0
      ELSE IF (reqsub(iob)(:6).eq.'ONLYSF') THEN
        IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP') 
c    *        .or.subset(:6).eq.'MSONET')
     +		isitob = 0
      ELSE IF (subset(:6).eq.reqsub(iob)(:6)) THEN
        isitob = 0
      END IF
      IF (mode(iob).eq.1.or.isitob.lt.0) RETURN
      IF (mode(iob).eq.2.and.nint(typ).ne.iobtyp(iob)) isitob = -1
      RETURN
      END
