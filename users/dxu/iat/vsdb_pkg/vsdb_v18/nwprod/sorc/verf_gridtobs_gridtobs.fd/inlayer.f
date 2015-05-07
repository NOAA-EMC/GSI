      FUNCTION inlayer(subset,pobs,kat,ilv)
      INCLUDE 'parm.inc'
      COMMON /layer/ mode(maxlvl), iplevel(maxlvl,2)
      CHARACTER*8 subset
      real*8 pobs
      ipobs = nint(pobs)
      IF (mode(ilv).eq.1) THEN
        IF (ipobs.eq.iplevel(ilv,1)) THEN
          inlayer = 0
          RETURN
        ELSE
          inlayer = -1
        END IF
      ELSE IF (mode(ilv).eq.2) THEN
        IF (ipobs.le.iplevel(ilv,1).and.ipobs.ge.iplevel(ilv,2)) THEN
          inlayer = 0
          RETURN
        ELSE
          inlayer = -1
        END IF
      ELSE IF (mode(ilv).eq.3) THEN
        IF ((subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP'.or
     *               .subset(:6).eq.'MSONET'.or.
     *                subset(:6).eq.'GPSIPW').and.(kat
     +              .eq.6.or.kat.eq.0)) THEN
          inlayer = 0
          RETURN
        ELSE
          inlayer = -1
        END IF
      ELSE IF (mode(ilv).eq.4) THEN
        IF (kat.eq.5) THEN
          inlayer = 0
          RETURN
        ELSE
          inlayer = -1
        END IF
      END IF
      RETURN
      END
