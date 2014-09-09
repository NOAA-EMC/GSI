      SUBROUTINE setmodel(mod,namodel,nchr)
      INCLUDE 'parm.inc'
C       STORE MODEL (AND PRP) NAMES IN REAL ARRAY
      CHARACTER*24 namodel, nam24
      character*(80) msg
      character*(4) agrd
      CHARACTER*8 nam8(3), mod8, prp8
      EQUIVALENCE (nam24,nam8(1))
      REAL fmod8, fprp
      EQUIVALENCE (mod8,fmod8)
      LOGICAL latlong,lambert,polarstereo
      COMMON /model/ fprp, fmodel(maxmod)
      LOGICAL vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
      DATA prp8 /'PRP     '/
      IF (mod.eq.1) THEN
        mod8 = prp8
        fprp = fmod8
        PRINT 1000, mod, fprp
 1000   FORMAT (' STORED PRP NAME: ',I3,2X,A8)
      END IF
      idash = index(namodel,'/')
      IF (idash.gt.0) THEN
        nam24 = namodel(:idash-1)
      ELSE
        nam24 = namodel
      END IF
      mod8 = nam8(1)
      fmodel(mod) = fmod8
      PRINT 1100, mod, fmodel(mod)
 1100 FORMAT (' MOD & MODEL NAME: ',I3,2X,A8)
C
C*	Check for model grid.
C
	IF ( nmbgrd (mod) .gt. 0 ) THEN
	    igrd = nmbgrd (mod)
            CALL gtgdef(igrd,istat,imx,jmx,alatx,
     +              elonx,dx,dy,cenlon(mod),alatan,
     +              latlong,lambert,polarstereo)
	    IF ( latlong ) THEN
		nmbgrd (mod) = -1
	    ELSE IF ( polarstereo ) THEN
		concon (mod) = 1.0
	    ELSE IF ( lambert ) THEN
		concon (mod) = SIN ( alatan * 3.1415926 / 180.0 )
	    ELSE
                msg=' Projection type is not supported.'
                call errmsg(msg)
                write(agrd,'(i4)') igrd
                msg=' Grid number = '// agrd
                call errmsg(msg)
                call errexit(1)
	    END IF
	END IF
C*
      RETURN
      END
