      FUNCTION inarea(mod, sid, xobi, yobi, iar, rm1, rm2, namarea )
C**********************************************************************
C     rm1 = rotation matrix element 1,1
C     rm2 = rotation matrix element 1,2
C*
      INCLUDE 'parm.inc'

      CHARACTER*(*) sid
      CHARACTER*3 regions(30)
      COMMON /grdef/ mode(mxarea), imax(mxarea), imin(mxarea), 
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea), 
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea), 
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea), 
     +            lambert(mxarea), polarstereo(mxarea), numreg(mxarea),
     +            ig104(147,110), regions
      LOGICAL latlong, lambert, polarstereo
      CHARACTER*8 stnlist
      character*24 namarea
      real*8 xobi,yobi
      real xob,yob
      COMMON /stndef/ nstns (mxarea), stnlist (mxarea,maxj)
      LOGICAL vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
C-----------------------------------------------------------------------
      inarea = -1

c     print*,'namarea=',namarea
C
       if(xobi.gt.180) then
        xob=xobi-360.0
        else
        xob=xobi
       endif
       yob=yobi
c      print*,'xob,yob=',xob,yob
        if(namarea.eq.'G3') then
          inarea=0
          goto 123
          RETURN
        endif
        if(namarea.eq.'GEUR') then
          if(xob.le.-10.0.or.xob.ge.28.0.or.yob.le.25.0.or.yob.ge.
     *        70.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GASI') then
          if(xob.le.60.0.or.xob.ge.145.0.or.yob.le.25.0.or.yob.ge.
     *        65.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GAFR') then
          if(xob.le.-20.0.or.xob.ge.55.0.or.yob.le.-30.0.or.yob.ge.
     *        30.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GSA') then
          if(xob.le.-83.0.or.xob.ge.-35.0.or.yob.le.-50.0.or.yob.ge.
     *        15.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GNH') then
          if(xob.gt.20.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GSH') then
          if(xob.lt.-20.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GTRP') then
          if(xob.lt.-20.0.or.xob.gt.20.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GNA') then
          if(xob.le.-145.0.or.xob.ge.-50.0.or.yob.le.25.or.yob.ge.
     *        60.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

        if(namarea.eq.'GAUS') then
          if(xob.le.-180.0.or.xob.ge.-90.0.or.yob.le.-10.0.or.yob.ge.
     *        -55.0) then
          inarea=-1
          RETURN
          else
          inarea=0
          goto 123
          endif
        endif

      IF ( xobi .lt. 0.0 ) THEN
	xob = xobi + 360.
      ELSE
	xob = xobi
      END IF
      yob = yobi
c     print*,'xob,yob=',xob,yob
c     print*,'elon1(iar)=',elon1(iar)
c     print*,'alat1(iar)=',alat1(iar)
c     print*,'mode(iar)=',mode(iar)
c     print*,'alatan(iar)=',alatan(iar)
c     print*,'elonv(iar)=',elonv(iar)
c     print*,'dxx=',dxx(iar)
      IF (mode(iar).eq.1.or.mode(iar).eq.2) THEN
C       CHECK WHETHER THE OBSERVATION IS LOCATED WITHIN GRID AREA
C       ---------------------------------------------------------
C       Depending on the grid type,
C       CALCULATE Grid coordinates of obs lat,long 
C       Change has been made for LATLON calculation --- Yuejian Zhu
C       ------------------------------------------
c       print*,'latlong(iar)=',latlong(iar)
        IF (latlong(iar)) THEN
C         Latitiude - Longitude grid  - NOT global
          xi = (xob-elon1(iar)) / dxx(iar) + 1.0
          yj = (yob-alat1(iar)) / dyy(iar) + 1.0
        END IF
c       print*,'polarstereo(iar)=',polarstereo(iar)
        IF (polarstereo(iar)) THEN
C         Polar Stereographic grid
C         W3FB06 wants grid spacing in meters
          dxm = dxx(iar) * 1000.
          CALL w3fb06(yob,xob,alat1(iar),elon1(iar),dxm,elonv(iar),xi,yj
     +                )
        END IF
c       print*,'lambert(iar)=',lambert(iar)
        IF (lambert(iar)) THEN
C         Lambert Conic Conformal grid
C         W3FB11 wants grid spacing in meters
          dxm = dxx(iar) * 1000.
          CALL w3fb11(yob,xob,alat1(iar),elon1(iar),dxm,elonv(iar),
     +                alatan(iar),xi,yj)
c         call w3fb13(yob,xob,alat1(iar),elon1(iar),dxm,elonv(iar),
c    +                alatan(iar),alatan(iar),xi,yj)

        END IF
        kxi = nint(xi)
        kyj = nint(yj)
c       PRINT *, 'IMIN,IMAX,JMIN,JMAX=', imin(iar), imax(iar), 
c    +              jmin(iar), jmax(iar), kxi, kyj
C       CHECK IF OB IS WITHIN DOMAIN
C       ----------------------------
        IF (kxi.lt.imin(iar).or.kxi.gt.imax(iar).or.kyj.lt.jmin(iar).or.
     +              kyj.gt.jmax(iar)) THEN
          inarea = -1
          RETURN
        ELSE
          IF (mode(iar).eq.1.or.(mode(iar).eq.2.and.ig104(kxi,kyj).eq.
     +                numreg(iar))) inarea = 0
        END IF
      ELSE IF ( mode(iar) .eq. 3 ) THEN
	inarea = -1
	ixs = 0
	DO WHILE ( inarea .eq. -1 .and. ixs .lt. nstns (iar) )
	  ixs = ixs + 1
	  IF ( stnlist (iar,ixs) .eq. sid ) inarea = 0
	END DO
      ELSE
        inarea = -1
        PRINT *, ' INVALID MODE IN INAREA', mode(iar)
      END IF
123   continue
C
C*	Compute wind rotation matrix elements.
C
	rm1 = 1.0
	rm2 = 0.0
	IF ( nmbgrd (mod) .gt. 0 .and. inarea .eq. 0 ) THEN
	  IF ( cenlon (mod) .gt. 180.0 ) THEN
		rlamc = cenlon (mod) - 360.0
	  ELSE
		rlamc = cenlon (mod)
	  END IF
	  IF ( xob .gt. 180. ) THEN
		rlam = xob - 360.
	  ELSE
		rlam = xob
	  END IF
	  theta = ( rlam - rlamc ) * factor * concon (mod)
	  rm1 = COS ( theta )
	  rm2 = SIN ( theta )
	END IF
C*
c     print*,'end of inarea'
      RETURN
      END
