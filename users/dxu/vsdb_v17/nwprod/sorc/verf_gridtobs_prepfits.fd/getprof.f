C-----------------------------------------------------------------------
C  SUBROUTINE GETPROF 
C  INTERPOLATE BACKGROUND PROFILE TO REPORT LOCATIONS
C* 
C* Log
C* K. Brill/EMC		11/98	Added wind rotation
C* K. Brill/EMC		12/98	Check mask at all corners
C* P. Shafran/EMC        5/99   Added CAPE, CIN, and LI
C-----------------------------------------------------------------------
      SUBROUTINE getprof(iearth)

      INCLUDE 'parm.inc'
      CHARACTER*8 target
      REAL*8 xyz
      real*8 xdft(3,255,mxr)
      integer xlev(mxr)

      use debug
      use guser
      use guesfc
      use guess
      use weights
      use gridef
      use grid3d
      use surfce
      real*8 psi,zsi,tsi,usi,vsi,qsi,pmi,cpi,cni,pxi,tmxi,dpti,visbi,
     *    tmni,tocci,gusti,pcpi,pbli,pwi,tropi,cdti,pblrii,pblmxi
      real*8 pi,zi,ti,ui,vi,qi,ai

c     real*8 wtsw,wtse,wtnw,wtne
c     real*8 alat,elon

C
      LOGICAL latlong, lambert, polarstereo
C
c      real*8 z,t,u,v,q,alnq
C
      DATA bmiss /10E10/
      LOGICAL	chk
      chk ( i, j, k, l ) = ( i .gt. 0 .and. j .gt. 0 .and.
     +				k .gt. 0 .and. l .gt. 0 )
C*
      IF (ibak.eq.1) THEN
C       INITIALIZE PROFILES TO BMISS ONCE
C       --------------------------------
        psi = bmiss
        pmi = bmiss
        zsi = bmiss
        tsi = bmiss
        usi = bmiss
        vsi = bmiss
        qsi = bmiss
        cpi = bmiss
        cni = bmiss
        pxi = bmiss
        tmxi = bmiss
        tmni = bmiss
        dpti = bmiss
        visbi = bmiss
        tocci = bmiss
        gusti = bmiss
        thuni = bmiss
        pbli = bmiss
        pi = bmiss
        zi = bmiss
        ti = bmiss
        ui = bmiss
        vi = bmiss
        qi = bmiss
        ai = bmiss

C       CALCULATE INTERPOLATION WEIGHTS ONCE
C       ------------------------------------

        CALL setterp

        DO 10 n = 1, nrep
          xob = xyz(1,n)
          yob = xyz(2,n)
C         Depending on the grid type,
C         CALCULATE Grid coordinates of obs lat,long 
C         ------------------------------------------
          IF (latlong) THEN
C           Latitiude - Longitude grid  - even global NOW
            xi = (xob-elon1) / dxx + 1.0
            yj = (yob-alat1) / dyy + 1.0
	    rmx1 = 1.0
	    rmx2 = 0.0
          END IF
          IF (polarstereo) THEN
C           Polar Stereographic grid
C           W3FB06 expects grid length in meters
            dxm = dxx * 1000.
            CALL w3fb06(yob,xob,alat1,elon1,dxm,elonv,xi,yj)
	    xlon = xob
	    clon = elonv
	    IF ( xlon .gt. 180. ) xlon = xlon - 360.
	    IF ( clon .gt. 180. ) clon = clon - 360.
	    theta = ( xlon - clon ) * factor
	    rmx1 = COS ( theta )
	    rmx2 = SIN ( theta )
          END IF
          IF (lambert) THEN
C           Lambert Conic Conformal grid
C           W3FB11 expects grid length in meters
            dxm = dxx * 1000.
c           print*,'yob,xob,alat1,elon1,dxm,elonv,alatan1,xi,yj=',
c    *        yob,xob,alat1,elon1,dxm,elonv,alatan1,xi,yj
            CALL w3fb13(yob,xob,alat1,elon1,dxm,elonv,alatan1,
     *        alatan2,xi,yj)
c           CALL w3fb11(yob,xob,alat1,elon1,dxm,elonv,alatan,xi,yj)
c           concon = SIN ( alatan * factor )
            concon = SIN ( alatan1 * factor )
	    xlon = xob
	    clon = elonv
	    IF ( xlon .gt. 180. ) xlon = xlon - 360.
	    IF ( clon .gt. 180. ) clon = clon - 360.
	    theta = ( xlon - clon ) * factor * concon
	    rmx1 = COS ( theta )
	    rmx2 = SIN ( theta )
          END IF
          kxi(n) = INT (xi)
          kyj(n) = INT (yj)
c         print*,'n,kxi(n),kyj(n)=',n,kxi(n),kyj(n)
          if(iearth.eq.0) then
           rm1(n) = rmx1
           rm2(n) = rmx2
          else
           rm1(n) = 1
           rm2(n) = 0
          endif
C*
          IF (n .eq. indux) THEN
            PRINT *, n, ' LATLON = ', yob, xob, ' GRDREL = ', xi, yj
            PRINT *, n, kxi(n), kyj(n), imax, jmax
	    PRINT *, n, ' Rm1, Rm2 = ', rm1 (n), ', ', rm2 (n)
          END IF
C
C*         CHECK IF OB IS WITHIN DOMAIN
C         ----------------------------
          IF ( kxi(n) .lt. 1 .or. kxi(n) .ge. imax .or.
     +         kyj(n) .lt. 1 .or. kyj(n) .ge. jmax ) THEN
            kxi(n) = 0
            kyj(n) = 0
            xi = 0.
            yj = 0.
          END IF

C         COMPUTE WEIGHTS FOR 4 POINTS
C         ----------------------------
          dx = xi - kxi(n)
          dy = yj - kyj(n)
          wtsw(n) = (1.-dx) * (1.-dy)
          wtse(n) = dx * (1.-dy)
          wtnw(n) = (1.-dx) * dy
          wtne(n) = dx * dy
   10   CONTINUE
      END IF

C     BI-LINEAR HORIZONTAL INTERPOLATION FROM GES TO ALL DATA POINTS
C     --------------------------------------------------------------
      nwith = 0
      DO n = 1, nrep
        IF (kxi(n).gt.0) THEN
	  i = kxi (n)
	  i1 = i + 1
	  j = kyj (n)
	  j1 = j + 1
          nwith = nwith + 1
	  IF ( chk ( mskps(i,j), mskps(i1,j), mskps(i,j1),
     +		     mskps(i1,j1) ) ) psi(n,ibak) = wtsw(n) * 
     +                ps(kxi(n),kyj(n)) + wtse(n) * ps(kxi(n)+1,kyj(n))
     +                + wtnw(n) * ps(kxi(n),kyj(n)+1) + wtne(n) * 
     +                ps(kxi(n)+1,kyj(n)+1)
	  IF ( chk ( mskpm(i,j), mskpm(i1,j), mskpm(i,j1),
     +		     mskpm(i1,j1) ) ) pmi(n,ibak) = wtsw(n) * 
     +                pm(kxi(n),kyj(n)) + wtse(n) * pm(kxi(n)+1,kyj(n))
     +                + wtnw(n) * pm(kxi(n),kyj(n)+1) + wtne(n) * 
     +                pm(kxi(n)+1,kyj(n)+1)
	  IF ( chk ( mskzs(i,j), mskzs(i1,j), mskzs(i,j1),
     +		     mskzs(i1,j1) ) ) zsi(n,ibak) = wtsw(n) * 
     +                zs(kxi(n),kyj(n)) + wtse(n) * zs(kxi(n)+1,kyj(n))
     +                + wtnw(n) * zs(kxi(n),kyj(n)+1) + wtne(n) * 
     +                zs(kxi(n)+1,kyj(n)+1)
	  IF ( chk ( mskts(i,j), mskts(i1,j), mskts(i,j1),
     +		     mskts(i1,j1) ) ) tsi(n,ibak) = wtsw(n) * 
     +                ts(kxi(n),kyj(n)) + wtse(n) * ts(kxi(n)+1,kyj(n))
     +                + wtnw(n) * ts(kxi(n),kyj(n)+1) + wtne(n) * 
     +                ts(kxi(n)+1,kyj(n)+1)
c           print*,'n,ibak,tsi(n,ibak)=',n,ibak,tsi(n,ibak)
	  IF ( chk ( mskus(i,j), mskus(i1,j), mskus(i,j1),
     +		     mskus(i1,j1) )  .and.
     +         chk ( mskvs(i,j), mskvs(i1,j), mskvs(i,j1),
     +		     mskvs(i1,j1) ) ) THEN
    		      uf = wtsw(n) * 
     +                us(kxi(n),kyj(n)) + wtse(n) * us(kxi(n)+1,kyj(n))
     +                + wtnw(n) * us(kxi(n),kyj(n)+1) + wtne(n) * 
     +                us(kxi(n)+1,kyj(n)+1)
		      vf = wtsw(n) * 
     +                vs(kxi(n),kyj(n)) + wtse(n) * vs(kxi(n)+1,kyj(n))
     +                + wtnw(n) * vs(kxi(n),kyj(n)+1) + wtne(n) * 
     +                vs(kxi(n)+1,kyj(n)+1)
    		      usi(n,ibak) = rm1(n) * uf + rm2(n) * vf
          	      vsi(n,ibak) = -rm2(n) * uf + rm1(n) * vf
	  END IF
c           print*,'mskqs(i,j)=',mskqs(i,j)
c           print*,'mskqs(i1,j)=',mskqs(i1,j)
c           print*,'mskqs(i,j1)=',mskqs(i,j1)
c           print*,'mskqs(i1,j1)=',mskqs(i1,j1)
	  IF ( chk ( mskqs(i,j), mskqs(i1,j), mskqs(i,j1),
     +		     mskqs(i1,j1) ) ) then
c            print*,'i,j=',i,j
c            print*,'n,ibak=',n,ibak
c            print*,'kxi,kyj,qs=',kxi(n),kyj(n),qs(kxi(n),kyj(n))
c            print*,'wtsw,wtnw,wtne,wtnw=',
c    *    wtsw(n),wtnw(n),wtne(n),wtnw(n)
                    qsi(n,ibak) = wtsw(n) * 
     +                qs(kxi(n),kyj(n)) + wtse(n) * qs(kxi(n)+1,kyj(n))
     +                + wtnw(n) * qs(kxi(n),kyj(n)+1) + wtne(n) * 
     +                qs(kxi(n)+1,kyj(n)+1)
c           print*,'n,ibak,qsi(n,ibak)=',n,ibak,qsi(n,ibak)
           endif
c          if (n.eq.20) then
c           print*,'mskcp(i,j)=',mskcp(i,j)
c           print*,'mskcp(i1,j)=',mskcp(i1,j)
c           print*,'mskcp(i,j1)=',mskcp(i,j1)
c           print*,'mskcp(i1,j1)=',mskcp(i1,j1)
c          endif
           IF ( chk ( mskcp(i,j), mskcp(i1,j), mskcp(i,j1),
     +                mskcp(i1,j1) ) ) then
c            print*,'i,j=',i,j
c            print*,'n,ibak=',n,ibak
c            print*,'cape=',cape(kxi(n),kyj(n))
                cpi(n,ibak) = wtsw(n) *
     +                cape(kxi(n),kyj(n)) + wtse(n) * 
     +                cape(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                cape(kxi(n),kyj(n)+1) + wtne(n) *
     +                cape(kxi(n)+1,kyj(n)+1)
c           print*,'cpi(n,ibak)=',cpi(n,ibak)
           endif
           IF ( chk ( mskcn(i,j), mskcn(i1,j), mskcn(i,j1),
     +                mskcn(i1,j1) ) ) cni(n,ibak) = wtsw(n) *
     +                cin(kxi(n),kyj(n)) + wtse(n) * 
     +                cin(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                cin(kxi(n),kyj(n)+1) + wtne(n) *
     +                cin(kxi(n)+1,kyj(n)+1)
c            print*,'i,j=',i,j
c            print*,'n,ibak=',n,ibak
c            print*,'pli=',pli(kxi(n),kyj(n))
           IF ( chk ( mskli(i,j), mskli(i1,j), mskli(i,j1),
     +                mskli(i1,j1) ) ) pxi(n,ibak) = wtsw(n) *
     +                pli(kxi(n),kyj(n)) + wtse(n) * 
     +                pli(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pli(kxi(n),kyj(n)+1) + wtne(n) *
     +                pli(kxi(n)+1,kyj(n)+1)
c          print*,'pxi(n,ibak)=',pxi(n,ibak)
          IF ( chk ( msktmax(i,j), msktmax(i1,j) , msktmax(i,j1),
     *               msktmax(i1,j1) ) ) tmxi(n,ibak) = wtsw(n) *
     *               tmax(kxi(n),kyj(n)) + wtse(n) *
     *               tmax(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               tmax(kxi(n),kyj(n)+1) + wtne(n) *
     *               tmax(kxi(n)+1,kyj(n)+1)
c        print*,'after tmx computation'
          IF ( chk ( msktmin(i,j), msktmin(i1,j) , msktmin(i,j1),
     *               msktmin(i1,j1) ) ) tmni(n,ibak) = wtsw(n) *
     *               tmin(kxi(n),kyj(n)) + wtse(n) *
     *               tmin(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               tmin(kxi(n),kyj(n)+1) + wtne(n) *
     *               tmin(kxi(n)+1,kyj(n)+1)
          IF ( chk ( mskdpt(i,j), mskdpt(i1,j) , mskdpt(i,j1),
     *               mskdpt(i1,j1) ) ) dpti(n,ibak) = wtsw(n) *
     *               dpt(kxi(n),kyj(n)) + wtse(n) *
     *               dpt(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               dpt(kxi(n),kyj(n)+1) + wtne(n) *
     *               dpt(kxi(n)+1,kyj(n)+1)
          IF ( chk ( mskvis(i,j), mskvis(i1,j), mskvis(i,j1),
     *               mskvis(i1,j1) ) ) visbi(n,ibak) = wtsw(n) *
     *               vis(kxi(n),kyj(n)) + wtse(n) *
     *               vis(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               vis(kxi(n),kyj(n)+1) + wtne(n) *
     *               vis(kxi(n)+1,kyj(n)+1)
          IF ( chk ( msktocc(i,j), msktocc(i1,j), msktocc(i,j1),
     *               msktocc(i1,j1) ) ) tocci(n,ibak) = wtsw(n) *
     *               tocc(kxi(n),kyj(n)) + wtse(n) *
     *               tocc(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               tocc(kxi(n),kyj(n)+1) + wtne(n) *
     *               tocc(kxi(n)+1,kyj(n)+1)
          IF ( chk ( mskgust(i,j), mskgust(i1,j), mskgust(i,j1),
     *               mskgust(i1,j1) ) ) gusti(n,ibak) = wtsw(n) *
     *               gust(kxi(n),kyj(n)) + wtse(n) *
     *               gust(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               gust(kxi(n),kyj(n)+1) + wtne(n) *
     *               gust(kxi(n)+1,kyj(n)+1)
          IF ( chk ( mskthun(i,j), mskthun(i1,j), mskthun(i,j1),
     *               mskthun(i1,j1) ) ) thuni(n,ibak) = wtsw(n) *
     *               thun(kxi(n),kyj(n)) + wtse(n) *
     *               thun(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               thun(kxi(n),kyj(n)+1) + wtne(n) *
     *               thun(kxi(n)+1,kyj(n)+1)
c      print*,'kxi(n),kyj(n)=',kxi(n),kyj(n),pbls(kxi(n),kyj(n))
           IF ( chk ( mskpbl(i,j), mskpbl(i1,j), mskpbl(i,j1),
     +                mskpbl(i1,j1) ) ) pbli(n,ibak) = wtsw(n) *
     +                pbls(kxi(n),kyj(n)) + wtse(n) *
     +                pbls(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pbls(kxi(n),kyj(n)+1) + wtne(n) *
     +                pbls(kxi(n)+1,kyj(n)+1)
c         if(gusti(n,ibak).gt.200.0) then
c           print*,'n,ibak,pbli(n,ibak)=',n,ibak,pbli(n,ibak)
c         endif

          DO l = 1, kmax
c          print*,'l,n,ibak=',l,n,ibak
c          print*,'i,j,l=',i,j,l
c          print*,'kxi(n),kyj(n)=',kxi(n),kyj(n)
c          print*,'pgd(kxi(n),kyj(n),l)=',pgd(kxi(n),kyj(n),l)
	    IF ( chk ( maskz(i,j,l), maskz(i1,j,l), maskz(i,j1,l),
     +                 maskz(i1,j1,l) ) ) pi(l,n,ibak) = wtsw(n) * 
     +                  pgd(kxi(n),kyj(n),l) + wtse(n) * 
     +                  pgd(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  pgd(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  pgd(kxi(n)+1,kyj(n)+1,l)
           if(l.eq.1) print*,'pi(l,n,ibak)=',pi(l,n,ibak)
	    IF ( chk ( maskz(i,j,l), maskz(i1,j,l), maskz(i,j1,l),
     +                 maskz(i1,j1,l) ) ) zi(l,n,ibak) = wtsw(n) * 
     +                  z(kxi(n),kyj(n),l) + wtse(n) * 
     +                  z(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  z(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  z(kxi(n)+1,kyj(n)+1,l)
	    IF ( chk ( maskt(i,j,l), maskt(i1,j,l), maskt(i,j1,l),
     +                 maskt(i1,j1,l) ) ) ti(l,n,ibak) = wtsw(n) * 
     +                  t(kxi(n),kyj(n),l) + wtse(n) * 
     +                  t(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  t(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  t(kxi(n)+1,kyj(n)+1,l)
	    IF ( chk ( masku(i,j,l), masku(i1,j,l), masku(i,j1,l),
     +                 masku(i1,j1,l) ) .and.
     +		 chk ( maskv(i,j,l), maskv(i1,j,l), maskv(i,j1,l),
     +                 maskv(i1,j1,l) ) ) THEN
		        uf = wtsw(n) * 
     +                  u(kxi(n),kyj(n),l) + wtse(n) * 
     +                  u(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  u(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  u(kxi(n)+1,kyj(n)+1,l)
			vf = wtsw(n) * 
     +                  v(kxi(n),kyj(n),l) + wtse(n) * 
     +                  v(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  v(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  v(kxi(n)+1,kyj(n)+1,l)
		        ui(l,n,ibak) = rm1(n) * uf + rm2(n) * vf
			vi(l,n,ibak) = -rm2(n) * uf + rm1(n) * vf
	    END IF
	    IF ( chk ( maskq(i,j,l), maskq(i1,j,l), maskq(i,j1,l),
     +                 maskq(i1,j1,l) ) ) THEN
			qi(l,n,ibak) = wtsw(n) * 
     +                  q(kxi(n),kyj(n),l) + wtse(n) * 
     +                  q(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  q(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  q(kxi(n)+1,kyj(n)+1,l)
			ai(l,n,ibak) = wtsw(n) * 
     +                  alnq(kxi(n),kyj(n),l) + wtse(n) * 
     +                  alnq(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  alnq(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  alnq(kxi(n)+1,kyj(n)+1,l)
	    END IF
          END DO
        END IF
      END DO

      PRINT *, ' COMPUTED ', nwith, ' POINTS WITHIN DOMAIN'

      RETURN
      END
