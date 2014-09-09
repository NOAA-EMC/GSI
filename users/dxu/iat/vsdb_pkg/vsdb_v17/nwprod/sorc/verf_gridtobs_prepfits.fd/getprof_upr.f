C-----------------------------------------------------------------------
C  SUBROUTINE GETPROF 
C  INTERPOLATE BACKGROUND PROFILE TO REPORT LOCATIONS
C* 
C* Log
C* K. Brill/EMC		11/98	Added wind rotation
C* K. Brill/EMC		12/98	Check mask at all corners
C* P. Shafran/EMC        5/99   Added CAPE, CIN, and LI
C-----------------------------------------------------------------------
      SUBROUTINE getprofupr(iearth,sub,pob)


      use surfce
      use grid3d
      use gridef
      use guesfc
      use guess
      use weights
      use debug
      use vrtfac
      use guser
      INCLUDE 'parm.inc'
      character*8 sub(mxr)
      real xobd(mxr), yobd(mxr), pobd(mxr)
      real xid(mxr), yjd(mxr)
      real*8 pob(255,mxr)
      real xi,yj
      real dx,dy,dxm
      real*8 D001,D1000
      real xob,yob

c     real*8 wtsw,wtse,wtnw,wtne
c     real*8 alat,elon

C
c      real*8 z,t,u,v,q,alnq
C
      DATA bmiss /10E10/
      data D001 /1.0/
      data D1000 /1000.0/
      LOGICAL	chk
      chk(i,j,k,l)=(i.gt.0.and.j.gt.0.and.k.gt.0.and.l.gt.0)
C*
c       allocate(kxid(mxr,255))
c       allocate(kyjd(mxr,255))
c       allocate(wtswd(mxr,255))
c       allocate(wtsed(mxr,255))
c       allocate(wtnwd(mxr,255))
c       allocate(wtned(mxr,255))

c       allocate(kxi(mxr))
c       allocate(kyj(mxr))
c       allocate(wtsw(mxr))
c       allocate(wtse(mxr))
c       allocate(wtnw(mxr))
c       allocate(wtne(mxr))

c       allocate(rm1(mxr))
c       allocate(rm2(mxr))
      print*,'ibak=',ibak

      IF (ibak.eq.1) THEN
C       INITIALIZE PROFILES TO BMISS ONCE
C       --------------------------------
        print*,'ibak=1'
        allocate(pi(mxl,mxr,mxb))
        allocate(zi(mxl,mxr,mxb))
        allocate(ti(mxl,mxr,mxb))
        allocate(ui(mxl,mxr,mxb))
        allocate(vi(mxl,mxr,mxb))
        allocate(qi(mxl,mxr,mxb))
        allocate(ai(mxl,mxr,mxb))

        allocate(psi(mxr,mxb))
        allocate(zsi(mxr,mxb))
        allocate(tsi(mxr,mxb))
        allocate(usi(mxr,mxb))
        allocate(vsi(mxr,mxb))
        allocate(qsi(mxr,mxb))
        allocate(bcpi(mxr,mxb))
        allocate(pxi(mxr,mxb))
        allocate(tmxi(mxr,mxb))
        allocate(dpti(mxr,mxb))
        allocate(visbi(mxr,mxb))
        allocate(tmni(mxr,mxb))
        allocate(tocci(mxr,mxb))
        allocate(gusti(mxr,mxb))
        allocate(pbli(mxr,mxb))
        allocate(pwi(mxr,mxb))
        allocate(tropi(mxr,mxb))
        allocate(cdti(mxr,mxb))
        allocate(pblrii(mxr,mxb))
        allocate(pblmxi(mxr,mxb))
        allocate(pmi(mxr,mxb))
        allocate(cpi(mxr,mxb))
        allocate(cni(mxr,mxb))

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
        bcpi = bmiss
        tmxi = bmiss
        tmni = bmiss
        dpti = bmiss
        visbi = bmiss
        tocci = bmiss
        gusti = bmiss
        thuni = bmiss
        pbli = bmiss
        pblrii = bmiss
        pblmxi = bmiss
        pwi = bmiss
        tropi = bmiss
        cdti = bmiss
        pi = bmiss
        zi = bmiss
        ti = bmiss
        ui = bmiss
        vi = bmiss
        qi = bmiss
        ai = bmiss

        allocate(kxid(mxr,255))
        allocate(kyjd(mxr,255))
        allocate(wtswd(mxr,255))
        allocate(wtsed(mxr,255))
        allocate(wtnwd(mxr,255))
        allocate(wtned(mxr,255))

        allocate(kxi(mxr))
        allocate(kyj(mxr))
        allocate(wtsw(mxr))
        allocate(wtse(mxr))
        allocate(wtnw(mxr))
        allocate(wtne(mxr))

        allocate(rm1(mxr))
        allocate(rm2(mxr))
        allocate(vrterp(1200))

       endif

        print*,'allocating'

C       CALCULATE INTERPOLATION WEIGHTS ONCE
C       ------------------------------------

        CALL setterp

        DO 10 n = 1, nrep
      if(sub(n)(:6).eq.'ADPUPA') then
          do i=1,xlev(n)
           xobd(i)=xdft(1,i,n)
           yobd(i)=xdft(2,i,n)
           pobd(i)=pob(i,n)
          enddo
         else
          xob = xyz(1,n)
          yob = xyz(2,n)
c         if(n.eq.13741) print*,'xyz(1,n),xyz(2,n)=',
c    *      xyz(1,n),xyz(2,n)
         endif
C         Depending on the grid type,
C         CALCULATE Grid coordinates of obs lat,long 
C         ------------------------------------------
          IF (latlong) THEN
C           Latitiude - Longitude grid  - even global NOW
      if(sub(n)(:6).eq.'ADPUPA') then
            do i=1,xlev(n)
             xid(i)=(xobd(i)-elon1)/dxx+1.0
             yjd(i)=(yobd(i)-alat1)/dyy+1.0
            enddo
            else
             xi = (xob-elon1) / dxx + 1.0
             yj = (yob-alat1) / dyy + 1.0
            endif
	    rmx1 = 1.0
	    rmx2 = 0.0
          END IF
          IF (polarstereo) THEN
C           Polar Stereographic grid
C           W3FB06 expects grid length in meters
            dxm = dxx * 1000.
c           dxm = dxx * D1000
      if(sub(n)(:6).eq.'ADPUPA') then
            do i=1,xlev(n)
             call w3fb06(yobd(i),xobd(i),alat1,elon1,dxm,elonv,
     *          xid(i),yjd(i))
             xlon=xobd(i)
             clon=elonv
             IF ( xlon .gt. 180. ) xlon = xlon - 360.
             IF ( clon .gt. 180. ) clon = clon - 360.
             theta = ( xlon - clon ) * factor
             rmx1 = COS ( theta )
             rmx2 = SIN ( theta ) 
            enddo
            else
             CALL w3fb06(yob,xob,alat1,elon1,dxm,elonv,xi,yj)
	     xlon = xob
	     clon = elonv
	     IF ( xlon .gt. 180. ) xlon = xlon - 360.
	     IF ( clon .gt. 180. ) clon = clon - 360.
	     theta = ( xlon - clon ) * factor
	     rmx1 = COS ( theta )
             rmx2 = SIN ( theta )
            endif
          END IF
          IF (lambert) THEN
C           Lambert Conic Conformal grid
C           W3FB11 expects grid length in meters
c           dxm = dxx * 1000.
            dxm = dxx * D1000 
      if(sub(n)(:6).eq.'ADPUPA') then
            do i=1,xlev(n)
            CALL w3fb11(yobd(i),xobd(i),alat1,elon1,dxm,elonv,alatan1,
     *        xid(i),yjd(i))
            concon = SIN ( alatan1 * factor )
	    xlon = xobd(i)
	    clon = elonv
	    IF ( xlon .gt. 180. ) xlon = xlon - 360.
	    IF ( clon .gt. 180. ) clon = clon - 360.
	    theta = ( xlon - clon ) * factor * concon
	    rmx1 = COS ( theta )
	    rmx2 = SIN ( theta )
            enddo
            else
             CALL w3fb11(yob,xob,alat1,elon1,dxm,elonv,alatan1,
     *        xi,yj)
c           if(n.eq.13741) print*,'yob,xob,xi,yj=',yob,xob,xi,yj
            concon = SIN ( alatan1 * factor )
            xlon = xob
            clon = elonv
            IF ( xlon .gt. 180. ) xlon = xlon - 360.
            IF ( clon .gt. 180. ) clon = clon - 360.
            theta = ( xlon - clon ) * factor * concon
            rmx1 = COS ( theta )
            rmx2 = SIN ( theta )
            endif
          END IF
      if(sub(n)(:6).eq.'ADPUPA') then
          do i=1,xlev(n)
          kxid(n,i) = INT (xid(i))
          kyjd(n,i) = INT (yjd(i))
          enddo
          else
            kxi(n) = INT (xi)
            kyj(n) = INT (yj)
          endif
          if(iearth.eq.0) then
           rm1(n) = rmx1
           rm2(n) = rmx2
          else
           rm1(n) = 1
           rm2(n) = 0
          endif
C*
c         IF (n .eq. indux) THEN
c           PRINT *, n, ' LATLON = ', yob, xob, ' GRDREL = ', xi, yj
c           PRINT *, n, kxi(n), kyj(n), imax, jmax
c    PRINT *, n, ' Rm1, Rm2 = ', rm1 (n), ', ', rm2 (n)
c         END IF
C
C*         CHECK IF OB IS WITHIN DOMAIN
C         ----------------------------
      if(sub(n)(:6).eq.'ADPUPA') then
          do i=1,xlev(n)
         
          IF ( kxid(n,i) .lt. 1 .or. kxid(n,i) .gt. imax .or.
     +         kyjd(n,i) .lt. 1 .or. kyjd(n,i) .gt. jmax ) THEN
            kxid(n,i) = 0
            kyjd(n,i) = 0
            xid(i) = 0.
            yjd(i) = 0.
          END IF

C         COMPUTE WEIGHTS FOR 4 POINTS
C         ----------------------------
          dx = xid(i) - kxid(n,i)
          dy = yjd(i) - kyjd(n,i)
          wtswd(n,i) = (1.-dx) * (1.-dy)
          wtsed(n,i) = dx * (1.-dy)
          wtnwd(n,i) = (1.-dx) * dy
          wtned(n,i) = dx * dy
         enddo    !   i=1,xlev
         else
            IF ( kxi(n) .lt. 1 .or. kxi(n) .gt. imax .or.
     +         kyj(n) .lt. 1 .or. kyj(n) .gt. jmax ) THEN
            kxi(n) = 0
            kyj(n) = 0
            xi = 0.
            yj = 0.
          END IF
           
C         COMPUTE WEIGHTS FOR 4 POINTS
C         ----------------------------
          dx = xi - kxi(n)
          dy = yj - kyj(n)
          wtsw(n) = (D001-dx) * (D001-dy)
          wtse(n) = dx * (D001-dy)
          wtnw(n) = (D001-dx) * dy
          wtne(n) = dx * dy
         endif  ! if sub.eq.ADPUPA
   10   CONTINUE
c     END IF

C     BI-LINEAR HORIZONTAL INTERPOLATION FROM GES TO ALL DATA POINTS
C     --------------------------------------------------------------
      nwith = 0
      DO n = 1, nrep
      if(sub(n)(:6).eq.'ADPUPA') then
         i = kxid (n,1)
         i1 = i + 1
        j = kyjd (n,1)
        j1 = j + 1
c         nwith = nwith + 1
          kxi(n)=kxid(n,1)
          kyj(n)=kyjd(n,1)
          wtnw(n)=wtnwd(n,1)
          wtne(n)=wtned(n,1)
          wtsw(n)=wtswd(n,1)
          wtse(n)=wtsed(n,1)
          else
          i=kxi(n)
          i1=i+1
          j=kyj(n)
          j1=j+1
          endif  !  subset.eq.ADPUPA
        IF (kxi(n).gt.0.and.kyj(n).gt.0.and.
     *        kxi(n).lt.imax.and.kyj(n).lt.jmax) THEN
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
     +		     mskts(i1,j1) ) ) 
     +                tsi(n,ibak) = wtsw(n) * 
     +                ts(kxi(n),kyj(n)) + wtse(n) * ts(kxi(n)+1,kyj(n))
     +                + wtnw(n) * ts(kxi(n),kyj(n)+1) + wtne(n) * 
     +                ts(kxi(n)+1,kyj(n)+1)
c         print*,'n,ibak=',n,ibak
c         print*,'kxi(n),kyj(n)=',kxi(n),kyj(n)
c         print*,'ts(kxi(n),kyj(n))=',ts(kxi(n),kyj(n))
c         print*,'ts(kxi(n)+1,kyj(n))=',ts(kxi(n)+1,kyj(n))
c         print*,'ts(kxi(n),kyj(n)+1)=',ts(kxi(n),kyj(n)+1)
c         print*,'ts(kxi(n)+1,kyj(n)+1)=',ts(kxi(n)+1,kyj(n)+1)
c         print*,'wtsw(n)=',wtsw(n)
c         print*,'wtse(n)=',wtse(n)
c         print*,'wtnw(n)=',wtnw(n)
c         print*,'wtne(n)=',wtne(n)
c         print*,'tsi(n,ibak)=',tsi(n,ibak)
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
	  IF ( chk ( mskqs(i,j), mskqs(i1,j), mskqs(i,j1),
     +		     mskqs(i1,j1) ) ) then
                    qsi(n,ibak) = wtsw(n) * 
     +                qs(kxi(n),kyj(n)) + wtse(n) * qs(kxi(n)+1,kyj(n))
     +                + wtnw(n) * qs(kxi(n),kyj(n)+1) + wtne(n) * 
     +                qs(kxi(n)+1,kyj(n)+1)
           endif
           IF ( chk ( mskcp(i,j), mskcp(i1,j), mskcp(i,j1),
     +                mskcp(i1,j1) ) ) then
                cpi(n,ibak) = wtsw(n) *
     +                cape(kxi(n),kyj(n)) + wtse(n) * 
     +                cape(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                cape(kxi(n),kyj(n)+1) + wtne(n) *
     +                cape(kxi(n)+1,kyj(n)+1)
           endif
           IF ( chk ( mskbc(i,j), mskbc(i1,j), mskbc(i,j1),
     +                mskbc(i1,j1) ) ) then
                bcpi(n,ibak) = wtsw(n) *
     +                bcape(kxi(n),kyj(n)) + wtse(n) *
     +                bcape(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                bcape(kxi(n),kyj(n)+1) + wtne(n) *
     +                bcape(kxi(n)+1,kyj(n)+1)
c               print*,'n,ibak,bcpi(n,ibak)=',n,ibak,bcpi(n,ibak)
           endif
           IF ( chk ( mskcn(i,j), mskcn(i1,j), mskcn(i,j1),
     +                mskcn(i1,j1) ) ) cni(n,ibak) = wtsw(n) *
     +                cin(kxi(n),kyj(n)) + wtse(n) * 
     +                cin(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                cin(kxi(n),kyj(n)+1) + wtne(n) *
     +                cin(kxi(n)+1,kyj(n)+1)
           IF ( chk ( mskli(i,j), mskli(i1,j), mskli(i,j1),
     +                mskli(i1,j1) ) ) pxi(n,ibak) = wtsw(n) *
     +                pli(kxi(n),kyj(n)) + wtse(n) * 
     +                pli(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pli(kxi(n),kyj(n)+1) + wtne(n) *
     +                pli(kxi(n)+1,kyj(n)+1)
          IF ( chk ( msktmax(i,j), msktmax(i1,j) , msktmax(i,j1),
     *               msktmax(i1,j1) ) ) tmxi(n,ibak) = wtsw(n) *
     *               tmax(kxi(n),kyj(n)) + wtse(n) *
     *               tmax(kxi(n)+1,kyj(n)) + wtnw(n) *
     *               tmax(kxi(n),kyj(n)+1) + wtne(n) *
     *               tmax(kxi(n)+1,kyj(n)+1)
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
c         IF ( chk ( mskthun(i,j), mskthun(i1,j), mskthun(i,j1),
c    *               mskthun(i1,j1) ) ) thuni(n,ibak) = wtsw(n) *
c    *               thun(kxi(n),kyj(n)) + wtse(n) *
c    *               thun(kxi(n)+1,kyj(n)) + wtnw(n) *
c    *               thun(kxi(n),kyj(n)+1) + wtne(n) *
c    *               thun(kxi(n)+1,kyj(n)+1)
           IF ( chk ( mskpbl(i,j), mskpbl(i1,j), mskpbl(i,j1),
     +                mskpbl(i1,j1) ) ) pbli(n,ibak) = wtsw(n) *
     +                pbls(kxi(n),kyj(n)) + wtse(n) *
     +                pbls(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pbls(kxi(n),kyj(n)+1) + wtne(n) *
     +                pbls(kxi(n)+1,kyj(n)+1)
           IF ( chk ( mskpri(i,j), mskpri(i1,j), mskpri(i,j1),
     +                mskpri(i1,j1) ) ) pblrii(n,ibak) = wtsw(n) *
     +                pblris(kxi(n),kyj(n)) + wtse(n) *
     +                pblris(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pblris(kxi(n),kyj(n)+1) + wtne(n) *
     +                pblris(kxi(n)+1,kyj(n)+1)
           IF ( chk ( mskpmx(i,j), mskpmx(i1,j), mskpmx(i,j1),
     +                mskpmx(i1,j1) ) ) pblmxi(n,ibak) = wtsw(n) *
     +                pblmxs(kxi(n),kyj(n)) + wtse(n) *
     +                pblmxs(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pblmxs(kxi(n),kyj(n)+1) + wtne(n) *
     +                pblmxs(kxi(n)+1,kyj(n)+1)
           IF ( chk ( mskpw(i,j), mskpw(i1,j), mskpw(i,j1),
     +                mskpw(i1,j1) ) ) pwi(n,ibak) = wtsw(n) *
     +                pwo(kxi(n),kyj(n)) + wtse(n) *
     +                pwo(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                pwo(kxi(n),kyj(n)+1) + wtne(n) *
     +                pwo(kxi(n)+1,kyj(n)+1)
           IF ( chk ( msktrp(i,j), msktrp(i1,j), msktrp(i,j1),
     +                msktrp(i1,j1) ) ) tropi(n,ibak) = wtsw(n) *
     +                trop(kxi(n),kyj(n)) + wtse(n) *
     +                trop(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                trop(kxi(n),kyj(n)+1) + wtne(n) *
     +                trop(kxi(n)+1,kyj(n)+1)
           IF ( chk ( mskcdtz(i,j), mskcdtz(i1,j), mskcdtz(i,j1),
     +                mskcdtz(i1,j1) ) ) cdti(n,ibak) = wtsw(n) *
     +                cdtz(kxi(n),kyj(n)) + wtse(n) *
     +                cdtz(kxi(n)+1,kyj(n)) + wtnw(n) *
     +                cdtz(kxi(n),kyj(n)+1) + wtne(n) *
     +                cdtz(kxi(n)+1,kyj(n)+1)
       if(cdti(n,ibak).lt.0.0) cdti(n,ibak)=bmiss
       ENDIF  !  kxi.ne.0

          nn=1
          kx=kxi(n)
          igood=0

          do l=1,kmax
          if(sub(n)(:6).eq.'ADPUPA') then
          nn=xlev(n)
          do ii=1,nn
c         kx=kxi(n)
        kx=kxid(n,ii)
        ky=kyjd(n,ii)
        kxi(n)=kx
        kyj(n)=ky
        if(kx.gt.0.and.ky.gt.0) then
          if(abs(pgd(kxi(n),kyj(n),l)-pob(ii,n)).lt.5.0) then
           i = kxid (n,ii)
           i1 = i + 1
           j = kyjd (n,ii)
           j1 = j + 1
           nwith = nwith + 1
c          kxi(n)=kxid(n,ii)
c          kyj(n)=kyjd(n,ii)
           wtnw(n)=wtnwd(n,ii)
           wtne(n)=wtned(n,ii)
           wtsw(n)=wtswd(n,ii)
           wtse(n)=wtsed(n,ii)
           igood=1
           goto 333
          elseif (abs(pgd(kxi(n),kyj(n),l)-pob(ii,n)).gt.1.0
     *                  .and.ii.gt.1) then
            p1=log10(pob(ii-1,n))-log10(pob(1,n))
            p2=log10(pob(ii-1,n))-log10(pob(ii,n))
            if(nint(p1).eq.0) then
                 wtnw(n)=wtnwd(n,ii)
                 wtne(n)=wtned(n,ii)
                 wtsw(n)=wtswd(n,ii)
                 wtse(n)=wtsed(n,ii)   
                 i = kxid (n,ii)
                 i1 = i + 1
                 j = kyjd (n,ii)
                 j1 = j + 1
                 goto 333
            endif
            prat=p2/p1
            i=prat*(kxid(n,ii)-kxid(n,ii-1))+kxid(n,ii-1) 
            i1=i+1
            j=prat*(kyjd(n,ii)-kyjd(n,ii-1))+kyjd(n,ii-1)
            j1=j+1
            kxi(n)=i
            kyj(n)=j
            wtnw(n)=prat*(wtnwd(n,ii)-wtnwd(n,ii-1))+wtnwd(n,ii-1)
            wtne(n)=prat*(wtned(n,ii)-wtned(n,ii-1))+wtned(n,ii-1)
            wtsw(n)=prat*(wtswd(n,ii)-wtswd(n,ii-1))+wtswd(n,ii-1)
            wtse(n)=prat*(wtsed(n,ii)-wtsed(n,ii-1))+wtsed(n,ii-1)
            goto 333
          endif    !  .lt.10
        endif      !  if kx.gt.0
       enddo       !  ii=1,nn
       endif       !  if sub.eq.ADPUPA
333    continue

c     if(sub(n)(:6).eq.'ADPUPA') then
c       kx=kxid(n,ii)
c       ky=kyjd(n,ii)
c       print*,'kx,ky=',kx,ky
c     else
        kx=kxi(n)
        ky=kyj(n)
c     endif

c     if(ibak.eq.2) then
c     print*,'kx,ky,imax,jmax=',kx,ky,imax,jmax
c     endif
      if(kx.gt.0.and.ky.gt.0.and.kx.lt.imax.and.ky.lt.jmax) then
	    IF ( chk ( maskz(i,j,l), maskz(i1,j,l), maskz(i,j1,l),
     +                 maskz(i1,j1,l) ) ) pi(l,n,ibak) = wtsw(n) * 
     +                  pgd(kxi(n),kyj(n),l) + wtse(n) * 
     +                  pgd(kxi(n)+1,kyj(n),l) + wtnw(n) * 
     +                  pgd(kxi(n),kyj(n)+1,l) + wtne(n) * 
     +                  pgd(kxi(n)+1,kyj(n)+1,l)
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
c      if(n.le.100.and.ibak.eq.2) then
       if(ibak.eq.2) then
c      print*,'kxi(n),kyj(n)=',kxi(n),kyj(n)
c      print*,'wtsw,wtse,wtnw,wtne=',wtsw(n),wtse(n),wtnw(n),wtne(n)
c      print*,'t(kxi(n),kyj(n),l)=',t(kxi(n),kyj(n),l)
c      print*,'t(kxi(n)+1,kyj(n),l)=',t(kxi(n)+1,kyj(n),l)
c      print*,'t(kxi(n),kyj(n)+1,l)=',t(kxi(n),kyj(n)+1,l)
c      print*,'t(kxi(n)+1,kyj(n)+1,l)=',t(kxi(n)+1,kyj(n)+1,l)
c      print*,'l,n,ibak,ti(l,n,ibak)=',l,n,ibak,ti(l,n,ibak)
c      print*,'i,j,l=',i,j,l
c      print*,'i1,j1=',i1,j1
c      print*,'maskt(i,j,l)=',maskt(i,j,l)
c      print*,'maskt(i1,j,l)=',maskt(i1,j,l)
c      print*,'maskt(i,j1,l)=',maskt(i,j1,l)
c      print*,'maskt(i1,j1,l)=',maskt(i1,j1,l)
       endif
       
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
300       continue
          igood=0
          endif   !  if kmax.gt.0
          enddo   !  l=1 to kmax
301       continue
302    continue
      END DO  !  n=1 to nrep

      PRINT *, ' COMPUTED ', nwith, ' POINTS WITHIN DOMAIN'

      deallocate(z)
      deallocate(t)
      deallocate(u)
      deallocate(v)
      deallocate(q)
      deallocate(alnq)

      deallocate(pgd)
      deallocate(maskz)
      deallocate(maskt)
      deallocate(masku)
      deallocate(maskv)
      deallocate(maskq)

      deallocate(ps)
      deallocate(zs)
      deallocate(ts)
      deallocate(qs)
      deallocate(us)
      deallocate(vs)
      deallocate(pm)

      deallocate(cape)
      deallocate(cin)
      deallocate(bcape)
      deallocate(pli)

      deallocate(vis)
      deallocate(tmax)
      deallocate(tmin)
      deallocate(dpt)
      deallocate(tocc)
      deallocate(gust)
      deallocate(pbls)

      deallocate(pwo)
      deallocate(trop)
      deallocate(cdtz)
      deallocate(pblris)
      deallocate(pblmxs)

      deallocate(mskps)
      deallocate(mskzs)
      deallocate(mskts)
      deallocate(mskqs)
      deallocate(mskus)
      deallocate(mskvs)
      deallocate(mskpm)

      deallocate(mskcp)
      deallocate(mskcn)
      deallocate(mskbc)
      deallocate(mskli)

      deallocate(mskvis)
      deallocate(msktmax)
      deallocate(msktmin)
      deallocate(mskdpt)
      deallocate(msktocc)
      deallocate(mskgust)
      deallocate(mskpbl)

      deallocate(mskpw)
      deallocate(msktrp)
      deallocate(mskcdtz)
      deallocate(mskpri)
      deallocate(mskpmx)
 
c     deallocate(kxid)
c     deallocate(kyjd)
c     deallocate(wtswd)
c     deallocate(wtsed)
c     deallocate(wtnwd)
c     deallocate(wtned)

c     deallocate(kxi)
c     deallocate(kyj)
c     deallocate(wtsw)
c     deallocate(wtse)
c     deallocate(wtnw)
c     deallocate(wtne)

c     deallocate(rm1)
c     deallocate(rm2)

      print*,'stuff deallocated'

      RETURN
      END
