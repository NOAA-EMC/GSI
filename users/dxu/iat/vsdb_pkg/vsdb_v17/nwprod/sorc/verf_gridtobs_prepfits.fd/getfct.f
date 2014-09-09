C-----------------------------------------------------------------------
C  GETFCT - INTERPOLATE BACKGROUND PROFILES to the irep report
C           VERTICALLY AND IN TIME
C-----------------------------------------------------------------------
      SUBROUTINE getfct(irep,ibak,fhour,onlysf,subset)

      use gridef
      use observ
      use backgv
      use guesfc
      use guess
      use counts
      use vdates
      use vrtfac

      INCLUDE 'parm.inc'

      LOGICAL onlysf

      character*8 subset
      real*8 pob,psig,zob
c     integer ipcp(5)

      REAL*8 adate,bdate

      real*8 p(mxl), z(mxl), t(mxl), u(mxl), v(mxl), q(mxl), a(mxl)

      real*8 f100
      DATA bmiss /10E10/
      DATA tzero /273.15/
      DATA betap /.0552/
      DATA beta /.00650/
      DATA rog /29.261/
      DATA g /9.81/
      DATA r /287.05/
      data f100 /100.0/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  COMPUTE THE TIME INTERPOLATION WEIGHT FOR THIS REPORT
C  -----------------------------------------------------

      IF (ibak.lt.nbak.and.nofo(ibak).ne.1) THEN
        dvhr = mod(vdata,f100)
        fvhr = mod(vdate(ibak),f100)
        IF (fvhr.gt.dvhr) fvhr = dvhr - fvhr + 24.
        timewt = abs((dvhr-fvhr)/(fhr(ibak+1)-fhr(ibak)))
        fhour = fhr(ibak) + abs(dvhr-fvhr)
        in = 1
      ELSE
        timewt = 0.
        fhour = fhr(ibak)
c       print*,'ibak,fhr(ibak),fhour=',ibak,fhr(ibak),fhour
        in = 0
      END IF

      bak = 0
      bak2 = 0
      bak3 = 0
      bak4 = 0

C     TIME INTERPOLATE VALUES FROM ADJACENT FORECAST PROFILES
C     -------------------------------------------------------

      DO it = 0, in
        jbak = ibak + it
        timw = 1 - it - timewt

C       GET GUESS PROFILE AT OB LOCATION
C       --------------------------------

        ps = psi(irep,jbak)
        pm = pmi(irep,jbak)
        zs = zsi(irep,jbak)
        ts = tsi(irep,jbak)
        us = usi(irep,jbak)
        vs = vsi(irep,jbak)
        qs = qsi(irep,jbak)
        cp = cpi(irep,jbak)
        bc = bcpi(irep,jbak)
        cn = cni(irep,jbak)
        px = pxi(irep,jbak)
        tmx = tmxi(irep,jbak)
        tmn = tmni(irep,jbak)
        dpt = dpti(irep,jbak)
        vis = visbi(irep,jbak)
        toc = tocci(irep,jbak)
        gus = gusti(irep,jbak)
c       th = thuni(irep,jbak)
        pbl = pbli(irep,jbak)
        pblri = pblrii(irep,jbak)
        pblmx = pblmxi(irep,jbak)
        pw = pwi(irep,jbak)
        trp = tropi(irep,jbak)
        cd = cdti(irep,jbak)

        DO k = 1, kmax
          p(k) = pi(k,irep,jbak)
          z(k) = zi(k,irep,jbak)
          t(k) = ti(k,irep,jbak)
c         print*,'k,irep,jbak,t=',k,irep,jbak,t(k)
          u(k) = ui(k,irep,jbak)
          v(k) = vi(k,irep,jbak)
          q(k) = qi(k,irep,jbak)
          a(k) = ai(k,irep,jbak)
        END DO

C       INTERPOLATE GUESS PROFILES TO OB PRESSURES
C       ------------------------------------------

        IF ( onlysf ) THEN
          lstop = 1
          pfc = psi(irep,jbak)
          pmo = pmi(irep,jbak)
          zob = zsi(irep,jbak)
          tob = tsi(irep,jbak)
          uob = usi(irep,jbak)
          vob = vsi(irep,jbak)
          qob = qsi(irep,jbak)
          tmxob = tmxi(irep,jbak)
          tmnob = tmni(irep,jbak)
          dptob = dpti(irep,jbak)
          visob = visbi(irep,jbak)
          tccob = tocci(irep,jbak)
          gstob = gusti(irep,jbak)
c         thunob = thuni(irep,jbak)
          pwob = pwi(irep,jbak)
          tropob = tropi(irep,jbak)
          cdtob = cdti(irep,jbak)
          IF (pfc.lt.bmiss) THEN
		bak(1,2) = bak(1,2) + pfc * timw
	  ELSE
		bak(1,2) = bmiss
	  END IF
          IF (qob.lt.bmiss) THEN
		bak(2,2) = bak(2,2) + qob * timw * 1.E6
	  ELSE
		bak(2,2) = bmiss
	  END IF
          IF (tob.lt.bmiss) THEN
		bak(3,2) = bak(3,2) + tob * timw - tzero
	  ELSE
		bak(3,2) = bmiss
	  END IF
          IF (zob.lt.bmiss) THEN
		bak(4,2) = bak(4,2) + zob * timw
	  ELSE
		bak(4,2) = bmiss
	  END IF
          IF (uob.lt.bmiss) THEN
		bak(5,2) = bak(5,2) + uob * timw
	  ELSE
		bak(5,2) = bmiss
	  END IF
          IF (vob.lt.bmiss) THEN
		bak(6,2) = bak(6,2) + vob * timw
	  ELSE
		bak(6,2) = bmiss
	  END IF
          IF (pmo.lt.bmiss) THEN
		bak(7,2) = bak(7,2) + pmo * timw
	  ELSE
		bak(7,2) = bmiss
	  END IF
          IF (tmxob.lt.bmiss) THEN
                bak2(1,2) = bak2(1,2) + tmxob * timw - tzero
          ELSE 
                bak2(1,2) = bmiss
          endif
          IF (tmnob.lt.bmiss) THEN
                bak2(2,2) = bak2(2,2) + tmnob * timw - tzero
          ELSE
                bak2(2,2) = bmiss
          endif
          IF (dptob.lt.bmiss) THEN
                bak2(3,2) = bak2(3,2) + dptob * timw
          ELSE
                bak2(3,2) = bmiss
          endif
          if (visob.lt.bmiss) then
                bak2(4,2) = bak2(4,2) + visob * timw
          else
                bak2(4,2) = bmiss
          endif
          if(tccob.lt.bmiss) then
                bak2(5,2) = bak2(5,2) + tccob * timw
          else
                bak2(5,2) = bmiss
          endif
          if(gstob.lt.bmiss) then
                bak2(6,2) = bak2(6,2) + gstob * timw
          else
                bak2(6,2) = bmiss
          endif
          if(cdtob.lt.bmiss) then
                bak2(9,2) = bak2(9,2) + cdtob * timw
          else
                bak2(9,2) = bmiss
          endif
c         if(thunob.lt.bmiss) then
c               bak2(9,2) = bak2(9,2) + thunob * timw
c         else
c               bak2(9,2) = bmiss
c         endif
c         print*,'pwob=',pwob
c         if(pwob.lt.bmiss) then
c               bak3(1,1) = bak3(1,2) + pwob * timw
c         else
c               bak3(1,2) = bmiss
c         endif
c         print*,'tob=',tob
          IF ( tob.lt.bmiss ) kntgsf = kntgsf + 1
c       kntgsf = kntgsf + 1
	ELSE
	    lstop = nlev
	END IF	
c       if(bak2(1,2).eq.0.0.and.bak2(2,2).eq.0.0.and.bak2(3,2).eq.0.0)
c    *     then
c       if(subset(:6).eq.'GPSIPW') then
c          bak=bmiss
c          bak(13,1)=0.0
c       endif
c       if(subset(:6).eq.'ADPUPA') then
c          bak2=bmiss
c       endif
C*

          IF (pbl.gt.0.and.pbl.lt.bmiss) THEN
c               bak3(1,2) = bak3(1,2) + pbl * timw
                bak4(1,2) = bak4(1,2) + pbl * timw
          ELSE
c               bak3(1,2) = bmiss
                bak4(1,2) = bmiss
          ENDIF
          IF (pblri.gt.0.and.pblri.lt.bmiss) THEN
                bak4(2,2) = bak4(2,2) + pblri * timw
          ELSE
                bak4(2,2) = bmiss
          ENDIF
          IF (pblmx.gt.0.and.pblmx.lt.bmiss) THEN
                bak4(3,2) = bak4(3,2) + pblmx * timw
          ELSE
                bak4(3,2) = bmiss
          ENDIF
          IF (cp.lt.bmiss) THEN
                bak3(1,2) = bak3(1,2) + cp * timw
          ELSE
                bak3(1,2) = bmiss
          END IF
          IF (cn.lt.bmiss) THEN
                bak3(2,2) = bak3(2,2) + cn * timw
          ELSE
                bak3(2,2) = bmiss
          END IF
          IF (px.lt.bmiss) THEN
                bak3(3,2) = bak3(3,2) + px * timw
          ELSE
                bak3(3,2) = bmiss
          END IF
          IF (trp.lt.bmiss) THEN
                bak3(4,2) = bak3(4,2) + trp * timw
          ELSE
                bak3(4,2) = bmiss
          END IF
          if(pw.lt.bmiss) then
                bak3(5,2) = bak3(5,2) + pw * timw
          else
                bak3(5,2) = bmiss
          endif
          IF (bc.lt.bmiss) THEN
                bak3(6,2) = bak3(6,2) + bc * timw
          ELSE
                bak3(6,2) = bmiss
          END IF
          if(subset(:6).eq.'ADPUPA') then
           do i=1,8
            bak2(i,2)=bmiss
           enddo
          endif
          
        DO l = 1, lstop

          pob = obs(1,l)
          qob = obs(2,l)
          tob = obs(3,l)
          zob = obs(4,l)
          uob = obs(5,l)
          vob = obs(6,l)
          pmo = obs(7,l)


C         SEA-LEVEL PRESSURE
C         ------------------

          IF ((cat(l).eq.0.or.cat(l).eq.6).and.pm.lt.bmiss) THEN
            pmo = pm
            bak(7,l) = bak(7,l) + pmo * timw
          ELSE
            pmo = bmiss
            bak(7,l) = bmiss
          END IF

C         SURFACE PRESSURE
C         ----------------
          IF (cat(l).eq.0.or.(cat(l).eq.6.and.pm.lt.bmiss)) THEN
            cat(l) = 0
            IF (pob.lt.bmiss) THEN
              pfc = pob
            ELSE IF (zob.lt.bmiss.and.ts.lt.bmiss.and.ps.lt.bmiss.and.zs
     +                  .lt.bmiss) THEN
              dz = zob - zs
              tm = ts - dz * beta * .5
              pfc = ps * exp(-dz/(tm*rog))
            ELSE
              pfc = bmiss
            END IF
          ELSE
            pfc = bmiss
          END IF

C         PSIG IS THE SURFACE (HIGHEST) GUESS PRESSURE
          psig = p(1)

          IF (pob.gt.0..and.pob.lt.bmiss) THEN
            ip = pob
            ip = min(ip,maxprs)
            ip = max(ip,1)

C           SPECIFIC HUMIDITY
C           -----------------

            IF (qob.lt.bmiss) THEN
              lb = vrterp(ip)
              wt = vrterp(ip) - lb
              la = min(lb+1,kmax)
              if(nint(pob).eq.nint(p(lb))) la=lb
              IF (a(la).lt.bmiss.and.a(lb).lt.bmiss) THEN
                aob = a(lb) + (a(la)-a(lb)) * wt
                qob = exp(aob) * 1.E6
              ELSE
                qob = bmiss
              END IF
            END IF

C           TEMPERATURE
C           -----------

            tobo=tob
            IF (tob.lt.bmiss) THEN
              IF (pob.ge.psig) THEN
                tob = t(1) + (pob-psig) * betap - tzero
                IF (t(1).ge.bmiss) tob = bmiss
              ELSE 
                lb = vrterp(ip)
                wt = vrterp(ip) - lb
                la = min(lb+1,kmax)
                if(nint(pob).eq.nint(p(lb))) la=lb
                IF (t(la).lt.bmiss.and.t(lb).lt.bmiss) THEN
                  tob = t(lb) + (t(la)-t(lb)) * wt - tzero
                ELSE
                  tob = bmiss
                END IF
              END IF
c            enddo
            END IF

C           HEIGHT
C           ------

            zobo=zob
            IF (zob.lt.bmiss) THEN
              IF (pob.gt.psig) THEN
                IF (t(1).lt.bmiss.and.p(1).lt.bmiss) THEN
                  tm = t(1) + (.5*(pob-psig)) * betap
                  zob = z(1) - rog * tm * log(pob/p(1))
                ELSE
                  zob = bmiss
                END IF
              else if (psig.eq.1000..and.pob.eq.psig) then
                zob = z(1)
              ELSE
                lb = vrterp(ip)
                wt = vrterp(ip) - lb
                la = max(min(lb+1,kmax),1)
                if(nint(pob).eq.nint(p(lb))) la=lb
                IF (t(la).lt.bmiss.and.t(lb).lt.bmiss.and.p(lb).lt.bmiss
     +                      ) THEN
                  tm = t(lb) + (t(la)-t(lb)) * wt
                  if(nint(pob).ne.nint(p(lb))) then
                   zob = z(lb) - rog * tm * log(pob/p(lb))
                  else if(nint(pob).eq.nint(p(lb))) then
                   zob = z(lb)
                  endif
                ELSE
                  zob = bmiss
                END IF
              END IF
            END IF

c           print*,'zob=',zob
c           print*,'pob=',pob
c           print*,'la,lb=',la,lb
c           print*,'t(la), t(lb)=',t(la), t(lb)
c           print*,'bmiss=',bmiss
c           print*,'psig=',psig
c           print*,'tm=',tm
c           print*,'wt=',wt
c           print*,'vrterp(ip)=',vrterp(ip)

C           U AND V COMPONENTS
C           ------------------

            IF (uob.lt.bmiss.or.vob.lt.bmiss) THEN
              lb = vrterp(ip)
              wt = vrterp(ip) - lb
              la = min(lb+1,kmax)
              IF (u(la).lt.bmiss.and.u(lb).lt.bmiss.and.v(la).lt.bmiss
     +                    .and.v(lb).lt.bmiss) THEN
                uob = u(lb) + (u(la)-u(lb)) * wt
                vob = v(lb) + (v(la)-v(lb)) * wt
              ELSE
                uob = bmiss
                vob = bmiss
              END IF
            END IF
          ELSE
            pfc = bmiss
            qob = bmiss
            tob = bmiss
            zob = bmiss
            uob = bmiss
            vob = bmiss
          END IF

C         FILL THE MISSING VALUES WITH A LARGE NUMBER
C         -------------------------------------------

          IF (pfc.ge.bmiss) bak(1,l) = bmiss
          IF (qob.ge.bmiss) bak(2,l) = bmiss
          IF (tob.ge.bmiss) bak(3,l) = bmiss
          IF (zob.ge.bmiss) bak(4,l) = bmiss
          IF (uob.ge.bmiss) bak(5,l) = bmiss
          IF (vob.ge.bmiss) bak(6,l) = bmiss

C         SCATTER BACK THE RELEVANT FORECAST VALUES
C         -----------------------------------------

          IF (pfc.lt.bmiss) bak(1,l) = bak(1,l) + pfc * timw
          IF (qob.lt.bmiss) bak(2,l) = bak(2,l) + qob * timw
          IF (tob.lt.bmiss) bak(3,l) = bak(3,l) + tob * timw
          IF (zob.lt.bmiss) bak(4,l) = bak(4,l) + zob * timw
          IF (uob.lt.bmiss) bak(5,l) = bak(5,l) + uob * timw
          IF (vob.lt.bmiss) bak(6,l) = bak(6,l) + vob * timw

c         if(l.ne.1) then
c           bak(8,l)=bmiss
c           bak(9,l)=bmiss
c           bak(10,l)=bmiss
c           bak(11,l)=bmiss
c           bak(12,l)=bmiss
c           bak(13,l)=bmiss
c         endif

        END DO
      END DO
c     enddo

c       deallocate(pi)
c       deallocate(zi)
c       deallocate(ti)
c       deallocate(ui)
c       deallocate(vi)
c       deallocate(qi)
c       deallocate(ai)

c       deallocate(psi)
c       deallocate(zsi)
c       deallocate(tsi)
c       deallocate(usi)
c       deallocate(vsi)
c       deallocate(qsi)
c       deallocate(bcpi)
c       deallocate(pxi)
c       deallocate(tmxi)
c       deallocate(dpti)
c       deallocate(visbi)
c       deallocate(tmni)
c       deallocate(tocci)
c       deallocate(gusti)
c       deallocate(pbli)
c       deallocate(pwi)
c       deallocate(tropi)
c       deallocate(cdti)
c       deallocate(pblrii)
c       deallocate(pblmxi)
c       deallocate(pmi)
c       deallocate(cpi)
c       deallocate(cni)

      RETURN
      END
