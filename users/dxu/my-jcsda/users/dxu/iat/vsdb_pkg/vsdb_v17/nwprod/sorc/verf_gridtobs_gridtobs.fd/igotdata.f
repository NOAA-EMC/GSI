      FUNCTION igotdata(obsval,forcst,obs,obs2,obs4,obs5,qms,nevn,
     *            nlv,imodel,ifh,
     +            ivr,ilv,iob,rm1,rm2,subset,stnid,iar)
C
      INCLUDE 'parm.inc'
C
      COMMON /obmrk/ iqmod(maxobs)
      COMMON /fcsthr/ fhour(mxfcst)
      COMMON /model/ fprp, fmodel(maxmod)
      LOGICAL	  vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
C
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), 
     +            namvfdate(mxdate), namvfyobs(maxobs), 
     +            namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)
C
      character*8 subset,stnid
      CHARACTER*8 eric,eric1(maxmod)
      REAL 	  ericr1(maxmod)
      EQUIVALENCE (eric,ericr)
      EQUIVALENCE (eric1,ericr1)
      real*8 probs
      real*8 obsval,forcst
      real*8 tobsval,qobsval,pobsval,tforcst,qforcst,pforcst
      real*8 vpobs,vpfcs,qsobs,qsfcs
      real*8 D001
      real*8 D000001
      real*8 D273
      real*8 D10
      real*8 D62197
      real*8 D622
      real*8 D378
      real*8 D100

      data D001 /0.001/
      data D000001 /0.000001/
      data D273 /273.16/
      data D10 /10.0/
      data D62197 /.62197/
      data D622 /.622/
      data D378 /.378/
      data D100 /100.0/
C
      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
C
      real*8 obs(10,255,mxb), obs2(9,255,mxb), 
     *      qms(7,255),obs4(6,1,mxb),obs5(3,1,mxb)
C...   STRING FOR THE OB, GUESS, ANALYSIS ....
C     DATA OBSTR /'SRC FHR POB QOB TOB ZOB UOB VOB PMO CAPE CINH LI'/
C...   STRING FOR THE QUALITY MARKS ....
C     DATA QMSTR /'CAT PRC PQM QQM TQM ZQM WQM'/
      DATA bmiss /10E10/
C
c     NPRP = 0
c     DO N=1,NEVN
C  FIRST CHECK MODEL NAME FOR PRP
c     IF(OBS(1,NLV,N).EQ.FPRP) THEN
c       NPRP=N
c     ENDIF
c     ENDDO
      probs=obs(3,nlv,nevn)
      nprp = nevn
      nevm = nevn - 1
      nfcs = 0
      DO n = 1, nevm
        ericr = obs (1,nlv,n)
        ericr1 (imodel) = fmodel (imodel)
        ericr=ericr1 (imodel)
C       NEXT CHECK FOR  B O T H  FMODEL  A N D  FHOUR
c       print*,'obs(2,nlv,n),fhour(ifh)=',obs(2,nlv,n),fhour(ifh)
        IF ( ericr .eq. ericr1 (imodel) .and.
     +      nint(obs(2,nlv,n)).eq.nint(fhour(ifh))) THEN
          nfcs = n
        END IF
      END DO
      igotdata = -1
      IF (nfcs.eq.0) THEN
c       PRINT*,'*** NFCS=0 ***',NLV,NPRP,NFCS
c       PRINT 1000,NEVN,IMODEL,IFH,FPRP,FMODEL(IMODEL),FHOUR(IFH)
c       PRINT 1100,((OBS(I,NLV,N),I=1,8),N=1,NEVN)
        RETURN
      END IF
      if(probs.eq.1000.0.and.subset(:6).eq.'ADPUPA') then
         if(nlv.eq.1) i=1
         if(nlv.eq.2) i=2
      endif
      if(subset(:6).eq.'GPSIPW') i=2
C     CHECK FOR NON-MISSING SEA-LEVEL PRESSURE WITH PROPER QUALITY MARK
      IF (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'SLP' ) THEN
        IF ((iqmod(iob).eq.1.and.qms(3,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(3,nlv).ge.3.)) THEN
          IF (obs(9,nlv,nprp).lt.bmiss.and.obs(9,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(9,nlv,nprp)
            forcst = obs(9,nlv,nfcs)
c           print*,'obsval,forcst=',obsval,forcst
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR CAPE
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'CAPE'.
     *      and.nlv.le.2) then
        IF (obs4(1,1,nprp).lt.bmiss.and.obs4(1,1,nfcs).lt.bmiss)
     +              THEN
           obsval = obs4(1,1,nprp)
           forcst = obs4(1,1,nfcs)
           igotdata = 0
           RETURN
         END IF
C     CHECK FOR BEST CAPE
      ELSE IF (nchrvarbl(ivr).eq.5.and.namvarbl(ivr).eq.'BCAPE'.
     *      and.nlv.le.2) then
        IF (obs4(6,1,nprp).lt.bmiss.and.obs4(6,1,nfcs).lt.bmiss)
     +              THEN
           obsval = obs4(6,1,nprp)
           forcst = obs4(6,1,nfcs)
           igotdata = 0
           RETURN
         END IF
C     CHECK FOR CINH
      ELSE IF (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'CIN'.
     *    and.nlv.le.2) THEN
         IF (obs4(2,1,nprp).lt.bmiss.and.obs4(2,1,nfcs).lt.bmiss)
     +               THEN
            obsval = obs4(2,1,nprp)
            forcst = obs4(2,1,nfcs)
            igotdata = 0
            RETURN
          ENDIF
C      CHECK FOR LI
       ELSE IF (nchrvarbl(ivr).eq.2.and.namvarbl(ivr).eq.'LI'.
     *    and.nlv.le.2) THEN
         IF (obs4(3,1,nprp).lt.bmiss.and.obs4(3,1,nfcs).lt.bmiss)
     +               THEN
            obsval = obs4(3,1,nprp)
            forcst = obs4(3,1,nfcs)
            igotdata = 0
            RETURN
          ENDIF
C      CHECK FOR PBL
c      print*,'namvarbl(ivr)=',namvarbl(ivr)
c      ELSE IF (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'PBL'.
c    *    and.nlv.le.2) THEN
c        IF (obs4(1,1,nprp).lt.bmiss.and.obs4(1,1,nfcs).lt.bmiss)
c    +               THEN
c           obsval = obs4(1,1,nprp)
c           forcst = obs4(1,1,nfcs)
c           igotdata = 0
c           RETURN
c         ENDIF
C     CHECK FOR TKEPBL
      ELSE IF (nchrvarbl(ivr).eq.6.and.namvarbl(ivr).eq.'TKEPBL'.
     *    and.nlv.le.2) THEN
        IF (obs5(1,1,nprp).lt.bmiss.and.obs5(1,1,nfcs).lt.bmiss)
     +              THEN
           obsval = obs5(1,1,nprp)
           forcst = obs5(1,1,nfcs)
           igotdata = 0
           RETURN
         END IF
C     CHECK FOR RIPBL
      ELSE IF (nchrvarbl(ivr).eq.5.and.namvarbl(ivr).eq.'RIPBL'.
     *    and.nlv.le.2 ) THEN
         IF (obs5(2,1,nprp).lt.bmiss.and.obs5(2,1,nfcs).lt.bmiss)
     +               THEN
            obsval = obs5(2,1,nprp)
            forcst = obs5(2,1,nfcs)
            igotdata = 0
            RETURN
          ENDIF
C      CHECK FOR MIXHT
       ELSE IF (nchrvarbl(ivr).eq.5.and.namvarbl(ivr).eq.'MIXHT'.
     *    and.nlv.le.2 ) THEN
         IF (obs5(3,1,nprp).lt.bmiss.and.obs5(3,1,nfcs).lt.bmiss)
     +               THEN
            obsval = obs5(3,1,nprp)
            forcst = obs5(3,1,nfcs)
            igotdata = 0
            RETURN
          ENDIF
C     CHECK FOR TMAX
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'TMAX') THEN
        if (obs2(1,nlv,nprp).lt.bmiss.and.obs2(1,nlv,nfcs).lt.bmiss)
     *         THEN
          obsval = obs2(1,nlv,nprp)
          forcst = obs2(1,nlv,nfcs)
          igotdata = 0
          RETURN
        endif 
C     CHECK FOR TMIN
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'TMIN') THEN
        if (obs2(2,nlv,nprp).lt.bmiss.and.obs2(2,nlv,nfcs).lt.bmiss)
     *         THEN
          obsval = obs2(2,nlv,nprp)
          forcst = obs2(2,nlv,nfcs)
          igotdata = 0
          RETURN
        endif
C     CHECK FOR DPT
      ELSE IF (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'DPT') THEN
        if (obs2(3,nlv,nprp).lt.bmiss.and.obs2(3,nlv,nfcs).lt.bmiss)
     *        THEN
          obsval = obs2(3,nlv,nprp)
          forcst = obs2(3,nlv,nfcs)
          igotdata = 0
          return
        endif
C     CHECK FOR VIS
      else if (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'VIS') then
         if (obs2(4,nlv,nprp).lt.bmiss.and.obs2(4,nlv,nfcs).lt.bmiss)
     *        THEN
          obsval = obs2(4,nlv,nprp)
          forcst = obs2(4,nlv,nfcs)
          igotdata = 0
          return
        endif
C     CHECK FOR TOTAL CLOUD COVER
      else if (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'TCLD') then
         if (obs2(5,nlv,nprp).lt.bmiss.and.obs2(5,nlv,nfcs).lt.bmiss)
     *        THEN
          obsval = obs2(5,nlv,nprp)
          forcst = obs2(5,nlv,nfcs)
          igotdata = 0
          return
        endif
C     CHECK FOR WIND GUST
      else if (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'GUST') then
         if (obs2(6,nlv,nprp).lt.bmiss.and.obs2(6,nlv,nfcs).lt.bmiss)
     *        THEN
          obsval = obs2(6,nlv,nprp)
          forcst = obs2(6,nlv,nfcs)
          igotdata = 0
          return
        endif
C     CHECK FOR HEAT INDEX
      else if (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'HEAT') then
         if (obs2(7,nlv,nprp).lt.bmiss.and.obs2(7,nlv,nfcs).lt.bmiss
     *       .and.qms(7,nlv).lt.3.0.and.qms(5,nlv).lt.3.0)
     *        THEN
          obsval = obs2(7,nlv,nprp)
          forcst = obs2(7,nlv,nfcs)
          igotdata = 0
          return
        endif
C     CHECK FOR WIND CHILL
      else if (nchrvarbl(ivr).eq.5.and.namvarbl(ivr).eq.'CHILL') then
         if (obs2(8,nlv,nprp).lt.bmiss.and.obs2(8,nlv,nfcs).lt.bmiss)
     *        THEN
          obsval = obs2(8,nlv,nprp)
          forcst = obs2(8,nlv,nfcs)
          igotdata = 0
          return
        endif
c     CHECK FOR CLOUD BASE
      else if (nchrvarbl(ivr).eq.5.and.namvarbl(ivr).eq.'CLDBT') then
         if (obs2(9,nlv,nprp).lt.bmiss.and.obs2(9,nlv,nfcs).lt.bmiss)
     *        THEN
          obsval = obs2(9,nlv,nprp)
          forcst = obs2(9,nlv,nfcs)
          igotdata = 0
          return
        endif
c    CHECK FOR PRECIPITABLE WATER
c     print*,'namvarbl(ivr)=',namvarbl(ivr),subset
      else if (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'PWO' 
     *      .and.nlv.le.2.and.(subset(:6).eq.'ADPUPA'.or
     *      .subset(:6).eq.'GPSIPW')) then
         if (obs4(5,1,nprp).lt.bmiss.and.obs4(5,1,nfcs).lt.bmiss.
     *        and.subset(:6).eq.'ADPUPA')
     *        THEN
          obsval = obs4(5,1,nprp)
          forcst = obs4(5,1,nfcs)
          igotdata = 0
          return
        endif
         if (obs4(5,1,nprp).lt.bmiss.and.obs4(5,1,nfcs).lt.bmiss.
     *        and.subset(:6).eq.'GPSIPW'.and.nlv.eq.1)
     *        THEN
          obsval = obs4(5,2,nprp)
          forcst = obs4(5,2,nfcs)
          igotdata = 0
          return
        endif
      else if (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'TROP'
     *       .and.nlv.le.2) then
         if (obs4(5,1,nprp).lt.bmiss.and.obs4(5,1,nfcs).lt.bmiss)
     *        THEN
c         print*,'TROP nlv=',nlv
          obsval = obs4(5,1,nprp)
          forcst = obs4(5,1,nfcs)
          igotdata = 0
          return
        endif
C     CHECK FOR NON-MISSING HEIGHT WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'Z'.and.nchrvarbl(ivr).eq.1) THEN
c       if(subset(:6).eq.'ADPUPA') then
c          print*,'subset,stnid=',subset(:6),stnid
c          print*,'qms(6,nlv)=',qms(6,nlv)
c          print*,'iqmod(iob)=',iqmod(iob)
c          print*,'obs(6,nlv,nprp)=',obs(6,nlv,nprp)
c          print*,'obs(6,nlv,nfcs)=',obs(6,nlv,nfcs)
c          print*,'nlv,nprp,nfcs=',nlv,nprp,nfcs
c       endif
        IF ((iqmod(iob).eq.1.and.qms(6,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(6,nlv).ge.3.)) THEN
          IF (obs(6,nlv,nprp).lt.bmiss.and.obs(6,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(6,nlv,nprp)
            forcst = obs(6,nlv,nfcs)
c           if(subset(:6).eq.'ADPUPA') then
c            print*,'obsval,forcst=',obsval,forcst
c           endif
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING TEMPERATURE WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'T'.and.nchrvarbl(ivr).eq.1) THEN
c       if(stnid.eq.'D0695  a') then
c           print*,'iqmod(iob)=',iqmod(iob)
c           print*,'qms(5,nlv)=',qms(5,nlv)
c           print*,'obs(5,nlv,nprp)=',obs(5,nlv,nprp)
c           print*,'obs(5,nlv,nfcs)=',obs(5,nlv,nfcs)
c       endif
        IF ((iqmod(iob).eq.1.and.qms(5,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(5,nlv).ge.3.)) THEN
          IF (obs(5,nlv,nprp).lt.bmiss.and.obs(5,nlv,nfcs).lt.bmiss) 
     +                THEN
	    prs = obs(3,nlv,nprp)
	    IF ( vtflg .and. prs .gt. 400 ) THEN
		IF ((iqmod(iob).eq.1.and.qms(4,nlv).lt.3.).or.
     +		    (iqmod(iob).eq.2.and.qms(4,nlv).ge.3.)) THEN
		    IF ( obs(4,nlv,nprp).lt.bmiss) THEN
                	forcst = obs(5,nlv,nfcs)
			fac = 1. + .608 * obs(4,nlv,nprp) * .000001
			obsval = ( 273.15 + obs (5,nlv,nprp) ) / fac
			obsval = obsval - 273.15
			igotdata = 0
		    END IF
		END IF
	    ELSE
                obsval = obs(5,nlv,nprp)
                forcst = obs(5,nlv,nfcs)
                igotdata = 0
	    END IF
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING SPEC.HUM. WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'Q'.and.nchrvarbl(ivr).eq.1) THEN
        IF ((iqmod(iob).eq.1.and.qms(4,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(4,nlv).ge.3.)) THEN
          IF (obs(4,nlv,nprp).lt.bmiss.and.obs(4,nlv,nfcs).lt.bmiss) 
     +                THEN
c           obsval = obs(4,nlv,nprp) * 0.001
c           forcst = obs(4,nlv,nfcs) * 0.001
            obsval = obs(4,nlv,nprp) * D001
            forcst = obs(4,nlv,nfcs) * D001
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING TEMP&SPCHUM WITH PROPER QUALITY MARK
      ELSE IF (nchrvarbl(ivr).eq.2.and.(namvarbl(ivr).eq.'TV'
     +            .or.namvarbl(ivr).eq.'TD'
     +            .or.namvarbl(ivr).eq.'RH') ) THEN
        IF ((iqmod(iob).eq.1.and.(qms(4,nlv).lt.3..and.qms(5,nlv).lt.3.)
     +              ).or.(iqmod(iob).eq.2.and.(qms(4,nlv).ge.3..or.
     +              qms(5,nlv).ge.3.))) THEN
          IF (obs(3,nlv,nprp).lt.bmiss.and.obs(4,nlv,nprp).lt.bmiss.and.
     +                obs(4,nlv,nfcs).lt.bmiss.and.obs(5,nlv,nprp).lt.
     +                bmiss.and.obs(5,nlv,nfcs).lt.bmiss) THEN
            pobsval = obs(3,nlv,nprp)
c           qobsval = obs(4,nlv,nprp) * 0.000001
c           qforcst = obs(4,nlv,nfcs) * 0.000001
c           tobsval = obs(5,nlv,nprp) + 273.16
c           tforcst = obs(5,nlv,nfcs) + 273.16
            qobsval = obs(4,nlv,nprp) * D000001
            qforcst = obs(4,nlv,nfcs) * D000001
            tobsval = obs(5,nlv,nprp) + D273
            tforcst = obs(5,nlv,nfcs) + D273
            IF (namvarbl(ivr).eq.'TV') THEN
              obsval = (tobsval*(1.+qobsval/.62197)/(1.+qobsval)) - 
     +                    273.16
              forcst = (tforcst*(1.+qforcst/.62197)/(1.+qforcst)) - 
     +                    273.16
            ELSE IF (namvarbl(ivr).eq.'TD')
     +                  THEN
              vpobs = (pobsval*qobsval) / (.622+.378*qobsval)
              obsval = log(vpobs/6.112) * 243.5 / (17.67-
     +                   log(vpobs/6.112))
              vpfcs = (pobsval*qforcst) / (.622+.378*qforcst)
              forcst = log(vpfcs/6.112) * 243.5 / (17.67-
     +                    log(vpfcs/6.112))
            ELSE IF (namvarbl(ivr).eq.'RH')
     +                  THEN
c             print*,'tobsval,tforcst=',tobsval,tforcst
c             print*,'qobsval,qforcst=',qobsval,qforcst
c             vpobs = w3fa09a(tobsval) * 10.
c             qsobs = .622 * vpobs / (pobsval-.378*vpobs)
c             vpfcs = w3fa09a(tforcst) * 10.
c             print*,'vpobs,vpfcs=',vpobs,vpfcs
c             qsfcs = .622 * vpfcs / (pobsval-.378*vpfcs)
c             obsval = (qobsval/qsobs) * 100.
c             forcst = (qforcst/qsfcs) * 100.
              vpobs = w3fa09a(tobsval) * D10
              qsobs = D622 * vpobs / (pobsval - D378*vpobs)
              vpfcs = w3fa09a(tforcst) * D10
              qsfcs = D622 * vpfcs / (pobsval - D378*vpobs)
              obsval = (qobsval/qsobs) * D100
              forcst = (qforcst/qsfcs) * D100
c             if (nint(fhour(ifh)).eq.12.and.subset(:6).eq.'ADPSFC') 
c    *            print*,'obsval,forcst=',obsval,forcst
            END IF
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING U-COMP WITH PROPER QUALITY MARK
      ELSE IF ((namvarbl(ivr).eq.'U'.and.nchrvarbl(ivr).eq.1).or.
     +            namvarbl(ivr).eq.'VWND') THEN
c       if(stnid.eq.'C2280   ') then
c           print*,'iqmod(iob)=',iqmod(iob)
c           print*,'qms(7,nlv)=',qms(7,nlv)
c           print*,'obs(7,nlv,nprp)=',obs(7,nlv,nprp)
c           print*,'obs(7,nlv,nfcs)=',obs(7,nlv,nfcs)
c       endif
        IF ((iqmod(iob).eq.1.and.qms(7,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(7,nlv).ge.3.)) THEN
          IF (obs(7,nlv,nprp).lt.bmiss.and.obs(7,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(7,nlv,nprp)
	    IF ( nmbgrd (imodel) .gt. 0 ) THEN
                vf = obs(8,nlv,nfcs)
		IF ( vf .lt. bmiss ) THEN
		    uf = obs(7,nlv,nfcs)
		    forcst = rm1 * uf + rm2 * vf
		    igotdata = 0
		END IF
	    ELSE
                forcst = obs(7,nlv,nfcs)
                igotdata = 0
	    END IF
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING V-COMP WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'V'.and.nchrvarbl(ivr).eq.1) THEN
        IF ((iqmod(iob).eq.1.and.qms(7,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(7,nlv).ge.3.)) THEN
          IF (obs(8,nlv,nprp).lt.bmiss.and.obs(8,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(8,nlv,nprp)
	    IF ( nmbgrd (imodel) .gt. 0) THEN
                uf = obs(7,nlv,nfcs)
		IF ( uf .lt. bmiss ) THEN
		    vf = obs(8,nlv,nfcs)
		    forcst = -rm2 * uf + rm1 * vf
		    igotdata = 0
		END IF
	    ELSE
                forcst = obs(8,nlv,nfcs)
                igotdata = 0
	    END IF
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING U-&V-COMP WITH PROPER QUALITY MARK
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'WSPD') THEN
        IF ((iqmod(iob).eq.1.and.qms(7,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(7,nlv).ge.3.)) THEN
          IF (obs(7,nlv,nprp).lt.bmiss.and.obs(7,nlv,nfcs).lt.bmiss.and.
     +                obs(8,nlv,nprp).lt.bmiss.and.obs(8,nlv,nfcs).lt.
     +                bmiss) THEN
            uobsval = obs(7,nlv,nprp)
            uf = obs(7,nlv,nfcs)
            vobsval = obs(8,nlv,nprp)
            vf = obs(8,nlv,nfcs)
	    IF ( nmbgrd (imodel) .gt. 0) THEN
		uforcst = rm1 * uf + rm2 *vf
		vforcst = -rm2 * uf + rm1 * vf
	    ELSE
		uforcst = uf
		vforcst = vf
	    END IF
            obsval = sqrt(uobsval*uobsval+vobsval*vobsval)
            forcst = sqrt(uforcst*uforcst+vforcst*vforcst)
            igotdata = 0
            RETURN
          END IF
        END IF
      END IF
 1000 FORMAT (3I5,2A8,F7.1)
 1100 FORMAT (A8,7(F8.2,2X))
      END
