c********************************************************************
C  GRIDTOBS   -  CREATE VERIFICATION STATS BETWEEN OBSERVATIONS AND
C                BACKGROUNDS.  THE BACKGROUND & OBS VALUES ARE READ
C                FROM A PREPBTIM (GLOBAL) OR PREPFITS (MESO) BUFR FILE
C
      PROGRAM gridtobs

      INCLUDE 'parm.inc'

      real*8 sumdata(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            sumgrid(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            sumprod(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            ssqdata(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            ssqgrid(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            count(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxstat),
     +            summae(mxfcst,mxvrbl,maxlvl,mxarea,maxobs)

      real*8 summ,sumd,ssqd,sumg,ssqg,prod,sump
 
      real*8 mae
      real*8 obsval,forcst

      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)

      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      LOGICAL	  vtflg, nmbflg
      COMMON /cnvrsns/ vtflg, nmbflg (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
      CHARACTER*3 regions (30)
      COMMON /grdef/ mode(mxarea), imax(mxarea), imin(mxarea), 
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea), 
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea), 
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea), 
     +            lambert(mxarea), polarstereo(mxarea), numreg(mxarea),
     +            ig104(147,110), regions

      COMMON /fho/ numthr(mxvrbl), thresh(mxvrbl,mxthr)

      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (3)

      CHARACTER*1 blank, equal
C
      CHARACTER*80 headr, obstr, obstr2,obstr3,obstr4,qmstr
      CHARACTER*8 subset, stnid
C
      INTEGER	lstart (maxobs), lstop (maxobs)
      REAL*8 hdr(10), obs(10,255,mxb), obs2(9,255,mxb),
     *     qms(7,255),obs4(6,1,mxb),obs5(3,1,mxb)
      real*8 dhr,probs
      EQUIVALENCE (hdr(1),stnid)
C
C...   STRING FOR HEADER PARAMETERS
C
      DATA headr /'SID XOB YOB DHR ELV TYP T29 ITP'/
C
C...   STRING FOR THE OB, GUESS, ANALYSIS ....
C
c     DATA obstr /'SRC FHR POB QOB TOB ZOB UOB VOB PMO CAPE CINH LI'/
c     data obstr /'SRC FHR POB QOB TOB ZOB UOB VOB PMO PBL CAPE CINH LI'/
      data obstr2 /'MXTM MITM TDO HOVI TOCC MXGS THI TCH CDBZ'/
      data obstr3 /'CAPE CINH LI TROP PWO BCAPE'/
      data obstr4 /'TKEPBL RIPBL MIXHT'/
C
C...   STRING FOR THE QUALITY MARKS ....
C
      DATA qmstr /'CAT PRC PQM QQM TQM ZQM WQM    '/
      DATA stnid /'        '/
C----------------------------------------------------
      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
C
      DATA bmiss /10E10/
      DATA rmiss /99999./

C-- Verification using thresholds.
cmmm  parameter (numthr=6)
cmmm  dimension thresh(numthr)
      dimension fot(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxthr),
     +          hot(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxthr),
     +          oot(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxthr),
     +          fg(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxthr),
     +          hg(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxthr),
     +          og(mxfcst,mxvrbl,maxlvl,mxarea,maxobs,mxthr),
     +          tg(mxfcst,mxvrbl,maxlvl,mxarea,maxobs)
cmmm  DATA THRESH /50., 65., 85., 105., 125., 150./

c     call datelen(10)
c     obstr='SRC FHR POB QOB TOB ZOB UOB VOB PMO PBL CAPE CINH LI'
      obstr='SRC FHR POB QOB TOB ZOB UOB VOB PMO'
      print*,'obstr=',obstr
   10 CONTINUE
      call datelen(10)
C     
C     READ VERIFICATION DATABASE VERSION AND INPUT UNIT NUMBER
C
C     Also read in logical flag (T or F) to convert virtual temperature
C     observed into actual temperature.
C     
c     READ (5,'(A)',END=160) input
c     CALL ST_CLST ( input, ' ', ' ', 3, substr, num, ier )
c     namversion = substr (1)
c     CALL ST_NUMB ( substr (2), lunin, ier )
      READ (5,*,END=160) namversion,lunin
      PRINT '(A3,I5)', namversion, lunin
c     vtflg = ( substr (3) .eq. 't' .or. substr (3) .eq. 'T' )
c     vtflg = .true.

      CALL datebf(lunin,iy,im,id,ih,idate)
      print*,'iy,im,id,ih=',iy,im,id,ih
      CALL openbf(lunin,'IN ',lunin)
      call maxout(30000)
      PRINT *, ' DATE OF INPUT BUFR FILE', idate, ' UNIT=', lunin
C     
C     READ REST OF THIS CONTROL-FILE GROUP
C     TO GET NUMBER OF THINGS TO BE VERIFIED
C     
      CALL readcntl(numodel,numfcst,numvfdate,numvfyobs,numarea,numstat,
     +            numvarbl,numlevel,numvector)

c     print*,'after readcntl'

      IF (numvfdate.eq.1.and.nchrvfdate(1).eq.2) THEN
        WRITE (namvfdate(1)(1:10),'(I10)') idate
        nchrvfdate(1) = 10
        PRINT '(" NEW NAMVFDATE =",A24)', namvfdate(1)
        PRINT '(" CHARACTER COUNT =",I5)', nchrvfdate(1)
      END IF
C     
C     LET'S DO THE LOOPS
C     
C     OUTERMOST LOOP OVER VERIFYING DATE
C     
      DO 150 ivfdate = 1, numvfdate
c      print*,'inside 150 loop'

          FG      = 0.
          HG      = 0.
          OG      = 0.
          TG      = 0.
C       
C       OUTER LOOP OVER FORECAST MODEL TO BE VERIFIED
C       
c       print*,'numodel=',numodel
        DO 140 imodel = 1, numodel
c         print*,'inside 140 loop'
C         
C         ZERO THE SUMS
C         
          count = 0.0
          sumdata = 0.0
          ssqdata = 0.0
          sumgrid = 0.0
          ssqgrid = 0.0
          sumprod = 0.0
          summae = 0.0
C         
C         HERE WE GET A BUFR FILE AND START LOOP OVER EACH OB
C         
          DO WHILE (ireadmg(lunin,subset,jdate).eq.0)
C           nsub = nmsub(lunin)
c           PRINT *, 'SUBSET,NSUB,JDATE', subset, nsub, jdate
            ichk = 0
            DO 20 iob = 1, numvfyobs
	      lstart (iob) = 1
	      lstop (iob) = 0
              IF (namvfyobs(iob)(:6).eq.'ANYSFC') THEN
                IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP'.or.
     +                      subset(:6).eq.'ADPUPA'.or.subset(:6).eq.
c    +                      'PROFLR'.or.subset(:6).eq.'MSONET'.
     *                      'PROFLR'.
     *                      or.subset(:6).eq.'GPSIPW') THEN
			    ichk = 1
			    IF (subset(:6).eq.'ADPSFC'.or.
     +			        subset(:6).eq.'SFCSHP') lstop (iob) = 1
		END IF
              ELSE IF (namvfyobs(iob)(:6).eq.'ANYAIR') THEN
                IF (subset(:6).eq.'AIRCAR'.or.subset(:6).eq.'AIRCFT') 
     +                      ichk = 1
	      ELSE IF (namvfyobs(iob)(:6).eq.'ONLYSF') THEN
c               IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP')
                IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP'.
     *              or.subset(:6).eq.'MSONET')
     +		THEN
	            ichk = 1
		    lstart (iob) = 2
		END IF
              ELSE IF (subset(:6).eq.namvfyobs(iob)(:6)) THEN
                ichk = 1
		IF (subset(:6).eq.'ADPSFC'.or.
     +		    subset(:6).eq.'SFCSHP'.or.subset(:6).eq.'MSONET') then
                  lstop (iob) = 1
                endif
              END IF
   20       CONTINUE
            DO WHILE (ireadsb(lunin).eq.0.and.ichk.eq.1)
              CALL ufbint(lunin,hdr,10,1,nlev,headr)
c             PRINT *, 'NLEV,HDR', nlev, hdr
              dhr = hdr(4)
              adate = jdate
C             
C             INNER LOOP FOR ACTUAL OB-TYPE TO BE VERIFIED
C             
              DO 80 iob = 1, numvfyobs
                IF (isitob(subset,hdr(6),iob).eq.0) THEN
C                 
C                 INNER LOOP OVER VERIFYING AREA
C                 
                  DO 70 iar = 1, numarea
c                   print*,'before inarea,stnid=',stnid
                    IF (inarea(imodel,stnid,hdr(2),hdr(3),iar,rm1,rm2,
     *                  namarea(iar))
     +			.eq.0) THEN
c        print*,'before ufbints'
                      CALL ufbin3(lunin,obs,10,255,mxb,nlev,nevn,obstr)
           if((subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP').
     *        and.obs(9,1,nevn).eq.bmiss)
     *       then
             obs(9,2,nevn)=bmiss
           endif
c          if(subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP') then
c           print*,'subset,stnid=',subset,stnid
c          endif
c          print*,'nevn=',nevn
c          if(subset(:6).eq.'ADPUPA') then
c          print*,'subset,stnid=',subset,stnid
c          do n=1,nevn
c           do j=1,nlev
c            do i=4,4
c           print*,'obs(i,j,n)=',i,j,n,obs(i,j,n)
c          enddo
c          enddo
c          enddo
c          endif
c          obs2=bmiss
                      call ufbin3(lunin,obs2,9,255,mxb,nlev,nevn,
     *                   obstr2)
            if(subset(:6).eq.'ADPSFC') then
c             print*,'subset,stnid=',subset,stnid
              do j=1,nevn
              print*,'subset,stnid=',subset,stnid
              do i=1,6
                print*,'obs2(i,2,j)=',obs2(i,2,j)
              enddo
              enddo
            endif
c                     call ufbin3(lunin,obs3,2,255,mxb,nlev,nevn,
c    *                   'TROP PWO')
c               print*,'after trop pwo'
                if(subset(:6).eq.'ADPUPA'.or.subset(:6).eq.
     *             'GPSIPW') then
c                     print*,'obs4 read'
                      obs4=bmiss
                      nlev2=nlev
                      nlev=1
c                     print*,'before ufbin3'
                      call ufbin3(lunin,obs4,6,1,mxb,nlev,nevn,
     *                  obstr3)
c                     print*,'nevn=',nevn
c                     do n=1,nevn
c                      do j=1,2
c                      do i=1,6
c                      print*,'i,j,n,obs4(i,j,n)=',i,j,n,obs4(i,j,n)
c                     enddo;enddo;enddo
c                     print*,'after ufbin3 of obs4'
                      nlev=nlev2
                 endif
              if(subset(:6).eq.'ADPUPA') then
c                     print*,'obs5 read'
                      obs5=bmiss
                      nlev2=nlev
                      nlev=1
                      call ufbin3(lunin,obs5,3,1,mxb,nlev,nevn,
     *                  obstr4)
                      nlev=nlev2
              endif
                      CALL ufbint(lunin,qms,7,255,nlev,qmstr)
C                     
c                     print*,'after ufbints'
C                     INNER LOOP OVER OBSERVATION LEVELS
C                     
		      IF ( lstop (iob) .eq. 0 ) THEN
			nstop = nlev
		      ELSE
			nstop = lstop (iob)
		      END IF
                      
c                     IF (namvfyobs(iob)(:6).eq.'ONLYSF') then
c                       print*,'lstart(iob),nstop=',lstart(iob),nstop
c                     endif
          
                      DO 60 nlv = lstart (iob), nstop
                        probs = obs(3,nlv,nevn)
                        kat = nint(qms(1,nlv))
C                       INNER LOOP OVER REQUESTED LEVELS
                        DO 50 ilv = 1, numlevel
                          IF (inlayer(subset,probs,kat,ilv).eq.0) THEN
C                           
C                           INNER LOOP OVER VARIABLE 
C                           
                            DO 40 ivr = 1, numvarbl
C                             
C                             INNER LOOP OVER NUMBER OF FORECAST HOUR (EVENT)
C                             
c      icnt=0
                              DO 30 ifh = 1, numfcst
c            if(probs.eq.1000.)then
c            if(namvarbl(ivr).eq.'PBL'.or.
c    +          namvarbl(ivr).eq.'CAPE'.or.
c    +          namvarbl(ivr).eq.'CINH'.or.
c    +          namvarbl(ivr).eq.'LI') then
c            if(namvarbl(ivr).eq.'PBL') then
c               nlv_save=nlv
c               nlv=1
c            endif
c            endif
c            if(nlv.eq.1) then
c               print*,'namvarbl(ivr)=',namvarbl(ivr)
c               print*,'probs=',prob
c            endif
c            if(stnid.eq.'D0695  a') print*,'before igotdata'
                                IF (igotdata(obsval,forcst,obs,obs2,
     *                                      obs4,obs5,qms,
     +                                      nevn,nlv,imodel,ifh,ivr,ilv,
     +                                     iob,rm1,rm2,subset,stnid,iar)
     +                                      .eq.0) THEN
c            if(subset(:6).eq.'ADPUPA') print*,'stnid=',stnid
                     DO 200 ist=1,numstat
              
                     IF(namstat(ist).eq.'FHO') THEN
c             
                                   DO LTHR=1,NUMTHR(IVR)
                                      IF(obsval.GT.THRESH(IVR,LTHR))
     +                               og(ifh,ivr,ilv,iar,iob,lthr)=
     +                               og(ifh,ivr,ilv,iar,iob,lthr)+1.
                                      IF(forcst.GT.THRESH(IVR,LTHR))
     +                                  fg(ifh,ivr,ilv,iar,iob,lthr)=
     +                                  fg(ifh,ivr,ilv,iar,iob,lthr)+1.
                                      IF(obsval.GT.THRESH(IVR,LTHR).AND.
     +                                   forcst.GT.THRESH(IVR,LTHR))
     +                                  hg(ifh,ivr,ilv,iar,iob,lthr)=
     +                                  hg(ifh,ivr,ilv,iar,iob,lthr)+1.
                                   ENDDO
                                        tg(ifh,ivr,ilv,iar,iob)=
     +                                  tg(ifh,ivr,ilv,iar,iob)+1.
              
                                  count(ifh,ivr,ilv,iar,iob,ist) =
     +                                    count(ifh,ivr,ilv,iar,iob,ist)
     +                                    + 1.0

c     
c      icnt=icnt+1
c      print*,'icnt=',icnt
c            if(probs.eq.1000.)then
c            if(namvarbl(ivr).eq.'PBL'.or.
c    +          namvarbl(ivr).eq.'CAPE'.or.
c    +          namvarbl(ivr).eq.'CINH'.or.
c    +          namvarbl(ivr).eq.'LI') then
c            if(namvarbl(ivr).eq.'PBL') then
c               nlv=nlv_save
c            endif
c            endif

                     ELSEIF(namstat(ist).eq.'SL1L2') THEN
      if(namvfyobs(iob).eq.'ONLYSF'.and.namarea(iar).eq.'G236'.
     *   and.namfcst(ifh).eq.'00'.and.namvarbl(ivr).eq.'TMAX'.
     *   and.namlevel(ilv).eq.'SFC') then
         print*,'count=',count(ifh,ivr,ilv,iar,iob,ist)
         print*,'stnid,subset=',stnid,subset,namfcst(ifh)
         print*,'obsval,forcst=',obsval,forcst
      endif

                                  mae=abs(forcst-obsval)
                                  summ=summae(ifh,ivr,ilv,iar,iob)*
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
     +                                          + mae         
                                  sumd = sumdata(ifh,ivr,ilv,iar,iob) *
     +                                count(ifh,ivr,ilv,iar,iob,ist)
     +                                        + obsval
                                  ssqd = ssqdata(ifh,ivr,ilv,iar,iob) *
     +                                count(ifh,ivr,ilv,iar,iob,ist)
     +                                        + obsval * obsval
                                  sumg = sumgrid(ifh,ivr,ilv,iar,iob) *
     +                                count(ifh,ivr,ilv,iar,iob,ist)
     +                                        + forcst
c     if(namvfyobs(iob).eq.'ONLYSF'.and.namarea(iar).eq.'G236'.
c    *   and.namfcst(ifh).eq.'12'.and.namvarbl(ivr).eq.'RH'.
c    *   and.namlevel(ilv).eq.'SFC') then
c        print*,'sumg=',sumg
c     endif
                                  ssqg = ssqgrid(ifh,ivr,ilv,iar,iob) *
     +                                count(ifh,ivr,ilv,iar,iob,ist)
     +                                        + forcst * forcst
                                  prod = forcst * obsval
                                  sump = sumprod(ifh,ivr,ilv,iar,iob) *
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
     +                                        + prod
                                  count(ifh,ivr,ilv,iar,iob,ist) = 
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
     +                                        + 1.0
                                  sumdata(ifh,ivr,ilv,iar,iob) = sumd /
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
                                  ssqdata(ifh,ivr,ilv,iar,iob) = ssqd /
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
                                  sumgrid(ifh,ivr,ilv,iar,iob) = sumg /
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
                                  ssqgrid(ifh,ivr,ilv,iar,iob) = ssqg /
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
                                  sumprod(ifh,ivr,ilv,iar,iob) = sump /
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
                                  summae(ifh,ivr,ilv,iar,iob) = summ /
     +                                 count(ifh,ivr,ilv,iar,iob,ist)
                               endif ! stat type
200      continue
                                END IF  ! igotdata
C                             
C                             END INNER LOOP OVER NUMBER OF FORECAST HOUR (EVENT)
   30                         CONTINUE
C                           END INNER LOOP OVER VARIABLE
   40                       CONTINUE
                          END IF
C                       END INNER LOOP OVER REQUESTED LEVELS
   50                   CONTINUE
C                     END INNER LOOP OVER OBSERVATION LEVELS
   60                 CONTINUE
                    END IF
C                 END INNER LOOP OVER VERIFYING AREA
   70             CONTINUE
                END IF
C             END LOOP FOR ACTUAL OB-TYPE
   80         CONTINUE
C           END DO WHILE LOOP OVER BUFR REPORT
            END DO
C         END DO WHILE LOOP OVER BUFR MESSAGE
          END DO
C         
C         NOW IS THE TIME TO WRITE OUT THE STAT RECORDS
C         
          ist = 1
         DO 300 ist=1,numstat
          DO 130 iob = 1, numvfyobs
            DO 120 iar = 1, numarea
              DO 110 ifh = 1, numfcst
                numv = numvarbl
                IF (numvector.gt.0) numv = numvarbl - 1
                DO 100 ivr = 1, numv
                  DO 90 ilv = 1, numlevel
c                   rtest = count(ifh,ivr,ilv,iar,iob)
c                   print *, 'COUNT(IFH,IVR,ILV,IAR,IOB)=',RTEST
                    IF (count(ifh,ivr,ilv,iar,iob,ist).gt.0.0) THEN
                      iend = 3
                      vdbhdr132(1:iend) = namversion(:3)
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrmodel(imodel)
                      vdbhdr132(istrt:iend) = 
     +                            namodel(imodel)(:nchrmodel(imodel))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrfcst(ifh)
                      vdbhdr132(istrt:iend) = 
     +                            namfcst(ifh)(:nchrfcst(ifh))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfdate(ivfdate)
                      vdbhdr132(istrt:iend) = 
     +                            namvfdate(ivfdate)(:
     +                            nchrvfdate(ivfdate))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfyobs(iob)
                      vdbhdr132(istrt:iend) = 
     +                            namvfyobs(iob)(:nchrvfyobs(iob))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrarea(iar)
                      vdbhdr132(istrt:iend) = 
     +                            namarea(iar)(:nchrarea(iar))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      ivstrt = istrt
                      iend = iend + nchrstat(ist)
                      vdbhdr132(istrt:iend) = 
     +                            namstat(ist)(:nchrstat(ist))
                      iend = iend + 1
c-------------------------------------------------------------
                        if(namstat(ist).eq.'FHO')then
                          vdbhdr132(iend:iend) = '>'
                          ibreak1=iend
                          ibreak2=iend + 1
                          iend = iend + 1
                        endif
c-------------------------------------------------------------
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvarbl(ivr)
                      vdbhdr132(istrt:iend) = 
     +                            namvarbl(ivr)(:nchrvarbl(ivr))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrlevel(ilv)
                      vdbhdr132(istrt:iend) = 
     +                            namlevel(ilv)(:nchrlevel(ilv))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      iend = iend + 1
                      vdbhdr132(iend:iend) = equal
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                     IF(namstat(ist).eq.'FHO') THEN
              
c                 print*,'FHO'
c------ FHO
                          IF(TG(ifh,ivr,ilv,iar,iob).gt.0)THEN
                           DO LTHR=1,NUMTHR(IVR)
                   fot(ifh,ivr,ilv,iar,iob,lthr)=
     +             fg(ifh,ivr,ilv,iar,iob,lthr)/tg(ifh,ivr,ilv,iar,iob)
                   hot(ifh,ivr,ilv,iar,iob,lthr)=
     +             hg(ifh,ivr,ilv,iar,iob,lthr)/tg(ifh,ivr,ilv,iar,iob)
                   oot(ifh,ivr,ilv,iar,iob,lthr)=
     +             og(ifh,ivr,ilv,iar,iob,lthr)/tg(ifh,ivr,ilv,iar,iob)
              
c                       PRINT'(A,F7.0,7E18.9)',VDBHDR132(:IEND)
                        WRITE (50,1202) vdbhdr132(:ibreak1),
     +                       THRESH(IVR,LTHR),
     +                       vdbhdr132(ibreak2:iend),
     +                       count(ifh,ivr,ilv,iar,iob,ist),
     +                       fot(ifh,ivr,ilv,iar,iob,lthr),
     +                       hot(ifh,ivr,ilv,iar,iob,lthr),
     +                       oot(ifh,ivr,ilv,iar,iob,lthr)
                           ENDDO
                         ENDIF
 1202                   FORMAT (A,f0.0,A,F7.0,3f8.5)
c------
              
                     ELSEIF(namstat(ist).eq.'SL1L2') THEN

c                     print *, 'IVR=',IVR,' and NUMVECTOR=',NUMVECTOR
                      IF (ivr.ne.numvector) THEN
C                       PRINT'(A,F7.0,5E18.9)',VDBHDR132(:IEND),
c     print*,'namvfyobs(iob)=',namvfyobs(iob)
c     print*,'namarea(iar)=',namarea(iar)
c     print*,'namfcst(ifh)=',namfcst(ifh)
c     print*,'namvarbl(ivr)=',namvarbl(ivr)
c     print*,'namlevel(ilv)=',namlevel(ilv)
      if(namvfyobs(iob).eq.'ONLYSF'.and.namarea(iar).eq.'G236'.
     *   and.namfcst(ifh).eq.'12'.and.namvarbl(ivr).eq.'RH'.
     *   and.namlevel(ilv).eq.'SFC') 
     *   print*,'sumgrid=',sumgrid(ifh,ivr,ilv,iar,iob)
                        WRITE (50,1000) vdbhdr132(:iend), 
     +                              count(ifh,ivr,ilv,iar,iob,ist), 
     +                              sumgrid(ifh,ivr,ilv,iar,iob), 
     +                              sumdata(ifh,ivr,ilv,iar,iob), 
     +                              sumprod(ifh,ivr,ilv,iar,iob), 
     +                              ssqgrid(ifh,ivr,ilv,iar,iob), 
     +                              ssqdata(ifh,ivr,ilv,iar,iob),
     +                              summae(ifh,ivr,ilv,iar,iob)
 1000                   FORMAT (A,F7.0,6E18.9)
                      END IF
C                     
                      IF (ivr.eq.numvector) THEN
                        sump = sumprod(ifh,ivr,ilv,iar,iob) + 
     +                              sumprod(ifh,ivr+1,ilv,iar,iob)
                        sumg = ssqgrid(ifh,ivr,ilv,iar,iob) + 
     +                              ssqgrid(ifh,ivr+1,ilv,iar,iob)
                        sumd = ssqdata(ifh,ivr,ilv,iar,iob) + 
     +                              ssqdata(ifh,ivr+1,ilv,iar,iob)
                        vdbhdr132(ivstrt:ivstrt) = namversion(1:)
C                       PRINT'(A,F7.0,7E18.9)',VDBHDR132(:IEND),
                        WRITE (50,1100) vdbhdr132(:iend), 
     +                              count(ifh,ivr,ilv,iar,iob,ist), 
     +                              sumgrid(ifh,ivr,ilv,iar,iob), 
     +                              sumgrid(ifh,ivr+1,ilv,iar,iob), 
     +                              sumdata(ifh,ivr,ilv,iar,iob), 
     +                              sumdata(ifh,ivr+1,ilv,iar,iob), 
     +                              sump, sumg, sumd
 1100                   FORMAT (A,F7.0,7E18.9)
                      END IF   !  vector
                     END IF    !  stat type
                    END IF     !  count
   90             CONTINUE
  100           CONTINUE
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
C         END LOOP OVER STATISTIC TYPE
  300     CONTINUE
C         END OUTER LOOP OVER FORECAST MODEL
  140     CONTINUE
C         END OUTERMOST LOOP OVER VERIFYING DATE
  150     CONTINUE
          CALL closbf(lunin)
C         GO BACK AND DO IT ALL AGAIN   WHOOPEE!
          GO TO 10
C23456789012345678901234567890123456789012345678901234567890123456789012
  160     CONTINUE
          print*,'END OF GRIDTOBS'
          STOP
          END
