      PROGRAM VRFYGEN
!
!  M BALDWIN 9-10-96
!  M BALDWIN 2-05-97  Modified to increase size of largest grid
!  M BALDWIN 8-11-97  Modified to put in 32km as grid 92
!  M BALDWIN 10-30-97 Fixed up some things, DWD and ECMWF in particular
!  Y ZHU     09-20-02 To convert to operational implementation             
!  F Yang    09-20-2011 Modified for offline verification in VSDB package.
!  F Yang    03-23-2012 Modified for compatability with both intel and ibm machines
!
!  GENERAL PRECIP VERF CODE, USES M IRDELL'S IPLIB TO INTERPOLATE
!   CODE IS MORE OR LESS GENERAL, DRIVEN BY THE ANALYSIS GDS
!    BUT DIMENSIONS, ETC ARE SET UP TO GO TO GRID 211 (80km LMBC)
!    PARTICULARLY FOR THE REGIONAL MASKS
!
!  1. READ IN PRECIP ANALYSIS
!  2. READ IN PRECIP FORECAST
!  3. INTERPOLATE FORECAST TO ANALYSIS GRID
!  4. DO VERIFICATION
!  5. OUTPUT STATS, FORECAST ON ANALYSIS GRID FOR ARCHIVE
!
 
!     parameter(JI=2500*1500,JO=93*65,NUMREG=10,NUMTHR=9) 
      parameter(JI=4000*2000,JO=93*65,NUMREG=10,NUMTHR=9) 
      parameter(nc=200)

      integer IPOPT(20)
      integer KPDSA(nc),KGDSA(nc),KPDSF(nc),KGDSF(nc),KPDS1(nc)
      integer JPDS(nc),JGDS(nc)
      integer KGDSF2(nc),KPDSO(nc),KGDSO(nc),KGDS80(nc)
      integer ICYC(8),MNTH(12),JSTAT(32),MLANA(JI)
      INTEGER MASKREG(JO,NUMREG),MASKNAT(JI),MASK80(JI)

      LOGICAL*1 LFCI(JI),LANA(JO),LFCO(JO),LNAT(JI),FIRST,FIRCT
      LOGICAL*1 L80(JI),LFC80(JI),LPCPDA

      DIMENSION RLAT(JO),RLON(JO),SMASK(JO)
      DIMENSION FCI(JI),SUMFC(JI),FC2(JI),FCO(JO),SUM80(JI)
      DIMENSION ANA(JO),THRESH(NUMTHR),ANANAT(JI),ANAN80(JI)
      DIMENSION THRESH2(NUMTHR),FCOLD(JI)

      CHARACTER GDSO(400),FNAME*400,MDLNAM*8,FMASK(NUMREG)*4
      CHARACTER DATESTR*15,MONTH(12)*3,MDLVERF*10
      CHARACTER DATCMD*18
      CHARACTER*400 CPGBA,CPGIA,CPGBF,CPGIF,PCPDA,CMASK,DMASK,CTMPD

      data thresh/0.2,2.,5.,10.,15.,25.,35.,50.,75./
!     data thresh/.1,.25,1.,2.5,5.,10.,20.,25.,30.,40.,50.,75./
      data thresh2/0.01,0.10,0.25,0.50,0.75,1.0,1.5,2.0,3.0/
      data mnth/31,28,31,30,31,30,31,31,30,31,30,31/
      data month/'jan','feb','mar','apr','may','jun',
     &           'jul','aug','sep','oct','nov','dec'/
      data FMASK/ '    ','Appl','Npln','Spln',
     &           'East','Gulf','West','Rkys','Midw','Fntr'/
      data kgds80/201,12902,1,182,210113,136,92,141,577,538,64,
     &            0,0,0,0,0,0,0,0,255,0,0,178*0/
!
!  ASSIGN THE UNITS
      KGDSF  =  0
      KPDSF  =  0
      LUMASK = 13
      LPCDAT = 15
      MDLINF = 17
      LUNOUT = 51
      LOANA  = 61
      LSTOUT = 82

!
!  SET UP READING CART DATA FORMAT
 80   FORMAT(A80)
 88   FORMAT(A10)
 98   FORMAT(A40)
 8810 FORMAT(8A10)

!
!  ASSIGN AND OPEN STATS OUTPUT FILE 
      OPEN (UNIT=LSTOUT,FILE='stat.out',FORM='FORMATTED')
      OPEN (UNIT=LOANA,FILE='obs_box.dat',FORM='UNFORMATTED',
     *      STATUS='unknown')

!
!  READ IN MODEL INFO FILE NAME AND OPENED
      READ   (5,'(a)') FNAME
      WRITE  (6,'(a)') 'MODEL  INFORMATION FILE: ',trim(FNAME)
      CLOSE  (MDLINF)
      OPEN   (UNIT=MDLINF,FILE=FNAME,FORM='FORMATTED')
 99   REWIND (MDLINF)

!
!  READ IN CONUS and US REGIONAL MASK FILE NAME                   
      READ   (5,'(a)',END=9000) CMASK 
      WRITE  (6,'(a)') 'MASK: ',trim(CMASK)
      LCMASK=LEN_TRIM(CMASK)

!
!  READ IN FOR TEMP DIRECTORY                                
      READ   (5,'(a)',END=9000) CTMPD 
      WRITE  (6,'(a)') 'TEMP DIR: ',trim(CTMPD)
      LCTMPD=LEN_TRIM(CTMPD)

!
!  READ IN OBSERVATION PRECIPITATION DATA FILE NAME AND OPENED
      READ   (5,'(a)') PCPDA
      WRITE  (6,'(a)') 'OBS PRECIP: ',trim(PCPDA)
      IF (PCPDA(1:4).NE.'NONE') THEN
       LPCPDA=.TRUE.
       CLOSE(LPCDAT)
       OPEN(UNIT=LPCDAT,FILE=PCPDA,FORM='FORMATTED')
      ELSE
       LPCPDA=.FALSE.
      ENDIF
!
      read(5,*) KSTHR
      read(5,*) kpds5
!
!  READ IN MRF FACTOR FOR BIAS CORECTION ( Default:1.0 )
      READ   (5,*) FMRF
      WRITE  (6,*) 'MRF FACTER IS ',FMRF

      WRITE(6,*) 'MAKING ANALYSIS ON GRID 211 '
      DO K=1,NC
       KGDSA=0
      ENDDO
      CALL MAKGDS(211,KGDSA,GDSO,LENGDS,IRET)
      IMA = KGDSA(2)
      JMA = KGDSA(3)

      REWIND(LPCDAT)
      CALL ANNATV(LPCDAT,IMA,JMA,NC,211,KPDSA,KGDSA,ANA,MLANA)

      LANA=.FALSE.
      DO J=1,IMA*JMA
       LANA(J)=.FALSE.
       IF (MLANA(J).EQ.1) LANA(J)=.TRUE.
      ENDDO

      IPOPT=0
 
      WRITE(6,*) ' INPUT RES = ',KPDSA(3)
      WRITE(6,*) ' KGDSA-211 = ',KGDSA
      IG    = KPDSA(3)
      IMA   = KGDSA(2)
      JMA   = KGDSA(3)
      JSIZE = JMA*IMA
      IF (KGDSA(1).EQ.3) KGDSA(14)=KGDSA(12)

!
!  READ IN REGIONAL MASKS FOR THIS GRID
      DO NN=1,NUMREG
       WRITE(FNAME,881) IG,FMASK(NN)
       DMASK=CMASK(1:LCMASK) // FNAME(1:8)
       OPEN(UNIT=LUMASK,FILE=DMASK,FORM='UNFORMATTED')
       REWIND(LUMASK)
       WRITE (6,'(a)') ' START TO READ: ',trim(DMASK)
       READ(LUMASK) (SMASK(KK),KK=1,JSIZE)
       CLOSE(LUMASK)

       IC = 0

       DO KK=1,JSIZE
        IC=IC+NINT(SMASK(KK))
        MASKREG(KK,NN)=NINT(SMASK(KK))
       ENDDO

      ENDDO
C--------+---------+---------+---------+---------+---------+---------+---------+
 881   FORMAT(i3.3,'.',a4)
 886   FORMAT('  Irec  pds5 pds6 pds7 pds8 pds9 pd10 pd11 pd14',
     .       '  ndata  Minimun    Maximum')
 888   FORMAT (i4,2x,8i5,i8,2g12.4)
 889   FORMAT ('IRET = ',i3)               

!
!  SET UP DATES FOR VERIFICATION VALID TIME
      WRITE (6,*) 'kpds15=',KPDSA(15),'kpds11=',KPDSA(11)
      IACC = KPDSA(15)
      IVYR = (KPDSA(21)-1)*100+KPDSA(8)
      IVMN = KPDSA(9)
      IF (MOD(IVYR,100).NE.0.AND.MOD(IVYR,4).EQ.0) MNTH(2)=29
      IF (MOD(IVYR,400).EQ.0) MNTH(2)=29
      IVDA = KPDSA(10)
      IVHR = KPDSA(11)+KPDSA(15)
      DO WHILE (IVHR.GT.23)
       IVDA = IVDA+1
       IVHR = IVHR-24
      ENDDO
      IF (IVDA.GT.MNTH(IVMN)) THEN
       IVDA = IVDA-MNTH(IVMN)
       IVMN = IVMN+1
      ENDIF 
      IF (IVMN.GT.12) THEN
       IVMN = 1
       IVYR = IVYR+1
      ENDIF
      WRITE(6,*) 'VERIFICATION DATE: ',ivyr,ivmn,ivda,ivhr
!
!  MAIN LOOP, MAIN LOOP, MAIN LOOP
!  LOOP THROUGH MODELS TO VERIFY
!
!  READ MODEL NAME  
!
      READ(MDLINF,'(a)',END=99) MDLNAM
      MDLNAM=trim(MDLNAM)
      DO WHILE (MDLNAM.NE.'done')
       FIRST=.TRUE.
       FIRCT=.TRUE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      READ IN GRID NUM          - NGRID        C
!      NUM OF START TIMES        - NSTART       C
!      MODEL START TIME CYCLES   - ICYC         C
!      OUTPUT FREQUENCY          - IFREQ        C
!      FORECAST DURATION         - IFDUR        C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       READ(MDLINF,*) NGRID
       READ(MDLINF,*) NSTART
       DO N=1,NSTART
        READ(MDLINF,*) ICYC(N)
       ENDDO
       READ(MDLINF,*) IFREQ
       READ(MDLINF,*) IFDUR
       READ(MDLINF,*) IBUCKET
!
!MEB PROBABLY WANT TO CHANGE THIS
!
!      KDAT = INDEX(MDLNAM,' ') -1
!      IF (KDAT.LE.0) KDAT = LEN(MDLNAM)
       KDAT=len_trim(MDLNAM)

!  SET UP DATES FOR FORECAST START TIME, GIVEN VALID TIME
!
       INUMF  = IACC/IFREQ
       IFREQ2 = 24      
!
!  GET THE FORECASTS THAT ARE VALID AT THE ANALYSIS DATE/TIME
!  AND COVER THE SAME ACCUMULATION PERIOD AS THE ANALYSIS
!
       DO KTIME=IACC,IFDUR,IFREQ2
        ismn=ivmn
        isyr=ivyr
        isda=ivda
!       ishr=ivhr-ktime - ivhr
!       ishr=ivhr-ktime
        ishr=KSTHR-ktime
        do while (ishr.lt.0)
         isda=isda-1
         ishr=ishr+24
        ENDDO
        do while (isda.lt.1) 
         ismn=ismn-1
         do while (ismn.lt.1)
          isyr=isyr-1
          ismn=ismn+12
         ENDDO
         isda=isda+mnth(ismn)
        ENDDO
        WRITE(6,*) 'STARTED      TIME: ',isyr,ismn,isda,ishr
 
!  LOOP OVER FORECAST START CYCLEs
!
        DO n=1,nstart
         if (ishr.eq.ICYC(n)) THEN
          fc2   = 0.
          sumfc = 0.
          IFACC = 0
!
! loop over the number of forecasts we need to get an iacc h accum
!
          do kn=1,inumf
           ifhr=ktime + ivhr - KSTHR - (kn-1)*ifreq
!          ifhr=ktime + ivhr - (kn-1)*ifreq
           ifhr1=ifhr - ifreq

!           for continuous accumulation
            if (IBUCKET.EQ.0 .and. kn.gt.1) then
              ifhr=ifhr-ifreq
              ifhr1=ifhr1-ifreq
            endif

           CLOSE(21)
           CLOSE(22)
           if (ifhr1.lt.100.and.ifhr.lt.100) THEN
            WRITE (datcmd,120) isyr,ismn,isda,ishr,ifhr1,ifhr
           elseif (ifhr1.lt.100.and.ifhr.ge.100) THEN
            WRITE (datcmd,121) isyr,ismn,isda,ishr,ifhr1,ifhr
           else
            WRITE (datcmd,122) isyr,ismn,isda,ishr,ifhr1,ifhr
           ENDIF
           IF ((MDLNAM.eq.'meso').and.
     &         (kn.lt.inumf.or.ishr.eq.0)) THEN
            WRITE (datcmd,120) mod(isyr,100),ismn,isda,
     &             ishr+3,ifhr1-3,ifhr-3
           ENDIF
 120       format(i4.4,3i2.2,'_',i2.2,'_',i2.2)
 121       format(i4.4,3i2.2,'_',i2.2,'_',i3.3)
 122       format(i4.4,3i2.2,'_',i3.3,'_',i3.3)

!          CPGIF=' '
           CPGBF=CTMPD(1:LCTMPD) // '/' // MDLNAM(1:KDAT) // '/' //
     .        MDLNAM(1:KDAT) // '_' // datcmd 
           LPGB=LEN_TRIM(CPGBF)
!          CPGIF(1:LPGB)=CPGBF(1:LPGB)
!          CPGIF(LPGB+1:LPGB+6)='.index'         
!
!        CALL FUNCTION STAT TO FIND NUMBER OF BYTES IN FILE
!
           WRITE  (6,*) '==============================================' 
           WRITE  (6,'(a)') 'FORECAST DATA NAME: ',trim(CPGBF)
!          WRITE  (6,'(a)') 'FORECAST DATA NAME: ',trim(CPGIF)

           LPGB=LEN_TRIM(CPGBF)
!          LPGI=LEN_TRIM(CPGIF)
           CALL BAOPENR(21,CPGBF(1:LPGB),IER21)
!          CALL BAOPENR(22,CPGIF(1:LPGI),IER22)
!          IERRS = IER21 + IER22
           IERRS = IER21
           IF (IERRS.NE.0 .and. ifhr1 .ge. 0) THEN
            WRITE(6,'(a)') 'GRIB:BAOPEN ERR FOR DATA ',CPGBF           
!           WRITE(6,'(a)') 'GRIB:BAOPEN ERR FOR DATA ',CPGIF           
            WRITE(6,'(a)') 'PLEASE CHECK DATA AVAILABLE OR NOT'        
            GOTO 9100 
           ENDIF

!
!  READ IN PRECIP FORECAST, AND SUM UP IF NEEDED
           MDATA=ji
           J   =-1
           JPDS=-1
           JGDS=-1
           JPDS(5)=kpds5
           if (MDLNAM.eq.'ukm') then
            JPDS(5)=226
           ENDIF
!
!          CALL GETGB(21,22,MDATA,J,JPDS,JGDS,
           CALL GETGB(21,0,MDATA,J,JPDS,JGDS,
     .                KF,K,KPDSF,KGDSF,LFCI,FCI,IRET)
           IF (IRET.EQ.0) THEN
            CALL GRANGE(KF,LFCI,FCI,DMIN,DMAX)
            WRITE(*,886)
            WRITE(*,888) K,(KPDSF(I),I=5,11),KPDSF(14),KF,DMIN,DMAX
           ELSE
            WRITE(*,889) IRET
            WRITE(*,888) K,(KPDSF(I),I=5,11),KPDSF(14),KF
           ENDIF
!--------+---------+---------+---------+---------+---------+---------+--
           IF (KPDSF(5).EQ.61.OR.KPDSF(5).EQ.228.OR.KPDSF(5).EQ.226
     .        .OR.KPDSF(5).EQ.50.OR.KPDSF(5).EQ.59) THEN
 
            IF (NGRID.EQ.-1) NGRID=KPDSF(3)
            IF (KPDSF(3).EQ.NGRID) THEN
               
             IF (KPDSF(5).eq.61.or.KPDSF(5).eq.50.or.         
     &           KPDSF(5).eq.226)                  FCTR=1.
             IF (KPDSF(5).eq.59) FCTR=(KPDSF(15)-KPDSF(14))*3600.
          WRITE(6,*) 'FCTR=',FCTR,'PDS14=',KPDSF(14),'PDS15=',KPDSF(15)
!  global has units of mm/s
!

!  SUM UP ACCUMULATION PRECIPITATION
             IF (IBUCKET.EQ.0 .OR. KPDSF(15).EQ.0) THEN

!             if no bucket only use first and last outputs
              write(6,*)"IBUCKET= ", IBUCKET
              IF(KN.EQ.1) THEN
               IFACC=KPDSF(15)
               DO IJ=1,KF
                IF (LFCI(ij).AND.FCI(IJ).GT.0.0)
     &           SUMFC(IJ)=FCI(IJ)*FCTR*FMRF
               ENDDO
              ELSEIF (kn.eq.inumf .and. ifhr1.ge.0) then
               IFACC=IFACC-KPDSF(15)
               DO IJ=1,KF
                IF (LFCI(IJ).AND.FCI(IJ).GT.0.0)
     &           SUMFC(IJ)=SUMFC(IJ)-FCI(IJ)*FCTR*FMRF
               ENDDO
              ENDIF

             ELSE

              DO IJ=1,KF
               IF (LFCI(IJ).AND.FCI(IJ).GT.0.0) 
     &          SUMFC(IJ)=SUMFC(IJ)+FCI(IJ)*FCTR*FMRF
              ENDDO
              IFACC=IFACC+KPDSF(15)-KPDSF(14)

             ENDIF
            ENDIF
           ENDIF
          ENDDO
!
!  if we've found enough fcst precip to match the obs acc period...
!
          WRITE (6,*) 'IFACC,IACC=',IFACC,IACC
          IF (IFACC.EQ.IACC) THEN
           IF (LPCPDA) THEN
!
!   DO NATIVE GRID ANALYSIS AND VERIFICATION
!
            IF (FIRST) THEN
             IMF   = KGDSF(2)
             JMF   = KGDSF(3)
             IMJMF = IMF*JMF
             IF (KGDSF(1).EQ.201) THEN
              IMF   = KGDSF(7)
              JMF   = KGDSF(8)
              IMJMF = IMF*JMF-JMF/2
             ENDIF
             IGRIDF = KPDSF(3)
             IGMDL  = KPDSF(3)
             REWIND(LPCDAT)
!--------+---------+---------+---------+---------+---------+---------+---------+
             CALL ANNATV(LPCDAT,IMF,JMF,NC,IGRIDF,KPDS1,
     +                   KGDSF,ANANAT,MASKNAT)
             III=0
             DO KNA=1,IMJMF
              IF (MASKNAT(KNA).EQ.1) THEN
               LNAT(KNA)=.TRUE.
c              WRITE(*,'(i5,f10.3)') KNA,ANANAT(KNA)
               III=III+1
              ELSE
               LNAT(KNA)=.FALSE.
              ENDIF
             ENDDO
!
!   WRITE OUT THE OBSERVATION ANALYSIS AT EACH GRID POINTS
!
             WRITE(LOANA) (MASKNAT(KNA),KNA=1,IMF*JMF)
             WRITE(LOANA) (ANANAT(KNA),KNA=1,IMF*JMF)

             WRITE(6,*) 'NUM. OF PTS IN NAT GRID ANALYSIS = ',III
             IF(MDLNAM.EQ.'eta'.OR.MDLNAM.EQ.'meso') THEN
              REWIND(LPCDAT)
              CALL ANNATV(LPCDAT,92,141,NC,90,
     +                    KPDS1,KGDS80,ANAN80,MASK80)
              DO KNA=1,12902
               IF (MASK80(KNA).EQ.1) THEN
                l80(KNA)=.TRUE.
               ELSE
                l80(KNA)=.FALSE.
               ENDIF
              ENDDO
             ENDIF
             FIRST=.FALSE.
            ENDIF
            CHKSUM=0.
            DO KNA=1,IMJMF
             IF(LNAT(KNA)) CHKSUM=CHKSUM+SUMFC(KNA)
            ENDDO

            WRITE(6,*) 'CHKSUM= ',CHKSUM
            IF (CHKSUM.GT.0.) THEN
             CALL VERF(SUMFC,ANANAT,LFCI,LNAT,MASKNAT,IMJMF,1,8,THRESH2,
     .                 2,IGRIDF,IGMDL,1,FMASK,ISYR,ISMN,ISDA,ISHR,
     .                 IVYR,IVMN,IVDA,IVHR,KTIME,IACC,MDLNAM,LSTOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     FCST    - FORECAST GRID                                             C
C     ANA     - ANALYSIS GRID                                             C
C     LFC     - BIT MAP FOR FORECAST GRID                                 C
C     LAN     - BIT MAP FOR ANALYSIS GRID TO DETERMINE VERF DOMAIN        C
C     MASKREG - INTEGER MAP FOR TO DETERMINE REGIONAL VERF DOMAINS        C
C     JO      - SIZE OF FCST,ANA,LFC,LAN                                  C
C     NUMREG  - NUMBER OF REGIONS                                         C
C     NUMTHR  - NUMBER OF THRESHOLDS                                      C
C     THRESH  - VERIFICATION THRESHOLDS                                   C
C     TNAME   - STRING VERSION OF THESHOLDS                               C
C     IGRID   - GRID NUMBER                                               C
C     IANATYP - ANALYSIS TYPE (1=NATV, 2=MPCP)                            C
C     FMASK   - NAMES OF THE REGIONAL MASKS                               C
C     IYR     - YEAR                                                      C
C     IMN     - MONTH                                                     C
C     IDA     - DAY                                                       C
C     IHR     - HOUR                                                      C
C     IFHR    - FCST HOUR                                                 C
C     IACC    - LENGTH OF ACCUMULATION                                    C
C     MDLNAM  - NAME OF MODEL                                             C
C     IOUNIT  - UNIT TO WRITE STATS OUT TO                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            ENDIF
           ENDIF
 
!  put this on the filled grid if unfilled
!
           if (KGDSF(1).eq.201) THEN
            call ipxetas(1,ji,ji,1,KGDSF,sumfc,KGDSF2,fc2,IRET)
            IMF2=KGDSF2(7) 
            JMF2=KGDSF2(8) 
            kf=IMF2*JMF2
            if(IRET.ne.0) call exit(IRET)
            lfci=.TRUE.
           else
            do ij=1,kf
             fc2(ij)=sumfc(ij)
            ENDDO
            KGDSF2=KGDSF
           ENDIF
 
           amax=0.
           do ij=1,kf
            if (fc2(ij).gt.amax) amax=fc2(ij)
           ENDDO
          IF (amax.gt.0.) THEN
 
!  INTERPOLATE PRECIP FORECAST TO ANALYSIS GRID ( Default: 211 grid )
!
           DO K=1,NC
            KGDSO(K)=0
           ENDDO
           CALL MAKGDS(IG,KGDSO,GDSO,LENGDS,IRET)
           if(IRET.ne.0) call exit(IRET)
           IPOPT(1)=-1
           IPOPT(2)=-1
           CALL IPOLATES(6,IPOPT,KGDSF2,KGDSO,ji,jo,1,1,lfci,fc2,
     .                   ko,rlat,rlon,ibo,lfco,fco,IRET)
           if(IRET.ne.0) CALL exit(IRET)
 
           AMAXX=0.
           print *, 'KO=',KO
           DO IJ=1,KO
            IF (FCO(IJ).GT.AMAXX) AMAXX=FCO(IJ)
           ENDDO
 
!  MAIN VERIFICATION PROGRAM CALL
!
            IGMDL=KPDSF(3)
            CALL VERF(FCO,ANA,LFCO,LANA,MASKREG,KO,NUMREG,NUMTHR,THRESH,
     .                1,ig,igmdl,2,fmask,isyr,ismn,isda,ishr,
     .                ivyr,ivmn,ivda,ivhr,ktime,iacc,MDLNAM,LSTOUT)
           ENDIF
          ENDIF
         ENDIF
        ENDDO
 9100  CONTINUE
       ENDDO    ! DO KTIME=IACC,IFDUR,IFREQ2
!
!  READ NEXT MODEL NAME  
!
      READ(MDLINF,'(a)',END=99) MDLNAM
      ENDDO    ! DO WHILE (trim(MDLNAM).NE.'done')

 9000 STOP
      END

