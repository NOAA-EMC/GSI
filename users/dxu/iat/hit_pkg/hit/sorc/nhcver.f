      PROGRAM TRUTH
C
C**   THIS PROGRAM CALCULATES THE ERRORS FROM A GIVEN SET OF
C**   TRACK FORECAST MODELS USING DATA FROM THE AALNNYR.DAT AND
C**   BALNNYR.DAT FILES.  IT ALSO YIELDS THE AVERAGE ERRORS COMPARED
C**   TO ANY OTHER MODEL.  THIS VERSION IS MODIFIED TO INCLUDE INTENSITY
C**   VERIFICATION.
C
C**   JAMES GROSS DEVELOPED THIS CODE WHICH IS BASED ON THE PROGRAM
C**   QKVER ORIGINATED BY MARK DEMARIA
C
C**   12/1/97
C
C***********************************************************************
C
C**   THIS VERSION WAS MODIFIED FOR JIM GROSS ON 1/8/92.  THE UNIRAS
C**   GRAPHICS WAS REMOVED AND THE DIRECTORY INFORMATION FROM THE
C**   ATCF INPUT FILES WAS REMOVED.
C
C**   THIS VERSION OF QKVER WAS MODIFIED BY JIM GROSS TO RUN ON THE NAS
C**     COMPUTER 1/23/92
C
C**   THIS VERSION OF TRUTH WAS MODIFIED BY JIM GROSS TO RUN ON A PC AND
C**     A WORKSTATION USING A INPUT FILE TO DETERMINE THE RUN PARAMETERS
C**     AND THE STORMS ON WHICH TO RUN  -- 10/19/93
C
C**   PROGRAM CHANGES TO USE HP FEATURE OF INPUTING FLNAME NAME FROM THE
C**     COMMAND LINE.  ALSO MADE OUTPUT FILE NAME RELATE TO INPUT FLIE
C**     NAME.  -- 11/07/94
C
C**   ADDED THE INITIAL POSITION ERROR AND THE PRODUCTION OF A HARVARD
C**     GRAPHICS FILE FOR PROCESSING.  ALSO MADE THE CLIPER FORMULATION
C**     A SEPARATE SUBROUTINE -- 9/25/95
C
C**     GIVE CREDIT TO ALL MODELS IN CASE OF TIE FOR "SUPERIOR PERFORMANCE"
C**     SAMPSON, NRL 3/15/01               
C
C**     CONVERTED TO RUN IN ATCF3.5 OFF CONFIGURATION FILE SUPPLIED BY GUI 
C**     SAMPSON, NRL 4/10/01               
C
C**     INCLUDED MORE NHC IMPROVEMENTS AS PER JIM GROSS
C**     SAMPSON, NRL 5/01/01               
C
C**     MORE NHC IMPROVEMENTS AS PER JIM GROSS
C**     SAMPSON, NRL 1/15/02               
C
c**   ADDED CODE TO VERIFY QUADRANT WIND RADII ala TIM MARCHOK -- Sampson 6/3/04
c**   ALSO ADDED Marchok code to verify intensity change.  With
c          this new code, you will find the following new arrays:
c          ften - stores the forecast 12h (or 24h) intensity changes
c                 (i.e., 12-24h, 24-36h, 36-48h and 48-72h changes).
c          ftenhh - stores the forecast intensity change for 00-hh,
c                   where hh = 12, 24, 36, 48, 72, 96 and 120h.
c          vten - stores the verif 12h (or 24h) intensity changes
c                 (i.e., 12-24h, 24-36h, 36-48h, 48-72h, etc. changes).
c          vtenhh - stores the verif intensity change for 00-hh,
c                   where hh = 12, 24, 36, 48, 72, 96 and 120h.
C
C**     DEVELOPMENT LEVEL SCREENING           
C**     SAMPSON, NRL 2/15/04               
C
C**     Intensity change threshold (icincr) assigned in readin
C**     SAMPSON, NRL 6/15/05               

C
C**   0:7 (00, 12, 24, 36, 48, 72, 96, 120) THE MAXIMUM NUMBER OF 
c**       FORECAST PERIODS
C
C**   MXNMD IS THE MAXIMUM NUMBER OF FORECAST MODELS
C**   MXNST IS THE MAXIMUM NUMBER OF STORMS
C**   MXCS  IS THE MAXIMUM NUMBER OF CASES
C**   MXCSS IS THE MAXIMUM NUMBER OF CASES PER STORM
c**   mtau  is the maximum number of forecast periods per DTG (max = 7)
c
      include 'dataformats.inc'
C
cx    These should be adjusted higher if more data to be analyzed.
cx    Watch out for memory problems if set too high.
cx    PARAMETER (MXNMD=10,MXNST=212,MXCS= 500,MXCSS=100,mtau=7,
cx   & mtaup1=mtau + 1)
      PARAMETER (MXNMD=15,MXNST=300,MXCS= 5000,MXCSS=100,mtau=7,
     & mtaup1=mtau + 1)
      parameter (mxquad=16,MXMDTI=MXNMD*mtaup1)
      parameter (mxcsti=mxcs*mtaup1)
      parameter (mxdev=20)

C
      PARAMETER (MXDTI=MXNMD*MXCS*mtaup1,MXPBI=MXNMD*MXNMD*mtaup1)
C
      REAL BTLAT(MXCS,0:mtau),BTLON(MXCS,0:mtau),BTVMAX(MXCS,0:mtau)
cx
      character*1  btns(MXCS,0:mtau),BTew(MXCS,0:mtau)
      character*1  ns,ew
      REAL BHLAT(MXCS),BHLON(MXCS),BHVMAX(MXCS)
      REAL XLON(MXNMD,MXCS,0:mtau),YLAT(MXNMD,MXCS,0:mtau),
     & VMAX(MXNMD,MXCS,0:mtau)
      REAL ERR(MXNMD,MXCS,0:mtau),XBIAS(MXNMD,MXCS,0:mtau),
     & YBIAS(MXNMD,MXCS,0:mtau)
      REAL ERRM(MXNMD,0:mtau),STDEV(MXNMD,0:mtau),XBIASM(MXNMD,0:mtau),
     & YBIASM(MXNMD,0:mtau),RECLIP(MXNMD,MXNMD,0:mtau)
      REAL FSP(MXNMD,0:mtau),ERRS(MXNMD,MXCS,0:mtau),ERROR(MXNMD,0:mtau)
      REAL PROB(MXNMD,MXNMD,0:mtau),PROBA(MXNMD,MXNMD,0:mtau),
     & RNHAA(0:mtau)
      real ften(mxnmd,mxcs,0:mtau),ftenhh(mxnmd,mxcs,0:mtau)
      real vten(mxcs,0:mtau),vtenhh(mxcs,0:mtau)
      real icver(mxnmd,0:mtau),icverhh(mxnmd,0:mtau)
      real fcorr(mxcs),fcorrhh(mxcs),vcorr(mxcs),vcorrhh(mxcs)
      real R2(mxnmd,0:mtau),R2hh(mxnmd,0:mtau)

      REAL*8 TDF,TSTAT,TSTATA,RNH,RNHA
      real flat(0:mtau),flon(0:mtau),fvmax(0:mtau)
      real, allocatable :: qrad(:,:,:,:),bqrad(:,:,:)
      real, allocatable :: dr(:,:,:,:),drm(:,:,:)
      real, allocatable :: drs(:,:,:,:),rerror(:,:,:)
      real  temprad(mxquad),rbiasm(mxnmd,0:mtau,mxquad)
      real  tempdr,icincr
      real, allocatable :: quadtot(:,:,:)

C
      EQUIVALENCE (YBIAS,ERRS),(YBIASM,ERROR)
C
      INTEGER NDMO(12),NHCASE(0:mtau),IDEL(0:mtau),NFSP(MXNMD,0:mtau),
     & ICASECT(0:mtau),KTIME(0:mtau),NDMOS(12),nfcase(0:mtau)
      INTEGER GETARG, result
      integer quadctm(mxnmd,0:mtau,mxquad),podm(mxnmd,0:mtau,mxquad,4)
      integer iqtot(mxquad),ipct(mxquad),irerct(mxnmd,0:mtau,mxquad)
      integer itemprad(mxquad),compmod(0:2,0:mtau)
      integer ictot(mxnmd,0:mtau),ictothh(mxnmd,0:mtau)
      integer nqrmodel,iccheck

C
      LOGICAL INT00,INT06,INT12,INT18
      logical screendev
C
      CHARACTER*1   FRCSTR,qradalso,dtg_found,tau_found
      CHARACTER*2   CENT,btype,bttype(mxcs,0:mtau)
      CHARACTER*3   FCSTPD(0:7)
      CHARACTER*4   MNAME(MXNMD),MNAMEF
      CHARACTER*2   dname(mxdev)
      character*8   quades(mxquad)
      CHARACTER*8   FNAME(MXCS),FHNAME(MXCS),FNNEW,FNOLD
      CHARACTER*8   STRMID,flname(mxnst)
      CHARACTER*10  CDATE,BDATE,CCDATE(MXCS),CHDATE(MXCS)
      CHARACTER*10  dtgnext,dtgcur,dtglast,btdtg,dtgcheck
      CHARACTER*10  SFNAME(MXNST),SNAME(MXCS),SHNAME(MXCS)
      CHARACTER*18  STRMPATH
cx    CHARACTER*13  FILEA,FILEB
cx    CHARACTER*25  INPUT
cx    CHARACTER*30  OUTPUT
      character*120 filea,fileb
      character*120 input
      character*120 output
      character*124 outtxt
      character*124 outhvd
      character*120 storms
      character*200 bline
      integer       ninput
      integer       noutpt
      integer       nstdir
      integer       iwind
      integer       iav34, iav50, iav64
      integer       nav34, nav50, nav64
      character*10  startdtg
      character*10  checkdtg
      character*10  enddtg
      integer       ind
      integer       nmodel, ndevs, idev
      character*2   unitch(2)
C
      type ( BIG_AID_DATA ) aidsData
      type ( AID_DATA )     aidData, tauData
c
      DATA IERRPR    /0/
      DATA ISTMPR    /1/
      DATA ITRKPR    /0/
      DATA LULG,LUHR,LUAA,LUBA,LUHV  /31,32,21,22,150/
      DATA NCASE     /0/
      DATA NDMO      /31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31/
      DATA NDMOS     / 0,31,59,90,120,151,181,212,243,273,304,334/
      DATA NHCASE    /mtaup1*0/
      DATA PI        /3.141593/
cx  no longer required, set in subroutine ... sampson
cx    DATA RLATHI    /  90.0/
cx    DATA RLATLO    / -90.0/
cx    DATA RLONHI    / 180.0/
cx    DATA RLONLO    /-180.0/
cx    DATA VMHI      /999.0/
cx    DATA VMLO      /34.0/
cx    DATA VMVER     /34.0/
cx    DATA VMVER1    /250.0/
cx    DATA INTENS    /0/
      DATA XLON      /MXDTI*-99.0/
      DATA YLAT      /MXDTI*-99.0/
      DATA VMAX      /MXDTI*-99.0/
      DATA SAMADJ    / 30.0/
      data icver     /mxmdti*0.0/
      data icverhh   /mxmdti*0.0/
      data ictot     /mxmdti*0/
      data ictothh   /mxmdti*0/
      data ften      /mxdti*-99.0/
      data vten      /mxcsti*-99.0/
      data ftenhh    /mxdti*-99.0/
      data vtenhh    /mxcsti*-99.0/
      data R2        /mxmdti*0.0/
      data R2hh      /mxmdti*0.0/
C
      DATA KTIME     /0,12,24,36,48,72,96,120/
      DATA FCSTPD    /' 00',' 12',' 24',' 36',' 48',' 72',' 96','120'/
      DATA PROB,PROBA/MXPBI*0.0,MXPBI*0.0/
      data unitch    /'NM','KM'/
      data quades    / ' 34KT NE', ' 34KT SE', ' 34KT SW', ' 34KT NW',
     &                 ' 50KT NE', ' 50KT SE', ' 50KT SW', ' 50KT NW',
     &                 ' 64KT NE', ' 64KT SE', ' 64KT SW', ' 64KT NW',
     &                 '100KT NE', '100KT SE', '100KT SW', '100KT NW'/
C
C**   SET VMVER TO RESTRICT CASES TO THOSE WITH BEST TRACK MAX WINDS
C**     GREATER THAN VMVER (KTS). ENTIRE CASE IS REMOVED IF INITIAL
C**     VMAX IS .LE. VMVER. FORECAST CASE IS REMOVED IF VMAX AT FORECAST
C**     TIME IS .LE. VMVER. THIS PARAMETER IS USEFUL FOR ELIMINATING ALL
C**     DEPRESSION CASES.
cxx   set vmver1 to restrict cases to max winds less than vmver1 (kts).
cxx     similar to vmver. this parameter would be useful for eliminating
cxx     all hurricane or typhoon cases.
C**
C**   SET VMLO TO RESTRICT CASES WITH INITIAL VMAX .GT. VMLO.
C**   SET VMHI TO RESRTICT CASES WITH INITIAL VMAX .LT. VMHI.
C**
C**   SET RLATLO, RLATHI FOR LATITUDE  SIMILAR TO VMLO, VMHI
C**   SET RLONLO, RLONHI FOR LONGITUDE SIMILAR TO VMLO, VMHI
C**
C**   SET IERRPR=1 TO PRINT ALL ERRORS FOR HOMOGENEOUS SAMPLE  (ELSE=0)
C**
C**   SET ISTMPR=1 TO PRINT STORM ERRORS FOR HOMOGENEOUS SAMPLE  (ELSE=0)
C**
C**   SET ITRKPR=1 TO PRINT TRACKS (OR INTENSITY) OF ALL CASES (ELSE=0)
C**
C**   SET INTENS =0 FOR TRACK OR =1 FOR INTENSITY VERIFICATION
C
CC    NAMELIST/INFO/NDATAF,SFNAME,NMODEL,MNUMC,MNAME,VMVER,VMLO,VMHI,
CC   &              RLATLO,RLATHI,RLONLO,RLONHI,ITRKPR,IERRPR,IYR,
CC   &              INT00,INT06,INT12,INT18,
CC   &              SAMADJ,INTENS,INCFSP
C
C**   INITIALIZE VALUES
C
      DTR = PI/180.0
C
cx    RLATLO =  -90.0
cx    RLATHI =   90.0
cx    RLONLO = -180.0
cx    RLONHI =  180.0
C
      INT00 = .TRUE.
      INT06 = .TRUE.
      INT12 = .TRUE.
      INT18 = .TRUE.
C
      VMHI  = 999.0
C
C**   CHECK NUMBER OF COMMAND LINE ARGUMENTS AND READ ONE ARGUMENT
C
cx    NARG = IARGC()
cx    IF (NARG.NE.1) THEN
cx       STOP ' PROGRAM REQUIRES INPUT FILE NAME ON THE COMMAND LINE'
cx    ENDIF
      narg = iargc()
      if (narg.lt.2) then
       print *, ' '
       print *, 'Requires three arguments (input, output, storms dir)'
       print *, 'nhcver.x input_file output_file dir_of_adecks_&_bdecks'
       stop 
      endif
C
      NINPUT = GETARG(1,INPUT)
      NOUTPT = GETARG(2,OUTPUT)
      nstdir = 0
      storms = ''
      nstdir = getarg(3,storms)
      print *,'at AAA'

      NINPUT = len_trim(INPUT)
      NOUTPT = len_trim(OUTPUT)
      nstdir = len_trim(storms)

      print *,'INPUT= ',INPUT
      print *,'OUTPUT= ',OUTPUT
      print *,'storms= ',storms
      print *,'nstdir= ',nstdir
      print *,'NINPUT= ',NINPUT
      print *,'NOUTPT= ',NOUTPT
C
cx    OUTPUT = INPUT(1:NINPUT)//'.out'
cx    OPEN (UNIT=LULG,FILE=OUTPUT,STATUS='UNKNOWN',IOSTAT=IOS,ERR=1020)
      outtxt = trim(output(1:noutpt))//'.txt'
      print *,'outtxt= ',outtxt
      open (unit=lulg,file=outtxt,status='unknown',iostat=ios,err=1020)
      print *,'at A'
C
C**   OPEN THE INPUT CONTROL FILE, CREATE THE OUTPUT FILE NAMES AND OPEN
C
      OPEN (11,FILE=INPUT,STATUS='OLD',IOSTAT=IOS,ERR=1010)
      print *,'at B'
C
cx    OUTPUT = INPUT(1:NINPUT)//'.hvd'
cx    OPEN (UNIT=LUHV,FILE=OUTPUT,STATUS='UNKNOWN',IOSTAT=IOS,ERR=1020)
      outhvd = trim(output(1:noutpt))//'.hvd'
      print *,'outhvd= ',outhvd
      print *,'luhv= ',luhv
      open (unit=luhv,file=outhvd,status='unknown',iostat=ios,err=1020)
      print *,'at C'
C
C**   READ IN THE CONTROL PARAMETERS
C
cx    READ (11,'(12X,I1,15X,I1,19X,I1)') IERRPR,ISTMPR,ITRKPR
cx    WRITE (*,'('' IERRPR, ISTMPR ,ITRKPR = '',3I5)')
cx   & IERRPR,ISTMPR,ITRKPR
cx    READ (11,'(12X,I1,2(15X,F5.1))') INTENS,VMLO,VMVER
cx    WRITE (*,'('' INTENS, VMAXLO, VMAXVER = '',I5,2F5.1)')
cx   & INTENS,VMLO,VMVER
C
C**   READ THE NUMBER OF MODELS AND MODEL IDS TO VERIFY
C
cx    READ (11,'(11X,I2,12((1X,A4)))') NMODEL,(MNAME(I), I = 1,NMODEL)
cx    WRITE (*,'('' NMODEL = '',I5)') NMODEL
C
CC    INTENS = 0
CC    INTENS = 1
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx
cx  read the data at the top of the input file, individual storms (bottom)
cx  are read in loop below
cx
      call readin ( lulg, mxnmd, mxdev, intens, metric, ierrpr, istmpr, 
     &     itrkpr,vmhi, vmlo, vmver1, vmver, icincr,
     &     rlathi, rlatlo, rlonlo, rlonhi, startdtg, enddtg, 
     &     int00, int06, int12, int18, ndevs, dname, nmodel, mname )
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx

cx  new wind radii eval
      if (intens .eq. 2) then
        qradalso = 'y'
        allocate (bqrad(mxcs,0:mtau,mxquad),stat=ibq)
        allocate (qrad(mxnmd,mxcs,0:mtau,mxquad),stat=iqr)
        allocate (dr(mxnmd,mxcs,0:mtau,mxquad),stat=idr)
        allocate (drm(mxnmd,0:mtau,mxquad),stat=idm)
        allocate (drs(mxnmd,mxcs,0:mtau,mxquad),stat=ids)
        allocate (rerror(mxnmd,0:mtau,mxquad),stat=irr)
        allocate (quadtot(mxnmd,0:mtau,mxquad),stat=idm)

        if (ibq /= 0 .or. iqr /= 0 .or. idr /= 0 .or.
     &      idm /= 0 .or. ids /= 0 .or. irr /= 0) then
          print *,' '
          print *,'!!! ERROR allocating quadrant radii arrays:'
          print *,'!!! ibq = ',ibq,' iqr= ',iqr,' idr= ',idr
          print *,'!!! idm = ',idm,' ids= ',ids,' irr= ',irr
          stop 99
        endif
        bqrad  = -99.0
        rbiasm =   0.0
        rerror =   0.0
        drm    =   0.0
        quadtot=   0.0
      else
        qradalso = 'n'
      endif

C
C**   SET INCFSP=1 TO INCLUDE NO CHANGE FORECAST IN FREQUENCY OF
C**   SUPERIOR PERFORMANCE FOR INTENSITY VERIFICATION
C
      INCFSP = 0
C
CC    IERRPR = 1
CC    IERRPR = 0
CC    ISTMPR = 1
CC    ISTMPR = 0
CC    ITRKPR = 1
CC    ITRKPR = 0
C
CC    VMVER =   0.0
CC    VMVER =  34.0
CC    VMLO  =   0.0
CC    VMLO  =  34.0
C
C**   SET TIME IN HOURS FOR NO SERIAL CORRELATION. USED FOR
C**   DETERMINING ADJUSTED SAMPLE SIZE FOR SIGNIFICANCE TESTING
C
      SAMADJ = 30.0
C
C**   DO A BEST TRACK CLIPER OR NOT?
C
cfy   IXCLIP = 0
      IXCLIP = 1
C
C**   READ THE STORM IDS FROM THE INPUT FILE
C
      II = 0
   10 READ (11,'(A)',END=100) STRMID
      WRITE (*,'('' STRMID = '',A)') STRMID
      II = II + 1
C
      NTIME = 0
      NCASEI = NCASE + 1
C
C**   Create the adeck and bdeck file names for the storm
C
      FILEA = 'a'//STRMID//'.dat'
      FILEB = 'b'//STRMID//'.dat'
      if (nstdir.gt.0) then
	  filea = storms(1:nstdir)//'/a'//strmid//'.dat'
	  fileb = storms(1:nstdir)//'/b'//strmid//'.dat'
      endif
      print *,'storms= ',storms
      print *,'nstdir= ',nstdir
      print *,'storms(1:nstdir)= ',storms(1:nstdir)
      
C  
cx    WRITE (*,'('' FILEA, FILEB = '',A16,1X,A16)') FILEA,FILEB
      write (*,*)'adeck=', filea
      write (*,*)'bdeck=', fileb
      
cx    write storms attempted to output
      write (lulg, * ) 'using ', strmid
      
C
C**   OPEN A-DECK AND B-DECK FOR A PARTICULAR STORM
C
      OPEN (LUAA,FILE=FILEA,STATUS='OLD',IOSTAT=IOS,ERR=1030)
      OPEN (LUBA,FILE=FILEB,STATUS='OLD',IOSTAT=IOS,ERR=1040)
C
C**   Read the first and last DTG from the storm's a-deck
C
cx  changed to prevent endless loop ... sampson 2/19/02
      n = 0
      dtgcur = '1945010100'
   15 continue
      n = n + 1
      read ( luaa, '( 8x, a10 )', end=17 ) dtgcheck
      call dtgdif2(dtgcheck,dtgcur,ihrs,istat)
      if ( istat .ne. 0 ) then
	 go to 1055
      else if( n .eq. 1 ) then
	 dtgcur = dtgcheck
         goto 15
      else if( ihrs .gt. 2000 ) then
	 go to 1056
      else
	 dtglast = dtgcheck
         goto 15
      endif
c
   17 rewind ( luaa )
c
   20 write ( *, '( ''Searching for DTG = '', a10, '' last DTG = '',
     & a10 )') dtgcur, dtglast
c
c**   Read in all the aids associated with that DTG or the next DTG until
c**     end-of-file is reached
c
      call getBigAidDTG ( luaa, dtgcur, aidsData, result )
      if ( result .eq. 0 ) then
         if ( dtgcur .gt. dtglast ) then
	    goto 50
         else
	    goto 45
         endif
      endif
c
c**   Process the model initial conditions from the TAU = 0 CARQ card
c 
      call getTech ( aidsData, "CARQ", aidData, result )
      if ( result .eq. 0 ) goto 45
c
      call getSingleTAU ( aidData, 0, tauData, result )
      if ( result .eq. 0 ) goto 45
c
      cdate = tauData%aRecord(1)%DTG
C
C**   Check the synoptic hour
C
      READ ( cdate, '(8x,i2)' ) ITM
c
      if (itm.eq. 0.and..not.int00) goto 45
      if (itm.eq. 6.and..not.int06) goto 45
      if (itm.eq.12.and..not.int12) goto 45
      if (itm.eq.18.and..not.int18) goto 45
c
      flat(0)  = tauData%aRecord(1)%lat
      flon(0)  = tauData%aRecord(1)%lon
      fvmax(0) = tauData%aRecord(1)%vmax
c
      PRINT * , ' CENTER INFO = ', dtgcur, fLAT(0),fLON(0),fVMAX(0)
c
      FLNAME(II) = strmid
c
      NCASE = NCASE + 1
cx    stop the program if NCASE goes beyond MXCS (array problems)
      if (ncase .gt. mxcs) then
           print *, 'Maximum number of cases (',mxcs,') exceeded'
           stop ' TRUTH PROGRAM ABORTED'
      endif
	      
      NTIME = NTIME + 1
c
      WRITE (*,'('' NCASE = '',I5,'' NTIME = '',I5,'' NSTORMS = '',
     & I5,'' ID = '',A8)') NCASE, NTIME, II, FLNAME(II)
c
      CCDATE(NCASE) = CDATE
      FNAME(NCASE)  = FLNAME(II)
      SFNAME(II)    = tauData%aRecord(1)%stormname
      SNAME(NCASE)  = SFNAME(II)
C
C**   PROCESS THE AIDS for the models read in
C
      DO 40 JJ = 1,NMODEL
C
         call getTech ( aidsData, mname(jj), aidData, result )
         if ( result .eq. 0 ) go to 40

         ! This next bit of junk is all rigged up to get the radii
         ! for the OFCL forecast.  The current position & intensity
         ! are listed under hour 0, but the "current" radii are
         ! listed under hour 3.

cx       if (mname(jj) == 'OFCL' .and. intens == 1 .and.
cx   &       qradalso == 'y') then
         if (mname(jj) == 'OFCL' .and. qradalso == 'y') then
           init_tau = 3
         else
           init_tau = 0
         endif
         ! End bit of junk
c
         call getSingleTAU ( aidData, init_tau, tauData, result )
c
c**   See if the model has a TAU = 0 entry.  If it does, use it,
c**    otherwise use the initial position from the CARQ card.
c
         if ( result .eq. 1 ) then
c
            XLON(JJ,NCASE,0) = tauData%aRecord(1)%lon
            IF (XLON(JJ,NCASE,0).LE.0.0) XLON(JJ,NCASE,0) = -99.0
C
            YLAT(JJ,NCASE,0) = tauData%aRecord(1)%lat
            IF (YLAT(JJ,NCASE,0).LE.0.0) YLAT(JJ,NCASE,0) = -99.0
C
            VMAX(JJ,NCASE,0) = tauData%aRecord(1)%vmax
            IF (VMAX(JJ,NCASE,0).LE.0.0) VMAX(JJ,NCASE,0) = -99.0

            if (qradalso == 'y') then
              call load_radii (tauData,temprad,mxquad,loadret)
              if (loadret == 0) then
                do ir = 1,mxquad
                  qrad(jj,NCASE,0,ir) = temprad(ir)
                enddo
              else
                do ir = 1,mxquad
                  qrad(jj,NCASE,0,ir) = -99.0
                enddo
              endif
            endif

c
         else   
c
            if (init_tau == 3) then
              print *,'0 lon= ',tauData%aRecord(1)%lon,' wc= '
     &               ,tauData%aRecord(1)%windcode,' numrec= '
     &               ,tauData%numrcrds
            endif

            XLON(JJ,NCASE,0) = flon(0)
            IF (XLON(JJ,NCASE,0).LE.0.0) XLON(JJ,NCASE,0) = -99.0
C
            YLAT(JJ,NCASE,0) = flat(0)
            IF (YLAT(JJ,NCASE,0).LE.0.0) YLAT(JJ,NCASE,0) = -99.0
C
            VMAX(JJ,NCASE,0) = fvmax(0)
            IF (VMAX(JJ,NCASE,0).LE.0.0) VMAX(JJ,NCASE,0) = -99.0
c
         endif   
c
c**      Obtain the 12, 24, 36, 48 and 72 hour forecast 
c**      information, if it exists. 
c
         do itime = 1, mtau
 
            call getSingleTAU ( aidData, ktime(itime), tauData, result )
 
            if ( result .eq. 1 ) then   
 
               XLON(JJ,NCASE,itime) = tauData%aRecord(1)%lon
               IF (XLON(JJ,NCASE,itime).LE.0.0) 
     &                       XLON(JJ,NCASE,itime) = -99.0
 
               YLAT(JJ,NCASE,itime) = tauData%aRecord(1)%lat
               IF (YLAT(JJ,NCASE,itime).LE.0.0) 
     &                       YLAT(JJ,NCASE,itime) = -99.0
 
               VMAX(JJ,NCASE,itime) = tauData%aRecord(1)%vmax
               IF (VMAX(JJ,NCASE,itime).LE.0.0) 
     &                       VMAX(JJ,NCASE,itime) = -99.0
               if (qradalso == 'y') then
                 call load_radii (tauData,temprad,mxquad,loadret)
                 if (loadret == 0) then
                   do ir = 1,mxquad
                     qrad(jj,NCASE,itime,ir) = temprad(ir)
                   enddo
                 else
                   do ir = 1,mxquad
                     qrad(jj,NCASE,itime,ir) = -99.0
                   enddo
                 endif
               endif
               if (vmax(JJ,NCASE,itime)   > 0.0 .and.
     &             vmax(JJ,NCASE,itime-1) > 0.0) then
                 ften(jj,ncase,itime) = vmax(JJ,NCASE,itime) -
     &                                  vmax(JJ,NCASE,itime-1)
               else
                 ften(jj,ncase,itime)   = -99.0
               endif

               if (vmax(JJ,NCASE,itime) > 0.0 .and.
     &             vmax(JJ,NCASE,0)     > 0.0) then
                 ftenhh(jj,ncase,itime) = vmax(JJ,NCASE,itime) -
     &                                    vmax(JJ,NCASE,0)
               else
                 ftenhh(jj,ncase,itime) = -99.0
               endif

	    else
 
	       XLON(JJ,NCASE,itime) = -99.0
 
	       YLAT(JJ,NCASE,itime) = -99.0
 
	       VMAX(JJ,NCASE,itime) = -99.0

               ften(jj,ncase,itime)   = -99.0
               ftenhh(jj,ncase,itime) = -99.0

 
            endif
 
          enddo
 
   40 continue
 
   45 continue

c
c**   Increment the current DTG by 6 hours and then read the next DTG block
c**    of forecasts
c 
      call dtgmod ( dtgcur, 6, dtgnext, result )
      dtgcur = dtgnext
c
      goto 20
c
C**   PROCESS THE A FORECASTER'S VERIFICATION
C
CC        IF (CLINE(1:2).EQ.'01'.AND.FRCSTR.NE.'J') GO TO 40
CC        IF (CLINE(1:2).EQ.'01'.AND.(FRCSTR.EQ.' '.OR.FRCSTR.EQ.'J'))
CC   &      GO TO 40
c
   50 CONTINUE
C
C**   PROCESS THE STORM'S B-DECK
C
      caseloop: do N = NCASEI,NCASE
C
         CDATE = CCDATE(N)
cx don't know about this one
         rewind (luaa )
c
         tauloop: do i = 0, mtau
c
            call dtgmod ( cdate, ktime(i), dtgnext, result )
c
            rewind ( luba )
c
cajs   Want all data access to use dataio.f routines.  ajs
c            blat  = -99.0
c            blon  = -99.0
c            bvmax = -99.0
c            iwind = -99
c
c   80       read ( luba, '(a)', end=83, iostat=ios, err=1060 ) bline
c
c            if ( bline(9:18) .ne. dtgnext ) goto 80
c
C**   INCLUDE THIS LINE IF YOU WANT THE VERIFICATION PERFORMED OVER
C**      A SPECIFIED PERIOD OF TIME (YYYYMMDDHR)
C
cc          IF ( bline(9:18) .lt. '2000080112' .OR. 
cc   &           bline(9:18) .gt. '2000093012' ) GO TO 80
cx    included check permanently ... sampson, nrl     
c	    if ( bline(13:18) .lt. startdtg(1:6) .or.
c     &           bline(13:18) .gt. enddtg(1:6) ) go to 80
c
c            read ( bline, '(35x,f3.1,a1,2x,f4.1,a1,2x,f3.0,8x,a2)' ) 
c     &	    blat,ns,blon,ew,bvmax,btype
cajs   Want all data access to use dataio.f routines.  ajs

   80        call doReadBT( luba,btdtg,blat,ns,blon,ew,iwind,btype,ios)
             if( ios .lt. 0 ) then
                blat  = -99.0
                blon  = -99.0
                bvmax = -99.0
                iwind = -99
                goto 83
             endif
             if( ios .gt. 0 ) goto 1060
             if( btdtg .ne. dtgnext ) goto 80
             if( btdtg(5:10) .lt. startdtg(1:6) .or.
     &            btdtg(5:10) .gt. enddtg(1:6) ) go to 80
c
   83       continue

cx          if (intens == 1 .and. qradalso == 'y') then
            if (qradalso == 'y') then
c
c**   If btrak winds were not found, don't bother with radii
c
              if (blat < -98.0) goto 85
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c
c
c**   Until we have best track radii, process the OFCL TAU 3 radii as
c**       best track radii
c
cxxxxxx       call getBigAidDTG ( luaa, dtgnext, aidsData, result )
cxxxxxx       if ( result .eq. 0 ) goto 85
c
cx  for the navy, it is the JTWC line
cxxxxxx       call getTech ( aidsData, "OFCL", aidData, result )
cxxxxxx       if ( result .eq. 0 ) then
cxxxxxx          call getTech ( aidsData, "JTWC", aidData, result )
cxxxxxx       endif
cxxxxxx       if ( result .eq. 0 ) goto 85
cxxxxxx
cxxxxxx       call getSingleTAU ( aidData, 3, tauData, result )
cxxxxxx       if ( result .eq. 0 ) then
cxxxxxx          call getSingleTAU ( aidData, 0, tauData, result )
cxxxxxx       endif
cxxxxxx       if ( result .eq. 0 ) goto 85
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

cx   new new new new new new new new new new new new new new new new new
              rewind ( luba )
              call getBigAidDTG ( luba, dtgnext, aidsData, result )
              if ( result .eq. 0 ) goto 85
              call getTech ( aidsData, "BEST", aidData, result )
              if ( result .eq. 0 ) goto 85
              call getSingleTAU ( aidData, 0, tauData, result )
              if ( result .eq. 0 ) goto 85
cx   new new new new new new new new new new new new new new new new new
 
              call load_radii ( tauData, temprad, mxquad, loadret )
 
              if ( loadret == 0 ) then
                do ir = 1,mxquad
                   bqrad( N, I, ir ) = temprad( ir )
                enddo
              else
                do ir = 1,mxquad
                   bqrad( N, I, ir ) = 0.0
                enddo
              endif
 
            endif
 
 85         continue

c
            BTLAT(N,I)  = BLAT
            BTLON(N,I)  = BLON
cajs            BTVMAX(N,I) = BVMAX
            BTVMAX(N,I) = float(iwind)
cx
	    btns(n,i)   = ns
	    btew(n,i)   = ew
	    bttype(n,i) = btype
            
cx   these checks must be done after the first go around
            if ( i .gt. 0) then
             if (BTVMAX(N,I) == -99.0 .or. BTVMAX(N,I-1) == -99.0) then
               vten(N,I)   = -99.0
             else
               vten(N,I) = BTVMAX(N,I) - BTVMAX(N,I-1)
             endif
c
             if (BTVMAX(N,I) == -99.0 .or. BTVMAX(N,0) == -99.0) then
               vtenhh(N,I) = -99.0
             else
               vtenhh(N,I) = BTVMAX(N,I) - BTVMAX(N,0)
             endif
            endif

         enddo tauloop
 
      enddo caseloop
 
      CLOSE ( LUAA )
      CLOSE ( LUBA )

c     This next "GO TO 10" loops all the way back up to the
c     beginning of this read stuff in order to read all the
c     forecast & verification decks for the next storm.
 
      GO TO 10
C
  100 NDATAF = II
C
C**   IF DESIRED, ADD ATLANTIC BEST TRACK CLIPER TO THE SET OF FORECASTS
C
cfy   IF (INTENS.EQ.0.AND.IXCLIP.EQ.1) THEN
C
cfy       NMODEL = NMODEL + 1
cfy       MNAME(NMODEL) = 'BCLP'
C
cc        CALL ABCLIP (BTLAT,BTLON,BTVMAX,XLON,YLAT,VMAX,CCDATE,FNAME,
cc   & MXCS,MXNMD,NCASE,NMODEL)
C
cfy   ENDIF
C
C**   ADD NO CHANGE FORECAST AS EXTRA MODEL FOR INTENSITY FORECASTS
c     Since no quadrant radii forecast records exist for NCHG, we must
c     keep the value of the number of models from being increased for
c     the quadrant radii verification part, thus the next statement

      nqrmodel = nmodel  ! save, since nchg not used for quad radii

      IF (INTENS.EQ.1.AND.IXCLIP.EQ.0) THEN
C
         NMODEL = NMODEL + 1
         MNAME(NMODEL) = 'NCHG'
C
         DO N = 1, NCASE
            DO K = 0, mtau
               VMAX(NMODEL,N,K) = BTVMAX(N,0)
               if (k > 0) then
                 ften(NMODEL,N,K)   = 0.0
                 ftenhh(NMODEL,N,K) = 0.0
               endif
            enddo   
         enddo
C
      ENDIF
C
C**   ADD 65KT FORECAST AS EXTRA MODEL FOR INTENSITY FORECASTS
c     Since no quadrant radii forecast records exist for 65KT, we must
c     keep the value of the number of models from being increased for
c     the quadrant radii verification part, thus the next statement

      nqrmodel = nmodel  ! save, since 65kt not used for quad radii

      IF (INTENS.EQ.1.AND.IXCLIP.EQ.0) THEN
C
         NMODEL = NMODEL + 1
         MNAME(NMODEL) = '65KT'
C
         DO N = 1, NCASE
            DO K = 0, mtau
               VMAX(NMODEL,N,K) = 65
               if (k > 0) then
                 ften(NMODEL,N,K)   = 0.0
                 ftenhh(NMODEL,N,K) = 0.0
               endif
            enddo   
         enddo
C
      ENDIF
C
C**   ZERO OUT THE MEAN ARRAYS
C
      DO K = 0, mtau
         DO J = 1, NMODEL
            ERRM(J,K)   = 0.0
            STDEV(J,K)  = 0.0
            XBIASM(J,K) = 0.0
            YBIASM(J,K) = 0.0
         enddo   
      enddo
C
C**   CALCULATE ERRORS FOR HOMOGENEOUS SAMPLE
C
      DO 260 N = 1, NCASE
c
cx       IF (bttype(n,0) .eq. 'EX'  ) go to 260
cx       IF (bttype(n,0) .eq. 'LO'  ) go to 260
cx       IF (bttype(n,0) .eq. 'WV'  ) go to 260
cc       IF (bttype(n,0) .eq. 'SS'  ) go to 260
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxxc
cx -------------- check development level of initial point ---
            screendev = .true.
            do 210 idev = 1, ndevs
cxcx temporary fix to set ST to TY because it isn't one of the current dev levels ... sampson 7/20/05
               if ( bttype(n,0) .eq. "ST") bttype(n,0) = "TY" 
               if ( bttype(n,0) .eq. dname(idev)) screendev = .false.
  210       continue
	    if ( screendev ) go to 260
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxxc
cjknaff  IF (BTVMAX(N,0) .LE. VMVER ) GO TO 260
cx   vmver1 is the upper restriction comparable to vmver
cjknaff  if (btvmax(n,0) .ge. vmver1) go to 260
         IF (BTVMAX(N,0) .LE. VMLO  ) GO TO 260
         IF (BTVMAX(N,0) .GE. VMHI  ) GO TO 260
cx   account for SH, WP and IO  ... sampson, nrl 
cx       IF (BTLAT(N,0)  .LE. RLATLO) GO TO 260
cx       IF (BTLAT(N,0)  .GE. RLATHI) GO TO 260
cx       IF (BTLON(N,0)  .LE. RLONLO) GO TO 260
cx       IF (BTLON(N,0)  .GE. RLONHI) GO TO 260
cx     
         if (btns(n,0) .eq. 'N') btlatchk =  btlat(n,0)
         if (btns(n,0) .eq. 'S') btlatchk = -btlat(n,0)
	 if (btew(n,0) .eq. 'W') btlonchk =  btlon(n,0)
	 if (btew(n,0) .eq. 'E') btlonchk =  360-btlon(n,0)
	 if (btlatchk  .le. rlatlo)   go to 260
	 if (btlatchk  .ge. rlathi)   go to 260
	 if (btlonchk  .le. rlonlo)   go to 260
	 if (btlonchk  .ge. rlonhi)   go to 260
	  
         IDELT = 0
C
         DO K = 0,mtau
            IDEL(K) = 0
         enddo
C
C**   WEED OUT MISSING CASES
C
         DO 230 K = 0, mtau
c
cx	    IF ( bttype(N,K) .eq. 'EX' ) THEN
cx		IDEL(K) = 1
cx		IDELT = IDELT + 1
cx 		 GO TO 230
cx          ENDIF
c
cx	    IF ( bttype(N,K) .eq. 'LO' ) THEN
cx		IDEL(K) = 1
cx		IDELT = IDELT + 1
cx		 GO TO 230
cx          ENDIF
c
cx	    IF ( bttype(N,K) .eq. 'WV' ) THEN
cx		IDEL(K) = 1
cx		IDELT = IDELT + 1
cx		 GO TO 230
cx          ENDIF
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxxc
cx -------------- check development level of forecast points ---
            screendev = .true.
            do 220 idev = 1, ndevs
cx -------------- bailing wire and duct tape for super typhoons in old records ... sampson july 05
               if ( bttype(n,k) .eq. "ST") bttype(n,k) = "TY" 
               if ( bttype(n,k) .eq. dname(idev)) screendev = .false.
  220       continue
	    if ( screendev ) then
		idel(k) = 1
		idelt = idelt + 1
		 go to 230
            endif
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxxc
c
cx          IF ( BTVMAX(N,K) .LE. VMVER ) THEN
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
            if ( btvmax(n,k).le.vmver .or. btvmax(n,k).ge.vmver1 ) then
                IDEL(K) = 1
                IDELT = IDELT + 1
                 GO TO 230
            ENDIF
C
            IF ( INTENS .EQ. 0 ) THEN
                IF ( BTLAT(N,K) .LE. 0.0 .OR.BTLON(N,K).LE.0.0) THEN
                    IDEL(K) = 1
                    IDELT = IDELT + 1
                    GO TO 230
                ENDIF
C
                DO J = 1, NMODEL
                   IF (XLON(J,N,K).LE.0.0.OR.YLAT(J,N,K).LE.0.0) THEN
                       IDEL(K) = 1
                       IDELT = IDELT + 1
                       GO TO 230
                   ENDIF
                enddo
            ELSE
                IF ( BTVMAX(N,K) .LE. 0.0 ) THEN
                    IDEL(K) = 1
                    IDELT = IDELT + 1
                    GO TO 230
                ENDIF

                if (qradalso == 'y') then
                  nqtot = 0
                  do iq = 1,mxquad
                    if (bqrad(N,K,iq) < -98.0) then
                      nqtot = nqtot + 1
                    endif
                  enddo
                  if (nqtot == mxquad) then
                    IDEL(K) = 1
                    IDELT = IDELT + 1
                    GO TO 230
                  endif
                endif
C
                DO J = 1, NMODEL
                   IF ( VMAX(J,N,K) .LE. 0.0 ) THEN
                       IDEL(K) = 1
                       IDELT = IDELT + 1
                       GO TO 230
                   ENDIF
               enddo
                if (qradalso == 'y') then
                  do j = 1,nqrmodel
                    do iq = 1,mxquad
                      if (qrad(j,N,K,iq) < -98.0) then
                        idel(k) = 1
                        idelt = idelt + 1
                        go to 230
                      endif
                    enddo
                  enddo
                endif

            ENDIF
  230    CONTINUE
C
         IF ( IDELT .GE. mtaup1 ) GO TO 260
C
C**   CASE HAS AT LEAST ONE VALID FORECAST TIME
C
         kloop: DO K = 0, mtau
C
            IF (IDEL(K).EQ.0) NHCASE(K) = NHCASE(K) + 1
C
            IF (K.EQ.0) THEN
                NH         = NHCASE(0)
                FHNAME(NH) = FNAME(N)
                SHNAME(NH) = SNAME(N)
                CHDATE(NH) = CCDATE(N)
                BHLAT(NH)  = BTLAT(N,0)
                BHLON(NH)  = BTLON(N,0)
                BHVMAX(NH) = BTVMAX(N,0)
            ENDIF
C
            jloop: DO J = 1,NMODEL
               IF ( IDEL(K) .EQ. 1 ) THEN
                  ERR(J,NH,K)   = 9999.0
                  XBIAS(J,NH,K) = 9999.0
                  YBIAS(J,NH,K) = 9999.0
                  if (qradalso == 'y' .and. j <= nqrmodel) then
                    do iq = 1,mxquad
                      dr(j,nh,k,iq) = 9999.0
                    enddo
                  endif
                  ften(J,N,K)   = -99.0
                  ftenhh(J,N,K) = -99.0
                  vten(N,K)     = -99.0
                  vtenhh(N,K)   = -99.0
               ELSE
                  IF ( INTENS .EQ. 0 ) THEN
                     CAVGL = COS(0.5*DTR*(BTLAT(N,K) + YLAT(J,N,K)))
cx
cx*   POSITION ERROR IN KILOMETERS
cx
cx                   DX =   111.0*(BTLON(N,K) - XLON(J,N,K))*CAVGL
cx                   DY = - 111.0*(BTLAT(N,K) - YLAT(J,N,K))
cx
cx*   POSITION ERROR IN NAUTICAL MILES
cx
cx                   DX =    60.0*(BTLON(N,K) - XLON(J,N,K))*CAVGL
cx                   DY = -  60.0*(BTLAT(N,K) - YLAT(J,N,K))
cx
cx*   POSITION ERROR IN KILOMETERS
cx
                     if ( metric .eq. 1 ) then
		        DX =   111.0*(BTLON(N,K) - XLON(J,N,K))*CAVGL
                        DY = - 111.0*(BTLAT(N,K) - YLAT(J,N,K))
C
C**   POSITION ERROR IN NAUTICAL MILES
C
		     else
                        DX =    60.0*(BTLON(N,K) - XLON(J,N,K))*CAVGL
                        DY = -  60.0*(BTLAT(N,K) - YLAT(J,N,K))
		     endif
C
                  ELSE
                     DX    = VMAX(J,N,K) - BTVMAX(N,K)
                     DY    = 0.0

                     if (K > 0) then
                       if (ften(J,N,K) /= -99.0 .and.
     &                     vten(N,K)   /= -99.0) then
                         icver(J,K) = icver(J,K)
     &                      + iccheck(vten(N,K),ften(J,N,K),icincr)
                         ictot(J,K) = ictot(J,K) + 1
                       endif
                       if (ftenhh(J,N,K) /= -99.0 .and.
     &                     vtenhh(N,K)   /= -99.0) then
                         icverhh(J,K) = icverhh(J,K)
     &                      + iccheck(vtenhh(N,K),ftenhh(J,N,K),icincr)
                         ictothh(J,K) = ictothh(J,K) + 1
                       endif
                     endif

                  ENDIF
C
                  RERR          = SQRT(DX*DX + DY*DY)
                  ERR(J,NH,K)   = RERR
                  XBIAS(J,NH,K) = DX
                  YBIAS(J,NH,K) = DY
                  ERRM(J,K)     = ERRM(J,K)   + RERR
                  STDEV(J,K)    = STDEV(J,K)  + RERR*RERR
                  XBIASM(J,K)   = XBIASM(J,K) + DX
                  YBIASM(J,K)   = YBIASM(J,K) + DY

c                 This next part gets the errors for the quadrant radii
c                 forecasts.  An important note is that many of the
c                 forecasts were poor and did not predict any winds
c                 beyond the thresholds in certain cases. So in those
c                 cases, it would be invalid to calculate the error, as
c                 you'd have the verification radius minus zero.  So
c                 we'll only calculate the error if there are both
c                 forecast and verification winds in a quadrant.  We
c                 will, however, keep track of POD and FAR stats.
c                 For the radius errors (dr), missing values are
c                 assigned as follows:
c                 9999 = No fcst winds, No verify winds (good forecast)
c                 6666 = Yes fcst winds, No verify winds (False Alarm)
c                 7777 = No fcst winds, Yes verify winds (Missed fcst)
c
c                 NOTE: The last index on the pod array is 4, with the
c                 4 spots being as follows:
c                  (1) Forecast Yes, Verify Yes
c                  (2) Forecast Yes, Verify No
c                  (3) Forecast No, Verify Yes
c                  (4) Forecast No, Verify No

                  if (qradalso == 'y' .and. j <= nqrmodel) then

                    quadloop: do iq = 1,mxquad

c                     For the case in which there is both a forecast
c                     radius and a verification radius (good forecast).
c                     Calculate the error and bias, and also update
c                     the pod and podm arrays.

                      if (qrad(j,N,K,iq) > 0.0  .and.
     &                     bqrad(N,K,iq) > 0.0) then

                        tempdr = qrad(j,N,K,iq) - bqrad(N,K,iq)
                        dr(j,NH,K,iq)    = sqrt(tempdr*tempdr)
                        drm(j,K,iq)      = sqrt(tempdr*tempdr) +
     &                                      drm(j,K,iq)
                        rbiasm(j,K,iq)   = rbiasm(j,K,iq) + tempdr
                        podm(j,K,iq,1)   = podm(j,K,iq,1) + 1
                        quadctm(j,K,iq)  = quadctm(j,K,iq) + 1
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
cx                    accumulate the radii evaluated
                        quadtot(j,K,iq)=quadtot(j,K,iq) + bqrad(N,K,iq)
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c                        write (*,455) '+++ YES, j N K iq drm quadct: '
c     &                       ,j,N,K,iq,drm(j,K,iq),quadctm(j,K,iq)
c 455                    format (1x,a30,i2,i4,i3,i3,f8.3,2x,i4)

c                     For the case in which there is a forecast
c                     radius but no verification radius (False alarm)

                      else if (qrad(j,N,K,iq) >  0.0  .and.
     &                          bqrad(N,K,iq) == 0.0) then

                        podm(j,K,iq,2)   = podm(j,K,iq,2) + 1
                        dr(j,NH,K,iq)    = 6666.0

c                     For the case in which there is no forecast
c                     radius but a radius verified (Missed forecast)

                      else if (qrad(j,N,K,iq) == 0.0  .and.
     &                          bqrad(N,K,iq) >  0.0) then

                        podm(j,K,iq,3)   = podm(j,K,iq,3) + 1
                        dr(j,NH,K,iq)    = 7777.0

c                     For the case in which there is no forecast
c                     radius and no radius verified (good forecast).

                      else if (qrad(j,N,K,iq) == 0.0  .and.
     &                          bqrad(N,K,iq) == 0.0) then

                        podm(j,K,iq,4)   = podm(j,K,iq,4) + 1
                        dr(j,NH,K,iq)    = 9999.0

                      endif

                    enddo quadloop

                  endif
C
               ENDIF

            enddo jloop
         enddo kloop
  260 CONTINUE
C
C**   CALCULATE AVERAGE ERRORS
C
      DO K = 0, mtau
         CASES  = FLOAT(MAX(NHCASE(K),1))
         SCASES = FLOAT(MAX((NHCASE(K) - 1),1))
         DO J = 1, NMODEL
            STDEV(J,K) = SQRT((STDEV(J,K) - (ERRM(J,K)*ERRM(J,K)/CASES))
     &                                /SCASES)
            ERRM(J,K)   = ERRM(J,K)/CASES
            XBIASM(J,K) = XBIASM(J,K)/CASES
            YBIASM(J,K) = YBIASM(J,K)/CASES
         enddo   
cx       if (intens == 1 .and. qradalso == 'y') then
         if (qradalso == 'y') then
           do j = 1,nqrmodel
             do iq = 1,mxquad

c               write (*,445) 'before ave ',J,K,iq,drm(J,K,iq)
c     &                       ,quadctm(J,K,iq)

               drm(J,K,iq) = drm(J,K,iq) /
     &                       float(MAX(quadctm(J,K,iq),1))
               if (drm(J,K,iq) == 0.0 .and.
     &             quadctm(J,K,iq) == 0) then
                 drm(J,K,iq) = -999
               endif
cx   mean radii
               quadtot(J,K,iq) = quadtot(J,K,iq) /
     &                       float(MAX(quadctm(J,K,iq),1))
               if (quadtot(J,K,iq) == 0.0 .and.
     &             quadctm(J,K,iq) == 0) then
                 quadtot(J,K,iq) = -999
               endif

c               write (*,445) 'after  ave ',J,K,iq,drm(J,K,iq)
c     &                       ,quadctm(J,K,iq)

c               write (*,445) 'before bias',J,K,iq,rbiasm(J,K,iq)
c     &                       ,quadctm(J,K,iq)

               rbiasm(J,K,iq) = rbiasm(J,K,iq) /
     &                          float(MAX(quadctm(J,K,iq),1))
               if (rbiasm(J,K,iq) == 0.0 .and.
     &             quadctm(J,K,iq) == 0) then
                 rbiasm(J,K,iq) = -999
               endif

c               write (*,445) 'after  bias',J,K,iq,rbiasm(J,K,iq)
c     &                       ,quadctm(J,K,iq)

             enddo
           enddo
         endif
      enddo

 445  format (1x,a11,' j= ',i2,' k= ',i3,' iq= ',i2,3x,f8.3,2x,i5)

C
C**   Calculate percentage of correct forecasts for intensity change
C
      if (intens == 1) then
        do k = 0,mtau
          do j = 1,nmodel
            icver(j,k)   = icver(j,k)   / float(MAX(ictot(j,k),1))
     &                   * 100.0
            icverhh(j,k) = icverhh(j,k) / float(MAX(ictothh(j,k),1))
     &                   * 100.0
          enddo
        enddo
      endif
C
C**   Calculate coefficients for correlations between the forecast
c     intensity change for each of the models and the verification
c     intensity change.  Since all the tendencies for the No-change
c     "model" added by this program are obviously going to be 0, we
c     take this model out of the following logic by only having the
c     loop go to nmodtmp (i.e., nmodel - 1).

      if (intens == 1) then

        nmodtmp = nmodel - 1

        kcorrloop: do K = 1,mtau
          jcorrloop: do J = 1,nmodtmp

            ict   = 0
            icthh = 0
            do N = 1,NCASE
              fcorr(N)   = 0.0
              vcorr(N)   = 0.0
              fcorrhh(N) = 0.0
              vcorrhh(N) = 0.0
            enddo

            ic_caseloop: do N = 1,NCASE

              if (ften(J,N,K) == -99.0 .or.
     &            vten(N,K)   == -99.0) cycle ic_caseloop

C              if (k .eq. 5) then
C                write (6,292) 'mod= ',mname(j),' J N K = ',J,N,K
C     &               ,ccdate(N),' ften vten = ',ften(J,N,K),vten(N,K)
C     &               ,' wd= ',weedout(J,N,K),' storm= ',sname(N)
C 292            format(1x,a5,a4,a9,3i3,1x,a8,a13,2f8.2,a5,i3,a8,a10)
C              endif

              if (ften(J,N,K) /= -99.0 .and.
     &            vten(N,K)   /= -99.0) then
                ict = ict + 1
                fcorr(ict) = ften(J,N,K)
                vcorr(ict) = vten(N,K)
              endif

              if (ftenhh(J,N,K) /= -99.0 .and.
     &            vtenhh(N,K)   /= -99.0) then
                icthh = icthh + 1
                fcorrhh(ict) = ftenhh(J,N,K)
                vcorrhh(ict) = vtenhh(N,K)
              endif

            enddo ic_caseloop

            if (ict > 0) then
              print *,' '
              print *,'------------------------- '
              print *,'Calling calccorr, J K = ',J,K,'  model= '
     &               ,mname(J),' ict= ',ict
              call calccorr(fcorr,vcorr,ict,R2(J,K))
            else
              print *,' '
              print *,'------------------------- '
              print *,' !!! NOT Calling calccorr, J K = ',J,K
     &               ,'  model= ',mname(J)
              do jc = 1,nmodtmp
                R2(jc,K)   = 0.0000
                R2hh(jc,K) = 0.0000
              enddo
              cycle kcorrloop
            endif
            if (icthh > 0) then
              print *,' '
              print *,'------------------------- '
              print *,'Calling calccorrhh, J K = ',J,K,'  model= '
     &               ,mname(J),' icthh= ',icthh
              call calccorr(fcorrhh,vcorrhh,ict,R2hh(J,K))
            else
              print *,' '
              print *,'------------------------- '
              print *,' !!! NOT Calling calccorrhh, J K = ',J,K
     &               ,'  model= ',mname(J)
              do jc = 1,nmodtmp
                R2(jc,K)   = 0.0000
                R2hh(jc,K) = 0.0000
              enddo
              cycle kcorrloop
            endif

          enddo jcorrloop
        enddo kcorrloop

      endif

C
C**   CALCULATE ERRORS RELATIVE TO THE DIFFERENT MODELS
C
      DO JK = 1, NMODEL
         DO K = 0, mtau
            DO J = 1, NMODEL
               IF ( ERRM(JK,K) .GT. 0.0 ) THEN
                  RECLIP(J,JK,K) = 100.0*( ERRM(J,K) - ERRM(JK,K) )
     &                                         /ERRM(JK,K)
               ELSE
                  RECLIP(J,JK,K) = 9999.0
               ENDIF
            enddo
         enddo   
      enddo
C
C**   CALCULATE FREQUENCIES OF SUPERIOR PERFORMANCE
C
      IF ( INCFSP .EQ. 1 .AND. INTENS .EQ. 1 ) THEN
	  NMFSP = NMODEL - 1
      ELSE
	  NMFSP = NMODEL
      ENDIF
C
C** C
c**   Originally only last tie counted.  Now all ties counted.
c**      Due to Sampson 01/04/26
c
      DO 330 K = 0, mtau
         DO 330 N = 1, NHCASE(0)
c
            DO J = 1, NMFSP
               IF (ERR(J,N,K) .GT. 9000.0) GO TO 330
            enddo
c
c**   Find the minimum value
c
            EMIN = 10000.0
            DO J = 1, NMFSP
               IF (ERR(J,N,K) .LT. EMIN) THEN
                  EMIN = ERR(J,N,K)
               ENDIF
            enddo
c
c**   Count all ties of minimum value
c
            do j = 1, nmfsp
               if ( err(j,n,k) .eq. emin ) then
                  NFSP(J,K) = NFSP(J,K) + 1
                  nfcase(k) = nfcase(k) + 1
               endif
            enddo
c
  330 CONTINUE
      DO J = 1, NMFSP
         DO K = 0, mtau        
            FSP(J,K) = 100.0*FLOAT(NFSP(J,K))/FLOAT(MAX(1,nfcase(K)))
cc            FSP(J,K) = 100.0*FLOAT(NFSP(J,K))/FLOAT(MAX(1,NHCASE(K)))
         enddo   
      enddo
C
C**   CALCULATE PROBABILITIES FOR SIGNIFICANCE TESTING
C
      NMAX = NHCASE(0)
      DO 430 K = 0, mtau
         RNH  = AMAX1(2.0,FLOAT(NHCASE(K)))
         RNHM = RNH - 1.0
C
         DO 420 J = 1, NMODEL - 1
         DO 420 JJ = J + 1,NMODEL
            RNHA  = 0.0
            TOLD  = -2.0*SAMADJ
            FNOLD = 'NONE'
            VAR   = 0.0
            DBAR  = ERRM(JJ,K) - ERRM(J,K)
C
            DO 410 N = 1, NMAX
               E1   = ERR(J,N,K)
               E2   = ERR(JJ,N,K)
               DIFF = E2-E1
               IF (E1 .GT. 9000.0 .OR. E2 .GT. 9000.0) GO TO 410
               VAR = VAR + (DIFF - DBAR)**2
C
C**   SAMPLE SIZE ADJUSTMENT CALCULATION
C
               READ (CHDATE(N),'(i4,3I2)') IYR,IMO,IDA,ITM
               TNEW = FLOAT(24*(IDA + NDMOS(IMO)) + ITM)
               IF (MOD(IYR,4).EQ.0 .AND. IMO.GT.2) TNEW = TNEW + 24.0
C
C**   RESET TOLD IF A NEW STORM STARTS
C
               FNNEW = FHNAME(N)
               IF (FNNEW .NE. FNOLD) TOLD = -2.0*SAMADJ
C
               DELT = TNEW - TOLD
               IF (DELT .GE. SAMADJ) THEN
                  RNHA = RNHA + 1.0
               ELSE
                  RNHA = RNHA + DELT/SAMADJ
               ENDIF
               TOLD  = TNEW
               FNOLD = FNNEW
 410        continue
C
            VAR = VAR/RNHM
            SIGMA = SQRT(VAR)
C
            IF (SIGMA .GT. 0.0) THEN
                TSTAT  = ABS(DBAR)/(SIGMA/SQRT(RNH ))
                TSTATA = ABS(DBAR)/(SIGMA/SQRT(RNHA))
                PROB(J,JJ,K)  = TDF(TSTAT ,RNH )
                PROBA(J,JJ,K) = TDF(TSTATA,RNHA)
            ELSE
                PROB(J,JJ,K)  = 0.0
                PROBA(J,JJ,K) = 0.0
            ENDIF
C
  420    CONTINUE
C
         RNHAA(K) = RNHA
C
  430 CONTINUE
C
C**   PRINT VERIFICATION SPECIFICATIONS
C
cx  other parameters added to specs ... sampson, nrl 
cx  
cx    WRITE (LULG,1) INPUT,VMVER
cx    WRITE (LULG,2) VMLO,VMHI
cx    WRITE (LULG,3) RLATLO,RLATHI
cx    WRITE (LULG,4) RLONLO,RLONHI
cx  1 FORMAT (' FORECAST VERIFICATION RESULTS FOR ',A,//,
cx   &         ' CASES ELIMINATED WITH VMAX .LE.' ,F5.0,
cx   &            ' AT INITIAL OR FORECAST TIME')
cx  2 FORMAT (/,' CASES RESTRICTED TO INITIAL VMAX IN THE INTERVAL: ',
cx   &         F6.1,' TO ',F6.1)
cx  3 FORMAT (' CASES RESTRICTED TO INITIAL LAT  IN THE INTERVAL: ',
cx   &         F6.1,' TO ',F6.1)
cx  4 FORMAT (' CASES RESTRICTED TO INITIAL LON  IN THE INTERVAL: ',
cx   &         F6.1,' TO ',F6.1)
cx  
cx    write (lulg,1) input,vmver,vmver1
cx    write (lulg,2) vmlo,vmhi
cx    nslo = 'N'
cx    nshi = 'N'
cx    if (rlatlo .lt. 0) nslo = 'S'
cx    if (rlathi .lt. 0) nshi = 'S'
cx    ewlo1 = 'W'
cx    ewhi1 = 'W'
cx    if (rlonlo .gt. 180.0) then
cx      ewlo1 = 'E'
cx      lonlo1 = 360 - rlonlo
cx    endif
cx    if (rlonhi .gt. 180.0) then
cx      ewhi1 = 'E'
cx	rlonhi1 = 360 - rlonhi
cx    endif
cx    write (lulg,3) abs(rlatlo),nslo,abs(rlathi),nshi
cx    write (lulg,4) rlonlo1,ewlo1,rlonhi1,ewhi1
cx  1 format (' FORECAST VERIFICATION RESULTS FOR ',A,//,
cx   &         ' CASES ELIMINATED WITH VMAX .LE.' ,F5.0,
cx   &         ' AND VMAX .GE.' ,F5.0, ' AT INITIAL OR FORECAST TIME')
cx  2 format (/,' CASES RESTRICTED TO INITIAL VMAX IN THE INTERVAL: ',
cx   &         F6.1,' TO ',F6.1)
cx  3 format (' CASES RESTRICTED TO INITIAL LAT  IN THE INTERVAL: ',
cx   &         F6.1,A1,' TO ',F6.1,A1)
cx  4 format (' CASES RESTRICTED TO INITIAL LON  IN THE INTERVAL: ',
cx   &         F6.1,A1,' TO ',F6.1,A1)

cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  this whole section applies to posits and intensity, skip for wind radii
      if (qradalso == 'y') go to 400
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
C
C**   PRINT AVERAGE ERRORS
C
      IF ( INTENS .EQ. 0 ) THEN
cx       WRITE ( LULG, 5 )
CC  5 FORMAT(//,' AVERAGE TRACK ERRORS (KM) FOR HOMOGENEOUS SAMPLE')
cx  5 FORMAT(//,' AVERAGE TRACK ERRORS (NM) FOR HOMOGENEOUS SAMPLE')
         write ( lulg, 5 ) unitch( metric+1 )
    5 format(//,' average track errors (',a2,') FOR HOMOGENEOUS SAMPLE')
      ELSE
         WRITE (LULG,6)
    6 FORMAT(//,' AVERAGE INTENSITY ERRORS (KT) FOR HOMOGENEOUS SAMPLE'
     &)
      ENDIF
C
      WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
      DO J = 1,NMODEL
         WRITE ( LULG, 8 ) MNAME(J), ( ERRM(J,K), K = 0, mtau )
    8 FORMAT (1X,A4,4X,8(F6.1,1X))
      enddo
C
      WRITE (LULG,9) (NHCASE(K),K = 0,mtau)
    9 FORMAT (1X,'#CASES   ',8(I4,3X))
C
C**   PRINT spreadsheet FORM OF THE AVERAGE ERRORS
C
      write ( luhv, '( ''INPUT FILE NAME - '', a )' ) input
c
      IF (INTENS .EQ. 0) THEN
         WRITE (LUHV,5)
      ELSE
         WRITE (LUHV,6)
      ENDIF
C
      WRITE (LUHV,11) (MNAME(J), J = 1,NMODEL)
   11 FORMAT (/,'    #, FPD,',2X,15(a4,',  '))
cc   11 FORMAT (/,' FPD   #',3X,15(2X,A4))
C
      DO K = 0, mtau
c
         WRITE (LUHV,12) nhcase(k),FCSTPD(K),(ERRM(J,K), J = 1,NMODEL)
   12    FORMAT (1X,i4,', ',a3,',',15(F6.1,','))
cc   12    FORMAT (1X,A3,1x,i4,2X,15(F6.1))
c
         IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')

      enddo
C
cc      WRITE (LUHV,'('' n = '',I4,'' at T = 0...n = '',I4,
cc     & '' at T = '',a3)') NHCASE(0),NHCASE(mtau),fcstpd(mtau)
C
C**   PRINT ERROR STANDARD DEVIATION
C
cx    WRITE (LULG,801)
cx801 FORMAT(//,' ERROR STANDARD DEVIATION (NM) FOR HOMOGENEOUS SAMPLE')
      write (lulg,801) unitch( metric + 1 )
  801 FORMAT(//,' ERROR STANDARD DEVIATION (',a2,
     &') FOR HOMOGENEOUS SAMPLE')
C
      WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
      DO J = 1, NMODEL
         WRITE ( LULG, 8 ) MNAME(J), ( STDEV(J,K), K = 0, mtau )
      enddo
C
      WRITE (LULG,9) (NHCASE(K),K = 0, mtau)
C
C**   PRINT HARVARD GRAPHICS FORM OF THE AVERAGE ERRORS
C
cc      WRITE (LUHV,801)
C
cc      WRITE (LUHV,11) (MNAME(J), J = 1,NMODEL)
C
cc      DO K = 0, mtau
c
cc         WRITE (LUHV,12) FCSTPD(K),nhcase(k),(stdev(J,K), J = 1,NMODEL)
c
cc         IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')

cc      enddo
C
cc      WRITE (LUHV,'('' n = '',I4,'' at T = 0...n = '',I4,
cc     & '' at T = '',a3)') NHCASE(0),NHCASE(mtau),fcstpd(mtau)
C
C**   PRINT AVERAGE XBIAS FOR THE FORECASTS
C
      IF (INTENS .EQ. 0) THEN
CC        WRITE (LULG,'(/,'' AVERAGE XBIAS (KM) FOR HOMOGENEOUS SAMPLE''
CC   &     )')
cx        WRITE (LULG,'(/,'' AVERAGE XBIAS (NM) FOR HOMOGENEOUS SAMPLE''
cx   &     )')
          WRITE (LULG, 711) unitch( metric + 1 )
  711 FORMAT (/,' AVERAGE XBIAS (',a2,') FOR HOMOGENEOUS SAMPLE')
      ELSE
          WRITE (LULG,'(/,'' AVERAGE INTENSITY BIAS (KT) FOR HOMOGENEOUS
     & SAMPLE'')')
C
C**   PRINT OUT spreadsheet FORM FOR THE INTENSITY BIAS
C
          WRITE (LUHV,'(/,'' AVERAGE INTENSITY BIAS (KT) FOR HOMOGENEOUS
     & SAMPLE'')')
C
          WRITE (LUHV,11) (MNAME(J), J = 1,NMODEL)
C
C
          DO K = 0, mtau
c
             WRITE (LUHV,12) nhcase(k),FCSTPD(K),
     &                       (xbiasm(J,K), J = 1,NMODEL)
c
             IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')

          enddo
C
cc          WRITE (LUHV,'('' n = '',I4,'' at T = 0...n = '',I4,
cc     &     '' at T = '',a3)') NHCASE(0),NHCASE(mtau),fcstpd(mtau)
C
      ENDIF
C
      WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
      DO J = 1, NMODEL
         WRITE ( LULG,8 ) MNAME(J), ( XBIASM(J,K), K = 0, mtau )
      enddo
C
      WRITE (LULG,9) (NHCASE(K),K = 0,mtau)
C
      IF ( INTENS .EQ. 0 ) THEN
C
C**   PRINT AVERAGE YBIAS FOR THE FORECASTS
C
cx       WRITE ( LULG, 511 )
CC511    FORMAT(/,1X,'AVERAGE YBIAS (KM) FOR HOMOGENEOUS SAMPLE')
cx511    FORMAT(/,1X,'AVERAGE YBIAS (NM) FOR HOMOGENEOUS SAMPLE')
         write ( lulg, 511 ) unitch( metric + 1 )
  511    format(/,1X,'AVERAGE YBIAS (',a2,') FOR HOMOGENEOUS SAMPLE')
C
C
         WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
         DO J = 1, NMODEL
            WRITE ( LULG, 8 ) MNAME(J), ( YBIASM(J,K), K = 0, mtau )
         enddo
C
         WRITE ( LULG, 9 ) ( NHCASE(K), K = 0, mtau )
C
      ENDIF
c     ---------------------------------------------
c**   Print out intensity change verification stuff
c     ---------------------------------------------

      if (intens == 1) then

        write (lulg,522)
        write (lulg,523)
        write (lulg,524)
        write (lulg,525)
        write (lulg,526)
        write (lulg,527)
        write (lulg,534) icincr
  522   format(//,1x,'--- BEGIN INTENSITY CHANGE VERIFICATION ---')
  523   format(/,' Values listed are percentages of cases that the'
     &          ,' models')
  524   format(1x,'correctly predicted the intensity tendency (i.e., ')
  525   format(1x,'fcst_intens-->obs_intens, fcst_weak-->obs_weak or ')
  526   format(1x,'fcst_nochge-->obs_nochge) for the indicated ')
  527   format(1x,'forecast interval.  Intensity change threshold')
  534   format(1x,'(in knots) = ',f4.1,'  SAMPLE IS HOMOGENEOUS.')

        write (lulg,528)
  528   FORMAT(/,'                  00-12  12-24  24-36  36-48  48-72'
     &          ,'  72-96  96-120')

        do j = 1,nmodel
           write (lulg,529) mname(j),(icver(j,k), k = 1,mtau)
        enddo
        write (lulg,531) (ictot(1,k), k = 1,mtau)

  529   FORMAT (1X,A4,12X,7(F6.1,1X))
  531   FORMAT (1X,'#CASES ',11x,7(I4,3X))

        write (lulg,532)
  532   FORMAT(/,'                  00-12  00-24  00-36  00-48  00-72'
     &          ,'  00-96  00-120')

        do j = 1,nmodel
           write (lulg,529) mname(j),(icverhh(j,k), k = 1,mtau)
        enddo
        write (lulg,531) (ictothh(1,k), k = 1,mtau)

c
c       NOW PRINT OUT INTENSITY CHANGE CORRELATION STUFF
c

        write (lulg,542)
        write (lulg,544)
        write (lulg,545)
        write (lulg,546)
  542   format(//,' Values listed are correlation coefficients (r) for')
  544   format(1x,'correlations between the intensity change forecast')
  545   format(1x,'by a model and the intensity change that was')
  546   format(1x,'observed.  SAMPLE IS HOMOGENEOUS.')

c       Since all the tendencies for the No-change
c       "model" added by this program are obviously going to be 0, we
c       take this model out of the following logic by only having the
c       loop go to nmodtmp (i.e., nmodel - 1).

        write (lulg,528)

        nmodtmp = nmodel - 1

        do j = 1,nmodtmp
           write (lulg,547) mname(j),(sqrt(R2(j,k)), k = 1,mtau)
        enddo
        write (lulg,531) (ictot(1,k), k = 1,mtau)

  547   FORMAT (1X,A4,12X,7(F6.4,1X))

        write (lulg,532)

        do j = 1,nmodtmp
           write (lulg,547) mname(j),(sqrt(R2hh(j,k)), k = 1,mtau)
        enddo
        write (lulg,531) (ictothh(1,k), k = 1,mtau)

        write (lulg,533)
  533   format(/,1x,'--- END   INTENSITY CHANGE VERIFICATION ---')

      endif

cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  the above section applies to posits and intensity, skipped for wind radii
cx  see the comment section like this one above for start of non-wind radii code
  400 continue 
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx

c     -------------------------
c     Print quadrant wind radii verification stats, if requested.
c     -------------------------

cx    if (intens == 1 .and. qradalso == 'y') then
      if (qradalso == 'y') then

cx    Start with the average radii
        write (lulg,809)
  809   format(//,1x,'AVERAGE WIND RADII (NM) FOR VERIFIED FORECASTS')
        do k=0,mtau
         ikhr = ktime(k)
         write (lulg,819)
         write (lulg,820)
         do j = 1,nqrmodel
cx       get the average of the quads, print out at the end of the line
cx       allow summaries when one or more quadrants have no data         
         nav34 = 0
         iav34 = 0
         nav50 = 0
         iav50 = 0
         nav64 = 0
         iav64 = 0

         do iii = 1, 4
             if ( quadtot(j,k,iii) .gt. 0 ) then
                  nav34 = nav34 + quadctm(j,k,iii)
                  iav34 = iav34 + quadtot(j,k,iii)*quadctm(j,k,iii)
             endif
             if ( quadtot(j,k,iii+4) .gt. 0 ) then
                  nav50 = nav50 + quadctm(j,k,iii+4)
                  iav50 = iav50 + quadtot(j,k,iii+4)*quadctm(j,k,iii+4)
             endif
             if ( quadctm(j,k,iii+8) .gt. 0 ) then
                  nav64 = nav64 + quadctm(j,k,iii+8)
                  iav64 = iav64 + quadtot(j,k,iii+8)*quadctm(j,k,iii+8)
             endif
         enddo
         if (nav34 .gt. 0) iav34 = nint(float(iav34)/float(nav34))
         if (nav50 .gt. 0) iav50 = nint(float(iav50)/float(nav50))
         if (nav64 .gt. 0) iav64 = nint(float(iav64)/float(nav64))
 
cx       take care of missing data
         if (quadtot(j,k,1).lt.0. .and. quadtot(j,k,2).lt.0. .and. 
     &       quadtot(j,k,3).lt.0. .and. quadtot(j,k,4).lt.0.) then  
                        iav34=-999
                        nav34=-999
         endif
         if (quadtot(j,k,5).lt.0. .and. quadtot(j,k,6).lt.0. .and. 
     &       quadtot(j,k,7).lt.0. .and. quadtot(j,k,8).lt.0.) then  
                        iav50=-999
                        nav50=-999
         endif
         if (quadtot(j,k,9).lt.0. .and. quadtot(j,k,10).lt.0. .and. 
     &       quadtot(j,k,11).lt.0. .and. quadtot(j,k,12).lt.0.) then        
                        iav64=-999
                        nav64=-999
         endif
         write (lulg,823) mname(j),ikhr,
     &                    (int(quadtot(j,k,isect)),isect=1,12),
     &                    iav34,nav34,iav50,nav50,iav64,nav64
         enddo
        enddo
C     
C**   PRINT OUT spreadsheet FORM FOR THE AVERAGE QUADRANT RADII VALUES
C**     FOR EACH THRESHOLD
C
        do isect = 1, 12
c
          WRITE (LUHV,'(/,'' AVERAGE RADII VALUES (NM) FOR '',a8)')
     &                quades(isect)
C
          WRITE (LUHV,11) (MNAME(J), J = 1,NqrMODEL)
C
          DO K = 0, mtau
c
             ikhr = ktime(k)
             WRITE (LUHV,821) nhcase(k), fcstpd(k),
     &                       (int(quadtot(j,k,isect)), J = 1,NqrMODEL)
c
             IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')

          enddo
        enddo
c
c**     PRINT THE AVERAGE ERRORS....
c
        write (lulg,810)
  810   format(//,1x,'AVERAGE ERRORS (NM) FOR QUADRANT RADII FORECASTS')
        write (lulg,813)
        write (lulg,814)
        write (lulg,815)
        write (lulg,816)
        write (lulg,817)
        write (lulg,818)
  813   format(/,1x,'(Sample is NOT Homogeneous, due to the fact that')
  814   format(1x,' the models did not always forecast winds at the')
  815   format(1x,' specified wind threshold levels.)',/)
  816   format(1x,'NOTE: A value of -999 indicates that there were NO')
  817   format(1x,'observations for this model in this quadrant at ')
  818   format(1x,'the specified forecast time and threshold level.')
        do k=0,mtau
         ikhr = ktime(k)
         write (lulg,819)
         write (lulg,820)
         do j = 1,nqrmodel
cx       get the average of the quads, print out at the end of the line
cx       allow summaries when one or more quadrants have no data         

         nav34 = 0
         iav34 = 0
         nav50 = 0
         iav50 = 0
         nav64 = 0
         iav64 = 0
         do iii = 1, 4
             if ( quadctm(j,k,iii) .gt. 0 ) then
                  nav34 = nav34 + quadctm(j,k,iii)
                  iav34 = iav34 + drm(j,k,iii)*quadctm(j,k,iii)
             endif
             if ( quadctm(j,k,iii+4) .gt. 0 ) then
                  nav50 = nav50 + quadctm(j,k,iii+4)
                  iav50 = iav50 + drm(j,k,iii+4)*quadctm(j,k,iii+4)
             endif
             if ( quadctm(j,k,iii+8) .gt. 0 ) then
                  nav64 = nav64 + quadctm(j,k,iii+8)
                  iav64 = iav64 + drm(j,k,iii+8)*quadctm(j,k,iii+8)
             endif
         enddo
         if (nav34 .gt. 0) iav34 = nint(float(iav34)/float(nav34))
         if (nav50 .gt. 0) iav50 = nint(float(iav50)/float(nav50))
         if (nav64 .gt. 0) iav64 = nint(float(iav64)/float(nav64))
 
cx       take care of missing data
         if (drm(j,k,1).lt.0. .and. drm(j,k,2).lt.0. .and. 
     &       drm(j,k,3).lt.0. .and. drm(j,k,4).lt.0.) then  
                        iav34=-999
                        nav34=-999
         endif
         if (drm(j,k,5).lt.0. .and. drm(j,k,6).lt.0. .and. 
     &       drm(j,k,7).lt.0. .and. drm(j,k,8).lt.0.) then  
                        iav50=-999
                        nav50=-999
         endif
         if (drm(j,k,9).lt.0. .and. drm(j,k,10).lt.0. .and. 
     &       drm(j,k,11).lt.0. .and. drm(j,k,12).lt.0.) then        
                        iav64=-999
                        nav64=-999
         endif
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
cx          write (lulg,823) mname(j),ikhr
cx   &           ,(int(drm(j,k,isect)),isect=1,12)
         write (lulg,823) mname(j),ikhr,
     &                    (int(drm(j,k,isect)),isect=1,12),
     &                    iav34,nav34,iav50,nav50,iav64,nav64
         enddo
        enddo
c
cx820   format(/,10x,' ne34 se34 sw34 nw34  ne50 se50 sw50 nw50'
cx   &               ,'  ne64 se64 sw64 nw64')
  819   format(/,10x,'---------------------individual quadrants'
     &               ,'---------------------'
     &               ,'|-----------combined-----------')
  820   format(/,10x,' ne34 se34 sw34 nw34  ne50 se50 sw50 nw50'
     &               ,'  ne64 se64 sw64 nw64'
     &               ,'  av34  n34 av50  n50 av64  n64')
cx823   format(1x,a4,2x,i3.3,1x,4(i4,1x),1x,4(i4,1x),1x,4(i4,1x))
  823   format(1x,a4,2x,i3.3,1x,4(i4,1x),1x,4(i4,1x),1x,4(i4,1x),
     &         1x,6(i4,1x))
C
C**   PRINT OUT spreadsheet FORM FOR THE AVERAGE QUADRANT RADII ERRORS
C**     FOR EACH THRESHOLD
C
        do isect = 1, 12
c
          WRITE (LUHV,'(/,'' AVERAGE RADII ERRORS (NM) FOR '',a8)')
     &                quades(isect)
C
          WRITE (LUHV,11) (MNAME(J), J = 1,NqrMODEL)
C
          DO K = 0, mtau
c
             ikhr = ktime(k)
             WRITE (LUHV,821) nhcase(k), fcstpd(k),
     &                       (int(drm(j,k,isect)), J = 1,NqrMODEL)
 821         FORMAT (1X,i4,', ',a3,',',15(i6,','))
c
             IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')

          enddo
        enddo

c       NOW PRINT THE AVERAGE BIASES....

        write (lulg,830)
  830   format(//,1x,'AVERAGE BIAS (NM) FOR QUADRANT RADII FORECASTS')
        write (lulg,813)
        write (lulg,814)
        write (lulg,815)
        write (lulg,816)
        write (lulg,817)
        write (lulg,818)

        write (lulg,832)
  832   format(/,1x,'NOTE: Numbers < 0 indicate underforecast bias.')

        do k=0,mtau
          ikhr = ktime(k)
          write (lulg,824)
          do j = 1,nqrmodel
            write (lulg,826) mname(j),ikhr
     &           ,(int(rbiasm(j,k,isect)),isect=1,12)
          enddo
        enddo
  824   format(/,10x,' ne34 se34 sw34 nw34  ne50 se50 sw50 nw50'
     &               ,'  ne64 se64 sw64 nw64')
  826   format(1x,a4,2x,i3.3,1x,4(i4,1x),1x,4(i4,1x),1x,4(i4,1x))
C     
C**   PRINT OUT spreadsheet FORM FOR THE AVERAGE QUADRANT RADII BIASES
C**     FOR EACH THRESHOLD
C
        do isect = 1, 12
c
          WRITE (LUHV,'(/,'' AVERAGE RADII BIASES (NM) FOR '',a8)')
     &                quades(isect)
C
          WRITE (LUHV,11) (MNAME(J), J = 1,NqrMODEL)
C
          DO K = 0, mtau
c
             ikhr = ktime(k)
             WRITE (LUHV,821) nhcase(k), fcstpd(k),
     &                       (int(rbiasm(j,k,isect)), J = 1,NqrMODEL)
c
             IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')
c
          enddo
        enddo
c
      endif

C     -----------------------
C     For quadrant wind radii forecasts, print Probability of
C     Detection stats and False Alarm Rate stats.
C     -----------------------

cx    if (intens == 1 .and. qradalso == 'y') then
      if (qradalso == 'y') then

        write (lulg,840)
        write (lulg,841)
        write (lulg,842)
  840   format(//,1x,'QUADRANT WIND RADII FORECAST STATS:')
  841   format(1x,'Probability of Detection and False Alarm Rate')
  842   format(/,1x,'   !!!! Sample *IS* Homogeneous. !!!!')

C       FIRST DO PROBABILITY OF DETECTION STATS.

        write (lulg,871)
        write (lulg,872)
        write (lulg,873)
        write (lulg,874)
        write (lulg,875)
  871   format(//,1x,' POD Stats are listed first.  Fractions show the')
  872   format(1x,'number of times that the winds were forecast by the')
  873   format(1x,'model, divided by the total number of times winds ')
  874   format(1x,'did verify in that quadrant. The pcts are in ')
  875   format(1x,'parentheses.  "-99" ==> no observations')

        do k=0,mtau
          ikhr = ktime(k)
          do j = 1,nqrmodel

            write (lulg,845) mname(j),'POD'

            do it = 1,3
              itfcst = 0
              itver  = 0
              itpct  = 0
              do iq = 1,4
                isect = (it - 1) * 4 + iq
                iqtot(isect) = podm(j,K,isect,1) + podm(j,K,isect,3)
                if (iqtot(isect) == 0) then
                  ipct(isect) = -99
                else
                  ipct(isect)  = int(float(podm(j,K,isect,1))
     &                         / float(iqtot(isect)) * 100 + 0.5)
                endif
                itfcst = podm(j,K,isect,1) + itfcst
                itver  = iqtot(isect) + itver
              enddo

              if (itver .eq. 0) then
                itpct = -99
              else
                itpct = int(float(itfcst)/float(itver) * 100 + 0.5)
              endif

              if (it .eq. 1) then
                write (lulg,847) ikhr,34
     &                ,podm(j,K,1,1),iqtot(1),ipct(1)
     &                ,podm(j,K,2,1),iqtot(2),ipct(2)
     &                ,podm(j,K,3,1),iqtot(3),ipct(3)
     &                ,podm(j,K,4,1),iqtot(4),ipct(4)
     &                ,itfcst,itver,itpct
              elseif (it .eq. 2) then
                write (lulg,847) ikhr,50
     &                ,podm(j,K,5,1),iqtot(5),ipct(5)
     &                ,podm(j,K,6,1),iqtot(6),ipct(6)
     &                ,podm(j,K,7,1),iqtot(7),ipct(7)
     &                ,podm(j,K,8,1),iqtot(8),ipct(8)
     &                ,itfcst,itver,itpct
              elseif (it .eq. 3) then
                write (lulg,847) ikhr,64
     &                ,podm(j,K,9,1),iqtot(9),ipct(9)
     &                ,podm(j,K,10,1),iqtot(10),ipct(10)
     &                ,podm(j,K,11,1),iqtot(11),ipct(11)
     &                ,podm(j,K,12,1),iqtot(12),ipct(12)
     &                ,itfcst,itver,itpct
              endif

            enddo

          enddo

        enddo

  845   format(/,1x,a4,'  ',a3,':    NE           SE           SW'
     &                ,'           NW          QUADS TOTAL')
  847   format(1x,i3,1x,i2,' kt:',4(1x,i3,'/',i3,'(',i3,')')
     &              ,1x,i4,'/',i4,'(',i3,')')


C       NOW DO FALSE ALARM STATS.

        write (lulg,849)
  849   format(//,1x,' FAR Stats are now listed....',/)

        write (lulg,881)
        write (lulg,882)
        write (lulg,883)
        write (lulg,884)
        write (lulg,885)
  881   format(//,1x,' FAR Stats are listed next.  Fractions show the')
  882   format(1x,'number of times that the winds were forecast by the')
  883   format(1x,'model, divided by the total number of times winds')
  884   format(1x,'did *NOT* verify in that quadrant. The pcts are in')
  885   format(1x,'parentheses.  "-99" ==> no observations')

        do k=0,mtau
          ikhr = ktime(k)
          do j = 1,nqrmodel

            write (lulg,845) mname(j),'FAR'

            do it = 1,3
              itfcst = 0
              itver  = 0
              itpct  = 0
              do iq = 1,4
                isect = (it - 1) * 4 + iq
                iqtot(isect) = podm(j,K,isect,2) + podm(j,K,isect,4)
                if (iqtot(isect) .eq. 0) then
                  ipct(isect) = -99
                else
                  ipct(isect)  = int(float(podm(j,K,isect,2))
     &                         / float(iqtot(isect)) * 100 + 0.5)
                endif
                itfcst = podm(j,K,isect,2) + itfcst
                itver  = iqtot(isect) + itver
              enddo

              if (itver .eq. 0) then
                itpct = -99
              else
                itpct = int(float(itfcst)/float(itver) * 100 + 0.5)
              endif

              if (it .eq. 1) then
                write (lulg,847) ikhr,34
     &                ,podm(j,K,1,2),iqtot(1),ipct(1)
     &                ,podm(j,K,2,2),iqtot(2),ipct(2)
     &                ,podm(j,K,3,2),iqtot(3),ipct(3)
     &                ,podm(j,K,4,2),iqtot(4),ipct(4)
     &                ,itfcst,itver,itpct
              elseif (it .eq. 2) then
                write (lulg,847) ikhr,50
     &                ,podm(j,K,5,2),iqtot(5),ipct(5)
     &                ,podm(j,K,6,2),iqtot(6),ipct(6)
     &                ,podm(j,K,7,2),iqtot(7),ipct(7)
     &                ,podm(j,K,8,2),iqtot(8),ipct(8)
     &                ,itfcst,itver,itpct
              elseif (it .eq. 3) then
                write (lulg,847) ikhr,64
     &                ,podm(j,K,9,2),iqtot(9),ipct(9)
     &                ,podm(j,K,10,2),iqtot(10),ipct(10)
     &                ,podm(j,K,11,2),iqtot(11),ipct(11)
     &                ,podm(j,K,12,2),iqtot(12),ipct(12)
     &                ,itfcst,itver,itpct
              endif

            enddo

          enddo

        enddo

      endif

cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  this whole section applies to posits and intensity, skip for wind radii
      if (qradalso == 'y') go to 600
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
C
C**   PRINT ERRORS RELATIVE TO EACH MODEL
C
      DO NN = 1,NMODEL
        WRITE (LULG,521) MNAME(NN)
  521   FORMAT(//,1X,'AVERAGE ERRORS RELATIVE TO ',A,' (%)')
C
        WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
        DO J = 1, NMODEL
          WRITE ( LULG, 8 ) MNAME(J), ( RECLIP(J,NN,K), K = 0, mtau )
        enddo
C
        WRITE ( LULG, 9 ) ( NHCASE(K), K = 0, mtau )
C
      enddo
C
C**   PRINT spreadsheet FORM OF ERRORS RELATIVE TO EACH MODEL
C
      DO 543 NN = 1,NMODEL
C
	if ( mname(nn) .eq. 'CLIP' .or. mname(nn) .eq. 'CLP5'
     &   .or. mname(nn) .eq. 'SHFR' .or. mname(nn) .eq. 'SHF5' ) then
C
            WRITE (LUHV,521) MNAME(NN)
C
            WRITE (LUHV,11) (MNAME(J), J = 1,NMODEL)
C
            DO K = 0, mtau
c
               WRITE (LUHV,12) nhcase(k),FCSTPD(K),
     &                     (RECLIP(J,NN,K), J = 1,NMODEL)
c
               IF ( K .gt. 3 ) WRITE (LUHV,'('' '')')

            enddo
        endif
C
cc        WRITE (LUHV,'('' n = '',I4,'' at T = 0...n = '',I4,
cc     &   '' at T = '',a3)') NHCASE(0),NHCASE(mtau),fcstpd(mtau)
c
  543 CONTINUE
C
C**   PRINT FREQUENCIES OF SUPERIOR PERFORMANCE by percent
C
      WRITE (LULG,'(/,'' FREQUENCY OF SUPERIOR PERFORMANCE (%)'')')
C
C
      WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
      DO J = 1, NMFSP
         WRITE ( LULG, 8 ) MNAME(J), ( FSP(J,K), K = 0, mtau )
      enddo
C
C**   PRINT FREQUENCIES OF SUPERIOR PERFORMANCE by number
C
      WRITE (LULG,'(/,'' FREQUENCY OF SUPERIOR PERFORMANCE (number)'')')
C
      WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
      DO J = 1, NMFSP
         WRITE ( LULG, 548 ) MNAME(J), (NFSP(J,K), K = 0, mtau )
 548     FORMAT (1X,A4,3X,8(i6,1X))
      enddo
C
      WRITE ( LULG, 549 ) ( nfcase(K), K = 0, mtau )
 549  FORMAT (1X,'#TOTAL   ',8(I4,3X))
      WRITE ( LULG, 9 ) ( NHCASE(K), K = 0, mtau )

C
C**   PRINT PROBABILITIES FOR SIGNIFICANCE TESTS
C
      DO K = 0, mtau
c
         WRITE (LULG,551) KTIME(K),NHCASE(K)
  551    FORMAT (/,1X,'PROBABILITIES FOR MODEL DIFFERENCES AT T= ',
     &             I3,'  SAMPLE SIZE=',I4)
         WRITE (LULG,552) (MNAME(JJ),JJ = 2,NMODEL)
  552    FORMAT (6X,10(A4,2X))
c
         DO J = 1, NMODEL - 1
            WRITE (LULG,553) MNAME(J),(PROB(J,JJ,K),JJ = 2,NMODEL)
  553       FORMAT (1X,A4,1X,10(F5.3,1X))
         enddo
c
      enddo
C
C**   PRINT PROBABILITIES WITH ADJUSTED SAMPLE SIZE
C
      WRITE(LULG,571) SAMADJ
  571 FORMAT (//,1X,' SAMPLE SIZE ADJUSTED FOR ',F5.1,
     &              ' HOUR SERIAL CORRELATION')
      DO K = 0, mtau
c
         RNHA = RNHAA(K)
         WRITE (LULG,572) KTIME(K),RNHA
  572    FORMAT (/,1X,'ADJUSTED PROBABILITIES AT T= ',
     &             I3,' ADJUSTED SAMPLE SIZE=',F6.1)
         WRITE (LULG,552) (MNAME(JJ),JJ=2,NMODEL)
c
         DO J = 1, NMODEL - 1
            WRITE (LULG,553) MNAME(J),(PROBA(J,JJ,K),JJ = 2,NMODEL)
         enddo
      enddo
C
C**   PRINT AVERAGE ERRORS FOR THE INDIVIDUAL CASES
C
      IF ( IERRPR .EQ. 1 ) THEN
c
         IF ( INTENS .EQ. 0 ) THEN
CC           WRITE (LULG,'(//,'' TRACK ERRORS (KM) FOR HOMOGENEOUS SAMPL
CC   &E'')')
cx           WRITE (LULG,'(//,'' TRACK ERRORS (NM) FOR HOMOGENEOUS SAMPL
cx   &E'')')
             write (lulg, 573) unitch ( metric + 1 ) 
  573        format(//,' TRACK ERRORS (',a2,') FOR HOMOGENEOUS SAMPLE ')
         ELSE
             WRITE (LULG,'(//,'' INTENSITY ERRORS (KTS) FOR HOMOGENEOUS
     &SAMPLE'')')
         ENDIF
C
cx  Figure out the maximum number of cases so that you print more errors.  
cx  Note: This still won't print all errors. 
cx  Taus greater than 12 for which no verifying 12h taus exist aren't printed.
         imax = 0
         do i = 0, mtau
            if (nhcase(i) .gt. imax) imax = nhcase(i) 
         enddo

cx       DO I = 1, NHCASE(0)
         DO I = 1, imax
            WRITE (LULG,'(/,1X,A8,2X,A10,2X,A10,3(2X,F6.1))') FHNAME(I),
     &       SHNAME(I),CHDATE(I),BHLAT(I),BHLON(I),BHVMAX(I)
C
            WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
            DO J = 1, NMODEL
               IF ( INTENS .EQ. 0 ) THEN
                  WRITE ( LULG, 8 ) MNAME(J), ( ERR(J,I,K), K = 0,mtau)
               ELSE
                  WRITE ( LULG, 8 ) MNAME(J), (XBIAS(J,I,K),K = 0,mtau)
               ENDIF
            enddo
         enddo
C
      ENDIF
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  the above section applies to posits and intensity, skipped for wind radii
cx  see the comment section like this one above for start of non-wind radii code
  600 continue 
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx

c     ---------------
c**   PRINT quadrant errors for the individual cases
c     ---------------

cx    if (intens == 1 .and. qradalso == 'y' .and. ierrpr == 1) then
      if (qradalso == 'y' .and. ierrpr == 1) then

        write (lulg,602)
  602   format(//,1x,'ERRORS (NM) FOR QUADRANT RADII FORECASTS BY CASE')
        write (lulg,813)
        write (lulg,814)
        write (lulg,815)
        write (lulg,861)
        write (lulg,862)
        write (lulg,863)
        write (lulg,864)
  861   format(/,1x,'Special codes below are as follows: ')
  862   format(1x,'  9999 = No fcst winds, No verify winds (Null '
     &           ,'Forecast)')
  863   format(1x,'  6666 = Yes fcst winds, No verify winds (False '
     &           ,'Alarm)')
  864   format(1x,'  7777 = No fcst winds, Yes verify winds (Missed '
     &           ,'Forecast)')

        do i = 1,nhcase(0)

          WRITE (LULG,'(/,1X,A8,2X,A10,2X,A10,3(2X,F6.1))') FHNAME(I),
     &     SHNAME(I),CHDATE(I),BHLAT(I),BHLON(I),BHVMAX(I)

          do k=0,mtau
            ikhr = ktime(k)
            if (k == 0) then
              write (lulg,820)
            else
              write (lulg,*) ' '
            endif
            do j = 1,nqrmodel
              write (lulg,823) mname(j),ikhr
     &             ,(int(dr(j,i,k,isect) + 0.5),isect=1,12)
            enddo
          enddo

        enddo

      endif

cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  this whole section applies to posits and intensity, skip for wind radii
      if (qradalso == 'y') go to 800
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx

C
C**   CALCULATE AND PRINT THE AVERAGE ERRORS FOR EACH STORM
C
      IF ( ISTMPR .EQ. 1 ) THEN
         ISTORM = 0
c
         IF ( INTENS .EQ. 0 ) THEN
CC           WRITE (LULG,'(//,'' TRACK ERRORS (KM) FOR HOMOGENEOUS SAMPL
CC   &E'')')
cx           WRITE (LULG,'(//,'' TRACK ERRORS (NM) FOR HOMOGENEOUS SAMPL
cx   &E'')')
             write (lulg,574)unitch( metric + 1)
  574        format(//,' TRACK ERRORS (',a2,') FOR HOMOGENEOUS SAMPLE')
         ELSE
             WRITE (LULG,'(//,'' INTENSITY ERRORS (KTS) FOR HOMOGENEOUS
     &SAMPLE'')')
         ENDIF
C
         DO I = 1, NDATAF
            ICASE = 0
C
            DO J = 1, NHCASE(0)
               IF ( FLNAME(I). EQ. FHNAME(J) ) THEN
                   ICASE = ICASE + 1
                   DO K = 1, NMODEL
                      DO L = 0, mtau
                         ERRS(K,ICASE,L) = ERR(K,J,L)
                      enddo   
                   enddo
               ENDIF
            enddo
C
            DO L = 0, mtau
               ICASECT(L) = ICASE
               DO K = 1, NMODEL
                  ERROR(K,L) = 0.0
               enddo   
            enddo
C
            DO J = 1, ICASE
               DO K = 1, NMODEL
                  DO L = 0, mtau
                     IF ( ERRS(K,J,L) .NE. 9999.0 ) THEN
                        ERROR(K,L) = ERROR(K,L) + ERRS(K,J,L)
                     ELSE
                        IF (K.EQ.1) ICASECT(L) = ICASECT(L) - 1
                     ENDIF
                  enddo
               enddo   
            enddo
C
            DO L = 0, mtau
               DO K = 1, NMODEL
                  IF ( ICASECT(L) .NE. 0.0 ) THEN
                     ERROR(K,L) = ERROR(K,L)/ICASECT(L)
                  ELSE
                     ERROR(K,L) = 9999.0
                  ENDIF
               enddo   
            enddo
C
            IF ( INTENS .EQ. 0 ) THEN
cx              WRITE (LULG,61) FLNAME(I),SFNAME(I)
cx 61           FORMAT(//,' FORECAST ERRORS (KM) FOR ',A8,2X,A10)
cx 61           FORMAT(//,' FORECAST ERRORS (NM) FOR ',A8,2X,A10)
                write (lulg,61) unitch( metric + 1 ),flname(i),sfname(i)
   61           format(//,' forecast errors (',a2,') FOR ',A8,2X,A10)
            ELSE
                WRITE (LULG,62) FLNAME(I),SFNAME(I)
   62           FORMAT(//,' FORECAST ERRORS (KT) FOR ',A8,2X,A10)
            ENDIF
C
            WRITE ( LULG, '(7x,8(4x,a3))' ) ( fcstpd(k), k = 0, mtau )
C
            DO K = 1, NMODEL
               WRITE ( LULG, 8 ) MNAME(K), (ERROR(K,L), L = 0, mtau )
            enddo
C
            WRITE ( LULG, 9 ) ( ICASECT(L), L = 0, mtau )
C
        enddo
C
      ENDIF
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  the above section applies to posits and intensity, skipped for wind radii
cx  see the comment section like this one above for start of non-wind radii code
  800 continue 
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
c     ------------------
c     Calculate and print the average errors for each storm
c     for the quadrant radii forecasts
c     ------------------

cx    if (intens == 1 .and. qradalso == 'y' .and. istmpr == 1) then
      if (qradalso == 'y' .and. istmpr == 1) then

        WRITE (LULG,672)
  672   FORMAT(//,' --- QUADRANT WIND RADII FORECAST ERRORS BY '
     &           ,'STORM ---')

        write (lulg,813)
        write (lulg,814)
        write (lulg,815)

        DO 699 I = 1,NDATAF

          ICASE = 0

          DO 678 J = 1,NHCASE(0)
            IF (FLNAME(I).EQ.FHNAME(J)) THEN
              ICASE = ICASE + 1
              DO 675 K = 1,NQRMODEL
                DO 675 L = 0,mtau
                  do isect = 1,mxquad
                    drs(K,ICASE,L,isect) = dr(K,J,L,isect)
                  enddo
  675         CONTINUE
            ENDIF
  678     CONTINUE
C
          DO 680 L = 0,mtau
            DO 680 K = 1,NQRMODEL
              do 680 isect = 1,12
                rerror(K,L,isect) = 0.0
                irerct(K,L,isect) = 0
  680     CONTINUE
C
          DO 685 J = 1,ICASE
            DO 685 K = 1,NQRMODEL
              DO 685 L = 0,mtau
                DO 685 isect=1,mxquad
                  IF (drs(K,J,L,isect).NE.9999.0 .and.
     &                drs(K,J,L,isect).NE.6666.0 .and.
     &                drs(K,J,L,isect).NE.7777.0) then
                    rerror(K,L,isect) = rerror(K,L,isect)
     &                                + drs(K,J,L,isect)
                    irerct(K,L,isect) = irerct(K,L,isect) + 1
                  ENDIF
  685     CONTINUE
C
          DO 690 L = 0,mtau
            DO 690 K = 1,NQRMODEL
              DO 690 isect = 1,mxquad
                IF (irerct(K,L,isect) .ne. 0) THEN
                  rerror(K,L,isect) = rerror(K,L,isect)
     &                               /irerct(K,L,isect)
                ELSE
                  rerror(K,L,isect) = 9999.0
                ENDIF
  690     CONTINUE
C
          WRITE (LULG,692) FLNAME(I),SFNAME(I)
  692     FORMAT(//,' QUADRANT RADII FORECAST ERRORS (NM) FOR '
     &             ,A8,2X,A10)

          do L=0,mtau
            ilhr = ktime(L)
            if (L == 0) then
              write (lulg,820)
            else
              write (lulg,*) ' '
            endif
            do K = 1,nqrmodel
              write (lulg,823) mname(K),ilhr
     &             ,(int(rerror(K,L,isect) + 0.5),isect=1,12)
            enddo
          enddo

  699   CONTINUE

      endif

      if (qradalso == 'y') then
        deallocate (qrad); deallocate(bqrad)
        deallocate (drs);  deallocate(rerror)
        deallocate (dr);   deallocate(drm)
      endif
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  this whole section applies to posits and intensity, skip for wind radii
      if (qradalso == 'y') go to 1000
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx

C
C**   PRINT TRACKS OF ALL POSSIBLE CASES
C
      IF ( ITRKPR .EQ. 1 ) THEN
C
         IF ( INTENS .EQ. 0 ) THEN
            WRITE ( LULG, '(//,'' TRACKS FOR ALL CASES'')' )
         ELSE
            WRITE ( LULG, '(//,'' INTENSITY FOR ALL CASES'')' )
         ENDIF
C
         DO I = 1, NCASE
c
            IF ( INTENS .EQ. 0 ) THEN
c
               WRITE (LULG,71) FNAME(I),SNAME(I),CCDATE(I)
   71          FORMAT (/,1X,A8,2X,A10,2X,A10)
C
               WRITE ( LULG, '(8x,a3,7(10x,a3))' ) 
     &               ( fcstpd(k), k = 0, mtau )
C
               WRITE (LULG,72) (BTLAT(I,K),BTLON(I,K), K=0,mtau)
   72          FORMAT('BTRK',8(F5.1,1X,F5.1,2X))
C
               if (YLAT(1,I,0) >= 0.0 .and. XLON(1,I,0) >= 0.0 .and.
     &             BTLAT(I,0)  > 0.0 .and. BTLON(I,0)  > 0.0 .and.
     &             BTVMAX(I,0) > VMLO .and.
     &             BTVMAX(I,0) > VMVER) then

c     &             YLAT(2,I,0) > 0.0 .and. XLON(2,I,0) > 0.0 .and.

                 compmod = 0

                 do m = 0,mtau

                   if (BTLAT(I,m) > 0.0 .and. BTLON(I,m) > 0.0 .and.
     &                 BTVMAX(I,m) > VMVER) then

                     compmod(0,m) = 1

                     if (YLAT(1,I,m) > 0.0 .and. XLON(1,I,m) > 0.0)
     &               then
                       compmod(1,m) = 1
                     else
                       compmod(1,m) = 0
                     endif

c                     if (YLAT(2,I,m) > 0.0 .and. XLON(2,I,m) > 0.0) then
c                       compmod(2,m) = 1
c                     else
c                       compmod(2,m) = 0
c                     endif

                   else

                     compmod(0,m) = -9
                     compmod(1,m) = -9
                     compmod(2,m) = -9

                   endif

                 enddo

cx               WRITE (LTIM,745) CCDATE(I),FNAME(I)(1:4),'BTRK'
cx   &                           ,(compmod(0,m),m=0,mtau)
                 do j = 1,nmodel
cx                 WRITE (LTIM,745) CCDATE(I),FNAME(I)(1:4),mname(j)
cx   &                             ,(compmod(j,m),m=0,mtau)
                 enddo

               endif

               DO J = 1, NMODEL
                  WRITE (LULG,73) MNAME(J),
     &                             (YLAT(J,I,K),XLON(J,I,K), K = 0,mtau)
   73             FORMAT (A4,8(F5.1,1X,F5.1,2X))
               enddo
c
            ELSE
c
               WRITE (LULG,71) FNAME(I),SNAME(I),CCDATE(I)
C
               WRITE ( LULG, '(7x,a3,7(6x,a3))' ) 
     &               ( fcstpd(k), k = 0, mtau )
C
               WRITE (LULG,74) (BTVMAX(I,K), K = 0,mtau)
   74          FORMAT('BTRK',8(F7.1,2X))
               DO J = 1, NMODEL
                  WRITE (LULG,75) MNAME(J),(VMAX(J,I,K),K = 0,mtau)
   75             FORMAT (A4,8(F7.1,2X))
               enddo
c
            ENDIF
c
         enddo
c
 745  format (1x,a10,3x,a4,3x,a4,4X,8(i4,1X))

cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx  the above section applies to posits and intensity, skipped for wind radii
cx  see the comment section like this one above for start of non-wind radii code
 1000 continue 
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx

C
      ENDIF
C
      CLOSE (LULG)
      CLOSE (LUHV)
C
      STOP ' NORMAL END TO THE TRUTH'
C
C**   ERROR MESSAGES
C
 1010 WRITE (*,*) ' ERROR OPENING input DATA FILE = ',IOS
      WRITE (lulg,*) ' ERROR OPENING input DATA FILE = ',IOS
      STOP
C
 1020 WRITE (*,*) ' ERROR OPENING output DATA FILE = ',IOS
      STOP
C
 1030 WRITE (*,*) ' ERROR OPENNING THE A-DECK = ',IOS
      WRITE (lulg,*) ' ERROR OPENNING THE A-DECK = ',IOS
cx    missing adeck is not a show stopper, there may be others to work with
      go to 10
cx    STOP
C
 1040 WRITE (*,*) ' ERROR OPENNING THE B-DECK = ',IOS
      WRITE (lulg,*) ' ERROR OPENNING THE B-DECK = ',IOS
      STOP
C
 1050 WRITE (*,*) ' ERROR READING THE A-DECK (MAYBE NO CARQ) = ',IOS
      WRITE (lulg,*) ' ERROR READING THE A-DECK (MAYBE NO CARQ) =',IOS
      STOP
C
 1055 WRITE (*,*) 'LINE ',n,' DTG PROBLEM IN A-DECK:',DTGCHECK
      WRITE (lulg,*) 'LINE ',n,' DTG PROBLEM IN A-DECK:',DTGCHECK
      STOP
C
 1056 WRITE (*,*) 'LINE ',n,' DTG TOO FAR FROM INITIAL DTG:',DTGCHECK
      WRITE (lulg,*) 'LINE ',n,' DTG PROBLEM IN A-DECK:',DTGCHECK
      STOP
C
 1060 WRITE (*,*) ' ERROR READING THE B-DECK = ',IOS
      WRITE (lulg,*) ' ERROR READING THE B-DECK = ',IOS
      STOP
C
      END
C***********************************************************************
      FUNCTION TDF (T,DF)
C
      IMPLICIT REAL*8 (A-H,O-Z)
CC    REAL*8 PROB,BETAI,TDF,T,DF
C
      B = 0.5
      PROB = BETAI (0.5 * DF,B,DF / (DF + T**2))
C
      TDF = (PROB - 2.0) / (-2.0)
C
      RETURN
      END
C***********************************************************************
      FUNCTION BETAI(A,B,X)
C
C**   RETURNS THE INCOMPLETE BETA FUNCTION IX(A,B).
C
C**   TAKEN FROM PRESS ET AL., NUMERICAL RECIPES, PP 166FF
C
      IMPLICIT REAL*8 (A-H,O-Z)
CC    REAL*8 BETAI,BT,A,B,X,GAMMLN,BETACF
C
      IF (X.LT.0.0.OR.X.GT.1.0) WRITE (6,'('' BAD ARGUMENT X IN BETAI'')
     & ')
      IF (X.EQ.0.0.OR.X.EQ.1.0) THEN
          BT = 0.0
      ELSE
C
C**   FACTORS IN FORM OF THE CONTINUED FRACTION
C
          BT = EXP (GAMMLN(A + B) - GAMMLN(A) - GAMMLN(B)
     &     + A*LOG(X) + B*LOG(1.0 - X))
        END IF
C
      IF (X.LT.(A + 1.0)/(A + B + 2.0)) THEN
C
C**   USE CONTINUED FRACTION DIRECTLY
C
          BETAI = BT*BETACF(A,B,X)/A
C
          RETURN
      ELSE
C
C**   USE CONTINUED FRACTION AFTER MAKING THE SYMMETRY TRANSFORMATION
C
          BETAI = 1.0 - BT*BETACF(B,A,1.0 - X)/B
C
          RETURN
C
      END IF
C
      END
C***********************************************************************
      FUNCTION BETACF(A,B,X)
C
C**   CONTINUED FRACTION FOR INCOMPLETE BETA FUNCTION, BETAI
C
C**   TAKEN FROM PRESS ET AL., NUMERICAL RECIPES, P. 168
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (ITMAX = 100)
      PARAMETER (EPS = 3.E-27)
C
      AM = 1.0
      BM = 1.0
      AZ = 1.0
C
C**   THESE Q'S WILL BE USED IN FACTORS WHICH OCCUR IN THE
C**     COEFFICIENTS (6.3.6)
C
      QAB = A + B
      QAP = A + 1.0
      QAM = A - 1.0
      BZ  = 1.0 - QAB*X/QAP
C
C**   CONTINUED FRACTION EVALUATION BY THE RECURRENCE METHOD (5.2.5)
C
      DO 10 M = 1,ITMAX
         EM  = M
         TEM = EM + EM
         D   = EM*(B - M)*X/((QAM + TEM)*(A + TEM))
C
C**   ONE STEP (THE EVEN ONE) OF THE RECURRENCE
C
        AP = AZ + D*AM
        BP = BZ + D*BM
        D  = - (A + EM)*(QAB + EM)*X/((A + TEM)*(QAP + TEM))
C
C**   NEXT STEP OF THE RECURRENCE (THE ODD ONE)
C
        APP = AP + D*AZ
        BPP = BP + D*BZ
C
C**   SAVE THE OLD ANSWER
C
        AOLD = AZ
C
C**   RENORMALITIES TO PREVENT OVERFLOWS
C
        AM = AP/BPP
        BM = BP/BPP
        AZ = APP/BPP
        BZ = 1
C
C**   ARE WE DONE?
C
        IF (ABS(AZ - AOLD).LT.EPS*ABS(AZ)) GO TO 20
C
   10 CONTINUE
C
      WRITE (6,'('' A OR B TOO BIG, OR ITMAX TOO SMALL'')')
C
   20 BETACF = AZ
C
      RETURN
      END
C***********************************************************************
      FUNCTION GAMMLN (XX)
C
C**   RETURNS THE VALUE OF LN [GAMMA (XX)] FOR XX > 0
C**     FULL ACCURACY IS OBTAINED FOR XX > 1
C**     FOR 0 < XX < 1, THE REFLECTION FORMULA 6.1.4 CAN BE USED FIRST
C**     TAKEN FROM PRESS ET AL., NUMERICAL RECIPES, PP 156FF
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XX,GAMMLN
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
C
C**   INTERNAL ARITHMETIC WILL BE DONE IN DOUBLE PRECISION, A NICETY
C**     THAT YOU CAN OMIT IF FIVE-FIGURE ACCURACY IS GOOD ENOUGH.
C
      DATA COF /76.18009173D0 ,-86.50532033D0,  24.01409822D0,
     &          -1.231739516D0,   .120858003D-2, -.536382D-5/
      DATA STP / 2.50662827465D0/
      DATA HALF /0.5D0/, ONE /1.0D0/, FPF /5.5D0/
C
      X   = XX - ONE
      TMP = X + FPF
      TMP = (X + HALF)*LOG(TMP) - TMP
      SER = ONE
C
      DO 10 J = 1,6
         X = X + ONE
         SER = SER + COF(J)/X
   10 CONTINUE
C
      GAMMLN = TMP + LOG(STP*SER)
C
      RETURN
      END
cx
cxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcxcx
cx
      subroutine readin(lulg,mxnmd,mxdev,intens,metric,ierrpr,istmpr,
     &          itrkpr,vmhi, vmlo, vmver1, vmver, icincr,
     &          rlathi, rlatlo, rlonlo, rlonhi, startdtg, enddtg, 
     &          int00, int06, int12, int18, ndevs, dname, nmodel, mname)
cx
cx  Read the data at the top of the configuration file.
cx  Individual storms are read in a loop within the main program.
cx
cx  input parameters:
cx                    lulg  - integer, output file lu for run.
cx                    mxnmd - integer, maximum number of fcst models
cx                    mxdev - integer, maximum number of development levels
cx  passed back to main:
cx                    intens - integer, 0=track, 1=intensity, 2=wind radii
cx                    metric - integer, 1=metric, 0=english
cx                    ierrpr - integer, 1=individual storm averages
cx                    istmpr - integer, 1=all storm errors processed
cx                    itrkpr - integer, 1=all lat/lon values processed
cx                    vmhi   - real, upper limit of initial vmax
cx                    vmlo   - real, lower limit of initial vmax
cx                    vmver1 - restrict cases to those verifying best track
cx                             max winds lower than vmver1 (kts).
cx                    vmver -  restrict cases to those verifying best track
cx                             max winds greater than vmver (kts).
cx                    icincr-  intensity threshold for tendencies (kts).         
cx                    rlathi - real, northern limit of verifying latitude
cx                    rlatlo - real, southern limit of verifying latitude
cx                    rlonlo - real, eastern limit of verifying longitude
cx                    rlonhi - real, western limit of verifying longitude
cx                    startdtg - character*10, starting dtg (MMDDHH) of evaluation
cx                    enddtg - character*10, ending dtg (MMDDHH) of evaluation
cx                    int00  - boolean, .true.=include 00 forecasts in evaluation
cx                    int06  - boolean, .true.=include 06 forecasts in evaluation
cx                    int12  - boolean, .true.=include 12 forecasts in evaluation
cx                    int18  - boolean, .true.=include 18 forecasts in evaluation
cx                    ndevs  - integer, number of development levels to evaluate
cx                    dname -  char array,  development levels to evaluate
cx                    nmodel - integer, number of models to evaluate
cx                    mname -  char array,  model ids to evaluate
      integer        intens 
      integer        metric 
      integer        ierrpr, istmpr, itrkpr
      real           vmhi, vmlo, vmver1, vmver, icincr 
      real           rlathi, rlatlo
      character*10   startdtg, enddtg
      logical        int00, int06, int12, int18
      integer        nmodel 
      integer        ndevs 
      character*4    mname(mxnmd)
      character*2    dname(mxdev)
      character*80   line
      integer        ind
      integer        i

cx   this should probably be in the data file too... bs      
      icincr = 10.0

      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
cx    parameter (track or intensity)
      ind = index(line,':')
      intens = 0
      if (line(ind:ind+10) .eq. ': intensity') intens = 1
      if (line(ind:ind+11) .eq. ': wind radii')intens = 2
cx    units (english or metric)
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      metric = 0
      if (line(ind:ind+7) .eq. ': metric') metric = 1
cx    individual storm averages
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      istmpr = 0
      if (line(ind:ind+7) .eq. ': .true.') istmpr = 1
cx    all storm errors processed
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      ierrpr = 0
      if (line(ind:ind+7) .eq. ': .true.') ierrpr = 1
cx    all lat/lon values processed
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      itrkpr = 0
      if (line(ind:ind+7) .eq. ': .true.') itrkpr = 1
cx    initial vmax must be below ....
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      vmhi = 250.0
      read(line(ind+1:ind+4), *) vmhi
cx    initial vmax must be above ....
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      vmlo = 0.0
      read(line(ind+1:ind+4), *) vmlo
cx    verifying vmax must be below vmver1
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      vmver1 = 250.0
      read(line(ind+1:ind+4), *,err=1015) vmver1
cx    verifying vmax must be above vmver
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      vmver = 0.0
      read(line(ind+1:ind+4), *) vmver
cx    verifying lat must be below ....
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      rlathi = 90.0
      read(line(ind+1:ind+4), '(f4.0)',err=1015) rlathi
      if (line(ind+5:ind+5) .eq. 'S') rlathi = -rlathi
cx    verifying lat must be above ....
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      rlatlo = 0.0
      read(line(ind+1:ind+4), '(f4.0)',err=1015) rlatlo
      if (line(ind+5:ind+5) .eq. 'S') rlatlo = -rlatlo
cx    verifying lon must be above (west) of this lon
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      rlonlo = 0.0
      read(line(ind+1:ind+4), '(f4.0)',err=1015) rlonlo
      if (line(ind+5:ind+5) .eq. 'E') rlonlo = 360-rlonlo
cx    verifying lon must be below (east) of this lon
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      rlonhi = 0.0
      read(line(ind+1:ind+4), '(f4.0)',err=1015) rlonhi
      if (line(ind+5:ind+5) .eq. 'E') rlonhi = 360-rlonhi
cx    no dates before this date...
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      startdtg = line(ind+2:ind+7)
cx    no dates after this date...
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      enddtg = line(ind+2:ind+7)
cx    process errors for initial 00 hr
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      int00 = .true.
      read (line(ind+2:ind+7), '(l6)',err=1015) int00
cx    process errors for initial 06 hr
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      int06 = .true.
      read (line(ind+2:ind+7), '(l6)',err=1015) int06
cx    process errors for initial 12 hr
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      int12 = .true.
      read (line(ind+2:ind+7), '(l6)',err=1015) int12
cx    process errors for initial 18 hr
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      int18 = .true.
      read (line(ind+2:ind+7), '(l6)',err=1015) int18
cx    read the number of development levels        
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      read (line(ind+2:ind+4), *,err=1015) ndevs 
cx    read the development level ids     
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      if (ndevs .gt. mxdev) then
            write (lulg, *) ' Too many development levels:',ndevs
            stop
      endif  
      read(line,'(20(a2,1x))',err=1015) (dname(i), i = 1, ndevs)
      ind = index(line,':')
cx    read the number of models        
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      ind = index(line,':')
      read (line(ind+2:ind+4), *,err=1015) nmodel
cx    read the model ids     
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      read(11,'(a80)',err=1015)line
      write (lulg,'(a80)') line
      if (nmodel .gt. mxnmd) then
            write (lulg, *) ' Too many models to evaluate:',nmodel
            stop
      endif  
      read(line,'(10(a4,1x))',err=1015) (mname(i), i = 1, nmodel)
      ind = index(line,':')
cx    read a blank line. read specific storm ids later in code
      read(11,'(a80)',err=1015)line
      return
 1015 continue
      write (lulg, *) ' Unexpected line in configuration file:',line
      stop
      end
c---------------------------------------------------------------
c
c---------------------------------------------------------------
      subroutine load_radii (tauData,temprad,mxquad,loadret)
c
c     ABSTRACT: This subroutine fills a temporary qrad array with
c     the quadrant wind radii values for a single forecast tau
c     for a specific model technique.  The array index for the
c     quadrant radii array goes from 1-12 (as opposed to the more
c     logical way of doing it as (3,4) -- I didn't want to add yet
c     another dimension to this array in the calling program).
c     Indeces are as follows: 1-4 are clockwise, beginning with NE
c     quadrant, for 34kt winds; 5-8 are clockwise same way for
c     50kt winds; 9-12 are clockwise for 64kt winds.
c
c     INPUT:
c     tauData   supplied AID_DATA structure
c     mxquad    max number of quadrant threshold levels (34,50,...)
c               times 4 quadrants per level (so if only using 34,
c               50 and 64, mxquad = 12)
c
c     OUTPUT:
c     temprad   output array of size mxquad containing 4 quadrant
c               radii values at each separate wind threshold
c     loadret   return code from this routine.  If any radii records
c               were found for this time, a 0 is returned.  If radii
c               records could not be found, a 1 is returned.  This
c               has implications in the calling routine.  If records
c               were found but the radii all indicated 0 or -9, that
c               means that a forecast was found but no winds were
c               found at that threshold, which is much different
c               than the case of forecast track radii just not being
c               issued.
c
c     AUTHOR:  Tim Marchok (4/18/2002)
c

      include 'dataformats.inc'
      type ( AID_DATA )  tauData
      character   techsave*4,found_radii*1
      integer     tausave,rindex,loadret,full_circle_val
      real        temprad(mxquad)

      techsave = tauData%aRecord(1)%tech
      tausave  = tauData%aRecord(1)%tau

      found_radii = 'n'
      temprad = 0.0

      ii = 1
      do while (ii <= tauData%numrcrds)

        ! Make sure you are looking at the same model, same tau as
        ! you loop through the successive tauData records....

        if (tauData%aRecord(ii)%tech == techsave .and.
     &      tauData%aRecord(ii)%tau  == tausave) then

          ! Check to see if atcfunix record has wind radii included,
          ! then copy the values if it does.  If it does not, the
          ! values returned to the calling subroutine are just the
          ! default 0 values given to temprad above.

          if (tauData%aRecord(ii)%rad > 0) then
            found_radii = 'y'
            if (tauData%aRecord(ii)%rad ==  34)  istart =  1
            if (tauData%aRecord(ii)%rad ==  35)  istart =  1
            if (tauData%aRecord(ii)%rad ==  50)  istart =  5
            if (tauData%aRecord(ii)%rad ==  64)  istart =  9
            if (tauData%aRecord(ii)%rad ==  65)  istart =  9
            if (tauData%aRecord(ii)%rad == 100)  istart = 13
            rindex=1
            if (tauData%aRecord(ii)%windcode == 'NEQ') then
              do irquad = istart,istart+3
                temprad(irquad) =
     &                     float(tauData%aRecord(ii)%radii(rindex))
                rindex = rindex + 1
              enddo
            else if (tauData%aRecord(ii)%windcode == 'AAA') then
              full_circle_val = tauData%aRecord(ii)%radii(1)
              do irquad = istart,istart+3
                temprad(irquad) = float(full_circle_val)
              enddo
            endif
          endif

        endif

        ii = ii + 1

      enddo

      if (found_radii == 'y') then
        loadret = 0
      else
        loadret = 1
      endif
c
      return
      end

C----------------------------------------------------
C
C----------------------------------------------------
      Function iccheck (vten,ften,icincr)
c
c     This function returns a value of 1 (for a good forecast)
c     or 0 (for a bad or missed forecast) for intensity change.
c     vten is the verification intensity change, and ften is
c     the forecast intensity change.
c
c     NOTE: NHC outputs intensity in increments of 5 kts, but
c     the model forecast data are put out in the atcf file in
c     increments of 1 knot.  However, it's not really valid to
c     consider a storm as intensifying if it only deviates from
c     the previous intensity by 1 knot.  Therefore, we will
c     only consider a forecast as changing intensity if it
c     changes by at least 2 knots from the previous forecast
c     time (even 2 knots is probably not quite enough...)
c
c     UPDATE: The value of the intensity increment is now
c     passed in as a parameter, and is originally passed in
c     through the namelist for the program, in the input
c     real variable ICINCR.

      real vten,ften,icincr

      iccheck = 0

      if (vten > 0.0) then
        if (ften >= icincr) then
          iccheck = 1
          return
        endif
      elseif (vten == 0.0) then
        if (abs(ften) < icincr) then
          iccheck = 1
          return
        endif
      elseif (vten < 0.0) then
        if (ften <= -icincr) then
          iccheck = 1
          return
        endif
      else
        return
      endif

      end

C----------------------------------------------------
C
C----------------------------------------------------
      subroutine calccorr(xdat,ydat,numpts,R2)
c
c     This subroutine is the main driver for a series of
c     other subroutines below this that will calculate the
c     correlation between two input arrays, xdat and ydat.
c
c     INPUT:
c      xdat     array of x (independent) data points
c      ydat     array of y (dependent)   data points
c      numpts   number of elements in each of xdat and ydat
c
c     OUTPUT:
c      R2    R-squared, the coefficient of determination
c
c     xdiff   array of points for xdat - xmean
c     ydiff   array of points for ydat - ymean
c     yestim  array of regression-estimated points
c     yresid  array of residuals (ydat(i) - yestim(i))

      real    xdat(numpts),ydat(numpts)
      real    xdiff(numpts),ydiff(numpts)
      real    yestim(numpts),yresid(numpts)
      real    xmean,ymean,slope,yint,R2


c
      call getmean(xdat,numpts,xmean)
      call getmean(ydat,numpts,ymean)
c
      call getdiff(xdat,numpts,xmean,xdiff)
      call getdiff(ydat,numpts,ymean,ydiff)
c
      call getslope(xdiff,ydiff,numpts,slope)
      yint = ymean - slope * xmean
c
      call getyestim(xdat,slope,yint,numpts,yestim)
      call getresid(ydat,yestim,numpts,yresid)
c
      call getcorr(yresid,ydiff,numpts,R2)
c
      print *,'  i     ydat     xdat    ydiff    xdiff        e'
     &       ,'       e2   ydiff2'
      print *,'----   -----    -----    -----    -----    -----   '
     &       ,' -----    -----'
C      do i = 1,numpts
C        write(6,'(1x,i3,7(2x,f7.2))') i,ydat(i),xdat(i),ydiff(i)
C     &         ,xdiff(i),yresid(i),yresid(i)*yresid(i)
C     &         ,ydiff(i)*ydiff(i)
C      enddo
c
      print *,'----   -----    -----    -----    -----    -----   '
     &       ,' -----    -----'
      write(6,'(a6,f7.2,2x,f7.2)') ' mean ',ymean,xmean

      write (6,*) ' '
      write (6,30) 'slope= ',slope,'         y-intercept = ',yint
  30  format (1x,a7,f7.3,a23,f7.3)
      if (slope .gt. 0.0) then
        write(6,40) 'Regression equation:   Y = ',yint,' + ',slope
      else
        write(6,40) 'Regression equation:   Y = ',yint,' - ',slope
      endif
  40  format (1x,a27,f6.2,a3,f6.2,'X')
c
      print *,' '
      print *,'R2 = ',R2,'   r = ',sqrt(R2)
      print *,' '
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getmean(xarr,inum,zmean)
c
c     This subroutine is part of the correlation calculation,
c     and it simply returns the mean of the input array, xarr.
c
c     INPUT:
c      xarr   input array of data points
c      inum   number of data points in xarr
c
c     OUTPUT:
c      zmean  mean of data values in xarr

      real   xarr(inum)
      real   xsum,zmean
c
      xsum = 0.0
      do i = 1,inum
        xsum = xsum + xarr(i)
      enddo
c
      zmean = xsum / float(MAX(inum,1))
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getdiff(xarr,inum,zmean,zdiff)
c
c     This subroutine is part of the correlation calculation,
c     and it returns in the array zdiff the difference values
c     between each member of the input array xarr and the
c     mean value, zmean.
c
c     INPUT:
c      xarr   input array of data points
c      inum   number of data points in xarr
c      zmean  mean of input array (xarr)
c
c     OUTPUT:
c      zdiff  array containing xarr(i) - zmean

      real xarr(inum),zdiff(inum)
      real zmean
c
      do i = 1,inum
        zdiff(i) = xarr(i) - zmean
      enddo
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getslope(xarr,yarr,inum,slope)
c
c     This subroutine is part of the correlation calculation,
c     and it returns the slope of the regression line.
c
c     INPUT:
c      xarr   input array of xdiffs (x - xmean)
c      yarr   input array of ydiffs (y - ymean)
c      inum   number of points in x & y arrays
c
c     OUTPUT:
c      slope  slope of regression line

      real xarr(inum),yarr(inum)
      real slope,sumxy,sumx2

c     First sum up the xarr*yarr products....

      sumxy = 0.0
      do i = 1,inum
        sumxy = sumxy + xarr(i) * yarr(i)
      enddo

c     Now sum up the x-squared terms....

      sumx2 = 0.0
      do i = 1,inum
        sumx2 = sumx2 + xarr(i) * xarr(i)
      enddo

c     Now get the slope....

      slope = sumxy / sumx2

      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getyestim(xarr,slope,yint,inum,yestim)
c
c     This subroutine is part of the correlation calculation,
c     and it calculates all the predicted y-values using the
c     regression equation that has been calculated.
c
c     INPUT:
c      xarr   array of x data points
c      slope  slope of the calculated regression line
c      yint   y-intercept of the calculated regression line
c      inum   number of input points
c
c     OUTPUT:
c      yestim array of y pts estimated from regression eqn.

      real xarr(inum),yestim(inum)
      real slope,yint
c
      do i = 1,inum
        yestim(i) = yint + xarr(i) * slope
      enddo
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getresid(yarr,yestim,inum,yresid)
c
c     This subroutine is part of the correlation calculation,
c     and it calculates all the residual values between the
c     input y data points and the y-estim predicted y values.
c
c     INPUT:
c      yarr   array of y data points
c      yestim array of y pts estimated from regression eqn.
c      inum   number of input points
c
c     OUTPUT:
c      yresid array of residuals (ydat(i) - yestim(i))

      real yarr(inum),yestim(inum),yresid(inum)
c
      do i = 1,inum
        yresid(i) = yarr(i) - yestim(i)
      enddo
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getcorr(yresid,ydiff,inum,R2)
c
c     This subroutine is part of the correlation calculation,
c     and it does the actual correlation calculation.
c
c     INPUT:
c      yresid array of residuals (ydat(i) - yestim(i))
c      ydiff  array of points for ydat - ymean
c      inum   number of points in the arrays
c
c     OUTPUT:
c      R2     R-squared, the coefficient of determination

      real yresid(inum),ydiff(inum)
      real R2,sumyresid,sumydiff
c
      sumyresid = 0.0
      sumydiff  = 0.0

      do i = 1,inum
        sumyresid = sumyresid + yresid(i) * yresid(i)
        sumydiff  = sumydiff  + ydiff(i) * ydiff(i)
      enddo

      write (6,*)  ' '
      write (6,30) 'Sum of y-residuals squared (e2) = ',sumyresid
      write (6,30) 'Sum of y-diffs squared (ydiff2) = ',sumydiff
      write (6,*)  ' '

  30  format (1x,a35,f10.2)

      R2 = 1 - sumyresid / sumydiff
c
      return
      end

