C-----------------------------------------------------------------------
      SUBROUTINE GRIBIT(gribi,LBM,IDRT,IM,JM,MXBIT,COLAT1,
     &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
     &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,
     &                  INA,INM,ICEN2,IDS,IENS,
     &                  XLAT1,XLON1,DELX,DELY,ORIENT,PROJ,
     &                  GRIB,LGRIB,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GRIBIT      CREATE GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
C
C ABSTRACT: CREATE A GRIB MESSAGE FROM A FULL FIELD.
C   AT PRESENT, ONLY GLOBAL LATLON GRIDS AND GAUSSIAN GRIDS
C   AND REGIONAL POLAR PROJECTIONS ARE ALLOWED.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   94-05-04  JUANG (FOR GSM AND RSM USE)
C   97-09-17  IREDELL  MADE Y2K COMPLIANT
C
C USAGE:    CALL GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
C    &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
C    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,
C    &                  INA,INM,ICEN2,IDS,IENS,
C    &                  XLAT1,XLON1,DELX,DELY,ORIENT,PROJ,
C    &                  GRIB,LGRIB,IERR)
C   INPUT ARGUMENT LIST:
C     F        - REAL (IM*JM) FIELD DATA TO PACK INTO GRIB MESSAGE
C     LBM      - LOGICAL (IM*JM) BITMAP TO USE IF IBMS=1
C     IDRT     - INTEGER DATA REPRESENTATION TYPE
C                (0 FOR LATLON OR 4 FOR GAUSSIAN OR 5 FOR POLAR)
C     IM       - INTEGER LONGITUDINAL DIMENSION
C     JM       - INTEGER LATITUDINAL DIMENSION
C     MXBIT    - INTEGER MAXIMUM NUMBER OF BITS TO USE (0 FOR NO LIMIT)
C     COLAT1   - REAL FIRST COLATITUDE OF GRID IF IDRT=4 (RADIANS)
C     ILPDS    - INTEGER LENGTH OF THE PDS (USUALLY 28)
C     IPTV     - INTEGER PARAMETER TABLE VERSION (USUALLY 1)
C     ICEN     - INTEGER FORECAST CENTER (USUALLY 7)
C     IGEN     - INTEGER MODEL GENERATING CODE
C     IBMS     - INTEGER BITMAP FLAG (0 FOR NO BITMAP)
C     IPU      - INTEGER PARAMETER AND UNIT INDICATOR
C     ITL      - INTEGER TYPE OF LEVEL INDICATOR
C     IL1      - INTEGER FIRST LEVEL VALUE (0 FOR SINGLE LEVEL)
C     IL2      - INTEGER SECOND LEVEL VALUE
C     IYR      - INTEGER 4-DIGIT YEAR
C     IMO      - INTEGER MONTH
C     IDY      - INTEGER DAY
C     IHR      - INTEGER HOUR
C     IFTU     - INTEGER FORECAST TIME UNIT (1 FOR HOUR)
C     IP1      - INTEGER FIRST TIME PERIOD
C     IP2      - INTEGER SECOND TIME PERIOD (0 FOR SINGLE PERIOD)
C     ITR      - INTEGER TIME RANGE INDICATOR (10 FOR SINGLE PERIOD)
C     INA      - INTEGER NUMBER INCLUDED IN AVERAGE
C     INM      - INTEGER NUMBER MISSING FROM AVERAGE
C     ICEN2    - INTEGER FORECAST SUBCENTER
C                (USUALLY 0 BUT 1 FOR REANAL OR 2 FOR ENSEMBLE)
C     IDS      - INTEGER DECIMAL SCALING
C     IENS     - INTEGER (5) ENSEMBLE EXTENDED PDS VALUES
C                (APPLICATION,TYPE,IDENTIFICATION,PRODUCT,SMOOTHING)
C                (USED ONLY IF ICEN2=2 AND ILPDS>=45)
C     XLAT1    - REAL FIRST POINT OF REGIONAL LATITUDE (RADIANS)
C     XLON1    - REAL FIRST POINT OF REGIONAL LONGITUDE (RADIANS)
C     DELX     - REAL DX ON 60N FOR REGIONAL (M)
C     DELY     - REAL DY ON 60N FOR REGIONAL (M)
C     PROJ     - REAL POLAR PROJECTION FLAG 0 FOR NORTH 1 FOR SOUTH
C     ORIENT   - REAL ORIENTATION OF REGIONAL DOMAIN
C
C   OUTPUT ARGUMENT LIST:
C     GRIB     - CHARACTER (LGRIB) GRIB MESSAGE
C     LGRIB    - INTEGER LENGTH OF GRIB MESSAGE
C                (NO MORE THAN 100+ILPDS+IM*JM*(MXBIT+1)/8)
C     IERR     - INTEGER ERROR CODE (0 FOR SUCCESS)
C
C SUBPROGRAMS CALLED:
C   GETBIT     - COMPUTE NUMBER OF BITS AND ROUND DATA APPROPRIATELY
C   W3FI68     - MAKE GENERAL PDS
C   PDSENS     - MAKE ENSEMBLE PDS
C   W3FI72     - ENGRIB DATA INTO A GRIB1 MESSAGE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      use machine
      implicit none
!!
      integer itr,ip2,inm,ina,ihr,idy,ip1,iftu,ids,icen2,ilpds,icen,
     &        iptv,im,idrt,mxbit,jm,il2,il1,imo,iyr,ibms,igen,itl,ipu,
     &        icy,iyc,nbit,nbm,jp2,jp1,ipx,jtr,kclust,nfo,kmembr,
     &        kprob,ilast,iresfl,igrid,iscan,i,ierr,nf,igds11,igds09,
     &        igds10,loni,lon1,lati,igds12,jftu,lat1,lgrib
      real (kind=KIND_IO8) pi,xprob,fmax,fmin
      real (kind=KIND_IO8) proj,xlon1,xlat1,colat1,
     &                     orient,dely,delx
      INTEGER IENS(5)
      real (kind=kind_io4) gribi(im*jm)
      REAL (KIND=KIND_IO8) F(IM*JM)
      LOGICAL(1) LBM(IM*JM)
      CHARACTER GRIB(*)
      INTEGER IBM(IM*JM*IBMS+1-IBMS),IPDS(100),IGDS(100),IBDS(100)
      REAL (KIND=KIND_IO8) FR(IM*JM)
      CHARACTER PDS(ILPDS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE GRID PARAMETERS
      do i=1,im*jm
      f(i)=gribi(i)
      enddo
csela  write(0,*) 'gribit top'
      PI=ACOS(-1.)
      NF=IM*JM
      IF(IDRT.EQ.0) THEN
        IF(IM.EQ.144.AND.JM.EQ.73) THEN
          IGRID=2
        ELSEIF(IM.EQ.360.AND.JM.EQ.181) THEN
          IGRID=3
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3)
        LON1=0
        LATI=NINT(180.E3/(JM-1))
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
      ELSEIF(IDRT.EQ.4) THEN
        IF(IM.EQ.192.AND.JM.EQ.94) THEN
          IGRID=98
        ELSEIF(IM.EQ.384.AND.JM.EQ.190) THEN
          IGRID=126
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3-180.E3/PI*COLAT1)
        LON1=0
        LATI=JM/2
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
      ELSEIF(IDRT.EQ.5) THEN    ! POLAR PROJECTION
        IGRID=255
        IRESFL=0
        ISCAN=2
        LAT1=180.E3/PI*XLAT1
        LON1=180.E3/PI*XLON1
        IGDS09=ORIENT*1.E3
        IGDS10=DELX
        IGDS11=DELY
        IGDS12=PROJ
      ELSE
        IERR=40
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RESET FORECAST HOUR UNITS IN CASE OF OVERFLOW
      JFTU=IFTU
      JP1=IP1
      JP2=IP2
      JTR=ITR
      IPX=MAX(IP1,IP2)
CCC   IF(ITR.GE.2.AND.ITR.LE.5.AND.IPX.GE.256) THEN
CCC     JP1=IP2
CCC     JP2=0
CCC     JTR=10
CCC   ENDIF
      IF(IFTU.EQ.1) THEN
        IF((ITR.GE.2.AND.ITR.LE.5.AND.IPX.GE.256).OR.IPX.GE.65536) THEN
          IF(MOD(IP1,24)+MOD(IP2,24).EQ.0.AND.IPX.LT.24*256) THEN
            JFTU=2
            JP1=IP1/24
            JP2=IP2/24
          ELSEIF(MOD(IP1,12)+MOD(IP2,12).EQ.0.AND.IPX.LT.12*256) THEN
            JFTU=12
            JP1=IP1/12
            JP2=IP2/12
          ELSEIF(MOD(IP1,6)+MOD(IP2,6).EQ.0.AND.IPX.LT.6*256) THEN
            JFTU=11
            JP1=IP1/6
            JP2=IP2/6
          ELSEIF(MOD(IP1,3)+MOD(IP2,3).EQ.0.AND.IPX.LT.3*256) THEN
            JFTU=10
            JP1=IP1/3
            JP2=IP2/3
          ELSE
            JP1=IPX
            JP2=0
            JTR=10
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FIX YEAR AND CENTURY
      IYC=MOD(IYR-1,100)+1
      ICY=(IYR-1)/100+1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL PDS PARAMETERS
      IPDS(01)=ILPDS    ! LENGTH OF PDS
      IPDS(02)=IPTV     ! PARAMETER TABLE VERSION ID
      IPDS(03)=ICEN     ! CENTER ID
      IPDS(04)=IGEN     ! GENERATING MODEL ID
      IPDS(05)=IGRID    ! GRID ID
      IPDS(06)=1        ! GDS FLAG
      IPDS(07)=IBMS     ! BMS FLAG
      IPDS(08)=IPU      ! PARAMETER UNIT ID
      IPDS(09)=ITL      ! TYPE OF LEVEL ID
      IPDS(10)=IL1      ! LEVEL 1 OR 0
      IPDS(11)=IL2      ! LEVEL 2
      IPDS(12)=IYC      ! YEAR
      IPDS(13)=IMO      ! MONTH
      IPDS(14)=IDY      ! DAY
      IPDS(15)=IHR      ! HOUR
      IPDS(16)=0        ! MINUTE
      IPDS(17)=JFTU     ! FORECAST TIME UNIT ID
      IPDS(18)=JP1      ! TIME PERIOD 1
      IPDS(19)=JP2      ! TIME PERIOD 2 OR 0
      IPDS(20)=JTR      ! TIME RANGE INDICATOR
      IPDS(21)=INA      ! NUMBER IN AVERAGE
      IPDS(22)=INM      ! NUMBER MISSING
      IPDS(23)=ICY      ! CENTURY
      IPDS(24)=ICEN2    ! FORECAST SUBCENTER
      IPDS(25)=IDS      ! DECIMAL SCALING
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL GDS AND BDS PARAMETERS
      IGDS(01)=0        ! NUMBER OF VERTICAL COORDS
      IGDS(02)=255      ! VERTICAL COORD FLAG
      IGDS(03)=IDRT     ! DATA REPRESENTATION TYPE
      IGDS(04)=IM       ! EAST-WEST POINTS
      IGDS(05)=JM       ! NORTH-SOUTH POINTS
      IGDS(06)=LAT1     ! LATITUDE OF ORIGIN
      IGDS(07)=LON1     ! LONGITUDE OF ORIGIN
      IGDS(08)=IRESFL   ! RESOLUTION FLAG
      IGDS(09)=IGDS09   ! LATITUDE OF END OR ORIENTATION
      IGDS(10)=IGDS10   ! LONGITUDE OF END OR DX IN METER ON 60N
      IGDS(11)=IGDS11   ! LAT INCREMENT OR GAUSSIAN LATS OR DY IN METER
      IGDS(12)=IGDS12   ! LONGITUDE INCREMENT OR PROJECTION
      IGDS(13)=ISCAN    ! SCANNING MODE FLAGS
      IGDS(14:18)=0     ! NOT USED
      IBDS(1:9)=0       ! BDS FLAGS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL BITMAP AND COUNT VALID DATA.  RESET BITMAP FLAG IF ALL VALID.
      NBM=NF
      IF(IBMS.NE.0) THEN
        NBM=0
        DO I=1,NF
          IF(LBM(I)) THEN
            IBM(I)=1
            NBM=NBM+1
          ELSE
            IBM(I)=0
          ENDIF
        ENDDO
        IF(NBM.EQ.NF) IPDS(7)=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ROUND DATA AND DETERMINE NUMBER OF BITS
csela  write(0,*) 'gribit getbit'
      IF(NBM.EQ.0) THEN
        DO I=1,NF
          FR(I)=0.
        ENDDO
        NBIT=0
      ELSE
        CALL GETBIT(IPDS(7),0,IDS,NF,IBM,F,FR,FMIN,FMAX,NBIT)
C       WRITE(0,'("GETBIT:",4I4,4X,2I4,4X,2G16.6)')
C    &   IPU,ITL,IL1,IL2,IDS,NBIT,FMIN,FMAX
csela  write(66,'(g20.10)') fmin
        IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CREATE PRODUCT DEFINITION SECTION
csela  write(0,*) 'gribit w3fi68'
      CALL W3FI68(IPDS,PDS)
      IF(ICEN2.EQ.2.AND.ILPDS.GE.45) THEN
        ILAST=45
        CALL PDSENS(IENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CREATE GRIB MESSAGE
csela  write(0,*) 'gribit w3fi72'
      CALL W3FI72(0,FR,0,NBIT,1,IPDS,PDS,
     &            1,255,IGDS,0,0,IBM,NF,IBDS,
     &            NFO,GRIB,LGRIB,IERR)
csela  write(0,*) 'gribit end'
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
