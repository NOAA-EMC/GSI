!-------------------------------------------------------
      Program bin2grib1_grapes
!-------------------------------------------------------
!$$$  PROGRAM DOCUMENTATION BLOCK
! PROGRAM:  bin2grib1_grapes  CONVERTS BINARY DATA TO GRIB1
! PRGMMR:   FANGLIN YANG    ORG: EMC/NCEP/NOAA   DATE: Dec 2011
! ABSTRACT: converts CMA GRAPES forecasts in binary format 
!           into GRIB1 format (NCEP GRIB TABLE). Please note 
!           1) input data are first interpolated to standard 181x360
!              grid, and then the north-south direction is reversed;
!           2) total recipitation instead of large-scale precipitation
!              is written out for the convenience of making graphics. 
!

      use machine
      implicit none

      integer, parameter ::  n3d=11          !number of 3D variable   
      integer, parameter ::  n2d=10          !number of 2D variable   
      integer, parameter ::  nvar=n3d+n2d    !number of total variable
      integer, parameter ::  MXBIT=16        !INTEGER MAXIMUM NUMBER OF BITS TO USE 

      character*10  :: cdate            !initial condition time
      integer       :: idate(4)         !hh.mm.dd.yyyy   
      integer       :: IDS(255)         !scaling power of 10 factors
      integer       :: IENS(5)
      integer       :: ITLvar(nvar), IPUvar(nvar)
      CHARACTER(40) :: CFHOUR, CFORM
!     CHARACTER(10) :: vname(nvar)
      integer       :: nlats,nlat,nlon,fhe,fhs,fhout,nfcst
      real (kind=kind_io4), allocatable :: vtmp(:,:), var(:,:,:)
      real (kind=kind_io4), allocatable :: bufr(:)
      LOGICAL(1), allocatable :: LBM(:)
      CHARACTER, allocatable  :: G(:)
      integer, allocatable    :: plev(:)     !pressure (mb) of layers

      integer :: nlev, ns
      integer :: IDS1, ITL, ITHR, IFHR, i, j, ij, IPU, nv, IERR, LG,
     &        k, ITR, IL1, IL2, IDRT, IPTV, ILPDS, IMO, IYR,
     &        IHR, IDA, nt, IFTU, n, nout, IGEN, ICEN, IBMS, ICEN2,
     &        iostat,KH,NDIG
      real(kind=kind_io8) :: COLAT1,FHOUR
      real(kind=kind_io8), parameter :: d00=0.0d0
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!!--isobaric layers (hPa)
!      data plev/ 1000, 925, 850, 700, 600, 500, 400, 
!     &  300, 250, 200, 150, 100, 70, 50, 30, 20, 10/ 
!--variable description
!     data vname /"U_WIND", "V_WIND", "TEMP", "HGT", "SPFH", "CLD_WATER", 
!    &   "CLD_ICE", "CLD_SNOW", "CLD_RAIN", "CLD_GRAUPEL", "W_WIND", 
!    &   "PSFC(pa)", "PSEA(pa)", "RAIN_CONV(kg/m2)", "RAIN_TOTAL(kg/m2)", 
!    &   "TSFC","DOWN_LW","DOWN_SW", "GND_FLX", "LH_FLX", "ZSFC"/
!--GRIB PARAMETR INDEX               
      data IPUvar /33,  34,  11,  7,   51,  153,
     &             178, 171, 170, 179, 40,
     &             1,   2,   63,  61,  11,  205,
     &             204, 155, 121, 7/ 
!--LEVEL INDICATOR 
      data ITLvar /100, 100, 100, 100, 100, 100,
     &             100, 100, 100, 100, 100, 
     &             1,   102, 1,   1,   1,   1,
     &             1,   1,   1,   1/
!---------------------------------------------------------------------

!--get forecast data information   
      read(5,*) cdate              !forecast cycle yyyymmddhh
      read(5,*) fhs,fhe,fhout      !forecast starting and ending hours & output frequency in hours
      read(5,*) nlev, nlats, nlon  !vertical layer, latitudinal and longitudinal points
 
      nlat=nlats+1                 !latitudinal for aligning to poles
      nfcst=(fhe-fhs)/fhout+1      !number of forecasts
      ns=(n3d*nlev+n2d)            !number of 2D slices   
      allocate ( vtmp(nlon,nlats), var(nlon,nlat,ns) )
      allocate ( bufr(nlon*nlat) )
      allocate ( LBM(nlon*nlat) )
      allocate ( G(200+nlon*nlat*(MXBIT+1)/8) )

      allocate ( plev(nlev) )      !pressure (mb) of layers
      read(5,*) (plev(k),k=1,nlev)


!--get forecast cycle initial time 
      if(len_trim(cdate).ne.10) then
        print*,"cdate=",cdate," is incorrect, must be yyyymmddhh, exit"
        stop
      endif
      read(cdate(1:4),'(i4)') idate(4)
      read(cdate(5:6),'(i2)') idate(2)
      read(cdate(7:8),'(i2)') idate(3)
      read(cdate(9:10),'(i2)') idate(1)
      IYR=IDATE(4)
      IMO=IDATE(2)
      IDA=IDATE(3)
      IHR=IDATE(1)
      print*, "forecast cycle: ", IYR,IMO,IDA,IHR

!--initialize GRIB data string
      G=' '
!--define scaling factors, IDSDEF comes from w3 lib
      IDS=0
      CALL IDSDEF(1,IDS)
       IDS(153)=4   !cloud water mixing ratio,missing in IDSDEF
       IDS(178)=4   !cloud ice mixing ratio,missing in IDSDEF
       IDS(171)=4   !cloud snow mixing ratio,missing in IDSDEF
       IDS(170)=4   !cloud rain mixing ratio,missing in IDSDEF
       IDS(179)=4   !cloud graupel mixing ratio,missing in IDSDEF

!--define common GRIB header information 
      IDRT=0     !0 for lat-lon grid, 4 for gaussion grid
      COLAT1=d00 !FIRST COLATITUDE OF GRID IF IDRT=4 (RADIANS)
      IPTV=2     !INTEGER PARAMETER TABLE VERSION 
      ICEN=7     !FORECAST CENTER CODE
      IGEN=82    !MODEL GENERATING CODE
      ICEN2=0    !FORECAST SUBCENTER (USUALLY 0 BUT 1 FOR REANAL OR 2 FOR ENSEMBLE)
      IBMS=0     !BITMAP FLAG (0 FOR NO BITMAP)
      IFTU=1     !INTEGER FORECAST TIME UNIT (1 FOR HOUR)
      IENS=0     !(5) ENSEMBLE EXTENDED PDS VALUES (USED ONLY IF ICEN2=2 AND ILPDS>=45)
      ILPDS=28   !LENGTH OF THE PDS (USUALLY 28) 
      IF(ICEN2.EQ.2) ILPDS = 45


      rewind (10)
      do 100 nt=1,nfcst
       FHOUR=fhs+(nt-1.0)*fhout
       nout=20+nt            

!--open output file as, for instance, "pgbf00"
        KH=NINT(FHOUR)
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
        call BAOPENWT(nout,'pgbf'//CFHOUR,iostat)


!--read in source data and interpolate to nlat*nlon grid,
!  note the north-south direction is resversed.
      do n=1,ns  
        read(10) ((vtmp(i,j),i=1,nlon),j=nlats,1,-1)
        do i=1,nlon
          var(i,1,n)=vtmp(i,1) 
          var(i,nlat,n)=vtmp(i,nlats) 
          do j=2,nlat-1
           var(i,j,n)=0.5*(vtmp(i,j-1)+vtmp(i,j))
          enddo
        enddo
      enddo
!--save total precipitation instead of large-scale precipitation
!  convert surface and sea-level pressure unit from hPa to Pa
      do j=1,nlat
      do i=1,nlon
       var(i,j,n3d*nlev+1)=100*var(i,j,n3d*nlev+1)
       var(i,j,n3d*nlev+2)=100*var(i,j,n3d*nlev+2)
       var(i,j,n3d*nlev+4)=var(i,j,n3d*nlev+4)+var(i,j,n3d*nlev+3)
      enddo
      enddo


!--write out 3D fields 
      do 200 nv=1,n3d
       IPU=IPUvar(nv)    !PARAMETR and UNIT INDICATOR
       ITL=ITLvar(nv)    !LEVEL INDICATOR 
       IDS1=IDS(IPU)     !DECIMAL SCALING
       IFHR=NINT(FHOUR)  !FIRST TIME PERIOD            
       ITHR=NINT(FHOUR)  !SECOND TIME PERIOD (0 FOR SINGLE PERIOD)
       ITR=10            !TIME RANGE INDICATOR (10 FOR SINGLE PERIOD)

      do 250 k=1,nlev
       IL1=plev(k)       !FIRST LEVEL VALUE (0 FOR SINGLE LEVEL)
       IL2=IL1           !SECOND LEVEL VALUE
       n=(nv-1)*nlev+k
       do j=1,nlat 
       do i=1,nlon 
        ij=(j-1)*nlon+i
        bufr(ij)=var(i,j,n)
       enddo
       enddo

!   G    -CHARACTER (LGRIB) GRIB MESSAGE 
!   LG   -INTEGER LENGTH OF GRIB MESSAGE
      call gribit(bufr,LBM,IDRT,nlon,nlat,MXBIT,COLAT1,
     &            ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,
     &            IL1,IL2,IYR,IMO,IDA,IHR,IFTU,IFHR,
     &            ITHR,ITR,0,0,ICEN2,IDS1,IENS,
     &            d00,d00,d00,d00,d00,d00,G,LG,IERR)
       if(IERR.NE.0)print*,'gribit ierr=',ierr,' nv=',nv,' K=',K
       IF(IERR.EQ.0) CALL WRYTE(nout,LG,G)

 250  continue
 200  continue


!--write 2D fields
      do 300 nv=1,n2d
       IPU=IPUvar(n3d+nv)    !PARAMETR and UNIT INDICATOR
       ITL=ITLvar(n3d+nv)    !LEVEL INDICATOR 
       IDS1=IDS(IPU)         !DECIMAL SCALING
       IFHR=NINT(FHOUR)      !FIRST TIME PERIOD            
       ITHR=NINT(FHOUR)      !SECOND TIME PERIOD (0 FOR SINGLE PERIOD)
       IL1=0                 !FIRST LEVEL VALUE (0 FOR SINGLE LEVEL)
       IL2=IL1               !SECOND LEVEL VALUE
       ITR=10                !TIME RANGE INDICATOR (10 FOR SINGLE PERIOD)
!      if(nv.ge.6 .and. nv.le.9) then   !for time-averaged fluxes
!        ITR=3    
!        IFHR=ITHR-6       !assume flux bucket is 6 hours            
!      endif

       n=n3d*nlev+nv
       do j=1,nlat 
       do i=1,nlon 
        ij=(j-1)*nlon+i
        bufr(ij)=var(i,j,n)
       enddo
       enddo

!   G    -CHARACTER (LGRIB) GRIB MESSAGE 
!   LG   -INTEGER LENGTH OF GRIB MESSAGE
      call gribit(bufr,LBM,IDRT,nlon,nlat,MXBIT,COLAT1,
     &            ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,
     &            IL1,IL2,IYR,IMO,IDA,IHR,IFTU,IFHR,
     &            ITHR,ITR,0,0,ICEN2,IDS1,IENS,
     &            d00,d00,d00,d00,d00,d00,G,LG,IERR)
       if(IERR.NE.0)print*,'gribit ierr=',ierr,' 2D nv=',nv
       IF(IERR.EQ.0) CALL WRYTE(nout,LG,G)

 300  continue

      call BACLOSE(nout, iostat)
 100   continue

      deallocate (vtmp,var,bufr,LBM,G)

      end


      
