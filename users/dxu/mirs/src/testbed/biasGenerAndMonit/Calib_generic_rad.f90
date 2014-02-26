!-------------------------------------------------------------------------------
! To rewrite Calib_generic_rad.pro in fortran 90 to speed up processing.
! The stats show that Fortran is 180 times faster than IDL.
! Originally for one NWP reference data of high res. global it need 6 hours,
! while this fortran only takes 2 minutes!
! 
! Note: minlon and maxlon are not used on purpose, 
!       only need to filter out high/low latitudes, not by longitude.
!
! 10/25/2012  Wanchun Chen      original coder
! 03/14/2013  Kevin Garrett     -added cloud detection/filter for:
!                               n18/n19/metopA/metopB/npp
!-------------------------------------------------------------------------------
Program Calib_generic_rad

  USE Consts
  USE IO_MeasurData
  USE IO_Misc
  USE IO_NOISE
  USE IO_Scene
  USE utils
  USE misc
  USE noise
  USE ErrorHandling
  
  implicit none
  !---- INTRINSIC functions used in this module
  INTRINSIC :: TRIM,SUM
  
  integer,parameter  :: maxTbDiffallowed = 10
  integer,parameter  :: maxclw=0.05
  integer,parameter  :: nbin = 40
  integer            :: maxChan=20,nchan
  integer            :: maxPrf=3000
  
  integer            :: iuNWP,iuFWD,iuFM
  integer            :: nfileNWP,nfileFWD,nfileFM,nfile,ifile
  
  CHARACTER(LEN=256), DIMENSION(:), POINTER :: filesNWP
  CHARACTER(LEN=256), DIMENSION(:), POINTER :: filesFWD
  CHARACTER(LEN=256), DIMENSION(:), POINTER :: filesFM
  
  integer                       :: iscan,ifov,nfov
  integer                       :: iscanTotal,nscanTotal,iprfTotal,nprfTotal
  integer                       :: ichan,itpw,iwind
  
  !---- FM Structure
  TYPE(MeasurData_type)         :: FM
  integer                       :: iFM,nFM,ierrFM
  
  !---- FWD Structure
  TYPE(MeasurData_type)         :: FWD
  integer                       :: iFWD,nFWD,ierrFWD
  
  !---- NWP Structure
  TYPE(Scene_type)              :: NWP
  integer                       :: iNWP,nNWP,ierrNWP
  
  !---- FMSDR variables
  real,dimension(:,:),allocatable    :: latScans
  real,dimension(:,:),allocatable    :: lonScans
  integer,dimension(:,:),allocatable :: cendScans
  integer,dimension(:,:),allocatable :: posScans
  real,dimension(:,:),allocatable    :: angScans
  real,dimension(:,:,:),allocatable  :: tbFMScans
 
  !---- FWD variables
  real,dimension(:,:,:),allocatable  :: tbFWDScans
 
  !---- NWP variables
  real,dimension(:,:),allocatable    :: tpwScans
  real,dimension(:,:),allocatable    :: tskinScans
  integer,dimension(:,:),allocatable :: sfcScans
  real,dimension(:,:),allocatable    :: windScans
  real,dimension(:,:,:),allocatable  :: emScans 
  real,dimension(:,:),allocatable    :: clwScans
  
  !---- output variables
  real,dimension(:,:,:,:),allocatable :: meanBiasReg
  real,dimension(:,:,:,:),allocatable :: meanBiasByHist
  real,dimension(:,:,:,:),allocatable :: meanTbsimu
  real,dimension(:,:,:,:),allocatable :: meanTbmeas
  real,dimension(:,:,:,:),allocatable :: stdv
  real,dimension(:,:,:,:),allocatable :: nPoints
  real,dimension(:,:,:,:),allocatable :: Intercept
  real,dimension(:,:,:,:),allocatable :: Slope

  real,dimension(:,:,:),allocatable :: AnlysTPW
  real,dimension(:,:,:),allocatable :: AnlysTskin
  real,dimension(:,:,:),allocatable :: AnlysEm
  
  real,dimension(:,:),allocatable :: meanTbsimu2Output
  real,dimension(:,:),allocatable :: meanTbmeas2Output
  real,dimension(:,:),allocatable :: meanBiasReg2Output
  real,dimension(:,:),allocatable :: meanBiasByHist2Output
  real,dimension(:,:),allocatable :: Slope2Output
  real,dimension(:,:),allocatable :: Intercept2Output

  real,dimension(:),allocatable :: stdv2Output
  
  real,dimension(:,:),allocatable :: tbmeasScan
  real,dimension(:,:),allocatable :: tbsimuScan
  real,dimension(:,:),allocatable :: tbdiffScan
  real,dimension(:,:),allocatable :: emScan
  
  integer,dimension(:),allocatable :: scanIndex
  
  real,dimension(:),allocatable :: tbmeasScanFiltered
  real,dimension(:),allocatable :: tbsimuScanFiltered
  real,dimension(:),allocatable :: tbdiffScanFiltered
  real,dimension(:),allocatable :: emScanFiltered
  
  real,dimension(:),allocatable :: tpwScanFiltered
  real,dimension(:),allocatable :: tskinScanFiltered
  
  real,dimension(:),allocatable :: scalFact
  real,dimension(:),allocatable :: freqs
  real,dimension(:),allocatable :: badchannel
 
  
  !---- local variables
  real                   :: tpw,meanTbdiffScan
  integer                :: cont1,cont2
  real                   :: mintpw,maxtpw,minwind,maxwind
  integer, parameter     :: ntpw=1,nwind=1
  
  real, dimension(ntpw)  :: mintpws,maxtpws
  real, dimension(nwind) :: minwinds,maxwinds
  
  data mintpws /0.0/
  data maxtpws /100.0/
  data minwinds /0.0/
  data maxwinds /100.0/
  real(kind=8),dimension(0:1) :: coef
  real,dimension(nbin)        :: loc,res
  integer,dimension(nbin)     :: histo_gram
  real                        :: mean4, cont4
  integer                     :: ibin
  
  !---- variables for amsua cloud detection algorithm
  real, dimension(5)          :: ccw=(/8.24, 0.754, 2.265, 2.622, 1.846/)
  real, dimension(5)          :: ctw=(/247.92, 116.270, 73.409, 69.235, 44.177/)
  real                        :: tb23, tb31, ama_sza, mu, p23, p31

  !---- namelist variables
  character(LEN=256) :: fileListNWP
  character(LEN=256) :: fileListFWD
  character(LEN=256) :: fileListFM
  integer            :: iBiasComputMethod=1
  character(LEN=256) :: fileBias
  character(LEN=256) :: fileErr
  integer            :: sensorId
  integer            :: fmType
  real               :: minlat
  real               :: maxlat
  real               :: minlon
  real               :: maxlon
  
  NAMELIST /Namelist_Calib_generic_rad/fileListNWP,fileListFWD,fileListFM,&
           iBiasComputMethod,fileBias,fileErr,sensorId,fmType,&
           minlat,maxlat,minlon,maxlon
  
  !---- read namelist
  READ(*,NML=Namelist_Calib_generic_rad)
  
  call ReadList3(iuFM,TRIM(fileListFM),filesFM,nfileFM)
  call ReadList3(iuFWD,TRIM(fileListFWD),filesFWD,nfileFWD)
  call ReadList3(iuNWP,TRIM(fileListNWP),filesNWP,nfileNWP)
   
  !---- simple check
  if( nfileNWP .ne. nfileFWD .or. nfileNWP .ne. nfileFM ) STOP 
  nfile = nfileNWP
  
  nfov = 0

  !---- set maxChan
  if( sensorId .eq. SENSOR_ID_N18 .or. sensorId .eq. SENSOR_ID_METOPA .or. &
      sensorId .eq. SENSOR_ID_N19 .or. sensorId .eq. SENSOR_ID_METOPB ) then
    maxChan = 20
    if( fmType .eq. 0 ) then 
       nfov = 30
    else
       nfov = 90
    endif
    
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))

    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.9
    scalFact(5)=0.9
    scalFact(6)=0.9
    scalFact(7)=0.9
    scalFact(8)=0.9
    scalFact(9)=0.9
    scalFact(10)=0.9
    scalFact(11)=0.9
    scalFact(12)=0.9
    scalFact(13)=0.9
    scalFact(14)=0.9
    scalFact(15)=0.3
    scalFact(16)=0.3
    scalFact(17)=0.3
    scalFact(18)=0.5
    scalFact(19)=0.5
    scalFact(20)=0.5

    !---flag bad channels by setting array element to 1
    badchannel = 0
    if ( sensorId .eq. SENSOR_ID_METOPA ) badchannel(7) = 1
    if ( sensorId .eq. SENSOR_ID_N19 ) then
       badchannel(8)  = 1
       badchannel(18) = 1
    endif

  else if( sensorId .eq. SENSOR_ID_F16 .or. sensorId .eq. SENSOR_ID_F17 .or. sensorId .eq. SENSOR_ID_F18 ) then
    maxChan = 24

    if( fmType .eq. 0 ) then
       nfov = 30
    else if( fmType .eq. 1 ) then
       nfov = 60
    else if( fmType .eq. 2 ) then
       nfov = 90
    else if( fmType .eq. 4 ) then
       nfov = 180
    else
       write(*,*)'fmType=',fmType
       STOP 'Un-supported fmType for DMSP'
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))

    scalFact(1)=0.3
    scalFact(2)=0.9
    scalFact(3)=0.9
    scalFact(4)=0.9
    scalFact(5)=0.9
    scalFact(6)=0.9
    scalFact(7)=0.9
    scalFact(8)=0.5
    scalFact(9)=0.5
    scalFact(10)=0.5
    scalFact(11)=0.5
    scalFact(12)=0.3
    scalFact(13)=0.3
    scalFact(14)=0.3
    scalFact(15)=0.3
    scalFact(16)=0.3
    scalFact(17)=0.3
    scalFact(18)=0.3
    scalFact(19)=0.5
    scalFact(20)=0.5
    scalFact(21)=0.5
    scalFact(22)=0.5
    scalFact(23)=0.5
    scalFact(24)=0.5

    !---flag bad channels by setting array element to 1
    badchannel = 0
  
  else if( sensorId .eq. SENSOR_ID_NPP ) then
    maxChan = 22
    
    if( fmType .eq. 0 ) then 
       nfov = 32
    else
       nfov = 96
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.9
    scalFact(5)=0.9
    scalFact(6)=0.9
    scalFact(7)=0.9
    scalFact(8)=0.9
    scalFact(9)=0.9
    scalFact(10)=0.9
    scalFact(11)=0.9
    scalFact(12)=0.9
    scalFact(13)=0.9
    scalFact(14)=0.9
    scalFact(15)=0.3
    scalFact(16)=0.3
    scalFact(17)=0.3
    scalFact(18)=0.5
    scalFact(19)=0.5
    scalFact(20)=0.5
    scalFact(21)=0.5
    scalFact(22)=0.5

    !---flag bad channels by setting array element to 1
    badchannel = 0
  
  else if( sensorId .eq. SENSOR_ID_TRMM ) then
    maxChan = 9
    
    if( fmType .eq. -1 ) then 
       nfov = 26
    else if( fmType .eq. 0 ) then 
       nfov = 104
    else if( fmType .eq. 1 ) then 
       nfov = 208
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3
    scalFact(7)=0.3
    scalFact(8)=0.3
    scalFact(9)=0.3

    !---flag bad channels by setting array element to 1
    badchannel = 0
  
  else if( sensorId .eq. SENSOR_ID_GPM ) then
    maxChan = 13
    
    if( fmType .eq. -1 ) then 
       nfov = 26
    else if( fmType .eq. 0 ) then 
       nfov = 104
    else if( fmType .eq. 1 ) then 
       nfov = 208
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3
    scalFact(7)=0.3
    scalFact(8)=0.3
    scalFact(9)=0.3
    scalFact(10)=0.3
    scalFact(11)=0.3
    scalFact(12)=0.3
    scalFact(13)=0.3

    !---flag bad channels by setting array element to 1
    badchannel = 0
  
  else if( sensorId .eq. SENSOR_ID_MTMA ) then
    maxChan = 9
    
    if( fmType .eq. -1 ) then 
       nfov = 60
    else if( fmType .eq. 0 ) then 
       nfov = 240
    else if( fmType .eq. 1 ) then 
       nfov = 480
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3
    scalFact(7)=0.3
    scalFact(8)=0.3
    scalFact(9)=0.3

    !---flag bad channels by setting array element to 1
    badchannel = 0  

  else if( sensorId .eq. SENSOR_ID_MTSA ) then
    maxChan = 6
    
    if( fmType .eq. -1 ) then 
       nfov = 45
    else if( fmType .eq. 0 ) then 
       nfov = 91
    else if( fmType .eq. 1 ) then 
       nfov = 182
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3

    !---flag bad channels by setting array element to 1
    badchannel = 0
  
  else if( sensorId .eq. SENSOR_ID_AMSRE ) then
    maxChan = 12
    nfov = 191
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3
    scalFact(7)=0.3
    scalFact(8)=0.3
    scalFact(9)=0.3
    scalFact(10)=0.3
    scalFact(11)=0.3
    scalFact(12)=0.3

    !---flag bad channels by setting array element to 1
    badchannel = 0  

  else if( sensorId .eq. SENSOR_ID_GCOMW1 ) then
    maxChan = 14
    if( fmType .eq. -1 ) then 
       nfov = 27
    else if( fmType .eq. 0 ) then 
       nfov = 243
    else if( fmType .eq. 1 ) then 
       nfov = 486
    endif
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3
    scalFact(7)=0.3
    scalFact(8)=0.3
    scalFact(9)=0.3
    scalFact(10)=0.3
    scalFact(11)=0.3
    scalFact(12)=0.3
    scalFact(13)=0.3
    scalFact(14)=0.3

    !---flag bad channels by setting array element to 1
    badchannel = 0
  
  else if( sensorId .eq. SENSOR_ID_FY3RI ) then
    maxChan = 10
    nfov = 120
    allocate(scalFact(maxChan))
    allocate(freqs(maxChan))
    allocate(badchannel(maxChan))
    
    scalFact(1)=0.3
    scalFact(2)=0.3
    scalFact(3)=0.3
    scalFact(4)=0.3
    scalFact(5)=0.3
    scalFact(6)=0.3
    scalFact(7)=0.3
    scalFact(8)=0.3
    scalFact(9)=0.3
    scalFact(10)=0.3
    
    !---flag bad channels by setting array element to 1
    badchannel = 0

  endif
  
  if( nfov .eq. 0 ) then
    write(*,*)'Error: nfov=0'
    STOP
  endif
  
  !---- get maxChan and maxPrf vlaues
  nscanTotal = 0
  nprfTotal = 0
  do ifile = 1, nfile
    call getRadNprf(TRIM(filesFM(ifile)),nFM)
    nprfTotal = nprfTotal + nFM
    if( nFM .gt. maxPrf ) maxPrf = nFM
    nscanTotal = nscanTotal + nFM/nfov
  enddo
  
  write(*,*) 'maxChan    =', maxChan
  write(*,*) 'maxPrf     =', maxPrf
  write(*,*) 'nscanTotal =', nscanTotal
  write(*,*) 'nprfTotal  =', nprfTotal
  
  allocate( latScans(1:nscanTotal,1:nfov) )
  allocate( lonScans(1:nscanTotal,1:nfov) )
  allocate( cendScans(1:nscanTotal,1:nfov) )
  allocate( posScans(1:nscanTotal,1:nfov) )
  allocate( angScans(1:nscanTotal,1:nfov) )
  
  allocate( tbFMScans(1:nscanTotal,1:nfov,maxChan) )
  allocate( tbFWDScans(1:nscanTotal,1:nfov,maxChan) )
  allocate( tpwScans(1:nscanTotal,1:nfov) )
  allocate( tskinScans(1:nscanTotal,1:nfov) )
  allocate( sfcScans(1:nscanTotal,1:nfov) )
  allocate( windScans(1:nscanTotal,1:nfov) )
  allocate( emScans(1:nscanTotal,1:nfov,maxChan) )
  allocate( clwScans(1:nscanTotal,1:nfov) )

  !---- Arbitrarily set min/max Latitude/Longitude regardless of namelist values
  minLat = -45.
  maxLat = 45.
  minLon = -180.
  maxLon = 180.

  !---- read FMSDR
  iscanTotal = 0
  iprfTotal = 0

  do ifile = 1, nfile
    
    CALL ReadHdrMeasurmts(TRIM(filesFM(ifile)),iuFM,nFM,FM)
    write(*,*) TRIM(filesFM(ifile))
    write(*,*) 'nFM=', nFM
    
    if( ifile .eq. 1 ) then
      NFOV = FM%nPosScan 
      freqs(:) = FM%CentrFreq(:)
    endif
    
    do iFM = 1, nFM
      CALL ReadMeasurmts(iuFM,FM,ierrFM)

      iprfTotal = iprfTotal + 1
      iscanTotal = (iprfTotal-1)/NFOV + 1
      !iscan = (iFM-1)/NFOV + 1
      ifov = MOD( iFM, NFOV )
      if(ifov .eq. 0 ) ifov = NFOV

      latScans(iscanTotal,ifov)    = FM%lat
      lonScans(iscanTotal,ifov)    = FM%lon
      cendScans(iscanTotal,ifov)   = FM%node
      posScans(iscanTotal,ifov)    = FM%iscanPos
      angScans(iscanTotal,ifov)    = SUM(FM%angle(:))/maxChan
      tbFMScans(iscanTotal,ifov,:) = FM%tb(:)

      !---- amsua/atms cloud detection algorithm
      clwScans(iscanTotal,ifov)    = 0     
      if( sensorId .eq. SENSOR_ID_N18 .or. sensorId .eq. SENSOR_ID_METOPA .or. &
           sensorId .eq. SENSOR_ID_N19 .or. sensorId .eq. SENSOR_ID_METOPB .or. &
           sensorId .eq. SENSOR_ID_NPP ) then
         tb23    = FM%tb(1)
         tb31    = FM%tb(2)
         if (tb23 .ge. 285. .or. tb31 .ge. 285.) cycle
         ama_sza = angScans(iscanTotal,ifov)

         mu = cos(ama_sza*(3.1415926/180.0))
         p23 = log(285.0-tb23)
         p31 = log(285.0-tb31)
         clwScans(iscanTotal,ifov) = mu*(ccw(1)+ccw(2)*p23-ccw(3)*p31-(ccw(4)-ccw(5)*mu)*mu)
         if (clwScans(iscanTotal,ifov) .gt. 20) clwScans(iscanTotal,ifov)=20
         if (clwScans(iscanTotal,ifov) .lt. 0)  clwScans(iscanTotal,ifov)=0
      endif

   enddo
    
    CLOSE(iuFM)
    DEALLOCATE(FM%CentrFreq,FM%Rad,FM%qc,FM%Tb,FM%polar,FM%angle,FM%secant_view)

  enddo
  
  write(*,*)'FM iscanTotal=',iscanTotal
  write(*,*)'nscanTotal   =',nscanTotal
  write(*,*)


  !---- read FWD  
  iscanTotal = 0
  iprfTotal = 0

  do ifile = 1, nfile
    
    CALL ReadHdrMeasurmts(TRIM(filesFWD(ifile)),iuFWD,nFWD,FWD)
    write(*,*) TRIM(filesFWD(ifile))
    write(*,*) 'nFWD=', nFWD
   
    do iFWD = 1, nFWD
      iprfTotal = iprfTotal + 1
      iscanTotal = (iprfTotal-1)/NFOV + 1
      !iscan = (iFWD-1)/NFOV + 1
      ifov = MOD( iFWD, NFOV )
      if(ifov .eq. 0 ) ifov = NFOV
      
      CALL ReadMeasurmts(iuFWD,FWD,ierrFWD)
      
      tbFWDScans(iscanTotal,ifov,:) = FWD%tb(:)
    enddo
    
    CLOSE(iuFWD)
    DEALLOCATE(FWD%CentrFreq,FWD%Rad,FWD%qc,FWD%Tb,FWD%polar,FWD%angle,FWD%secant_view)
    
  enddo
  
  write(*,*)'FWD iscanTotal=',iscanTotal
  write(*,*)


  !---- read NWP
  iscanTotal = 0
  iprfTotal = 0

  fileNWPLoop: do ifile = 1, nfile
    
    CALL ReadHdrScene(iuNWP,TRIM(filesNWP(ifile)),NWP,nNWP)
    
    write(*,*) TRIM(filesNWP(ifile))
    write(*,*) 'nNWP=', nNWP
    
    profileNWPLoop: do iNWP=1,nNWP
      CALL ReadScene(iuNWP,NWP,ierrNWP)
      iprfTotal = iprfTotal + 1
      iscanTotal = (iprfTotal-1)/NFOV + 1
      !iscan = (iNWP-1)/NFOV + 1
      ifov = MOD( iNWP, NFOV )
      if(ifov .eq. 0 ) ifov = NFOV
      
      if(ierrNWP .eq. Warn_EndOfFile)   EXIT  profileNWPLoop
      if(ierrNWP .eq. Warn_readInvalid) CYCLE profileNWPLoop
      if(ierrNWP .ne. 0) CALL ErrHandl(ErrorType,Err_ReadingFile, '. NWP file.')

      !---- tpw need to be computed
      call ComputeTPW( NWP%Pres_lev(1:NWP%nLev),NWP%SfcPress, &
                       NWP%Absorb_lay(1:NWP%nLay,NWP%iH2o),tpw )

      tskinScans(iscanTotal,ifov) = NWP%Tskin
      sfcScans(iscanTotal,ifov)   = NWP%iTypSfc
      windScans(iscanTotal,ifov)  = NWP%WindSp
      tpwScans(iscanTotal,ifov)   = tpw
      emScans(iscanTotal,ifov,:)  = NWP%Emiss(:)

    enddo profileNWPLoop
    
    CLOSE(iuNWP)
    CALL DestroyScene(NWP)
    
  enddo fileNWPLoop
  
  write(*,*)'NWP iscanTotal=',iscanTotal
  write(*,*)


  !-----------------------------------------------------------------------------
  ! now do computation, etc
  !-----------------------------------------------------------------------------
  
  !---- identifiers for stratified variables needed (bias, stdv )
  nchan = maxChan

  allocate( meanBiasReg(nfov,nchan,ntpw,nwind) )
  allocate( meanBiasByHist(nfov,nchan,ntpw,nwind) )
  allocate( meanTbsimu(nfov,nchan,ntpw,nwind) )
  allocate( meanTbmeas(nfov,nchan,ntpw,nwind) )
  allocate( stdv(nfov,nchan,ntpw,nwind) )
  allocate( nPoints(nfov,nchan,ntpw,nwind) )
  allocate( Intercept(nfov,nchan,ntpw,nwind) )
  allocate( Slope(nfov,nchan,ntpw,nwind) )
  
  allocate( AnlysTPW(nfov,ntpw,nwind) )
  allocate( AnlysTskin(nfov,ntpw,nwind) )
  allocate( AnlysEm(nfov,ntpw,nwind) )
  
  allocate( meanTbsimu2Output(nfov,nchan) )
  allocate( meanTbmeas2Output(nfov,nchan) )
  allocate( meanBiasReg2Output(nfov,nchan) )
  allocate( meanBiasByHist2Output(nfov,nchan) )
  allocate( Slope2Output(nfov,nchan) )
  allocate( Intercept2Output(nfov,nchan) )
  allocate( stdv2Output(nchan) )
  
  allocate( tbmeasScan(nscanTotal,nfov) )
  allocate( tbsimuScan(nscanTotal,nfov) )
  allocate( tbdiffScan(nscanTotal,nfov) )
  allocate( emScan(nscanTotal,nfov) )
  
  !---- initialize array
  meanBiasReg = 0.0
  meanBiasByHist = 0.0
  meanTbsimu = 0.0
  meanTbmeas = 0.0
  stdv = 0.0
  nPoints = 0.0
  Intercept = 0.0
  Slope = 0.0
  
  AnlysTPW = 0.0
  AnlysTskin = 0.0
  AnlysEm = 0.0
  
  meanTbsimu2Output = 0.0
  meanTbmeas2Output = 0.0
  meanBiasReg2Output = 0.0
  meanBiasByHist2Output = 0.0
  Slope2Output = 0.0
  Intercept2Output = 0.0
  stdv2Output = 0.0
  
  tbmeasScan = 0.0
  tbsimuScan = 0.0
  tbdiffScan = 0.0
  emScan = 0.0
  
  
  !---- compute scan-dependent and stratified mean bias/stdv/number of points,etc
  allocate( scanIndex(nscanTotal) )
  scanIndex(:) = -1
  
  LoopChan: do ichan = 1, nchan

    if (badchannel(ichan) .eq. 1) CYCLE LoopChan

    tbmeasScan = tbFMScans(:,:,ichan)
    tbsimuScan = tbFWDScans(:,:,ichan)
    tbdiffScan = tbmeasScan - tbsimuScan
    emScan     = emScans(:,:,ichan)

    LoopTpw: do itpw = 1, ntpw

      mintpw = mintpws(itpw)
      maxtpw = maxtpws(itpw)
      
      LoopWind: do iwind = 1, nwind

        minwind = minwinds(iwind)
        maxwind = maxwinds(iwind)
        
        LoopFov: do ifov = 1, nfov
          
          meanTbdiffScan = 0.0
          cont1 = 0
          LoopScan1: do iscan = 1, nscanTotal

          if( sfcScans(iscan,ifov)   .eq. 0       .and. &
              latScans(iscan,ifov)   .ge. minlat  .and. &
              latScans(iscan,ifov)   .le. maxlat  .and. &
              tpwScans(iscan,ifov)   .ge. mintpw  .and. &
              tpwScans(iscan,ifov)   .le. maxtpw  .and. &
              windScans(iscan,ifov)  .ge. minwind .and. &
              windScans(iscan,ifov)  .le. maxwind .and. &
              clwScans(iscan,ifov)   .le. maxclw  .and. &
              tbsimuScan(iscan,ifov) .gt. 0       .and. &
              tbmeasScan(iscan,ifov) .gt. 0 ) then

              meanTbdiffScan = meanTbdiffScan + tbdiffScan(iscan,ifov)
              cont1 = cont1 + 1

            endif
          enddo LoopScan1

	  cont2 = 0
          if( cont1 .gt. 1 ) then  !---- cont1 if branch starts

            meanTbdiffScan = meanTbdiffScan/REAL(cont1)

            meanTbsimu(ifov,ichan,itpw,iwind) = 0.0
            meanTbmeas(ifov,ichan,itpw,iwind) = 0.0
            meanBiasReg(ifov,ichan,itpw,iwind) = 0.0
            nPoints(ifov,ichan,itpw,iwind) = 0.0

            cont2 = 0
            LoopScan2: do iscan = 1, nscanTotal
            if( sfcScans(iscan,ifov)   .eq. 0       .and. &
                latScans(iscan,ifov)   .ge. minlat  .and. &
                latScans(iscan,ifov)   .le. maxlat  .and. &
                tpwScans(iscan,ifov)   .ge. mintpw  .and. &
                tpwScans(iscan,ifov)   .le. maxtpw  .and. &
                windScans(iscan,ifov)  .ge. minwind .and. &
                windScans(iscan,ifov)  .le. maxwind .and. &
                clwScans(iscan,ifov)   .le. maxclw  .and. &
                tbsimuScan(iscan,ifov) .gt. 0       .and. &
                tbmeasScan(iscan,ifov) .gt. 0       .and. &
                ABS( tbdiffScan(iscan,ifov) - meanTbdiffScan ) .le. maxTbDiffallowed ) then            

                meanTbsimu(ifov,ichan,itpw,iwind)  = meanTbsimu(ifov,ichan,itpw,iwind)  + tbsimuScan(iscan,ifov)
                meanTbmeas(ifov,ichan,itpw,iwind)  = meanTbmeas(ifov,ichan,itpw,iwind)  + tbmeasScan(iscan,ifov)
                meanBiasReg(ifov,ichan,itpw,iwind) = meanBiasReg(ifov,ichan,itpw,iwind) + tbdiffScan(iscan,ifov)
                cont2 = cont2 + 1
                scanIndex(cont2) = iscan
            endif
            enddo LoopScan2

          endif  !---- cont1 if branch ends
          
          if( cont2 .gt. 2 ) then  !---- cont2 if branch starts

            meanTbsimu(ifov,ichan,itpw,iwind)  = meanTbsimu(ifov,ichan,itpw,iwind)  / cont2
            meanTbmeas(ifov,ichan,itpw,iwind)  = meanTbmeas(ifov,ichan,itpw,iwind)  / cont2
            meanBiasReg(ifov,ichan,itpw,iwind) = meanBiasReg(ifov,ichan,itpw,iwind) / cont2

            allocate( tbsimuScanFiltered(cont2) )
            allocate( tbmeasScanFiltered(cont2) )
            allocate( tbdiffScanFiltered(cont2) )
            allocate( emScanFiltered(cont2) )
            allocate( tpwScanFiltered(cont2) )
            allocate( tskinScanFiltered(cont2) )

            tbsimuScanFiltered = 0.0
            tbmeasScanFiltered = 0.0
            tbdiffScanFiltered = 0.0
            emScanFiltered = 0.0
            tpwScanFiltered = 0.0
            tskinScanFiltered = 0.0

            LoopInnerScan: do iscan=1,cont2
              tbsimuScanFiltered(iscan)  = tbsimuScan(scanIndex(iscan),ifov)
              tbmeasScanFiltered(iscan)  = tbmeasScan(scanIndex(iscan),ifov)
              tbdiffScanFiltered(iscan)  = tbdiffScan(scanIndex(iscan),ifov)
              emScanFiltered(iscan)      = emScan(scanIndex(iscan),ifov)
              tpwScanFiltered(iscan)     = tpwScans(scanIndex(iscan),ifov)
              tskinScanFiltered(iscan)   = tskinScans(scanIndex(iscan),ifov)
            enddo LoopInnerScan

            stdv(ifov,ichan,itpw,iwind)    = stdev2(tbdiffScanFiltered,cont2)
            nPoints(ifov,ichan,itpw,iwind) = cont2

            !---- y=a0 + a1 * x, poly fit
            call polyFit(REAL(tbmeasScanFiltered,8),REAL(tbsimuScanFiltered,8),cont2,1,coef)
            Intercept(ifov,ichan,itpw,iwind) = coef(0)
            Slope(ifov,ichan,itpw,iwind)     = coef(1)

            !---- Bias computation by histogram adjustment
            call histogram( cont2, REAL(tbdiffScanFiltered,8), nbin, histo_gram, loc )
            res = REAL(histo_gram,8)/REAL(maxval(histo_gram),8)*100.0

            mean4 = 0.0
            cont4 = 0.0
            do ibin = 1, nbin
              if( res(ibin) .ge. 80 ) then
                  mean4 = mean4 + loc(ibin)
                  cont4 = cont4 + 1.0
              endif 
            enddo

	    if( cont4 .ge. 1.0 ) then
              mean4 = mean4/cont4
            else
              mean4 = 0.0
            endif

            meanBiasByHist(ifov,ichan,itpw,iwind) = mean4

            deallocate( tbsimuScanFiltered )
            deallocate( tbmeasScanFiltered )
            deallocate( tbdiffScanFiltered )
            deallocate( emScanFiltered )
            deallocate( tpwScanFiltered )
            deallocate( tskinScanFiltered )

          endif !---- cont2 if branch ends

        enddo LoopFov

       enddo LoopWind

    enddo LoopTpw

  enddo LoopChan
  

  !---- Fill-in Bias/Std file(s)
  do ichan = 1, nchan
    do ifov = 1, nfov
      meanTbsimu2Output(ifov,ichan)     = SUM(meanTbsimu(ifov,ichan,1:ntpw,1:nwind))/(ntpw*nwind)
      meanTbmeas2Output(ifov,ichan)     = SUM(meanTbmeas(ifov,ichan,1:ntpw,1:nwind))/(ntpw*nwind)
      meanBiasReg2Output(ifov,ichan)    = SUM(meanBiasReg(ifov,ichan,1:ntpw,1:nwind))/(ntpw*nwind)
      meanBiasByHist2Output(ifov,ichan) = SUM(meanBiasByHist(ifov,ichan,1:ntpw,1:nwind))/(ntpw*nwind)
      Slope2Output(ifov,ichan)          = SUM(Slope(ifov,ichan,1:ntpw,1:nwind))/(ntpw*nwind)
      Intercept2Output(ifov,ichan)      = SUM(Intercept(ifov,ichan,1:ntpw,1:nwind))/(ntpw*nwind)
    enddo
    stdv2Output(ichan) = SUM(stdv(1:nfov,ichan,1:ntpw,1:nwind))/(ntpw*nwind*nfov)*scalFact(ichan)
  enddo
  
  !---- temporary fix for Metop-A channel 7
  if( sensorId .eq. 2 ) then
    meanTbsimu2Output(:,7) = 0.0
    meanTbmeas2Output(:,7) = 0.0
    meanBiasReg2Output(:,7) = 0.0
    meanBiasByHist2Output(:,7) = 0.0
    Slope2Output(:,7) = 0.0
    Intercept2Output(:,7) = 0.0
    stdv2Output(7) = 0.0
  endif

  !---- write out bias correction file and model error file
  if( iBiasComputMethod .eq. 0 ) then
    CALL WriteBias2(fileBias,nchan,nfov,freqs,meanBiasReg2Output,Slope2Output,&
                    Intercept2Output,meanTbsimu2Output,meanTbmeas2Output)
  else
    CALL WriteBias2(fileBias,nchan,nfov,freqs,meanBiasByHist2Output,Slope2Output,&
                    Intercept2Output,meanTbsimu2Output,meanTbmeas2Output)
  endif 
  
  open(77,file=fileErr)
  write(77,'(a25,i8)' ) 'nChan =',nchan
  write(77,'(a25)'    ) 'CentrFreq ='
  write(77,'(10f10.3)') freqs(1:nchan)
  write(77,'(a25)'    ) 'Error ='
  write(77,'(10f10.3)') stdv2Output(1:nchan)
  close(77)
  
  deallocate( latScans )
  deallocate( lonScans )
  deallocate( cendScans )
  deallocate( posScans )
  deallocate( angScans )
  
  deallocate( tbFMScans )
  deallocate( tbFWDScans )
  deallocate( tpwScans )
  deallocate( tskinScans )
  deallocate( sfcScans )
  deallocate( windScans )
  deallocate( emScans )
  deallocate( clwScans )
  
  deallocate( meanBiasReg )
  deallocate( meanBiasByHist )
  deallocate( meanTbsimu )
  deallocate( meanTbmeas )
  deallocate( stdv )
  deallocate( nPoints )
  deallocate( Intercept )
  deallocate( Slope )
  
  deallocate( AnlysTPW )
  deallocate( AnlysTskin )
  deallocate( AnlysEm )
  
  deallocate( meanTbsimu2Output )
  deallocate( meanTbmeas2Output )
  deallocate( meanBiasReg2Output )
  deallocate( meanBiasByHist2Output )
  deallocate( Slope2Output )
  deallocate( Intercept2Output )
  deallocate( stdv2Output )
  
  deallocate( tbmeasScan )
  deallocate( tbsimuScan )
  deallocate( tbdiffScan )
  deallocate( emScan )
 
  deallocate( scanIndex )

  deallocate( filesFM )
  deallocate( filesNWP )
  deallocate( filesFWD)
  
  deallocate( scalFact )
  deallocate( freqs )
  deallocate( badchannel )

  CONTAINS
  
  !-----------------------------------------------------------------------------
  !
  ! Function and Subroutine Sections
  !
  !-----------------------------------------------------------------------------
  
  !---- compute standard deviation 
  REAL FUNCTION Stdev2(X,n_elts)
       INTEGER            :: n_elts
       REAL, DIMENSION(:) :: X
       REAL               :: mean
       mean = SUM(X)/REAL(n_elts)
       Stdev2 = SQRT(SUM((X-mean)**2)/REAL(n_elts-1))
  RETURN
  END FUNCTION Stdev2

  !---- just read, the main calling procedue should deallocate files
  SUBROUTINE ReadList3(iu,fileList,files,nfile)
    !----Input/Output variables
    INTEGER                                 :: iu,nfile
    CHARACTER(LEN=*)                        :: fileList
    CHARACTER(LEN=*), DIMENSION(:), POINTER :: files
    
    !----Local variables
    CHARACTER(LEN=256)                      :: xfile
    INTEGER                                 :: ifile
    
    !---- Open file and establish the number of files present in the list
    iu=Get_Lun()
    !write(*,*) 'iu=', iu
    
    open(iu,file=trim(fileList),status='old',form='formatted')
    nfile=0
    DO While ( .true. )
       read(iu,'(A)',err=100,end=100) xfile
       !write(*,*) nfile, xfile
       nfile=nfile+1
    ENDDO
100 CONTINUE
    close(iu)
    
    !---- Allocate memory and read again the files names into array
    ALLOCATE(files(nfile))
    open(iu,file=TRIM(fileList),status='old',form='formatted')
    DO ifile=1,nfile
       read(iu,'(A)') xfile
       files(ifile) = TRIM(ADJUSTL(xfile))
    ENDDO
    close(iu)
    return
  END SUBROUTINE ReadList3

  !---------------------------------------------------------------
  !  input 
  !  x, y   - input array
  !  n      - number of elements in x and y
  !  m      - Y = a(0) + a(1).X + a(2).X^2 + ... + a(m).X^m
  !  
  !  output
  !  coef    - power coefficients
  !---------------------------------------------------------------
  subroutine polyFit(x,y,n,m,  coef)

    USE lsq
    IMPLICIT NONE

    integer                 :: n, m
    real(dp),dimension(N)   :: x, y
    real(dp),dimension(0:m) :: coef


    REAL(dp) :: xrow(0:20), wt = 1.0_dp, beta(0:20), var, covmat(231), sterr(0:20), totalSS
    INTEGER  :: i, ier, j
    LOGICAL  :: fit_const = .TRUE., lindep(0:20)


    !---- Least-squares calculations

    CALL startup(m, fit_const)
    DO i = 1, n
      xrow(0) = 1.0_dp
      DO j = 1, m
        xrow(j) = x(i) * xrow(j-1)
      END DO
      CALL includ(wt, xrow, y(i))
    END DO

    CALL sing(lindep, ier)
    IF (ier /= 0) THEN
      DO i = 0, m
        IF (lindep(i)) WRITE(*, '(a, i3)') ' Singularity detected for power: ', i
        !IF (lindep(i)) WRITE(9, '(a, i3)') ' Singularity detected for power: ', i
      END DO
    END IF

    ! Calculate progressive residual sums of squares
    CALL ss()
    var = rss(m+1) / (n - m - 1)

    ! Calculate least-squares regn. coeffs.
    CALL regcf(beta, m+1, ier)

    ! Calculate covariance matrix, and hence std. errors of coeffs.
    CALL cov(m+1, var, covmat, 231, sterr, ier)

    !WRITE(*, *) 'Power  Coefficient          Std.error      Resid.sum of sq.'
    DO i = 0, m
      !WRITE(*, '(i4, g20.12, "   ", g14.6, "   ", g14.6)') i, beta(i), sterr(i), rss(i+1)
      coef(i) = beta(i)
    END DO

    !WRITE(*, *)
    !WRITE(*, '(a, g20.12)') ' Residual standard deviation = ', SQRT(var)
    totalSS = rss(1)
    !WRITE(*, '(a, g20.12)') ' R^2 = ', (totalSS - rss(m+1))/totalSS

    return
  end subroutine polyFit


  !---- mimic IDL histogram function
  subroutine histogram ( n, a, nbin, histo_gram, locations )

    implicit none

    integer ( kind = 4 ) nbin
    integer ( kind = 4 ) n

    real ( kind = 8 ) a(n)
    real ( kind = 8 ) a_hi
    real ( kind = 8 ) a_lo
    real ( kind = 8 ) delta
    integer ( kind = 4 ) i
    integer ( kind = 4 ) j

    real,dimension(nbin) :: locations
    real,dimension(nbin) :: bins

    integer ( kind = 4 ) histo_gram(nbin)
    histo_gram(1:nbin) = 0

    a_lo = MINVAL(a)
    a_hi = MAXVAL(a)

    delta = ( a_hi - a_lo ) / ( nbin - 1 ) 

    !write(*,*) a_lo, a_hi, delta

    do i = 1, nbin
      bins(i) = a_lo + (i-1) * delta
    enddo
    locations(:) = bins(:)

    do i = 1, n

      j = INT( ( nbin*(a(i)-a_lo) + (a_hi-a(i)) ) / ( a_hi - a_lo ) )

      if( j .le. 0 ) then
         histo_gram(1) = histo_gram(1) + 1
      else if( j .gt. nbin ) then
         histo_gram(nbin) = histo_gram(nbin) + 1
      else
         histo_gram(j) = histo_gram(j) + 1
      endif

    end do

    return
  end subroutine HISTOGRAM
  
  
  !---- to be consistent with IDL, ichan starts at index 0
  SUBROUTINE WriteBias2(BiasFile,nchan,npos,cfreq,Bias,Slope,Intercept,tbSimu,tbMeas)
    
    CHARACTER(LEN=*)                   :: BiasFile
    INTEGER                            :: nchan,npos,iu,ichan
    REAL,            DIMENSION(:)      :: Cfreq
    REAL,            DIMENSION(:,:)    :: Bias,Slope,Intercept,tbSimu,tbMeas
    CHARACTER(LEN=17)                  :: fmt
    
    fmt(1:9)='(i4,f8.3,'
    if(npos .lt. 10 ) then
      write(fmt(10:10),'(I1)') npos
      fmt(11:15)='f7.2)'
    else if ( npos .ge. 10  .and. npos .lt. 100 ) then
      write(fmt(10:11),'(I2)') npos
      fmt(12:16)='f7.2)'
    else if ( npos .gt. 100 ) then
      write(fmt(10:12),'(I3)') npos
      fmt(13:17)='f7.2)'
    endif
    
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=BiasFile,form='formatted',status='unknown')
    write(iu,'(2i4)') nchan,npos
    !----Bias
    DO ichan=1,nchan
       write(iu,TRIM(fmt)) ichan-1,cfreq(ichan),bias(1:npos,ichan)
    ENDDO
    !----Slope
    DO ichan=1,nchan
       write(iu,TRIM(fmt)) ichan-1,cfreq(ichan),slope(1:npos,ichan)
    ENDDO
    !----Intercept
    DO ichan=1,nchan
       write(iu,TRIM(fmt)) ichan-1,cfreq(ichan),intercept(1:npos,ichan)
    ENDDO
    !----tbSimu
    DO ichan=1,nchan
       write(iu,TRIM(fmt)) ichan-1,cfreq(ichan),tbSimu(1:npos,ichan)
    ENDDO
    !----tbMeas
    DO ichan=1,nchan
       write(iu,TRIM(fmt)) ichan-1,cfreq(ichan),tbMeas(1:npos,ichan)
    ENDDO
    CLOSE(iu)
    RETURN
  END SUBROUTINE WriteBias2
  
End Program Calib_generic_rad
