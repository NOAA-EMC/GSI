!$Id: colocNWPwRad.f90 3331 2013-08-26 13:25:25Z chrisg $
!===============================================================
! Name:    colocNWPwRad.f90
!
!
! Type:    Main Program
!
!
! Description:
!       Program that collocates the NWP gridded data into an
!       orbit-based space using radiance measurements.
!
!
! Modules needed:
!       - CRTM_Module
!       - MWwaterCoeff_Define
!       - CRTM_MWwaterCoeff
!       - misc
!       - Consts
!       - utils
!       - IO_MeasurData
!       - IO_Scene
!       - IO_Misc
!       - GeophCovBkg
!       - ErrorHandling
!       - Preclassif
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara    Original coder
!       07-12-2009      Wanchun Chen           Added GFS branch
!       04-27-2011      Wanchun Chen           Modified to handle any number of files
!       03-14-2013      Kevin Garrett          Modify for CRTM 2.1.1 (for emissivities)
!                                              Ocean and non-ocean emissivity now computed
!                                              in call to ComputeEmiss subroutine
!
!===============================================================

program colocNWPwRad

  !---Use CRTM-provided modules
  USE CRTM_Module
  USE MWwaterCoeff_Define, ONLY: MWwaterCoeff_type
  USE CRTM_MWwaterCoeff

  USE Consts
  USE misc
  USE utils
  USE IO_MeasurData
  USE IO_Scene
  USE IO_Misc
  USE IO_Misc_LE
  USE GeophCovBkg
  USE ErrorHandling
  USE Preclassif
  USE FwdOperator
  USE TuningParams

  implicit none

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM
  !---Different parameters
  INTEGER            :: iu_listrad=20,iu_listnwpsfc=30,iu_listnwpatm=40
  INTEGER            :: nvarSfc,nvarAtm,nAnalys2use,nLay,nLev,sfcTypeIdx
  INTEGER, PARAMETER :: len=256
  INTEGER, PARAMETER :: nlat=181,nlon=360
  REAL,    PARAMETER :: minLatNWP=-90,maxLatNWP=90,minLonNWP=0,maxLonNWP=359
  INTEGER, PARAMETER :: nAbsorb=2
  INTEGER            :: nqc=1
  INTEGER            :: ntime=2
  INTEGER            :: len_file=0

  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: RadFiles,nwpFiles
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: sfcNWPfiles,atmNWPfiles,dumfiles
  REAL,               DIMENSION(:,:,:,:), ALLOCATABLE :: sfcArr,atmArr
  REAL,               DIMENSION(:,:,:),   ALLOCATABLE :: sfcArr0,atmArr0
  REAL,               DIMENSION(:),       ALLOCATABLE :: latNWP,lonNWP
  !---Variables used to interface with CRTM components
  TYPE(CRTM_ChannelInfo_type), DIMENSION(:),   POINTER     :: ChannelInfo
  TYPE(CRTM_Atmosphere_type)                               :: Atmos(1)
  TYPE(CRTM_Surface_type)                                  :: Sfc(1)
  TYPE(CRTM_Options_type)                                  :: Options(1)
  TYPE(CRTM_RTSolution_type),  DIMENSION(:,:), ALLOCATABLE :: RTSolution
  REAL(fp_kind),               DIMENSION(:), ALLOCATABLE   :: Emissivity
  INTEGER                                                  :: nSensors,ichan
  CHARACTER(STRLEN),           DIMENSION(:),   ALLOCATABLE :: CHAR_SensorID
  INTEGER,                     DIMENSION(:),   ALLOCATABLE :: channel_Index
  !---Radiances
  TYPE(MeasurData_type)   :: Ym
  INTEGER                 :: nChan
  !REAL                   :: julHour_Ym
  
  !---Bias Application variables
  INTEGER                                     :: nchanBias,nposBias,errReadingBias
  REAL,           DIMENSION(:),   POINTER     :: cfreq_bias
  REAL,           DIMENSION(:,:), POINTER     :: Bias,Slope,Intercept
  REAL,           DIMENSION(:),   ALLOCATABLE :: tb
  !---Single variables
  CHARACTER(LEN=10)  :: OutFilePrefix
  INTEGER            :: iuMeasur,iuOut,indx,i,ierr,Error_status,allocate_status
  INTEGER            :: nfile,ifile,nprofiles,nProfsYm,iprof,ilat,ilon,itime  
  INTEGER            :: nProfsProcessedOKqc,nProfsProcessedbadqc,nProfsProcessed
  REAL               :: xalt,xcover
  REAL               :: PresBotT,PresTopT,PresTopQ,PresBotQ
  REAL               :: PresBotclwc,PresTopclwc,PresBotciwc,PresTopciwc
  REAL               :: wind_random
  !---Data to compute emissivity
  REAL,       DIMENSION(:), ALLOCATABLE :: Emiss_Adjusted,Emiss2Adjust
  !---NWP-related scene data
  TYPE(Scene_type)                      :: Scene    
  INTEGER,    DIMENSION(:), ALLOCATABLE :: AbsorbID
  INTEGER(2), DIMENSION(:), ALLOCATABLE :: qc
  INTEGER                               :: nFilesNWPsfc,nFilesNWPatm,iSfcTyp,iSfcTypOut
  INTEGER                               :: nLayOut,nLevOut,iSfcTypCRTM,ilev
  INTEGER                               :: nLevsAboveSfc
  INTEGER                               :: SfcClass,month,day
  INTEGER                               :: julDay_NWP
  INTEGER,  DIMENSION(:),   ALLOCATABLE :: idxPLevs2Use
  REAL,     DIMENSION(:),   POINTER     :: pressLayOut,presslevOut
  REAL,     DIMENSION(:,:), ALLOCATABLE :: timeNWP
  REAL,     DIMENSION(:),   ALLOCATABLE :: height,temper,relHum,specHum
  REAL,     DIMENSION(:),   ALLOCATABLE :: clwc,ciwc
  REAL,     DIMENSION(:),   ALLOCATABLE :: temper2Use,relHum2Use !,clwc2Use
  REAL,     DIMENSION(:),   ALLOCATABLE :: level_p,level_p2Use
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_t,layer_q,layer_rh,layer_sh
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_clwc,level_clwc
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_ciwc,level_ciwc
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_rain,level_rain
  REAL                                  :: julHour_NWP,Min_julHour_NWP,Max_julHour_NWP
  REAL                                  :: dLatNWP,dLonNWP,TskPreclass
  REAL                                  :: SkinTemp=DEFAULT_VALUE_REAL
  REAL                                  :: SnwDepth=DEFAULT_VALUE_REAL
  REAL                                  :: WindSp=DEFAULT_VALUE_REAL
  REAL                                  :: SfcPress=DEFAULT_VALUE_REAL
  REAL                                  :: windU=DEFAULT_VALUE_REAL
  REAL                                  :: windV=DEFAULT_VALUE_REAL
  REAL                                  :: SurfTemp=DEFAULT_VALUE_REAL
  REAL                                  :: SurfRH=DEFAULT_VALUE_REAL
  REAL                                  :: SurfMixRatio=DEFAULT_VALUE_REAL
  REAL                                  :: tpw=DEFAULT_VALUE_REAL
  REAL                                  :: clw=DEFAULT_VALUE_REAL
  REAL                                  :: ice=DEFAULT_VALUE_REAL
  REAL                                  :: prate=DEFAULT_VALUE_REAL
  REAL                                  :: ptotal=DEFAULT_VALUE_REAL
  
  INTEGER :: year0,jday0,sec0, month0,day0,hour0  ! starting time 
  INTEGER :: dHourNWP=6  ! NWP file hour step or hour interval
  
  !---Namelist data 
  CHARACTER(LEN=len) :: RadFileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: atmNWPFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: sfcNWPFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=200) :: Coeff_Path
  CHARACTER(LEN=len) :: pathNWPout=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: Topogr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: CovBkgFileAtm=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: LogFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: SpcCoeffFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: TauCoeffFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: CldOptPropFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: BiasFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: TuningFile=DEFAULT_VALUE_STR4
  INTEGER            :: norbits2process=DEFAULT_VALUE_INT
  INTEGER            :: nprofs2process=DEFAULT_VALUE_INT
  INTEGER            :: sensor_id=DEFAULT_VALUE_INT
  INTEGER            :: nwp_source=DEFAULT_VALUE_INT
  
  NAMELIST /ControlNWP/RadFileList,atmNWPFilesList,sfcNWPFilesList,pathNWPout,&
            Topogr,CovBkgFileAtm,norbits2process,LogFile,nprofs2process,sensor_id,&
            nwp_source,Coeff_Path,BiasFile,TuningFile

  !---Initialize variables
  julHour_NWP=-999.
  julDay_NWP=-999
  Min_julHour_NWP = -999.
  Max_julHour_NWP = -999.
  
  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlNWP)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  if( sensor_id .eq. SENSOR_ID_TRMM ) nqc = 14

  !---Set conditional variables
  IF (nwp_source .eq. 1) THEN
      nvarSfc     = 14
      nvarAtm     = 73
      sfcTypeIdx  = 8
      OutFilePrefix  = 'NWP_GDAS'
      nLay = 26
      nLev = nLay+1
      dHourNWP = 6
  ELSE IF (nwp_source .eq. 2) THEN
      nvarSfc     = 6
      nvarAtm     = 91*6+1
      sfcTypeIdx  = 1
      OutFilePrefix  = 'NWP_ECMW'
      nLay = 90
      nLev = nLay+1
      dHourNWP = 6
  ELSE IF (nwp_source .eq. 3) THEN
      nvarSfc     = 13
      nvarAtm     = 42
      sfcTypeIdx  = 1
      OutFilePrefix  = 'NWP_GFS'
      nLay = 21
      nLev = nLay+1
      dHourNWP = 6
  ENDIF
  
  !---Read the file names of radiance data and build output NWP files names
  call ReadList(iu_listrad,trim(RadFileList),RadFiles,nfile,nwpFiles,pathNWPout,trim(OutFilePrefix))
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(radiance files in coloc)')
  nfile=minval((/norbits2process,nfile/))
  !---Read the file names of sfc/atm analyses data 
  call ReadList(iu_listnwpsfc,trim(sfcNWPFilesList),sfcNWPFiles,nFilesNWPsfc,dumFiles,'.','')
  DEALLOCATE(dumFiles)
  call ReadList(iu_listnwpatm,trim(atmNWPFilesList),atmNWPFiles,nFilesNWPatm,dumFiles,'.','')
  DEALLOCATE(dumFiles)
!  IF (nfilesNWPsfc .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(sfc-NWP files in coloc)') 
!  IF (nfilesNWPatm .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(atm-NWP files in coloc)') 
  IF (nfilesNWPatm .ne. nfilesNWPsfc) CALL ErrHandl(ErrorType,Err_InconsNumber,'of # of analyses found atm/sfc') 
  nAnalys2Use=nfilesNWPatm
  ntime = nfilesNWPatm
  
  !-------------------------------------------------------------------
  !   Inilization(s) for RT model                          
  !-------------------------------------------------------------------
  CALL GetSensorInfo(sensor_id,CHAR_SensorID,nSensors)

  ALLOCATE(ChannelInfo(nSensors))
  Error_Status = CRTM_Init(CHAR_SensorID,ChannelInfo,File_Path=Coeff_Path,MWwaterCoeff_File='FASTEM5.MWwater.EmisCoeff.bin')
  IF (Error_Status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Init Failed')
  nchan=SUM(ChannelInfo%n_Channels)
  !---Create Atmophere, Surface and Options structures
  CALL CRTM_Atmosphere_Create(Atmos(1),MAXLAYS,MAXABSORB,MAXCLOUDS,MAXAEROSOL)
  CALL CRTM_Surface_Create(Sfc,nchan)
  CALL CRTM_Options_Create(Options,nchan)
  !---Allocate and Initialize RTSolution structures
  ALLOCATE(RTSolution(nchan,1),STAT=allocate_status)
  IF (allocate_status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'RT Structure Allocation Error')
  CALL CRTM_RTSolution_Create(RTSolution,MAXLAYS)
  !-----------------------------------------------------
  !---Load Tuning Parameters for bias correction purposes
  !-----------------------------------------------------
  call LoadTunParams(1,(/TuningFile/))
 
  !-----------------------------------------------------
  !     Read  pressure-grid from covariance matrix file
  !-----------------------------------------------------
  CALL GetPressFromCovFile(CovBkgFileAtm,nLayOut,pressLayOut,nLevOut,pressLevOut)
 
  !-----------------------------------------------------
  !     Loop-Reading over the gridded analyses files
  !-----------------------------------------------------
  ALLOCATE(level_p(nLev),temper(nLev),relHum(nLev),height(nLev),specHum(nLev), &
       clwc(nLev),ciwc(nLev))
  ALLOCATE(sfcArr(nlat,nlon,nvarSfc,nAnalys2use),atmArr(nlat,nlon,nvarAtm,nAnalys2use),&
       latNWP(nlat),lonNWP(nlon),sfcArr0(nlat,nlon,nvarSfc),atmArr0(nlat,nlon,nvarAtm),&
       layer_t(nLayOut),layer_q(nLayOut),layer_rh(nLayOut),layer_sh(nLayOut),&
       layer_clwc(nLayOut),layer_ciwc(nLayOut),layer_rain(nLayOut),&
       level_clwc(nLevOut),level_ciwc(nLevOut),level_rain(nLevOut),&
       timeNWP(nAnalys2Use,4))
  
  do i=1,ntime

      IF (nwp_source .eq. 1) THEN
        level_p=(/10.,20.,30.,50.,70.,100.,150.,200.,250.,300., &
             350.,400.,450.,500.,550.,600.,650.,700.,750.,800., &
             850.,900.,925.,950.,975.,1000.,1100./)
        call readGDASanalys(sfcNWPFiles(i),sfcArr0(:,:,:),nlat,nlon,nvarSfc)
        call readGDASanalys(atmNWPFiles(i),atmArr0(:,:,:),nlat,nlon,nvarAtm)
        sfcArr(:,:,:,i)=sfcArr0(:,:,:)
        atmArr(:,:,:,i)=atmArr0(:,:,:)
      ELSE IF (nwp_source .eq. 2) THEN
        CALL readECMWFanalys(sfcNWPFiles(i),atmNWPFiles(i),sfcArr0(:,:,:),nvarSfc,1)
        CALL readECMWFanalys(sfcNWPFiles(i),atmNWPFiles(i),atmArr0(:,:,:),nvarAtm,2)
        sfcArr(:,:,:,i)=sfcArr0(nlat:1:-1,:,:)
        atmArr(:,:,:,i)=atmArr0(nlat:1:-1,:,:)
      ELSE IF (nwp_source .eq. 3) THEN
        level_p=(/100.,150.,200.,250.,300.,350.,400.,450.,500.,550.,600.,&
                  650.,700.,750.,800.,850.,900.,925.,950.,975.,1000./)
        call readGFSforcst(sfcNWPFiles(i),sfcArr0(:,:,:),nlat,nlon,nvarSfc)
        call readGFSforcst(atmNWPFiles(i),atmArr0(:,:,:),nlat,nlon,nvarAtm)
        sfcArr(:,:,:,i)=sfcArr0(:,:,:)
        atmArr(:,:,:,i)=atmArr0(:,:,:)
      ENDIF

      !---- nwp file name: gfs_sfc2011-04-09.t00,gdas_sfc2011-04-09.t00,ecmwf_sfc2011-04-09.t00
      len_file = LEN_TRIM(sfcNWPFiles(i))
      indx = len_file - 13
      read(sfcNWPFiles(i)(indx:indx+3),    *) timeNWP(i,1) ! yyyy
      read(sfcNWPFiles(i)(indx+5:indx+6),  *) timeNWP(i,2) ! mm
      read(sfcNWPFiles(i)(indx+8:indx+9),  *) timeNWP(i,3) ! dd
      read(sfcNWPFiles(i)(indx+12:indx+13),*) timeNWP(i,4) ! hh
      
      IF( i .eq. 1 ) THEN
        year0 = INT(timeNWP(i,1))
	month0 = INT(timeNWP(i,2))
	day0 = INT(timeNWP(i,3))
	hour0 = INT(timeNWP(i,4))
	call compJulDay(year0,month0,day0,jday0)
      ENDIF
      
      dLatNWP=(maxLatNWP-minLatNWP)/(nLat-1)
      dLonNWP=(maxLonNWP-minLonNWP)/(nLon-1)
      latNWP(1:nLat:1) = minLatNWP +(/(i-1,i=1,nLat)/)*dLatNWP !+ dLatNWP/2.
      lonNWP(1:nLon:1) = minLonNWP +(/(i-1,i=1,nLon)/)*dLonNWP !+ dLonNWP/2.
      
  enddo

  !-----------------------------------------------------
  !    Initialize some items to go in the scene files
  !-----------------------------------------------------
  ALLOCATE(AbsorbID(nAbsorb),qc(nqc))
  AbsorbID = (/1,3/)
  call ReadBias(BiasFile,nchanBias,nposBias,cfreq_bias,Bias,Slope,Intercept,errReadingBias)

  !-----------------------------------------------------
  !     Loop over the radiance files
  !-----------------------------------------------------
  FilesLoop: DO ifile=1,nfile

      !---Open and Read the header of the measurement(s) file 
      call ReadHdrMeasurmts(RadFiles(ifile),iuMeasur,nProfsYm,Ym)
      nProfiles=minval((/nprofs2process,nProfsYm/))
      !---Allocate emissivity/reflectivity vectors
      ALLOCATE(emissivity(Ym%nchan),Emiss2Adjust(Ym%nChan),Emiss_Adjusted(Ym%nChan),tb(Ym%nChan))
      !---Initialize the output (NWP) scene file and write header
      CALL InitHdrScene(nLevOut,nLayOut,Ym%nChan,Ym%CentrFreq,                 &
          Ym%polar,pressLevOut,pressLayOut,Scene,nLayOut,nLayOut,nLayOut,nLayOut,nLayOut,&
          nAbsorb,AbsorbID,nqc,0,Ym%nPosScan,Ym%nScanLines,0)
      !---Write header of adjusted scene
      CALL WriteHdrScene(iuOut,nwpFiles(ifile),Scene,nProfiles)     
      
      !----Loop over the profiles
      nProfsProcessed      = 0
      nProfsProcessedOKqc  = 0
      nProfsProcessedbadqc = 0
      ProfLoop: DO iprof=1,nprofiles
        qc(:) = 0
        layer_t(1:nLayOut) = -999.
        layer_q(1:nLayOut) = -999.
        layer_clwc(1:nLayOut) = -999.
        layer_ciwc(1:nLayOut) = -999.
        layer_rain(1:nLayOut) = -999.
        !---Read the measurement(s)
        call ReadMeasurmts(iuMeasur,Ym,ierr)
        IF (ierr.ne.0) THEN
           CALL ErrHandl(WarningType,Warn_readInvalid,'')
           EXIT ProfLoop
        ENDIF
        nProfsProcessed = nProfsProcessed+1
        !---Determine the surface type  (from topography)
        !   Note: This type could be (and is) different from GDAS definition of soil type
        call Read_topography(Topogr,Ym%lat,Ym%lon,xalt,iSfcTypOut,xcover,iSfcTypCRTM)
        
        !---FIND CORRESPONDING NWP SCENES AND FILL SCENE STRUCTURE
        IF ( Ym%qc(1) .eq. 0 ) THEN

           !---Determine the corresponding Analysis file(s) -Sfc and Atm-
	   call  determineNWPindex(Ym%lat,Ym%lon,minLatNWP,dLatNWP,minLonNWP,dLonNWP,&
                 Ym%Year,Ym%julDay,Ym%secs,dHourNWP,year0,jday0,hour0,&
		 ilat,ilon,itime,nlat,nlon,ntime)
	     
           !---GDAS Case
           IF (nwp_source .eq. 1) THEN
              !---Initialize some arrays
              relHum(:) = -999
              temper(:) = -999
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,1, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SkinTemp,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,3, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SnwDepth,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,11,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfTemp,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,13,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfRH,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,14,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SfcPress,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,9, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindU,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,10,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindV,qc,dHourNWP)
              DO ilev=1,nlev-1
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,&
                      iLev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,height(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,&
                      nLev-1+iLev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,temper(iLev+1),qc,dHourNWP)
              ENDDO
              DO ilev=1,nlev-6
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,&
                      2*(nLev-1)+iLev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,relHum(iLev+1),qc,dHourNWP)
                 relHum(iLev+1)=maxval((/relHum(iLev+1),0./)) 
              ENDDO
             
              relHum(nLev-4:nLev) = 0. 
              surfMixRatio        = RelHum_to_mixingratio(SurfRH/100.,SurfTemp,SfcPress/100.)
              SfcPress            = SfcPress/100.
              !---Reverse array order to match level_p
              temper  = temper(nLev:1:-1)
              relHum  = relHum(nLev:1:-1)
              !---Find Sfc Press and index only layers above surface
              nLevsAboveSfc = COUNT(level_p(:) .lt. sfcPress)
              ALLOCATE(idxPLevs2Use(nlevsAboveSfc),level_p2Use(nLevsAboveSfc+1),&
                temper2Use(nLevsAboveSfc+1),relHum2Use(nLevsAboveSfc+1))
              temper2Use(:) = -999
              relHum2Use(:) = -999   
              idxPLevs2Use = PACK( (/(i,i=1,SIZE(level_p(:)))/), (level_p(:) .lt. sfcPress))
              level_p2Use(1:nLevsAboveSfc) = level_p(idxPLevs2Use)

              temper2Use(1:nLevsAboveSfc)  = temper(idxPLevs2Use)
              relHum2Use(1:nLevsAboveSfc)  = relHum(idxPLevs2Use)
              level_p2Use(nLevsAboveSfc+1) = sfcPress
              temper2Use(nLevsAboveSfc+1)  = SurfTemp
              relHum2Use(nLevsAboveSfc+1)  = SurfRH
              !---Converting the relative humidity into mixing ratio (not currently being used for interpolation)
              !do i=1,nLev-4
              !   WVmixratio(i)    = RelHum_to_mixingratio(RelHum(i)/100.,Temper(i),level_p(i))
              !enddo
              IF (relHum(5) .lt. 1) relHum(5) = 0.
              IF (relHum(4) .lt. 1) relHum(4) = 0.
              !---Compute layer values
              CALL intrpLevs2Lays(temper2Use(:),level_p2Use(:),nLevOut,pressLevOut,PresBotT,PresTopT,pressLayOut,layer_t,qc)
              CALL intrpLevs2Lays(relHum2Use(4:nLevsAboveSfc+1),level_p2Use(4:nLevsAboveSfc+1),nLevOut,pressLevOut,PresBotQ,&
                   PresTopQ,pressLayOut,layer_rh,qc)
              !---Convert interpolated RH to Mixing Ratio
              DO i=1,nLayOut
                 layer_q(i) = -999
                 IF (layer_rh(i) .gt. 0 .and. layer_t(i) .gt. 0) THEN
                    layer_q(i) = RelHum_to_mixingratio(layer_rh(i)/100.,layer_t(i),PressLayOut(i))
                 ENDIF
              ENDDO
              DEALLOCATE(idxPLevs2Use,level_p2Use,temper2Use,relHum2Use)

           !---ECMWF CASE
           ELSE IF (nwp_source .eq. 2) THEN
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,2,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SfcPress,qc,dHourNWP)  
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,3,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SkinTemp,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,4,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfTemp,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,5,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindU,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,6,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindV,qc,dHourNWP)
              DO ilev=1,nLev
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,ilev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,temper(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,ilev+nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,specHum(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,ilev+2*nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,clwc(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,ilev+3*nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,ciwc(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,ilev+5*nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,level_p(iLev),qc,dHourNWP)
              ENDDO
              CALL intrpLevs2Lays(temper,level_p,nLevOut,pressLevOut,PresBotT,PresTopT,pressLayOut,layer_t,qc)
              CALL intrpLevs2Lays(specHum,level_p,nLevOut,pressLevOut,PresBotQ,PresTopQ,pressLayOut,layer_sh,qc)
              CALL interprofile(clwc,level_p,nLevOut,pressLevOut,PresBotclwc,PresTopclwc,level_clwc,qc)
              CALL interprofile(ciwc,level_p,nLevOut,pressLevOut,PresBotciwc,PresTopciwc,level_ciwc,qc)
              CALL LayersICWC(level_clwc,pressLevOut,layer_clwc) 
              CALL LayersICWC(level_ciwc,pressLevOut,layer_ciwc) 
              !---Convert Specific Humidity to Mixing Ratio
              DO i=1,nLayOut
                 layer_q(i) = -999
                 IF (layer_sh(i) .gt. 0) THEN
                    layer_q(i) = layer_sh(i)/(1-(0.001*layer_sh(i)))
                 ENDIF
              ENDDO
           
           !--- GFS CASE
           ELSE IF (nwp_source .eq. 3) THEN
              !---Initialize some arrays
              temper(:) = -999.
              relHum(:) = -999.
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,2,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SfcPress,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,3,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfTemp,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,3,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SkinTemp,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,5, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,tpw,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,6, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,clw,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,7, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SnwDepth,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,8, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,ice,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,9, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindU,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,10, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindV,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,11,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfRH,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,12,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,prate,qc,dHourNWP)
              CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,sfcArr,13,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,ptotal,qc,dHourNWP)
              DO ilev=1,nlev-1
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,&
                      ilev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,temper(iLev),qc,dHourNWP)
              ENDDO
              DO ilev=1,nlev-1
                 CALL interpolBetw4points(Ym%lat,Ym%lon,Ym%Year,month,day,Ym%secs,atmArr,&
                      21+ilev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,relHum(ilev),qc,dHourNWP)
              ENDDO
              
              !---Compute layer values
              CALL intrpLevs2Lays(temper,level_p,nLevOut,pressLevOut,PresBotT,PresTopT,pressLayOut,layer_t,qc)
              CALL intrpLevs2Lays(relHum,level_p,nLevOut,pressLevOut,PresBotQ,PresTopQ,pressLayOut,layer_rh,qc)
              
              DO i=1,nLayOut
                 layer_q(i) = -999
                 IF (layer_rh(i) .gt. 0 .and. layer_t(i) .gt. 0) THEN
                    layer_q(i) = RelHum_to_mixingratio(layer_rh(i)/100.,layer_t(i),PressLayOut(i))
                 ENDIF
              ENDDO
              
              layer_clwc(1:nLayOut) = clw / nLayOut
              layer_rain(1:nLayOut) = ptotal / nLayOut / 6.0

              SfcPress = SfcPress * 0.01
           ENDIF
  
           windSp = sqrt(WindU**2+WindV**2)
   
           !---- this is for covariance matrix generation only, to make wind speed
           !---- variability in NWP step to make emissivity over ocean more variable.
           !!!!!call random_number(wind_random)
           !!!!!windSp = wind_random * 30.0
   
           iSfcTyp = INT(sfcArr(ilat,ilon,sfcTypeIdx,itime)) 
           !---Determine now the classifier-driven surface type (used in 1DVAR pre-classification)
           !TskPreclass = SkinTemp
           TskPreclass = -999.
           call PreClassSfc(Ym%Year,Ym%julDay,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,&
                iSfcTypOut,sensor_id,Ym%tb,SfcClass,TskPreclass)
           !---Removing values less than zero from the CLWC and CIWC profiles
           DO i=1,nLayOut
              IF (layer_clwc(i) < 0) layer_clwc(i) = 0
              IF (layer_ciwc(i) < 0) layer_ciwc(i) = 0
              IF (layer_rain(i) < 0) layer_rain(i) = 0
           ENDDO
           !---Mask snow over sea-ice scenes, classify as snow type if SnwDepth gt 0
           IF (SfcClass .eq. 2 .and. SnwDepth .gt. 0) SfcClass=3
           IF (SfcClass .eq. 1) SnwDepth=0
           !---Fill-up the scene content before output
           call SetUpScene(pressLevOut,pressLayOut,mean(Ym%angle(1:Ym%nchan)),Scene,       &
                SfcClass,iProf,Ym%lat,Ym%lon,Ym%node,Ym%julDay,Ym%Year,Ym%secs,Ym%iscanPos,&
                Ym%iscanLine,Ym%RelAziAngle,Ym%SolZenAngle)
           Scene%SfcPress                         = SfcPress
           Scene%Temp_lay(1:nLayOut)              = layer_t(1:nLayOut)
           Scene%Absorb_lay(1:nLayOut,Scene%iH2o) = layer_q(1:nLayOut)
           Scene%Absorb_lay(1:nLayOut,Scene%iO3)  = 0.                !O3
           Scene%CLW(1:nLayOut)          = layer_clwc(1:nLayOut)
           Scene%Rain(1:nLayOut)         = layer_rain(1:nLayOut)
           Scene%Snow(1:nLayOut)         = 0.
           Scene%Ice(1:nLayOut)          = 0.
           Scene%Graupel(1:nLayOut)      = layer_ciwc(1:nLayOut)
           Scene%Emiss(1:Ym%nChan)       = 0.5
           Scene%Windsp                  = windSp
           Scene%Tskin                   = SkinTemp
           Scene%SnowDepth               = SnwDepth/10.
           Scene%DeltaT                  = SurfTemp-SkinTemp
           Scene%WindU                   = WindU
           Scene%WindV                   = WindV
           Scene%Refl(1:Ym%nChan)        = 0.5
           Scene%qc(1:nqc)               = qc(1:nqc)
           !-------------------------------------------------------------------------------
           !---Compute the emisivities/reflectivities
           !-------------------------------------------------------------------------------
           Emissivity(1:Ym%nchan)=-999.
           !---Apply Correction to Measurement if Chosen to
           Call apply_bias(Ym%tb,tb,Ym%iscanPos,Ym%nPosScan,nposBias,Bias,Slope,Intercept,Ym%nChan,&
                TunParams(1)%iWhere2Apply,TunParams(1)%iCorrMethod,SfcClass,errReadingBias, &
                TunParams(1)%applyBias_oc_byChan,TunParams(1)%applyBias_ic_byChan,          &
                TunParams(1)%applyBias_ld_byChan,TunParams(1)%applyBias_sn_byChan)
           call ComputeEmiss(Scene,Ym,tb,Atmos,Sfc,Options,RTSolution,ChannelInfo,MWwaterC,Emissivity,LogFile)
           !---Adjust emissivity values for non-ocean
           Emiss2Adjust(1:Ym%nChan) = Emissivity(1:Ym%nChan)
           IF (SfcClass .eq. 0) Emiss_Adjusted(1:Ym%nChan) = Emissivity(1:Ym%nChan)
           IF (SfcClass .ne. 0) call AdjustEmiss(Emiss2Adjust,Emiss_Adjusted,Ym%nChan,Ym%centrfreq,sensor_id)
           !---Write Emissivity and Reflectivity values to scene
           Scene%Emiss(1:Ym%nChan) = Emiss_Adjusted(1:Ym%nChan)
           Scene%Refl(1:Ym%nChan)  = 1.-Emiss_Adjusted(1:Ym%nChan)
           IF(sensor_id .eq. SENSOR_ID_MTSA .and. SfcClass .ne. 0)Scene%Emiss(1:Ym%nChan)=0.8
           IF (ALL(Scene%Emiss .gt. 1) .or. ALL(Scene%Emiss .lt. 0)) THEN
              Scene%Emiss=-999.
              Scene%Refl=-999.
           ENDIF
           IF (ALL(Scene%Temp_Lay .lt. 0)) THEN
              call setSceneEDRs2default(Scene)
              Scene%qc=1
              nProfsProcessedbadqc=nProfsProcessedbadqc+1
           ELSE
              nProfsProcessedOKqc=nProfsProcessedOKqc+1
           ENDIF
        ELSE
           !---FILL SCENE STRUCTURE
           qc(1:nqc) = 1
           !---Determine now the classifier-driven surface type (used in 1DVAR pre-classification)
           TskPreclass = -999.
           call PreClassSfc(Ym%Year,Ym%julDay,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,&
                iSfcTypOut,sensor_id,Ym%tb,SfcClass,TskPreclass)
           call SetUpScene(pressLevOut,pressLayOut,mean(Ym%angle(1:Ym%nchan)),Scene,       &
                SfcClass,iProf,Ym%lat,Ym%lon,Ym%node,Ym%julDay,Ym%Year,Ym%secs,Ym%iscanPos,&
                Ym%iscanLine,Ym%RelAziAngle,Ym%SolZenAngle)
           Scene%qc  = qc
           Scene%lat = Ym%lat
           Scene%lon = Ym%lon
           CALL setSceneEDRs2default(Scene)
           nProfsProcessedbadqc = nProfsProcessedbadqc+1
        ENDIF
        !-------------------------------------------
        !---Output results
        !-------------------------------------------
        CALL WriteScene(iuOut,Scene)
      ENDDO ProfLoop
      write(*,'(A)')'Input  Rad File='//TRIM( RadFiles(ifile) )
      write(*,'(A)')'Output NWP File='//TRIM( nwpFiles(ifile) )
      print *, 'Number of profiles processed (total)	:',nProfsProcessed
      print *, 'Number of profiles processed (OK qc)	:',nProfsProcessedOKqc
      print *, 'Number of profiles processed (failed qc):',nProfsProcessedbadqc
      write(*,*)
!      DEALLOCATE(emissivity,freq,Emiss2Adjust,Emiss_Adjusted,tb)
      DEALLOCATE(emissivity,Emiss2Adjust,Emiss_Adjusted,tb)
      !--- Release memory allocated in ReadHdrMeasurmts of src/lib/io/IO_MeasurData.f90
      DEALLOCATE(Ym%CentrFreq,Ym%Rad,Ym%qc,Ym%Tb,Ym%polar,Ym%angle,Ym%secant_view)
      !--- Release memory allocated in InitHdrScene of src/lib/io/IO_Scene.f90
      CALL DestroyScene(Scene)
      CLOSE(iuOut)
      CLOSE(iuMeasur)
  ENDDO FilesLoop
  
  DEALLOCATE(RadFiles)
  DEALLOCATE(nwpFiles)
  DEALLOCATE(sfcNWPFiles)
  DEALLOCATE(atmNWPFiles)
  !--- Release memory allocated in GetPressFromCovFile of src/lib/InversProcess/GeophCovBkg.f90
  DEALLOCATE(pressLayOut,pressLevOut)
  !--- Release memory allocated in LoadTunParams of src/lib/misc/TunParams.f90 
  DEALLOCATE(TunParams)
  !---Release memory allocated in Allocate_State of RTmodel_Profile_Init.f90 (RTmodel_Initial)
  Error_Status = CRTM_Destroy(ChannelInfo)
  CALL CRTM_RTSolution_Destroy(RTSolution)
  CALL CRTM_Atmosphere_Destroy(Atmos)
  CALL CRTM_Surface_Destroy(Sfc)
  CALL CRTM_Options_Destroy(Options)
  !---Release memory allocated in CRTM_init
  DEALLOCATE(AbsorbID,qc)
  !---Release memory allocated in readBias of src/lib/io/IO_MeasurData.f90
  !DEALLOCATE(cfreq_bias,Bias,Slope,Intercept)
  DEALLOCATE(level_p,temper,relHum,height,specHum,clwc,ciwc)
  DEALLOCATE(sfcArr,atmArr,timeNWP,latNWP,lonNWP,sfcArr0,atmArr0,&
       layer_t,layer_q,layer_rh,layer_sh,layer_clwc,layer_ciwc,layer_rain,&
       level_clwc,level_ciwc,level_rain)
  
  CALL CloseLogFile()

CONTAINS

!===============================================================
! Name:         interpolBetw4points
!
!
! Type:         Subroutine
!
!
! Description:  Performs interpolation between four NWP points
!               to the location of the measurement.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - lat                I              Latitude of measur
!       - lon                I              Longitude
!       - Year               I              Year
!       - month              I              Month
!       - day                I              Day
!       - secs               I              Seconds
!       - Arr                I              Array of NWP data
!       - iparam             I              Index (within Arr) of param
!       - ilat               I              Index of latitude (within Arr)
!       - ilon               I              Index of longitude
!       - itime              I              Index of time
!       - nlat               I              Number of latitude grids
!       - nlon               I              Number of longitude grids
!       - latNWP             I              vector of latitudes of NWP grid 
!       - lonNWP             I              Vector of longitudes of NWP grid
!       - timeNWP            I              Vector of times of NWP fields
!       - outVar             O              Interpolated variable
!       - qc                 I/O            QC associated with variable
!       - qc                 I              hour interval
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       04-28-2011      Wanchun Chen, Added dhour to be flexible
!
!===============================================================

  SUBROUTINE interpolBetw4points(lat,lon,year,month,day,secs,Arr,iparam,idxSfcTyp,&
             ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,outVar,qc,dhour)
    REAl,    DIMENSION(:,:,:,:) :: Arr
    INTEGER                     :: iparam,ilat,ilon,itime,nlat,nlon,iLat0,iLat1,iLon0,iLon1
    INTEGER                     :: year,month,day,idxSfcTyp
    REAL                        :: OutVar,OutVar1,OutVar2,outVarInterInlat(2),lat,lon,secs
    REAL                        :: fracLat,fracLon,latLoc,lonLoc,hourLoc,fracHour
    REAL,    DIMENSION(:)       :: latNWP,lonNWP 
    REAL,    DIMENSION(:,:)     :: timeNWP    
    INTEGER(2), DIMENSION(:)    :: qc
    INTEGER                     :: dhour  ! hour interval or hour step
    
    lonLoc              = lon
    latLoc              = lat
    hourLoc             = secs/3600.
    IF (lonLoc .lt. 0.) LonLoc=lonLoc+360.
    iLat0  = iLat
    iLat1  = iLat+1
    iLon0  = iLon
    iLon1  = iLon+1
    
    if(iLat1 .gt. nlat) iLat1=nlat
    if(iLat1 .lt. 1   ) iLat1=1
    
    if(iLon1 .gt. nlon) iLon1=1
    if(iLon1 .lt. 1   ) iLon1=1
    
    OutVar = DEFAULT_VALUE_REAL
    !---Fractions
    fracLat             = (latLoc-latNWP(iLat0))/(latNWP(iLat1)-latNWP(iLat0))
    fracLon             = (lonLoc-lonNWP(ilon0))/(lonNWP(ilon1)-lonNWP(ilon0))
    fracHour            = (hourLoc-timeNWP(itime,4))/(dhour*1.0)
    
    !----4-points space-interpolation at time index itime
    IF (Arr(ilat0,ilon0,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (Arr(ilat1,ilon0,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (Arr(ilat0,ilon1,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (Arr(ilat1,ilon1,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (qc(1) .eq. 0) THEN 
       outVarInterInlat(1) = Arr(iLat0,iLon0,iparam,itime)+fracLat*               &
            (Arr(iLat1,iLon0,iparam,itime)-Arr(iLat0,iLon0,iparam,itime))
       outVarInterInlat(2) = Arr(ilat0,ilon1,iparam,itime)+fracLat*               &
            (Arr(iLat1,ilon1,iparam,itime)-Arr(ilat0,ilon1,iparam,itime))
       OutVar1             = outVarInterInlat(1)+fracLon*(outVarInterInlat(2)-outVarInterInlat(1))
    ENDIF

    !---Time-interpolation 
    IF (Arr(ilat0,ilon0,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (Arr(ilat1,ilon0,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (Arr(ilat0,ilon1,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (Arr(ilat1,ilon1,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (qc(1) .eq. 0) THEN 
       outVarInterInlat(1) = Arr(iLat0,iLon0,iparam,itime+1)+fracLat*		&
    	    (Arr(iLat1,iLon0,iparam,itime+1)-Arr(iLat0,iLon0,iparam,itime+1))
       outVarInterInlat(2) = Arr(ilat0,ilon1,iparam,itime+1)+fracLat*		&
    	    (Arr(iLat1,ilon1,iparam,itime+1)-Arr(ilat0,ilon1,iparam,itime+1))
       OutVar2  	   = outVarInterInlat(1)+fracLon*(outVarInterInlat(2)-outVarInterInlat(1))
       OutVar		   = OutVar1+fracHour*(OutVar2-OutVar1)
    ENDIF

    RETURN
  END SUBROUTINE interpolBetw4points


!===============================================================
!  to compute hour difference of two times
!   
!  04-28-2011    Wanchun Chen       original coder
!===============================================================

 subroutine hour_diff( year1,jday1,hour1, year2,jday2,hour2, nhour )
   
   !---- input variables
   integer :: year1,jday1,hour1,year2,jday2,hour2
   
   !---- output variables
   integer :: nhour
   
   !---- local variables
   integer :: nday, nleap, year
   
   nday = 0
   nleap = 0
    
   if( year1 .eq. year2 ) then
      nday = jday2 - jday1
   else
      ! how many leap years
      do year = year1, year2-1
	if( ( MOD(year,4) .eq. 0 .and. MOD(year,100) .ne. 0 ) .or. MOD(year,400) .eq. 0 ) then
	  nleap = nleap+1
	endif 
      enddo
      nday = (year2-year1) * 365 + nleap + jday2 - jday1
   endif
   
   nhour = nday * 24 + ( hour2 - hour1 )
   
   return
   
 end subroutine hour_diff


!===============================================================
! Name:         determineNWPindex
!
!
! Type:        Subroutine
!
!
! Description:  Determines the indexes within the NWP field for
!               latitude, longitude, time, etc
!
!
! Arguments:
!
!         Name              Type            Description
!      ---------------------------------------------------
!       - lat                I              Latitude of measur        
!       - lon                I              Longitude of measur
!       - julDay             I              Julian day
!       - Year               I              Year
!       - month              O              Month
!       - day                O              Day
!       - secs               I              seconds
!       - ilat               O              Latitude index
!       - ilon               O              Longitude index
!       - itime              O              Time index
!       - minLatNWP          I              Minimum latitude in NWP grid
!       - dLatNWP            I              Latitude increment in NWP grid
!       - minLonNWP          I              Minimum longitude in NWP grid
!       - dLonNWP            I              Latitude increment in NWP grid
!       - latNWP             I              Latitudes vector in NWP grid
!       - lonNWP             I              Longitudes vector in NWP grid
!       - timeNWP            I              Times vector in NWP grid
!       - nAnalys2use        I              Number of analyses to use
!       - nlat               I              Number of latitudes (not used)
!       - nlon               I              Number of longitudes (not used)
!       - ntime              I              Number of times
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       04-28-2011      Wanchun Chen, Modified to handle any numer case
!
!===============================================================

  SUBROUTINE determineNWPindex(lat,lon,minLatNWP,dLatNWP,minLonNWP,dLonNWP,&
             year,jday,sec,dhour,year0,jday0,hour0,&
             ilat,ilon,itime, nlat,nlon,ntime)
    
    !---- input variables
    REAL     :: lat,lon,minLatNWP,dLatNWP,minLonNWP,dLonNWP ! space info
    INTEGER  :: year,jday           ! time info of year and jday
    REAL     :: sec                 ! time info of seconds
    INTEGER  :: year0,jday0,hour0   ! starting time info
    INTEGER  :: dhour               ! hour interval or hour step
    
    INTEGER  :: nlat,nlon,ntime     ! total number of lat/lon/time
    
    !---- output variables
    INTEGER  :: ilat,ilon,itime
    
    !---- local variable
    INTEGER  :: hour, nhour
    REAL     :: latLoc,lonLoc
    
    !---- Determine the lat/lon indexes (to point to the right grid-point)
    lonLoc = lon
    latLoc = lat
    IF( lonLoc .lt. 0. ) LonLoc = lonLoc+360.
    ilat=int((latLoc-minLatNWP)/dLatNWP)+1
    ilon=int((lonLoc-minLonNWP)/dLonNWP)+1
    
    if(ilat .gt. nlat) ilat=nlat
    if(ilat .lt. 1   ) ilat=1
    
    if(ilon .gt. nlon) ilon=1
    if(ilon .lt. 1   ) ilon=1
    
    !---- Determine the time index (to point to the right time, pick the index of low boundary)
    hour = INT(sec/3600.)
    
    !---- compute hour difference
    call hour_diff(year0,jday0,hour0,year,jday,hour, nhour)
    itime = INT( (nhour*1.0) / (dhour*1.0) ) + 1
   
    if( itime .lt. 1     ) itime = 1
    if( itime .gt. ntime ) itime = ntime
    
    RETURN

  END SUBROUTINE determineNWPindex

end program colocNWPwRad
