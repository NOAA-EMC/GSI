!$Id:$
!===============================================================
! Name:  pp1dvar     
!
!
! Type:         Main Program
!
!
! Description:
!       Main retrieval program. Used for microwave sensors in general.
!
! Modules needed:
!       - misc
!       - utils
!       - CntrlParams
!       - TuningParams
!       - SeFeErrCov
!       - GeophCovBkg   
!       - IO_MeasurData 
!       - IO_Scene      
!       - IO_Monitor    
!       - IO_Noise      
!       - QCchecking    
!       - Preclassif    
!       - Noise         
!       - FwdOperator   
!       - VarOprs       
!       - Consts        
!       - ErrorHandling 
!       -         
!       - CRTM_Module   
!       - Type_Kinds    
!       - Error_Handler 
!       - Profile_Init  
!       - RTmodel_Init
!       - forward_model 
!       - CRTM_SpcCoeff 
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

MODULE pp1dvar

  !---CRTM-provided Utility modules
  USE CRTM_Module

  USE Consts
  USE misc,  ONLY: Read_topography,EstimatSfcPress,mean,ShrkMatrx,ShrkVect,compJulDay,CountClassTypes
  USE utils
  USE CntrlParams
  USE TuningParams
  USE SeFeErrCov
  USE GeophCovBkg
  USE IO_MeasurData
  USE IO_Scene
  USE IO_Monitor
  USE IO_Noise
  USE IO_InstrConfig
  USE QCchecking
  USE Preclassif
  USE Noise
  USE FwdOperator
  USE VarOprs
  USE ErrorHandling

  use mpeu_util, only: getindex
  use kinds, only: i_kind
  use crtm_interface, only: channelinfo

  !---Everything is explicitly declared
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ABS,ADJUSTL,ANY,CPU_TIME,EXP,MAXVAL,SUM
  PUBLIC :: pp1dvar_main

  CONTAINS


SUBROUTINE pp1dvar_main(mype,init_pass,sensor,satId,meas,lat,lon,angle,iscanpos,day,month,year,fgt,fgq,fgp,fgsst,fgwindsp,&
        satqc,chisquare,clw,rwp,gwp,emissivity,tbf,nprofiles)
!SUBROUTINE pp1dvar_main(mype,satId,sensor,meas,lat,lon,angle,iscanpos,day,month,year,satqc,chisquare,clw,rwp,gwp,emissivity,tbf,nprofiles)

  !-------------------------------------------------------------------
  !   Variables declaration section
  !-------------------------------------------------------------------
  !---In/Out Arguments
!  INTEGER                          :: day,month,year,satId,sensor,iscanpos
!  REAL, DIMENSION(:)               :: meas
!  REAL, DIMENSION(:,:)             :: satqc
!  REAL                             :: lat, lon, angle, chisquare

  INTEGER(i_kind), intent(in)      :: mype
  LOGICAL,         intent(in)      :: init_pass
  INTEGER                          :: satId   !sensor gsi kidsat
  CHARACTER(*)                     :: sensor  !sensor string name
  INTEGER, DIMENSION(:)            :: day, month, year, iscanpos
  REAL,    DIMENSION(:,:)          :: fgt,fgq,fgp
  REAL,    DIMENSION(:)            :: fgsst,fgwindsp
  REAL,    DIMENSION(:)            :: chisquare, lat, lon, angle, clw, rwp, gwp
  REAL,    DIMENSION(:,:)          :: meas,emissivity,tbf
  INTEGER(2), DIMENSION(:,:)       :: satqc



  CHARACTER(STRLEN),    DIMENSION(:), ALLOCATABLE   :: SensorID
  CHARACTER(LEN=256)                                :: namelistfile,OUTPUTFILE,TXTOUTPUTFILE,OUTPUTFILEFG
  CHARACTER(LEN=256)                               :: stringappend
  INTEGER                                           :: nSensors
  INTEGER,              PARAMETER                   :: nqc=4
  INTEGER                                           :: julday
  INTEGER,              DIMENSION(:), ALLOCATABLE   :: node
  !---Measurement/Modeling error covariances
  REAL,                 DIMENSION(:,:), ALLOCATABLE :: Se,Fe
  REAL,                 DIMENSION(:,:), ALLOCATABLE :: SeStar,FeStar,ModelErrTot
  REAL,                 DIMENSION(:),   ALLOCATABLE :: ModelErrCFreq,ModelErr
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: ShrkRadVec,ChanSel
  REAL,                 DIMENSION(:),   ALLOCATABLE :: NoiseRMS
  !---Covariance/background/Transf matrx
  REAL,                 DIMENSION(:,:), ALLOCATABLE     :: SaAtm,SaSfc,Sa
  REAL,                 DIMENSION(:,:,:,:), ALLOCATABLE :: SaAtmTot,SaSfcTot
  REAL,                 DIMENSION(:,:), ALLOCATABLE     :: UAtm,USfc,U
  REAL,                 DIMENSION(:,:,:,:), ALLOCATABLE :: UAtmTot,USfcTot
  REAL,                 DIMENSION(:),   ALLOCATABLE     :: XbAtm,XbSfc,Xb
  REAL,                 DIMENSION(:,:,:),   ALLOCATABLE :: XbAtmTot,XbSfcTot
  REAL,                 DIMENSION(:),   ALLOCATABLE :: DX,DXtilda,DXtildaNew
  CHARACTER(LEN=10),    DIMENSION(:),   ALLOCATABLE :: ParamLabel
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: ParamIndx,ParamLength
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: iSpaceModeFlag,nGsel
  !---Geophysical and retrieval space vectors & 1st Guess(es)
  REAL,                 DIMENSION(:,:), ALLOCATABLE :: Xg
  REAL,                 DIMENSION(:,:), ALLOCATABLE :: Ustar,Lambda,opt_lay
  REAL,                 DIMENSION(:),   ALLOCATABLE :: Xout
  REAL,                 DIMENSION(:),   ALLOCATABLE :: X1stAtm,X1stSfc,X1st
  REAL,                 DIMENSION(:),   ALLOCATABLE :: EmissAnalyt
  TYPE(Scene_type)                                  :: Scene_Ext,Scene_ExtIn
  TYPE(Scene_type)                                  :: Scene
  !---Retrieval process & QC -related items
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: nIterTot,nAttempTot
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: EDR_nEOF,EDR_cntrlRetr
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: EDR_ExtDataUse
  REAL,                 DIMENSION(:),   ALLOCATABLE :: ChiSq
  REAL,                 DIMENSION(:,:), ALLOCATABLE :: G,A,S
  INTEGER                                           :: mxiter,nConvProfs
  INTEGER                                           :: nProfsOverOcean,nProfsOverSeaIce
  INTEGER                                           :: nProfsOverLand,nProfsOverSnow
  INTEGER,              DIMENSION(:),   ALLOCATABLE :: TBqc
  !---Radiances/Jacobians/Noise
  TYPE(InstrConfig_type)                            :: InstrConfig
  TYPE(MeasurData_type)                             :: Ym      
  REAL,                 DIMENSION(:,:), ALLOCATABLE :: Y,K,Kstar,Ktilda
  REAL,                 DIMENSION(:),   ALLOCATABLE :: Ystar,YmStar
  REAL,                 DIMENSION(:),   ALLOCATABLE :: dY2,dnWelling_rad,upWelling_rad
  !---CRTM-specific items 
!  TYPE(CRTM_ChannelInfo_type), DIMENSION(:),   ALLOCATABLE :: ChannelInfo2
  TYPE(CRTM_Atmosphere_type)                               :: Atmos(1)
  TYPE(CRTM_Atmosphere_type),  DIMENSION(:,:), ALLOCATABLE :: Atmos_K
  TYPE(CRTM_Surface_type)                                  :: Sfc(1)
  TYPE(CRTM_Surface_type),     DIMENSION(:,:), ALLOCATABLE :: Sfc_K
  TYPE(CRTM_Options_type)                                  :: Options(1)
  TYPE(CRTM_RTSolution_type),  DIMENSION(:,:), ALLOCATABLE :: RTSolution,RTSolution_K
  !---Timing-related variables
  REAL,     DIMENSION(20)                           :: Tim=0.
  REAL                                              :: t0,t1,t00
  !--Bias-related variables
  REAL,               DIMENSION(:),    POINTER     :: Cfreq_Bias
  REAL,               DIMENSION(:,:),  POINTER     :: Bias,Slope,Intercept
  REAL,               DIMENSION(:),    ALLOCATABLE :: tb
  INTEGER                                          :: nchanBias,nposBias,errReadingBias
  !---Various scalar variables
  REAL    :: xalt,xcover,trx,tpw1,TskPreclass
  INTEGER :: iuMeasur,iuExtern,iuOutput,iuMonitor,Error_status,allocate_status
  INTEGER :: nSfcTypes,nprofiles,nMeasurData,nExternData,nAtm,nSfc,nG,nR,nChan,nch
  INTEGER :: nchanModelErr,nprofsEff=0
  INTEGER :: i,iprof,iAtm,iSfc,stypeSfc,stypeSfcCRTM,AtmClass,SfcClass,iattempt
  INTEGER :: nEDRs,nEDRs_atm,nEDRs_sfc,ierr,iTer,ichan,istat
  INTEGER :: iEDR_clw,iEDR_rain,iEDR_snow,iEDR_ice,iEDR_grpl
  INTEGER :: iEDR_temp,iEDR_wvap,iEDR_ozon,iEDR_emis,iEDR_refl,iEDR_SfcP,iEDR_SfcT
  INTEGER :: nEDRselec,nGselec,nG0,nLev,nLay,nLayEff,UseorNotExterData
  LOGICAL :: OkRetrvl,CvgceReached
  INTEGER :: ilun,iuedr,iufg,ilay,guess_nlay,lay_start
  REAL, DIMENSION(100) :: nwp_t_guess, nwp_q_guess
  REAL                 :: sfcp_guess
  INTEGER :: checkSupport

  !-------------------------------------------------------------------
  !   Loading off-line databases and control parameters 
  !-------------------------------------------------------------------
  t00= 0.
  
  checkSupport=0
  IF (sensor .eq. 'atms' .and. satId .eq. 224) checkSupport=1 
  IF (sensor .eq. 'ssmis' .and. satId .eq. 286) checkSupport=1

  IF (checkSupport .eq. 0 .or. nprofiles .lt. 1) RETURN 

  !Satid value based on read_bufrtovs
  namelistfile='intializechar'
  iuOutput=mype
  
   write(stringappend,'(i4)') iuOutput
  stringappend=ADJUSTL(stringappend)




  IF (satId .eq. 209 .and. sensor .eq. 'amsua') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/n18_amsua_1dvar.in'
  IF (satId .eq. 223 .and. sensor .eq. 'amsua') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/n19_amsua_1dvar.in'
  IF (satId .eq. 4 .and. sensor .eq. 'amsua') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/metopa_amsua_1dvar.in'
  IF (satId .eq. 5 .and. sensor .eq.'amsua') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/metopb_amsua_1dvar.in'

  IF (satId .eq. 209 .and. sensor .eq. 'mhs') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/n18_mhs_1dvar.in'
  IF (satId .eq. 223 .and. sensor .eq. 'mhs') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/n19_mhs_1dvar.in'
  IF (satId .eq. 4 .and. sensor .eq. 'mhs') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/metopa_mhs_1dvar.in'
  IF (satId .eq. 5 .and. sensor .eq. 'mhs') namelistfile='/data/kgarrett/tools/mirs_trunk/data/ControlData/metopb_mhs_1dvar.in'



  IF (satId .eq. 224 .and. sensor .eq. 'atms') THEN
        namelistfile='/scratch2/portfolios/NESDIS/drt/save/Kevin.Garrett/tools/mirs_trunk/data/ControlData/npp_1dvar.in'
        OUTPUTFILE=trim('/scratch2/portfolios/NESDIS/drt/noscrub/Kevin.Garrett/ptmp/pr1dvar/1dvaredr_npp_'//stringappend)
        OUTPUTFILEFG=trim('/scratch2/portfolios/NESDIS/drt/noscrub/Kevin.Garrett/ptmp/pr1dvar/1dvarguess_npp_'//stringappend)
        iuedr=get_lun()
        iuedr=iuedr+mype+1000
        iufg=get_lun()
        iufg=iufg+mype+1100
  ENDIF   

  IF (satId .eq. 286 .and. sensor .eq. 'ssmis') THEN 
        namelistfile='/scratch2/portfolios/NESDIS/drt/save/Kevin.Garrett/tools/mirs_trunk/data/ControlData/f18_ssmis_1dvar.in'
        OUTPUTFILE=trim('/scratch2/portfolios/NESDIS/drt/noscrub/Kevin.Garrett/ptmp/pr1dvar/1dvaredr_f18_'//stringappend)
        OUTPUTFILEFG=trim('/scratch2/portfolios/NESDIS/drt/noscrub/Kevin.Garrett/ptmp/pr1dvar/1dvarguess_f18_'//stringappend)
        iuedr=get_lun()
        iuedr=iuedr+mype+1200
        iufg=get_lun()
        iufg=iufg+mype+1300
  ENDIF   


  call LoadCntrlParams_sub(namelistfile)
  !call OpenLogFile(CntrlConfig_sub%logFile) 
  call LoadTunParams(CntrlConfig_sub%nAttempts,CntrlConfig_sub%TuningFile(1:CntrlConfig_sub%nAttempts),mype)



  call LoadGlobGeophyCovBkg()            !---Load the global/stratified cov matrix -Atm & Sfc- as well as  U
  call ReadInstrConfig(CntrlConfig_sub%InstrConfigFile,InstrConfig)  !---Read instrconfig file for frequencies
  call LoadNoise(CntrlConfig_sub%NoiseFile)  !---Load Noise Info and related items (NRF, NEDT, etc)
  !------------------------------------------------------------------
  !    Opening/Header reading of Measurmts/Extern Data/Bias/etc
  !------------------------------------------------------------------

  Ym%nChan = InstrConfig%nChan
  Ym%nqc   = nqc

  ALLOCATE(Ym%CentrFreq(Ym%nchan),Ym%Rad(Ym%nchan),   &
         Ym%qc(Ym%nqc),Ym%Tb(Ym%nchan),               &
         Ym%polar(Ym%nchan),Ym%angle(Ym%nchan),       &
         Ym%secant_view(Ym%nchan),stat=istat)
  IF (istat .ne. 0) CALL ErrHandl(ErrorType,Err_AllocMemPb,'(in 1dvar)')
  Ym%CentrFreq(1:Ym%nChan)=InstrConfig%CentrFreq(1:Ym%nChan)
  Ym%Polar(1:Ym%nChan)=InstrConfig%Polarity(1:Ym%nChan)
  Ym%nPosScan=96
  IF (sensor .eq. 'ssmis') Ym%nPosScan=60
  Ym%nScanLines=1

  UseorNotExterData=CntrlConfig_sub%ExternDataAvailab

! Initialize Scene_Ext structure for guess info
  IF (CntrlConfig_sub%ExternDataAvailab .eq. 1) THEN
     CALL InitHdrScene(GeophStatsT_Atm(1)%nLev,GeophStatsT_Atm(1)%nLay,Ym%nChan,&
          GeophStatsT_Sfc(1)%Freq,GeophStatsT_Sfc(1)%polar,&
          GeophStatsT_Atm(1)%pres_lev,GeophStatsT_Atm(1)%pres_lay,Scene_Ext,            &
          100,100,100,100,100,2,                                                  &
          GeophStatsT_Atm(1)%AbsorbID(1:GeophStatsT_Atm(1)%nAbsorb),nqc,1,              &
          Ym%nPosScan,Ym%nScanLines,9999)
          !print *,'iufg header',iufg,mype
     if (init_pass) then
        CALL WriteHdrScene(iufg,OUTPUTFILEFG,Scene_Ext,nprofiles)
     endif
  ENDIF
  call ReadBias(CntrlConfig_sub%BiasFile,nchanBias,nposBias,cfreq_bias,Bias,Slope,Intercept,errReadingBias)
  IF (errReadingBias.eq.0) THEN 
      IF (Ym%nChan .ne. nchanBias) CALL ErrHandl(ErrorType,Err_InconsNumber,' of channels in RAD/Bias')
      IF (ANY(abs(Ym%CentrFreq(1:Ym%nChan)-CFreq_bias(1:Ym%nChan)).gt.epsilonLoose)) THEN
        DO ichan=1,Ym%nChan
           print *, ichan,' radFreq:',Ym%CentrFreq(ichan),' BiasFreq:',CFreq_bias(ichan)
        enddo
        CALL ErrHandl(ErrorType,Err_InconsFreq,' in RAD/Bias files')
      ENDIF
  ENDIF
  !-------------------------------------------------------------------
  !   CRTM already initialized, but allocate structures
  !-------------------------------------------------------------------
  !---Get number of sensors on satellite and determine SensorID character string
  CALL GetSensorInfo(CntrlConfig_sub%SensorID,SensorID,nSensors)
  nChan=SUM(ChannelInfo%n_Channels)
  !---Allocate Atmos_K, Sfc_K and RTSolution and RTSolution_K structures
  ALLOCATE(Atmos_K(nchan,1),Sfc_K(nchan,1),RTSolution(nchan,1),RTSolution_K(nchan,1),STAT=allocate_status)
  IF (allocate_status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Allocation Failed')
  !---Create Atmophere, Surface and Options structures
  CALL CRTM_Atmosphere_Create(Atmos(1),MAXLAYS,MAXABSORB,MAXCLOUDS,MAXAEROSOL)
  IF (ANY(.NOT. CRTM_Atmosphere_Associated(Atmos)) ) &
       CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Create Atmos Structure Failed')

  CALL CRTM_Surface_Create(Sfc,nchan)
  IF (ANY(.NOT. CRTM_Surface_Associated(Sfc)) ) &
       CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Create Sfc Structure Failed')

  CALL CRTM_Options_Create(Options,nchan)
  IF (ANY(.NOT. CRTM_Options_Associated(Options)) ) &
       CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Create Options Structure Failed')

  DO ichan=1,nchan
     CALL CRTM_Atmosphere_Create(Atmos_K(ichan,1),MAXLAYS,MAXABSORB,MAXCLOUDS,MAXAEROSOL)
     CALL CRTM_Surface_Create(Sfc_K(ichan,1),nchan)
  ENDDO
  !---Allocate and Initialize RTSolution structures
  CALL CRTM_RTSolution_Create(RTSolution,MAXLAYS)
  CALL CRTM_RTSolution_Create(RTSolution_K,MAXLAYS)
  !------------------------------------------------------------------
  !    Initialization of commonly used variables 
  !----------------------------------------------------------------
  nAtm       = computeNG(GeophStatsT_Atm(1))
  nSfc       = computeNG(GeophStatsT_Sfc(1))
  nG         = nAtm + nSfc
  nEDRs_atm  = GeophStatsT_Atm(1)%nEDRs
  nEDRs_sfc  = GeophStatsT_Sfc(1)%nEDRs
  nEDRs      = nEDRs_atm + nEDRs_sfc
  nSfcTypes  = GeophStatsT_Atm(1)%nTypes
  !nChan      = Ym%nChan
  nLev       = GeophStatsT_Atm(1)%nLev
  nLay       = GeophStatsT_Atm(1)%nLay
  mxIter     = maxval(TunParams(1:CntrlConfig_sub%nattempts)%nIterations)  
  iEDR_temp  = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'TEMP')
  iEDR_wvap  = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'WVAP')
  iEDR_ozon  = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'OZON')
  iEDR_clw   = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'CLW')
  iEDR_rain  = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'RAIN')
  iEDR_snow  = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'SNOW')
  iEDR_ice   = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'ICE')
  iEDR_grpl  = FindIndxStr(GeophStatsT_Atm(1)%EDR_Desc,'GRPL')
  iEDR_emis  = FindIndxStr(GeophStatsT_Sfc(1)%EDR_Desc,'EMIS')
  iEDR_refl  = FindIndxStr(GeophStatsT_Sfc(1)%EDR_Desc,'REFL')
  iEDR_sfcp  = FindIndxStr(GeophStatsT_Sfc(1)%EDR_Desc,'SFCP')
  iEDR_sfcT  = FindIndxStr(GeophStatsT_Sfc(1)%EDR_Desc,'TSKIN')
  ALLOCATE(nGsel(CntrlConfig_sub%nattempts))
  DO iattempt=1,CntrlConfig_sub%nattempts
      nGsel(iattempt)   = computeNGselec(GeophStatsT_Atm(iattempt)%EDR_Desc,          &
          GeophStatsT_Sfc(iattempt)%EDR_Desc,TunParams(iattempt)%EDR_nEOF_atm,        &
          TunParams(iattempt)%EDR_nEOF_sfc,GeophStatsT_Atm(iattempt)%Stats(:,1)%npEDR,&
          GeophStatsT_Sfc(iattempt)%Stats(:,1)%npEDR)
  ENDDO
  !------------------------------------------------------------------
  !    Open Output file(s) and write out header
  !------------------------------------------------------------------
  !---Determine the number of profiles to be processed
  !call GetNprofs2Process(CntrlConfig_sub%ExternDataAvailab,CntrlConfig_sub%nprofs2process,   &
  !     nMeasurData,nExternData,nprofiles)


  CALL InitHdrScene(nLev,nLay,nChan,GeophStatsT_Sfc(1)%Freq,GeophStatsT_Sfc(1)%polar,&
       GeophStatsT_Atm(1)%pres_lev,GeophStatsT_Atm(1)%pres_lay,Scene,                &
       GeophStatsT_Atm(1)%Stats(iEDR_clw,1)%npEDR,                                   &
       GeophStatsT_Atm(1)%Stats(iEDR_rain,1)%npEDR,                                  &
       GeophStatsT_Atm(1)%Stats(iEDR_snow,1)%npEDR,                                  &
       GeophStatsT_Atm(1)%Stats(iEDR_ice,1)%npEDR,                                   &
       GeophStatsT_Atm(1)%Stats(iEDR_grpl,1)%npEDR,                                  &
       GeophStatsT_Atm(1)%nAbsorb,                                                   &
       GeophStatsT_Atm(1)%AbsorbID(1:GeophStatsT_Atm(1)%nAbsorb),nqc,1,              &
       Ym%nPosScan,Ym%nScanLines,9999)
       !print *,'iuedr header',iuedr,mype
  if (init_pass) then
     CALL WriteHdrScene(iuedr,OutputFile,Scene,nprofiles)
  endif
  !---Open monitoring file if requested by Cntrol configuration
!  IF (CntrlConfig_sub%MonitorIterat .eq.1) THEN
!      call WriteHdrMonitor(CntrlConfig_sub%MonitorFile,iuMonitor,nprofiles, &
!          GeophStatsT_Atm(1)%pres_lev(2:GeophStatsT_Atm(1)%nLev))
!  ENDIF
  !------------------------------------------------------------------
  !   Allocation of vectors/matrices needed across the program
  !------------------------------------------------------------------
  !---Geophysical-Retrieval vectors/matrices / Radiances / Derivatives
  nG0=maxval(nGsel(1:CntrlConfig_sub%nattempts))
  ALLOCATE(Se(nChan,nChan),Fe(nChan,nChan),SaAtm(nAtm,nAtm),SaSfc(nSfc,nSfc),               &
       SaAtmTot(nAtm,nAtm,nSfcTypes,CntrlConfig_sub%nattempts),                                 &
       SaSfcTot(nSfc,nSfc,nSfcTypes,CntrlConfig_sub%nattempts),                                 &
       XbAtm(nAtm),XbSfc(nSfc),X1stAtm(nAtm),X1stSfc(nSfc),Xb(nG0),Sa(nG0,nG0),             &
       XbAtmTot(nAtm,nSfcTypes,CntrlConfig_sub%nattempts),                                      & 
       XbSfcTot(nSfc,nSfcTypes,CntrlConfig_sub%nattempts),                                      &
       X1st(nG0),Xg(nG0,0:mxIter),Y(nChan,0:mxIter),NoiseRMS(nChan),K(nChan,nG0),           &
       Xout(nG0),nIterTot(nprofiles),nAttempTot(nprofiles),ChiSq(0:mxIter),                 &
       UAtm(nAtm,nAtm),USfc(nSfc,nSfc),U(nG0,nG0),node(nprofiles),                              &
       UAtmTot(nAtm,nAtm,nSfcTypes,CntrlConfig_sub%nattempts),                                  &
       USfcTot(nSfc,nSfc,nSfcTypes,CntrlConfig_sub%nattempts),                                  &
       ParamIndx(nEDRs),ParamLength(nEDRs),ParamLabel(nEDRs),ShrkRadVec(nChan),             &
       SeStar(nChan,nChan),dY2(nchan),FeStar(nChan,nChan),Ystar(nChan),                     &
       YmStar(nChan),Kstar(nChan,nG0),ChanSel(nchan),DX(nG0),DXtilda(nG0),                  &
       DXtildaNew(nG0),Ktilda(nChan,nG0),Ustar(nG0,nG0),Lambda(nG0,nG0),G(nG0,nchan),       &
       A(nG0,nG0),S(nG0,nG0),EDR_nEOF(nEDRs),EDR_cntrlRetr(nEDRs),                          &
       EDR_ExtDataUse(nEDRs),iSpaceModeFlag(nEDRs),EmissAnalyt(nchan),upWelling_rad(nchan), &
       dnWelling_rad(nchan),opt_lay(nLay,nchan),ModelErrCFreq(nchan),ModelErr(nchan),       &
       ModelErrTot(nchan,CntrlConfig_sub%nattempts),tb(Ym%nChan),TBqc(Ym%nchan),stat=istat)
  IF (istat .ne.0) CALL ErrHandl(ErrorType,Err_AllocMemPb,'(in 1dvar)')
  !------------------------------------------------------------------
  !   Load modeling errors from 'nattempts' files and do checks
  !------------------------------------------------------------------
  DO iattempt=1,CntrlConfig_sub%nattempts
      call ReadNoise(CntrlConfig_sub%FileModelErr(iattempt),ModelErrCFreq,ModelErr,nchanModelErr)
      ModelErrTot(1:nchan,iattempt) = ModelErr(1:nchan)
  ENDDO
  call ConsistCheck(Ym%nChan,nChan,nAtm,nSfc,nchanModelErr)
  !---Time elapsed for the uploading/initialization/setting of different data
!  CALL CPU_TIME(t1)
!  Tim(2)=Tim(2) + (t1-t00)
  !------------------------------------------------------------------
  !   Load Backgrounds, Covariances and Eigenvectors for all surfaces
  !------------------------------------------------------------------
  iAtm=0
  iSfc=0
  DO iattempt=1,CntrlConfig_sub%nattempts
     DO i=1,nSfcTypes
        iAtm=i
        iSfc=i
        !---Read in and Store Climo Geophys Bkd/Cov -Atm & Sfc-
        call GetGeophBkgCov(SaAtm,XbAtm,iAtm,SaSfc,XbSfc,iSfc,iattempt)
        SaAtmTot(1:nAtm,1:nAtm,i,iattempt) = SaAtm(1:nAtm,1:nAtm)
        SaSfcTot(1:nSfc,1:nSfc,i,iattempt) = SaSfc(1:nSfc,1:nSfc)
        XbAtmTot(1:nAtm,i,iattempt)        = XbAtm(1:nAtm)
        XbSfcTot(1:nSfc,i,iattempt)        = XbSfc(1:nSfc)
        !---Read in and Store the Transformation matrx (eigenvectors) -Atm & Sfc-
        call GetEigVectMatrx(UAtm,iAtm,USfc,iSfc,iattempt)
        UAtmTot(1:nAtm,1:nAtm,i,iattempt)  = UAtm(1:nAtm,1:nAtm)
        USfcTot(1:nSfc,1:nSfc,i,iattempt)  = USfc(1:nSfc,1:nSfc)
     ENDDO
  ENDDO

  !------------------------------------------------------------------
  !    Start of the 1D-VAR retrieval process (Loop over measurements)
  !------------------------------------------------------------------
  nConvProfs       = 0
  nProfsOverOcean  = 0
  nProfsOverSeaIce = 0
  nProfsOverLand   = 0
  nProfsOverSnow   = 0

  !---Loop over the measurements
  ProfLoop: DO iprof=1,nprofiles

     call compJulDay(year(iprof),month(iprof),day(iprof),julDay)

     Ym%tb(1:Ym%nChan)=meas(1:Ym%nChan,iprof)
     Ym%lat=lat(iprof)
     Ym%lon=lon(iprof)
     Ym%angle(1:Ym%nChan)=angle(iprof)
     Ym%julDay=julDay
     Ym%month=month(iprof)
     Ym%day=day(iprof)
     Ym%year=year(iprof)
     Ym%iScanPos=iscanpos(iprof)
     Ym%node=0
     IF (Ym%lon .gt. 180) Ym%lon=(360-Ym%lon)*(-1)

     !---Interpolate FG profiles to MIRS pressure grid
     IF (UseorNotExterData .eq. 1) THEN
        guess_nlay=64
        lay_start=1
        sfcp_guess=fgp(guess_nlay,iprof)
        CALL LINT(fgp(:,iprof),fgt(:,iprof),guess_nlay,guess_nlay,100,GeophStatsT_Atm(1)%pres_lay,nwp_t_guess)
        CALL LINT(fgp(:,iprof),fgq(:,iprof),guess_nlay,guess_nlay,100,GeophStatsT_Atm(1)%pres_lay,nwp_q_guess)

        !---Fill Scene_Ext structure
        Scene_Ext%Temp_Lay=nwp_t_guess
        Scene_Ext%Absorb_Lay(:,1)=nwp_q_guess
        Scene_Ext%SfcPress=sfcp_guess
        
        Scene_Ext%Pres_Lay=GeophStatsT_Atm(1)%pres_lay
        Scene_Ext%Pres_Lev=GeophStatsT_Atm(1)%pres_lev
        
        Scene_Ext%WindSp=fgwindsp(iprof)
        Scene_Ext%Tskin=fgsst(iprof)


        IF (ANY(Scene_Ext%Temp_Lay .le. 100)) UseorNotExterData=0
        IF (ANY(Scene_Ext%Absorb_Lay(:,1) .le. 0)) UseorNotExterData=0
!        print *,'useornotexternal',UseorNotExterData
!         call printscene(Scene_Ext)
         !---Check whether any variables in external data scene are negative
         !---If they are, fill values at top and/or bottom with lowest/heighest layer with non-negative values, respectively
!         IF( (UseorNotExterData .eq. 1) .and. &
!              ((ANY(Scene_Ext%Temp_lay(1:Scene_Ext%nLay) .le. 0.)) .or. &
!              (ANY(Scene_Ext%Absorb_lay(1:Scene_Ext%nLay,Scene_Ext%iH2O) .lt. 0.)) .or. &
!              (ANY(Scene_Ext%CLW(1:Scene_Ext%nLay) .lt. 0.)) .or. &
!              (ANY(Scene_Ext%Graupel(1:Scene_Ext%nLay) .lt. 0.)) .or. &
!              (ANY(Scene_Ext%Rain(1:Scene_Ext%nLay) .lt. 0.)))) then
!            Scene_ExtIn=Scene_Ext
!            call FillSceneNegValues(Scene_ExtIn,Scene_Ext)
!         ENDIF
        Scene_ext%node=0
        Scene_ext%lat=lat(iprof)
        Scene_ext%lon=lon(iprof)
        !print *,'iufg scene',iufg,mype
        if (init_pass) then
           CALL WriteScene(iufg,Scene_ext)
        endif
     ENDIF
      !---Determine the surface type and water fraction 
  call Read_topography(CntrlConfig_sub%Topogr,Ym%lat,Ym%lon,xalt,stypeSfc,xcover,stypeSfcCRTM)

      !---Time elapsed reading radiance & external data 
  !CALL CPU_TIME(t1)
  !tim(3) = tim(3) + (t1-t0)
      !---Determine if 1d-var should proceed based on QC(Ym) and QC(Scene_Ext)
!      call DetermOKsgnl(UseorNotExterData,Ym%tb,Ym%qc,Scene_Ext%qc,   &
!           CntrlConfig_sub%GeogrLimits,CntrlConfig_sub%minLat,CntrlConfig_sub%maxLat,   &
!           CntrlConfig_sub%minLon,CntrlConfig_sub%maxLon,CntrlConfig_sub%pass,Ym%lat,   &
!           Ym%lon,Ym%node,stypeSfc,OkRetrvl)
  OkRetrvl=.TRUE.
      !---Initialization 
  Scene%qc(:)=DEFAULT_VALUE_INT
  Scene%ChiSq=DEFAULT_VALUE_REAL
  Xout(:)=DEFAULT_VALUE_REAL
  nIterTot=DEFAULT_VALUE_INT
  

 IF (ANY(Ym%tb .lt. 10 .or. Ym%tb .gt. 400)) OkRetrvl=.FALSE.
! call Set2DefaultQC(vec_Int=Scene%qc,vec_Real=Xout,singl_Int=nIterTot(iprof),singl_Real=Scene%ChiSq)
      !---Proceed with retrieval if OKRetrvl signal signals it
  IF (OKRetrvl) THEN
     !CALL CPU_TIME(t0)
        !---Increment the number of profiles effectively processed
     nprofsEff=nprofsEff+1
        !---Start the iterative process (#of retrieval attempts)
 
       AttemptsLoop: DO iattempt=1,CntrlConfig_sub%nattempts
           nAttempTot(iprof) = iAttempt
           nIterTot(iprof)   = 0
           !---Pre-classification
           TskPreclass = DEFAULT_VALUE_REAL
  !         IF (UseorNotExterData .eq. 1 .and. TunParams(iattempt)%EDR_ExtDataUse_sfc(iEDR_sfcT).ne.0) THEN
  !            TskPreclass=Scene_Ext%Tskin
  !         ENDIF

           call PreClassAtm(Ym%Year,Ym%Julday,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,stypeSfc,&
                TunParams(iattempt)%Sensor_ID,Ym%tb,AtmClass,TskPreclass)
           call PreClassSfc(Ym%Year,Ym%Julday,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,stypeSfc,&
                TunParams(iattempt)%Sensor_ID,Ym%tb,SfcClass,TskPreclass)
           !---In case the surface type is chosen from external data, override preclassifier.
           IF (UseorNotExterData .ne. 0 .and. TunParams(iattempt)%EDR_ExtDataUse_sTyp .eq. 1) THEN
              SfcClass = Scene_ext%iTypSfc
           ENDIF
           !---Count surface class types
           IF (iattempt.eq.1) call CountClassTypes(SfcClass,nProfsOverOcean,nProfsOverSeaIce,nProfsOverLand,nProfsOverSnow)
           !---Class-Index determination
           call DetermIndx(AtmClass,iAtm,GeophStatsT_Atm(iattempt)%nTypes,GeophStatsT_Atm(iattempt)%Type_IDs)
           call DetermIndx(SfcClass,iSfc,GeophStatsT_Sfc(iattempt)%nTypes,GeophStatsT_Sfc(iattempt)%Type_IDs)
           !---Set the scene (that will feed the fwd operator) to Bkg
           call SetUpScene(GeophStatsT_Atm(iattempt)%pres_lev(1:nLev),GeophStatsT_Atm(iattempt)%pres_lay(1:nLay),&
                mean(Ym%angle(1:Ym%nchan)),Scene,SfcClass,iProf,Ym%lat,Ym%lon,Ym%node,     &
                Ym%julDAY,Ym%Year,Ym%secs,Ym%iscanPos,Ym%iscanLine,Ym%RelAziAngle,Ym%SolZenAngle)
           !---Generic Screen printing
           IF (CntrlConfig_sub%PrintMonitor.eq.1) THEN 
              write(*,'(a75)')  '**************************************************************************'
              write(*,'(a5,i6,a10,L2,a6,i2,a6,i2,a5,f7.2,a5,f6.1,a5,f6.1)')'Prof#',iprof,' OKsignl:',&
                   OkRetrvl,' sTyp:',SfcClass,' Node:',Ym%node,' Ang:',mean(Ym%angle(1:Ym%nchan)),   &
                   ' Lat:',Ym%lat,' Lon:',Ym%lon
              write(*,'(a75)')  '**************************************************************************'
           ENDIF
           !---Compute the temperature-dependent noise vector (and scale)
           call ComputeNoise(noiseRMS)
           call scaleError(noiseRMS,SfcClass,nchan,TunParams(iattempt)%scalFactEF_oc_byChan,         &
                TunParams(iattempt)%scalFactEF_ic_byChan,TunParams(iattempt)%scalFactEF_ld_byChan,   &
                TunParams(iattempt)%scalFactEF_sn_byChan)
           !---Select appropriate Climo Geophys Bkd/Cov -Atm & Sfc-
 !          call GetGeophBkgCov(SaAtm,XbAtm,iAtm,SaSfc,XbSfc,iSfc,iattempt)
           !---Select appropriate Transformation matrx (eigenvectors) -Atm & Sfc-
 !          call GetEigVectMatrx(UAtm,iAtm,USfc,iSfc,iattempt)
           !REPLACE BY ARRAY READ OUTSIDE OF PROFILE LOOP
           !---Select appropriate Climo Geophys Bkd/Cov -Atm & Sfc-
           !call GetGeophBkgCov(SaAtm,XbAtm,iAtm,SaSfc,XbSfc,iSfc,iattempt)

           XbAtm=XbAtmTot(1:nAtm,iAtm,iattempt)
           XbSfc=XbSfcTot(1:nSfc,iSfc,iattempt)
           SaAtm=SaAtmTot(1:nAtm,1:nAtm,iAtm,iattempt)
           SaSfc=SaSfcTot(1:nSfc,1:nSfc,iSfc,iattempt)
           !---Select appropriate Transformation matrx (eigenvectors) -Atm & Sfc-
           !call GetEigVectMatrx(UAtm,iAtm,USfc,iSfc,iattempt)
           UAtm=UAtmTot(1:nAtm,1:nAtm,iAtm,iattempt)
           USfc=USfcTot(1:nSfc,1:nSfc,iSfc,iattempt)
           !---Build The measurement error cov Matrx Se from the noise info
           call BuildMatrxFromDiagVec(nchan,noiseRMS(1:nchan),Se(1:nchan,1:nchan))
           !---Build The modeling error cov Matrx Fe from the model error info (after scaling)
           ModelErr(1:nchan)=ModelErrTot(1:nchan,iattempt)
           call scaleError(ModelErr(1:nchan),SfcClass,nchan,                             &
                TunParams(iattempt)%scalFactEF_oc_byChan,TunParams(iattempt)%scalFactEF_ic_byChan,   &
                TunParams(iattempt)%scalFactEF_ld_byChan,TunParams(iattempt)%scalFactEF_sn_byChan)
           call BuildMatrxFromDiagVec(nchan,ModelErr(1:nchan),Fe(1:nchan,1:nchan))
           !---Replace certain elements of Bkg by Ext data (if applicable)
           IF (UseorNotExterData .eq. 1) THEN
              call SetBkg2Ext(Scene_ext,GeophStatsT_Atm(iattempt)%EDR_Desc,        &
                   GeophStatsT_Atm(iattempt)%EDR_IDs,GeophStatsT_Atm(iattempt)%Stats(:,iAtm)%npEDR,  &
                   nEDRs_Atm,TunParams(iattempt)%EDR_ExtDataUse_atm,XbAtm,               &
                   GeophStatsT_Atm(iattempt)%iSpaceMode)
              call SetBkg2Ext(Scene_ext,GeophStatsT_Sfc(iattempt)%EDR_Desc,                &
                   GeophStatsT_Sfc(iattempt)%EDR_IDs,GeophStatsT_Sfc(iattempt)%Stats(:,iSfc)%npEDR,  &
                   nEDRs_Sfc,TunParams(iattempt)%EDR_ExtDataUse_sfc,XbSfc,               &
                   GeophStatsT_Sfc(iattempt)%iSpaceMode)
              Scene%Pres_lev(1:Scene%nLev) = Scene_ext%Pres_lev(1:Scene%nLev)
              Scene%Pres_lay(1:Scene%nLay) = Scene_ext%Pres_lay(1:Scene%nLay)
          ENDIF
           !---Set the 1st guess to the background -Atm & Sfc-
           call Set1stG2Bkg(XbAtm,XbSfc,X1stAtm,X1stSfc)
           !---Time elapsed preparing different matrices (Noise,Sa,Xb,Se,Fe,U,X1st)
           !CALL CPU_TIME(t1)
           !tim(4) = tim(4) + (t1-t0)
           !CALL CPU_TIME(t0)
           !---Bias application
           Call apply_bias(Ym%tb,tb,Ym%iscanPos,Ym%nPosScan,nposBias,Bias,Slope,Intercept,nChan, &
                TunParams(iattempt)%iWhere2Apply,TunParams(iattempt)%iCorrMethod,SfcClass,errReadingBias, &
                TunParams(iattempt)%applyBias_oc_byChan,TunParams(iattempt)%applyBias_ic_byChan, &
                TunParams(iattempt)%applyBias_ld_byChan,TunParams(iattempt)%applyBias_sn_byChan)
           !---Adjust ad-hoc the brightness temperatures
           !call AdjustBiasAdHoc(tb,nchan,SfcClass,TunParams(iattempt)%applyBias_oc_byChan,       &
           !     TunParams(iattempt)%applyBias_ic_byChan,TunParams(iattempt)%applyBias_ld_byChan, & 
           !     TunParams(iattempt)%applyBias_sn_byChan,TunParams(iattempt)%Sensor_ID)
           !---Dimension of Retrieval space and allocation of vectors

           call DetermNumbEOFs(TunParams(iattempt),nR)
           !---Consistency checks
           IF (TunParams(iattempt)%nChanSel .ne. nChan) THEN 
                print *,'mype,iattempt,satid,sensor,nChanSel,nChan',mype,iattempt,satId,sensor,TunParams(iattempt)%nChanSel,nChan
                print *,'tuning files',CntrlConfig_sub%TuningFile(1)
                CALL ErrHandl(ErrorType,Err_InconsNumber,'of channels turned ON/OFF')
           ENDIF   
           IF (nR.gt.nG) CALL ErrHandl(ErrorType,Err_InconsNumber,' (nR > nG)') 
           !---Tune Background/1st Guess
           call TuneBkgAtm(iEDR_clw,iEDR_rain,iEDR_snow,iEDR_ice,iEDR_grpl,XbAtm,   &
                X1stAtm,GeophStatsT_Atm(iattempt)%EDR_IDs(:),TunParams(iattempt)%EDR_nEOF_atm,&
                GeophStatsT_Atm(iattempt)%EDR_Desc,iattempt)
           !---Turn OFF channels if their values are dummy
           call SetchanFlagsBasedonTbValues(tb,Ym%nchan,TunParams(iattempt),TBqc)
           !--------------------------------------------------------------------
           !  Geophysical/retrieval background preparation and projection
           !--------------------------------------------------------------------
           !---Put Vectors into the scene
           call TransfXg2Scene(XbSfc,Scene,GeophStatsT_Sfc(iattempt)%EDR_Desc,          &
                GeophStatsT_Sfc(iattempt)%EDR_IDs,GeophStatsT_Sfc(iattempt)%Stats(:,iSfc)%npEDR,  &
                GeophStatsT_Sfc(iattempt)%iSpaceMode)
           call TransfXg2Scene(XbAtm,Scene,GeophStatsT_Atm(iattempt)%EDR_Desc,          &
                GeophStatsT_Atm(iattempt)%EDR_IDs,GeophStatsT_Atm(iattempt)%Stats(:,iAtm)%npEDR,  &
                GeophStatsT_Atm(iattempt)%iSpaceMode)
           !---Estimate the surface pressure from the local topography (over land) -only when external data not present-
           !---Use first-guess sfc pressure if available, otherwise scale based on altitude over land
           IF (UseorNotExterData .eq. 0) THEN
              call EstimatSfcPress(xalt,SfcClass,Scene%SfcPress)
           ENDIF
           !---Disable retrieval of layers below surface level
           call DisabLayBelowSfc(GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_temp),                  &
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_wvap),GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_ozon),&
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_clw),                                    &
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_rain),GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_snow),&
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_ice),GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_grpl), &
                SaAtm,Scene%SfcPress,iattempt)
           !---Disable retrieval of layers above top-pressure level
           call DisabLayAbovTopSensPress(GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_temp),          &
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_wvap),GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_ozon),&
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_clw),                                    &
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_rain),GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_snow),&
                GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_ice),GeophStatsT_Atm(iattempt)%EDR_IDs(iEDR_grpl), &
                SaAtm,TunParams(iattempt)%topSensPressT,TunParams(iattempt)%topSensPressWV,iattempt)
           !---Merge Sfc and Atm background/Covariances/1st Guess/Eigenv. Matrx
           call MergeSfcAndAtm(SaAtm,XbAtm,UAtm,SaSfc,XbSfc,USfc,X1stAtm,     &
                X1stSfc,Sa,Xb,U,X1st,GeophStatsT_Atm(iattempt)%EDR_Desc,                &
                GeophStatsT_Atm(iattempt)%EDR_IDs,GeophStatsT_Sfc(iattempt)%EDR_Desc,             &
                GeophStatsT_Sfc(iattempt)%EDR_IDs,TunParams(iattempt)%EDR_nEOF_atm,     &
                TunParams(iattempt)%EDR_nEOF_sfc,ParamLabel,ParamIndx,        &
                ParamLength,EDR_nEOF,GeophStatsT_Atm(iattempt)%Stats(:,iAtm)%npEDR,     &
                GeophStatsT_Sfc(iattempt)%Stats(:,iSfc)%npEDR,nGselec,nEDRselec,        &
                TunParams(iattempt)%EDR_cntrlRetr_atm,TunParams(iattempt)%EDR_cntrlRetr_sfc,  &
                EDR_cntrlRetr,EDR_ExtDataUse,TunParams(iattempt)%EDR_ExtDataUse_atm,  &
                TunParams(iattempt)%EDR_ExtDataUse_sfc,GeophStatsT_Atm(iattempt)%iSpaceMode,    &
                GeophStatsT_Sfc(iattempt)%iSpaceMode,iSpaceModeFlag)
           !---Combine External data with Background (& 1st Guess)
           call CombExtWithBkgCov(Xb,Scene_Ext,X1st,ParamLabel,       &
                   ParamIndx,ParamLength,EDR_ExtDataUse,nEDRselec,iSpaceModeFlag,&
                   UseorNotExterData)
           !---Tune the resulting geophysical covariance matrix
           call TuneCov(Sa,Scene%SfcPress,nEDRselec,EDR_cntrlRetr,ParamIndx,  &
                ParamLength,ParamLabel,EDR_nEOF,Xb)
           !---Shrink Transform Matrx (retain only significant EOFs)
           call ShrkU(ParamLabel(1:nEDRselec),ParamIndx(1:nEDRselec),         &
                ParamLength(1:nEDRselec),U(1:nGselec,1:nGselec),              &
                Ustar(1:nGselec,1:nR),EDR_nEOF(1:nEDRselec),TunParams(iattempt))
           !---Project covariance matrix into EOF space (diagonalization)
           call ProjCov(nR,nGselec,Ustar(1:nGselec,1:nR),Sa(1:nGselec,1:nGselec),   &
                Lambda(1:nR,1:nR))
           !---Time elapsed setting up scene/merging cov, EOF projection
           !CALL CPU_TIME(t1)
           !tim(5) = tim(5) + (t1-t0)
           !--------------------------------------------------------------------
           !  Radiance preparation and projection
           !--------------------------------------------------------------------
           !---Set first solution estimate to 1st Guess 
           Xg(1:nGselec,0) = X1st(1:nGselec)
           !---Determine which channels to use depending on Tuning data
           ChanSel(1:nchan) = TunParams(iattempt)%ChanSel2use(1:nchan) 
           call DeterminChanSel(Ym%CentrFreq,nchan,ChanSel,                    &
                TunParams(iattempt)%ChanSelectFlag,TunParams(iattempt)%FreqMin,&
                TunParams(iattempt)%FreqMax,nch,ShrkRadVec)
           !---Transfer Info to Scene
           call TransfXg2Scene(Xg(1:nGselec,0),Scene,ParamLabel(1:nEDRselec),  &
                ParamIndx(1:nEDRselec),ParamLength(1:nEDRselec),               &
                iSpaceModeFlag(1:nEDRselec))
        !print *,'Wind',Scene_ext%windsp, Scene%windsp
           !---Forward operator on 1st Guess
           !CALL CPU_TIME(t0)
           !Scene%SfcPress=Scene_Ext%SfcPress
           !call printscene(Scene)
           call FwdOper(Scene,Y(1:nchan,0),K(1:nchan,1:nGselec),ChannelInfo,    &
                Atmos,Atmos_K,Sfc,Sfc_K,Options,RTSolution,RTSolution_K,        &
                ParamLabel(1:nEDRselec),ParamIndx(1:nEDRselec),                 &
                ParamLength(1:nEDRselec),iSpaceModeFlag(1:nEDRselec),           &
                nLayEff,dnWelling_rad,upWelling_rad,opt_lay,xalt,               &
                TunParams(iattempt)%topSensPressT,                              &
                TunParams(iattempt)%topSensPressWV,CntrlConfig_sub%logFile,Error_Status)

           !---Deduct analytically the emissivity
 !          IF (UseorNotExterData .ne. 0) THEN
 !             DO ichan=1,nChan1
 !                trx=exp(-(sum(opt_lay(1:nLayEff,ichan)*Ym%secant_view(ichan))))                
 !                EmissAnalyt(ichan)=-99.
 !                IF (trx .gt.0.05) EmissAnalyt(ichan)=&
 !                     (((tb(ichan)-upWelling_rad(ichan))/(trx))- &
 !                     dnWelling_rad(ichan))/(Scene%Tskin-dnWelling_rad(ichan))
 !             ENDDO
 !          ENDIF
           !---Time elapsed in the forward operator
          ! CALL CPU_TIME(t1)
           !tim(6) = tim(6) + (t1-t0)
           !---Compute Convergence metric ChiSq
           call Convgce(ChanSel,tb(1:nchan),Y(1:nchan,0),NoiseRMS(1:nchan)+   &
                ModelErr(1:nchan),ChiSq(0),CvgceReached,TunParams(iattempt)%ChiSqThresh,dY2)
           !---Screen-Output Convergence monitoring for 1st guess perfs
           IF (CntrlConfig_sub%PrintMonitor.eq.1) THEN 
              CALL ComputeTPW(Scene%pres_Lev(1:Scene%nLev),Scene%Sfcpress,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2O),tpw1)
              write(*,'(3a10,4x,10a6)') 'Att#','Iter#','N-ChiSq','CLW','RWP','GWP','IWP','TPW','TSK','EMIS'
              write(*,'(2i10,f10.2,4x,5f6.2,f6.1,3f6.3)') iattempt,0,ChiSq(0),    &
                   ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay), &
                   Scene%SfcPress,Scene%CLW(1:Scene%nLay)),             &
                   ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay), &
                   Scene%SfcPress,Scene%Rain(1:Scene%nLay)),            &
                   ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay), &
                   Scene%SfcPress,Scene%Graupel(1:Scene%nLay)),         &
                   ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay), &
                   Scene%SfcPress,Scene%Ice(1:Scene%nLay)),tpw1,Scene%tskin,Scene%Emiss(1:3)
                   
           ENDIF
           !---If convergence reached @ 1st Guess, do not perform iterative process
           IF (CvgceReached) EXIT AttemptsLoop
           !---Shrink Ym to represent only selected channels
           call ShrkVect(nch,ShrkRadVec(1:nch),tb,YmStar(1:nch)) 
           !---Compute Departure From Bkg
           call ComputeDeltaX(Xg(1:nGselec,0),Xb(1:nGselec),DX(1:nGselec))
           !--------------------------------------------------------------------
           ! CORE 1-DVAR
           !--------------------------------------------------------------------
           IterLoop: DO iter=0,TunParams(iattempt)%nIterations-1
              nIterTot(iprof)   = Iter+1
              !CALL CPU_TIME(t0)
              !---Shrink Se/Fe and Y/K to represent only selected channels
              call ShrkMatrx(nch,ShrkRadVec(1:nch),Se,SeStar(1:nch,1:nch),0)
              call ShrkMatrx(nch,ShrkRadVec(1:nch),Fe,FeStar(1:nch,1:nch),0)
              call ShrkMatrx(nch,ShrkRadVec(1:nch),K(:,1:nGselec),Kstar(1:nch,1:nGselec),1)
              call ShrkVect(nch,ShrkRadVec(1:nch),Y(:,iter),Ystar(1:nch))
              !---Transform K and DX into EOF space
              call transfGeo2EOF(Ustar(1:nGselec,1:nR),Kstar(1:nch,1:nGselec),         &
                   DX(1:nGselec),Ktilda(1:nch,1:nR),DXtilda(1:nR),nGselec,nR,nch)
              !---Account for non-linearity
              call Adjust4NonLinearity(SeStar(1:nch,1:nch),dY2(ShrkRadVec(1:nch)),     &
                   TunParams(iattempt)%alpha,TunParams(iattempt)%beta,                 &
                   Kstar(1:nch,1:nGselec),nch,nGselec,Sa(1:nGselec,1:nGselec),iter)
              !---Compute the Levenberg-Marquardt optimal solution 
              call SolEstimat(nchan,nch,nGselec,nR,DXtilda(1:nR),Ktilda(1:nch,1:nR),   &
                   Ystar(1:nch),YmStar(1:nch),SeStar(1:nch,1:nch),                     &
                   FeStar(1:nch,1:nch),Lambda(1:nR,1:nR),DXtildaNew(1:nR))
              !---Transform DXtildaNew into Geophysical space
              call transfEOF2Geo(Ustar(1:nGselec,1:nR),DXtildanew(1:nR),DX(1:nGselec), &
                   nGselec,nR)
              !---Compute geophysical vector 
              call ComputeGeoX(DX(1:nGselec),Xb(1:nGselec),Xg(1:nGselec,iter+1))
              !---Transfer Info to Scene
              call TransfXg2Scene(Xg(1:nGselec,iter+1),Scene,ParamLabel(1:nEDRselec),  &
                   ParamIndx(1:nEDRselec),ParamLength(1:nEDRselec),                    &
                   iSpaceModeFlag(1:nEDRselec))
              !---Check QC and validity of Scene before feeding it to Fwd Operator
              call PerfQConScene(Scene,nGselec,Sa(1:nGselec,1:nGselec),DX(1:nGselec),  &
                   Xb(1:nGselec),Xg(1:nGselec,iter+1),ParamLabel(1:nEDRselec),         &
                   ParamIndx(1:nEDRselec),ParamLength(1:nEDRselec),                    &
                   iSpaceModeFlag(1:nEDRselec),nEDRselec,TunParams(iattempt)%rhMaxAllowed)
              !---Time elapsed in the solution estimation as well as pre&post-procesing
              !CALL CPU_TIME(t1)
             ! tim(7) = tim(7) + (t1-t0)
              !---Forward operator on 1st Guess
              !CALL CPU_TIME(t0)
              IF (ANY(Scene%Emiss .le. 0 .or. Scene%Emiss .gt.1)) print *,'Emiss Invalid',Scene%Emiss
              IF (Scene%Tskin .lt. 190) THEN
                 print *,'Tskin invalid',Scene%Tskin
                 OKRetrvl=.FALSE.
                 CYCLE IterLoop
              ENDIF
              IF (ANY(Scene%Temp_Lay .lt. 100)) THEN
                 print *,'Temp invalid',Scene%Temp_Lay
                 OKRetrvl=.FALSE.
                 CYCLE IterLoop
              ENDIF   
              IF (Scene%Windsp .lt. 0) print *,'Windsp invalid',Scene%WindSp
              call FwdOper(Scene,Y(1:nchan,iter+1),K(1:nchan,1:nGselec),ChannelInfo, &
                   Atmos,Atmos_K,Sfc,Sfc_K,Options,RTSolution,RTSolution_K,        &
                   ParamLabel(1:nEDRselec),ParamIndx(1:nEDRselec),                 &
                   ParamLength(1:nEDRselec),iSpaceModeFlag(1:nEDRselec),           &
                   nLayEff,dnWelling_rad,upWelling_rad,opt_lay,xalt,               &
                   TunParams(iattempt)%topSensPressT,                              &
                   TunParams(iattempt)%topSensPressWV,CntrlConfig_sub%logFile,Error_Status)
              IF (Error_status .ne. 0) THEN
                 print *,'measurements',Ym%tb
                 print *,'Scene'
                 call printscene(Scene)
                 print *,'Scene_ext'
                 call printscene(Scene_Ext)
                 CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM K-matrix Failed')
              ENDIF
              !CALL CPU_TIME(t1)
              !tim(6) = tim(6) + (t1-t0)
              !---Determ if convgce reached (based on radiance fitting)
              call Convgce(ChanSel,tb(1:nchan),Y(1:nchan,iter+1),NoiseRMS(1:nchan)+  &
                   ModelErr(1:nchan),ChiSq(iter+1),CvgceReached, &
                   TunParams(iattempt)%ChiSqThresh,dY2)
              !---Screen-Output Convergence monitoring
              IF (CntrlConfig_sub%PrintMonitor.eq.1) THEN 
                 CALL ComputeTPW(Scene%pres_Lev(1:Scene%nLev),Scene%Sfcpress,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2O),tpw1)
                 write(*,'(2i10,f10.2,4x,5f6.2,f6.1,3f6.3)') iattempt,iter+1,ChiSq(iter+1), &
                      ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
                      Scene%SfcPress,Scene%CLW(1:Scene%nLay)),                    &
                      ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
                      Scene%SfcPress,Scene%Rain(1:Scene%nLay)),                   &
                      ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
                      Scene%SfcPress,Scene%Graupel(1:Scene%nLay)),                &
                      ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
                      Scene%SfcPress,Scene%Ice(1:Scene%nLay)),tpw1,Scene%tskin,Scene%Emiss(1:3)
              ENDIF
              !---If convergence reached, exit iterative process
              IF (CvgceReached) EXIT AttemptsLoop
           END DO IterLoop
           !---Attempt a second retrieval based on TunParams(iattempt)%ChiSqThresh4Attempt
           IF (ChiSq(nIterTot(iprof)).le.TunParams(iattempt)%ChiSqThresh4Attempt) EXIT AttemptsLoop
        END DO AttemptsLoop
        !---Error Analysis And characterization
        !CALL CPU_TIME(t0)
        IF (ChiSq(nIterTot(iprof)).le.TunParams(nAttempTot(iprof))%ChiSqThresh) &
             nConvProfs=nConvProfs+1
        IF (TunParams(nAttempTot(iprof))%RetrErrCharac .ne. 0) THEN 
           call CompContribFcts(nGselec,nchan,Sa(1:nGselec,1:nGselec),K(1:nchan,1:nGselec),&
                Se(1:nchan,1:nchan),G(1:nGselec,1:nchan)) !G:Called also Gain Fct
           call CompAvgKern(nGselec,nchan,G(1:nGselec,1:nchan),K(1:nchan,1:nGselec),       &
                A(1:nGselec,1:nGselec))                   !A:Average Kernel matrx
           call CompCovSol(nGselec,Sa(1:nGselec,1:nGselec),A(1:nGselec,1:nGselec),         &
                S(1:nGselec,1:nGselec))                   !S:Covariance of solution
        ENDIF
        !----Assess time spent in Analysis And characterization
        !CALL CPU_TIME(t1)
        !tim(8) = tim(8) + (t1-t0)
        !---Overwrite the QC for this profile
        Scene%qc(1:Scene%nqc) = 0
        !---Transfer some important information to Scene for output
        call TransfData2Scene(nAttempTot(iprof),nIterTot(iprof),ChiSq(nIterTot(iprof)),    &
             Scene,Y(1:nchan,nIterTot(iprof)),nchan,ChanSel,Ym%tb(1:nchan),tb(1:nchan))
        !---Output results
!        IF (UseorNotExterData .ne. 0) THEN
!           Scene%Refl(1:nchan) = EmissAnalyt(1:nchan) !use reflectivity spot
!        ENDIF
        !---Output Monitoring information (iteration by iteration)
!        IF (CntrlConfig_sub%MonitorIterat .eq.1) THEN
!           call WriteMonitor(iuMonitor,iprof,nEDRselec,nGselec,nIterTot(iprof),  &
!                nAttempTot(iprof),nchan,nch,Xg(1:nGselec,0:nIterTot(iprof)),     &
!                Scene%SfcPress,ParamLabel(1:nEDRselec),ParamIndx(1:nEDRselec),   &
!                ParamLength(1:nEDRselec),iSpaceModeFlag(1:nEDRselec),            &
!                Xb(1:nGselec),tb(1:nchan),Y(1:nchan,0:nIterTot(iprof)),       &
!                ShrkRadVec(1:nch),Ym%CentrFreq(1:nchan),Ym%polar(1:nchan),       &
!                ChiSq(0:nIterTot(iprof)),A(1:nGselec,1:nGselec),                 &
!                S(1:nGselec,1:nGselec),Sa(1:nGselec,1:nGselec),AtmClass,SfcClass)
!        ENDIF
      ELSE
        !---Pre-classification
        TskPreclass = DEFAULT_VALUE_REAL
        call PreClassSfc(Ym%Year,Ym%Julday,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,stypeSfc,&
             TunParams(1)%Sensor_ID,Ym%tb,SfcClass,TskPreclass)
        !---Set the scene to write out metadata even though nattempts were 0
        call SetUpScene(GeophStatsT_Atm(1)%pres_lev(1:nLev),GeophStatsT_Atm(1)%pres_lay(1:nLay),&
             mean(Ym%angle(1:Ym%nchan)),Scene,SfcClass,iProf,Ym%lat,Ym%lon,Ym%node,     &
             Ym%julDAY,Ym%Year,Ym%secs,Ym%iscanPos,Ym%iscanLine,Ym%RelAziAngle,Ym%SolZenAngle)
        Scene%Ym       = Ym%tb
        Scene%YmCorr   = Ym%tb
        Scene%YFwd     = -99.
        Scene%nAttempt = 0
        Scene%nIter    =-99
        Scene%ChiSq    =-99.
      ENDIF
      !CALL CPU_TIME(t0)
      !---Set the QC for the scene just produced
      call setQC(Scene,nqc,Ym%qc,Ym%nqc,OKRetrvl)
      !---Set to default value all non-officially delivered products
      !call set2defaultNonOfficialproducts(Scene)

      clw(iprof) = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
           Scene%SfcPress,Scene%CLW(1:Scene%nLay))
      rwp(iprof) = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
           Scene%SfcPress,Scene%Rain(1:Scene%nLay))
      gwp(iprof) = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),        &
           Scene%SfcPress,Scene%Graupel(1:Scene%nLay))

      satqc(1:nqc,iprof)=Scene%qc(1:nqc)
      chisquare(iprof)=Scene%chisq
      emissivity(1:nchan,iprof)=Scene%emiss(1:nchan)
      tbf(1:nchan,iprof)=Scene%YFwd(1:nchan)
      
!      write(ilun,fmt='(i8,2x,i2,2x,i2,2x,2(f7.5,2x))') iprof,SfcClass,Scene%qc(1),Scene%Emiss(1:2)


!      print *,CLW(iprof),rwp(iprof),gwp(iprof),satqc(1,iprof),Scene%chisq

!      write(*,*),'iprof,qc,chisq',iprof,satqc(1,iprof),chisquare(iprof)
      !---Write out scene
       ! print *,'iuedr scene',iuedr,mype
      if (init_pass) then
          call WriteScene(iuedr,Scene)
      endif
!      call printscene(Scene)
      !----Assess time spent in outputing
      !CALL CPU_TIME(t1)
      !tim(9) = tim(9) + (t1-t0)
  ENDDO ProfLoop
  if (init_pass) then
     close(iuedr)
     close(iufg)
  endif

  !close(ilun)
  !---Assess total time cost
  !CALL CPU_TIME(t1)
  !Tim(1) = Tim(1) + (t1-t00)
  !---Print-on-screen major characteristics of the run
!  call printSummary(nprofiles,nIterTot,Tim,nConvProfs,nprofsEff,  &
!       nProfsOverOcean,nProfsOverSeaIce,nProfsOverLand,nProfsOverSnow,CntrlConfig_sub%MeasurmtFile,CntrlConfig_sub%OutputFile)
  !--------------------------------------------------------------------
  ! De-allocation of arrays
  !--------------------------------------------------------------------
  Print *,'Deallocation in progress...'
  DEALLOCATE(Se,Fe,SaAtm,SaSfc,XbAtm,XbSfc,X1stAtm,X1stSfc,Xb,Sa,X1st,Xg,Y,NoiseRMS,K, &
       SaAtmTot,SaSfcTot,XbAtmTot,XbSfcTot,UAtmTot,USfcTot,                            &
       Xout,nIterTot,nAttempTot,ChiSq,UAtm,USfc,U,ParamIndx,ParamLength,ParamLabel,    &
       ShrkRadVec,SeStar,dY2,FeStar,Ystar,YmStar,Kstar,ChanSel,DX,DXtilda,DXtildaNew,  &
       Ktilda,Ustar,Lambda,G,A,S,EDR_nEOF,EDR_cntrlRetr,EDR_ExtDataUse,iSpaceModeFlag, &
       EmissAnalyt,dnWelling_rad,upWelling_rad,opt_lay,ModelErrCFreq,ModelErr,         &
       ModelErrTot,tb,TBqc,node) 

  !--- Release memory allocated in ReadHdrMeasurmts of src/lib/io/IO_MeasurData.f90
  DEALLOCATE(Ym%CentrFreq,Ym%Rad,Ym%qc,Ym%Tb,Ym%polar,Ym%angle,Ym%secant_view)
  !--- Release memory allocated in LoadTunParams of src/lib/misc/TunParams.f90 
  DEALLOCATE(TunParams)
  !---Release memory allocated in ReadStats of src/lib/InversProcess/GeophCovBkg.f90
  DO iattempt=1,CntrlConfig_sub%nAttempts
    CALL Destroy_GeophStatsT_Atm( GeophStatsT_Atm(iattempt) )
    CALL Destroy_GeophStatsT_Sfc( GeophStatsT_Sfc(iattempt) )
  ENDDO
  !---Release memory allocated in LoadNoise of src/lib/noise/Noise.f90
  DEALLOCATE(noiseInfo%CentrFreq,noiseInfo%rms,noiseInfo%nedt)
  !---Release memory allocated in ReadHdrScene of src/lib/io/IO_Scene.f90
  IF (CntrlConfig_sub%ExternDataAvailab .ne. 0) CALL DestroyScene(Scene_Ext)
  CALL DestroyScene(Scene)
  !---Release memory allocated in readBias of src/lib/io/IO_MeasurData.f90
  IF (errReadingBias.eq.0) DEALLOCATE(cfreq_bias,Bias,Slope,Intercept)
  !---Release memory allocated in CRTM
!  Error_Status = CRTM_Destroy(ChannelInfo2)
!  CALL CRTM_RTSolution_Destroy(RTSolution)
!  CALL CRTM_RTSolution_Destroy(RTSolution_K)
!  CALL CRTM_Atmosphere_Destroy(Atmos)
!  CALL CRTM_Surface_Destroy(Sfc)
!  CALL CRTM_Atmosphere_Destroy(Atmos_K)
!  CALL CRTM_Surface_Destroy(Sfc_K)
!  CALL CRTM_Options_Destroy(Options)
  DEALLOCATE(nGsel)
  print *, '_____________'
  print *, 'End of 1dvar.'
  CALL CloseLogFile()

  RETURN

END SUBROUTINE pp1dvar_main


END MODULE pp1dvar
