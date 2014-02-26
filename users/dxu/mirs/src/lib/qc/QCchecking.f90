!$Id: QCchecking.f90 3030 2012-06-22 16:19:15Z flavio $
!-----------------------------------------------------------------------------------------------
! Name:         QCchecking
! 
! Type:         F90 module
!
! Description:
!       Module that contains various subroutines related to the 
!       quality control and other related processing.
!
! Modules needed:
!       - misc
!       - utils
!       - CntrlParams
!       - IO_Scene
!       - TuningParams
!       - SeFeErrCov
!       - GeophCovBkg
!       - Noise
!
! Subroutines contained:
!       - ConsistCheck
!       - GetNprofs2Process
!       - GetNprofs2Process_fwd(
!       - DetermOKsgnl
!       - PerfQConXg
!       - PerfQConScene
!       - Convgce
!       - setQC
!
! Data type included:
!       - none
!
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE QCchecking
  USE misc
  USE Consts
  USE utils
  USE ErrorHandling
  USE CntrlParams
  USE IO_Scene
  USE TuningParams
  USE SeFeErrCov
  USE GeophCovBkg
  USE Noise

  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: GetNprofs2Process,GetNprofs2Process_fwd,DetermOKsgnl
  PUBLIC :: PerfQConXg,PerfQConScene,Convgce,ConsistCheck
  PUBLIC :: setQC
  !---Declaration sections

  !---INTRINSIC functions used in this module
  INTRINSIC :: TRIM,ADJUSTL,ABS,ANY,BTEST,IBSET,LOG,SIZE,MINVAL,ALL

CONTAINS

!===============================================================
! Name:         ConsistCheck
!
!
! Type:         Subroutine
!
!
! Description:  Performs several consistency checks regarding 
!               - number of channels
!               - number of EDRs
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - YmNchan            I             # channels in Rad file
!       - CRTMnchan          I             # channels in CRTM file
!       - nAtm               I             # atmospheric parameters
!       - nSfc               I             # surface parameters
!       - nchanModelErr      I             # channels in Model errors file
!
!
! Modules needed:
!       - See USE in Module top
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ConsistCheck(YmNchan,CRTMnchan,nAtm,nSfc,nchanModelErr)  
    INTEGER :: YmNchan,CRTMnchan,nAtm,nSfc,nchanModelErr
    INTEGER :: iEDR,iattempt,iEDRatm,iEDRsfc
    IF (nchanModelErr .ne. YmnChan)   CALL ErrHandl(ErrorType,Err_FEneYM,'')  
    IF (YmnChan .ne. NoiseInfo%nChan) CALL ErrHandl(ErrorType,Err_SEneNOISE,'')
    IF (YmnChan .ne. CRTMnchan)       CALL ErrHandl(ErrorType,Err_SEneCRTM,'')
    AttemptLoop: DO iattempt=1,CntrlConfig%nattempts
       IF (nAtm    .ne. GeophStatsT_Atm(iattempt)%AllEDRstats(1)%npEDR) CALL ErrHandl(ErrorType,Err_CovAtmIncons,'')
       IF (nSfc    .ne. GeophStatsT_Sfc(iattempt)%AllEDRstats(1)%npEDR) CALL ErrHandl(ErrorType,Err_CovSfcIncons,'')
       !---QC-check number of EDRs (sfc and atmosphere)
       IF (TunParams(iattempt)%nEDRs_atm .ne. GeophStatsT_Atm(iattempt)%nEDRs) &
            CALL ErrHandl(ErrorType,Err_Atm_COVneTUN,'')
       IF (TunParams(iattempt)%nEDRs_sfc .ne. GeophStatsT_Sfc(iattempt)%nEDRs) &
            CALL ErrHandl(ErrorType,Err_Sfc_COVneTUN,'')
       !---QC-Check atmospheric labels
       DO iEDR=1,TunParams(iattempt)%nEDRs_atm
          IF (adjustl(trim(TunParams(iattempt)%EDR_Label_atm(iEDR))) .ne. &
               adjustl(trim(GeophStatsT_Atm(iattempt)%EDR_Desc(iEDR)))) &
               CALL ErrHandl(ErrorType,Err_Atm_LabelsInconsCov,'Either wrong order or different EDRs.')
       ENDDO
       !---QC-Check surface labels
       DO iEDR=1,TunParams(iattempt)%nEDRs_sfc
          IF (adjustl(trim(TunParams(iattempt)%EDR_Label_sfc(iEDR))) .ne. &
               adjustl(trim(GeophStatsT_Sfc(iattempt)%EDR_Desc(iEDR))))  &
               CALL ErrHandl(ErrorType,Err_Sfc_LabelsInconsCov,'Either wrong order or different EDRs.')
       ENDDO
       !---Check that Tskin is selected only once. Either with Sfc or with Atm 
       DO iEDRatm=1,TunParams(iattempt)%nEDRs_atm
          DO iEDRsfc=1,TunParams(iattempt)%nEDRs_sfc
             IF (TunParams(iattempt)%EDR_Label_sfc(iEDRsfc) .eq. &
                  TunParams(iattempt)%EDR_Label_atm(iEDRatm)) THEN
                IF (TunParams(iattempt)%EDR_nEOF_sfc(iEDRsfc) .ne. 0 .and. &
                     TunParams(iattempt)%EDR_nEOF_atm(iEDRatm) .ne. 0) THEN
                   CALL ErrHandl(ErrorType,Err_DuplicateEDRselection,'Please turn one OFF')
                ENDIF
             ENDIF
          ENDDO
       ENDDO
    ENDDO AttemptLoop
    RETURN
  END SUBROUTINE ConsistCheck


!===============================================================
! Name:         GetNprofs2Process
!
!
! Type:         Subroutine
!
!
! Description: Determines number of profiles to be processed
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - ExternDataAvailab  I              Flag if external data available (1) or not (0)
!       - nprofs2process     I              Number of profiles to process (requested)
!       - nMeasurData        I              Number of measurements data
!       - nExternData        I              Number of external data
!       - nprofiles          O              Number of profiles to be processed
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE GetNprofs2Process(ExternDataAvailab,nprofs2process,nMeasurData,nExternData,nprofiles)
    INTEGER                  :: ExternDataAvailab,nprofs2process
    INTEGER                  :: nMeasurData,nExternData,nprofiles
    !---Warn user if number(s) not identical
    IF (nMeasurData .ne. nprofs2process) &
         CALL ErrHandl(WarningType,Warn_nProfDiffMeasAndCntrl,'')
    IF (ExternDataAvailab .ne. 0) THEN !Use of external data requested by user
       IF (nMeasurData .ne. nExternData) CALL ErrHandl(WarningType,Warn_nProfDiffMeasAndExt,'')
    ENDIF
    !---Minimum number used for the output
    nprofiles=minval((/nMeasurData,nprofs2process/))
    IF (ExternDataAvailab .ne. 0) nprofiles=minval((/nprofiles,nExternData/))
    RETURN
  END SUBROUTINE GetNprofs2Process


!===============================================================
! Name:         GetNprofs2Process_fwd
!
!
! Type:         Subroutine
!
!
! Description:  Determines number of profiles to be processed in fwd operator
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nprofs2process     I              Requested number of profiles to be processed
!       - nPrf               I              Available number of profiles
!       - nprofiles          O              Number of profiles to be processed
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE GetNprofs2Process_fwd(nprofs2process,nPrf,nprofiles)
    INTEGER :: nprofs2process,nPrf,nprofiles
    IF (nPrf .ne. nprofs2process) CALL ErrHandl(WarningType,Warn_nProfDiffAvailReques,'')
    !---Minimum number used for the output
    nprofiles=minval((/nprofs2process,nPrf/))
    RETURN
  END SUBROUTINE GetNprofs2Process_fwd


!===============================================================
! Name:         DetermOKsgnl
!
!
! Type:         Subroutine
!
!
! Description: Determines if retrieval should go ahead, depending 
!              on many factors (location, QC, etc).
!
!
! Arguments:
!
!          Name                    Type            Description
!      ---------------------------------------------------
!       - extDataAvail               I              Available external data flag
!       - tbYm                       R              Brightness Temperatures
!       - qcYm                       I              QC of the radiances
!       - qcXg                       I              QC of the params
!       - GeogrLimits                I              Selection Flag (of Geogr. limits) 
!       - minLat                     I              Min latitude of geogr limits
!       - maxLat                     I              Max latitude
!       - minLon                     I              Min Longitude
!       - maxLon                     I              Max longitude
!       - pass                       I              Passing mode (asc/desc)
!       - lat                        I              Latitude of measurements
!       - lon                        I              Longitude of measurements
!       - node                       I              Selected pass
!       - stypeSfc                   I              Surface type
!       - OkRetrvl                   O              Log. signal (true->retrieval can go ahead)
!
!
! Modules needed:
!       - DetermGeogrLimits
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE DetermOKsgnl(extDataAvail,tbYm,qcYm,qcXg,GeogrLimits,minLat,maxLat,     &
       minLon,maxLon,pass,lat,lon,node,stypeSfc,OkRetrvl)

    INTEGER,    DIMENSION(:) :: qcYm
    INTEGER(2), DIMENSION(:) :: qcXg
    LOGICAL                  :: OkRetrvl,OkGeogrLimits
    INTEGER                  :: GeogrLimits,stypeSfc,pass,node,extDataAvail
    REAL,       DIMENSION(:) :: tbYm
    REAL                     :: minLat,maxLat,minLon,maxLon
    REAL                     :: lat,lon

    OkRetrvl =.TRUE.
    IF (qcYm(1).ne.0)     OkRetrvl=.FALSE.
    IF (ALL(tbYm .lt. 0)) OkRetrvl=.FALSE.
    !IF (extDataAvail .ne. 0) THEN 
    !   IF (qcXg(1).ne.0)  OkRetrvl=.FALSE.
    !ENDIF
    CALL DetermGeogrLimits(OkGeogrLimits,GeogrLimits,stypeSfc,lat,lon,minLat,&
         maxLat,minLon,maxLon,pass,node)
    OkRetrvl = OkRetrvl .and. OkGeogrLimits
    RETURN
  END SUBROUTINE DetermOKsgnl


!===============================================================
! Name:         PerfQConXg
!
!
! Type:         Subroutine
!
!
! Description: Corrects in an adhoc fashion the values of the 
!              state vector. This function is not implemented.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Xg                 I/O            State vector
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE PerfQConXg(Xg)
    REAL, DIMENSION(:) :: Xg
    !---Modify the non-realistic values of Xg
    Xg=Xg
    RETURN
  END SUBROUTINE PerfQConXg




!===============================================================
! Name:         PerfQConScene
!
!
! Type:         Subroutine
!
!
! Description: Performs QC checking and correction on the Scene 
!              structure.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Scene              I/O            Scene
!       - nGselec            I              # geophysical parameters selected
!       - Sa                 I              Covariance matrix
!       - DX                 O              State vector departure from the background
!       - Xb                 I              Background vector
!       - Xg                 O              Contains the state vector (from Scene)
!       - Label              I              Parameters Labels vector  
!       - Indx               I              Parameters Index vector (within Xg vector)
!       - ParamLength        I              Parameters lengths vector (within Xg)
!       - iSpaceMode         I              Parameters mode vector (natural, logarithm, etc)
!       - nEDRselec          I              Number of EDRs selected.
!       - rhMaxAllowed       I              Maximum relative humidity allowed (used to reset)
!
!
! Modules needed:
!       - PutIntoXg
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE PerfQConScene(Scene,nGselec,Sa,DX,Xb,Xg,Label,Indx,ParamLength,iSpaceMode,nEDRselec,rhMaxAllowed)
    TYPE(Scene_type)                 :: Scene    
    REAL,             DIMENSION(:)   :: Xg,Xb,DX
    REAL,             DIMENSION(:,:) :: Sa
    CHARACTER(LEN=*), DIMENSION(:)   :: Label
    INTEGER,          DIMENSION(:)   :: Indx,ParamLength,iSpaceMode
    INTEGER                          :: nGselec,nEDRselec,iEDR,iG,nG,Imode,i
    REAL,             DIMENSION(Scene%nLay)  :: Temp,Press,MixRatio,MixRatioNew
    REAL,             DIMENSION(Scene%nchan) :: Emissnew
    REAL                                     :: mixr0,rh,rhMaxAllowed,emiss0
    Temp        = Scene%Temp_Lay(1:Scene%nLay)
    Press       = Scene%Pres_Lay(1:Scene%nLay)
    MixRatio    = Scene%Absorb_lay(1:Scene%nLay,1)
    MixRatioNew = MixRatio
    EmissNew    = Scene%emiss(1:Scene%nChan)
    !---Loop over the EDRs
    DO iEDR=1,nEDRselec
       iG    = Indx(iEDR)
       nG    = ParamLength(iEDR)
       iMode = iSpaceMode(iEDR)
       !---Check super-saturation (saturated vapor and relative humidity)
       IF(adjustl(trim(Label(iEDR))).eq.'WVAP') THEN
          IF (nG.ne.Scene%nLay) CALL ErrHandl(ErrorType,Err_InconsSize4XGandScene,'(in PerfQConScene for water vapor)')
          LayLoop: DO i=1,nG
             IF (Press(i) .gt. Scene%SfcPress) EXIT LayLoop
             mixr0=MixRatio(i)
             rh=Mixingratio_to_RelHum(mixr0/1000.,Temp(i),Press(i))
             IF (rh .gt. rhMaxAllowed/100.) THEN
                mixr0=RelHum_to_mixingratio(rhMaxAllowed/100.,Temp(i),Press(i))
                MixRatioNew(i) = mixr0
             ENDIF
          ENDDO LayLoop
          Scene%Absorb_lay(1:nG,1)=MixRatioNew(1:nG)
          CALL PutIntoXg(Xg,iG,nG,Scene%Absorb_lay(1:nG,1),iMode)
          DX(iG:iG+nG-1) = Xg(iG:iG+nG-1) - Xb(iG:iG+nG-1)
       ENDIF
       !---Check emissivity values realisticness
       IF(adjustl(trim(Label(iEDR))).eq.'EMIS') THEN
          IF (nG.ne.Scene%nchan) CALL ErrHandl(ErrorType,Err_InconsSize4XGandScene,'(in PerfQConScene for emissivity)')
          ChanLoop: DO i=1,nG
             emiss0=EmissNew(i)  
             IF (emiss0.gt.1.) EmissNew(i) = 1.
             IF (emiss0.lt.0.) EmissNew(i) = 0.
          ENDDO ChanLoop
          Scene%emiss(1:nG)=EmissNew(1:nG)
          CALL PutIntoXg(Xg,iG,nG,Scene%emiss(1:nG),iMode)
          DX(iG:iG+nG-1) = Xg(iG:iG+nG-1) - Xb(iG:iG+nG-1)
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE PerfQConScene


!===============================================================
! Name:         Convgce
!
!
! Type:         Subroutine
!
!
! Description: Computes the convergence metric Xi2 and determines 
!              the logical signal: converged or not-converged.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - ChanSel            I             Selected channels vector
!       - Ym                 I             Measured radiances
!       - Y                  I             Simulated radiances
!       - Noise              I             Noise vector
!       - ChiSq              O             Convergence metric
!       - CvgceReached       O             Logical sgnl (false: nonConv, true:Conv)
!       - ChiSqThresh        I             Xi2 Threshold used to determine cvgce
!       - DY2                O             Square of radiances departures from measurmts
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE Convgce(ChanSel,Ym,Y,Noise,ChiSq,CvgceReached,ChiSqThresh,DY2)
    REAL,    DIMENSION(:) :: Ym,Y,Noise,dY2
    INTEGER, DIMENSION(:) :: ChanSel
    REAL                  :: ChiSq,ChiSqThresh
    LOGICAL               :: CvgceReached
    INTEGER               :: nchan,nch,ichan
    ChiSq = 0.
    nchan = size(Y)
    nch   = 0
    DO ichan=1,nchan
       dY2(ichan) = 0.
       IF (ChanSel(ichan).EQ.1) THEN
          nch=nch+1
          dY2(ichan)= (Ym(ichan)-Y(ichan))**2.
          ChiSq=ChiSq+dY2(ichan)/(Noise(ichan)**2.)
       ENDIF
    ENDDO
    !ChiSq=sqrt(ChiSq/nCh)
    ChiSq=(ChiSq/nCh)
    CvgceReached=.FALSE.
    IF (ChiSq.le.ChiSqThresh) CvgceReached=.TRUE.
    RETURN
  END SUBROUTINE Convgce



!====================================================================
! Name:         setQC
!
!
! Type:         Subroutine
!
!
! Description: Sets the quality control flags of the MIRS retrieved 
!              products.
!
! History:
!       05-14-2008     Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/ORA
!
!===================================================================

  SUBROUTINE setQC(Scene,nqc,Ymqc,Ymnqc,OKRetrvl)
    IMPLICIT NONE
    !---I/O variables   
    TYPE(Scene_type)                          :: Scene
    INTEGER                                   :: nqc
    INTEGER,         DIMENSION(:)             :: Ymqc
    INTEGER                                   :: Ymnqc
    LOGICAL                                   :: OKRetrvl
    !--- Variable declaration   
    INTEGER                                   :: iLay,ichan,iqc,count,countSS,imax
    INTEGER(2),      DIMENSION(nqc)           :: QC
    REAL                                      :: ChiSqMAX=10.0
    REAL,            PARAMETER                :: ChiSqMIN=5.0,RWPMIN=0.05,RWPMED=0.2,RWPMAX=1.0
    REAL,            PARAMETER                :: GWPMINVALIDITY=0.01
    REAL,            PARAMETER                :: TSKINMIN=150.0,TSKINMAX=350.0,dTMAX=3.0
    REAL,            PARAMETER                :: TEMPMIN=150.0,TEMPMAX=350.0,QMIN=0.0,QMAX=90.0
    REAL,            PARAMETER                :: EMISSMIN=0.0,EMISSMAX=1.0,TPWMIN=0.0,TPWMAX=120.0
    REAL,            PARAMETER                :: ICLWMIN=0.0,ICLWMAX=10.0,RWPBMIN=0.0,RWPBMAX=15.0,CLRSKY=0.02
    REAL,            PARAMETER                :: GWPMIN=0.0,GWPMAX=4.0,MAXPRESQ=200
    REAL,            PARAMETER                :: RO=287.053,g=9.80665,w=0.61
    REAL                                      :: TPW,ICLW,RWP,GWP,SWP,IWP
    REAL                                      :: Tvt,dH,dT,LR,RH,dQ
    
    CALL ComputeTPW(Scene%pres_lev(1:Scene%nLev),&
         Scene%SfcPress,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2O),TPW)
    ICLW=ColumIntegr(Scene%nParmCLW,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%clw(1:Scene%nParmCLW))
    GWP=ColumIntegr(Scene%nParmGrpl,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%graupel(1:Scene%nParmGrpl))
    RWP=ColumIntegr(Scene%nParmRain,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%rain(1:Scene%nParmRain))
    SWP=ColumIntegr(Scene%nParmSnow,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%snow(1:Scene%nParmSnow))
    IWP=ColumIntegr(Scene%nParmIce,Scene%pres_lev(1:Scene%nLev),Scene%SfcPress,Scene%ice(1:Scene%nParmIce))
    QC(:)=0
    
    !-Defining Maximum ChiSq Threshold
    IF (Scene%nAttempt .EQ. 1) ChiSqMAX=10.0
    IF (Scene%nAttempt .EQ. 2) ChiSqMAX=100.0
    !-Convergence flag
    IF (Scene%ChiSq >= ChiSqMax) QC(2)=IBSET(QC(2),0)
    IF (Scene%ChiSq >= ChiSqMin .AND. Scene%ChiSq < ChiSqMax) QC(2)=IBSET(QC(2),1)
    !-Precipitation flag
    IF (RWP >= RWPMIN)           QC(2)=IBSET(QC(2),2)
    !-Type of precipitation flag
    IF (RWP >= RWPMIN .AND. RWP <  RWPMED ) QC(2)=IBSET(QC(2),3)
    IF (RWP >= RWPMED .AND. RWP <  RWPMAX ) QC(2)=IBSET(QC(2),4)
    IF (RWP >= RWPMAX )                     QC(2)=IBSET(QC(2),5)
    !-Skin Temperature out-of-bound flag
    IF (Scene%Tskin < TSKINMIN .OR. Scene%Tskin > TSKINMAX)  QC(2)=IBSET(QC(2),6)
    !-Out-of-bound flags
    ProfTemp: DO iLay=1,Scene%Nlay
       IF (Scene%Temp_lay(iLay) < TEMPMIN .OR. Scene%Temp_lay(iLay) > TEMPMAX)   QC(2)=IBSET(QC(2),7) 
       IF (BTEST(QC(2),7) .EQV. .TRUE.) EXIT ProfTemp
    ENDDO ProfTemp
    ProfQ: DO iLay=1,Scene%Nlay
       IF (Scene%Absorb_lay(iLay,1) < QMIN .OR. Scene%Absorb_lay(iLay,1) > QMAX) QC(2)=IBSET(QC(2),8) 
       IF (BTEST(QC(2),8) .EQV. .TRUE.) EXIT ProfQ
    ENDDO ProfQ
    ProfEmiss: DO ichan=1,Scene%nchan
       IF (Scene%Emiss(ichan) < EMISSMIN .OR. Scene%Emiss(ichan) > EMISSMAX)     QC(2)=IBSET(QC(2),9) 
       IF (BTEST(QC(2),9) .EQV. .TRUE.) EXIT ProfEmiss
    ENDDO ProfEmiss
    IF (TPW < TPWMIN .OR. TPW > TPWMAX)      QC(2)=IBSET(QC(2),10) 
    IF (ICLW < ICLWMIN .OR. ICLW > ICLWMAX)  QC(2)=IBSET(QC(2),11)
    IF (RWP < RWPBMIN .OR. RWP > RWPBMAX)    QC(2)=IBSET(QC(2),12)
    IF (GWP < GWPMIN .OR. GWP > GWPMAX)      QC(2)=IBSET(QC(2),13)
    !-Measurement QC flag
    IF (ANY(Ymqc .GT. 0))      QC(2)=IBSET(QC(2),14)
    DO iqc=0,Ymnqc-1
       IF (Ymqc(iqc+1) .gt. 0) QC(4)=IBSET(QC(4),iqc)
    ENDDO
    
    !-Check Scene for 'nan' in profiles
    !IF (ANY(Scene%Temp_Lay .EQ. 'nan') .OR. ANY(Scene%Absorb_Lay(:,1) .EQ. 'nan') .OR. ANY(Scene%CLW .EQ. 'nan')) THEN
    !   QC(2)=IBSET(QC(2),15)
    !ENDIF   
    
    !-make use of the property that a NaN does not compare equal to anything, including itself
    DO iLay=1,Scene%Nlay
      IF( Scene%Temp_Lay(iLay) .NE. Scene%Temp_Lay(iLay) .OR. Scene%Absorb_Lay(iLay,1) .NE. Scene%Absorb_Lay(iLay,1) ) & 
        QC(2)=IBSET(QC(2),15)
    ENDDO
    DO iLay=1,Scene%nParmCLW
       IF( Scene%CLW(iLay) .NE. Scene%CLW(iLay) ) QC(2)=IBSET(QC(2),15)    
    ENDDO
    
    !-Temperature lapse rate and Temperature inversion flag
    count=0
    DO iLay=1,Scene%Nlay
       IF (Scene%Pres_lay(iLay) <= Scene%SfcPress) count=count+1
    ENDDO
    DO iLay=1,count-1
       Tvt=Scene%Temp_lay(iLay+1)*(1+w*(Scene%Absorb_lay(iLay+1,1)/1000.0))
       dH=((RO*Tvt)/g)*(LOG(Scene%Pres_lay(iLay+1)/Scene%Pres_lay(iLay)))*0.001
       dT=(Scene%Temp_lay(iLay)-Scene%Temp_lay(iLay+1))
       LR=dT/dH
       IF(ABS(LR) > 10.0) QC(3)=IBSET(QC(3),0)
       IF(Scene%Pres_lay(iLay) >= Scene%SfcPress-200) THEN
          IF(dT > dTMAX)   QC(3)=IBSET(QC(3),1)
       ENDIF
    ENDDO
    !-Supersaturation flag
    ProfSupS: DO iLay=1,count
       RH=Mixingratio_to_RelHum(Scene%Absorb_lay(iLay,1)/1000.,Scene%Temp_lay(iLay),Scene%Pres_lay(iLay))
       IF(RH >= 0.999) QC(3)= IBSET(QC(3),2)
       IF (BTEST(QC(3),2) .EQV. .TRUE.) EXIT ProfSupS
    ENDDO ProfSupS
    countSS=0
    imax=0
    ProfSupS3L: DO iLay=1,count
       RH=Mixingratio_to_RelHum(Scene%Absorb_lay(iLay,1)/1000.,Scene%Temp_lay(iLay),Scene%Pres_lay(iLay))
       IF(RH >= 0.999) countSS=countSS+1
       IF(countSS == 1 .AND. imax == 0) imax=iLay+2
       IF(countSS == 3 .AND. iLay == imax) THEN
          QC(3)= IBSET(QC(3),3)
          IF (BTEST(QC(3),3) .EQV. .TRUE.) EXIT ProfSupS3L
       ENDIF
       IF(iLay >= imax) THEN
          countSS=0
          imax=0
       ENDIF
    ENDDO ProfSupS3L
    !-Humidity inversion flag
    DO iLay=1,count-1
       IF(Scene%Pres_lay(iLay) > MAXPRESQ ) THEN
          dQ=Scene%Absorb_lay(iLay,1) - Scene%Absorb_lay(iLay+1,1)
          IF(dQ > 0 ) QC(3)= IBSET(QC(3),4)
       ENDIF
    ENDDO
    !-Cloud flag
    IF (ICLW > CLRSKY) QC(3)=IBSET(QC(3),5)
    !-Validity flags
    IF (RWP >= RWPMIN .OR. GWP >= GWPMINVALIDITY) THEN
      QC(3)= IBSET(QC(3),6) ! Tskin
      QC(3)= IBSET(QC(3),7) ! Temperature Profile
!      QC(3)= IBSET(QC(3),8) ! Water Vapor Profile
      QC(3)= IBSET(QC(3),9) ! Emissivity
      QC(3)= IBSET(QC(3),10)! TPW
      QC(3)= IBSET(QC(3),11)! CLWP
    ENDIF
    IF (Scene%iTypSfc .ne. OC_TYP) QC(3)= IBSET(QC(3),7)
    !-Ocean and Land surface type flag
    IF (Scene%iTypSfc == OC_TYP)  QC(4)=IBSET(QC(4),12)
    IF (Scene%iTypSfc == LD_TYP)  QC(4)=IBSET(QC(4),13)
    !-QC(1) flag: 0=GOOD,1=SOME PROBLEM,2=BAD 
    IF((BTEST(QC(2),0) .EQV. .FALSE.) .AND. (BTEST(QC(2),1) .EQV. .FALSE.) .AND. (BTEST(QC(2),2) .EQV. .FALSE.) &
       .AND. (BTEST(QC(2),6) .EQV. .FALSE.) .AND. (BTEST(QC(2),7) .EQV. .FALSE.) .AND. (BTEST(QC(2),8) .EQV. .FALSE.) &
       .AND. (BTEST(QC(2),9) .EQV. .FALSE.) .AND. (BTEST(QC(2),10) .EQV. .FALSE.) .AND. (BTEST(QC(2),11) .EQV. .FALSE.) &
       .AND. (BTEST(QC(2),12) .EQV. .FALSE.) .AND. (BTEST(QC(2),13) .EQV. .FALSE.) .AND. (BTEST(QC(3),0) .EQV. .FALSE.) &
       .AND. (BTEST(QC(3),1) .EQV. .FALSE.) .AND. (BTEST(QC(3),2) .EQV. .FALSE.) .AND. (BTEST(QC(3),4) .EQV. .FALSE.)) QC(1)=0
    IF((BTEST(QC(2),0) .EQV. .FALSE.) .AND. (BTEST(QC(2),6) .EQV. .FALSE.) &
       .AND. (BTEST(QC(2),7) .EQV. .FALSE.) .AND. (BTEST(QC(2),8) .EQV. .FALSE.) .AND. (BTEST(QC(2),9) .EQV. .FALSE.) &
       .AND. (BTEST(QC(2),10) .EQV. .FALSE.) .AND. (BTEST(QC(2),11) .EQV. .FALSE.) .AND. (BTEST(QC(2),12) .EQV. .FALSE.) &
       .AND. (BTEST(QC(2),13) .EQV. .FALSE.)) THEN
       IF ((BTEST(QC(2),2) .EQV. .TRUE.) .OR. (BTEST(QC(3),0) .EQV. .TRUE.) .OR. (BTEST(QC(3),1) .EQV. .TRUE.) &
            .OR. (BTEST(QC(3),2) .EQV. .TRUE.) .OR. (BTEST(QC(3),4) .EQV. .TRUE.) .OR. (BTEST(QC(2),1) .EQV. .TRUE.)) QC(1)=1
    ENDIF
    IF((BTEST(QC(2),0) .EQV. .TRUE.) .OR. (BTEST(QC(4),0) .EQV. .TRUE.) .OR. (BTEST(QC(2),6) .EQV. .TRUE.) &
         .OR. (BTEST(QC(2),7) .EQV. .TRUE.) .OR. (BTEST(QC(2),8) .EQV. .TRUE.) .OR. (BTEST(QC(2),9) .EQV. .TRUE.) &
         .OR. (BTEST(QC(2),10) .EQV. .TRUE.) .OR. (BTEST(QC(2),11) .EQV. .TRUE.) .OR. (BTEST(QC(2),12) .EQV. .TRUE.) &
         .OR. (BTEST(QC(2),13) .EQV. .TRUE.) .OR. (BTEST(QC(2),15) .EQV. .TRUE.) .OR. (OKRetrvl .EQV. .FALSE.)) QC(1)=2
    !-Set Scene EDRs to -999 if QC(1) equals 2 and set Scene%qc to qc
    IF (BTEST(QC(2),15) .EQV. .TRUE.) call setSceneEDRs2default(Scene)
    Scene%qc(:)=QC(:) 
  END SUBROUTINE setQC


END MODULE QCchecking




