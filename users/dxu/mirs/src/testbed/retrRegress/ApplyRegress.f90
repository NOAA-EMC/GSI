!$Id: ApplyRegress.f90 3351 2013-10-09 17:38:27Z amims $
!===============================================================
! Name:     ApplyRegress  
!
!
! Type:         Main Program
!
!
! Description:
!       Program that Applies regression algorithms to generate
!       external data to be used with MIRS.
!
! Modules needed:
!       - Consts
!       - misc
!       - utils
!       - GeophCovBkg
!       - IO_Scene
!       - IO_MeasurData
!       - IO_Regress
!       - ErrorHandling
!       - Preclassif
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       09-24-2008      Kevin Garrett - New coeff file for each edr and sfc type
!                                       Include CLW retrieved on corrected TBs
!                                       Tskin,TPW,T,Q on uncorrected TBs
!
!===============================================================

Program ApplyRegress
  USE Consts
  USE misc  
  USE TuningParams
  USE CntrlParams
  USE utils
  USE GeophCovBkg
  USE IO_Scene
  USE IO_MeasurData
  USE IO_Misc
  USE IO_Regress
  USE ErrorHandling
  USE Preclassif
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: LOG,ABS,ANY,COS,COUNT,EXP,MAXVAL,MINVAL,TRIM

  INTEGER,            PARAMETER                     :: mxSfc=6,nSfcTypes=4,mxElemts=120,mxIndepVar=50,mxAlgs=7,mxAngBins=10
  INTEGER,            PARAMETER                     :: nAbsorb=2,nqc=4
  INTEGER                                           :: iu_list,nFiles,nAngleBins,ifile,ialgo,iTyp,iAng
  INTEGER                                           :: nprofiles,iprof,iu,iuOut,ierr,iLay,iAngBin,cnt
  INTEGER                                           :: nLay,nLev,nchan2use,ScanPos_Factor
  INTEGER                                           :: stype,stypeCRTM,IselectsfcTyp,SfcClass,AtmClass,mqc
  REAL                                              :: xalt,xcover,AngInRads,cosAngle
  REAL                                              :: PresTop,PresBot
  TYPE(Scene_type)                                  :: Scene
  TYPE(MeasurData_type)                             :: Ym
  CHARACTER(LEN=250), DIMENSION(:), POINTER         :: RadMeasFiles,REGRESS_Files
  INTEGER,            DIMENSION(nAbsorb)            :: AbsorbID=(/1,3/)
  CHARACTER(LEN=40),  DIMENSION(mxAlgs)             :: Labels
  INTEGER,            DIMENSION(mxAlgs)             :: nElemts
  INTEGER,            DIMENSION(mxAlgs,mxSfc)       :: nIndepVar
  INTEGER,            DIMENSION(mxAlgs,mxSfc,mxIndepVar) :: TbIndxArr,FormTBIndxArr
  REAL,               DIMENSION(mxAlgs,mxSfc,mxElemts,mxIndepVar,mxAngBins) :: Algors
  REAL,               DIMENSION(mxElemts)           :: cfreq,presLay,presLev
  REAL,               DIMENSION(mxElemts)           :: EDR
  INTEGER,            DIMENSION(mxElemts)           :: pol
  INTEGER,            DIMENSION(nqc)                :: qc
  INTEGER(2),         DIMENSION(nqc)                :: qcIntp
  INTEGER,            DIMENSION(mxAngBins+1)        :: AngleBins
  REAL,               DIMENSION(:),    ALLOCATABLE  :: Emiss2Adjust
  REAL                                              :: tpw,angle,TskPreclass,clw,gwp
  !--Bias-related variables
  REAL,               DIMENSION(:),    POINTER      :: Cfreq_Bias
  REAL,               DIMENSION(:,:),  POINTER      :: Bias,Slope,Intercept
  REAL,               DIMENSION(:),    ALLOCATABLE  :: tb
  INTEGER                                           :: nchanBias,nposBias,ichan,errReadingBias
  !---Covariance Matrix variables
  INTEGER                                           :: isfc,nsfc,iatm,natm
  REAL                                              :: ScalFac
  REAL,               DIMENSION(:,:), ALLOCATABLE   :: TempBkg,WVapBkg,CLWBkg,GrplBkg,EmissBkg
  REAL,               DIMENSION(:),   ALLOCATABLE   :: TskinBkg,tpwBkg,iCLWBkg,iGWPBkg
  REAL,               DIMENSION(:,:), ALLOCATABLE   :: SaAtm,SaSfc
  REAL,               DIMENSION(:),   ALLOCATABLE   :: XbAtm,XbSfc
  !---Namelist data 
  INTEGER            :: nAlgors=DEFAULT_VALUE_INT
  INTEGER            :: sensor_id=DEFAULT_VALUE_INT
  INTEGER            :: nOrbits2Process=DEFAULT_VALUE_INT
  INTEGER            :: AlgSN=DEFAULT_VALUE_INT
  CHARACTER(LEN=250),DIMENSION(mxAlgs,nSfcTypes) :: algorsfiles=DEFAULT_VALUE_STR4 
  CHARACTER(LEN=250) :: ListRadMeasFiles=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: pathRegress=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: Topogr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: BiasFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: CovBkgFileAtm=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: CovBkgFileSfc=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: LogFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250) :: TuningFile=DEFAULT_VALUE_STR4

  NAMELIST /ContrlRegress/nAlgors,algorsfiles,ListRadMeasFiles,pathRegress,Topogr,CovBkgFileAtm,&
       CovBkgFileSfc,LogFile,BiasFile,sensor_id,nOrbits2Process,TuningFile,AlgSN
  !---Read control-data from namelist
  READ(*,NML=ContrlRegress)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Load tuning paramaters
  call LoadTunParams(1,(/TuningFile/))
  !---Load the global/stratified cov matrix -Atm & Sfc- (for background values)
  CntrlConfig%nAttempts=1
  CntrlConfig%CovBkgFileAtm=CovBkgFileAtm
  CntrlConfig%CovBkgFileSfc=CovBkgFileSfc
  call LoadGlobGeophyCovBkg()
  !---Read the file names of EC-FM- SDRs
  call ReadList(iu_list,trim(ListRadMeasFiles),RadMeasFiles,nFiles,REGRESS_Files,pathRegress,'REGRESS')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(in Applyregress)') 
  nFiles=minval((/nFiles,nOrbits2Process/))
  !---Read algorithms
  DO ialgo=1,nAlgors
    DO iTyp=1,nSfcTypes
       CALL readRegressAlgor(AlgorsFiles(ialgo,iTyp),Algors,Labels,nElemts,nIndepVar,nSfcTypes,TbIndxArr,&
            FormTBIndxArr,ialgo,iTyp,nLay,nLev,nchan2use,nAngleBins,AngleBins,cfreq,pol,presLay,presLev)
    ENDDO
  ENDDO
  !---Read Bias info
  call ReadBias(BiasFile,nchanBias,nposBias,cfreq_bias,Bias,Slope,Intercept,errReadingBias)
  !call ReadBias(BiasFile,nchanBias,nposBias,nGeophParam,cfreq_bias,Bias,Slope, &
  !    Intercept,meanTBsimu,meanTBmeas,constBias,regBias,errReadingBias)
  nAtm       = computeNG(GeophStatsT_Atm(1))
  nSfc       = computeNG(GeophStatsT_Sfc(1))
  ALLOCATE(tb(nChanBias),Emiss2Adjust(nChanBias),SaAtm(nAtm,nAtm),SaSfc(nSfc,nSfc),XbAtm(nAtm),XbSfc(nSfc),  &
       EmissBkg(nChanBias,nSfcTypes),TempBkg(nLay,nSfcTypes),WVapBkg(nLay,nSfcTypes),CLWBkg(nLay,nSfcTypes), &
       GrplBkg(nLay,nSfcTypes),TskinBkg(nSfcTypes),TPWBkg(nSfcTypes),iCLWBkg(nSfcTypes),iGWPBkg(nSfcTypes))
  !---Get climate backgrounds for each surface type
  DO iTyp=1,nSfcTypes
    SfcClass=iTyp-1
    AtmClass=iTyp-1
    call DetermIndx(AtmClass,iAtm,GeophStatsT_Atm(1)%nTypes,GeophStatsT_Atm(1)%Type_IDs)
    call DetermIndx(SfcClass,iSfc,GeophStatsT_Sfc(1)%nTypes,GeophStatsT_Sfc(1)%Type_IDs)
    call GetGeophBkgCov(SaAtm,XbAtm,iAtm,SaSfc,XbSfc,iSfc,1)
    TempBkg(1:nLay,iTyp)       = XbAtm(1:100)
    WVapBkg(1:nLay,iTyp)       = exp(XbAtm(101:200))
    CLWBkg(1:nLay,iTyp)        = exp(XbAtm(301:400))
    GrplBkg(1:nLay,iTyp)       = exp(XbAtm(701:800))
    EmissBkg(1:nchanBias,iTyp) = XbSfc(1:nchanBias)
    TskinBkg(iTyp)             = XbAtm(801)
    iCLWBkg(iTyp)=ColumIntegr(nLay,presLay(1:nLay),1000.,CLWBkg(1:nLay,iTyp))
    iGWPBkg(iTyp)=ColumIntegr(nLay,presLay(1:nLay),1000.,GrplBkg(1:nLay,iTyp))
    CALL ComputeTPW(presLay(1:nLay),1000.,WVapBkg(1:nLay,iTyp),tpwBkg(iTyp))
  ENDDO
  !------------------------------------------------------------------------------
  !  Read radiances (measurements), apply regr-based algors and output 
  !------------------------------------------------------------------------------
  FileLoop: DO ifile=1,nFiles 
    write(*,*)
    write(*,*) 'ApplyRegress',ifile
    write(*,*) 'Input    FMSDR='//TRIM(RadMeasFiles(ifile))
    write(*,*) 'Output REGRESS='//TRIM(REGRESS_Files(ifile))
    call ReadHdrMeasurmts(RadMeasFiles(ifile),iu,nProfiles,Ym)
    !IF (Ym%nchan .ne. nchan) CALL ErrHandl(ErrorType,Err_InconsNumber,' of channels (regress/rad) (in Applyregress)')
    IF (errReadingBias.eq.0) THEN 
       IF (Ym%nChan .ne. nchanBias) CALL ErrHandl(ErrorType,Err_InconsNumber,' of channels in RAD/Bias')
       IF (ANY(abs(Ym%CentrFreq(1:Ym%nChan)-CFreq_bias(1:Ym%nChan)).gt.epsilonLoose)) THEN
          DO ichan=1,Ym%nChan
             print *, ichan,' radFreq:',Ym%CentrFreq(ichan),' BiasFreq:',CFreq_bias(ichan)
          enddo
          CALL ErrHandl(ErrorType,Err_InconsFreq,' in RAD/Bias files')
       ENDIF
    ENDIF
    !---Determine scan position factor for indexing coefficients (to deal with higher resolution data)
    ScanPos_Factor=Ym%nPosScan/5
    !---Write regress-based scene file (header)
    CALL InitHdrScene(nLev,nLay,Ym%nChan,cfreq(1:Ym%nchan),pol(1:Ym%nchan),presLev(1:nLev), &
         presLay(1:nLay),Scene,nLay,nLay,nLay,nLay,nLay,nAbsorb,AbsorbID(1:nAbsorb), &
         nqc,0,Ym%nPosScan,Ym%nScanLines,AlgSN)
    CALL WriteHdrScene(iuOut,REGRESS_Files(ifile),Scene,nProfiles)
    !---Loop over the profiles
    ProfLoop: DO iprof=1,nprofiles
       qc(:) = 0
       mqc   = 0
       CLW   = 0.
       GWP   = 0.
       iAngBin = DEFAULT_VALUE_INT
       TskPreclass = DEFAULT_VALUE_REAL
       !---Read the measurement(s)
       call ReadMeasurmts(iu,Ym,ierr)
       IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfLoop
       IF (ierr.eq.Warn_readInvalid) CYCLE ProfLoop
       IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. Radiance file. (ApplyRegress)')
       angle=mean(Ym%angle(1:Ym%nchan))
       AngInRads = angle*(3.14159265/180.)
       cosAngle  = cos(AngInRads)
       !---Determine angle bin to read coefficients from for regression
       IF (sensor_id .eq. sensor_id_n18 .or. sensor_id .eq. sensor_id_metopA .or. &
           sensor_id .eq. sensor_id_n19 .or. sensor_id .eq. sensor_id_metopB .or. & 
           sensor_id .eq. sensor_id_npp) THEN
          DO iAng=1,nAngleBins
             IF ( angle .ge. AngleBins(iAng) .and. angle .lt. AngleBins(iAng+1) ) iAngBin = iAng
          ENDDO
       ENDIF
       IF (sensor_id .eq. sensor_id_f16 .or. sensor_id .eq. sensor_id_f17 .or. sensor_id .eq. sensor_id_f18) THEN
           iAngBin = ( (Ym%iscanPos-1) / ScanPos_Factor ) + 1
       ENDIF
       IF (sensor_id .eq. sensor_id_trmm) THEN
          iAngBin = 1
       ENDIF
       IF (iAngBin .le. 0) THEN
          iAngBin=1
          mqc=1
       ENDIf
       !---Determine the surface type and water fraction 
       call Read_topography(Topogr,Ym%lat,Ym%lon,xalt,stype,xcover,stypeCRTM)
       IF (stype .lt. 0 .or. stype .gt. 3) THEN
          stype=0
          mqc=1
          Ym%lat=0.
       ENDIF
       !---Determine now the classifier-driven surface type (used in 1DVAR pre-classification)
       call PreClassAtm(Ym%Year,Ym%julDay,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,stype,sensor_id,Ym%tb,AtmClass,TskPreclass)
       call PreClassSfc(Ym%Year,Ym%julDay,Ym%nchan,Ym%CentrFreq,Ym%lat,Ym%lon,stype,sensor_id,Ym%tb,SfcClass,TskPreclass)
       !---Bias application for use to retrieve CLW
       Call apply_bias(Ym%tb,tb,Ym%iscanPos,Ym%nPosScan,nposBias,Bias,Slope,Intercept,Ym%nchan, &
            TunParams(1)%iWhere2Apply,TunParams(1)%iCorrMethod,SfcClass,errReadingBias,TunParams(1)%applyBias_oc_byChan,&
            TunParams(1)%applyBias_ic_byChan,TunParams(1)%applyBias_ld_byChan, &
            TunParams(1)%applyBias_sn_byChan)
       !---TRMM data trained on uncorrected TBs
       if (sensor_id .eq. sensor_id_trmm) tb=Ym%tb
       !Call apply_bias(Ym%tb,tb,Ym%iscanPos,nposBias,Bias,Slope,Intercept,constBias,regBias,Ym%nchan,  &
       !     TunParams(1)%iWhere2Apply,0,SfcClass,Scene,errReadingBias,TunParams(1)%applyBias_oc_byChan,&
       !     TunParams(1)%applyBias_ic_byChan,TunParams(1)%applyBias_ld_byChan,TunParams(1)%applyBias_sn_byChan)
       !---QC for this profile
       IF (Ym%nqc .gt. 0) THEN
          Scene%qc(1:nqc) = Ym%qc(1)
       ELSE
          Scene%qc(1:nqc) = 0
       ENDIF
       !---Fill-in Scene structure
       call SetUpScene(PresLev(1:nLev),PresLay(1:nLay),angle,Scene,SfcClass,iProf,    &
            Ym%lat,Ym%lon,Ym%node,Ym%julDAY,Ym%Year,Ym%secs,Ym%iscanPos,Ym%iscanLine, &
            Ym%RelAziAngle,Ym%SolZenAngle)
       !----Determine which algo to use from the surface type
       IF(SfcClass .eq. OC_TYP)     IselectsfcTyp=1
       IF(SfcClass .eq. SEAICE_TYP) IselectsfcTyp=2
       IF(SfcClass .eq. LD_TYP)     IselectsfcTyp=3
       IF(SfcClass .eq. SNOW_TYP)   IselectsfcTyp=4
       !---Retrieve Cloud Liquid Water
       CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,nElemts,nIndepVar, &
            TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,1)
       CLW = EDR(1)
       !---Convert CLW from Log to Natural Space
       CLW = exp(CLW)
       IF (CLW .le. 0.05 .or. abs(Scene%lat) .gt. 55) THEN 
          CLW = 0.
          Scene%CLW(1:nLay) = 0
       ELSE
          scalFac=CLW/iCLWBkg(IselectsfcTyp)
          DO iLay=1,nLay
             Scene%CLW(iLay) = CLWBkg(iLay,IselectsfcTyp)*scalFac
             IF ( Scene%CLW(iLay) .lt. 0.00001) Scene%CLW(iLay) = 0.
          ENDDO
       ENDIF
       !----Apply the regression algors to get the EDRs & Set up scene structure
       Scene%Rain                 =0.
       Scene%Snow                 =0.
       Scene%Ice                  =0.
       Scene%Graupel              =0.
       !---UnComment if other EDRs trained on real data
       tb = ym%tb
       !---Tskin
       CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,&
            nElemts,nIndepVar,TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,2)
       Scene%Tskin=EDR(1)
       !---TPW
       CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,&
            nElemts,nIndepVar,TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,3)
       scalFac=maxval((/EDR(1),0.1/))/tpwBkg(IselectsfcTyp)
       !---Emissivity
       CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,&
            nElemts,nIndepVar,TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,4)
       IF (IselectsfcTyp .eq. 1) THEN 
          Scene%Emiss(1:Ym%nchan)       = EDR(1:nElemts(4))
       ELSE
          Emiss2Adjust = EDR(1:nElemts(4))
          CALL AdjustEmiss(Emiss2Adjust,Scene%Emiss,Ym%nchan,Ym%CentrFreq,sensor_id)
       ENDIF
       Scene%Refl(1:Ym%nchan)        = 1.- Scene%Emiss(1:Ym%nchan)
       !---Use WV background and scale it by TPW scaling factor (by default)
       Scene%Absorb_lay(1:nLay,1)=WVapBkg(1:nLay,IselectsfcTyp)*scalFac
       !---Set to dummy values the ozone profile
       Scene%Absorb_lay(1:nLay,2)=0.
       !---Water Vapor 
       CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,&
         nElemts,nIndepVar,TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,5)
       Scene%Absorb_lay(:,1) = -999
       !---Interpolate values for layers retrieved with negative values
       CALL interprofile(EDR(1:nElemts(5)),Scene%Pres_Lay,nLay,Scene%Pres_Lay,PresBot,PresTop,Scene%Absorb_Lay(:,1),qcIntp)
       !---Integrate vertically the Q profile
       CALL ComputeTPW(presLay(1:nLay),1000.,Scene%Absorb_lay(1:nLay,1),tpw) 
       !---Temperature Profile
       CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,&
            nElemts,nIndepVar,TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,6)
       Scene%Temp_lay(1:nLay)       = EDR(1:nElemts(6))
       !---Retrieve Graupel
       IF (sensor_id .eq. sensor_id_trmm) THEN
          CALL ApplyRegr(tb(1:Ym%nchan),cosAngle,Ym%Lat,-999.,Algors,nElemts,nIndepVar, &
               TbIndxArr,iAngBin,FormTBIndxArr,sensor_id,EDR,IselectsfcTyp,7)
          GWP = EDR(1)
          !---Convert GWP from Log to Natural Space
          GWP = exp(GWP)
          IF (GWP .le. 0.04) THEN 
             GWP = 0.
             Scene%Graupel(1:nLay) = 0
          ELSE
             scalFac=GWP/iGWPBkg(IselectsfcTyp)
             DO iLay=1,nLay
                Scene%Graupel(iLay) = GrplBkg(iLay,IselectsfcTyp)*scalFac
                IF ( Scene%Graupel(iLay) .lt. 0.00001) Scene%Graupel(iLay) = 0.
             ENDDO
          ENDIF
       ENDIF
       !---Sanity check of retrieved EDRs
       IF (CLW .gt. 2) THEN
          Scene%CLW(1:nLay) = -999.
          Scene%qc(1)=2
       ENDIF
       IF (GWP .gt. 5) THEN
          Scene%Graupel(1:nLay) = -999.
          Scene%qc(1)=2
       ENDIF
       IF ((ANY(Scene%Temp_lay(1:nLay) .gt. MAXIM_ATM_TEMP)) .or. (ANY(Scene%Temp_lay(1:nLay) .lt. MINIM_ATM_TEMP &
            .and. Scene%Temp_lay(1:nLay) .gt. -999.))) THEN
          Scene%Temp_lay(1:nLay)   = -999.
          Scene%qc(1)=2
       ENDIF
       IF ((ANY(Scene%Absorb_lay(1:nLay,1) .gt. MAXIM_ATM_HUMID)) .or. (ANY(Scene%Absorb_lay(1:nLay,1) .lt. MINIM_ATM_HUMID &
            .and. Scene%Absorb_lay(1:nLay,1) .gt. -999.))) THEN
          Scene%Absorb_lay(1:nLay,1)   = -999.
          Scene%qc(1)=2
       ENDIF
       IF (Scene%Tskin .GT. MAXIM_SKIN_TEMP .or. Scene%Tskin .LT. MINIM_SKIN_TEMP) THEN
          Scene%Tskin=-999.
          Scene%qc(1)=2
       ENDIF
       IF ((ANY(Scene%Emiss(1:Ym%nchan) .gt. 1.)) .or. (ANY(Scene%Emiss(1:Ym%nchan) .lt. 0.))) THEN
          Scene%Emiss(1:Ym%nchan)    = -999.
          Scene%qc(1)=2
       ENDIF
       IF (mqc .eq. 1) Scene%qc(:)=2
       !---Find Surface Pressure and fills layers below surface
       SfcPressLoop: DO ilay=nlay,1,-1
          IF (Scene%Temp_lay(ilay) .gt. 0) THEN
             Scene%SfcPress = Scene%Pres_lev(ilay+1)
             Scene%Temp_lay(ilay+1:nlay) = Scene%Temp_lay(ilay)
             EXIT SfcPressLoop
          ENDIF
       ENDDO SfcPressLoop
       FillQ2SfcLoop: DO ilay=nlay,1,-1
          IF (Scene%Absorb_lay(ilay,1) .gt. 0) THEN
             Scene%Absorb_lay(ilay+1:nlay,1) = Scene%Absorb_lay(ilay,1)
             EXIT FillQ2SfcLoop
          ENDIF
       ENDDO FillQ2SfcLoop
       !---Fill in top layers of profile
       cnt = COUNT(Scene%Temp_Lay(1:70) .lt. 0)
       IF (cnt .gt. 0) Scene%Temp_Lay(1:cnt) = Scene%Temp_Lay(cnt+1)
       cnt = COUNT(Scene%Absorb_Lay(1:70,1) .lt. 0)
       IF (cnt .gt. 0) Scene%Absorb_Lay(1:cnt,1) = Scene%Absorb_Lay(cnt+1,1)
       !---Output results
       call WriteScene(iuOut,Scene)
    END DO ProfLoop
    !---Close Input/output files
    close(iu)
    close(iuOut)
    !---Release meomory allocated in InitHdrScene of src/lib/io/IO_Scene.f90
    CALL DestroyScene(Scene)
    !---Release meomory allocated in ReadHdrMeasurmts of src/lib/io/IO_MeasurData.f90
    DEALLOCATE(Ym%CentrFreq,Ym%Rad,Ym%qc,Ym%Tb,Ym%polar,Ym%angle,Ym%secant_view)
      
  END DO FileLoop

  !---Release memory
  CALL Destroy_GeophStatsT_Atm(GeophStatsT_Atm(1))
  CALL Destroy_GeophStatsT_Sfc(GeophStatsT_Sfc(1))
  DEALLOCATE(RadMeasFiles)
  DEALLOCATE(REGRESS_Files)
  DEALLOCATE(tb,Emiss2Adjust,SaAtm,SaSfc,XbAtm,XbSfc)
  DEALLOCATE(TempBkg,WVapBkg,CLWBkg,iCLWBkg,TskinBkg,TPWBkg,EmissBkg)
  DEALLOCATE(cfreq_bias,Bias,Slope,Intercept)
  DEALLOCATE(TunParams)

  CALL CloseLogFile()


CONTAINS

!===============================================================
! Name:        ApplyRegr
!
!
! Type:        Subroutine
!
!
! Description:  Applies the regression algorithms to the brightness
!               temperatures to obtain a retrieval based on these algos
!
!
! Arguments:
!
!           Name            Type           Description
!      ---------------------------------------------------
!       - tb                 I             Brightness temperatures
!       - angle              I             Angle
!       - lat                I             Latitude
!       - Algors             I             Array of algorihms coeffs
!       - nElts              I             Number of elts representing the EDR
!       - nIndepVar          I             #Indep variables for EDR and algo typ
!       - TbIndxArr          I             Array of TB mappers for EDRs and algos
!       - EDR                O             Result of the application of the algo
!       - sfcTyp             I             Type of algo used (generally sfc type)
!       - iEDR               I             Index of EDR being retrieved here
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

  SUBROUTINE ApplyRegr(tb,angle,lat,clw,Algors,nElts,nIndepVar,TbIndxArr,&
       AngBin,FormTBIndxArr,sensor_id,EDR,sfcTyp,iEDR)
    REAL,    DIMENSION(:)         :: EDR,tb
    INTEGER, DIMENSION(:)         :: nElts
    INTEGER, DIMENSION(:,:)       :: nIndepVar
    INTEGER, DIMENSION(:,:,:)     :: TbIndxArr,FormTBIndxArr
    REAL,    DIMENSION(:,:,:,:,:) :: Algors
    INTEGER                       :: iEDR,sfcTyp,AngBin,sensor_id
    REAL                          :: angle,lat,clw
    INTEGER :: nElemts,nInd,nChan2Use,ivar,iInd
    REAl    :: a0

    nElemts   = nElts(iEDR)
    nInd      = nIndepVar(iEDR,SfcTyp)
    nChan2Use = COUNT(TbIndxArr(iEDR,sfcTyp,:) .gt. 0)
    !---Set number of channels use for CLW retrieval to 3
    IF (iEDR .eq. 1) nChan2Use = 3
    !---Set number of channels for TMI (9) and for GWP and Em
    IF (sensor_id .eq. sensor_id_trmm) nChan2Use=9
    IF (iEDR .eq. 7 .and. sensor_id .eq. sensor_id_trmm .and. sfcTyp .eq. OC_TYP) nChan2Use=5
    IF (iEDR .eq. 7 .and. sensor_id .eq. sensor_id_trmm .and. sfcTyp .eq. LD_TYP) nChan2Use=4
    IF (iEDR .eq. 4 .and. sensor_id .eq. sensor_id_trmm) nChan2Use=4
    DO ivar=1,nElemts 
       a0=Algors(iEDR,sfcTyp,ivar,1,AngBin)
       EDR(ivar)=a0
       IF ( ABS(EDR(ivar) + 99.) .lt. epsilon ) THEN
          EDR(ivar)=-999.
       ELSE
          DO iInd=1,nChan2Use
             IF (FormTBIndxArr(iEDR,sfcTyp,iInd) .eq. 0) THEN
                EDR(ivar)=EDR(ivar)+tb(TbIndxArr(iEDR,sfcTyp,iInd))*Algors(iEDR,sfcTyp,ivar,1+iInd,AngBin)              
             ENDIF
             IF (FormTBIndxArr(iEDR,sfcTyp,iInd) .eq. 1 .and. tb(TbIndxArr(iEDR,sfcTyp,iInd)) .lt. 300.) THEN
                EDR(ivar)=EDR(ivar)+log(300. - tb(TbIndxArr(sfcTyp,iEDR,iInd)))*Algors(iEDR,sfcTyp,ivar,1+iInd,AngBin)
             ENDIF
             IF (FormTBIndxArr(iEDR,sfcTyp,iInd) .eq. 1 .and. tb(TbIndxArr(iEDR,sfcTyp,iInd)) .ge. 300.) THEN
                EDR(ivar)=-999.
             ENDIF
          END DO
          IF (EDR(ivar) .gt. -999.) THEN
             !---IF retrieving CLW
             IF (  ABS(clw + 999.) .lt. epsilon .and. sensor_id .ne. sensor_id_trmm) THEN
                IF (sensor_id .eq. sensor_id_n18 .or. sensor_id .eq. sensor_id_metopA .or. &
                    sensor_id .eq. sensor_id_n19 .or. sensor_id .eq. sensor_id_metopB .or. &
                    sensor_id .eq. sensor_id_npp) &
                     EDR(ivar)=EDR(ivar)+(angle*Algors(iEDR,sfcTyp,ivar,nInd,AngBin))
                EDR(ivar)=EDR(ivar)+lat*Algors(iEDR,sfcTyp,ivar,nInd+1,AngBin)
             ENDIF
             !---IF retrieving all other EDRs
             IF (clw .ge. 0 .and. sensor_id .ne. sensor_id_trmm) THEN
                IF (sensor_id .eq. sensor_id_n18 .or. sensor_id .eq. sensor_id_metopA .or. &
                    sensor_id .eq. sensor_id_n19 .or. sensor_id .eq. sensor_id_metopB .or. &
                    sensor_id .eq. sensor_id_npp) &
                     EDR(ivar)=EDR(ivar)+(angle*Algors(iEDR,sfcTyp,ivar,nInd-1,AngBin))
                EDR(ivar)=EDR(ivar)+(clw*Algors(iEDR,sfcTyp,ivar,nInd,AngBin))
                EDR(ivar)=EDR(ivar)+(lat*Algors(iEDR,sfcTyp,ivar,nInd+1,AngBin))
             ENDIF
          ENDIF
       ENDIF
    END DO
  END SUBROUTINE ApplyRegr

end Program ApplyRegress
