!===============================================================
! Name:  vipp
!
!
! Type:  Main Program
!
!
! Description:
!        Program that performs vertical integration and post-
!        processing to the core products of MIRS, in order to
!        generate the derived EDR products (DEP)
!
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!       - ErrorHandling
!       - IO_Scene
!       - IO_DEP
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================
program vipp
  USE misc
  USE Consts
  USE utils
  USE ErrorHandling
  USE IO_Scene
  USE IO_DEP
  USE IO_Misc
  USE Preclassif
  IMPLICIT NONE
  !---INTRINSIC functions used in this module
  INTRINSIC :: ABS,COS,EXP,LOG,SIN,SIZE,SQRT,MINVAL,TRIM,ATAN2

  !---Different parameters
  INTEGER, PARAMETER :: LEN_FILE=250
  !---Single variables
  INTEGER            :: iu_listedr,nFiles,ifile,nprf,iuEDR,iuDEP,ierr,iprof,iSnow,iIce
  INTEGER            :: landindex=0,landindx=0
  INTEGER            :: surface_type
  REAL               :: tpw
  REAL               :: GTHK,FLVH
  !---Pointers
  CHARACTER(LEN=LEN_FILE), DIMENSION(:), POINTER  :: edrFiles,depFiles
  !---Structures
  TYPE(Scene_type)                           :: Scene
  TYPE(DEP_type)                             :: DEP
  ! Sea Ice variables
  INTEGER                                    :: nprofSeaice, nParamSeaice, nAngleStratification, &
                                                nLatStratification
  REAL,    DIMENSION(:,:),  POINTER          :: SeaiceData
  ! Snow Cover variables
  INTEGER                                    :: nprofSnow,nParamSnow
  REAL,    DIMENSION(:,:),  POINTER          :: SnowData
  INTEGER            :: iu_seaice
  INTEGER            :: iu_Snow
  
  !---Namelist data 
  CHARACTER(LEN=LEN_FILE) :: edrFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LEN_FILE) :: SeaiceFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LEN_FILE) :: SnowCoverFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LEN_FILE) :: pathDEPOut=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LEN_FILE) :: LogFile=DEFAULT_VALUE_STR4
  INTEGER                 :: norbits2process=DEFAULT_VALUE_INT
  INTEGER                 :: nprofs2process=DEFAULT_VALUE_INT
  INTEGER                 :: sensor_id=DEFAULT_VALUE_INT
   
  NAMELIST /VIPPNMLST/edrFilesList,SeaiceFile,SnowCoverFile,pathDEPOut,&
            LogFile,norbits2process,nprofs2process,sensor_id

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=VIPPNMLST)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of radiance data and build output NWP files names
  call ReadList(iu_listedr,trim(edrFilesList),edrFiles,nFiles,depFiles,pathDEPOut,'DEP_')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
  nfiles=minval((/norbits2process,nfiles/))
 
  !---Read off-line sea ice catalog of surface emissivities and sea ice fractions 
  !---Exception for Megha-Tropiques MADRAS and SAPHIR
  if(Sensor_id .ne. sensor_id_mtma .and. Sensor_id .ne. sensor_id_mtsa)then
     iu_Seaice = get_lun()
     open(iu_SeaIce, file = SeaIceFile, status = 'old', form = 'formatted')
     read(iu_SeaIce,*) nprofSeaice, nParamSeaice, nLatStratification,nAngleStratification
     ALLOCATE(SeaiceData(nprofSeaice,nParamSeaice))
     do iIce =1,nprofSeaice
        read(iu_SeaIce,*) SeaiceData(iIce,1:nParamSeaice)
     enddo
     close(iu_SeaIce)
  endif

  !---Read off-line Snow catalog of surface emissivities and Snow Cover properties
  !---Exception for Megha-Tropiques MADRAS and SAPHIR
  if(Sensor_id .ne. sensor_id_mtma .and. Sensor_id .ne. sensor_id_mtsa)then
     iu_snow = get_lun()
     open(iu_Snow, file = SnowCoverFile, status = 'old', form = 'formatted')
     read(iu_Snow,*) nprofSnow,nParamSnow
     ALLOCATE(SnowData(nprofSnow,nParamSnow))
     do iSnow = 1,nprofSnow
        read(iu_Snow,*) SnowData(isnow,1:nParamSnow)
     enddo
     close(iu_Snow)
  endif
  !-----------------------------------------------------
  !     Loop over the files
  !-----------------------------------------------------
  FilesLoop: DO ifile=1,nfiles
      !---Read header of EDR file
      CALL ReadHdrScene(iuEDR,edrFiles(ifile),Scene,nprf)
      write(*,*)
      write(*,*)'ifile=', ifile
      write(*,'(A)')'EDR file='//TRIM(edrFiles(ifile))
      write(*,'(A)')'DEP file='//TRIM(depFiles(ifile))
      
      !---Populate DEP header from Scene items
      DEP%AlgSN      = Scene%AlgSN
      DEP%iTyp       = Scene%iTyp
      DEP%nPosScan   = Scene%nPosScan 
      DEP%nScanLines = Scene%nScanLines
      
      !---Output header of DEP (derived products)
      CALL WriteHdrDEP(iuDEP,depFiles(ifile),DEP,nprf)
      
      !---Loop over the profiles within the file
      ProfilesLoop: DO iprof=1,nPrf
        !---Read content of Scene file
        CALL ReadScene(iuEDR,Scene,ierr)
        IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
        IF (ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. Scene file. (VIPP)')
        !---Populate DEP with repeatable scene information
        DEP%ProfIndx   = Scene%ProfIndx
        DEP%lat        = Scene%lat
        DEP%lon        = Scene%lon
        DEP%Angle      = Scene%Angle
        DEP%RelAziAngle= Scene%RelAziAngle
        DEP%SolZenAngle= Scene%SolZenAngle
        DEP%node       = Scene%node
        DEP%scanDAY    = Scene%scanDAY
        DEP%scanYear   = Scene%scanYear
        DEP%scanUTC    = Scene%scanUTC
        DEP%iscanPos   = Scene%iscanPos
        DEP%iscanLine  = Scene%iscanLine
        DEP%iTypAtm    = Scene%iTypAtm
        DEP%DescTypAtm = Scene%DescTypAtm
        DEP%iTypSfc    = Scene%iTypSfc
        DEP%DescTypSfc = Scene%DescTypSfc
        DEP%nIter      = Scene%nIter
        DEP%ChiSq      = Scene%ChiSq
        !---Populate DEP with vertically integrated products
        CALL ComputeTPW(Scene%Pres_lev(1:Scene%nLev),Scene%SfcPress,&
             Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o),tpw)
        DEP%TPW        = tpw
        DEP%CLW        = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                         Scene%SfcPress,Scene%Clw(1:Scene%nLay))
        DEP%RWP        = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                         Scene%SfcPress,Scene%Rain(1:Scene%nLay))
        !DEP%SWP        = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
        !                 Scene%SfcPress,Scene%Snow(1:Scene%nLay))
        !DEP%IWP        = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
        !                 Scene%SfcPress,Scene%Ice(1:Scene%nLay))
        DEP%SWP        = DEFAULT_VALUE_REAL
        DEP%IWP        = DEFAULT_VALUE_REAL
        DEP%GWP        = ColumIntegr(Scene%nLay,Scene%Pres_lay(1:Scene%nLay),&
                         Scene%SfcPress,Scene%Graupel(1:Scene%nLay))
        !---Implement a climatological sea-ice mask for F16 GWP and RWP prducts
        IF(sensor_id .EQ. SENSOR_ID_F16 .OR. sensor_id .EQ. SENSOR_ID_NPP      &
	     .OR. sensor_id .EQ. SENSOR_ID_F17 .OR. sensor_id .EQ. SENSOR_ID_F18 ) THEN
           IF(DEP%iTypSfc .EQ. SEAICE_TYP .OR. DEP%iTypSfc .EQ. OC_TYP ) THEN
              surface_type=SEAICE_TYP
              landindx=0
              CALL applySeaIceClimo(DEP%ScanYear,DEP%ScanDay,DEP%lat,DEP%lon,landindx,surface_type)
              IF(surface_type .NE. OC_TYP) THEN
                 DEP%RWP = 0.0
                 DEP%GWP = 0.0
              ENDIF
           ENDIF
        ENDIF
        !---Compute the Liquid Water Path (sum of rain and cloud liquid water)
        DEP%LWP        = DEP%CLW + DEP%RWP
        !---Disable cloud Retrievals over certain surfaces.
        IF (Scene%iTypSfc .ne. OC_TYP)                                      DEP%CLW = DEFAULT_VALUE_REAL
        IF (Scene%iTypSfc .ne. OC_TYP)                                      DEP%LWP = DEFAULT_VALUE_REAL
        IF (Scene%iTypSfc .eq. SEAICE_TYP .or. Scene%iTypSfc .eq. SNOW_TYP) DEP%RWP = DEFAULT_VALUE_REAL
        !IF (Scene%iTypSfc .eq. SEAICE_TYP .or. Scene%iTypSfc .eq. SNOW_TYP) DEP%GWP = DEFAULT_VALUE_REAL

        !----Populate with post-processing derived products (hydrometeors)
        DEP%SFR        = DEFAULT_VALUE_REAL
        DEP%CldTop     = DEFAULT_VALUE_REAL
        DEP%CldBase    = DEFAULT_VALUE_REAL
        DEP%CldThick   = DEFAULT_VALUE_REAL
        DEP%PrecipType = DEFAULT_VALUE_REAL
        DEP%RainFlag   = DEFAULT_VALUE_REAL
        !----Populate with post-processing derived products (surface)
        DEP%SIC = DEFAULT_VALUE_REAL
        DEP%SIC_FY = DEFAULT_VALUE_REAL
        DEP%SIC_MY = DEFAULT_VALUE_REAL
        DEP%SWE = DEFAULT_VALUE_REAL
        DEP%SnowGS = DEFAULT_VALUE_REAL
        DEP%SnowCover = DEFAULT_VALUE_REAL
        !---Exception for Megha-Tropiques MADRAS and SAPHIR - no sea ice or snow retrieved
        if(Sensor_id .ne. sensor_id_mtma .and. Sensor_id .ne. sensor_id_mtsa)then
           CALL ComputeSIC(sensor_id,Scene%Emiss(1:Scene%Nchan),&
                Scene%nchan,Scene%Tskin,DEP%Angle,DEP%lat,Scene%iTypSfc,&
                Scene%qc,Seaicedata,nprofSeaice,nparamSeaice, &
                nLatStratification,nAngleStratification,DEP%SIC,DEP%SIC_FY,DEP%SIC_MY)
           !----Apply sea ice climatology to reset scenes unlikely to be sea ice, back to ocean
           if(DEP%SIC .gt. 0.)then
              surface_type=SEAICE_TYP
              call applySeaIceClimo(DEP%ScanYear, DEP%ScanDay, DEP%lat, DEP%lon, landindex, surface_type)
              if(surface_type .eq. OC_TYP)then
                 DEP%SIC=0.
                 DEP%SIC_FY=0.
                 DEP%SIC_MY=0.
              endif
           endif

           CALL ComputeSWEandGS(sensor_id,Scene%Emiss(1:Scene%nchan),Scene%nchan,Scene%Tskin,DEP%lat,DEP%lon, &
                Scene%iTypSfc,Scene%qc,SnowData,nprofsnow,nparamsnow,DEP%SnowGS,DEP%SWE)

           DEP%iTypSfc    = ComputeSfcTyp(DEP%SIC, DEP%SWE)
           CALL DetectSnowCover(DEP%SWE,DEP%SnowCover)
           if( sensor_id .eq. sensor_id_fy3ri .or. &
               sensor_id .eq. sensor_id_trmm  .or. & 
               sensor_id .eq. sensor_id_gpm ) DEP%iTypSfc = Scene%iTypSfc
           ! special case for amsre since swe still not retrieved (ComputeSfcTyp erroneously switches land type to ocean type)
           if( ( sensor_id .eq. sensor_id_amsre .or. sensor_id .eq. sensor_id_gcomw1 ) .and. &
	       ( Scene%iTypSfc .eq. LD_TYP .or. Scene%iTypSfc .eq. SNOW_TYP ) ) then
              DEP%iTypSfc = Scene%iTypSfc
              if(Scene%iTypSfc .eq. SNOW_TYP) DEP%SnowCover=1
           endif

           ! Flag SWE and Grain Size over snow-covered areas in Greenland and Antarctica 
           IF( (DEP%lat .le. -62.) .or. (DEP%lat .ge. 59.6 .and. DEP%lat .le. 83.4 .and. DEP%lon .ge. -71.8 &
                .and. DEP%lon .le. -14.1) ) THEN
              DEP%SWE =DEFAULT_VALUE_REAL
              DEP%SnowGS = DEFAULT_VALUE_REAL
           Endif
        endif

        CALL ComputeGEOTHK(Scene%nLay,Scene%pres_lay,Scene%Temp_lay,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o), &
             Scene%SfcPress,Scene%qc,GTHK)
        !---Computation of freezing level heigh
        CALL ComputeFLVH(Scene%nLay,Scene%pres_lay,Scene%Temp_lay,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o), &
             Scene%SfcPress,Scene%qc,FLVH)
        !---Computation of Rain Rate from MIRS core products and parameters
        CALL RegressRR(sensor_id,DEP%ScanYear,DEP%ScanDay,DEP%lon,Scene%Lat,DEP%iTypSfc,DEP%LWP,DEP%GWP, &
             DEP%RWP,DEP%CLW,DEP%TPW,Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o),Scene%Temp_lay,GTHK,FLVH, &
             Scene%Tskin,Scene%qc,DEP%RR)
        !---Computation of Rain Rate based on MSPPS rain rate algorithm
!        CALL ComputeRR(Scene%Lat,Scene%Lon,DEP%iTypSfc,Scene%Tskin,DEP%LWP,tpw,DEP%GWP,Scene%Emiss(1:Scene%nchan), &
!             Scene%Ym,Scene%YmCorr,Scene%Angle,DEP%SIC,Scene%node,DEP%RR)
        DEP%SM         = DEFAULT_VALUE_REAL
        DEP%WindSp     = DEFAULT_VALUE_REAL
        DEP%WindDir    = DEFAULT_VALUE_REAL
        DEP%WindU      = DEFAULT_VALUE_REAL
        DEP%WindV      = DEFAULT_VALUE_REAL

        !----Ocean Surface Wind Speed 
        IF((sensor_id .EQ. sensor_id_f16 .or. sensor_id .EQ. sensor_id_f18 .or. &
             sensor_id .EQ. sensor_id_f17 .or. &
             sensor_id .EQ. sensor_id_trmm .or. &
             sensor_id .EQ. sensor_id_mtma) .and. &
             (DEP%iTypSfc .eq. OC_TYP .and. all(Scene%Ym .gt. 0.) .and. all(Scene%Emiss .gt. 0.)))then
           call regressWspd(sensor_id,DEP%iTypSfc,DEP%RR,Scene%Ym,Scene%Emiss,DEP%Windsp)
           ! No wspd retrievals at low-res scan positions 28-30 for F16 and F18 due to large bias in 37V uncorrected TBs
           if(sensor_id .EQ. sensor_id_f16 .or. sensor_id .EQ. sensor_id_f18  &
	        .or. sensor_id .EQ. sensor_id_f17)then
              if(DEP%nPosScan .eq. 30 .and. DEP%iscanPos .ge. 28)DEP%WindSp = DEFAULT_VALUE_REAL
              if(DEP%nPosScan .eq. 180 .and. DEP%iscanPos .ge. 165)DEP%WindSp = DEFAULT_VALUE_REAL
           endif
        endif

        !---Populate of DEP with QC elements
        IF (Scene%nqc .ne. 4) CALL ErrHandl(ErrorType,Err_Incompat,'. Nqc (scene and dep). (VIPP)')
        DEP%qc(1:4)    = Scene%qc(1:4)
        !---Output DEP
        CALL WriteDEP(iuDEP,DEP)
      ENDDO ProfilesLoop
      !---Close the EDR & DEP files
      CLOSE (iuEDR)
      CLOSE (iuDEP)
      !---Release memory
      CALL DestroyScene(Scene)
  ENDDO FilesLoop
  
  !--- release memory allocated
  if(Sensor_id .ne. sensor_id_mtma .and. Sensor_id .ne. sensor_id_mtsa)then
     DEALLOCATE(SeaiceData)
     DEALLOCATE(SnowData)  
  endif
  DEALLOCATE(edrFiles)
  DEALLOCATE(depFiles)
  
CONTAINS


!===============================================================
! Name:         RegressRR
!
!
! Type:         Subroutine
!
!
! Description:  Computes the rain rate (mm/hr) from MIRS
!               products and parameters. 
!              
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Lat                I              Latitude 
!       - SfcTyp             I              Surface Type
!       - LWPI               I              Liquid Water Path (mm)
!       - GWPI               I              Groupel Water Path (mm)
!       - RWPI               I              Rain Water Path (mm)
!       - CLWI               I              Integrated Cloud Liquid Water (mm)
!       - TPWI               I              Total Precipitable Water Vapor (mm)
!       - Q_lay              I              Layer water vapor profile
!       - Temp_lay           I              Layer temperature profile
!       - GTHKI              I              Geopotential Thickness
!       - FLVHI              I              Freezing Level Height
!       - TSKNI              I              Skin Temperature
!       - QC                 I              QC vector
!       - RR                 O              Rain Rate (mm/hr)
!
! Modules needed:
!       - None
!
!
! History:
!       10-03-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
 SUBROUTINE RegressRR(SatID,Year,JDay,Lon,Lat,SfcTyp,LWPI,GWPI,RWPI,CLWI,TPWI,Q_lay, &
      Temp_lay,GTHKI,FLVHI,TSKNI,QC,RR)
   !---Input/Output variables  
   INTEGER, INTENT(IN)                  :: SfcTyp
   INTEGER, INTENT(IN)                  :: Year,JDay,SatID
   INTEGER                              :: landindex,sfc_type
   INTEGER(2), DIMENSION(:), INTENT(IN) :: QC 
   REAL, DIMENSION(:), INTENT(IN)       :: Q_lay,Temp_lay 
   REAL, INTENT(IN)                     :: Lon,Lat,LWPI,RWPI,GWPI,CLWI,TPWI
   REAL, INTENT(IN)                     :: GTHKI,FLVHI,TSKNI
   REAL                                 :: RR
   !---Local variables
   REAL                                 :: LWP,RWP,GWP,CLW,TPW,Q950,T950
   REAL                                 :: GTHK,FLVH,TSKN
   !---TMI variables
   REAL                                 :: RRTMI,RRMM5,RRMSPPS
   !----Regression coefficients over Land:GWP and RWP
   REAL, PARAMETER      :: A0_LD=-0.02645,AGWP_LD=12.03619,ARWP_LD=9.74240
   !----Regression coeficcients over Ocean:GWP,RWP,TPW,Q950,T950,GTHK,FLVH,TSKN and CLW
   REAL, PARAMETER      :: A0_OC=14.25770,AGWP_OC=6.59885,ARWP_OC=1.41296,ATPW_OC=-0.04646
   REAL, PARAMETER      :: AQ950_OC=0.21514,AT950_OC=0.10524,AGTHK_OC=-8.61667,AFLVH_OC=0.20229
   REAL, PARAMETER      :: ATSKN_OC=0.01934,ACLW_OC=0.16851
   !-Variable Definition
   RR=0.0
   LWP=LWPI
   GWP=GWPI
   RWP=RWPI
   CLW=CLWI
   TPW=TPWI
   Q950=Q_lay(96)
   T950=Temp_lay(96)
   GTHK=GTHKI
   FLVH=FLVHI
   TSKN=TSKNI
   !--Rainfall Rate Trained on the MSPPS RR.
   IF(SfcTyp .EQ. LD_TYP) THEN
      CALL MSPPSRR(SfcTyp,QC,CLW,GWP,RWP,TPW,Q950,T950,GTHK,FLVH,TSKN,RRMSPPS)
      RR=RRMSPPS
   ENDIF
   !--Rainfall Rate Trained on the MM5 Mesoscale Model RR.
   IF(SfcTyp .EQ. OC_TYP) THEN
      CALL MM5RR(SfcTyp,QC,CLW,GWP,RWP,RRMM5)
      RR=RRMM5
   ENDIF
   !--Rainfall Rate Trained on the MM5 Mesoscale Model RR. (AMSR2)
   IF(Sensor_id .EQ. sensor_id_gcomw1) THEN
      CALL MM5RR(SfcTyp,QC,CLW,GWP,RWP,RRMM5)
      RR=RRMM5
   ENDIF
   !--Post-processing of the Rainfall Rate.
   IF(RR .LT. 0) RR = 0  
   !--Apply Sea-ice Climatology Mask
      IF(SfcTyp .EQ. SEAICE_TYP .OR. SfcTyp .EQ. OC_TYP ) THEN
         sfc_type=SEAICE_TYP
         landindex=0
         CALL applySeaIceClimo(Year,JDay,Lat,Lon,landindex,sfc_type)
         IF(sfc_type .NE. OC_TYP) THEN
            RR = 0.0
         ENDIF
      ENDIF
   IF (SfcTyp .EQ. LD_TYP .AND. Lat .LT. -60) RR = DEFAULT_VALUE_REAL
   !--Set non-valid values over Snow-covered Land and Sea-ice surfaces
   IF (SfcTyp .EQ. SNOW_TYP) RR = DEFAULT_VALUE_REAL
   IF (SfcTyp .EQ. SEAICE_TYP) RR = DEFAULT_VALUE_REAL

 END SUBROUTINE RegressRR


!===============================================================
! Name:         ComputeRR
!
!
! Type:         Subroutine
!
!
! Description:  Calculates rain rate (mm/hr) based on the mspps
!               rain rate algorithm. 
!              
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Lat                I              Latitude 
!       - Lon                I              Longitude   
!       - SfcTyp             I              Surface Type
!       - ts                 I              Surface Temperature
!       - CLWP               I              Integrated Cloud Liquid Water (mm)
!       - TPW                I              Total Precipitable Water Vapor (mm)
!       - EMISS              I              Array of Emmissivities
!       - Ym                 I              Measured Brightness Temperatures (K)
!       - YmCorr             I              Corrected Brigthness Temperatures (K)
!       - Angle              I              Angle (degrees)
!       - WindSp             I              Wind speed (m/s)
!       - Sice               I              Sea ice concentration (percentage)
!       - node               I              node: 0-Ascending, 1-Descending 
!       - RR                 O              Rain Rate (mm/hr)
!
! Modules needed:
!       - None
!
!
! History:
!       07-25-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE ComputeRR(Lat,Lon,SfcTyp,tskn,LWPI,TPWI,GWP,EMISS,Ym,YmCorr,Angle,Sice,node,RR)
    !---Input/Output variables  
    INTEGER, INTENT(IN)  :: SfcTyp,node
    REAL, INTENT(IN)     :: Lat,Lon,tskn,LWPI,TPWI,GWP,Angle,Sice
    REAl, DIMENSION(:)   :: EMISS,Ym,YmCorr
    REAL                 :: RR
    !---RR variables
    INTEGER              :: ind
    INTEGER              :: clr,conv_index,iwp_flag,rr_flag
    INTEGER, PARAMETER   :: T_CLOUD_LAYER=2,ICE_DEN=920,IWP_SCAL=100,RR_SCAL=10
    INTEGER, PARAMETER   :: SSMIArea=182,RR_MAX=400
    INTEGER, PARAMETER   :: INDETERM_SNOW=-7,INDETERM_SICE=-8
    INTEGER, PARAMETER   :: INDETERM_FROZEN=-13,INDETERM=-10
    INTEGER, PARAMETER   :: INDETERM_DESERT=-11,INDETERM_ELEV=-12
    INTEGER, PARAMETER   :: INDETERM_COAST=-9,MISSING=-99
    REAL, PARAMETER      :: PI=3.141593
    REAL, PARAMETER      :: A0a=3.37,A1a=0.595,A2a=1.582
    REAL, PARAMETER      :: A0b=-1.319,A1b=-1.555,A2b=-1.55
    REAL, PARAMETER      :: al_89=1.03486,bl_89=-0.0097151,cl_89=-0.000065914
    REAL, PARAMETER      :: al_150=1.7129,bl_150=0.0051329,cl_150=-0.00022475
    REAL, PARAMETER      :: a0_89=0.108333,b0_89=-0.000221042
    REAL, PARAMETER      :: a0_150=0.030698,b0_150=-0.000071433
    REAL, PARAMETER      :: kv_89=0.0115839,kv_150=0.029519
    REAL, PARAMETER      :: fix=0.5
    REAL, DIMENSION(5,6) :: coe_rr
    REAL, DIMENSION(4)   :: b0,b1
    REAL                 :: A0,A1,A2,a53L,acoslza
    REAL                 :: kl_89,kl_150
    REAL                 :: tu0_89,tu0_150
    REAL                 :: tul_89,tul_150
    REAL                 :: tuv_89,tuv_150
    REAL                 :: em89,em150
    REAL                 :: trans_89,trans_150
    REAL                 :: pred89,pred150
    REAL                 :: omega89,omega150,omega,omegan
    REAL                 :: lza,lza0,lza1
    REAL                 :: coef_a,coef_b,coef_c,coef_d
    REAL                 :: coef_iwp_a,coef_iwp_b,coef_iwp_c
    REAL                 :: theta,mu,ratio
    REAL                 :: TB23,TB31,TB52,TB53,TB89,TB150,TB176,TB180,TB182
    REAL                 :: TB89_0,TB89_1,TB150_0,TB150_1
    REAL                 :: dta,dta1,dta2,dta3
    REAL                 :: coef0,coef1,coef2
    REAL                 :: ltd,rr_temp,sigma,suma,alfa,AMSUArea
    REAL                 :: slope,exponente,factor,ordinate,ci
    REAL                 :: de,bde,brr
    REAL                 :: iwp,biwp
    REAL                 :: CLWP,TPW
    !---Emissivity subroutine variables
    INTEGER              :: LwpFg,EMFg,TsFg,GWPFg
    REAL                 :: ts
    REAL                 :: sinthetas,costhetas
    REAL, DIMENSION(2)   :: em89hv,em150hv
    REAL, PARAMETER      :: earthrad=6371
    REAL, PARAMETER      :: satheight=833
    REAL, PARAMETER      :: salinity=35.5
    !---tpwclw subroutine variables
    INTEGER              :: nchan
    REAL                 :: tpwm,clwm
    REAL, PARAMETER      :: WindSp=10.0
    !---Variable initialization
    LwpFg=1
    TsFg=0
    EMFg=1
    GWPFg=0
    de=0.0
    bde=0.0
    biwp=0.0
    RR=0.0
    brr=0.0
    pred150=0.0
    pred89=0.0 
    em89=EMISS(16)
    em150=EMISS(17)
    TB23=Ym(1)
    TB31=Ym(2)
    TB52=Ym(4)
    TB53=Ym(5) 
    TB89=Ym(16)
    TB150=Ym(17)
    TB176=Ym(20)   
    TB180=Ym(19)
    TB182=Ym(18)
    lza=Angle
    theta=Angle*PI/180.
    mu = cos(theta)
    nchan=size(Ym)
    CLWP=LWPI
    TPW=TPWI
    ts=tskn
    !---Flags for the use of constant surface temperatures
    if (TsFg .eq. 1) then
       if ((Lat .le. 40) .or. (Lat .ge. -40)) ts=300
       if ((Lat .gt. 40) .or. (Lat .lt. -40)) ts=270
    endif  
    !---Computing tpw and clw based on MSPPS algorithm
    call tpwclw(node,SfcTyp,Lat,Ym,Sice,WindSp,ts,theta,mu,nchan,EMISS,tpwm,clwm)
    if (SfcTyp .eq. OC_TYP .and. LwpFg .eq. 1) then 
       TPW=tpwm
       CLWP=clwm
    endif
    !---Calculate cloud base brightness temperature
    if (SfcTyp .eq. OC_TYP) then 
       sinthetas = sin(theta)* earthrad/(earthrad + satheight)
       sinthetas = (sinthetas**2)
       costhetas = 1.0 - sinthetas
       if( EMFg .eq. 1) then
          !---Computing emmisivities based on MSPPS algorithm
          call emissivity_water(WindSp,theta,ts,salinity,89.e9,em89hv)
          em89 = costhetas*em89hv(2) + sinthetas*em89hv(1)
          call emissivity_water(WindSp,theta,ts,salinity,150.e9,em150hv)
          em150 = costhetas*em150hv(2) + sinthetas*em150hv(1)
       ENDIF
       kl_89   = al_89 + bl_89*T_CLOUD_LAYER + cl_89*(T_CLOUD_LAYER**2)
       kl_150  = al_150 + bl_150*T_CLOUD_LAYER + cl_150*(T_CLOUD_LAYER**2)
       tu0_89  = a0_89 + b0_89*ts
       tu0_150 = a0_150 + b0_150*ts
       IF(CLWP < 0.0) CLWP = 0. 
       tul_89  = kl_89*CLWP
       tul_150 = kl_150*CLWP
       IF(TPW < 0.0) TPW = 0.
       tuv_89 = kv_89*TPW
       tuv_150 = kv_150*TPW
       trans_89 = exp(-(tu0_89 + tuv_89 + tul_89)/mu)
       pred89 = ts * (1 - (1 - em89) * (trans_89**2))
       trans_150 = exp(-(tu0_150 + tuv_150 + tul_150)/mu)
       pred150 = ts * (1 - (1 - em150) * (trans_150**2))
    ENDIF
    IF (SfcTyp .EQ. LD_TYP .OR. SfcTyp .EQ. COAST_TYP) THEN
       pred89 = 17.88 + 1.61* TB23 - 0.67 * TB31
       pred150 = 33.78 + 1.69* TB23 - 0.80* TB31 
    ENDIF
    !---Correcting TB89 and TB150 for their dependency on local zenith angle
    IF(ABS(Angle) .GT. 0. .AND. ABS(Angle) .LT. 10.) THEN
       lza0=0.0
       lza1=10.0
       TB89_0=183.073-0.649864*TB89+0.00355388*TB89*TB89
       TB150_0=89.4492+0.133525*TB150+0.00193974*TB150*TB150
       TB89_1=168.617-0.526129*TB89+0.00329590*TB89*TB89
       TB150_1=85.7358+0.169666*TB150+0.00185847*TB150*TB150
       TB89=(TB89_1-TB89_0)*(abs(lza)-lza0)/(lza1-lza0)+TB89_0
       TB150=(TB150_1-TB150_0)*(abs(lza)-lza0)/(lza1-lza0)+TB150_0
    ENDIF
    IF(ABS(Angle) .GE. 10. .AND. ABS(Angle) .LT. 20.) THEN
       lza0=10.0
       lza1=20.0
       TB89_0=168.617-0.526129*TB89+0.00329590*TB89*TB89
       TB150_0=85.7358+0.169666*TB150+0.00185847*TB150*TB150
       TB89_1=135.886-0.239320*TB89+0.00268872*TB89*TB89
       TB150_1=72.1034+0.300571*TB150+0.00156526*TB150*TB150
       TB89=(TB89_1-TB89_0)*(abs(lza)-lza0)/(lza1-lza0)+TB89_0
       TB150=(TB150_1-TB150_0)*(abs(lza)-lza0)/(lza1-lza0)+TB150_0
    ENDIF
    IF(ABS(Angle) .GE. 20. .AND. ABS(Angle) .LT. 30.0) THEN
       lza0=20.0
       lza1=30.0
       TB89_0=135.886-0.239320*TB89+0.00268872*TB89*TB89
       TB150_0=72.1034+0.300571*TB150+0.00156526*TB150*TB150
       TB89_1=99.8433+0.0911668*TB89+0.00196905*TB89*TB89
       TB150_1=51.6176+0.501623*TB150+0.00110930*TB150*TB150
       TB89=(TB89_1-TB89_0)*(abs(lza)-lza0)/(lza1-lza0)+TB89_0
       TB150=(TB150_1-TB150_0)*(abs(lza)-lza0)/(lza1-lza0)+TB150_0
    ENDIF
    IF(ABS(Angle) .GE. 30. .AND. ABS(Angle) .LT. 40.) THEN
       lza0=30.0
       lza1=40.0
       TB89_0=99.8433+0.0911668*TB89+0.00196905*TB89*TB89
       TB150_0=51.6176+0.501623*TB150+0.00110930*TB150*TB150
       TB89_1=52.4938+0.535288*TB89+0.000986296*TB89*TB89
       TB150_1=26.8442+0.753185*TB150+0.000528123*TB150*TB150
       TB89=(TB89_1-TB89_0)*(abs(lza)-lza0)/(lza1-lza0)+TB89_0
       TB150=(TB150_1-TB150_0)*(abs(lza)-lza0)/(lza1-lza0)+TB150_0
    ENDIF
    IF(ABS(Angle) .GE. 40. .AND. ABS(Angle) .LT. 50.) THEN 
       lza0=40.0
       lza1=50.0
       TB89_0=52.4938+0.535288*TB89+0.000986296*TB89*TB89
       TB150_0=26.8442+0.753185*TB150+0.000528123*TB150*TB150
       TB89_1=7.92203+0.981133*TB89-0.0000394*TB89*TB89
       TB150_1=-2.74337+1.06524*TB150-0.000209793*TB150*TB150
       TB89=(TB89_1-TB89_0)*(abs(lza)-lza0)/(lza1-lza0)+TB89_0
       TB150=(TB150_1-TB150_0)*(abs(lza)-lza0)/(lza1-lza0)+TB150_0
    ENDIF
    IF(ABS(Angle) .GE. 50) THEN 
       TB89=7.92203+0.981133*TB89-0.0000394*TB89*TB89
       TB150=-2.74337+1.06524*TB150-0.000209793*TB150*TB150
    ENDIF  
    omega89  = (pred89 - TB89)/TB89
    omega150 = (pred150 - TB150)/TB150
    ratio = omega89/omega150
    !---Conditions for the existance of detectable clouds 
    IF((omega89 .GT. 0.0) .AND. (omega150 .GT. 0.0) .AND. (TB176 .LT. 265.0)) THEN
       IF((ratio .GT. 0.0) .AND. (ratio .LE. 0.8)) THEN
          !---Calculate the ice particle effective diameter
           coef_a = -0.300323
           coef_b = 4.30881
           coef_c = -3.98255
           coef_d = 2.78323
           bde = coef_a + coef_b*ratio + coef_c*(ratio**2) + coef_d*(ratio**3)
           !---Calculate the ice water path 
           omega=omega89
           coef_iwp_a = -1.19301
           coef_iwp_b = 2.08831
           coef_iwp_c = -0.857469
           IF(bde .LE. 1.0) THEN
               omega=omega150
               coef_iwp_a = -0.294459
               coef_iwp_b = 1.38838
               coef_iwp_c = -0.753624
            ENDIF
           IF (bde .GT. 0.0) THEN
               omegan = exp(coef_iwp_a + coef_iwp_b*LOG(bde) + coef_iwp_c*((LOG(bde))**2))
               IF(omegan .GT. 0.0) biwp = (omega * bde * 1.e-3 * mu * ICE_DEN/omegan)
           ENDIF
        ENDIF
    ENDIF
    !---Calculation of a53L is needed to check for rain by new algorithm   
    acoslza = cos(Angle*PI/180.) 
    IF((TB52 .GT. 0) .AND. (TB53 .GT. 0)) THEN
       A0 = A0a + A0b * acoslza
       A1 = A1a + A1b * acoslza
       A2 = A2a + A2b * acoslza
       a53L = A0 - A1*TB53 + A2*TB52+ 4*(1 - acoslza)
    ENDIF
    !---The new coastal extension to the existing algorithm starts here 
    !---Apply the new algorithm over coast AMSU-A FOV or over coast AMSU-B FOV 
    !-- and for iwp or de values less than necessary conditions for the existance of rain 
    IF(((biwp .LT. 0.05) .OR. (bde .LT. 0.3) .OR. (CLWP .LE. 0.2)) .AND. (SfcTyp .EQ. COAST_TYP)) THEN
       IF(((a53L-TB176) .GT. -6.0) .AND. ((a53L-TB180) .GT. 0.0) .AND. ((TB89-TB150) .GT. 3.0)) THEN
          biwp = 0.01*(292.49289 - 1.83559*TB176 + 0.002554378*TB176*TB176 + 23.45537*mu)
          !---Remove confusion due to cold snow cover or ice effects
          IF((a53L .LT. 244.5) .AND. ((a53L - TB182) .LT. 15.0)) biwp = MISSING
          !---Remove confusion of light or warm rain with clouds
          IF((TB176 .GT. 260.0) .OR. (TB180 .GT. 256.0)) biwp = 0.0
          IF (biwp .GT. 0.0) iwp_flag = 1
       ENDIF
    ENDIF
    IF(biwp .GT. 3.0)  biwp = 3.0
    IF(biwp .LT. 0.0)  biwp = 0.0
    IF(bde .GT. 3.5)   bde = 3.5 
    IF(bde .LT. 0.0)   bde = 0.0
      iwp = biwp*IWP_SCAL + 0.5
      de = bde*IWP_SCAL + 0.5
    !---No retrieval over surface covered with snow
    IF(SfcTyp  .EQ. SNOW_TYP) THEN
       iwp = INDETERM_SNOW
       de  = INDETERM_SNOW
    ENDIF
    !---No retrieval over sea ice 
    IF(SfcTyp .EQ. SEAICE_TYP) THEN 
       iwp = INDETERM_SICE
       de =  INDETERM_SICE
    ENDIF
    !---High latitude frozen surface undetected by snow and sea ice
    IF(ts .LT. 269.0) THEN
       iwp = INDETERM_FROZEN
       de = INDETERM_FROZEN
    ENDIF
    IF((SfcTyp .EQ. LD_TYP) .AND. (TB89 .LT. TB150) .AND. (bde .GT. 0)) THEN
       iwp = INDETERM
       de  = INDETERM
    ENDIF
    IF((SfcTyp .EQ. OC_TYP) .AND. (TB89 .LT. TB150) .AND. (bde .GT. 0)) THEN
       iwp = INDETERM
       de = INDETERM
    ENDIF
    !---Possibly over desert
    IF(((ts - TB176) .LT. 10.0) .AND. (SfcTyp .GE. LD_TYP) .AND. iwp  .GT. 0.0) THEN
       iwp = INDETERM_DESERT
       de = INDETERM_DESERT
    ENDIF
    !Special check over the Himalayas Mountains
    IF(((TB89-TB150) .LT. 0) .AND. (Lat .GT. 22.) .AND. (Lat .LT. 47.) &
       .AND. (Lon .GT. 65.) .AND.  (Lon .LT. 112) .AND. (iwp  .GT. 0.0)) THEN
       iwp = INDETERM_ELEV
       de = INDETERM_ELEV
    ENDIF
    !---Calculate the convective index
    dta = TB89 - TB150
    dta1= TB182 - TB176
    dta2= TB180 - TB176
    dta3= TB182 - TB180
    clr=0
    IF((dta .GT. 0) .AND. (dta2 .GT. 0) .AND. (dta2 .GT. dta1) .AND. (dta2 .GT. dta3)) clr=1
    IF((dta .GT. 0) .AND. (dta2 .GT. 0) .AND. (dta1 .GT. 0) .AND. (dta3 .GT. 0)  &
         .AND. (dta1 .GT. dta2) .AND. (dta1 .GT. dta3) .AND. (dta2 .GT. dta3)) clr=2
    IF((dta .GT. 0) .AND. (dta2 .GT. 0) .AND. (dta1 .GT. 0) .AND. (dta3 .GT. 0) &
         .AND. (dta1 .GT. dta2) .AND. (dta1 .GT. dta3) .AND. (dta2 .LE. dta3)) clr=3
    conv_index = clr
    !---Calculate rain rate
    coef0 = 0.321717
    coef1 = 16.5043
    coef2 = -3.3419

    !---Flag for the use of MIRS GWP
    IF (GWPFg .EQ. 1) THEN
       iwp=GWP*IWP_SCAL
    ENDIF

    IF(iwp .LE. 0) brr=iwp
    IF(iwp .GT. 0.0) THEN 
       IF(conv_index .GE. 3) THEN
          coef0 = 0.08925
          coef1 = 20.8194
          coef2 = -2.9117
       ENDIF
       biwp=iwp/IWP_SCAL
       bde=de/IWP_SCAL
       rr_flag = iwp_flag 
       !---Over ocean
       IF(SfcTyp .EQ. OC_TYP) THEN 
          IF((CLWP .GE. 0.2) .AND. (biwp .GE. 0.05) .AND. (bde .GE. 0.3)) THEN 
             brr = coef0 + coef1*biwp + coef2*(biwp**2)
          ENDIF
       ENDIF
       !---Over LAND or COAST
       IF((SfcTyp .EQ. LD_TYP) .OR. (SfcTyp .EQ. COAST_TYP)) THEN
          IF((biwp .GE. 0.05) .AND. (bde .GE. 0.3) .AND. ((TB89 - TB150) .GT. 3.0)) THEN
             brr = coef0 + coef1*biwp + coef2*(biwp**2)
          ENDIF
       ENDIF
       !---New coastal algorithm starts here: coast FOV (AMSU-A and -B and when RR from 
       !---current algorithm is zero
       IF((brr .LE. 0.0) .AND. (SfcTyp .EQ. COAST_TYP)) THEN
          IF ((rr_flag .EQ. 1) .AND. (biwp .GT. 0.05)) THEN 
             brr = coef0 + coef1*biwp + coef2*(biwp**2)
          ENDIF
       ENDIF
       !---Remove the noises near coast at high latitue
       IF((SfcTyp .EQ. COAST_TYP) .AND. (ABS(Lat) .GT. 50)) brr = INDETERM_COAST
    ENDIF
    IF(brr .GT. 0) RR=brr*RR_SCAL+0.5
    IF(brr .LE. 0) RR=brr
    !---Rain rate correction 
    coe_rr(1,1:6) = (/8.4352514756117, -0.060296877987446, -0.00054936395647573, -0.033675721411174, &
         0.00083674824564975, -0.00086265338741617/)
    coe_rr(2,1:6) = (/3.6702179933091, -0.017302697728341, -0.00026283313777525, -0.014163338358315, &
         0.00035417305674957, -0.00052079582170665/)
    coe_rr(3,1:6) = (/4.1652486678776, -0.037312486505719, -0.00015803016559758, -0.012934749819459, &
         0.00051213974089996, -0.00037569662434794/)
    coe_rr(4,1:6) = (/3.2898081570968, -0.015392931621478, -0.00036783945503082,  0.000891466966662, &
         0.00028204892832443, -0.00047162080113714/)
    coe_rr(5,1:6) = (/278.867, -3.69141000, 0.58227500, -0.0175858, 0.000240862, 0.00000000000000000/)
    !---LZA normalization process (see AGU, 2006)
    IF(RR .GT. 0) THEN
      IF(SfcTyp .EQ. OC_TYP) ind = 1
      IF(SfcTyp .GE. LD_TYP) ind = 3
      ltd = ABS(Lat)
      lza1 = ABS(Angle)
      rr_temp = RR/RR_SCAL
      mu=coe_rr(ind,1)+coe_rr(ind,2)*lza1+coe_rr(ind,3)*lza1*lza1+ &
         coe_rr(ind,4)*ltd+coe_rr(ind,5)*ltd*lza1+coe_rr(ind,6)*ltd*ltd
      sigma=coe_rr(ind+1,1)+coe_rr(ind+1,2)*lza1+coe_rr(ind+1,3)*lza1*lza1+ &
            coe_rr(ind+1,4)*ltd+coe_rr(ind+1,5)*ltd*lza1+coe_rr(ind+1,6)*ltd*ltd
      suma=mu+2*sigma
      alfa=0.3+lza1*0.0051
      AMSUArea=coe_rr(5,5)*(lza1**4)+coe_rr(5,4)*(lza1**3) & 
              +coe_rr(5,3)*(lza1**2)+coe_rr(5,2)*lza1+coe_rr(5,1)
      IF((SfcTyp .EQ. LD_TYP) .OR. (SfcTyp .EQ. COAST_TYP)) THEN
        slope=1.3*SQRT(AMSUArea/SSMIArea)
      ENDIF
      IF(SfcTyp .EQ. OC_TYP) slope=1.5*SQRT(AMSUArea/SSMIArea)
      exponente = -0.5*(((suma-mu)/sigma)**2)
      factor = 1 - (1-alfa)*EXP(exponente)
      ordinate = factor*suma - slope*suma
      IF(rr_temp .LT. suma) THEN
        exponente = -0.5*(((rr_temp - mu)/sigma)**2)
        IF(rr_temp .LT. mu) exponente=-0.5*(((rr_temp - mu)/(sigma*2))**2)
         factor= 1 - (1 - alfa)*EXP(exponente)
         RR=factor*rr_temp*RR_SCAL+fix
      ENDIF
      IF (rr_temp .GE. suma) RR=(slope*rr_temp + ordinate)*RR_SCAL + fix
  ENDIF
  !---Perform convective index (ci) and fill up the gaps (indeterm = -10)
  IF( ABS(RR+10) .LT. epsilon .OR. ABS(RR) .LT. epsilon ) THEN
    ci = conv_index
    IF((SfcTyp .GE. LD_TYP) .AND. ( ABS(RR+10) .LT. epsilon) ) THEN
        IF(ABS(ci-0) .LT. epsilon) RR=0.0
        IF(ABS(ci-1) .LT. epsilon) RR=1.97*RR_SCAL+fix
        IF(ABS(ci-2) .LT. epsilon) RR=5.95*RR_SCAL+fix
        IF(ABS(ci-3) .LT. epsilon) RR=10.95*RR_SCAL+fix
    ENDIF
    IF(SfcTyp .EQ. OC_TYP) THEN 
        IF(CLWP .GE. 0.4) THEN
            b0(1:4) = (/1.50155, 1.93515, 4.17168, 7.81001/)
            b1(1:4) = (/0.0591993, 0.566360, 2.03765, 4.91456/)
!           ci=0
            IF(ABS(ci-0) .LT. epsilon) RR = (b0(1)+b1(1)*CLWP)*RR_SCAL+fix
            IF(ABS(ci-1) .LT. epsilon) RR = (b0(2)+b1(2)*CLWP)*RR_SCAL+fix
            IF(ABS(ci-2) .LT. epsilon) RR = (b0(3)+b1(3)*CLWP)*RR_SCAL+fix
            IF(ABS(ci-3) .LT. epsilon) RR = (b0(4)+b1(4)*CLWP)*RR_SCAL+fix
        ENDIF
        IF((CLWP .LT. 0.4) .AND. (CLWP .GE. 0)) THEN
            IF( (ABS(ci-0) .LT. epsilon) .OR. (ABS(ci-1) .LT. epsilon) .OR. &
                (ABS(ci-2) .LT. epsilon) .OR. (ABS(ci-3) .LT. epsilon) ) RR = 0
        ENDIF
    ENDIF
  ENDIF
  IF(ABS(RR-1) .LT. epsilon) RR = 2
  IF(RR .GE. RR_MAX) RR = RR_MAX
  RR=RR/RR_SCAL
  IF(RR .LT. 0) RR = 0.

  IF (SfcTyp .EQ. SEAICE_TYP .OR. SfcTyp .EQ. SNOW_TYP) RR = DEFAULT_VALUE_REAL
  IF (SfcTyp .EQ. LD_TYP .AND. Lat .LT. -60) RR = DEFAULT_VALUE_REAL
  IF (SfcTyp .EQ. SEAICE_TYP .AND. Lat .GT. 75) RR = DEFAULT_VALUE_REAL

  END SUBROUTINE ComputeRR

!===============================================================
! Name:         tpwclw
!
!
! Type:         Subroutine
!
!
! Description:  Computes clw and tpw (mm) over ocean based on the mspps
!               algorithm. 
!              
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nose              I              Node
!       - SfcTyp            I              Surface type
!       - alat              I              Latitude
!       - tb                I              Brightness temperatures (K)
!       - aice              I              Sea ice concentration (percentage)
!       - WindSp            I              Wind speed (m/s)
!       - ts                I              Surface Temperature (K)
!       - theta             I              Incident angle in radians (input)
!       - mu                I              mu = cos(theta)
!       - nchan             I              number of channel measurements of Ym
!       - tpw               O              Total precipitable water vapor (mm)
!       - clw               O              Integrated cloud liquid water (mm)
!
! Modules needed:
!       - None
!
!
! History: 
!       07-31-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE tpwclw(node,SfcTyp,alat,tb,aice,WindSp,ts,theta,mu,nchan,EMISS,tpw,clw)
    !---Input/Output variables
    INTEGER              :: SfcTyp,node,nchan
    REAL                 :: alat,aice,WindSp,ts
    REAL                 :: theta,mu,tpw,clw
    REAl, DIMENSION(:)   :: tb
    REAl, DIMENSION(:)   :: EMISS
    !---Internal variables
    INTEGER                :: ich
    REAL, PARAMETER        :: MISSING=-99,PECENT_ICE=0.0,INDETERM_SICE=-8
    REAL, PARAMETER        :: CLW_SCAL=100.0,TPW_SCAL=10.0
    REAL, PARAMETER        :: TPW_upper=75.0,TPW_lower=0.0
    REAL, PARAMETER        :: CLW_upper=6.0,CLW_lower=0.0
    REAL, DIMENSION(2)     :: products
    REAL, DIMENSION(4)     :: corr,corr1
    REAl, DIMENSION(nchan) :: tba
    REAL, DIMENSION(4,6)   :: corr_asc,corr_desc
    REAL                   :: theta_deg,df1,tb23,tb31,tb50
 
    products(:) = MISSING
    corr(:)=0.
    tpw=MISSING
    clw=MISSING
    IF(SfcTyp .EQ. OC_TYP) THEN
       corr_asc(1,1:6) = (/-2.28626, 68.2791, 9.08166, -2.39233e-08, 0.00146645, 0.000136875/)
       corr_asc(2,1:6) = (/-1.77232, 62.2333, 7.74059, -4.61005e-08, 0.00298159, -0.000294810/)
       corr_asc(3,1:6) = (/3.25164, 60.1840, 17.9200, -0.0115559, -0.00411548, 0.000423336/)
       corr_asc(4,1:6) = (/-3.94168, 71.3562, 16.7611, 0.000458363, 0.0217407, 0.000400434/)
       corr_desc(1,1:6)= (/6.97408, 77.7821, 36.1330, -0.687439, -0.0520968, -0.000590253/)
       corr_desc(2,1:6)= (/-1.34805, -66.8981, 9.67602, 1.44355e-08, -0.00364870, -0.000270088/)
       corr_desc(3,1:6)= (/1.73330, 54.9991, 11.7439, -3.00480e-05, 0.0100818, 0.000744985/)
       corr_desc(4,1:6)= (/-3.73790, 60.6724, 11.5138, 3.47802e-06, 0.0463184, 0.000465675/)
       theta_deg=theta*180/PI
       IF(node .EQ. 0) THEN
          DO ich=1,3
             corr(ich)=corr_asc(ich,1)*exp(-(((theta_deg-corr_asc(ich,2))/corr_asc(ich,3))**2)/2.)+ &
                  corr_asc(ich,4)+corr_asc(ich,5)*theta_deg+corr_asc(ich,6)*(theta_deg**2)
          ENDDO
       ENDIF
       IF(node .EQ. 1) THEN
          DO ich=1,3
             corr(ich)=corr_desc(ich,1)*exp(-(((theta_deg-corr_desc(ich,2))/corr_desc(ich,3))**2)/2.)+ &
                  corr_desc(ich,4)+corr_desc(ich,5)*theta_deg+corr_desc(ich,6)*(theta_deg**2.0)
             corr1(ich)=corr_desc(ich,1)*exp(-(((theta_deg-corr_desc(ich,2))/corr_desc(ich,3))**2)/2.)+ &
                  corr_desc(ich,4)+corr_desc(ich,5)*theta_deg+corr_desc(ich,6)*(theta_deg**2.0)
          ENDDO
       ENDIF
       tba=tb
       tb23=tba(1)+corr(1)
       tb31=tba(2)+corr(2)
       tb50=tba(3)+corr(3)
       tba(1)=tb23
       tba(2)=tb31
       IF(aice .GT. PECENT_ICE) THEN
          products = INDETERM_SICE
       ELSE
          df1 = 2.85 + 0.020 * tb23 - 0.028 * tb50
          IF(((alat .GT. 50.0) .OR. (alat .LT. -50.0)) .AND. (df1 .GT. 0.2)) THEN
             products = INDETERM_SICE
          ENDIF
          CALL water_vapor_liquid(tba,WindSp,ts,theta,mu,EMISS,products)
          IF(products(1) .GT. TPW_upper*TPW_SCAL) products(1)= TPW_upper*TPW_SCAL
          IF(products(1) .LT. TPW_lower*TPW_SCAL) products(1) = MISSING
          IF(products(2) .GT. CLW_upper*CLW_SCAL) products(2)= CLW_upper*CLW_SCAL
          IF(products(2) .LT. CLW_lower*CLW_SCAL) products(2) = MISSING
       ENDIF
    ENDIF
    IF(products(1) .GE. 0) tpw=products(1)/TPW_SCAL
    IF(products(2) .GE. 0) clw=products(2)/CLW_SCAL
  END SUBROUTINE tpwclw

!===============================================================
! Name:         water_vapor_liquid
!
!
! Type:         Subroutine
!
!
! Description:  Calculates clw and tpw (mm) based on the mspps
!               algorithm. 
!              
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - tb                I              Brightness temperatures (K)
!       - wind              I              wind speed (m/s)  
!       - tsavn             I              surface temperature (K)
!       - ts                I              Surface Temperature (K)
!       - theta             I              Incident angle in radians (input)
!       - mu                I              mu = cos(theta)
!       - products          I/O            products(1)=tpw,products(2)=clw
!
! Modules needed:
!       - None
!
!
! History: 
!       07-31-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  SUBROUTINE water_vapor_liquid(tb,wind,tsavn,theta,mu,EMISS,products) 
    !---Input/Output variables  
    REAL                 :: wind,tsavn,theta,mu
    REAL,DIMENSION(2)    :: products
    REAl, DIMENSION(:)   :: tb
    REAl, DIMENSION(:)   :: EMISS
    !---Internal variables
    REAL, PARAMETER      :: BELOW_PRODUCT=-2.0
    REAL, PARAMETER      :: kv23=4.80423e-3 
    REAL, PARAMETER      :: kv31=1.93241e-3
    REAL, PARAMETER      :: a1=kv31/kv23
    REAL, PARAMETER      :: salinity=35.5
    REAL, PARAMETER      :: earthrad=6371
    REAL, PARAMETER      :: satheight=833
    REAL, PARAMETER      :: coeA=0.968
    REAL, PARAMETER      :: coeB=-1.878
    REAL, PARAMETER      :: coe1_a=-2.2682
    REAL, PARAMETER      :: coe1_b=-2.7575
    REAL, PARAMETER      :: coe1_c=3.064293419
    REAL, PARAMETER      :: coe1_d=-1.844769074
    REAL, PARAMETER      :: coe1_e=1.100935855
    REAL, PARAMETER      :: coe1_f=2.573123026
    REAL, PARAMETER      :: coe2_a=44.7
    REAL, PARAMETER      :: coe2_b=-73.142
    REAL, PARAMETER      :: coe2_c=27.95998241
    REAL, PARAMETER      :: coe2_d=-2.699010169
    REAL, PARAMETER      :: coe2_e=1.116107868
    REAL, PARAMETER      :: coe2_f=4.454987186
    REAL, PARAMETER      :: coe3_a=35.984
    REAL, PARAMETER      :: coe3_b=-79.406
    REAL, PARAMETER      :: coe3_c=37.87657236
    REAL, PARAMETER      :: coe3_d=-3.907292323
    REAL, PARAMETER      :: coe3_e=1.20336045
    REAL, PARAMETER      :: coe3_f=3.959459873
    REAL, PARAMETER      :: CLW_SCAL=100.0,TPW_SCAL=10.0
    REAL                 :: tbir,tl,below_prod,tb23,tb31,sinthetas,costhetas
    REAL                 :: em23,em31,tauo23,tauo31,kl23,kl31
    REAL                 :: b0,b1,b2,a0,a2,tpwtmp,clwtmp,tmp
    REAL,DIMENSION(2)    :: em23hv,em31hv
    !---Emissivities variables
    INTEGER              :: EmFg

    EmFg=1
    tbir = 285 !Use constant because AVHRR data is not available
    tl = tbir - 273.15
    if(tl < 0.0 ) tl=5
    below_prod=BELOW_PRODUCT
    tb23 = tb(1)
    tb31 = tb(2)
    sinthetas = sin(theta)* earthrad/(earthrad + satheight)
    sinthetas = (sinthetas**2)
    costhetas = 1.0 - sinthetas
    em23=EMISS(1)
    em31=EMISS(2)
    IF (EmFg .EQ. 1) THEN
       CALL emissivity_water(wind,theta,tsavn,salinity,23.8e9,em23hv)
       em23 = costhetas*em23hv(2) + sinthetas*em23hv(1)
       CALL emissivity_water(wind,theta,tsavn,salinity,31.4e9,em31hv)
       em31 = costhetas*em31hv(2) + sinthetas*em31hv(1)
    ENDIF
    tauo23 = 3.21410e-2 - 6.31860e-5*tsavn
    tauo31 = 5.34214e-2 - 1.04835e-4*tsavn
    kl23 = 1.18203e-1 - 3.48761e-3*tl + 5.01301e-5*tl*tl
    kl31 = 1.98774e-1 - 5.45692e-3*tl + 7.18339e-5*tl*tl
    b0 = .5*kl23/(kv23*kl31 - kv31*kl23)
    b1 =  kl31/kl23
    b2 = - 2.0*(tauo31 - b1*tauo23)/mu + (1.0 - b1)*log(tsavn) + &
         log(1.0 - em31) - b1*log(1.0 - em23)
    a0 = -.5*kv23/(kv23*kl31 - kv31*kl23)
    a2 = -2.0*(tauo31 -a1*tauo23)/mu + (1.0 - a1)*log(tsavn) + &
         log(1.0 - em31) - a1*log(1.0 - em23)
    tpwtmp = coeA * (mu*b0*(log(tsavn - tb31) - b1*log(tsavn - tb23) - b2)) + coeB 
    clwtmp = mu*a0*(log(tsavn - tb31) - a1*log(tsavn - tb23) - a2) 
    if (clwtmp .LT. 0.0) clwtmp = 0.
    !---Move TPW lower limit check from main routine to here 
    if(tpwtmp .LT. 0) products(1) = below_prod
    IF(tpwtmp .GE. 0) THEN
       IF((clwtmp .GE. 0.) .AND. (clwtmp .LT. 0.2)) THEN
          tmp = tpwtmp + coe1_a*mu*mu+ coe1_b*mu + coe1_c
          IF(tmp .GT. 0.) tpwtmp = coe1_d*log(tmp) + coe1_e*tmp + coe1_f
       ENDIF
       IF((clwtmp .GE. 0.2) .AND. (clwtmp .LT. 0.8)) THEN
          tmp = tpwtmp + coe2_a*mu*mu+ coe2_b*mu + coe2_c
          IF(tmp .GT. 0.) tpwtmp = coe2_d*log(tmp) + coe2_e*tmp + coe2_f
       ENDIF
       IF(clwtmp .GE. 0.8) THEN
          tmp = tpwtmp + coe3_a*mu*mu+ coe3_b*mu + coe3_c
          IF(tmp .GT. 0.) THEN
             !---5/1/06: flag TPW by its negative value if CLW >= 0.8 mm because of possible rain contamination
             tpwtmp = -(coe3_d*log(tmp) + coe3_e*tmp + coe3_f)
             !---5/1/06: Adjust certain values to avoid confusion with existing flags 
             !---flag for missing data: -99
             IF( ABS( ABS(tpwtmp) - 9.9) .LT. epsilon ) tpwtmp = -10.
             !---flag for land or coast: -10
             IF( ABS( ABS(tpwtmp) - 1.0) .LT. epsilon ) tpwtmp = -1.1
             !---flag for sea ice: -8
             IF( ABS( ABS(tpwtmp) - 0.8) .LT. epsilon ) tpwtmp = -0.9
             !---flag for BELOW_PROD: -2
             IF( ABS( ABS(tpwtmp) - 0.2) .LT. epsilon ) tpwtmp = -0.3
          ENDIF
       ENDIF
       products(1) = TPW_SCAL * tpwtmp
       products(2) = CLW_SCAL * clwtmp
    ENDIF
  END SUBROUTINE water_vapor_liquid

!==================================================================
! Name:         emissivity_water
!
!
! Type:         Subroutine
!
!
! Description:  Calculates the emissivity based on the mspps
!               algorithm. 
!
!     Variables: angle = incident angle in radians (input)
!         t     = temperature (K)
!         s     = sea water salinity (per thousand)
!         f     = frequency (Hz)
!         wind  = wind speed (m/s)
!
!     Internal Variables
!
!         foam  = foam fraction
!         g,tr  = emperical functions for wind induced
!                 changes in reflection coefficient
!
!     Output
!         Emissivity vector (eh, ev)
!
!     Literature Sources:
!
!     (1) Calm water emissvity
!         Klein and Swift (KS) emissivity for calm water (1977) IEEE Trans.
!         Antennas Propag., 25, 104-111
!     (2) Roughtness correction part for emissivity
!         Francies et al. 1983
!         Satellite microwave remote sensing
!         Observations was made by Hollinger (1971), HO71
!     (3) Foam emissivity is based on
!         Stogryn (1972). JGR, 77, 641-653, ST72
!
! History: 
!       07-31-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!==================================================================
  SUBROUTINE emissivity_water(wind,angle,t,s,f,em_vector)
    !---Input/Output variables  
    REAL                 :: wind,angle,t,s,f
    REAL, DIMENSION(2)   :: em_vector
    !---Internal variables
    REAL, PARAMETER      :: PI=3.141593
    REAL                 :: epsx,epsy,degre,xx,yy,dd,rr,aid1x,aid1y
    REAL                 :: foam,aid2x,aid2y,aid3x,aid3y,degre2
    REAL                 :: degre3,degre10,g,tr,aidx,aidy,tmp
    REAL                 :: ref,rclear,rfoam,rh,rv
    em_vector=0.
    CALL water_dielectric(t,s,f,epsx,epsy)
    !---eps is the complex dielectric constant of saltine water
    degre= (angle / PI) * 180.0
    xx = epsx - ((sin(angle))**2)
    yy = epsy 
    dd = atan2(yy, xx)/2.0 
    rr = ( ( (xx**2) + (yy**2) )**0.25)
    aid1x = rr*cos(dd)
    aid1y = rr*sin(dd)
    !---Fractional amount of foam coverage
    foam = 7.751e-6 * (wind**.231)
    if (foam < 0.0) foam = 0.0
    if (foam > 1.0) foam = 1.0
    !---Compute the emissivity for horizontal polarization
    aid2x = cos(angle) - aid1x
    aid2y = - aid1y
    aid3x = cos(angle) + aid1x
    aid3y = aid1y
    degre2 = (degre**2)
    degre3 = (degre**3)
    degre10 = (degre**10)
    !---Empirical functions for wind induced reflection change
    g = 1.0 - 1.748e-3 * degre - 7.336e-5 * degre2 + 1.044e-7 *degre3
    tr = wind * (1.15e-1 + 3.8e-5 * degre2 ) * sqrt(f*1.0e-9)
    aidx = (aid2x * aid3x + aid2y * aid3y)/((aid3x**2) + (aid3y**2))
    aidy = (aid2y * aid3x - aid2x * aid3y)/((aid3x**2) + (aid3y**2))
    tmp = sqrt(aidx * aidx + aidy * aidy) 
    ref = (tmp**2)
    rclear = ref - tr / t
    !---Reflection coeff. of foam covered sea surface
    rfoam = 1.0 - (208.0 + 1.29e-9 * f)/ t*g
    !---Linear interpolation between foam free and foam covered reflection coeff.
    rh = ( 1.0 - foam) * rclear + foam * rfoam
    if (rh > 1.0) rh = 1.0
    if (rh < 0.0) rh = 0.0
    em_vector(1) = 1.0 - rh
    !---Compute the emissivity for vertical polarization
    aid2x = epsx * cos(angle) - aid1x
    aid2y = epsy * cos(angle) - aid1y
    aid3x = epsx * cos(angle) + aid1x
    aid3y = epsy * cos(angle) + aid1y
    !---Empirical functions for wind induced reflection changes
    g  = 1.0 - 9.946e-4 * degre + 3.218e-5 * degre2 - 1.187e-6 * degre3 + 7.e-20*degre10
    tr = wind*(1.17e-1-2.09e-3*exp(7.32e-2*degre)) *sqrt(f*1.0e-9)
    aidx = (aid2x * aid3x + aid2y * aid3y)/((aid3x**2) + (aid3y**2))
    aidy = (aid2y * aid3x - aid2x * aid3y)/((aid3x**2) + (aid3y**2))
    tmp = sqrt(aidx * aidx + aidy * aidy)
    ref = (tmp**2)
    rclear = ref - tr / t
    !--- Reflection coeff. of foam covered sea surface
    rfoam = 1.0 - (208.0 + 1.29e-9 * f)/ t*g
    !---Linear interpolation between foam free and foam covered reflection coeff.
    rv = ( 1.0 - foam) * rclear + foam * rfoam
    if (rv > 1.0) rv = 1.0
    if (rv < 0.0) rv = 0.0
    em_vector(2) = 1.0 - rv
  END SUBROUTINE emissivity_water

!==================================================================
! This subroutine calculates the dielectric constant of saline water
! based on the MSPPS algorithms
! Reference:
!
! Microwave remote sensing by Ulaby et al (1984)  pp 2022
!
! t1    Water Skin temperature (K)
! s     Salinity  Parts per thousand
! f     Frequency (Hz)
!
!
! History: 
!       07-31-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!==================================================================
  SUBROUTINE water_dielectric(t1,s,f,epsx,epsy)
    !---Input/Output variables  
    REAL                 :: t1,s,f,epsx,epsy
    !---Internal variables
    REAL, PARAMETER      :: eswi=4.9,eo=8.854e-12,PI=3.141593
    REAL                 :: t,t2,t3,s2,s3,eswo,a,esw
    REAL                 :: tswo,b,tsw,epsp,sswo,d
    REAL                 :: fi,ssw,epspp
    t = t1 - 273.0
    t2 = t*t
    t3 = t**3
    s2 = s*s
    s3 = s**3
    eswo = 87.134 - 1.949e-1 * t - 1.276e-2 * t2 + 2.491e-4 * t3
    a = 1.0 + 1.613e-5 * t*s - 3.656e-3 * s + 3.210e-5 * s2 - 4.232e-7 * s3
    esw = eswo * a
    tswo = 1.1109e-10 - 3.824e-12 * t + 6.938e-14 * t2 - 5.096e-16 * t3
    b = 1.0 + 2.282e-5 * t * s - 7.638e-4 * s - 7.760e-6 * s2 + 1.105e-8 * s3
    tsw = tswo*b
    epsp = eswi + (esw - eswi)/ (1.0 + ((f * tsw)**2) )
    sswo = s*(0.18252-1.4619e-3*s+2.093e-5*s2-1.282e-7*s3);
    d  = 25.0 - t;
    fi = d * (2.033e-2 + 1.266e-4 * d + 2.464e-6 * d * d - &
         s * (1.849e-5 - 2.551e-7 * d + 2.551e-8 * d * d))
    ssw = sswo * exp(-fi)
    epspp = tsw * f * (esw - eswi) / (1.0 + ((tsw * f)**2) )
    epspp = epspp + ssw/(2.0 * PI * eo * f);
    epsx =   epsp;
    epsy = - epspp;
  END SUBROUTINE water_dielectric


  SUBROUTINE DetectSnowCover(SWE,SnowCover)
    IMPLICIT NONE
    REAL    :: SWE,SnowCover
    SnowCover = DEFAULT_VALUE_REAL
    IF (SWE .GE. epsilon) THEN
      SnowCover =1.
    ELSE IF ( ABS(SWE) .LT. epsilon) THEN
      SnowCover =0.
    ENDIF
    RETURN
  END SUBROUTINE DetectSnowCover

  SUBROUTINE ComputeSWEandGS(sensor_id,Emissivity,nchan,Tskin,flat,flon,SfcTyp,qc,SnowData,nprofSnow,&
             nparmSnow,EffGrainSize,ComputeSWE)
    IMPLICIT NONE
    REAL                  :: Tskin,mindistance,distance,distance0,ComputeSWE,slope1,slope2,slope3,slope4, &
                             slope5,slope6,coeff19,coeff22,coeff23,coeff31,coeff37,coeff50,coeff89, &
                             coeff92,coeff157,coeff190,flat,flon
    INTEGER                  :: sensor_id,i,nchan,SfcTyp,nprofSnow,nparmSnow
    REAL,    DIMENSION(:) :: Emissivity
    REAL,    DIMENSION(nprofSnow) :: GrainSize,SD,SWE,EM19V,EM19H,EM22V,EM23,EM31,EM37V,EM37H,EM50, &
                             EM89,EM92V,EM92H,EM157,EM190
    REAL,    DIMENSION(nprofSnow,nparmSnow):: SnowData
    REAL                     :: EffGrainSize,tb1,tb3,tb19H,PRtb19
    INTEGER(2), DIMENSION(:) :: qc
    INTEGER :: offset_amsr ! needed to account for the 2 extra channels at 6.9 GHz on AMSR2

    REAL :: SWE0 ! Background SWE (cm)
    REAL :: GS0 ! Background GS (mm)
    REAL :: errSWE ! Background SWE uncertainty (cm)
    REAL :: errGS ! Background GS uncertainty (mm)
    REAL :: scaleBGerrSWE, scaleBGerrGS ! weights to modulate errSWE and errGS
    REAL :: deltaSWE, deltaGS, wtSWE, wtGS, multSWE, multGS


    ! Updated version of Snow Water and Grain Size retrieval algorithm
    ! C. Grassotti, IMSG, Inc. 
    ! August 2010
    
    ! This version uses a quasi-variational formulation in which the retrieved SWE and GS
    ! are found by minimizing a cost function with 3 terms:
    ! 1. Distance of current retrieved emissivities (actually emissivity gradients)
    !    with corresponding values in the precomputed emissivity catalog. 
    !    This term is scaled by the variance of that emissivity (gradient)
    !    in the actual catalog.
    ! 2. A penalty term which constrains how far the solution can deviate from the background SWE (specified below)
    ! 3. A penalty term which constrains how far the solution can deviate from the background GS (specified below)
    ! The penalty terms are also scaled by the assumed uncertainty in the background SWE and GS (fixed for all sensors),
    ! but modulated by scale factors which multiply the assumed uncertainty and which vary slightly with sensor (see below).
    ! Increasing scaleBGerrSWE (or decreasing scaleBGerrGS) allows the retrieved SWE to vary more relative to GS.
    ! Increasing scaleBGerrGS (or decreasing scaleBGerrSWE) allows the retrieved GS to vary more relative to SWE.
    ! All uncertainties and variances are assumed uncorrelated with one another.

    ! NOTE: Also updated are the emissivity catalogs which have been regenerated 
    ! using the NESDIS LandEM physical emissivity model embedded in version 2.x of CRTM. 
    ! This means the the values used for background means, uncertainties, and scale factors are all specific
    ! to, and based on testing with, the new emissivity catalogs. Any changes to the catalogs, or to the 
    ! number of terms in the cost function may require adjustments to these values.

    ! Background SWE and GS: assumed same for all sensors
    SWE0=1.5 ! (SWE, cm)
    GS0=0.40 ! (GS, mm)


    ! Baseline Background SWE and GS uncertainty: assumed same for all sensors
    ! However, these values are subsequently multiplied 
    ! by sensor-dependent scale factors scaleBGerrSWE and scaleBGerrGS
    errSWE=2.52 ! BG SWE uncertainty (cm),  equivalent to 10 cm snow depth uncertainty
                ! assuming volume fraction= 0.275 and ice density= 0.917 gm/cm**3        

    errGS=0.2   ! BG GS uncertainty (mm)



    ! Initialize curent SWE and GS to missing
    ComputeSWE   = DEFAULT_VALUE_REAL
    EffGrainSize = DEFAULT_VALUE_REAL

    !print *,'Begin computeSWEand GS'
    !print *,'sensor_id_npp,sensor_id,SNOW_TYP,SfcTyp=',sensor_id_npp,sensor_id,SNOW_TYP,SfcTyp

    ! Sensor specific sections

    IF ( (Sensor_id .eq. sensor_id_n18 .or. Sensor_id .eq. sensor_id_n19) &
         .and. (SfcTyp .eq. SNOW_TYP .or. SfcTyp .eq. LD_TYP) ) THEN

       ! Weights to modulate errSWE and errGS: varies with sensor
       ! These are scaling factors that multiply the baseline errors in SWE and GS
       ! Ideally, they should be the same for all sensors, but testing shows that thus far N18 factors need to 
       ! be slightly different than those for F16, possibly due to the emissivity catalogs and/or the number of terms used 
       ! in the distance0 part of the cost function (i.e. slope1, slope2, etc.)

       ! For N18, we found (scaleBGerrSWE)**2=10 and (scaleBGerrGS)**2=0.25 optimal
       scaleBGerrSWE=3.16 ! sqrt(10.)
       scaleBGerrGS=0.5 ! sqrt(0.25)

       ! Compute "weight" factors for SWE and GS for this sensor
       ! This will multiply the current search distance to the BG SWE and GS

       wtSWE=1./((scaleBGerrSWE * errSWE)**2)
       wtGS=1./((scaleBGerrGS * errGS)**2)

       ! sqrt(variance) of catalog tables produced from CRTM emis model
       coeff23 = 1.0
       coeff31 = 0.025
       coeff50 = 0.138
       coeff89 = 0.245
       coeff157 = coeff89*2.0
       coeff190 = coeff89*3.0

       mindistance = 1.0E+09
       EM23 = SnowData(:,5)
       EM31 = SnowData(:,6)
       EM50 = SnowData(:,7)
       EM89 = SnowData(:,8)
       EM157 = SnowData(:,9)
       EM190 = SnowData(:,10)
       GrainSize = SnowData(:,1)
       SWE = SnowData(:,4)
       SD = SnowData(:,3)


       ! Loop over entire catalog, finding the SWE,GS combination that minimizes the total cost function
       DO i=1,nprofSnow

          ! Compute distance of retrieved emissivity (gradients) to catalog values
          slope1 = ( (Emissivity(1) - Emissivity(2)) -  (EM23(i) - EM31(i)) )**2
          slope2 = ( (Emissivity(1) - Emissivity(3)) -  (EM23(i) - EM50(i)) )**2
          slope6 = ( (Emissivity(2) - Emissivity(3)) -  (EM31(i) - EM50(i)) )**2
          slope3 = ( (Emissivity(1) - Emissivity(16)) -  (EM23(i) - EM89(i)) )**2
          slope4 = ( (Emissivity(16) - Emissivity(17)) -  (EM89(i) - EM157(i)) )**2
          slope5 = ( (Emissivity(17) - Emissivity(20)) -  (EM157(i) - EM190(i)) )**2

          ! Scale distance by catalog variance
          slope1 = slope1/(coeff31**2) 
          slope2 = slope2/(coeff50**2) 
          slope6 = slope6/(coeff50**2) 
          slope3 = slope3/(coeff89**2) 
          slope4 = slope4/(coeff157**2) 
          slope5 = slope5/(coeff190**2) 


          ! Distance is the sum over several channel combinations (in this case: 23-31, 23-50, 31-50 GHz)
          distance = slope1+slope2+slope6
          distance0=distance

          ! Compute current distance to BG SWE and GS, then scale by appropriate factors
          ! to get penalty terms
          deltaSWE= SWE(i) - SWE0
          deltaGS= (10.0 * GrainSize(i)) - GS0
          multSWE=wtSWE*(deltaSWE**2)
          multGS=wtGS*(deltaGS**2)

          ! Total cost function
          distance=distance0+multSWE+multGS

          IF(distance  .le. mindistance) THEN
             mindistance = distance
             ComputeSWE = SWE(i)
             EffGrainSize = 10.0 * GrainSize(i)
          ENDIF
       ENDDO

       IF( (flat .le. -62.) .or. (flat .ge. 59.6 .and. flat .le. 83.4 .and. flon .ge. -71.8 .and. &
            flon .le. -14.1) ) THEN
          tb1 = tskin * emissivity(1)
          tb3 = tskin * emissivity(3)
          if(tb1 <= 215.0 .and. tb3 <=225.0) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
          if(emissivity(16) - emissivity(17) >= 0.04) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
       ELSE
          IF( (tskin .gt. 273.0) .or. (Emissivity(1) - Emissivity(2)) .lt. 0.0) then
             ComputeSWE = 0.0
          ENDIF
       ENDIF

       IF ( ABS(ComputeSWE) .lt. epsilon ) EffGrainSize = 0.0

    ENDIF

    IF (Sensor_id .eq. sensor_id_metopA .and. (SfcTyp .eq. SNOW_TYP .or. SfcTyp .eq. LD_TYP) ) THEN

       ! Weights to modulate errSWE and errGS: varies with sensor
       ! These are scaling factors that multiply the baseline errors in SWE and GS
       ! Ideally, they should be the same for all sensors, but testing shows that thus far N18 factors need to 
       ! be slightly different than those for F16, possibly due to the emissivity catalogs and/or the number of terms used 
       ! in the distance0 part of the cost function (i.e. slope1, slope2, etc.)

       ! For N18, we found (scaleBGerrSWE)**2=10 and (scaleBGerrGS)**2=0.25 optimal
       scaleBGerrSWE=3.16 ! sqrt(10.)
       scaleBGerrGS=0.5 ! sqrt(0.25)

       ! Compute "weight" factors for SWE and GS for this sensor
       ! This will multiply the current search distance to the BG SWE and GS

       wtSWE=1./((scaleBGerrSWE * errSWE)**2)
       wtGS=1./((scaleBGerrGS * errGS)**2)

       ! sqrt(variance) of catalog tables produced from CRTM emis model
       coeff23 = 1.0
       coeff31 = 0.025
       coeff50 = 0.138
       coeff89 = 0.245
       coeff157 = coeff89*2.0
       coeff190 = coeff89*3.0

       mindistance = 1.0E+09
       EM23 = SnowData(:,5)
       EM31 = SnowData(:,6)
       EM50 = SnowData(:,7)
       EM89 = SnowData(:,8)
       EM157 = SnowData(:,9)
       EM190 = SnowData(:,10)
       GrainSize = SnowData(:,1)
       SWE = SnowData(:,4)
       SD = SnowData(:,3)


       ! Loop over entire catalog, finding the SWE,GS combination that minimizes the total cost function
       DO i=1,nprofSnow

          ! Compute distance of retrieved emissivity (gradients) to catalog values
          slope1 = ( (Emissivity(1) - Emissivity(2)) -  (EM23(i) - EM31(i)) )**2
          slope2 = ( (Emissivity(1) - Emissivity(3)) -  (EM23(i) - EM50(i)) )**2
          slope6 = ( (Emissivity(2) - Emissivity(3)) -  (EM31(i) - EM50(i)) )**2
          slope3 = ( (Emissivity(1) - Emissivity(16)) -  (EM23(i) - EM89(i)) )**2
          slope4 = ( (Emissivity(16) - Emissivity(17)) -  (EM89(i) - EM157(i)) )**2
          slope5 = ( (Emissivity(17) - Emissivity(20)) -  (EM157(i) - EM190(i)) )**2

          ! Scale distance by catalog variance
          slope1 = slope1/(coeff31**2) 
          slope2 = slope2/(coeff50**2) 
          slope6 = slope6/(coeff50**2) 
          slope3 = slope3/(coeff89**2) 
          slope4 = slope4/(coeff157**2) 
          slope5 = slope5/(coeff190**2) 


          ! Distance is the sum over several channel combinations (in this case: 23-31, 23-50, 31-50 GHz)
          distance = slope1+slope2+slope6
          distance0=distance

          ! Compute current distance to BG SWE and GS, then scale by appropriate factors
          ! to get penalty terms
          deltaSWE= SWE(i) - SWE0
          deltaGS= (10.0 * GrainSize(i)) - GS0
          multSWE=wtSWE*(deltaSWE**2)
          multGS=wtGS*(deltaGS**2)

          ! Total cost function
          distance=distance0+multSWE+multGS

          IF(distance  .le. mindistance) THEN
             mindistance = distance
             ComputeSWE = SWE(i)
             EffGrainSize = 10.0 * GrainSize(i)
          ENDIF
       ENDDO

       IF( (flat .le. -62.) .or. (flat .ge. 59.6 .and. flat .le. 83.4 .and. flon .ge. -71.8 .and. &
            flon .le. -14.1) ) THEN
          tb1 = tskin * emissivity(1)
          tb3 = tskin * emissivity(3)
          if(tb1 <= 215.0 .and. tb3 <=225.0) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
          if(emissivity(16) - emissivity(17) >= 0.04) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
       ELSE
          IF( (tskin .gt. 273.0) .or. (Emissivity(1) - Emissivity(2)) .lt. 0.0) then
             ComputeSWE = 0.0
          ENDIF
       ENDIF

       IF ( ABS(ComputeSWE) .lt. epsilon ) EffGrainSize = 0.0
    ENDIF
    
    IF (Sensor_id .eq. sensor_id_metopB .and. (SfcTyp .eq. SNOW_TYP .or. SfcTyp .eq. LD_TYP) ) THEN

       ! Weights to modulate errSWE and errGS: varies with sensor
       ! These are scaling factors that multiply the baseline errors in SWE and GS
       ! Ideally, they should be the same for all sensors, but testing shows that thus far N18 factors need to 
       ! be slightly different than those for F16, possibly due to the emissivity catalogs and/or the number of terms used 
       ! in the distance0 part of the cost function (i.e. slope1, slope2, etc.)

       ! For N18, we found (scaleBGerrSWE)**2=10 and (scaleBGerrGS)**2=0.25 optimal
       scaleBGerrSWE=3.16 ! sqrt(10.)
       scaleBGerrGS=0.5 ! sqrt(0.25)

       ! Compute "weight" factors for SWE and GS for this sensor
       ! This will multiply the current search distance to the BG SWE and GS

       wtSWE=1./((scaleBGerrSWE * errSWE)**2)
       wtGS=1./((scaleBGerrGS * errGS)**2)

       ! sqrt(variance) of catalog tables produced from CRTM emis model
       coeff23 = 1.0
       coeff31 = 0.025
       coeff50 = 0.138
       coeff89 = 0.245
       coeff157 = coeff89*2.0
       coeff190 = coeff89*3.0

       mindistance = 1.0E+09
       EM23 = SnowData(:,5)
       EM31 = SnowData(:,6)
       EM50 = SnowData(:,7)
       EM89 = SnowData(:,8)
       EM157 = SnowData(:,9)
       EM190 = SnowData(:,10)
       GrainSize = SnowData(:,1)
       SWE = SnowData(:,4)
       SD = SnowData(:,3)


       ! Loop over entire catalog, finding the SWE,GS combination that minimizes the total cost function
       DO i=1,nprofSnow

          ! Compute distance of retrieved emissivity (gradients) to catalog values
          slope1 = ( (Emissivity(1) - Emissivity(2)) -  (EM23(i) - EM31(i)) )**2
          slope2 = ( (Emissivity(1) - Emissivity(3)) -  (EM23(i) - EM50(i)) )**2
          slope6 = ( (Emissivity(2) - Emissivity(3)) -  (EM31(i) - EM50(i)) )**2
          slope3 = ( (Emissivity(1) - Emissivity(16)) -  (EM23(i) - EM89(i)) )**2
          slope4 = ( (Emissivity(16) - Emissivity(17)) -  (EM89(i) - EM157(i)) )**2
          slope5 = ( (Emissivity(17) - Emissivity(20)) -  (EM157(i) - EM190(i)) )**2

          ! Scale distance by catalog variance
          slope1 = slope1/(coeff31**2) 
          slope2 = slope2/(coeff50**2) 
          slope6 = slope6/(coeff50**2) 
          slope3 = slope3/(coeff89**2) 
          slope4 = slope4/(coeff157**2) 
          slope5 = slope5/(coeff190**2) 


          ! Distance is the sum over several channel combinations (in this case: 23-31, 23-50, 31-50 GHz)
          distance = slope1+slope2+slope6
          distance0=distance

          ! Compute current distance to BG SWE and GS, then scale by appropriate factors
          ! to get penalty terms
          deltaSWE= SWE(i) - SWE0
          deltaGS= (10.0 * GrainSize(i)) - GS0
          multSWE=wtSWE*(deltaSWE**2)
          multGS=wtGS*(deltaGS**2)

          ! Total cost function
          distance=distance0+multSWE+multGS

          IF(distance  .le. mindistance) THEN
             mindistance = distance
             ComputeSWE = SWE(i)
             EffGrainSize = 10.0 * GrainSize(i)
          ENDIF
       ENDDO

       IF( (flat .le. -62.) .or. (flat .ge. 59.6 .and. flat .le. 83.4 .and. flon .ge. -71.8 .and. &
            flon .le. -14.1) ) THEN
          tb1 = tskin * emissivity(1)
          tb3 = tskin * emissivity(3)
          if(tb1 <= 215.0 .and. tb3 <=225.0) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
          if(emissivity(16) - emissivity(17) >= 0.04) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
       ELSE
          IF( (tskin .gt. 273.0) .or. (Emissivity(1) - Emissivity(2)) .lt. 0.0) then
             ComputeSWE = 0.0
          ENDIF
       ENDIF

       IF ( ABS(ComputeSWE) .lt. epsilon ) EffGrainSize = 0.0
    ENDIF
    
    IF ( (Sensor_id .eq. sensor_id_f16 .or. Sensor_id .eq. sensor_id_f17 .or. Sensor_id .eq. sensor_id_f18) &
         .and. (SfcTyp .eq. SNOW_TYP .or. SfcTyp .eq. LD_TYP .or. &
       SfcTyp .eq. DESERT_TYP) ) THEN


       ! Weights to modulate errSWE and errGS: varies with sensor
       ! These are scaling factors that multiply the baseline errors in SWE and GS
       ! Ideally, they should be the same for all sensors, but testing shows that thus far N18 factors need to 
       ! be slightly different than those for F16, possibly due to the emissivity catalogs and/or the number of terms used 
       ! in the distance0 part of the cost function (i.e. slope1, slope2, etc.)

       ! For F16, we found (scaleBGerrSWE)**2=5 and (scaleBGerrGS)**2=0.10 optimal
       scaleBGerrSWE=2.24 ! sqrt(5.0)
       scaleBGerrGS=0.32 ! sqrt(0.10)
       
       ! Compute "weight" factors for SWE and GS for this sensor
       ! This will multiply the current search distance to the BG SWE and GS

       wtSWE=1./((scaleBGerrSWE * errSWE)**2)
       wtGS=1./((scaleBGerrGS * errGS)**2)

       ! sqrt(variance) of catalog tables produced from CRTM emis model
       coeff22 = 0.006
       coeff37 = 0.070
       coeff92 = 0.250


       mindistance = 1.0E+09
       EM19H = SnowData(:,5) 
       EM19V = SnowData(:,6)
       EM22V = SnowData(:,7)
       EM37H = SnowData(:,8)
       EM37V = SnowData(:,9)
       EM92V = SnowData(:,10)
       EM92H = SnowData(:,11)
       GrainSize = SnowData(:,1)
       SWE = SnowData(:,4)
       SD = SnowData(:,3)

       ! Loop over entire catalog, finding the SWE,GS combination that minimizes the total cost function
       DO i=1,nprofSnow
          
          ! Compute distance of retrieved emissivity (gradients) to catalog values
          slope1 = ( (Emissivity(12) - Emissivity(15)) -  (EM19H(i) - EM37H(i)) )**2
          slope2 = ( (Emissivity(13) - Emissivity(16)) -  (EM19V(i) - EM37V(i)) )**2
          slope3 = ( (Emissivity(13) - Emissivity(14)) -  (EM19V(i) - EM22V(i)) )**2
          slope4 = ( (Emissivity(12) - Emissivity(18)) -  (EM19H(i) - EM92H(i)) )**2
          slope5 = ( (Emissivity(13) - Emissivity(17)) -  (EM19V(i) - EM92V(i)) )**2

          ! Scale distance by catalog variance
          slope1=slope1/(coeff37**2)
          slope2=slope2/(coeff37**2)
          slope3=slope3/(coeff22**2)


          ! Distance is the sum over several channel combinations (in this case: 19H-37H, 19V-37V, 19V-22V GHz)
          distance = slope1+slope2+slope3
          distance0=distance

          ! Compute current distance to BG SWE and GS, then scale by appropriate factors
          ! to get penalty terms
          deltaSWE= SWE(i) - SWE0
          deltaGS= (10.0 * GrainSize(i)) - GS0
          multSWE=wtSWE*(deltaSWE**2)
          multGS=wtGS*(deltaGS**2)

          ! Total cost function
          distance=distance+multSWE+multGS

          IF(distance  .le. mindistance ) THEN
             mindistance = distance
             ComputeSWE = SWE(i)
             EffGrainSize = 10.0 * GrainSize(i)
          ENDIF

       ENDDO

       IF( (flat .le. -62.) .or. (flat .ge. 59.6 .and. flat .le. 83.4 .and. flon .ge. -71.8 .and. &
        flon .le. -14.1) ) THEN
          tb19H = tskin * emissivity(12)
          PRtb19  = tskin * (emissivity(13) - emissivity(12))
          if(tb19H .le. 215.0 .and. PRtb19 .ge. 23.0) then
             EffGrainSize = 0.75
             ComputeSWE = 15.0
          ENDIF
       ELSE
          IF(  (tskin .gt. 273.0) .or. (Emissivity(13) - Emissivity(16) .lt. 0.0) &
	  .or. (Emissivity(12) - Emissivity(15) .lt. 0.0)) THEN
             ComputeSWE = 0.0
          ENDIF
       ENDIF

       IF ( ABS(ComputeSWE) .lt. epsilon) EffGrainSize = 0.0
    ENDIF


    IF ( (Sensor_id .eq. sensor_id_amsre .or. Sensor_id .eq. sensor_id_gcomw1) &
         .and. (SfcTyp .eq. SNOW_TYP .or. SfcTyp .eq. LD_TYP .or. &
       SfcTyp .eq. DESERT_TYP) ) THEN

       if(Sensor_id .eq. sensor_id_amsre)then
          offset_amsr=0
       else
          offset_amsr=2 ! needed to account for the 2 extra channels at 6.9 GHz on AMSR2
       endif

       ! Weights to modulate errSWE and errGS: varies with sensor
       ! These are scaling factors that multiply the baseline errors in SWE and GS
       ! Ideally, they should be the same for all sensors, but testing shows that thus far N18 factors need to 
       ! be slightly different than those for F16, possibly due to the emissivity catalogs and/or the number of terms used 
       ! in the distance0 part of the cost function (i.e. slope1, slope2, etc.)

       ! For F16, we found (scaleBGerrSWE)**2=5 and (scaleBGerrGS)**2=0.10 optimal
       scaleBGerrSWE=2.24 ! sqrt(5.0)
       scaleBGerrGS=0.32 ! sqrt(0.10)
       
       ! Compute "weight" factors for SWE and GS for this sensor
       ! This will multiply the current search distance to the BG SWE and GS

       wtSWE=1./((scaleBGerrSWE * errSWE)**2)
       wtGS=1./((scaleBGerrGS * errGS)**2)

       ! sqrt(variance) of catalog tables produced from CRTM emis model
       coeff22 = 0.006
       coeff37 = 0.070
       coeff92 = 0.250


       mindistance = 1.0E+09
       EM19H = SnowData(:,5) 
       EM19V = SnowData(:,6)
       EM22V = SnowData(:,7)
       EM37H = SnowData(:,8)
       EM37V = SnowData(:,9)
       EM92V = SnowData(:,10)
       EM92H = SnowData(:,11)
       GrainSize = SnowData(:,1)
       SWE = SnowData(:,4)
       SD = SnowData(:,3)

       ! Loop over entire catalog, finding the SWE,GS combination that minimizes the total cost function
       DO i=1,nprofSnow
          
          ! Compute distance of retrieved emissivity (gradients) to catalog values
          slope1 = ( (Emissivity(6+offset_amsr) - Emissivity(10+offset_amsr)) -  (EM19H(i) - EM37H(i)) )**2
          slope2 = ( (Emissivity(5+offset_amsr) - Emissivity(9+offset_amsr)) -  (EM19V(i) - EM37V(i)) )**2
          slope3 = ( (Emissivity(5+offset_amsr) - Emissivity(7+offset_amsr)) -  (EM19V(i) - EM22V(i)) )**2
          slope4 = ( (Emissivity(6+offset_amsr) - Emissivity(12+offset_amsr)) -  (EM19H(i) - EM92H(i)) )**2
          slope5 = ( (Emissivity(5+offset_amsr) - Emissivity(11+offset_amsr)) -  (EM19V(i) - EM92V(i)) )**2

          ! Scale distance by catalog variance
          slope1=slope1/(coeff37**2)
          slope2=slope2/(coeff37**2)
          slope3=slope3/(coeff22**2)


          ! Distance is the sum over several channel combinations (in this case: 19H-37H, 19V-37V, 19V-22V GHz)
          distance = slope1+slope2+slope3
          distance0=distance

          ! Compute current distance to BG SWE and GS, then scale by appropriate factors
          ! to get penalty terms
          deltaSWE= SWE(i) - SWE0
          deltaGS= (10.0 * GrainSize(i)) - GS0
          multSWE=wtSWE*(deltaSWE**2)
          multGS=wtGS*(deltaGS**2)

          ! Total cost function
          distance=distance+multSWE+multGS

          IF(distance  .le. mindistance ) THEN
             mindistance = distance
             ComputeSWE = SWE(i)
             EffGrainSize = 10.0 * GrainSize(i)
          ENDIF

       ENDDO

       IF( (flat .le. -62.) .or. (flat .ge. 59.6 .and. flat .le. 83.4 .and. flon .ge. -71.8 .and. &
        flon .le. -14.1) ) THEN
          tb19H = tskin * emissivity(6+offset_amsr)
          PRtb19  = tskin * (emissivity(5+offset_amsr) - emissivity(6+offset_amsr))
          if(tb19H .le. 215.0 .and. PRtb19 .ge. 23.0) then
             EffGrainSize = 0.75
             ComputeSWE = 15.0
          ENDIF
       ELSE
          IF(  (tskin .gt. 273.0) .or. (Emissivity(5+offset_amsr) - Emissivity(9+offset_amsr) .lt. 0.0) &
	  .or. (Emissivity(6) - Emissivity(10) .lt. 0.0)) THEN
             ComputeSWE = 0.0
          ENDIF
       ENDIF

       IF ( ABS(ComputeSWE) .lt. epsilon) EffGrainSize = 0.0
    ENDIF


    IF (Sensor_id .eq. sensor_id_npp .and. (SfcTyp .eq. SNOW_TYP .or. SfcTyp .eq. LD_TYP) ) THEN

       ! Weights to modulate errSWE and errGS: varies with sensor
       ! These are scaling factors that multiply the baseline errors in SWE and GS
       ! Ideally, they should be the same for all sensors, but testing shows that thus far N18 factors need to 
       ! be slightly different than those for F16, possibly due to the emissivity catalogs and/or the number of terms used 
       ! in the distance0 part of the cost function (i.e. slope1, slope2, etc.)

       ! For N18, we found (scaleBGerrSWE)**2=10 and (scaleBGerrGS)**2=0.25 optimal
       ! NOTE: These scale factors may need to be adjusted post-launch with real NPP/ATMS data
       scaleBGerrSWE=3.16 ! sqrt(10.)
       scaleBGerrGS=0.5 ! sqrt(0.25)

       ! Compute "weight" factors for SWE and GS for this sensor
       ! This will multiply the current search distance to the BG SWE and GS

       wtSWE=1./((scaleBGerrSWE * errSWE)**2)
       wtGS=1./((scaleBGerrGS * errGS)**2)

       ! sqrt(variance) of catalog tables produced from CRTM emis model
       coeff23 = 1.0
       coeff31 = 0.025
       coeff50 = 0.138
       coeff89 = 0.245
       coeff157 = coeff89*2.0
       coeff190 = coeff89*3.0

       mindistance = 1.0E+09
       EM23 = SnowData(:,5)
       EM31 = SnowData(:,6)
       EM50 = SnowData(:,7)
       EM89 = SnowData(:,8)
       EM157 = SnowData(:,9)
       EM190 = SnowData(:,10)
       GrainSize = SnowData(:,1)
       SWE = SnowData(:,4)
       SD = SnowData(:,3)


       ! Loop over entire catalog, finding the SWE,GS combination that minimizes the total cost function
       DO i=1,nprofSnow

          ! Compute distance of retrieved emissivity (gradients) to catalog values
          slope1 = ( (Emissivity(1) - Emissivity(2)) -  (EM23(i) - EM31(i)) )**2
          slope2 = ( (Emissivity(1) - Emissivity(3)) -  (EM23(i) - EM50(i)) )**2
          slope6 = ( (Emissivity(2) - Emissivity(3)) -  (EM31(i) - EM50(i)) )**2
          slope3 = ( (Emissivity(1) - Emissivity(16)) -  (EM23(i) - EM89(i)) )**2
          slope4 = ( (Emissivity(16) - Emissivity(17)) -  (EM89(i) - EM157(i)) )**2
          slope5 = ( (Emissivity(17) - Emissivity(20)) -  (EM157(i) - EM190(i)) )**2

          ! Scale distance by catalog variance
          slope1 = slope1/(coeff31**2) 
          slope2 = slope2/(coeff50**2) 
          slope6 = slope6/(coeff50**2) 
          slope3 = slope3/(coeff89**2) 
          slope4 = slope4/(coeff157**2) 
          slope5 = slope5/(coeff190**2) 


          ! Distance is the sum over several channel combinations (in this case: 23-31, 23-50, 31-50 GHz)
          distance = slope1+slope2+slope6
          distance0=distance

          ! Compute current distance to BG SWE and GS, then scale by appropriate factors
          ! to get penalty terms
          deltaSWE= SWE(i) - SWE0
          deltaGS= (10.0 * GrainSize(i)) - GS0
          multSWE=wtSWE*(deltaSWE**2)
          multGS=wtGS*(deltaGS**2)

          ! Total cost function
          distance=distance0+multSWE+multGS

          IF(distance  .le. mindistance) THEN
             mindistance = distance
             ComputeSWE = SWE(i)
             EffGrainSize = 10.0 * GrainSize(i)
          ENDIF
       ENDDO

       IF( (flat .le. -62.) .or. (flat .ge. 59.6 .and. flat .le. 83.4 .and. flon .ge. -71.8 .and. &
            flon .le. -14.1) ) THEN
          tb1 = tskin * emissivity(1)
          tb3 = tskin * emissivity(3)
          if(tb1 <= 215.0 .and. tb3 <=225.0) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
          if(emissivity(16) - emissivity(17) >= 0.04) then
             EffGrainSize = 0.75
             ComputeSWE = 15.00
          endif
       ELSE
          IF( (tskin .gt. 273.0) .or. (Emissivity(1) - Emissivity(2)) .lt. 0.0) then
             ComputeSWE = 0.0
          ENDIF
       ENDIF

       IF ( ABS(ComputeSWE) .lt. epsilon ) EffGrainSize = 0.0
    ENDIF
    RETURN
  END SUBROUTINE ComputeSWEandGS

  SUBROUTINE ComputeSIC(sensor_id,Emissivity,nchan,Tskin,angle,flat,SfcTyp,qc,SeaiceData, &
                       nprofSeaice,nparmSeaice,nLatStratification,nAngleStratification,SIC,SIC_FY,SIC_MY)
    IMPLICIT NONE
    INTEGER                       :: sensor_id,nchan,SfcTyp,nprofseaIce,nparmSeaice,i, &
                                     nAngleStratification,nLatStratification,indexLow,indexHigh
    REAL                          :: SIC,SIC_FY,SIC_MY,flat,Tskin,mindistance,distance,gradient31, &
                                     gradient50,gradient89,gradient157,diff31,diff50,diff89,diff157, &
                                     c_total,c_my,c_fy,c_total1,c_fy1,c_my1,angle,distance1,mindistance1, &
                                     PR19,PR37,PR89,PR19_cat,PR37_cat,PR89_cat, GR37_89H, GR37_89V, GR19_37V,&
                                     GR19_37H,GR19_89V,GR19_89H,GR19_37V_cat,GR19_37H_cat, GR37_89H_cat, GR37_89V_cat, &
                                     GR19_89V_cat,GR19_89H_cat,distance2, mindistance2,c_total2,c_fy2,c_my2
    REAL                          :: distance3, mindistance3,c_total3,c_fy3,c_my3
    REAL                          :: distance4, mindistance4,c_total4,c_fy4,c_my4
    REAL,    PARAMETER            :: SIC_MIN = 20.0, LAT_ICE = 0.0, TSK_LOW = 272.0, TSK_HIGH = 280.0
    REAL,    PARAMETER            :: SIC_LOW = 35.0, SIC_HIGH = 70.0, GR1937V_SICTOT = 0.05, GR1937V_SICMY = 0.035
    INTEGER, PARAMETER            :: nchan_ssmis = 24

    REAL,    DIMENSION(:)         :: Emissivity
    REAL,    DIMENSION(nprofSeaice,nparmSeaice):: SeaIceData
    REAL,    DIMENSION(nchan)     :: diff
    REAL,    DIMENSION(nchan_ssmis)     :: diff_amsre
    INTEGER                       :: offset_amsr ! needed to account for 2 additional channels at 6.9 GHz on AMSR2
    INTEGER(2), DIMENSION(:)      :: qc

    SIC=DEFAULT_VALUE_REAL
    SIC_MY=DEFAULT_VALUE_REAL
    SIC_FY=DEFAULT_VALUE_REAL
    
    ! Updated version:
    ! C. Grassotti, Sept. 2010
    ! 1. Minor bug fixes to SIC algorithm for n18,n19,Metop,npp
    ! 2. Substantial changes to SIC algorithm for f16,f18: see detailed comments in f16,f18 section below

    ! Updated version:
    ! C. Grassotti, Feb. 2014
    ! 1. Extension/updates to AMSR2

    !print *,'Begin computeSIC'
    !print *,'sensor_id_npp,Sensor_id,SfcTyp=',sensor_id_npp,Sensor_id,SfcTyp
    IF( (Sensor_id .eq. sensor_id_n18 .or. Sensor_id .eq. sensor_id_n19) &
        .and. (SfcTyp .eq. SEAICE_TYP .or.  SfcTyp .eq. OC_TYP)) THEN
       if(nAngleStratification .eq. 1 .and. nLatStratification .eq. 1) then
          indexLow = 1
          indexHigh = nprofSeaIce
       endif

       if(nLatStratification .eq. 1 .and. nAngleStratification .gt. 1) then
          if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif

       if(nAngleStratification .eq. 1 .and. nLatStratification .gt. 1) then
          if(flat .gt. 0.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif
       if(nAngleStratification .gt. 1 .and. nLatStratification .gt. 1) then
          if (flat .gt. 0.0) then
             if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
                indexLow = 1
                indexHigh = nprofSeaIce / 4
             else
                indexLow = 1 + nprofSeaIce / 4
                indexHigh = nprofSeaIce / 2
             endif
          else
             if(abs(angle) .le. 25.0 .or. abs(angle) .gt. 50.0) then
               indexLow = 1 + nprofSeaIce /2
               indexHigh = 3 * nprofSeaIce / 4
             else
               indexLow = 1 + 3*nprofSeaIce / 4
               indexHigh = nprofSeaIce
             endif
          endif
       endif
       mindistance = 10000.0
       mindistance1 = 10000.0
       gradient31 = Emissivity(1) - Emissivity(2)
       gradient50 = Emissivity(1) - Emissivity(3)
       gradient89 = Emissivity(1) - Emissivity(16)
       gradient157 = Emissivity(1) - Emissivity(17)

       DO i=indexLow,indexHigh
           diff31 = ( gradient31 - (SeaIceData(i,1) - SeaIceData(i,2)) ) **2
           diff50 = ( gradient50 - (SeaIceData(i,1) - SeaIceData(i,3)) ) **2
           diff89 = ( gradient89 - (SeaIceData(i,1) - SeaIceData(i,16)) ) **2
           diff157 = ( gradient157 - (SeaIceData(i,1) - SeaIceData(i,17)) ) **2
           diff(1:nchan) =(SeaIceData(i,1:nchan) - Emissivity(1:nchan))**2
           if(gradient50 .ge. 0.045) then
               distance = sqrt( (diff31 + diff50) / 2.0)
           else
               distance    = sqrt((diff(1) + diff(2) + diff(3) + diff(16) + diff(17) ) / 5.0)
               distance1   = sqrt((diff31 + diff50 + diff89 + diff157) / 4.0)
           endif
           IF(distance .le. mindistance) THEN
               mindistance = distance
               c_total = 100.0*SeaIceData(i,nchan+1)
               c_fy = 100.0*SeaIceData(i,nchan+2)
               c_my = 100.0*SeaIceData(i,nchan+3)
           ENDIF
           IF(distance1 .le. mindistance1 .and. gradient50 .lt. 0.045) THEN
               mindistance1 = distance1
               c_total1 = 100.0*SeaIceData(i,nchan+1)
               c_fy1 = 100.0*SeaIceData(i,nchan+2)
               c_my1 = 100.0*SeaIceData(i,nchan+3)
           ENDIF
       ENDDO

       SIC = c_total
       SIC_FY = c_fy
       SIC_MY = c_my

       if(SIC .gt. SIC_HIGH) then
           if(c_total1 .gt. c_total .and. gradient50 .lt. 0.045) then
               SIC = c_total1
               SIC_FY = c_fy1
               SIC_MY = c_my1
           endif
       endif

       if (SIC .lt. SIC_LOW .and. SIC .gt. 0.0) then
           if (gradient31 .le. -0.04 .or. gradient50 .le. -0.1 .or. gradient89 .le. -0.19 ) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
           if(SIC .gt. 0.0) then
               if(c_total1 .lt. c_total .and. gradient50 .lt. 0.045) then
                   SIC = c_total1
                   SIC_FY = c_fy1
                   SIC_MY = c_my1
               endif
           endif
       endif
       if( (tskin .gt. TSK_HIGH) .or. (SIC .lt. SIC_MIN) .or. (abs(flat) .lt. LAT_ICE)  ) then
          if(SIC .lt. SIC_HIGH) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
          endif
       else if( SIC .lt. SIC_LOW .and. tskin .gt. TSK_LOW) then
           SIC = 0.0
           SIC_FY = 0.0
           SIC_MY = 0.0
       endif
    ENDIF
    
    IF (Sensor_id .eq. sensor_id_metopA .and. (SfcTyp .eq. SEAICE_TYP .or.  SfcTyp .eq. OC_TYP)) THEN
       if(nAngleStratification .eq. 1 .and. nLatStratification .eq. 1) then
          indexLow = 1
          indexHigh = nprofSeaIce
       endif

       if(nLatStratification .eq. 1 .and. nAngleStratification .gt. 1) then
          if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif

       if(nAngleStratification .eq. 1 .and. nLatStratification .gt. 1) then
          if(flat .gt. 0.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif
       if(nAngleStratification .gt. 1 .and. nLatStratification .gt. 1) then
          if (flat .gt. 0.0) then
             if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
                indexLow = 1
                indexHigh = nprofSeaIce / 4
             else
                indexLow = 1 + nprofSeaIce / 4
                indexHigh = nprofSeaIce / 2
             endif
          else
             if(abs(angle) .le. 25.0 .or. abs(angle) .gt. 50.0) then
               indexLow = 1 + nprofSeaIce /2
               indexHigh = 3 * nprofSeaIce / 4
             else
               indexLow = 1 + 3*nprofSeaIce / 4
               indexHigh = nprofSeaIce
             endif
          endif
       endif
       mindistance = 10000.0
       mindistance1 = 10000.0
       gradient31 = Emissivity(1) - Emissivity(2)
       gradient50 = Emissivity(1) - Emissivity(3)
       gradient89 = Emissivity(1) - Emissivity(16)
       gradient157 = Emissivity(1) - Emissivity(17)
       DO i=indexLow,indexHigh
           diff31 = ( gradient31 - (SeaIceData(i,1) - SeaIceData(i,2)) ) **2
           diff50 = ( gradient50 - (SeaIceData(i,1) - SeaIceData(i,3)) ) **2
           diff89 = ( gradient89 - (SeaIceData(i,1) - SeaIceData(i,16)) ) **2
           diff157 = ( gradient157 - (SeaIceData(i,1) - SeaIceData(i,17)) ) **2
           diff(1:nchan) =(SeaIceData(i,1:nchan) - Emissivity(1:nchan))**2
           if(gradient50 .ge. 0.045) then
               distance = sqrt( (diff31 + diff50) / 2.0)
           else
               distance    = sqrt((diff(1) + diff(2) + diff(3) + diff(16) + diff(17) ) / 5.0)
               distance1   = sqrt((diff31 + diff50 + diff89 + diff157) / 4.0)
           endif
           IF(distance .le. mindistance) THEN
               mindistance = distance
               c_total = 100.0*SeaIceData(i,nchan+1)
               c_fy = 100.0*SeaIceData(i,nchan+2)
               c_my = 100.0*SeaIceData(i,nchan+3)
           ENDIF
           IF(distance1 .le. mindistance1 .and. gradient50 .lt. 0.045) THEN
               mindistance1 = distance1
               c_total1 = 100.0*SeaIceData(i,nchan+1)
               c_fy1 = 100.0*SeaIceData(i,nchan+2)
               c_my1 = 100.0*SeaIceData(i,nchan+3)
           ENDIF
       ENDDO
       SIC = c_total
       SIC_FY = c_fy
       SIC_MY = c_my
       if(SIC .gt. SIC_HIGH) then 
           if(c_total1 .gt. c_total .and. gradient50 .lt. 0.045) then
               SIC = c_total1
               SIC_FY = c_fy1
               SIC_MY = c_my1
           endif
       endif
       if (SIC .lt. SIC_LOW .and. SIC .gt. 0.0) then
           if (gradient31 .le. -0.04 .or. gradient50 .le. -0.1 .or. gradient89 .le. -0.19 ) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
           if(SIC .gt. 0.0) then
               if(c_total1 .lt. c_total .and. gradient50 .lt. 0.045) then
                  SIC = c_total1
                  SIC_FY = c_fy1
                  SIC_MY = c_my1
               endif
           endif
       endif
       if( (tskin .gt. TSK_HIGH) .or. (SIC .lt. SIC_MIN) .or. (abs(flat) .lt. LAT_ICE)  ) then
           if(SIC .lt. SIC_HIGH) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
       else if( SIC .lt. SIC_LOW .and. tskin .gt. TSK_LOW) then
           SIC = 0.0
           SIC_FY = 0.0
           SIC_MY = 0.0
       endif
    ENDIF
    
    IF (Sensor_id .eq. sensor_id_metopB .and. (SfcTyp .eq. SEAICE_TYP .or.  SfcTyp .eq. OC_TYP)) THEN
       if(nAngleStratification .eq. 1 .and. nLatStratification .eq. 1) then
          indexLow = 1
          indexHigh = nprofSeaIce
       endif

       if(nLatStratification .eq. 1 .and. nAngleStratification .gt. 1) then
          if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif

       if(nAngleStratification .eq. 1 .and. nLatStratification .gt. 1) then
          if(flat .gt. 0.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif
       if(nAngleStratification .gt. 1 .and. nLatStratification .gt. 1) then
          if (flat .gt. 0.0) then
             if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
                indexLow = 1
                indexHigh = nprofSeaIce / 4
             else
                indexLow = 1 + nprofSeaIce / 4
                indexHigh = nprofSeaIce / 2
             endif
          else
             if(abs(angle) .le. 25.0 .or. abs(angle) .gt. 50.0) then
               indexLow = 1 + nprofSeaIce /2
               indexHigh = 3 * nprofSeaIce / 4
             else
               indexLow = 1 + 3*nprofSeaIce / 4
               indexHigh = nprofSeaIce
             endif
          endif
       endif
       mindistance = 10000.0
       mindistance1 = 10000.0
       gradient31 = Emissivity(1) - Emissivity(2)
       gradient50 = Emissivity(1) - Emissivity(3)
       gradient89 = Emissivity(1) - Emissivity(16)
       gradient157 = Emissivity(1) - Emissivity(17)
       DO i=indexLow,indexHigh
           diff31 = ( gradient31 - (SeaIceData(i,1) - SeaIceData(i,2)) ) **2
           diff50 = ( gradient50 - (SeaIceData(i,1) - SeaIceData(i,3)) ) **2
           diff89 = ( gradient89 - (SeaIceData(i,1) - SeaIceData(i,16)) ) **2
           diff157 = ( gradient157 - (SeaIceData(i,1) - SeaIceData(i,17)) ) **2
           diff(1:nchan) =(SeaIceData(i,1:nchan) - Emissivity(1:nchan))**2
           if(gradient50 .ge. 0.045) then
               distance = sqrt( (diff31 + diff50) / 2.0)
           else
               distance    = sqrt((diff(1) + diff(2) + diff(3) + diff(16) + diff(17) ) / 5.0)
               distance1   = sqrt((diff31 + diff50 + diff89 + diff157) / 4.0)
           endif
           IF(distance .le. mindistance) THEN
               mindistance = distance
               c_total = 100.0*SeaIceData(i,nchan+1)
               c_fy = 100.0*SeaIceData(i,nchan+2)
               c_my = 100.0*SeaIceData(i,nchan+3)
           ENDIF
           IF(distance1 .le. mindistance1 .and. gradient50 .lt. 0.045) THEN
               mindistance1 = distance1
               c_total1 = 100.0*SeaIceData(i,nchan+1)
               c_fy1 = 100.0*SeaIceData(i,nchan+2)
               c_my1 = 100.0*SeaIceData(i,nchan+3)
           ENDIF
       ENDDO
       SIC = c_total
       SIC_FY = c_fy
       SIC_MY = c_my
       if(SIC .gt. SIC_HIGH) then 
           if(c_total1 .gt. c_total .and. gradient50 .lt. 0.045) then
               SIC = c_total1
               SIC_FY = c_fy1
               SIC_MY = c_my1
           endif
       endif
       if (SIC .lt. SIC_LOW .and. SIC .gt. 0.0) then
           if (gradient31 .le. -0.04 .or. gradient50 .le. -0.1 .or. gradient89 .le. -0.19 ) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
           if(SIC .gt. 0.0) then
               if(c_total1 .lt. c_total .and. gradient50 .lt. 0.045) then
                  SIC = c_total1
                  SIC_FY = c_fy1
                  SIC_MY = c_my1
               endif
           endif
       endif
       if( (tskin .gt. TSK_HIGH) .or. (SIC .lt. SIC_MIN) .or. (abs(flat) .lt. LAT_ICE)  ) then
           if(SIC .lt. SIC_HIGH) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
       else if( SIC .lt. SIC_LOW .and. tskin .gt. TSK_LOW) then
           SIC = 0.0
           SIC_FY = 0.0
           SIC_MY = 0.0
       endif
    ENDIF
    
    ! Modified SIC algorithm for F16/F17/F18 SSMIS
    ! C. Grassotti, Sept. 2010

    ! Modifications in F16/F17/F18 processing related to:
    ! 1. threshold of 19V-37V gradient (GR1937V_SICMY = 0.02) determining whether we are in the predominantly "MY ice" regime
    !    or on the "FY Ice/ocean regime: threshold lowered from 0.05 to 0.02
    !
    ! 2. When in MY ice regime (19V-37V gradient .ge. GR1937V_SICMY), penalty term (distance2, fit vs. catalog) changed to:
    !    distance2 = sqrt ( ( (GR19_37V - GR19_37V_cat) **2  + (GR19_37H - GR19_37H_cat) **2) /1.0)
    !    Previous version used:
    !    distance2 = sqrt ( ( (GR19_37V - GR19_37V_cat) **2 + (PR19 - PR19_cat) **2 + (PR37 - PR37_cat) **2) /1.0)
    !    This change results in slightly more MY vs. FY ice in mid-late winter (Feb-March 2010) in regions where MY ice expected
    !    to dominate (areas north of Canadian archipelago and Greenland).
    !
    ! 3. Additional penalty terms for total SIC (distance3, distance4) which constrain the total ice amount
    !    These penalty terms are identical to the previous version and also depend on a critical threshold
    !    of the 19V-37V gradient (GR1937V_SICTOT = 0.05)
    !
    !    When 19V-37V gradient .ge. GR1937V_SICTOT use:
    !    distance3 = sqrt ( ( (GR19_37V - GR19_37V_cat) **2 + (PR19 - PR19_cat) **2 + (PR37 - PR37_cat) **2) /1.0)
    !
    !    When 19V-37V gradient .lt. GR1937V_SICTOT use:
    !    distance4 = sqrt( diff(13) + diff(14) + diff(16) + diff(17) )
    !    
    ! 4. threshold for applying distance3 or distance4 total SIC added (GR1937V_SICTOT), and set to 0.05
    !
    ! Therefore, total ice amount retrieved using distance4 (FY/Ocean regime) or distance3 (MY regime), 
    ! depending on gradient, while MY ice is retrieved using either distance and distance1 (FY/Ocean regime), 
    ! or distance2 (MY regime). NB: FY ice is computed as the residual between SIC total and SIC MY.
    ! Result is better consistency between f16/f18 retrievals and n18/n19/metop

    ! Updated: to address overestimation of FY ice vs. MY ice, especially in spring/summer (e.g. 2011-06-26)
    ! Updated: threshold for FY/Ocean and MY ice regime lowered to 0.01
    ! Updated: MY ice always retrieved using distance1 under FY/Ocean regime)

   IF( (Sensor_id .eq. sensor_id_f16 .or. Sensor_id .eq. sensor_id_f17 .or. Sensor_id .eq. sensor_id_f18) &
       .and. (SfcTyp .eq. SEAICE_TYP .or.  SfcTyp .eq. OC_TYP) ) THEN
       if( nLatStratification .eq. 1) then
          indexLow = 1
          indexHigh = nprofSeaIce
       endif
       if(nLatStratification .gt. 1) then
          if(flat .gt. 0.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif
       mindistance = 10000.0
       mindistance1 = 10000.0
       mindistance2 = 10000.0
       mindistance3 = 10000.0
       mindistance4 = 10000.0

       PR19  =  (Emissivity(13) - Emissivity(12)) / (Emissivity(13) + Emissivity(12)) 
       PR37  =  (Emissivity(16) - Emissivity(15)) / (Emissivity(16) + Emissivity(15))
       PR89  =  (Emissivity(17) - Emissivity(18))   /(Emissivity(17) + Emissivity(18)) 
       GR19_37V = (Emissivity(13) - Emissivity(16))  / (Emissivity(13) + Emissivity(16))
       GR19_37H = (Emissivity(12) - Emissivity(15))  / (Emissivity(12) + Emissivity(15))
       GR19_89V = (Emissivity(13) - Emissivity(17)) / (Emissivity(13) + Emissivity(17))
       GR19_89H = (Emissivity(12) - Emissivity(18)) / (Emissivity(12) + Emissivity(18))
       GR37_89H = (Emissivity(15) - emissivity(18)) / (Emissivity(15) + emissivity(18))
       GR37_89V = (Emissivity(16) - emissivity(17)) / (Emissivity(16) + emissivity(17))


       DO i=indexLow,indexHigh
          diff(1:nchan) =( (SeaIceData(i,1:nchan) - Emissivity(1:nchan))  & 
                           / (SeaIceData(i,1:nchan) + Emissivity(1:nchan)) )**2
          PR19_cat = (SeaIceData(i,13) - SeaIceData(i,12)) / (SeaIceData(i,13) + SeaIceData(i,12))
          PR37_cat = (SeaIceData(i,16) - SeaIceData(i,15)) / (SeaIceData(i,16) + SeaIceData(i,15))
          PR89_cat = (SeaIceData(i,17) - SeaIceData(i,18)) / (SeaIceData(i,17) + SeaIceData(i,18))
          GR19_37V_cat = (SeaIceData(i,13) - SeaIceData(i,16))/ (SeaIceData(i,13) + SeaIceData(i,16))
          GR19_37H_cat = (SeaIceData(i,12) - SeaIceData(i,15))/ (SeaIceData(i,12) + SeaIceData(i,15))
          GR19_89V_cat = (SeaIceData(i,13) - SeaIceData(i,17))/ (SeaIceData(i,13) + SeaIceData(i,17))
          GR19_89H_cat = (SeaIceData(i,12) - SeaIceData(i,18))/ (SeaIceData(i,12) + SeaIceData(i,18))
          GR37_89H_cat = (SeaIceData(i,15) - SeaIceData(i,18))/ (SeaIceData(i,15) + SeaIceData(i,18))
          GR37_89V_cat = (SeaIceData(i,16) - SeaIceData(i,17))/ (SeaIceData(i,16) + SeaIceData(i,17))

          ! constraint on total ice
          distance4 = sqrt( diff(13) + diff(14) + diff(16) + diff(17) ) ! orig
          distance3 = sqrt ( ( (GR19_37V - GR19_37V_cat) **2 + (PR19 - PR19_cat) **2 +  &
                      (PR37 - PR37_cat) **2) /1.0)  ! orig

          IF(distance3  .le. mindistance3 ) THEN
             mindistance3 = distance3
             c_total3 = 100.0*SeaIceData(i,nchan+1)
             c_fy3 = 100.0*SeaIceData(i,nchan+2)
             c_my3 = 100.0*SeaIceData(i,nchan+3)
          ENDIF
          IF(distance4  .le. mindistance4 ) THEN
             mindistance4 = distance4
             c_total4 = 100.0*SeaIceData(i,nchan+1)
             c_fy4 = 100.0*SeaIceData(i,nchan+2)
             c_my4 = 100.0*SeaIceData(i,nchan+3)
          ENDIF


       ENDDO

       DO i=indexLow,indexHigh
          diff(1:nchan) =( (SeaIceData(i,1:nchan) - Emissivity(1:nchan))  & 
                           / (SeaIceData(i,1:nchan) + Emissivity(1:nchan)) )**2
          PR19_cat = (SeaIceData(i,13) - SeaIceData(i,12)) / (SeaIceData(i,13) + SeaIceData(i,12))
          PR37_cat = (SeaIceData(i,16) - SeaIceData(i,15)) / (SeaIceData(i,16) + SeaIceData(i,15))
          PR89_cat = (SeaIceData(i,17) - SeaIceData(i,18)) / (SeaIceData(i,17) + SeaIceData(i,18))
          GR19_37V_cat = (SeaIceData(i,13) - SeaIceData(i,16))/ (SeaIceData(i,13) + SeaIceData(i,16))
          GR19_37H_cat = (SeaIceData(i,12) - SeaIceData(i,15))/ (SeaIceData(i,12) + SeaIceData(i,15))
          GR19_89V_cat = (SeaIceData(i,13) - SeaIceData(i,17))/ (SeaIceData(i,13) + SeaIceData(i,17))
          GR19_89H_cat = (SeaIceData(i,12) - SeaIceData(i,18))/ (SeaIceData(i,12) + SeaIceData(i,18))
          GR37_89H_cat = (SeaIceData(i,15) - SeaIceData(i,18))/ (SeaIceData(i,15) + SeaIceData(i,18))

          ! If 19-37V gradient less than threshold for FY/MY ice regime (FY dominant) use penalty term from distance end distance1
          if(GR19_37V .lt. GR1937V_SICMY) then
             distance = sqrt( diff(13) + diff(14) + diff(16) + diff(17) ) ! orig
             distance1 = sqrt( (GR19_89V - GR19_89V_cat) ** 2 + (GR37_89H - GR37_89H_cat) ** 2 ) ! better results for MY?
!             distance1 = sqrt( (GR19_37H - GR19_37H_cat) ** 2 + (GR19_89V - GR19_89V_cat) ** 2 + &
!            (GR37_89H - GR37_89H_cat) ** 2 ) ! same results as above

          ! Otherwise, if 19-37V gradient exceeds threshold for FY/MY ice regime (MY dominant) use penalty term from distance2
          else
             distance2 = sqrt ( ( (GR19_37H - GR19_37H_cat) **2 ) /1.0)
          endif
          
          ! If 19-37V gradient less than threshold for FY/MY ice regime (FY dominant) use estimate based on distance and distance1
          ! distance: used to set baseline ice amounts for scenes where total estimated SIC > SIC_LOW (i.e. higher amounts)
          !           based on differences in absolute magnitudes with catalog values
          ! distance1: used to set baseline ice amounts for scenes where total SIC < SIC_LOW (i.e. lower amounts, esp near ice edge)
          !           based on differences in relative gradients with catalog values
          IF(distance  .le. mindistance .and. GR19_37V .lt. GR1937V_SICMY) THEN
             mindistance = distance
             c_total = 100.0*SeaIceData(i,nchan+1)
             c_my = 100.0*SeaIceData(i,nchan+3)
             c_fy = c_total-c_my ! FY ice calculated as residual between total and MY
          ENDIF
          IF(distance1  .le. mindistance1 .and. GR19_37V .lt. GR1937V_SICMY) THEN
             mindistance1 = distance1
             c_total1 = 100.0*SeaIceData(i,nchan+1)
             c_my1 = 100.0*SeaIceData(i,nchan+3)
             c_fy1 = c_total1-c_my1 ! FY ice calculated as residual between total and MY
          ENDIF
          ! Otherwise, if 19-37V gradient exceeds threshold for FY/MY ice regime (MY dominant) use estimate based on distance2
          ! distance2: used to set baseline ice amounts 
          !           based on differences in relative gradients with catalog values
          IF(distance2  .le. mindistance2 .and. GR19_37V .ge. GR1937V_SICMY) THEN
             mindistance2 = distance2
              if(GR19_37V .ge. GR1937V_SICTOT)then
                c_total2 = 100.0*SeaIceData(i,nchan+1)
                c_fy2 = 100.0*SeaIceData(i,nchan+2)
                c_my2 = 100.0*SeaIceData(i,nchan+3)
             else
                c_total2 = 100.0*SeaIceData(i,nchan+1)
                c_fy2 = 100.0*SeaIceData(i,nchan+2)
                c_my2 = 100.0*SeaIceData(i,nchan+3)
             endif
          ENDIF
       ENDDO

       ! Assign final values to output variables
       ! MY Ice dominant
       if(GR19_37V .ge. GR1937V_SICMY) then
          SIC = c_total2
          SIC_FY = c_fy2
          SIC_MY = c_my2
       ! FY Ice dominant
       else
          SIC = c_total 
          SIC_FY = c_fy
          SIC_MY = c_my
          SIC_FY = SIC-SIC_MY ! FY ice calculated as residual between total and MY
          ! for low ice total amounts use different baseline values computed above
          if(SIC .lt. SIC_LOW .and. SIC .gt. 0.0 .and. GR19_37V .lt. GR1937V_SICMY) then
                SIC = c_total1
                SIC_FY = c_fy1
                SIC_MY = c_my1
          endif
       endif
       if( (tskin .gt. TSK_HIGH) .or. (SIC .lt. SIC_MIN) .or. (abs(flat) .lt. LAT_ICE)  ) then
          if(SIC .lt. SIC_HIGH) then
             SIC = 0.0
             SIC_FY = 0.0
             SIC_MY = 0.0
          endif
       else if( SIC .lt. SIC_LOW .and. tskin .gt. TSK_LOW) then
          SIC = 0.0
          SIC_FY = 0.0
          SIC_MY = 0.0
       endif
!! testing: fill SIC with PR and GR variables for plotting
!       SIC = PR19
!       SIC_FY = PR37
!       SIC_MY = PR89

!       SIC = GR19_89V
!       SIC_FY = GR19_89H
!       SIC_MY = GR37_89H

!       SIC = GR19_37V
!       SIC_FY = GR19_37H
!       SIC_MY = mindistance


    ENDIF

   !---- Add branch for AMSRE and AMSR2 (use similar algorithm to F16/F18 SSMIS)
   IF( ( Sensor_id .eq. sensor_id_amsre .or. Sensor_id .eq. sensor_id_gcomw1 ) .and. &
       ( SfcTyp .eq. SEAICE_TYP .or.  SfcTyp .eq. OC_TYP ) ) THEN
      if(Sensor_id .eq. sensor_id_amsre)then
         offset_amsr=0
      else
         offset_amsr=2 ! needed to account for 2 additional channels at 6.9 GHz on AMSR2
      endif
       if( nLatStratification .eq. 1) then
          indexLow = 1
          indexHigh = nprofSeaIce
       endif
       if(nLatStratification .gt. 1) then
          if(flat .gt. 0.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif
       mindistance = 10000.0
       mindistance1 = 10000.0
       mindistance2 = 10000.0
       mindistance3 = 10000.0
       mindistance4 = 10000.0
       PR19  =  (Emissivity(5+offset_amsr) - Emissivity(6+offset_amsr))   / &
            (Emissivity(5+offset_amsr) + Emissivity(6+offset_amsr)) 
       PR37  =  (Emissivity(9+offset_amsr) - Emissivity(10+offset_amsr))  / &
            (Emissivity(9+offset_amsr) + Emissivity(10+offset_amsr))
       PR89  =  (Emissivity(11+offset_amsr) - Emissivity(12+offset_amsr)) / &
            (Emissivity(11+offset_amsr) + Emissivity(12+offset_amsr)) 
       GR19_37V = (Emissivity(5+offset_amsr) - Emissivity(9+offset_amsr)) / &
            (Emissivity(5+offset_amsr) + Emissivity(9+offset_amsr))
       GR19_37H = (Emissivity(6+offset_amsr) - Emissivity(10+offset_amsr))/ &
            (Emissivity(6+offset_amsr) + Emissivity(10+offset_amsr))
       GR19_89V = (Emissivity(5+offset_amsr) - Emissivity(11+offset_amsr))/ &
            (Emissivity(5+offset_amsr) + Emissivity(11+offset_amsr))
       GR19_89H = (Emissivity(6+offset_amsr) - Emissivity(12+offset_amsr))/ &
            (Emissivity(6+offset_amsr) + Emissivity(12+offset_amsr))
       GR37_89H = (Emissivity(10+offset_amsr) - Emissivity(12+offset_amsr))/& 
            (Emissivity(10+offset_amsr) + Emissivity(12+offset_amsr))
       GR37_89V = (Emissivity(9+offset_amsr) - emissivity(11+offset_amsr)) /&
            (Emissivity(9+offset_amsr) + Emissivity(11+offset_amsr))


       DO i=indexLow,indexHigh
          diff_amsre(13) = ( (SeaIceData(i,13) - Emissivity(5+offset_amsr)) / &
               (SeaIceData(i,13) + Emissivity(5+offset_amsr)) )**2
          diff_amsre(14) = ( (SeaIceData(i,14) - Emissivity(7+offset_amsr)) / &
               (SeaIceData(i,14) + Emissivity(7+offset_amsr)) )**2
          diff_amsre(16) = ( (SeaIceData(i,16) - Emissivity(9+offset_amsr)) / &
               (SeaIceData(i,16) + Emissivity(9+offset_amsr)) )**2
          diff_amsre(17) = ( (SeaIceData(i,17) - Emissivity(11+offset_amsr))/ &
               (SeaIceData(i,17) + Emissivity(11+offset_amsr)) )**2

          PR19_cat = (SeaIceData(i,13) - SeaIceData(i,12)) / (SeaIceData(i,13) + SeaIceData(i,12))
          PR37_cat = (SeaIceData(i,16) - SeaIceData(i,15)) / (SeaIceData(i,16) + SeaIceData(i,15))
          PR89_cat = (SeaIceData(i,17) - SeaIceData(i,18)) / (SeaIceData(i,17) + SeaIceData(i,18))
          GR19_37V_cat = (SeaIceData(i,13) - SeaIceData(i,16))/ (SeaIceData(i,13) + SeaIceData(i,16))
          GR19_37H_cat = (SeaIceData(i,12) - SeaIceData(i,15))/ (SeaIceData(i,12) + SeaIceData(i,15))
          GR19_89V_cat = (SeaIceData(i,13) - SeaIceData(i,17))/ (SeaIceData(i,13) + SeaIceData(i,17))
          GR19_89H_cat = (SeaIceData(i,12) - SeaIceData(i,18))/ (SeaIceData(i,12) + SeaIceData(i,18))
          GR37_89H_cat = (SeaIceData(i,15) - SeaIceData(i,18))/ (SeaIceData(i,15) + SeaIceData(i,18))
          GR37_89V_cat = (SeaIceData(i,16) - SeaIceData(i,17))/ (SeaIceData(i,16) + SeaIceData(i,17))
          

          distance4 = sqrt( diff_amsre(13) + diff_amsre(14) + diff_amsre(16) + diff_amsre(17) ) ! orig
          distance3 = sqrt ( ( (GR19_37V - GR19_37V_cat) **2 + (PR19 - PR19_cat) **2 +  &
                      (PR37 - PR37_cat) **2) /1.0)  ! orig

          IF(distance3  .le. mindistance3 ) THEN
             mindistance3 = distance3
             c_total3 = 100.0*SeaIceData(i,nchan_ssmis+1)
             c_fy3 = 100.0*SeaIceData(i,nchan_ssmis+2)
             c_my3 = 100.0*SeaIceData(i,nchan_ssmis+3)
          ENDIF
          IF(distance4  .le. mindistance4 ) THEN
             mindistance4 = distance4
             c_total4 = 100.0*SeaIceData(i,nchan_ssmis+1)
             c_fy4 = 100.0*SeaIceData(i,nchan_ssmis+2)
             c_my4 = 100.0*SeaIceData(i,nchan_ssmis+3)
          ENDIF


       ENDDO

       DO i=indexLow,indexHigh
          diff_amsre(13) = ( (SeaIceData(i,13) - Emissivity(5+offset_amsr)) / &
               (SeaIceData(i,13) + Emissivity(5+offset_amsr)) )**2
          diff_amsre(14) = ( (SeaIceData(i,14) - Emissivity(7+offset_amsr)) / &
               (SeaIceData(i,14) + Emissivity(7+offset_amsr)) )**2
          diff_amsre(16) = ( (SeaIceData(i,16) - Emissivity(9+offset_amsr)) / &
               (SeaIceData(i,16) + Emissivity(9+offset_amsr)) )**2
          diff_amsre(17) = ( (SeaIceData(i,17) - Emissivity(11+offset_amsr))/ &
               (SeaIceData(i,17) + Emissivity(11+offset_amsr)) )**2

          PR19_cat = (SeaIceData(i,13) - SeaIceData(i,12)) / (SeaIceData(i,13) + SeaIceData(i,12))
          PR37_cat = (SeaIceData(i,16) - SeaIceData(i,15)) / (SeaIceData(i,16) + SeaIceData(i,15))
          PR89_cat = (SeaIceData(i,17) - SeaIceData(i,18)) / (SeaIceData(i,17) + SeaIceData(i,18))
          GR19_37V_cat = (SeaIceData(i,13) - SeaIceData(i,16))/ (SeaIceData(i,13) + SeaIceData(i,16))
          GR19_37H_cat = (SeaIceData(i,12) - SeaIceData(i,15))/ (SeaIceData(i,12) + SeaIceData(i,15))
          GR19_89V_cat = (SeaIceData(i,13) - SeaIceData(i,17))/ (SeaIceData(i,13) + SeaIceData(i,17))
          GR19_89H_cat = (SeaIceData(i,12) - SeaIceData(i,18))/ (SeaIceData(i,12) + SeaIceData(i,18))
          GR37_89H_cat = (SeaIceData(i,15) - SeaIceData(i,18))/ (SeaIceData(i,15) + SeaIceData(i,18))


          ! If 19-37V gradient less than threshold for FY/MY ice regime (FY dominant) use penalty term for distance end distance1
          if(GR19_37V .lt. GR1937V_SICMY) then
             distance = sqrt( diff_amsre(13) + diff_amsre(14) + diff_amsre(16) + diff_amsre(17) ) ! orig
             distance1 = sqrt( (GR19_89V - GR19_89V_cat) ** 2 + (GR37_89H - GR37_89H_cat) ** 2 ) ! better results for MY?
          ! Otherwise, if 19-37V gradient exceeds threshold for FY/MY ice regime (MY dominant) use penalty term from distance2
          else
             distance2 = sqrt ( ( (GR19_37H - GR19_37H_cat) **2 ) /1.0)
          endif
          
          ! If 19-37V gradient less than threshold for FY/MY ice regime (FY dominant) use estimate based on distance and distance1
          IF(distance  .le. mindistance .and. GR19_37V .lt. GR1937V_SICMY) THEN
             mindistance = distance
             c_total = 100.0*SeaIceData(i,nchan_ssmis+1) ! orig
             c_my = 100.0*SeaIceData(i,nchan_ssmis+3) ! orig
             c_fy = c_total-c_my
          ENDIF
          IF(distance1  .le. mindistance1 .and. GR19_37V .lt. GR1937V_SICMY) THEN
             mindistance1 = distance1
             c_total1 = 100.0*SeaIceData(i,nchan_ssmis+1)
             c_my1 = 100.0*SeaIceData(i,nchan_ssmis+3)
             c_fy1 = c_total1-c_my1
          ENDIF

          ! Otherwise, if 19-37V gradient exceeds threshold for FY/MY ice regime (MY dominant) use estimate based on distance2
          IF(distance2  .le. mindistance2 .and. GR19_37V .ge. GR1937V_SICMY) THEN
             mindistance2 = distance2
             ! Total ice concentration constraint: FY Ice computed as residual of Total SIC-MY SIC
             ! If 19-37V gradient exceeds threshold for total ice regime (MY dominant) use estimate from distance3
             if(GR19_37V .ge. GR1937V_SICTOT)then
                c_total2 = 100.0*SeaIceData(i,nchan_ssmis+1) ! orig
                c_fy2 = 100.0*SeaIceData(i,nchan_ssmis+2) ! orig
                c_my2 = 100.0*SeaIceData(i,nchan_ssmis+3) ! orig
             ! Otherwise, if 19-37V gradient less than threshold for total ice regime (FY dominant) use estimate from distance4
             else
                c_total2 = 100.0*SeaIceData(i,nchan_ssmis+1) ! orig
                c_fy2 = 100.0*SeaIceData(i,nchan_ssmis+2) ! orig
                c_my2 = 100.0*SeaIceData(i,nchan_ssmis+3) ! orig
            endif
          ENDIF

       ENDDO


       ! Assign final values to output variables
       ! MY Ice dominant    
       if(GR19_37V .ge. GR1937V_SICMY) then
          SIC = c_total2
          SIC_FY = c_fy2
          SIC_MY = c_my2
       ! FY Ice dominant
       else
          SIC = c_total ! orig
          SIC_FY = c_fy ! orig
          SIC_MY = c_my ! orig
          SIC_FY = SIC-SIC_MY ! new: FY ice calculated as residual between total and MY
          if(SIC .lt. SIC_LOW .and. SIC .gt. 0.0 .and. GR19_37V .lt. GR1937V_SICMY) then
               SIC = c_total1
               SIC_FY = c_fy1
               SIC_MY = c_my1
          endif
       endif
       if( (tskin .gt. TSK_HIGH) .or. (SIC .lt. SIC_MIN) .or. (abs(flat) .lt. LAT_ICE)  ) then
          if(SIC .lt. SIC_HIGH) then
             SIC = 0.0
             SIC_FY = 0.0
             SIC_MY = 0.0
          endif
       else if( SIC .lt. SIC_LOW .and. tskin .gt. TSK_LOW) then
          SIC = 0.0
          SIC_FY = 0.0
          SIC_MY = 0.0
       endif
!! testing: fill SIC with PR and GR variables for plotting
!       SIC = PR19
!       SIC_FY = PR37
!       SIC_MY = PR89

!       SIC = GR19_89V
!       SIC_FY = GR19_89H
!       SIC_MY = GR37_89H

!       SIC = GR19_37V
!       SIC_FY = GR19_37H
!       SIC_MY = mindistance
       
!       print *,'SIC,SIC_FY,SIC_MY: ',SIC,SIC_FY,SIC_MY


    ENDIF

    IF (Sensor_id .eq. sensor_id_npp  .and. (SfcTyp .eq. SEAICE_TYP .or.  SfcTyp .eq. OC_TYP)) THEN

       if(nAngleStratification .eq. 1 .and. nLatStratification .eq. 1) then
          indexLow = 1
          indexHigh = nprofSeaIce
       endif

       if(nLatStratification .eq. 1 .and. nAngleStratification .gt. 1) then
          if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif

       if(nAngleStratification .eq. 1 .and. nLatStratification .gt. 1) then
          if(flat .gt. 0.0) then
             indexLow = 1
             indexHigh = nprofSeaIce / 2
          else
             indexLow = 1 + nprofSeaIce / 2
             indexHigh = nprofSeaIce
          endif
       endif
       if(nAngleStratification .gt. 1 .and. nLatStratification .gt. 1) then
          if (flat .gt. 0.0) then
             if(abs(angle) .gt. 25.0 .and. abs(angle) .lt. 50.0) then
                indexLow = 1
                indexHigh = nprofSeaIce / 4
             else
                indexLow = 1 + nprofSeaIce / 4
                indexHigh = nprofSeaIce / 2
             endif
          else
             if(abs(angle) .le. 25.0 .or. abs(angle) .gt. 50.0) then
               indexLow = 1 + nprofSeaIce /2
               indexHigh = 3 * nprofSeaIce / 4
             else
               indexLow = 1 + 3*nprofSeaIce / 4
               indexHigh = nprofSeaIce
             endif
          endif
       endif
       mindistance = 10000.0
       mindistance1 = 10000.0
       gradient31 = Emissivity(1) - Emissivity(2)
       gradient50 = Emissivity(1) - Emissivity(3)
       gradient89 = Emissivity(1) - Emissivity(16)
       gradient157 = Emissivity(1) - Emissivity(17)
       !print *,'gradient31,gradient50,gradient89,gradient157=',gradient31,gradient50,gradient89,gradient157
       DO i=indexLow,indexHigh
           diff31 = ( gradient31 - (SeaIceData(i,1) - SeaIceData(i,2)) ) **2
           diff50 = ( gradient50 - (SeaIceData(i,1) - SeaIceData(i,3)) ) **2
           diff89 = ( gradient89 - (SeaIceData(i,1) - SeaIceData(i,16)) ) **2
           diff157 = ( gradient157 - (SeaIceData(i,1) - SeaIceData(i,17)) ) **2
!           diff(1:nchan-) =(SeaIceData(i,1:nchan) - Emissivity(1:nchan))**2
           diff(1:nparmSeaice-3) =(SeaIceData(i,1:nparmSeaice-3) - Emissivity(1:nparmSeaice-3))**2

           if(gradient50 .ge. 0.045) then
               distance = sqrt( (diff31 + diff50) / 2.0)
           else
               distance    = sqrt((diff(1) + diff(2) + diff(3) + diff(16) + diff(17) ) / 5.0)
               distance1   = sqrt((diff31 + diff50 + diff89 + diff157) / 4.0)
           endif
           IF(distance .le. mindistance) THEN
               mindistance = distance
!               c_total = 100.0*SeaIceData(i,nchan+1)
!               c_fy = 100.0*SeaIceData(i,nchan+2)
!               c_my = 100.0*SeaIceData(i,nchan+3)
               c_total = 100.0*SeaIceData(i,nparmSeaice-2)
               c_fy = 100.0*SeaIceData(i,nparmSeaice-1)
               c_my = 100.0*SeaIceData(i,nparmSeaice)
           ENDIF
           IF(distance1 .le. mindistance1 .and. gradient50 .lt. 0.045) THEN
               mindistance1 = distance1
!               c_total1 = 100.0*SeaIceData(i,nchan+1)
!               c_fy1 = 100.0*SeaIceData(i,nchan+2)
!               c_my1 = 100.0*SeaIceData(i,nchan+3)
               c_total1 = 100.0*SeaIceData(i,nparmSeaice-2)
               c_fy1 = 100.0*SeaIceData(i,nparmSeaice-1)
               c_my1 = 100.0*SeaIceData(i,nparmSeaice)
           ENDIF
       ENDDO
       SIC = c_total
       SIC_FY = c_fy
       SIC_MY = c_my
       if(SIC .gt. SIC_HIGH) then 
           if(c_total1 .gt. c_total .and. gradient50 .lt. 0.045) then
               SIC = c_total1
               SIC_FY = c_fy1
               SIC_MY = c_my1
           endif
       endif
       if (SIC .lt. SIC_LOW .and. SIC .gt. 0.0) then
           if (gradient31 .le. -0.04 .or. gradient50 .le. -0.1 .or. gradient89 .le. -0.19 ) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
           if(SIC .gt. 0.0) then
               if(c_total1 .lt. c_total .and. gradient50 .lt. 0.045) then
                  SIC = c_total1
                  SIC_FY = c_fy1
                  SIC_MY = c_my1
               endif
           endif
       endif
       if( (tskin .gt. TSK_HIGH) .or. (SIC .lt. SIC_MIN) .or. (abs(flat) .lt. LAT_ICE)  ) then
           if(SIC .lt. SIC_HIGH) then
               SIC = 0.0
               SIC_FY = 0.0
               SIC_MY = 0.0
           endif
       else if( SIC .lt. SIC_LOW .and. tskin .gt. TSK_LOW) then
           SIC = 0.0
           SIC_FY = 0.0
           SIC_MY = 0.0
       endif
       !print *,'SIC,SIC_FY,SIC_MY=',SIC,SIC_FY,SIC_MY
    ENDIF 
    !print *,'End computeSIC'
   

  END SUBROUTINE ComputeSIC

  FUNCTION ComputeSfcTyp(SIC,SWE)
    IMPLICIT NONE
    INTEGER    ::ComputeSfcTyp
    REAL       ::SWE,SIC
    IF (SWE .gt. epsilon) THEN 
       ComputeSfcTyp = SNOW_TYP
    ELSE IF ( ABS(SWE) .le. epsilon) THEN 
       ComputeSfcTyp = LD_TYP
    ELSE IF (SIC .gt. epsilon) THEN 
       ComputeSfcTyp = SEAICE_TYP
    ELSE IF (ABS(SIC) .le. epsilon) THEN 
       ComputeSfcTyp = OC_TYP
    ENDIF
   
END FUNCTION ComputeSfcTyp


!===============================================================
! Name:         ComputeGEOTHK
!
!
! Type:         Subroutine
!
!
! Description:  Calculates the Geopotential Thickness (km) from
!               pressure close to the surface pressure to 500 mb
!              
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nLay              I              Number of Layers 
!       - Pres_lay          I              Layer pressure grid
!       - Temp_lay          I              Layer temperature profile
!       - Q_lay             I              Layer water vapor profile
!       - QC                I              QC vector
!       - GTHK              0              Geopotential Thickness
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       09-29-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
 SUBROUTINE ComputeGEOTHK(nLay,Pres_lay,Temp_Lay,Q_Lay,SfcPress,QC,GTHK)
   !---Input/Output variables  
   INTEGER, INTENT(IN)                  :: nLay
   INTEGER(2), DIMENSION(:), INTENT(IN) :: QC         
   REAL, DIMENSION(:), INTENT(IN)       :: Pres_lay,Temp_lay,Q_lay 
   REAL,INTENT(IN)                      :: SfcPress
   REAL                                 :: GTHK
   !---Local variables
   INTEGER                  :: idxpsfc,idxp500,iLay
   REAL                     :: Tvt
   REAL, PARAMETER          :: RO=287.053,g=9.80665,w=0.61
   GTHK=-999.0
   idxpsfc=0
   idxp500=76
   DO iLay=1,nLay
      IF(Pres_lay(iLay) .LE. SfcPress) idxpsfc=idxpsfc+1
   ENDDO
   IF((QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      Tvt=Temp_lay(idxpsfc)*(1+w*(Q_lay(idxpsfc)/1000.0))
      GTHK=((RO*Tvt)/g)*(LOG(Pres_lay(idxpsfc)/Pres_lay(idxp500)))*0.001
   ENDIF
   IF(GTHK .LT. 0) GTHK=-999.0
 END SUBROUTINE ComputeGEOTHK


!===============================================================
! Name:         ComputeFLVH
!
!
! Type:         Subroutine
!
!
! Description:  Calculates the Freezing Level Height (km).
!              
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nLay              I              Number of Layers 
!       - Pres_lay          I              Layer pressure grid
!       - Temp_lay          I              Layer temperature profile
!       - Q_lay             I              Layer water vapor profile
!       - QC                I              QC vector
!       - FLVH              0              Freezing Level Height
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       09-29-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
 SUBROUTINE ComputeFLVH(nLay,Pres_lay,Temp_Lay,Q_Lay,SfcPress,QC,FLVH)
   !---Input/Output variables  
   INTEGER, INTENT(IN)                  :: nLay
   INTEGER(2), DIMENSION(:), INTENT(IN) :: QC         
   REAL, DIMENSION(:), INTENT(IN)       :: Pres_lay,Temp_lay,Q_lay 
   REAL,INTENT(IN)                      :: SfcPress
   REAL                                 :: FLVH
   !---Local variables
   INTEGER                  :: iLay,idxpsfc,idxp300,ipflvh,cntpfrez
   REAL                     :: Tvt
   REAL, PARAMETER          :: RO=287.053,g=9.80665,w=0.61,TFLV=273.15
   FLVH=-999.0
   idxpsfc=0
   cntpfrez=0
   idxp300=64
   DO iLay=1,nLay
      IF(Pres_lay(iLay) .LE. SfcPress) idxpsfc=idxpsfc+1
      IF((Temp_lay(iLay) .GE. TFLV) .AND. (iLay .GE. idxp300) .AND. (Pres_lay(iLay) .LE. SfcPress)) THEN
         cntpfrez=cntpfrez+1
      ENDIF
   ENDDO
   ipflvh=idxpsfc-cntpfrez
   IF((QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      Tvt=Temp_lay(idxpsfc)*(1+w*(Q_lay(idxpsfc)/1000.0))
      FLVH=((RO*Tvt)/g)*(LOG(Pres_lay(idxpsfc)/Pres_lay(ipflvh)))*0.001
      IF(ipflvh .EQ. idxpsfc) FLVH = 0.0
   ENDIF
   IF(FLVH .LT. 0) FLVH=-999.0
 END SUBROUTINE ComputeFLVH

!===============================================================
! Name:         MSPPSRR
!
!
! Type:         Subroutine
!
!
! Description:  45km-Resolution-MSPPS-Based Regression Algorithm  
!               that computes rain rate in mm/hr from MIRS products. 
!         
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SfcTyp             I              Surface Type 
!       - QC                 I              Quality Control Flag
!       - CLW                I              Cloud Liquid Water
!       - GWP                I              Graupel Water Path
!       - RWP                I              Rain Water Path
!       - RR                 0              Computed Regressed RR
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-18-2009      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
 SUBROUTINE MSPPSRR(SfcTyp,QC,CLW,GWP,RWP,TPW,Q950,T950,GTHK,FLVH,TSKN,RR)
   !---Input/Output variables  
   INTEGER                  :: SfcTyp
   INTEGER(2), DIMENSION(:) :: QC
   REAL                     :: CLW,GWP,RWP
   REAL                     :: TPW,Q950,T950
   REAL                     :: GTHK,FLVH,TSKN
   REAL                     :: RR

   !---Local variables
   !----Regression coefficients over Land:GWP and RWP
   REAL, PARAMETER      :: A0_LD=-0.02645,AGWP_LD=12.03619,ARWP_LD=9.74240
   !----Regression coeficcients over Ocean:GWP,RWP,TPW,Q950,T950,GTHK,FLVH,TSKN and CLW
   REAL, PARAMETER      :: A0_OC=14.25770,AGWP_OC=6.59885,ARWP_OC=1.41296,ATPW_OC=-0.04646
   REAL, PARAMETER      :: AQ950_OC=0.21514,AT950_OC=0.10524,AGTHK_OC=-8.61667,AFLVH_OC=0.20229
   REAL, PARAMETER      :: ATSKN_OC=0.01934,ACLW_OC=0.16851

   RR=0.0   !---- This is a must, otherwise, if not met, RR get a random value ----
   !-Over Ocean Surface
   IF((SfcTyp .EQ. OC_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF((CLW .GT. 0.275) .OR. (GWP .GT. 0.005) .OR. (RWP .GT. 0.01)) THEN
         !-Training Parameteres over Ocean:GWP,RWP,TPW,Q950,T950,GTHK,FLVH,TSKN and CLW
         IF(CLW .LT. 0.0) CLW = 0.0
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR = A0_OC + AGWP_OC*GWP + ARWP_OC*RWP + ATPW_OC*TPW + AQ950_OC*Q950 + AT950_OC*T950 &
              + AGTHK_OC*GTHK + AFLVH_OC*FLVH + ATSKN_OC*TSKN + ACLW_OC*CLW
      ENDIF
   ENDIF
   !-Over Land Surface
   IF((SfcTyp .EQ. LD_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF((GWP .GT. 0.0001) .OR. (RWP .GT. 0.0001)) THEN
         !-Training Parameteres over Land:GWP and RWP
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR = A0_LD + AGWP_LD*GWP + ARWP_LD*RWP
      ENDIF
   ENDIF
   !-Over Snow-Land Surface
   IF((SfcTyp .EQ. SNOW_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF((GWP .GT. 0.0001) .OR. (RWP .GT. 0.0001)) THEN
         !-Training Parameteres over Land:GWP and RWP
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR = A0_LD + AGWP_LD*GWP + ARWP_LD*RWP
      ENDIF
   ENDIF

 END SUBROUTINE MSPPSRR

!===============================================================
! Name:         TMI2A12RR
!
!
! Type:         Subroutine
!
!
! Description:  TMI-Based Regression Algorithm that computes rain rate 
!               in mm/hr from MIRS products. 
!         
!              
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SfcTyp             I              Surface Type 
!       - QC                 I              Quality Control Flag
!       - CLW                I              Cloud Liquid Water
!       - RWP                I              Rain Water Path
!       - RR                 0              Computed Regressed RR
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-18-2009      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
 SUBROUTINE TMI2A12RR(SfcTyp,QC,CLW,RWP,RR)
   !---Input/Output variables  
   INTEGER                  :: SfcTyp
   INTEGER(2), DIMENSION(:) :: QC
   REAL                     :: CLW,RWP
   REAL                     :: RR

   !---Local variables
   !----Regression coefficients over Land:RWP
   REAL, PARAMETER      :: A0_LD=0.02681,ARWP_LD=4.08532
   !----Regression coeficcients over Ocean:RWP and CLW
   REAL, PARAMETER      :: A0_OC=-0.02722,ARWP_OC=3.37616,ACLW_OC=2.93181
   
   RR=0.0 !---- This is a must, otherwise, if not met, RR get a random value ----
   
   IF((SfcTyp .EQ. OC_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF((CLW .GT. 0.200) .OR. (RWP .GT. 0.0001)) THEN
         IF(CLW .LT. 0.0) CLW = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR = A0_OC + ARWP_OC*RWP + ACLW_OC*CLW
      ENDIF
   ENDIF

   IF((SfcTyp .EQ. LD_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF(RWP .GT. 0.0001) THEN
         RR = A0_LD + ARWP_LD*RWP
      ENDIF
   ENDIF

 END SUBROUTINE TMI2A12RR


!===============================================================
! Name:         MM5RR
!
!
! Type:         Subroutine
!
!
! Description:  45km-Resolution-MM5-Based Regression Algorithm  
!               that computes rain rate in mm/hr from MIRS products. 
!         
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SfcTyp             I              Surface Type 
!       - QC                 I              Quality Control Flag
!       - CLW                I              Cloud Liquid Water
!       - GWP                I              Graupel Water Path
!       - RWP                I              Rain Water Path
!       - RR                 0              Computed Regressed RR
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-18-2009      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
 SUBROUTINE MM5RR(SfcTyp,QC,CLW,GWP,RWP,RR)
   !---Input/Output variables  
   INTEGER                  :: SfcTyp
   INTEGER(2), DIMENSION(:) :: QC
   REAL                     :: CLW,GWP,RWP
   REAL                     :: RR,RR_CLW,RR_IWP,RR_RWP

   !---Local variables
   !----Regression coefficients over Land:RWP
   REAL, PARAMETER      :: ARWP_LD=5.731
   !----Regression coefficients over Land:GWP and RWP
   !   REAL, PARAMETER      :: A0_LD=0.505,AGWP_LD=-0.814,ARWP_LD=6.190
   !----Regression coefficients over Ocean/Land:CLW,GWP,RWP
   REAL, PARAMETER      :: A0_OC=0.274,ACLW_OC=2.202,AGWP_OC=-0.302,ARWP_OC=5.329

   RR=0.0   !---- This is must, otherwise, if not met, RR get a random value ----

   !-Over Ocean Surface
   IF( (SfcTyp .EQ. OC_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF( (CLW .GT. 0.275) .OR. (GWP .GT. 0.001) .OR. (RWP .GT. 0.001)) THEN
         IF(CLW .LT. 0.0) CLW = 0.0
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR = A0_OC + ACLW_OC*CLW + AGWP_OC*GWP + ARWP_OC*RWP 
      ENDIF
   ENDIF
   !-Over Sea-Icea Surface
   IF( (SfcTyp .EQ. SEAICE_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF( (CLW .GT. 0.275) .OR. (GWP .GT. 0.001) .OR. (RWP .GT. 0.001)) THEN
         IF(CLW .LT. 0.0) CLW = 0.0
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR = A0_OC + ACLW_OC*CLW + AGWP_OC*GWP + ARWP_OC*RWP 
      ENDIF
   ENDIF
   !-Over Land Surface
   IF((SfcTyp .EQ. LD_TYP) .AND. (QC(1) .GE. 0) .AND. (QC(1) .NE. 2)) THEN
      IF(RWP .GT. 0.0001) THEN
         RR = ARWP_LD*RWP
         !         RR = A0_LD + AGWP_LD*GWP + ARWP_LD*RWP
      ENDIF
   ENDIF

   RR = 0.0
   !----------- AMSR2--------------
   IF ( (Sensor_id .eq. sensor_id_gcomw1) .AND. (SfcTyp .EQ. OC_TYP) ) THEN
      IF( (CLW .GT. 0.5) .OR. (GWP .GT. 0.001) .OR. (RWP .GT. 0.001)) THEN
         IF(CLW .LT. 0.0) CLW = 0.0
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR_CLW = 14.39*CLW
         RR_IWP = 4.41*GWP
         RR_RWP = 5.27*RWP
         RR = RR_CLW+(0.4*RR_IWP+0.6*RR_RWP)
      ENDIF
   ENDIF

   IF ( (Sensor_id .eq. sensor_id_gcomw1) .AND. (SfcTyp .EQ. LD_TYP) ) THEN
      IF( (GWP .GT. 0.001) .OR. (RWP .GT. 0.001)) THEN
         IF(GWP .LT. 0.0) GWP = 0.0
         IF(RWP .LT. 0.0) RWP = 0.0
         RR_IWP = 4.41*GWP
         RR_RWP = 5.27*RWP
         RR = 0.4*RR_IWP+0.6*RR_RWP
      ENDIF
   ENDIF

 END SUBROUTINE MM5RR


!===============================================================
! Name:         regressWspd
!
!
! Type:         Subroutine
!
!
! Description:  Use Uncorrected TBs to retrieve ocean surface wind speed (m/s)
!               
!         
!              
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------

!       - Sensor_Id          I              Sensor ID
!       - SfcTyp             I              Surface Type 
!       - rrMirs             I              Current Estimate of MiRS rain rate
!       - Tbu                I              Uncorrected brightness temperatures
!       - Emiss              I              Estimate of Sfc Emissivities (currently not used)
!       - Wspd               0              Estimated Wind Speed (m/s)
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       08-05-2011      C. Grassotti, IMSG Inc @ NOAA/NESDIS/STAR
!
  SUBROUTINE regressWspd(sensor_id,SfcTyp,rrMirs,Tbu,Emiss,Wspd)
    !---Input/Output variables
    INTEGER, INTENT(IN)              :: sensor_id, SfcTyp
    REAL, INTENT(IN)                 :: rrMirs
    REAL, INTENT(IN), DIMENSION(:)   :: Tbu ! uncorrected TBs
    REAL, INTENT(IN), DIMENSION(:)   :: Emiss ! Emissivity
    REAL, INTENT(OUT)                :: Wspd
    !---Internal variables
    integer :: icoef, nchan
    real :: wspd0
    REAL, PARAMETER        :: wspdMin=0.,wspdMax=40.,rrMax=6.,emPRmin=-0.25

! Number of linear regression coefficients, including a0 (intercept)
    INTEGER, PARAMETER     :: nCoef_f18=6,nCoef_f17=6,nCoef_f16=6,nCoef_trmm=6,nCoef_gpm=6,nCoef_mtma=6

! Applicable to: F16,F17,F18,TRMM,GPM,MT/MADRAS

!!!! F18
!
! Regression coefficients and predictor channel indices for f18: Wspd=a0+a1*TB(19h)+a2*TB(19v)+a3*TB(22v)+a4*TB(37h)+a5*TB(37v)
    ! From training (2011-08-02): TB 19h,19v,22v,37h,37v and ECMWF Wspd
    ! Unc TBs AND Filtering out all AnalysEm <=0. 
    REAL, PARAMETER, dimension(nCoef_f18) :: coefRegress_f18=(/180.43318,-0.12356,0.2545,0.06159,0.76884,-1.59048/)
    INTEGER, PARAMETER, dimension(nCoef_f18-1) :: indPred_f18=(/12,13,14,15,16/)

!!!! F17
!
! Regression coefficients and predictor channel indices for f17: Wspd=a0+a1*TB(19h)+a2*TB(19v)+a3*TB(22v)+a4*TB(37h)+a5*TB(37v)
    ! From training (2011-08-02): TB 19h,19v,22v,37h,37v and ECMWF Wspd
    ! Unc TBs AND Filtering out all AnalysEm <=0. 
    REAL, PARAMETER, dimension(nCoef_f17) :: coefRegress_f17=(/180.43318,-0.12356,0.2545,0.06159,0.76884,-1.59048/)
    INTEGER, PARAMETER, dimension(nCoef_f17-1) :: indPred_f17=(/12,13,14,15,16/)

!!!! F16 
!
! Regression and predictor channel indices coefficients for f16: Wspd=a0+a1*TB(19h)+a2*TB(19v)+a3*TB(22v)+a4*TB(37h)+a5*TB(37v)
    ! From training (2011-08-02): TB 19h,19v,22v,37h,37v and ECMWF Wspd
    ! Unc TBs AND Filtering out all AnalysEm <=0. 
    REAL, PARAMETER, dimension(nCoef_f16) :: coefRegress_f16=(/189.24045,-0.30974,0.55810,0.05988,0.97281,-1.94233/)
    INTEGER, PARAMETER, dimension(nCoef_f16-1) :: indPred_f16=(/12,13,14,15,16/)

!!!! TRMM
! Regression coefficients and predictor channel indices for trmm/tmi: Wspd=a0+a1*TB(19v)+a2*TB(19h)+a3*TB(22v)+a4*TB(37v)+a5*TB(37h)
    ! From training (2010-09-19): Uncorrected Obs TBs 19v,19h,22v,37v,37h and ECMWF Wspd 
    ! Unc TBs AND Filtering out all AnalysEm <=0. 
    REAL, PARAMETER, dimension(nCoef_trmm) :: coefRegress_trmm=(/147.64955,0.62839,-0.35246,0.08832,-1.69042,0.80615/)
    INTEGER, PARAMETER, dimension(nCoef_trmm-1) :: indPred_trmm=(/3,4,5,6,7/)

!!!! GPM
! Regression coefficients and predictor channel indices for gpm/gmi: Wspd=a0+a1*TB(19v)+a2*TB(19h)+a3*TB(22v)+a4*TB(37v)+a5*TB(37h)
! NB: these are placeholders based on trmm/tmi until coefficients based on real gmi data are computed
    REAL, PARAMETER, dimension(nCoef_gpm) :: coefRegress_gpm=(/147.64955,0.62839,-0.35246,0.08832,-1.69042,0.80615/)
    INTEGER, PARAMETER, dimension(nCoef_gpm-1) :: indPred_gpm=(/3,4,5,6,7/)

!!!! MADRAS
! Regression coefficients and predictor channel indices for mt/madras: Wspd=a0+a1*TB(19v)+a2*TB(19h)+a3*TB(22v)+a4*TB(37v)+a5*TB(37h)
! NB: these are placeholders based on trmm/tmi until coefficients based on real madras data are computed
    REAL, PARAMETER, dimension(nCoef_mtma) :: coefRegress_mtma=(/147.64955,0.62839,-0.35246,0.08832,-1.69042,0.80615/)
    INTEGER, PARAMETER, dimension(nCoef_mtma-1) :: indPred_mtma=(/1,2,3,4,5/)


!-----------------------------------------------------------------------------------



    wspd=DEFAULT_VALUE_REAL

    if(SfcTyp .NE. OC_TYP) return
    if(rrMirs .gt. rrMax)return
    nchan=size(Tbu)
    if(any(Tbu(1:nchan) .lt. 0.))return
    
    if(sensor_id .eq. sensor_id_f16)then
! Filter out points likely to be precip-contaminated using polarization ratios PR(19), PR(37)
       if((Emiss(13)-Emiss(12))/(Emiss(13)+Emiss(12)) .le. emPRmin .or. &
          (Emiss(16)-Emiss(15))/(Emiss(16)+Emiss(15)) .le. emPRmin )then
          return
       endif
       wspd0=coefRegress_f16(1)
       do icoef=2,nCoef_f16
          wspd0=wspd0+coefRegress_f16(icoef)*Tbu(indPred_f16(icoef-1))
       enddo

    elseif(sensor_id .eq. sensor_id_f17)then
! Filter out points likely to be precip-contaminated using polarization ratios PR(19), PR(37)
       if((Emiss(13)-Emiss(12))/(Emiss(13)+Emiss(12)) .le. emPRmin .or. &
          (Emiss(16)-Emiss(15))/(Emiss(16)+Emiss(15)) .le. emPRmin )then
          return
       endif
       wspd0=coefRegress_f17(1)
       do icoef=2,nCoef_f17
          wspd0=wspd0+coefRegress_f17(icoef)*Tbu(indPred_f17(icoef-1))
       enddo

    elseif(sensor_id .eq. sensor_id_f18)then
! Filter out points likely to be precip-contaminated using polarization ratios PR(19), PR(37)
       if((Emiss(13)-Emiss(12))/(Emiss(13)+Emiss(12)) .le. emPRmin .or. &
          (Emiss(16)-Emiss(15))/(Emiss(16)+Emiss(15)) .le. emPRmin )then
          return
       endif
       wspd0=coefRegress_f18(1)
       do icoef=2,nCoef_f18
          wspd0=wspd0+coefRegress_f18(icoef)*Tbu(indPred_f18(icoef-1))
       enddo

    elseif(sensor_id .eq. sensor_id_trmm)then
! Filter out points likely to be precip-contaminated using polarization ratios PR(19), PR(37)
       if((Emiss(3)-Emiss(4))/(Emiss(3)+Emiss(4)) .le. emPRmin .or. &
          (Emiss(6)-Emiss(7))/(Emiss(6)+Emiss(7)) .le. emPRmin )then
          return
       endif
       wspd0=coefRegress_trmm(1)
       do icoef=2,nCoef_trmm
          wspd0=wspd0+coefRegress_trmm(icoef)*Tbu(indPred_trmm(icoef-1))
       enddo

    elseif(sensor_id .eq. sensor_id_gpm)then
! Filter out points likely to be precip-contaminated using polarization ratios PR(19), PR(37)
       if((Emiss(3)-Emiss(4))/(Emiss(3)+Emiss(4)) .le. emPRmin .or. &
          (Emiss(6)-Emiss(7))/(Emiss(6)+Emiss(7)) .le. emPRmin )then
          return
       endif
       wspd0=coefRegress_gpm(1)
       do icoef=2,nCoef_gpm
          wspd0=wspd0+coefRegress_gpm(icoef)*Tbu(indPred_gpm(icoef-1))
       enddo

    elseif(sensor_id .eq. sensor_id_mtma)then
! Filter out points likely to be precip-contaminated using polarization ratios PR(19), PR(37)
       if((Emiss(1)-Emiss(2))/(Emiss(1)+Emiss(2)) .le. emPRmin .or. &
          (Emiss(4)-Emiss(5))/(Emiss(4)+Emiss(5)) .le. emPRmin )then
          return
       endif
       wspd0=coefRegress_mtma(1)
       do icoef=2,nCoef_mtma
          wspd0=wspd0+coefRegress_mtma(icoef)*Tbu(indPred_mtma(icoef-1))
       enddo

    else
       return
    endif

    wspd=wspd0

! Make sure value is within bounds

    if(wspd .lt. wspdMin)wspd=wspdMin
    if(wspd .gt. wspdMax)wspd=wspdMax



  end SUBROUTINE regressWspd

end program Vipp

