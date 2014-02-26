!$Id: GeophCovBkg.f90 3297 2013-06-19 16:02:29Z chrisg $
!-----------------------------------------------------------------------------------------------
! Name:                GeophCovBkg
! 
! Type:                F90 module
!
! Description:
!          This module is dedicated to loading the geophysical covariance 
!          matrix/Bkg/Transf Matrx. This cov matrx is dimensioned np x np x nTyp. 
!          It is assumed that we could have several
!          types of covariances (depending on the sfc/atm classes, etc).
!
! Modules needed:
!       - misc
!       - utils
!       - CntrlParams
!       - IO_Scene
!       - TuningParams
!
! Subroutines contained:
!       - LoadGlobGeophyCovBkg
!       - GetGeophBkgCov
!       - Set1stG2Bkg
!       - CombExtWithBkgCov
!       - mergeSfcAndAtm
!       - GetEigVectMatrx
!       - TuneCov
!       - DisablRetr
!       - ShrkU
!       - ProjCov
!       - computeNG
!       - computeNGselec
!       - DisabLayBelowSfc
!       - DisabLayAbovTopSensPress
!       - SetBkg2Ext
!       - GetPressFromCovFile
!       - ReadStats
!       - TuneBkgAtm
!       - DetermIndx
!       - Destroy_GeophStatsT_Atm
!       - Destroy_GeophStatsT_Sfc
!       - LoadBkgAtm_Ext
!       - GetBkgAtm_Ext
!       - ReplaceBkgAtm_Ext
!       - Destroy_BkgAtm_Ext
!
! Data type included:
!       - IndivCovBkgTrnsf_type
!       - CovBkgTrnsf_type
!       - GeophStatsT_Atm
!       - GeophStatsT_Sfc
!       - bkgAtm_Ext_type
!
! 
! History:
!        2006        S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE GeophCovBkg
  USE misc
  USE utils
  USE ErrorHandling
  USE CntrlParams
  USE IO_Scene
  USE TuningParams
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: LoadGlobGeophyCovBkg,GetGeophBkgCov,Set1stG2Bkg,CombExtWithBkgCov
  PUBLIC :: mergeSfcAndAtm,GetEigVectMatrx,TuneCov,DisablRetr,ShrkU,ProjCov
  PUBLIC :: computeNG,computeNGselec,DisabLayBelowSfc,DisabLayAbovTopSensPress
  PUBLIC :: SetBkg2Ext,GetPressFromCovFile,ReadStats,TuneBkgAtm,DetermIndx
  PUBLIC :: Destroy_GeophStatsT_Atm, Destroy_GeophStatsT_Sfc
  PUBLIC :: LoadBkgAtm_Ext,GetBkgAtm_Ext,ReplaceBkgAtm_Ext,Destroy_BkgAtm_Ext
  !---Publicly available data/type definitions
  PUBLIC :: IndivCovBkgTrnsf_type,CovBkgTrnsf_type,GeophStatsT_Atm,GeophStatsT_Sfc
  !---Declaration sections
  TYPE  :: IndivCovBkgTrnsf_type
    !---Header Information
    INTEGER                                              :: npEDR   !Number of params representing this EDR
    CHARACTER(LEN=6)                                     :: DescEDR !Label describing the EDR
    INTEGER                                              :: idEDR   !ID of the EDR
    !---Statistics
    REAL,                        DIMENSION(:,:), POINTER :: Sa      !Covariance mtrx
    REAL,                        DIMENSION(:,:), POINTER :: U       !Transform  mtrx
    REAL,                        DIMENSION(:),   POINTER :: Xb      !Background
  END TYPE IndivCovBkgTrnsf_type

  TYPE  :: CovBkgTrnsf_type
    !---General Info
    INTEGER                                              :: nEDRs
    INTEGER,                     DIMENSION(:),   POINTER :: EDR_IDs
    CHARACTER(LEN=10),           DIMENSION(:),   POINTER :: EDR_Desc
    INTEGER,                     DIMENSION(:),   POINTER :: iSpaceMode
    INTEGER                                              :: nTypes
    INTEGER,                     DIMENSION(:),   POINTER :: Type_IDs
    CHARACTER(LEN=10),           DIMENSION(:),   POINTER :: Type_Desc
    !---Pressure Info (valid only for Atmospheric file)
    INTEGER                                              :: nLev
    INTEGER                                              :: nLay
    INTEGER                                              :: nAbsorb
    INTEGER,                     DIMENSION(:),   POINTER :: AbsorbID
    REAL,                        DIMENSION(:),   POINTER :: pres_lev
    REAL,                        DIMENSION(:),   POINTER :: pres_lay
    !---Channel Info (valid only for surface [emiss] file)
    INTEGER                                              :: nchan
    REAL,                        DIMENSION(:),   POINTER :: Freq
    INTEGER,                     DIMENSION(:),   POINTER :: polar
    !---Statistics (Cov/Bkg/Trnsf matrxs)
    TYPE(IndivCovBkgTrnsf_type), DIMENSION(:,:), POINTER :: Stats
    !---Statistics of all-EDRs together
    TYPE(IndivCovBkgTrnsf_type), DIMENSION(:),   POINTER :: AllEDRstats
  END TYPE CovBkgTrnsf_type

  TYPE  :: bkgAtm_Ext_type
    INTEGER                                              :: nZ,nLat,nLon,nHr,nDay,nTypes          
    INTEGER						 :: nEDRs,nLay,nLev
    INTEGER,			 DIMENSION(:),   POINTER :: EDR_IDs
    CHARACTER(LEN=10),  	 DIMENSION(:),   POINTER :: EDR_Desc
    INTEGER,			 DIMENSION(:),   POINTER :: iSpaceMode
    INTEGER,			 DIMENSION(:),   POINTER :: Type_IDs
    CHARACTER(LEN=10),  	 DIMENSION(:),   POINTER :: Type_Desc
    REAL                                                 :: Lat1,LatN,dLat
    REAL                                                 :: Lon1,LonN,dLon
    INTEGER                                              :: Hr1,HrN,dHr
    INTEGER,  	                 DIMENSION(:),   POINTER :: julDay
    REAL,                        DIMENSION(:),   POINTER :: pres_lev
    REAL,                        DIMENSION(:),   POINTER :: pres_lay
    REAL,              DIMENSION(:,:,:,:,:,:),   POINTER :: Stats
    INTEGER,                     DIMENSION(:,:), POINTER :: mapEDR  ! map external EDRs to the COV EDRs            
  END TYPE bkgAtm_Ext_type

  !---Global variables 
  INTEGER,                        PARAMETER               :: mxAtt = 2, mxNday_bkgAtm_Ext=24
  TYPE(CovBkgTrnsf_type),         DIMENSION(mxAtt)        :: GeophStatsT_Atm 
  TYPE(CovBkgTrnsf_type),         DIMENSION(mxAtt)        :: GeophStatsT_Sfc
  TYPE(bkgAtm_Ext_type)                                   :: bkgAtm_Ext
  REAL,              DIMENSION(:,:,:,:,:,:),   POINTER    :: Stats1
  !---INTRINSIC functions used in this module
  INTRINSIC :: ALL,SUM,MATMUL,TRANSPOSE,SHAPE,SIZE,MINVAL,TRIM,ADJUSTL

CONTAINS

!===============================================================
! Name:                DetermIndx
!
!
! Type:                Subroutine
!
!
! Description:  Determines the index of the covariance matrix
!               that corresponds to the class selected.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iClass             I              Class selected
!       - Indx               O              Index corresponding to iClass
!       - nTypes             I              Number of types within the cov matrx
!       - Type_IDs           I              Type-IDs vector available in cov matrx
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

  SUBROUTINE DetermIndx(iClass,Indx,nTypes,Type_IDs)
    INTEGER               :: iClass,Indx,nTypes,i
    INTEGER, DIMENSION(:) :: Type_IDs
    IF (iClass .ge. 0) THEN 
       IF (ALL(Type_IDs(1:nTypes) .ne. iClass)) THEN
          print *, 'Iclass:',iClass,' Cov Ids:',Type_IDs(1:nTypes)
          CALL ErrHandl(ErrorType,Err_NoClassFoundInCov,'(In determining Covariance class index)')
       ENDIF
       DO i=1,nTypes
          IF (iClass .eq. Type_IDs(i)) indx=i
       ENDDO
    ELSE
       !---by default (if sfcClass invalid, use first index of cov matrx)
       indx=1
    ENDIF
    RETURN
  END SUBROUTINE DetermIndx


!===============================================================
! Name:                 computeNG
!
!
! Type:                Function
!
!
! Description:  Computes the total number of geophysical 
!               parameters within the covariance matrix
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - GeophStatsT        I              Cov Matrix structure 
!                                           (see definition in top 
!                                           section of the module)
!       - computeNG          O              Result: Total # geoph params
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

  INTEGER FUNCTION computeNG(GeophStatsT)
    TYPE(CovBkgTrnsf_type) :: GeophStatsT
    computeNG= SUM(GeophStatsT%Stats(1:GeophStatsT%nEDRs,1)%npEDR)
    RETURN
  END FUNCTION computeNG

!===============================================================
! Name:                 computeNGselec
!
!
! Type:                Function
!
!
! Description:  Computes the total number of geophysical 
!               parameters selected for retrieval
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - ParamLabelAtm      I             Atmosph params labels
!       - ParamLabelSfc      I             Sfc params labels
!       - EDR_nEOF_atm       I             #EOFs selected for Atm params
!       - EDR_nEOF_sfc       I             #EOFs selected for Sfc params
!       - nGatm              I             Number of parameters for each Atm EDR
!       - nGsfc              I             Number of parameters for each Sfc EDR
!       - computeNGselec     O             Result: total # of params selected
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

  INTEGER FUNCTION computeNGselec(ParamLabelAtm,ParamLabelSfc,EDR_nEOF_atm,EDR_nEOF_sfc,nGatm,nGsfc)
    INTEGER                          :: i,nEDRs_atm,nEDRs_sfc
    INTEGER,          DIMENSION(:)   :: EDR_nEOF_atm,EDR_nEOF_sfc,nGatm,nGsfc
    CHARACTER(LEN=*), DIMENSION(:)   :: ParamLabelAtm,ParamLabelSfc
    nEDRs_atm        = size(ParamLabelAtm)
    nEDRs_sfc        = size(ParamLabelSfc)
    computenGselec   = 0
    DO i=1,nEDRs_atm
       IF (EDR_nEOF_atm(i) .ne. 0) THEN
          computenGselec = computenGselec + nGatm(i)
       ENDIF
    ENDDO
    DO i=1,nEDRs_sfc
       IF (EDR_nEOF_sfc(i) .ne. 0) THEN
          computenGselec = computenGselec   +nGsfc(i)
       ENDIF
    ENDDO
  END FUNCTION computeNGselec

!===============================================================
! Name:                GetPressFromCovFile
!
!
! Type:                Subroutine
!
!
! Description:  Gets the pressure info from a Bkgd file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - CovBkgFile         I              Atm Bkgd file name
!       - nLay               0              Number of layers
!       - pressLay           0              Layer-based pressure vector
!       - nLev               O              Number of levels
!       - pressLev           O              Level-based pressure vector
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

  SUBROUTINE GetPressFromCovFile(CovBkgFile,nLay,pressLay,nLev,pressLev)
    INTEGER                     :: iu,nLay,nLev
    CHARACTER(LEN=*)            :: CovBkgFile
    REAL, DIMENSION(:), POINTER :: pressLay,pressLev
    iu=get_lun()
    OPEN(iu,file=CovBkgFile,status='old',form='formatted')
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    !---Read the EDRs Information
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    !---Read the Classes Information
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    !---Read pressure info 
    READ(iu,'(25x,i8)')  nLev
    READ(iu,'(a)')    
    ALLOCATE(presslev(nLev))
    READ(iu,'(10f10.3)') presslev
    READ(iu,'(25x,i8)')  nLay
    READ(iu,'(a)')    
    ALLOCATE(presslay(nLay))
    READ(iu,'(10f10.3)') presslay
    CLOSE(iu)
  END SUBROUTINE GetPressFromCovFile

!===============================================================
! Name:                ReadStats
!
!
! Type:                Subroutine
!
!
! Description:  Reads info and statistics from the total geophysical 
!               background file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iAtmOrSfc          I              Switch Flag: 0->Atm bkgd, 1->Sfc bkgd
!       - CovBkgFile         I              Covariance background file name
!       - GeophStatsT        O              Total Cov Matrix structure 
!                                           (see definition in top 
!                                           section of the module)
!
!
! Modules needed:
!       - LoadIndivStats
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ReadStats(iAtmOrSfc,CovBkgFile,GeophStatsT)
    TYPE(CovBkgTrnsf_type) :: GeophStatsT
    CHARACTER(LEN=*)       :: CovBkgFile
    INTEGER                :: iu,iEDR,iTyp,iAtmOrSfc,i
    iu=get_lun()
    OPEN(iu,file=CovBkgFile,status='old',form='formatted')
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    !---Read the EDRs Information
    READ(iu,'(25x,i8)') GeophStatsT%nEDRs   
    ALLOCATE(GeophStatsT%EDR_IDs(GeophStatsT%nEDRs))
    ALLOCATE(GeophStatsT%EDR_Desc(GeophStatsT%nEDRs))
    ALLOCATE(GeophStatsT%iSpaceMode(GeophStatsT%nEDRs))
    ALLOCATE(GeophStatsT%AbsorbID(GeophStatsT%nEDRs))
    READ(iu,'(25x,10i8)') GeophStatsT%EDR_IDs(1:GeophStatsT%nEDRs)   
    READ(iu,'(25x,10a8)') GeophStatsT%EDR_Desc(1:GeophStatsT%nEDRs)
    READ(iu,'(25x,10i8)') GeophStatsT%iSpaceMode(1:GeophStatsT%nEDRs)
    GeophStatsT%nAbsorb =0
    DO i=1,GeophStatsT%nEDRs 
       IF (adjustl(trim(GeophStatsT%EDR_Desc(i))) .eq. 'WVAP')  THEN
          GeophStatsT%nAbsorb=GeophStatsT%nAbsorb+1
          GeophStatsT%AbsorbID(GeophStatsT%nAbsorb) = 1
       ENDIF
       IF (adjustl(trim(GeophStatsT%EDR_Desc(i))) .eq. 'OZON')  THEN
          GeophStatsT%nAbsorb=GeophStatsT%nAbsorb+1
          GeophStatsT%AbsorbID(GeophStatsT%nAbsorb) = 3
       ENDIF
    ENDDO
    !---Read the Classes Information
    READ(iu,'(25x,i8)')   GeophStatsT%nTypes
    ALLOCATE(GeophStatsT%Type_IDs(GeophStatsT%nTypes))
    ALLOCATE(GeophStatsT%Type_Desc(GeophStatsT%nTypes))
    READ(iu,'(25x,10i8)') GeophStatsT%Type_IDs(1:GeophStatsT%nTypes)   
    READ(iu,'(25x,10a20)') GeophStatsT%Type_Desc(1:GeophStatsT%nTypes)
    !---Read pressure info if Atmospheric file
    IF (iAtmOrSfc .eq. 0) THEN
       READ(iu,'(25x,i8)')  GeophStatsT%nLev
       READ(iu,'(a)')    
       ALLOCATE(GeophStatsT%pres_lev(GeophStatsT%nLev))
       READ(iu,'(10f10.3)') GeophStatsT%pres_lev
       READ(iu,'(25x,i8)')  GeophStatsT%nLay
       READ(iu,'(a)')    
       ALLOCATE(GeophStatsT%pres_lay(GeophStatsT%nLay))
       READ(iu,'(10f10.3)') GeophStatsT%pres_lay
    ENDIF
    !---Read channel if Surfce (emiss) file
    IF (iAtmOrSfc .eq. 1) THEN
       READ(iu,'(25x,i8)')  GeophStatsT%nChan
       READ(iu,'(a)')    
       ALLOCATE(GeophStatsT%Freq(GeophStatsT%nchan))
       ALLOCATE(GeophStatsT%polar(GeophStatsT%nchan))
       READ(iu,'(10f10.3)') GeophStatsT%Freq
       READ(iu,'(a)')    
       READ(iu,'(30i3)')    GeophStatsT%polar
    ENDIF
    !---Read the stats by type and by EDR
    ALLOCATE(GeophStatsT%Stats(GeophStatsT%nEDRs,GeophStatsT%nTypes),&
         GeophStatsT%AllEDRstats(GeophStatsT%nTypes))
    TypLoop: DO iTyp=1,GeophStatsT%nTypes
       EDRLoop: DO iEDR=1,GeophStatsT%nEDRs
          CALL LoadIndivStats(iu,GeophStatsT%Stats(iEDR,iTyp))
       ENDDO EDRLoop
       !---Read the all-EDRs stats
       CALL LoadIndivStats(iu,GeophStatsT%AllEDRstats(iTyp))
    ENDDO TypLoop

    !---close file
    close(iu)
  END SUBROUTINE ReadStats


!===============================================================
! Name:                LoadIndivStats
!
!
! Type:                Subroutine
!
!
! Description:  Reads individual sections of the total background
!               matrix file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 I              Unit number
!       - Stats              I/O            Structure that contains 
!                                           individual statistics (Sa, U, Xb)
!                                           for a specific EDR
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

  SUBROUTINE LoadIndivStats(iu,Stats)
    TYPE(IndivCovBkgTrnsf_type) :: Stats
    INTEGER                     :: iu,ip
    READ(iu,'(a)')    
    !---read general info about EDR
    READ(iu,'(25x,i8)')   Stats%idEDR
    READ(iu,'(25x,a10)')  Stats%DescEDR
    READ(iu,'(25x,i8)')   Stats%npEDR
    !---Read covariance
    ALLOCATE(Stats%Sa(Stats%npEDR,Stats%npEDR))
    READ(iu,'(a)')    
    DO ip=1,Stats%npEDR
       READ(iu,'(10f10.4)')   Stats%Sa(ip,1:Stats%npEDR)
    ENDDO
    READ(iu,'(a)')    
    !---Read transformation matrix
    ALLOCATE(Stats%U(Stats%npEDR,Stats%npEDR))
    READ(iu,'(a)')    
    DO ip=1,Stats%npEDR
       READ(iu,'(10f10.4)')   Stats%U(ip,1:Stats%npEDR)
    ENDDO
    READ(iu,'(a)')    
    !---Read background vector
    ALLOCATE(Stats%Xb(Stats%npEDR))
    READ(iu,'(a)')    
    READ(iu,'(10f10.4)')   Stats%Xb(1:Stats%npEDR)
    RETURN
  END SUBROUTINE LoadIndivStats


!===============================================================
! Name:                LoadGlobGeophyCovBkg
!
!
! Type:                Subroutine
!
!
! Description:  Reads/loads both surface and atmospheric 
!               covariance files.
!
!
! Arguments:  None
!
!
! Modules needed:
!       - ReadStats
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE LoadGlobGeophyCovBkg()
    INTEGER :: iattempt
    !INTEGER :: iTypSea,iTypIce,iTypLand,iTypSnow,iLay
    !REAL, DIMENSION(:), ALLOCATABLE :: meanT

    DO iattempt=1,CntrlConfig%nAttempts
       CALL ReadStats(0,CntrlConfig%CovBkgFileAtm(iattempt),GeophStatsT_Atm(iattempt))
       CALL ReadStats(1,CntrlConfig%CovBkgFileSfc(iattempt),GeophStatsT_Sfc(iattempt))
    ENDDO
    
    !----Overwrite the background info (ad-hoc modification).'
    !iTypSnow = 4
    !iTypLand = 3
    !iTypIce  = 2
    !iTypSea  = 1
    !ALLOCATE(meanT(GeophStatsT_Atm(1)%nLay))
    !MeanT=(/&
    !     -999.000,    -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,&
    !     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,&
    !     -999.000,     -999.000,     -999.000,      227.942,      226.976,      225.350,      224.111,      222.906,&
    !     221.119,      220.396,      219.833,      219.009,      218.177,      217.621,      216.982,      216.319,&
    !     215.659,      214.928,      214.078,      213.346,      212.617,      212.030,      211.753,      211.311,&
    !     211.048,      210.949,      211.046,      211.312,      211.578,      212.202,      212.956,      213.676,&
    !     214.377,      215.167,      215.674,      216.245,      216.837,      217.473,      218.109,      218.767,&
    !     219.609,      220.601,      221.739,      223.036,      224.609,      226.382,      228.242,      229.515,&
    !     231.552,      233.631,      235.728,      237.856,      240.001,      242.148,      244.290,      246.421,&
    !     248.488,      250.540,      252.543,      254.472,      256.233,      258.033,      259.812,      261.564,&
    !     263.287,      265.007,      266.620,      268.148,      269.649,      271.049,      272.500,      273.715,&
    !     274.750,      275.798,      276.803,      277.903,      278.897,      279.782,      280.338,      280.867,&
    !     -999.000,     -999.000,     -999.000,     -999.000/)

    !MeanT=(/&
    ! -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,&
    ! -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,     -999.000,&
    ! -999.000,      216.468,      215.543,      226.228,      225.431,      225.430,      224.218,      223.257,&
    !  221.621,      220.968,      220.245,      219.456,      218.545,      217.883,      217.010,      216.297,&
    !  215.605,      214.841,      213.982,      212.948,      212.229,      211.590,      211.294,      210.821,&
    !  210.543,      210.443,      210.587,      210.868,      211.089,      211.739,      212.523,      213.273,&
    !  213.964,      214.710,      215.324,      215.980,      216.699,      217.457,      218.238,      219.096,&
    !  220.075,      221.187,      222.428,      223.819,      225.423,      227.194,      229.123,      230.475,&
    !  232.556,      234.712,      236.878,      239.058,      241.251,      243.434,      245.589,      247.713,&
    !  249.778,      251.808,      253.785,      255.702,      257.541,      259.373,      261.174,      262.943,&
    !  264.667,      266.348,      267.961,      269.529,      271.061,      272.510,      273.964,      275.323,&
    !  276.551,      277.770,      278.924,      280.033,      281.046,      281.985,      282.766,      283.227,&
    ! -999.000,     -999.000,     -999.000,     -999.000/)

    !DO ilay=1,GeophStatsT_Atm(1)%nLay
    !   IF(meanT(iLay) .gt. 0.) GeophStatsT_Atm(1)%AllEDRstats(iTypSea)%Xb(iLay)=meanT(iLay)
    !ENDDO

    !---Replace Geoph stats of snow by land
    !DO iattempt=1,CntrlConfig%nAttempts
    !   GeophStatsT_Sfc(iattempt)%AllEDRstats(iTypSnow)%Xb=GeophStatsT_Sfc(iattempt)%AllEDRstats(iTypLand)%Xb
    !   GeophStatsT_Atm(iattempt)%AllEDRstats(iTypSnow)%Xb=GeophStatsT_Atm(iattempt)%AllEDRstats(iTypLand)%Xb
    !ENDDO

    !---Scale certain backgrounds
    !---Emiss
    !GeophStatsT_Sfc(1)%AllEDRstats(iTypLand)%Xb(1:GeophStatsT_Sfc(1)%nChan)=&
    !GeophStatsT_Sfc(1)%AllEDRstats(iTypLand)%Xb(1:GeophStatsT_Sfc(1)%nChan)
    !---Tskin
    !GeophStatsT_Atm(1)%AllEDRstats(iTypLand)%Xb(GeophStatsT_Atm(1)%AllEDRstats(iTypLand)%npEDR-1)=&
    !GeophStatsT_Atm(1)%AllEDRstats(iTypLand)%Xb(GeophStatsT_Atm(1)%AllEDRstats(iTypLand)%npEDR-1)+5.

    RETURN
  END SUBROUTINE LoadGlobGeophyCovBkg


!===============================================================
! Name:                GetGeophBkgCov
!
!
! Type:                Subroutine
!
!
! Description:  Selects the right sfc and atm covariance matrix 
!               & background, depending on the sfc and atm classes 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SaAtm              O              Atmospheric covar matrx
!       - XbAtm              0              Atmospheric background vector
!       - iTypAtm            I              Atmospheric class selected
!       - SaSfc              O              Surface covar matrx
!       - XbSfc              0              Surface background vector
!       - iTypSfc            I              Surface class selected
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

  SUBROUTINE GetGeophBkgCov(SaAtm,XbAtm,iTypAtm,SaSfc,XbSfc,iTypSfc,iattempt)
    REAL,     DIMENSION(:,:) :: SaAtm,SaSfc
    REAL,     DIMENSION(:)   :: XbAtm,XbSfc
    INTEGER                  :: iTypAtm,iTypSfc,i,j,nszAtm,nszSfc,iattempt
    nszAtm=SIZE(XbAtm)
    nszSfc=SIZE(XbSfc)
    DO i=1,nszAtm
       XbAtm(i)   = GeophStatsT_Atm(iattempt)%AllEDRstats(iTypAtm)%Xb(i)
       do j=1,nszAtm
          SaAtm(i,j) = GeophStatsT_Atm(iattempt)%AllEDRstats(iTypAtm)%Sa(i,j)
       ENDDO
    ENDDO
    DO i=1,nszSfc
       XbSfc(i)   = GeophStatsT_Sfc(iattempt)%AllEDRstats(iTypSfc)%Xb(i)
       do j=1,nszSfc
          SaSfc(i,j) = GeophStatsT_Sfc(iattempt)%AllEDRstats(iTypSfc)%Sa(i,j)
       ENDDO
    ENDDO
    !----commented out to accomodate bug in IBM compiler
    !SaAtm(:,:) = GeophStatsT_Atm%AllEDRstats(iTypAtm)%Sa(:,:)
    !SaSfc(:,:) = GeophStatsT_Sfc%AllEDRstats(iTypSfc)%Sa(:,:)
    !XbAtm(:)   = GeophStatsT_Atm%AllEDRstats(iTypAtm)%Xb(:)
    !XbSfc(:)   = GeophStatsT_Sfc%AllEDRstats(iTypSfc)%Xb(:)
    RETURN
  END SUBROUTINE GetGeophBkgCov


!===============================================================
! Name:                DisabLayBelowSfc
!
!
! Type:                Subroutine
!
!
! Description: Disables covariance values for leyers located below
!              the surface pressure.
!
!
! Arguments:
!
!            Name                    Type            Description
!      ---------------------------------------------------
!       - iEDR_temp                  I          Temp. Index (within SaAtm)
!       - iEDR_wvap                  I          Humid. Index
!       - iEDR_ozon                  I          Ozone Index
!       - iEDR_clw                   I          Cloud Index
!       - iEDR_rain                  I          Rain index
!       - iEDR_snow                  I          Snow index
!       - iEDR_ice                   I          Ice index
!       - iEDR_grpl                  I          Graupel index
!       - SaAtm                      I/O        Atmospheric covar matrx
!       - SfcPress                   I          Surface pressure
!
!
! Modules needed:
!       - DeterminNlayEff
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE DisabLayBelowSfc(iEDR_temp,iEDR_wvap,iEDR_ozon,iEDR_clw,   &
       iEDR_rain,iEDR_snow,iEDR_ice,iEDR_grpl,SaAtm,SfcPress,iattempt)
    INTEGER              :: iEDR_temp,iEDR_wvap,iEDR_ozon,iEDR_clw
    INTEGER              :: iEDR_rain,iEDR_snow,iEDR_ice,iEDR_grpl
    REAL, DIMENSION(:,:) :: SaAtm
    REAl                 :: SfcPress
    INTEGER              :: nLayEff,i,nLay,iattempt
    !---Readjust nLay/nLev according to SfcPress and Maxim Retrievalble Top press
    nLay    = GeophStatsT_Atm(iattempt)%nLay
    CALL DeterminNlayEff(nLay,GeophStatsT_Atm(iattempt)%pres_lay(1:nLay),SfcPress,nLayEff)
    !---Disable retrieval of below-sfc layers by setting covar to high value and decorrel.

    !---Temp
    SaAtm(iEDR_temp+nLayEff:iEDR_temp+nLay-1,:)  =0.
    SaAtm(:,iEDR_temp+nLayEff:iEDR_temp+nLay-1)  =0.
    Do i=iEDR_temp+nLayEff,iEDR_temp+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Water Vapor
    SaAtm(iEDR_wvap+nLayEff:iEDR_wvap+nLay-1,:)  =0.
    SaAtm(:,iEDR_wvap+nLayEff:iEDR_wvap+nLay-1)  =0.
    Do i=iEDR_wvap+nLayEff,iEDR_wvap+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Ozone
    SaAtm(iEDR_ozon+nLayEff:iEDR_ozon+nLay-1,:)  =0.
    SaAtm(:,iEDR_ozon+nLayEff:iEDR_ozon+nLay-1)  =0.
    Do i=iEDR_ozon+nLayEff,iEDR_ozon+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Clw
    SaAtm(iEDR_clw+nLayEff:iEDR_clw+nLay-1,:)    =0.
    SaAtm(:,iEDR_clw+nLayEff:iEDR_clw+nLay-1)    =0.
    Do i=iEDR_clw+nLayEff,iEDR_clw+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Rain
    SaAtm(iEDR_rain+nLayEff:iEDR_rain+nLay-1,:)  =0.
    SaAtm(:,iEDR_rain+nLayEff:iEDR_rain+nLay-1)  =0.
    Do i=iEDR_rain+nLayEff,iEDR_rain+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Snow
    SaAtm(iEDR_snow+nLayEff:iEDR_snow+nLay-1,:)  =0.
    SaAtm(:,iEDR_snow+nLayEff:iEDR_snow+nLay-1)  =0.
    Do i=iEDR_snow+nLayEff,iEDR_snow+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Ice
    SaAtm(iEDR_ice+nLayEff:iEDR_ice+nLay-1,:)    =0.
    SaAtm(:,iEDR_ice+nLayEff:iEDR_ice+nLay-1)    =0.
    Do i=iEDR_ice+nLayEff,iEDR_ice+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    !---Graupel
    SaAtm(iEDR_grpl+nLayEff:iEDR_grpl+nLay-1,:)  =0.
    SaAtm(:,iEDR_grpl+nLayEff:iEDR_grpl+nLay-1)  =0.
    Do i=iEDR_grpl+nLayEff,iEDR_grpl+nLay-1
       SaAtm(i,i) = 0.
    ENDDO
    RETURN
  END SUBROUTINE DisabLayBelowSfc


!===============================================================
! Name:                DisabLayAbovTopSensPress
!
!
! Type:                Subroutine
!
!
! Description:  Disables covariance values for leyers located above
!               the top pressure selected.
!
!
! Arguments:
!
!            Name                    Type            Description
!      ---------------------------------------------------
!       - iEDR_temp                  I          Temp. Index (within SaAtm)
!       - iEDR_wvap                  I          Humid. Index
!       - iEDR_ozon                  I          Ozone Index
!       - iEDR_clw                   I          Cloud Index
!       - iEDR_rain                  I          Rain index
!       - iEDR_snow                  I          Snow index
!       - iEDR_ice                   I          Ice index
!       - iEDR_grpl                  I          Graupel index
!       - SaAtm                      I/O        Atmospheric covar matrx
!       - topSensPressT              I          Press above which no sensitivity to Temp exists
!       - topSensPressWV             I          Press above which no sensitivity to WV exists
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

  SUBROUTINE DisabLayAbovTopSensPress(iEDR_temp,iEDR_wvap,iEDR_ozon,iEDR_clw,   &
       iEDR_rain,iEDR_snow,iEDR_ice,iEDR_grpl,SaAtm,topSensPressT,topSensPressWV,iattempt)
    INTEGER              :: iEDR_temp,iEDR_wvap,iEDR_ozon,iEDR_clw
    INTEGER              :: iEDR_rain,iEDR_snow,iEDR_ice,iEDR_grpl
    REAL, DIMENSION(:,:) :: SaAtm
    REAl                 :: topSensPressT,topSensPressWV
    INTEGER              :: i,nLay,iLayTopT,iLayTopWV,iattempt
    nLay      = GeophStatsT_Atm(iattempt)%nLay
    CALL DeterminLayIndxOnTopPress(nLay,GeophStatsT_Atm(iattempt)%pres_lay(1:nLay),topSensPressT,iLayTopT)
    CALL DeterminLayIndxOnTopPress(nLay,GeophStatsT_Atm(iattempt)%pres_lay(1:nLay),topSensPressWV,iLayTopWV)
    !---Disable retrieval of above-topSensPress by setting covar to high value and decorrel.

    !---Temp
    SaAtm(iEDR_temp:iEDR_temp+iLayTopT-1,:)       =0.
    SaAtm(:,iEDR_temp:iEDR_temp+iLayTopT-1)       =0.
    Do i=iEDR_temp,iEDR_temp+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---Water vapor
    SaAtm(iEDR_wvap:iEDR_wvap+iLayTopWV-1,:)      =0.
    SaAtm(:,iEDR_wvap:iEDR_wvap+iLayTopWV-1)      =0.
    Do i=iEDR_wvap,iEDR_wvap+iLayTopWV-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---ozone
    SaAtm(iEDR_ozon:iEDR_ozon+iLayTopT-1,:)       =0.
    SaAtm(:,iEDR_ozon:iEDR_ozon+iLayTopT-1)       =0.
    Do i=iEDR_ozon,iEDR_ozon+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---clw
    SaAtm(iEDR_clw:iEDR_clw+iLayTopT-1,:)         =0.
    SaAtm(:,iEDR_clw:iEDR_clw+iLayTopT-1)         =0.
    Do i=iEDR_clw,iEDR_clw+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---Rain
    SaAtm(iEDR_rain:iEDR_rain+iLayTopT-1,:)       =0.
    SaAtm(:,iEDR_rain:iEDR_rain+iLayTopT-1)       =0.
    Do i=iEDR_rain,iEDR_rain+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---Snow
    SaAtm(iEDR_snow:iEDR_snow+iLayTopT-1,:)       =0.
    SaAtm(:,iEDR_snow:iEDR_snow+iLayTopT-1)       =0.
    Do i=iEDR_snow,iEDR_snow+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---Ice
    SaAtm(iEDR_ice:iEDR_ice+iLayTopT-1,:)         =0.
    SaAtm(:,iEDR_ice:iEDR_ice+iLayTopT-1)         =0.
    Do i=iEDR_ice,iEDR_ice+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    !---Graupel
    SaAtm(iEDR_grpl:iEDR_grpl+iLayTopT-1,:)       =0.
    SaAtm(:,iEDR_grpl:iEDR_grpl+iLayTopT-1)       =0.
    Do i=iEDR_grpl,iEDR_grpl+iLayTopT-1
       SaAtm(i,i) = 1000.
    ENDDO
    RETURN
  END SUBROUTINE DisabLayAbovTopSensPress


!===============================================================
! Name:                GetEigVectMatrx
!
!
! Type:                Subroutine
!
!
! Description:  Selects the transformation matrix (or Eigenvectors matrix) 
!               from the many classes available in the cov file, 
!               depending on the sfc and atm classes selected.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - UAtm               O             Transf matrx for the Atm class selected
!       - iTypAtm            I             Atm Class index selected
!       - USfc               O             Transf matrx for the Sfc class selected
!       - iTypSfc            I             Sfc Class index selected
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

  SUBROUTINE GetEigVectMatrx(UAtm,iTypAtm,USfc,iTypSfc,iattempt)
    REAL,   DIMENSION(:,:) :: UAtm,USfc
    INTEGER                :: iTypAtm,iTypSfc,shpAtm(2),shpSfc(2),i,j,iattempt
    shpAtm(1:2)  = shape(UAtm)
    shpSfc(1:2)  = shape(USfc)
    DO i=1,shpAtm(1)
       DO j=1,shpAtm(1)
          UAtm(i,j) = GeophStatsT_Atm(iattempt)%AllEDRstats(iTypAtm)%U(i,j)
       ENDDO
    ENDDO
    DO i=1,shpSfc(1)
       DO j=1,shpSfc(1)
          USfc(i,j) = GeophStatsT_Sfc(iattempt)%AllEDRstats(iTypSfc)%U(i,j)
       ENDDO
    ENDDO
    !----Commented out to accomodate a bug in the IBM AIX compiler
    !UAtm(:,:) = GeophStatsT_Atm%AllEDRstats(iTypAtm)%U(:,:)
    !USfc(:,:) = GeophStatsT_Sfc%AllEDRstats(iTypSfc)%U(:,:)
    RETURN
  END SUBROUTINE GetEigVectMatrx


!===============================================================
! Name:                TuneCov
!
!
! Type:                Subroutine
!
!
! Description:  Tunes elements of the covariance matrix depending
!               on some tuning parameters
!
!
! Arguments:
!
!            Name                    Type            Description
!      ---------------------------------------------------
!       - Sa                        I/O          Covar matrx
!       - SfcPress                  I            Sfc Pressure
!       - nEDRselec                 I            #EDRS selected for retrieval
!       - EDR_cntrlRetr             I            Control parameters for each EDR
!       - ParamIndx                 I            Index of EDRs (within Sa)
!       - ParamLength               I            Length of EDRs (//)
!       - ParamLabel                I            Labels of EDRs
!       - EDR_nEOF                  I            # EOFs selected for each EDR
!       - Xb                        I            Background vector
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

  SUBROUTINE TuneCov(Sa,SfcPress,nEDRselec,EDR_cntrlRetr,ParamIndx,        &
                ParamLength,ParamLabel,EDR_nEOF,Xb)
    !---Input/output variables
    REAL,    DIMENSION(:,:)  :: Sa
    REAL,    DIMENSION(:)    :: Xb
    REAL                     :: SfcPress
    INTEGER                  :: nEDRselec
    INTEGER, DIMENSION(:)    :: EDR_cntrlRetr,ParamIndx,ParamLength,EDR_nEOF
    CHARACTER(LEN=*), DIMENSION(:) :: ParamLabel
    !---Local variables
    INTEGER                  :: iEDR,iParam,iG,nG
    REAL                     :: SaDiag
    !-------------------------------------------------------------------
    !   For those parameters to be retrieved (selected), the 
    !   following allow some tuning to the way the retrieval is
    !   actually performed, following the convention below:
    !   0 -> retrieve normally (based on X1st and Cov)
    !   1 -> retrieve normally but reduce variance by half.
    !   2 -> stick to Bkg (be it climo/ext depending on control file)
    !   3 -> stick to Bkg & uncorrelate with other EDRs
    !   4 -> uncorrelate with other EDRs but not stick to bkg
    !   5 -> uncorrelate with other EDRs/not stick to bkg & x 100 the diagonal matrx value
    !   6 -> uncorrelate with other EDRs/not stick to bkg & x 10  the diagonal matrx value
    !   7 -> retrieve normally but multiply variance by 2.
    !   8 -> retrieve normally but multiply variance by 5.
    !   9 -> retrieve normally but multiply variance by 10.
    !   10-> retrieve normally but multiply variance by 100.
    !-------------------------------------------------------------------
    DO iEDR=1,nEDRselec
       iG=ParamIndx(iEDR)
       nG=ParamLength(iEDR)
       !---Retrieve normally but reduce the variance by half
       IF (EDR_cntrlRetr(iEDR).EQ.1) THEN 
          DO iparam=1,nG
             Sa(iG+iparam-1,iG+iparam-1)=Sa(iG+iparam-1,iG+iparam-1)/2.
          ENDDO
       ENDIF
       !---Stick to Bkg by setting Cov (of select EDRs) to zero
       IF (EDR_cntrlRetr(iEDR).EQ.2) THEN 
          DO iparam=1,nG
             Sa(iG+iparam-1,iG+iparam-1)=0.
          ENDDO
       ENDIF
       !---Stick to Bkg and uncorrelating from other EDRs
       IF (EDR_cntrlRetr(iEDR).EQ.3) THEN 
          DO iparam=1,nG
             Sa(:,iG+iparam-1)=0.
             Sa(iG+iparam-1,:)=0.
          ENDDO
       ENDIF
       !---uncorrelate with other EDRs but not stick to bkg
       IF (EDR_cntrlRetr(iEDR).EQ.4) THEN 
          DO iparam=1,nG
             SaDiag=Sa(iG+iparam-1,iG+iparam-1)
             Sa(:,iG+iparam-1)=0.
             Sa(iG+iparam-1,:)=0.
             Sa(iG+iparam-1,iG+iparam-1)=SaDiag
          ENDDO
       ENDIF
       !---uncorrelate with other EDRs/not stick to bkg & x 100 the diagonal matrx value
       IF (EDR_cntrlRetr(iEDR).EQ.5) THEN 
          DO iparam=1,nG
             SaDiag=Sa(iG+iparam-1,iG+iparam-1)
             Sa(:,iG+iparam-1)=0.
             Sa(iG+iparam-1,:)=0.
             Sa(iG+iparam-1,iG+iparam-1)=SaDiag*100.
          ENDDO
       ENDIF
       !---uncorrelate with other EDRs/not stick to bkg & x 10 the diagonal matrx value
       IF (EDR_cntrlRetr(iEDR).EQ.6) THEN 
          DO iparam=1,nG
             SaDiag=Sa(iG+iparam-1,iG+iparam-1)
             Sa(:,iG+iparam-1)=0.
             Sa(iG+iparam-1,:)=0.
             Sa(iG+iparam-1,iG+iparam-1)=SaDiag*10.
          ENDDO
       ENDIF
       !---Retrieve normally but multiply the variance by 2
       IF (EDR_cntrlRetr(iEDR).EQ.7) THEN 
          DO iparam=1,nG
             Sa(iG+iparam-1,iG+iparam-1)=Sa(iG+iparam-1,iG+iparam-1)*2.
          ENDDO
       ENDIF
       !---Retrieve normally but multiply the variance by 5
       IF (EDR_cntrlRetr(iEDR).EQ.8) THEN 
          DO iparam=1,nG
             Sa(iG+iparam-1,iG+iparam-1)=Sa(iG+iparam-1,iG+iparam-1)*5.
          ENDDO
       ENDIF
       !---Retrieve normally but multiply the variance by 10
       IF (EDR_cntrlRetr(iEDR).EQ.9) THEN 
          DO iparam=1,nG
             Sa(iG+iparam-1,iG+iparam-1)=Sa(iG+iparam-1,iG+iparam-1)*10.
          ENDDO
       ENDIF
       !---Retrieve normally but multiply the variance by 100
       IF (EDR_cntrlRetr(iEDR).EQ.10) THEN 
          DO iparam=1,nG
             Sa(iG+iparam-1,iG+iparam-1)=Sa(iG+iparam-1,iG+iparam-1)*100.
          ENDDO
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE TuneCov


!===============================================================
! Name:                DisablRetr
!
!
! Type:                Subroutine
!
!
! Description:  Disables the retrieval by resetting the 
!               covariance matrix values
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - GeoVecSelect       I           Flag vector of which parameters to affect
!       - Sa                 I/O         Covar matrx
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

  SUBROUTINE DisablRetr(GeoVecSelect,Sa)
    !---Input/Output variables
    REAL,    DIMENSION(:,:) :: Sa
    INTEGER, DIMENSION(:)   :: GeoVecSelect
    !---Local variables
    INTEGER                 :: nParams,i
    INTEGER, DIMENSION(2)   :: shp
    nParams = SIZE(GeoVecSelect)
    shp     = shape(Sa)
    IF (nparams.ne.shp(1)) CALL ErrHandl(ErrorType,Err_InconsSiz,' in DisablRetr') 
    DO i=1,nparams
       IF (GeoVecSelect(i).EQ.0) THEN
          Sa(i,1:nParams) = 0.
          Sa(1:nParams,i) = 0.
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE DisablRetr


!===============================================================
! Name:                Set1stG2Bkg
!
!
! Type:                Subroutine
!
!
! Description:  Sets the 1st guess vector the values of the 
!               background vector
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - XbAtm              I            Atm background vector
!       - XbSfc              I            Sfc background vector
!       - X1stAtm            I/O          Atm 1st guess vector
!       - X1stSfc            I/O          Sfc 1st guess vector
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

  SUBROUTINE Set1stG2Bkg(XbAtm,XbSfc,X1stAtm,X1stSfc)
    REAL, DIMENSION(:)  :: X1stAtm,X1stSfc,XbAtm,XbSfc
    X1stAtm(:) = XbAtm(:)
    X1stSfc(:) = XbSfc(:)
    RETURN
  END SUBROUTINE Set1stG2Bkg



!===============================================================
! Name:                TuneBkgAtm
!
!
! Type:                Subroutine
!
!
! Description:  Tunes the atmospheric background vector and 
!               adjusts the 1st guess vector accordingly
!
!
! Arguments:
!
!            Name                    Type            Description
!      ---------------------------------------------------
!       - iEDR_clw                   I          Cloud Index (within indexes)
!       - iEDR_rain                  I          Rain index
!       - iEDR_snow                  I          Snow index
!       - iEDR_ice                   I          Ice index
!       - iEDR_grpl                  I          Graupel index
!       - XbAtm                      I/O        Atmospheric background vector
!       - X1stAtm                    I/O        Atmospheric 1st guess vector
!       - Indexes                    I          Indexes of all EDRs
!       - nEOF_atm                   I          Number of EOFs used for Atm EDRs
!       - labelAtm                   I          Labels of Atm EDRs
!
!
! Modules needed:
!       - ReduceDeparture
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE TuneBkgAtm(iEDR_clw,iEDR_rain,iEDR_snow,iEDR_ice, &
       iEDR_grpl,XbAtm,X1stAtm,Indexes,nEOF_atm,labelAtm,iattempt)
    REAL,             DIMENSION(:) :: X1stAtm,XbAtm
    INTEGER,          DIMENSION(:) :: Indexes,nEOF_atm
    INTEGER                        :: iEDR_clw,iEDR_rain,iEDR_snow,iattempt
    INTEGER                        :: iEDR_ice,iEDR_grpl,i,nEDRs
    CHARACTER(LEN=*), DIMENSION(:) :: LabelAtm
    !---Local variables
    ! Bkg/1stG Depart. Factor !=0.1-> amplif. =1->no reduction (neutral), N-> reduction by N, ~1000, Xb flattened to minval
    REAL                           :: FactRed_grpl=4.,FactRed_clw=2.,FactRed_rain=2.
    REAL                           :: FactRed_snow=1000.,FactRed_ice=1000.  
    REAL                           :: FactRed1stG_grpl=1.,FactRed1stG_clw=1.,FactRed1stG_rain=1.
    REAL                           :: FactRed1stG_snow=1000.,FactRed1stG_ice=1000.  
    nEDRs = size(Indexes)
    do i=1,nEDRs
       !---CLW
       IF(adjustl(trim(LabelAtm(i))).eq.'CLW' .and. nEOF_atm(i).eq. 0) THEN
          XbAtm(Indexes(iEDR_clw):Indexes(iEDR_clw)+GeophStatsT_Atm(iattempt)%nLay-1)      =-25.
          X1stAtm(Indexes(iEDR_clw):Indexes(iEDR_clw)+GeophStatsT_Atm(iattempt)%nLay-1)    =-25.
       ELSE IF (adjustl(trim(LabelAtm(i))).eq.'CLW' .and. nEOF_atm(i).ne. 0) THEN
          CALL ReduceDeparture(Indexes(iEDR_clw),GeophStatsT_Atm(iattempt)%nLay,XbAtm,FactRed_clw)
          CALL ReduceDeparture(Indexes(iEDR_clw),GeophStatsT_Atm(iattempt)%nLay,X1stAtm,FactRed1stG_clw)
       END IF
       !---Rain
       IF(adjustl(trim(LabelAtm(i))).eq.'RAIN' .and. nEOF_atm(i).eq. 0) THEN
          XbAtm(Indexes(iEDR_rain):Indexes(iEDR_rain)+GeophStatsT_Atm(iattempt)%nLay-1)    =-25.
          X1stAtm(Indexes(iEDR_rain):Indexes(iEDR_rain)+GeophStatsT_Atm(iattempt)%nLay-1)  =-25.
       ELSE IF (adjustl(trim(LabelAtm(i))).eq.'RAIN' .and. nEOF_atm(i).ne. 0) THEN
          CALL ReduceDeparture(Indexes(iEDR_rain),GeophStatsT_Atm(iattempt)%nLay,XbAtm,FactRed_rain)
          CALL ReduceDeparture(Indexes(iEDR_rain),GeophStatsT_Atm(iattempt)%nLay,X1stAtm,FactRed1stG_rain)
       END IF
       !---SNOW
       IF(adjustl(trim(LabelAtm(i))).eq.'SNOW' .and. nEOF_atm(i).eq. 0) THEN
          XbAtm(Indexes(iEDR_snow):Indexes(iEDR_snow)+GeophStatsT_Atm(iattempt)%nLay-1)    =-25.
          X1stAtm(Indexes(iEDR_snow):Indexes(iEDR_snow)+GeophStatsT_Atm(iattempt)%nLay-1)  =-25.
       ELSE IF (adjustl(trim(LabelAtm(i))).eq.'SNOW' .and. nEOF_atm(i).ne. 0) THEN
          CALL ReduceDeparture(Indexes(iEDR_snow),GeophStatsT_Atm(iattempt)%nLay,XbAtm,FactRed_snow)
          CALL ReduceDeparture(Indexes(iEDR_snow),GeophStatsT_Atm(iattempt)%nLay,X1stAtm,FactRed1stG_snow)
       END IF
       !---GRAUPEL
       IF(adjustl(trim(LabelAtm(i))).eq.'GRPL' .and. nEOF_atm(i).eq. 0) THEN
          XbAtm(Indexes(iEDR_grpl):Indexes(iEDR_grpl)+GeophStatsT_Atm(iattempt)%nLay-1)    =-25.
          X1stAtm(Indexes(iEDR_grpl):Indexes(iEDR_grpl)+GeophStatsT_Atm(iattempt)%nLay-1)  =-25.
       ELSE IF (adjustl(trim(LabelAtm(i))).eq.'GRPL' .and. nEOF_atm(i).ne. 0) THEN
          CALL ReduceDeparture(Indexes(iEDR_grpl),GeophStatsT_Atm(iattempt)%nLay,XbAtm,FactRed_grpl)
          CALL ReduceDeparture(Indexes(iEDR_grpl),GeophStatsT_Atm(iattempt)%nLay,X1stAtm,FactRed1stG_grpl)
       END IF
       !---ICE
       IF(adjustl(trim(LabelAtm(i))).eq.'ICE' .and. nEOF_atm(i).eq. 0) THEN
          XbAtm(Indexes(iEDR_ice):Indexes(iEDR_ice)+GeophStatsT_Atm(iattempt)%nLay-1)      =-25.
          X1stAtm(Indexes(iEDR_ice):Indexes(iEDR_ice)+GeophStatsT_Atm(iattempt)%nLay-1)    =-25.
       ELSE IF (adjustl(trim(LabelAtm(i))).eq.'ICE' .and. nEOF_atm(i).ne. 0) THEN
          CALL ReduceDeparture(Indexes(iEDR_ice),GeophStatsT_Atm(iattempt)%nLay,XbAtm,FactRed_ice)
          CALL ReduceDeparture(Indexes(iEDR_ice),GeophStatsT_Atm(iattempt)%nLay,X1stAtm,FactRed1stG_ice)
       END IF
    enddo    
    RETURN
  END SUBROUTINE TuneBkgAtm


!===============================================================
! Name:                ReduceDeparture
!
!
! Type:                Subroutine
!
!
! Description:  Adjusts the background vector so that the min-max
!               difference is reduced.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iG                 I              Index (within Xg) of the EDR to treat
!       - nLay               I              Number of layers
!       - Xb                 I/O            Geoph state vector
!       - Fact               I              Reduction/Amplification factor
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

  SUBROUTINE ReduceDeparture(iG,nLay,Xb,Fact)
    REAL,     DIMENSION(:)        :: Xb
    INTEGER                       :: iG,nLay
    REAL,     DIMENSION(nLay)     :: departure
    REAL                          :: Fact
    departure=Xb(iG:iG+nLay-1)-minval(Xb(iG:iG+nLay-1))
    Xb(iG:iG+nLay-1)=minval(Xb(iG:iG+nLay-1))+departure(1:nLay)/Fact
    RETURN
  END SUBROUTINE ReduceDeparture

  
!===============================================================
! Name:                SetBkg2Ext
!
!
! Type:                Subroutine
!
!
! Description:  Sets the background vector using values from the
!               External data
!
!
! Arguments:
!
!            Name                    Type            Description
!      ---------------------------------------------------
!       - Scene_ext                  I             External data Scene  
!       - ParamLabel                 I             Labels of the params
!       - ParamIndx                  I             Indexes of the params
!       - ParamLength                I             Lengths of the params
!       - nEDRs                      I             Number of EDRs
!       - ExtDataUse                 I             Flag to use or not ext data
!       - Xb                         I/O           Background vector
!       - iSpaceMode                 I             Mode of treatment of EDRs
!
!
! Modules needed:
!       - TransfScene2Xg
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE SetBkg2Ext(Scene_ext,ParamLabel,ParamIndx,ParamLength,nEDRs,&
       ExtDataUse,Xb,iSpaceMode)
    !---Input/Output variables
    TYPE(Scene_type)                      :: Scene_ext
    INTEGER                               :: nEDRs
    INTEGER,          DIMENSION(:)        :: ParamIndx,ParamLength,ExtDataUse
    INTEGER,          DIMENSION(:)        :: iSpaceMode
    REAL,             DIMENSION(:)        :: Xb
    CHARACTER(LEN=*), DIMENSION(:)        :: ParamLabel
    !---Local variables
    INTEGER                               :: iEDR,iG,nG
    REAL,             DIMENSION(SIZE(Xb)) :: Xext
    call TransfScene2Xg(Xext,Scene_Ext,ParamLabel,ParamIndx,ParamLength,iSpaceMode)
    DO iEDR=1,nEDRs
       iG=ParamIndx(iEDR)
       nG=ParamLength(iEDR)
       IF (ExtDataUse(iEDR).eq.3) THEN
          Xb(iG:iG+nG-1)=Xext(iG:iG+nG-1)
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE SetBkg2Ext


!===============================================================
! Name:                CombExtWithBkgCov
!
!
! Type:                Subroutine
!
!
! Description:  Combines external data with background info (and 1st Guess).
!
!
! Arguments:
!
!            Name                    Type            Description
!      ---------------------------------------------------
!       - Xb                       I/O          Background vector        
!       - Scene_Ext                I            External data structure
!       - X1st                     I/O          1st Guess vector
!       - ParamLabel               I            Labels of params
!       - ParamIndx                I            Indexes of params (within Xb)
!       - ParamLength              I            Lengths of params
!       - ExtDataUse               I            Flag : Extern. Data usage
!       - nEDRs                    I            Number of EDRs
!       - iSpaceModeFlag           I            Mode of EDR treatment (log, etc)
!
!
! Modules needed:
!       - TransfScene2Xg
!       - ExtX2_X1stAndXb
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE CombExtWithBkgCov(Xb,Scene_Ext,X1st,ParamLabel, &
       ParamIndx,ParamLength,ExtDataUse,nEDRs,iSpaceModeFlag,ExtDataAvailab)
    REAL,             DIMENSION(:) :: Xb,X1st
    INTEGER,          DIMENSION(:) :: ParamIndx
    INTEGER,          DIMENSION(:) :: ParamLength
    INTEGER,          DIMENSION(:) :: iSpaceModeFlag
    INTEGER,          DIMENSION(:) :: ExtDataUse
    CHARACTER(LEN=*), DIMENSION(:) :: ParamLabel
    TYPE(Scene_type)               :: Scene_Ext
    INTEGER                        :: ExtDataAvailab
    !---Local variables
    INTEGER                        :: nEDRs
    REAL, DIMENSION(SIZE(Xb))      :: Xext
    !---Use of ext data requested (otherwise, do nothing to SaAtm,SaSfc,XbAtm and XbSfc)
    IF (ExtDataAvailab .ne. 0 ) THEN
       !---Initialize
       Xext(:)=Xb(:)
       !---Get the external data in a form of Xg (vector)
       call TransfScene2Xg(Xext(:),Scene_Ext,ParamLabel(1:nEDRs),     &
            ParamIndx(1:nEDRs),ParamLength(1:nEDRs),iSpaceModeFlag(1:nEDRs))
       !---Transfer Ext data to X1st and Xb depending on ExtDataUse
       CALL ExtX2_X1stAndXb(nEDRs,ParamIndx,ParamLength,    &
            ExtDataUse,Xext,X1st,Xb)
    ENDIF
    RETURN
  END SUBROUTINE CombExtWithBkgCov


!===============================================================
! Name:                ExtX2_X1stAndXb
!
!
! Type:                Subroutine
!
!
! Description:  Adjusts Background and 1st Guess with values from 
!               External data
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nEDRs              I             Number of EDRs
!       - EDR_Indx           I             EDRs indexes (within Xb)
!       - EDR_Length         I             EDRs lengthes 
!       - ExtDataUse         I             Flag about exter. data usage
!       - Xext               I             External data vector
!       - X1st               I/O           1st Guess vector
!       - Xb                 I/O           Background vector
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

  SUBROUTINE ExtX2_X1stAndXb(nEDRs,EDR_Indx,EDR_Length,ExtDataUse,Xext,X1st,Xb)
    !---Input/Output variables
    INTEGER               :: nEDRs
    INTEGER, DIMENSION(:) :: EDR_Indx,EDR_Length,ExtDataUse
    REAL,    DIMENSION(:) :: Xext,X1st,Xb
    !---Local variables
    INTEGER               :: iEDR,iG,nG
    !---Loop over all EDRs
    DO iEDR=1,nEDRs
       iG=EDR_Indx(iEDR)
       nG=EDR_Length(iEDR)
       !----Weigh errors in combining external data with climo (EXTDATAUSE =1)
       IF (ExtDataUse(iEDR) .eq. 1) CALL ErrHandl(ErrorType,Err_NotImplemented,' combine EXT data w. climo')
       !----Replace First Guess with Extern data (EXTDATAUSE =2)
       IF (ExtDataUse(iEDR) .eq. 2) X1st(iG:iG+nG-1) = Xext(iG:iG+nG-1)
       !----Replace First Guess and Background with Extern data (EXTDATAUSE =3)
       IF (ExtDataUse(iEDR) .eq. 3) THEN  
          X1st(iG:iG+nG-1) = Xext(iG:iG+nG-1)
          Xb(iG:iG+nG-1)   = X1st(iG:iG+nG-1)
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE ExtX2_X1stAndXb


!===============================================================
! Name:                mergeSfcAndAtm
!
!
! Type:                Subroutine
!
!
! Description:  Merges covariance matrices, background vectors,
!               transformation matrixes, 1st guess vectors, 
!               (from surface and atmosphere) into a 
!               single matrix and vectors.
!
!
! Arguments:
!
!            Name                   Type       Description
!      ---------------------------------------------------
!       - SaAtm                      I         Atm. cov matrix
!       - XbAtm                      I         Atm bkgd vector
!       - UAtm                       I         Atm transf. matrx
!       - SaSfc                      I         Sfc cov matrix
!       - XbSfc                      I         Sfc bkgd vector
!       - USfc                       I         Sfc transf. matrx
!       - X1stAtm                    I         Atm. 1st Guess vector                    
!       - X1stSfc                    I         Sfc 1st Guess vector
!       - Sa                         O         Combined covar. matrx
!       - Xb                         O         Combined background vector
!       - U                          O         Combined transf. matrx
!       - X1st                       O         Combined 1st Guess vector
!       - ParamLabelAtm              I         Labels of Atmosph params
!       - ParamIndxAtm               I         Indexes of atmopsh params
!       - ParamLabelSfc              I         Labels of sfc parameters
!       - ParamIndxSfc               I         Indexes of Sfc params
!       - EDR_nEOF_atm               I         #EOFs used for Atmos. params
!       - EDR_nEOF_sfc               I         #EOFs used for Sfc params
!       - ParamLabel                 O         Combined Atm/Sfc Labels vector
!       - ParamIndx                  O         Combined Atm/Sfc Indexes vector
!       - ParamLength                O         Combined Atm/Sfc Lengths vector
!       - EDR_nEOF                   O         Combined Atm/Sfc #EOFs vector
!       - nGatm                      I         Number of Atmosph. parameters
!       - nGsfc                      I         Number of Surface parameters
!       - nGselec                    O         Number of selected params (Sfc & Atm)
!       - nEDRselec                  O         Number of selected EDRs (Sfc & Atm)
!       - EDR_cntrlRetr_atm          I         Controlling flags vector for Atmosp.
!       - EDR_cntrlRetr_sfc          I         Controlling flags vector for Surface
!       - EDR_cntrlRetr              O         Combined Atm/Sfc Contr. Flags vector
!       - EDR_ExtDataUse             O         Atm/Sfc flags vect. 4 External Data Usage
!       - EDR_ExtDataUse_atm         I         Atmosph flags vect. 4 External Data Usage
!       - EDR_ExtDataUse_sfc         I         Surface flags vect. 4 External Data Usage
!       - iSpaceModeAtm              I         Vector of how to treat Atmosph EDRs (Log, etc)
!       - iSpaceModeSfc              I         Vector of how to treat Surface EDRs (Log, etc)
!       - iSpaceModeFlag             O         Combined Atm/Sfc surface treatment mode vector 
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

  SUBROUTINE mergeSfcAndAtm(SaAtm,XbAtm,UAtm,SaSfc,XbSfc,USfc,X1stAtm,X1stSfc,Sa,Xb,&
       U,X1st,ParamLabelAtm,ParamIndxAtm,ParamLabelSfc,ParamIndxSfc,EDR_nEOF_atm,   &
       EDR_nEOF_sfc,ParamLabel,ParamIndx,ParamLength,EDR_nEOF,nGatm,nGsfc,nGselec,  &
       nEDRselec,EDR_cntrlRetr_atm,EDR_cntrlRetr_sfc,EDR_cntrlRetr,EDR_ExtDataUse,  &
       EDR_ExtDataUse_atm,EDR_ExtDataUse_sfc,iSpaceModeAtm,iSpaceModeSfc,iSpaceModeFlag)
    REAL,             DIMENSION(:,:) :: SaAtm,SaSfc,Sa,U,UAtm,USfc
    REAL,             DIMENSION(:)   :: XbAtm,XbSfc,X1stAtm,X1stSfc,Xb,X1st
    INTEGER,          DIMENSION(:)   :: ParamIndxAtm,ParamIndxSfc,ParamIndx,ParamLength
    INTEGER,          DIMENSION(:)   :: EDR_nEOF_atm,EDR_nEOF_sfc,EDR_nEOF,nGatm,nGsfc
    INTEGER,          DIMENSION(:)   :: EDR_cntrlRetr_atm,EDR_cntrlRetr_sfc,EDR_cntrlRetr
    INTEGER,          DIMENSION(:)   :: EDR_ExtDataUse,EDR_ExtDataUse_atm,EDR_ExtDataUse_sfc
    INTEGER,          DIMENSION(:)   :: iSpaceModeAtm,iSpaceModeSfc,iSpaceModeFlag
    CHARACTER(LEN=*), DIMENSION(:)   :: ParamLabelAtm,ParamLabelSfc,ParamLabel
    INTEGER                          :: natm,nsfc,nEDRs_atm,nEDRs_sfc
    INTEGER                          :: nGselec,nGselecAtm,nGselecSfc,nEDRselec,i,j
    !---Local variables
    INTEGER,          DIMENSION(:), ALLOCATABLE :: filterAtm,filterSfc
    !----Initialization
    nAtm                          = Size(XbAtm)
    nSfc                          = Size(XbSfc)
    nEDRs_atm                     = size(ParamLabelAtm)
    nEDRs_sfc                     = size(ParamLabelSfc)
    Sa                            = 0.
    U                             = 0.
    iSpaceModeFlag                = -1
    !----Determine Atmopsheric filter to keep only selected EDRs
    ALLOCATE(filterAtm(nAtm),filterSfc(nSfc))
    nEDRselec = 0
    nGselec   = 0
    DO i=1,nEDRs_atm
       IF (EDR_nEOF_atm(i) .ne. 0) THEN
          nEDRselec                = nEDRselec +1
          nGselec                  = nGselec + nGatm(i)
          !---Merge the labels/ndexes of the parameters
          ParamLabel(nEDRselec)    = ParamLabelAtm(i)
          ParamIndx(nEDRselec)     = nGselec - nGatm(i) +1
          ParamLength(nEDRselec)   = nGatm(i)
          iSpaceModeFlag(nEDRselec)= iSpaceModeAtm(i)
          EDR_nEOF(nEDRselec)      = EDR_nEOF_atm(i)
          EDR_cntrlRetr(nEDRselec) = EDR_cntrlRetr_atm(i)
          EDR_ExtDataUse(nEDRselec)= EDR_ExtDataUse_atm(i)
          Do j=1,nGatm(i)
             filterAtm(ParamIndx(nEDRselec)+j-1)=ParamIndxAtm(i)+j-1
          Enddo
       ENDIF
    ENDDO
    Xb(1:nGselec)           = XbAtm(filterAtm(1:nGselec))
    X1st(1:nGselec)         = X1stAtm(filterAtm(1:nGselec))
    Sa(1:nGselec,1:nGselec) = SaAtm(filterAtm(1:nGselec),filterAtm(1:nGselec))
    U(1:nGselec,1:nGselec)  = UAtm(filterAtm(1:nGselec),filterAtm(1:nGselec))
    nGselecAtm              = nGselec
    !----Do the same cumulatively to the surface component
    nGselecSfc   = 0
    DO i=1,nEDRs_sfc
       IF (EDR_nEOF_sfc(i) .ne. 0) THEN
          nEDRselec                = nEDRselec +1
          nGselec                  = nGselec   +nGsfc(i)
          nGselecSfc               = nGselecSfc   +nGsfc(i)
          !---Merge the labels/ndexes of the parameters
          ParamLabel(nEDRselec)    = ParamLabelSfc(i)
          ParamIndx(nEDRselec)     = nGselec - nGsfc(i) +1
          ParamLength(nEDRselec)   = nGsfc(i)
          iSpaceModeFlag(nEDRselec)= iSpaceModeSfc(i)
          EDR_nEOF(nEDRselec)      = EDR_nEOF_sfc(i)
          EDR_cntrlRetr(nEDRselec) = EDR_cntrlRetr_sfc(i)
          EDR_ExtDataUse(nEDRselec)= EDR_ExtDataUse_sfc(i)
          Do j=1,nGsfc(i)
             filterSfc(ParamIndx(nEDRselec)-nGselecAtm+j-1)=ParamIndxSfc(i)+j-1
          Enddo
       ENDIF
    ENDDO
    Xb(nGselecAtm+1:nGselec)                      = XbSfc(filterSfc(1:nGselecSfc))
    X1st(nGselecAtm+1:nGselec)                    = X1stSfc(filterSfc(1:nGselecSfc))
    Sa(nGselecAtm+1:nGselec,nGselecAtm+1:nGselec) = SaSfc(filterSfc(1:nGselecSfc),filterSfc(1:nGselecSfc))
    U(nGselecAtm+1:nGselec,nGselecAtm+1:nGselec)  = USfc(filterSfc(1:nGselecSfc),filterSfc(1:nGselecSfc))
    RETURN
  END SUBROUTINE mergeSfcAndAtm


!===============================================================
! Name:                ShrkU
!
!
! Type:                Subroutine
!
!
! Description:  Shrinks the transformation matrix U into a smaller
!               matrix containing only the parameters that were
!               selected for retrieval and for the number of EOFs
!               chosen.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - ParamLabel         I            Labels of parameters (Temp, WVap, etc)
!       - ParamIndx          I            Indexes of parameters (within U)
!       - ParamLength        I            Lengths of the parameters 
!       - U                  I            Transformation matrix
!       - Ustar              O            reduced (shrinked) transf. matrix
!       - EDR_nEOF           I            Number of EOFs chosen for each EDR
!       - TunParams          I            Tuning structure that contains all
!                                         tuning parameters
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

  SUBROUTINE ShrkU(ParamLabel,ParamIndx,ParamLength,U,Ustar,EDR_nEOF,TunParams)   
    !---Input/Output variables
    TYPE(TunParams_type)                        :: TunParams
    CHARACTER(LEN=*), DIMENSION(:)              :: ParamLabel
    REAL,             DIMENSION(:,:)            :: U,Ustar
    INTEGER,          DIMENSION(:)              :: ParamIndx,ParamLength
    INTEGER,          DIMENSION(:)              :: EDR_nEOF
    !---Local variables
    INTEGER,          DIMENSION(:), ALLOCATABLE :: IndexVec
    INTEGER,          DIMENSION(2)              :: shU,shUstar
    INTEGER                                     :: iEDR,ieof,i,nEDRs
    
    shU     = shape(U)
    shUstar = shape(Ustar)
    nEDRs   = size(ParamIndx)
    ALLOCATE(IndexVec(shUstar(2)))
    !---Consistency check
    IF (shU(1).ne.shUstar(1)) CALL ErrHandl(ErrorType,Err_DimensPb,'U and Ustar-1-') 
    IF (shU(2).lt.shUstar(2)) CALL ErrHandl(ErrorType,Err_DimensPb,'U and Ustar-2-') 
    !---Built the Index vector (used to shrink U)(in descending order because IDL 
    !   is used in performing the EOF decomposition (EOFs produced in ascending order)
    i=0
    DO iEDR=1,nEDRs
       DO ieof=1,EDR_nEOF(iEDR)
          i=i+1
          IndexVec(i) = ParamIndx(iEDR) + ParamLength(iEDR) -iEOF
       ENDDO
    ENDDO
    !---Get Ustar as a subset of U
    Ustar(1:shUstar(1),1:shUstar(2)) = U(1:shU(1),IndexVec(1:shUstar(2)))
    DEALLOCATE(IndexVec)
    RETURN
  END SUBROUTINE ShrkU

!===============================================================
! Name:                ProjCov
!
!
! Type:                Subroutine
!
!
! Description:  Projects the covariance matrix Sa into an EOF-based
!               space (matrix Lambda) using the transformation matrix
!               Ustar.
!
!
! Arguments:
!
!      Name                 Type          Description
!      ---------------------------------------------------
!       - nR                 I            Number of retrieval parameters (EOFs)
!       - nG                 I            Number of geophysical parameters
!       - Ustar              I            Transformation matrix
!       - Sa                 I            Geophysical covariance matrix
!       - Lambda             O            Resulting EOF-based covariance matrix
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
        
  SUBROUTINE ProjCov(nR,nG,Ustar,Sa,Lambda)
    !---Input/Output variables
    INTEGER              :: nR,nG
    REAL, DIMENSION(:,:) :: Ustar
    REAl, DIMENSION(:,:) :: Sa
    REAL, DIMENSION(:,:) :: Lambda
    !---Local variables
    REAl, DIMENSION(nR,nG) :: UstarT
    REAL, DIMENSION(nG,nR) :: A
    UstarT = TRANSPOSE(Ustar(1:nG,1:nR))
    A      = MATMUL(Sa(1:nG,1:nG),Ustar(1:nG,1:nR))
    Lambda(1:nR,1:nR)=MATMUL(UstarT,A)
    RETURN
  END SUBROUTINE ProjCov

!===============================================================
! Name:                LoadBkgAtm_Ext
!
!
! Type:                Subroutine
!
!
! Description:  Reads/loads atmospheric backgroud database with spatial and temporal visibilities.
!
!
! Arguments:  None
!
!
! Modules needed:
!       - Nono
!
!
! History:
!       03-25-2013      Pan Liang, AER Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE LoadBkgAtm_Ext()
    INTEGER                  :: nZ,nLat,nLon,nHr,nDay,nType
    INTEGER                  :: iu,iLat,iLon,iHr,iDay,iTyp
    INTEGER                  :: iEDR,jEDR
    iu=get_lun()
    OPEN(iu,file=CntrlConfig%BkgAtmFile_Ext,status='old',&
         form='formatted')
    READ(iu,'(a)')    
    READ(iu,'(a)')    
    !---Read the hearder
    READ(iu,'(25x,i8)') bkgAtm_Ext%nEDRs   
    ALLOCATE(bkgAtm_Ext%EDR_IDs(bkgAtm_Ext%nEDRs))
    ALLOCATE(bkgAtm_Ext%EDR_Desc(bkgAtm_Ext%nEDRs))
    ALLOCATE(bkgAtm_Ext%iSpaceMode(bkgAtm_Ext%nEDRs))
    READ(iu,'(25x,10i8)') bkgAtm_Ext%EDR_IDs(1:bkgAtm_Ext%nEDRs)   
    READ(iu,'(25x,10a8)') bkgAtm_Ext%EDR_Desc(1:bkgAtm_Ext%nEDRs)
    READ(iu,'(25x,10i8)') bkgAtm_Ext%iSpaceMode(1:bkgAtm_Ext%nEDRs)
    READ(iu,'(a)') 
    READ(iu,'(25x,10i8)') bkgAtm_Ext%nZ,bkgAtm_Ext%nLat,bkgAtm_Ext%nLon,&
         bkgAtm_Ext%nHr,bkgAtm_Ext%nDay,bkgAtm_Ext%nTypes
    ALLOCATE(bkgAtm_Ext%julDay(bkgAtm_Ext%nDay))
    ALLOCATE(bkgAtm_Ext%Type_IDs(bkgAtm_Ext%nTypes))
    ALLOCATE(bkgAtm_Ext%Type_Desc(bkgAtm_Ext%nTypes))
    READ(iu,'(25x,3f10.3)') bkgAtm_Ext%Lat1,bkgAtm_Ext%LatN,bkgAtm_Ext%dLat
    READ(iu,'(25x,3f10.3)') bkgAtm_Ext%Lon1,bkgAtm_Ext%LonN,bkgAtm_Ext%dLon
    READ(iu,'(25x,3i8)') bkgAtm_Ext%Hr1,bkgAtm_Ext%HrN,bkgAtm_Ext%dHr
    READ(iu,'(25x,12i8)') bkgAtm_Ext%julDay(1:bkgAtm_Ext%nDay)
    READ(iu,'(25x,10i8)') bkgAtm_Ext%Type_IDs(1:bkgAtm_Ext%nTypes)   
    READ(iu,'(25x,10a8)') bkgAtm_Ext%Type_Desc(1:bkgAtm_Ext%nTypes)
    !---Read pressure info
    READ(iu,'(25x,i8)')  bkgAtm_Ext%nLev
    READ(iu,'(a)')    
    ALLOCATE(bkgAtm_Ext%pres_lev(bkgAtm_Ext%nLev))
    READ(iu,'(10f10.3)') bkgAtm_Ext%pres_lev
    READ(iu,'(25x,i8)')  bkgAtm_Ext%nLay
    READ(iu,'(a)')    
    ALLOCATE(bkgAtm_Ext%pres_lay(bkgAtm_Ext%nLay))
    READ(iu,'(10f10.3)') bkgAtm_Ext%pres_lay
    !--- Read stats
    !--- At same time, also supplement background variables, with following grids repeated:
    !--- 1.First and last longitude repeated (wrap-around at 0/360 deg)
    !---   This is to support interpolation to exact lat/lon of scene
    !--- 2.First and last UTC hour repeated (wraparound at 0/24 UTC)
    !--- 3.First and last Julian day repeated (wraparound at julian day 15/370

    READ(iu,'(a)')
    ALLOCATE(bkgAtm_Ext%Stats(bkgAtm_Ext%nZ,bkgAtm_Ext%nLat,bkgAtm_Ext%nLon+1,&
         bkgAtm_Ext%nHr+1,bkgAtm_Ext%nDay+1,bkgAtm_Ext%nTypes))
    DO iTyp=1,bkgAtm_Ext%nTypes
       DO iDay=1,bkgAtm_Ext%nDay
          DO iHr=1,bkgAtm_Ext%nHr
             DO iLon=1,bkgAtm_Ext%nLon
                DO iLat=1,bkgAtm_Ext%nLat
                   READ(iu,'(10f10.4)')bkgAtm_Ext%Stats(:,iLat,iLon,iHr,iDay,iTyp)
                ENDDO
                if(iLon .eq. bkgAtm_Ext%nLon)bkgAtm_Ext%Stats(:,:,iLon+1,iHr,iDay,iTyp)=bkgAtm_Ext%Stats(:,:,1,iHr,iDay,iTyp)
             ENDDO
             if(iHr .eq. bkgAtm_Ext%nHr)bkgAtm_Ext%Stats(:,:,:,iHr+1,iDay,iTyp)=bkgAtm_Ext%Stats(:,:,:,1,iDay,iTyp)
          ENDDO
          if(iDay .eq. bkgAtm_Ext%nDay)bkgAtm_Ext%Stats(:,:,:,:,iDay+1,iTyp)=bkgAtm_Ext%Stats(:,:,:,:,1,iTyp)
       ENDDO
    ENDDO
    CLOSE(iu)

    ! test: make sure values were correctly duplicated

!    print *,'bkgAtm_Ext%Stats(:,1,1,1,1)',bkgAtm_Ext%Stats(:,1,1,1,1,1)
!    print *,'bkgAtm_Ext%Stats(:,1,bkgAtm_Ext%nLon+1,bkgAtm_Ext%nHr+1,bkgAtm_Ext%nDay+1)',&
!         bkgAtm_Ext%Stats(:,1,bkgAtm_Ext%nLon+1,bkgAtm_Ext%nHr+1,bkgAtm_Ext%nDay+1,1)
!    print *,'Diff: ',bkgAtm_Ext%Stats(:,1,1,1,1,1)-&
!         bkgAtm_Ext%Stats(:,1,bkgAtm_Ext%nLon+1,bkgAtm_Ext%nHr+1,bkgAtm_Ext%nDay+1,1)

    !---Locate IG and NG for mapping to the original background EDR
    ALLOCATE(bkgAtm_Ext%mapEDR(2,bkgAtm_Ext%nEDRs))
    bkgAtm_Ext%mapEDR=-1
    DO iEDR = 1, bkgAtm_Ext%nEDRs
       DO jEDR = 1,GeophStatsT_Atm(1)%nEDRs
          IF (GeophStatsT_Atm(1)%EDR_Desc(jEDR) .EQ.  bkgAtm_Ext%EDR_Desc(iEDR)) THEN
             bkgAtm_Ext%mapEDR(1,iEDR)=GeophStatsT_Atm(1)%EDR_IDs(jEDR)
             bkgAtm_Ext%mapEDR(2,iEDR)=GeophStatsT_Atm(1)%Stats(jEDR,1)%npEDR
          ENDIF
       ENDDO
    ENDDO

    RETURN
  END SUBROUTINE  LoadBkgAtm_Ext

!===============================================================
! Name:                GetBkgAtm_Ext
!
!
! Type:                Subroutine
!
!
! Description:  Selects the right atm  background, depending on 
!               the laction, time and atm classes 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - XbAtm_Ext          0              External atmospheric background vector
!       - lat                I              Latitude
!       - lat                I              Longitude
!       - julDay             I              Julday
!       - secs               I              Seconds within the day
!       - iTypAtm            I              Atmospheric class selected
!       - interpStats        I              Interpolate in space and time to scene 
!                                           (1=yes; 2=no, use nearest neighbor)
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-26-2013      Pan Liang, AER Inc @ NOAA/NESDIS/STAR
!       03-26-2013      C. Grassotti, AER Inc @ NOAA/NESDIS/STAR
!                       Modified to include option for temporal and spatial interpolation
!
!===============================================================

  SUBROUTINE GetBkgAtm_Ext(XbAtm_Ext,lat,lon,julDay,secs,iTypAtm,&
       interpStats)
    REAL,     DIMENSION(:)   :: XbAtm_Ext
    REAL                     :: lat,lon,secs
    INTEGER                  :: julDay,year,iTypAtm
    INTEGER                  :: interpStats
    REAL                     :: hr,day,lon360
    INTEGER                  :: ilat,ilat0,ilat1,ilon,ilon0,ilon1
    INTEGER                  :: ihr,ihr0,ihr1,iday,iday0,iday1
    INTEGER,  DIMENSION(mxNday_bkgAtm_Ext+1)  :: deltaDay
    INTEGER,  DIMENSION(1)   :: idayArr,jdayArr

    INTEGER                  :: ilev
    REAL                     :: Lat0,Lat1,Lon0,Lon1
    REAL                     :: fracLat,fracLon,fracHour,fracDay,juldayP
    REAL                     :: hr0,hr1,secs24
    REAL, DIMENSION(2)       :: StatIntInLat
    REAL, DIMENSION(2)       :: StatIntInHr
    REAL, DIMENSION(2)       :: StatIntInDay
    REAL                     :: StatsInt1,StatsInt2,StatsInt3,StatsInt4
    REAL, DIMENSION(:), ALLOCATABLE :: StatsInt
    REAL, DIMENSION(mxNday_bkgAtm_Ext+1) :: juldayArr

    !---NB: For spatial and temporal interpolation option,
    !---assumes the database has been enlarged with space and time wraparound to
    !---allow interpolation at grid boundaries

    !---Fill julian day array needed for time interpolation option
    juldayArr(1:bkgAtm_Ext%NDay)=REAL(bkgAtm_Ext%julday(1:bkgAtm_Ext%NDay))
    juldayArr(bkgAtm_Ext%NDay+1)=juldayArr(1)+365.
!    print *,'juldayArr(1:bkgAtm_Ext%NDay+1)=',juldayArr(1:bkgAtm_Ext%NDay+1)

    ! test: manually reset space/time params to make sure interpolation is robust
!    julday=1
!    secs=86440.

    !---Find the nearest spatial and temporal grid indexes
    IF (lon .LT. 0) THEN 
       lon360 = lon+360.
    ELSE
       lon360 = lon
    ENDIF
    if(interpStats .eq. 0)then
!       print *,'lat,lon,secs,secs/3600.,julday=',lat,lon,secs,secs/3600.,julday
       !---Nearest neighbor spatial
       ilat = (lat - bkgAtm_Ext%Lat1)/bkgAtm_Ext%dLat + 1.5  ! +1: start from 1, +0.5 to the nearest neighbor
       ilon = (lon360 - bkgAtm_Ext%Lon1)/bkgAtm_Ext%dLon + 1.5
       IF (ilon .GT. bkgAtm_Ext%nLon) ilon=1           ! cross 360 Deg
       ihr = (secs/3600. - bkgAtm_Ext%Hr1)/bkgAtm_Ext%dHr + 1.5

       deltaDay(1:bkgAtm_Ext%NDay) = ABS(julday - bkgAtm_Ext%julday(1:bkgAtm_Ext%NDay))
       idayArr = MINLOC(deltaDay(1:bkgAtm_Ext%NDay))
       iday = idayArr(1)

!       print *,'ilat,ilon,ihr,iday=',ilat,ilon,ihr,iday

    !---Fill the background vector
       XbAtm_Ext(1:bkgAtm_Ext%nZ) = bkgAtm_Ext%Stats(1:bkgAtm_Ext%nZ,ilat,ilon,ihr,iday,iTypAtm)

    else
       !---Spatial interpolation
       secs24=secs
       if (secs .ge. 86400.)secs24=secs-86400.
!       print *,'lat,lon,secs,secs24,secs24/3600.,julday=',lat,lon,secs,secs24,secs24/3600.,julday
       ALLOCATE(StatsInt(bkgAtm_Ext%nZ))
       ilat0 = (lat - bkgAtm_Ext%Lat1)/bkgAtm_Ext%dLat + 1.    ! get southern bounding y grid coordinate
       ilat1 = ilat0+1                                        !     northern bounding y grid coordinate
       ilon0 = (lon360 - bkgAtm_Ext%Lon1)/bkgAtm_Ext%dLon + 1. ! get western bounding x grid coordinate
       ilon1 = ilon0+1                                        !     eastern bounding x grid coordinate
       Lat0 = bkgAtm_Ext%Lat1+(ilat0-1)*bkgAtm_Ext%dLat
       Lat1 = bkgAtm_Ext%Lat1+(ilat1-1)*bkgAtm_Ext%dLat
       Lon0 = bkgAtm_Ext%Lon1+(ilon0-1)*bkgAtm_Ext%dLon
       Lon1 = bkgAtm_Ext%Lon1+(ilon1-1)*bkgAtm_Ext%dLon
       if(Lon0 .lt. 0.)Lon0=Lon0+360.
       if(Lon1 .lt. 0.)Lon1=Lon1+360.
       fracLat = (lat-Lat0)/(Lat1-Lat0)
       fracLon = (lon360-Lon0)/(Lon1-Lon0)
       
!       print *,'ilat0,ilat1,ilon0,ilon1=',ilat0,ilat1,ilon0,ilon1
!       print *,'Lat0,Lat1,Lon0,Lon1=',Lat0,Lat1,Lon0,Lon1
!       print *,'fracLat,fracLon=',fracLat,fracLon
       !---Time interpolation
       fracHour=0.
          ihr = (secs/3600. - bkgAtm_Ext%Hr1)/bkgAtm_Ext%dHr + 1.5
       ihr0 = (secs24/3600. - bkgAtm_Ext%Hr1)/bkgAtm_Ext%dHr + 1
       ihr1 = ihr0+1
       hr0 = bkgAtm_Ext%Hr1+(ihr0-1)*bkgAtm_Ext%dHr
       hr1 = bkgAtm_Ext%Hr1+(ihr1-1)*bkgAtm_Ext%dHr
       fracHour = ((secs24/3600.)- hr0)/(hr1-hr0)
!       print *,'secs24/3600,ihr0,ihr1,hr0,hr1,fracHour=',secs24/3600,ihr0,ihr1,hr0,hr1,fracHour
       juldayP=julday
       if(julday .lt. bkgAtm_Ext%julday(1))&
            juldayP=juldayArr(bkgAtm_Ext%NDay+1)-(juldayArr(1)-REAL(julday))
       deltaDay(1:bkgAtm_Ext%NDay+1) = juldayP - juldayArr(1:bkgAtm_Ext%NDay+1)
       jdayArr = MINLOC(deltaDay(1:bkgAtm_Ext%NDay+1),MASK=deltaDay(1:bkgAtm_Ext%NDay+1) .ge. 0.)
       iday = jdayArr(1)
       iday0 = jdayArr(1)
       iday1 = iday0+1
       fracDay = REAL(juldayP-juldayArr(iday0))/REAL(juldayArr(iday1)-juldayArr(iday0))
!       print *,'julday,juldayP,iday0,iday1,fracDay=',julday,juldayP,iday0,iday1,fracDay

!       stop

       do ilev=1,bkgAtm_Ext%nZ

          !---Interpolation for stored background julian day index iday0
          !---At stored hour index ihr0 
          !---At stored longitude index ilon0
          StatIntInLat(1) = bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr0,iday0,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon0,ihr0,iday0,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr0,iday0,iTypAtm))

          !---At stored longitude index ilon1
          StatIntInLat(2) = bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr0,iday0,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon1,ihr0,iday0,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr0,iday0,iTypAtm))

          StatsInt1 = StatIntInLat(1)+fracLon*(StatIntInLat(2)-StatIntInLat(1))
          
          !---At stored hour index ihr1 
          !---At stored longitude index ilon0
          StatIntInLat(1) = bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr1,iday0,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon0,ihr1,iday0,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr1,iday0,iTypAtm))
          
          !---At stored longitude index ilon1
          StatIntInLat(2) = bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr1,iday0,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon1,ihr1,iday0,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr1,iday0,iTypAtm))
          
          !---Interpolate between ilon0 and ilon1
          StatsInt2 = StatIntInLat(1)+fracLon*(StatIntInLat(2)-StatIntInLat(1))
          StatsInt3 = StatsInt1+fracHour*(StatsInt2-StatsInt1)          

 
          !---Interpolation for stored background julian day index iday1
          !---At stored hour index ihr0 
          !---At stored longitude index ilon0
          StatIntInLat(1) = bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr0,iday1,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon0,ihr0,iday1,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr0,iday1,iTypAtm))

          !---At stored longitude index ilon1
          StatIntInLat(2) = bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr0,iday1,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon1,ihr0,iday1,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr0,iday1,iTypAtm))

          StatsInt1 = StatIntInLat(1)+fracLon*(StatIntInLat(2)-StatIntInLat(1))
          
          !---At stored hour index ihr1 
          !---At stored longitude index ilon0
          StatIntInLat(1) = bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr1,iday1,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon0,ihr1,iday1,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon0,ihr1,iday1,iTypAtm))
          
          !---At stored longitude index ilon1
          StatIntInLat(2) = bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr1,iday1,iTypAtm)+fracLat* &
               (bkgAtm_Ext%Stats(ilev,ilat1,ilon1,ihr1,iday1,iTypAtm) - &
               bkgAtm_Ext%Stats(ilev,ilat0,ilon1,ihr1,iday1,iTypAtm))
          
          !---Interpolate between ilon0 and ilon1
          StatsInt2 = StatIntInLat(1)+fracLon*(StatIntInLat(2)-StatIntInLat(1))
          StatsInt4 = StatsInt1+fracHour*(StatsInt2-StatsInt1)          

          !---Final interpolation between stored julian days to observation julian day          

          StatsInt(ilev) = StatsInt3+fracDay*(StatsInt4-StatsInt3)

       enddo

    !---Fill the background vector
       XbAtm_Ext(1:bkgAtm_Ext%nZ) = StatsInt(1:bkgAtm_Ext%nZ)
!       print *,'XbAtm_Ext(1:bkgAtm_Ext%nZ)=',XbAtm_Ext(1:bkgAtm_Ext%nZ)

       DEALLOCATE(StatsInt)
!       stop

    endif
!    DEALLOCATE(JdayArr)
!    print *,'XbAtm_Ext(1:bkgAtm_Ext%nZ)=',XbAtm_Ext(1:bkgAtm_Ext%nZ)
!    stop

   RETURN
  END SUBROUTINE GetBkgAtm_Ext

!===============================================================
! Name:                ReplaceBkgAtm_Ext
!
!
! Type:                Subroutine
!
!
! Description:   Replace the atmospheric background vector 
!                by external database
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - XbAtm              I/0            Atmospheric background vector
!       - XbAtm_Ext          I              External atmospheric background vector
!
! Modules needed:
!       - None
!
!
! History:
!       03-26-2013      Pan Liang, AER Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE ReplaceBkgAtm_Ext(XbAtm,XbAtm_Ext)
    REAL,     DIMENSION(:)   :: XbAtm,XbAtm_Ext
    INTEGER                  :: iEDR,iG,nG
    DO iEDR = 1, bkgAtm_Ext%nEDRs
       iG = bkgAtm_Ext%mapEDR(1,iEDR)
       nG = bkgAtm_Ext%mapEDR(2,iEDR)
       XbAtm(iG:iG+nG-1) = XbAtm_Ext(bkgAtm_Ext%EDR_IDs(iEDR):bkgAtm_Ext%EDR_IDs(iEDR)+nG-1)
    ENDDO
   RETURN
 END SUBROUTINE ReplaceBkgAtm_Ext

!===============================================================
! Name:                Destroy_BkgAtm_Ext
!
!
! Type:                Subroutine
!
!
! Description:  Release memory allocated
!
!
! Arguments:    None
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-26-2013      Pan Liang, AER Inc @ NOAA/NESDIS/STAR
!
!===============================================================
        
  SUBROUTINE Destroy_BkgAtm_Ext()
    
    DEALLOCATE(bkgAtm_Ext%EDR_IDs)
    DEALLOCATE(bkgAtm_Ext%EDR_Desc)
    DEALLOCATE(bkgAtm_Ext%iSpaceMode)
    DEALLOCATE(bkgAtm_Ext%Type_IDs)
    DEALLOCATE(bkgAtm_Ext%Type_Desc)
    DEALLOCATE(bkgAtm_Ext%pres_lev)
    DEALLOCATE(bkgAtm_Ext%pres_lay)
    DEALLOCATE(bkgAtm_Ext%Stats)
    DEALLOCATE(bkgAtm_Ext%mapEDR)
    RETURN
  END SUBROUTINE Destroy_BkgAtm_Ext

!===============================================================
! Name:                Destroy_GeophStatsT_Atm
!
!
! Type:                Subroutine
!
!
! Description:  Release memory allocated
!
!
! Arguments:
!
!      Name                 Type          Description
!      ---------------------------------------------------
!       - GeophStatsT_Atm    I            type to be deallocated
!
!
! Modules needed:
!       - None
!
!
! History:
!       07-22-2008      Wanchun Chen
!
!===============================================================
        
  SUBROUTINE Destroy_GeophStatsT_Atm( GeophStatsT_Atm )
    TYPE(CovBkgTrnsf_type) :: GeophStatsT_Atm
    INTEGER                :: iTyp,iEDR
    
    DEALLOCATE(GeophStatsT_Atm%EDR_IDs)
    DEALLOCATE(GeophStatsT_Atm%EDR_Desc)
    DEALLOCATE(GeophStatsT_Atm%iSpaceMode)
    DEALLOCATE(GeophStatsT_Atm%AbsorbID)
    DEALLOCATE(GeophStatsT_Atm%Type_IDs)
    DEALLOCATE(GeophStatsT_Atm%Type_Desc)
    DEALLOCATE(GeophStatsT_Atm%pres_lev)
    DEALLOCATE(GeophStatsT_Atm%pres_lay)

    TypLoop: DO iTyp=1,GeophStatsT_Atm%nTypes
      EDRLoop: DO iEDR=1,GeophStatsT_Atm%nEDRs
        DEALLOCATE(GeophStatsT_Atm%Stats(iEDR,iTyp)%Sa)
        DEALLOCATE(GeophStatsT_Atm%Stats(iEDR,iTyp)%U)
        DEALLOCATE(GeophStatsT_Atm%Stats(iEDR,iTyp)%Xb)
      ENDDO EDRLoop
      DEALLOCATE(GeophStatsT_Atm%AllEDRstats(iTyp)%Sa)
      DEALLOCATE(GeophStatsT_Atm%AllEDRstats(iTyp)%U)
      DEALLOCATE(GeophStatsT_Atm%AllEDRstats(iTyp)%Xb)
    ENDDO TypLoop

    DEALLOCATE(GeophStatsT_Atm%Stats)
    DEALLOCATE(GeophStatsT_Atm%AllEDRstats)

    RETURN
  END SUBROUTINE Destroy_GeophStatsT_Atm

!===============================================================
! Name:                Destroy_GeophStatsT_Sfc
!
!
! Type:                Subroutine
!
!
! Description:  Release memory allocated
!
!
! Arguments:
!
!      Name                 Type          Description
!      ---------------------------------------------------
!       - GeophStatsT_Sfc    I            type to be deallocated
!
!
! Modules needed:
!       - None
!
!
! History:
!       07-22-2008      Wanchun Chen
!
!===============================================================
        
  SUBROUTINE Destroy_GeophStatsT_Sfc( GeophStatsT_Sfc )
    TYPE(CovBkgTrnsf_type) :: GeophStatsT_Sfc
    INTEGER                :: iTyp,iEDR
    
    DEALLOCATE(GeophStatsT_Sfc%EDR_IDs)
    DEALLOCATE(GeophStatsT_Sfc%EDR_Desc)
    DEALLOCATE(GeophStatsT_Sfc%iSpaceMode)
    DEALLOCATE(GeophStatsT_Sfc%AbsorbID)
    DEALLOCATE(GeophStatsT_Sfc%Type_IDs)
    DEALLOCATE(GeophStatsT_Sfc%Type_Desc)
    DEALLOCATE(GeophStatsT_Sfc%Freq)
    DEALLOCATE(GeophStatsT_Sfc%polar)

    TypLoop: DO iTyp=1,GeophStatsT_Sfc%nTypes
      EDRLoop: DO iEDR=1,GeophStatsT_Sfc%nEDRs
        DEALLOCATE(GeophStatsT_Sfc%Stats(iEDR,iTyp)%Sa)
        DEALLOCATE(GeophStatsT_Sfc%Stats(iEDR,iTyp)%U)
        DEALLOCATE(GeophStatsT_Sfc%Stats(iEDR,iTyp)%Xb)
      ENDDO EDRLoop
      DEALLOCATE(GeophStatsT_Sfc%AllEDRstats(iTyp)%Sa)
      DEALLOCATE(GeophStatsT_Sfc%AllEDRstats(iTyp)%U)
      DEALLOCATE(GeophStatsT_Sfc%AllEDRstats(iTyp)%Xb)
    ENDDO TypLoop

    DEALLOCATE(GeophStatsT_Sfc%Stats)
    DEALLOCATE(GeophStatsT_Sfc%AllEDRstats)

    RETURN
  END SUBROUTINE Destroy_GeophStatsT_Sfc


END MODULE GeophCovBkg
