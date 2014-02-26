!===============================================================
! Name:      fmsdr2heriAlgoRetr 
!
!
! Type:         Main Program
!
!
! Description:
!	Calculate hydrometeors parameters using heritage algorithms
!    	from SSMIS footprintmatched data.
!	
!
! Modules needed:
!       - USE CntrlParams
!       - USE IO_MeasurData
!       - USE IO_Scene
!       - USE QCchecking
!       - USE misc
!       - USE ssmis_heriAlgors
!       - USE ErrorHandling
!
! History:
!     	2006/07/18     Ninghai Sun    Create original program.
!	2006/08/04     Ninghai Sun    Add emissivity and Tskin retrieval.
!
!===============================================================

PROGRAM fmsdr2heriAlgoRetr
USE CntrlParams
USE IO_MeasurData
USE IO_Scene
USE QCchecking
USE misc
USE ssmis_heriAlgors
USE ErrorHandling
IMPLICIT NONE

  INTEGER, PARAMETER :: nqc=1, iTyp=1, nLev=1, nLay=1, nAbsorb=2
  INTEGER, PARAMETER :: nParmCLW=1, nParmRain=1, nParmSnow=1, nParmIce=1, nParmGrpl=1
  REAL, DIMENSION(nLay) :: pres_lay=-999.0
  REAL, DIMENSION(nLev) :: pres_lev=-999.0
  INTEGER, DIMENSION(nAbsorb) :: AbsorbID=(/1,3/) ! 1->H2O,3->O3; must have in EDR

  !---retrieval space vectors
  TYPE(Scene_type) :: Scene
  
  !---Radiances
  TYPE(MeasurData_type) :: Ym

  !---Local variables
  INTEGER :: iuMeasur, iuOutput,ierr,stypeSfcCRTM
  INTEGER :: nProfiles, nMeasurData, nExternData, nChan
  INTEGER :: Stype, nSea=0, nLand=0, iChan
  REAL :: xalt, xcover
  REAL :: RWP, Snow_Cover, Sea_Ice, TPW, CLW, WindSp, Tskin
  REAL, DIMENSION(7) :: Emiss
  
  INTEGER :: iProfiles
  
  !-------------------------------------------------------------------
  !   Loading and control parameters
  !-------------------------------------------------------------------
  CALL LoadCntrlParams()
           
  !------------------------------------------------------------------
  !    Opening/Header reading of Measurmts Data/etc
  !------------------------------------------------------------------
  !---Open and Read the header of the measurement(s) file
  CALL ReadHdrMeasurmts(CntrlConfig%MeasurmtFile,iuMeasur,nMeasurData,Ym)
  
  nChan       = Ym%nChan
  nExternData = nMeasurData
  
  !------------------------------------------------------------------
  !    Open Output file(s) and write out header
  !------------------------------------------------------------------
  !---Determine the number of profiles to be processed
  CALL GetNprofs2Process(CntrlConfig%ExternDataAvailab,CntrlConfig%nprofs2process,   &
       nMeasurData,nExternData,nprofiles)
  !---Write out header
  CALL InitHdrScene(nLev,nLay,nChan,Ym%CentrFreq,Ym%polar,pres_lev, &
                    pres_lay,Scene,nParmCLW,nParmRain,nParmSnow,nParmIce,nParmGrpl, &
                    nAbsorb,AbsorbID,nqc,iTyp,Ym%nPosScan,Ym%nScanLines,-99)
  CALL WriteHdrScene(iuOutput,CntrlConfig%OutputFile,Scene,nProfiles)
  !------------------------------------------------------------------
  !    Start of the Heritage Algorithem retrieval process
  !------------------------------------------------------------------
  !---Loop over the measurements
  ProfLoop: DO iProfiles=1,nProfiles
     !---Read the measurement(s)
     CALL ReadMeasurmts(iuMeasur,Ym,ierr)
     IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'Radiance')
    !---Initilize Scene structure
    Scene%angle=mean(Ym%angle(1:Ym%nchan))
    Scene%node=Ym%Node
    Scene%lat=Ym%lat
    Scene%lon=Ym%lon
    Scene%iScanPos=Ym%iscanPos
    Scene%iScanLine=Ym%iScanLine
    Scene%scanYear=Ym%Year
    Scene%scanDAY=Ym%julDAY
    Scene%scanUTC=Ym%secs
    Scene%ChiSq=-999.0

    !---Determine the surface type and water fraction
    CALL Read_topography(CntrlConfig%Topogr,Ym%lat,Ym%lon,xalt,Stype,xcover,stypeSfcCRTM)
    IF ( Stype == 1 ) THEN
      nSea=nSea+1 
    ELSE 
      nLand=nLand+1
    ENDIF

    Scene%iTypSfc=Stype
    
    !---Determine the Precipitation
    CALL DETERM_RWP(nChan, Ym%Tb, Stype, RWP)
    Scene%Rain(1)=RWP

    !---Determine the Snow_Cover
    CALL DETERM_SNOW_COVER(nChan, Ym%Tb, Stype, Snow_Cover)
    Scene%Snow(1)=Snow_Cover

    !---Determine the Sea_Ice
    CALL DETERM_SEA_ICE(nChan, Ym%Tb, Stype, Sea_Ice)
    Scene%Ice(1)=Sea_Ice

    !---Determine the Cloud Water Vapor
    CALL DETERM_TPW(nChan, Ym%Tb, Stype, TPW)
    Scene%Absorb_lay(1,1)=TPW
    Scene%Absorb_lay(1,2)=-999.0

    !---Determine the Cloud Liquid Water
    CALL DETERM_CLW(nChan, Ym%Tb, Stype, CLW)
    Scene%Clw(1)=CLW

    !---Determine the Wind Speed
    CALL DETERM_WINDSP(nChan, Ym%Tb, Stype, WindSp)
    Scene%WindSp=WindSp
    
    !---Determine the land emissivity for SSMI channels
    DO iChan=1, nChan
      Scene%Emiss(iChan)=-999.0
    ENDDO
    CALL DETERM_EMISS(nChan, Ym%Tb, Stype, Emiss)
    Scene%Emiss(13)=Emiss(1)     ! 19v
    Scene%Emiss(12)=Emiss(2)     ! 19h
    Scene%Emiss(14)=Emiss(3)     ! 22v
    Scene%Emiss(16)=Emiss(4)     ! 37v
    Scene%Emiss(15)=Emiss(5)     ! 37h
    Scene%Emiss(17)=Emiss(6)     ! 91v(85v)
    Scene%Emiss(18)=Emiss(7)     ! 91h(85h)
    
    !---Determine the land Skin Temperature
    CALL DETERM_TSKIN(nChan, Ym%Tb, Stype, Tskin)
    Scene%Tskin=Tskin
    
    !---Write out the retrieval
    CALL WriteScene(iuOutput,Scene)
  
  END DO ProfLoop

  PRINT *, 'Number of profiles over ocean =', nSea
  PRINT *, 'Number of profiles over land =', nLand

END PROGRAM fmsdr2heriAlgoRetr



