!$Id:$
!-----------------------------------------------------------------------------------------------
! Name:         IO_DEP
! 
! Type:         F90 module
!
! Description:  This module is dedicated to the I/O of the derived EDRs products
!               (DEPs). It is based on the IO_Scene.f90
!
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!
! Subroutines contained:
!       - WriteHdrDEP
!       - WriteDEP
!       - ReadHdrDEP
!       - ReadDEP
!
! Data type included:
!       - DEP_type
!
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_DEP
  USE Consts
  USE misc
  USE utils
  USE ErrorHandling
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: WriteHdrDEP,WriteDEP,ReadHdrDEP,ReadDEP,getDepNprf
  !---Publicly available data/type definitions
  PUBLIC :: DEP_type
  !---Declaration sections
  TYPE   :: DEP_type
    INTEGER                                    :: AlgSN      !MIRS Algorithm serial number (svn) 
    INTEGER                                    :: iTyp       !0->DEP from Scene, 1->DEP from retrieved scene 
    INTEGER                                    :: ProfIndx   !Profile Index 
    !---Positioning Data
    REAL                                       :: Angle      !Angle
    REAL                                       :: RelAziAngle!Relative Azimuth Angle
    REAL                                       :: SolZenAngle!Solar Zenith Angle
    REAL                                       :: lat        !Latitude
    REAL                                       :: lon        !Longitude
    INTEGER                                    :: node       !=0->ASC, =1->DESC
    INTEGER                                    :: scanDAY    !Day 
    INTEGER                                    :: scanYear   !Year
    REAL                                       :: scanUTC    !UTC time
    INTEGER                                    :: iscanPos   !Scan position 
    INTEGER                                    :: iscanLine  !Scan Line index 
    INTEGER                                    :: nPosScan   !Number of scan positions within scanline
    INTEGER                                    :: nScanLines !Number of scan lines within orbit (msome might be missing)
    !---Atmospheric/Hydrometeors/Cloud-related information
    INTEGER                                    :: iTypAtm    !Atmospheric type ID
    CHARACTER(LEN=20)                          :: DescTypAtm !Label of the atmospheric class
    REAL                                       :: TPW        !Total precipitable Water 
    REAL                                       :: Clw        !Integrated Cloud amount 
    REAL                                       :: RWP        !Integrated Liquid Rain water path
    REAL                                       :: LWP        !Integrated Liquid Water Path
    REAL                                       :: SWP        !Integrated Snow water path
    REAL                                       :: IWP        !Integrated Ice water path
    REAL                                       :: GWP        !Integrated Graupel water path
    REAL                                       :: RR         !Surface rain rate
    REAL                                       :: SFR        !Snow falling rate
    REAL                                       :: CldTop     !Cloud Top Pressure
    REAL                                       :: CldBase    !Cloud Base Pressure
    REAL                                       :: CldThick   !Cloud thickness
    REAL                                       :: PrecipType !Precipitation type (frozen/liquid)
    REAL                                       :: RainFlag   !Rain flag
    !---Surface -related information
    INTEGER                                    :: iTypSfc    !Surface type ID
    CHARACTER(LEN=20)                          :: DescTypSfc !Label of the surface class
    REAL                                       :: SWE        !Snow water equivalent
    REAL                                       :: SnowGS     !Snow Effective Grain Size
    REAL                                       :: SnowCover  !Snow cover extent
    REAL                                       :: SM         !Soil Moisture
    REAL                                       :: SIC        !Sea-ice concentration
    REAL                                       :: SIC_MY     !Multi-year Sea-ice concentration
    REAL                                       :: SIC_FY     !First-year Sea-ice concentration
    REAL                                       :: WindSp     !Wind speed
    REAL                                       :: WindDir    !Wind vector
    REAL                                       :: WindU      !U-direction wind speed
    REAL                                       :: WindV      !V-direction wind speed
    !---QC info 
    INTEGER,           DIMENSION(4)            :: qc         !QC vector
    !---Convergence items (when the DEP is coming from a retrieved scene )
    INTEGER                                    :: nIter      !Number of iterations
    REAL                                       :: ChiSq      !Convergence metric
  END TYPE DEP_type

CONTAINS


!===============================================================
! Name:         ReadHdrDEP
!
!
! Type:         Subroutine
!
!
! Description:  Reads the header from a geophysical DEP file.
!               It also performs memory allocation for arrays 
!               needed later on.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 O              Unit number after opening
!       - InputFile          I              Name of the input file
!       - DEP                I/O            Structure containing geoph data (DEP)
!       - nprf               O              Number of profiles in file
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

  SUBROUTINE ReadHdrDEP(iu,InputFile,DEP,nprf)
    CHARACTER(LEN=*)      :: InputFile
    INTEGER               :: iu,nPrf
    TYPE(DEP_type)        :: DEP
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=InputFile,form='unformatted')
    READ(iu) DEP%iTyp,DEP%AlgSN               !iTyp=0->Simple Scene, =1->Retrieved Scene
    READ(iu) nprf
    READ(iu) DEP%nPosScan
    READ(iu) DEP%nScanLines
    RETURN
  END SUBROUTINE ReadHdrDEP


!===============================================================
! Name:         getDepNprf
!
!
! Type:         Subroutine
!
!
! Description:  Return number of profile in the dep file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - fileDep            I              Name of the input dep file
!       - nprf               O              Number of profiles in file
!
!
! Modules needed:
!       - get_lun
!
!
! History:
!       06-29-2011      Wanchun Chen        DELL Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE getDepNprf(fileDep,nprf)
    CHARACTER(LEN=*)      :: fileDep
    INTEGER               :: iu,nprf,ityp,algSn
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=fileDep,form='unformatted')
    READ(iu) ityp,algSn
    READ(iu) nprf
    CLOSE(iu)
    RETURN
  END SUBROUTINE getDepNprf


!===============================================================
! Name:         WriteHdrDEP
!
!
! Type:         Subroutine
!
!
! Description:  Writes header of DEP structure in to file
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - iu                 O             Unit number
!       - OutputFile         I             Name of output file
!       - DEP                I             Structure whose header 
!                                          is to be written out
!       - nprf               I             Number of profiles to write
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

  SUBROUTINE WriteHdrDEP(iu,OutputFile,DEP,nprf)
    CHARACTER(LEN=*)      :: OutputFile
    INTEGER               :: iu,nPrf
    TYPE(DEP_type)        :: DEP
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=OutputFile,form='unformatted')
    WRITE(iu) DEP%iTyp,DEP%AlgSN
    WRITE(iu) nprf
    WRITE(iu) DEP%nPosScan
    WRITE(iu) DEP%nScanLines
    RETURN
  END SUBROUTINE WriteHdrDEP



!===============================================================
! Name:         WriteDEP
!
!
! Type:         Subroutine
!
!
! Description:  Writes the DEP content (not header) into a file
!
!
! Arguments:
!
!      Name                Type             Description
!      ---------------------------------------------------
!       - iu                I               Unit number
!       - DEP               I               DEP structure whose
!                                           content is to be written out
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

  SUBROUTINE WriteDEP(iu,DEP)
    TYPE(DEP_type)   :: DEP
    INTEGER          :: iu
    WRITE(iu) DEP%ProfIndx
    !---Atmospheric , cloud and hydrometeors constituents
    WRITE(iu) DEP%iTypAtm,DEP%TPW,DEP%Clw,DEP%RWP,DEP%SWP,      &
         DEP%IWP,DEP%GWP,DEP%RR,DEP%SFR,DEP%CldTop,DEP%CldBase, &
         DEP%CldThick,DEP%PrecipType,DEP%RainFlag,DEP%LWP   
    !---Surface parameters
    WRITE(iu)  DEP%iTypSfc,DEP%SWE,DEP%SnowCover,DEP%SM,DEP%SIC,&
         DEP%WindSp,DEP%WindDir,DEP%WindU,DEP%WindV,DEP%SnowGS, &
         DEP%SIC_FY,DEP%SIC_MY
    !---QC variables 
    WRITE(iu) DEP%qc(1:4)   
    !---Positioning variables
    WRITE(iu) DEP%lat,DEP%lon,DEP%node,DEP%scanUTC,DEP%scanYear,DEP%scanDay,DEP%iscanPos,DEP%iscanLine,&
         DEP%angle,DEP%RelAziAngle,DEP%SolZenAngle
    !---In case the DEP is from a retrieved scene 
    IF (DEP%iTyp .eq. 1) THEN
       WRITE(iu) DEP%nIter,DEP%ChiSq
    ENDIF
    RETURN
  END SUBROUTINE WriteDEP


!===============================================================
! Name:         ReadDEP
!
!
! Type:         Subroutine
!
!
! Description:  Reads content of a geophysical DEP file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 I              Unit number
!       - DEP                I/O            Structure to be filled 
!                                           with content of file
!       - ierr               O              Error index when reading
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

  SUBROUTINE ReadDEP(iu,DEP,ierr)
    TYPE(DEP_type)   :: DEP
    INTEGER          :: iu,ierr
    READ(iu,iostat=ierr,end=10) DEP%ProfIndx
    IF (ierr.ne.0) THEN
       CALL ErrHandl(WarningType,Warn_readInvalid,'DEP invalid.')
       RETURN
    ENDIF
    !---Atmospheric , cloud and hydrometeors constituents
    READ(iu,err=20) DEP%iTypAtm,DEP%TPW,DEP%Clw,DEP%RWP,DEP%SWP,      &
         DEP%IWP,DEP%GWP,DEP%RR,DEP%SFR,DEP%CldTop,DEP%CldBase, &
         DEP%CldThick,DEP%PrecipType,DEP%RainFlag,DEP%LWP    
    !---Surface parameters
    READ(iu,err=20)  DEP%iTypSfc,DEP%SWE,DEP%SnowCover,DEP%SM,DEP%SIC,&
         DEP%WindSp,DEP%WindDir,DEP%WindU,DEP%WindV,DEP%SnowGS,&
         DEP%SIC_FY,DEP%SIC_MY 
    !---QC variables 
    READ(iu,err=20) DEP%qc(1:4)   
    !---Positioning variables
    READ(iu,err=20) DEP%lat,DEP%lon,DEP%node,DEP%scanUTC,DEP%scanYear,DEP%scanDay,DEP%iscanPos,DEP%iscanLine,&
         DEP%angle,DEP%RelAziAngle,DEP%SolZenAngle
    !---In case the DEP is from a retrieved scene 
    IF (DEP%iTyp .eq. 1) THEN
       READ(iu,err=20) DEP%nIter,DEP%ChiSq
    ENDIF
    RETURN
10  ierr=Warn_EndOfFile
    CALL ErrHandl(WarningType,Warn_EndOfFile,'Dep') 
    RETURN
20  ierr=Warn_readInvalid
    CALL ErrHandl(WarningType,Warn_readInvalid,'(ReadDEP)')
    RETURN
  END SUBROUTINE ReadDEP



END MODULE IO_DEP
