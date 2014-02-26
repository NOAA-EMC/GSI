!$Id: IO_Noise.f90 1986 2009-08-27 21:03:27Z kgarrett $
!-----------------------------------------------------------------------------------------------
! Name:         IO_Noise
! 
! Type:         F90 module
!
! Description:
!       Module that contains various subroutines related to the 
!       handling of the noise I/O (except for the loading of Noise
!       structures, still under the Noise module).
!
! Modules needed:
!       - misc
!
! Subroutines contained:
!       - ReadNoise
!       - WriteNoise
!       - WriteWarmTarget
!       - ReadWarmTarget
!
! Data type included:
!       - none
!
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_Noise
  USE misc
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: ReadNoise,WriteNoise,WriteWarmTarget,ReadWarmTarget
  !---INTRINSIC functions used in this module
  INTRINSIC :: MAXVAL  

CONTAINS

!===============================================================
! Name:         WriteNoise
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the noise information in a specific format
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - NoiseFile          I             Name of the noise file
!       - CentrFreq          I             Central Frequency vector
!       - nedt               I             Values of the NEDT for all channels
!       - computedOrNot      I             Computed or nominal noise flag
!       - nchan              I             Number of channels
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

  SUBROUTINE WriteNoise(NoiseFile,CentrFreq,nedt,computedOrNot,nchan)
    CHARACTER(LEN=*)              :: NoiseFile
    REAL,            DIMENSION(:) :: CentrFreq,nedt
    INTEGER,         DIMENSION(:) :: computedOrNot
    INTEGER                       :: iu,nchan
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=NoiseFile,form='formatted',status='unknown')
    WRITE(iu,'(a25,i8)')  'nChan =',nChan
    WRITE(iu,'(a25)')     'CentrFreq ='
    WRITE(iu,'(10f10.3)') CentrFreq(1:nChan)
    WRITE(iu,'(a25)')     'Error' 
    WRITE(iu,'(10f10.3)') nedt(1:nChan)
    WRITE(iu,'(a25)')     'Computed Or Default'
    WRITE(iu,'(20i4)') computedOrNot(1:nChan)
    CLOSE(iu)
    RETURN
  END SUBROUTINE WriteNoise

!===============================================================
! Name:         ReadNoise
!
!
! Type:         Subroutine
!
!
! Description:  Reads the noise values from the Noise file
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - NoiseFile          I             Name of the noise file
!       - CentrFreq          0             Central Frequency vector
!       - nedt               0             Values of the NEDT for all channels
!       - nchan              0             Number of channels
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

  SUBROUTINE ReadNoise(NoiseFile,CentrFreq,nedt,nchan)
    CHARACTER(LEN=*)                       :: NoiseFile
    REAL,            DIMENSION(:)          :: CentrFreq,nedt
    INTEGER                                :: iu,nchan
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=NoiseFile,form='formatted',status='unknown')
    READ(iu,'(25x,i8)')  nChan
    READ(iu,'(a)')     
    READ(iu,'(10f10.3)') CentrFreq(1:nChan)
    READ(iu,'(a)')     
    READ(iu,'(10f10.3)') nedt(1:nChan)
    CLOSE(iu)
    RETURN
  END SUBROUTINE ReadNoise

!===============================================================
! Name:         WriteWarmTarget
!
!
! Type:         Subroutine
!
!
! Description:  Writes the warm target temperatures in to a file
!               for the purpose of monitoring them (only available 
!               for N18 and METOP AMSU/MHS pair)
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - WarmTargetFile     I             Name of file that to contain warm targets
!       - CentrFreq          I             Central frequency vector
!       - warmTargetTb       I             Warm target temperatures 
!       - nchan              I             Number of channels
!       - nOrbits            I             Number of rbits
!       - nScanLvec          I             Number of scalines containe din each orbit    
!       - nWarmView          I             Number of views of the warm target.
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

  SUBROUTINE WriteWarmTarget(WarmTargetFile,CentrFreq,warmTargetTb,nchan,nOrbits,nScanLvec,nWarmView)
    CHARACTER(LEN=*)                    :: WarmTargetFile
    REAL,            DIMENSION(:)       :: CentrFreq
    REAL,            DIMENSION(:,:,:,:) :: warmTargetTb
    INTEGER(2),      DIMENSION(:)       :: nScanLvec
    INTEGER                             :: iu,nchan,nOrbits,nWarmView
    INTEGER                             :: i,k,ichan
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=WarmTargetFile,form='formatted',status='unknown')
    WRITE(iu,'(a25,i8)')  'nChan =',nChan
    WRITE(iu,'(a25,i8)')  'nOrbits =',nOrbits
    WRITE(iu,'(a25)')  'nScanLvec ='
    WRITE(iu,'(25x,20i8)')  nScanLvec(1:nOrbits)
    WRITE(iu,'(a25,i8)')  'nWarmView =',nWarmView
    WRITE(iu,'(a25)')     'CentrFreq ='
    WRITE(iu,'(10f10.3)') CentrFreq(1:nChan)
    WRITE(iu,'(a25)')     'Warm Target TBs:'
    DO ichan = 1, nchan
       DO k=1,nOrbits
          Do i=1,nScanLvec(k)
             write(iu,'(3i6,10f10.3)') ichan,k,i,warmTargetTb(i,ichan,1:nWarmView,k)
          ENDDO
       ENDDO
    ENDDO
    CLOSE(iu)
    RETURN
  END SUBROUTINE WriteWarmTarget

!===============================================================
! Name:         ReadWarmTarget
!
!
! Type:         Subroutine
!
!
! Description:  Reads the warm target temperatures from a file
!
!
! Arguments:
!
!      Name                  Type          Description
!      ---------------------------------------------------
!       - WarmTargetFile     I             Name of file that to contain warm targets
!       - CentrFreq          O             Central frequency vector
!       - warmTargetTb       O             Warm target temperatures 
!       - nchan              O             Number of channels
!       - nOrbits            O             Number of rbits
!       - nScanLvec          O             Number of scalines containe din each orbit    
!       - nWarmView          O             Number of views of the warm target.
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

  SUBROUTINE ReadWarmTarget(WarmTargetFile,CentrFreq,warmTargetTb,nchan,nOrbits,nScanLvec,nWarmView)
    CHARACTER(LEN=*)                             :: WarmTargetFile
    REAL,            DIMENSION(:),       POINTER :: CentrFreq
    REAL,            DIMENSION(:,:,:,:), POINTER :: warmTargetTb
    INTEGER(2),      DIMENSION(:),       POINTER :: nScanLvec
    INTEGER                                      :: iu,nchan,nOrbits,nWarmView
    INTEGER                                      :: i,k,ichan,mxScanL,ichan0,k0,i0
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=WarmTargetFile,form='formatted',status='unknown')
    READ(iu,'(25x,i8)')  nChan
    READ(iu,'(25x,i8)')  nOrbits
    READ(iu,'(a)')
    ALLOCATE(nScanLvec(nOrbits),CentrFreq(nChan))
    READ(iu,'(25x,20i8)')  nScanLvec(1:nOrbits)
    READ(iu,'(25x,i8)')  nWarmView
    READ(iu,'(a)')
    READ(iu,'(10f10.3)') CentrFreq(1:nChan)
    READ(iu,'(a)')
    mxScanL=maxval(nScanLvec(1:nOrbits))
    ALLOCATE(warmTargetTb(mxScanL,nchan,nWarmView,nOrbits))
    DO ichan = 1, nchan
       DO k=1,nOrbits
          Do i=1,nScanLvec(k)
             READ(iu,'(3i6,10f10.3)') ichan0,k0,i0,warmTargetTb(i,ichan,1:nWarmView,k)
          ENDDO
       ENDDO
    ENDDO
    CLOSE(iu)
    RETURN
  END SUBROUTINE ReadWarmTarget



END MODULE IO_Noise
