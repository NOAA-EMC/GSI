!$Id: Noise.f90 1729 2009-02-09 20:20:55Z kgarrett $
!-----------------------------------------------------------------------------------------------
! Name:         Noise
! 
! Type:         F90 module
!
! Description:
!       Module that contains various subroutines related to the 
!       handling of the noise.
!
! Modules needed:
!       - misc
!       - IO_Noise
!
! Subroutines contained:
!       - LoadNoise
!       - ComputeNoise
!       - NoiseOnTopOfRad
!       - SetUpRandomGenSeed
!       - GenerateNoiseErr
!       - BuildMatrxFromDiagVec
!
! Data type included:
!       - Noise_type
!       - noiseInfo
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE Noise
  USE misc
  USE IO_Noise
  IMPLICIT NONE
  PRIVATE
  
  !---Publicly available subroutine
  PUBLIC :: LoadNoise,ComputeNoise,NoiseOnTopOfRad,GRNF
  PUBLIC :: SetUpRandomGenSeed,GenerateNoiseErr,BuildMatrxFromDiagVec
  
  !---Publicly available data/types, etc
  PUBLIC :: Noise_type,noiseInfo
  !---Declaration sections
  TYPE :: Noise_type
     INTEGER                         :: nchan         !Number of channels
     REAL,    DIMENSION(:), POINTER  :: CentrFreq     !Center Frequencies
     REAL,    DIMENSION(:), POINTER  :: rms           !rms
     REAL,    DIMENSION(:), POINTER  :: nedt          !nedt vector
  END TYPE Noise_type
  TYPE(Noise_type)  :: noiseInfo 
  
  !---Module-wide visible variables
  INTEGER :: ISEED=2
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: RANDOM_SEED,ALOG,ATAN,COS,RANDOM_NUMBER,SQRT
    
CONTAINS

!===============================================================
! Name:         BuildMatrxFromDiagVec
!
!
! Type:         Subroutine
!
!
! Description:  Builds a matrix from diagonal elements 
!               contained in a vector. Off-diagonal are set to 0
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nchan              I           Number of channels
!       - noiseRMS           I           Vector of diagonal elements
!       - Se                 O           Matrix with diagonal elmts 
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

  SUBROUTINE BuildMatrxFromDiagVec(nchan,noiseRMS,Se)
    REAL,   DIMENSION(:)   :: noiseRMS
    REAl,   DIMENSION(:,:) :: Se
    INTEGER                :: nchan,ichan
    Se = 0.
    DO ichan=1,nchan
       Se(ichan,ichan) = noiseRMS(ichan)**2.
    ENDDO
    RETURN
  END SUBROUTINE BuildMatrxFromDiagVec

!===============================================================
! Name:         LoadNoise
!
!
! Type:         Subroutine
!
!
! Description:  Reads noise values from noise file and loads 
!               them into Noise structure called NoiseInfo
!               (see definition on top of module)
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - NoiseFile          I           Name of noise file
!
!
! Modules needed:
!       - ReadNoise
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE LoadNoise(NoiseFile)
    CHARACTER(LEN=*) :: NoiseFile
    INTEGER          :: iu
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=NoiseFile,form='formatted',status='old')
    READ(iu,'(25x,i8)') noiseInfo%nChan
    ALLOCATE(noiseInfo%CentrFreq(noiseInfo%nChan),noiseInfo%rms(noiseInfo%nChan),&
         noiseInfo%nedt(noiseInfo%nChan))
    CLOSE(iu)
    CALL ReadNoise(NoiseFile,noiseInfo%CentrFreq,noiseInfo%nedt,noiseInfo%nchan)
    noiseInfo%rms(1:noiseInfo%nChan)=noiseInfo%nedt(1:noiseInfo%nChan)
    RETURN
  END SUBROUTINE LoadNoise

!===============================================================
! Name:         ComputeNoise
!
!
! Type:         Subroutine
!
!
! Description:  Puts the noise elements contained in a structure 
!               into an independent vector
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - noiseRMS           O             Vector of noise values
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

  SUBROUTINE ComputeNoise(noiseRMS)
    REAL, DIMENSION(:) :: noiseRMS
    noiseRMS(:) = noiseInfo%rms(:)
    RETURN
  END SUBROUTINE ComputeNoise


!===============================================================
! Name:         NoiseOnTopOfRad
!
!
! Type:         Subroutine
!
!
! Description:  Adds noise on top of the brightness temperatures
!               Useful when doing forward simulations with CRTM
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nchan              I           Number of channels
!       - TB                 I/O         Vector of brightness temperatures
!       - NoiseErr           I           Vector of channel-based noise
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

  SUBROUTINE NoiseOnTopOfRad(nchan,TB,NoiseErr)
    REAL,    DIMENSION(:) :: TB,NoiseErr
    INTEGER               :: nchan,ichan
    DO ichan=1,nchan
       TB(ichan)= TB(ichan)+NoiseErr(ichan)
    ENDDO
    RETURN
  END SUBROUTINE NoiseOnTopOfRad


!===============================================================
! Name:         SetUpRandomGenSeed
!
!
! Type:         Subroutine
!
!
! Description:  Sets up the random generator seed. The seed 
!               value is declared in top section of this module
!
!
! Arguments: None
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

  SUBROUTINE SetUpRandomGenSeed()
    CALL RANDOM_SEED(SIZE=ISEED)
    RETURN
  END SUBROUTINE SetUpRandomGenSeed


!===============================================================
! Name:        GenerateNoiseErr
!
!
! Type:         Subroutine
!
!
! Description:  Generates a random normal distribution noise
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nchan              I              Number of channels
!       - NoiseErr           O              Vector of randomly generated 
!                                           noise values
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

  SUBROUTINE GenerateNoiseErr(nchan,NoiseErr)
    REAL,    DIMENSION(:) :: NoiseErr
    REAL                  :: X
    INTEGER               :: nchan,ichan
    DO ichan=1,nchan
       CALL GRNF (X)
       NoiseErr(ichan) = X*noiseInfo%rms(ichan)
    ENDDO
    RETURN
  END SUBROUTINE GenerateNoiseErr


  SUBROUTINE GRNF (X)
    ! Gaussian random number generated from uniform random number. 
    REAL, INTENT (OUT) :: X
    REAL :: PI,R1,R2,X0
    PI = 4.0*ATAN(1.0)
    CALL RANDOM_NUMBER(HARVEST=X0)
    R1 = -ALOG(1.0-X0)
    CALL RANDOM_NUMBER(HARVEST=X0)
    R2 = 2.0*PI*X0
    R1 = SQRT(2.0*R1)
    X  = R1*COS(R2)
  END SUBROUTINE GRNF


END MODULE Noise
