!$Id: IO_InstrConfig.f90 1367 2008-06-24 20:47:12Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:         IO_InstrConfig
! 
! Type:         F90 module
!
! Description:
!       Module that contains the necessary routines and corresponding data
!       related to reading instrument configuration information.
!
! Modules needed:
!       - misc
!
! Subroutines contained:
!       - ReadInstrConfig
!
! Data type included:
!       - InstrConfig_type
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_InstrConfig
  USE misc
  IMPLICIT NONE
  PRIVATE
  !----Publicly available subroutine(s)
  PUBLIC :: ReadInstrConfig
  !----Publicly available data/types
  PUBLIC :: InstrConfig_type
  !----Global section of the module
  TYPE :: InstrConfig_type
     INTEGER                         :: nchan              !Number of channels
     REAL,    DIMENSION(:), POINTER  :: CentrFreq          !Central frequencies
     INTEGER, DIMENSION(:), POINTER  :: Polarity           !polarizations
     REAL                            :: MinAng             !Minimum angle
     REAL                            :: MaxAng             !Maxmimum angle
     REAL                            :: secant_view_angle  !Secant of view angle
     REAL                            :: secant_solar_angle !Secant of solar angle
  END TYPE InstrConfig_type
  
CONTAINS

!===============================================================
! Name:         ReadInstrConfig
!
!
! Type:         Subroutine
!
!
! Description:  Reads the information in the instrument 
!               configuration file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - InstrConfigFile    I              Name of the instr. Config file
!       - InstrConfig        I/O            Structure containing the 
!                                           instrument information (see
!                                           definition in top section)
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

  SUBROUTINE ReadInstrConfig(InstrConfigFile,InstrConfig)
    CHARACTER(LEN=*)        :: InstrConfigFile
    TYPE(InstrConfig_type)  :: InstrConfig
    INTEGER                 :: iu
    !---Open file containing radiance measurements
    iu=get_lun()
    OPEN(iu,file=InstrConfigFile,form='formatted',status='old')
    !---Read header
    READ(iu,'(25x,i8)') InstrConfig%nchan
    ALLOCATE(InstrConfig%CentrFreq(InstrConfig%nchan),InstrConfig%Polarity(InstrConfig%nchan))
    READ(iu,*)
    READ(iu,*) InstrConfig%CentrFreq(1:InstrConfig%nchan)
    READ(iu,*)
    READ(iu,*) InstrConfig%polarity(1:InstrConfig%nchan)
    READ(iu,*)
    READ(iu,*) InstrConfig%MinAng
    READ(iu,*)
    READ(iu,*) InstrConfig%MaxAng
    READ(iu,*)
    READ(iu,*) InstrConfig%secant_view_angle
    READ(iu,*)
    READ(iu,*) InstrConfig%secant_solar_angle
    close(iu)
    RETURN
  END SUBROUTINE ReadInstrConfig


END MODULE IO_InstrConfig
