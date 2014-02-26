!===============================================================
! Name:       ModifyNedt_ssmis
!
!
! Type:         Main Program
!
!
! Description:
!       Program that modifies teh NEDT values after the
!       footprint matching process. This should be tied to
!       the FM process itself.
!
!
! Modules needed:
!       - IO_Noise
!
! History:
!       10-22-2005     Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

program ModifyNedt_ssmis
  USE IO_Noise
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: SQRT

  INTEGER,  PARAMETER             :: mxChan =100
  REAL,     DIMENSION(mxChan)     :: CentrFreq,nedt
  INTEGER                         :: nchan,ichan
  INTEGER,  DIMENSION(mxChan)     :: indxIMG,indxENV,indxLAS,indxUAS 
  INTEGER,  DIMENSION(mxChan)     :: ComputedOrDefault
  REAL,     DIMENSION(mxChan)     :: fact
  REAL                            :: fact_img,fact_env,fact_las,fact_uas
  
  !---Namelist data 
  CHARACTER(LEN=250)              :: NedtFile_in=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)              :: NedtFile_out=DEFAULT_VALUE_STR4
  INTEGER                         :: FMresol=DEFAULT_VALUE_INT
  NAMELIST /NedtModif/NedtFile_in,NedtFile_out,FMresol
  
  !---Read control-data from namelist
  READ(*,NML=NedtModif)
  !---Read Noise data
  CALL ReadNoise(NedtFile_in,CentrFreq,nedt,nchan)
  !---Amplify the noise for channels that are ENV/LAS/UAS
  !   This corresponds to the FM at the highest resolution
  !-------------------------------------------------------
  indxIMG(1:6) =(/8, 9, 10,11,17,18/)
  indxENV(1:5) =(/12,13,14,15,16/)
  indxLAS(1:8) =(/1, 2, 3, 4, 5, 6, 7, 24/)
  indxUAS(1:5) =(/19,20,21,22,23/)
  !---Factors by which to boost the NEDT values
  IF (FMresol.eq.1) THEN
      fact_img = 1.
      fact_env = 2.
      fact_las = 3.
      fact_uas = 6.
  ENDIF
  IF (FMresol.eq.2) CALL ErrHandl(ErrorType,Err_NotImplemented,'(SSMIS/S Noise-Ampl. @ ENV resolution)') 
  IF (FMresol.eq.3) THEN
      fact_img = 1./sqrt(9.)
      fact_env = 1./sqrt(6.)
      fact_las = 1./sqrt(3.)
      fact_uas = 1.
  ENDIF
  IF (FMresol.eq.4) THEN
      fact_img = 1./sqrt(36.)
      fact_env = 1./sqrt(18.)
      fact_las = 1./sqrt(12.)
      fact_uas = 1./sqrt(6.)
  ENDIF

  !---Create a unique vector of factors to be applied uniformly
  fact               = 1.
  fact(indxIMG(1:6)) = fact(indxIMG(1:6))*fact_img
  fact(indxENV(1:5)) = fact(indxENV(1:5))*fact_env
  fact(indxLAS(1:8)) = fact(indxLAS(1:8))*fact_las
  fact(indxUAS(1:5)) = fact(indxUAS(1:5))*fact_uas
  !---Loop over the NEDT values
  Do ichan=1,nChan
      !print *, ichan,CentrFreq(ichan),nedt(ichan),fact(ichan),nedt(ichan)*fact(ichan)
      nedt(ichan)=nedt(ichan)*fact(ichan)
  Enddo
  ComputedOrDefault=1
  !---Output the newly generated NEDT values
  CALL WriteNoise(NedtFile_out,CentrFreq,nedt,ComputedOrDefault,nchan)
end program ModifyNedt_ssmis
