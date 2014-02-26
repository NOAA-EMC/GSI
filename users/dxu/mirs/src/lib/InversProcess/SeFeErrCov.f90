!$Id: SeFeErrCov.f90 2218 2010-06-28 16:55:57Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:       SeFeErrCov
! 
! Type:       F90 module
!
! Description:
!
!          This module is dedicated to loading the measurements error 
!          covariance matrix. This matrix is dimensioned 
!          nChanSe x nChanSe x nSeTyp. It is assumed that we could have
!          several types of measurement error covariances.
!          Extended to include other subroutines related to building
!          the covariance matrix of the instrumental/forward erros
!
! Modules needed:
!       - misc
!       - CntrlParams
!
! Subroutines contained:
!       - GetModelErr
!       - LoadSeFeCovMatrx
!       - LoadCovMatrx
!       - GetErrCovMatr
!
! Data type included:
!       - SeT
!       - FeT
!       - SeCov_type
!
! 
! History:
!        2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE SeFeErrCov
  USE misc
  USE Consts
  USE CntrlParams
  USE ErrorHandling
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: LoadSeFeCovMatrx,GetErrCovMatr,GetModelErr,scaleError
  !---Publicly available data/type definitions
  PUBLIC :: SeT,FeT,SeCov_type
  !---Declaration sections
  TYPE  :: SeCov_type
    INTEGER                            :: nSe
    INTEGER                            :: nChan
    INTEGER, DIMENSION(:),     POINTER :: iSeTyp
    REAL,    DIMENSION(:,:,:), POINTER :: Se
  END TYPE SeCov_type
  TYPE(SeCov_type)                      :: SeT,FeT
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: SIZE

CONTAINS


!===============================================================
! Name:                scaleError
!
!
! Type:                Subroutine
!
!
! Description: Scales the errors (RTM and instrumental) by a 
!              vector of factors.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nchan              I              Number of channels
!       - SfcClass           I              Sfc class
!       - Error              I/O            Errors vector
!       - scalFactEF_xx_byChan I            Scaling factor vectors (oc, sn, ic, ld)
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
  SUBROUTINE scaleError(Error,SfcClass,nchan,scalFactEF_oc_byChan,scalFactEF_ic_byChan, &
       scalFactEF_ld_byChan,scalFactEF_sn_byChan)
    REAL,    DIMENSION(:), INTENT(INOUT) :: Error
    INTEGER,               INTENT(IN)    :: SfcClass,nchan
    REAL,    DIMENSION(:), INTENT(IN)    :: scalFactEF_oc_byChan,scalFactEF_ic_byChan
    REAL,    DIMENSION(:), INTENT(IN)    :: scalFactEF_ld_byChan,scalFactEF_sn_byChan
    !---Local variables
    REAL,    DIMENSION(nchan) :: Scal  
    INTEGER                   :: ichan
    IF (SfcClass .eq. OC_TYP)     Scal(1:nchan) = scalFactEF_oc_byChan(1:nchan)
    IF (SfcClass .eq. SEAICE_TYP) Scal(1:nchan) = scalFactEF_ic_byChan(1:nchan)
    IF (SfcClass .eq. LD_TYP)     Scal(1:nchan) = scalFactEF_ld_byChan(1:nchan)
    IF (SfcClass .eq. SNOW_TYP)   Scal(1:nchan) = scalFactEF_sn_byChan(1:nchan)
    DO ichan=1,nchan
       Error(ichan) = Error(ichan)*Scal(ichan)
    ENDDO
    RETURN
  END SUBROUTINE scaleError
           
!===============================================================
! Name:                GetModelErr
!
!
! Type:                Subroutine
!
!
! Description: Get the diagonal elements of the Error matrix 
!              and puts them into a vector.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - nchan              I              Number of channels
!       - ModelErr           O              Errors vector
!       - Fe                 I              Errors matrix
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

  SUBROUTINE GetModelErr(nchan,ModelErr,Fe)
    REAL,   DIMENSION(:)   :: ModelErr
    REAL,   DIMENSION(:,:) :: Fe
    INTEGER                :: nchan,ichan
    DO ichan=1,nchan
       ModelErr(ichan)=Fe(ichan,ichan)
    ENDDO
    RETURN
  END SUBROUTINE GetModelErr

!===============================================================
! Name:                LoadSeFeCovMatrx
!
!
! Type:                Subroutine
!
!
! Description:  Loads the errors from a multi-dimension matrix 
!               stored as global variable.
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

  SUBROUTINE LoadSeFeCovMatrx()
    !call LoadCovMatrx(CntrlConfig%CovarFileMeasurmErr,SeT)
    !call LoadCovMatrx(CntrlConfig%CovarFileModelErr,FeT)
    CALL ErrHandl(ErrorType,Err_NotSupported,'')
    RETURN
  END SUBROUTINE LoadSeFeCovMatrx


!===============================================================
! Name:                LoadCovMatrx
!
!
! Type:                Subroutine
!
!
! Description: Reads the instrumental/model errors covariance matrix
!
!
! Arguments:
!
!      Name                  Type            Description
!      ---------------------------------------------------
!       - SeFile             I               Matrix file name
!       - CovT               O               Covariance structure
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

  SUBROUTINE LoadCovMatrx(SeFile,CovT)
    TYPE(SeCov_type)                         :: CovT
    CHARACTER(LEN=*)                         :: SeFile
    INTEGER                                  :: iu,iSe,iChan
    !---Open Measur Err covar file
    iu=get_lun()
    OPEN(iu,file=SeFile,status='old',form='formatted')
    !---Reading header info of the Instrument Error covariance 
    READ(iu,'(25x,i8)') CovT%nSe
    READ(iu,'(25x,i8)') CovT%nChan
    !---Allocate pointers
    Allocate(CovT%iSeTyp(CovT%nSe),CovT%Se(CovT%nChan,CovT%nChan,CovT%nSe))
    !---Load the types
    READ(iu,'(25x,10i8)') CovT%iSeTyp(1:CovT%nSe)
    !---Load the covariance matrix
    READ(iu,*) 
    SeTypLoop: DO iSe=1,CovT%nSe 
       ChanLoop: DO iChan=1,CovT%nChan
          READ(iu,'(100f10.3)') CovT%Se(ichan,1:CovT%nChan,iSe)
       ENDDO ChanLoop
    ENDDO SeTypLoop
    !---Close file
    Close(iu)
    RETURN
  END SUBROUTINE LoadCovMatrx
        

!===============================================================
! Name:                GetErrCovMatr
!
!
! Type:                Subroutine
!
!
! Description:  Selects the right error covariance matrix 
!               from the big one, depending on the index iFe.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Fe                 O              Selected error matrix
!       - iFe                I              Index of which part to select.
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

  SUBROUTINE GetErrCovMatr(Fe,iFe)
    REAL,    DIMENSION(:,:)  :: Fe
    INTEGER                  :: iFe, II, JJ, I, J
    II = size(Fe,1)
    JJ = size(Fe,2)
    do J = 1, JJ
    do I = 1, II        
      Fe(I,J) = FeT%Se(I,J,iFe)
    enddo
    enddo
    RETURN
  END SUBROUTINE GetErrCovMatr

END MODULE SeFeErrCov
