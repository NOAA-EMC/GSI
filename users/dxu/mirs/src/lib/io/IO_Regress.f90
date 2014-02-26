!$Id: IO_Regress.f90 2219 2010-06-28 16:59:36Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:        IO_Regress
! 
! Type:         F90 module
!
! Description:
!       Module that contains various subroutines related to the 
!       regression algorithms.
!
! Modules needed:
!       - misc
!
! Subroutines contained:
!       - readRegressAlgor
!
! Data type included:
!       - none
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

Module IO_Regress
  USE misc
  USE ErrorHandling

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: readRegressAlgor

CONTAINS
!===============================================================
! Name:         readRegressAlgor
!
!
! Type:         Subroutine
!
!
! Description:  Reads regression algorithms coefficients from 
!               the regression file(s)
!
!
! Arguments:
!
!         Name                     Type          Description
!      ---------------------------------------------------
!       - AlgoFile                   I           Regression coeffs file name
!       - sfcTyp                     O           Surface type of the regress
!       - Algor                      O           Algor coeffs for all EDRs
!       - Labels                     O           Labels of all EDRs contained
!       - nElemts                    O           # elements representing each EDR
!       - nIndepVar                  O           # Indep variables used as argumnts 
!       - nEDRs                      O           # EDRs concerned by these regress
!       - TbIndxV                    O           Pointer to the TBs used as inputs to algo
!       - ialgo                      I           Algo index (usually correspond to sfc type)
!       - nLay                       O           Number of layers
!       - nLev                       O           Number of levels
!       - nchan                      0           Number of channels
!       - cfreq                      O           Central frequencies
!       - pol                        O           Polarizations
!       - presLay                    O           Layer-based pressure grid
!       - presLev                    0           Level-based pressure grid
!       - err                        0           Error returned upon file open failure
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

  SUBROUTINE readRegressAlgor(AlgoFile,Algor,Labels,nElemts,nIndepVar,nSfc,TbIndxV,FormIndxV,ialgo,&
       isfc,nLay,nLev,nchan,nAngBins,AngleBins,cfreq,pol,presLay,presLev)
    CHARACTER(LEN=*)                       :: AlgoFile
    INTEGER                                :: Bin
    INTEGER,          DIMENSION(:)         :: AngleBins
    REAL,             DIMENSION(:,:,:,:,:) :: Algor
    CHARACTER(LEN=*), DIMENSION(:)         :: Labels
    INTEGER,          DIMENSION(:)         :: nElemts
    INTEGER,          DIMENSION(:,:)       :: nIndepVar
    INTEGER,          DIMENSION(:,:,:)     :: TbIndxV,FormIndxV
    INTEGER                                :: nSfc,ialgo,nLay,nLev,nchan,nAngBins,err
    REAL,             DIMENSION(:)         :: cfreq,presLay,presLev
    INTEGER,          DIMENSION(:)         :: pol
    INTEGER                                :: sfcTyp
    INTEGER                                :: iu,iSfc,ivar,ivar0,iAng
    INTEGER,          DIMENSION(200)       :: stat
    REAL,             DIMENSION(200)       :: mCorr

    iu=get_lun()
    OPEN(iu,file=AlgoFile,form='formatted',status='old',iostat=err)
    IF (err .ne. 0) THEN
       print *, 'File does not exist: ',AlgoFile
       RETURN
    ENDIF
    !---Header
    read(iu,'(60x,i4)') nLay
    read(iu,'(60x,i4)') nLev
    read(iu,'(60x,i4)') nchan
    read(iu,'(60x,i4)') nAngBins
    read(iu,'(60x,20i4)') AngleBins
    read(iu,'(60x,100f12.4)') cfreq(1:nchan)
    read(iu,'(60x,100i3)') pol(1:nchan)
    read(iu,'(60x,120f9.3)') presLay(1:nLay)
    read(iu,'(60x,120f9.3)') presLev(1:nLev)
    !---Body
    DO iAng=1,nAngBins 
       read(iu,'(a)') 
       read(iu,'(60x,a30)') labels(ialgo)
       read(iu,'(60x,i4)') sfcTyp
       IF (sfcTyp+1 .ne. isfc) CALL ErrHandl(ErrorType,Err_InconsSfc,' in '//AlgoFile)
       read(iu,'(60x,i4)') Bin
       read(iu,'(60x,i4)') nElemts(ialgo)
       read(iu,'(60x,i4)') nIndepVar(ialgo,iSfc)
       read(iu,'(a)')
       read(iu,'(60x,100i3)') TbIndxV(ialgo,iSfc,1:nchan) 
       read(iu,'(60x,100i3)') FormIndxV(ialgo,iSfc,1:nchan) 
       read(iu,'(60x,200i4)') stat(1:nElemts(ialgo))
       read(iu,'(60x,200f12.4)') mCorr(1:nElemts(ialgo))
       DO ivar=1,nElemts(ialgo)
          read(iu,'(20x,i4)') ivar0
          read(iu,'(12x,30f12.5)') Algor(ialgo,iSfc,ivar,1:nIndepVar(ialgo,iSfc)+1,iAng)
          read(iu,'(a)')
       END DO
    END DO
    close(iu)
  END SUBROUTINE readRegressAlgor

END module
