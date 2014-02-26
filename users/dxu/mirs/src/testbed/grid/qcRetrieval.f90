!$Id:qcRetrieval.f90
!===============================================================
! Name:    qcRetrieval.f90
!
!
! Type:    Main Program
!
!
! Description:
!       Generate QC retrieval Stats.
!       Output abnormal orbit information.
!
! Modules needed:
!       - Consts
!       - misc
!       - IO_DEP
!       - IO_Misc
!       - ErrorHandling
!
!
! History:
!       04/20/2012      Wanchun Chen 
!
!===============================================================

Program qcRetrieval

  USE Consts
  USE misc
  USE IO_DEP
  USE IO_Misc
  USE ErrorHandling
 
  IMPLICIT NONE
  !---INTRINSIC functions used

  INTRINSIC :: ABS,COS,INT,MOD,TRIM,ALLOCATED
  
  INTEGER             :: iu_list=20,iuDEP,ierr,iprf,nprf,iuMon,iuAb
  INTEGER, PARAMETER  :: LENF=256
  INTEGER             :: ifile, nfile
  CHARACTER(LEN=6)    :: prefix_mon='QC_MON'
  INTEGER             :: qc, cend
  CHARACTER(LEN=LENF), DIMENSION(:), POINTER :: depFiles, monFiles 
  REAL                :: cnt0, cnt1, cnt2
  REAL                :: nPrfValid, cntChiSq
  INTEGER             :: jday,year
  REAL                :: day,time
  REAL                :: perc_QC0,perc_QC1,perc_QC2,perc_Con
  
  
  !---- Structures ----
  TYPE(DEP_type)      :: Dep
  
  CHARACTER(LEN=LENF) :: listDep=DEFAULT_VALUE_STR4
  CHARACTER(LEN=16)   :: satId=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LENF) :: pathMon=DEFAULT_VALUE_STR4
  CHARACTER(LEN=LENF) :: fileAbnormal=DEFAULT_VALUE_STR4

  NAMELIST /qcRetrievalNameList/listDep,satId,pathMon,fileAbnormal
  
  READ(*,NML=qcRetrievalNameList)
  !listDep='/disk1/pub/mirs_oper/src/testbed/grid/data/listDep'
  !satId='npp'
  !pathMon='/disk1/pub/mirs_oper/src/testbed/grid/data/'
  !fileAbnormal='/disk1/pub/mirs_oper/src/testbed/grid/data/qcRetrieval_abnormal_npp_2012_04_19'
  
  call ReadList(iu_list, trim(listDep), depFiles, nfile, monFiles, pathMon, trim(prefix_mon))
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'') 
  
  iuAb = get_lun()
  open(iuAb,file=TRIM(fileAbnormal))
  
  FilesLoop: DO ifile=1,nfile

    !write(*,'(A)') trim(depFiles(ifile))
    
    cnt0 = 0.
    cnt1 = 0.
    cnt2 = 0.
    nPrfValid = 0.
    cntChiSq = 0.
    
    !---- Read header of DEP file ----
    CALL ReadHdrDEP(iuDEP,depFiles(ifile),Dep,nprf)

    !---Loop over the profiles within the file
    ProfilesLoop: DO iprf=1,nPrf

      CALL ReadDep(iuDEP,Dep,ierr)
      
      IF (iprf .eq. 1) THEN
          year = DEP%scanYear
          time = DEP%scanUTC/3600./24.
          jday  = DEP%scanDAY
          day  = jday+time
      ENDIf
      
      IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfilesLoop
      IF (ierr.eq.Warn_readInvalid) CYCLE ProfilesLoop
  
      qc = Dep%qc(1)
      if( qc .eq. 0 ) then
          cnt0 = cnt0 + 1
      else if( qc .eq. 1 ) then   
          cnt1 = cnt1 + 1
      else if( qc .eq. 2 ) then   
          cnt2 = cnt2 + 1
      endif
        
      !-- Test convergence only on good measurements
      IF( BTEST(DEP%QC(4),0) .EQV. .FALSE. ) THEN
         nPrfValid = nPrfValid+1
         IF( DEP%ChiSq .le. 1 .and. DEP%ChiSq .ge. 0 ) cntChiSq=cntChiSq+1.
      ENDIF
          
    ENDDO ProfilesLoop
    !---Close the DEP file
    CLOSE(iuDEP)
    !---Calculate QC and Convergence Stats
    perc_QC0 = cnt0/nprf
    perc_QC1 = cnt1/nprf
    perc_QC2 = cnt2/nprf
    
    if( perc_QC0 .gt. 1. ) perc_QC0 = 0.
    if( perc_QC1 .gt. 1. ) perc_QC1 = 0.
    if( perc_QC2 .gt. 1. ) perc_QC2 = 0.
    
    IF( INT(nPrfValid) .eq. 0 ) THEN
       perc_Con = 1
    ELSE
       perc_Con = cntChiSq/nPrfValid
    ENDIF
    
    !---- output stats
    iuMon = get_lun()
    open(iuMon,file=monFiles(ifile),form='formatted',status='unknown')
    write(iuMon,fmt='(i4,1x,f9.5,1x,4(f7.4,1x))') year,day,perc_QC0,perc_QC1,perc_QC2,perc_Con
    close(iuMon)
    
    !---- output abnormal stats, if any
    if( perc_QC0 .lt. 0.3  .or. perc_QC2 .gt. 0.1 .or. perc_Con .lt. 0.7 ) then
      write(iuAb,*)
      write(iuAb,'(A)') trim(depFiles(ifile))
      write(iuAb,'(A6,F6.2,A1)') 'QC(0)=', 100*perc_QC0, '%'
      write(iuAb,'(A6,F6.2,A1)') 'QC(1)=', 100*perc_QC1, '%'
      write(iuAb,'(A6,F6.2,A1)') 'QC(2)=', 100*perc_QC2, '%'
      write(iuAb,'(A6,F6.2,A1)') 'Convg=', 100*perc_Con, '%'
    endif
    
  ENDDO FilesLoop
  
  close(iuAb)        
          
end program qcRetrieval
