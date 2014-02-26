!$Id: prepNWP.f90 2533 2011-03-15 17:17:23Z wchen $
!===============================================================
! Name:    prepNWP
!
!
! Type:         Main Program
!
!
! Description:
!       Program that get start time and end time of orbits
!
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!       - IO_MeasurData
!       - IO_Scene
!       - IO_Misc
!       - GeophCovBkg
!       - ErrorHandling
!       - Preclassif
!       - Type_Kinds
!       - Surface_Emissivity_Model
!       - CRTM_Fastem3
!       - Binary_File_Utility
!
! History:
!       03-15-2011      Wanchun Chen           Original Coder
!
!===============================================================

program prepNWP

  USE Consts
  USE misc
  USE utils
  USE IO_MeasurData
  USE ErrorHandling

  implicit none
  TYPE(MeasurData_type) :: Ym
  
  INTEGER            :: iu,iuMeasur,nfile=0,nprof,iprof,ierr,iuOut
  CHARACTER(LEN=256) :: file,fileOut
  INTEGER            :: year1,year2,month1,month2,day1,day2,jday1,jday2
  INTEGER            :: sec1,sec2
  
  
  !---- Namelist data 
  CHARACTER(LEN=256) :: RadFileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=16)  :: satId='npp'
  
  NAMELIST /prepNWPNameList/RadFileList,satId,fileOut
  
  READ(*,NML=prepNWPNameList)
  
  iuOut=get_Lun()
  open(iuOut,file=trim(fileOut),form='formatted')
  
  iu=get_Lun()
  open(iu,file=trim(RadFileList),status='old',form='formatted')
  nfile = 0
  do while(.true.)
    read(iu,'(A)', err=100, end=100) file
    
    call ReadHdrMeasurmts(TRIM(file),iuMeasur,nprof,Ym)
    ProfLoop: DO iprof=1,nprof
      call ReadMeasurmts(iuMeasur,Ym,ierr)
      if( iprof .eq. 1     ) then
        year1 = Ym%year
        month1 = Ym%month
        day1 = Ym%day
        jday1 = Ym%julday
        sec1 = INT(Ym%secs)
      endif
      if( iprof .eq. nprof ) then
        year2 = Ym%year
        month2 = Ym%month
        day2 = Ym%day
        jday2 = Ym%julday
        sec2 = INT(Ym%secs)
      endif
    ENDDO ProfLoop
    close(iuMeasur)
    
    write(iuOut,'(A,I5,I4,I6,I5,I4,I6)') TRIM(file),year1,jday1,sec1, year2,jday2,sec2
   
    nfile = nfile + 1
  enddo
100 continue
  close(iu)
  close(iuOut)
  
end program prepNWP
