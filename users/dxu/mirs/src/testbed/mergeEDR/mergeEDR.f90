!===============================================================
! Name:       mergeEDR
!
!
! Type:         Main Program
!
!
! Description:
!       Merges the granulized EDR files into a full orbital file.
!
! Modules needed:
!       - IO_Scene
!       - misc
!       - utils
!       - ErrorHandling
!
!
! History:
!       04-22-2006      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

program mergeEDR
  USE IO_Scene       
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used
  INTRINSIC :: ADJUSTL,TRIM

  CHARACTER(LEN=3)                :: FILEINDX
  CHARACTER(LEN=250)              :: xFile,mergedFile,miniedrFile
  INTEGER                         :: iu_miniedr,nProfs,np,iprof
  INTEGER                         :: iMiniFile,iu_OrbFile,ierr
  TYPE(Scene_type)                :: Scn    

  !---Namelist data 
  CHARACTER(LEN=250)              :: FullOrbFileName=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)              :: pathEDR=DEFAULT_VALUE_STR4
  INTEGER                         :: nMiniEDRfiles=DEFAULT_VALUE_INT
  CHARACTER(LEN=250)              :: LogFile=DEFAULT_VALUE_STR4
  
  NAMELIST /ContrlMergeEDR/FullOrbFileName,pathEDR,nMiniEDRfiles,LogFile
  
  !---Read control-data from namelist
  READ(*,NML=ContrlMergeEDR)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of RDR data and build TDR files names
  !---------------------------------------------------------
  !  Loop over the orbit file to merge the chopped EDR files
  !---------------------------------------------------------
  xFile=trim(adjustl(FullOrbFileName))        
  mergedFile=trim(adjustl(FullOrbFileName))//'.ORB'
  print *, 'Processing :',mergedFile
  !----Read the header of each chopped files for total profile number for the full orbit 
  np=0
  DO iMiniFile=1,nMiniEDRfiles         
      WRITE(FILEINDX,'(i3.3)') iMiniFile
      miniedrFile=trim(adjustl(xFile))//'_'//FILEINDX
      CALL ReadHdrScene(iu_miniedr,miniedrFile,Scn,nProfs)
      np=np+nProfs
      CALL DestroyScene(Scn)
      CLOSE(iu_miniedr)
  ENDDO

  !----Write the header of the full orbt EDR file with totale number of profiles
  WRITE(FILEINDX,'(i3.3)') 1
  miniedrFile=trim(adjustl(xFile))//'_'//FILEINDX
  CALL ReadHdrScene(iu_miniedr,miniedrFile,Scn,nProfs)
  CLOSE(iu_miniedr)
  CALL WriteHdrScene(iu_OrbFile,mergedFile,Scn,np)
  CALL DestroyScene(Scn)
  
  !----Loop over the nMiniEDRfiles read/write the scene for each profile 
  Scn%ProfIndx=0
  DO iMiniFile=1,nMiniEDRfiles
      WRITE(FILEINDX,'(i3.3)') iMiniFile
      miniedrFile=trim(adjustl(xFile))//'_'//FILEINDX 
      !----Skip the header of the mini EDR files            
      CALL ReadHdrScene(iu_miniedr,miniedrFile,Scn,nProfs)
      ProfsLoop: do iprof=1,nProfs
        !---Read the EDR data from mini files
        CALL ReadScene(iu_miniedr,Scn,ierr)
        IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfsLoop
        IF (ierr.eq.Warn_readInvalid) CYCLE ProfsLoop
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. Scene file. (MergeEDR)')
        !---Write to full orbit EDR  files
        CALL WriteScene(iu_OrbFile,Scn)
      ENDDO ProfsLoop
      CALL DestroyScene(Scn)
      CLOSE(iu_miniedr) 
  ENDDO
  close(iu_OrbFile)
  CALL CloseLogFile()
end program mergeEDR
