!$Id:$
!===============================================================
! Name:       chopp
!
!
! Type:         Main Program
!
!
! Description:
!       Program that transforms the FM-SDRs  into EC-FMSDRs.
!       This should be done by applying  the bias correction.
!
! Modules needed:
!       - IO_MeasurData
!       - misc
!       - ErrorHandling
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

program chopp
  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  USE Consts
  IMPLICIT NONE
  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,INDEX,INT,LEN_TRIM,TRIM
 
  CHARACTER(LEN=250), DIMENSION(:),    POINTER     :: fmsdrFiles,dummyFiles
  CHARACTER(LEN=250)                               :: choppFile,xfile,yfile
  CHARACTER(LEN=10)                                :: pref
  CHARACTER(LEN=3)                                 :: FILEINDX
  INTEGER                                          :: nfiles,ifile,iu_list,iu_fmsdr,nProfs,np,np0,iprof
  INTEGER                                          :: isubFile,iu_subFile,ierr,indx,npref,nSiz,nEffProfs
  TYPE(MeasurData_type)                            :: Y      
  !---Namelist data 
  CHARACTER(LEN=250)                               :: fmsdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                               :: pathFMSDR_Chopp=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)                               :: LogFile=DEFAULT_VALUE_STR4
  INTEGER                                          :: nChoppedFiles=DEFAULT_VALUE_INT
  
  NAMELIST /ContrlChopping/fmsdrfileList,pathFMSDR_Chopp,nChoppedFiles,LogFile
  !---Read control-data from namelist
  READ(*,NML=ContrlChopping)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  !---Read the file names of RDR data and build TDR files names
  call ReadList(iu_list,trim(fmsdrfileList),fmsdrFiles,nFiles,dummyFiles,pathFMSDR_Chopp,'CHP_')
  IF (nfiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')
  !---------------------------------------------------------
  !  Loop over the FMSDR files
  !---------------------------------------------------------
  pref  = 'CHP'
  npref = len_trim(pref)
  FilesLoop: DO ifile=1,nFiles
    write(*,'(A)')'Input file='//fmsdrFiles(ifile)
    yFile=trim(adjustl(fmsdrFiles(ifile)))
    !----Read/Write the Headers of the RAD files
    CALL ReadHdrMeasurmts(fmsdrFiles(ifile),iu_fmsdr,nProfs,Y)
    write(*,*)'Number of profiles, nProfs=', nProfs
    write(*,*)'Number of Sub-Files, nChoppedFiles=', nChoppedFiles
    !----Compute number of profiles in sub-File
    np=int(nProfs/nChoppedFiles)
    write(*,*)'Expected average number of profiles in each Sub-File, np=', np
    write(*,*)
    
    !----Loop over the nChoppedFiles-1 sub-files (with identical # profiles)
    SubFilesLoop: DO isubFile=1,nChoppedFiles-1
       WRITE(FILEINDX,'(i3.3)') isubFile
       indx=INDEX(yfile,'/',.true.)
       nSiz=len_trim(xfile)-indx-npref
       xfile(1:nSiz)=trim(adjustl(yfile(indx+1+npref:len_trim(yfile))))
       choppFile = trim(adjustl(pathFMSDR_Chopp))//trim(adjustl(pref))//trim(adjustl(xfile(1:nSiz)))//'_'//FILEINDX
       CALL WriteHdrMeasurmts(choppFile,iu_subFile,np,Y%nqc,Y%nchan,Y%nPosScan,Y%CentrFreq,Y%polar,Y%nScanLines)
       nEffProfs=0
       ProfsLoop: do iprof=(isubFile-1)*np+1,(isubFile-1)*np+np
          !---Read the RAD data
          CALL ReadMeasurmts(iu_fmsdr,Y,ierr)
          !---QC-check
          IF (ierr.ne.0 .and. nEffProfs.eq.1) THEN 
             CALL ErrHandl(WarningType,Warn_readInvalid,'')
             CLOSE(iu_fmsdr)
             CYCLE FilesLoop
          ENDIF
          IF (ierr.eq.Warn_EndOfFile)   EXIT  ProfsLoop
          IF (ierr.eq.Warn_readInvalid) CYCLE ProfsLoop
          IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. Radiance file. (Chopp)')
          !---Write it to Chopped file
          nEffProfs=nEffProfs+1
          CALL WriteMeasurmts(iu_subFile,Y%nqc,Y%qc,Y%nchan,Y%angle,Y%tb,Y%lat,Y%lon,&
               Y%node,Y%secs,Y%julDAY,Y%Year,Y%iscanPos,Y%iscanLine,Y%RelAziAngle,Y%SolZenAngle)  
       ENDDO ProfsLoop
       print *, 'Sub-File number:',isubFile
       write(*,'(A)')trim(choppFile)
       print *, 'Number of effective profiles:',nEffProfs
       close(iu_subFile)
       write(*,*)
    ENDDO SubFilesLoop
    
    !----Process last chopped file
    WRITE(FILEINDX,'(i3.3)') nChoppedFiles
    indx=INDEX(yfile,'/',.true.)
    nSiz=len_trim(xfile)-indx-npref
    xfile(1:nSiz)=trim(adjustl(yfile(indx+1+npref:len_trim(yfile))))
    choppFile = trim(adjustl(pathFMSDR_Chopp))//trim(adjustl(pref))//trim(adjustl(xfile(1:nSiz)))//'_'//FILEINDX
    np0=nProfs-(nChoppedFiles-1)*np
    CALL WriteHdrMeasurmts(choppFile,iu_subFile,np0,Y%nqc,Y%nchan,Y%nPosScan,Y%CentrFreq,Y%polar,Y%nScanLines)
    nEffProfs=0
    do iprof=(nChoppedFiles-1)*np+1,nProfs
       !---Read the RAD data
       CALL ReadMeasurmts(iu_fmsdr,Y,ierr)
       IF (ierr.eq.Warn_EndOfFile)   EXIT  
       IF (ierr.eq.Warn_readInvalid) CYCLE 
       IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'. Radiance file. (Chopp)')
       !---Write it to Chopped file
       nEffProfs=nEffProfs+1
       CALL WriteMeasurmts(iu_subFile,Y%nqc,Y%qc,Y%nchan,Y%angle,Y%tb,Y%lat,Y%lon,&
            Y%node,Y%secs,Y%julDAY,Y%Year,Y%iscanPos,Y%iscanLine,Y%RelAziAngle,Y%SolZenAngle)  
    ENDDO
    print *, 'Sub-File number:',nChoppedFiles
    write(*,'(A)')trim(choppFile)
    print *, 'Number of effective profiles:',nEffProfs
    close(iu_subFile)
    write(*,*)

    CLOSE(iu_fmsdr)
  ENDDO FilesLoop
  
  !--- release memory 
  DEALLOCATE(fmsdrFiles)
  DEALLOCATE(dummyFiles)
  DEALLOCATE(Y%CentrFreq,Y%Rad,Y%qc,Y%Tb,Y%polar,Y%angle,Y%secant_view)
  
  CALL CloseLogFile()
end program chopp
