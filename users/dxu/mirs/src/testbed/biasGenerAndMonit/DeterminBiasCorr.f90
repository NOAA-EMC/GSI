!===============================================================
! Name:       determinBiasCorr
!
!
! Type:         Main Program
!
!
! Description:
! 	This program aims at determining the bias correction to be
! 	applied on radiances before the retrieval process.
! 	This is done by determining the bias and slope corrections
! 	needed for each channel to fit the simulated radiances
! 	by the measurements.
!	
!
! Modules needed:
!       - IO_MeasurData
!       - misc
!       - Consts
!
! History:
!       10-22-2005      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

program determinBiasCorr
  USE IO_MeasurData
  USE IO_Misc
  USE misc  
  USE Consts
  USE ErrorHandling
  implicit none
  INTEGER, PARAMETER                        :: mxchan=30,mxProf=1000000
  INTEGER, PARAMETER                        :: iu_DetailOut=40
  INTEGER, PARAMETER                        :: npos=1 
  REAL,    PARAMETER                        :: epsil=0.001
  CHARACTER(LEN=250), DIMENSION(:), POINTER :: fwdFiles,radFiles,dumFiles
  INTEGER                                   :: iu_listRad,iu_listFWD
  INTEGER                                   :: iu_rad,iu_fwd,ierr
  INTEGER                                   :: nFilesRad,nFilesFWD,ifile,nfiles
  INTEGER                                   :: nRad,nFWD,nchan,ichan,nProfs,iprof,ipos
  INTEGER                                   :: iprofProcessed,nprofsProcessed,stype,stypeCRTM
  REAL                                      :: xalt,xcover
  REAL,   DIMENSION(mxchan,mxProf)          :: tbdiff
  REAL,   DIMENSION(npos,mxchan)            :: Bias,Slope,Intercept
  CHARACTER(LEN=5), DIMENSION(5)            :: polars=(/'VH','VC','HC','VV','HH'/)
  !---Structures containing the files contents
  TYPE(MeasurData_type)                     :: Yrad,Yfwd      
  !---Namelist data 
  INTEGER             :: DetailOut
  CHARACTER(LEN=250)  :: FWDsimulAnalysList,RadList,BiasFile,Topogr,DetailOutFile
  NAMELIST /BiasDetermin/FWDsimulAnalysList,RadList,BiasFile,Topogr,DetailOut,DetailOutFile
  !---Read control-data from namelist
  READ(*,NML=BiasDetermin)
  !---Read the file names of FWD simulations
  call ReadList(iu_listFWD,trim(FWDsimulAnalysList),FWDFiles,nFilesFWD,dumFiles,'./','XXX_')
  IF (nfilesFWD .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(FWD files)') 
  !---Read the file names of Radiances 
  call ReadList(iu_listRad,trim(RadList),RadFiles,nFilesRad,dumFiles,'./','XXX_')
  IF (nfilesRAD .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(RAD files)')
  !---Consistency checks
  nfiles=minval((/nfilesRAD,nfilesFWD/))
  IF (nfilesFWD .ne. nfilesRAD) CALL ErrHandl(WarningType,Warn_NfilesNotIdentical,'Smaller assumed.')
  !---Potentially, open the detailed output file
  IF (DetailOut.eq.1) OPEN(iu_DetailOut,file=DetailOutFile,form='formatted',status='unknown')
  !--------------------------------------------------------------------
  !   Loop over the files
  !--------------------------------------------------------------------
  iprofProcessed=0
  FilesLoop: do ifile=1,nfiles
     !----Read the Headers of the FWD and RAD files
     CALL ReadHdrMeasurmts(FWDfiles(ifile),iu_fwd,nFWD,Yfwd)
     CALL ReadHdrMeasurmts(RADfiles(ifile),iu_rad,nRAD,Yrad)
     !---Consistency checks
     IF (Yfwd%nchan .ne. Yrad%nchan) CALL ErrHandl(ErrorType,Err_InconsNumber,'of channels in RAD/FWD')
     nchan=Yfwd%nchan
     IF (ANY(Yfwd%polar(1:nchan) .ne. Yrad%polar(1:nchan))) &
          CALL ErrHandl(ErrorType,Err_InconsPolar,'in RAD/FWD')
     IF (ANY(abs(Yfwd%CentrFreq(1:nchan)-Yrad%CentrFreq(1:nchan)).gt.epsil)) &
          CALL ErrHandl(ErrorType,Err_InconsFreq,'in RAD/FWD')
     nProfs=minval((/nFWD,nRAD/))
     IF (nFWD.ne.nRAD) CALL ErrHandl(WarningType,Warn_NprofilesNotIdentical,'Lowest value assumed.')
     !---Potentially, output header in detailed comparison file
     IF (DetailOut.eq.1 .and. ifile.eq.1) THEN
        Write(iu_DetailOut,'(i4)')     nchan 
        Write(iu_DetailOut,'(15f9.3)') Yfwd%CentrFreq(1:nchan)
        Write(iu_DetailOut,'(30i3)')   Yfwd%polar(1:nchan)
     ENDIF
     !--------------------------------------------------------------------
     !   Loop over the number of measurements within the files
     !--------------------------------------------------------------------
     ProfsLoop: do iprof=1,nprofs
        !---Read the FWD and RAD data
        CALL ReadMeasurmts(iu_fwd,Yfwd,ierr)
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'(Fwd)') 
        CALL ReadMeasurmts(iu_rad,Yrad,ierr)
        IF (ierr.ne.0) CALL ErrHandl(ErrorType,Err_ReadingFile,'(Measur.)') 
        !---Consistency checks
        IF (abs(Yfwd%lat-Yrad%lat) .gt. epsil) CALL ErrHandl(ErrorType,Err_InconsLat,'')  
        IF (abs(Yfwd%lon-Yrad%lon) .gt. epsil) CALL ErrHandl(ErrorType,Err_InconsLon,'')  
        !---Determine the surface type 
        call Read_topography(Topogr,Yfwd%lat,Yfwd%lon,xalt,stype,xcover,stypeCRTM)
        !---Account only for ocean cases
        IF (stype.eq.OC_TYP) THEN
           iprofProcessed=iprofProcessed+1
           ChanLoop: DO ichan=1,nchan
              Tbdiff(ichan,iprofProcessed) = Yfwd%tb(ichan) - Yrad%tb(ichan)
           END DO ChanLoop
           IF (DetailOut.eq.1) THEN
              write(iu_DetailOut,'(i10,2f12.4)')  iprofProcessed,Yfwd%lat,Yfwd%lon
              write(iu_DetailOut,'(10f12.4)') Yfwd%tb(1:nchan)
              write(iu_DetailOut,'(10f12.4)') Tbdiff(1:nchan,iprofProcessed)
           ENDIF
        ENDIF
     enddo ProfsLoop
     !---Close files
     close(iu_fwd)
     close(iu_rad)
  enddo FilesLoop
  !---Compute the channel/channel bias and output it
  nprofsProcessed=iprofProcessed
  DO ichan=1,nchan
     do ipos=1,npos
        Bias(ipos,ichan)=SUM(Tbdiff(ichan,1:nprofsProcessed))/nprofsProcessed
        Slope(ipos,ichan)=1.
        Intercept(ipos,ichan)=0.
        write(*,'(i4,6x,f12.5,6x,a5,6x,f7.2)') ichan,Yfwd%CentrFreq(ichan),&
             polars(Yfwd%polar(ichan)),Bias(ipos,ichan)
        write(*,'(a)') '-----------------------------------------------------------'
     ENDDO
  ENDDO
  CALL WriteBias(BiasFile,nchan,npos,Yfwd%CentrFreq,Bias,Slope,Intercept)
  CLOSE(iu_DetailOut)
end program determinBiasCorr
