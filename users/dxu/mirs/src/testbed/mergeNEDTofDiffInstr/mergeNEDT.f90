!$Id:$
!===============================================================
! Name:      mergeNEDT 
!
!
! Type:         Main Program
!
!
! Description:
!
!
! Modules needed:
!       - Noise
!       - IO_Noise
!       - ErrorHandling
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

PROGRAM mergeNEDT
  USE Noise
  USE IO_Noise
  USE ErrorHandling
  USE Consts
  Implicit none
  INTEGER, PARAMETER         :: mxchan = 200
  !---NEDT variables
  INTEGER                    :: nchan1,nchan2,nchan,nchanNominal
  INTEGER                    :: ichan
  INTEGER, DIMENSION(mxChan) :: ComputedOrDefault
  REAL,    DIMENSION(mxChan) :: cFreq1,cFreq2,ErRMS1,ErRMS2
  REAL,    DIMENSION(mxChan) :: cFreq,ErRMS,cFreqNominal,ErRMSNominal
  !---Namelist Data
  CHARACTER(LEN=250)         :: NEDTfile1=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)         :: NEDTfile2=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)         :: NEDTfile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)         :: NominalNEDTfile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)         :: Logfile=DEFAULT_VALUE_STR4
  NAMELIST /MergeNEDTCntrl/NEDTfile1,NEDTfile2,NEDTfile,NominalNEDTfile,LogFile

  !---Read Control parameters
  READ(*,NML=MergeNEDTCntrl)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  ComputedOrDefault=0
  !--------------------------------------------------
  !  First section: Merge NEDTs
  !--------------------------------------------------

  !---Read NEDT file #1
  CALL LoadNoise(NEDTfile1)
  nchan1           = noiseInfo%nChan
  IF (nchan1 .gt. mxchan) CALL ErrHandl(ErrorType,Err_DimensPb,'(increase mxchan)') 
  Cfreq1(1:nChan1) = noiseInfo%CentrFreq(1:nchan1)
  ErRMS1(1:nChan1) = noiseInfo%rms(1:nChan1)
  DEALLOCATE(noiseInfo%CentrFreq,noiseInfo%rms,noiseInfo%nedt)

  !---Read NEDT file #2
  call LoadNoise(NEDTfile2)
  nchan2            = noiseInfo%nChan
  IF (nchan2 .gt. mxchan) CALL ErrHandl(ErrorType,Err_DimensPb,'(increase mxchan)') 
  Cfreq2(1:nchan2)  = noiseInfo%CentrFreq(1:nchan2)
  ErRMS2(1:nchan2)  = noiseInfo%rms(1:nchan2)
  DEALLOCATE(noiseInfo%CentrFreq,noiseInfo%rms,noiseInfo%nedt)

  !---Read NEDT file #3 (Nominal NEDTs)
  call LoadNoise(NominalNEDTfile)
  nchanNominal            = noiseInfo%nChan
  IF (nchanNominal .gt. mxchan) CALL ErrHandl(ErrorType,Err_DimensPb,'(increase mxchan)') 
  CfreqNominal(1:nchanNominal)  = noiseInfo%CentrFreq(1:nchanNominal)
  ErRMSNominal(1:nchanNominal)  = noiseInfo%rms(1:nchanNominal)
  DEALLOCATE(noiseInfo%CentrFreq,noiseInfo%rms,noiseInfo%nedt)

  !---Merge NEDTs
  nchan=nchan1+nchan2
  IF (nchan .gt. mxchan) CALL ErrHandl(ErrorType,Err_DimensPb,'(increase mxchan)')
  IF (nchan .ne. nchanNominal) CALL ErrHandl(ErrorType,Err_DimensPb,'(nchan \= nchanNominal)')
  cFreq(1:nchan) = (/Cfreq1(1:nChan1),Cfreq2(1:nchan2)/)
  ErRMS(1:nchan) = (/ErRMS1(1:nChan1),ErRMS2(1:nchan2)/)

  !---Check and replace invalid NEDT with Nominal values
  DO ichan=1,nchan
     IF (ErRMS(ichan) .lt. 0) THEN
        ErRMS(ichan)=ErRMSNominal(ichan)
        ComputedOrDefault(ichan)=1
     ENDIF
  ENDDO

  !---Output merged data
  CALL WriteNoise(NEDTfile,CFreq,ErRMS,ComputedOrDefault,nchan)
  CALL CloseLogFile()

END PROGRAM mergeNEDT
