!$Id: ErrorHandling.f90 2189 2010-06-09 17:31:06Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:         ErrorHandling
! 
! Type:         F90 module
!
! Description:
!       Module that contains various subroutines related to the 
!       error handling from fortran, especially op_msg which
!       is a mimic function (in function) to the OSDPD function
!       by the same name. Its use is in lieu of linking to the 
!       OSDPD-specific subroutine
!
! Modules needed:
!       - utils
!       - misc
!
! Subroutines contained:
!       - OpenLogFile
!       - op_msg
!       - CloseLogFile
! 
! Data type included:
!       - iuLog
!
! Note to OSDPD USers:
!
!       - When linking to OPUS, in order to link to IBM's op_msg and EXIT_,
!         please modify the following:
!         * comment out op_msg
!         * replace STOP by EXIT_ in StopProcess 
!
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE ErrorHandling
  USE misc
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: OpenLogFile,op_msg,CloseLogFile
  PUBLIC :: StopProcess,ErrHandl
  !---Declaration sections (available across the module but not public)
  INTEGER                    :: iuLog
  !---Safe/Warning/Error Indicators
  INTEGER, PUBLIC            :: Safe=0,WarningType=1,ErrorType=2
  !---Error codes corresponding to table 'ErrorTab'
  INTEGER, PUBLIC            :: Err_NoFilesFound           = 1
  INTEGER, PUBLIC            :: Err_FileNotFound           = 2
  INTEGER, PUBLIC            :: Err_CRTMneCNTRL            = 3
  INTEGER, PUBLIC            :: Err_NOISEneCNTRL           = 4
  INTEGER, PUBLIC            :: Err_FEneYM                 = 5
  INTEGER, PUBLIC            :: Err_SEneNOISE              = 6
  INTEGER, PUBLIC            :: Err_SEneCRTM               = 7
  INTEGER, PUBLIC            :: Err_CovAtmIncons           = 8
  INTEGER, PUBLIC            :: Err_CovSfcIncons           = 9
  INTEGER, PUBLIC            :: Err_Atm_COVneTUN           = 10
  INTEGER, PUBLIC            :: Err_Sfc_COVneTUN           = 11
  INTEGER, PUBLIC            :: Err_Atm_LabelsInconsCov    = 12
  INTEGER, PUBLIC            :: Err_Atm_LabelsInconsCntrl  = 13
  INTEGER, PUBLIC            :: Err_Sfc_LabelsInconsCov    = 14
  INTEGER, PUBLIC            :: Err_Sfc_LabelsInconsCntrl  = 15
  INTEGER, PUBLIC            :: Err_DuplicateEDRselection  = 16
  INTEGER, PUBLIC            :: Err_InconsSize4XGandScene  = 17
  INTEGER, PUBLIC            :: Err_InconsNchanAndNGemis   = 18
  INTEGER, PUBLIC            :: Err_InconsNGne1_ws         = 19
  INTEGER, PUBLIC            :: Err_InconsNGne1_tsk        = 20
  INTEGER, PUBLIC            :: Err_InconsNGne1_deltaT     = 21
  INTEGER, PUBLIC            :: Err_InconsNGne1_sfcP       = 22
  INTEGER, PUBLIC            :: Err_NoGoodScanLineFound    = 23
  INTEGER, PUBLIC            :: Err_NotSupported           = 24
  INTEGER, PUBLIC            :: Err_NoClassFoundInCov      = 25
  INTEGER, PUBLIC            :: Err_InconsSiz              = 26
  INTEGER, PUBLIC            :: Err_NotImplemented         = 27
  INTEGER, PUBLIC            :: Err_DimensPb               = 28
  INTEGER, PUBLIC            :: Err_NoMatching             = 29
  INTEGER, PUBLIC            :: Err_SingularMatrx          = 30
  INTEGER, PUBLIC            :: Err_AbsolPathNotPresnt     = 31
  INTEGER, PUBLIC            :: Err_ReadingFile            = 32
  INTEGER, PUBLIC            :: Err_InconsNumber           = 33
  INTEGER, PUBLIC            :: Err_InconsPolar            = 34
  INTEGER, PUBLIC            :: Err_InconsFreq             = 35
  INTEGER, PUBLIC            :: Err_InconsLat              = 36
  INTEGER, PUBLIC            :: Err_InconsLon              = 37
  INTEGER, PUBLIC            :: Err_DifferFromExpect       = 38
  INTEGER, PUBLIC            :: Err_ScanPosPb              = 39
  INTEGER, PUBLIC            :: Err_AllocMemPb             = 40
  INTEGER, PUBLIC            :: Err_GDASfileAt00h          = 41
  INTEGER, PUBLIC            :: Err_Incompat               = 42
  INTEGER, PUBLIC            :: Err_NbPtsSelect            = 43
  INTEGER, PUBLIC            :: Err_NovalidOrbit           = 44
  INTEGER, PUBLIC            :: Err_OptionNotSupported     = 45
  INTEGER, PUBLIC            :: Err_InconsSfc              = 46
  INTEGER, PUBLIC            :: Err_NoFmsdrGenerated       = 47
  !---Warning codes corresponding to table 'WarningTab'
  INTEGER, PUBLIC            :: Warn_BadScanline           = 1
  INTEGER, PUBLIC            :: Warn_LatitudeGap           = 2
  INTEGER, PUBLIC            :: Warn_TimeGap               = 3
  INTEGER, PUBLIC            :: Warn_nProfDiffMeasAndCntrl = 4
  INTEGER, PUBLIC            :: Warn_nProfDiffMeasAndExt   = 5
  INTEGER, PUBLIC            :: Warn_nProfDiffAvailReques  = 6
  INTEGER, PUBLIC            :: Warn_NoMolecIndxFound      = 7
  INTEGER, PUBLIC            :: Warn_readInvalid           = 8
  INTEGER, PUBLIC            :: Warn_EndOfFile             = 9
  INTEGER, PUBLIC            :: Warn_NfilesNotIdentical    = 10
  INTEGER, PUBLIC            :: Warn_NprofilesNotIdentical = 11
  INTEGER, PUBLIC            :: Warn_TimeIncons            = 12
  INTEGER, PUBLIC            :: Warn_LatIncons             = 13
  INTEGER, PUBLIC            :: Warn_AngleIncons           = 14
  INTEGER, PUBLIC            :: Warn_NoValidWarmTarget     = 15
  INTEGER, PUBLIC            :: Warn_NotCoverROI           = 16

  CHARACTER(LEN=200), DIMENSION(20), PUBLIC :: WarningTab  =  (/     &
       '1. Warning: Reading bad scanline: Skipping...           ',   &
       '2. Warning: Latitude gap detected between scanlines.    ',   &
       '3. Warning: Time gap detected between scanlines.        ',   &
       '4. Warning: # profiles different -Measur vs Cntrl-      ',   &
       '5. Warning: # profiles different -Measur vs Extern-     ',   &
       '6. Warning: # profiles different -Available/Requested-  ',   &
       '7. Warning: No Molecule Indx found for molecule ID      ',   &
       '8. Warning: Warning: Error reading file.                ',   &
       '9. Warning: End of file reached.                        ',   &
       '10.Warning: Number of files not identical in 2 lists.   ',   &
       '11.Warning: Number of profiles not identical in 2 files.',   &
       '12.Warning: Error in time matching. Skipping..          ',   &
       '13.Warning: Error in latitude matching   . Skipping..   ',   &
       '14.Warning: Error in Angle matching. Skipping..         ',   &
       '15.Warning: No Valid warm target found in orbit.        ',   &
       '16.Warning: No scanline found in the ROI                ',   &
       '17.Warning:                                             ',   &
       '18.Warning:                                             ',   &
       '19.Warning:                                             ',   &
       '20.Warning:                                             '    &
       /)
       
  CHARACTER(LEN=200), DIMENSION(50), PUBLIC :: ErrorTab  = (/        & 
       '1. Err: No files found in list.               ', '2. Err: Open file error ...                   ', &
       '3. Err: Incons. #Chanls contrl/CRTM.          ', '4. Err: Incons. #Chanls contrl/Noise.         ', &
       '5. Err: Incons. #s in Fe/Ym.                  ', '6. Err: Incons. #s in Se/Noise.               ', &
       '7. Err: Incons. #s in Se/CRTM.                ', '8. Err: Incons. #s Params Cov matrx (Atm)     ', &
       '9. Err: Incons. #s of Parms in Cov matrx (Sfc)', '10.Err: Incons. #s of EDRs Cov/Tun (Atm).     ', &
       '11.Err: Incons. #s of EDRs Cov/Tun (Sfc).     ', '12.Err: Incons. EDRs labels: Cov/Tun -ATM-    ', &
       '13.Err: Incons. EDRs labels: Cntrl/Tun -ATM-  ', '14.Err: Incons. EDRs labels: Cov/Tun -Sfc-    ', &
       '15.Err: Incons. EDRs labels: Cntrl/Tun -Sfc-  ', '16.Err: EDR selected in Sfc & Atm covariances.', &
       '17.Err: Incons. Scene & Xg sizes for WVap     ', '18.Err: Incons. nchan<>nG(emiss)              ', &
       '19.Err: Incons. nG<>1(windsp)                 ', '20.Err: Incons. nG<>1(Tskin)                  ', &
       '21.Err: Incons. nG<>1(DeltaT)                 ', '22.Err: Incons. nG<>1(SFCP)                   ', &
       '23.Err: No good scanline found in file.       ', '24.Err: Subroutine is not used anymore.       ', &
       '25.Err: Class not found in cov/Bkg file.      ', '26.Err: Incons. sizes                         ', &
       '27.Err: Not implemented yet                   ', '28.Err: Error in dimensionality:              ', &
       '29.Err: No matching                           ', '30.Err: Singular Matrix                       ', &
       '31.Err: Absolute path must precede name       ', '32.Err: In reading file :                     ', &
       '33.Err: Incons. Numbers                       ', '34.Err: Incons. Polarizations                 ', &
       '35.Err: Incons. Frequencies                   ', '36.Err: Incons. Latitude(s)                   ', &
       '37.Err: Incons. Longitude(s)                  ', '38.Err: Different from expected value         ', &
       '39.Err: Scan Position Problem                 ', '40.Err: Allocation memory Problem             ', &
       '41.Err: Last GDAS file must be 00h next day   ', '42.Err: Incompatible                          ', &
       '43.Err: Error in number of points selected    ', '44.Err: No valid orbit found                  ', &
       '45.Err: Option Not supported                  ', '46.Err: Incons. surface type                  ', &
       '47.Err: No FMSDR generated                    ', '48.Err:                                       ', &
       '49.Err:                                       ', '50.Err:                                       '  &
       /)

CONTAINS


  SUBROUTINE StopProcess(Indx)
    INTEGER :: Indx
    IF (Indx.eq.0) STOP
    IF (Indx.gt.0) STOP 1
    !CALL EXIT(Indx)
  END SUBROUTINE StopProcess


  SUBROUTINE ErrHandl(iTyp,Indx,AttachString)
    INTRINSIC        :: trim
    INTRINSIC        :: adjustl
    INTEGER          :: iTyp,Indx
    CHARACTER(LEN=*) :: AttachString
    IF (iTyp.eq.ErrorType) THEN
       CALL op_msg(adjustl(trim(ErrorTab(Indx)))//' '//adjustl(trim(AttachString)))
       !---- make sure to close log file before exiting the program ----
       CALL CloseLogFile()
       CALL StopProcess(Indx)
    ELSE IF(iTyp.eq.WarningType) THEN
       WRITE(iuLog,'(a)') adjustl(trim(WarningTab(Indx)))//' '//adjustl(trim(AttachString))
    ENDIF
    RETURN
  END SUBROUTINE ErrHandl


!===============================================================
! Name:         OpenLogFile
!
!
! Type:         Subroutine
!
!
! Description:  Opens the log file
!
!
! Arguments:
!
!      Name              Type             Description
!      ---------------------------------------------------
!     - Logfile           I               Log File name 
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

  SUBROUTINE OpenLogFile(Logfile)
    CHARACTER(LEN=*) :: LogFile
    iuLog=get_lun()
    OPEN(iuLog,file=LogFile,form='formatted',status='unknown',position='append')
  END SUBROUTINE OpenLogFile

!===============================================================
! Name:         op_msg
!
!
! Type:         Subroutine
!
!
! Description: mimics the OPUS op_msg subroutine (on IBM) that 
!              writes the error/warnign message.
!
!
! Arguments:
!
!      Name                 Type          Description
!      ---------------------------------------------------
!      - message            I              message to print out
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

  SUBROUTINE op_msg(message)
    CHARACTER(LEN=*) :: message
    WRITE(iuLog,'(a)') message
    RETURN
  END SUBROUTINE op_msg

!===============================================================
! Name:         CloseLogFile
!
!
! Type:         Subroutine
!
!
! Description:  Closes the Log file
!
!
! Arguments: None. (uses the iuLog in the global section)
!
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

  SUBROUTINE CloseLogFile()
    CLOSE(iuLog)
  END SUBROUTINE CloseLogFile

END MODULE ErrorHandling



