!$Id: IO_Monitor.f90 3365 2013-10-15 05:36:54Z tislam $

!-----------------------------------------------------------------------------------------------
! Name:         IO_Monitor
! 
! Type:         F90 module
!
! Description:
!       Module that contains the necessary routines and corresponding data
!       related to handling monitoring data (iteration/iteration).
!
! Modules needed:
!       - misc
!       - TuningParams
!
! Subroutines contained:
!       - WriteHdrMonitor
!       - WriteMonitor
!
! Data type included:
!       - none
!
! 
! History:
!      2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_Monitor
  USE misc
  USE TuningParams
  IMPLICIT NONE
  PRIVATE
  !----Publicly available subroutine(s)
  PUBLIC :: WriteHdrMonitor,WriteMonitor,WriteJacobians
  PUBLIC :: WriteHdrPrepDiagnostic,WritePrepDiagnostic
  PUBLIC :: ReadHdrPrepDiagnostic,ReadPrepDiagnostic
  PUBLIC :: WriteHdrDiagnostic,WriteDiagnostic
  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTR,EXP,SIZE
  
CONTAINS

!===============================================================
! Name:         WriteHdrMonitor
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the header of the monitoring file.
!               This file is aimed at being read by an IDL 
!               specialized tool to monitor the evolution
!               of the retrieval iterations.
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - MonitorFile       I              Monitoring file
!       - iu                O              Unit number
!       - nData             I              Number of measurements
!       - press_lev         I              Level pressure grid
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

  SUBROUTINE WriteHdrMonitor(MonitorFile,iu,nData,press_lev)
    CHARACTER(LEN=*)        :: MonitorFile
    INTEGER                 :: iu,nData,nTun,i
    REAL,     DIMENSION(:)  :: press_lev
    Ntun=size(TunParams)
    !---Open file containing iterative-process monitoring data
    iu=get_lun()
    OPEN(iu,file=MonitorFile,form='formatted',status='unknown')
    WRITE(iu,'(a)') '*********************************************************************'
    WRITE(iu,'(a25,i8)')     'Number of Measurements:',nData  
    WRITE(iu,'(a25,i8)')     '#  attempts requested :',nTun 
    WRITE(iu,'(a)') '----ATMOSPHERE-----'
    WRITE(iu,'(a25,i8)') 'nLev=nLay:',size(press_lev)
    WRITE(iu,'(a25)') 'Pressure Level-grid:'
    WRITE(iu,'(10f10.3)') press_lev
    WRITE(iu,'(a25,2i15)')   '# Atmosph EDRs :',(TunParams(i)%nEDRs_atm,i=1,nTun)
    DO i=1,nTun
       WRITE(iu,'(a25,15a10)') 'EDRs :',adjustr(TunParams(i)%EDR_Label_atm(1:TunParams(i)%nEDRs_atm))
       WRITE(iu,'(a25,15i10)') '#EOFs:',TunParams(i)%EDR_nEOF_atm(1:TunParams(i)%nEDRs_atm)
    ENDDO
    WRITE(iu,'(a)') '----SURFACE-----'
    WRITE(iu,'(a25,2i15)')   '# Surface EDRs :',(TunParams(i)%nEDRs_sfc,i=1,nTun)
    DO i=1,nTun
       WRITE(iu,'(a25,15a10)') 'EDRs :',adjustr(TunParams(i)%EDR_Label_sfc(1:TunParams(i)%nEDRs_sfc))
       WRITE(iu,'(a25,15i10)') '#EOFs:',TunParams(i)%EDR_nEOF_sfc(1:TunParams(i)%nEDRs_sfc)
    ENDDO
    WRITE(iu,'(a)') '----CHANNELS SELECTION-----'
    WRITE(iu,'(a25,2i15)')   'Channels selection flag :',(TunParams(i)%ChanSelectFlag,i=1,nTun)
    WRITE(iu,'(a25,2f15.2)') 'ChiSquare Threshold:',(TunParams(i)%ChiSqThresh,i=1,nTun)
    WRITE(iu,'(a)') '*********************************************************************'
    WRITE(iu,'(a)') 
    WRITE(iu,'(a)') 
    RETURN
  END SUBROUTINE WriteHdrMonitor


!===============================================================
! Name:        WriteMonitor
!
!
! Type:        Subroutine
!
!
! Description:  Writes the content of the monitoring file.
!
!
! Arguments:
!
!           Name                    Type          Description
!      ---------------------------------------------------
!       - iu                         I            Unit number
!       - iprof                      I            Profile index
!       - nEDRselec                  I            # EDRs selected for retr.
!       - nGselec                    I            Total # parameters of selected EDRs
!       - nIterTot                   I            Total # of iterations
!       - nAttempTot                 I            Total # attempts made for the retr.
!       - nchan                      I            Number of channels
!       - nch                        I            Number of selected chanels for retr
!       - Xg                         I            State vector for all iterations
!       - SfcPress                   I            Surface pressure
!       - ParamLabel                 I            Labels of EDRs selected
!       - ParamIndx                  I            Indexes of EDRs within Xg
!       - ParamLength                I            Lengthes of EDRs within Xg
!       - iSpaceModeFlag             I            Way EDRs are treated (log, natural, etc)
!       - Xb                         I            Background vector
!       - Ym                         I            Measurements vector
!       - Y                          I            Simulated TBs array (for all iterations)
!       - ShrkRadVec                 I            Index vector pointing 2 selected channels
!       - cfreq                      I            Central Frequencies
!       - polar                      I            Polarizations
!       - ChiSq                      I            Convergence metric Chi Square
!       - A                          I            Average Kernel
!       - S                          I            Uncertainty matrix
!       - Sa                         I            Background covaiance matrix
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

  SUBROUTINE WriteMonitor(iu,iprof,nEDRselec,nGselec,nIterTot,nAttempTot,nchan,  &
       nch,Xg,SfcPress,ParamLabel,ParamIndx,ParamLength,iSpaceModeFlag,Xb,Ym,Y,  &
       ShrkRadVec,cfreq,polar,ChiSq,A,S,Sa,AtmClass,SfcClass)
    INTEGER :: iu,iprof,nEDRselec,nGselec,nIterTot,nAttempTot,nchan,nch
    REAL,             DIMENSION(nGselec,0:nIterTot) :: Xg,Xg0
    CHARACTER(LEN=*), DIMENSION(nEDRselec)          :: ParamLabel
    INTEGER,          DIMENSION(nEDRselec)          :: ParamIndx
    INTEGER,          DIMENSION(nEDRselec)          :: ParamLength,iSpaceModeFlag
    INTEGER,          DIMENSION(nch)                :: ShrkRadVec
    REAL,             DIMENSION(nGselec)            :: Xb,Xb0
    REAL,             DIMENSION(nchan)              :: Ym
    REAL,             DIMENSION(nchan,0:nIterTot)   :: Y
    REAL,             DIMENSION(nchan)              :: Cfreq
    INTEGER,          DIMENSION(nchan)              :: polar
    REAL,             DIMENSION(0:nIterTot)         :: ChiSq
    REAL,             DIMENSION(nGselec,nGselec)    :: A,S,Sa
    REAL                                            :: SfcPress
    INTEGER                                         :: AtmClass,SfcClass
    !---Local variables
    INTEGER :: iEDR,iparam,iG,nG,ich,imap,iMode
    WRITE(iu,'(a)') '-----------------------------------------------------------'
    WRITE(iu,'(a25,i10)') 'Profile# :',iprof
    WRITE(iu,'(a25,i10)') '# of EDRs retrieved :',nEDRselec
    WRITE(iu,'(a25,i10)') '# of iterations :',nIterTot
    WRITE(iu,'(a25,i10)') '# of retrieval attempt :',nAttempTot
    WRITE(iu,'(a25,10a10)') 'EDRs retrieved :',adjustr(ParamLabel(1:nEDRselec))
    WRITE(iu,'(a25,10i10)') '# params in each EDR :',ParamLength(1:nEDRselec)
    WRITE(iu,'(a)') 
    WRITE(iu,'(a25,f12.4)') 'Surface Pressure :',SfcPress
    WRITE(iu,'(a25,i4)') 'Atmosphere class :',AtmClass
    WRITE(iu,'(a25,i4)') 'Surface class :',SfcClass
    WRITE(iu,'(a)') 'State Vector monitoring:'
    DO iEDR=1,nEDRselec
       iG     = ParamIndx(iEDR)
       nG     = ParamLength(iEDR)
       iMode  = iSpaceModeFlag(iEDR)
       Xb0(iG:iG+nG-1)               = Xb(iG:iG+nG-1)
       Xg0(iG:iG+nG-1,0:nIterTot)    = Xg(iG:iG+nG-1,0:nIterTot)
       IF(iMode .eq. 1)THEN
          Xb0(iG:iG+nG-1)            = exp(Xb(iG:iG+nG-1))
          Xg0(iG:iG+nG-1,0:nIterTot) = exp(Xg(iG:iG+nG-1,0:nIterTot))
       ENDIF
       IF(iMode .eq. 2)THEN
          Xb0(iG:iG+nG-1)            = exp(-exp(Xb(iG:iG+nG-1)))
          Xg0(iG:iG+nG-1,0:nIterTot) = exp(-exp(Xg(iG:iG+nG-1,0:nIterTot)))
       ENDIF
       DO iparam=iG,iG+nG-1
          WRITE(iu,'(i2,a10,i4,40f12.6)') iEDR,adjustr(ParamLabel(iEDR)),      &
               iparam-iG+1,Xb0(iparam),Xg0(iparam,0:nIterTot)
       ENDDO
    ENDDO
    WRITE(iu,'(a)') 
    WRITE(iu,'(a)') 'Radiance Vector monitoring:'
    WRITE(iu,'(a25,i10)') 'Total # of channels:',nchan
    WRITE(iu,'(a25,i10)') '# of channels used:',nch
    WRITE(iu,'(a25,100i3)') 'Channels used:',ShrkRadVec(1:nch)
    DO ich=1,nch
       imap=ShrkRadVec(ich)
       WRITE(iu,'(3i4,f12.4,40f8.2)') ich,imap,polar(imap),Cfreq(imap),&
            Y(imap,0:nIterTot),Ym(imap)
    ENDDO
    WRITE(iu,'(a25,40f12.3)') 'Resulting ChiSquare:',ChiSq(0:nIterTot)
    RETURN
  END SUBROUTINE WriteMonitor

!===============================================================
! Name:         WriteHdrPrepDiagnostic
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the header of the prepDiagnostic data file.
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - PrepDiagFile       I           PrepDiagnostic data file
!       - iu                 O           Unit number
!       - sensorID           I           Sensor Number ID
!       - AlgSN              I           Algorithm Serial number
!       - nProfiles          I           Number of Porfiles
!       - nqc                I           Number of Quality Flags 
!       - nchan              I           Total Number of channels
!       - nch                I           Number of channels used
!       - CentrFreq          I           Center Frequency (GHz)
!       - Indxchan           I           Index of channels used
!       - nLev               I           Number Pressure Levels
!       - press_lev          I           Level pressure grid
!       - nLay               I           Number Pressure Layers
!       - press_lay          I           Layer pressure grid
!       - ParamLabelAtm      I           Label of Atmospheric Parameters
!       - ParamLabelSfc      I           Label of Surface Parameters
!       - nGatm              I           Number Atmospheric Parameters
!       - nGsfc              I           Number Surface Parameters
!       - EDR_nEOF_atm1      I           Number of Atmospheric EOFs for 1st Attempt
!       - EDR_nEOF_atm2      I           Number of Atmospheric EOFs for 2nd Attempt
!       - EDR_nEOF_sfc1      I           Number of Surface EOFs for 1st Attempt
!       - EDR_nEOF_sfc2      I           Number of Surface EOFs for 2nd Attempt
!       - Se                 I           Measurement Error Covariance Matrix
!
! Modules needed:
!       - None
!
!
! History:
!       05-17-2012       Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR     
!
!===============================================================

  SUBROUTINE WriteHdrPrepDiagnostic(iu,PrepDiagFile,sensorID,AlgSN,nProfiles,nqc,nchan,nch, &
       CentrFreq,Polar,Indxchan,nabsorb,AbsorbID,nPosScan,nScanLines,nLev,press_lev,nLay, &
       press_lay,ParamLabelAtm,ParamLabelSfc,nGatm,nGsfc,EDR_nEOF_atm1,EDR_nEOF_atm2, &
       EDR_nEOF_sfc1,EDR_nEOF_sfc2,Se)
    !---Input/Output Variables
    CHARACTER(LEN=*)        :: PrepDiagFile
    INTEGER, INTENT(IN)     :: sensorID,nqc,AlgSN,nProfiles
    INTEGER, INTENT(IN)     :: nchan,nch,nLev,nLay,nabsorb
    INTEGER, INTENT(IN)     :: nPosScan,nScanLines
    INTEGER, DIMENSION(:), INTENT(IN) :: Indxchan
    INTEGER, DIMENSION(:), INTENT(IN) :: nGatm
    INTEGER, DIMENSION(:), INTENT(IN) :: nGsfc
    INTEGER, DIMENSION(:), INTENT(IN) :: EDR_nEOF_atm1
    INTEGER, DIMENSION(:), INTENT(IN) :: EDR_nEOF_atm2
    INTEGER, DIMENSION(:), INTENT(IN) :: EDR_nEOF_sfc1
    INTEGER, DIMENSION(:), INTENT(IN) :: EDR_nEOF_sfc2
    INTEGER, DIMENSION(:), INTENT(IN) :: Polar
    INTEGER, DIMENSION(:), INTENT(IN) :: AbsorbID
    REAL, DIMENSION(:), INTENT(IN)    :: CentrFreq
    REAL, DIMENSION(:), INTENT(IN)    :: press_lev
    REAL, DIMENSION(:), INTENT(IN)    :: press_lay
    REAL, DIMENSION(:,:), INTENT(IN)  :: Se
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: ParamLabelAtm,ParamLabelSfc
    !---Local Variables
    INTEGER        :: iu,iLay,ichan,nEDRs_atm,nEDRs_sfc
    !Variable Initialization
    nEDRs_atm     = size(ParamLabelAtm)
    nEDRs_sfc     = size(ParamLabelSfc)
    !---Open file containing PrepDiagnostic data
    iu=get_lun()
    OPEN(iu,file=PrepDiagFile,form='formatted',status='unknown')
    print*,'Open PrepDiagnostic Data File... '
    !---Writing Header information
    WRITE(iu,'(a35)') '    Header Information   '
    WRITE(iu,'(a25,i10)') 'Sensor ID :',sensorID
    WRITE(iu,'(a25,i10)') 'MiRS Version :',AlgSN
    WRITE(iu,'(a25,i10)') '# nProfile :',nProfiles
    WRITE(iu,'(a25,i10)') '# Quality Control Flags :',nqc
    WRITE(iu,'(a25,i10)') 'Total # of channels :',nchan
    WRITE(iu,'(a25,i10)') '# of channels used :',nch
    WRITE(iu,'(a25,i10)') '# Pressure Levels :',nLev
    WRITE(iu,'(a25,i10)') '# Pressure Layers :',nLay
    WRITE(iu,'(a25,i10)') 'Total # Atmos. EDRs :',nEDRs_atm
    WRITE(iu,'(a25,i10)') 'Total # Sfc. EDRs :',nEDRs_sfc
    WRITE(iu,'(a25,i10)') 'nabsorb :',nabsorb
    WRITE(iu,'(a25,i10)') 'nPosScan :',nPosScan
    WRITE(iu,'(a25,i10)') 'nScanLines :',nScanLines
    WRITE(iu,'(a25,110(f10.3))') 'Pressure Levels (mb) :', press_lev(1:nLev)
    WRITE(iu,'(a25,110(f10.3))') 'Pressure Layers (mb) :',press_lay(1:nLay)
    WRITE(iu,'(a25,30(f10.3))') 'Center Freq. (GHz) :',CentrFreq(1:nchan)
    WRITE(iu,'(a25,30(i4))') 'Polar information :',Polar(1:nchan)
    WRITE(iu,'(a25,30(i4))') 'Index channels used :',Indxchan(1:nch)
    WRITE(iu,'(a25,30(i4))') 'AbsorbID :',AbsorbID(1:nabsorb)
    WRITE(iu,'(a)') ''
    WRITE(iu,'(a25,20(a10))') 'Label Atmos. Params. :',ParamLabelAtm(1:nEDRs_atm)
    WRITE(iu,'(a25,20(a10))') 'Label Sfc. Params. :',ParamLabelSfc(1:nEDRs_sfc)
    WRITE(iu,'(a25,20(i4))') '# Atmospheric Params. :',nGatm(1:nEDRs_atm)
    WRITE(iu,'(a25,20(i4))') '# Surface Params. :',nGsfc(1:nEDRs_sfc)
    WRITE(iu,'(a25,20(i4))') '# 1st Atmpt. Atmo. EOFs :',EDR_nEOF_atm1(1:nEDRs_atm)
    WRITE(iu,'(a25,20(i4))') '# 1st Atmpt. Sfc. EOFs :',EDR_nEOF_sfc1(1:nEDRs_sfc)
    WRITE(iu,'(a25,20(i4))') '# 2nd Atmpt. Atmo. EOFs :',EDR_nEOF_atm2(1:nEDRs_atm)
    WRITE(iu,'(a25,20(i4))') '# 2nd Atmpt. Sfc. EOFs :',EDR_nEOF_sfc2(1:nEDRs_sfc)
    WRITE(iu,'(a)') ''
    WRITE(iu,'(a)') 'Se (Measurement Error Covariance) Matrix :'
    DO ichan=1, nchan
       WRITE(iu,'(30(f20.15))') Se(ichan,1:nchan)
    ENDDO
    RETURN
  END SUBROUTINE WriteHdrPrepDiagnostic

!===============================================================
! Name:        WritePrepDiagnostic
!
!
! Type:        Subroutine
!
!
! Description:  Writes out the content of the prepDiagnostic file.
!
!
! Arguments:
!
!           Name                 Type          Description
!      ---------------------------------------------------
!       - iu                      I         Unit number
!       - iprof                   I         Profile index
!       - iattempt                I         Number of attempts
!       - nEDRselec               I         Number of EDRs selected for retrieval
!       - ParamLabel              I         Labels of EDRs selected
!       - ParamLength             I         Length of EDRs within Xg
!       - ParamIndx               I         Indexes of EDRs within Xg
!       - Fe                      I         Model Error Covariance Matrix
!       - Sa                      I         Geophysical Error Covariance Matrix
!       - K                       I         Jacobian Matrix
!       - Xb                      I         Geophysical Background Vector
!       - X1st                    I         First Guess Vector
!       - Xg                      I         Retrieval State Vector
!       - Y                       I         Observed Brightness Temperature
!       - Ycorr                   I         Bias Corrected Brightness Temperature
!       - Fi                      I         Forward Brightness Temperature
!       - Psfc                    I         Surface Pressure
!       - Lat                     I         Latitude
!       - Lon                     I         Longitude
!       - time                    I         Seconds within the Day
!       - Sfctype                 I         Surface Type
!       - Year                    I         Year
!       - Month                   I         Month
!       - Day                     I         Day
!       - node                    I         node
!       - iscanPos                I         Scan position
!       - iscanLine               I         Scan line index
!       - RelAziAngle             I         Relative Azimuth Angle
!       - SolZenAngle             I         Solar Zenith Angle
!       - angle                   I         Viewing Angle
!       - qc                      I         Qulity Control Flag
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-17-2012       Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR       
!
!===============================================================

  SUBROUTINE WritePrepDiagnostic(iu,iprof,iattempt,nEDRselec,ParamLabel,ParamLength,ParamIndx, &
       Fe,Sa,K,Xb,X1st,Xg,Y,Ycorr,Fi,Psfc,Lat,Lon,time,Sfctype,Year,Month,Day,node,iscanPos, &
       iscanLine,RelAziAngle,SolZenAngle,angle,qc)
    !---Input/Output Variables
    INTEGER, INTENT(IN) :: iu,iprof,nEDRselec
    INTEGER, INTENT(IN) :: iattempt,Year,Month,Day,node
    INTEGER, INTENT(IN) :: iscanPos,iscanLine,Sfctype
    INTEGER, DIMENSION(nEDRselec)        :: ParamLength
    INTEGER, DIMENSION(nEDRselec)        :: ParamIndx
    INTEGER(2), DIMENSION(:), INTENT(IN) :: qc
    REAL, INTENT(IN)                     :: Psfc,Lat,Lon,time
    REAL, INTENT(IN)                     :: RelAziAngle,SolZenAngle
    REAL, DIMENSION(:), INTENT(IN)       :: angle
    REAL, DIMENSION(:), INTENT(IN)       :: Xb
    REAL, DIMENSION(:), INTENT(IN)       :: X1st
    REAL, DIMENSION(:), INTENT(IN)       :: Y
    REAL, DIMENSION(:), INTENT(IN)       :: Ycorr
    REAL, DIMENSION(:,:), INTENT(IN)     :: Xg
    REAL, DIMENSION(:,:), INTENT(IN)     :: K
    REAL, DIMENSION(:,:), INTENT(IN)     :: Fi
    REAL, DIMENSION(:,:), INTENT(IN)     :: Sa
    REAL, DIMENSION(:,:), INTENT(IN)     :: Fe
    CHARACTER(LEN=*), DIMENSION(nEDRselec) :: ParamLabel
    !---Local variables
    INTEGER  :: iLay,ichan,nchan,nch,nqc,nGselec
    INTEGER  :: iter,nIterTot,nIterTotFi,nLev,nLay
    !Variable Initialization
    nGselec   = size(Sa,1)
    nchan     = size(K,1)
    nIterTot  = size(Xg,2)
    nIterTotFi= size(Fi,2)
    nqc       = size(qc,1)
    WRITE(iu,'(a)') '-----------------------------------------------------------------------'
    WRITE(iu,'(a25,i10)') 'Profile# :',iprof
    WRITE(iu,'(a25,i10)') '# Attempts :',iattempt
    WRITE(iu,'(a25,i10)') 'Total # iterations :',nIterTot
    WRITE(iu,'(a25,i10)') 'Total # Params selected :',nGselec
    WRITE(iu,'(a25,i10)') '# of EDRs retrieved :',nEDRselec
    WRITE(iu,'(a25,10a10)') 'EDRs retrieved :',adjustr(ParamLabel(1:nEDRselec))
    WRITE(iu,'(a25,10i10)') '# Params in each EDR :',ParamLength(1:nEDRselec)
    WRITE(iu,'(a25,10i10)') 'Index of each EDR :',ParamIndx(1:nEDRselec)
    WRITE(iu,'(a)') 'Fe (Model Error Covariance) Matrix :'
    DO ichan=1, nchan
       WRITE(iu,'(30(f20.15))') Fe(ichan,1:nchan)
    ENDDO
    WRITE(iu,'(a)') 'Sa (Geophysical Error Covariance) Matrix :'
    DO iLay=1,nGselec
       WRITE(iu,'(1100(f20.15))') Sa(iLay,1:nGselec)
    ENDDO
    WRITE(iu,'(a)') 'K (Jacobian) Matrix :'
    DO ichan=1, nchan
       WRITE(iu,'(1100(f20.15))') K(ichan,1:nGselec)
    ENDDO
    WRITE(iu,'(a)') 'Xa (Geophysical Background) Vector :'
    WRITE(iu,'(1100(f20.15))') Xb(1:nGselec)
    WRITE(iu,'(a)') 'Xo (First Guess) Vector :'
    WRITE(iu,'(1100(f20.15))') X1st(1:nGselec)
    WRITE(iu,'(a)') 'Xi (Retrieval State) Vector :'
    DO iter=1, nIterTot
       WRITE(iu,'(a25,i10)') ' iteration # :',iter
       WRITE(iu,'(1100(f20.15))') Xg(1:nGselec,iter)
    ENDDO
    WRITE(iu,'(a)') 'Y (Observed Brightness Temperature) Vector (K) :'
    WRITE(iu,'(30(f20.15))') Y(1:nchan)
    WRITE(iu,'(a)') 'Ycorr (Bias Corrected Brightness Temperature) Vector (K) :'
    WRITE(iu,'(30(f20.15))') Ycorr(1:nchan)
    WRITE(iu,'(a)') 'Fo (1st Forward Brightness Temperature) Vector (K) :'
    WRITE(iu,'(30(f20.15))') Fi(1:nchan,1)
    WRITE(iu,'(a)') 'Fi (Forward Brightness Temperature) Vector (K) :'
    DO iter=2, nIterTotFi
       WRITE(iu,'(a25,i10)') ' iteration # :',iter-1
       WRITE(iu,'(30(f20.15))') Fi(1:nchan,iter)
    ENDDO
    WRITE(iu,'(a25,30(f12.4))') 'Viewing Angle :',angle(1:nchan)
    WRITE(iu,'(a25,f12.4)') 'Relative Azimuth Angle :',RelAziAngle
    WRITE(iu,'(a25,f12.4)') 'Solar Zenith Angle :',SolZenAngle
    WRITE(iu,'(a25,f12.4)') 'Surface Pressure(mb) :',Psfc
    WRITE(iu,'(a25,f12.4)') 'Latitude :',Lat
    WRITE(iu,'(a25,f12.4)') 'Longitude :',Lon
    WRITE(iu,'(a25,f12.4)') 'Seconds within the Day :',time
    WRITE(iu,'(a25,i10)') 'Surface Type :',Sfctype
    WRITE(iu,'(a25,i10)') 'Year :',Year
    WRITE(iu,'(a25,i10)') 'Month :',Month
    WRITE(iu,'(a25,i10)') 'Day :',Day
    WRITE(iu,'(a25,i10)') 'node :',node
    WRITE(iu,'(a25,i10)') 'Scan position :',iscanPos
    WRITE(iu,'(a25,i10)') 'Scan line index :',iscanLine
    WRITE(iu,'(a25,10(i10))') 'Qulity Control Flag :',qc(1:nqc)
    RETURN
  END SUBROUTINE WritePrepDiagnostic

!===============================================================
! Name:         ReadHdrPrepDiagnostic
!
!
! Type:         Subroutine
!
!
! Description: 
!               
!               
!               
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - iu                 I           Unit number
!       - PrepDiagFile       I           PrepDiagnostic data file
!       - AlgSN              O           Algorithm Serial number
!       - sensorID           O           Sensor Number ID
!       - nProfiles          O           Number of Porfiles
!       - nqc                O           Number of Quality Flags 
!       - nchan              O           Total Number of channels
!       - nch                O           Number of channels used
!       - nLev               O           Number Pressure Levels
!       - nLay               O           Number Pressure Layers
!       - nEDRs_atm          O           Total # Atmos. EDRs
!       - nEDRs_sfc          O           Total # Surface EDRs
!       - press_lev          O           Level pressure grid
!       - press_lay          O           Layer pressure grid
!       - CentrFreq          O           Center Frequency (GHz)
!       - Indxchan           O           Index of channels used
!       - ParamLabelAtm      O           Label of Atmospheric Parameters
!       - ParamLabelSfc      O           Label of Surface Parameters
!       - nGatm              O           Number Atmospheric Parameters
!       - nGsfc              O           Number Surface Parameters
!       - EDR_nEOF_atm1      O           Number of Atmospheric EOFs for 1st Attempt
!       - EDR_nEOF_atm2      O           Number of Atmospheric EOFs for 2nd Attempt
!       - EDR_nEOF_sfc1      O           Number of Surface EOFs for 1st Attempt
!       - EDR_nEOF_sfc2      O           Number of Surface EOFs for 2nd Attempt
!       - Se                 O           Measurement Error Covariance Matrix
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-24-2012    Flavio Iturbide-Sanchez I.M. Systems Group @ NOAA/NESDIS/STAR 
!
!===============================================================

  SUBROUTINE ReadHdrPrepDiagnostic(iu,PrepDiagFile,AlgSN,sensorID,nProfiles,nqc,nchan,nch, &
       nLev,nLay,nEDRs_atm,nEDRs_sfc,press_lev,press_lay,CentrFreq,Polar,Indxchan,nabsorb,AbsorbID, &
       nPosScan,nScanLines,ParamLabelAtm,ParamLabelSfc,nGatm,nGsfc,EDR_nEOF_atm1,EDR_nEOF_sfc1, &
       EDR_nEOF_atm2,EDR_nEOF_sfc2,Se)
    !---Input/Output Variables
    CHARACTER(LEN=*)        :: PrepDiagFile
    INTEGER                 :: iu,AlgSN,sensorID,nProfiles,nqc,nchan
    INTEGER                 :: nch,nLev,nLay,nEDRs_atm,nEDRs_sfc,nabsorb
    INTEGER                 :: nPosScan,nScanLines
    INTEGER, DIMENSION(:), POINTER    :: Indxchan
    INTEGER, DIMENSION(:), POINTER    :: nGatm
    INTEGER, DIMENSION(:), POINTER    :: nGsfc
    INTEGER, DIMENSION(:), POINTER    :: EDR_nEOF_atm1
    INTEGER, DIMENSION(:), POINTER    :: EDR_nEOF_atm2
    INTEGER, DIMENSION(:), POINTER    :: EDR_nEOF_sfc1
    INTEGER, DIMENSION(:), POINTER    :: EDR_nEOF_sfc2
    INTEGER, DIMENSION(:), POINTER    :: Polar
    INTEGER, DIMENSION(:), POINTER    :: AbsorbID
    REAL, DIMENSION(:), POINTER       :: press_lev
    REAL, DIMENSION(:), POINTER       :: press_lay
    REAL, DIMENSION(:), POINTER       :: CentrFreq
    REAL, DIMENSION(:,:), POINTER     :: Se
    CHARACTER(LEN=10), DIMENSION(:), POINTER :: ParamLabelAtm
    CHARACTER(LEN=10), DIMENSION(:), POINTER :: ParamLabelSfc
    !---Local Variables
    INTEGER        :: ichan
    !---Open file containing PrepDiagnostic data
    iu=get_lun()
    OPEN(iu,file=PrepDiagFile,form='formatted',status='unknown')
    !---Read Header information
    READ(iu,'(a)')
    READ(iu,'(25x,i10)') sensorID
    READ(iu,'(25x,i10)') AlgSN
    READ(iu,'(25x,i10)') nProfiles
    READ(iu,'(25x,i10)') nqc
    READ(iu,'(25x,i10)') nchan
    READ(iu,'(25x,i10)') nch
    READ(iu,'(25x,i10)') nLev
    READ(iu,'(25x,i10)') nLay
    READ(iu,'(25x,i10)') nEDRs_atm
    READ(iu,'(25x,i10)') nEDRs_sfc
    READ(iu,'(25x,i10)') nabsorb
    READ(iu,'(25x,i10)') nPosScan
    READ(iu,'(25x,i10)') nScanLines
    !--Allocate Variables
    ALLOCATE(press_lev(nLev),press_lay(nLay),CentrFreq(nchan),Polar(nchan),Indxchan(nch), &
         AbsorbID(nabsorb),ParamLabelAtm(nEDRs_atm),ParamLabelSfc(nEDRs_sfc),nGatm(nEDRs_atm), &
         nGsfc(nEDRs_sfc),EDR_nEOF_atm1(nEDRs_atm),EDR_nEOF_atm2(nEDRs_atm), &
         EDR_nEOF_sfc1(nEDRs_sfc),EDR_nEOF_sfc2(nEDRs_sfc),Se(nchan,nchan))
    READ(iu,'(25x,110(f10.3))') press_lev(1:nLev)
    READ(iu,'(25x,110(f10.3))') press_lay(1:nLay)
    READ(iu,'(25x,30(f10.3))') CentrFreq(1:nchan)
    READ(iu,'(25x,30(i4))') Polar(1:nchan)
    READ(iu,'(25x,30(i4))') Indxchan(1:nch)
    READ(iu,'(25x,30(i4))') AbsorbID(1:nabsorb)
    READ(iu,'(a)')
    READ(iu,'(25x,20(a10))') ParamLabelAtm(1:nEDRs_atm)
    READ(iu,'(25x,20(a10))') ParamLabelSfc(1:nEDRs_sfc)
    READ(iu,'(25x,20(i4))') nGatm(1:nEDRs_atm)
    READ(iu,'(25x,20(i4))') nGsfc(1:nEDRs_sfc)
    READ(iu,'(25x,20(i4))') EDR_nEOF_atm1(1:nEDRs_atm)
    READ(iu,'(25x,20(i4))') EDR_nEOF_sfc1(1:nEDRs_sfc)
    READ(iu,'(25x,20(i4))') EDR_nEOF_atm2(1:nEDRs_atm)
    READ(iu,'(25x,20(i4))') EDR_nEOF_sfc2(1:nEDRs_sfc)
    READ(iu,'(a)')
    READ(iu,'(a)')
    DO ichan=1, nchan
       READ(iu,'(30(f20.15))') Se(ichan,1:nchan)
    ENDDO
    RETURN
  END SUBROUTINE ReadHdrPrepDiagnostic


!===============================================================
! Name:        ReadPrepDiagnostic
!
!
! Type:        Subroutine
!
!
! Description:  
!
!
! Arguments:
!
!           Name                 Type       Description
!      ---------------------------------------------------
!       - iu                      I         Unit number
!       - nchan                   I         Total Number of channels
!       - nqc                     I         Number of Quality Flags 
!       - iprof                   O         Profile index
!       - iattempt                O         Number of attempts
!       - nIterTot                O         Total Number of Iterations
!       - nGselec                 O         Total Number Parameters selected
!       - nEDRselec               I         Number of EDRs selected for retrieval
!       - Sa                      O         Geophysical Error Covariance Matrix
!       - K                       O         Jacobian Matrix
!       - Xa                      O         Geophysical Background Vector
!       - Xo                      O         First Guess Vector
!       - Xi                      O         Retrieval State Vector
!       - Y                       O         Observed Brightness Temperature
!       - Ycorr                   O         Bias Corrected Brightness Temperature
!       - Fo                      O         1st Forward Brightness Temperature
!       - Fi                      O         Forward Brightness Temperature
!       - angle                   O         Viewing Angle
!       - RelAziAngle             O         Relative Azimuth Angle
!       - SolZenAngle             O         Solar Zenith Angle
!       - Psfc                    O         Surface Pressure
!       - Lat                     O         Latitude
!       - Lon                     O         Longitude
!       - time                    O         Seconds within the Day
!       - Sfctype                 O         Surface Type
!       - Year                    O         Year
!       - Month                   O         Month
!       - Day                     O         Day
!       - node                    O         node
!       - iscanPos                O         Scan position
!       - iscanLine               O         Scan line index
!       - qc                      O         Qulity Control Flag
!
! Modules needed:
!       - None
!
!
! History:
!       05-24-2012    Flavio Iturbide-Sanchez I.M. Systems Group @ NOAA/NESDIS/STAR 
!
!===============================================================

  SUBROUTINE ReadPrepDiagnostic(iu,ierr,nchan,nqc,iprof,iattempt,nIterTot,nGselec,nEDRselec,ParamLabel, &
       ParamLength,ParamIndx,Fe,Sa,K,Xa,Xo,Xi,Y,Ycorr,Fo,Fi,angle,RelAziAngle,SolZenAngle,Psfc, &
       Lat,Lon,time,Sfctype,Year,Month,Day,node,iscanPos,iscanLine,qc)
    !---Input/Output Variables
    INTEGER                 :: iu,iprof,iattempt,nIterTot,nGselec,nEDRselec,nchan,nqc
    INTEGER                 :: Sfctype,Year,Month,Day,node,iscanPos,iscanLine
    REAL                    :: RelAziAngle,SolZenAngle,Psfc,Lat,Lon,time    
    INTEGER, DIMENSION(:), POINTER    :: ParamLength
    INTEGER, DIMENSION(:), POINTER    :: ParamIndx
    INTEGER(2), DIMENSION(:), POINTER :: qc
    REAL, DIMENSION(:), POINTER       :: Xa
    REAL, DIMENSION(:), POINTER       :: Xo
    REAL, DIMENSION(:), POINTER       :: Y
    REAL, DIMENSION(:), POINTER       :: Fo
    REAL, DIMENSION(:), POINTER       :: Ycorr
    REAL, DIMENSION(:), POINTER       :: angle
    REAL, DIMENSION(:,:), POINTER     :: Sa
    REAL, DIMENSION(:,:), POINTER     :: K
    REAL, DIMENSION(:,:), POINTER     :: Xi
    REAL, DIMENSION(:,:), POINTER     :: Fi
    REAL, DIMENSION(:,:), POINTER     :: Fe
    CHARACTER(LEN=10), DIMENSION(:), POINTER :: ParamLabel
    !---Local Variables
    INTEGER        :: iLay,ichan,iter,ierr
    ierr=0
    !---Read the PrepDiagnostic Data
    READ(iu,'(a)',iostat=ierr)
    IF (ierr.ne.0) THEN
       RETURN
    ENDIF
    READ(iu,'(25x,i10)') iprof
    READ(iu,'(25x,i10)') iattempt
    READ(iu,'(25x,i10)') nIterTot
    READ(iu,'(25x,i10)') nGselec
    READ(iu,'(25x,i10)') nEDRselec
    ALLOCATE(ParamLabel(nEDRselec),ParamLength(nEDRselec),ParamIndx(nEDRselec))
    READ(iu,'(25x,10(a10))') ParamLabel(1:nEDRselec)
    READ(iu,'(25x,10i10)') ParamLength(1:nEDRselec)
    READ(iu,'(25x,10i10)') ParamIndx(1:nEDRselec)
    !--Allocate Variables
    ALLOCATE(Fe(nchan,nchan),Sa(nGselec,nGselec),K(nchan,nGselec),Xa(nGselec),Xo(nGselec), &
         Xi(nGselec,nIterTot),Y(nchan),Ycorr(nchan),Fo(nchan),Fi(nchan,nIterTot), &
         angle(nchan),qc(nqc))
    READ(iu,'(a)')
    DO ichan=1, nchan
       READ(iu,'(30(f20.15))') Fe(ichan,1:nchan)
    ENDDO
    READ(iu,'(a)')
    DO iLay=1,nGselec
       READ(iu,'(1100(f20.15))') Sa(iLay,1:nGselec)
    ENDDO
    READ(iu,'(a)')
    DO ichan=1, nchan
       READ(iu,'(1100(f20.15))') K(ichan,1:nGselec)
    ENDDO
    READ(iu,'(a)')
    READ(iu,'(1100(f20.15))') Xa(1:nGselec)
    READ(iu,'(a)')
    READ(iu,'(1100(f20.15))') Xo(1:nGselec)
    READ(iu,'(a)')
    DO iter=1, nIterTot
       READ(iu,'(a)')
       READ(iu,'(1100(f20.15))') Xi(1:nGselec,iter)
    ENDDO
    READ(iu,'(a)')
    READ(iu,'(30(f20.15))') Y(1:nchan)
    READ(iu,'(a)')
    READ(iu,'(30(f20.15))') Ycorr(1:nchan)
    READ(iu,'(a)')
    READ(iu,'(30(f20.15))') Fo(1:nchan)
    READ(iu,'(a)')
    DO iter=1, nIterTot
       READ(iu,'(a)')
       READ(iu,'(30(f20.15))') Fi(1:nchan,iter)
    ENDDO
    READ(iu,'(25x,30(f12.4))') angle(1:nchan)
    READ(iu,'(25x,f12.4)') RelAziAngle
    READ(iu,'(25x,f12.4)') SolZenAngle
    READ(iu,'(25x,f12.4)') Psfc
    READ(iu,'(25x,f12.4)') Lat
    READ(iu,'(25x,f12.4)') Lon
    READ(iu,'(25x,f12.4)') time
    READ(iu,'(25x,i10)') Sfctype
    READ(iu,'(25x,i10)') Year
    READ(iu,'(25x,i10)') Month
    READ(iu,'(25x,i10)') Day
    READ(iu,'(25x,i10)') node
    READ(iu,'(25x,i10)') iscanPos
    READ(iu,'(25x,i10)') iscanLine
    READ(iu,'(25x,10(i10))') qc(1:nqc)

    RETURN
  END SUBROUTINE ReadPrepDiagnostic


!===============================================================
! Name:         WriteHdrDiagnostic
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the header of the prepDiagnostic data file.
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - iu                 I           Unit number
!       - DiagFile           I           Diagnostic data file
!       - sensorID           I           Sensor Number ID
!       - AlgSN              I           Algorithm Serial number
!       - nProfiles          I           Number of Porfiles
!       - nqc                I           Number of Quality Flags 
!       - nchan              I           Total Number of channels
!       - nch                I           Number of channels used
!       - nEDRs_atm          I           Total # Atmos. EDRs
!       - nEDRs_sfc          I           Total # Surface EDRs
!       - nLev               I           Number Pressure Levels
!       - nLay               I           Number Pressure Layers
!       - press_lev          I           Level pressure grid
!       - press_lay          I           Layer pressure grid
!       - CentrFreq          I           Center Frequency (GHz)
!       - Indxchan           I           Index of channels used
!
! Modules needed:
!       - None
!
!
! History:
!       06-14-2012       Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR     
!
!===============================================================

  SUBROUTINE WriteHdrDiagnostic(iu,DiagFile,sensorID,AlgSN,nProfiles,nqc,nchan,nch,nEDRs_atm, &
       nEDRs_sfc,nLev,nLay,press_lev,press_lay,CentrFreq,Indxchan)
    !---Input/Output Variables
    CHARACTER(LEN=*)        :: DiagFile
    INTEGER, INTENT(IN)     :: sensorID,AlgSN,nProfiles,nqc
    INTEGER, INTENT(IN)     :: nchan,nch,nLev,nLay
    INTEGER, DIMENSION(:), INTENT(IN) :: Indxchan
    REAL, DIMENSION(:), INTENT(IN)    :: CentrFreq
    REAL, DIMENSION(:), INTENT(IN)    :: press_lev
    REAL, DIMENSION(:), INTENT(IN)    :: press_lay
    !---Local Variables
    INTEGER        :: iu,nEDRs_atm,nEDRs_sfc
    !---Open file containing iterative-process monitoring data
    iu=get_lun()
    OPEN(iu,file=DiagFile,form='formatted',status='unknown')
    !---Writing Header information
    WRITE(iu,'(a35)') '    Header Information   '
    WRITE(iu,'(a30,i10)') 'Sensor ID :',sensorID
    WRITE(iu,'(a30,i10)') 'MiRS Version :',AlgSN
    WRITE(iu,'(a30,i10)') '# nProfile :',nProfiles
    WRITE(iu,'(a30,i10)') '# Quality Control Flags :',nqc
    WRITE(iu,'(a30,i10)') 'Total # of channels (nchan):',nchan
    WRITE(iu,'(a30,i10)') '# of channels used (nch):',nch
    WRITE(iu,'(a30,i10)') 'Total # Atmos. EDRs :',nEDRs_atm
    WRITE(iu,'(a30,i10)') 'Total # Sfc. EDRs :',nEDRs_sfc
    WRITE(iu,'(a30,i10)') '# Pressure Levels :',nLev
    WRITE(iu,'(a30,i10)') '# Pressure Layers :',nLay
    WRITE(iu,'(a30,110(f10.3))') 'Pressure Levels (mb) :', press_lev(1:nLev)
    WRITE(iu,'(a30,110(f10.3))') 'Pressure Layers (mb) :',press_lay(1:nLay)
    WRITE(iu,'(a30,30(f10.3))') 'Center Freq. (GHz) :',CentrFreq(1:nchan)
    WRITE(iu,'(a30,30(i4))') 'Index of channels used :',Indxchan(1:nch)
    RETURN
  END SUBROUTINE WriteHdrDiagnostic

!===============================================================
! Name:        WriteDiagnostic
!
!
! Type:        Subroutine
!
!
! Description:  Writes out the content of the prepDiagnostic file.
!
!
! Arguments:
!
!           Name                 Type          Description
!      ---------------------------------------------------
!       - iu                      I         Unit number
!       - iprof                   I         Profile index
!       - iattempt                I         Number of attempts
!       - nIterTot                I         Total Number of Iterations
!       - nGselec                 I         Total Number Parameters selected
!       - nGselecEff              I         Effective Number Parameters selected
!       - nEDRselec               I         Number of EDRs selected for retrieval
!       - nch                     I         Number of channels used
!       - nchan                   I         Total Number of channels
!       - nqc                     I         Number of Quality Flags 
!       - ParamLabel              I         Labels of EDRs selected
!       - ParamLength             I         Length of EDRs within Xg
!       - ParamIndx               I         Indexes of EDRs within Xg
!       - ParamLengthEff          I         Effective Length of EDRs within Xg
!       - ParamIndxEff            I         Effective Indexes of EDRs within Xg
!       - DoF                     I         Retrieval Degrees of Freedom
!       - AK                      I         Averaging Kernel Matrix
!       - CF                      I         Contribution Function Matrix
!       - K                       I         Jacobian Matrix
!       - Sxco                    I         Modified Error Covariance Matrix
!       - Sxm                     I         Measurement Error Covariance Matrix
!       - Sxa                     I         Apriori Error Covariance Matrix
!       - FilterIndx              I         Effective Index of EDR Parameters
!       - Psfc                    I         Surface Pressure
!       - Lat                     I         Latitude
!       - Lon                     I         Longitude
!       - time                    I         Seconds within the Day
!       - Sfctype                 I         Surface Type
!       - Year                    I         Year
!       - Month                   I         Month
!       - Day                     I         Day
!       - node                    I         node
!       - qc                      I         Qulity Control Flag
!
!
! Modules needed:
!       - None
!
!
! History:
!       06-14-2012       Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR       
!
!===============================================================

  SUBROUTINE WriteDiagnostic(iu,iprof,iattempt,nIterTot,nGselec,nGselecEff,nEDRselec, &
       nch,nchan,nqc,ParamLabel,ParamLength,ParamIndx,ParamLengthEff,ParamIndxEff,DoF,AK,CF,K, &
       KMod,Xa,Xo,Xi,Sxco,Sxm,Sxa,FilterIndx,Psfc,Lat,Lon,time,Sfctype,Year,Month,Day,node,qc)
    !---Input/Output Variables
    INTEGER, INTENT(IN) :: iu,iprof,iattempt,nIterTot
    INTEGER, INTENT(IN) :: nch,nchan,nqc
    INTEGER, INTENT(IN) :: nGselec,nGselecEff,nEDRselec
    INTEGER, INTENT(IN) :: Year,Month,Day,node,Sfctype
    INTEGER, DIMENSION(:), INTENT(IN)         :: FilterIndx
    INTEGER, DIMENSION(nEDRselec), INTENT(IN) :: ParamLength
    INTEGER, DIMENSION(nEDRselec), INTENT(IN) :: ParamLengthEff
    INTEGER, DIMENSION(nEDRselec), INTENT(IN) :: ParamIndx
    INTEGER, DIMENSION(nEDRselec), INTENT(IN) :: ParamIndxEff
    INTEGER(2), DIMENSION(:), INTENT(IN)      :: qc
    REAL, INTENT(IN)                          :: DoF
    REAL, DIMENSION(:,:)                      :: K
    REAL, DIMENSION(:,:)                      :: KMod
    REAL, DIMENSION(:), POINTER               :: Xa
    REAL, DIMENSION(:), POINTER               :: Xo
    REAL, DIMENSION(:,:), POINTER             :: Xi
    REAL(kind=8), DIMENSION(:,:), INTENT(IN)  :: AK
    REAL(kind=8), DIMENSION(:,:), INTENT(IN)  :: CF
    REAL(kind=8), DIMENSION(:,:), INTENT(IN)  :: Sxco
    REAL(kind=8), DIMENSION(:,:), INTENT(IN)  :: Sxm
    REAL(kind=8), DIMENSION(:,:), INTENT(IN)  :: Sxa
    REAL, INTENT(IN)                          :: Psfc,Lat,Lon,time
    CHARACTER(LEN=*), DIMENSION(nEDRselec), INTENT(IN) :: ParamLabel
    !---Local variables
    INTEGER  :: iLay,ichan,iter
    WRITE(iu,'(a)') '-----------------------------------------------------------------------'
    WRITE(iu,'(a30,i10)') 'Profile # :',iprof
    WRITE(iu,'(a30,i10)') '# Attempts :',iattempt
    WRITE(iu,'(a30,i10)') 'Total # iterations :',nIterTot
    WRITE(iu,'(a30,i10)') 'Surface Type :',Sfctype
    WRITE(iu,'(a30,i10)') 'Year :',Year
    WRITE(iu,'(a30,i10)') 'Month :',Month
    WRITE(iu,'(a30,i10)') 'Day :',Day
    WRITE(iu,'(a30,i10)') 'node :',node
    WRITE(iu,'(a30,f12.4)') 'Surface Pressure(mb) :',Psfc
    WRITE(iu,'(a30,f12.4)') 'Latitude :',Lat
    WRITE(iu,'(a30,f12.4)') 'Longitude :',Lon
    WRITE(iu,'(a30,f12.4)') 'Seconds within the Day :',time
    WRITE(iu,'(a30,i10)') 'Total # Params. (np):',nGselec
    WRITE(iu,'(a30,i10)') 'Total # Effec. Params. (npeff):',nGselecEff
    WRITE(iu,'(a30,i10)') '# of EDRs retrieved :',nEDRselec
    WRITE(iu,'(a30,10a10)') 'EDRs retrieved :',adjustr(ParamLabel(1:nEDRselec))
    WRITE(iu,'(a30,10i10)') '# Params in each EDR :',ParamLength(1:nEDRselec)
    WRITE(iu,'(a30,10i10)') 'Index of each EDR :',ParamIndx(1:nEDRselec)
    WRITE(iu,'(a30,10i10)') '# Effec. Params in each EDR :',ParamLengthEff(1:nEDRselec)
    WRITE(iu,'(a30,10i10)') 'Effec. Index of each EDR :',ParamIndxEff(1:nEDRselec)
    WRITE(iu,'(a30,10(i10))') 'Qulity Control Flag :',qc(1:nqc)
    WRITE(iu,'(a30,1100i6)') 'Effective Index of Params. :', FilterIndx(1:nGselecEff)
    WRITE(iu,'(a30,f12.4)') 'Retrieval Degrees of Freedom :',DoF
    WRITE(iu,'(a)') 'AK (Averaging Kernel Matrix) (npeff x npeff):'
    DO iLay=1,nGselecEff
       WRITE(iu,'(1100(f20.12))') AK(iLay,1:nGselecEff)
    ENDDO
    WRITE(iu,'(a)') 'CF (Contribution Function Matrix) (nch x npeff):'
    DO ichan=1,nch
       WRITE(iu,'(1100(f20.15))') CF(1:nGselecEff,ichan)
    ENDDO
    WRITE(iu,'(a)') 'K (Jacobian Matrix) (nchan x np) :'
    DO ichan=1,nchan
       WRITE(iu,'(1100(f20.15))') K(ichan,1:nGselec)
    ENDDO
    WRITE(iu,'(a)') 'KMod (Modified Jacobian Matrix) (nch x npeff) :'
    DO ichan=1,nch
       WRITE(iu,'(1100(f20.15))') KMod(ichan,1:nGselecEff)
    ENDDO
    WRITE(iu,'(a)') 'Err.Cov.Matx.: Diag and Lower Triangl. Err.Corr.: Upper Triangl.(npeffxnpeff)'
    DO iLay=1,nGselecEff
       WRITE(iu,'(1100(f20.15))') Sxco(iLay,1:nGselecEff)
    ENDDO
    WRITE(iu,'(a)') 'Sxm (Measurement Error Covariance Matrix) (npeff x npeff)'
    DO iLay=1,nGselecEff
       WRITE(iu,'(1100(f20.12))') Sxm(iLay,1:nGselecEff)
    ENDDO
    WRITE(iu,'(a)') 'Sxa (Apriori Error Covariance Matrix) (npeff x npeff)'
    DO iLay=1,nGselecEff
       WRITE(iu,'(1100(f20.12))') Sxa(iLay,1:nGselecEff)
    ENDDO
    WRITE(iu,'(a)') 'Xa (Geophysical Background) Vector :'
    WRITE(iu,'(1100(f20.15))') Xa(1:nGselec)
    WRITE(iu,'(a)') 'Xo (First Guess) Vector :'
    WRITE(iu,'(1100(f20.15))') Xo(1:nGselec)
    WRITE(iu,'(a)') 'Xi (Retrieval State) Vector :'
    DO iter=1, nIterTot
       WRITE(iu,'(a25,i10)') ' iteration # :',iter
       WRITE(iu,'(1100(f20.15))') Xi(1:nGselec,iter)
    ENDDO
    RETURN
  END SUBROUTINE WriteDiagnostic

!===============================================================
! Name:        WriteJacobians
!
!
! Type:        Subroutine
!
!
! Description:  Jacobians monitoring(iteration by iteration).
!
!
! Arguments:
!
!           Name                    Type          Description
!      ---------------------------------------------------
!       - iu                         I            Unit number
!       - iprof                      I            Profile index
!       - nEDRselec                  I            # EDRs selected for retr.
!       - nGselec                    I            Total # parameters of selected EDRs
!       - nIterTot                   I            Total # of iterations
!       - nAttempTot                 I            Total # attempts made for the retr.
!       - nch                        I            Number of selected chanels for retr
!       - ParamLabel                 I            Labels of EDRs selected
!       - ParamIndx                  I            Indexes of EDRs within Xg
!       - ParamLength                I            Lengthes of EDRs within Xg
!       - Kstar                      I            Kstar matrix after shrinking
!
!
!
! Modules needed:
!       - None
!
!
! History:
!       08-22-2013      Dr. Tanvir Islam, CIRA @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE WriteJacobians(iu,iprof,nEDRselec,nGselec,nIterTot,nAttempTot, &
       nch,ParamLabel,ParamIndx,ParamLength,Kstar,AtmClass,SfcClass)
    INTEGER :: iu,iprof,nEDRselec,nGselec,nIterTot,nAttempTot,nch,iattempt,iter,nchan
    CHARACTER(LEN=*), DIMENSION(nEDRselec)          :: ParamLabel
    INTEGER,          DIMENSION(nEDRselec)          :: ParamIndx
    INTEGER,          DIMENSION(nEDRselec)          :: ParamLength
    INTEGER                                         :: AtmClass,SfcClass
    REAL,             DIMENSION(:,:)                :: Kstar
    !---Local variables
    INTEGER :: iEDR,iparam,iG,nG
    REAL,             DIMENSION(nGselec,nch)        :: Kjac

    WRITE(iu,'(a)') '-----------------------------------------------------------'
    WRITE(iu,'(a)') 'Jacobians Monitoring:'
    WRITE(iu,'(a25,i10)') 'Profile # :',iprof
    WRITE(iu,'(a25,i10)') '# of EDRs retrieved :',nEDRselec
    WRITE(iu,'(a25,i10)') 'Attempt # :',nAttempTot
    WRITE(iu,'(a25,i10)') 'Iteration # :',nIterTot-1
    WRITE(iu,'(a25,10a10)') 'EDRs retrieved :',adjustr(ParamLabel(1:nEDRselec))
    WRITE(iu,'(a25,10i10)') '# params in each EDR :',ParamLength(1:nEDRselec)
    WRITE(iu,'(a)') 
    WRITE(iu,'(a25,i10)') 'Atmosphere class :',AtmClass
    WRITE(iu,'(a25,i10)') 'Surface class :',SfcClass
 
    Kjac = transpose(Kstar(1:nch,1:nGselec))

    DO iEDR=1,nEDRselec
       iG     = ParamIndx(iEDR)
       nG     = ParamLength(iEDR)

       DO iparam=iG,iG+nG-1
          WRITE(iu,'(i2,a10,i4,40f12.6)') iEDR,adjustr(ParamLabel(iEDR)),      &
               iparam-iG+1,Kjac(iparam,1:nch)
       ENDDO
    ENDDO

    RETURN
    
  END SUBROUTINE WriteJacobians

!===============================================================
END MODULE IO_Monitor
