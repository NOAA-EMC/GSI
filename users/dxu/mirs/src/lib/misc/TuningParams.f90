!$Id: TuningParams.f90 3265 2013-03-14 20:58:34Z kgarrett $
!-----------------------------------------------------------------------------------------------
! Name:         TuningParams
! 
! Type:         F90 module
!
! Description:
!       Module that contains the necessary routines and corresponding data
!       related to the tuning paramaters used in the 1dVAR/retrieval.
!
! Modules needed:
!       - misc
!       - CntrlParams
!       - Consts
!
! Subroutines contained:
!       - LoadTunParams
!       - SetchanFlagsBasedonTbValues
!       - DeterminChanSel
!       - DetermNumbEOFs
!
! Data type included:
!       - TunParams
!       - TunParams_type
! 
! History:
!      2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE TuningParams
  USE misc
  USE Consts
  IMPLICIT NONE
  PRIVATE
  !----Publicly available subroutine(s)
  PUBLIC :: LoadTunParams,DeterminChanSel,DetermNumbEOFs
  PUBLIC :: SetchanFlagsBasedonTbValues
  !----Publicly available data/types
  PUBLIC :: TunParams,TunParams_type
  !----Global section of the module
  INTEGER, PARAMETER           :: nLEN=6,mxCh=5000,mxEDR=100
  TYPE :: TunParams_type
    !---Sensor/Retrieval-specific items
    INTEGER                               :: Sensor_ID           !Sensor ID
    REAL                                  :: rhMaxAllowed        !Maximum relative humidity allowed in retrieval
    INTEGER                               :: RetrErrCharac       !Flag to output retrieval error characteristics
    !---What EDRs to retrieve and how to compress
    INTEGER                               :: nEDRs_atm           !Number of Atmospheric EDRs      
    CHARACTER(LEN=nLEN), DIMENSION(mxEDR) :: EDR_Label_atm       !Labels of the atmospheric EDRs
    INTEGER,             DIMENSION(mxEDR) :: EDR_nEOF_atm        !#EOFs associated with Atm. EDRs
    INTEGER                               :: nEDRs_sfc           !Number of Surface EDRs  
    CHARACTER(LEN=nLEN), DIMENSION(mxEDR) :: EDR_Label_sfc       !Labels of the Surface EDRs
    INTEGER,             DIMENSION(mxEDR) :: EDR_nEOF_sfc        !#EOFs associated with Sfc EDRs
    INTEGER,             DIMENSION(mxEDR) :: EDR_cntrlRetr_atm   !Control parameters for the atmospheric EDRs
    INTEGER,             DIMENSION(mxEDR) :: EDR_cntrlRetr_sfc   !Control parameters for the surface EDRs
    INTEGER,             DIMENSION(mxEDR) :: EDR_ExtDataUse_atm  !How-to-use Extern data flags (atmosph. EDRs)
    INTEGER,             DIMENSION(mxEDR) :: EDR_ExtDataUse_sfc  !How-to-use Extern data flags (surface EDRs)
    INTEGER                               :: EDR_ExtDataUse_sTyp !How-to-use Extern data surface type
    !------Turn ON/OFF channels (based on Vector or Freq range)
    INTEGER                               :: ChanSelectFlag      !Flag on how to select channels
    INTEGER,             DIMENSION(mxCh)  :: ChanSel             !Channel selection flag vector
    INTEGER,             DIMENSION(mxCh)  :: ChanSel2Use         !List of channels to use in retr.
    INTEGER                               :: nChanSel            !Number of channels to select
    REAL                                  :: FreqMin             !Minim freq of channel to select
    REAL                                  :: FreqMax             !maxim freq of channel to select
    !---Convergence-related items
    INTEGER                               :: nIterations         !Number of iterations allowed
    REAL                                  :: ChiSqThresh         !ChiSq threshold used 4 declaring cvgce 
    REAL                                  :: ChiSqThresh4Attempt !ChiSq threshold used 4 attempting another retrvl
    REAL                                  :: alpha               !Non-linearity tuning parameter#1
    REAL                                  :: beta                !Non-linearity tuning parameter#2
    !----Minimum top pressure to which sensor is sensitive
    REAL                                  :: topSensPressT       !Top-pressure where T sensiti exists 
    REAL                                  :: topSensPressWV      !Top-pressure where WV sensit. exists 
    !----Flag vectors to control the bias coorection
    INTEGER                               :: iWhere2Apply        !Bias application flag
    INTEGER                               :: iCorrMethod         !Bias application mode
    INTEGER,             DIMENSION(mxCh)  :: applyBias_oc_byChan !Flag vector to choose which channel to apply bias to (over ocean) 
    INTEGER,             DIMENSION(mxCh)  :: applyBias_ic_byChan !Flag vector to choose which channel to apply bias to (over ice)
    INTEGER,             DIMENSION(mxCh)  :: applyBias_ld_byChan !Flag vector to choose which channel to apply bias to (over land) 
    INTEGER,             DIMENSION(mxCh)  :: applyBias_sn_byChan !Flag vector to choose which channel to apply bias to (over snow) 
    !----Flag vectors to control the scaling of the RTM/Instrument errors (E+F)
    REAL,                DIMENSION(mxCh)  :: scalFactEF_oc_byChan 
    REAL,                DIMENSION(mxCh)  :: scalFactEF_ic_byChan 
    REAL,                DIMENSION(mxCh)  :: scalFactEF_ld_byChan 
    REAL,                DIMENSION(mxCh)  :: scalFactEF_sn_byChan 
    !-----Various variables needed to interpret content of structure
    INTEGER                               :: maxCh               !Max number of channels 
    INTEGER                               :: maxEDR              !max number of EDRs used in declaration
    INTEGER                               :: LEN                 !Max Length of file names (strings)
  END TYPE TunParams_type
  TYPE(TunParams_type), DIMENSION(:), POINTER  :: TunParams

CONTAINS


!===============================================================
! Name:         SetchanFlagsBasedonTbValues
!
!
! Type:         Subroutine
!
!
! Description:  Sets the channels selection flags to 0/1 depending
!               on the TB values themselves. For instance, if TB is
!               negative, the channel won't be used, therfore the
!               channel selection flag would be set to zero.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - tb                 I              Brightness temperatures vector
!       - nchan              I              Number of channels
!       - qc                 I/O            QC vector
!       - TunParams          I/O            Structure of Tuning parameters 
!                                           (see top section for definition)
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

  SUBROUTINE SetchanFlagsBasedonTbValues(tb,nchan,TunParams,TBqc)

    TYPE(TunParams_type)     :: TunParams
    INTEGER                  :: nchan,ichan
    REAL,    DIMENSION(:)    :: tb
    INTEGER, DIMENSION(:)    :: TBqc

    DO ichan=1,nchan
       TunParams%ChanSel2use(ichan) = TunParams%ChanSel(ichan)
       IF (tb(ichan) .lt. 0.) THEN
          TunParams%ChanSel2use(ichan)=0
          TBqc(ichan) = 1
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE SetchanFlagsBasedonTbValues


!===============================================================
! Name:         DetermNumbEOFs
!
!
! Type:         Subroutine
!
!
! Description:  Determines the total number of EOFs used in the 
!               retrieval, encompassing both surface and 
!               atmospheric EDRs
!
!
! Arguments:
!
!      Name                  Type       Description
!      ---------------------------------------------------
!       - TunParams          I          Structure of Tuning parameters 
!                                       (see top section for definition)
!       - nR                 O          Total number of EOFs or 
!                                       effectively retrieved parameters
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

  SUBROUTINE DetermNumbEOFs(TunParams,nR)
    INTRINSIC             :: SUM
    TYPE(TunParams_type)  :: TunParams
    INTEGER               :: nR,nR_atm,nR_sfc
    nR_atm = SUM(TunParams%EDR_nEOF_atm(1:TunParams%nEDRs_atm))
    nR_sfc = SUM(TunParams%EDR_nEOF_sfc(1:TunParams%nEDRs_sfc))
    nR     = nR_atm + nR_sfc
    RETURN
  END SUBROUTINE DetermNumbEOFs

!===============================================================
! Name:        LoadTunParams
!
!
! Type:        Subroutine
!
!
! Description:  Loads the tuning parameters form the tuning file(s).
!               Note that there might be more than one tuning file.
!               These parameters will be loaded to a structure whose
!               definition is in the top section of module. And then
!               made available through the PUBLIC section.
!
!
! Arguments:   None
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

  SUBROUTINE LoadTunParams(nAttempts,TuningFiles)
    CHARACTER(LEN=*),    DIMENSION(:)     :: TuningFiles
    !---tuning structure related variables
    CHARACTER(LEN=nLEN), DIMENSION(mxEDR) :: EDR_Label_atm     = DEFAULT_VALUE_STR4
    INTEGER,             DIMENSION(mxEDR) :: EDR_nEOF_atm      = DEFAULT_VALUE_INT
    CHARACTER(LEN=nLEN), DIMENSION(mxEDR) :: EDR_Label_sfc     = DEFAULT_VALUE_STR4
    INTEGER,             DIMENSION(mxEDR) :: EDR_nEOF_sfc      = DEFAULT_VALUE_INT
    INTEGER, DIMENSION(mxEDR) :: EDR_cntrlRetr_atm  = DEFAULT_VALUE_INT !Control parameters for the atmospheric EDRs
    INTEGER, DIMENSION(mxEDR) :: EDR_cntrlRetr_sfc  = DEFAULT_VALUE_INT !Control parameters for the surface EDRs
    INTEGER, DIMENSION(mxEDR) :: EDR_ExtDataUse_atm = DEFAULT_VALUE_INT !How-to-use Extern data flags (atmosph. EDRs)
    INTEGER, DIMENSION(mxEDR) :: EDR_ExtDataUse_sfc = DEFAULT_VALUE_INT !How-to-use Extern data flags (surface EDRs)
    INTEGER                               :: EDR_ExtDataUse_sTyp  = DEFAULT_VALUE_INT!How-to-use Extern data surface type
    INTEGER                               :: nEDRs_atm            = DEFAULT_VALUE_INT
    INTEGER                               :: nEDRs_sfc            = DEFAULT_VALUE_INT              
    INTEGER                               :: ChanSelectFlag       = DEFAULT_VALUE_INT
    INTEGER                               :: RetrErrCharac        = DEFAULT_VALUE_INT 
    INTEGER,             DIMENSION(mxCh)  :: ChanSel              = DEFAULT_VALUE_INT
    INTEGER                               :: nChanSel             = DEFAULT_VALUE_INT
    REAL                                  :: FreqMin              = DEFAULT_VALUE_REAL
    REAL                                  :: FreqMax              = DEFAULT_VALUE_REAL
    INTEGER                               :: nIterations          = DEFAULT_VALUE_INT 
    INTEGER                               :: Sensor_ID            = DEFAULT_VALUE_INT 
    INTEGER                               :: iWhere2Apply         = DEFAULT_VALUE_INT 
    INTEGER                               :: iCorrMethod          = DEFAULT_VALUE_INT 
    REAL                                  :: ChiSqThresh          = DEFAULT_VALUE_REAL 
    REAL                                  :: ChiSqThresh4Attempt  = DEFAULT_VALUE_REAL 
    REAL                                  :: topSensPressT        = DEFAULT_VALUE_REAL 
    REAL                                  :: topSensPressWV       = DEFAULT_VALUE_REAL 
    REAL                                  :: alpha                = DEFAULT_VALUE_REAL 
    REAL                                  :: beta                 = DEFAULT_VALUE_REAL 
    REAL                                  :: rhMaxAllowed         = DEFAULT_VALUE_REAL 
    INTEGER,             DIMENSION(mxCh)  :: applyBias_oc_byChan  = DEFAULT_VALUE_INT 
    INTEGER,             DIMENSION(mxCh)  :: applyBias_ic_byChan  = DEFAULT_VALUE_INT
    INTEGER,             DIMENSION(mxCh)  :: applyBias_ld_byChan  = DEFAULT_VALUE_INT 
    INTEGER,             DIMENSION(mxCh)  :: applyBias_sn_byChan  = DEFAULT_VALUE_INT
    REAL,                DIMENSION(mxCh)  :: scalFactEF_oc_byChan = DEFAULT_VALUE_REAL
    REAL,                DIMENSION(mxCh)  :: scalFactEF_ic_byChan = DEFAULT_VALUE_REAL 
    REAL,                DIMENSION(mxCh)  :: scalFactEF_ld_byChan = DEFAULT_VALUE_REAL  
    REAL,                DIMENSION(mxCh)  :: scalFactEF_sn_byChan = DEFAULT_VALUE_REAL
    !---local variables
    INTEGER                               :: iu,nAttempts,i,ichan,ipar
    NAMELIST /TuningP/Sensor_ID,rhMaxAllowed,RetrErrCharac,EDR_Label_atm,EDR_nEOF_atm,EDR_Label_sfc,     &
         EDR_nEOF_sfc,EDR_cntrlRetr_atm,EDR_cntrlRetr_sfc,EDR_ExtDataUse_atm,EDR_ExtDataUse_sfc,         &
         EDR_ExtDataUse_sTyp,ChanSelectFlag,ChanSel,FreqMin,FreqMax,nIterations,                         &
         ChiSqThresh,ChiSqThresh4Attempt,alpha,beta,topSensPressT,topSensPressWV,                        &
         iWhere2Apply,iCorrMethod,applyBias_oc_byChan,applyBias_ic_byChan,applyBias_ld_byChan,           &
         applyBias_sn_byChan,scalFactEF_oc_byChan,scalFactEF_ic_byChan,scalFactEF_ld_byChan,             &
         scalFactEF_sn_byChan
    ALLOCATE(TunParams(nAttempts))

    AttLoop: Do i=1,nAttempts
       TunParams(i)%applyBias_oc_byChan  = DEFAULT_VALUE_INT
       TunParams(i)%applyBias_ic_byChan  = DEFAULT_VALUE_INT
       TunParams(i)%applyBias_ld_byChan  = DEFAULT_VALUE_INT
       TunParams(i)%applyBias_sn_byChan  = DEFAULT_VALUE_INT
       TunParams(i)%scalFactEF_oc_byChan = DEFAULT_VALUE_REAL
       TunParams(i)%scalFactEF_ic_byChan = DEFAULT_VALUE_REAL
       TunParams(i)%scalFactEF_ld_byChan = DEFAULT_VALUE_REAL
       TunParams(i)%scalFactEF_sn_byChan = DEFAULT_VALUE_REAL
       TunParams(i)%EDR_cntrlRetr_atm    = DEFAULT_VALUE_INT
       TunParams(i)%EDR_cntrlRetr_sfc    = DEFAULT_VALUE_INT
       TunParams(i)%EDR_ExtDataUse_atm   = DEFAULT_VALUE_INT
       TunParams(i)%EDR_ExtDataUse_sfc   = DEFAULT_VALUE_INT
       iu=get_lun()
       OPEN(iu,file=TuningFiles(i),form='formatted',access='sequential')
       READ(iu,NML=TuningP)        
       !---Determination # of channels turned ON/OFF
       nChansel =0
       ChanLoop: Do ichan=1,mxCh
          IF (ChanSel(ichan).eq.DEFAULT_VALUE_INT) CYCLE  ChanLoop
          nChansel=nChansel+1          
       ENDDO ChanLoop
       !---Determination # of EDRs turned ON/OFF
       nEDRs_atm =0
       nEDRs_sfc =0
       ParamLoop: Do ipar=1,mxEDR
          IF (EDR_nEOF_atm(ipar).eq.DEFAULT_VALUE_INT) CYCLE  ParamLoop
          nEDRs_atm=nEDRs_atm+1          
          IF (EDR_nEOF_sfc(ipar).eq.DEFAULT_VALUE_INT) CYCLE  ParamLoop
          nEDRs_sfc=nEDRs_sfc+1          
       ENDDO ParamLoop
       !---Setting the tuning structure to the parameters read
       TunParams(i)%Sensor_ID           = Sensor_ID
       TunParams(i)%rhMaxAllowed        = rhMaxAllowed
       TunParams(i)%RetrErrCharac       = RetrErrCharac
       TunParams(i)%EDR_Label_atm       = EDR_Label_atm
       TunParams(i)%EDR_nEOF_atm        = EDR_nEOF_atm
       TunParams(i)%nEDRs_atm           = nEDRs_atm
       TunParams(i)%EDR_Label_sfc       = EDR_Label_sfc
       TunParams(i)%EDR_nEOF_sfc        = EDR_nEOF_sfc
       TunParams(i)%nEDRs_sfc           = nEDRs_sfc
       TunParams(i)%EDR_cntrlRetr_atm   = EDR_cntrlRetr_atm
       TunParams(i)%EDR_cntrlRetr_sfc   = EDR_cntrlRetr_sfc
       TunParams(i)%EDR_ExtDataUse_atm  = EDR_ExtDataUse_atm
       TunParams(i)%EDR_ExtDataUse_sfc  = EDR_ExtDataUse_sfc
       TunParams(i)%EDR_ExtDataUse_sTyp = EDR_ExtDataUse_sTyp
       TunParams(i)%ChanSelectFlag      = ChanSelectFlag
       TunParams(i)%ChanSel             = ChanSel
       TunParams(i)%ChanSel2use         = ChanSel
       TunParams(i)%nChanSel            = nChanSel
       TunParams(i)%FreqMin             = FreqMin
       TunParams(i)%FreqMax             = FreqMax
       TunParams(i)%nIterations         = nIterations
       TunParams(i)%ChiSqThresh         = ChiSqThresh
       TunParams(i)%ChiSqThresh4Attempt = ChiSqThresh4Attempt
       TunParams(i)%alpha               = alpha
       TunParams(i)%beta                = beta
       TunParams(i)%maxCh               = mxCh
       TunParams(i)%maxEDR              = mxEDR
       TunParams(i)%LEN                 = nLEN
       TunParams(i)%topSensPressT       = topSensPressT
       TunParams(i)%topSensPressWV      = topSensPressWV
       TunParams(i)%iWhere2Apply        = iWhere2Apply
       TunParams(i)%iCorrMethod         = iCorrMethod 
       TunParams(i)%applyBias_oc_byChan = applyBias_oc_byChan
       TunParams(i)%applyBias_ic_byChan = applyBias_ic_byChan
       TunParams(i)%applyBias_ld_byChan = applyBias_ld_byChan
       TunParams(i)%applyBias_sn_byChan = applyBias_sn_byChan

       TunParams(i)%scalFactEF_oc_byChan = scalFactEF_oc_byChan
       TunParams(i)%scalFactEF_ic_byChan = scalFactEF_ic_byChan
       TunParams(i)%scalFactEF_ld_byChan = scalFactEF_ld_byChan
       TunParams(i)%scalFactEF_sn_byChan = scalFactEF_sn_byChan
       CLOSE(iu)
    End Do AttLoop
  END SUBROUTINE LoadTunParams


!===============================================================
! Name:         DeterminChanSel
!
!
! Type:         Subroutine
!
!
! Description:  This subroutine determines the channels selection
!               flag vector, depending on how channels are selected in 
!               Tuning file (by selection vector or by frequency range)
!               It also computes the total number of channels selected
!
!
! Arguments:
!
!           Name                    Type      Description
!      ---------------------------------------------------
!       - CentrFreq                  I        Central frequencies
!       - nchan                      I        Total number of channels
!       - ChanSel                    I/O      Channels selection vector
!       - ChanSelectFlag             I        Flag on how to select channels
!       - FreqMin                    I        Minimum freq of channel selected
!       - FreqMax                    I        Maximum freq of channel selected
!       - nch                        O        Number of channels selected
!       - ShrkRadVec                 O        Index vector of channels selected 
!                                             (within nchan)
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE DeterminChanSel(CentrFreq,nchan,ChanSel,ChanSelectFlag,&
       FreqMin,FreqMax,nch,ShrkRadVec)
    REAL,    DIMENSION(:) :: CentrFreq
    INTEGER, DIMENSION(:) :: ChanSel,ShrkRadVec
    INTEGER               :: nchan,ChanSelectFlag,ichan,nch
    REAL                  :: FreqMin,FreqMax
    IF (ChanSelectFlag.EQ.1) THEN
       ChanSel(1:nchan) = 0
       ChanLoop: Do ichan=1,nChan
          IF (CentrFreq(ichan).GE.FreqMin .AND. CentrFreq(ichan).LE.FreqMax) THEN
             ChanSel(ichan)=1
          ENDIF
       ENDDO ChanLoop
    ENDIF
    nch=0
    DO ichan=1,nchan
       IF (ChanSel(ichan).EQ.1) THEN
          nch=nch+1
          ShrkRadVec(nch) = ichan
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE DeterminChanSel


END MODULE TuningParams
