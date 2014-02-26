MODULE calib_ssmis_sub
Use Consts
!========================================================================================================
!
!  Purpose:
!    To calibrate TDR orbit data (Level 1B) in each scene point.
!
!  Reference:
!    (1) F. Weng, B. Yan, and N. Sun
!        Correction of SSMIS Radiance Anomalies,NRL/JCSDA Mini-Workshop on Preparation
!        for SSMIS Radiance Assimilation, 26-27 October, 2005, Monterey, CA.
!    (2) B. Yan, F. Weng, and Tsan Mo
!        Calibration of DMSP F-16 Special Sensor Microwave Imager and Sounder, submitted
!        to the 14th Conference on Satellite Meteorology and Oceanography,
!        29 JAN ~ 2 FEB, 2006, ATLANTA, GA
!    (3) B. Yan and F. Weng
!        Assessments of Special Sensor Microwave Imager and Sounder (SSMIS) Data for NOAA
!        Operational Applications, submitted to the 9th Specialist Meeting on Microwave
!        Radiometry and Remote Sensing Applications,
!        San Juan, Puerto Rico, from 28 February to 03 March 2006.
!
!  Subroutines:
!    SSMIS_RADIANCE_RECALIBRATION
!    ANTENNA_EMISSION_ESTIMATE
!    SOLAR_CONTAMINATION_ESTIMATE
!    Get_antenna_emiss
!    TR_FROM_TRLUT
!    Remove_CW_ANOMALY
!    Remove_TW_ANOMALY
!    Produce_newly_calibrated_data
!    WarmTarget_Reconstruction
!    get_fft_inputs
!    FFT_Analysis_CW
!    fft_filter
!    Predict_orbit_from_orbit
!    FFT_CALCULATION
!    FFT
!    TA_NOISE_REDUCTION2
!    REMOVE_TBA_BIAS
!    SC_Location_Indices
!    PSide_SolarContamination
!    MSide_SolarContamination
!    First_PC_Contamination
!    Remove_Wcontamination
!    get_LSweak_Clocations
!    get_RSweak_Clocations
!    CW_Quality_Check
!    ORBIT_INITIALIZATION
!    FIND_POINT_CLOSE_TO_TRC
!    Gauss_fitting
!    Clinear_interpolation
!    linear_interpolation
!    Point_linear_interpolation
!    get_Tclosest_point
!    get_TSclosest_point
!    get_TLclosest_point
!    get_maximum_pixel
!    get_minimum_pixel
!    realdistance
!
!  Record of revisions:
!     ver.  YYYY/MM/DD    Programmer                       Description of change
!    =====  ==========  =============  =================================================================
!           2006/01/11   Banghua Yan    Create programs
!     v00   2006/08/11   Ninghai Sun    Change program to fortran90 module
!     v01   2006/12/07   Banghua Yan    Update SSMIS Recalibration Algorithm where TR varies with channel
!           2006/12/26   Banghua Yan    beta version of the improved algorithm. Replace five refelctor
!                                       temperature curve fitting prediction with lookup table
!     v02   2007/01/07   Banghua Yan    Deal with small number of scans cases by adding Space_Filter_FLAG,
!                                       NPeak_init and MPeak_init in SOLAR_CONTAMINATION_ESTIMATE
!                                       subroutines. Uncomment TA_NOISE_REDUCTION2 and called by
!                                       Space_Filter_FLAG in order to provide smoother (5-scan average)
!                                       Cw, Cc to re-calculate Ta from Cscene.
!                                       Add Channel 4 coefficient.
!     v03   2007/01/17   Banghua Yan    Changed TR_LUT.DAT to DTR_LUT.DAT in case of unstable TARM
!     v04   2007/01/23   Ninghai Sun    Update ORBIT_INITIALIZATION arguments, Add intent(in)
!                                       SOLAR_CONTAMINATION_ESTIMATE arguments.
!                                       Use explicit-shape array instead of deferred-shape(allocate)
!                                       or assumed-size array in subroutines.
!     v05   2007/03/01   Banghua Yan    Added a quality control for TW in case TW<0
!     v06   2007/03/07   Banghua Yan    Changed all allocatable to explicit-shape variables
!                                       Removed the residual biases
!     v07   2007/04/07   Banghua Yan    Added bias correction at ch.2
!     v08   2007/06/15   Banghua Yan    Revised the warm count anomaly correction algorithm
!                                       Updated the fitting coefficients to reduce the residual errors from ch. 2 to ch. 7 
!========================================================================================================
IMPLICIT NONE

PRIVATE

! Make subroutines public
PUBLIC :: SSMIS_RADIANCE_RECALIBRATION, ORBIT_INITIALIZATION

!---INTRINSIC functions used in this module
INTRINSIC :: ABS,ACOS,COS,SIN,INT,ALOG,EXP,MAXVAL,REAL,SIZE,MINLOC,LOG,NINT,SUM,MAXLOC

CONTAINS


  SUBROUTINE SSMIS_RADIANCE_RECALIBRATION(scan_day, jul_day,nscans,                                       &
                               scan_st_init, aux_wc, aux_cc, aux_wl, aux_sfid, aux_mux,                   &
                               img_lat,img_lon,env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,       &
                               img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                       &
                               env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                              &
                               las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,         &
                               uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23,                            &
                               Previous_CWAnomaly_Correction,jul_day_init,                                &
                               nscans_init, TimeCW_init,ucw_cor_init,dcw_init, utw_cor_init,              &
                               cw_mean_init,lat_init,NPeak_init,MPeak_init,nday,nlat,DTR_YEAR,            &
                               Space_Filter_FLAG,Anomaly_Chan_filter_index,                               &
                               dtb_ndeg,dtb_nlats,dtb_nlate,dtbcoe)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To recalibrate SSMIS measurements of antenna temperatures by removing effects resulting
  !    from the emission and/or scattering through its main reflector and from anomalies related
  !    to calibration targets.
  !
  !  Inputs:
  !    scan_day   : Julian day number for each scan line
  !    jul_day    : Julian Day Number for the observed data
  !    nscans     : the number of scanning lines per orbit
  !    NSCENES_IMG: the number of observed pixel per scanning line for image channels
  !    NSCENES_ENV: the number of observed pixel per scanning line for environment channels!
  !    NSCENES_LAS: the number of observed pixel per scanning line for lower atmosphere-sensitive channels!
  !    NSCENES_UAS: the number of observed pixel per scanning line for higher atmosphere-sensitive channels;
  !    scan_st    : time series of scanning lines per orbit
  !    aux_wc     : counts of warm load
  !    aux_cc     : counts of cold calibration
  !    aux_wl     : PRT temperatures
  !    aux_sfid   : an indicator for Sub Frame ID number of MUX parameter
  !                 i.e., Subframe ID = 0, the 4 MUX values are:
  !                       mux(0) = dcdc_t            ; DC/DC Converter Temperature
  !                       mux(1) = rflct_arm_t     ; Reflector Arm Temp
  !                       mux(2) = plo_pr_t        ; PLO PRT
  !                       mux(3) = plo_bk_t        ; Backup PLO PRT
  !    aux_mux     : MUX parameters
  !    img_lat     : latitude per observation pixel for image channel
  !    img_ch8    ~ uas_ch23: antenna temperature at corresponding channels
  !    Previous_CWAnomaly_Correction : a logical variable
  !    Previous_CWAnomaly_Correction= .T. : the CW anomaly correction is being applied to a new orbit
  !                                 = .F. : the CW anomaly correction is being applied to the first orbit
  !    nscans_init   : the number of the scan lines for the previous orbit
  !    TimeTR_init   : the time series of the arm temperature for the previous orbit
  !    TimeCW_init   : the time series of the scan lines for the previous orbit
  !    Tcenter_init  : the main reflector temperature for the previous orbit
  !    ucw_cor_init  : the warm load variance for the previous orbit
  !    dcw_init      : maximum warm load anomaly for the previous orbit
  !    utw_cor_init  : PRT temperature for the previous orbit
  !    cw_mean_init  : the mean warm load for the previous orbit
  !    Space_Filter_FLAG  = .T. : Apply the space-filter to TA and obtain the noise-reduction Ta
  !                       = .F. : no noise-reduction
  !
  !  Outputs :
  !    img_ch8 ~ uas_ch23: recalibrated antenna temperature at corresponding channels
  !    Previous_CWAnomaly_Correction= .T. : the CW anomaly correction has been applied to a new orbit
  !                                 = .F. : the CW anomaly correction has been applied to the first orbit
  !    Tarm_num_init : the number of the scan lines with avaliable arm temperatures for the current orbit
  !    nscans_init   : the number of the scan lines  ..
  !    TimeCW_init   : the time series of the scan lines  ..
  !    Tcenter_init  : the main reflector temperature  ..
  !    ucw_cor_init  : the warm load variance  ..
  !    dcw_init      : maximum warm load anomaly  ..
  !    utw_cor_init  : PRT temperature  ..
  !    cw_mean_init  : the mean warm load  ..
  !
  !  Subroutines called:
  !    (1) ANTENNA_EMISSION_ESTIMATE    : remove the effect of antenna emission from antenna temperatures
  !    (2) SOLAR_CONTAMINATION_ESTIMATE : remove the effect of solar contaminations in warm load,
  !        cold calibration and PRT temperatures
  !
  !  Release Version:
  !    Beta version in IDL: 10 November 2005
  !    Beta version in F95: 30 December 2005
  !    In the beta version, several major features are noted as follows:
  !      (1) Correction about antenna emission is applied to all SSMIS channels;
  !      (2) Correction about warm load anomaly is applied to SSMIS channels 1~7
  !      (3) Correction about PRT temperature anomaly is applied to SSMIS channels 1~7
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/05    Banghua Yan      Create programs
  !     2005/06    Banghua Yan      Added algorithm in time DOmain (except for 37 GHz)
  !     2005/07    Banghua Yan      Added algorithm in frequency DOmain (except for 37 GHz)
  !     2005/08    Banghua Yan      Added algorithm for 37V and 37H
  !     2005/09    Banghua Yan      Added algorithm for antenna emission correction
  !     2006/01    Banghua Yan      added the bias correction for SSMIS TDR data for the period
  !                                 January and February
  !     2006/12    Banghua Yan      Improved the algorithm to predict reflector temperature
  !     2007/01    Banghua Yan      Fixed a bug in saving the information for previous orbit
  !     2007/01    Banghua Yan      Changed TR to DTR in case of unstable TARM
  !     2007/06    Banghua Yan      Added variables related to tb residual bias 
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE
    ! Declare parameters
    ! Array Parameters
    INTEGER(4), PARAMETER :: npeakm = 6,O2_CHANNELS4 = 7
    INTEGER(2), PARAMETER :: Naux_wl = 3, Naux_mux = 4,O2_CHANNELS = 7
    INTEGER(2), PARAMETER :: ALL_CORRECTIONS = 1, TARGET_ANOMALY_CORRECTION = 2, NO_CORRECTION = 3
    INTEGER(2), PARAMETER :: NSCENES_IMG  = 180, NSCENES_ENV  = 90 , NSCENES_LAS  = 60
    INTEGER(2), PARAMETER :: NSCENES_UAS  = 30 , NCAL_PTS     = 24,  ndaytab = 24
    REAL(4), PARAMETER    :: TIME_MINIMUM_LENGTH = 20.0, across_twoday_index = 0.1
    REAL(4), PARAMETER     :: ONEDAY_SECOND_CONVERSION = 86400.0, second_to_minute = 0.0166667
    INTEGER(2), PARAMETER :: MAX_NPOINTS = 9000
    INTEGER(2),PARAMETER   ::  ndaytab_dtb = 23, ncoe_max_dtb = 5, nphase_max_dtb = 5

    ! Declare variables
    LOGICAL    :: Previous_CWAnomaly_Correction,Space_Filter_FLAG
    INTEGER(2) :: jul_day, jul_day_init,nscans,nday,nlat,DATA_INDEX,DD,id
    INTEGER(4) :: ns
    INTEGER(2) :: anomaly_type
    INTEGER(4) :: nscans_init
    INTEGER(2), DIMENSION(nscans) :: scan_day ,aux_sfid
    INTEGER(2),DIMENSION(nscans,NCAL_PTS) ::  aux_wc, aux_cc
    INTEGER(2),DIMENSION(ndaytab) :: data_date
    REAL(4) :: time_begin, time_end
    REAL(4), DIMENSION(nscans)   :: scan_st_init,scan_st
    REAL(4), DIMENSION(nscans,NSCENES_IMG) :: img_lat,img_lon,img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18
    REAL(4),DIMENSION(nscans,NSCENES_ENV) :: env_lat, env_lon,env_ch12,env_ch13,env_ch14,env_ch15,env_ch16
    REAL(4),DIMENSION(nscans,NSCENES_LAS) :: las_lat,las_lon,las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24
    REAL(4),DIMENSION(nscans,NSCENES_UAS) :: uas_lat, uas_lon,uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23
    REAL(4), DIMENSION(nscans,Naux_wl)    ::  aux_wl
    REAL(4), DIMENSION(nscans,Naux_mux)   ::  aux_mux
    REAL(4), DIMENSION(O2_CHANNELS,MAX_NPOINTS) ::  ucw_cor_init
    REAL(4), DIMENSION(O2_CHANNELS)   ::  dcw_init
    REAL(4), DIMENSION(MAX_NPOINTS)   :: utw_cor_init,TimeCW_init,lat_init
    REAL(4), DIMENSION(O2_CHANNELS)   :: cw_mean_init
    INTEGER(4), DIMENSION(npeakm)      :: NPeak_init,MPeak_init
    REAL(4), DIMENSION(nday,nlat)     :: DTR_YEAR
    REAL(4), DIMENSION(nlat)          :: DTR
   
    !TA RESIDUAL ERROR
    REAL(8),     DIMENSION(ndaytab_dtb,O2_CHANNELS4,nphase_max_dtb,ncoe_max_dtb) :: dtbcoe
    INTEGER(2), DIMENSION(ndaytab_dtb,O2_CHANNELS4,nphase_max_dtb)              :: dtb_ndeg,dtb_nlats,dtb_nlate
    ! NRL Comparison purpose
    INTEGER(4)                        :: num_tarm
    REAL(4), DIMENSION(nscans)        :: TimeO_arm,latO_arm,TarmO,TRO
    REAL(4), DIMENSION(nscans)        :: ut
    REAL(4), DIMENSION(NCAL_PTS,nscans)    :: ucw, ucw_cor
    REAL(4), DIMENSION(O2_CHANNELS4)  ::Anomaly_Chan_filter_index

    DATA data_date/5,25,36,51,64,79,95,110,121,135,152,170,182,195,213,221,244,258,274,298,309,329,339,365/

    ! GET TR INFORMATION
    DATA_INDEX = 1
    DD = 30

    DO id = 1, nday
      IF (ABS(jul_day-data_date(id)) <= DD) THEN
        DD = ABS(jul_day-data_date(id))
        DATA_INDEX = id
        IF (DD <= 1) EXIT
      ENDIF
    ENDDO
    DTR(1:nlat) = DTR_YEAR(DATA_INDEX,1:nlat)

    ! CORRECTION FOR TIME OF SCANNING LINES WHICH ACROSS TWO DAYS
    DO ns = 1, nscans
      IF (abs(scan_day(ns) - jul_day) <= across_twoday_index) THEN
        scan_st(ns) = scan_st_init(ns)
      ELSE
        scan_st(ns) = scan_st_init(ns) + ONEDAY_SECOND_CONVERSION
      ENDIF
    ENDDO

    ! CLASSIFY ANOMALY TYPE
    ! BOTH EMISSION AND TARGET-ANOMALY CORRECTIONS
    anomaly_type = ALL_CORRECTIONS

    ! NO SSMIS RADIANCE ANOMALY CORRECTIONS
    time_begin = scan_st(1)*second_to_minute
    time_end = scan_st(nscans)*second_to_minute
    IF ( time_end - time_begin <= TIME_MINIMUM_LENGTH) anomaly_type = NO_CORRECTION


    ! REMOVE SSMIS RADIANCE ANOMALIES
    GET_option: SELECT CASE (anomaly_type)
      CASE (ALL_CORRECTIONS)
          CALL ANTENNA_EMISSION_ESTIMATE(jul_day,nscans,                                                                &
                                         NSCENES_IMG, NSCENES_ENV ,NSCENES_LAS, NSCENES_UAS,                            &
                                         scan_st, aux_sfid, aux_mux, img_lat,env_lat, las_lat,uas_lat,                  &
                                         img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                           &
                                         env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                                  &
                                         las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,             &
                                         uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23,nlat,DTR,                       & 
                                         num_tarm,TimeO_arm,latO_arm,TarmO,TRO)

          CALL SOLAR_CONTAMINATION_ESTIMATE(jul_day,nscans,MAX_NPOINTS,NSCENES_IMG,NSCENES_ENV,NSCENES_LAS,NSCENES_UAS, &
                                            scan_st, aux_wc, aux_cc, aux_wl,img_lat, img_lon,                           &
                                            env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,                        &
                                            img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                        &
                                            env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                               &
                                            las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,          &
                                            uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23,                             &
                                            Previous_CWAnomaly_Correction,jul_day_init,                                 &
                                            nscans_init, TimeCW_init,ucw_cor_init, dcw_init,lat_init,                   &
                                            utw_cor_init,cw_mean_init,NPeak_init,MPeak_init,Space_Filter_FLAG,          & 
                                            ut,ucw,ucw_cor,Anomaly_Chan_filter_index,                                   &
                                            ndaytab_dtb, ncoe_max_dtb, nphase_max_dtb,dtb_ndeg,dtb_nlats,dtb_nlate,dtbcoe)

      CASE (TARGET_ANOMALY_CORRECTION)
          CALL SOLAR_CONTAMINATION_ESTIMATE(jul_day,nscans,MAX_NPOINTS,NSCENES_IMG,NSCENES_ENV,NSCENES_LAS,NSCENES_UAS, &
                                            scan_st, aux_wc, aux_cc, aux_wl,img_lat, img_lon,                           &
                                            env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,                        &
                                            img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                        &
                                            env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                               &
                                            las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,          &
                                            uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23,                             &
                                            Previous_CWAnomaly_Correction,jul_day_init,                                 &
                                            nscans_init, TimeCW_init,ucw_cor_init, dcw_init,lat_init,                   &
                                            utw_cor_init,cw_mean_init,NPeak_init,MPeak_init,Space_Filter_FLAG,          & 
                                            ut,ucw,ucw_cor,Anomaly_Chan_filter_index,                                   &
                                            ndaytab_dtb, ncoe_max_dtb, nphase_max_dtb,dtb_ndeg,dtb_nlats,dtb_nlate,dtbcoe)

       CASE (NO_CORRECTION)

    END SELECT GET_option

  END SUBROUTINE SSMIS_RADIANCE_RECALIBRATION


  SUBROUTINE ANTENNA_EMISSION_ESTIMATE(jul_day, nscans,                                                        &
                                       NSCENES_IMG, NSCENES_ENV ,NSCENES_LAS, NSCENES_UAS,                     &
                                       scan_st, aux_sfid, aux_mux,img_lat,env_lat, las_lat,uas_lat,            &
                                       img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                    &
                                       env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                           &
                                       las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,      &
                                       uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23,nlat,DTRLUT,             & !)
                                       num_tarm,TimeO_arm,latO_arm,TarmO,TRO)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To remove effects resulting from the emission and/or scattering through SSMIS main reflector
  !
  !  Inputs  :
  !    jul_day    : Julian Day Number for the observed data
  !    nscans     : the number of scanning lines per orbit
  !    NSCENES_IMG: the number of observed pixel per scanning line for image channels
  !    NSCENES_ENV: the number of observed pixel per scanning line for environment channels!
  !    NSCENES_LAS: the number of observed pixel per scanning line for lower atmosphere-sensitive channels;
  !    NSCENES_UAS: the number of observed pixel per scanning line for higher atmosphere-sensitive channels;
  !    scan_st    : time series of scanning lines per orbit (unit: second)
  !    aux_sfid   : an indicator for Sub Frame ID number of MUX parameter
  !                 i.e., Subframe ID = 0, the 4 MUX values are:
  !                      mux(0) = dcdc_t            ; DC/DC Converter Temperature
  !                      mux(1) = rflct_arm_t     ; Reflector Arm Temp
  !                      mux(2) = plo_pr_t        ; PLO PRT
  !                      mux(3) = plo_bk_t        ; Backup PLO PRT
  !    aux_mux     : MUX parameters
  !    img_lat     : latitude per observation pixel for image channel
  !    las_ch1    ~ las_ch24: antenna temperature at corresponding channels
  !    TRLUT       : the reflector temperature at channel 4 per orbit
  !  Outputs :
  !    las_ch1    ~ las_ch24: recalibrated antenna temperature at corresponding channels after removing
  !                         antenna emissions
  !
  !  Subroutines called:
  !    (1) TR_FROM_TRLUT           : Predict the reflector temperature
  !    (2) Get_antenna_emiss       : Predict the scene temperature removing the antenna emissions/scattering
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/05    Banghua Yan      Create programs
  !     2005/05    Banghua Yan      Added an algorithm removing antenna emission
  !     2006/12    Banghua Yan      Modified the algorithm to predict TR and emissivity
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    ! Declare parameters
    REAL(4), PARAMETER    :: second_to_minute = 0.0166667
    REAL(4), PARAMETER    :: MISSING_DATA = -999.0
    INTEGER(2), PARAMETER :: TARM_ID = 0, new = 24, Naux_mux = 4, TWO = 2
    INTEGER(2), PARAMETER :: ASCENDING_NODE = 1, DESCENDING_NODE = - 1

    ! Declare variables
    INTEGER(2) :: NSCENES_IMG, NSCENES_ENV ,NSCENES_LAS, NSCENES_UAS
    INTEGER(2) :: jul_day, nscans,nlat
    INTEGER(4) :: jul_day4,orbit_status,ich,jp, jp_img , ns, jp_env ,jp_las ,jp_uas ,num_tarm,istat
    INTEGER(2), DIMENSION(nscans),INTENT(IN) :: aux_sfid
    REAL(4),DIMENSION(nscans,NSCENES_IMG) :: img_lat,img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18
    REAL(4),DIMENSION(nscans,NSCENES_ENV) :: env_lat,env_ch12,env_ch13,env_ch14,env_ch15,env_ch16
    REAL(4),DIMENSION(nscans,NSCENES_LAS) :: las_lat,las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24
    REAL(4),DIMENSION(nscans,NSCENES_UAS) :: uas_lat,uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23
    REAL(4), DIMENSION(nscans)             ::  scan_st
    REAL(4), DIMENSION(nscans,Naux_mux)    ::  aux_mux
    REAL(4), DIMENSION(new)                :: TA,tbs
    REAL(4), DIMENSION(nscans)             :: xx,yy,zz
    REAL(4), DIMENSION(:), ALLOCATABLE     :: Time_arm,lat_arm,Tarm,TR
    REAL(4)                                :: timep, lat_init
    REAL(4), DIMENSION(nlat)               :: DTRLUT

! NRL Comparison purpose
    REAL(4), DIMENSION(nscans)             :: TimeO_arm,latO_arm,TarmO,TRO

    !****************************
    ! START ANTENNA EMISSION ISSUE
    !****************************
    jul_day4 = jul_day*1

    !GET DATA OF THE ARM TEMPERATURE OF SSMIS MAIN REFLECTOR
    ! Calculate the scanning lines with available tarm
    num_tarm = 0
    TARM_SELECT: DO ns = 1,nscans
      IF (aux_sfid(ns) == TARM_ID) THEN
        num_tarm = num_tarm + 1
        xx(num_tarm) =scan_st(ns)*second_to_minute  ! in minute
        yy(num_tarm) = aux_mux(ns,2)
        zz(num_tarm) = las_lat(ns,30)   ! use the central point's latitude at las channels as the reference
      ENDIF
    ENDDO TARM_SELECT

    ALLOCATE(Time_arm(num_tarm),STAT= istat)
    ALLOCATE(Tarm(num_tarm),STAT= istat)
    ALLOCATE(lat_arm(num_tarm),STAT= istat)
    ALLOCATE(TR(num_tarm),STAT= istat)

    Time_arm(1:num_tarm) = xx(1:num_tarm)
    Tarm(1:num_tarm)     = yy(1:num_tarm)
    lat_arm(1:num_tarm)  = zz(1:num_tarm)

!NRL COMPARISON PURPOSE
    TimeO_arm(1:num_tarm) = Time_arm(1:num_tarm)
    latO_arm(1:num_tarm)  = lat_arm(1:num_tarm)
    TarmO(1:num_tarm)     = Tarm(1:num_tarm)

    ! Estimate the reflector temperature based upon the lookup table of TR
    CALL TR_FROM_TRLUT(nlat,DTRLUT,num_tarm,lat_arm,Tarm,TR)

!NRL
    TRO(1:num_tarm)       = TR(1:num_tarm)-10.0

    !DO CORRECTION OF ANTENNA EMISSION
    lat_init = img_lat(1,1)
    IF (img_lat(2,1) >= img_lat(1,1)) THEN
      orbit_status = ASCENDING_NODE
    ELSE
      orbit_status = DESCENDING_NODE
    ENDIF

    SCAN_LOOP: DO ns = 1, nscans            ! ALL SCANNING LINE
      timep = scan_st(ns)*second_to_minute   ! in minute
      IF ( (ns >= TWO) .AND. (img_lat(ns, 1) > lat_init) ) orbit_status  = ASCENDING_NODE
      IF ( (ns >= TWO) .AND. (img_lat(ns, 1) <= lat_init) ) orbit_status = DESCENDING_NODE

      PIX_LOOP: DO jp = 1, NSCENES_IMG   ! ALL PIXELS PER SCANNING LINE
        jp_img = jp
        jp_env = jp
        jp_las = jp
        jp_uas = jp
        IF (jp >= NSCENES_ENV) jp_env = NSCENES_ENV
        IF (jp >= NSCENES_LAS) jp_las = NSCENES_LAS
        IF (jp >= NSCENES_UAS)  jp_uas = NSCENES_UAS

        tbs(8)  = img_ch8(ns,jp_img)
        tbs(9)  = img_ch9(ns,jp_img)
        tbs(10) = img_ch10(ns,jp_img)
        tbs(11) = img_ch11(ns,jp_img)
        tbs(17) = img_ch17(ns,jp_img)
        tbs(18) = img_ch18(ns,jp_img)

        tbs(12) = env_ch12(ns,jp_env)
        tbs(13) = env_ch13(ns,jp_env)
        tbs(14) = env_ch14(ns,jp_env)
        tbs(15) = env_ch15(ns,jp_env)
        tbs(16) = env_ch16(ns,jp_env)

        tbs(1)  = las_ch1(ns,jp_las)
        tbs(2)  = las_ch2(ns,jp_las)
        tbs(3)  = las_ch3(ns,jp_las)
        tbs(4)  = las_ch4(ns,jp_las)
        tbs(5)  = las_ch5(ns,jp_las)
        tbs(6)  = las_ch6(ns,jp_las)
        tbs(7)  = las_ch7(ns,jp_las)
        tbs(24) = las_ch24(ns,jp_las)

        tbs(19) = uas_ch19(ns,jp_uas)
        tbs(20) = uas_ch20(ns,jp_uas)
        tbs(21) = uas_ch21(ns,jp_uas)
        tbs(22) = uas_ch22(ns,jp_uas)
        tbs(23) = uas_ch23(ns,jp_uas)

        DO ich=1,new
          IF ( ABS(tbs(ich) - MISSING_DATA) < epsilon .OR. tbs(ich) <= 30.0) CYCLE PIX_LOOP
        ENDDO

        !initialization
        TA(1:new) = tbs(1:new)

        ! Estimate the antenna emission using the reflector temperature and antenna temperatures
        CALL Get_antenna_emiss(jul_day,orbit_status,img_lat(ns,jp_img),num_tarm,Time_arm,TR,tbs,timep,TA)

        ! Update the antenna temperatures
        img_ch8(ns,jp_img)  = TA(8)
        img_ch9(ns,jp_img)  = TA(9)
        img_ch10(ns,jp_img) = TA(10)
        img_ch11(ns,jp_img) = TA(11)
        img_ch17(ns,jp_img) = TA(17)
        img_ch18(ns,jp_img) = TA(18)

        IF (jp <= NSCENES_ENV) THEN
          env_ch12(ns,jp_env) = TA(12)
          env_ch13(ns,jp_env) = TA(13)
          env_ch14(ns,jp_env) = TA(14)
          env_ch15(ns,jp_env) = TA(15)
          env_ch16(ns,jp_env) = TA(16)
        ENDIF

        IF (jp <= NSCENES_LAS) THEN
          las_ch1(ns,jp_las)  = TA(1)
          las_ch2(ns,jp_las)  = TA(2)
          las_ch3(ns,jp_las)  = TA(3)
          las_ch4(ns,jp_las)  = TA(4)
          las_ch5(ns,jp_las)  = TA(5)
          las_ch6(ns,jp_las)  = TA(6)
          las_ch7(ns,jp_las)  = TA(7)
          las_ch24(ns,jp_las) = TA(24)
        ENDIF

        IF (jp <= NSCENES_UAS) THEN
          uas_ch19(ns,jp_uas) = TA(19)
          uas_ch20(ns,jp_uas) = TA(20)
          uas_ch21(ns,jp_uas) = TA(21)
          uas_ch22(ns,jp_uas) = TA(22)
          uas_ch23(ns,jp_uas) = TA(23)
        ENDIF

      ENDDO PIX_LOOP ! all pixels per scan line

      lat_init = img_lat(ns,1)

    ENDDO SCAN_LOOP ! all scan lines

    DEALLOCATE(Time_arm,STAT= istat)
    DEALLOCATE(Tarm,STAT= istat)
    DEALLOCATE(lat_arm,STAT= istat)
    DEALLOCATE(TR,STAT= istat)

  END SUBROUTINE ANTENNA_EMISSION_ESTIMATE


  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !     To remove the effects of anomalies at warm /cold counts and PRT temperatures
  !
  !  Inputs  :
  !    jul_day    : Julian Day Number for the observed data
  !    nscans     : the number of scanning lines per orbit
  !    NSCENES_IMG: the number of observed pixel per scanning line for image channels
  !    NSCENES_ENV: the number of observed pixel per scanning line for environment channels!
  !    NSCENES_LAS: the number of observed pixel per scanning line for lower atmosphere-sensitive channels!
  !    NSCENES_UAS: the number of observed pixel per scanning line for higher atmosphere-sensitive channels;
  !    scan_st    : time series of scanning lines per orbit
  !    aux_wc     : counts of warm load
  !    aux_cc     : counts of cold calibration
  !    aux_wl     : PRT temperatures
  !    img_lat    : latitude per observation pixel for image channel
  !    las_ch1 ~ las_ch24: antenna temperature at corresponding channels
  !    Previous_CWAnomaly_Correction= .T. : the CW anomaly correction is being applied to a new orbit
  !                                 = .F. : the CW anomaly correction is being applied to the first orbit
  !    Tarm_num_init : the number of the scan lines with avaliable arm temperatures for the previous orbit
  !    nscans_init   : the number of the scan lines  for the previuous orbit
  !    TimeCW_init   : the time series of the scan lines  ..
  !    ucw_cor_init  : the warm load variance  ..
  !    dcw_init      : maximum warm load anomaly  ..
  !    utw_cor_init  : PRT temperature  ..
  !    cw_mean_init  : the mean warm load  ..
  !
  !  Outputs :
  !    las_ch1 ~ las_ch24: recalibrated antenna temperature at corresponding channels after removing
  !                        anomalies in warm/cold counts and PRT temperatures
  !    Previous_CWAnomaly_Correction= .T. : the CW anomaly correction has been applied to a new orbit
  !                                 = .F. : the CW anomaly correction has been applied to the first orbit
  !    nscans_init   : the number of the scan lines  for the current orbit
  !    TimeCW_init   : the time series of the scan lines  ..
  !    ucw_cor_init  : the warm load variance  ..
  !    dcw_init      : maximum warm load anomaly  ..
  !    utw_cor_init  : PRT temperature  ..
  !    cw_mean_init  : the mean warm load  ..
  !    NPeak_init    : Solar-contamination area index on the maximum side of CW
  !    MPeak_init    : Solar-contamination area index on the minimum side of CW
  !  Subroutines called:
  !    (1) Remove_CWCalibration_Target_Contamination: reconstruct the warm counts after removing anomaly in
  !        warm counts
  !    (2) Remove_CCCalibration_Target_Contamination: reconstruct the cold counts after removing anomaly in
  !        cold counts
  !    (3) Remove_TWCalibration_Target_Contamination: reconstruct the PRT temperatures after removing anomaly in
  !        PRT temperatures
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/05    Banghua Yan      Create programs
  !     2005/07    Banghua Yan      Added an algorithm removing anomaly in CW
  !                                 Added an algorithm removing anomaly in CC
  !     2005/08    Banghua Yan      Added an algorithm removing anomaly in TW
  !     2006/11    Banghua Yan      Added an algorithm to reduce noise by applying a space-filter
  !     2007/01    Banghua Yan      Fix an bug in saving the information for previous orbit
  !     2007/01    Ninghai Sun      Use explicit-shape array instead of deferred-shape(allocate)
  !                                 or assumed-size array in subroutines.
  !     2007/03    Banghua Yan      Use explicit-shape array instead of deferred-shape(allocate)
  !                                 or assumed-size array in Remove_CW_ANOMALY subroutine
  !     2007/04    Banghua Yan      Add lat_init 
  !-----------------------------------------------------------------------------------------------------------


SUBROUTINE SOLAR_CONTAMINATION_ESTIMATE(jul_day,nscans,MAX_NPOINTS,NSCENES_IMG,NSCENES_ENV,NSCENES_LAS,NSCENES_UAS,    &
                                          scan_st, aux_wc, aux_cc, aux_wl, img_lat, img_lon,                 &
                                          env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,               &
                                          img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,               &
                                          env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                      &
                                          las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24, &
                                          uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23,                    &
                                          Previous_CWAnomaly_Correction,jul_day_init,                        &
                                          nscans_init, TimeCW_init,ucw_cor_init, dcw_init,lat_init,          &
                                          utw_cor_init,cw_mean_init,NPeak_init,MPeak_init,Space_Filter_FLAG, &  !)
                                          ut,ucw, ucw_cor,Anomaly_Chan_filter_index,                         &
                                          ndaytab_dtb, ncoe_max_dtb, nphase_max_dtb,dtb_ndeg,dtb_nlats,dtb_nlate,dtbcoe)


   IMPLICIT NONE
   ! Declare parameters
    INTEGER(2), PARAMETER :: new = 24, Naux_wl = 3, NContamination_araes = 6,   &
                             O2_CHANNELS = 7,NCAL_PTS = 24, NSCAN_CG = 3000
    INTEGER(4), PARAMETER :: REAL_TIME_CORR = 1, HISTRICAL_CORR = 2, NO_ANOMALY_CORR = 3
    INTEGER(4), PARAMETER :: ASCENDING_NODE = 1, DESCENDING_NODE = 2
    INTEGER(4), PARAMETER :: Min_fft_sample = 15,O2_CHANNELS4 = 7
    REAL(4), PARAMETER    :: MISSING_DATA = -999.,time_interval = 1.0
    REAL(4), PARAMETER    :: TC_INIT = 2.73,second_to_minute = 0.0166667
    REAL(4), PARAMETER    :: cw_minimum = -900.0
    ! Declare input variables
    INTEGER(2), INTENT(IN) :: MAX_NPOINTS, NSCENES_IMG, NSCENES_ENV, NSCENES_LAS, NSCENES_UAS
    INTEGER(2), INTENT(IN) :: nscans, jul_day
    REAL(4), DIMENSION(nscans), INTENT(IN) :: scan_st
    INTEGER(2), DIMENSION(nscans,NCAL_PTS), INTENT(IN) :: aux_wc, aux_cc
    REAL(4), DIMENSION(nscans,Naux_wl), INTENT(IN) :: aux_wl
    REAL(4), DIMENSION(nscans,NSCENES_IMG), INTENT(INOUT) :: img_lat,img_lon,img_ch8,img_ch9,&
                                                             img_ch10,img_ch11,img_ch17,img_ch18
    REAL(4), DIMENSION(nscans,NSCENES_ENV), INTENT(INOUT) :: env_lat, env_lon,env_ch12,env_ch13,&
                                                             env_ch14,env_ch15,env_ch16
    REAL(4), DIMENSION(nscans,NSCENES_LAS), INTENT(INOUT) :: las_lat,las_lon,las_ch1,las_ch2,las_ch3, &
                                                             las_ch4,las_ch5,las_ch6,las_ch7,las_ch24
    REAL(4), DIMENSION(nscans,NSCENES_UAS), INTENT(INOUT) :: uas_lat, uas_lon,uas_ch19, uas_ch20, &
                                                             uas_ch21,uas_ch22,uas_ch23
    LOGICAL, INTENT(INOUT) :: Previous_CWAnomaly_Correction, Space_Filter_FLAG
    INTEGER(4), INTENT(INOUT) :: nscans_init
    INTEGER(2), INTENT(INOUT) :: jul_day_init
    INTEGER(4), DIMENSION(NContamination_araes) :: NPeak_init, MPeak_init
    REAL(4), DIMENSION(MAX_NPOINTS) :: utw_cor_init, TimeCW_init,lat_init
    REAL(4), DIMENSION(O2_CHANNELS,MAX_NPOINTS) :: ucw_cor_init
    REAL(4), DIMENSION(O2_CHANNELS) :: dcw_init
    REAL(4), DIMENSION(O2_CHANNELS) :: cw_mean_init

    ! Declare local variables
    INTEGER(4) :: nscans4,nfft,jul_day4,anomaly_cor_method,orbit_status,np1,np2,npm
    INTEGER(4) :: npc,ngood
    INTEGER(2) :: ich,ns,jp,jp_env,jp_las,jp_img, jp_uas,node,node1,node2
    REAL(4)    :: dcw,dtw,cc,cw,cs,tw,tc,ts,lat_old,theta_lat,theta_lon,timep
    REAL(4)    :: x1,x2,y1,y2,dlat,ymin1,ymin2
    INTEGER(4), DIMENSION(NContamination_araes) :: NPeak,MPeak
    REAL(4), DIMENSION(nscans)       :: zz,lat_ref,cw_ref,cw_o
    REAL(4), DIMENSION(nscans_init)  :: zz_init
    REAL(4), DIMENSION(new)          :: DTA
    REAL(4), DIMENSION(O2_CHANNELS4) :: Anomaly_Chan_filter_index
    LOGICAL                          :: New_Orbit_CW_Good
    !Declare allocatable variables
    REAL(4), DIMENSION(new) :: TA
    REAL(4), DIMENSION(nscans) :: ut, utw, utw_cor, cw_mean,cc_mean,yy
    REAL(4), DIMENSION(new,nscans) :: ucw, ucc, ucw_cor, ucc_cor

    !TB RESIDUAL ERROR
    INTEGER(2),INTENT(IN) :: ndaytab_dtb, ncoe_max_dtb, nphase_max_dtb
    REAL(8),    DIMENSION(ndaytab_dtb,O2_CHANNELS4,nphase_max_dtb,ncoe_max_dtb) :: dtbcoe
    INTEGER(2), DIMENSION(ndaytab_dtb,O2_CHANNELS4,nphase_max_dtb)              :: dtb_ndeg,dtb_nlats,dtb_nlate


    nscans4 = nscans
    jul_day4 = jul_day

    ! Some aux_wc(ns,ich) is not reasonable
    ! e.g., NPR.TDRN.SA.D06271.S1146.E1303.B1519697.NS : aux_wc(386,1) = -25359
    DO ich = 1, new
      yy(1:nscans) = aux_wc(1:nscans,ich)
      cw_mean(ich) = SUM(yy)/nscans
      DO ns = 1, nscans
        ucw(ich, ns) = aux_wc(ns,ich) -  cw_mean(ich)
      ENDDO
    ENDDO

    DO ich = 1, new
      yy(1:nscans) = aux_cc(1:nscans, ich)
      cc_mean(ich) = SUM(yy)/nscans
      DO ns = 1, nscans
        ucc(ich, ns) = aux_cc(ns,ich) -  cc_mean(ich)
      ENDDO
    ENDDO

    ! 02/28/07 (nscans = 2350, the case with tw <0.0 happens
    ! data: NPR.TDRN.SA.D07033.S1119.E1305.B1699091.NS
    DO ns = 1, nscans
      ut(ns) = scan_st(ns)*second_to_minute   ! in minute
      IF (aux_wl(ns,1) >= 280.0 .and. aux_wl(ns,2) >= 280.0 .and. aux_wl(ns,3) >= 280.0) then
          utw(ns) = (aux_wl(ns,1) + aux_wl(ns,2) + aux_wl(ns,3))/3.0
      else  ! invalid PRT TW
          IF (aux_wl(ns,1) >= 280.0 .and. aux_wl(ns,2) >= 280.0) then
             utw(ns) = (aux_wl(ns,1) + aux_wl(ns,2))*0.5
          else
             IF (aux_wl(ns,1) >= 280.0 .and. aux_wl(ns,3) >= 280.0) then
                utw(ns) = (aux_wl(ns,1) + aux_wl(ns,3))*0.5
             else
                 IF (aux_wl(ns,2) >= 280.0 .and. aux_wl(ns,3) >= 280.0) then
                     utw(ns) = (aux_wl(ns,2) + aux_wl(ns,3))*0.5
                 else
                    if (aux_wl(ns,1) >= 280.0) utw(ns) = aux_wl(ns,1)
                    if (aux_wl(ns,2) >= 280.0) utw(ns) = aux_wl(ns,2)
                    if (aux_wl(ns,3) >= 280.0) utw(ns) = aux_wl(ns,3)
                 endif
             endif
          endif
      endif
    ENDDO

    tc = TC_INIT
    !INITIALIZATION
    ucw_cor(1:new,1:nscans) = ucw(1:new,1:nscans)
    ucc_cor(1:new,1:nscans) = ucc(1:new,1:nscans)
    utw_cor(1:nscans) = utw(1:nscans)

    IF (abs(jul_day_init - jul_day) > 1) then
        Previous_CWAnomaly_Correction = .FALSE.
        Anomaly_Chan_filter_index(1:O2_CHANNELS4) = -999.0
        dcw_init(1:O2_CHANNELS4)      = -999.0
    ENDIF


   
    IF (.NOT. Previous_CWAnomaly_Correction) THEN
       anomaly_cor_method = REAL_TIME_CORR
       IF (nscans < 2800) anomaly_cor_method = NO_ANOMALY_CORR
    ELSE
       anomaly_cor_method = HISTRICAL_CORR
       node1 = 1
       if (las_lat(1,1) > las_lat(3,1)) node1 = -1
       node2 = 1
       if (las_lat(nscans,1) < las_lat(nscans-3,1))  node2 = -1
       if (nscans >= 3300 .OR. (nscans >= 3000 .and. node1 == node2)) anomaly_cor_method = REAL_TIME_CORR
    ENDIF

    ! ESTIMATE ANOMALY AND DO CORRECTIONS
    ! Obtain the number of available points (03/06/07)
     nfft = INT( (ut(nscans4) - ut(1) + 0.5*time_interval)/time_interval + 1 )
     GET_SC_option: SELECT CASE (anomaly_cor_method)
      CASE (REAL_TIME_CORR)
          lat_ref(1:nscans) = las_lat(1:nscans,1)

          IF (nfft > Min_fft_sample) CALL Remove_TW_ANOMALY(nscans4,nfft,ut,utw,utw_cor)
          IF (nfft > Min_fft_sample) CALL Remove_CW_ANOMALY(nscans4,nfft,ut,ucw,NPeak,MPeak,lat_ref,  &
                                          ucw_cor,Anomaly_Chan_filter_index,dcw_init)

          !ORBIT CONTINUITY
          if (Previous_CWAnomaly_Correction) then
              node1 = 1
              if ( lat_ref(1) > lat_ref(3)) node = -1
              node2 = 1
              if (lat_init(nscans_init)<lat_init(nscans_init-3)) node2 = -1
              if (node1 == node2) then
                 np1 = nscans_init - 150
                 if (np1 < 1) np1 = 1
                 dlat = 20.0
                 npc = nscans_init
                 do ns = nscans_init, np1, -1
                    node2 = 1
                    if (lat_init(ns)<lat_init((ns-1))) node2 = -1
                    if (abs(lat_init(ns)-lat_ref(1)) <=dlat .and. node1==node2) then
                       npc = ns
                    endif
                 enddo
                 if (abs(lat_ref(1)-lat_init(npc)) <= 2.0 .and. ucw_cor(2,1)>ucw_cor_init(2,npc)) then
                     x1 = 1.0
                     do ns = 1, nscans
                        if (ucw_cor(2,ns) <ucw_cor_init(2,npc)) then
                            npm = ns
                            exit
                        endif
                     enddo
                     x2 = 500.
                     if (npm <= 500) then 
                          x2 = REAL(npm) + 90.0
                     endif
                     y1 = ucw_cor_init(2,npc)
                     y2 = ucw_cor(2,INT(x2))
                     cw_ref(1:nscans) = ucw_cor(2,1:nscans)
                     CALL Point_linear_interpolation(x1,x2,y1,y2,cw_ref)
                     ucw_cor(2,1:nscans) = cw_ref(1:nscans)
                 endif
              endif 
           endif

          !QC        
          if (nscans_init >= NSCAN_CG .and. Previous_CWAnomaly_Correction) then
              zz_init(1:nscans_init) = ucw_cor_init(4,1:nscans_init)
              zz(1:nscans) = ucw_cor(4,1:nscans)
              New_Orbit_CW_Good =.TRUE.
              CALL New_Orbit_QC(nscans_init,zz_init,lat_init,nscans4,zz,lat_ref,New_Orbit_CW_Good)
              zz_init(1:nscans_init) = ucw_cor_init(7,1:nscans_init)
              zz(1:nscans) = ucw_cor(7,1:nscans)
              CALL New_Orbit_QC(nscans_init,zz_init,lat_init,nscans4,zz,lat_ref,New_Orbit_CW_Good)
           endif
          
          !Orbit-to-orbit Continuity
          if (Previous_CWAnomaly_Correction) then
              DO ich =1, O2_CHANNELS
                 if (ut(1)-TimeCW_init(nscans_init) < 2.0 .and. ut(1)-TimeCW_init(nscans_init) >0.0) &
                 dcw = ucw_cor_init(ich,nscans_init) - ucw_cor(ich,1)
                 if (TimeCW_init(nscans_init)-ut(1)  <= 15.0 .and. TimeCW_init(nscans_init)-ut(1) >=0.0) then
                     np1 = 1
                     np2 = nscans_init
                     timep = ut(1)
                     CALL get_TLclosest_point(np1,np2,timep,TimeCW_init,npm)          
                     dcw = ucw_cor_init(ich,npm) - ucw_cor(ich,1)
                 endif
                 dcw = 0.0
                 ucw_cor(ich,1:nscans) = ucw_cor(ich,1:nscans) + dcw
              ENDDO
          endif
   
          ! Save data
          cw_ref(1:nscans) = ucw(4,1:nscans)
          node = 1
          if (lat_ref(3)<lat_ref(1)) node = -1
          IF ((nscans >= NSCAN_CG)  .AND.                                                                            &
              (((.NOT.Previous_CWAnomaly_Correction) .AND. ucw(4,1) .LE. 0.8*MAXVAL(cw_ref) .and. node==1) .OR.      &
                 (Previous_CWAnomaly_Correction  .AND. ucw_cor(4,1) .LE. 0.0 .AND. New_Orbit_CW_Good   )  )  ) THEN
            Previous_CWAnomaly_Correction = .TRUE.
            jul_day_init = jul_day
            nscans_init = nscans
            ucw_cor_init(1:O2_CHANNELS,1:nscans) = ucw_cor(1:O2_CHANNELS,1:nscans)
            utw_cor_init(1:nscans)               = utw_cor(1:nscans)
            TimeCW_init(1:nscans)                = ut(1:nscans)
            lat_init(1:nscans)                   = las_lat(1:nscans, 1)
            cw_mean_init(1:O2_CHANNELS)          = cw_mean(1:O2_CHANNELS)
            NPeak_init(1:NContamination_araes)   = NPeak(1:NContamination_araes)
            MPeak_init(1:NContamination_araes)   = MPeak(1:NContamination_araes)
          ENDIF

      CASE (HISTRICAL_CORR)

          lat_ref(1:nscans) = las_lat(1:nscans,1)
          DO ich =1, O2_CHANNELS
             zz_init(1:nscans_init) = ucw_cor_init(ich,1:nscans_init)
             cw_o(1:nscans) = ucw(ich,1:nscans)
             CALL Predict_orbit_from_orbit(nscans_init,TimeCW_init,MAX_NPOINTS,zz_init,lat_init,  &
                                           nscans4,ut,zz,lat_ref,cw_o)


             !Continuity check
             ymin1 = 200.0
             ymin2 = 200.0
             do ns = 1,nscans-30  ! IN CASE OF NON-REASONABLE DATA
                if (cw_o(ns) > cw_minimum .and. abs(cw_o(ns)-cw_o(ns+30)) <= 100.0  .and.         &
                    cw_o(ns) <= ymin1) ymin1 = cw_o(ns)
                if (zz(ns)   > cw_minimum .and. zz(ns)<= ymin2) ymin2 = zz(ns)
             enddo
    
             if (abs(ymin1- ymin2) > 10.0) dcw = 0.5*(ymin1- ymin2)  
             do ns = 1, nscans
                    if (zz(ns) >=-900.0) then
                        zz(ns) = zz(ns) + dcw
                    else
                        zz(ns) =  cw_o(ns)
                    endif
              enddo

              !QC
              ngood = 0
              do ns = 1, nscans
                 if (abs(zz(ns)-cw_o(ns)) <= 30.0) ngood = ngood + 1
              enddo
              ! Save updated data
              if (ngood >= INT(nscans*0.05)) ucw_cor(ich,1:nscans) = zz(1:nscans)

          ENDDO
       
      CASE (NO_ANOMALY_CORR)

          PRINT*,'NO CORRECTION FOR SOLAR CONTAMINATION ON CW!'
    
      END SELECT GET_SC_option

    ! ORBIT NODE
    lat_old = img_lat(1,1)
    IF (img_lat(2,1) >= img_lat(1,1)) THEN
      orbit_status = ASCENDING_NODE
    ELSE
      orbit_status = DESCENDING_NODE
    ENDIF

    SCAN_COR: DO ns = 1, nscans
      dtw = utw(ns) - utw_cor(ns)
      theta_lat = 0.5*(img_lat(ns,1)+img_lat(ns,NSCENES_IMG))
      IF ( (ns >= 2) .AND. (img_lat(ns, 1) > lat_old) ) orbit_status  = ASCENDING_NODE
      IF ( (ns >= 2) .AND. (img_lat(ns, 1) <= lat_old) ) orbit_status = DESCENDING_NODE
      PIXEL_COR: DO jp = 1, NSCENES_IMG
        theta_lon = img_lon(ns,jp)

        jp_img = jp
        jp_env = jp
        jp_las = jp
        jp_uas = jp

        IF (jp >= NSCENES_ENV)  jp_env = NSCENES_ENV
        IF (jp >= NSCENES_LAS)  jp_las = NSCENES_LAS
        IF (jp >= NSCENES_UAS)  jp_uas = NSCENES_UAS

        CALL REMOVE_TBA_BIAS(ndaytab_dtb, ncoe_max_dtb, nphase_max_dtb,dtb_ndeg,dtb_nlats,dtb_nlate,dtbcoe,  &
                             jul_day4,orbit_status,las_lat(ns, jp_las),DTA)

        TA(8) = img_ch8(ns,jp_img)
        TA(9) = img_ch9(ns,jp_img)
        TA(10) = img_ch10(ns,jp_img)
        TA(11) = img_ch11(ns,jp_img)
        TA(17) = img_ch17(ns,jp_img)
        TA(18) = img_ch18(ns,jp_img)

        TA(12) = env_ch12(ns,jp_env)
        TA(13) = env_ch13(ns,jp_env)
        TA(14) = env_ch14(ns,jp_env)
        TA(15) = env_ch15(ns,jp_env)
        TA(16) = env_ch16(ns,jp_env)

        TA(1) = las_ch1(ns,jp_las)
        TA(2) = las_ch2(ns,jp_las)
        TA(3) = las_ch3(ns,jp_las)
        TA(4) = las_ch4(ns,jp_las)
        TA(5) = las_ch5(ns,jp_las)
        TA(6) = las_ch6(ns,jp_las)
        TA(7) = las_ch7(ns,jp_las)
        TA(24) = las_ch24(ns,jp_las)

        TA(19) = uas_ch19(ns,jp_uas)
        TA(20) = uas_ch20(ns,jp_uas)
        TA(21) = uas_ch21(ns,jp_uas)
        TA(22) = uas_ch22(ns,jp_uas)
        TA(23) = uas_ch23(ns,jp_uas)

        DO ich=1,new
          IF ( ABS(TA(ich) - MISSING_DATA) .LT. epsilon .OR. TA(ich) <= 30.0)  CYCLE PIXEL_COR
        ENDDO

        TA_RECALIBRATION: DO ich = 1, O2_CHANNELS
        ! CALCULATE OBSERVED SCENE COUNTS
          cw = ucw(ich,ns) + cw_mean(ich)
          cc = ucc(ich,ns) + cc_mean(ich)
          tw = utw(ns)
          cs = (TA(ich) - tc)*(cw - cc)/(tw - tc) + cc
        ! Use calibrated cw,cc,tw and recalibrate TA
          cw = ucw_cor(ich,ns) + cw_mean(ich)
          cc = ucc_cor(ich,ns) + cc_mean(ich)
          tw = utw_cor(ns)
          ts =  tc + (tw-tc)*(cs-cc)/(cw-cc)
          if(ts >= TA(ich)) TA(ich) = ts   !ONLY RECALIBRATE TA WITH DCW > 0.0
        ENDDO TA_RECALIBRATION   ! all channels per pixel per scan line

        ! Remove residual bias
        DO ich =1, new
          TA(ich) = TA(ich) - DTA(ich)
        ENDDO

        !update antenna temperature
        img_ch8(ns,jp_img)  = TA(8)
        img_ch9(ns,jp_img)  = TA(9)
        img_ch10(ns,jp_img) = TA(10)
        img_ch11(ns,jp_img) = TA(11)
        img_ch17(ns,jp_img) = TA(17)
        img_ch18(ns,jp_img) = TA(18)

        IF (jp <= NSCENES_ENV) THEN
          env_ch12(ns,jp_env)  = TA(12)
          env_ch13(ns,jp_env)  = TA(13)
          env_ch14(ns,jp_env)  = TA(14)
          env_ch15(ns,jp_env)  = TA(15)
          env_ch16(ns,jp_env)  = TA(16)
        ENDIF

        IF (jp <= NSCENES_LAS) THEN
          las_ch1(ns,jp_las)   = TA(1)
          las_ch2(ns,jp_las)   = TA(2)
          las_ch3(ns,jp_las)   = TA(3)
          las_ch4(ns,jp_las)   = TA(4)
          las_ch5(ns,jp_las)   = TA(5)
          las_ch6(ns,jp_las)   = TA(6)
          las_ch7(ns,jp_las)   = TA(7)
        ENDIF

        IF (jp <= NSCENES_UAS) THEN
          uas_ch19(ns,jp_uas)   = TA(19)
          uas_ch20(ns,jp_uas)   = TA(20)
          uas_ch21(ns,jp_uas)   = TA(21)
          uas_ch22(ns,jp_uas)   = TA(22)
          uas_ch23(ns,jp_uas)   = TA(23)
        ENDIF

      ENDDO  PIXEL_COR     ! all pixels per scan line

      lat_old = img_lat(ns,1)

    ENDDO SCAN_COR           ! all scan lines per orbit

    !******************************************
    ! Data Averaging by using a space filter
    !******************************************
    ! For User's option if the space filter is applied to the measurements

    IF (Space_Filter_FLAG)  &

    CALL TA_NOISE_REDUCTION2(nscans,NSCENES_IMG, NSCENES_ENV ,NSCENES_LAS, NSCENES_UAS,               &
                             img_lat, img_lon,  env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,  &
                             img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                     &
                             env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                            &
                             las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,       &
                             uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23)

 END SUBROUTINE SOLAR_CONTAMINATION_ESTIMATE



! SUBROUTINES RELATED TO ANTENNA EMISSION
 SUBROUTINE Get_antenna_emiss(jul_day,orbit_status,img_lat,num_trefl,Time_refl,Trefl_Init,Tapp,time,TA)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To estimate effects resulting from the emission and/or scattering through SSMIS main reflector
  !
  !  Inputs  :
  !    orbit_status: 1: ascending node    -1: descending node
  !    jul_day    : Julian Day Number for an observed date
  !    num_trefl  : the number of scanning lines with an avaliable arm temperature
  !    time       : time of a scanning line (unit: minute)
  !    Time_refl  : the time series of scanning lines with an avaliable arm temperature (unit: minute)
  !    Trefl      : the arm/reflector temperature of scanning lines  (K)
  !    Tapp       : the apparent temperature at a corresponding channel, i.e., an antenna temperature
  !                 contaminated by the antenna emission (100K)
  !
  !  Outputs :
  !    TA         :  antenna temperature  (K)
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/07    Banghua Yan      Create programs
  !     2006/12    Banghua Yan      Set emR as the model simulation, TR5 = TR4 - 15.0, TR6,7 = TR4 - 10.0
  !     2007/02    Banghua Yan      No correction on seven SSMI-like channels
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: new = 24
    INTEGER(4)  :: num_trefl,orbit_status,jp, ich, nt, n1
    INTEGER(2)  :: jul_day
    REAL(4)      :: img_lat,Time_refl(*),Trefl_Init(*),time,TA(*),Tapp(*),t1, t2
    REAL(4), DIMENSION(num_trefl) :: Trefl
    REAL(4), DIMENSION(new) :: emiss_init,emiss

    ! emissivity data set
    data emiss_init/0.0183, 0.0187, 0.0189, 0.0190, 0.0192, 0.0195, 0.0199,0.0314, 0.0350,    &
                    0.0350, 0.0350, 0.0120, 0.0120, 0.0127, 0.0160, 0.0160, 0.0250, 0.0250,   &
                    0.0204, 0.0201, 0.0201, 0.0201, 0.0201, 0.0201/

    ! initialization of emissivity
    emiss(1:new) = emiss_init(1:new)

    !Compute antenna temperatures
    IF (time <= Time_refl(1)) THEN
      DO ich =1, new
        Trefl(1) =  Trefl_Init(1)

        IF (ich == 5) Trefl(1) =  Trefl_Init(1) - 15.0
        IF (ich == 6) Trefl(1) =  Trefl_Init(1) - 10.0
        IF (ich == 7) Trefl(1) =  Trefl_Init(1) - 10.0

        TA(ich) = (Tapp(ich) -emiss(ich)*Trefl(1))/(1.0-emiss(ich))

      ENDDO
    ELSE
      IF (time >= Time_refl(num_trefl-1)) THEN
        DO ich =1, new
          Trefl(num_trefl-1) =  Trefl_Init(num_trefl-1)

          IF (ich == 5) Trefl(num_trefl-1) =  Trefl_Init(num_trefl-1) - 15.0
          IF (ich == 6) Trefl(num_trefl-1) =  Trefl_Init(num_trefl-1) - 10.0
          IF (ich == 7) Trefl(num_trefl-1) =  Trefl_Init(num_trefl-1) - 10.0

          TA(ich) = (Tapp(ich) -emiss(ich)*Trefl(num_trefl-1))/(1.0-emiss(ich))
        ENDDO
      ELSE
        nt = num_trefl
        DO jp = 1, num_trefl
          IF (Time_refl(jp) >= time) THEN
            nt = jp
            EXIT
          ENDIF
        ENDDO

        n1 = nt -1
        IF (n1 < 1) n1 = 1

        DO ich =1, new
          Trefl(n1:nt) =  Trefl_Init(n1:nt)
          IF (ich == 5) Trefl(n1:nt) =  Trefl_Init(n1:nt) - 15.0
          IF (ich == 6) Trefl(n1:nt) =  Trefl_Init(n1:nt) - 10.0
          IF (ich == 7) Trefl(n1:nt) =  Trefl_Init(n1:nt) - 10.0
          t1 = (Tapp(ich) -emiss(ich)*Trefl(n1))/(1.0-emiss(ich))
          t2 = (Tapp(ich) -emiss(ich)*Trefl(nt))/(1.0-emiss(ich))

          IF (nt >= 2) THEN
            TA(ich) = t1 + (t2-t1)*(time-Time_refl(n1))/(Time_refl(nt)-Time_refl(n1))
          ELSE
            TA(ich) = t2
          ENDIF

        ENDDO
      ENDIF
    ENDIF

! NO CORRECTION ON SEVEN SSMI-LIKE CHANNELS

    TA(12:18) =  Tapp(12:18)

  END SUBROUTINE Get_antenna_emiss


 SUBROUTINE TR_FROM_TRLUT(nlat,TRLUT,ncount,lat,Tarm,TR)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To Predict the reflector temperatures at a given latitude from orbital-based TRLUT
  !
  !  Inputs  :
  !    nlat       : the number of latitude from 90N to 90S (nlat = 360)
  !    ncount     : the number of scanning lines per orbit
  !    lat        : latitude (degree)
  !    TRLUT      : the reflector temperature (vs. latitude) per orbit
  !  Outputs  :
  !    TR         : the estimated reflector temperature (K)
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2006/12    Banghua Yan      Create programs
  !     2007/01                     Change TR to DTR
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(2), PARAMETER    :: ASNODE = 1, DSNODE = -1
    INTEGER(2)               :: nlat,xlat,ip,lp,ip1,ip2
    INTEGER(4),INTENT(IN)    :: ncount
    REAL(4),DIMENSION(ncount) :: lat,TR,Tarm,tmp
    REAL(4),DIMENSION(nlat)   :: TRLUT
    REAL(4)                   :: km,bm,x1,x2,y1,y2
    INTEGER(2), DIMENSION(ncount) :: node,mp1,mp2

    !Initialization
     tmp(1:ncount) = 0.0

    ! GET node index for each pixel
    node(1) = DSNODE
    IF (lat(1) < lat(2)) node(1) = ASNODE
    DO ip = 2, ncount
      node(ip) = DSNODE
      IF (lat(ip) >= lat(ip-1)) node(ip) = ASNODE
    ENDDO

    ! PUT TO THE GRIDED LOCATION
    DO ip = 1, ncount
      xlat = 180 + (90-int(lat(ip)))
      IF (xlat <1)    xlat = 1
      IF (xlat > nlat) xlat =  nlat
      IF (node(ip) == ASNODE) xlat = int(lat(ip)) + 90
      TR(ip) = TRLUT(INT(xlat)) + Tarm(ip)
    ENDDO

    ! QC FOR DISOTINEOUS POINTS
    ! (1) NEAR MAXMIMUM REGION
    mp1 = MAXLOC(TR)
    ip1 = mp1(1)
    ip2 = mp1(1) + 5
    if (ip2 >ncount) ip2 = ncount
    tmp(ip1:ip2) = TR(ip1:ip2)
    mp2 = MINLOC(tmp,tmp > 100.0)
    ip2 = mp2(1)+10
    if (ip2 >ncount) ip2 = ncount

    !A LINEAR FITTING IS APPLIED TO A RANGE
    if (ip2-ip1 <= 2) ip2 = ip2 + 8
    if (ip2 > ncount) ip2 = ncount
    IF (ip2 - ip1 >= 1 ) THEN
       IF (TR(ip1)-TR(ip1+1) >= 3.0) THEN
        x1 = ip1*1.0
        x2 = ip2*1.0
        y1 = TR(ip1)
        y2 = TR(ip2)
        km = (y1-y2)/(x1-x2)
        bm = y1 - km*x1
        DO ip = ip1, ip2
           TR(ip) = km*ip + bm
        ENDDO
       ENDIF
    ENDIF

    ! (2) OTHER REGIONS
    DO ip = 2, ncount
       IF(ABS(TR(ip)-TR(ip-1))>=4.0) THEN
          ip1 = ip - 10
          if (ip1 < 1) ip1 = 1
          ip2 = ip + 10
          if (ip2 > ncount) ip2 = ncount
          IF (ip2 - ip1 >= 1.0) THEN
             x1 = ip1*1.0
             x2 = ip2*1.0
             y1 = TR(ip1)
             y2 = TR(ip2)
             km = (y1-y2)/(x1-x2)
             bm = y1 - km*x1
             DO lp = ip1, ip2
                TR(lp) = km*lp + bm
             ENDDO
          ENDIF
       ENDIF
    ENDDO

  END SUBROUTINE TR_FROM_TRLUT

! Subroutines related to calibration targets
  SUBROUTINE Remove_CW_ANOMALY(ncount,nfft,ut,ucw,ONPeak,OMPeak,las_lat,ucw_cor,Anomaly_Chan_filter_index,dcw_init)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose :
  !    To reconstruct the warm counts at SSMIS channels between 1 and 18 by removing anomalies in the warm
  !    counts due to solar contamination of radiances
  !
  !  Inputs  :
  !    ncount  : the number of scanning lines per orbit
  !    ut      : observation time for each scanning line per orbit (unit: minute)
  !    ucw     : warm counts for each channel per orbit
  !    dcw_init: maximum warm count anomaly in the previous orbit
  !
  !  Outputs    :
  !    ut      : observation time for each scanning line (unit: minute)
  !    ucw_cor : reconstructed warm counts for each channel
  !    dcw_init: maximum warm count anomaly in the current orbit if it is updated
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/05    Banghua Yan      Create programs
  !     2005/06    Banghua Yan      Added algorithm in time DOmain(except for 37 GHz)
  !     2005/07    Banghua Yan      Added algorithm in frequency DOmain(except for 37 GHz)
  !     2005/09    Banghua Yan      Added algorithm for 37V and 37H
  !     2007/04    Banghua Yan      Added las_lat information(Check continuity of data for the same latitude, etc)
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: Min_fft_sample = 15,SSMIS_Chan = 24,SSMIS_Anomaly_Chan = 7,   &
                             max_contamination_araes = 6
    INTEGER(2),PARAMETER  :: O2_Chan = 7 
    REAL(4), PARAMETER     :: time_interval = 1.0
    INTEGER(4), INTENT(IN) :: nfft
    INTEGER(2)             :: ip
    INTEGER(4)  :: ncount,ich,itype, mp1, mp2, npmax,npc,npc5,CW_Type_Index
    INTEGER(4)  :: nfilter,npc1,npc2,ncount2,nfft2
    INTEGER(4), DIMENSION(max_contamination_araes) :: ONPeak, OMPeak, NPeak, MPeak
    REAL(4)     :: ut(ncount), ucw(SSMIS_Chan,ncount), timep,ymean,yc
    REAL(4)     :: las_lat(ncount)
    REAL(4)     :: ucw_cor(SSMIS_Chan,ncount), ucws(ncount), you(ncount)
    REAL(4), dimension(3*ncount) :: ucws2,ut2,las_lat2
    REAL(4), DIMENSION(3*nfft)   :: xtt2,ytt2,yfft_multiharmonic2
    REAL(4), DIMENSION(nfft)     :: ytt_s,yfft_multiharmonic,xtt, ytt
    REAL(4), DIMENSION(SSMIS_Anomaly_Chan) :: Anomaly_Chan_filter_index
    REAL(4), DIMENSION(O2_Chan),INTENT(INOUT) :: dcw_init
    REAL(4)     :: x1,ymin,filter_index
    LOGICAL    :: EXTENSION_STATUS

    !(1) quality control for a couple of pixels with a big jump
    CALL CW_Quality_Check(ncount,ucw)


    !Initializations
    ucw_cor(1:SSMIS_Chan,1:ncount) = ucw(1:SSMIS_Chan,1:ncount)
    ONPeak(1:max_contamination_araes) = -999
    OMPeak(1: max_contamination_araes)= -999
    ! Obtain the number of available points
    IF (nfft <= Min_fft_sample) RETURN  ! all corrections end!

    !*********************************************************************
    ! (2) IdentIFy and remove effect of solar contamination in warm counts
    !*********************************************************************
    !Get indicator for contaminated areas

    CALL SC_Location_Indices(ncount,ut,ucw,nfft,xtt,ytt,NPeak,MPeak)
    !ONPeak and OMPeak
    DO itype = 1, max_contamination_araes
      mp1 = 1
      mp2 = ncount
      timep = xtt(NPeak(itype))
      CALL get_Tclosest_point(mp1,mp2,timep,ut,npc)
      ONPeak(itype) = npc

      mp1 = 1
      mp2 = ncount
      timep = xtt(MPeak(itype))
      CALL get_Tclosest_point(mp1,mp2,timep,ut,npc)
      OMPeak(itype) = npc
    ENDDO
    CW_Type_Index = 1  ! default
    LAS_CHANNELS: DO ich=1, SSMIS_Anomaly_Chan
      !Define the reconstructed data
      !Initialization
      yfft_multiharmonic2(1:3*nfft)     = 0.0
      ucws(1:ncount) = ucw(ich,1:ncount)
      !QC (non-reasonable resuls: an example: 09/01/2006)
      do ip = 2, ncount-30
         if (ucws(ip)-ucws(ip+30) < -50.0) then
             print*,'****',ich,ip,ucws(ip)
             ucws(ip) = ucws(ip-1)
         endif
      enddo    


      !Obtain the data for fft computation using original data
      CALL get_fft_inputs(ncount,ut,ucws,nfft,xtt,ytt)

      !Get the indicator for contaminated araes over positive period in warm load function
      CALL PSide_SolarContamination(nfft,ytt,NPeak)
! revision ends

      !Initialization of recalibrated data set
      ytt_s(1:nfft) = ytt(1:nfft)

      ! FFT analysis of original data
      filter_index = 3.0
      CALL FFT_Analysis_CW(nfft,ytt,filter_index,yfft_multiharmonic)
      ! GET MEAN MAXIMUM      
      CALL get_maximum_pixel(1,ncount,ucws,npmax)
      mp1 = npmax - 10 
      mp2 = npmax + 10
      if (mp1 <1) mp1 = 1
      if (mp2 >ncount) mp2 = ncount
      ymean = 0.0
      x1 = 0.0
      do ip = mp1, mp2
         ymean = ymean + ucws(ip)
         x1 = x1 + 1.0
      enddo
      if (x1 >= 1.) then
         ymean = ymean/x1
      else
         ymean = maxval(ucws)
      endif
      x1 = 0.0 
 

      !Find point of tmax+/-5.0 where ucws(tmax) = maxval(ucws)
      timep = ut(npmax) + 3.0
      if (timep >ut(ncount)) timep = ut(npmax) - 3.0
      CALL get_TLclosest_point(1,ncount,timep,ut,npc5)
      yc = ymean - 40.0
      if (yc < ucws(npc5)) yc = ucws(npc5)
      if (dcw_init(ich) > 0.0 .and. yc < maxval(ucws)-dcw_init(ich)) yc = maxval(ucws)-dcw_init(ich)

      !Build CW data from one orbit to more orbits (05/08/07)
      CALL Build_ucws2_from_ucws(ncount,ut,las_lat,ucws,ncount2,ut2,las_lat2,ucws2,npc1,npc2,EXTENSION_STATUS)

      IF (.NOT. EXTENSION_STATUS) THEN
          PRINT*,'TO EXTEND THE DATA FAILS!!!'
          do nfilter = 1, 7
             filter_index = 1.0+nfilter*0.5
             CALL FFT_Analysis_CW(nfft,ytt,filter_index,yfft_multiharmonic)
             if (maxval(yfft_multiharmonic)+10.0 >= yc) exit
          enddo
          ytt_s(1:nfft) = yfft_multiharmonic(1:nfft)
          CALL Produce_newly_calibrated_data(ncount,ut,nfft,xtt,ytt_s,you)
          ucw_cor(ich,1:ncount) = you(1:ncount)
          CYCLE LAS_CHANNELS
      ENDIF

      ! Time shift if ut2(1) <0.0
      if (ut2(1)<0.0) ut2(1:ncount2) = ut2(1:ncount2) - ut2(1)
      nfft2 = INT((ut2(ncount2) - ut2(1) + 0.5*time_interval)/time_interval + 1)
      CALL get_fft_inputs(ncount2,ut2,ucws2,nfft2,xtt2,ytt2)
      ! FInd positions of npc1 and npc2 in the times series of xtt2
      mp1 = 1
      mp2 = nfft2
      timep = ut2(npc1)
      CALL get_TSclosest_point(mp1,mp2,timep,xtt2,npc1)
      timep = ut2(npc2)
      CALL get_TSclosest_point(mp1,mp2,timep,xtt2,npc2)
      ! FFT analysis of extended ytt2

      do nfilter = 1, 7
         filter_index = 1.0+nfilter*0.5
         CALL FFT_Analysis_CW(nfft2,ytt2,filter_index,yfft_multiharmonic2)
         if (maxval(yfft_multiharmonic2) >= yc) then
             Anomaly_Chan_filter_index(ich) = filter_index
             exit
         endif
      enddo
      if (dcw_init(ich) < 0.0) dcw_init(ich) = maxval(ucws)- maxval(yfft_multiharmonic2)

      !Get required ytt, yfft_oneharmonic,.., by removing their margin effect
      mp1 = 1
      ymin = abs(ytt(1))
      do ip = 2, INT(nfft/2.0)
         if (abs(ytt(ip)) < ymin) then
             mp1 = ip
             ymin = abs(ytt(ip))
         endif
      enddo
      if (mp1 <= 20) mp1 = 20
      mp2 = nfft-20
      ymin = abs(ytt(INT(nfft/2.0)))
      do ip= INT(nfft/2.0), nfft
         if (abs(ytt(ip)) <ymin) then
             mp2 = ip
             ymin = abs(ytt(ip))
         endif
      enddo
      if (nfft-mp2<20) mp2 = nfft-20 
      do ip = 1, mp1
         yfft_multiharmonic(ip) = yfft_multiharmonic2(ip+npc1-1)
      enddo
      do ip = mp2, nfft
         if (npc2-(nfft-ip) > nfft2) then
             yfft_multiharmonic(ip) = yfft_multiharmonic2(nfft2)
         else
             yfft_multiharmonic(ip) = yfft_multiharmonic2(npc2-(nfft-ip))
         endif
      enddo

      do ip = npc1,npc2
         if (yfft_multiharmonic2(ip) > 10.0) yfft_multiharmonic(ip-npc1+1) = yfft_multiharmonic2(ip)
      enddo

      ! Remove solar-contaminated effects based upon each specIFic type
      CALL First_PC_Contamination(ncount,las_lat,ut,nfft,ytt,xtt,yfft_multiharmonic, NPeak, ytt_s)  

      !Remove effects resulting from the weak solar radiances
      CALL Remove_Wcontamination(ncount,ut,ucws,nfft,xtt,Mpeak,ytt_s)

      !************************************************************************
      !(3) Produce the calibrated data consistent with scanning lines per orbit
      !************************************************************************
      CALL Produce_newly_calibrated_data(ncount,ut,nfft,xtt,ytt_s,you)

      ! Smooth the data using FFT analysis
      CALL WarmTarget_Reconstruction(ncount,ut,las_lat,you,ucws,nfft,ytt_s)

      CALL Produce_newly_calibrated_data(ncount,ut,nfft,xtt,ytt_s,you)
      ucw_cor(ich,1:ncount) = you(1:ncount)

    ENDDO LAS_CHANNELS ! all contaminated channels

  END SUBROUTINE Remove_CW_ANOMALY



  SUBROUTINE Remove_TW_ANOMALY(ncount,nfft,ut,utw,utw_cor)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To reconstruct the PRT temperatures by removing anomaly
  !
  !  Inputs  :
  !    ncount  : the number of scanning lines per orbit
  !    ut      : observation time for each scanning line per orbit (unit: minute)
  !    utw     : the time series of the PRT temperature per orbit with the solar contamination
  !
  !  Outputs    :
  !    utw_cor : reconstructed PRT temperature per orbit
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/05    Banghua Yan      Create programs
  !     2005/06    Banghua Yan      Added algorithm in time DOmain(except for 37 GHz)
  !     2005/07    Banghua Yan      Added algorithm in frequency DOmain(except for 37 GHz)
  !     2005/09    Banghua Yan      Added algorithm for 37V and 37H
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: Min_fft_sample = 15
    INTEGER(4),INTENT(IN) :: nfft
    INTEGER(4)  :: ncount, ip, jp
    REAL(4), DIMENSION(ncount) :: ut,utw,utw_cor
    REAL(4), DIMENSION(nfft)   :: xtt, ytt, yfft_twoharmonic, ytt_s,yt_fft
    REAL(4)   :: filter_parameter,ytt_mean

    ! Estimate the number of FFT calculation points
    IF (nfft <= Min_fft_sample) RETURN  ! all corrections end!

    !fft using original data
    CALL get_fft_inputs(ncount,ut,utw,nfft,xtt,ytt)

    !quality control
    DO ip=1,nfft-4,4
      DO jp=ip, ip+3
        IF (ytt(jp)-ytt(ip) >= 100.) ytt(jp) = ytt(ip)
      ENDDO
    ENDDO

    !fft computation
    filter_parameter = 2.   !*the number of the cycles for samples
    CALL fft_filter(nfft,ytt,filter_parameter,yt_fft)
    yfft_twoharmonic(1:nfft) = yt_fft(1:nfft)

    !reconstrute ytt
    !initialization
    ytt_s(1:nfft) = ytt(1:nfft)
    ytt_mean = SUM(ytt)/SIZE(ytt)
    DO ip=10,nfft-10
      ytt_s(ip) = yfft_twoharmonic(ip)
    ENDDO

    DO ip=1,9
      IF (yfft_twoharmonic(ip) <= ytt(ip) .and. ytt(ip) > ytt_mean)    &
      ytt_s(ip) = yfft_twoharmonic(ip)
    ENDDO

    DO ip= nfft-9,nfft
      IF (yfft_twoharmonic(ip) <= ytt(ip) .and. ytt(ip) > ytt_mean)    &
      ytt_s(ip) = yfft_twoharmonic(ip)
    ENDDO

    !Output data with correction
    CALL Produce_newly_calibrated_data(ncount,ut,nfft,xtt,ytt_s,utw_cor)

  END SUBROUTINE Remove_TW_ANOMALY

  SUBROUTINE Produce_newly_calibrated_data(ncount,time,nfft,xobs,yobs,you)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To produce the data set consistent with all scanning lines per orbit
  !
  !  Inputs  :
  !    ncount  : the number of scanning lines per orbit
  !    nfft    : the munber of the sample for fft analysis per channel
  !    xobs    : observation time for each sample in fft analysis
  !    yobs    : data set used in the fft analysis
  !
  !  Outputs    :
  !    you     : reconstructed data set per orbit
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/07    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4)           :: nfft, ncount,ip,jp,n1,n2
    REAL(4), INTENT(IN)  :: time(*), xobs(*),yobs(*)
    REAL(4), INTENT(OUT), DIMENSION(ncount) :: you
    REAL(4)              :: time0,x1,x2,y1,y2

    DO ip = 1, ncount
      time0 = time(ip)
      IF (time0 <= xobs(1)) THEN
        you(ip) = yobs(1)
      ELSE
        IF (time0 >= xobs(nfft)) THEN
          you(ip) = yobs(nfft)
        ELSE
          n1 = 1
          n2 = 2
          DO jp = 1, nfft
            IF (xobs(jp) >= time0) THEN
              n1 = jp - 1
              n2 = jp
              EXIT
             ENDIF
          ENDDO
          x1 = xobs(n1)
          x2 = xobs(n2)
          y1 = yobs(n1)
          y2 = yobs(n2)
          you(ip) = y1 + (y2-y1)*(time0-x1)/(x2-x1)
        ENDIF
      ENDIF
    ENDDO  ! all scan lines per orbit

  END SUBROUTINE Produce_newly_calibrated_data

  SUBROUTINE WarmTarget_Reconstruction(ncount,ut,lat,ucws,ucwso,nfft,ytt)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To reconstruct warm counts after removing all major anomalies due to solar contamination
  !
  !  Inputs    :
  !    nfft    : the munber of the sample for fft analysis per channel
  !    ucws    : time series of warm count 
  !    ytt     : reconstructed warm counts
  !
  !  Outputs    :
  !    ytt   : newly reconstructed warm counts using FFT results which contain the first eight harmonic components
  !             in the constructed data set
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/08    Banghua Yan      Create programs
  !     2007/03    Banghua Yan      Use fixed-space instead of allocatable variables
  !     2007/05    Banghua Yan      Use a new approach to smooth the data
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT   NONE

    REAL(4), PARAMETER          :: time_interval = 1.0
    INTEGER(4), INTENT(IN)     :: nfft,ncount
    INTEGER(4)                 :: npc1,npc2,ncount2,nfft2,ip,mp1,mp2,mp3,mpx,mp00,mp01,mp02,mpe,mps,mpmin
    REAL(4), DIMENSION(ncount)  :: ut,lat,ucws,ucwso
    REAL(4), DIMENSION(3*ncount):: ut2,lat2,ucws2
    REAL(4), DIMENSION(nfft)    :: ytt
    REAL(4), DIMENSION(3*nfft)  :: xtt2,ytt2,yfft_multiharmonic2
    REAL(4)                     :: timep,filter_index,yc_max,x1,x2,y1,y2,dy,dcw
    LOGICAL                    :: EXTENSION_STATUS

      !Reconstruct Cw data on the left side of max(cw) and cw>cw_max-40.0)
      CALL get_maximum_pixel(1,INT(ncount/3.0),ucws,mp1)
      yc_max = maxval(ucws)
      do ip = 1, mp1
         if (ucws(ip) > yc_max-40.0 .and. ucws(ip) <= yc_max .and. ucws(ip) > 0.0) then
             ucws(ip) = ucwso(ip)
             if (ucwso(ip) > yc_max) ucws(ip) = yc_max
         endif
      enddo
      npc1 = mp1
      mp1 = mp1 + 100
      if (mp1 > INT(ncount*0.7)) mp1 = INT(ncount*0.7)
      CALL get_maximum_pixel(mp1,INT(ncount*0.7),ucws,mp2)
      CALL get_minimum_pixel(mp1,INT(ncount*0.7),ucws,mpmin)
      if (mpmin < mp2) then
          do ip = mpmin,mp2
             if (ucws(ip) > yc_max-40.0 .and. ucws(ip) <= yc_max .and. ucws(ip) > 0.0) then
                 ucws(ip) = ucwso(ip)
                 if (ucwso(ip) > yc_max) ucws(ip) = yc_max
             endif
          enddo
          CALL get_minimum_pixel(mp2,ncount,ucws,mpmin)
          CALL get_maximum_pixel(mpmin,ncount,ucws,mp2)
          do ip = mpmin,mp2
             if (ucws(ip) > yc_max-40.0 .and. ucws(ip) <= yc_max .and. ucws(ip) > 0.0) then
                 ucws(ip) = ucwso(ip)
                 if (ucwso(ip) > yc_max) ucws(ip) = yc_max
             endif
          enddo
      else
          mp1 = mpmin
          CALL get_maximum_pixel(mp1,ncount,ucws,mp2)
          do ip = mpmin,mp2
             if (ucws(ip) > yc_max-40.0 .and. ucws(ip) <= yc_max .and. ucws(ip) > 0.0) then
                 ucws(ip) = ucwso(ip)
                 if (ucwso(ip) > yc_max) ucws(ip) = yc_max
             endif
          enddo
      endif

      ! Build CW data from one orbit to more orbits (05/08/07)
      CALL Build_ucws2_from_ucws(ncount,ut,lat,ucws,ncount2,ut2,lat2,ucws2,npc1,npc2,EXTENSION_STATUS)

      IF (.NOT. EXTENSION_STATUS) RETURN

      ! Time shift in case of ut2(1) <0.0
      if (ut2(1)<0.0) ut2(1:ncount2) = ut2(1:ncount2) - ut2(1)
      nfft2 = INT( (ut2(ncount2) - ut2(1) + 0.5*time_interval)/time_interval + 1 )
      if (nfft2 <10)  RETURN

      CALL get_fft_inputs(ncount2,ut2,ucws2,nfft2,xtt2,ytt2)

      ! Find positions of npc1 and npc2 in the times series of xtt2
      mp1 = 1
      mp2 = nfft2
      timep = ut2(npc1)
      CALL get_TSclosest_point(mp1,mp2,timep,xtt2,npc1)
      timep = ut2(npc2)
      CALL get_TSclosest_point(mp1,mp2,timep,xtt2,npc2)
      ! FFT analysis of extended ytt2
      filter_index = 11.0
      CALL FFT_Analysis_CW(nfft2,ytt2,filter_index,yfft_multiharmonic2)


      !UPDATE ytt using yfft_multiharmonic2
      !Find several critical points
       CALL get_maximum_pixel(1,INT(nfft/2.0),ytt,mp1)   !maximum between 0 and nfft/2
       CALL get_minimum_pixel(mp1,nfft,ytt,mp2)          !minimum between mp1 and nfft
       CALL get_maximum_pixel(mp2,nfft,ytt,mp3)          !maximum betweeb mp2 and nfft
       dy = 30.0
       mp00 = 1
       do ip = 1, mp1                                    ! first zero-point
          if (abs(ytt(ip)) <= dy) then
              dy = abs(ytt(ip))
              mp00 = ip
              if (dy <= 10.0) exit
          endif
       enddo
       dy = 30.0
       mp01 = -1
       do ip = mp1, mp2                                  ! second zero-point
          if (abs(ytt(ip)) <= dy) then
              dy = abs(ytt(ip))
              mp01 = ip
              if (dy <= 10.0) exit
          endif
       enddo
       if (mp01 <1) mp01=int((mp1+mp2)*0.5)
       dy = 30.0
       mp02 = -1
       do ip = mp2,mp3                                   !third zero-point
          if (abs(ytt(ip)) <= dy) then
              dy = abs(ytt(ip))
              mp02 = ip
              if (dy <= 10.0) exit
          endif
       enddo
       if (mp02 <1) mp02=int((mp2+mp3)*0.5)

       mps = mp00
       do ip = mp00, mp1
          if (ytt(ip) > maxval(yfft_multiharmonic2)-10.0) then
              mps = ip
              exit
          endif
       enddo
       if (mps <5) mps = 1
       mpx = mp1+15
       if (mpx >mp01) mpx = mp01
       mpe = mpx
       do ip = mpx, mp2
          if (ytt(ip) < maxval(yfft_multiharmonic2)-10.0) then
             mpe = ip
             exit
          endif
       enddo
       if (mpe >mp01) mpe = mp01
 
       !Update
       do ip = mps, mpe
          if (ip+npc1-1 <= nfft2 .and. ytt(ip) > yfft_multiharmonic2(ip+npc1-1))  &
          ytt(ip)   = yfft_multiharmonic2(ip+npc1-1)
       enddo
       !continuity
       dcw = 0.0
       if (mps >1) dcw = ytt(mps) - ytt(mps-1)
       ytt(mps:mpe) = ytt(mps:mpe) -dcw 
       x1 = mpe
       x2 = mpe+3.0
       if (x2 < mp01) x2 = mp01
       y1 = ytt(int(x1))
       y2 = ytt(int(x2))
       CALL Point_linear_interpolation(x1,x2,y1,y2,ytt)

       mps = mp02
       do ip = mp2,mp3
          if (ytt(ip) > maxval(yfft_multiharmonic2)-10.0) then
              mps = ip
              exit
          endif
       enddo
       if (mps<mp02) mps = mp02

       if (nfft-mps < 5) mps = nfft
       mpx = mp3 + 15
       if (mpx > nfft) mpx = nfft
       mpe = mpx
       do ip = mpx,nfft
          if (ytt(ip) < maxval(yfft_multiharmonic2)-10.0) then
              mpe = ip
              exit
          endif
       enddo
       if (nfft-mpe <5) mpe = nfft
       !Update
       do ip = mps, mpe
          if (ip+npc1-1 <= nfft2 .and. ytt(ip) > yfft_multiharmonic2(ip+npc1-1)) &
          ytt(ip)   = yfft_multiharmonic2(ip+npc1-1)
       enddo
       !continuity
       x1 = mps-3.0
       if (x1 <mp02) x1 = mp02
       x2 = mps
       if (x2<x1) x2 = x1
       y1 = ytt(int(x1))
       y2 = ytt(int(x2))
       CALL Point_linear_interpolation(x1,x2,y1,y2,ytt)
       x1 = mpe
       x2 = mpe+3.0
       if(x2 >nfft) x2 = nfft
       y1 = ytt(int(x1))
       y2 = ytt(int(x2))
       CALL Point_linear_interpolation(x1,x2,y1,y2,ytt)

      !Get required ytt from yfft_multiharmonic2
      !do ip = 1,nfft
      !   if (ip+npc1-1 <= nfft2 .and.  yfft_multiharmonic2(ip+npc1-1) >0.0 ) then
      !       ytt(ip)   = yfft_multiharmonic2(ip+npc1-1)
      !   endif
      !enddo
  END SUBROUTINE WarmTarget_Reconstruction



  SUBROUTINE get_fft_inputs(ncount,time,cw,nfft,xtt,ytt)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To produce the data set for FFT analysis, i.e., the time interval in the data set is a constant
  !    (where the time interval = 1 minute)
  !
  !  Inputs     :
  !    ncount  : the number of scanning lines per orbit
  !    time    : observation time for each scanning line per orbit (unit: minute)
  !     cw     : warm counts for each channel per orbit
  !
  !  Outputs    :
  !    nfft    : the munber of the sample for fft analysis per channel
  !    xo      : observation time for each sample in fft analysis
  !    yo      : warm counts for each sample in fft analysis
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    REAL(4), PARAMETER     :: time_interval = 1.0, smallV = 0.001
    INTEGER(4)             :: nfft, ncount, mp1,mp2
    REAL(4), INTENT(IN)     :: time(ncount), cw(ncount)
    REAL(4), DIMENSION(nfft) :: ytt,xtt
    INTEGER(4) :: npL,npH, nn
    REAL(4)    :: timep,timemax,t1, t2, y1, y2, time_init

    timemax = time(ncount)
    ! Beging/ending points
    ytt(1) = cw(1)
    xtt(1) = time(1)
    ytt(nfft) = cw(ncount)
    xtt(nfft) = time(ncount)
    time_init = time(1)
    DO nn = 2, nfft-1
      timep = time_init + time_interval
      mp1 = 1
      mp2 = ncount
      CALL get_TSclosest_point(mp1,mp2,timep,time,npL)

      IF (timep >= timemax) THEN
          npH = ncount
      ELSE
          CALL get_TLclosest_point(mp1,mp2,timep,time,npH)
      ENDIF
      t1 = time(npL)
      t2 = time(npH)
      y1 = cw(npL)
      y2 = cw(npH)
      IF (abs(t2 - t1) <= smallV) THEN
          ytt(nn) =  y1
      ELSE
          ytt(nn) = y1 + (y2-y1)*(timep-t1)/(t2-t1)
      ENDIF
      xtt(nn) = timep
      time_init = timep
    ENDDO

  END SUBROUTINE get_fft_inputs

  SUBROUTINE Build_ucws2_from_ucws(ncount,ut,lat,ucws,ncount2,ut2,lat2,ucws2,npstart,npend,EXTENSION_STATUS)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To extend onr orbit of CW observations to longer time series
  !
  !  Inputs     :
  !    ncount   : the munber of the scan line in an orbit of observations
  !    ut       : observation time of each scan line during an orbit 
  !    las_lat  : latitude of each LAS observation in a scan line 
  !    ucws     : time series of warm count in an orbit of observations for a certain channel
  !
  !   Outputs   :
  !   ncount2   : the munber of the scan line in the extended orbit of observations
  !   ut2       : observation time of each scan line for an extended orbit of observations
  !   lat2      : latitude of each LAS observation in a scan lin for an extended orbit of observations
  !   ucws2     : time series of warm count in the extended orbit of observations for a certain channel
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2007/05    Banghua Yan      Create programs 
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE
    INTEGER(2), PARAMETER      :: NCW_PLUS_PHASE = 1, NCW_MIXED_PHASE = 2 
    REAL(4), PARAMETER         :: Torbit = 102.0  ,n_unit = 30.0
    INTEGER(2)                 :: NCW_TYPE
    INTEGER(2)                 :: node1,node2,node3
    INTEGER(4)                 :: ncount,ncount2,ncre,ip,npc,npc1,npc2,dlp,lp1,lp2,nphalf,npzero
    INTEGER(4)                 :: npstart,npend
    REAL(4)                    :: x1,x2,y1,y2,t0,tb,theta0,thetab,dlat !n_unit
    REAL(4),DIMENSION(ncount)  :: ut,lat,ucws
    REAL(4),DIMENSION(3*ncount) :: ut2,ucws2,lat2 
    LOGICAL                   :: EXTENSION_STATUS
    ! Calculations of several important variables    
    node1 = 1
    if (lat(1) > lat(3)) node1 = -1
    node2 = 1
    if (lat(ncount) <lat(ncount-2)) node2 = -1

    EXTENSION_STATUS = .TRUE. 
    NCW_TYPE = NCW_PLUS_PHASE
    if (node2 /= node1 .OR. ncount >= 4000) NCW_TYPE = NCW_MIXED_PHASE 

    GET_OPTION: SELECT CASE (NCW_TYPE)

     CASE (NCW_PLUS_PHASE)

      ! Part I (to make up observations in the previous orbit)
      
     ! Build observations in the previour/after orbits

      nphalf = INT(ncount/2.0)
      theta0 = lat(1)
      thetab = lat(ncount)
      t0     = ut(1)
      tb     = ut(ncount)

      ! Previous orbit
      if ((node1 == 1 .and. thetab < theta0) .OR. (node1 == -1 .and. thetab > theta0) ) then
          do ip = nphalf, ncount
             ucws2(ip-nphalf+1) = ucws(ip)
             ut2(ip-nphalf+1)   = ut(ip) - Torbit
             lat2(ip-nphalf+1)  = lat(ip)
          enddo
          ncre = INT((t0-(tb - Torbit))*n_unit) - 5  ! In case of overlapping time: ut2(lp2) <t0 (see below)  
          if (ncre <1) ncre = 1

          lp1 = ncount - nphalf  
          lp2 = lp1 + ncre+1
 
          x1 = real(lp1)
          x2 = real(lp2)
          y1 = ucws(ncount)
          y2 = ucws(1)
          CALL Point_linear_interpolation(x1,x2,y1,y2,ucws2)   
          y1 = lat(ncount)
          y2 = lat(1)
          CALL Point_linear_interpolation(x1,x2,y1,y2,lat2) 
          do ip = lp1, lp2
             ut2(ip) = (tb-Torbit) + (ip-lp1)/n_unit   ! ut2(lp2)<t0
             if (ut2(ip) > t0) then
                 ut2(ip) = t0
             endif
          enddo
          lp1 = (ncount - nphalf) + ncre+1
          lp2 = (ncount - nphalf) + ncre + ncount
          do ip = lp1, lp2
             ucws2(ip) = ucws(ip - lp1 + 1 )
             ut2(ip)   = ut(ip-lp1+1)
             lat2(ip)  = lat(ip-lp1+1)
          enddo    

          ! Save positions of original data
          npstart = lp1
          npend   = lp2

          !Afterward orbit
          lp1 = lp2
          lp2 = lp1 + ncre+1
          x1 = real(lp1)
          x2 = real(lp2)
          y1 = ucws(ncount)
          y2 = ucws(1)
          CALL Point_linear_interpolation(x1,x2,y1,y2,ucws2)
          y1 = lat(ncount)
          y2 = lat(1)
          CALL Point_linear_interpolation(x1,x2,y1,y2,lat2)
          do ip = lp1, lp2
             ut2(ip) = tb + (ip-lp1)/n_unit   ! ut2(lp2)<t0
             if (ut2(ip) > (t0+Torbit)) then
                 ut2(ip) = t0+Torbit
             endif
          enddo

          lp1 = lp2
          lp2 = lp1 + (nphalf-1)
          do ip = lp1, lp2
             ucws2(ip) = ucws(ip-lp1+1)
             ut2(ip)   = ut(ip-lp1+1)+Torbit
             lat2(ip)  = lat(ip-lp1+1)
          enddo
   
      else
          !Make up previous orbit: Remove the overlapping part
          dlat = 20.0
          npc1 = ncount
          do ip = ncount, nphalf, -1
             if (ip > 1) then
                  node3 = 1
                  if (lat(ip)-lat(ip-1) < 0.0) node3 = -1
             endif
             if (abs(lat(ip)-theta0) <= dlat .and. node3 == node1) then
                 dlat = abs(lat(ip)-theta0)
                 npc1 = ip
                 if (dlat <= 0.5) exit
             endif
          enddo

          if (abs(lat(npc1)-theta0) > 2.0) then
             PRINT*,'FAILURE!!!!!!!!!!!!!' 
             EXTENSION_STATUS = .FALSE.
             RETURN
          endif
          npzero = nphalf
          if (npc1 <npzero+300) npzero = npc1 - 300
          if (npzero <1) npzero = 1 
          do ip = npzero, npc1
             ucws2(ip-npzero+1) = ucws(ip)
             ut2(ip-npzero+1)   = ut(ip)- Torbit
             lat2(ip-npzero+1)  = lat(ip) 
          enddo
          lp1 = (npc1 - npzero) + 1
          lp2 = (npc1 - npzero) + ncount
          
          ! Save positions of original data
          npstart = lp1
          npend   = lp2

          if (lp1 <1 .OR. lp2 < 1) then
              EXTENSION_STATUS = .FALSE.
              RETURN
          endif

          do ip = lp1, lp2
             ucws2(ip) = ucws(ip - lp1 + 1 )
             ut2(ip)   = ut(ip - lp1 + 1 )
             lat2(ip)  = lat(ip - lp1 + 1 )
          enddo
          ! Make up the afterward orbit: Remove the overlapping part
          dlat = 20.0
          npc2 = 1
          node3 = node1
          do ip = 1, nphalf
             if (ip >1) then
                 node3 = 1
                 if (lat(ip)<lat(ip-1)) node3 = -1
             endif
             if (abs(lat(ip)-thetab) <=dlat .and. node3 == node2) then
                 dlat = abs(lat(ip)-thetab)
                 npc2 = ip
                 if (dlat <= 0.5) exit
             endif
          enddo
   
          if (abs(lat(npc2)-thetab) > 2.0) then
             PRINT*,'FAILURE!!!!!!!!!!!!!'
             EXTENSION_STATUS = .FALSE.
             RETURN
          endif

         dlp = (nphalf-npc2)
         if (dlp <300) dlp = 300
         if (dlp > INT(ncount/2.0)) dlp = INT(ncount/2.0)
         lp1 = lp2 
         lp2 = lp1 + dlp

          do ip = lp1, lp2
             ucws2(ip) = ucws(ip-lp1+npc2)
             ut2(ip)   = ut(ip-lp1+1)+Torbit + (ut(npc2)-ut(1))
             lat2(ip)  = lat(ip-lp1+npc2)
          enddo

          !Continuity check
          x1 = lp1-20.
          if (x1 <1.) x1 = 1.0
          x2 = lp1+20.0
          y1 = ucws2(INT(x1))
          y2 = ucws2(INT(x2))
          CALL Point_linear_interpolation(x1,x2,y1,y2,ucws2)

      endif

    CASE (NCW_MIXED_PHASE)
        theta0 = lat(1)
        thetab = lat(ncount)
        lp1 = 1200 ! about 40 minutes ~ 0.5 Period
        lp2 = ncount
        dlat = 2.0
        npc = -1
        do ip = lp1, lp2
           node2 = 1
           if (lat(ip) < lat(ip-1)) node2 = -1
           if (node1 == node2 .and. abs(lat(ip)-theta0) <= dlat) then
               npc = ip
               dlat = abs(lat(ip)-theta0)
               if (abs(lat(ip)-theta0) <= 0.5) exit
            endif 
         enddo
         lp1 = npc - 1200
         if (lp1 <1) lp1 = 1
         if (npc == -1) then
             PRINT*,'TOO SHORT!'
             EXTENSION_STATUS = .FALSE.
             RETURN
         endif

         do ip = lp1, npc
             ucws2(ip-lp1+1) = ucws(ip)
             ut2(ip-lp1+1)   = ut(ip) - Torbit
             lat2(ip-lp1+1)  = lat(ip)
          enddo
          npc1 = npc  - lp1
          npstart = npc1

          lp1 = npc1+1
          lp2 = npc1 + ncount
          do ip = lp1, lp2
             ucws2(ip) = ucws(ip - npc1 )
             ut2(ip)   = ut(ip   - npc1 )
             lat2(ip)  = lat(ip  - npc1 )
          enddo
          npc2 = lp2
          npend = npc2

          lp1 = 1
          lp2 = INT(ncount/2.0)
          node2 = 1
          if (lat(ncount)<lat(ncount-1)) node2 = -1
          node3 = node1
          dlat = 2.0
          npc = ncount
          do ip = lp1, lp2
             node3 = 1
             if (lat(ip)<lat(ip-1)) node3 = -1
             if (abs(lat(ip)-thetab) <= dlat .and. node2==node3) then
                 dlat = abs(lat(ip)-thetab)
                 npc = ip
                 if (abs(lat(ip)-thetab) <= 0.5) exit
             endif
          enddo
   
          lp1 = npc
          lp2 = npc + 1200
          if (lp2 > ncount) lp2 = ncount
          do ip = lp1, lp2
             ucws2(npc2+ip-lp1) = ucws(ip)
             ut2(npc2+ip-lp1)   = ut(ip) + Torbit
             lat2(npc2+ip-lp1)  = lat(ip)
          enddo
          
          lp2 = npc2 + lp2-lp1
   END SELECT GET_OPTION

   ! Save parameters
   ncount2 = lp2 ! Number of the extended data
  END SUBROUTINE Build_ucws2_from_ucws

  SUBROUTINE FFT_Analysis_CW(nfft,ytt,filter_parameter,yfft_multiharmonic)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To obtain the first one/two/three harmonic components in warm counts using FFT analysis
  !
  !  Inputs     :
  !    nfft    : the munber of the sample for fft analysis per channel
  !    ytt     : warm count for each sample in fft analysis
  !
  !  Outputs    :
  !    yfft_multiharmonic: reversed warm counts using a low pass filter with a cutting frequency (Fc) equal to the
  !                 first harmonic component (F0) in warm count per channel, i.e., Fc =  nF0, n>3
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/07    Banghua Yan      Create programs (except for 37 GHz)
  !     2005/09    Banghua Yan      Added algorithm for 37V and 37H
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4)           :: nfft
    REAL(4)               :: filter_parameter
    REAL(4), DIMENSION(nfft) :: yfft_multiharmonic
    REAL(4), DIMENSION(nfft) :: ytt, yt_fft

    CALL fft_filter(nfft,ytt,filter_parameter,yt_fft)
    yfft_multiharmonic(1:nfft) = yt_fft(1:nfft)

  END SUBROUTINE FFT_Analysis_CW



  SUBROUTINE fft_filter(ncount,yt,filter_parameter,yt_t)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To obtain the first a few harmonic components using FFT analysis
  !
  !  Inputs     :
  !    ncount  : the munber of the sample for fft analysis
  !    yt      : the sample series in the time field
  !    filter_parameter : a control parameter to catch up how many harmonic components in FFT analyses
  !
  !  Outputs    :
  !    yt_t: reversed sample time series after FFT analyses
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/11    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: ONEORBIT_TLENGTH = 102
    INTEGER(4) :: ncount, INTPOW2
    REAL(4)     :: filter_parameter
    REAL(4), DIMENSION(ncount) :: AR, AI,yt,yt_t

    ! READ DATA IN
    AR(1: ncount) = yt(1:ncount)
    AI(1: ncount) = 0.0
    INTPOW2 = 128

    IF (ncount >= 1.5*ONEORBIT_TLENGTH) INTPOW2 = 256

    CALL FFT_CALCULATION(AR, AI, NCOUNT, INTPOW2, filter_parameter)
    yt_t(1:ncount) = AR(1:ncount)

  END SUBROUTINE fft_filter

  SUBROUTINE Predict_orbit_from_orbit(nscans_init,TimeCW_init,MAX_NPOINTS,zz_init,lat_init,  &
             nscans,Time,zz,lat,zz_o)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To estimate the values in a new orbit based upon the values at a reference orbit
  !
  !  Inputs  :
  !    nscans_init: the number of scanning lines of a reference orbit
  !    time_init  : time function of scall lines of a reference orbit
  !    zz_init    : variation of a parameter in a reference orbit
  !    nscans     : the number of scanning lines of a new orbit
  !    time       : time function of scall lines of a new orbit
  !    lat_ref    : time function of las lat of a new orbit
  !    zz_o       : time function of original cw of a new orbit
  !  Outputs  :
  !    zz         : variation of a parameter in a new orbit
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2006/12    Banghua Yan      Create programs
  !     2007/04    Banghua Yan      UPdated code
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(2)  :: node1,node2
    
    INTEGER(2), INTENT(IN) :: MAX_NPOINTS
    INTEGER(4), INTENT(IN) :: nscans_init,nscans
    INTEGER(4)             :: ncount2,npc1,npc2,ip,np0
    REAL(4)                 :: dlat
    REAL(4), DIMENSION(MAX_NPOINTS) :: TimeCW_init,lat_init
    REAL(4),DIMENSION(nscans_init)   :: zz_init,ut1,lat1
    REAL(4), DIMENSION(3*nscans_init):: ut2,lat2,zz2
    REAL(4), DIMENSION(nscans)       :: Time,zz,lat,zz_o
    LOGICAL EXTENSION_STATUS

    ! INITIALIZATION
    ut1(1:nscans_init)  = TimeCW_init(1:nscans_init)
    lat1(1:nscans_init) = lat_init(1:nscans_init) 

    CALL Build_ucws2_from_ucws(nscans_init,ut1,lat1,zz_init,ncount2,ut2,lat2,zz2,npc1,npc2,EXTENSION_STATUS)

    IF (.NOT. EXTENSION_STATUS) THEN
       zz(1:nscans) = -999.0
       return
    ENDIF


    node1 = 1
    if (lat(3)<lat(1)) node1 = -1
    dlat = 2.0
    np0 = 0
    do ip = 1, INT(ncount2/2.0)
       node2 = 1
       if (lat2(ip+1)<lat2(ip)) node2 = -1
       if (abs(lat(1)-lat2(ip)) <=dlat .and. node1==node2) then
           dlat = abs(lat(1)-lat2(ip))
           np0 = ip
           if (dlat <= 0.5) exit
       endif
    enddo
    if (np0 <1) then
       print*,'FAIL!!!!!!!!!!!!'
       return
    endif
    do ip = np0, ncount2
       zz(ip-np0+1) = zz2(ip)
       if (ip-np0+1 >nscans) exit
    enddo    
    return

  END SUBROUTINE Predict_orbit_from_orbit

  SUBROUTINE New_Orbit_QC(nscans_init,zz_init,lat_init,nscans,zz,lat,New_Orbit_CW_Good)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To estimate the values in a new orbit based upon the values at a reference orbit
  !
  !  Inputs  :
  !    nscans_init: the number of scanning lines of a reference orbit
  !    zz_init    : variation of a parameter in a reference orbit
  !    nscans     : the number of scanning lines of a new orbit
  !    lat_init   : time function of las lat of reference orbit
  !    lat        : time function of las lat of a new orbit
  !  Outputs  :
  !    New_Orbit_CW_Good : =.T., will be set to be a new reference orbit
  !                      : .F. ,        NOT..
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2007/04    Banghua Yan      Create programs
  !     2007/05    Banghua Yan      REvised code
  !-----------------------------------------------------------------------------------------------------------

 
    IMPLICIT NONE
    INTEGER(2)  :: node1,node2,nc
    INTEGER(4) :: nscans,nscans_init,ip,jp,np1,np2
    REAL(4),DIMENSION(nscans_init) :: zz_init,lat_init
    REAL(4), DIMENSION(nscans)     :: zz,lat
    REAL(4)                        :: dlat
    LOGICAL                       :: New_Orbit_CW_Good

    ! critic point 1
    np1 = 1
    node1 = 1
    if (lat(1)>lat(3)) node1 = -1
    node2 = 1
    if (lat_init(1)>lat_init(3)) node2 = -1
    dlat = 20.0
    do jp = 1, nscans_init
       if (jp >1) then
          node2 = 1
          if (lat_init(jp)<lat_init(jp-1)) node2 = -1
       endif
       if ((abs(lat(1) - lat_init(jp)) <= dlat) .and. (jp <= int(nscans_init/2.)) .and. (node1 == node2)) then
           dlat = abs(lat(1) - lat_init(jp))
           np1 = jp
       endif
    enddo
    np2 = nscans_init
    if (np2 > nscans) np2 = nscans
    node1 = 1
    if (lat(3) < lat(1)) node1 = 1
    node2 = node1
    nc = 0
    do ip = np1, np2
       if (ip >1 ) then
          node2 = 1
          if (lat(ip) < lat(ip-1)) node2 = -1
       endif
       if (node2 /=node1 .and. zz(ip) <=0.0 ) exit
       if (abs(zz(ip-np1+1)-zz_init(ip)) > 10.0) nc = nc + 1
    enddo
    if (nc >= 300) New_Orbit_CW_Good = .FALSE.

    !Continuity of node in the begining and ending sides
    node1 = 1
    if (lat(1)>lat(3)) node1 = -1
    node2 = 1
    if (lat(nscans-3)>lat(nscans)) node2 = -1
    if (node1/=node2) New_Orbit_CW_Good = .FALSE.

   
  END SUBROUTINE New_Orbit_QC


  SUBROUTINE FFT_CALCULATION(AR, AI, NCOUNT, INTPOW2, FILTER_PARAMETER)
  !*************************************************************************************************************
  !
  !     SUBROUTINE:  GET_FFT(Subroutine)
  !
  !     PURPOSE:
  !          To get Fast Fourier Transform result.
  !
  !     ARGUMENTS:
  !          AR                 R*4  (INPUT/OUTPUT)   Working Array (Real Part)
  !          AI                 R*4  (INPUT/OUTPUT)   Working Array (Image Part)
  !          NCOUNT             I*4  (INPUT)          The number of points in the working array
  !          INTPOW2            I*4  (INPUT)          The number of points applied in FFT
  !                                                   (INTPOW2 must be a power of 2 !!!)
  !          FILTER_PARAMETER   R*4  (INPUT)          Filter parameter for FFT
  !
  !     EXTERNALS:
  !          FFT().
  !
  !     RECORDS:
  !          DATE        PROGRAMER                DESCRIPTION
  !        ========    =============     ==================================
  !        12/06/05     NINGHAI SUN       Program created
  !        12/08/05     NINGHAI SUN       Change to SUBROUINE
  !
  !     CONTACT:
  !          NINGHAI.SUN@NOAA.GOV, BANGHUA.YAN@NOAA.GOV, FUZHON.WENG@NOAA.GOV
  !
  !*************************************************************************************************************
  IMPLICIT NONE

    ! Declare the input parameters
    INTEGER(4), INTENT(IN) :: NCOUNT, INTPOW2
    REAL(4), INTENT(IN) :: FILTER_PARAMETER
    REAL(4), DIMENSION(NCOUNT), INTENT(INOUT) :: AR, AI

    ! Declare local variables
    REAL(4), DIMENSION(INTPOW2) :: ARR, AII
    REAL(4), DIMENSION(2*INTPOW2) :: CDATA
    REAL(4), DIMENSION(INTPOW2) :: Y, FILTER
    INTEGER(4) :: I, J, DIRECTION
    INTEGER(4) :: LEFT, RIGHT
    REAL(4) :: TEMP, LOC, MU, MU2
    REAL(8) :: DPI

    ! To verIFy INTPOW2 is integer power of 2 otherwise print error message and STOP program.
    TEMP = LOG(INTPOW2*1.0)/LOG(2.0)
    IF ( ABS(TEMP - NINT(TEMP)) .gt. 0.0 ) THEN
      WRITE(*,*) '***************************************************************'
      WRITE(*,*) '*  ERROR MESSAGE: INTPOW2 must be a integer power of 2. !!!   *'
      WRITE(*,*) '***************************************************************'
      STOP
    END IF

    ! Interpolate AR/AI IF NCOUNT is not equal to INTPOW2
    DPI = ACOS(1.0D0)
    IF ( NCOUNT /= INTPOW2 ) THEN
      ARR(1) = AR(1)
      AII(1) = AI(1)
      ARR(INTPOW2) = AR(NCOUNT)
      AII(INTPOW2) = AI(NCOUNT)
      DO I = 2, INTPOW2-1
        LOC = NCOUNT*1.0/INTPOW2 * (I-1)
        LEFT = INT(LOC) + 1
        RIGHT = LEFT + 1
        MU = LOC - INT(LOC)
        MU2 = ( 1 - COS(MU*DPI) )/2.0
        ARR(I) = AR(LEFT) * (1.0 - MU2) + AR(RIGHT) * MU2
        AII(I) = AI(LEFT) * (1.0 - MU2) + AI(RIGHT) * MU2
      END DO
      ! Place ARR, AII into CDATA
      DO I = 1, INTPOW2
        CDATA(2*I-1) = ARR(I)
        CDATA(2*I) = AII(I)
      END DO
    ELSE
    !    WRITE(*,*) 'The number of points is the integer power of 2, no interpolation needed. '

      ! Place AR, AI into CDATA
      DO I = 1, NCOUNT
        CDATA(2*I-1) = AR(I)
        CDATA(2*I) = AI(I)
      END DO
    END IF

    ! Start Forward FFT
    DIRECTION = 1
    CALL FFT(CDATA, INTPOW2, DIRECTION)

    ! Create Butterworth frequency filter
    DO I = 1, INTPOW2
      IF ( I <= INTPOW2/2 ) THEN
        Y(I) = I - 1.0
      ELSE
        Y(I) = (I - 1.0) - INTPOW2
      END IF
    END DO
    J=1
    DO I = INTPOW2, 1, -1
      IF ( I >= INTPOW2/2+2 ) THEN
        Y(I) = Y(J)
        J = J + 1
      END IF
      FILTER(I) = 1.0/(1.0 + (Y(I)/FILTER_PARAMETER)**2)
    END DO

    ! Generate filtered inverse FFT input
    DO I = 1, INTPOW2
      CDATA(2*I-1) = CDATA(2*I-1) * FILTER(I)
      CDATA(2*I) = CDATA(2*I) * FILTER(I)
    END DO

    ! Start inverse FFT
    DIRECTION = -1
    CALL FFT(CDATA, INTPOW2, DIRECTION)

    ! Seperate AR/AI from CDATA. If NCOUNT is not equal to INTPOW2, interpolation is processed.
    IF ( NCOUNT /= INTPOW2 ) THEN
      DO I = 1, INTPOW2
        ARR(I) = CDATA(2*I-1)/INTPOW2
        AII(I) = CDATA(2*I)/INTPOW2
      END DO
      AR(1) = ARR(1)
      AI(1) = AII(1)
      AR(NCOUNT) = ARR(INTPOW2)
      AI(NCOUNT) = AII(INTPOW2)
      DO I = 2, NCOUNT-1
        LOC = INTPOW2*1.0/NCOUNT * (I - 1)
        LEFT = INT(LOC) + 1
        RIGHT = LEFT + 1
        MU = LOC - (LEFT - 1)
        MU2 = ( 1 - COS(MU*DPI) )/2.0
        AR(I) = ARR(LEFT) * (1.0 - MU2) + ARR(RIGHT) * MU2
        AI(I) = AII(LEFT) * (1.0 - MU2) + AII(RIGHT) * MU2
      END DO
    ELSE
      DO I = 1, NCOUNT
        AR(I) = CDATA(2*I-1)/INTPOW2
        AI(I) = CDATA(2*I)/INTPOW2
      END DO
    END IF

    RETURN

  END SUBROUTINE FFT_CALCULATION



  SUBROUTINE FFT(CDATA, NN, DIRECTION)
  !*************************************************************************************************************
  !
  !     PROGRAM:  FFT(Subroutine)
  !               From Numerical Recipes
  !
  !     PURPOSE:
  !          To compute the Fourier transform of a specIFied array.
  !
  !     ARGUMENTS:
  !          CDATA      R*4  (INPUT/OUTPUT)   Working array for Fourier transform
  !          NN         I*4  (INPUT)          Number of complex points in the working array
  !                                           (NN must be a power of 2 !!!)
  !          DIRECTION  I*4  (INPUT)          Indicator for "direction" of Fourier transform:
  !                                           =  1 for discrete Fourier transform
  !                                           = -1 for discrete inverse Fourier transform
  !          (NOTE: eg. spectrum-to-interferogram = inverse Fourier transform)
  !
  !     EXTERNALS:
  !          NONE.
  !
  !     HISTORY:
  !          Revision 2.400  1996/10/24 21:25:12  BenH
  !          Release 2.4 of AERI applications for AERI01, at CART.
  !          Revision 2.300  1996/01/10 14:18:23  BenH
  !          AERIAPPS.LIB, Release2_3
  !          Revision 1.1  1995/08/21 21:08:38  BenH
  !
  !     NOTES:
  !          CDATA is a array of length 2*NN. {R(1),I(1),R(2),I(2),...,R(NN),I(NN)}.
  !          Reversed part (negative for Imaginary part) has been included  from NN/2+1 to NN.
  !          NN MUST be an integer power of 2.
  !          If DIRECTION=1, CDATA will be replaced by its discrete Fourier transform.
  !          If DIRECTION=-1, CDATA will be replaced by NN times its inverse discrete Fourier transform
  !
  !     RECORDS:
  !          DATE        PROGRAMER                DESCRIPTION
  !        ========    =============     ==================================
  !        11/29/05     NINGHAI SUN       ModIFy F77 to F90.
  !
  !
  !     CONTACT:
  !          NINGHAI.SUN@NOAA.GOV, BANGHUA.YAN@NOAA.GOV, FUZHON.WENG@NOAA.GOV
  !
  !*************************************************************************************************************
  IMPLICIT NONE

    ! Declare Parameters
    INTEGER(4), INTENT(IN) :: NN, DIRECTION
    REAL(4), DIMENSION(2*NN), INTENT(INOUT) :: CDATA

    ! Define Local Variables
    REAL(8), PARAMETER :: DPI=3.141592653589793238
    REAL(8) :: WR, WI, WPR, WPI, WTEMP, THETA
    REAL(4) :: TEMPR, TEMPI
    INTEGER(4) :: M, N, I, J, MMAX, ISTEP

    ! Main Program
    ! Rearrange Imaginary and Real arrays
    N = 2 * NN
    J = 1
    DO I = 1, N, 2   ! Odd I points to Real; Even I points to Imaginary
      IF (J .GT. I) THEN
        TEMPR = CDATA(J)
        TEMPI = CDATA(J+1)
        CDATA(J) = CDATA(I)
        CDATA(J+1) = CDATA(I+1)
        CDATA(I) = TEMPR
        CDATA(I+1) = TEMPI
      ENDIF

      M = N/2   ! M == NN
      LOOP1:DO WHILE ( (M .GE. 2) .AND. (J .GT. M) )
        J = J - M
        M = M/2    ! M(=NN) must be integer power of 2
      END DO LOOP1

      J = J + M
    END DO

    MMAX = 2
    LOOP2:DO WHILE ( (N .GT. MMAX) )
      ISTEP = 2*MMAX
      THETA = 2.0*DPI/(DIRECTION*MMAX)
      WPR = -2.0*SIN(0.5*THETA)**2
      WPI = SIN(THETA)
      WR = 1.0
      WI = 0.0

      DO M = 1, MMAX, 2
        DO I = M, N, ISTEP
          J = I + MMAX
          TEMPR = WR*CDATA(J) - WI*CDATA(J+1)
          TEMPI = WR*CDATA(J+1) + WI*CDATA(J)
          CDATA(J) = CDATA(I) - TEMPR
          CDATA(J+1) = CDATA(I+1) - TEMPI
          CDATA(I) = CDATA(I) + TEMPR
          CDATA(I+1) = CDATA(I+1) + TEMPI
        END DO
        WTEMP = WR
        WR = WR*WPR - WI*WPI + WR
        WI = WI*WPR + WTEMP*WPI + WI
      END DO

      MMAX = ISTEP
    END DO LOOP2

    RETURN

  END SUBROUTINE FFT



  SUBROUTINE TA_NOISE_REDUCTION2(nscans,NSCENES_IMG, NSCENES_ENV ,NSCENES_LAS, NSCENES_UAS,           &
                                 img_lat, img_lon,  env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,  &
                                 img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18,                     &
                                 env_ch12,env_ch13,env_ch14,env_ch15,env_ch16,                            &
                                 las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24,       &
                                 uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23)
  !*************************************************************************************************************
  !
  !     PURPOSE:
  !          To reduce the noise by applying space filter
  !
  !  Inputs  :
  !    nscans     : the number of scanning lines per orbit
  !    NSCENES_IMG: the number of observed pixel per scanning line for image channels
  !    NSCENES_ENV: the number of observed pixel per scanning line for environment channels!
  !    NSCENES_LAS: the number of observed pixel per scanning line for lower atmosphere-sensitive channels!
  !    NSCENES_UAS: the number of observed pixel per scanning line for higher atmosphere-sensitive channels;
  !    img_lat     : latitude per observation pixel for image channel
  !    img_lon     : longitude per observation pixel for image channel
  !    env_lat     : latitude per observation pixel for environment channel
  !    env_lon     : longitude per observation pixel for environment channel
  !    las_lat     : latitude per observation pixel for las channel
  !    las_lon     : longitude per observation pixel for las channel
  !    uas_lat     : latitude per observation pixel for uaschannel
  !    uas_lon     : longitude per observation pixel for uas channel
  !    las_ch1    ~ las_ch24: antenna temperature at corresponding channels
  !  Outputs :
  !    las_ch1    ~ las_ch24: noise-reduced antenna temperature at corresponding channels
  !
  !     RECORDS:
  !          DATE        PROGRAMER                DESCRIPTION
  !        ========    =============     ==================================
  !        12/11/06     Banghua Yan       created the code
  !        02/05/07     Banghua Yan       update the code to fix the big space-caused bug
  !
  !*************************************************************************************************************
  IMPLICIT NONE

    ! Declare parameters
    INTEGER(2), PARAMETER :: new = 24,NCAL_PTS = 24
    REAL(4), PARAMETER :: MISSING_DATA = -999.

    ! Declare variables
    INTEGER(2) :: NSCENES_IMG, NSCENES_ENV ,NSCENES_LAS, NSCENES_UAS
    INTEGER(2) :: nscans
    INTEGER(2) :: ich,ns,jp,jp_env,jp_las,jp_img, jp_uas
    INTEGER(2) :: ns1,ns2,np1,np2,ii,jj
    REAL(4)    :: lat1,lat2,lon1,lon2,xnum(24),wei,sigma,dist
    REAL(4), DIMENSION(nscans,NSCENES_IMG) :: img_lat,img_lon,img_ch8,img_ch9,img_ch10,img_ch11,img_ch17,img_ch18
    REAL(4),DIMENSION(nscans,NSCENES_ENV) :: env_lat, env_lon,env_ch12,env_ch13,env_ch14,env_ch15,env_ch16
    REAL(4),DIMENSION(nscans,NSCENES_LAS) :: las_lat,las_lon,las_ch1,las_ch2,las_ch3, las_ch4,las_ch5,las_ch6,las_ch7,las_ch24
    REAL(4),DIMENSION(nscans,NSCENES_UAS) :: uas_lat, uas_lon,uas_ch19, uas_ch20, uas_ch21,uas_ch22,uas_ch23

    REAL(4),DIMENSION(NCAL_PTS)          :: TA,TAD,MTA

    ! Initialization
    sigma = 25.0

    Loop_scan: DO ns = 1, nscans
      Lopp_pixel:   DO jp = 1, NSCENES_IMG
        jp_img = jp
        jp_env = jp
        jp_las = jp
        jp_uas = jp

        IF (jp >= NSCENES_ENV)  jp_env = NSCENES_ENV
        IF (jp >= NSCENES_LAS)  jp_las = NSCENES_LAS
        IF (jp >= NSCENES_UAS)  jp_uas = NSCENES_UAS

        TA(8) = img_ch8(ns,jp_img)
        TA(9) = img_ch9(ns,jp_img)
        TA(10) = img_ch10(ns,jp_img)
        TA(11) = img_ch11(ns,jp_img)
        TA(17) = img_ch17(ns,jp_img)
        TA(18) = img_ch18(ns,jp_img)

        TA(12) = env_ch12(ns,jp_env)
        TA(13) = env_ch13(ns,jp_env)
        TA(14) = env_ch14(ns,jp_env)
        TA(15) = env_ch15(ns,jp_env)
        TA(16) = env_ch16(ns,jp_env)

        TA(1) = las_ch1(ns,jp_las)
        TA(2) = las_ch2(ns,jp_las)
        TA(3) = las_ch3(ns,jp_las)
        TA(4) = las_ch4(ns,jp_las)
        TA(5) = las_ch5(ns,jp_las)
        TA(6) = las_ch6(ns,jp_las)
        TA(7) = las_ch7(ns,jp_las)
        TA(24) = las_ch24(ns,jp_las)

        TA(19) = uas_ch19(ns,jp_uas)
        TA(20) = uas_ch20(ns,jp_uas)
        TA(21) = uas_ch21(ns,jp_uas)
        TA(22) = uas_ch22(ns,jp_uas)
        TA(23) = uas_ch23(ns,jp_uas)

        DO ich=1,new
          IF ( ABS(TA(ich) - MISSING_DATA) .LT. epsilon .OR. TA(ich) <= 30.0)  CYCLE Lopp_pixel
        ENDDO

        ! Further data average
        ! Initialization
        MTA(1:NCAL_PTS) = TA(1:NCAL_PTS)
        xnum(1:NCAL_PTS) = 1.

        LOOP_CHANNEL: DO ich = 1, NCAL_PTS
          sigma = 25.0
          if (ich == 2) sigma = 50.0

          lat1 = img_lat(ns,jp)
          lon1 = img_lon(ns,jp)
          IF (ich <= 7 .OR. ich ==24) THEN
            lat1 = las_lat(ns,jp_las)
            lon1 = las_lon(ns,jp_las)
          ENDIF
          IF (ich >= 12 .AND. ich <= 16) THEN
            lat1 = env_lat(ns,jp_env)
            lon1 = env_lon(ns,jp_env)
          ENDIF
          IF (ich >= 19 .AND. ich <= 23) THEN
            lat1 = uas_lat(ns,jp_uas)
            lon1 = uas_lon(ns,jp_uas)
          ENDIF

          ns1 = ns - 5
          IF (ns1 <1) ns1 = 1
          ns2 = ns + 5
          IF (ns2 > nscans) ns2 = nscans

          np1 = jp - 5
          IF (np1 <1) np1 = 1
          np2 = jp + 5
          IF (np2 > NSCENES_IMG ) np2 = NSCENES_IMG
          IF (ich <= 7 .OR. ich ==24 .AND. np2 > NSCENES_LAS) np2 = NSCENES_LAS
          IF (ich >= 12 .AND. ich <= 16 .AND. np2 > NSCENES_ENV) np2 = NSCENES_ENV
          IF (ich >= 19 .AND. ich <= 23 .AND. np2 > NSCENES_UAS) np2 = NSCENES_UAS
          IF (np1 > np2) np1 = np2

          SCAN_SUBREGION: DO ii = ns1, ns2
            PIXEL_SUBREGION: DO jj = np1, np2
              lat2 = img_lat(ii,jj)
              lon2 = img_lon(ii,jj)
              IF (ich <= 7 .OR. ich ==24) THEN
                lat2 = las_lat(ii,jj)
                lon2 = las_lon(ii,jj)
              ENDIF
              IF (ich >= 12 .AND. ich <= 16) THEN
                lat2 = env_lat(ii,jj)
                lon2 = env_lon(ii,jj)
              ENDIF
              IF (ich >= 19 .AND. ich <= 23) THEN
                lat2 = uas_lat(ii,jj)
                lon2 = uas_lon(ii,jj)
              ENDIF

              !Quality control
              IF (ich == 8) TAD(8) = img_ch8(ii,jj)
              IF (ich == 9) TAD(9) = img_ch9(ii,jj)
              IF (ich == 10) TAD(10) = img_ch10(ii,jj)
              IF (ich == 11) TAD(11) = img_ch11(ii,jj)
              IF (ich == 17) TAD(17) = img_ch17(ii,jj)
              IF (ich == 18) TAD(18) = img_ch18(ii,jj)

              IF (ich == 12) TAD(12) = env_ch12(ii,jj)
              IF (ich == 13) TAD(13) = env_ch13(ii,jj)
              IF (ich == 14) TAD(14) = env_ch14(ii,jj)
              IF (ich == 15) TAD(15) = env_ch15(ii,jj)
              IF (ich == 16) TAD(16) = env_ch16(ii,jj)

              IF (ich == 1) TAD(1) = las_ch1(ii,jj)
              IF (ich == 2) TAD(2) = las_ch2(ii,jj)
              IF (ich == 3) TAD(3) = las_ch3(ii,jj)
              IF (ich == 4) TAD(4) = las_ch4(ii,jj)
              IF (ich == 5) TAD(5) = las_ch5(ii,jj)
              IF (ich == 6) TAD(6) = las_ch6(ii,jj)
              IF (ich == 7)  TAD(7) = las_ch7(ii,jj)
              IF (ich == 24) TAD(24) = las_ch24(ii,jj)

              IF (ich == 19) TAD(19) = uas_ch19(ii,jj)
              IF (ich == 20) TAD(20) = uas_ch20(ii,jj)
              IF (ich == 21) TAD(21) = uas_ch21(ii,jj)
              IF (ich == 22) TAD(22) = uas_ch22(ii,jj)
              IF (ich == 23) TAD(23) = uas_ch23(ii,jj)

              IF ( ABS(TAD(ich) - MISSING_DATA) .LT. epsilon .OR. TAD(ich) <= 30.0)  CYCLE PIXEL_SUBREGION

              ! Calculate weighted value
              CALL realdistance(lat1,lon1,lat2,lon2,dist)

              IF (dist >= 100.0) CYCLE PIXEL_SUBREGION

              wei = exp(-0.5*(dist/sigma)*(dist/sigma))
              xnum(ich) = xnum(ich) + wei
              MTA(ich) = MTA(ich) + wei*TAD(ich)

            ENDDO PIXEL_SUBREGION

          ENDDO SCAN_SUBREGION

        ENDDO LOOP_CHANNEL

        DO ich = 1, NCAL_PTS
          MTA(ich) = MTA(ich)/xnum(ich)
        ENDDO

        ! Updated scene temperatures at 24 channels

        img_ch8(ns,jp_img)  = MTA(8) !TA_COR(8,jp_img,ns)
        img_ch9(ns,jp_img)  = MTA(9) !TA_COR(9,jp_img,ns)
        img_ch10(ns,jp_img) = MTA(10) !TA_COR(10,jp_img,ns)
        img_ch11(ns,jp_img) = MTA(11) !TA_COR(11,jp_img,ns)
        img_ch17(ns,jp_img) = MTA(17) !TA_COR(17,jp_img,ns)
        img_ch18(ns,jp_img) = MTA(18) !TA_COR(18,jp_img,ns)


        IF (jp <= NSCENES_ENV) THEN
          env_ch12(ns,jp_env)  = MTA(12) !TA_COR(12,jp_env,ns)
          env_ch13(ns,jp_env)  = MTA(13) !TA_COR(13,jp_env,ns)
          env_ch14(ns,jp_env)  = MTA(14) !TA_COR(14,jp_env,ns)
          env_ch15(ns,jp_env)  = MTA(15) !TA_COR(15,jp_env,ns)
          env_ch16(ns,jp_env)  = MTA(16) !TA_COR(16,jp_env,ns)
        ENDIF

        IF (jp <= NSCENES_LAS) THEN
          las_ch1(ns,jp_las)   = MTA(1) !TA_COR(1,jp_las,ns)
          las_ch2(ns,jp_las)   = MTA(2) !TA_COR(2,jp_las,ns)
          las_ch3(ns,jp_las)   = MTA(3) !TA_COR(3,jp_las,ns)
          las_ch4(ns,jp_las)   = MTA(4) !TA_COR(4,jp_las,ns)
          las_ch5(ns,jp_las)   = MTA(5) !TA_COR(5,jp_las,ns)
          las_ch6(ns,jp_las)   = MTA(6) !TA_COR(6,jp_las,ns)
          las_ch7(ns,jp_las)   = MTA(7) !TA_COR(7,jp_las,ns)
        ENDIF

        IF (jp <= NSCENES_UAS) THEN
          uas_ch19(ns,jp_uas)   = MTA(19) !TA_COR(19,jp_uas,ns)
          uas_ch20(ns,jp_uas)   = MTA(20) !TA_COR(20,jp_uas,ns)
          uas_ch21(ns,jp_uas)   = MTA(21) !TA_COR(21,jp_uas,ns)
          uas_ch22(ns,jp_uas)   = MTA(22) !TA_COR(22,jp_uas,ns)
          uas_ch23(ns,jp_uas)   = MTA(23) !TA_COR(23,jp_uas,ns)
        ENDIF

        ! Initialization
        xnum(1:NCAL_PTS) = 0.0
        MTA(1:NCAL_PTS) =  0.0

      ENDDO Lopp_pixel

    ENDDO Loop_scan

  END SUBROUTINE TA_NOISE_REDUCTION2

  SUBROUTINE REMOVE_TBA_BIAS(ndaytab, ncoe_max, nphase_max,ndeg,nlat_start,nlat_end,dtbcoe,  &
                             jul_day,node,lato,DTA)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To eatimate the bias of calibrated tb at 24 channels
  !
  !  Inputs  :
  !    jul_day    : Julian Day Number for the observed data
  !    lato       : latitude per observation pixel for image channel
  !    node        : ORBIT NODE
  !                  = 1: ASCENDING, OTHERWISE, DESCENDING
  !
  !  Outputs :
  !    DTA(1:24)    : the bias estomate of antenna temperatures at 24 channels
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2006/12    Banghua Yan      Create programs
  !     2007/01    Banghua Yan      added the bias correction on channel 4
  !     2007/06    Banghua Yan      Updated the code 
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE
    !TB RESIDUAL ERROR
    INTEGER(4), PARAMETER :: NASCENDING = 1, nlat = 362
    INTEGER(4), PARAMETER :: new=24, O2_CHANNELS4 = 7, ndaytabin = 23, np_edge = 3
    REAL(4),    PARAMETER :: dta_max = 1.5, xf00 = 0.01
    INTEGER(2),INTENT(IN) :: ndaytab, ncoe_max, nphase_max
    INTEGER(2)  :: ic,id,ich,nx,nphase,nphasex,DD,DATA_INDEX,ndegx
    INTEGER(4) :: jul_day,node
    INTEGER(2), DIMENSION(ndaytab,O2_CHANNELS4,nphase_max) :: ndeg,nlat_start,nlat_end
    INTEGER(2),DIMENSION(ndaytabin) :: nday_index
    REAL(4)                 :: lato,xlat0,xlat 
    REAL(4), DIMENSION(6)   :: xf
    REAL(4), DIMENSION(new) :: DTA,DTB,DTB0
    REAL(8)                 :: xlatin
    REAL(8),    DIMENSION(ndaytab,O2_CHANNELS4,nphase_max,ncoe_max) :: dtbcoe
    REAL(4), DIMENSION(8)   :: TB2TA_COE
    DATA xf/1.0e+2,1.0e+4,1.0e+6,1.0e+8,1.0e+10,1.0e+12/
    DATA nday_index/1,15,32,46,74,91,105,121,135,152,166,182,196,213,227,244,258,274,288,305,319,335,349/
    DATA TB2TA_COE /0.9850,0.9850,0.9850,0.9850,0.9850,0.9790,0.9815,0.9949/

    !INitialization
    DTA(1:new)  = 0.0
    DTB(1:new)  = 0.0
    DTB0(1:new) = 0.0
    !Update DTA
    ! GET TR INFORMATION
    DATA_INDEX = 1
    DD = 30
    DO id = 1, ndaytab
      IF (ABS(jul_day-nday_index(id)) <= DD) THEN
        DD = ABS(jul_day-nday_index(id))
        DATA_INDEX = id
        IF (DD <= 1) EXIT
      ENDIF
    ENDDO
    IF (DATA_INDEX > ndaytabin) DATA_INDEX = ndaytabin
    IF (DATA_INDEX <1         ) DATA_INDEX = 1
    ! GET NLAT INDEX FROM LAT
    xlat = 180 + (90-int(lato))
    IF (xlat <1)    xlat = 1
    IF (xlat > nlat) xlat =  nlat
    IF (node == NASCENDING) xlat = int(lato) + 90
    nphasex = -999
    CHANNEL_LOOP: DO ich = 2, O2_CHANNELS4
       nx = nphase_max
       if (ich == 2) nx = 2
       if (INT(xlat) <= nlat_start(DATA_INDEX,ich,1)) then
           xlat    = REAL(nlat_start(DATA_INDEX,ich,1))
           nphasex = 1
       else
          if (INT(xlat) >= nlat_start(DATA_INDEX,ich,nx)) then
              if (INT(xlat) >= nlat_end(DATA_INDEX,ich,nx)) xlat = REAL(nlat_end(DATA_INDEX,ich,nx))
              nphasex = nx
          else
              DO nphase = 1, nx
                 if (INT(xlat) >= nlat_start(DATA_INDEX,ich,nphase) .and. INT(xlat) <= nlat_end(DATA_INDEX,ich,nphase) ) then
                     nphasex = nphase
                     exit
                 endif
              ENDDO
              if (nphasex == -999) then
                 DO nphase = 1, nx-1
                   if (INT(xlat) >= nlat_end(DATA_INDEX,ich,nphase) .AND. INT(xlat) < nlat_start(DATA_INDEX,ich,nphase+1)) then
                      !xlat = REAL(nlat_end(DATA_INDEX,ich,nphase))
                      nphasex = nphase
                      if (abs(INT(xlat)-nlat_end(DATA_INDEX,ich,nphase)) > abs(INT(xlat)-nlat_start(DATA_INDEX,ich,nphase+1)))&
                      then
                          !xlat =  REAL(nlat_start(DATA_INDEX,ich,nphase+1))
                          nphasex = nphase + 1
                      endif
                   endif
                 ENDDO
              endif
          endif
       endif
       if (nphasex == -999) RETURN 
       !Calculate DTB and DTA
       ndegx = ndeg(DATA_INDEX,ich,nphasex) + 1
       if (ndegx > 5) ndegx = 5
       if (ndegx <1 ) ndegx = 1
       DTB(ich) = dtbcoe(DATA_INDEX,ich,nphasex,1)
       xlat0 = xlat
       xlatin = xlat0*xf00
       DO ic = 2, ndegx 
          DTB(ich) = DTB(ich) + dtbcoe(DATA_INDEX,ich,nphasex,ic) * xlatin*xf(ic-1)
          xlatin = xlatin*xlat0*xf00
       ENDDO
       if (abs(INT(xlat)-nlat_start(DATA_INDEX,ich,nphasex)) <= np_edge .and. nphasex >= 2) then
           ndegx = ndeg(DATA_INDEX,ich,nphasex-1) + 1
           DTB0(ich) = dtbcoe(DATA_INDEX,ich,nphasex-1,1)
           xlat0 = xlat
           xlatin = xlat0*xf00
           DO ic = 2, ndegx
              DTB0(ich) = DTB0(ich) + dtbcoe(DATA_INDEX,ich,nphasex-1,ic) * xlatin*xf(ic-1)
              xlatin = xlatin*xlat0*xf00
           ENDDO
           DTB(ich) = 0.5*(DTB(ich) + DTB0(ich))
           DTB0(ich) = 0.0
       else
          if (abs(INT(xlat)-nlat_end(DATA_INDEX,ich,nphasex)) <= np_edge .and. nphasex <nphase_max ) then
             ndegx = ndeg(DATA_INDEX,ich,nphasex+1) + 1
             DTB0(ich) = dtbcoe(DATA_INDEX,ich,nphasex+1,1)
             xlat0 = xlat
             xlatin = xlat0*xf00
             DO ic = 2, ndegx
                DTB0(ich) = DTB0(ich) + dtbcoe(DATA_INDEX,ich,nphasex+1,ic) * xlatin*xf(ic-1)
                xlatin = xlatin*xlat0*xf00
             ENDDO
             DTB(ich) = 0.5*(DTB(ich) + DTB0(ich))
             DTB0(ich) = 0.0
          endif
       endif
       IF(ich == 7) DTB(ich) = DTB(ich) - 1.4
       DTA(ich) = DTB(ich)*TB2TA_COE(ich)
    ENDDO CHANNEL_LOOP

    DTA(1)  = DTA(2) - 1.1
    DTA(24) = DTA(4)
    DO ich = 1, new
       if (DTA(ich) > dta_max ) DTA(ich)  =  dta_max
       if (DTA(ich) < -dta_max ) DTA(ich) = -dta_max
    ENDDO
  END SUBROUTINE REMOVE_TBA_BIAS

  SUBROUTINE SC_Location_Indices(ncount,ut,ucw,nfft,xtt,ytt,NPeak,MPeak)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find indicators for solar-contaminated areas from channel 4
  !
  !  Inputs    :
  !    ncount  : the number of scanning lines per orbit
  !    ut      : observation time for each scanning line per orbit (unit: minute)
  !    ucw     : warm counts for each channel per orbit
  !
  !  Outputs:
  !    NPeak     : indicators for solar-contaminated areas in the positive periods estimated from channel 4
  !    MPeak     : indicators for solar-contaminated areas in the negative periods..
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/09    Banghua Yan      Create programs
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: max_contamination_araes = 6, new = 24
    INTEGER(4)  :: ncount, nfft
    REAL(4), INTENT(IN)   :: ut(ncount),ucw(new,ncount)
    REAL(4), DIMENSION(nfft) :: xtt,ytt
    INTEGER(4)              :: NPeak(max_contamination_araes),MPeak(max_contamination_araes)
    REAL(4), DIMENSION(ncount) :: ucws

    !get data at channel 4
    ucws(1:ncount) = ucw(4,1:ncount)

    !fft format
    CALL get_fft_inputs(ncount,ut,ucws,nfft,xtt,ytt)
 
    !Get the indicator for contaminated araes over positive period in warm load function
    CALL PSide_SolarContamination(nfft,ytt,NPeak)

    !Get the indicator for contaminated araes over negative period in warm load function
    CALL MSide_SolarContamination(nfft,xtt,ytt,MPeak)

  END SUBROUTINE SC_Location_Indices



  SUBROUTINE  PSide_SolarContamination(nfft,yt,NPeak)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find indicators for solar-contaminated areas in the positive period from channel 4
  !
  !  Inputs    :
  !    nfft   : the munber of the sample for fft analysis per channel
  !    yt     : warm count for each sample in fft analysis
  !
  !  Outputs:
  !    NPeak     : indicators for solar-contaminated areas in the positive periods estimated from channel 4
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/09    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: max_contamination_araes = 6, ONE = 1, DISCONTINEOUS_C = 3
    REAL(4), PARAMETER    :: SContamination_check_Endwidth  = 20.0, Beg_End_WCount_check_DIFf      = 30.0,  &
                            SContamination_check_width     = 10.0
    INTEGER(4), INTENT(IN) :: nfft
    INTEGER(4) :: npm1st,npmin,npm2nd,npm3rd,npmin2st,npm4th
    INTEGER(4) :: mp1,mp2,ip,nplus,nminus,npm
    REAL(4), INTENT(IN)   :: yt(nfft)
    INTEGER(4) :: NPeak(max_contamination_araes)

    !(1) Find locations of three crtical points (/\/\) on the beging side
    ! npm1st, npmin, npm2nd
    mp1 = 1
    mp2 = nfft/2
    CALL get_maximum_pixel(mp1,mp2,yt,npm2nd)

    mp1 = INT(npm2nd + SContamination_check_width)
    IF (mp1 > nfft)  mp1 = nfft
    mp2 = nfft/2
    if (mp2 <=mp1+10) mp2 = mp1+10
    if (mp2 >nfft) mp2 = nfft


    CALL get_maximum_pixel(mp1,mp2,yt,npm)
    IF (yt(npm) + 30. >= yt(npm2nd)) THEN
       npm1st = npm2nd
       npm2nd = npm
    ELSE
       mp2 = INT(npm2nd - SContamination_check_width)
       IF (mp2 < ONE) mp2 = ONE
       mp1 = ONE
       CALL get_maximum_pixel(mp1,mp2,yt,npm1st)
    ENDIF
    mp1 = npm1st
    mp2 = npm2nd
    CALL get_minimum_pixel(mp1,mp2,yt,npmin)

    !(2) Find the locations of other three crtical points (/\/\) on the ending side
    ! npm3rd, npmin2st, npm4th
    mp1 = INT(nfft - SContamination_check_Endwidth)
    mp2 = nfft
    IF (mp1 < nfft/2+1) mp1 = nfft/2+1
    CALL get_maximum_pixel(mp1,mp2,yt,npm3rd)

    nplus = 0
    nminus = 0
    DO ip = mp1, mp2,2
      IF (yt(ip) - yt(ip-1) >= DISCONTINEOUS_C) nplus = nplus + 1
      IF (yt(ip) - yt(ip-1) <= -DISCONTINEOUS_C) nminus = nminus +1
    ENDDO
    IF ( (nplus >= 3 .and. nminus >= 3)  .or.    &
         (abs(yt(npm2nd)-yt(npm3rd)) <= Beg_End_WCount_check_DIFf)) THEN
      mp1 = nfft-15
      mp2 = nfft
      IF (mp1 <= nfft/2+1) mp1 = nfft/2+1
      CALL get_maximum_pixel(mp1,mp2,yt,npm4th)

      mp1 = nfft-30
      IF (mp1 <= nfft/2+1) mp1 = nfft/2+1
      mp2 = nfft-15
      IF (mp2 <= nfft/2+1) mp2 = nfft/2+1
      CALL get_maximum_pixel(mp1,mp2,yt,npm3rd)

      mp1 =npm3rd
      mp2 = npm4th
      CALL get_minimum_pixel(mp1,mp2,yt,npmin2st)
    ELSE
      npmin2st = nfft
      npm3rd = nfft
      npm4th = nfft
    ENDIF

    NPeak(1) = npm1st
    NPeak(2) = npmin
    NPeak(3) = npm2nd
    NPeak(4) = npm3rd
    NPeak(5) = npmin2st
    NPeak(6) = npm4th
 
  END SUBROUTINE  PSide_SolarContamination



  SUBROUTINE MSide_SolarContamination(nfft,time,yt,MPeak)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find indicators for solar-contaminated areas in the negative period from channel 4
  !
  !  Inputs    :
  !    time    : observation time for each sample in fft analysis
  !    yt     : warm count for each sample in fft analysis
  !
  !  Outputs:
  !    MPeak     : indicators for solar-contaminated areas in the negative period
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/09    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: max_contamination_araes = 6
    REAL(4), PARAMETER    :: width_LContamination = 20.0, width_RContamination = 25.0
    INTEGER(4) :: npmin
    INTEGER(4) :: nfft,nb1,nb2,nb3,na1,na2,na3,np_LS, np_RS,np_lm, np_rm,    &
                  npmin_1st,npmin_2nd,mp1,mp2, np1,np2,ntt
    REAL(4), INTENT(IN)   :: time(nfft),yt(nfft)
    INTEGER(4), INTENT(OUT)  :: MPeak(max_contamination_araes)
    REAL(4)               :: timep
    INTEGER(4), DIMENSION(nfft) :: nt

    !Define indicators for contaminated areas in the negative periods in warm load function
    !Start to find...
    ntt = SIZE(MINLOC(yt))
!    ALLOCATE(nt(ntt),STAT=istat)
    nt(1:ntt) = MINLOC(yt)
    npmin = nt(1)

    !Check original data in case of intrapolation error
    mp1 = 1
    mp2 = npmin
    timep=time(npmin) - 50.0
    IF (timep <= time(1)) timep = time(1)
    CALL get_Tclosest_point(mp1,mp2,timep,time,np_LS)

    mp1 = npmin
    mp2 = nfft
    timep=time(npmin) + 50.0
    IF (timep > time(nfft)) timep = time(nfft)
    CALL get_Tclosest_point(mp1,mp2,timep,time,np_RS)

    np1 = np_LS
    np2 = np_RS
    CALL get_minimum_pixel(np1,np2,yt,npmin_1st)

    !on the left side
    !check other minimum peak
    mp1 = np1
    mp2 = npmin_1st
    timep = time(npmin_1st) - 10.0
    IF (timep <= time(1)) timep = time(1)
    CALL get_Tclosest_point(mp1,mp2,timep,time,npmin_2nd)

    IF (abs(yt(npmin_2nd)-yt(npmin_1st)) <= 5) THEN
       npmin = npmin_2nd
    ELSE
       npmin = npmin_1st
    ENDIF

    mp1 = np1
    mp2 = npmin
    timep = time(npmin) - width_LContamination
    IF (timep <= time(1)) timep = time(1)
    CALL get_Tclosest_point(mp1,mp2,timep,time,np_lm)
    CALL get_LSweak_Clocations(yt,npmin,na1,na2,na3)
    MPeak(1) = na1
    MPeak(2) = na2
    MPeak(3) = na3

    !on the right side
    !check other minimum peak
    mp1 = npmin
    mp2 = np2
    timep = time(npmin) + width_RContamination
    IF (timep >= time(nfft)) timep = time(nfft)
    CALL get_Tclosest_point(mp1,mp2,timep,time,np_rm)
    CALL get_RSweak_Clocations(yt,npmin,np2,nb1,nb2,nb3)
    MPeak(4) = nb1
    MPeak(5) = nb2
    MPeak(6) = nb3

  END  SUBROUTINE MSide_SolarContamination


  SUBROUTINE  First_PC_Contamination(ncount,lat,ut,nfft,yt,xt,yfft_multiharmonic, NPeak, yt_s)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To removing anomalies in the warm counts starting with positive period
  !
  !  Inputs  :
  !    ncount  : the number of scan lines
  !    nfft    : the munber of the sample for fft analysis per channel
  !    yt      : time series of original CW data
  !    lat     : ..          of las_lat
  !    yfft_multiharmonic: reversed warm counts using a low pass filter with a cutting frequency (Fc) equal to the
  !                 first harmonic component (F0) in warm count per channel, i.e.,Fc = 5F0
  !    NPeak     : indicators for solar-contaminated areas in the positive periods estimated from channel 4
  !
  !  Outputs    :
  !    yt_s     : warm counts after removing major solar-contaminated effects
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/07    Banghua Yan      Create programs
  !     2007/05    Banghua Yan      Revised code
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    ! Define serval constraints
    INTEGER(4), PARAMETER :: SContamination_Rwidth = 15, SContamination_Lwidth = 5 

    INTEGER(4), INTENT(IN) :: nfft,ncount
    INTEGER(4) :: ip, npc,npp1, npp2,node1,node2,npmax1,npmax2,npmax
    INTEGER(4) :: NPeak(6),npm1st,npmin,npm2nd,npm3rd,npmin2st,npm4th
    REAL(4), DIMENSION(nfft)   :: xt,yt,yt_s,yfft_multiharmonic
    REAL(4), DIMENSION(ncount) :: lat,ut
    REAL(4)     :: x1,x2,y1,y2

    !get locations of three crtical points
    CALL PSide_SolarContamination(nfft,yt,NPeak)
    npm1st   = NPeak(1)
    npmin    = NPeak(2)
    npm2nd   = NPeak(3)
    npm3rd   = NPeak(4)
    npmin2st = NPeak(5)
    npm4th   = NPeak(6)
 
    ! node in the beginging and ending side of the data
    node1 = 1
    if (lat(1)>lat(3)) node1 = -1
    node2 = 1
    if (lat(ncount)<lat(ncount-2)) node2 = -1

    CALL get_maximum_pixel(1,int(nfft/2.0),yt,npmax1)
    CALL get_maximum_pixel(int(nfft/2.0),nfft,yt,npmax2)
    CALL get_maximum_pixel(1,nfft,yt,npmax)
    

    if (yt(npmax1)+40.0 >= yt(npmax2) .or. (node1==node2 .and. npmax <=int(nfft/2.0)) ) then
        npp1 = npm1st - SContamination_Lwidth 
        if (npp1<1) npp1 = 1
        npp2 = npm2nd + SContamination_Rwidth
        if (npp2 > nfft) npp2 = nfft

        do ip = npp1,npp2
        !   if (yt(ip) > yfft_multiharmonic(ip) ) yt_s(ip) = yfft_multiharmonic(ip)
        enddo
        ! Smooth 
        npp1 = npp2-3
        if (npp1 <1) npp1 = 1
        npp2 = npp2+3
        if (npp2 > nfft) npp2 = nfft
        x1=real(npp1)
        x2=real(npp2)
        y1=yt_s(npp1)
        y2=yt_s(npp2)
        CALL Point_linear_interpolation(x1,x2,y1,y2,yt_s)
        !Remove some singular points
        CALL get_maximum_pixel(1,INT(nfft/2.0),yfft_multiharmonic,npc)
        ! Underestimate yt_s
        npp2 = npc - 25 
        if (npp2 <1) npp2 = 1
        npp1 = npc - 40
        if (npp1 <1) npp1 = 1
        do ip = npp1, npp2
        !   if (yt(ip) > yfft_multiharmonic(ip) ) yt_s(ip) = yt(ip)
        enddo 
        DO ip = 1, nfft
           if (yt_s(ip) >maxval(yfft_multiharmonic)) yt_s(ip) = maxval(yfft_multiharmonic) 
        enddo


    endif
   
    if (yt(nfft)+40.0 < maxval(yt) .and. node1 == node2 .and. npmax <int(nfft/2.0)) return

    npp1 = npm3rd - SContamination_Lwidth
    if (nfft-npp1 <15) npp1 = nfft-15
    if (npp1 <1) npp1 = 1
    npp2 = npm4th + SContamination_Rwidth
    if (npp2 > nfft) npp2 = nfft
    do ip = npp1,npp2
    !   if (yt(ip) > yfft_multiharmonic(ip)) yt_s(ip) = yfft_multiharmonic(ip)
    enddo    
    !smooth
    npp1 = npp1-3
    if (npp1 <1) npp1 = 1
    npp2 = npp1+3
    if (npp2 > nfft) npp2 = nfft
    x1=real(npp1)
    x2=real(npp2)
    y1=yt_s(npp1)
    y2=yt_s(npp2)
    CALL Point_linear_interpolation(x1,x2,y1,y2,yt_s)   
    !Remove some singular points
    CALL get_maximum_pixel(INT(nfft/2.0),nfft,yfft_multiharmonic,npc)
    DO ip = npc,nfft
       if (yt_s(ip) >maxval(yfft_multiharmonic)) yt_s(ip) = maxval(yfft_multiharmonic)
    enddo
    ! Underestimate yt_s
    npp2 = npc + 40
    if (npp2 >nfft) npp2 = nfft
     npp1 = npc + 15
     if (npp1 >nfft) npp1 = nfft
     do ip = npp1, npp2
     !   if (yt(ip) > yfft_multiharmonic(ip)) yt_s(ip) = yt(ip)
     enddo
     


  END SUBROUTINE First_PC_Contamination


  SUBROUTINE Remove_Wcontamination(ncount,ut,ucws,nfft,time,MPeak,yt)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To remove weakly solar-contaminated effects
  !
  !  Inputs     :
  !    ncount  : the number of scanning lines per orbit
  !    ut      : observation time for each scanning line per orbit (unit: minute)
  !    ucws    : warm counts for each channel per orbit
  !    nfft    : the munber of the sample for fft analysis per channel
  !    time    : observation time for each sample in fft analysis
  !    yt      : constructed warm counts after removing strongly solar-contaminated effects
  !    MPeak     : indicators for solar-contaminated areas in the negative periods estimated from channel 4
  !
  !  Outputs    :
  !    yt    : reconstructed warm counts for each channel
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/05    Banghua Yan      Create programs
  !     2005/11    Banghua Yan      Revised code
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER    :: ONE = 1
    INTEGER(4) :: ncount, nfft, MPeak(*)
    INTEGER(4)  :: ln1, ln2,lnmin,lp1,lp2,lpmin,nc
    REAL(4), INTENT(IN)      :: ut(*),ucws(*),time(*)
    REAL(4), INTENT(INOUT)   :: yt(*)
    REAL(4), DIMENSION(nfft) :: yt_s
    REAL(4)     :: x1,x2,y1,y2,timep

    !initialization
    yt_s(1:nfft) = yt(1:nfft)
    !Linear fitting over solar-contaminated areas using channel4_derived indices
    ! Over left side: contaminated areas MPeak(1) ~ MPeak(3)
    ! (1) MPeak(1) ~ MPeak(2)
    ln1 = MPeak(1)
    ln2 = MPeak(1) + 3
    IF (ln2 >= MPeak(2))ln2 = MPeak(2)
    CALL get_minimum_pixel(ln1,ln2,yt,lnmin)
    IF (lnmin <= MPeak(2)) lnmin = MPeak(2)
    x1 = time(MPeak(1))
    y1 = yt_s(MPeak(1))

    !get minimum from the original data in case of interpolation error
    timep = time(lnmin)
    IF (timep <= time(1)) timep = time(1)
    CALL get_Tclosest_point(1,ncount,timep,ut,nc)
    lp1 = nc-20
    lp2 =nc+20
    IF (lp1 < ONE) lp1 = ONE
    IF (lp2 > ncount) lp2 = ncount
    CALL get_minimum_pixel(lp1,lp2,ucws,lpmin)
    y2 = ucws(lpmin)
    x2 = ut(lpmin)
    CALL linear_interpolation(x1,x2,y1,y2,MPeak(1), lnmin,time,yt_s)

    !(2) MPeak(2) ~ MPeak(3)
    y1 = y2
    x1 = x2
    !get minimum from the original data in case of interpolation error (06/09/2007: revised)
    timep = time(MPeak(3))
    IF (timep < time(1)) timep = time(1)
    CALL get_Tclosest_point(1,ncount,timep,ut,nc)
    lp1 = nc-20
    lp2 =nc+20
    IF (lp1 < ONE) lp1 = ONE
    IF (lp2 > ncount) lp2 = ncount
    CALL get_minimum_pixel(lp1,lp2,ucws,lpmin)
    y2 = ucws(lpmin)
    x2 = time(MPeak(3))
    CALL linear_interpolation(x1,x2,y1,y2,lnmin,MPeak(3),time,yt_s)

    ! Over right side: contaminated areas MPeak(4) ~ MPeak(6)
    !(1) get mean slope over the region (MPeak(4), MPeak(5))
    x1 = time(MPeak(4))
    x2 = time(MPeak(5))

    ! Use the original data set to get the minimum  in case of interpolation error
    timep = time(MPeak(4))
    IF (timep < time(1)) timep = time(1)
    CALL get_Tclosest_point(1,ncount,timep,ut,nc)

    lp1 = nc-20
    lp2 =nc+20
    IF (lp1 < ONE) lp1 = ONE
    IF (lp2 > ncount) lp2 = ncount
    CALL get_minimum_pixel(lp1,lp2,ucws,lpmin)
    y1 = ucws(lpmin)

    timep = time(MPeak(5))
    IF (timep < time(1)) timep = time(1)
    CALL get_Tclosest_point(1,ncount,timep,ut,nc)
    lp1 = nc-20
    lp2 = nc+20
    IF (lp1 < ONE) lp1 = ONE
    IF (lp2 > ncount) lp2 = ncount
    CALL get_minimum_pixel(lp1,lp2,ucws,lpmin)
    y2 = ucws(lpmin)

    CALL linear_interpolation(x1,x2,y1,y2,MPeak(4),MPeak(5),time,yt_s)

    !(2) get mean slope over the region (MPeak(5), MPeak(6))
    x1 = time(MPeak(5))
    x2 = time(MPeak(6))

    ! Use the original data set to get the minimum  in case of interpolation error
    timep = time(MPeak(5))
    IF (timep < time(1)) timep = time(1)
    CALL get_Tclosest_point(1,ncount,timep,ut,nc)
    lp1 = nc-20
    lp2 =nc+20
    IF (lp1 < ONE) lp1 = ONE
    IF (lp2 > ncount) lp2 = ncount
    CALL get_minimum_pixel(lp1,lp2,ucws,lpmin)
    y1 = ucws(lpmin)

    timep = time(MPeak(6))
    IF (timep < time(1)) timep = time(1)
    CALL get_Tclosest_point(1,ncount,timep,ut,nc)
    lp1 = nc-20
    lp2 = nc+20
    IF (lp1 < ONE) lp1 = ONE
    IF (lp2 > ncount) lp2 = ncount
    CALL get_minimum_pixel(lp1,lp2,ucws,lpmin)
    y2 = ucws(lpmin)

    CALL linear_interpolation(x1,x2,y1,y2,MPeak(5),MPeak(6),time,yt_s)

    ! update data
    yt(1:nfft) = yt_s(1:nfft)

  END SUBROUTINE Remove_Wcontamination



  SUBROUTINE get_LSweak_Clocations(yt,npmin,na1,na2,na3)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find the positions for the solar-contaminated areas on the left side in the negative period
  !    using the slope index
  !
  !  Inputs     :
  !    yt      : warm counts for each sample in fft analysis
  !    npmin      : the location with a minimum peak
  !    Outputs    :
  !    na1 ~ na 3 : three indicators for solar contaminated areas on the left side of the observed point at NP = npmin
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !     2005/08    Banghua Yan      Revised code
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: ONE = 1, Contamination_Time_length = 10   ! minutes
    INTEGER(4) :: npmin,na1,na2,na3
    INTEGER(4) :: np1, np2, n2nd,npmax,n2nd_min
    REAL(4), INTENT(IN) :: yt(*)

    !find the pixel next_to_minus_point
    np1 = npmin - Contamination_Time_length
    IF (np1 <= ONE) np1 = ONE
    np2 = npmin
    CALL get_maximum_pixel(np1,np2,yt,npmax)
    n2nd = npmax

    np1 = n2nd - Contamination_Time_length
    IF (np1 <= ONE) np1 = ONE
    np2 = n2nd

    CALL get_minimum_pixel(np1,np2,yt,n2nd_min)

    na1 = n2nd_min-Contamination_Time_length
    IF (na1 < ONE) na1 = ONE
    na2 = n2nd_min
    na3 = npmin

  END SUBROUTINE get_LSweak_Clocations



  SUBROUTINE get_RSweak_Clocations(yt,npmin,np_rm,na1,na2,na3)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find the positions for the solar-contaminated areas on the right side in the negative period
  !    using the slope index
  !
  !  Inputs     :
  !    yt      : warm counts for each sample in fft analysis
  !    npmin    : minimum peak in warm counts
  !
  !  Outputs    :
  !    na1 ~ na 3 : three indicators for solar contaminated areas on the right side of the observed point at NP = npmin
  !
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !     2005/08    Banghua Yan      Revised code
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4), PARAMETER :: Contamination1_Time_length = 12, Contamination2_Time_length = 10  ! minutes
    INTEGER(4) :: npmin,np_rm,na1,na2,na3
    INTEGER(4) :: np1, np2,n2nd, n2nd_min,npmax
    REAL(4), INTENT(IN) :: yt(*)

    np1 = npmin
    np2 = npmin + Contamination1_Time_length
    IF (np2 >= np_rm) np2 = np_rm
    CALL get_maximum_pixel(np1,np2,yt,npmax)
    n2nd = npmax

    np1 = n2nd
    np2 = n2nd + Contamination2_Time_length
    IF (np2 >= np_rm) np2 = np_rm
    CALL get_minimum_pixel(np1,np2,yt,n2nd_min)

    na1 = npmin
    na2 = n2nd_min
    na3 = n2nd_min + Contamination2_Time_length
    IF (na3 >= np_rm) na3 = np_rm

  END SUBROUTINE get_RSweak_Clocations



  SUBROUTINE CW_Quality_Check(ncount,ucw)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To remove any discontinuity observations of warm load counts at SSMIS channels except for channel 15
  !
  !  Inputs  :
  !    ncount  : the number of scanning lines per orbit
  !    ucw     : warm counts for each channel
  !
  !  Outputs  : ucw
  !
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/08    Banghua Yan      Create programs
  !     2005/11    Banghua Yan      Revised code F90
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(2), PARAMETER :: Npixel_interval = 3, NCHAN = 8, new = 24
    REAL(4), PARAMETER    :: Max_jump = 120.0

    INTEGER(4) :: ncount,ip, jp, ich
    REAL(4), INTENT(INOUT) :: ucw(new,ncount)

    DO ich=1,NCHAN
      DO ip=1, ncount - Npixel_interval, Npixel_interval
        DO jp=ip, ip+Npixel_interval
           IF ( ucw(ich,jp)-ucw(ich,ip) >= Max_jump .or. ucw(ich,jp)-ucw(ich,ip) <= - Max_jump )  &
             ucw(ich,jp) = ucw(ich,ip)
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE CW_Quality_Check



  SUBROUTINE ORBIT_INITIALIZATION (O2_CHANNELS, MAX_NPOINTS, npeakm, nday, nlat, fname_dtr_table, fname_TBBIAS_COE,  &
                                   nscans_init,Previous_CWAnomaly_Correction, jul_day_init,                          &
                                   TimeCW_init, ucw_cor_init, NPeak_init, MPeak_init,                                &
                                   dcw_init, utw_cor_init, cw_mean_init, lat_init,DTR_YEAR,Anomaly_Chan_filter_index,&
                                   ndeg,nlat_start,nlat_end,DTBCOE)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    SSMIS ORBIT KEY VARIABLE INIALIZATION
  !
  !  Inputs (Please refer to SSMIS tdr Document for details)
  !    O2_CHANNELS    : Number of sounder channels
  !    MAX_NPOINTS    : Max number of points in TimeCW_init, ucw_cor_init,  utw_cor_init
  !    nday           : Number of days in lookup table
  !    nlat           : Number of latitudes in lookup table
  !    fname_dtr_table: Lookup table file name
  !    nscans_init    : Number of scans
  !  Ouputs
  !    Previous_CWAnomaly_Correction = .T.: the CW anomaly correction is being applied to a new orbit
  !                                    .F.: the CW anomaly correction is being applied to the first orbit
  !    jul_day       : julian day
  !    TimeCW_init   : the time series of the scan lines for the previous orbit
  !    ucw_cor_init  : the warm load variance for the previous orbit
  !    NPeak_init    : the number of peak
  !    MPeak_init    : the number of peak
  !    dcw_init      : maximum warm load anomaly for the previous orbit
  !    utw_cor_init  : PRT temperature for the previous orbit
  !    cw_mean_init  : the mean warm load for the previous orbit
  !    DTR_YEAR      : Lookup tables of DTR (reflector - arm temperature) at channel 4 through a year
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/12    Banghua Yan      Create programs
  !     2006/12    Banghua Yan      Added the part related to TR_YEAR
  !     2007/01    Banghua Yan      Change TR to DTR ( DTR = TR - TARM)
  !                                 DTR0 : a fixed space
  !     2007/01    Ninghai Sun      Change Input/Output arguments. Use explicit-shape array instead
  !                                 of deferred-shape(allocate) and assumed-size array
  !     2007/04    Banghua Yan      Add lat_init (for las channel)
  !     2007/05    Banghua Yan      Add Anomaly_Chan_filter_index
  !     2007/06    Banghua Yan      Add TB bias coefficients
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    ! Define Input/Output variables
    INTEGER(4),PARAMETER   :: O2_CHANNELS4 = 7
    INTEGER(2),PARAMETER   ::  ndaytab = 23, ncoe_max = 5, nphase_max = 5

    INTEGER(2), INTENT(IN) :: O2_CHANNELS, npeakm, nday, nlat,MAX_NPOINTS
    CHARACTER(LEN=250), INTENT(IN) :: fname_dtr_table,fname_TBBIAS_COE
    INTEGER(4), INTENT(IN) :: nscans_init
    LOGICAL, INTENT(OUT) :: Previous_CWAnomaly_Correction
    REAL(4), DIMENSION(MAX_NPOINTS), INTENT(OUT) :: TimeCW_init,utw_cor_init,lat_init
    INTEGER(4), DIMENSION(npeakm), INTENT(OUT) :: NPeak_init,MPeak_init
    INTEGER(2), INTENT(OUT)                    :: jul_day_init
    REAL(4), DIMENSION(O2_CHANNELS,MAX_NPOINTS), INTENT(OUT) :: ucw_cor_init
    REAL(4), DIMENSION(O2_CHANNELS),INTENT(OUT) :: dcw_init
    REAL(4), DIMENSION(O2_CHANNELS), INTENT(OUT) :: cw_mean_init
    REAL(4), DIMENSION(nday, nlat), INTENT(OUT)  :: DTR_YEAR
    REAL(4), DIMENSION(O2_CHANNELS4)             :: Anomaly_Chan_filter_index

    ! Define local variables
    INTEGER(2) :: id, day_index
    REAL(4), DIMENSION(nlat) :: DTR0


    !BIAS
    REAL(8),     DIMENSION(ndaytab,O2_CHANNELS4,nphase_max,ncoe_max),INTENT(OUT) :: DTBCOE
    REAL(8),     DIMENSION(ncoe_max)                                          :: x5
    INTEGER(2), DIMENSION(ndaytab,O2_CHANNELS4,nphase_max),INTENT(OUT)       :: ndeg,nlat_start,nlat_end
    INTEGER(2), DIMENSION(nphase_max)                                        :: nx1,nx2,nx3
    INTEGER(2)                                                               :: idate,ich,nphase,ndegx,n1,n2,n3,nx
    ! GET RELATED DATA
    !DATA xf/1.0e+2,1.0e+4,1.0e+6,1.0e+8,1.0e+10,1.0e+12/
    !DATA nday_index/1,15,32,46,60,74,91,105,121,135,152,166,182,196,213,227,244,258,274,288,305,319,335,349/



    CLOSE(35)
    OPEN(35,FILE=fname_dtr_table,STATUS='OLD',ACTION='READ')
    DO id = 1, nday
      READ(35,'(I2,1X,180(F6.2,1X))') day_index,DTR0(1:180)
      READ(35,'(I2,1X,180(F6.2,1X))') day_index,DTR0(181:nlat)
      DTR_YEAR(id,1:nlat) = DTR0(1:nlat)
    ENDDO
    CLOSE(35)

   !TB BIAS FITTING COEFFICIENTS
   !INITIALIZATION 
    DTBCOE(1:ndaytab,1:O2_CHANNELS4,1:nphase_max,1:ncoe_max) = 0.0
    ndeg(1:ndaytab,1:O2_CHANNELS4,1:nphase_max)              = 999
    nlat_start(1:ndaytab,1:O2_CHANNELS4,1:nphase_max)        = 1
    nlat_end(1:ndaytab,1:O2_CHANNELS4,1:nphase_max)          = 1

    OPEN(35,FILE=fname_TBBIAS_COE,STATUS='OLD',ACTION='READ')
    DO id = 1, ndaytab
       DO ich = 2, O2_CHANNELS4
          READ(35,'(17(I4,1X))')idate,n1,nx1,nx2,nx3
          ndeg(id,ich,1:nphase_max)       = nx1(1:nphase_max)
          nlat_start(id,ich,1:nphase_max) = nx2(1:nphase_max)
          nlat_end(id,ich,1:nphase_max)   = nx3(1:nphase_max)
       ENDDO
    ENDDO
    DO id = 1, ndaytab
       DO ich = 2, O2_CHANNELS4
          nx = nphase_max
          if (ich == 2) nx = 2
          DO nphase = 1, nx
             if ( ndeg(id,ich,nphase) /= 999) then
                 ndegx = ndeg(id,ich,nphase)+1
                 READ(35,'(3(I3,1X),10(E13.5,1X))',END=999)n1,n2,n3,x5
                 DTBCOE(id,ich,nphase,1:ncoe_max) = x5(1:ncoe_max)
             endif 
         ENDDO
      ENDDO
    ENDDO
999 CONTINUE
    CLOSE(35)

    Previous_CWAnomaly_Correction = .FALSE.
    TimeCW_init = 0.0
    utw_cor_init = 0.0
    NPeak_init = 0
    MPeak_init = 0
    ucw_cor_init = 0.0
    dcw_init = -999.0
    cw_mean_init = 0.0
    lat_init     = 0.0
    jul_day_init = -999
    Anomaly_Chan_filter_index(1:O2_CHANNELS4) =  -999.0
  END SUBROUTINE ORBIT_INITIALIZATION



  SUBROUTINE FIND_POINT_CLOSE_TO_TRC( np1,np2,Tarm, TRC,NC)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find the closest point
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/12    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4)  :: np1, np2, jp, NC
    REAL(4)      :: Tarm(*), TRC

    ! INITIALIZATION
    NC = np2
    DO jp = np1, np2
      IF (Tarm(jp) >= TRC) THEN
        NC= jp
        EXIT
      ENDIF
    ENDDO

  END SUBROUTINE FIND_POINT_CLOSE_TO_TRC



  SUBROUTINE Gauss_fitting(NL,NC,NR,YL, YC,YR,dcw_G)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To fit curve using gaussian distribution
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/12    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4)  :: NL, NC, NR, ip
    REAL(4)     :: YL, YC,YR,dcw_G(*), xp, SigmaL2, SigmaR2,XC, XL, XR

    XR = NR
    XL = NL
    XC = NC
    SigmaL2 = (XL - XC)*(XL - XC)/ALOG(YC/YL)
    SigmaR2 = (XR - XC)*(XR - XC)/ALOG(YC/YR)
    DO ip = NL, NC
      xp = ip
      dcw_G(ip) = YC*EXP(- (xp - xc)*(xp - xc)/SigmaL2 )
    ENDDO

    DO ip = NC+1, NR
      xp = ip
      dcw_G(ip) = YC*EXP(- (xp - xc)*(xp - xc)/SigmaR2 )
    ENDDO

  END SUBROUTINE Gauss_fitting




  SUBROUTINE Clinear_interpolation(x1,x2,y1,y2,np1,np2,ytt_incre,x,y)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To interpolate points using linear regression
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/12    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    REAL(4), PARAMETER :: mininum_time_interval =  0.1, ZERO = 0.0

    INTEGER(4)  :: ip
    INTEGER(4)  :: np1, np2
    REAL(4), INTENT(IN)       :: x1,x2,y1,y2,x(*),ytt_incre(*)
    REAL(4), INTENT(INOUT)    :: y(*)
    REAL(4)                   :: km, bm,yfit

    IF (abs(x2 - x1) .le. mininum_time_interval) THEN
      DO ip = np1, np2
        IF (ytt_incre(ip) >= ZERO) y(ip) = y1
      ENDDO
    ELSE
      km = (y2-y1) /(x2-x1)
      bm = y2 - km*x2
      DO ip = np1, np2
        yfit = km*x(ip) + bm
        IF (ytt_incre(ip) >= ZERO .and. y(ip) > yfit ) y(ip) = yfit
      ENDDO
    ENDIF

  END SUBROUTINE Clinear_interpolation



  SUBROUTINE linear_interpolation(x1,x2,y1,y2,np1,np2,x,y)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To interpolate points using linear regression
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/12    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    REAL(4), PARAMETER :: mininum_time_interval =  0.1

    INTEGER(4)  :: ip
    INTEGER(4)  :: np1, np2
    REAL(4), INTENT(IN)       :: x1,x2,y1,y2,x(*)
    REAL(4), INTENT(INOUT)    :: y(*)
    REAL(4)                   :: km, bm

    IF (abs(x2 - x1) .le. mininum_time_interval) THEN
      y(np1:np2) = y1
    ELSE
      km = (y2-y1) /(x2-x1)
      bm = y2 - km*x2
      DO ip = np1, np2
         y(ip) = km*x(ip) + bm
      ENDDO
    ENDIF

  END SUBROUTINE linear_interpolation



  SUBROUTINE Point_linear_interpolation(x1,x2,y1,y2,y)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To interpolate points using linear regression
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/12    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4)  :: ip
    INTEGER(4)  :: np1, np2
    REAL(4), INTENT(IN)       :: x1,x2,y1,y2
    REAL(4), INTENT(INOUT)    :: y(*)
    REAL(4)                   :: km, bm

    np1 = INT(x1)
    np2 = INT(x2)

    IF (abs(x2 - x1) .le. 1) THEN
      y(np1:np2) = y1
    ELSE
      km = (y2-y1) /(x2-x1)
      bm = y2 - km*x2
      DO ip = np1, np2
        y(ip) = km*ip + bm
      ENDDO
    ENDIF

  END SUBROUTINE Point_linear_interpolation



  SUBROUTINE get_Tclosest_point(np1,np2,timep,time,npm)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To locate the time mostly close to the input time from a data set
  !
  !  Inputs     :
  !    np1 and np2: a constraint for a time region, time(np1)<= timep <=time(np2)
  !    time     : a data set of time
  !    timep    : the time user needs
  !
  !  Outputs    :
  !    npm       : the position of the point with |timep-time(npm)| = minimum
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    INTEGER(4) :: np1, np2,ip,npm
    REAL(4), INTENT(IN) :: time(*)
    REAL(4), INTENT(IN) :: timep
    REAL(4)             :: dt0

    dt0 = 2.0
    npm = np1
    DO ip = np1, np2
      IF (abs(time(ip)-timep) <= dt0) THEN
        npm = ip
        dt0 = abs(time(ip)-timep)
      ENDIF
    ENDDO

  END SUBROUTINE get_Tclosest_point



  SUBROUTINE get_TSclosest_point(np1,np2,timep,time,npm)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To locate the time mostly close to the input time from a data set
  !
  !  Inputs     :
  !    np1 and np2: a constraint for a time region, time(np1)<= timep <=time(np2)
  !    time     : a data set of time
  !    timep    : the time user needs
  !
  !  Outputs    :
  !    npm       : the position of the point with timep-time(npm) = minimum and time(npm) <= timep
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    REAL(4), PARAMETER :: ZERO = 0.0

    INTEGER(4) :: np1, np2,ip,npm
    REAL(4), INTENT(IN) :: time(*)
    REAL(4), INTENT(IN) :: timep
    REAL(4)             :: dt0

    dt0 = - 20.0
    npm = np1
    DO ip = np1, np2
      IF (time(ip) - timep >= dt0 .AND. time(ip) - timep <= ZERO) THEN
        npm = ip
        dt0 = time(ip) - timep
      ENDIF
    ENDDO

  END SUBROUTINE get_TSclosest_point



  SUBROUTINE get_TLclosest_point(np1,np2,timep,time,npm)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To locate the time mostly close to the input time from a data set
  !
  !  Inputs     :
  !    np1 and np2: a constraint for a time region, time(np1)<= timep <=time(np2)
  !    time     : a data set of time
  !    timep    : the time user needs
  !
  !  Outputs    :
  !    npm       : the position of the point with time(npm) - timep = minimum and time(npm) >= timep
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    REAL(4), PARAMETER :: ZERO = 0.0

    INTEGER(4) :: np1, np2,ip,npm
    REAL(4), INTENT(IN) :: time(*)
    REAL(4), INTENT(IN) :: timep
    REAL(4)             :: dt0

    dt0 = 20.0
    npm = np1
    DO ip = np1, np2
      IF (time(ip) - timep <= dt0 .AND. time(ip) - timep >= ZERO) THEN
        npm = ip
        dt0 = time(ip) - timep
      ENDIF
    ENDDO

  END SUBROUTINE get_TLclosest_point



  SUBROUTINE get_maximum_pixel(np1,np2,yt,npmax)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find the position of the minimum in the given data set
  !
  !  Inputs     :
  !    np1 and np2: a constraint for a detected region
  !    yt       : a data set
  !
  !  Outputs    :
  !    npmax     : the position of the point with yt(x) [x=(np1,np2)] = maximum
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !     2007/03    Banghua Yan      Change allocatable to fixed-space variables
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE
    INTEGER(4),INTENT(IN) :: np1,np2
    INTEGER(4)            :: ip
    REAL(4),INTENT(IN)     :: yt(*)
    INTEGER(4),INTENT(OUT):: npmax
    REAL(4)                :: ymax

    ymax = -200000.0
    DO ip = np1, np2
       if (yt(ip) >= ymax) then
          ymax = yt(ip)
          npmax = ip
       endif 
    ENDDO 

  END SUBROUTINE get_maximum_pixel



  SUBROUTINE get_minimum_pixel(np1,np2,yt,npmin)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find the position of the minimum in the given data set
  !
  !  Inputs     :
  !    np1 and np2: a constraint for a detected region
  !    yt       : a data set
  !
  !  Outputs    :
  !    npmin     : the position of the point with yt(x) [x=(np1,np2)]  = minimum
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2005/06    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    ! Declare variables
    INTEGER(4) :: ip,np1,np2
    REAL(4),INTENT(IN) :: yt(*)
    INTEGER(4),INTENT(OUT):: npmin
    REAL(4)        :: ymin

    ymin = 200000.0
    DO ip = np1, np2
       if (yt(ip) < ymin) then
          ymin = yt(ip)
          npmin = ip
       endif
    ENDDO

  END SUBROUTINE get_minimum_pixel



  SUBROUTINE realdistance(latin1,lonin1,latin2,lonin2,dist)
  !-----------------------------------------------------------------------------------------------------------
  !
  !  Purpose:
  !    To find the distance between two geolocations
  !
  !  Inputs     :
  !    latin1
  !    lonin1
  !    latin2
  !    lonin2
  !
  !  Outputs    :
  !    dist
  !
  !  Record of revisions:
  !     YYYY/MM    Programmer                       Description of change
  !    =========  =============    ===========================================================
  !     2006/12    Banghua Yan      Create programs
  !
  !-----------------------------------------------------------------------------------------------------------
  IMPLICIT NONE

    ! Define variables
    REAL(4) :: latin1,lonin1,latin2,lonin2,lat1,lon1,lat2,lon2
    REAL(4) :: PI,earth_radius,temp,dist

    PI=3.14159
    earth_radius=6378.
    lat1=latin1*PI/180.0
    lon1=lonin1*PI/180.0
    lat2=latin2*PI/180.0
    lon2=lonin2*PI/180.0

    temp=sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon2-lon1)

    if (abs(lat1-lat2) <= 0.001 .and. abs(lon1-lon2) <= 0.001) temp = 0.0
    temp=acos(temp)
    temp=temp*earth_radius

    IF (temp < 0.) temp=9999.
    dist = temp

    RETURN

  END SUBROUTINE realdistance


END MODULE calib_ssmis_sub


