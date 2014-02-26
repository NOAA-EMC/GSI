PROGRAM SSMIS_CTDR_MAIN_LITENDIAN
!========================================================================================================
!
!  Purpose:
!    To read SSMIS TDR raw data in little-endian machine and
!    apply SSMIS TDR calibration subroutine in each scene point
!
!  LANGUAGE:
!    Fortran 90/95
!
!  Dependency:
!    SSMIS_TDR_CALIBRATION_SUB.f90 (v03)
!
!  Steps:
!    1. Read TDR orbit data
!    2. Apply the antenna emission and calibration target anomaly corrections to each observed point
!    3. Rewrite calibrated TDR (CTDR) antenna temperatrue using the same data structure and endian
!       format of TDR files.
!
!  Usage:
!    1. Compile FORTRAN90 program ( Data open format changes over different fortran compiler )
!    2. Create target TDR RAW orbit file location in "tdr_list".
!    3. Run the program
!
!  Question?  Please contact by Fuzhong.Weng@noaa, Banghua.Yan@noaa.gov, Ninghai.Sun@noaa.gov
!
!  Copyright(C) 2006 Fuzhong Weng, Banghua Yan and Ninghai Sun @ NOAA/NESDIS/ORA/SPB
!
!  Record of revisions:
!    ver.   YYYY/MM/DD     Programmer                       Description of change
!   =====  ============   ============      ===========================================================
!   LITTLE-ENDIAN
!    v10    2005/11/17     Ninghai Sun       Little endian compiler (PGI, INTEL) optimization
!                                            (endian convert) for INTEGER more than 1-byte.
!                                            F_UFMTENDIAN (INTEL) doesn't work in 'BINARY' format.
!
!    v12    2005/12/20     Ninghai Sun       Keep Array using ALLOCATE/DEALLOCATE. Structure is fix size
!                                            Ta and Lat/Lon change to REAL(4)
!
!    MAIN_LITEND
!    v00    2006/10/13     Ninghai Sun       Seperate decoding/encoding and calibration parts into
!                                            main program and subroutine
!
!    v01    2006/10/19     Ninghai Sun       Modify little endian to big endian encoding problem
!
!    v02    2006/10/23     Ninghai Sun       Decode ENV_LAT/LON LAS_LAT/LON UAS_LAT/LON for calibration
!
!    v03    2006/11/06     Ninghai Sun       Add SSMIS Ta to Tb conversion and SSMIS to SSMI mapping (
!    (subroutine v00)                        for 5 env + 2 img channels only) subroutines. Set parameter 
!                                            flag in parameter defination part. Set them to 1 to apply 
!                                            Ta to Tb conversion and SSMIS to SSMI (7 channels) mapping.
!
!    v04    2006/12/28     Banghua Yan       Update subroutine calibration algorithm by fixing reflector
!    (subroutine v01)      Ninghai Sun       emissivity. Add look-up table for reflector temperature 
!                                            curve fitting
!
!    v05    2007/01/07     Banghua Yan       Update subroutine to deal with small scan number. Add flags
!    (subroutine v02)      Ninghai Sun       to control the implementation of noise_reduction_algorithm.
!
!    v06    2007/01/17     Banghua Yan       Use DTR_LUT.DAT instead of TD_LUT.DAT in case Tarm is unstable
!    (subroutine v03)      Ninghai Sun
!
!    v07    2007/01/23     Banghau Yan       Update ORBIT_INITIALIZATION arguments and add intent(in/out)
!    (subroutine v04)      Ninghai Sun       in SOLAR_CONTAMINATION_ESTIMATE
!
!    v08    2007/03/01     Banghua Yan       Add scence collocation to LAS resolution
!    (subroutine v06)      Ninghai Sun                 
!   
!    v09    2007/06/15     Banghua Yan       Add the data file for upload the fitting coefficients to estimate
!                          Ninghai Sun       the residual errors from ch. 2 to ch. 7
!    (subroutine v08)
!    v10    2010/04/22     Wanchun Chen      Add memory allocation check for each allocation try      
!========================================================================================================

USE calib_ssmis_sub
USE Consts
USE ErrorHandling

IMPLICIT NONE
!---INTRINSIC functions used
INTRINSIC :: CHAR,EXP,ICHAR,INDEX,INT,MOD,TRIM,ABS,ACOS,COS,SIN,LEN_TRIM

!  Array Parameters
INTEGER(2), PARAMETER :: NSCENES_IMG=180, NSCENES_ENV=90, NSCENES_LAS=60, NSCENES_UAS=30
INTEGER(2), PARAMETER :: NBASE_PTS=28, NCAL_PTS=24
INTEGER(2), PARAMETER :: ctok=27315, MAX_NPOINTS=9000
LOGICAL, PARAMETER    :: TA2TB_FLAG=.FALSE., TA_REMAP_FLAG=.FALSE., Space_Filter_FLAG=.FALSE.
LOGICAL, PARAMETER    :: COREGISTRATION=.FALSE.
!CHARACTER(LEN=256), PARAMETER :: fname_DTR_Table='DTR_LUT.DAT'
!CHARACTER(LEN=256), PARAMETER :: fname_DTBCOE_Table='BIAS_FIT.DAT'

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!
!    The following 8 statements define the structure of the Revolution
!    Header Record, the Scan Header Record, the Ephemeris Record, the
!    Auxilliary Data Record, the Imager Scene Record, the Environmental
!    Scene Record, the LAS Scene Record, and the UAS Scene Record,
!    respectively.  The outputs for the 24 Channels are for every scene
!    (180, 90, 60 or 30) of every scan.  The Ephemeris Record contains
!    3 minute vectors for every scan spanning the Scan Start Time.  The
!    Auxilliary Record contains the Warm & Cold Cals, Warm Loads, 4 MUX
!    values, up to 28 Base Point Lats, Longs, EIAs and Azimuths.
!
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! rev_hdr structure
TYPE :: rev_structure
  INTEGER(4) :: finfo
  INTEGER(4) :: rev_no
  CHARACTER(1),DIMENSION(4) :: year
  CHARACTER(1),DIMENSION(2) :: jul_day
  INTEGER(1) :: hour
  INTEGER(1) :: minute
  INTEGER(2) :: sat_id
  CHARACTER(1),DIMENSION(2) :: num_scans
  INTEGER(4), DIMENSION(5) :: spare
END TYPE rev_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! scan_hdr structure
TYPE :: scan_structure
  CHARACTER(1),DIMENSION(4) :: year
  CHARACTER(1),DIMENSION(2) :: jday
  INTEGER(1) :: hour
  INTEGER(1) :: minute
  INTEGER(2) :: spare1
  INTEGER(2) :: scan_no
  CHARACTER(1),DIMENSION(4) :: st_time
  INTEGER(4), DIMENSION(5) :: spare2
END TYPE scan_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! ephem structure
TYPE :: ephem_structure
  INTEGER(4) :: lat
  INTEGER(4) :: lon
  INTEGER(4) :: alt
  INTEGER(4) :: jd
  INTEGER(4) :: sec
END TYPE ephem_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! IMG structure
TYPE :: imager_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(2) :: scene
  INTEGER(1) :: surf
  INTEGER(1) :: rain
  CHARACTER(1),DIMENSION(2) :: ch8
  CHARACTER(1),DIMENSION(2) :: ch9
  CHARACTER(1),DIMENSION(2) :: ch10
  CHARACTER(1),DIMENSION(2) :: ch11
  CHARACTER(1),DIMENSION(2) :: lat17
  CHARACTER(1),DIMENSION(2) :: lon17
  CHARACTER(1),DIMENSION(2) :: ch17
  CHARACTER(1),DIMENSION(2) :: ch18
END TYPE imager_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! ENV structure
TYPE :: envir_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(1) :: scene
  INTEGER(1) :: surf
  CHARACTER(1),DIMENSION(2) :: ch12
  CHARACTER(1),DIMENSION(2) :: ch13
  CHARACTER(1),DIMENSION(2) :: ch14
  CHARACTER(1),DIMENSION(2) :: lat15
  CHARACTER(1),DIMENSION(2) :: lon15
  CHARACTER(1),DIMENSION(2) :: ch15
  CHARACTER(1),DIMENSION(2) :: ch16
END TYPE envir_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! LAS structure
TYPE :: las_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(2) :: scene
  INTEGER(2) :: surf
  CHARACTER(1),DIMENSION(2) :: ch1
  CHARACTER(1),DIMENSION(2) :: ch2
  CHARACTER(1),DIMENSION(2) :: ch3
  CHARACTER(1),DIMENSION(2) :: ch4
  CHARACTER(1),DIMENSION(2) :: ch5
  CHARACTER(1),DIMENSION(2) :: ch6
  CHARACTER(1),DIMENSION(2) :: ch7
  CHARACTER(1),DIMENSION(2) :: ch24
END TYPE las_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! UAS structure
TYPE :: uas_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(2) :: scene
  CHARACTER(1),DIMENSION(2) :: ch19
  CHARACTER(1),DIMENSION(2) :: ch20
  CHARACTER(1),DIMENSION(2) :: ch21
  CHARACTER(1),DIMENSION(2) :: ch22
  CHARACTER(1),DIMENSION(2) :: ch23
END TYPE uas_structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! aux structure
TYPE :: aux_structure
  CHARACTER(1),DIMENSION(2, NCAL_PTS) :: warm_cal
  CHARACTER(1),DIMENSION(2, NCAL_PTS) :: cold_cal
  CHARACTER(1),DIMENSION(2, 3) :: warm_load
  CHARACTER(1),DIMENSION(2) :: subfr_id
  CHARACTER(1),DIMENSION(2, 4) :: mux_data
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_ka
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_ka
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_ka
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_ka
END TYPE aux_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! Apply new types to strucuture variables
TYPE (rev_structure) :: rev_hdr
TYPE (scan_structure), DIMENSION(MAX_NPOINTS)               :: scan_hdr
TYPE (ephem_structure), DIMENSION(MAX_NPOINTS,3)            :: ephem
TYPE (imager_structure), DIMENSION(MAX_NPOINTS,NSCENES_IMG) :: imager
TYPE (envir_structure), DIMENSION(MAX_NPOINTS,NSCENES_ENV)  :: envir
TYPE (las_structure), DIMENSION(MAX_NPOINTS,NSCENES_LAS)    :: las
TYPE (uas_structure), DIMENSION(MAX_NPOINTS,NSCENES_UAS)    :: uas
TYPE (aux_structure), DIMENSION(MAX_NPOINTS)                :: aux

INTEGER(2) :: nscans, jul_day
INTEGER(1) :: hour, minute
INTEGER(4) :: year
INTEGER(2), ALLOCATABLE, DIMENSION(:) :: scan_day, aux_sfid
REAL(4), ALLOCATABLE, DIMENSION(:)    :: scan_st
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: img_lat, img_lon, img_lat17, img_lon17
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: img_ch8, img_ch9, img_ch10, img_ch11, img_ch17, img_ch18
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: env_lat, env_lon, env_lat15, env_lon15
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: env_ch12, env_ch13, env_ch14, env_ch15, env_ch16
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: las_lat, las_lon
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: las_ch1, las_ch2, las_ch3, las_ch4, las_ch5, las_ch6
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: las_ch7, las_ch24
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: uas_lat, uas_lon
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: uas_ch19, uas_ch20, uas_ch21, uas_ch22, uas_ch23
INTEGER(2), ALLOCATABLE, DIMENSION(:,:) :: aux_wc, aux_cc
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: aux_wl
REAL(4), ALLOCATABLE, DIMENSION(:,:) :: aux_mux

! Define local varables
INTEGER(4) :: icount, ierror, istat, i, j, ndummy, fname_index, fname_length
INTEGER(1), DIMENSION(512) :: dummy
CHARACTER(LEN=250) :: fname_in, fname_out
INTEGER(2) :: img_tmp, env_tmp, las_tmp, uas_tmp, wl_tmp, mux_tmp
REAL(4), DIMENSION(24) :: TA_tmp, TB_tmp

! Subroutine related variables by B. Yan
INTEGER(4),PARAMETER  :: O2_CHANNELS4 = 7
INTEGER(2), PARAMETER :: nday = 24, nlat = 360, npeakm = 6, O2_CHANNELS = 7
INTEGER(2) :: jul_day_init
INTEGER(4) :: nscans_init = DEFAULT_VALUE_INT
LOGICAL    :: Previous_CWAnomaly_Correction
REAL(4), DIMENSION(MAX_NPOINTS) :: TimeCW_init, utw_cor_init,lat_init
INTEGER(4), DIMENSION(npeakm) :: NPeak_init,MPeak_init
REAL(4), DIMENSION(O2_CHANNELS,MAX_NPOINTS) :: ucw_cor_init
REAL(4), DIMENSION(O2_CHANNELS) :: dcw_init
REAL(4), DIMENSION(O2_CHANNELS) :: cw_mean_init
REAL(4), DIMENSION(nday,nlat) :: DTR_YEAR
REAL(4), DIMENSION(O2_CHANNELS4) :: Anomaly_Chan_filter_index

! CO-REGISTER NON-LAS CHANNELS TA TO LAS-LIKE LOCATION TA
INTEGER(4) :: ns, ns1, ns2, np1, np2, jp, ii, jj, ICH
REAL(4) :: lat1, lon1, lat2, lon2, wei, sigma, dist
REAL(4), DIMENSION(8) :: xnum, MTA, TAC_IMG, TAC_ENV, TAC_UAS

!DTB RESIDUAL ERROR 
INTEGER(2), PARAMETER   ::  ndaytab_dtb = 23, ncoe_max_dtb = 5, nphase_max_dtb = 5
REAL(8),    DIMENSION(ndaytab_dtb,O2_CHANNELS4,nphase_max_dtb,ncoe_max_dtb) :: DTBCOE
INTEGER(2), DIMENSION(ndaytab_dtb,O2_CHANNELS4,nphase_max_dtb)              :: ndeg_dtb,nlat_start,nlat_end
INTEGER :: iLoop

 !---- Namelist data
 CHARACTER(LEN=250)                  :: tdr_path_input=DEFAULT_VALUE_STR4
 CHARACTER(LEN=250)                  :: tdr_list_input=DEFAULT_VALUE_STR4
 CHARACTER(LEN=250)                  :: tdr_path_output=DEFAULT_VALUE_STR4
 INTEGER                             :: norbits2process=DEFAULT_VALUE_INT
 CHARACTER(LEN=250)                  :: fname_DTBCOE_Table=DEFAULT_VALUE_STR4
 CHARACTER(LEN=250)                  :: fname_DTR_Table=DEFAULT_VALUE_STR4
 CHARACTER(LEN=16)                   :: accessStr=DEFAULT_VALUE_STR4
 CHARACTER(LEN=16)                   :: formStr=DEFAULT_VALUE_STR4

 NAMELIST /ContrlCalibTDR/tdr_path_input,tdr_list_input,tdr_path_output,norbits2process,&
           fname_DTBCOE_Table,fname_DTR_Table,accessStr,formStr

 READ(*,NML=ContrlCalibTDR)

 ! INITIALIZATION
 CALL ORBIT_INITIALIZATION(O2_CHANNELS, MAX_NPOINTS, npeakm, nday, nlat, fname_dtr_table,fname_DTBCOE_Table, & 
                           nscans_init,Previous_CWAnomaly_Correction,jul_day_init,                           &
                           TimeCW_init, ucw_cor_init, NPeak_init, MPeak_init,                                &
                           dcw_init, utw_cor_init, cw_mean_init, lat_init,DTR_YEAR,Anomaly_Chan_filter_index,&
                           ndeg_dtb,nlat_start,nlat_end,DTBCOE)

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!
!  Open Tdr File
!
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! Count the number of TDR files processed
icount=0

! Open TDR file list
OPEN(20, FILE=tdr_list_input, ACTION='READ', STATUS='OLD', IOSTAT=ierror)
IF (ierror /= 0) THEN
  WRITE(*,*) 'No SSM/IS TDR data file is available for processing ... Stopped.'
  STOP
END IF

! Looping for reading each orbit file
iLoop=0
TDR_LOOP: DO
  READ(20, '(A)', IOSTAT=ierror) fname_in
  IF (ierror .LT. 0) EXIT TDR_LOOP
  iLoop=iLoop+1
  IF (iLoop .gt. norbits2process) EXIT TDR_LOOP

  !OPEN(60, FILE=trim(fname_in), STATUS='OLD', ACTION='READ', FORM='BINARY', IOSTAT=ierror)
  !OPEN(60, FILE=trim(fname_in), STATUS='OLD', ACTION='READ', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=ierror)
  OPEN(60, FILE=trim(fname_in), STATUS='OLD', ACTION='READ', ACCESS=trim(accessStr), FORM=trim(formStr), IOSTAT=ierror)
  
  WRITE(*,*) 'Calibrating file # ', icount+1
  WRITE(*,'(A)') TRIM(fname_in)
  IF (ierror /= 0) THEN
    WRITE(*,*) TRIM(fname_in), ' open error !!! Go to next one. '
    WRITE(*,*) ' '
    CYCLE TDR_LOOP
  END IF

  ! Read in revolution header information
  READ(60, IOSTAT=ierror) rev_hdr

  year = ICHAR(rev_hdr%year(1))*256**3+ICHAR(rev_hdr%year(2))*256**2 &
        +ICHAR(rev_hdr%year(3))*256+ICHAR(rev_hdr%year(4))
  jul_day = ICHAR(rev_hdr%jul_day(1))*256+ICHAR(rev_hdr%jul_day(2))
  hour = rev_hdr%hour
  minute = rev_hdr%minute
  nscans = ICHAR(rev_hdr%num_scans(1))*256+ICHAR(rev_hdr%num_scans(2))


  IF (nscans >= MAX_NPOINTS) THEN
      PRINT *,'THE TDR DATA FILE ARE OUT OF BOUNDARY ABOUT THE DATA SIZE THE CODE DEFINES'
      PRINT *,'GO TO NEXT TDR DATA FILE'
      CYCLE TDR_LOOP
  END IF

  !WRITE(*,*) ' '
  !WRITE(*,*) 'TDR Orbit Information: '
  !WRITE(*,*) '    Year:', year
  !WRITE(*,*) '    Julian Day:    ', jul_day
  !WRITE(*,*) '    Start Hour:    ', hour
  !WRITE(*,*) '    Start Minute:     ', minute
  !WRITE(*,*) '    Number of Scans:  ', nscans
  !WRITE(*,*) ' '
  !WRITE(*,*) 'Reading TDR data for calibration. Please wait ...'

  ! Allocate array using nscans
  ! Scan Header
  ALLOCATE(scan_st(nscans),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(scan_st)')
  ALLOCATE(scan_day(nscans),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(scan_day)')

  ! IMG
  ALLOCATE(img_lat(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_lat)')
  ALLOCATE(img_lon(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_lon)')
  ALLOCATE(img_ch8(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_ch8)')
  ALLOCATE(img_ch9(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_ch9)')
  ALLOCATE(img_ch10(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_ch10)')
  ALLOCATE(img_ch11(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_ch11)')
  ALLOCATE(img_lat17(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_lat17)')
  ALLOCATE(img_lon17(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_lon17)')
  ALLOCATE(img_ch17(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_ch17)')
  ALLOCATE(img_ch18(nscans,NSCENES_IMG),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(img_ch18)')

  ! ENV
  ALLOCATE(env_lat(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_lat)')
  ALLOCATE(env_lon(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_lon)')
  ALLOCATE(env_ch12(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_ch12)')
  ALLOCATE(env_ch13(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_ch13)')
  ALLOCATE(env_ch14(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_ch14)')
  ALLOCATE(env_lat15(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_lat15)')
  ALLOCATE(env_lon15(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_lon15)')
  ALLOCATE(env_ch15(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_ch15)')
  ALLOCATE(env_ch16(nscans,NSCENES_ENV),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(env_ch16)')

  ! LAS
  ALLOCATE(las_lat(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_lat)')
  ALLOCATE(las_lon(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_lon)')
  ALLOCATE(las_ch1(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch1)')
  ALLOCATE(las_ch2(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch2)')
  ALLOCATE(las_ch3(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch3)')
  ALLOCATE(las_ch4(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch4)')
  ALLOCATE(las_ch5(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch5)')
  ALLOCATE(las_ch6(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch6)')
  ALLOCATE(las_ch7(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch7)')
  ALLOCATE(las_ch24(nscans,NSCENES_LAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(las_ch24)')

  ! UAS
  ALLOCATE(uas_lat(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_lat)')
  ALLOCATE(uas_lon(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_lon)')
  ALLOCATE(uas_ch19(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_ch19)')
  ALLOCATE(uas_ch20(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_ch20)')
  ALLOCATE(uas_ch21(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_ch21)')
  ALLOCATE(uas_ch22(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_ch22)')
  ALLOCATE(uas_ch23(nscans,NSCENES_UAS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(uas_ch23)')

  ! AUX
  ALLOCATE(aux_wc(nscans,NCAL_PTS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(aux_wc)')
  ALLOCATE(aux_cc(nscans,NCAL_PTS),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(aux_cc)')
  ALLOCATE(aux_wl(nscans,3),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(aux_wl)')
  ALLOCATE(aux_mux(nscans,4),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(aux_mux)')
  ALLOCATE(aux_sfid(nscans),STAT=istat)
  if ( istat /= 0 ) CALL ErrHandl(ErrorType,Err_AllocMemPb,': calib_f16_ssmis: ALLOCATE(aux_sfid)')

  ! Loop over each scan
  NSCAN_IN:DO i=1, nscans
    ! Read scan header information
    READ(60,IOSTAT=ierror) scan_hdr(i)

    scan_day(i) = ICHAR(scan_hdr(i)%jday(1))*256+ICHAR(scan_hdr(i)%jday(2))
    scan_st(i) = (ICHAR(scan_hdr(i)%st_time(1))*256**3+ICHAR(scan_hdr(i)%st_time(2))*256**2 &
                 +ICHAR(scan_hdr(i)%st_time(3))*256+ICHAR(scan_hdr(i)%st_time(4)))*0.001

    ! Read EPHEM data block
    DO j=1,3
      READ(60,IOSTAT=ierror) ephem(i,j)
    END DO

    ! Loop over IMG scenes
    SCENE_IMG_IN:DO j=1, NSCENES_IMG
      ! Read IMG data block
      READ(60,IOSTAT=ierror) imager(i,j)

      img_tmp = ICHAR(imager(i,j)%lat(1))*256+ICHAR(imager(i,j)%lat(2))
      img_lat(i,j) = img_tmp*0.01
      img_tmp = ICHAR(imager(i,j)%lon(1))*256+ICHAR(imager(i,j)%lon(2))
      img_lon(i,j) = img_tmp*0.01
      img_tmp = ICHAR(imager(i,j)%ch8(1))*256+ICHAR(imager(i,j)%ch8(2))
      img_ch8(i,j) = (img_tmp + ctok)*0.01
      img_tmp = ICHAR(imager(i,j)%ch9(1))*256+ICHAR(imager(i,j)%ch9(2))
      img_ch9(i,j) = (img_tmp + ctok)*0.01
      img_tmp = ICHAR(imager(i,j)%ch10(1))*256+ICHAR(imager(i,j)%ch10(2))
      img_ch10(i,j) = (img_tmp + ctok)*0.01
      img_tmp = ICHAR(imager(i,j)%ch11(1))*256+ICHAR(imager(i,j)%ch11(2))
      img_ch11(i,j) = (img_tmp + ctok)*0.01
      img_tmp = ICHAR(imager(i,j)%lat17(1))*256+ICHAR(imager(i,j)%lat17(2))
      img_lat17(i,j) = img_tmp*0.01
      img_tmp = ICHAR(imager(i,j)%lon17(1))*256+ICHAR(imager(i,j)%lon17(2))
      img_lon17(i,j) = img_tmp*0.01
      img_tmp = ICHAR(imager(i,j)%ch17(1))*256+ICHAR(imager(i,j)%ch17(2))
      img_ch17(i,j) = (img_tmp + ctok)*0.01
      img_tmp = ICHAR(imager(i,j)%ch18(1))*256+ICHAR(imager(i,j)%ch18(2))
      img_ch18(i,j) = (img_tmp + ctok)*0.01

    END DO SCENE_IMG_IN

    ! Loop over ENV scenes
    SCENE_ENV_IN:DO j=1, NSCENES_ENV
      ! Read ENV data block
      READ(60,IOSTAT=ierror) envir(i,j)

      env_tmp = ICHAR(envir(i,j)%lat(1))*256+ICHAR(envir(i,j)%lat(2))
      env_lat(i,j) = env_tmp*0.01
      env_tmp = ICHAR(envir(i,j)%lon(1))*256+ICHAR(envir(i,j)%lon(2))
      env_lon(i,j) = env_tmp*0.01
      env_tmp = ICHAR(envir(i,j)%ch12(1))*256+ICHAR(envir(i,j)%ch12(2))
      env_ch12(i,j) = (env_tmp + ctok)*0.01
      env_tmp = ICHAR(envir(i,j)%ch13(1))*256+ICHAR(envir(i,j)%ch13(2))
      env_ch13(i,j) = (env_tmp + ctok)*0.01
      env_tmp = ICHAR(envir(i,j)%ch14(1))*256+ICHAR(envir(i,j)%ch14(2))
      env_ch14(i,j) = (env_tmp + ctok)*0.01
      env_tmp = ICHAR(envir(i,j)%lat15(1))*256+ICHAR(envir(i,j)%lat15(2))
      env_lat15(i,j) = env_tmp*0.01
      env_tmp = ICHAR(envir(i,j)%lon15(1))*256+ICHAR(envir(i,j)%lon15(2))
      env_lon15(i,j) = env_tmp*0.01
      env_tmp = ICHAR(envir(i,j)%ch15(1))*256+ICHAR(envir(i,j)%ch15(2))
      env_ch15(i,j) = (env_tmp + ctok)*0.01
      env_tmp = ICHAR(envir(i,j)%ch16(1))*256+ICHAR(envir(i,j)%ch16(2))
      env_ch16(i,j) = (env_tmp + ctok)*0.01

    END DO SCENE_ENV_IN

    ! Loop over LAS scenes
    SCENE_LAS_IN:DO j=1, NSCENES_LAS
      ! Read LAS data block
      READ(60,IOSTAT=ierror) las(i,j)

      las_tmp = ICHAR(las(i,j)%lat(1))*256+ICHAR(las(i,j)%lat(2))
      las_lat(i,j) = las_tmp*0.01
      las_tmp = ICHAR(las(i,j)%lon(1))*256+ICHAR(las(i,j)%lon(2))
      las_lon(i,j) = las_tmp*0.01
      las_tmp = ICHAR(las(i,j)%ch1(1))*256+ICHAR(las(i,j)%ch1(2))
      las_ch1(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch2(1))*256+ICHAR(las(i,j)%ch2(2))
      las_ch2(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch3(1))*256+ICHAR(las(i,j)%ch3(2))
      las_ch3(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch4(1))*256+ICHAR(las(i,j)%ch4(2))
      las_ch4(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch5(1))*256+ICHAR(las(i,j)%ch5(2))
      las_ch5(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch6(1))*256+ICHAR(las(i,j)%ch6(2))
      las_ch6(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch7(1))*256+ICHAR(las(i,j)%ch7(2))
      las_ch7(i,j) = (las_tmp + ctok)*0.01
      las_tmp = ICHAR(las(i,j)%ch24(1))*256+ICHAR(las(i,j)%ch24(2))
      las_ch24(i,j) = (las_tmp + ctok)*0.01

    END DO SCENE_LAS_IN

    ! Loop over UAS scenes
    SCENE_UAS_IN:DO j=1, NSCENES_UAS
      ! Read UAS data block
      READ(60,IOSTAT=ierror) uas(i,j)

      uas_tmp = ICHAR(uas(i,j)%lat(1))*256+ICHAR(uas(i,j)%lat(2))
      uas_lat(i,j) = uas_tmp*0.01
      uas_tmp = ICHAR(uas(i,j)%lon(1))*256+ICHAR(uas(i,j)%lon(2))
      uas_lon(i,j) = uas_tmp*0.01
      uas_tmp = ICHAR(uas(i,j)%ch19(1))*256+ICHAR(uas(i,j)%ch19(2))
      uas_ch19(i,j) = (uas_tmp + ctok)*0.01
      uas_tmp = ICHAR(uas(i,j)%ch20(1))*256+ICHAR(uas(i,j)%ch20(2))
      uas_ch20(i,j) = (uas_tmp + ctok)*0.01
      uas_tmp = ICHAR(uas(i,j)%ch21(1))*256+ICHAR(uas(i,j)%ch21(2))
      uas_ch21(i,j) = (uas_tmp + ctok)*0.01
      uas_tmp = ICHAR(uas(i,j)%ch22(1))*256+ICHAR(uas(i,j)%ch22(2))
      uas_ch22(i,j) = (uas_tmp + ctok)*0.01
      uas_tmp = ICHAR(uas(i,j)%ch23(1))*256+ICHAR(uas(i,j)%ch23(2))
      uas_ch23(i,j) = (uas_tmp + ctok)*0.01

    END DO SCENE_UAS_IN

    ! Read AUX data block
    READ(60,IOSTAT=ierror) aux(i)

    aux_sfid(i) = ICHAR(aux(i)%subfr_id(1))*256+ICHAR(aux(i)%subfr_id(2))

    DO j=1, NCAL_PTS
      aux_wc(i,j) = ICHAR(aux(i)%warm_cal(1,j))*256+ICHAR(aux(i)%warm_cal(2,j))
      aux_cc(i,j) = ICHAR(aux(i)%cold_cal(1,j))*256+ICHAR(aux(i)%cold_cal(2,j))
    END DO
    DO j=1, 3
      wl_tmp = ICHAR(aux(i)%warm_load(1,j))*256+ICHAR(aux(i)%warm_load(2,j))
      aux_wl(i,j) = (wl_tmp + ctok)*0.01
    END DO
    DO j=1, 4
      mux_tmp=ICHAR(aux(i)%mux_data(1,j))*256+ICHAR(aux(i)%mux_data(2,j))
      aux_mux(i,j) = (mux_tmp + ctok)*0.01
    END DO

  END DO NSCAN_IN

  ! Read dummy bytes after all scan data if there is any.
  ndummy=0
  DUMMY_IN: DO i=1, 512
    READ(60, IOSTAT=ierror) dummy(i)
    IF ( ierror < 0 ) EXIT DUMMY_IN
    ndummy = ndummy + 1
  END DO DUMMY_IN

  CLOSE(60)

  !WRITE(*,*) '... Reading Done !'
  !WRITE(*,*) ' '

  !WRITE(*,*) 'Start SSMIS calibration. Please wait ...'
  
  !Call calibration subroutine
  CALL SSMIS_RADIANCE_RECALIBRATION(scan_day, jul_day, nscans,                                    &
           scan_st, aux_wc, aux_cc, aux_wl, aux_sfid, aux_mux,                                    &
           img_lat, img_lon, env_lat, env_lon, las_lat,las_lon, uas_lat, uas_lon,                 &
           img_ch8, img_ch9, img_ch10, img_ch11, img_ch17, img_ch18,                              &
           env_ch12, env_ch13, env_ch14, env_ch15, env_ch16,                                      &
           las_ch1, las_ch2, las_ch3, las_ch4, las_ch5, las_ch6, las_ch7, las_ch24,               &
           uas_ch19, uas_ch20, uas_ch21, uas_ch22, uas_ch23,                                      &
           Previous_CWAnomaly_Correction,jul_day_init,nscans_init,                                &
           TimeCW_init,ucw_cor_init,dcw_init, utw_cor_init,cw_mean_init,lat_init,                 &
           NPeak_init,MPeak_init,nday,nlat,DTR_YEAR,Space_Filter_FLAG,Anomaly_Chan_filter_index,  &
           ndeg_dtb,nlat_start,nlat_end,DTBCOE)

  !WRITE(*,*) '... Calibration Done !'
  !WRITE(*,*) ' '

  ! FOR EMS'S DATA ASSIMILTATION REQUIREMENT
  IF (COREGISTRATION) THEN
    WRITE(*,*) 'Star EMC Data Collocation to LAS scene ...'
    
    NSCAN_COREG: DO ns=1, nscans
      ns1 = ns - 5
      IF ( ns1 < 1 ) ns1 = 1
      ns2 = ns + 5
      IF ( ns2 > nscans ) ns2 = nscans
 
      PIXEL_COREG: DO jp = 1, NSCENES_LAS
        !INITIALIZATION
        sigma = 25.0
        TA_tmp(1:24) = -999.0
        lat1 = las_lat(ns,jp)
        lon1 = las_lon(ns,jp)

        np1 = jp - 5
        IF ( np1 < 1 ) np1 = 1
        np2 = jp + 5
        IF ( np2 > NSCENES_LAS ) np2 = NSCENES_LAS

        ! (1) REMAP IMG CHANNELS TO LAS CHANNEL LOCATION
        xnum(1:6) = 0.
        MTA(1:6) = 0.0
        IMG_CHANNEL: DO ICH = 1, 6
          SCAN_SUBREG_IMG: DO ii = ns1, ns2
            PIXEL_SUBREG_IMG: DO jj = np1, np2
              IF (ICH <= 4) THEN
                lat2 = img_lat(ii,INT(3.*jj))
                lon2 = img_lon(ii,INT(3.*jj))
              ELSE
                lat2 = img_lat17(ii,INT(3.*jj))
                lon2 = img_lon17(ii,INT(3.*jj))
              ENDIF
              IF ( ICH ==1 ) TAC_IMG(1) = img_ch8(ii,INT(3.0*jj))
              IF ( ICH ==2 ) TAC_IMG(2) = img_ch9(ii,INT(3.0*jj))
              IF ( ICH ==3 ) TAC_IMG(3) = img_ch10(ii,INT(3.0*jj))
              IF ( ICH ==4 ) TAC_IMG(4) = img_ch11(ii,INT(3.0*jj))
              IF ( ICH ==5 ) TAC_IMG(5) = img_ch17(ii,INT(3.0*jj))
              IF ( ICH ==6 ) TAC_IMG(6) = img_ch18(ii,INT(3.0*jj))
              IF ( TAC_IMG(ICH) <= 30.0)  CYCLE PIXEL_SUBREG_IMG
      
              ! Calculate weighted value
              CALL realdistance(lat1,lon1,lat2,lon2,dist)
              IF ( dist >= 200.0 ) CYCLE PIXEL_SUBREG_IMG
              wei = EXP(-0.5*(dist/sigma)*(dist/sigma))
              xnum(ich) = xnum(ich) + wei
              MTA(ich) = MTA(ich) + wei*TAC_IMG(ich)      
            ENDDO PIXEL_SUBREG_IMG
          ENDDO SCAN_SUBREG_IMG
        ENDDO IMG_CHANNEL
      
        IF (MTA(1) >= 50.0) THEN
          DO ICH = 1, 6
            MTA(ich) = MTA(ich)/xnum(ich)
          ENDDO
          !SAVE DATA
          TA_tmp(8:11)   = MTA(1:4)
          TA_tmp(17:18)  = MTA(5:6)
        ELSE
          TA_tmp(8) = img_ch8(ns,INT(3.0*jp))
          TA_tmp(9) = img_ch9(ns,INT(3.0*jp))
          TA_tmp(10) = img_ch10(ns,INT(3.0*jp))
          TA_tmp(11) = img_ch11(ns,INT(3.0*jp))
          TA_tmp(17) = img_ch17(ns,INT(3.0*jp))
          TA_tmp(18) = img_ch18(ns,INT(3.0*jp))
        ENDIF
      
        ! (2) REMAP ENV CHANNELS TO LAS CHANNEL LOCATION
        xnum(1:5) = 0.
        MTA(1:5) = 0.0
        ENV_CHANNEL: DO ICH = 1, 5
          SCAN_SUBREG_ENV: DO ii = ns1, ns2
            PIXEL_SUBREG_ENV: DO jj = np1, np2
              IF ( ICH <= 3) THEN
                lat2 = env_lat(ii,INT(1.5*jj))
                lon2 = env_lon(ii,INT(1.5*jj))
              ELSE
                lat2 = env_lat15(ii,INT(1.5*jj))
                lon2 = env_lon15(ii,INT(1.5*jj))
              ENDIF
              IF ( ICH == 1 ) TAC_ENV(1) = env_ch12(ii,INT(1.5*jj))
              IF ( ICH == 2 ) TAC_ENV(2) = env_ch13(ii,INT(1.5*jj))
              IF ( ICH == 3 ) TAC_ENV(3) = env_ch14(ii,INT(1.5*jj))
              IF ( ICH == 4 ) TAC_ENV(4) = env_ch15(ii,INT(1.5*jj))
              IF ( ICH == 5 ) TAC_ENV(5) = env_ch16(ii,INT(1.5*jj))

              IF ( TAC_ENV(ich) <= 30.0 ) CYCLE PIXEL_SUBREG_ENV
              
              ! Calculate weighted value
              CALL realdistance(lat1,lon1,lat2,lon2,dist)
              IF ( dist >= 200.0 ) CYCLE PIXEL_SUBREG_ENV
              wei = EXP(-0.5*(dist/sigma)*(dist/sigma))
              xnum(ich) = xnum(ich) + wei
              MTA(ich) = MTA(ich) + wei*TAC_ENV(ich)
            ENDDO PIXEL_SUBREG_ENV
          ENDDO SCAN_SUBREG_ENV
        ENDDO ENV_CHANNEL
      
        IF (MTA(1) >= 50.0) THEN
          DO ICH = 1, 5
            MTA(ich) = MTA(ich)/xnum(ich)
          ENDDO
          TA_tmp(12:16) = MTA(1:5)
        ELSE
         TA_tmp(12) = env_ch12(ns,INT(1.5*jp))
         TA_tmp(13) = env_ch13(ns,INT(1.5*jp))
         TA_tmp(14) = env_ch14(ns,INT(1.5*jp))
         TA_tmp(15) = env_ch15(ns,INT(1.5*jp))
         TA_tmp(16) = env_ch16(ns,INT(1.5*jp))
        ENDIF

        ! (3) REMAP UAS CHANNELS TO LAS CHANNEL LOCATION
        xnum(1:5) = 0.
        MTA(1:5) = 0.0
        sigma = 75.0
        UAS_CHANNEL: DO ICH = 1, 5
          SCAN_SUBREG_UAS: DO ii = ns1, ns2
            PIXEL_SUBREG_UAS: DO jj = np1, np2
              IF ( INT(jj/2.) < 1 ) THEN
                lat2 = uas_lat(ii,1)
                lon2 = uas_lon(ii,1)
                IF ( ICH == 1 ) TAC_UAS(1) = uas_ch19(ii,1)
                IF ( ICH == 2 ) TAC_UAS(2) = uas_ch20(ii,1)
                IF ( ICH == 3 ) TAC_UAS(3) = uas_ch21(ii,1)
                IF ( ICH == 4 ) TAC_UAS(4) = uas_ch22(ii,1)
                IF ( ICH == 5 ) TAC_UAS(5) = uas_ch23(ii,1)
              ELSE
                lat2 = uas_lat(ii,INT(jj/2.0))
                lon2 = uas_lon(ii,INT(jj/2.0))
                IF (ICH == 1) TAC_UAS(1) = uas_ch19(ii,INT(jj/2.0))
                IF (ICH == 2) TAC_UAS(2) = uas_ch20(ii,INT(jj/2.0))
                IF (ICH == 3) TAC_UAS(3) = uas_ch21(ii,INT(jj/2.0))
                IF (ICH == 4) TAC_UAS(4) = uas_ch22(ii,INT(jj/2.0))
                IF (ICH == 5) TAC_UAS(5) = uas_ch23(ii,INT(jj/2.0))
              ENDIF
              IF (TAC_UAS(ich) <= 30.0) CYCLE PIXEL_SUBREG_UAS
      
              ! Calculate weighted value
              CALL realdistance(lat1,lon1,lat2,lon2,dist)
              IF ( dist >= 400.0 ) CYCLE PIXEL_SUBREG_UAS
              wei = EXP(-0.5*(dist/sigma)*(dist/sigma))
              xnum(ich) = xnum(ich) + wei
              MTA(ich) = MTA(ich) + wei*TAC_UAS(ich)
            ENDDO PIXEL_SUBREG_UAS
          ENDDO SCAN_SUBREG_UAS
        ENDDO UAS_CHANNEL
        
        IF (MTA(1) >= 50.0) THEN
          DO ICH = 1, 5
            MTA(ich) = MTA(ich)/xnum(ich)
          ENDDO
          TA_tmp(19:23) = MTA(1:5)
        ELSE   ! IN CASE OF NO AVALIABLE DATA TO BE APPLIED TO UAS CHANNELS
          IF( INT(jp/2.0) <1) THEN
             TA_tmp(19) = uas_ch19(ns,1)
             TA_tmp(20) = uas_ch20(ns,1)
             TA_tmp(21) = uas_ch21(ns,1)
             TA_tmp(22) = uas_ch22(ns,1)
             TA_tmp(23) = uas_ch23(ns,1)
          ELSE
             TA_tmp(19) = uas_ch19(ns,INT(jp/2.0))
             TA_tmp(20) = uas_ch20(ns,INT(jp/2.0))
             TA_tmp(21) = uas_ch21(ns,INT(jp/2.0))
             TA_tmp(22) = uas_ch22(ns,INT(jp/2.0))
             TA_tmp(23) = uas_ch23(ns,INT(jp/2.0))
          ENDIF
        ENDIF
 
        !LAS CHANNELS
        TA_tmp(1) = las_ch1(ns,jp)
        TA_tmp(2) = las_ch2(ns,jp)
        TA_tmp(3) = las_ch3(ns,jp)
        TA_tmp(4) = las_ch4(ns,jp)
        TA_tmp(5) = las_ch5(ns,jp)
        TA_tmp(6) = las_ch6(ns,jp)
        TA_tmp(7) = las_ch7(ns,jp)
        TA_tmp(24) = las_ch24(ns,jp)       
        
        ! TA2TB Conversion
        CALL SSMIS_TA2TB(TA_tmp, TB_tmp)
      
      ENDDO PIXEL_COREG
    ENDDO NSCAN_COREG
  
    CLOSE(23)
    CLOSE(24)

    !WRITE(*,*) '... Collocation Done!'
  
  ENDIF

  ! Write out calibrated TDR data
  fname_length=LEN_TRIM(fname_in)
  fname_index=INDEX(fname_in,'NPR.TDRN',.TRUE.)
  !fname_out=TRIM(tdr_path_output)//TRIM(fname_in(fname_index:fname_index+42))//'.CALIB'  
  fname_out=TRIM(tdr_path_output)//TRIM(fname_in(fname_index:fname_length))//'.CALIB'  
  
  !WRITE(*,*) 'Calibrated TDR='//fname_out

  ! Reassign calibrated Ta to the structure
  !OPEN(65, FILE=fname_out, STATUS='REPLACE', ACTION='WRITE', FORM='BINARY', IOSTAT=ierror)
  !OPEN(65, FILE=fname_out, STATUS='REPLACE', ACTION='WRITE', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=ierror)
  OPEN(65, FILE=fname_out, STATUS='REPLACE', ACTION='WRITE', ACCESS=trim(accessStr), FORM=trim(formStr), IOSTAT=ierror)
  IF (ierror /= 0) THEN
    WRITE(*,*) fname_out, ' open error. Check the directory permission and space. '
    WRITE(*,*) '... STOPPED !!!'
    WRITE(*,*) ' '
    STOP
  END IF

  ! Write revolution header
  WRITE(65,IOSTAT=ierror) rev_hdr

  ! Loop over each scan
  NSCAN_OUT: DO i=1, nscans
    ! Write scan header information
    WRITE(65,IOSTAT=ierror) scan_hdr(i)

    ! Write EPHEM data block
    DO j = 1,3
      WRITE(65,IOSTAT=ierror) ephem(i,j)
    END DO

    ! Loop over IMG scenes
    SCENE_IMG_OUT: DO j=1, NSCENES_IMG
      
      ! SSMIS Ta remap to SSMI Ta ( for 2 91GHz imaging channels only )
      IF ( TA_REMAP_FLAG ) THEN
        TA_tmp(17) = img_ch17(i,j)
        TA_tmp(18) = img_ch18(i,j)

        CALL SSMIS_TA_REMAP(TA_tmp,TB_tmp)
        
        img_ch17(i,j) = TB_tmp(17)
        img_ch18(i,j) = TB_tmp(18)

        TA_tmp = 0.0
        TB_tmp = 0.0
      ENDIF

      ! SSMIS TA to TB correction
      IF ( TA2TB_FLAG ) THEN
        TA_tmp(8) = img_ch8(i,j)
        TA_tmp(9) = img_ch9(i,j)
        TA_tmp(10) = img_ch10(i,j)
        TA_tmp(11) = img_ch11(i,j)
        TA_tmp(17) = img_ch17(i,j)
        TA_tmp(18) = img_ch18(i,j)
      
        CALL SSMIS_TA2TB(TA_tmp,TB_tmp)
        
        img_ch8(i,j) = TB_tmp(8)
        img_ch9(i,j) = TB_tmp(9)
        img_ch10(i,j) = TB_tmp(10)
        img_ch11(i,j) = TB_tmp(11)
        img_ch17(i,j) = TB_tmp(17)
        img_ch18(i,j) = TB_tmp(18)

        TA_tmp = 0.0
        TB_tmp = 0.0
      ENDIF

      ! CH8
      img_ch8(i,j)=img_ch8(i,j)*100.0-ctok
      IF ( img_ch8(i,j) <= 0.0 ) THEN
        imager(i,j)%ch8(1)=CHAR(INT(img_ch8(i,j))/256-1)
        imager(i,j)%ch8(2)=CHAR(MOD(INT(img_ch8(i,j)),256)-1)
      ELSE
        imager(i,j)%ch8(1)=CHAR(INT(img_ch8(i,j))/256)
        imager(i,j)%ch8(2)=CHAR(MOD(INT(img_ch8(i,j)),256))
      ENDIF

      ! CH9
      img_ch9(i,j)=img_ch9(i,j)*100.0-ctok
      IF ( img_ch9(i,j) <= 0.0 ) THEN
        imager(i,j)%ch9(1)=CHAR(INT(img_ch9(i,j))/256-1)
        imager(i,j)%ch9(2)=CHAR(MOD(INT(img_ch9(i,j)),256)-1)
      ELSE
        imager(i,j)%ch9(1)=CHAR(INT(img_ch9(i,j))/256)
        imager(i,j)%ch9(2)=CHAR(MOD(INT(img_ch9(i,j)),256))
      ENDIF

      ! CH10
      img_ch10(i,j)=img_ch10(i,j)*100.0-ctok
      IF ( img_ch10(i,j) <= 0.0 ) THEN
        imager(i,j)%ch10(1)=CHAR(INT(img_ch10(i,j))/256-1)
        imager(i,j)%ch10(2)=CHAR(MOD(INT(img_ch10(i,j)),256)-1)
      ELSE
        imager(i,j)%ch10(1)=CHAR(INT(img_ch10(i,j))/256)
        imager(i,j)%ch10(2)=CHAR(MOD(INT(img_ch10(i,j)),256))
      ENDIF

      ! CH11
      img_ch11(i,j)=img_ch11(i,j)*100.0-ctok
      IF ( img_ch11(i,j) <= 0.0 ) THEN
        imager(i,j)%ch11(1)=CHAR(INT(img_ch11(i,j))/256-1)
        imager(i,j)%ch11(2)=CHAR(MOD(INT(img_ch11(i,j)),256)-1)
      ELSE
        imager(i,j)%ch11(1)=CHAR(INT(img_ch11(i,j))/256)
        imager(i,j)%ch11(2)=CHAR(MOD(INT(img_ch11(i,j)),256))
      ENDIF

      ! CH17
      img_ch17(i,j)=img_ch17(i,j)*100.0-ctok
      IF ( img_ch17(i,j) <= 0.0 ) THEN
        imager(i,j)%ch17(1)=CHAR(INT(img_ch17(i,j))/256-1)
        imager(i,j)%ch17(2)=CHAR(MOD(INT(img_ch17(i,j)),256)-1)
      ELSE
        imager(i,j)%ch17(1)=CHAR(INT(img_ch17(i,j))/256)
        imager(i,j)%ch17(2)=CHAR(MOD(INT(img_ch17(i,j)),256))
      ENDIF

      ! CH18
      img_ch18(i,j)=img_ch18(i,j)*100.0-ctok
      IF ( img_ch18(i,j) <= 0.0 ) THEN
        imager(i,j)%ch18(1)=CHAR(INT(img_ch18(i,j))/256-1)
        imager(i,j)%ch18(2)=CHAR(MOD(INT(img_ch18(i,j)),256)-1)
      ELSE
        imager(i,j)%ch18(1)=CHAR(INT(img_ch18(i,j))/256)
        imager(i,j)%ch18(2)=CHAR(MOD(INT(img_ch18(i,j)),256))
      ENDIF

      ! Write IMG data block
      WRITE(65,IOSTAT=ierror) imager(i,j)

    END DO SCENE_IMG_OUT

    ! Loop over ENV scenes
    SCENE_ENV_OUT:DO j=1, NSCENES_ENV
      
      ! SSMIS TA remap to SSMI TA ( all 5 env channels )
      IF ( TA_REMAP_FLAG ) THEN
        TA_tmp(12) = env_ch12(i,j)
        TA_tmp(13) = env_ch13(i,j)
        TA_tmp(14) = env_ch14(i,j)
        TA_tmp(15) = env_ch15(i,j)
        TA_tmp(16) = env_ch16(i,j)
        
        CALL SSMIS_TA_REMAP(TA_tmp, TB_tmp)
        
        env_ch12(i,j) = TB_tmp(12)
        env_ch13(i,j) = TB_tmp(13)
        env_ch14(i,j) = TB_tmp(14)
        env_ch15(i,j) = TB_tmp(15)
        env_ch16(i,j) = TB_tmp(16)
        
        TA_tmp = 0.0
        TB_tmp = 0.0
      ENDIF
      
      ! SSMIS TA to TB correction
      IF ( TA2TB_FLAG ) THEN
        TA_tmp(12) = env_ch12(i,j)
        TA_tmp(13) = env_ch13(i,j)
        TA_tmp(14) = env_ch14(i,j)
        TA_tmp(15) = env_ch15(i,j)
        TA_tmp(16) = env_ch16(i,j)
        
        CALL SSMIS_TA2TB(TA_tmp, TB_tmp)
        
        env_ch12(i,j) = TB_tmp(12)
        env_ch13(i,j) = TB_tmp(13)
        env_ch14(i,j) = TB_tmp(14)
        env_ch15(i,j) = TB_tmp(15)
        env_ch16(i,j) = TB_tmp(16)
        
        TA_tmp = 0.0
        TB_tmp = 0.0
      ENDIF
      
      ! CH12
      env_ch12(i,j)=env_ch12(i,j)*100.0-ctok
      IF ( env_ch12(i,j) <= 0.0 ) THEN
        envir(i,j)%ch12(1)=CHAR(INT(env_ch12(i,j))/256-1)
        envir(i,j)%ch12(2)=CHAR(MOD(INT(env_ch12(i,j)),256)-1)
      ELSE
        envir(i,j)%ch12(1)=CHAR(INT(env_ch12(i,j))/256)
        envir(i,j)%ch12(2)=CHAR(MOD(INT(env_ch12(i,j)),256))
      ENDIF

      ! CH13
      env_ch13(i,j)=env_ch13(i,j)*100.0-ctok
      IF ( env_ch13(i,j) <= 0.0 ) THEN
        envir(i,j)%ch13(1)=CHAR(INT(env_ch13(i,j))/256-1)
        envir(i,j)%ch13(2)=CHAR(MOD(INT(env_ch13(i,j)),256)-1)
      ELSE
        envir(i,j)%ch13(1)=CHAR(INT(env_ch13(i,j))/256)
        envir(i,j)%ch13(2)=CHAR(MOD(INT(env_ch13(i,j)),256))
      ENDIF

      ! CH14
      env_ch14(i,j)=env_ch14(i,j)*100.0-ctok
      IF ( env_ch14(i,j) <= 0.0 ) THEN
        envir(i,j)%ch14(1)=CHAR(INT(env_ch14(i,j))/256-1)
        envir(i,j)%ch14(2)=CHAR(MOD(INT(env_ch14(i,j)),256)-1)
      ELSE
        envir(i,j)%ch14(1)=CHAR(INT(env_ch14(i,j))/256)
        envir(i,j)%ch14(2)=CHAR(MOD(INT(env_ch14(i,j)),256))
      ENDIF

      ! CH15
      env_ch15(i,j)=env_ch15(i,j)*100.0-ctok
      IF ( env_ch15(i,j) <= 0.0 ) THEN
        envir(i,j)%ch15(1)=CHAR(INT(env_ch15(i,j))/256-1)
        envir(i,j)%ch15(2)=CHAR(MOD(INT(env_ch15(i,j)),256)-1)
      ELSE
        envir(i,j)%ch15(1)=CHAR(INT(env_ch15(i,j))/256)
        envir(i,j)%ch15(2)=CHAR(MOD(INT(env_ch15(i,j)),256))
      ENDIF

      ! CH16
      env_ch16(i,j)=env_ch16(i,j)*100.0-ctok
      IF ( env_ch16(i,j) <= 0.0 ) THEN
        envir(i,j)%ch16(1)=CHAR(INT(env_ch16(i,j))/256-1)
        envir(i,j)%ch16(2)=CHAR(MOD(INT(env_ch16(i,j)),256)-1)
      ELSE
        envir(i,j)%ch16(1)=CHAR(INT(env_ch16(i,j))/256)
        envir(i,j)%ch16(2)=CHAR(MOD(INT(env_ch16(i,j)),256))
      ENDIF

      ! Write ENV data block
      WRITE(65,IOSTAT=ierror) envir(i,j)
    END DO SCENE_ENV_OUT

    ! Loop over LAS scenes
    SCENE_LAS_OUT:DO j=1, NSCENES_LAS

      ! SSMIS TA to TB correction
      IF ( TA2TB_FLAG ) THEN
        TA_tmp(1) = las_ch1(i,j)
        TA_tmp(2) = las_ch2(i,j)
        TA_tmp(3) = las_ch3(i,j)
        TA_tmp(4) = las_ch4(i,j)
        TA_tmp(5) = las_ch5(i,j)
        TA_tmp(6) = las_ch6(i,j)
        TA_tmp(7) = las_ch7(i,j)
        TA_tmp(24) = las_ch24(i,j)
        
        CALL SSMIS_TA2TB(TA_tmp, TB_tmp)
      
        ! TEST
        !  IF (las_lat(i,j) == 67.57 .AND. las_lon(i,j) == 9.620) THEN
        !      PRINT*,TB_tmp(1:7)
        !      print*,i
        !  ENDIF

        ! TEST ENDS
  
        las_ch1(i,j) = TB_tmp(1)
        las_ch2(i,j) = TB_tmp(2)
        las_ch3(i,j) = TB_tmp(3)
        las_ch4(i,j) = TB_tmp(4)
        las_ch5(i,j) = TB_tmp(5)
        las_ch6(i,j) = TB_tmp(6)
        las_ch7(i,j) = TB_tmp(7)
        las_ch24(i,j) = TB_tmp(24)
        
        TA_tmp = 0.0
        TB_tmp = 0.0
      ENDIF

      ! CH1
      las_ch1(i,j)=las_ch1(i,j)*100.0-ctok
      IF ( las_ch1(i,j) <= 0.0 ) THEN
        las(i,j)%ch1(1)=CHAR(INT(las_ch1(i,j))/256-1)
        las(i,j)%ch1(2)=CHAR(MOD(INT(las_ch1(i,j)),256)-1)
      ELSE
        las(i,j)%ch1(1)=CHAR(INT(las_ch1(i,j))/256)
        las(i,j)%ch1(2)=CHAR(MOD(INT(las_ch1(i,j)),256))
      ENDIF

      ! CH2
      las_ch2(i,j)=las_ch2(i,j)*100.0-ctok
      IF ( las_ch2(i,j) <= 0.0 ) THEN
        las(i,j)%ch2(1)=CHAR(INT(las_ch2(i,j))/256-1)
        las(i,j)%ch2(2)=CHAR(MOD(INT(las_ch2(i,j)),256)-1)
      ELSE
        las(i,j)%ch2(1)=CHAR(INT(las_ch2(i,j))/256)
        las(i,j)%ch2(2)=CHAR(MOD(INT(las_ch2(i,j)),256))
      ENDIF

      ! CH3
      las_ch3(i,j)=las_ch3(i,j)*100.0-ctok
      IF ( las_ch3(i,j) <= 0.0) THEN
        las(i,j)%ch3(1)=CHAR(INT(las_ch3(i,j))/256-1)
        las(i,j)%ch3(2)=CHAR(MOD(INT(las_ch3(i,j)),256)-1)
      ELSE
        las(i,j)%ch3(1)=CHAR(INT(las_ch3(i,j))/256)
        las(i,j)%ch3(2)=CHAR(MOD(INT(las_ch3(i,j)),256))
      ENDIF

      ! CH4
      las_ch4(i,j)=las_ch4(i,j)*100.0-ctok
      IF ( las_ch4(i,j) <= 0.0 ) THEN
        las(i,j)%ch4(1)=CHAR(INT(las_ch4(i,j))/256-1)
        las(i,j)%ch4(2)=CHAR(MOD(INT(las_ch4(i,j)),256)-1)
      ELSE
        las(i,j)%ch4(1)=CHAR(INT(las_ch4(i,j))/256)
        las(i,j)%ch4(2)=CHAR(MOD(INT(las_ch4(i,j)),256))
      ENDIF

      ! CH5
      las_ch5(i,j)=las_ch5(i,j)*100.0-ctok
      IF ( las_ch5(i,j) <= 0.0 ) THEN
        las(i,j)%ch5(1)=CHAR(INT(las_ch5(i,j))/256-1)
        las(i,j)%ch5(2)=CHAR(MOD(INT(las_ch5(i,j)),256)-1)
      ELSE
        las(i,j)%ch5(1)=CHAR(INT(las_ch5(i,j))/256)
        las(i,j)%ch5(2)=CHAR(MOD(INT(las_ch5(i,j)),256))
      ENDIF

      ! CH6
      las_ch6(i,j)=las_ch6(i,j)*100.0-ctok
      IF ( las_ch6(i,j) <= 0.0 ) THEN
        las(i,j)%ch6(1)=CHAR(INT(las_ch6(i,j))/256-1)
        las(i,j)%ch6(2)=CHAR(MOD(INT(las_ch6(i,j)),256)-1)
      ELSE
        las(i,j)%ch6(1)=CHAR(INT(las_ch6(i,j))/256)
        las(i,j)%ch6(2)=CHAR(MOD(INT(las_ch6(i,j)),256))
      ENDIF

      ! CH7
      las_ch7(i,j)=las_ch7(i,j)*100.0-ctok
      IF ( las_ch7(i,j) <= 0.0 ) THEN
        las(i,j)%ch7(1)=CHAR(INT(las_ch7(i,j))/256-1)
        las(i,j)%ch7(2)=CHAR(MOD(INT(las_ch7(i,j)),256)-1)
      ELSE
        las(i,j)%ch7(1)=CHAR(INT(las_ch7(i,j))/256)
        las(i,j)%ch7(2)=CHAR(MOD(INT(las_ch7(i,j)),256))
      ENDIF

      ! CH24
      las_ch24(i,j)=las_ch24(i,j)*100.0-ctok
      IF ( las_ch24(i,j) <= 0.0 ) THEN
        las(i,j)%ch24(1)=CHAR(INT(las_ch24(i,j))/256-1)
        las(i,j)%ch24(2)=CHAR(MOD(INT(las_ch24(i,j)),256)-1)
      ELSE
        las(i,j)%ch24(1)=CHAR(INT(las_ch24(i,j))/256)
        las(i,j)%ch24(2)=CHAR(MOD(INT(las_ch24(i,j)),256))
      ENDIF

      ! Write LAS data block
      WRITE(65,IOSTAT=ierror) las(i,j)
    END DO SCENE_LAS_OUT

    ! Loop over UAS scenes
    SCENE_UAS_OUT:DO j=1, NSCENES_UAS
      
      ! SSMIS TA to TB correction
      IF ( TA2TB_FLAG ) THEN
        TA_tmp(19) = uas_ch19(i,j)
        TA_tmp(20) = uas_ch20(i,j)
        TA_tmp(21) = uas_ch21(i,j)
        TA_tmp(22) = uas_ch22(i,j)
        TA_tmp(23) = uas_ch23(i,j)

        CALL SSMIS_TA2TB(TA_tmp, TB_tmp)

        uas_ch19(i,j) = TB_tmp(19)
        uas_ch20(i,j) = TB_tmp(20)
        uas_ch21(i,j) = TB_tmp(21)
        uas_ch22(i,j) = TB_tmp(22)
        uas_ch23(i,j) = TB_tmp(23)

        TA_tmp = 0.0
        TB_tmp = 0.0
      ENDIF
      
      ! CH19
      uas_ch19(i,j)=uas_ch19(i,j)*100.0-ctok
      IF ( uas_ch19(i,j) <= 0.0 ) THEN
        uas(i,j)%ch19(1)=CHAR(INT(uas_ch19(i,j))/256-1)
        uas(i,j)%ch19(2)=CHAR(MOD(INT(uas_ch19(i,j)),256)-1)
      ELSE
        uas(i,j)%ch19(1)=CHAR(INT(uas_ch19(i,j))/256)
        uas(i,j)%ch19(2)=CHAR(MOD(INT(uas_ch19(i,j)),256))
      ENDIF

      ! CH20
      uas_ch20(i,j)=uas_ch20(i,j)*100.0-ctok
      IF ( uas_ch20(i,j) <= 0.0 ) THEN
        uas(i,j)%ch20(1)=CHAR(INT(uas_ch20(i,j))/256-1)
        uas(i,j)%ch20(2)=CHAR(MOD(INT(uas_ch20(i,j)),256)-1)
      ELSE
        uas(i,j)%ch20(1)=CHAR(INT(uas_ch20(i,j))/256)
        uas(i,j)%ch20(2)=CHAR(MOD(INT(uas_ch20(i,j)),256))
      ENDIF

      ! CH21
      uas_ch21(i,j)=uas_ch21(i,j)*100.0-ctok
      IF ( uas_ch21(i,j) <= 0.0 ) THEN
        uas(i,j)%ch21(1)=CHAR(INT(uas_ch21(i,j))/256-1)
        uas(i,j)%ch21(2)=CHAR(MOD(INT(uas_ch21(i,j)),256)-1)
      ELSE
        uas(i,j)%ch21(1)=CHAR(INT(uas_ch21(i,j))/256)
        uas(i,j)%ch21(2)=CHAR(MOD(INT(uas_ch21(i,j)),256))
      ENDIF

      ! CH22
      uas_ch22(i,j)=uas_ch22(i,j)*100.0-ctok
      IF ( uas_ch22(i,j) <= 0.0 ) THEN
        uas(i,j)%ch22(1)=CHAR(INT(uas_ch22(i,j))/256-1)
        uas(i,j)%ch22(2)=CHAR(MOD(INT(uas_ch22(i,j)),256)-1)
      ELSE
        uas(i,j)%ch22(1)=CHAR(INT(uas_ch22(i,j))/256)
        uas(i,j)%ch22(2)=CHAR(MOD(INT(uas_ch22(i,j)),256))
      ENDIF

      ! CH23
      uas_ch23(i,j)=uas_ch23(i,j)*100.0-ctok
      IF ( uas_ch23(i,j) <= 0.0 ) THEN
        uas(i,j)%ch23(1)=CHAR(INT(uas_ch23(i,j))/256-1)
        uas(i,j)%ch23(2)=CHAR(MOD(INT(uas_ch23(i,j)),256)-1)
      ELSE
        uas(i,j)%ch23(1)=CHAR(INT(uas_ch23(i,j))/256)
        uas(i,j)%ch23(2)=CHAR(MOD(INT(uas_ch23(i,j)),256))
      ENDIF

      ! Write UAS data block
      WRITE(65,IOSTAT=ierror) uas(i,j)
    END DO SCENE_UAS_OUT

    ! Write AUX data block
    WRITE(65,IOSTAT=ierror) aux(i)

  END DO NSCAN_OUT

  ! Write out dummy array in order to keep the same size as input file
  IF ( ndummy >= 1 ) THEN
    DUMMY_OUT: DO i=1, ndummy
      WRITE(65,IOSTAT=ierror) dummy(i)
    END DO DUMMY_OUT
  END IF

  CLOSE(65)
  !WRITE(*,*) '... Writing Done ! '
  !WRITE(*,*) ' '

  ! Deallocate array
  ! Scan Header
  DEALLOCATE(scan_st,STAT=istat)
  DEALLOCATE(scan_day,STAT=istat)
  ! IMG
  DEALLOCATE(img_lat,STAT=istat)
  DEALLOCATE(img_lon,STAT=istat)
  DEALLOCATE(img_ch8,STAT=istat)
  DEALLOCATE(img_ch9,STAT=istat)
  DEALLOCATE(img_ch10,STAT=istat)
  DEALLOCATE(img_ch11,STAT=istat)
  DEALLOCATE(img_lat17,STAT=istat)
  DEALLOCATE(img_lon17,STAT=istat)
  DEALLOCATE(img_ch17,STAT=istat)
  DEALLOCATE(img_ch18,STAT=istat)
  ! ENV
  DEALLOCATE(env_lat,STAT=istat)
  DEALLOCATE(env_lon,STAT=istat)
  DEALLOCATE(env_ch12,STAT=istat)
  DEALLOCATE(env_ch13,STAT=istat)
  DEALLOCATE(env_ch14,STAT=istat)
  DEALLOCATE(env_lat15,STAT=istat)
  DEALLOCATE(env_lon15,STAT=istat)
  DEALLOCATE(env_ch15,STAT=istat)
  DEALLOCATE(env_ch16,STAT=istat)
  ! LAS
  DEALLOCATE(las_lat,STAT=istat)
  DEALLOCATE(las_lon,STAT=istat)
  DEALLOCATE(las_ch1,STAT=istat)
  DEALLOCATE(las_ch2,STAT=istat)
  DEALLOCATE(las_ch3,STAT=istat)
  DEALLOCATE(las_ch4,STAT=istat)
  DEALLOCATE(las_ch5,STAT=istat)
  DEALLOCATE(las_ch6,STAT=istat)
  DEALLOCATE(las_ch7,STAT=istat)
  DEALLOCATE(las_ch24,STAT=istat)
  ! UAS
  DEALLOCATE(uas_lat,STAT=istat)
  DEALLOCATE(uas_lon,STAT=istat)
  DEALLOCATE(uas_ch19,STAT=istat)
  DEALLOCATE(uas_ch20,STAT=istat)
  DEALLOCATE(uas_ch21,STAT=istat)
  DEALLOCATE(uas_ch22,STAT=istat)
  DEALLOCATE(uas_ch23,STAT=istat)
  ! AUX
  DEALLOCATE(aux_wc,STAT=istat)
  DEALLOCATE(aux_cc,STAT=istat)
  DEALLOCATE(aux_wl,STAT=istat)
  DEALLOCATE(aux_mux,STAT=istat)
  DEALLOCATE(aux_sfid,STAT=istat)

  icount=icount+1

  !WRITE(*,*) '=============================================================================='
  !WRITE(*,*) ' '

END DO TDR_LOOP

CLOSE(20)
CLOSE(18)

WRITE(*,*) 'The number of TDR orbit file processed is ', icount
!WRITE(*,*) ' '

CONTAINS

  SUBROUTINE realdistance(latin1,lonin1,latin2,lonin2,dist)
  !========================================================================================================
  !  
  !  Purpose:
  !    To calculate geophysical distance of two points
  !
  !  Record of revisions:
  !     YYYY/MM/DD      Programmer                       Description of change
  !    ============   ==============   ===========================================================
  !     2007/03/01     Banghua Yan       Create orginal subroutine
  !                    (NOAA/NESDIS)
  !
  !========================================================================================================
  IMPLICIT NONE
  
    ! Declare subroutine arguments
    REAL(4), INTENT(IN) :: latin1,lonin1,latin2,lonin2
    REAL(4), INTENT(OUT) :: dist
    
    ! Declare local variables
    REAL(4) :: lat1,lon1,lat2,lon2
    REAL(4) :: PI,earth_radius,temp

    PI=3.14159
    earth_radius=6378.
    lat1=latin1*PI/180.0
    lon1=lonin1*PI/180.0
    lat2=latin2*PI/180.0
    lon2=lonin2*PI/180.0

    temp=SIN(lat1)*SIN(lat2) + COS(lat1)*COS(lat2)*COS(lon2-lon1)
    IF (ABS(lat1-lat2) <= 0.001 .AND. ABS(lon1-lon2) <= 0.001) temp = 0.0
    
    temp=ACOS(temp)
    temp=temp*earth_radius
    IF (temp < 0.) temp=9999.
    dist = temp
  
  END SUBROUTINE realdistance



  SUBROUTINE SSMIS_TA2TB(TA, TB)
  !========================================================================================================
  !  
  !  Purpose:
  !    To convert SSMIS antenna temperature to brightness temperature
  !
  !  Input/Output:
  !    TA -  Input    SSMIS 24 channel antenna brightness temperature
  !    TB -  Output   SSMIS 24 channel sensor brightness temperature of the earth scenes
  !    AP -  Internal 24 channel anntena gains
  !    BP -  Internal 24 channels for cross polarization spill-over
  !
  !  Correction Process:
  !    Sounding channels: corrected for attenna spill over effects using APC coefficients
  !      (1-7)            provided by Steve Swadley, NRL contractor
  !    Imaging channels:  using the same coefficients as F8 to F15 (SSM/I imaging channels were 
  !      (12-18)          corrected for both spill-over and xpol)
  !
  !  Record of revisions:
  !     YYYY/MM/DD      Programmer                       Description of change
  !    ============   ==============   ===========================================================
  !     2006/11/06     Fuzhong Weng      Create orginal subroutine
  !                    (NOAA/NESDIS)
  !
  !========================================================================================================
  IMPLICIT NONE

    REAL(4), DIMENSION(24), INTENT(IN) :: TA
    REAL(4), DIMENSION(24), INTENT(OUT) :: TB

    ! Intercept for TA to TB conversion
                                   ! F16 7 LAS 
                                   ! *** from NRL Swadley ***
                                   ! ch1    ch2   ch3    ch4    ch5    ch6    ch7 (SSMIS)
    REAL(8), DIMENSION(24) :: AP=(/0.9850,0.9850,0.9850,0.9850,0.9850,0.9790,0.9815,& 
                                   ! F16 4 ENV channels (150 Ghz plus three double-sided 183 bands)
                                   ! *** from NRL Swadley ***
                                   ! ch8   ch9    ch10   ch11 (SSMIS)
                                   0.9949,0.9934,0.9934,0.9934, &   
                                   ! F16 7 imaging channel gain 
                                   ! ************************************************************
                                   ! *** from NRL Swadley ***
                                   ! ch12   ch13   ch14   ch15   ch16   ch17   ch18 (SSMIS)
                                   !  19h    19v    22v    37h    37v    91v    91h (SSMIS)
                                   !0.9680,0.9720,0.9820,0.9810,0.9850,0.9820,0.9780,& 
                                   ! ************************************************************
                                   ! *** from NESDIS Weng ***
                                   ! ch2    ch1    ch3    ch5    ch4    ch6   ch7   (SSMI)
                                   ! 19h    19v    22v    37h    37v    85v    85h  
                                   ! ch12   ch13   ch14   ch15   ch16   ch17   ch18 (SSMIS Symmetric to SSMI)
                                   0.9690,0.9690,0.9740,0.9860,0.9860,0.9880,0.9880,& 
                                   ! F16 6 UAS channels
                                   ! *** from NRL Swadley ***
                                   ! ch19   ch20   ch21   ch22   ch23   ch24 (SSMIS)
                                   0.9815,0.9815,0.9815,0.9815,0.9815,0.9815/)         
                                  
    ! Slope for TA to TB conversion
                                   ! F16 7 LAS 
                                   ! *** from NRL Swadley & NESDIS Weng ***
                                   !ch1 ch2  ch3  ch4  ch5  ch6  ch7 (SSMIS)
    REAL(8), DIMENSION(24) :: BP=(/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,&
                                   ! F16 4 ENV channels (150 Ghz plus three double-sided 183 bands)
                                   ! *** from NRL Swadley & NESDIS Weng ***
                                   !ch8 ch9  ch10 ch11 (SSMI/S)
                                   0.0, 0.0, 0.0, 0.0, &
                                   ! F16 7 imaging channel 
                                   ! ************************************************************
                                   ! *** from NRL Swadley ***
                                   !ch12 ch13 ch14 ch15 ch16 ch17 ch18 (SSMIS)
                                   !19h  19v  22v  37h  37v  85v  85h  (SSMIS)
                                   !0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                                   ! ************************************************************
                                   ! *** from NESDIS Weng ***
                                   ! ch1     ch2    ch3    ch4     ch5     ch6     ch7 (SSMI)
                                   ! 19v     19h    22v    37v     37h     85v     85h (SSMI)
                                   !0.00473,0.00415,0.0107,0.0217,0.02612,0.01383,0.01947, & 
                                   !ch12    ch13   ch14    ch15   ch16    ch17    ch18 (SSMIS)
                                   ! 19h     19v    22v     37h    37v     85v     85h (SSMIS)
                                   0.00415,0.00473,0.0107,0.02612,0.0217,0.01383,0.01947,&  
                                   ! F16 6 UAS temperature 
                                   ! *** from NRL Swadley & NESDIS Weng ***
                                   !ch19 ch20 ch21 ch22 ch23 ch24 (SSMIS)
                                   0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)

    REAL(8), DIMENSION(24) :: CP, DP
    INTEGER(4) :: iChan

    DO iChan=1, 24
      CP(iChan) = 1.0/( AP(iChan)*(1.0 - BP(iChan)) )
      DP(iChan) = CP(iChan) * BP(iChan)
      TB(iChan) = CP(iChan)*TA(iChan)
    END DO
  
    ! SSMIS imaging channel cross polarization spill-over
    TB(12) = TB(12) - DP(12)*TA(13)               ! 19H
    TB(13) = TB(13) - DP(13)*TA(12)               ! 19V
    TB(14) = TB(14) - DP(13)*(0.65*TA(13)+96.6)   ! 22V
    TB(15) = TB(15) - DP(15)*TA(16)               ! 37H
    TB(16) = TB(16) - DP(16)*TA(15)               ! 37V
    TB(17) = TB(17) - DP(17)*TA(18)               ! 85V
    TB(18) = TB(18) - DP(18)*TA(17)               ! 85H

  END SUBROUTINE SSMIS_TA2TB


  SUBROUTINE SSMIS_TA_REMAP(SSMIS_TA, SSMI_TA)
  !========================================================================================================
  !  
  !  Purpose:
  !    To linearly map SSMIS image channel (12-18) antenna temperature to SSMI antenna temperature
  !
  !  Input/Output:
  !    SSMIS_TA -  Input    24 channels SSMIS antenna brightness temperature
  !    SSMI_TA  -  Output   24 channels SSMI antenna temperature where channel 12-18 are mapped from SSMIS
  !    A -  Internal slope 24 channel anntena gains
  !    B -  Internal 24 channels for cross polarization spill-over
  !
  !  Record of revisions:
  !     YYYY/MM/DD      Programmer                       Description of change
  !    ============   ==============   ===========================================================
  !     2006/11/06     Fuzhong Weng      Create orginal subroutine
  !
  !========================================================================================================
  IMPLICIT NONE
    REAL(4), DIMENSION(24), INTENT(IN) :: SSMIS_TA
    REAL(4), DIMENSION(24), INTENT(OUT) :: SSMI_TA

    ! Intercept for F16 SSMIS to F15 SSMI mapping
                                 ! F16 7 LAS and 4 ENV channels  
                                 ! ch1 ch2  ch3  ch4  ch5  ch6  ch7  ch8  ch9  ch10 ch11 (SSMIS)
    REAL(8), DIMENSION(24) :: A=(/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, & 
                                 ! F16 7 imaging channels
                                 ! ************************************************************
                                 ! *** from NRL Swadley ***
                                 ! ch12    ch13    ch14    ch15    ch16    ch17   ch18 (SSMIS)
                                 !  19h     19v     22v     37h     37v     91v    91h (SSMIS)
                                 ! -1.3,   -0.6,   -2.8,   -2.4,   -1.2,   -1.9,  -0.6, & 
                                 ! ************************************************************
                                 ! *** from NESDIS Weng ***
                                 !  ch1     ch2     ch3     ch4     ch5     ch6    ch7  (SSMI)
                                 !  19v     19h     22v     37v     37h     85v    85h  (SSMI)
                                 !7.80472,7.44254,6.76383,7.34409,8.55426,6.57813,6.45397, & 
                                 ! ch12    ch13    ch14    ch15    ch16    ch17   ch18 (SSMIS)
                                 !  19h     19v     22v     37h     37v     91v    91h (SSMIS)
                                  7.44254,7.80472,6.76383,8.55426,7.34409,6.57813,6.45397, & 
                                 ! F16 6 UAS channels
                                 !ch19 ch20 ch21 ch22 ch23 ch24 (SSMIS)
                                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)  

    ! Slope for F16 SSMIS to F15 SSMI mapping
                                 ! F16 7 LAS and 4 ENV channels  
                                 ! ch1 ch2  ch3  ch4  ch5  ch6  ch7  ch8  ch9  ch10 ch11 (SSMIS)
    REAL(8), DIMENSION(24) :: B=(/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,&
                                 ! F16 7 imaging channels
                                 ! ************************************************************
                                 ! *** from NRL Swadley ***
                                 !  ch12    ch13    ch14    ch15    ch16    ch17   ch18 (SSMIS)
                                 !   19h     19v     22v     37h     37v     91v    91h (SSMIS)
                                 ! 1.03000,1.04054,1.03846,1.01364,1.01481,1.02500,1.00588,&
                                 ! ************************************************************
                                 ! *** from NESDIS Weng ***
                                 !   ch1      ch2      ch3      ch4      ch5      ch6     ch7  (SSMI)
                                 !   19v      19h      22v      37v      37h      85v     85h  (SSMI)
                                 !0.967519,0.969424,0.959808,0.958955,0.954316,0.980339,0.978795, & 
                                 !  ch12     ch13     ch14     ch15     ch16     ch17    ch18 (SSMIS)
                                 !   19h      19v      22v      37h      37v      91v     91h (SSMIS)
                                  0.969424,0.967519,0.959808,0.954316,0.958955,0.980339,0.978795, & 
                                 ! F16 6 UAS channels
                                 !ch19 ch20 ch21 ch22 ch23 ch24 (SSMIS)
                                  1.0, 1.0, 1.0, 1.0, 1.0, 1.0/)
    
    INTEGER(4) :: iChan

    DO iChan=1, 24
      SSMI_TA(iChan) = A(iChan) + B(iChan)*SSMIS_TA(iChan)
    END DO

  END SUBROUTINE SSMIS_TA_REMAP

END PROGRAM SSMIS_CTDR_MAIN_LITENDIAN

