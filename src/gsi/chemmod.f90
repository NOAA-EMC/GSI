module chemmod

!   prgmmr: pagowski                date: 2010-09-13
! module contains functions
! 1) to convert units between obs and model function
!obs2model_anowbufr_pm
! 2) allocate and assign initial values to pm2_5 guess
!init_pm2_5_guess
! 3) declare/assign &CHEM namelist parameters 
!init_chem
! subroutine oneobschem
! for testing one observation assimilation

! NB: keep aerosol names capital for consistency with cmaq output names
! 2011-09-09 pagowski - add codes for PM2.5 for prepbufr and bufr dump files
! 2013-11-01 pagowski - add code for PM2.5 assimilation with wrf-chem
! 2019-03-21 Wei/Martin - logical if to read in aerosols from ext. file (default F) 
! 2020-10-02 Hongli Wang - fv3_cmaq_regional is for fv3lam-cmaq, which is
!                          one more option of model choice (different to cmaq_regional)

  use kinds, only : i_kind, r_kind, r_single
!Hongli wang
  use gridmod, only: fv3_cmaq_regional, cmaq_regional,wrf_mass_regional,lat2,lon2,nsig
  use constants, only : tiny_single,max_varname_length

  implicit none

  private

  public :: obs2model_anowbufr_pm,cmaq_pm,fv3_cmaq_pm,wrf_chem_pm,&
        cmaq_o3,wrf_chem_o3
  public :: iconc,ierror,ilat,ilon,itime,iid,ielev,isite,iikx,ilate,ilone

  public :: elev_tolerance,elev_missing,pm2_5_teom_max,pm10_teom_max

!Hongli wrfcmaq
  public :: ngrid1d_cmaq,ngrid2d_cmaq,nmet2d_cmaq,nmet3d_cmaq,&
        naero_cmaq,aeronames_cmaq

!Hongli fv3cmaq
  public :: naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3,ssmoke_cmaq_fv3,sdust_cmaq_fv3

  public :: naero_gocart_wrf,aeronames_gocart_wrf

  public :: pm2_5_guess,init_pm2_5_guess,&
       aerotot_guess,init_aerotot_guess
  public :: init_chem
  public :: berror_chem,oneobtest_chem,maginnov_chem,magoberr_chem,oneob_type_chem,conconeobs
  public :: oblat_chem,oblon_chem,obpres_chem,diag_incr,oneobschem
  public :: site_scale,nsites
  public :: tunable_error
  public :: in_fname,out_fname,incr_fname,maxstr
  public :: code_pm25_ncbufr,code_pm25_anowbufr
  public :: code_pm10_ncbufr,code_pm10_anowbufr

  public :: l_aoderr_table
!Hongli Wang
  public :: laeroana_gocart,laeroana_fv3cmaq,crtm_aerosol_model,crtm_aerosolcoeff_format,crtm_aerosolcoeff_file, &
            laod_crtm_cmaq,iaod_crtm_cmaq,iaod_recs_cmaq,icvt_cmaq_fv3, raod_radius_mean_scale,raod_radius_std_scale
  public :: visindx_cmaq_fv3,visindx_large_cmaq_fv3,humfac,humfac_large,humfac_ss,aemolwt_cmaq_fv3
  public :: visindx_recs_fv3,humfac_recs,humfac_recs_ss
  public :: ppmv_conv
  public :: aod_qa_limit
  public :: luse_deepblue

  public :: wrf_pm2_5

  public :: lread_ext_aerosol
  public :: aero_ratios
  public :: upper2lower,lower2upper

  public :: s_2_5,d_2_5,d_10,nh4_mfac,oc_mfac

  logical :: l_aoderr_table
  logical :: laeroana_gocart,laeroana_fv3cmaq,laod_crtm_cmaq
  logical :: luse_deepblue
  character(len=max_varname_length) :: crtm_aerosol_model,crtm_aerosolcoeff_format,crtm_aerosolcoeff_file
  integer(i_kind) :: iaod_crtm_cmaq
  integer(i_kind) :: aod_qa_limit  ! qa >=  aod_qa_limit will be retained
  integer(i_kind) :: iaod_recs_cmaq
  integer(i_kind) :: icvt_cmaq_fv3
  real(r_kind)    :: raod_radius_mean_scale,raod_radius_std_scale
  real(r_kind)    :: ppmv_conv = 96.06_r_kind/28.964_r_kind*1.0e+3_r_kind
  logical :: wrf_pm2_5

  logical :: lread_ext_aerosol ! if true, will read in aerosols from aerfXX rather than from sigfXX

  real(r_kind),parameter :: s_2_5=0.942_r_kind,d_2_5=0.286_r_kind,&
       d_10=0.87_r_kind,nh4_mfac=1.375_r_kind,oc_mfac=1.8_r_kind

  logical :: aero_ratios

  logical :: oneobtest_chem,diag_incr,berror_chem
  character(len=max_varname_length) :: oneob_type_chem
  integer(i_kind), parameter :: maxstr=256
  real(r_kind) :: maginnov_chem,magoberr_chem,conconeobs,&
        oblon_chem,oblat_chem,obpres_chem,elev_tolerance,tunable_error
  
  integer(i_kind), parameter :: code_pm25_ncbufr=11, &
       code_pm25_anowbufr=102,code_pm10_ncbufr=code_pm25_ncbufr,&
       code_pm10_anowbufr=code_pm25_anowbufr

  real(r_kind),parameter :: pm2_5_teom_max=100.0_r_kind !ug/m3
!some parameters need to be put here since convinfo file won't
!accomodate, stands for maximum realistic value of surface pm2.5
  real(r_kind),parameter :: pm10_teom_max=150.0_r_kind !ug/m3


  real(r_kind),parameter :: elev_missing=-9999.0_r_kind
!when elevation of the obs site is missing assign that

  integer(i_kind), parameter :: nsites=4

!character of sites:
! 1 - unknown
! 2 - urban
! 3 - suburban
! 4 - rural
! depending on the character of measurement site  observation
! error is assigned - see read_anowbufr.f90 for error calculation
!error_2=tunable_error*error_1*sqrt(dx/site_scale)

  real(r_kind), dimension (nsites), parameter:: &
        site_scale= (/3000._r_kind,2000._r_kind,&
        4000._r_kind,10000._r_kind/)

!conversion ratios between obs and model units
  real(r_kind),parameter :: kg2ug=1.e+9_r_kind,ppbv2ppmv=0.001_r_kind
  real(r_kind),parameter :: cmaq_pm=kg2ug,wrf_chem_pm=kg2ug,&
        cmaq_o3=ppbv2ppmv,wrf_chem_o3=ppbv2ppmv, & 
        fv3_cmaq_pm=kg2ug  !1.0_r_kind

!position of obs parameters in obs output file record
  integer(i_kind), parameter :: &
        iconc = 1,ierror= 2,&
        ilat  = 3,ilon  = 4,&
        itime = 5,iid=6,&
        ielev = 7,isite = 8,&
        iikx  = 9,ilate = 10,&
        ilone =11
  
!parameters for erading cmaq input file
  integer(i_kind), parameter :: &
        ngrid1d_cmaq=2,  &     !aeta1,eta1
        ngrid2d_cmaq=4,  &     !lat,lon,dx_mc,dy_mc
        nmet2d_cmaq=2, &       !ht,psfc
        nmet3d_cmaq=4,  &  !t,qv,u,v
        naero_cmaq=33,&  !number of cmaq aerosol species
        naero_gocart_wrf=15  !number of gocart aerosol species

  character(len=max_varname_length), dimension(naero_cmaq), parameter :: &
        aeronames_cmaq=(/&
        'ASO4I',  'ANO3I',  'ANH4I',  'AORGPAI',  'AECI',   'ACLI', &
        'ASO4J',  'ANO3J',  'ANH4J',  'AORGPAJ',  'AECJ',   'ANAJ',&
        'ACLJ',   'A25J',   'AXYL1J', 'AXYL2J',   'AXYL3J', 'ATOL1J',&
        'ATOL2J', 'ATOL3J', 'ABNZ1J', 'ABNZ2J',   'ABNZ3J', 'AALKJ',&
        'AOLGAJ', 'AISO1J', 'AISO2J', 'AISO3J',   'ATRP1J', 'ATRP2J',&
        'ASQTJ',  'AOLGBJ', 'AORGCJ'/)

!Hongli FV3CMAQ,
  integer(i_kind), parameter :: &
        naero_cmaq_fv3=70 !45  !number of cmaq aerosol species aero6

  character(len=max_varname_length), dimension(naero_cmaq_fv3), parameter :: &
        aeronames_cmaq_fv3=(/&
        'aso4i','aso4j','aso4k','ano3i','ano3j','ano3k', &
        'acli', 'aclj', 'aclk', 'anh4i','anh4j','anh4k', &
        'anai', 'anaj', 'amgj', 'akj',  'acaj', &
        'aeci', 'aecj', 'afej', 'aalj', 'asij', 'atij', &
        'amnj','aothri','aothrj','axyl1j','axyl2j','axyl3j', &
        'atol1j','asoil','acors','aseacat','alvpo1i','alvpo1j', &
        'asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j', &
        'aivpo1j','alvoo1i','alvoo2i','asvoo1i','asvoo2i', &  !/) !, &
        'atol2j','atol3j',          &
        'abnz1j','abnz2j','abnz3j', &
        'aiso1j','aiso2j','aiso3j', &
        'atrp1j','atrp2j','asqtj',  &
        'aalk1j','aalk2j',          &
        'apah1j','apah2j','apah3j', &
        'aorgcj','aolgbj','aolgaj', &
        'alvoo1j','alvoo2j',        &
        'asvoo1j','asvoo2j','asvoo3j', &
        'apcsoj'/)
        !'amassi','amassj','amassk'/)

   integer(i_kind),dimension(naero_cmaq_fv3), parameter :: &
        imodes_cmaq_fv3=(/&
         1,2,3,1,2,3, & 
         1,2,3,1,2,3, & 
         1,2,2,2,2,   &
         1,2,2,2,2,2, &
         2,1,2,2,2,2, & 
         2,3,3,3,1,2, &
         1,2,1,2,2,   & 
         2,1,1,1,1,   & ! /) 
         2,2,2,2,2,   &
         2,2,2,2,2,   &
         2,2,2,2,2,   &
         2,2,2,2,2,   &
         2,2,2,2,2 /)
   real(r_kind),dimension(naero_cmaq_fv3), parameter :: &
         ssmoke_cmaq_fv3 = (/&
         1.0,1.0,1.0,1.0,1.0,1.0, &
         1.0,0.0,0.0,1.0,1.0,0.0, &
         0.0,0.0,0.0,0.0,0.0,   &
         1.0,1.0,0.0,0.0,0.0,0.0, &
         0.0,1.0,1.0,1.0,1.0,1.0, &
         1.0,0.0,0.0,0.0,1.0,1.0, &
         1.0,1.0,1.0,1.0,1.0,     &
         1.0,1.0,1.0,1.0,1.0,     & ! /)
         1.0,1.0,1.0,1.0,1.0,     &
         1.0,1.0,1.0,1.0,1.0,     &
         1.0,1.0,1.0,1.0,1.0,     &
         1.0,1.0,1.0,1.0,1.0,     &
         1.0,1.0,1.0,1.0,1.0/)

   real(r_kind),dimension(naero_cmaq_fv3), parameter :: &
         sdust_cmaq_fv3 = (/&
         0.0,0.0,0.0,0.0,0.0,0.0, &
         0.0,0.0,0.0,0.0,0.0,0.0, &
         0.0,0.0,1.0,1.0,1.0,   &
         0.0,0.0,1.0,1.0,1.0,1.0, &
         1.0,0.0,0.0,0.0,0.0,0.0, &
         0.0,1.0,1.0,0.0,0.0,0.0, &
         0.0,0.0,0.0,0.0,0.0,      &
         0.0,0.0,0.0,0.0,0.0,     & !/)
         0.0,0.0,0.0,0.0,0.0,     &
         0.0,0.0,0.0,0.0,0.0,     &
         0.0,0.0,0.0,0.0,0.0,     &
         0.0,0.0,0.0,0.0,0.0,     &
         0.0,0.0,0.0,0.0,0.0/)
!!!!
!!!!!!AERO_DATA.F
   real(r_kind),dimension(naero_cmaq_fv3), parameter :: &
         visindx_cmaq_fv3 = (/&
         2.20,2.20,2.20,2.40,2.40,2.40, &
         1.70,1.70,1.70,0.00,0.00,0.00, &
         1.70,1.70,1.00,1.00,1.00,   &
         10.0,10.0,1.00,1.00,1.00,1.00, &
         1.00,1.00,1.00,2.80,2.80,2.80, &
         2.80,0.60,0.60,1.70,4.00,4.00, &
         4.00,4.00,4.00,4.00,4.00,      &
         4.00,4.00,4.00,4.00,4.00,      & ! /)
         2.80,2.80,2.80,2.80,2.80,      &
         2.80,2.80,2.80,2.80,2.80,      &
         2.80,2.80,2.80,2.80,2.80,      &
         2.80,2.80,2.80,2.80,2.80,      &
         2.80,2.80,2.80,2.80,2.80/)
   real(r_kind),dimension(naero_cmaq_fv3), parameter :: &
         visindx_large_cmaq_fv3 = (/&
         4.80,4.80,4.80,5.10,5.10,5.10, &
         1.70,1.70,1.70,0.00,0.00,0.00, &
         1.70,1.70,1.00,1.00,1.00,   &
         10.0,10.0,1.00,1.00,1.00,1.00, &
         1.00,1.00,1.00,2.80,2.80,2.80, &
         6.10,0.60,0.60,1.70,6.10,6.10, &
         6.10,6.10,6.10,6.10,6.10,      &
         6.10,6.10,6.10,6.10,6.10,      & ! /)
         6.10,6.10,6.10,6.10,6.10,      &
         6.10,6.10,6.10,6.10,6.10,      &
         6.10,6.10,6.10,6.10,6.10,      &
         6.10,6.10,6.10,6.10,6.10,      &
         6.10,6.10,6.10,6.10,6.10/)
   real(r_kind),dimension(naero_cmaq_fv3), parameter :: &
         aemolwt_cmaq_fv3 = (/&
         96.0,96.0,96.0,62.0,62.0,62.0, &
         35.5,35.5,35.5,18.0,18.0,18.0, &
         23.0,23.0,24.3,39.1,40.1,   &
         12.0,12.0,55.8,27.0,28.1,47.9, &
         54.9,200.,200.,174.,185.,218., &
         163.,100.,100.,23.75,218.,218., &
         230.,230.,241.,241.,253., &
         266.,136.,136.,135.,135., & ! /)
         163.,100.,100.,23.75,218.,& 
         163.,100.,100.,23.75,218.,&
         163.,100.,100.,23.75,218.,&
         163.,100.,100.,23.75,218.,&
         163.,100.,100.,23.75,218./)

!/scratch2/BMC/wrfruc/hwang/wf1/cmaq/5.2/CCTM/src/aero/aero6/AOD_DEFN.F
         real, parameter :: humfac( 99 ) = &
         (/1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.3800e+00,  1.4000e+00,  1.4200e+00,  1.4400e+00, &
           1.4600e+00,  1.4800e+00,  1.4900e+00,  1.5100e+00,  1.5300e+00, &
           1.5500e+00,  1.5700e+00,  1.5900e+00,  1.6200e+00,  1.6400e+00, &
           1.6600e+00,  1.6800e+00,  1.7100e+00,  1.7300e+00,  1.7600e+00, &
           1.7800e+00,  1.8100e+00,  1.8300e+00,  1.8600e+00,  1.8900e+00, &
           1.9200e+00,  1.9500e+00,  1.9900e+00,  2.0200e+00,  2.0600e+00, &
           2.0900e+00,  2.1300e+00,  2.1700e+00,  2.2200e+00,  2.2600e+00, &
           2.3100e+00,  2.3600e+00,  2.4100e+00,  2.4700e+00,  2.5400e+00, &
           2.6000e+00,  2.6700e+00,  2.7500e+00,  2.8400e+00,  2.9300e+00, &
           3.0300e+00,  3.1500e+00,  3.2700e+00,  3.4200e+00,  3.5800e+00, &
           3.7600e+00,  3.9800e+00,  4.2300e+00,  4.5300e+00,  4.9000e+00, &
           5.3500e+00,  5.9300e+00,  6.7100e+00,  7.7800e+00,  9.3400e+00, &
           9.3400e+00,  9.3400e+00,  9.3400e+00,  9.3400e+00/)
! air humidity scaling factors at 1% RH intervals for large mode
         real, parameter :: humfac_large( 99 ) = & 
         (/1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00, &
           1.0000e+00,  1.3100e+00,  1.3200e+00,  1.3400e+00,  1.3500e+00, &
           1.3600e+00,  1.3800e+00,  1.3900e+00,  1.4100e+00,  1.4200e+00, &
           1.4400e+00,  1.4500e+00,  1.4700e+00,  1.4900e+00,  1.5000e+00, &
           1.5200e+00,  1.5400e+00,  1.5500e+00,  1.5700e+00,  1.5900e+00, &
           1.6100e+00,  1.6300e+00,  1.6500e+00,  1.6700e+00,  1.6900e+00, &
           1.7100e+00,  1.7300e+00,  1.7500e+00,  1.7800e+00,  1.8000e+00, &
           1.8300e+00,  1.8600e+00,  1.8900e+00,  1.9200e+00,  1.9500e+00, &
           1.9800e+00,  2.0100e+00,  2.0500e+00,  2.0900e+00,  2.1300e+00, &
           2.1800e+00,  2.2200e+00,  2.2700e+00,  2.3300e+00,  2.3900e+00, &
           2.4500e+00,  2.5200e+00,  2.6000e+00,  2.6900e+00,  2.7900e+00, &
           2.9000e+00,  3.0200e+00,  3.1600e+00,  3.3300e+00,  3.5300e+00, &
           3.7700e+00,  4.0600e+00,  4.4300e+00,  4.9200e+00,  5.5700e+00, &
           5.5700e+00,  5.5700e+00,  5.5700e+00,  5.5700e+00/)
!C sea salt humidity scaling factors at 1% RH values

         real, parameter :: humfac_ss( 99 ) = (/ &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 1.00, 1.00, 1.00, 1.00, &
     &        1.00, 2.36, 2.38, 2.42, 2.45, &
     &        2.48, 2.50, 2.51, 2.53, 2.56, &
     &        2.58, 2.59, 2.62, 2.66, 2.69, &
     &        2.73, 2.78, 2.83, 2.83, 2.86, &
     &        2.89, 2.91, 2.95, 3.01, 3.05, &
     &        3.13, 3.17, 3.21, 3.25, 3.27, &
     &        3.35, 3.42, 3.52, 3.57, 3.63, &
     &        3.69, 3.81, 3.95, 4.04, 4.11, &
     &        4.28, 4.49, 4.61, 4.86, 5.12, &
     &        5.38, 5.75, 6.17, 6.72, 7.35, &
     &        7.35, 7.35, 7.35, 7.35/)

! For simplified reconstruciton method
!/scratch2/BMC/wrfruc/hwang/rk/comGSIv3.5_EnKFv1.1/src/main/cmaq_aod_calc.f90
   !axy,atol are not available in RK code
   real(r_kind),dimension(naero_cmaq_fv3), parameter :: &
         visindx_recs_fv3 = (/&
         3.00,3.00,3.00,3.00,3.00,3.00, &
         1.37,1.37,1.37,3.00,3.00,3.00, &
         1.37,1.37,1.00,1.00,1.00,   &
         10.0,10.0,1.00,1.00,1.00,1.00, &
         1.00,1.00,1.00,4.00,4.00,4.00, &
         4.00,0.60,0.60,1.37,4.00,4.00, &
         4.00,4.00,4.00,4.00,4.00,      &
         4.00,4.00,4.00,4.00,4.00,      & ! /)
         4.00,4.00,4.00,4.00,4.00,      &
         4.00,4.00,4.00,4.00,4.00,      &
         4.00,4.00,4.00,4.00,4.00,      &
         4.00,4.00,4.00,4.00,4.00,      &
         4.00,4.00,4.00,4.00,4.00/)
!! RK's code
! air humidity scaling factors at 1% RH intervals
 real(r_kind), parameter :: humfac_recs( 99 ) =  &
          (/1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  &
            1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  &
            1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0001e+00,  1.0001e+00,  &
            1.0004e+00,  1.0006e+00,  1.0024e+00,  1.0056e+00,  1.0089e+00,  &
            1.0097e+00,  1.0105e+00,  1.0111e+00,  1.0115e+00,  1.0118e+00,  &
            1.0122e+00,  1.0126e+00,  1.0130e+00,  1.0135e+00,  1.0139e+00,  &
            1.0173e+00,  1.0206e+00,  1.0254e+00,  1.0315e+00,  1.0377e+00,  &
            1.0486e+00,  1.0596e+00,  1.0751e+00,  1.0951e+00,  1.1151e+00,  &
            1.1247e+00,  1.1343e+00,  1.1436e+00,  1.1525e+00,  1.1615e+00,  &
            1.1724e+00,  1.1833e+00,  1.1955e+00,  1.2090e+00,  1.2224e+00,  &
            1.2368e+00,  1.2512e+00,  1.2671e+00,  1.2844e+00,  1.3018e+00,  &
            1.3234e+00,  1.3450e+00,  1.3695e+00,  1.3969e+00,  1.4246e+00,  &
            1.4628e+00,  1.5014e+00,  1.5468e+00,  1.5992e+00,  1.6516e+00,  &
            1.6991e+00,  1.7466e+00,  1.7985e+00,  1.8549e+00,  1.9113e+00,  &
            1.9596e+00,  2.0080e+00,  2.0596e+00,  2.1146e+00,  2.1695e+00,  &
            2.2630e+00,  2.3565e+00,  2.4692e+00,  2.6011e+00,  2.7330e+00,  &
            2.8461e+00,  2.9592e+00,  3.0853e+00,  3.2245e+00,  3.3637e+00,  &
            3.5743e+00,  3.7849e+00,  4.0466e+00,  4.3594e+00,  4.6721e+00,  &
            5.3067e+00,  5.9412e+00,  6.9627e+00,  8.3710e+00,  9.7793e+00,  &
            1.2429e+01,  1.5078e+01,  1.8059e+01,  2.1371e+01/)

         real, parameter :: humfac_recs_ss( 99 ) =  &
           (/1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 1.00, 1.00, 1.00, 1.00,     &
             1.00, 2.36, 2.38, 2.42, 2.45,     &
             2.48, 2.50, 2.51, 2.53, 2.56,     &
             2.58, 2.59, 2.62, 2.66, 2.69,     &
             2.73, 2.78, 2.83, 2.83, 2.86,     &
             2.89, 2.91, 2.95, 3.01, 3.05,     &
             3.13, 3.17, 3.21, 3.25, 3.27,     &
             3.35, 3.42, 3.52, 3.57, 3.63,     &
             3.69, 3.81, 3.95, 4.04, 4.11,     &
             4.28, 4.49, 4.61, 4.86, 5.12,     &
             5.38, 5.75, 6.17, 6.72, 7.35,     &
             7.98, 8.61, 9.24, 9.87/)


  character(len=max_varname_length), dimension(naero_gocart_wrf), parameter :: &
        aeronames_gocart_wrf=(/&
        'sulf      ','BC1       ','BC2       ','OC1       ',&
        'OC2       ','DUST1     ','DUST2     ','DUST3     ',&
        'DUST4     ','DUST5     ','SEAS1     ','SEAS2     ',&
        'SEAS3     ','SEAS4     ','P25       '/)

  real(r_single), allocatable, dimension(:,:,:) :: pm2_5_guess,&
       aerotot_guess

  character(len=maxstr) :: in_fname,out_fname,incr_fname
  
contains

  function obs2model_anowbufr_pm()

!   prgmmr: pagowski                date: 2010-09-13

! assigns conversion factors between AIRNow pm units and model units

    real(r_kind) :: obs2model_anowbufr_pm
    
    if (cmaq_regional) then 
       obs2model_anowbufr_pm=cmaq_pm
    elseif (wrf_mass_regional) then
       obs2model_anowbufr_pm=wrf_chem_pm
    elseif (fv3_cmaq_regional) then
!Hongli 
       obs2model_anowbufr_pm=fv3_cmaq_pm
       write(6,*)'!!! Please check fv3_cmaq_pm chem model in chemmod.f90'
    else
       write(6,*)'unknown chem model. stopping in chemmod.f90'
       call stop2(414)
    endif
    
  end function obs2model_anowbufr_pm


  subroutine init_pm2_5_guess

!   prgmmr: pagowski                date: 2010-09-13
!   allocates and assigns initial values to pm2_5_guess
    integer(i_kind) :: i,j,k

    allocate(pm2_5_guess(lat2,lon2,nsig))

    do i=1,lon2
       do j=1,lat2
          do k=1,nsig
             pm2_5_guess(j,i,k)=tiny_single
          enddo
       enddo
    enddo
    
  end subroutine init_pm2_5_guess

  subroutine init_aerotot_guess

!   prgmmr: pagowski                date: 2010-09-13
!   allocates and assigns initial values to aerotot_guess
    integer(i_kind) :: i,j,k

    allocate(aerotot_guess(lat2,lon2,nsig))

    do i=1,lon2
       do j=1,lat2
          do k=1,nsig
             aerotot_guess(j,i,k)=tiny_single
          enddo
       enddo
    enddo
    
  end subroutine init_aerotot_guess


  subroutine init_chem

!   prgmmr: pagowski                date: 2010-09-13
! program history log: 
!  2019-03-21 Martin  - cleaned up contribution from S-W Wei at UAlbany     

!initialiazes default values to &CHEM namelist parameters
    
    berror_chem=.false.
    oneobtest_chem=.false.
    maginnov_chem=30_r_kind
    magoberr_chem=2_r_kind
    oneob_type_chem='pm2_5'
    oblat_chem=45_r_kind
    oblon_chem=270_r_kind
    obpres_chem=1000_r_kind
    diag_incr=.false.
    elev_tolerance=500_r_kind
    tunable_error=0.5_r_kind
    in_fname='cmaq_input.bin'
    out_fname='cmaq_output.bin'
    incr_fname='chem_increment.bin'
!!! GOCART-GEOS5 GOCART CMAQ
    crtm_aerosol_model = "GOCART"
    crtm_aerosolcoeff_format = "Binary"
    crtm_aerosolcoeff_file   = "AerosolCoeff.bin"
    laeroana_gocart = .false.
    laeroana_fv3cmaq = .false.
    laod_crtm_cmaq = .false.
    l_aoderr_table = .false.
    iaod_crtm_cmaq = 0
    iaod_recs_cmaq  = 0
    icvt_cmaq_fv3   = 1
    raod_radius_mean_scale = 1.0
    raod_radius_std_scale  = 1.0
    aod_qa_limit = 3
    luse_deepblue = .false.

    wrf_pm2_5=.false.

    aero_ratios=.false.
    lread_ext_aerosol = .false.

  end subroutine init_chem

  subroutine oneobschem(nread,ndata,nodata,gstime,&
        infile,obstype,lunout,sis,nobs)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  one obs test for in-situ chem
!   prgmmr: pagowski                date: 2010-11-18
!
! program history log:
!   2010-11-18  pagowski
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
!   input argument list:
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nodata   - number of individual "obstype" observations retained for !further processing
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use constants, only: zero,one,deg2rad,rad2deg
    use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
         tll2xy,txy2ll,rlats,rlons
    use convinfo, only: nconvtype,icuse,ioctype
    use mpimod, only: npe
    
    implicit none
    
! declare passed variables
    character(len=*),intent(in   ) :: obstype
    character(len=*),intent(out) :: infile
    integer(i_kind) ,intent(in   ) :: lunout
    integer(i_kind) ,intent(inout) :: nread,ndata,nodata
    real(r_kind)    ,intent(in   ) :: gstime
    character(len=*),intent(in   ) :: sis
    integer(i_kind),dimension(npe) ,intent(inout) :: nobs
    
    
! declare local parameters
    
    integer(i_kind), parameter :: nsid=1,nxob=2,&
          nyob=3,ndhr=4,ntyp=5,ncopopm=6
!see headr input format below
    
    integer(i_kind), parameter:: nchanl=0,nreal=ilone
    
    real(r_kind),parameter :: r100 = 100.0_r_kind
    real(r_kind),parameter :: r360 = 360.0_r_kind
    
! declare local variables
    logical outside
    
    integer(i_kind) i
    integer(i_kind) ikx
    
    real(r_kind) :: tdiff,obstime
    real(r_kind) :: dlat,dlon,obserror,dlat_earth,dlon_earth
    
    real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
    integer(i_kind) ntest
    
    integer(i_kind) k,site_char,site_id
    real(r_kind) :: conc,site_elev
    real(r_kind), dimension(nreal,1):: cdata_all
    
    site_id=123456789
    site_char=1 ! set unknown site character
    site_elev=elev_missing ! set unknown site elevation
    
!**************************************************************************
! initialize variables
    infile='namelist'
    disterrmax=zero
    ntest=1
    nread=1
    ndata = 1
    nodata = 1
    
    if(oblon_chem >= r360)  oblon_chem = oblon_chem - r360
    if(oblon_chem <  zero)  oblon_chem = oblon_chem + r360
    
    dlon_earth=oblon_chem*deg2rad
    dlat_earth=oblat_chem*deg2rad
    obstime=gstime
    tdiff=zero
    conc=zero ! this is unimportant since only innovation counts
  
    if(regional)then
       
       call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
       
       if(diagnostic_reg) then
          
          call txy2ll(dlon,dlat,rlon00,rlat00)
          cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
               (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
          cdist=max(-one,min(cdist,one))
          disterr=acos(cdist)*rad2deg
          disterrmax=max(disterrmax,disterr)
       end if
       
       if(outside) then 
          write(6,*)'oneobtest_chem outside domain - stopping'
          call stop2(511)
       endif
       
    else
       dlat = dlat_earth
       dlon = dlon_earth
       call grdcrd1(dlat,rlats,nlat,1)
       call grdcrd1(dlon,rlons,nlon,1)
    endif
    
    do i = 1, nconvtype
       if (obstype == ioctype(i) .and. abs(icuse(i))== 1) ikx=i
    end do
           
    obserror=magoberr_chem
    
    cdata_all(iconc,ndata)  = conc                    ! pm2_5 obs    
    cdata_all(ierror,ndata) = obserror                ! pm2_5 obs error
    cdata_all(ilat,ndata)   = dlat                    ! grid relative latitude 
    
    cdata_all(ilon,ndata)   = dlon                    ! grid relative longitude 
    cdata_all(itime,ndata)  = obstime                 ! time of obs
    cdata_all(iid,ndata)    = site_id                 ! site id
    cdata_all(ielev,ndata)  = site_elev               ! elevation
    cdata_all(isite,ndata)  = site_char               ! site character
    cdata_all(iikx,ndata)   = ikx                     ! ordered number in convinfo table
    cdata_all(ilate,ndata)  = dlat_earth*rad2deg      ! earth relative latitude (degrees)
    cdata_all(ilone,ndata)  = dlon_earth*rad2deg      ! earth relative longitude (degrees)
    
    
! write header record and data to output file for further processing
    call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
    
    return
    
  end subroutine oneobschem


  function upper2lower(str) result(string)

    implicit none
    
    character(*), intent(in) :: str
    character(len(str))      :: string

    integer :: ic, i

    character(26), parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'

!   lowcase each letter if it is lowecase
    string = str
    do i = 1, len_trim(str)
        ic = index(upper, str(i:i))
        if (ic > 0) string(i:i) = lower(ic:ic)
    end do

  end function upper2lower

  function lower2upper(str) result (string)

    implicit none
    
    character(*), intent(in) :: str
    character(len(str))      :: string

    integer :: ic, i

    character(26), parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'

!   lowcase each letter if it is lowecase
    string = str
    do i = 1, len_trim(str)
        ic = index(lower, str(i:i))
        if (ic > 0) string(i:i) = upper(ic:ic)
    end do

  end function lower2upper

end module chemmod
