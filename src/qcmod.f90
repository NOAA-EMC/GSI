module qcmod
!$$$  module documentation block
!                .      .    .                                       .
! module:    qcmod
!   prgmmr: kleist           org: w/nmc20             date: 2003-09-30
!
! abstract: module containing data quality control variables
!
! program history log:
!   2003-09-30  kleist
!   2004-05-18  kleist, documentation
!   2004-07-23  derber - modify to include conventional sst
!   2004-10-12  parrish - modifications for nonlinear qc
!   2004-12-02  treadon - initialize b_ref and pg_ref
!   2005-01-20  okamoto - add ermax for ssmi/amsre/ssmis
!   2005-04-07  treadon - add logical flags to indicate nonlinear qc
!                         is on (=.true.) or off (=.false.)
!   2005-05-27  derber  - level output change
!   2005-08-03  derber  - remove qc parameters for conventional data
!   2005-09-29  derber  - remove qc parameters for sat and pcp data, move cg_term to constants 
!   2006-01-31  derber  - correct bug in upprof and dwprof loop logical test
!   2006-05-24  treadon - add vadfile to carry name of vad wind bufr file
!   2006-05-22  su - add noiqc flag
!   2006-07-28  derber  - add dfact1, initialize
!   2006-08-07  treadon - remove nlnqc_oz (not used)
!   2007-04-16       su - add c_varqc for determining the spped to turn on var. qc
!   2008-06-03  treadon - add use_poq7
!
! subroutines included:
!   sub init_qcvars
!   sub errormod
!   sub qc_ssmi         - qc ssmi data
!   sub qc_seviri       - qc seviri data
!   sub qc_ssu          - qc ssu data
!   sub qc_avhrr        - qc avhrr data
!   sub qc_goesimg      - qc goesimg data
!   sub qc_msu          - qc msu data
!   sub qc_irsnd        - qc ir sounder data (hirs,goesndr,iasi,airs)
!   sub qc_amsua        - qc amsua data
!   sub qc_mhs          - qc msu, amsub and hsb data
!
! remarks: variable definitions below
!   def dfact           - factor for duplicate obs at same location for conv. data
!   def dfact1          - time factor for duplicate obs at same location for conv. data
!   def repe_dw         - factor for error in radar doppler winds
!   def erradar_inflate - radar error inflation factor
!   def npres_print     - number of levels for print
!   def ptop,pbot       - arrays containing top pressure and bottom pressure of print levels
!   def ptopq,pbotq     - arrays containing top pressure and bottom pressure of print levels for q
!   def ptopo3,pboto3   - arrays containing top pressure and bottom pressure of print levels for o3 levels
!   def vadfile         - local name of bufr file containing vad winds (used by read_radar)
!   def use_poq7        - if true, accept sbuv/2 obs with profile ozone quality flag 7
!
!    following used for nonlinear qc:
!
!   def nlnqc_iter   - logical flag (T=nonlinear qc on, F=nonlinear qc off) for iteration
!
!   def noiqc        - logic flag for oiqc, noiqc='false' with oiqc on
!
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  use constants, only: zero,quarter,half,one,two,three,four,five,tiny_r_kind,rd,grav
  use constants, only: r0_01,r0_02,r0_03,r0_04,r0_05,r10,r60,r100,h300,r400,r1000,r2000,r2400,r4000
  use constants, only: deg2rad,rad2deg,t0c
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_qcvars
  public :: errormod
  public :: qc_ssmi
  public :: qc_seviri
  public :: qc_ssu
  public :: qc_avhrr 
  public :: qc_goesimg
  public :: qc_msu
  public :: qc_irsnd
  public :: qc_amsua
  public :: qc_mhs
! set passed variables to public
  public :: npres_print,nlnqc_iter,varqc_iter,pbot,ptop,c_varqc,repe_dw
  public :: use_poq7,noiqc,vadfile,dfact1,dfact,erradar_inflate
  public :: pboto3,ptopo3,pbotq,ptopq
  public :: igood_qc,ifail_crtm_qc,ifail_satinfo_qc,ifail_interchan_qc,ifail_gross_qc

  logical nlnqc_iter
  logical noiqc
  logical use_poq7

  character(10):: vadfile
  integer(i_kind) npres_print
  real(r_kind) dfact,dfact1,repe_dw,erradar_inflate,c_varqc
  real(r_kind) varqc_iter
  real(r_kind),allocatable,dimension(:)::ptop,pbot,ptopq,pbotq,ptopo3,pboto3

!  Definition of id_qc flags
!  Good Observations (0)
  integer(i_kind),parameter:: igood_qc=0

!  SETUPRAD or general flags (0-10)
!  Reject due to flag in radinfo in setuprad
  integer(i_kind),parameter:: ifail_satinfo_qc=1
!  Failure in CRTM in setuprad
  integer(i_kind),parameter:: ifail_crtm_qc=2
!  Reject due to gross check failure in setuprad
  integer(i_kind),parameter:: ifail_gross_qc=3
!  Reject due to interchannel check (i.e., if one channel fails in group whole group thrown out) in setuprad
  integer(i_kind),parameter:: ifail_interchan_qc=4
!  Reject due to not using over this surface in qc routine
  integer(i_kind),parameter:: ifail_surface_qc=5
!  Reject due to gross check in specific qc routine                                                                          
  integer(i_kind),parameter:: ifail_gross_routine_qc=6
!  Reject due to cloud > limit for channel in qc routine
  integer(i_kind),parameter:: ifail_cloud_qc=7
!  Reject due to inaccurate emissivity/surface temperature estimate in qc routine
  integer(i_kind),parameter:: ifail_emiss_qc=8
!  Reject due to observations being out of range in qc routine
  integer(i_kind),parameter:: ifail_range_qc=9

!  Failures specific to qc routine start at 50 and the numbers overlap
!  QC_SSMI failures 
!  Reject due to krain type not equal to 0 in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_krain_qc=50
!  Reject due to ierrret > 0 in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_ierrret_qc=51
!  Reject due to tpwc < 0 in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_tpwc_qc=52
!  Reject due to sgagl < 25. and amsre_low in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_sgagl_qc=53
!  Reject in topography check in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_topo_ssmi_qc=54
!  Reject because varinv < tiny in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_varinv_qc=55
!  Reject because ch2 check in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_ch2_qc=56
!  Reject because scattering over land in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_scatt_qc=57

! QC_IRSND        
!  Reject because wavenumber > 2400 in subroutine qc_irsnd
  integer(i_kind),parameter:: ifail_2400_qc=50
!  Reject because wavenumber > 2000 in subroutine qc_irsnd
  integer(i_kind),parameter:: ifail_2000_qc=51
!  Reject because goes sounder and satellite zenith angle > 60 in subroutine qc_irsnd
  integer(i_kind),parameter:: ifail_satzen_qc=52
!  Reject because of surface emissivity/temperature influence in subroutine qc_irsnd                                     
  integer(i_kind),parameter:: ifail_sfcir_qc=53

! QC_AMSUA          
!  Reject because factch6 > limit in subroutine qc_amsua
  integer(i_kind),parameter:: ifail_factch6_qc=50
!  Reject because factch4 > limit in subroutine qc_amsua
  integer(i_kind),parameter:: ifail_factch4_qc=51

! QC_MHS          
!  Reject because fact1 > limit in subroutine qc_mhs
  integer(i_kind),parameter:: ifail_fact1_qc=50

! QC_SSU          

! QC_MSU          

! QC_seviri          

! QC_avhrr          

! QC_goesimg          
!  Reject because of standard deviation in subroutine qc_goesimg
  integer(i_kind),parameter:: ifail_std_goesimg_qc=50 

contains
 
  subroutine init_qcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_qcvars
!   prgmmr: kleist           org: np20                date: 2003-09-30
!
! abstract: initialize variables used in data quality control
!
! program history log:
!   2003-09-30  kleist
!   2004-05-18  kleist, documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2005-01-20  okamoto - add ermax for ssmi/amsre/ssmis
!   2005-02-18  treadon - reduce ps gross limit from 10.0 to 5.0
!   2005-03-08  cucurull - reduce gps ro gross limit from 10.0 to 3.0
!   2005-06-03  cucurull - increase gps ro gross limit from 3.0 to 10.0
!   2007-01-09  sienkiewicz - new levels for ozone stat printout
!   2008-04-23  safford  - rm unused parameter
!   2008-09-05  lueken   - merged ed's changes into q1fy09 code
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    npres_print = 12_i_kind
    allocate(ptop(npres_print),pbot(npres_print),ptopq(npres_print), &
             pbotq(npres_print),ptopo3(npres_print),pboto3(npres_print))
    
! Set pressure level groupings.  There are npres_print groupings
    ptop(1) = r1000       ;    pbot(1)=  1200.0_r_kind
    ptop(2) = 900.0_r_kind;    pbot(2)=  999.9_r_kind
    ptop(3) = 800.0_r_kind;    pbot(3)=  899.9_r_kind
    ptop(4) = 600.0_r_kind;    pbot(4)=  799.9_r_kind
    ptop(5) = 400.0_r_kind;    pbot(5)=  599.9_r_kind
    ptop(6) = h300        ;    pbot(6)=  399.9_r_kind
    ptop(7) = 250.0_r_kind;    pbot(7)=  299.9_r_kind
    ptop(8) = 200.0_r_kind;    pbot(8)=  249.9_r_kind
    ptop(9) = 150.0_r_kind;    pbot(9)=  199.9_r_kind
    ptop(10)= 100.0_r_kind;    pbot(10)= 149.9_r_kind
    ptop(11)= 50.0_r_kind ;    pbot(11)= 99.9_r_kind
    ptop(12)= zero        ;    pbot(12)= 2000.0_r_kind

    ptopq(1)=  r1000       ;   pbotq(1)=  1200.0_r_kind
    ptopq(2)=  950.0_r_kind;   pbotq(2)=  999.9_r_kind
    ptopq(3)=  900.0_r_kind;   pbotq(3)=  949.9_r_kind
    ptopq(4)=  850.0_r_kind;   pbotq(4)=  899.9_r_kind
    ptopq(5)=  800.0_r_kind;   pbotq(5)=  849.9_r_kind
    ptopq(6)=  700.0_r_kind;   pbotq(6)=  799.9_r_kind
    ptopq(7)=  600.0_r_kind;   pbotq(7)=  699.9_r_kind
    ptopq(8)=  500.0_r_kind;   pbotq(8)=  599.9_r_kind
    ptopq(9)=  400.0_r_kind;   pbotq(9)=  499.9_r_kind
    ptopq(10)= h300        ;   pbotq(10)= 399.9_r_kind
    ptopq(11)= zero        ;   pbotq(11)= 299.9_r_kind
    ptopq(12)= zero        ;   pbotq(12)= 2000.0_r_kind

    ptopo3(1) = 120.0_r_kind;  pboto3(1) = h300
    ptopo3(2) =  70.0_r_kind;  pboto3(2) = 119.9_r_kind
    ptopo3(3) =  40.0_r_kind;  pboto3(3) =  69.9_r_kind
    ptopo3(4) =  25.0_r_kind;  pboto3(4) =  39.9_r_kind
    ptopo3(5) =  12.0_r_kind;  pboto3(5) =  24.99_r_kind
    ptopo3(6) =   7.0_r_kind;  pboto3(6) =  11.99_r_kind
    ptopo3(7) =  four       ;  pboto3(7) =   6.99_r_kind
    ptopo3(8) =   2.5_r_kind;  pboto3(8) =   3.99_r_kind
    ptopo3(9) =   1.2_r_kind;  pboto3(9) =  2.499_r_kind
    ptopo3(10) =  0.7_r_kind;  pboto3(10) =  1.199_r_kind
    ptopo3(11) =  0.4_r_kind;  pboto3(11) =  0.699_r_kind
    ptopo3(12) = zero       ;  pboto3(12) = 2000.0_r_kind

    dfact    = zero
    dfact1   = three
    repe_dw  = one
    varqc_iter=one

    erradar_inflate   = one

    nlnqc_iter= .false.
    noiqc = .false.
    c_varqc=one

    vadfile='none'

    use_poq7 = .false.

    return
  end subroutine init_qcvars

  subroutine errormod(pq,vq,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    errormod
!   prgmmr: derber           org: np23                date: 2003-09-30
!
! abstract: adjust observation error for conventional obs
!
! program history log:
!   2003-09-30  derber
!   2004-05-18  kleist, documentation
!   2004-10-26  kleist - add 0.5 half-layer factor
!   2006-02-15  treadon - add (l==levs,1) exit to upprof and dwprof loops
!   2006-12-20  Sienkiewicz  multiply tiny_r_kind in errout div-by-zero
!                            check by expected largest value for numerator
!                            max(2*vmax) = max(dpres) ~= 5 cb
!   2008-04-23  safford - rm unused vars and uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     pq     - pressure quality mark
!     vq     - observation quality mark (t,q,wind)
!     levs   - number of levels in profile for observation
!     plevs  - observation pressures
!     errout - observation error 
!     k      - observation level 
!     presl  - model pressure at half sigma levels
!     dpres  - delta pressure between model pressure levels
!     nsig   - number of vertical levels
!     lim_qm - qc limit 
!
!   output argument list:
!     errout - adjusted observation error
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    integer(i_kind)                     ,intent(in   ) :: levs,k,nsig,lim_qm
    real(r_kind)   ,dimension(255)      ,intent(in   ) :: plevs
    real(r_kind)   ,dimension(nsig)     ,intent(in   ) :: presl
    real(r_kind)   ,dimension(nsig-1)   ,intent(in   ) :: dpres
    integer(i_kind),dimension(255)      ,intent(in   ) :: pq,vq
    real(r_kind)                        ,intent(inout) :: errout

    integer(i_kind) n,l,ilev
    real(r_kind):: vmag,pdiffu,pdiffd,con
    
    errout=one
    if(levs == 1)return
    ilev=1
    do n=2,nsig-1
       if(plevs(k) < presl(n))ilev=n
    end do
    con=grav*500._r_kind/(273._r_kind*rd)
    vmag=min(max(half*dpres(ilev),r0_02*presl(ilev)),con*plevs(k))

!   vmag=max(half*dpres(ilev),r0_02*presl(ilev))
    pdiffu=vmag
    pdiffd=vmag
    if(pq(k) < lim_qm .and. vq(k) < lim_qm)then
! Move up through the profile.  
       l=k

! Array plevs is only defined from l=1 to l=levs.  Hence the check below
       if (l+1<=levs) then
          upprof: do while (abs(plevs(k)-plevs(l+1)) < vmag .and. l <= levs-1) 
             l=l+1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffu=abs(plevs(k)-plevs(l))
                exit upprof
             end if
             if (l==levs) exit upprof
          end do upprof
       endif
        
! Reset the level and move down through the profile
       l=k

! The check (l>=2) ensures that plevs(l-1) is defined
       if (l>=2_i_kind) then
          dwprof: do while (abs(plevs(l-one)-plevs(k)) < vmag .and. l >= 2_i_kind) 
             l=l-1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffd=abs(plevs(l)-plevs(k))
                exit dwprof
             end if
             if (l==1) exit dwprof
          end do dwprof
       endif

! Set adjusted error
       errout=sqrt(two*vmag/max(pdiffd+pdiffu,five*tiny_r_kind))

! Quality marks indicate bad data.  Set error to large value.
    else
       errout=1.e6_r_kind
    end if

    return
  end subroutine errormod
subroutine qc_ssmi(nchanl,   &
     sfchgt,luse,sea,ice,snow,mixed, &
     ts,pems,ierrret,kraintype,tpwc,clw,sgagl,    &
     tbc,tbcnob,tb_ges,ssmi,amsre_low,amsre_mid,amsre_hig,ssmis, &
     varinv,errf,aivals,id_qc )

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_ssmi      QC for ssmi/amsre
!
!   prgmmr: okamoto          org: np23            date: 2004-12-01
!
! abstract: set quality control criteria for SSM/I,AMSR-E,SSMIS(UKMO)
!
! program history log:
!     2004-12-01  okamoto 
!     2005-02-17  derber  clean up surface flags
!     2005-03-04  treadon  - correct underflow error for varinv
!     2005-09-05  derber - allow max error to change by channel
!     2005-10-07  Xu & Pawlak - add SSMIS qc code, add documentation
!     2005-10-20  kazumori - add AMSR-E qc code, add documentation
!     2006-02-03  derber  - modify for new obs control and stats         
!     2006-04-26  kazumori  - change clw theshold for AMSR-E
!     2006-04-27  derber - modify to do single profile - fix bug
!     2006-07-27  kazumori - modify AMSR-E qc and input of the subroutine
!     2006-12-01  derber - modify id_qc flags
!     2007-01-24  kazumori - modify SSMIS qc and input of the subroutine
!     2008-04-23  safford  - rm unused vars              
!     2010-07-16  yan      - update the qc criteria for ssmis
!      1) remove 'ssmis_uas,ssmis_las,ssmis_env,ssmis_img' 
!      2) add an input 'tbc' which is used to detect cloud-affected data over land
!      3) update the thresholds of cloud detection for some of the cloud-affected channels
!      4) add a new qc for ssmis data over oceans
!      5) update the qc criteria of the ssmis data over non-ocean surfaces
!      6) realx the qc criteria for the data at channels from 1 to 2
!      7) add two references
!
! input argument list:
!     nchanl  - number of channels per obs
!     sfchgt  - surface height (not use now)
!     luse    - logical use flag
!     sea     - logical, sea flag
!     ice     - logical, ice flag
!     snow    - logical, snow flag
!     mixed   - logical, mixed zone flag
!     ts      - skin temperature
!     pems    - surface emissivity
!     ierrret - result flag of retrieval_mi
!     kraintype - [0]no rain, [others]rain ; see retrieval_mi
!     clw     - retrieve clw [kg/m2]
!     sgagl   - sun glint angle [degrees]
!     tpwc    - retrieve tpw [kg/m2]
!     tbc     - Obs - Back TBB with bias correction
!     tbcnob  - Obs - Back TBB without bias correction
!     tb_ges  - simulated TBB
!     ssmi    - logical true if ssmi is processed 
!     ssmis    - logical true if ssmis is processed 
!     amsre_low   - logical true if amsre_low is processed 
!     amsre_mid   - logical true if amsre_mid is processed 
!     amsre_hig   - logical true if amsre_hig is processed 
!
! NOTE! if retrieved clw/tpwc not available over ocean,set -9.99e+11, 
!       but 0 over land/mixed/ice
!
! output argument list:
!     varinv  - observation weight (modified obs var error inverse)
!     errf    - criteria of gross error
!     aivals  - number of data not passing QC
!     id_qc   - qc index - see qcmod definitions
!
!
!
!     ... possibe QC to add ..........................
!     * decrease varinv at last several scan position
!  
!     clwcutofx is used to set cloud qc threshold  (kg/m2) 
!     Refernces:
!     (1) Weng, F. and N. C. Grody, 1994: Retrieval of cloud liquid water using the special sensor microwave
!       imager (SSM/I), J. Geophys. Res., 99, 25,535 -25, 551.
!     (2) Yan, B., F. Weng and J. Derber, 2010: An Effort toward Assimilation of F16 Special Sensor Microwave
!       Imager/Sounder Data into the NCEP Global Forecast System, to be submitted to Journal of Weather and
!       Forecasting
!
!     from Fuzhong Weng (need better reference) 
!     ................................................
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none


! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nchanl
  integer(i_kind)                  ,intent(in   ) :: kraintype,ierrret
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc

  logical                          ,intent(in   ) :: sea,snow,ice,mixed,luse
  logical                          ,intent(in   ) :: ssmi,amsre_low,amsre_mid,amsre_hig,ssmis

  real(r_kind)                     ,intent(in   ) :: sfchgt,tpwc,clw,sgagl
  real(r_kind)   ,dimension(nchanl),intent(in   ) :: ts,pems
  real(r_kind)   ,dimension(nchanl),intent(in   ) :: tbc,tbcnob,tb_ges

  real(r_kind)   ,dimension(nchanl),intent(inout) :: varinv,errf
  real(r_kind)   ,dimension(40)    ,intent(inout) :: aivals

! Declare local variables
  integer(i_kind) :: l,i
  real(r_kind) :: efact,vfact,dtempf,fact,dtbf,term
  real(r_kind),dimension(nchanl) :: demisf_mi,clwcutofx 
  real(r_kind) :: pred9,pred10,pred11

!------------------------------------------------------------------

! Set cloud qc criteria  (kg/m2) :  reject when clw>clwcutofx
  if(ssmi) then
     clwcutofx(1:nchanl) =  &  
          (/0.35_r_kind, 0.35_r_kind, 0.27_r_kind, 0.10_r_kind, &
          0.10_r_kind, 0.024_r_kind, 0.024_r_kind/) 
  else if(amsre_low.or.amsre_mid.or.amsre_hig) then
     clwcutofx(1:nchanl) =  &  
          (/0.350_r_kind, 0.350_r_kind, 0.350_r_kind, 0.350_r_kind, &
          0.300_r_kind, 0.300_r_kind, 0.250_r_kind, 0.250_r_kind, &
          0.100_r_kind, 0.100_r_kind, 0.020_r_kind, 0.020_r_kind/) 
!    --- amsre separate channel treatment depend on FOV
     if(amsre_low) varinv(5:12)=zero
     if(amsre_mid) varinv(1:4)=zero
     if(amsre_mid) varinv(11:12)=zero
     if(amsre_hig) varinv(1:10)=zero
  else if(ssmis) then
     clwcutofx(1:nchanl) =  &  !kg/m2  reject when clw>clwcutofx
            (/ 0.10_r_kind, 0.20_r_kind, &
               0.60_r_kind, 2.00_r_kind, &
               2.00_r_kind, 2.00_r_kind, &
               2.00_r_kind, 0.10_r_kind, &
               0.10_r_kind, 0.10_r_kind, &
               0.10_r_kind, 0.20_r_kind, &
               0.20_r_kind, 0.20_r_kind, &
               0.20_r_kind, 0.20_r_kind, &
               0.10_r_kind, 0.10_r_kind, &
               10.0_r_kind,10.0_r_kind, &
               10.0_r_kind,10.0_r_kind, &
               10.0_r_kind,10.0_r_kind  /)
  end if
  dtempf = half
  demisf_mi(1:nchanl) = 0.01_r_kind

! Loop over observations.

  efact     =one
  vfact     =one

!    Over sea               
  if(sea) then 

!    dtb/rain/clw qc using SSM/I RAYTHEON algorithm
     if( ierrret > 0  .or. kraintype /= 0 .or. tpwc<zero ) then 
        efact=zero; vfact=zero
        if(luse) then
          aivals(8) = aivals(8) + one
           
          do i=1,nchanl
             if( id_qc(i)== igood_qc .and. kraintype/= 0) id_qc(i)=ifail_krain_qc
             if( id_qc(i)== igood_qc .and. ierrret > 0)   id_qc(i)=ifail_ierrret_qc
             if( id_qc(i)== igood_qc .and. tpwc< zero )   id_qc(i)=ifail_tpwc_qc
          end do 
        end if
     else if (ssmis) then  ! in case of ssmis bad data or cloud-contaminated data
        do i = 1,24
           if( abs(tbcnob(i)) >= 3.5_r_kind) then
              varinv(i) = zero
              id_qc(i) = ifail_gross_routine_qc
           end if
        enddo

     else if(amsre_low .and. sgagl < 25.0_r_kind) then

! ---- sun glint angle qc (for AMSR-E)

        varinv(1:4)=zero
        do i=1,4
           if(id_qc(i) == igood_qc)id_qc(i) = ifail_sgagl_qc
        end do
        if(luse) aivals(11) = aivals(11) + one

     else if(amsre_low .or. amsre_mid .or. amsre_hig)then

! ---- dtb threshold qc for AMSR-E due to an inaccuracy of emis model

        if( abs(tbcnob(1)) > 6.0_r_kind .or. &
            abs(tbcnob(2)) > 6.0_r_kind .or. &
            abs(tbcnob(3)) > 6.0_r_kind .or. &
            abs(tbcnob(4)) > 6.0_r_kind .or. &
            abs(tbcnob(5)) > 6.0_r_kind .or. &
            abs(tbcnob(6)) > 8.0_r_kind .or. &
            abs(tbcnob(7)) > 8.0_r_kind .or. &
            abs(tbcnob(8)) > 10.0_r_kind .or. &
            abs(tbcnob(9)) > 6.0_r_kind .or. &
            abs(tbcnob(10)) > 6.0_r_kind) then
           do i=1,nchanl
              varinv(i)=zero
              id_qc(i)=ifail_emiss_qc
           end do
           if(luse) aivals(13) = aivals(13) + one
        end if

     else if(clw > zero)then

!      If dtb is larger than demissivity and dwmin contribution, 
!      it is assmued to be affected by  rain and cloud, tossing it out
        do l=1,nchanl

!          clw QC using ch-dependent threshold (clwch)
           if( clw > clwcutofx(l) ) then
              varinv(l)=zero
              if(luse) then
                 aivals(10) = aivals(10) + one
                 if(id_qc(l)== igood_qc) then
                    id_qc(l)=ifail_cloud_qc
                    aivals(9)=aivals(9) + one
                 end if
              end if
           end if
        end do  !l_loop
     end if

!    Use data not over over sea
  else  !land,sea ice,mixed

!   Reduce q.c. bounds over higher topography
     if ( .not. ssmis) then
!    demisf_mi=demisf_mi*0.7_r_kind   ! not necessary since data not used
        efact=zero
        vfact=zero
        do i=1,nchanl
           if(id_qc(i)== igood_qc ) id_qc(i)=ifail_surface_qc
        end do

        if (sfchgt > r2000) then
           fact = r2000/sfchgt
           efact = fact*efact
           vfact = fact*vfact
        end if

     else 
       !Use dtbc at 52.8 GHz to detect cloud-affected data
        if (abs(tbc(2)) >= 1.5_r_kind) then  ! the data at cloud-affected channels are not used
            do i =1,2
              varinv(i)  = zero
              if(id_qc(i)== igood_qc ) id_qc(i)=ifail_ch2_qc
            end do
            do i =12,16
              varinv(i)  = zero
              if(id_qc(i)== igood_qc ) id_qc(i)=ifail_ch2_qc
            end do
        endif
       !General qc criteria for all channels
        do i = 1,24
           if( abs(tbcnob(i)) >= 3.5_r_kind) then
              varinv(i) = zero
              if(id_qc(i)== igood_qc ) id_qc(i)=ifail_gross_routine_qc
           end if
        enddo

        if(mixed) then
           do i=1,3
             varinv(i)=zero
             if(id_qc(i)== igood_qc) id_qc(i)=ifail_surface_qc
           end do
           do i=8,18
             varinv(i)=zero
             if(id_qc(i)== igood_qc) id_qc(i)=ifail_surface_qc
           end do
        end if

        if (sfchgt > r2000) then
           varinv(9)=zero
           if(id_qc(9)== igood_qc) id_qc(9)=ifail_topo_ssmi_qc
        end if
        if (sfchgt > r4000) then
           varinv(3)=zero
           if(id_qc(3)== igood_qc) id_qc(3)=ifail_topo_ssmi_qc
           varinv(10)=zero
           if(id_qc(10)== igood_qc) id_qc(10)=ifail_topo_ssmi_qc
        end if


     end if
  end if

  if(ssmis)then
  ! scattering affected data removal
     pred9  =271.252327_r_kind - 0.485934_r_kind*tb_ges(17) + 0.473806_r_kind*tb_ges(8)
     pred10 =272.280341_r_kind - 0.413688_r_kind*tb_ges(17) + 0.361549_r_kind*tb_ges(8)
     pred11 =278.824902_r_kind - 0.400882_r_kind*tb_ges(17) + 0.270510_r_kind*tb_ges(8)
     if(pred9  - tbcnob(9)  - tb_ges(9)  > two) then
       varinv(9) =zero
       if(id_qc(9)== igood_qc) id_qc(9)=ifail_scatt_qc
     end if
     if(pred10 - tbcnob(10) - tb_ges(10) > two) then
       varinv(10)=zero
       if(id_qc(10)== igood_qc) id_qc(10)=ifail_scatt_qc
     end if
     if(pred11 - tbcnob(11) - tb_ges(11) > two) then
       varinv(11)=zero
       if(id_qc(11)== igood_qc) id_qc(11)=ifail_scatt_qc
     end if
  end if


! Generate q.c. bounds and modified variances.
  do l=1,nchanl

     errf(l)   = efact*errf(l)
     varinv(l) = vfact*varinv(l)
     
     if (varinv(l) > tiny_r_kind) then
        dtbf = demisf_mi(l)*abs(pems(l)) + dtempf*abs(ts(l))
        term = dtbf*dtbf
        if(term>tiny_r_kind) varinv(l)=one/(one/varinv(l)+term)
     else if(luse  .and. id_qc(l)== igood_qc )then
        id_qc(l)=ifail_varinv_qc
     endif
        

  end do ! l (ch) loop end
      

  return
end subroutine qc_ssmi
subroutine qc_irsnd(nchanl,is,ndat,nsig,sea,land,ice,snow,luse,goessndr,   &
     zsges,cenlat,frac_sea,pangs,trop5,zasat,tsavg5,tbc,tb_obs,tnoise,     &
     wavenumber,ptau5,prsltmp,tvp,temp,emissivity_k,ts,                    &
     id_qc,aivals,errf,varinv,varinv_use,cld,cldp)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_irsnd    QC for ir sounder data(hirs,goessndr,airs,iasi)
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for ir sounder data (hirs, 
!          goessndr, airs, iasi)
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     nsig         - number of model levels
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     goessndr     - logical flag - if goessndr data - true
!     zsges        - elevation of guess
!     cenlat       - latitude of observation
!     frac_sea     - fraction of grid box covered with water
!     pangs        - solar zenith angle
!     trop5        - tropopause pressure
!     zasat        - satellite zenith angle
!     tsavg5       - surface skin temperature
!     tbc          - simulated - observed BT with bias correction
!     tb_obs       - observed Brightness temperatures
!     tnoise       - channel noise array
!     wavenumber   - array of channel wavenumbers
!     ptau5        - transmittances as a function of level and channel
!     prsltmp      - array of layer pressures in vertical (surface to toa)
!     tvp          - array of temperatures in vertical (surface to toa)
!     temp         - temperature sensitivity array
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!     cld          - cloud fraction
!     cldp         - cloud pressure
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse,goessndr
  integer(i_kind),                    intent(in   ) :: nchanl,is,ndat,nsig
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges,cenlat,frac_sea,pangs,trop5
  real(r_kind),                       intent(in   ) :: tsavg5,zasat
  real(r_kind),                       intent(  out) :: cld,cldp
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,emissivity_k,ts,wavenumber,tb_obs
  real(r_kind),dimension(nchanl),     intent(in   ) :: tnoise
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5,temp
  real(r_kind),dimension(nsig),       intent(in   ) :: prsltmp,tvp
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv,varinv_use

! Declare local parameters

  real(r_kind),parameter:: oneover400=1.0_r_kind/400.0_r_kind


  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,cenlatx,fact,sfchgtfact
  real(r_kind) :: sum,sum2,sum3,cloudp,tmp,dts,delta
  real(r_kind),dimension(nchanl) :: dtb
  integer(i_kind) :: i,k,kk,lcloud


! Reduce weight given to obs for shortwave ir if
! solar zenith angle tiny_r_kind
  if (pangs <= 89.0_r_kind .and. frac_sea > zero) then
!    QC2 in statsrad
     if(luse)aivals(9,is) = aivals(9,is) + one
     do i=1,nchanl
        if(wavenumber(i) > r2000)then
           if(wavenumber(i) > r2400)then
              varinv(i)=zero
              varinv_use(i)=zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_2400_qc
           else
              tmp=one-(wavenumber(i)-r2000)*ptau5(1,i)&
                   *max(zero,cos(pangs*deg2rad))*oneover400
              varinv(i)=tmp*varinv(i)
              varinv_use(i)=tmp*varinv_use(i)
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_2000_qc
           end if
        end if
     end do
  endif

  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = r0_03
     dtempf = four
  else if(snow)then
     demisf = r0_02
     dtempf = two
  else
     demisf = r0_03
     dtempf = four
  end if

! If GOES and lza > 60. do not use
  if( goessndr .and. zasat*rad2deg > r60) then
!    QC5 in statsrad
     if(luse)aivals(12,is) = aivals(12,is) + one
     do i=1,nchanl
        varinv(i) = zero
        varinv_use(i)=zero
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_satzen_qc
     end do
  end if

! Reduce weight for obs over higher topography
  sfchgtfact=one
  if (zsges > r2000) then
!    QC1 in statsrad
     if(luse)aivals(8,is) = aivals(8,is) + one
     sfchgtfact    = (r2000/zsges)**4
  endif

! Generate q.c. bounds and modified variances for height change and ptau5
  sum3=zero
  do i=1,nchanl
     if (tb_obs(i) > r1000 .or. tb_obs(i) <= zero) then
         varinv(i)=zero
         varinv_use(i)=zero
     end if
     varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))
     varinv_use(i) = varinv_use(i)*(one-(one-sfchgtfact)*ptau5(1,i))

!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     varinv_use(i)=varinv_use(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)

!    QC based on presence/absence of cloud
     sum3=sum3+tbc(i)*tbc(i)*varinv_use(i)
  end do
  sum3=0.75_r_kind*sum3
  lcloud=0
  cld=zero
  cldp=r10*prsltmp(1)

  do k=1,nsig
     if(prsltmp(k) > trop5)then
        sum=zero
        sum2=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
             dtb(i)=(tvp(k)-tsavg5)*ts(i)
             do kk=1,k-1
                dtb(i)=dtb(i)+(tvp(k)-tvp(kk))*temp(kk,i)
             end do
             sum=sum+tbc(i)*dtb(i)*varinv_use(i)
             sum2=sum2+dtb(i)*dtb(i)*varinv_use(i)
           end if
        end do
        if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
        cloudp=min(max(sum/sum2,zero),one)
        sum=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
             tmp=tbc(i)-cloudp*dtb(i)
             sum=sum+tmp*tmp*varinv_use(i)
           end if
        end do
        if(sum < sum3)then
           sum3=sum
           lcloud=k
           cld=cloudp
           cldp=r10*prsltmp(k)
        end if
     end if

  end do
  if ( lcloud > 0 ) then  ! If cloud detected, reject channels affected by it.

     do i=1,nchanl

!       If more than 2% of the transmittance comes from the cloud layer,
!          reject the channel (0.02 is a tunable parameter)

        if ( ptau5(lcloud,i) > 0.02_r_kind) then
!          QC4 in statsrad
           if(luse)aivals(11,is)   = aivals(11,is) + one
           varinv(i) = zero
           varinv_use(i) = zero
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_cloud_qc
        end if
     end do

!    If no clouds check surface temperature/emissivity

  else                 ! If no cloud was detected, do surface temp/emiss checks
     sum=zero
     sum2=zero
     do i=1,nchanl
        sum=sum+tbc(i)*ts(i)*varinv_use(i)
        sum2=sum2+ts(i)*ts(i)*varinv_use(i)
     end do
     if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
     dts=abs(sum/sum2)
     if(abs(dts) > one)then
        if(.not. sea)then
           dts=min(dtempf,dts)
        else
           dts=min(three,dts)
        end if
        do i=1,nchanl
           delta=max(r0_05*tnoise(i),r0_02)
           if(abs(dts*ts(i)) > delta)then
!             QC3 in statsrad
              if(luse .and. varinv(i) > zero) &
                   aivals(10,is)   = aivals(10,is) + one
              varinv(i) = zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_sfcir_qc
           end if
        end do
     end if
  endif

  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact   = half*(cenlatx+one)
  else
     efact = one
  endif

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     if(varinv(i) > tiny_r_kind)then
        errf(i)=efact*errf(i)
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return

end subroutine qc_irsnd
subroutine qc_amsua(nchanl,is,ndat,nsig,npred,sea,land,ice,snow,mixed,luse,   &
     zsges,cenlat,tb_obsbc1,tsavg5,cosza,clw,tbc,ptau5,emissivity_k,ts,      &
     pred,predchan,id_qc,aivals,errf,varinv,factch4)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_amsua    QC for amsua data
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for seviri data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     nsig         - number of model levels
!     npred        - number of predictors
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     mixed        - logical, mixed flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tb_obsbc1    - bias corrected ob for channel 1
!     tsavg5       - surface skin temperature for FOV
!     cosza        - cosine of the satellite zenith angle
!     clw          - cloud liquid water estimate
!     tbc          - simulated - observed BT with bias correction
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     pred         - bias correction predictors
!     predchan     - bias correction coefficients
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     factch4      - quality control factor for channel 4
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                             intent(in   ) :: sea,land,ice,snow,mixed,luse
  integer(i_kind),                     intent(in   ) :: nchanl,is,ndat,nsig,npred
  integer(i_kind),dimension(nchanl),   intent(inout) :: id_qc
  real(r_kind),                        intent(in   ) :: zsges,cenlat,tb_obsbc1,tsavg5
  real(r_kind),                        intent(in   ) :: cosza,clw
  real(r_kind),                        intent(inout) :: factch4
  real(r_kind),dimension(40,ndat),     intent(inout) :: aivals
  real(r_kind),dimension(nchanl),      intent(in   ) :: tbc,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl), intent(in   ) :: ptau5
  real(r_kind),dimension(npred,nchanl),intent(in   ) :: pred,predchan
  real(r_kind),dimension(nchanl),      intent(inout) :: errf,varinv

! Declare local parameters


  real(r_kind),parameter:: w1f6=1.0_r_kind/10.0_r_kind
  real(r_kind),parameter:: w2f6=1.0_r_kind/0.8_r_kind
  real(r_kind),parameter:: w1f4=1.0_r_kind/0.3_r_kind
  real(r_kind),parameter:: w2f4=1.0_r_kind/1.8_r_kind

  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,cenlatx,fact
  real(r_kind) :: efactmc,vfactmc,dtde1,dtde2,dtde3,dsval,clwx
  real(r_kind) :: factch6,de1,de2,de3
  integer(i_kind) :: i,n


  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = 0.015_r_kind  !decrease due to more accurate emiss model AMSU-A+B
     dtempf = one           !decrease due to more accurate emiss model AMSU-A+B
  else if(snow)then
     demisf = r0_02 !decrease due to more accurate emiss model AMSU-A+B
     dtempf = two   !decrease due to more accurate emiss model AMSU-A+B
  else
     demisf = 0.20_r_kind
     dtempf = 4.5_r_kind
  end if

! Reduce qc bounds in tropics
  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact   = cenlatx*quarter+0.75_r_kind
  else
     efact   = one
  endif

  efactmc = one
  vfactmc = one
! sval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*tb_obsbc1)*tb_obsbc1 +  &
!      0.454_r_kind*tb_obsbc2-tb_obsbc15
  dsval=0.80_r_kind
  if(sea)then
     dsval=((2.41_r_kind-0.0098_r_kind*tb_obsbc1)*tbc(1) + &
          0.454_r_kind*tbc(2)-tbc(15))*w1f6
     dsval=max(zero,dsval)
  end if

  if(sea .and. tsavg5 > t0c)then
     clwx=cosza*clw*w1f4
  else
     clwx=0.6_r_kind
  end if
! QC6 in statsrad
  if(clwx >= one .and. luse)aivals(13,is) = aivals(13,is) + one
  factch4=clwx**2+(tbc(4)*w2f4)**2
! factch6x=((sval-five)/r10)**2+(tbc(6)/0.8_r_kind)**2
! QC7 in statsrad
  if(dsval >= one .and. luse)aivals(14,is) = aivals(14,is) + one
  factch6=dsval**2+(tbc(6)*w2f6)**2


  if(factch6 >= one)then
     efactmc=zero
     vfactmc=zero
     errf(6)=zero
     varinv(6)=zero
     do i=1,6
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
     end do
     if(id_qc(15) == igood_qc)id_qc(15)=ifail_factch6_qc
!    QC3 in statsrad
     if(.not. mixed.and. luse)aivals(10,is) = aivals(10,is) + one

  else if(factch4 > half)then
     efactmc=zero
     vfactmc=zero
     do i=1,5
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch4_qc
     end do
     if(id_qc(15) == igood_qc)id_qc(15)=ifail_factch4_qc
!    QC1 in statsrad
     if(luse) aivals(8,is) = aivals(8,is) + one

  else if(sea)then
!    QC based on ratio of obs-ges increment versus the sensitivity of
!    the simulated brightness temperature to the surface emissivity
!    Y2K hurricane season runs by QingFu Liu found the hurricane
!    forecast tracks to be degraded without this QC.
!    (Is this still true?)

     dtde1 = emissivity_k(1)
     de1   = zero
     if (dtde1 /= zero) de1=abs(tbc(1))/dtde1
     dtde2 = emissivity_k(2)
     de2   = zero
     if (dtde2 /= zero) de2=abs(tbc(2))/dtde2
     dtde3 = emissivity_k(3)
     de3   = zero
     if (dtde3 /= zero) de3=abs(tbc(3))/dtde3

     if (de2 > r0_03 .or. de3 > r0_05 .or. de1 > r0_05) then
!       QC2 in statsrad
        if(luse)aivals(9,is) = aivals(9,is) + one
        efactmc=zero
        vfactmc=zero
        do i=1,5
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_emiss_qc
        end do
        if(id_qc(15) == igood_qc)id_qc(15)=ifail_emiss_qc
     end if
  end if

! Reduce q.c. bounds over higher topography
  if (zsges > r2000) then
!    QC4 in statsrad
     if(luse)aivals(11,is) = aivals(11,is) + one
     fact    = r2000/zsges
     efactmc = fact*efactmc
     errf(6) = fact*errf(6)
     vfactmc = fact*vfactmc
     varinv(6)  = fact*varinv(6)
     if (zsges > r4000) then
!       QC5 in statsrad
        if(luse)aivals(12,is) = aivals(12,is) + one
        fact   = r4000/zsges
        errf(7)= fact*errf(7)
        varinv(7)= fact*varinv(7)
     end if
  end if

! Generate q.c. bounds and modified variances.
  do i=1,nchanl

!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)

     if(varinv(i) > tiny_r_kind)then
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term=dtbf*dtbf
        if(i <= 4 .or. i == 15)then

!          Adjust observation error based on magnitude of liquid
!          water correction.  0.2 is empirical factor
           term=term+0.2_r_kind*(predchan(3,i)*pred(3,i))**2

           errf(i)   = efactmc*errf(i)
           varinv(i) = vfactmc*varinv(i)
        end if
        errf(i)   = efact*errf(i)
        if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if

  end do

  return

end subroutine qc_amsua
subroutine qc_mhs(nchanl,is,ndat,nsig,sea,land,ice,snow,mhs,amsub,luse,   &
     zsges,tbc,tb_obs,ptau5,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv,dsi,fact1)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_mhs    QC for amsub,mhs and hsb data
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for amsub, mhs and hsb data   
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     nsig         - number of model levels
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     mhs          - logical, mhs flag - true if mhs data
!     amsub        - logical, amsub flag - true if amsub data
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tbc          - simulated - observed BT with bias correction
!     tb_obs       - observed BT 
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     dsi          - scattering index quality control factor
!     fact1        - fact1 quality control parameter
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,mhs,amsub,luse
  integer(i_kind),                    intent(in   ) :: nchanl,is,ndat,nsig
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges
  real(r_kind),                       intent(inout) :: dsi,fact1
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,tb_obs,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv

! Declare local parameters

  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,fact
  integer(i_kind) :: i,n

  efact = one
  vfact = one
  if(sea)then
     demisf = 0.015_r_kind
     dtempf = half
  else if(land)then
     demisf = r0_03
     dtempf = two
  else if(ice)then
     demisf = r0_02  !decrease due to more accurate emiss model AMSU-A+B
     dtempf = one    !decrease due to more accurate emiss model AMSU-A+B
  else if(snow)then
     demisf = r0_02  !decrease due to more accurate emiss model AMSU-A+B
     dtempf = two    !decrease due to more accurate emiss model AMSU-A+B
  else
     demisf = quarter
     dtempf = five
 end if
!   For now increase for mhs since emissivity model not as good
  if(mhs .and. .not. sea) then
     demisf = three*demisf
     dtempf = three*dtempf
  end if
  if(sea .or. ice .or. snow)then
     dsi=9.0_r_kind
     if(tb_obs(2) < h300)then
        dsi=0.13_r_kind*(tbc(1)-33.58_r_kind*tbc(2)/(h300-tb_obs(2)))
!       QC3 in statsrad
        if(luse .and. dsi >= one)aivals(10,is) = aivals(10,is) + one
     end if
!    si=42.72_r_kind+0.85_r_kind*tbc(1)-tbc(2)
  else
     dsi=0.85_r_kind*tbc(1)-tbc(2)
!    si=42.72_r_kind+0.85_r_kind*tb_obs(1)-tb_obs(2)
!    QC4 in statsrad
     if(luse .and. dsi >= one)aivals(11,is) = aivals(11,is) + one
  end if
  dsi=max(zero,dsi)
  fact1=((tbc(1)-7.5_r_kind*dsi)/r10)**2+(dsi)**2

  if(fact1 > one)then
     vfact=zero
!    QC1 in statsrad
     if(luse)aivals(8,is) = aivals(8,is) + one
     do i=1,nchanl
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_fact1_qc
     end do
  else
     if (amsub .or. mhs) then  ! wv sounding channels
       do i=3,nchanl
          if (abs(tbc(i)) >= two) then
             varinv(i) = zero
             if(id_qc(i) == igood_qc)id_qc(i)=ifail_gross_routine_qc
          end if
       end do
     end if
     efact = (one-fact1*fact1)*efact
     vfact = (one-fact1*fact1)*vfact
!    Reduce q.c. bounds over higher topography
     if (zsges > r2000) then
!       QC2 in statsrad
        if(luse)aivals(9,is) = aivals(9,is) + one
        fact = r2000/zsges
        efact = fact*efact
        vfact = fact*vfact
     end if
  end if

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
!     Modify error based on transmittance at top of model
         varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
         errf(i)=efact*errf(i)*ptau5(nsig,i)
         if(varinv(i)>tiny_r_kind)then
            dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
            term=dtbf*dtbf
            if(term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
         end if
  end do


  return

end subroutine qc_mhs
subroutine qc_ssu(nchanl,is,ndat,nsig,sea,land,ice,snow,luse,   &
     zsges,cenlat,tb_obs,ptau5,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_ssu    QC for ssu data
!
!   prgmmr: H. Liu        org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for ssu data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     nsig         - number of model levels                         
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     cenlat       - latitude of observation
!     tb_obs       - observed BT 
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                    intent(in   ) :: nchanl,is,ndat,nsig
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges,cenlat
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nchanl),     intent(in   ) :: tb_obs,emissivity_k,ts
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv

! Declare local parameters


  real(r_kind) :: demisf,dtempf,efact,dtbf,term,sfchgtfact,cenlatx
  integer(i_kind) :: i

  if(sea)then
     demisf = r0_01*half
     dtempf = half*half
  else if(land)then
     demisf = r0_02*half
     dtempf = two*half
  else if(ice)then
     demisf = r0_02*half
     dtempf = three*half
  else if(snow)then
     demisf = r0_02*half
     dtempf = three*half
  else
     demisf = r0_02*half
     dtempf = five*half
  end if

! Reduce weight for obs over higher topography
  sfchgtfact=one
  if (zsges > r2000) then
     sfchgtfact    = (r2000/zsges)**4
     if(luse) aivals(11,is)= aivals(11,is) + one
  endif

  do i=1,nchanl

     if (tb_obs(i) > r400 .or. tb_obs(i) <= r100) then
        varinv(i)=zero
        if(luse) aivals(12,is)= aivals(12,is) + one
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_range_qc
     endif
     varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))
!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)
  end do

! Reduce qc bounds in tropics
  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact = half*(cenlatx+one)
  else
     efact = one
  endif

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     if(varinv(i) > tiny_r_kind)then
        errf(i)=efact*errf(i)
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return

end subroutine qc_ssu
subroutine qc_msu(nchanl,is,ndat,nsig,sea,land,ice,snow,luse,   &
     zsges,cenlat,tbc,ptau5,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_msu    QC for msu data
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for seviri data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     nsig         - number of model levels
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tbc          - simulated - observed BT with bias correction
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                    intent(in   ) :: nchanl,is,ndat,nsig
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges,cenlat
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv

! Declare local parameters

  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,cenlatx,fact
  integer(i_kind) :: i

  vfact = one

! Reduce qc bounds in tropics
  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact   = half*(cenlatx+one)
  else
     efact   = one
  endif
  if(sea)then
     demisf = 0.015_r_kind
     dtempf = half
  else if(land)then
     demisf = r0_03
     dtempf = 2.5_r_kind
  else if(ice)then
     demisf = r0_05
     dtempf = three
  else if(snow)then
     demisf = r0_05
     dtempf = three
  else
     demisf = 0.20_r_kind
     dtempf = 4.5_r_kind
  end if

! Apply window test to channel 2 using channel 1
  if (abs(tbc(1)) > five) then
     errf(2) = zero
     varinv(2) = zero
     if(id_qc(2) == igood_qc)id_qc(2)=ifail_gross_routine_qc
!    QC1 in statsrad
     if(luse)aivals(8,is)   = aivals(8,is) + one
  endif

! Reduce q.c. bounds over higher topography
  if (zsges > r2000) then
!    QC2 in statsrad
     if(luse)aivals(9,is)   = aivals(9,is) + one
     fact = r2000/zsges
     errf(1) = fact*errf(1)
     errf(2) = fact*errf(2)
     errf(3) = fact*errf(3)
     vfact = fact
  end if



! Generate q.c. bounds and modified variances.
  errf(3) = two*errf(3)
  errf(4) = two*errf(4)
  do i=1,nchanl

!    Modify error based on transmittance at top of model
     varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
     errf(i)=efact*errf(i)*ptau5(nsig,i)

     if(varinv(i) > tiny_r_kind)then
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return

end subroutine qc_msu
subroutine qc_seviri(nchanl,is,ndat,sea,land,ice,snow,luse,   &
     zsges,tbc,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_seviri    QC for seviri data
!
!   prgmmr: H. Liu           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for seviri data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tbc          - simulated - observed BT with bias correction
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                          intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                  intent(in   ) :: nchanl,is,ndat
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc
  real(r_kind),                     intent(in   ) :: zsges
  real(r_kind),dimension(40,ndat),  intent(inout) :: aivals
  real(r_kind),dimension(nchanl),   intent(in   ) :: tbc,emissivity_k,ts
  real(r_kind),dimension(nchanl),   intent(inout) :: errf,varinv

! Declare local parameters

  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term
  integer(i_kind) :: i

  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = r0_02
     dtempf = three
  else if(snow)then
     demisf = r0_02
     dtempf = three
  else
     demisf = r0_02
     dtempf = five
  end if
  do i=1,nchanl

!    use chn 2 and 3 over both sea and land while other IR chns only over sea
     if (sea) then
        efact=one
        vfact=one
     else if (land ) then
        if (i == 2 .or. i ==3 ) then
           efact=one
           vfact=one
        else
           efact=zero
           vfact=zero
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_surface_qc
        end if
     else
        efact=zero
        vfact=zero
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_surface_qc
     end if

!    Reduce weight for obs over higher topography
!    QC_terrain: If seviri and terrain height > 1km. do not use
     if (zsges > r1000) then
        efact   = zero
        vfact   = zero
!       QC2 in statsrad
        if(luse)aivals(9,is)= aivals(9,is) + one
     end if

!    gross check
!    QC_o-g: If abs(o-g) > 2.0 do not use
     if ( abs(tbc(i)) > two ) then
        vfact = zero
        efact = zero
        if(id_qc(i) == igood_qc ) id_qc(i)=ifail_gross_routine_qc   !hliu check
!       QC1 in statsrad
        if(luse)aivals(8,is)= aivals(8,is) + one  !hliu check
     end if

!    modified variances.
     errf(i)   = efact*errf(i)
     varinv(i) = vfact*varinv(i)
!    Modify error based on transmittance at top of model
!    need this for SEVIRI??????
!    varinv(i)=varinv(i)*ptau5(nsig,i)
!    errf(i)=errf(i)*ptau5(nsig,i)

     if(varinv(i) > tiny_r_kind)then
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return

end subroutine qc_seviri
subroutine qc_avhrr(nchanl,is,ndat,sea,land,ice,snow,luse,   &
     dtp_avh,tb_obs,id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_avhrr     QC for avhrr  data
!
!   prgmmr: Xu Li            org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for avhrr  data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     dtp_avh      - ob type - set to 152.0 if day time
!     tb_obs       - observed BT 
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                          intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                  intent(in   ) :: nchanl,is,ndat
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc
  real(r_kind),                     intent(in   ) :: dtp_avh
  real(r_kind),dimension(40,ndat),  intent(inout) :: aivals
  real(r_kind),dimension(nchanl),   intent(in   ) :: tb_obs
  real(r_kind),dimension(nchanl),   intent(inout) :: errf,varinv

! Declare local parameters

  real(r_kind) :: fact,efact
  integer(i_kind) :: i

  if(tb_obs(2) > zero .and. tb_obs(2) < 400.0_r_kind)then
     fact=one
     if (.not. sea)then
        fact    = zero
!       QC1 in statsrad
        if(luse) aivals(12,is)= aivals(12,is) + one
        do i=1,nchanl
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                       
        end do
     end if
  else
     fact = zero
     do i=1,nchanl
       if(id_qc(i) == igood_qc ) id_qc(i)=ifail_range_qc                    
     end do
  end if

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     varinv(i) = fact*varinv(i)
!    Reject day time AVHRR Ch-3A observation
     if ( i == 1 .and. dtp_avh /= 152.0_r_kind ) then          ! day time
        varinv(i) = zero
!       QC3 in statsrad
        if(luse)aivals(10,is)= aivals(10,is) + one
     endif
  end do

  return

end subroutine qc_avhrr
subroutine qc_goesimg(nchanl,is,ndat,dplat,sea,land,ice,snow,luse,   &
     zsges,cld,tb_obs,tb_obs_sdv,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_seviri    QC for seviri data
!
!   prgmmr: H. Liu           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for seviri data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     dplat        - satellite identifier
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     cld          - cloud percentage within averaging box
!     tb_obs       - observed BT within averaging box
!     tb_obs_sdv   - observed BT standard deviation within averaging box
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                          intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                  intent(in   ) :: nchanl,is,ndat
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc
  real(r_kind),                     intent(in   ) :: zsges,cld
  real(r_kind),dimension(40,ndat),  intent(inout) :: aivals
  real(r_kind),dimension(nchanl),   intent(in   ) :: tb_obs,tb_obs_sdv,emissivity_k,ts
  real(r_kind),dimension(nchanl),   intent(inout) :: errf,varinv
  character(10),                    intent(in   ) :: dplat

! Declare local parameters

  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r0_3=0.3_r_kind
  real(r_kind),parameter:: r0_4=0.4_r_kind
  real(r_kind),parameter:: r0_6=0.6_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r0_8=0.8_r_kind
  real(r_kind),parameter:: r0_9=0.9_r_kind
  real(r_kind),parameter:: r1_1=1.1_r_kind
  real(r_kind),parameter:: r1_3=1.3_r_kind
  real(r_kind),parameter:: r1_4=1.4_r_kind


  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,fact2,fact3,fact4,fact5
  real(r_kind) :: fact
  integer(i_kind) :: i


  if(tb_obs(1) > zero .and. tb_obs(2) > zero .and. tb_obs(3) > zero .and. &
     tb_obs(4) > zero)then
     efact = one
     vfact = one
     fact2 = one
     fact3 = one
     fact4 = one
     fact5 = one
     if(sea)then
        demisf = r0_01
        dtempf = half
     else if(land)then
        do i=1,4
          if(i /= 2)then
            varinv(i)=zero
            if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
          end if
        end do
        demisf = r0_01
        dtempf = two
     else if(ice)then
        do i=1,4
          varinv(i)=zero
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
        end do
        demisf = r0_02
        dtempf = three
     else if(snow)then
        do i=1,4
          varinv(i)=zero
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
        end do
        demisf = r0_02
        dtempf = three
     else
        do i=1,4
          varinv(i)=zero
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
        end do
        demisf = r0_02
        dtempf = five
     end if

!    Filter out data according to clear sky fraction
     if(dplat == 'g10' .and. cld <r40 ) then
        do i=1,4
          varinv(i)=zero
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_cloud_qc                    
        end do
!       QC7 in statsrad
        if(luse)aivals(14,is)= aivals(14,is) + one
     else if(dplat == 'g12' .and. cld <r70 ) then
        do i=1,4
          varinv(i)=zero
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_cloud_qc                    
        end do
!       QC7 in statsrad
        if(luse)aivals(14,is)= aivals(14,is) + one
     end if

!    Quality control according to brightness temperature
!    standard deviation from data
     if(tb_obs_sdv(1) >one ) then
        varinv(1)=zero
!       QC3 in statsrad
        if(luse)aivals(10,is)= aivals(10,is) + one
        if(id_qc(1) == igood_qc ) id_qc(1)=ifail_std_goesimg_qc                    
     end if

     if(tb_obs_sdv(2) >1.5_r_kind ) then
        varinv(2)=zero
!       QC4 in statsrad
        if(luse)aivals(11,is)= aivals(11,is) + one
        if(id_qc(2) == igood_qc ) id_qc(2)=ifail_std_goesimg_qc                    
     end if

     if(tb_obs_sdv(3) >one ) then
        varinv(3)=zero
!       QC5 in statsrad
        if(luse)aivals(12,is)= aivals(12,is) + one
        if(id_qc(3) == igood_qc ) id_qc(3)=ifail_std_goesimg_qc                    
     end if

     if(tb_obs_sdv(4) >one ) then
        varinv(4)=zero
!       QC6 in statsrad
        if(luse)aivals(13,is)= aivals(13,is) + one
        if(id_qc(4) == igood_qc ) id_qc(4)=ifail_std_goesimg_qc                    
     end if

!    Reduce weight for obs over higher topography
     if (zsges > r2000) then
        fact    = r2000/zsges
        efact   = fact*efact
        vfact   = fact*vfact
!       QC2 in statsrad
        if(luse)aivals(9,is)= aivals(9,is) + one
     end if
  else
     vfact=zero
  end if
! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     varinv(i) = vfact*varinv(i)
     if( dplat == 'g10' .and. i== 2) then
        if (tb_obs_sdv(2) >r0_3 .and. tb_obs_sdv(2) <=r0_6) &
           varinv(i)=varinv(i)/1.05_r_kind
        if (tb_obs_sdv(2) >r0_6 .and. tb_obs_sdv(2) <=r0_7) &
           varinv(i)=varinv(i)/1.15_r_kind
        if (tb_obs_sdv(2) >r0_7 .and. tb_obs_sdv(2) <=r0_8) &
           varinv(i)=varinv(i)/1.24_r_kind
        if (tb_obs_sdv(2) >r0_8 .and. tb_obs_sdv(2) <=r0_9) &
           varinv(i)=varinv(i)/1.28_r_kind
        if (tb_obs_sdv(2) >r0_9 .and. tb_obs_sdv(2) <=one)  &
           varinv(i)=varinv(i)/1.32_r_kind
        if (tb_obs_sdv(2) >one  .and. tb_obs_sdv(2) <=r1_1) &
           varinv(i)=varinv(i)/1.35_r_kind
        if (tb_obs_sdv(2) >r1_1 .and. tb_obs_sdv(2) <=r1_3) &
           varinv(i)=varinv(i)/1.39_r_kind
        if (tb_obs_sdv(2) >r1_4 )                           &     
           varinv(i)=varinv(i)/1.48_r_kind
     else if(dplat == 'g12' .and. i== 2) then
        if (tb_obs_sdv(2) >r0_4 .and. tb_obs_sdv(2) <=half) &
           varinv(i)=varinv(i)/1.05_r_kind
        if (tb_obs_sdv(2) >half .and. tb_obs_sdv(2) <=r0_6) &
           varinv(i)=varinv(i)/1.09_r_kind
        if (tb_obs_sdv(2) >r0_6 .and. tb_obs_sdv(2) <=r0_7) &
           varinv(i)=varinv(i)/1.14_r_kind
        if (tb_obs_sdv(2) >r0_7 .and. tb_obs_sdv(2) <=r0_8) &
           varinv(i)=varinv(i)/1.17_r_kind
        if (tb_obs_sdv(2) >r0_8 .and. tb_obs_sdv(2) <=r1_1) &
           varinv(i)=varinv(i)/1.19_r_kind
        if (tb_obs_sdv(2) >r1_1 .and. tb_obs_sdv(2) <=r1_3) &
           varinv(i)=varinv(i)/1.25_r_kind
        if (tb_obs_sdv(2) >r1_3 )                         &
           varinv(i)=varinv(i)/1.29_r_kind
     end if
     if(varinv(i)>tiny_r_kind)then
        errf(i)   = efact*errf(i)
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term=dtbf*dtbf
        if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     endif
  end do

  return

end subroutine qc_goesimg
end module qcmod
