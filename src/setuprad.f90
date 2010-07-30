   subroutine setuprad(lunin,mype,aivals,stats,nchanl,nreal,nobs,&
     obstype,isis,is,rad_diagsave,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuprad    compute rhs of oi equation for radiances
!   prgmmr: derber           org: np23                date: 1995-07-06
!
! abstract: read in data, first guess, and obtain rhs of oi equation
!        for radiances.
!
! program history log:
!   1995-07-06  derber
!   1996-11-xx  wu, data from prepbufr file
!   1996-12-xx  mcnally, changes for diagnostic file and bugfix
!   1998-04-30  weiyu yang    mpi version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist - remove sigma assumptions (use pressure)
!   2004-05-28  kleist - subroutine call update
!   2004-06-17  treadon - update documenation
!   2004-07-23  weng,yan,okamoto - incorporate MW land and snow/ice emissivity
!                                  models for AMSU-A/B and SSM/I
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - modifications for nonlinear qc
!   2004-10-15  derber  - modify parts of IR quality control
!   2004-10-28  treadon - replace parameter tiny with tiny_r_kind
!   2004-11-22  derber  - remove weight, add logical for boundary point
!   2004-11-30  xu li   - add SST physical retrieval algorithm
!   2004-12-22  treadon - add outer loop number to name of diagnostic file
!   2005-01-20  okamoto - add ssm/i radiance assimilation 
!   2005-01-22  okamoto - add TB jacobian with respect to ocean surface wind 
!                         through MW ocean emissivity model
!   2005-02-22  derber  - alter surface determination and improve quality control
!   2005-02-28  treadon - increase size of character variable holding diagnostic 
!                         file name
!   2005-03-02  derber  - modify use of surface flages and quality control 
!                         and adjoint of surface emissivity
!   2005-03-04  xu li   - restructure code related to sst retrieval
!   2005-03-07  todling,treadon - place lower bound on sum2
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  derber  - save observation time
!   2005-04-11  treadon - add logical to toggle on/off nonlinear qc code
!   2005-04-18  treadon - modify sections of code related to sst retrieval
!   2005-06-01  treadon - add code to load/use extended vertical profile arrays in rtm
!   2005-07-06  derber  - modify for mhs and hirs/4
!   2005-07-29  treadon - modify tnoise initialization; add varinv_use
!   2005-09-20  xu,pawlak - modify sections of code related to ssmis
!   2005-09-28  derber  - modify for new radinfo and surface info input from read routines
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-17  treadon - generalize accessing of elements from obs array
!   2005-10-20  kazumori - modify sections of code related to amsre
!   2005-11-04  derber - place lower bound (0.0) on computed clw
!   2005-11-14  li - modify avhrr related code
!   2005-11-18  treadon - correct thin snow test to apply to microwave
!   2005-11-18  kazumori - modify sections of amsre diagnostic file
!   2005-11-29  parrish - remove call to deter_sfc_reg (earlier patch for regional mode)
!   2005-12-16  derber - add check on skin temperature to clw bias correction
!   2005-12-20  derber - add transmittance qc check to mw sensors
!   2006-01-09  treadon - introduce get_ij
!   2006-01-12  treadon - replace pCRTM with CRTM
!   2006-01-31  todling - add obs time to output diag files
!   2006-02-01  liu - add ssu
!   2006-02-02  treadon - rename prsi(l) as ges_prsi(l)
!   2006-02-03  derber - add new obs control and change printed stats
!   2006-03-21  treadon - add optional perturbation to observation
!   2006-03-24  treadon - bug fix - add iuse_rad to microwave channel varinv check
!   2006-04-19  treadon - rename emisjac as dtbduv_on (accessible via obsmod)
!   2006-04-27  derber - remove rad_tran_k, process data one  profile at a time
!                        write data in jppf chunks
!   2006-05-10  derber - add check on maximum number of levels for RT
!   2006-05-30  derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-27  kazumori - modify factor of bc predictor(clw) for AMSR-E
!                          and input of qcssmi
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc and add satellite and solar azimuth angles
!   2006-07-31  kleist  - change call to intrppx, no longer get ps at ob location
!   2006-12-21  sienkiewicz - add 'no85GHz' flag for F8 SSM/I 
!   2007-01-24  kazumori- modify to qcssmi subroutine output and use ret_ssmis
!                         for ssmis_las only (assumed UKMO SSMIS data)
!   2007-03-09      su  - remove the perturbation to the observation
!   2007-03-19  tremolet - binning of observations
!   2007-04-04  wu      - do not load ozone jacobian if running regional mode
!   2007-05-30  h.liu   - replace c1 with constoz in ozone jacobian
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_rad_file
!   2007-06-29  jung    - update CRTM interface
!   2008-01-30  h.liu/treadon - add SSU cell pressure correction block
!   2008-05-21  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-12-07  b.yan   - changed qc for channel 5 (relaxed)
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check(), and
!			  new arguments init_pass and last_pass.
!   2009-12-08  guo     - cleaned diag output rewind with open(position='rewind')
!			- fixed a bug in diag header output while is not init_pass.
!   2010-03-01  gayno - allow assimilation of "mixed" amsua fovs
!   2010-03-30  collard - changes for interface with CRTM v2.0. 
!   2010-03-30  collard - Add CO2 interface (fixed value for now).
!   2010-04-08  h.liu   -add SEVIRI assimilation 
!   2010-04-16  hou/kistler add interface to module ncepgfs_ghg
!   2010-05-19  todling - revisit intrppx CO2 handle
!   2010-06-10  todling - reduce pointer check by getting CO2 pointer at this level
!                       - start adding hooks of aerosols influence on RTM
!   2010-07-16  yan     - update qaulity control of mw water vapor sounding channels (amsu-b and mhs)
!
!  input argument list:
!     lunin   - unit from which to read radiance (brightness temperature, tb) obs
!     mype    - mpi task id
!     nchanl  - number of channels per obs
!     nreal   - number of pieces of non-tb information per obs
!     nobs    - number of tb observations to process
!     obstype - type of tb observation
!     isis    - sensor/instrument/satellite id  ex.amsua_n15
!     is      - integer counter for number of observation types to process
!     rad_diagsave - logical to switch on diagnostic output (.false.=no output)
!     channelinfo - structure containing satellite sensor information
!
!   output argument list:
!     aivals - array holding sums for various statistics as a function of obs type
!     stats  - array holding sums for various statistics as a function of channel
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind
  use crtm_module, only: crtm_atmosphere_type,crtm_surface_type,crtm_geometry_type, &
      crtm_surface_create,o3_id,co2_id,wet_soil,crtm_k_matrix,mass_mixing_ratio_units, &
      crtm_atmosphere_create,grass_scrub,grass_soil, meadow_grass,urban_concrete, &
      irrigated_low_vegetation,broadleaf_pine_forest,pine_forest,compacted_soil, &
      broadleaf_forest,broadleaf_brush,tundra,tilled_soil,scrub,scrub_soil,&
      crtm_options_type,crtm_init,crtm_destroy,success,crtm_options_destroy,&
      crtm_options_create, crtm_options_associated
  use crtm_rtsolution_define, only: crtm_rtsolution_type, crtm_rtsolution_create, &
      crtm_rtsolution_destroy, crtm_rtsolution_associated
  use crtm_spccoeff, only: sc
  use crtm_atmosphere_define, only:h2o_id,crtm_atmosphere_associated, & 
      crtm_atmosphere_destroy,volume_mixing_ratio_units,crtm_atmosphere_zero
  use crtm_surface_define, only: crtm_surface_destroy, crtm_surface_associated, crtm_surface_zero
  use crtm_channelinfo_define, only: crtm_channelinfo_type
  use crtm_parameters, only: limit_exp,toa_pressure,max_n_layers
  use message_handler, only: success
  use radinfo, only: nuchan,tlapmean,ifactq,predx,cbias,ermax_rad,&
       npred,jpch_rad,varch,iuse_rad,nusis,fbias,retrieval,b_rad,pg_rad,&
       crtm_coeffs_path,air_rad,ang_rad
  use guess_grids, only: add_rtm_layers,sfcmod_gfs,sfcmod_mm5,&
       comp_fact10
  use obsmod, only: ianldate,iadate,ndat,mype_diaghdr,nchan_total, &
           dplat,dtbduv_on,radhead,radtail,&
           i_rad_ob_type,obsdiags,obsptr,lobsdiagsave,nobskeep,lobsdiag_allocated,&
           dirname,time_offset
  use obsmod, only: rad_ob_type
  use obsmod, only: obs_diag
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_chemtracer_mod, only: gsi_chem_bundle    ! for now, a common block
  use gsi_chemtracer_mod, only: gsi_chemtracer_get
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use gridmod, only: nsig,nsig2,nsig3p1,&
       regional,nsig3p2,nsig3p3,msig,get_ij
  use satthin, only: super_val1
  use constants, only: half,constoz,amsua_clw_d1,amsua_clw_d2,tiny_r_kind,&
       fv,zero,one,deg2rad,rad2deg,one_tenth,quarter,two,three,four,five,&
       cg_term,tpwcon,t0c,r1000,wgtlim,r60,h300,grav
  use jfunc, only: jiter,miter
  use sst_retrieval, only: setup_sst_retrieval,avhrr_sst_retrieval,&
       finish_sst_retrieval,spline_cub
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use intrppx
  implicit none

! Declare passed variables
  logical                           ,intent(in   ) :: rad_diagsave
  character(10)                     ,intent(in   ) :: obstype
  character(20)                     ,intent(in   ) :: isis
  integer(i_kind)                   ,intent(in   ) :: lunin,mype,nchanl,nreal,nobs,is
  real(r_kind),dimension(40,ndat)   ,intent(inout) :: aivals
  real(r_kind),dimension(7,jpch_rad),intent(inout) :: stats
  logical                           ,intent(in   ) :: init_pass,last_pass	! state of "setup" processing

! Declare external calls for code analysis
  external:: stop2
  external:: ret_ssmis
  external:: retrieval_amsre
  external:: retrieval_mi
  external:: qcssmi

! Declare local parameters
  integer(i_kind),parameter:: ipchan=7
  integer(i_kind),parameter:: ireal=26

! CRTM structure variable declarations.
  integer(i_kind),parameter::  n_absorbers = 3
  integer(i_kind),parameter::  n_clouds = 0
  type(crtm_channelinfo_type),dimension(1) :: channelinfo

! Mapping land surface type of GFS to CRTM
!  Note: index 0 is water, and index 13 is ice. The two indices are not
!        used and just assigned to COMPACTED_SOIL.
  integer(i_kind), parameter, dimension(0:13) :: gfs_to_crtm=(/COMPACTED_SOIL, &
       BROADLEAF_FOREST, BROADLEAF_FOREST, BROADLEAF_PINE_FOREST, PINE_FOREST, &
       PINE_FOREST, BROADLEAF_BRUSH, SCRUB, SCRUB, SCRUB_SOIL, TUNDRA, &
       COMPACTED_SOIL, TILLED_SOIL, COMPACTED_SOIL/)

! Mapping land surface type of NMM to CRTM
!  Note: index 16 is water, and index 24 is ice. The two indices are not
!        used and just assigned to COMPACTED_SOIL.
  integer(i_kind), parameter, dimension(24) :: nmm_to_crtm=(/URBAN_CONCRETE, &
       COMPACTED_SOIL, IRRIGATED_LOW_VEGETATION, GRASS_SOIL, MEADOW_GRASS, &
       MEADOW_GRASS, MEADOW_GRASS, SCRUB, GRASS_SCRUB, MEADOW_GRASS, &
       BROADLEAF_FOREST, PINE_FOREST, BROADLEAF_FOREST, PINE_FOREST, &
       BROADLEAF_PINE_FOREST, COMPACTED_SOIL, WET_SOIL, WET_SOIL, &
       IRRIGATED_LOW_VEGETATION, TUNDRA, TUNDRA, TUNDRA, TUNDRA, &
       COMPACTED_SOIL/)

  integer(i_kind),parameter,dimension(12):: mday=(/0,31,59,90,&
       120,151,181,212,243,273,304,334/)

  real(r_kind),parameter:: r0_01=0.01_r_kind
  real(r_kind),parameter:: r0_02=0.02_r_kind
  real(r_kind),parameter:: r0_03=0.03_r_kind
  real(r_kind),parameter:: r0_05=0.05_r_kind

  real(r_kind),parameter:: r0_3=0.3_r_kind
  real(r_kind),parameter:: r0_4=0.4_r_kind
  real(r_kind),parameter:: r0_6=0.6_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r0_8=0.8_r_kind
  real(r_kind),parameter:: r0_9=0.9_r_kind
  real(r_kind),parameter:: r1_1=1.1_r_kind
  real(r_kind),parameter:: r1_3=1.3_r_kind
  real(r_kind),parameter:: r1_4=1.4_r_kind


  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r284=284.0_r_kind
  real(r_kind),parameter:: r285=285.0_r_kind
  real(r_kind),parameter:: r400=400.0_r_kind
  real(r_kind),parameter:: r2000=2000.0_r_kind
  real(r_kind),parameter:: r2400=2400.0_r_kind
  real(r_kind),parameter:: r4000=4000.0_r_kind
  real(r_kind),parameter:: r1e10=1.0e10_r_kind

  real(r_kind),parameter:: qsmall  = 1.e-6_r_kind
  real(r_kind),parameter:: ozsmall = 1.e-10_r_kind
  real(r_kind),parameter:: small_wind = 1.e-3_r_kind

! Declare local variables
  character(128) diag_rad_file

  integer(i_kind) iextra,jextra,error_status,itype,istat
  integer(i_kind) err1,err2,err3,err4,ich9
  integer(i_kind) isatid,itime,ilon,ilat,ilzen_ang,iscan_ang,ilazi_ang
  integer(i_kind) iszen_ang,ifrac_sea,ifrac_lnd,ifrac_ice
  integer(i_kind) ifrac_sno,ilone,ilate
  integer(i_kind) isst_hires,isst_navy,idata_type,iclr_sky,iscan_pos,iclavr
  integer(i_kind) isazi_ang,its_sea,its_lnd,its_ice,its_sno
  integer(i_kind) itsavg,ivty,ivfr,isty,istp,ism,isn,izz,idomsfc,isfcr,iff10
  integer(i_kind) isli
  integer(i_kind) m,mm,jc
  integer(i_kind) icc
  integer(i_kind) j,k,ncnt,i
  integer(i_kind) mm1
  integer(i_kind) ixx
  integer(i_kind) kk,n,nlev,kval,kk2,ibin,ioff,iii
  integer(i_kind) ii,jj,lcloud,idiag
  integer(i_kind) nadir
  integer(i_kind) kraintype,ierrret
  integer(i_kind) sensorindex
  integer(i_kind):: leap_day,day_of_year
  integer(i_kind):: ico2,ier
  integer(i_kind),dimension(8):: obs_time,anal_time


  real(r_single) freq4,pol4,wave4,varch4,tlap4
  real(r_kind) vfact,vfactmc
  real(r_kind) efact,efactmc
  real(r_kind) demisf,fact,dtempf,dtbf,tbcx1,tbcx2
  real(r_kind) term,sum,tlap,dsval,tb_obsbc1,clwx
  real(r_kind) de1,de2,de3,dtde1,dtde2,dtde3
  real(r_kind) fact1,fact2,fact3,fact4,fact5,dsi
  real(r_kind) drad,factch4,factch6
  real(r_kind) dradnob,varrad,delta,error
  real(r_kind) tmp,sum2,sum3,dts
  real(r_kind) sfchgtfact,ten
  real(r_kind) oneover25,cloudp,obserr,errinv,useflag
  real(r_kind) w1f4,w2f4,w1f6,w2f6,oneover400
  real(r_kind) cg_rad,wgross,wnotgross,wgt,arg,exp_arg
  real(r_kind) tsavg5,vegtype5,trop5,sfcr
  real(r_kind) secant_term   ! secant of viewing path angle at surface
  real(r_kind) pangs,panglr,cld,cldp
  real(r_kind) cenlon,cenlat,slats,slons,zsges,zasat,dtime
  real(r_kind) ys_bias_sst
  real(r_kind) sstnv,sstfg,sstcu,dtp_avh
  real(r_kind) cosza,val_obs
  real(r_kind) cenlatx,tpw5
  real(r_kind) bearaz,sun_zenith,sun_azimuth
  real(r_kind) uwind,vwind,f10
  real(r_kind) clw,tpwc,si85,sgagl,total_od
  real(r_kind) kgkg_gm2

! Declare local arrays

  real(r_single),dimension(ireal):: diagbuf
  real(r_single),allocatable,dimension(:,:):: diagbufex
  real(r_single),allocatable,dimension(:,:):: diagbufchan

  real(r_kind),dimension(npred+1,nchanl)::predterms
  real(r_kind),dimension(npred,nchanl):: pred
  real(r_kind),dimension(nchanl):: varinv,varinv_use,error0,errf
  real(r_kind),dimension(nchanl):: tb_obs,tbc,tbcnob,tlapchn,tb_obs_sdv
  real(r_kind),dimension(nchanl):: tnoise,errmax
  real(r_kind),dimension(nchanl):: var,ratio_raderr,radinv
  real(r_kind),dimension(nchanl):: emissivity,ts,emissivity_k
  real(r_kind),dimension(nchanl):: uwind_k,vwind_k
  real(r_kind),dimension(nchanl,nsig):: dtb
  real(r_kind),dimension(nsig,nchanl):: wmix,temp,omix,ptau5
  real(r_kind),dimension(nsig3p3,nchanl):: htlto
  real(r_kind),dimension(nreal+nchanl,nobs)::data_s
  real(r_kind),dimension(nsig+1):: pin5
  real(r_kind),dimension(nsig):: c2,c3,c4,c5,prsltmp
  real(r_kind),dimension(nsig):: prsr5,temp5,qvp,tvp,poz,co2
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_kind),dimension(msig)::  prsltmp_rtm
  real(r_kind),dimension(msig+1)::  prsitmp_rtm
  real(r_kind),dimension(4):: sfcpct
  real(r_kind),dimension(5):: tmp_time
  real(r_kind),dimension(0:3):: dtskin
  real(r_kind) dtsavg,r90,coscon,sincon
  real(r_kind):: sqrt_tiny_r_kind

  integer(i_kind)                              :: n_aerosols  ! number of aerosols
  character(len=20),allocatable,dimension(:)   :: aero_names  ! aerosols names
  character(len=20),allocatable,dimension(:)   :: aero_types  ! aerosols types
  integer(i_kind)   ,allocatable,dimension(:)  :: iaero       ! index pointers to aerosols in chem_bundle
  integer(i_kind)   ,allocatable,dimension(:)  :: iaero_types ! maps user aerosols to CRTM conventions
  real(r_kind)      ,allocatable,dimension(:,:):: aero        ! aerosols (guess) profiles at obs location

  integer(i_kind),dimension(nchanl):: ich,icxx,id_qc
  integer(i_kind),dimension(msig):: klevel

  character(10) filex
  character(12) string
  character(len=20),dimension(1):: sensorlist

  logical hirs2,msu,goessndr,hirs3,hirs4,hirs,amsua,amsub,airs,hsb,goes_img,mhs
  logical avhrr,avhrr_navy,lextra,ssu,iasi,seviri
  logical ssmi,ssmis,amsre,amsre_low,amsre_mid,amsre_hig
  logical ssmis_las,ssmis_uas,ssmis_env,ssmis_img
  logical sea,mixed,land,ice,snow,toss,l_may_be_passive
  logical micrim,microwave
  logical,dimension(nobs):: luse
  logical no85GHz

  type(crtm_atmosphere_type),dimension(1)   :: atmosphere
  type(crtm_surface_type),dimension(1)      :: surface
  type(crtm_geometry_type),dimension(1) :: geometryinfo
  type(crtm_options_type),dimension(1)      :: options

  type(crtm_atmosphere_type),allocatable,dimension(:,:):: atmosphere_k
  type(crtm_surface_type),   allocatable,dimension(:,:):: surface_k
  type(crtm_rtsolution_type),allocatable,dimension(:,:):: rtsolution
  type(crtm_rtsolution_type),allocatable,dimension(:,:):: rtsolution_k

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(rad_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  character(len=*),parameter:: myname="setuprad"

  n_alloc(:)=0
  m_alloc(:)=0
!**************************************************************************************
! Initialize variables and constants.
  mm1        = mype+1
  ten        = 10.0_r_kind
  w1f6       = one/ten
  w2f6       = one/0.8_r_kind
  w1f4       = one/0.3_r_kind
  w2f4       = one/1.8_r_kind
  oneover25  = one/25._r_kind
  oneover400 = one/400._r_kind
  r90        = 90._r_kind
  ncnt       = 0
  coscon     = cos( (r90-55.0_r_kind)*deg2rad )
  sincon     = sin( (r90-55.0_r_kind)*deg2rad )

  cld   = zero
  cldp  = zero
  tpwc  = zero
  clw   = zero
  si85  = zero
  sgagl = zero
  icc   = 0
  ich9  = min(9,nchanl)
  do i=1,nchanl
     do j=1,npred
        pred(j,i)=zero
     end do
  end do
  sqrt_tiny_r_kind = ten*sqrt(tiny_r_kind)

! Initialize logical flags for satellite platform

  hirs2      = obstype == 'hirs2'
  hirs3      = obstype == 'hirs3'
  hirs4      = obstype == 'hirs4'
  hirs       = hirs2 .or. hirs3 .or. hirs4
  msu        = obstype == 'msu'
  ssu        = obstype == 'ssu'
  goessndr   = obstype == 'sndr'  .or. obstype == 'sndrd1' .or.  &
               obstype == 'sndrd2'.or. obstype == 'sndrd3' .or.  &
               obstype == 'sndrd4'
  amsua      = obstype == 'amsua'
  amsub      = obstype == 'amsub'
  mhs        = obstype == 'mhs'
  airs       = obstype == 'airs'
  hsb        = obstype == 'hsb'
  goes_img   = obstype == 'goes_img'
  avhrr      = obstype == 'avhrr'
  avhrr_navy = obstype == 'avhrr_navy'
  ssmi       = obstype == 'ssmi'
  amsre_low  = obstype == 'amsre_low'
  amsre_mid  = obstype == 'amsre_mid'
  amsre_hig  = obstype == 'amsre_hig'
  amsre      = amsre_low .or. amsre_mid .or. amsre_hig
  ssmis      = obstype == 'ssmis'
  ssmis_las  = obstype == 'ssmis_las'
  ssmis_uas  = obstype == 'ssmis_uas'
  ssmis_img  = obstype == 'ssmis_img'
  ssmis_env  = obstype == 'ssmis_env'
  iasi       = obstype == 'iasi'
  seviri     = obstype == 'seviri'

  ssmis=ssmis_las.or.ssmis_uas.or.ssmis_img.or.ssmis_env.or.ssmis 

  micrim=ssmi .or. ssmis .or. amsre   ! only used for MW-imager-QC and id_qc(ch)

  microwave=amsua .or. amsub  .or. mhs .or. msu .or. hsb .or. &
        micrim 

  isatid    = 1  ! index of satellite id
  itime     = 2  ! index of analysis relative obs time 
  ilon      = 3  ! index of grid relative obs location (x)
  ilat      = 4  ! index of grid relative obs location (y)
  ilzen_ang = 5  ! index of local (satellite) zenith angle (radians)
  ilazi_ang = 6  ! index of local (satellite) azimuth angle (radians)
  iscan_ang = 7  ! index of scan (look) angle (radians)
  iscan_pos = 8  ! index of integer scan position 
  iszen_ang = 9  ! index of solar zenith angle (degrees)
  isazi_ang = 10 ! index of solar azimuth angle (degrees)
  ifrac_sea = 11 ! index of ocean percentage
  ifrac_lnd = 12 ! index of land percentage
  ifrac_ice = 13 ! index of ice percentage
  ifrac_sno = 14 ! index of snow percentage
  its_sea   = 15 ! index of ocean temperature
  its_lnd   = 16 ! index of land temperature
  its_ice   = 17 ! index of ice temperature
  its_sno   = 18 ! index of snow temperature
  itsavg    = 19 ! index of average temperature
  ivty      = 20 ! index of vegetation type
  ivfr      = 21 ! index of vegetation fraction
  isty      = 22 ! index of soil type
  istp      = 23 ! index of soil temperature
  ism       = 24 ! index of soil moisture
  isn       = 25 ! index of snow depth
  izz       = 26 ! index of surface height
  idomsfc   = 27 ! index of dominate surface type
  isfcr     = 28 ! index of surface roughness
  iff10     = 29 ! index of ten meter wind factor
  ilone     = 30 ! index of earth relative longitude (degrees)
  ilate     = 31 ! index of earth relative latitude (degrees)

! Get pointer to CO2
! NOTE: for now, not to rock the boat, this takes CO2 from 1st time slot
!       eventually this could do the time interpolation by taking CO2 from
!       two proper time slots.
  ico2=-1
  if(size(gsi_chem_bundle)>0) & ! check to see if bundle's allocated
  call gsi_bundlegetpointer(gsi_chem_bundle(1),'co2',ico2,ier)

! Are there aerosols to affect CRTM?
  call gsi_chemtracer_get ('aerosols::3d',n_aerosols,ier)
  if(n_aerosols>0)then
     allocate(aero(nsig,n_aerosols),iaero(n_aerosols))
     allocate(aero_names(n_aerosols))
     call gsi_chemtracer_get ('aerosols::3d',aero_names,ier)
     call gsi_bundlegetpointer(gsi_chem_bundle(1),aero_names,iaero,ier)

     allocate(aero_types(n_aerosols),iaero_types(n_aerosols))
     call gsi_chemtracer_get ('aerosol_types::3d',aero_types,ier)
  else
     n_aerosols=0 
     allocate(aero(0,0),iaero(0))
  endif

! Initialize channel related information
  tnoise = r1e10
  errmax = r1e10
  l_may_be_passive = .false.
  toss = .true.
  jc=0
  do j=1,jpch_rad
     if(isis == nusis(j))then 
        jc=jc+1
        if(jc > nchanl)then
           write(6,*)'SETUPRAD:  ***ERROR*** in channel numbers, jc,nchanl=',jc,nchanl,&
                '  ***STOP IN SETUPRAD***'
           call stop2(71)
        end if

!       Load channel numbers into local array based on satellite type

        ich(jc)=j
!
!       Set error instrument channels
        tnoise(jc)=varch(j)
        errmax(jc)=ermax_rad(j)
        if (iuse_rad(j)< -1 .or. (iuse_rad(j) == -1 .and.  &
              .not.rad_diagsave)) tnoise(jc)=r1e10
        if (iuse_rad(j)>-1) l_may_be_passive=.true.
        if (tnoise(jc) < 1.e4_r_kind) toss = .false.
     end if
  end do
  if ( mype == 0 .and. .not.l_may_be_passive) write(6,*)mype,'setuprad: passive obs',is,isis
  if(nchanl > jc) write(6,*)'SETUPRAD:  channel number reduced for ', &
       obstype,nchanl,' --> ',jc
  if(jc == 0) then
     if(mype == 0) write(6,*)'SETUPRAD: No channels found for ', &
          obstype,isis
     if(nobs > 0)read(lunin)
     go to 135
  end if
  if (toss) then
     if(mype == 0)write(6,*)'SETUPRAD: all obs var > 1e4.  do not use ',&
          'data from satellite is=',isis
     if(nobs >0)read(lunin)                    
     goto 135
  endif

! Initialize radiative transfer

  sensorlist(1)=isis
  if( crtm_coeffs_path /= "" ) then
     if(init_pass .and. mype==mype_diaghdr(is)) write(6,*)'SETUPRAD: crtm_init() on path "'//trim(crtm_coeffs_path)//'"'
     error_status = crtm_init(sensorlist,channelinfo,&
        Process_ID=mype,Output_Process_ID=mype_diaghdr(is), &
        Load_CloudCoeff=.FALSE.,Load_AerosolCoeff=.FALSE., &
        File_Path = crtm_coeffs_path )
  else
     error_status = crtm_init(sensorlist,channelinfo,&
        Process_ID=mype,Output_Process_ID=mype_diaghdr(is), &
        Load_CloudCoeff=.FALSE.,Load_AerosolCoeff=.FALSE.)
  endif
  if (error_status /= success) then
     write(6,*)'SETUPRAD:  ***ERROR*** crtm_init error_status=',error_status,&
          '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  endif

  sensorindex = 0
! determine specific sensor
! Added a fudge in here to prevent multiple script changes following change of AIRS naming
! convention in CRTM.
  if (channelinfo(1)%sensor_id == isis .OR. &
       (channelinfo(1)%sensor_id == 'airs281_aqua' .AND. &
       isis == 'airs281SUBSET_aqua')) sensorindex = 1
  if (sensorindex == 0 ) then
     write(6,*)'SETUPRAD:  ***WARNING*** problem with sensorindex=',isis,&
          ' --> CAN NOT PROCESS isis=',isis,'   TERMINATE PROGRAM EXECUTION found ',&
         channelinfo(1)%sensor_id
     call stop2(71)
  endif

! Initialize sensor specific array pointers
  if (goes_img) then
     iclr_sky      =  7 ! index of clear sky amount
  elseif (avhrr_navy) then
     isst_navy     =  7 ! index of navy sst (K) retrieval
     idata_type    = 30 ! index of data type (151=day, 152=night)
     isst_hires    = 31 ! index of interpolated hires sst (K)
  elseif (avhrr) then
     iclavr        = 32 ! index CLAVR cloud flag with AVHRR data
     isst_hires    = 33 ! index of interpolated hires sst (K)
  elseif (seviri) then
     iclr_sky      =  7 ! index of clear sky amount
  endif

! Set number of extra pieces of information to write to diagnostic file
! For most satellite sensors there is no extra information.  However, 
! for GOES Imager data we write additional information.
  iextra=0
  jextra=0
  if (goes_img) then
     iextra=1
     jextra=nchanl
  endif
  lextra = (iextra>0)

! Set CRTM to process given satellite/sensor
!  Error_Status = CRTM_Set_ChannelInfo(isis, ChannelInfo)
!  if (error_status /= success) &
!      write(6,*)'SETUPRAD:  ***ERROR*** crtm_set_channelinfo error_status=',&
!      error_status,' for satsensor=',isis


! Check for consistency between user specified number of channels (nchanl) 
! and those defined by CRTM channelinfo structure.   Return to calling
! routine if there is a mismatch.

  if (nchanl /= channelinfo(sensorindex)%n_channels) then
     write(6,*)'SETUPRAD:  ***WARNING*** mismatch between nchanl=',&
          nchanl,' and n_channels=',channelinfo(sensorindex)%n_channels,&
          ' --> CAN NOT PROCESS isis=',isis,'   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  endif



! Allocate structures for radiative transfer
  allocate(&
       rtsolution  (channelinfo(sensorindex)%n_channels,1),&
       rtsolution_k(channelinfo(sensorindex)%n_channels,1),&
       atmosphere_k(channelinfo(sensorindex)%n_channels,1),&
       surface_k   (channelinfo(sensorindex)%n_channels,1))

  if(msig > max_n_layers)then
     write(6,*) 'SETUPRAD:  msig > max_n_layers - increase crtm max_n_layers ',&
          msig,max_n_layers
     call stop2(36)
  end if
  CALL crtm_atmosphere_create(atmosphere(1),msig,n_absorbers,n_clouds,n_aerosols)
  CALL crtm_surface_create(surface(1),channelinfo(sensorindex)%n_channels)
  CALL crtm_rtsolution_create(rtsolution,msig)
  CALL crtm_rtsolution_create(rtsolution_k,msig)
  CALL crtm_options_create(options,nchanl)
  if (.NOT.(crtm_atmosphere_associated(atmosphere(1)))) &
       write(6,*)' ***ERROR** creating atmosphere.'
  if (.NOT.(crtm_surface_associated(surface(1)))) &
       write(6,*)' ***ERROR** creating surface.'
  if (.NOT.(ANY(crtm_rtsolution_associated(rtsolution)))) &
       write(6,*)' ***ERROR** creating rtsolution.'
  if (.NOT.(ANY(crtm_rtsolution_associated(rtsolution_k)))) &
       write(6,*)' ***ERROR** creating rtsolution_k.'
  if (.NOT.(ANY(crtm_options_associated(options)))) &
       write(6,*)' ***ERROR** creating options.'


  atmosphere(1)%n_layers = msig
!  atmosphere%level_temperature_input = 0
  atmosphere(1)%absorber_id(1) = H2O_ID
  atmosphere(1)%absorber_id(2) = O3_ID
  atmosphere(1)%absorber_id(3) = CO2_ID
  atmosphere(1)%absorber_units(1) = MASS_MIXING_RATIO_UNITS
  atmosphere(1)%absorber_units(2) = VOLUME_MIXING_RATIO_UNITS
  atmosphere(1)%absorber_units(3) = VOLUME_MIXING_RATIO_UNITS
  atmosphere(1)%level_pressure(0) = TOA_PRESSURE

! Take care of possible aerosols
  call set_aero_types_(iaero_types,aero_types)
  do ii=1,n_aerosols
     atmosphere(1)%aerosol(ii)%Type = iaero_types(ii)
  enddo

  if(nchanl /= channelinfo(sensorindex)%n_channels) write(6,*)'***ERROR** nchanl,n_channels ', &
           nchanl,channelinfo(sensorindex)%n_channels

! Load surface sensor data structure
  surface(1)%sensordata%n_channels = channelinfo(sensorindex)%n_channels
!! REL-1.2 CRTM
!!  surface(1)%sensordata%select_wmo_sensor_id  = channelinfo(1)%wmo_sensor_id
!! RB-1.1.rev1855 CRTM

  surface(1)%sensordata%sensor_id             = &
       channelinfo(sensorindex)%sensor_id
  surface(1)%sensordata%WMO_sensor_id         = &
       channelinfo(sensorindex)%WMO_sensor_id
  surface(1)%sensordata%WMO_Satellite_id      = &
       channelinfo(sensorindex)%WMO_Satellite_id
  surface(1)%sensordata%sensor_channel        = &
       channelinfo(sensorindex)%sensor_channel

  do i=1,nchanl

     atmosphere_k(i,1) = atmosphere(1)
     surface_k(i,1)   = surface(1)

  end do


! Special setup for SST retrieval (output)
  if (retrieval.and.init_pass) call setup_sst_retrieval(obstype,dplat(is),mype)


! If SSM/I, check for non-use of 85GHz channel, for QC workaround
! set no85GHz true if any 85GHz is not used, and other freq channel is used
  no85GHz = .false.
  if (ssmi) then
     if (iuse_rad(ich(6)) < 1 .or. iuse_rad(ich(7)) < 1 ) then
        do j = 1,5
           if (iuse_rad(ich(j)) >= 1) then
              no85GHz = .true.
              cycle
           endif
        enddo
        if (no85GHz .and. mype == 0) write(6,*) &
           'SETUPRAD: using no85GHZ workaround for SSM/I ',isis
     endif
  endif



! If diagnostic file requested, open unit to file and write header.
  if (rad_diagsave) then
     filex=obstype
     write(string,1976) jiter
1976 format('_',i2.2)
     diag_rad_file= trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // trim(string)
     if(init_pass) then
        open(4,file=trim(diag_rad_file),form='unformatted',status='unknown',position='rewind')
     else
        open(4,file=trim(diag_rad_file),form='unformatted',status='old',position='append')
     endif
     if (lextra) allocate(diagbufex(iextra,jextra))

!    Initialize/write parameters for satellite diagnostic file on
!    first outer iteration.
     if (init_pass .and. mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nchanl,npred,ianldate,ireal,ipchan,iextra,jextra
        write(6,*)'SETUPRAD:  write header record for ',&
             isis,ireal,iextra,' to file ',trim(diag_rad_file),' ',ianldate
        do i=1,nchanl
           n=ich(i)
           if( n < 1 )cycle
!           freq4=sc(sensorindex)%frequency(n)
!           pol4=sc(sensorindex)%polarization(n)
!           wave4=sc(sensorindex)%wavenumber(n)
           varch4=varch(n)
           tlap4=tlapmean(n)
           freq4=sc(sensorindex)%frequency(i)
           pol4=sc(sensorindex)%polarization(i)
           wave4=sc(sensorindex)%wavenumber(i)
           write(4)freq4,pol4,wave4,varch4,tlap4,iuse_rad(n),&
                nuchan(n),ich(i)
        end do
     endif
  endif

  idiag=ipchan+npred+1
  if (lobsdiagsave) idiag=idiag+4*miter+1
  allocate(diagbufchan(idiag,nchanl))
  

! Load data array for current satellite
  read(lunin) data_s,luse

  if (nobskeep>0) then
     write(6,*)'setuprad: nobskeep',nobskeep
     call stop2(275)
  end if

! PROCESSING OF SATELLITE DATA

! Loop over data in this block
  call dtime_setup()
  do n = 1,nobs
!    Extract analysis relative observation time.
     dtime = data_s(itime,n)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then

        id_qc = 0
        if(luse(n))aivals(1,is) = aivals(1,is) + one

!       Extract lon and lat.
        slons  = data_s(ilon,n)    ! grid relative longitude
        slats  = data_s(ilat,n)    ! grid relative latitude                     
        cenlon = data_s(ilone,n)   ! earth relative longitude (degrees)
        cenlat = data_s(ilate,n)   ! earth relative latitude (degrees)                       
!       Extract angular information         
        zasat  = data_s(ilzen_ang,n)
        cosza  = cos(zasat)
        secant_term  = one/cosza          ! view angle path factor

        zsges=data_s(izz,n)
        f10=data_s(iff10,n)
        if(sfcmod_gfs .or. sfcmod_mm5) then
           sfcr=data_s(isfcr,n)
           isli=data_s(idomsfc,n)
           call comp_fact10(slats,slons,dtime,data_s(itsavg,n),sfcr, &
              isli,mype,f10)
        end if

         
     if(goes_img)then
        panglr = zero
        cld = data_s(iclr_sky,n)
     else if(seviri)then
        panglr = zero
        cld = 100-data_s(iclr_sky,n)
        if(abs(data_s(iszen_ang,n)) > 180.0_r_kind) data_s(iszen_ang,n)=r100
     else
        panglr = data_s(iscan_ang,n)
     end if

        if(amsre)then
           bearaz= data_s(ilazi_ang,n)-180.0_r_kind
           sun_zenith=data_s(iszen_ang,n)
           sun_azimuth=data_s(isazi_ang,n)
           sgagl =  acos(coscon * &
             cos( (r90-bearaz)*deg2rad ) * &
             cos( sun_zenith*deg2rad ) * &
             cos( (r90-sun_azimuth)*deg2rad ) + &
             coscon * &
             sin( (r90-bearaz)*deg2rad ) * &
             cos( sun_zenith*deg2rad ) * &
             sin( (r90-sun_azimuth)*deg2rad ) + &
             sincon *  sin( sun_zenith*deg2rad ) &
             ) * rad2deg
        end if

!       Extract nadir (scan step position)
        nadir = nint(data_s(iscan_pos,n))
        pangs  = data_s(iszen_ang,n)
 
!       Turn off antenna correction
        options(1)% use_antenna_correction = .false.

!       Load geometry structure
        geometryinfo(1)%sensor_zenith_angle = zasat*rad2deg  ! local zenith angle
        geometryinfo(1)%source_zenith_angle = pangs          ! solar zenith angle
        geometryinfo(1)%sensor_azimuth_angle = data_s(ilazi_ang,n) ! local zenith angle
        geometryinfo(1)%source_azimuth_angle = data_s(isazi_ang,n) ! solar zenith angle
        geometryinfo(1)%sensor_scan_angle   = panglr*rad2deg ! scan angle
        geometryinfo(1)%ifov                = nadir          ! field of view position

!       For some microwave instruments the solar and sensor azimuth angles can be 
!       missing  (given a value of 10^11).  Set these to zero to get past CRTM QC.
        if (geometryinfo(1)%source_azimuth_angle > 360.0_r_kind .OR. &
            geometryinfo(1)%source_azimuth_angle < zero ) &
            geometryinfo(1)%source_azimuth_angle = zero 
        if (geometryinfo(1)%sensor_azimuth_angle > 360.0_r_kind .OR. &
            geometryinfo(1)%sensor_azimuth_angle < zero ) &
            geometryinfo(1)%sensor_azimuth_angle = zero 
          
!       Special block for SSU cell pressure leakage correction.   Need to compute
!       observation time and load into Time component of geometryinfo structure.  
!       geometryinfo%time is only defined in CFSRR CRTM.
        if (ssu) then
 
!          Compute absolute observation time
           anal_time=0
           obs_time=0
           tmp_time=zero
           tmp_time(2)=dtime
           anal_time(1)=iadate(1)
           anal_time(2)=iadate(2)
           anal_time(3)=iadate(3)
           anal_time(5)=iadate(4)
!external-subroutine w3movdat()
           call w3movdat(tmp_time,anal_time,obs_time)
 
!          Compute decimal year, for example 1/10/1983
!          d_year = 1983.0 + 10.0/365.0
           leap_day = 0
           if( mod(obs_time(1),4)==0 ) then
              if( (mod(obs_time(1),100)/=0).or.(mod(obs_time(1),400)==0) ) leap_day = 1
           endif
           day_of_year = Mday(obs_time(2)) + obs_time(3)
           if(obs_time(2) > 2) day_of_year = day_of_year + leap_day

!          WARNING:  Current /nwprod/lib/sorc/crtm_gfs does NOT include Time
!          as a component of the geometryinfo structure.   If SSU data is to
!          be assimilated with the cell pressure correction applied, one must
!          uncomment the line below and recompile the GSI with the CFSRR CRTM.
!          geometryinfo(1)%Time = float(obs_time(1)) + float(day_of_year)/(365.0_r_kind+leap_day)
           write(6,*)'SETUPRAD:  ***WARNING*** SSU cell pressure correction NOT applied'
        endif

        sfcpct(1) = data_s(ifrac_sea,n)
        sfcpct(2) = data_s(ifrac_lnd,n)
        sfcpct(3) = data_s(ifrac_ice,n)
        sfcpct(4) = data_s(ifrac_sno,n)
 
!  Set land/sea, snow, ice percentages and flags (no time interpolation)

        sea  = sfcpct(1)  >= 0.99_r_kind
        land = sfcpct(2)  >= 0.99_r_kind
        ice  = sfcpct(3)  >= 0.99_r_kind
        snow = sfcpct(4)  >= 0.99_r_kind
        mixed = .not. sea  .and. .not. ice .and.  &
                .not. land .and. .not. snow
         
!       Set relative weight value
        val_obs=one
        ixx=nint(data_s(nreal,n))
        if (ixx > 0 .and. super_val1(ixx) >= one) then
           val_obs=data_s(nreal-1,n)/super_val1(ixx)
        endif

!       Load channel data into work array.
        do i = 1,nchanl
           obserr = min(tnoise(i),errmax(i))
           tb_obs(i) = data_s(i+nreal,n)
        end do
 
        if(goes_img)then
           do i = 1,nchanl
              tb_obs_sdv(i) = data_s(i+29,n)
           end do
        end if
 
!       Extract Navy and NCEP SST
        if( avhrr_navy .or. avhrr )then
           if( avhrr_navy )then
              dtp_avh = data_s(idata_type,n)
              sstnv=data_s(isst_navy,n)
              sstfg=data_s(isst_hires,n)
              sstcu=data_s(isst_hires,n)      ! not available, assigned as interpolated sst
           elseif ( avhrr) then
              if ( pangs <= 89.0_r_kind) then              ! day time
                 dtp_avh = 151.0_r_kind
              else
                 dtp_avh = 152.0_r_kind
              endif
              sstfg=data_s(isst_hires,n)
              sstcu=data_s(isst_hires,n)      ! not available, assigned as interpolated sst
              sstnv=data_s(isst_hires,n)      ! not available, assigned as interpolated sst
           endif
        end if


     tsavg5=data_s(itsavg,n)
     vegtype5=data_s(ivty,n)
!    Interpolate model fields to observation location
     call intrppx1(dtime,tvp,qvp,poz,co2,aero,prsltmp,prsitmp, &
            trop5,dtskin,dtsavg,uwind,vwind,slats,slons, &
            ico2,n_aerosols,iaero)

     tsavg5=tsavg5+dtsavg

!  check for microwave and thin snow - if thin then reset to land 
!               but type == mixed
!    if(microwave .and. snow .and. snow5 < r0_1)then         !thin snow
!       sfcpct(2)=sfcpct(2)+sfcpct(4)
!       sfcpct(4)=zero
!       snow = .false.
!       mixed = .true.
!    end if
         
!       Count data of different surface types
        if(luse(n))then
           if (mixed) then
              aivals(5,is) = aivals(5,is) + one
           else if (ice .or. snow) then
              aivals(4,is) = aivals(4,is) + one
           else if (land) then
              aivals(3,is) = aivals(3,is) + one
           end if
        end if

!       For SST retrieval, use interpolated NCEP SST analysis
        if (retrieval) then
           tsavg5 = sstfg
           data_s(its_sea,n)=sstfg
           data_s(its_ice,n)=sstfg
           data_s(its_lnd,n)=sstfg
           data_s(its_sno,n)=sstfg
        endif
 
!       Load surface structure
 
!       Define land characteristics
 
!       **NOTE:  The model surface type --> CRTM surface type
!                mapping below is specific to the versions NCEP
!                GFS and NNM as of September 2005
        itype = int(vegtype5)
        if (regional) then
           itype = min(max(1,itype),24)
           surface(1)%land_type = nmm_to_crtm(itype)
        else
           itype = min(max(0,itype),13)
           surface(1)%land_type = gfs_to_crtm(itype)
        end if

        surface(1)%wind_speed            = f10*sqrt(uwind*uwind+vwind*vwind)
        surface(1)%wind_direction       = rad2deg*atan2(-uwind,-vwind)
        if ( surface(1)%wind_direction < ZERO ) surface(1)%wind_direction = &
             surface(1)%wind_direction + 180._r_kind
! CRTM will reject surface coverages if greater than one and it is possible for 
! these values to be larger due to round off.
        surface(1)%water_coverage        = min(max(ZERO,sfcpct(1)), ONE)
        surface(1)%land_coverage         = min(max(ZERO,sfcpct(2)), ONE)
        surface(1)%ice_coverage          = min(max(ZERO,sfcpct(3)), ONE)
        surface(1)%snow_coverage         = min(max(ZERO,sfcpct(4)), ONE)     
        surface(1)%water_temperature     = max(data_s(its_sea,n)+dtskin(0),270._r_kind)
        surface(1)%land_temperature      = data_s(its_lnd,n)+dtskin(1)
        surface(1)%ice_temperature       = min(data_s(its_ice,n)+dtskin(2),280._r_kind)
        surface(1)%snow_temperature      = min(data_s(its_sno,n)+dtskin(3),280._r_kind)
        surface(1)%soil_moisture_content = data_s(ism,n)
        surface(1)%vegetation_fraction   = data_s(ivfr,n)
        surface(1)%soil_temperature      = data_s(istp,n)
        surface(1)%snow_depth            = data_s(isn,n)


!       Load surface sensor data structure
        do i=1,channelinfo(sensorindex)%n_channels
           surface(1)%sensordata%tb(i) = tb_obs(i)
        end do

!       Load profiles into model layers

!       Prepare for accumulating information for statistics and passing
!       needed information on to minimization
        do k=1,nsig
           qvp(k) = max(qsmall,qvp(k))
           c2(k)=one/(one+fv*qvp(k))
           c3(k)=one/(one-qvp(k))
           c4(k)=fv*tvp(k)*c2(k)
           c5(k)=r1000*c3(k)*c3(k)
           pin5(k)  = r10*prsitmp(k)
           prsr5(k) = r10*prsltmp(k)
           temp5(k) = tvp(k)
        end do

        pin5(nsig+1) = r10*prsitmp(nsig+1)


!       Interpolate guess pressure at interfaces as well as 
!       log pressure at mid-layers to obs locations/times

        call add_rtm_layers(prsitmp,prsltmp,prsitmp_rtm,prsltmp_rtm,klevel)

!       Load profiles into extended RTM model layers
        do k = 1,msig
           kk = msig - k + 1
           atmosphere(1)%level_pressure(k) = r10*prsitmp_rtm(kk)
           atmosphere(1)%pressure(k)       = r10*prsltmp_rtm(kk)
 
           kk2 = klevel(kk)
           atmosphere(1)%temperature(k)    = tvp(kk2)
           atmosphere(1)%absorber(k,1)     = r1000*qvp(kk2)*c3(kk2)
           atmosphere(1)%absorber(k,2)     = max(ozsmall,poz(kk2)*constoz)
           atmosphere(1)%absorber(k,3)     = co2(kk2)

!          Get aerosols into CRTM
           kgkg_gm2=(atmosphere(1)%level_pressure(k)-atmosphere(1)%level_pressure(k-1))*r100/grav*r1000
           do ii=1,n_aerosols

!              copy and convert from kg/kg to g/m2
               atmosphere(1)%aerosol(ii)%concentration(k) = aero(kk2,ii)*kgkg_gm2

!              calculate effective radius
               atmosphere(1)%aerosol(ii)%effective_radius(k) &
                = GOCART_Aerosol_size( ii,atmosphere(1)%aerosol(ii)%Type, &
                                          aero_names(ii),aero_types(ii), &
                                          atmosphere(1)%temperature(k),&
                                          atmosphere(1)%absorber(k,1),&
                                          atmosphere(1)%pressure(k) )

           enddo

! Add in a drop-off to absorber amount in the stratosphere to be in more 
! agreement with ECMWF profiles.  This should be replaced when climatological fields
! are introduced.
           if (atmosphere(1)%level_pressure(k) < 200.0_r_kind) &
               atmosphere(1)%absorber(k,3) = atmosphere(1)%absorber(k,3) * &
              (0.977_r_kind + 0.000115_r_kind * atmosphere(1)%pressure(k))
        end do

!    Set up to return Tb jacobians.  Zero atmosphere
!    and surface jacobian structures
        do i=1,nchanl
           rtsolution_k(i,1)%radiance = zero
           rtsolution_k(i,1)%brightness_temperature = one
        end do
        call crtm_atmosphere_zero(atmosphere_k(:,:))
        call crtm_surface_zero(surface_k(:,:))


!       Call CRTM K Matrix model
        error_status = crtm_k_matrix(atmosphere,surface,rtsolution_k,&
            geometryinfo,channelinfo(sensorindex:sensorindex),atmosphere_k,&
            surface_k,rtsolution,options=options)

! If the CRTM returns an error flag, do not assimilate any channels for this ob 
! and set the QC flag to 10.
! We currently go through the rest of the QC steps, ensuring that the diagnostic
! files are populated, but this could be changed if it causes problems.  
        if (error_status /=0) then
           write(6,*)'RAD_TRAN_K:  ***ERROR*** during crtm_k_matrix call ',&
           error_status
           id_qc(1:nchanl) = 10
           varinv(1:nchanl) = zero
        endif

!       Compute transmittance from layer optical depths
        do i=1,nchanl
           emissivity(i)   = rtsolution(i,1)%surface_emissivity
           emissivity_k(i) = rtsolution_k(i,1)%surface_emissivity
           ts(i)   = surface_k(i,1)%water_temperature + &
                     surface_k(i,1)%land_temperature + &
                     surface_k(i,1)%ice_temperature + &
                     surface_k(i,1)%snow_temperature
           if (abs(ts(i))<sqrt_tiny_r_kind) ts(i) = sign(sqrt_tiny_r_kind,ts(i))
           if (surface(1)%wind_speed>small_wind) then
              term = surface_k(i,1)%wind_speed * f10*f10 / surface(1)%wind_speed
              uwind_k(i) = term * uwind
              vwind_k(i) = term * vwind
           else
              uwind_k(i)    = zero
              vwind_k(i)    = zero
           endif


!          Zero jacobian and transmittance arrays
           do k=1,nsig
              temp(k,i)   = zero
              wmix(k,i)   = zero
              omix(k,i)   = zero
              ptau5(k,i)  = zero
           end do

           total_od = zero
!          Accumulate values from extended into model layers
           do k=1,msig
              kk = klevel(msig-k+1)
              temp(kk,i) = temp(kk,i) + atmosphere_k(i,1)%temperature(k)
              wmix(kk,i) = wmix(kk,i) + atmosphere_k(i,1)%absorber(k,1)
              omix(kk,i) = omix(kk,i) + atmosphere_k(i,1)%absorber(k,2)
              total_od   = total_od + rtsolution(i,1)%layer_optical_depth(k)
!             if (total_od*secant < limit_exp)ptau5(kk,i) = exp(-total_od*secant_term)
              ptau5(kk,i) = exp(-min(limit_exp,total_od*secant_term))
           end do
           do k=1,nsig
              if (abs(temp(k,i))<sqrt_tiny_r_kind) temp(k,i)=sign(sqrt_tiny_r_kind,temp(k,i))
           end do
        end do

      
!*****
!     COMPUTE AND APPLY BIAS CORRECTION TO SIMULATED VALUES
!*****

!       Construct predictors for 1B radiance bias correction.
        do i=1,nchanl
           pred(1,i) = r0_01
           pred(2,i) = one_tenth*(secant_term-one)**2-.015_r_kind
           if(ssmi .or. ssmis .or. amsre)pred(2,i)=zero
           pred(3,i) = zero
        end do
 
!       Compute predictor for microwave cloud liquid water bias correction.
        clw=zero
        ierrret=0
        tpwc=zero
        kraintype=0
        if(microwave .and. sea)then
 
           if (amsua) then
 
              if(tsavg5>t0c)then
                 tbcx1=rtsolution(1,1)%brightness_temperature+cbias(nadir,ich(1))*ang_rad(ich(1))
                 tbcx2=rtsolution(2,1)%brightness_temperature+cbias(nadir,ich(2))*ang_rad(ich(2))
                 if (tbcx1 <=r284 .and. tbcx2<=r284 .and. tb_obs(1) > zero &
                      .and. tb_obs(2) > zero) &
                    clw=amsua_clw_d1*(tbcx1-tb_obs(1))/(r285-tbcx1)+ &
                        amsua_clw_d2*(tbcx2-tb_obs(2))/(r285-tbcx2)
              end if
         
           else if(ssmi) then

              call retrieval_mi(tb_obs(1),nchanl, ssmi,ssmis,no85GHz, &
                   tpwc,clw,si85,kraintype,ierrret ) 
 
           else if (ssmis) then
 
!          Compute guess total precipitable water
!             tpw5 = zero
!             do k=1,nsig
!                tpw5 = tpw5 + qvp(k) * &
!                     tpwcon*r10*(prsitmp(k)-prsitmp(k+1))
!             end do

              call ret_ssmis( tb_obs(1),nchanl,ssmis,tpwc, clw, ierrret)

           else if (amsre) then
 
              call retrieval_amsre(                                 &   
                   tb_obs(1),amsre_low,amsre_mid,amsre_hig,  &
                   uwind,vwind,f10,tsavg5,                          &
                   tpwc,clw,si85,kraintype,ierrret ) 
 
           endif
           clw = max(zero,clw)

           if (amsre) then
              do i=1,nchanl
                 pred(3,i) = clw
              end do
           else
              do i=1,nchanl
                 pred(3,i) = clw*cosza*cosza
              end do
           end if
        end if

!       Apply bias correction
        predterms=zero
        do i=1,nchanl
           mm=ich(i)
 
           predterms(1,i) = cbias(nadir,mm)*ang_rad(mm)             !global_satangbias
           tlapchn(i)= (ptau5(2,i)-ptau5(1,i))*(tsavg5-temp5(2))
           do k=2,nsig-1
              tlapchn(i)=tlapchn(i)+&
                   (ptau5(k+1,i)-ptau5(k,i))*(temp5(k-1)-temp5(k+1))
           end do
           tlapchn(i) = r0_01*tlapchn(i)
           tlap = tlapchn(i)-tlapmean(mm)
           pred(4,i)=tlap*tlap
           pred(5,i)=tlap
           do j=1,npred
              pred(j,i)=pred(j,i)*air_rad(mm)
           end do
 
           predterms(2,i) = pred(5,i)*predx(npred,mm)                 !tlap
           predterms(3,i) = pred(4,i)*predx(npred-1,mm)               !tlap*tlap
           do j = 1,npred-2
              predterms(j+3,i) = predx(j,mm)*pred(j,i)
           end do
 
!          Apply SST dependent bias correction with cubic spline
           if (retrieval) then
              call spline_cub(fbias(:,mm),tsavg5,ys_bias_sst)
              predterms(6,i) = ys_bias_sst
           endif

!          tbc    = obs - guess after bias correction
!          tbcnob = obs - guess before bias correction

           tbcnob(i)    = tb_obs(i) - rtsolution(i,1)%brightness_temperature  
           tbc(i)       = tbcnob(i)                     
 
           do j=1,npred+1
              tbc(i)=tbc(i) - predterms(j,i) !obs-ges with bias correction
           end do
 
           error0(i)     = tnoise(i)
           if(tnoise(i) < 1.e4_r_kind .or. (iuse_rad(ich(i))==-1  &
                .and. rad_diagsave))then
              varinv(i)     = val_obs/tnoise(i)**2
              errf(i)       = tnoise(i)
           else
              if(id_qc(i) == 0)id_qc(i)=1
              varinv(i)     = zero
              errf(i)       = zero
           endif
!       End of loop over channels         
        end do

!******
!    QC OBSERVATIONS BASED ON VARIOUS CRITERIA
!            Separate blocks for various instruments.
!******
     
!  ---------- IR -------------------
!       QC HIRS/2, GOES, HIRS/3 and AIRS sounder data
!
        ObsQCs: if (hirs .or. goessndr .or. airs .or. iasi) then

!     
!          Reduce weight given to obs for shortwave ir if 
!          solar zenith angle tiny_r_kind
           if (pangs <= 89.0_r_kind .and. (sfcpct(1) > zero)) then
!             QC2 in statsrad
              if(luse(n))aivals(9,is) = aivals(9,is) + one
              do i=1,nchanl
!                ll = ich(i)
!                if(sc(sensorindex)%wavenumber(ll) > r2000)then
                 if(sc(sensorindex)%wavenumber(i) > r2000)then
!                   if(sc(sensorindex)%wavenumber(ll) > r2400)then
                    if(sc(sensorindex)%wavenumber(i) > r2400)then
                       varinv(i)=zero
                       if(id_qc(i) == 0)id_qc(i)=2
                    else
!                      tmp=one-(sc(sensorindex)%wavenumber(ll)-r2000)*ptau5(1,i)&
                       tmp=one-(sc(sensorindex)%wavenumber(i)-r2000)*ptau5(1,i)&
                            *max(zero,cos(pangs*deg2rad))*oneover400
                       varinv(i)=tmp*varinv(i)
                       if(id_qc(i) == 0)id_qc(i)=3
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

!          If GOES and lza > 60. do not use
           if( goessndr .and. zasat*rad2deg > r60) then
!             QC5 in statsrad
              if(luse(n))aivals(12,is) = aivals(12,is) + one
              do i=1,nchanl
                 varinv(i) = zero
                 if(id_qc(i) == 0)id_qc(i)=4
              end do
           end if

!          Reduce weight for obs over higher topography
           sfchgtfact=one
           if (zsges > r2000) then
!             QC1 in statsrad
              if(luse(n))aivals(8,is) = aivals(8,is) + one
              sfchgtfact    = (r2000/zsges)**4
!             if(id_qc(i) == 0)id_qc(i)=5
           endif

!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              m=ich(i)
              if (tb_obs(i) > r1000 .or. tb_obs(i) <= zero) varinv(i)=zero
              varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))

!             Modify error based on transmittance at top of model
              varinv(i)=varinv(i)*ptau5(nsig,i)
              errf(i)=errf(i)*ptau5(nsig,i)

!       NOTE:  The qc below uses the inverse squared obs error.
!              If a particular channel is not being assimilated
!              (iuse_rad==-1), we should not use this channel
!              in the qc.  The loop below loads array varinv_use
!              such that this condition is satisfied.  Array
!              varinv_use is then used in the qc calculations.

              varinv_use(i) = varinv(i)
              if (iuse_rad(m)<0) varinv_use(i) = zero
           end do

!          QC based on presence/absence of cloud

           do k=1,nsig
              do i=1,nchanl
                 dtb(i,k)=(temp5(k)-tsavg5)*ts(i)
                 do kk=1,k-1
                    dtb(i,k)=dtb(i,k)+(temp5(k)-temp5(kk))*temp(kk,i)
                 end do
              end do
           end do
           sum3=zero
           do i=1,nchanl
              sum3=sum3+tbc(i)*tbc(i)*varinv_use(i)
           end do
           sum3=0.75_r_kind*sum3
        
           lcloud=0
           cld=zero
!          cldp=ten*prsr5(1)
           cldp=prsr5(1)
           do k=1,nsig
              if(prsr5(k) > trop5)then
                 sum=zero
                 sum2=zero
                 do i=1,nchanl
                    sum=sum+tbc(i)*dtb(i,k)*varinv_use(i)
                    sum2=sum2+dtb(i,k)*dtb(i,k)*varinv_use(i)
                 end do
                 if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
                 cloudp=min(max(sum/sum2,zero),one)
                 sum=zero
                 do i=1,nchanl
                    tmp=tbc(i)-cloudp*dtb(i,k)
                    sum=sum+tmp*tmp*varinv_use(i)
                 end do
                 if(sum < sum3)then
                    sum3=sum
                    lcloud=k
                    cld=cloudp
                    cldp=prsr5(k)
                 end if
              end if
           
           end do
      
           if ( lcloud > 0 ) then  ! If cloud detected, reject channels affected by it. 

              do i=1,nchanl

!                If more than 2% of the transmittance comes from the cloud layer,
!                   reject the channel (0.02 is a tunable parameter)

                 if ( ptau5(lcloud,i) > 0.02_r_kind) then 
!                   QC4 in statsrad
                    if(luse(n))aivals(11,is)   = aivals(11,is) + one
                    varinv(i) = zero
                    varinv_use(i) = zero
                    if(id_qc(i) == 0)id_qc(i)=6
                 end if
              end do

!             If no clouds check surface temperature/emissivity

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
!                      QC3 in statsrad
                       if(luse(n) .and. varinv(i) > zero) &
                            aivals(10,is)   = aivals(10,is) + one
                       varinv(i) = zero
                       if(id_qc(i) == 0)id_qc(i)=7
                    end if
                 end do
              end if
           endif

           cenlatx=abs(cenlat)*oneover25
           if (cenlatx < one) then
              if(luse(n))aivals(6,is) = aivals(6,is) + one
              efact   = half*(cenlatx+one)
           else
              efact = one
           endif

!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              if(varinv(i) > tiny_r_kind)then
                 errf(i)=efact*errf(i)
                 dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
                 term = dtbf*dtbf
                 if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
              end if
           end do

!       End of HIRS and GOES QC blocks


!  --------- MSU -------------------
!       QC MSU data
        else if (msu) then

           vfact = one

!          Reduce qc bounds in tropics
           cenlatx=abs(cenlat)*oneover25
           if (cenlatx < one) then
              if(luse(n))aivals(6,is) = aivals(6,is) + one
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

!          Apply window test to channel 2 using channel 1
           if (abs(tbc(1)) > five) then
              errf(2) = zero
              varinv(2) = zero
              if(id_qc(2) == 0)id_qc(2)=2
!             QC1 in statsrad
              if(luse(n))aivals(8,is)   = aivals(8,is) + one
           endif

!          Reduce q.c. bounds over higher topography
           if (zsges > r2000) then
!             QC2 in statsrad
              if(luse(n))aivals(9,is)   = aivals(9,is) + one
              fact = r2000/zsges
              errf(1) = fact*errf(1)
              errf(2) = fact*errf(2)
              errf(3) = fact*errf(3)
!             if(id_qc(1) == 0)id_qc(1)=3
!             if(id_qc(2) == 0)id_qc(2)=3
!             if(id_qc(3) == 0)id_qc(3)=3
              vfact = fact
           end if



!          Generate q.c. bounds and modified variances.
           errf(3) = two*errf(3)
           errf(4) = two*errf(4)
           do i=1,nchanl
 
!             Modify error based on transmittance at top of model
              varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
              errf(i)=efact*errf(i)*ptau5(nsig,i)
 
              if(varinv(i) > tiny_r_kind)then
                 dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
                 term = dtbf*dtbf
                 if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
              end if
           end do

!
!       End of MSU QC block


!  ---------- AMSU-A -------------------
!       QC AMSU-A data
        else if (amsua) then


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
         
!          Reduce qc bounds in tropics
           cenlatx=abs(cenlat)*oneover25
           if (cenlatx < one) then
              if(luse(n))aivals(6,is) = aivals(6,is) + one
              efact   = cenlatx*quarter+0.75_r_kind
           else
              efact   = one
           endif

           efactmc = one
           vfactmc = one
           tb_obsbc1=tb_obs(1)-cbias(nadir,ich(1))
!          sval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*tb_obsbc1)*tb_obsbc1 +  &
!               0.454_r_kind*tb_obsbc2-tb_obsbc15
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
!          QC6 in statsrad
           if(clwx >= one .and. luse(n))aivals(13,is) = aivals(13,is) + one
           factch4=clwx**2+(tbc(4)*w2f4)**2
!          factch6x=((sval-five)/r10)**2+(tbc(6)/0.8_r_kind)**2
!          QC7 in statsrad
           if(dsval >= one .and. luse(n))aivals(14,is) = aivals(14,is) + one
           factch6=dsval**2+(tbc(6)*w2f6)**2
         
           tpwc=factch4

           if(factch6 >= one)then
              efactmc=zero
              vfactmc=zero
              errf(6)=zero
              varinv(6)=zero
              do i=1,6
                 if(id_qc(i) == 0)id_qc(i)=2
              end do
              if(id_qc(15) == 0)id_qc(15)=2
!             QC3 in statsrad
              if(.not. mixed.and. luse(n))aivals(10,is) = aivals(10,is) + one

           else if(factch4 > half)then
              efactmc=zero
              vfactmc=zero
              do i=1,5
                 if(id_qc(i) == 0)id_qc(i)=3
              end do
              if(id_qc(15) == 0)id_qc(15)=3
!             QC1 in statsrad
              if(luse(n)) aivals(8,is) = aivals(8,is) + one

           else if(sea)then
!             QC based on ratio of obs-ges increment versus the sensitivity of
!             the simulated brightness temperature to the surface emissivity
!             Y2K hurricane season runs by QingFu Liu found the hurricane
!             forecast tracks to be degraded without this QC.
!             (Is this still true?)

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
!                QC2 in statsrad
                 if(luse(n))aivals(9,is) = aivals(9,is) + one
                 efactmc=zero
                 vfactmc=zero
                 do i=1,5
                    if(id_qc(i) == 0)id_qc(i)=4
                 end do
                 if(id_qc(15) == 0)id_qc(15)=4
              end if
           end if
 
!          Reduce q.c. bounds over higher topography
           if (zsges > r2000) then
!             QC4 in statsrad
              if(luse(n))aivals(11,is) = aivals(11,is) + one
              fact    = r2000/zsges
              efactmc = fact*efactmc
              errf(6) = fact*errf(6)
              vfactmc = fact*vfactmc
              varinv(6)  = fact*varinv(6)
              if (zsges > r4000) then
!                QC5 in statsrad
                 if(luse(n))aivals(12,is) = aivals(12,is) + one
                 fact   = r4000/zsges
                 errf(7)= fact*errf(7)
                 varinv(7)= fact*varinv(7)
              end if
           end if

!          Generate q.c. bounds and modified variances.
           do i=1,nchanl

!             Modify error based on transmittance at top of model
              varinv(i)=varinv(i)*ptau5(nsig,i)
              errf(i)=errf(i)*ptau5(nsig,i)

              if(varinv(i) > tiny_r_kind)then
                 dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
                 term=dtbf*dtbf
                 if(i <= 4 .or. i == 15)then
               
!                   Adjust observation error based on magnitude of liquid
!                   water correction.  0.2 is empirical factor
 
                    term=term+0.2_r_kind*(predx(3,ich(i))*pred(3,i))**2

                    errf(i)   = efactmc*errf(i)
                    varinv(i) = vfactmc*varinv(i)
                 end if
                 errf(i)   = efact*errf(i)
                 if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
              end if

              if ( (i <= 5 .or. i == 15) .and. &
                   (varinv(i)<1.e-9_r_kind) ) then
                 pred(3,i) = zero
              end if
            
           end do

!       End of AMSU-A QC block
!
!  ---------- AMSU-B -------------------
!       QC AMSU-B and MHS data

        else if (amsub .or. hsb .or. mhs) then

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
!                QC3 in statsrad
                 if(luse(n) .and. dsi >= one)aivals(10,is) = aivals(10,is) + one
              end if
!             si=42.72_r_kind+0.85_r_kind*tbc(1)-tbc(2)
           else
              dsi=0.85_r_kind*tbc(1)-tbc(2)
!             si=42.72_r_kind+0.85_r_kind*tb_obs(1)-tb_obs(2)
!             QC4 in statsrad
              if(luse(n) .and. dsi >= one)aivals(11,is) = aivals(11,is) + one
           end if
           dsi=max(zero,dsi)
           fact1=((tbc(1)-7.5_r_kind*dsi)/ten)**2+(dsi)**2

!  Allow saving of qc factors
           clw = dsi
           tpwc = fact1


           if(fact1 > one)then
              vfact=zero
!             QC1 in statsrad
              if(luse(n))aivals(8,is) = aivals(8,is) + one
              do i=1,nchanl
                 if(id_qc(i) == 0)id_qc(i)=2
              end do
           else
              efact = (one-fact1*fact1)*efact
              vfact = (one-fact1*fact1)*vfact
!             Reduce q.c. bounds over higher topography
              if (zsges > r2000) then
!                QC2 in statsrad
                 if(luse(n))aivals(9,is) = aivals(9,is) + one
                 fact = r2000/zsges
                 efact = fact*efact
                 vfact = fact*vfact
              end if
           end if

!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              if ((mhs .or. amsub) .and. i >= 3 .and. varinv(i) > tiny_r_kind) then  ! wv sounding channels
                 if (abs(tbc(i)) >= two) varinv(i) = zero
              else   ! other channels or other sensors
!             Modify error based on transmittance at top of model
                  varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
                  errf(i)=efact*errf(i)*ptau5(nsig,i)
                  if(varinv(i)>tiny_r_kind)then
                     dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
                     term=dtbf*dtbf
                     if(term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
                  end if
              end if
           end do

!       End of AMSU-B QC block
!
!  ---------- GOES imager --------------
!       GOES imager Q C
!
        else if(goes_img)then


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
                 fact2=zero
                 fact4=zero
                 fact5=zero
                 demisf = r0_01
                 dtempf = two
              else if(ice)then
                 fact2=zero
                 fact3=zero
                 fact4=zero
                 fact5=zero
                 demisf = r0_02
                 dtempf = three
              else if(snow)then
                 fact2=zero
                 fact3=zero
                 fact4=zero
                 fact5=zero
                 demisf = r0_02
                 dtempf = three
              else
                 fact2=zero
                 fact3=zero
                 fact4=zero
                 fact5=zero
                 demisf = r0_02
                 dtempf = five
              end if
            
!             Filter out data according to clear sky fraction
              if(dplat(is) == 'g10' .and. cld <r40 ) then
                 fact2=zero
                 fact3=zero
                 fact4=zero
                 fact5=zero
!                QC7 in statsrad
                 if(luse(n))aivals(14,is)= aivals(14,is) + one
              else if(dplat(is) == 'g12' .and. cld <r70 ) then
                 fact2=zero
                 fact3=zero
                 fact4=zero
                 fact5=zero
!                QC7 in statsrad
                 if(luse(n))aivals(14,is)= aivals(14,is) + one
              end if
            
!             Quality control according to brightness temperature 
!             standard deviation from data
              if(tb_obs_sdv(1) >one ) then
                 fact2=zero
!                QC3 in statsrad
                 if(luse(n))aivals(10,is)= aivals(10,is) + one
              end if
            
              if(tb_obs_sdv(2) >1.5_r_kind ) then
                 fact3=zero
!                QC4 in statsrad
                 if(luse(n))aivals(11,is)= aivals(11,is) + one
              end if
            
              if(tb_obs_sdv(3) >one ) then
                 fact4=zero
!                QC5 in statsrad
                 if(luse(n))aivals(12,is)= aivals(12,is) + one
              end if
            
              if(tb_obs_sdv(4) >one ) then
                 fact5=zero
!                QC6 in statsrad
                 if(luse(n))aivals(13,is)= aivals(13,is) + one
              end if
            
!             Reduce weight for obs over higher topography
              if (zsges > r2000) then
                 fact    = r2000/zsges
                 efact   = fact*efact
                 vfact   = fact*vfact
!                QC2 in statsrad
                 if(luse(n))aivals(9,is)= aivals(9,is) + one
              end if
           else
              vfact=zero
           end if
            
!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              varinv(i) = vfact*varinv(i)
              if(i == 1)varinv(i) = fact2*varinv(i)
              if(i == 2)varinv(i) = fact3*varinv(i)
              if(i == 3)varinv(i) = fact4*varinv(i)
              if(i == 4)varinv(i) = fact5*varinv(i)
              if( dplat(is) == 'g10' .and. i== 2) then
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
                 if (tb_obs_sdv(2) >r1_4 )                         &
                    varinv(i)=varinv(i)/1.48_r_kind
              else if(dplat(is) == 'g12' .and. i== 2) then
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

!       End of GOES imager QC block
!

!  ---------- SEVIRI  -------------------
!       SEVIRI Q C

        else if (seviri) then

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

!             use chn 2 and 3 over both sea and land while other IR chns only over sea
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
                 end if
              else
                 efact=zero
                 vfact=zero
              end if

!             Reduce weight for obs over higher topography
!             QC_terrain: If seviri and terrain height > 1km. do not use
              if (zsges > r1000) then
                 efact   = zero
                 vfact   = zero
!                QC2 in statsrad
                 if(luse(n))aivals(9,is)= aivals(9,is) + one
              end if

!             gross check
!             QC_o-g: If abs(o-g) > 2.0 do not use
              if ( abs(tbc(i)) > two ) then
                 vfact = zero
                 efact = zero
                 if(id_qc(i) == 0 ) id_qc(i)=2   !hliu check 
!                QC1 in statsrad
                 if(luse(n))aivals(8,is)= aivals(8,is) + one  !hliu check
              end if

!             modified variances.
              errf(i)   = efact*errf(i)
              varinv(i) = vfact*varinv(i)

!             Modify error based on transmittance at top of model
!             need this for SEVIRI??????
!             varinv(i)=varinv(i)*ptau5(nsig,i)
!             errf(i)=errf(i)*ptau5(nsig,i)

              if(varinv(i) > tiny_r_kind)then
                 dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
                 term = dtbf*dtbf
                 if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
              end if
           end do

!       End of SEVIRI imager QC block
!

!  ---------- AVRHRR --------------
!       NAVY AVRHRR Q C

        else if (avhrr_navy .or. avhrr) then

           if(tb_obs(2) > zero .and. tb_obs(2) < 400.0_r_kind)then
              fact=one
              efact=one
              if (.not. sea)then
                 fact    = zero
!                QC1 in statsrad
                 if(luse(n)) aivals(12,is)= aivals(12,is) + one
              end if
           else
              vfact = zero
           end if

!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              errf(i)   = efact*errf(i)
              varinv(i) = fact*varinv(i)
!             Reject day time AVHRR Ch-3A observation
              if ( i == 1 .and. dtp_avh /= 152.0_r_kind ) then          ! day time
                 varinv(i) = zero
!                QC3 in statsrad
                 if(luse(n))aivals(10,is)= aivals(10,is) + one
              endif
           end do

!       End of AVHRR QC block
!
!  ---------- SSM/I , SSMIS, AMSRE  -------------------
!       SSM/I, SSMIS, & AMSRE Q C

        else if( ssmi .or. amsre .or. ssmis )then   

           call qcssmi(nchanl, &
                zsges,luse(n),sea,ice,snow,mixed, &
                ts,emissivity_k,ierrret,kraintype,tpwc,clw,sgagl, &
                tbcnob,tb_obs,ssmi,amsre_low,amsre_mid,amsre_hig,ssmis, &
                ssmis_uas,ssmis_las,ssmis_env,ssmis_img, &
                varinv,errf,aivals(1,is),id_qc)

!  ---------- SSU  -------------------
!       SSU Q C

        elseif (ssu) then

           if(sea)then
              demisf  = r0_01*half
              dtempf  = half*half
           else if(land)then
              demisf = r0_02*half
              dtempf = two*half
           else if(ice)then
              demisf = r0_03*half
              dtempf = four*half
           else if(snow)then
              demisf = r0_02*half
              dtempf = two*half
           else
              demisf = r0_03*half
              dtempf = four*half
           end if

!          Reduce weight for obs over higher topography
           sfchgtfact=one
           if (zsges > r2000) then
              sfchgtfact    = (r2000/zsges)**4
              if(luse(n)) aivals(11,is)= aivals(11,is) + one
           endif
         
!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              m=ich(i)
              if (tb_obs(i) > r400 .or. tb_obs(i) <= r100) then
                 varinv(i)=zero
                 if(luse(n)) aivals(12,is)= aivals(12,is) + one
              endif
              varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))

!             Modify error based on transmittance at top of model
              varinv(i)=varinv(i)*ptau5(nsig,i)
              errf(i)=errf(i)*ptau5(nsig,i)
           end do

!          Reduce qc bounds in tropics
           cenlatx=abs(cenlat)*oneover25
           if (cenlatx < one) then
              if(luse(n))aivals(6,is) = aivals(6,is) + one
              efact = half*(cenlatx+one)
           else
              efact = one
           endif
                                                                                                
!          Generate q.c. bounds and modified variances.
           do i=1,nchanl
              if(varinv(i) > tiny_r_kind)then
                 errf(i)=efact*errf(i)
                 dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
                 term = dtbf*dtbf
                 if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
              end if
           end do
 
!       End of SSU qc block
        end if ObsQCs

!       Done with sensor qc blocks.  Now make final qc decisions.


!       Apply gross check to observations.  Toss obs failing test.
        do i = 1,nchanl
           if (varinv(i) > tiny_r_kind ) then
              m=ich(i)

              if (avhrr .or. avhrr_navy ) then
                 errf(i) = min(2.2_r_kind*errf(i),ermax_rad(m))
                 if ( tbc(i) < -errf(i) .or. tbc(i) > 2.4_r_kind*errf(i) ) then
                    if(id_qc(i) == 0)id_qc(i)=8
                    varinv(i) = zero
                    if(luse(n))stats(2,m) = stats(2,m) + one
                    if(luse(n))aivals(7,is) = aivals(7,is) + one
                 endif

              else

                 errf(i) = min(three*errf(i),ermax_rad(m))
                 if (abs(tbc(i)) > errf(i)) then
!                   If mean obs-ges difference around observations
!                   location is too large and difference at the 
!                   observation location is similarly large, then
!                   toss the observation.
                    if(id_qc(i) == 0)id_qc(i)=8
                    varinv(i) = zero
                    if(luse(n))stats(2,m) = stats(2,m) + one
                    if(luse(n))aivals(7,is) = aivals(7,is) + one
                 end if

              endif
           end if
        end do

        if(amsua .or. amsub .or. mhs .or. msu .or. hsb)then
           if(amsua)nlev=6
           if(amsub .or. mhs)nlev=5
           if(hsb)nlev=4
           if(msu)nlev=4
           kval=0
           do i=2,nlev
!          do i=1,nlev
              if (varinv(i)<tiny_r_kind .and. iuse_rad(ich(i))>=1) then
                 kval=max(i-1,kval)
                 if(amsub .or. hsb .or. mhs)kval=nlev
                 if(amsua .and. i <= 3)kval = zero
              end if
           end do
           if(kval > 0)then
              do i=1,kval
                 varinv(i)=zero
                 if(id_qc(i) == 0)id_qc(i)=9
              end do
              if(amsua)then
                 varinv(15)=zero
                 if(id_qc(15) == 0)id_qc(15)=9
              end if
           end if
        end if

!       If requested, generate SST retrieval (output)
        if(retrieval) then
           if(avhrr_navy .or. avhrr) then
              call avhrr_sst_retrieval(obstype,dplat(is),nchanl,tnoise,&
                   varinv,tsavg5,sstnv,sstcu,temp,wmix,ts,tbc,cenlat,cenlon,zasat,&
                   dtime,dtp_avh,tlapchn,predterms,emissivity,pangs,tbcnob,tb_obs,&
                   rad_diagsave,sfcpct,id_qc,nadir,ireal,ipchan,luse(n))
              go to 100
           endif
        endif

        icc = 0
        do i = 1,nchanl

!          Only process observations to be assimilated

           if (varinv(i) > tiny_r_kind ) then

              m = ich(i)
              if(luse(n))then
                 drad    = tbc(i)   
                 dradnob = tbcnob(i)
                 varrad  = drad*varinv(i)
                 stats(1,m)  = stats(1,m) + one              !number of obs
                 stats(3,m)  = stats(3,m) + drad             !obs-mod(w_biascor)
                 stats(4,m)  = stats(4,m) + tbc(i)*drad      !(obs-mod(w_biascor))**2
                 stats(5,m)  = stats(5,m) + tbc(i)*varrad    !penalty contribution
                 stats(6,m)  = stats(6,m) + dradnob          !obs-mod(w/o_biascor)

                 exp_arg = -half*(tbc(i)/error0(i))**2
                 error=sqrt(varinv(i))
                 if (pg_rad(m) > tiny_r_kind .and. error > tiny_r_kind) then
                    arg  = exp(exp_arg)
                    wnotgross= one-pg_rad(m)
                    cg_rad=b_rad(m)*error
                    wgross = cg_term*pg_rad(m)/(cg_rad*wnotgross)
                    term = log((arg+wgross)/(one+wgross))
                    wgt  = one-wgross/(arg+wgross)
                 else
                    term = exp_arg
                    wgt  = one
                 endif
                 stats(7,m)  = stats(7,m) -two*(error0(i)**2)*varinv(i)*term
              end if
           
!             Only "good" obs are included in J calculation.
              if (iuse_rad(m) >= 1)then
                 if(luse(n))then
                    aivals(40,is) = aivals(40,is) + tbc(i)*varrad
                    aivals(39,is) = aivals(39,is) -two*(error0(i)**2)*varinv(i)*term
                    aivals(38,is) = aivals(38,is) +one
                    if(wgt < wgtlim) aivals(2,is)=aivals(2,is)+one
                 end if


!******
!    CONSTRUCT SENSITIVITY VECTORS.  WRITE TO OUTPUT FILE.
!******

!                Load arrays used in minimization

                 icc      = icc+1                ! total channel count
                 radinv(icc) = tbc(i)            ! obs-ges innovation
                 icxx(icc) = m                   ! channel index
                 var(icc) = one/error0(i)**2     ! 1/(obs error)**2  (original uninflated error)
                 ratio_raderr(icc)=error0(i)**2*varinv(i) ! (original error)/(inflated error)
 
!                Load jacobian for temperature (dTb/dTv).  The factor c2
!                converts the temperature from sensible to virtual
                 do k = 1,nsig
                    htlto(k,icc) = temp(k,i)*c2(k)
                 end do

!                Load jacobian for moisture (dTb/dq)
                 do k = 1,nsig
                    htlto(nsig+k,icc)=c5(k)*wmix(k,i)-c4(k)*temp(k,i)

!                   Deflate moisture jacobian above the tropopause.
                    if (pin5(k) < trop5) then
                       ifactq(m)=15
                       term = (pin5(k)-trop5)/(trop5-pin5(nsig))
                       htlto(nsig+k,icc) = exp(ifactq(m)*term)*htlto(nsig+k,icc)
                    endif
                 end do

!                Load jacobian for ozone (dTb/doz).  For hirs and goes channel 9
!                (ozone channel) we do not let the observations change the ozone.
!                There currently is no ozone analysis when running in the NCEP 
!                regional mode, therefore set ozone jacobian to 0.0
                 if ( regional .or. ((hirs .or. goessndr).and.(varinv(ich9) < tiny_r_kind))) then
                    do k = 1,nsig
                       htlto(nsig2+k,icc) = zero
                    end do
                 else
                    do k = 1,nsig
                       htlto(nsig2+k,icc) = omix(k,i)*constoz
                    end do
                 endif

!                Load Jacobian for wind speed (dTb/du, dTb/dv)
                 if( dtbduv_on .and. microwave) then
                    htlto(nsig3p1,icc) = uwind_k(i)
                    htlto(nsig3p2,icc) = vwind_k(i)
                 else
                    htlto(nsig3p1,icc) = zero
                    htlto(nsig3p2,icc) = zero
                 end if

!                Load jacobian for skin temperature (dTb/dTskin)
                 htlto(nsig3p3,icc) = ts(i)

!             End of use data block
              end if

!          End of varinv>tiny_r_kind block
           endif

!       End loop over channels.
        end do

     endif ! (in_curbin)

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if rad_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>1) then
           ibin = NINT( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif
        IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

        if(in_curbin) then
!          Load data into output arrays
           if(.not. retrieval)then
              if(icc > 0)then
                 ncnt =ncnt+1
                 nchan_total=nchan_total+icc
 
                 if(.not. associated(radhead(ibin)%head))then
                    allocate(radhead(ibin)%head,stat=istat)
                    if(istat /= 0)write(6,*)' failure to write radhead '
                    radtail(ibin)%head => radhead(ibin)%head
                 else
                    allocate(radtail(ibin)%head%llpoint,stat=istat)
                    if(istat /= 0)write(6,*)' failure to write radtail%llpoint '
                    radtail(ibin)%head => radtail(ibin)%head%llpoint
                 end if

                 m_alloc(ibin) = m_alloc(ibin) +1
                 my_head => radtail(ibin)%head
                 my_head%idv = is
                 my_head%iob = n

                 allocate(radtail(ibin)%head%res(icc),radtail(ibin)%head%err2(icc), &
                          radtail(ibin)%head%diags(icc),&
                          radtail(ibin)%head%raterr2(icc),radtail(ibin)%head%pred(npred,icc), &
                          radtail(ibin)%head%dtb_dvar(nsig3p3,icc), &
                          radtail(ibin)%head%ich(icc),&
                          radtail(ibin)%head%icx(icc))

                 radtail(ibin)%head%nchan  = icc         ! profile observation count
                 call get_ij(mm1,slats,slons,radtail(ibin)%head%ij(1),radtail(ibin)%head%wij(1))
                 radtail(ibin)%head%time=dtime
                 radtail(ibin)%head%luse=luse(n)
                 radtail(ibin)%head%ich(:)=-1
                 iii=0
                 do ii=1,nchanl
                    m=ich(ii)
                    if (varinv(ii)>tiny_r_kind .and. iuse_rad(m)>=1) then

                       iii=iii+1
                       radtail(ibin)%head%res(iii)=radinv(iii)
                       radtail(ibin)%head%err2(iii)=var(iii)
                       radtail(ibin)%head%raterr2(iii)=ratio_raderr(iii)
                       radtail(ibin)%head%icx(iii)=icxx(iii)
                       do k=1,npred
                          radtail(ibin)%head%pred(k,iii)=pred(k,ii)
                       end do
                       do k=1,nsig3p3
                          radtail(ibin)%head%dtb_dvar(k,iii)=htlto(k,iii)
                       end do
                       my_head%ich(iii)=ii
                    end if
                 end do
              end if
           endif ! <.not. retrieval>
        endif ! (in_curbin)

!       Link obs to diagnostics structure
        iii=0
        do ii=1,nchanl
           if (.not.lobsdiag_allocated) then
              if (.not.associated(obsdiags(i_rad_ob_type,ibin)%head)) then
                 allocate(obsdiags(i_rad_ob_type,ibin)%head,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setuprad: failure to allocate obsdiags',istat
                    call stop2(276)
                 end if
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%head
              else
                 allocate(obsdiags(i_rad_ob_type,ibin)%tail%next,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setuprad: failure to allocate obsdiags',istat
                    call stop2(277)
                 end if
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%tail%next
              end if
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%muse(miter+1))
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%nldepart(miter+1))
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%tldepart(miter))
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%obssen(miter))
              obsdiags(i_rad_ob_type,ibin)%tail%indxglb=(n-1)*nchanl+ii
              obsdiags(i_rad_ob_type,ibin)%tail%nchnperobs=-99999
              obsdiags(i_rad_ob_type,ibin)%tail%luse=.false.
              obsdiags(i_rad_ob_type,ibin)%tail%muse(:)=.false.
              obsdiags(i_rad_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
              obsdiags(i_rad_ob_type,ibin)%tail%tldepart(:)=zero
              obsdiags(i_rad_ob_type,ibin)%tail%wgtjo=-huge(zero)
              obsdiags(i_rad_ob_type,ibin)%tail%obssen(:)=zero

              n_alloc(ibin) = n_alloc(ibin) +1
              my_diag => obsdiags(i_rad_ob_type,ibin)%tail
              my_diag%idv = is
              my_diag%iob = n
              my_diag%ich = ii
           else
              if (.not.associated(obsdiags(i_rad_ob_type,ibin)%tail)) then
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%head
              else
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%tail%next
              end if
              if (obsdiags(i_rad_ob_type,ibin)%tail%indxglb/=(n-1)*nchanl+ii) then
                 write(6,*)'setuprad: index error'
                 call stop2(278)
              endif
           endif

           if(in_curbin) then
              if (ii==1) obsptr => obsdiags(i_rad_ob_type,ibin)%tail
              if (ii==1) obsdiags(i_rad_ob_type,ibin)%tail%nchnperobs = nchanl
              obsdiags(i_rad_ob_type,ibin)%tail%luse = luse(n)
              obsdiags(i_rad_ob_type,ibin)%tail%nldepart(jiter) = tbc(ii)
              obsdiags(i_rad_ob_type,ibin)%tail%wgtjo=varinv(ii)
 
!             Load data into output arrays
              m=ich(ii)
              if (varinv(ii)>tiny_r_kind .and. iuse_rad(m)>=1) then
                 if(.not. retrieval)then
                    iii=iii+1
                    radtail(ibin)%head%diags(iii)%ptr => obsdiags(i_rad_ob_type,ibin)%tail
                    obsdiags(i_rad_ob_type,ibin)%tail%muse(jiter) = .true.
 
	     	     ! verify the pointer to obsdiags

	            my_head => radtail(ibin)%head
	            my_diag => radtail(ibin)%head%diags(iii)%ptr

                    if(my_head%idv      /= my_diag%idv .or. &
                       my_head%iob      /= my_diag%iob .or. &
                       my_head%ich(iii) /= my_diag%ich ) then
                       call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                             (/is,i,ii,ibin/))
                       call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,my_head%ich(iii)/))
                       call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
                       call die(myname)
                    endif

                 endif
              endif
           endif ! (in_curbin)
        enddo
        if(in_curbin) then
           if(.not. retrieval.and.(iii/=icc)) then
              write(6,*)'setuprad: error iii icc',iii,icc
              call stop2(279)
           endif
        endif ! (in_curbin)
 
!    End of l_may_be_passive block
     endif

     if(in_curbin) then
!       Write diagnostics to output file.
        if (rad_diagsave .and. luse(n)) then
           diagbuf(1)  = cenlat                         ! observation latitude (degrees)
           diagbuf(2)  = cenlon                         ! observation longitude (degrees)
           diagbuf(3)  = zsges                          ! model (guess) elevation at observation location
 
           diagbuf(4)  = dtime-time_offset              ! observation time (hours relative to analysis time)
 
           diagbuf(5)  = data_s(iscan_pos,n)            ! sensor scan position
           diagbuf(6)  = zasat*rad2deg                  ! satellite zenith angle (degrees)
           diagbuf(7)  = data_s(ilazi_ang,n)            ! satellite azimuth angle (degrees)
           diagbuf(8)  = pangs                          ! solar zenith angle (degrees)
           diagbuf(9)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
           diagbuf(10) = sgagl                          ! sun glint angle (degrees) (sgagl)
 
           diagbuf(11) = surface(1)%water_coverage         ! fractional coverage by water
           diagbuf(12) = surface(1)%land_coverage          ! fractional coverage by land
           diagbuf(13) = surface(1)%ice_coverage           ! fractional coverage by ice
           diagbuf(14) = surface(1)%snow_coverage          ! fractional coverage by snow
           diagbuf(15) = surface(1)%water_temperature      ! surface temperature over water (K)
           diagbuf(16) = surface(1)%land_temperature       ! surface temperature over land (K)
           diagbuf(17) = surface(1)%ice_temperature        ! surface temperature over ice (K)
           diagbuf(18) = surface(1)%snow_temperature       ! surface temperature over snow (K)
           diagbuf(19) = surface(1)%soil_temperature       ! soil temperature (K)
           diagbuf(20) = surface(1)%soil_moisture_content  ! soil moisture
           diagbuf(21) = surface(1)%land_type              ! surface land type
           diagbuf(22) = surface(1)%vegetation_fraction    ! vegetation fraction
           diagbuf(23) = surface(1)%snow_depth             ! snow depth
           diagbuf(24) = surface(1)%wind_speed             ! surface wind speed (m/s)
 
!          Note:  The following quantities are not computed for all sensors
           if (.not.microwave) then
              diagbuf(25)  = cld                        ! cloud fraction (%)
              diagbuf(26)  = cldp                       ! cloud top pressure (hPa)
           else
              diagbuf(25)  = clw                        ! cloud liquid water (kg/m**2)
              diagbuf(26)  = tpwc                       ! total column precip. water (km/m**2)
           endif

           if (goes_img) then
              do i=1,nchanl
                 diagbufex(1,i)=tb_obs_sdv(i)
              end do
           endif

           do i=1,nchanl
              diagbufchan(1,i)=tb_obs(i)       ! observed brightness temperature (K)
              diagbufchan(2,i)=tbc(i)          ! observed - simulated Tb with bias corrrection (K)
              diagbufchan(3,i)=tbcnob(i)       ! observed - simulated Tb with no bias correction (K)
              errinv = sqrt(varinv(i))
              diagbufchan(4,i)=errinv          ! inverse observation error
              useflag=one
              if (iuse_rad(ich(i)) < 1) useflag=-one
              diagbufchan(5,i)= id_qc(i)*useflag! quality control mark or event indicator

              diagbufchan(6,i)=emissivity(i)   ! surface emissivity
              diagbufchan(7,i)=tlapchn(i)      ! stability index
              do j=1,npred+1
                 diagbufchan(7+j,i)=predterms(j,i) ! Tb bias correction terms (K)
              end do
           end do

           if (lobsdiagsave) then
              if (l_may_be_passive) then
                 do ii=1,nchanl
                    if (.not.associated(obsptr)) then
                       write(6,*)'setuprad: error obsptr'
                       call stop2(280)
                    end if
                    if (obsptr%indxglb/=(n-1)*nchanl+ii) then
                       write(6,*)'setuprad: error writing diagnostics'
                       call stop2(281)
                    end if
 
                    ioff=7+npred+1
                    do jj=1,miter
                       ioff=ioff+1
                       if (obsptr%muse(jj)) then
                          diagbufchan(ioff,ii) = one
                       else
                          diagbufchan(ioff,ii) = -one
                       endif
                    enddo
                    do jj=1,miter+1
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%nldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%tldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%obssen(jj)
                    enddo

                    obsptr => obsptr%next
                 enddo
              else
                 ioff=7+npred+1
                 diagbufchan(ioff+1:ioff+4*miter+1,1:nchanl) = zero
              endif
           endif

           if (.not.lextra) then
              write(4) diagbuf,diagbufchan
           else
              write(4) diagbuf,diagbufchan,diagbufex
           endif

        end if
     endif ! (in_curbin)

100  continue

! End of n-loop over obs
  end do

! If retrieval, close open bufr sst file (output)
  if (retrieval.and.last_pass) call finish_sst_retrieval

! Jump here when there is no data to process for current satellite
! Deallocate arrays
  deallocate(diagbufchan)
  error_status = crtm_destroy(channelinfo)
  if (error_status /= success) &
  write(6,*)'OBSERVER:  ***ERROR*** crtm_destroy error_status=',error_status

  CALL crtm_atmosphere_destroy(atmosphere(1))
  CALL crtm_surface_destroy(surface(1))
  CALL crtm_rtsolution_destroy(rtsolution)
  CALL crtm_rtsolution_destroy(rtsolution_k)
  CALL crtm_options_destroy(options)
  if (crtm_atmosphere_associated(atmosphere(1))) &
       write(6,*)' ***ERROR** destroying atmosphere.'
  if (crtm_surface_associated(surface(1))) &
       write(6,*)' ***ERROR** destroying surface.'
  if (ANY(crtm_rtsolution_associated(rtsolution))) &
       write(6,*)' ***ERROR** destroying rtsolution.'
  if (ANY(crtm_rtsolution_associated(rtsolution_k))) &
       write(6,*)' ***ERROR** destroying rtsolution_k.'
  if (ANY(crtm_options_associated(options))) &
       write(6,*)' ***ERROR** destroying options.'
  deallocate(rtsolution,rtsolution_k,atmosphere_k,surface_k)


  if (rad_diagsave) then
     call dtime_show(myname,'diagsave:rad',i_rad_ob_type)
     close(4)
     if (lextra) deallocate(diagbufex)
  endif

135 continue

  if(n_aerosols>0)then
     deallocate(aero_names)
     deallocate(aero_types,iaero_types)
  endif
  deallocate(aero,iaero)

! End of routine
  return

  contains

  subroutine set_aero_types_(iaero_types,aero_types)
  use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
      DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL
  implicit none
  integer(i_kind), dimension(:),intent(out) :: iaero_types
  character(len=*),dimension(:),intent(in ) ::  aero_types
  if(n_aerosols<=0) return
  iaero_types=-1
  do i=1,n_aerosols
     if(aero_types(i)=='sulfate'            ) iaero_types(i)=SULFATE_AEROSOL
     if(aero_types(i)=='dust'               ) iaero_types(i)=DUST_AEROSOL 
     if(aero_types(i)=='dry_black_carbon'   ) iaero_types(i)=BLACK_CARBON_AEROSOL   ! crtm does not distinguish dry/wet
     if(aero_types(i)=='wet_black_carbon'   ) iaero_types(i)=BLACK_CARBON_AEROSOL   ! crtm does not distinguish dry/wet
     if(aero_types(i)=='dry_organic_carbon' ) iaero_types(i)=ORGANIC_CARBON_AEROSOL ! crtm does not distinguish dry/wet
     if(aero_types(i)=='wet_organic_carbon' ) iaero_types(i)=ORGANIC_CARBON_AEROSOL ! crtm does not distinguish dry/wet
     if(aero_types(i)=='ssam'               ) iaero_types(i)=SEASALT_SSAM_AEROSOL
     if(aero_types(i)=='sscm1'              ) iaero_types(i)=SEASALT_SSCM1_AEROSOL
     if(aero_types(i)=='sscm2'              ) iaero_types(i)=SEASALT_SSCM2_AEROSOL
     if(aero_types(i)=='sscm3'              ) iaero_types(i)=SEASALT_SSCM3_AEROSOL
  enddo
  if(any(iaero_types<0)) then
    write(6,*) 'set_aero_types_: trouble in aero settings for CRTM'
    call stop2(999)
  endif
  end subroutine set_aero_types_

  FUNCTION GOCART_Aerosol_size( kk,ITYPE,          &  ! Input
                                AERONAME,AEROTYPE, &  ! Input
                                                t, &  ! Input in K
                                                q, &  ! Input in g/kg
                                                p) &  ! Input in hPa
                                   RESULT( R_eff )    ! in micrometer
  use kinds, only: i_kind,r_kind
  use constants, only: g  => grav
  use constants, only: rd
  use constants, only: eps
  use crtm_module, only: SULFATE_AEROSOL,&
                         BLACK_CARBON_AEROSOL,&
                         ORGANIC_CARBON_AEROSOL,&
                         DUST_AEROSOL, &
                         SEASALT_SSAM_AEROSOL, &
                         SEASALT_SSCM1_AEROSOL, &
                         SEASALT_SSCM2_AEROSOL, &
                         SEASALT_SSCM3_AEROSOL
  use crtm_aerosolcoeff, ONLY: AeroC
  implicit none
! NOTES:
!   2010-06-10 todling  placed this function here temporarily
! REMARKS:
!   This function came from Mark in Paul van Delst's group.
!
!   Conversation with Arlindo da Silva suggests that the
!   ideal way to deal with the dependence of particle size on
!   humidity is to have a look-up table, instead of this function
!   here - which is tailored to a particular user.  A look up table
!   provides a more general for making the aerosols influence on GSI-CRTM
!   to the radiative transfer used in the underlying GCM. 
!   I am putting this function here temporarily until implementing a
!   look up table. Here is what I plan to do:
!   I'll introduce an aeroinfo.txt file that will have a table of the form:
!      aerosols_size::
!      ! aero_name  dim values
!      ::
!   e.g.,
!      aerosols_size::
!      ! aero_name  dim values
!      rh           50  0  2 4 6 8 ... 100
!      ss001        50  0.001 0.002 0.0023 ... (50 values of size as function of rh)
!      ss002        50  0.023 0.043 0.0063 ... (50 values of size as function of rh)
!      so4          50  0.003 0.003 0.0003 ... (constant in this case, for example)
!      ::
!   the first row in the table will always be rh, followed by
!   how many values of RH there are (50, here), followed by the relative humidity
!   themselves (doesn't have to be a linear scale). Then, all other rows
!   will correspond to a given aerosol, the same number of entries (50, in
!   the example here), followed by the effective size for that value of RH.
!
!   I will change radinfo to check for the presence of aerosols in the
!   gsi_chem_bundle and, when applicable, to consequently load the above table 
!   in memory. Lastly, this function will simple calculate RH and do a table
!   look-up, interpolating between two values to return the effective size.
!   
  integer(i_kind) ,INTENT(IN) :: kk,ITYPE
  REAL(r_kind)    ,INTENT(IN) :: t, q, p
  character(len=*),INTENT(IN) :: aeroname
  character(len=*),INTENT(IN) :: aerotype
!
  REAL(r_kind), PARAMETER :: CC = (1.0_r_kind/0.622_r_kind-1.0_r_kind)/1000.0_r_kind
!_RT  REAL(r_kind), PARAMETER :: Rd = 287.054_r_kind
!_RT  REAL(r_kind), PARAMETER :: g = 9.80665_r_kind
  REAL(r_kind), PARAMETER :: T0 = 273.16_r_kind
  REAL(r_kind), PARAMETER :: R3 = 21.875_r_kind
  REAL(r_kind), PARAMETER :: R4 = 7.66_r_kind  
  REAL(r_kind), PARAMETER :: R3w = 17.269_r_kind
  REAL(r_kind), PARAMETER :: R4w = 35.86_r_kind
!_RT  REAL(r_kind), PARAMETER :: eps = 0.622_r_kind
  REAL(r_kind) :: esat, eh, H1
  REAL(r_kind), PARAMETER :: reff_seasalt(4) = Reshape( (/0.3_r_kind, 1.0_r_kind, 3.25_r_kind, 7.5_r_kind/), (/4/) )
  INTEGER(i_kind) :: j1,j2,k
  REAL(r_kind) :: R_eff

  ! compute relative humidity
  esat = t - T0
  IF( esat < -15.0_r_kind ) THEN
    esat = 6.1078_r_kind*exp( R3*esat/(t-R4) )
  ELSE
    esat = 6.1078_r_kind*exp( R3w*esat/(t-R4w) )
  END IF
  
  eh = 0.001_r_kind*p*q/(0.001_r_kind*q*(1.0-eps)+eps)
  eh = eh/esat
  
  IF( ITYPE == DUST_AEROSOL ) THEN    
    if(trim(aeroname)=='du0001') R_eff = 0.55_r_kind
    if(trim(aeroname)=='du0002') R_eff = 1.4_r_kind
    if(trim(aeroname)=='du0003') R_eff = 2.4_r_kind
    if(trim(aeroname)=='du0004') R_eff = 4.5_r_kind
    if(trim(aeroname)=='du0005') R_eff = 8.0_r_kind   
    RETURN
  ELSE IF( ITYPE== BLACK_CARBON_AEROSOL .and. aerotype(1:3)=='wet' ) THEN
    R_eff = AeroC%Reff(1,IType )
    RETURN
  ELSE IF( ITYPE== ORGANIC_CARBON_AEROSOL .and. aerotype(1:3)=='wet' ) THEN
    R_eff = AeroC%Reff(1,IType )
    RETURN
  END IF

  j2 = 0
  IF( eh < AeroC%RH(1) ) THEN
    j1 = 1
  ELSE IF( eh > AeroC%RH(AeroC%n_RH) ) THEN
    j1 = AeroC%n_RH
  ELSE
    DO k = 1, AeroC%n_RH-1
      IF( eh <= AeroC%RH(k+1) .and. eh > AeroC%RH(k) ) THEN
        j1 = k
        j2 = k+1
        H1 = (eh-AeroC%RH(k))/(AeroC%RH(k+1)-AeroC%RH(k))
        go to 311
      END IF
    END DO
  END IF
  311 CONTINUE

  IF( j2 == 0 ) THEN
    R_eff = AeroC%Reff(j1,IType )
  ELSE
    R_eff = (1.0_r_kind-H1)*AeroC%Reff(j1,IType ) + H1*AeroC%Reff(j2,IType )
  END IF
  
  RETURN
  END FUNCTION GOCART_Aerosol_size
 
 end subroutine setuprad
