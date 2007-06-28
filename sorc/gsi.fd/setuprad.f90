subroutine setuprad(lunin,mype,aivals,stats,nchanl,nreal,nobs,&
     obstype,isis,is,rad_diagsave,channelinfo,perturb)
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
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_rad_file
!
!   input argument list:
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
!     perturb - observation perturbation factor
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

  use kinds, only: r_kind,r_single,i_kind
  use crtm_module
! use crtm_module, only: crtm_channelinfo_type,crtm_set_channelinfo
  use crtm_spccoeff, only: sc
  use crtm_parameters, only: limit_exp,toa_pressure,max_n_layers
  use error_handler, only: success
  use radinfo, only: nuchan,tlapmean,ifactq,predx,cbias,ermax_rad,&
       npred,jpch,varch,iuse_rad,nusis,fbias,retrieval,b_rad,pg_rad
  use guess_grids, only: nfldsig,hrdifsig,add_rtm_layers
  use obsmod, only: iadate,ndat,mype_diaghdr,lunobs_obs,nchan_total, &
           dplat,dtbduv_on,rmiss_single,rad_ob_type,radhead,radtail, &
           dirname
  use gridmod, only: istart,jstart,jlon1,nlon,nsig,nlat,nsig2,nsig3,nsig3p1,&
       lat2,regional,nsig4,nsig5,nsig3p2,nsig3p3,msig,nlayers,get_ij
  use satthin, only: super_val1
  use constants, only: half,ozcon,constoz,amsua_clw_d1,amsua_clw_d2,tiny_r_kind,&
       fv,zero,one,izero,deg2rad,rad2deg,one_tenth,quarter,two,three,four,five,&
       zero_single,ttp,grav,cg_term,tpwcon,t0c,r1000,wgtlim
  use jfunc, only: jiter
  use sst_retrieval, only: setup_sst_retrieval,avhrr_sst_retrieval,&
       finish_sst_retrieval,spline_cub
  implicit none

! Declare passed variables
  logical,intent(in):: rad_diagsave
  character(10),intent(in):: obstype
  character(20),intent(in):: isis
  integer(i_kind),intent(in):: lunin,mype,nchanl,nreal,nobs,is
  real(r_kind),dimension(40,ndat),intent(inout):: aivals
  real(r_kind),dimension(7,jpch),intent(inout):: stats
  real(r_kind),dimension(nchanl,nobs),intent(in):: perturb
  type(crtm_channelinfo_type):: channelinfo

! Declare local parameters
  integer(i_kind),parameter:: ipchan=7
  integer(i_kind),parameter:: ireal=26

! CRTM structure variable declarations.
  integer(i_kind),parameter::  n_absorbers = 2
  integer(i_kind),parameter::  n_clouds = 0
  integer(i_kind),parameter::  n_aerosols = 0

! Mapping land surface type of GFS to CRTM
!  Note: index 0 is water, and index 13 is ice. The two indices are not
!        used and just assigned to COMPACTED_SOIL.
  integer, parameter, dimension(0:13) :: gfs_to_crtm=(/COMPACTED_SOIL, &
       BROADLEAF_FOREST, BROADLEAF_FOREST, BROADLEAF_PINE_FOREST, PINE_FOREST, &
       PINE_FOREST, BROADLEAF_BRUSH, SCRUB, SCRUB, SCRUB_SOIL, TUNDRA, &
       COMPACTED_SOIL, TILLED_SOIL, COMPACTED_SOIL/)

! Mapping land surface type of NMM to CRTM
!  Note: index 16 is water, and index 24 is ice. The two indices are not
!        used and just assigned to COMPACTED_SOIL.
  integer, parameter, dimension(24) :: nmm_to_crtm=(/URBAN_CONCRETE, &
       COMPACTED_SOIL, IRRIGATED_LOW_VEGETATION, GRASS_SOIL, MEADOW_GRASS, &
       MEADOW_GRASS, MEADOW_GRASS, SCRUB, GRASS_SCRUB, MEADOW_GRASS, &
       BROADLEAF_FOREST, PINE_FOREST, BROADLEAF_FOREST, PINE_FOREST, &
       BROADLEAF_PINE_FOREST, COMPACTED_SOIL, WET_SOIL, WET_SOIL, &
       IRRIGATED_LOW_VEGETATION, TUNDRA, TUNDRA, TUNDRA, TUNDRA, &
       COMPACTED_SOIL/)

  real(r_kind),parameter:: r0_01=0.01_r_kind
  real(r_kind),parameter:: r0_02=0.02_r_kind
  real(r_kind),parameter:: r0_03=0.03_r_kind
  real(r_kind),parameter:: r0_05=0.05_r_kind

  real(r_kind),parameter:: r0_1=0.1_r_kind
  real(r_kind),parameter:: r0_2=0.2_r_kind
  real(r_kind),parameter:: r0_3=0.3_r_kind
  real(r_kind),parameter:: r0_4=0.4_r_kind
  real(r_kind),parameter:: r0_5=0.5_r_kind
  real(r_kind),parameter:: r0_6=0.6_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r0_8=0.8_r_kind
  real(r_kind),parameter:: r0_9=0.9_r_kind
  real(r_kind),parameter:: r1_1=1.1_r_kind
  real(r_kind),parameter:: r1_2=1.2_r_kind
  real(r_kind),parameter:: r1_3=1.3_r_kind
  real(r_kind),parameter:: r1_4=1.4_r_kind


  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
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
  integer(i_kind) ifrac_sno,ilone,ilate,isst_ncep_ges,isst_ncep_anl
  integer(i_kind) isst_hires,isst_navy,idata_type,iclr_sky,iscan_pos
  integer(i_kind) isazi_ang,isgnt_ang
  integer(i_kind) l,ll
  integer(i_kind) m,mm,nblktot,jc,nobs1,nblk
  integer(i_kind) icc
  integer(i_kind) j,k,ncnt,i,ksat
  integer(i_kind) mm1
  integer(i_kind) ixx
  integer(i_kind) jsat,kk,n,nlev,kval,kk2
  integer(i_kind) itmp,iflg,ii,loc,lcloud,idate,iiflg
  integer(i_kind) nadir
  integer(i_kind) kraintype,ierrret

  real(r_single) freq4,pol4,wave4,varch4,tlap4
  real(r_kind) vfact,vfactmc
  real(r_kind) efact,efactmc
  real(r_kind) demisf,fact,dtempf,dtbf,tbcx1,tbcx2
  real(r_kind) term,sum,tlap,dsval,tb_obsbc1,clwx
  real(r_kind) de1,de2,de3,dtde1,dtde2,dtde3
  real(r_kind) fact1,fact2,fact3,fact4,fact5,dsi,si
  real(r_kind) drad,factch4,factch6
  real(r_kind) dradnob,varrad,delta,error
  real(r_kind) tmp,sum2,sum3,dts
  real(r_kind) sfchgtfact,ten
  real(r_kind) oneover25,cloudp,obserr,errinv,useflag
  real(r_kind) w1f4,w2f4,w1f6,w2f6,oneover400
  real(r_kind) cg_rad,wgross,wnotgross,wgt,arg,exp_arg
  real(r_kind) tsavg5,soilt5,vegf5,vegtype5,soilm5,soiltype5,snow5,trop5
  real(r_kind) secant_term   ! secant of viewing path angle at surface
  real(r_kind) pangs,panglr,cld,cldp
  real(r_kind) cenlon,cenlat,slats,slons,zsges,zasat,dtime
  real(r_kind) ys_bias_sst
  real(r_kind) sstnv,sstfg,sstcu,dtp_avh
  real(r_kind) cosza,val_obs
  real(r_kind) earth_lon,earth_lat,cenlatx,tpw5
  real(r_kind) uwind,vwind,f10
  real(r_kind) clw,tpwc,si85,sgagl,total_od

! Declare local arrays

  real(r_single),dimension(ireal):: diagbuf
  real(r_single),allocatable,dimension(:,:):: diagbufex
  real(r_single),dimension(ipchan+npred+1,nchanl)::diagbufchan

  real(r_kind),dimension(npred+1,nchanl)::predterms
  real(r_kind),dimension(npred-2):: pred
  real(r_kind),dimension(nchanl):: varinv,varinv_use,error0,errf
  real(r_kind),dimension(nchanl):: tb_obs,tbc,tbcnob,tlapchn,tb_obs_sdv
  real(r_kind),dimension(nchanl):: tmpvar,tnoise,errmax
  real(r_kind),dimension(nchanl):: var,ratio_raderr,radinv,pred_tlap
  real(r_kind),dimension(nchanl):: emissivity,ts,emissivity_k
  real(r_kind),dimension(nchanl):: uwind_k,vwind_k
  real(r_kind),dimension(nchanl,nsig):: dtb
  real(r_kind),dimension(nsig,nchanl):: wmix,temp,omix,ptau5
  real(r_kind),dimension(nsig3p3,nchanl):: htlto
  real(r_kind),dimension(nreal+nchanl,nobs)::data_s
  real(r_kind),dimension(nsig+1):: pin5
  real(r_kind),dimension(nsig):: c1,c2,c3,c4,c5,prsltmp
  real(r_kind),dimension(nsig):: prsr5,temp5,qvp,tvp,poz
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_kind),dimension(msig)::  prsltmp_rtm
  real(r_kind),dimension(msig+1)::  prsitmp_rtm
  real(r_kind),dimension(4):: sfcpct,ts5

  integer(i_kind),dimension(nchanl):: ich,icxx,id_qc
  integer(i_kind),dimension(msig):: klevel

  character(10) filex
  character(12) string

  logical hirs2,msu,goessndr,hirs3,hirs4,hirs,amsua,amsub,airs,hsb,goes_img,mhs
  logical avhrr,avhrr_navy,lextra,ssu
  logical ssmi,ssmis,amsre,amsre_low,amsre_mid,amsre_hig
  logical ssmis_las,ssmis_uas,ssmis_env,ssmis_img
  logical sea,mixed,land,ice,snow,toss
  logical micrim,microwave
  logical,dimension(nobs):: luse

  type(crtm_atmosphere_type) :: atmosphere
  type(crtm_surface_type) :: surface
  type(crtm_geometryinfo_type) :: geometryinfo

  type(crtm_atmosphere_type),allocatable,dimension(:):: atmosphere_k
  type(crtm_surface_type),   allocatable,dimension(:):: surface_k
  type(crtm_rtsolution_type),allocatable,dimension(:):: rtsolution
  type(crtm_rtsolution_type),allocatable,dimension(:):: rtsolution_k


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
  iiflg      = izero
  ncnt       = izero



  cld   = zero
  cldp  = zero
  tpwc  = zero
  clw   = zero
  si85  = zero
  sgagl = zero
  icc   = izero
  ich9  = min(9,nchanl)
  do j=1,npred-2
     pred(j)=zero
  end do

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

  ssmis=ssmis_las.or.ssmis_uas.or.ssmis_img.or.ssmis_env

  micrim=ssmi .or. ssmis .or. amsre   ! only used for MW-imager-QC and id_qc(ch)

  microwave=amsua .or. amsub  .or. mhs .or. msu .or. hsb .or. &
        micrim 

  isatid    = 1  ! index of satellite id
  itime     = 2  ! index of analysis relative obs time 
  ilon      = 3  ! index of grid relative obs location (x)
  ilat      = 4  ! index of grid relative obs location (y)
  ilzen_ang = 5  ! index of local (satellite) zenith angle (radians)
  ilazi_ang = 6  ! index of local (satellite) zenith angle (radians)
  iscan_ang = 7  ! index of scan (look) angle (radians)
  iscan_pos = 8  ! index of integer scan position 
  iszen_ang = 9  ! index of solar zenith angle (degrees)
  isazi_ang = 10 ! index of solar azimuth angle (degrees)
  ifrac_sea = 11 ! index of ocean percentage
  ifrac_lnd = 12 ! index of land percentage
  ifrac_ice = 13 ! index of ice percentage
  ifrac_sno = 14 ! index of snow percentage
  ilone     = 15 ! index of earth relative longitude (degrees)
  ilate     = 16 ! index of earth relative latitude (degrees)

! Initialize sensor specific array pointers
  if (goes_img) then
     iclr_sky = 7 ! index of clear sky amount
  elseif (avhrr_navy) then
     isst_navy     =  7 ! index of navy sst (K) retrieval
     idata_type    = 17 ! index of data type (151=day, 152=night)
     isst_hires    = 18 ! index of interpolated hires sst (K)
  elseif (avhrr) then
     isst_hires    = 18 ! index of interpolated hires sst (K)
  elseif (amsre) then
     isgnt_ang     = 17 ! index of sun glint angle (degrees)
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
  Error_Status = CRTM_Set_ChannelInfo(isis, ChannelInfo)
  if (error_status /= success) &
       write(6,*)'SETUPRAD:  ***ERROR*** crtm_set_channelinfo error_status=',&
       error_status,' for satsensor=',isis

! Allocate structures for radiative transfer
  allocate(&
       rtsolution  (channelinfo%n_channels),&
       rtsolution_k(channelinfo%n_channels),&
       atmosphere_k(channelinfo%n_channels),&
       surface_k   (channelinfo%n_channels))

  err1=0; err2=0; err3=0; err4=0
  if(msig > max_n_layers)then
    write(6,*) 'SETUPRAD:  msig > max_n_layers - increase crtm max_n_layers ',&
         msig,max_n_layers
    call stop2(36)
  end if
  err1 = crtm_allocate_atmosphere(msig,n_absorbers,n_clouds,n_aerosols,atmosphere)
  err2 = crtm_allocate_surface(channelinfo%n_channels,surface)
  err3 = crtm_allocate_rtsolution(msig,rtsolution)
  err4 = crtm_allocate_rtsolution(msig,rtsolution_k)
  if (err1/=success) write(6,*)' ***ERROR** allocating atmosphere.    err=',err1
  if (err2/=success) write(6,*)' ***ERROR** allocating surface.       err=',err2
  if (err3/=success) write(6,*)' ***ERROR** allocating rtsolution.    err=',err3
  if (err4/=success) write(6,*)' ***ERROR** allocating rtsolution_k.  err=',err4

  atmosphere%n_layers = msig
  atmosphere%level_temperature_input = 0
  atmosphere%absorber_id(1) = H2O_ID
  atmosphere%absorber_id(2) = O3_ID
  atmosphere%absorber_units(1) = MASS_MIXING_RATIO_UNITS
  atmosphere%absorber_units(2) = VOLUME_MIXING_RATIO_UNITS
  atmosphere%level_pressure(0) = TOA_PRESSURE

  if(nchanl /= channelinfo%n_channels) write(6,*)'***ERROR** nchanl,n_channels ', &
           nchanl,channelinfo%n_channels

! Load surface sensor data structure
  surface%sensordata%n_channels = channelinfo%n_channels
  surface%sensordata%sensor_id  = channelinfo%wmo_sensor_id(1)

  do i=1,nchanl

    error_status = crtm_assign_atmosphere(atmosphere,atmosphere_k(i))
    if (error_status /= success) &
         write(6,*)'  ***ERROR*** crtm_assign_atmosphere ',&
         error_status

    error_status = crtm_assign_surface(surface,surface_k(i))
    if (error_status /= success) &
         write(6,*)'  ***ERROR*** crtm_assign_surface ',&
         error_status


  end do

! Initialize channel related information
  tnoise = r1e10
  errmax = r1e10
  toss = .true.
  jc=izero
  do j=1,jpch
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
        if (tnoise(jc) < 1.e4) toss = .false.
     end if
  end do
  if(nchanl > jc) write(6,*)'SETUPRAD:  channel number reduced for ', &
       obstype,nchanl,' --> ',jc
  if(jc == izero) then
     if(mype == izero) write(6,*)'SETUPRAD: No channels found for ', &
          obstype,isis
     if(nobs > izero)read(lunin)
     go to 135
  end if
  if (toss) then
     if(mype == izero)write(6,*)'SETUPRAD: all obs var > 1e4.  do not use ',&
          'data from satellite is=',isis
     if(nobs >izero)read(lunin)                    
     goto 135
  endif

! Special setup for SST retrieval
  if (retrieval) call setup_sst_retrieval(obstype,dplat(is),mype)

! If diagnostic file requested, open unit to file and write header.
  if (rad_diagsave) then
     filex=obstype
     write(string,1976) jiter
1976 format('_',i2.2)
     diag_rad_file= trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // trim(string)
     open(4,file=diag_rad_file,form='unformatted')
     rewind 4
     if (lextra) allocate(diagbufex(iextra,jextra))

!    Initialize/write parameters for satellite diagnostic file on
!    first outer iteration.
     if (mype==mype_diaghdr(is)) then
        idate=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000
        write(4) isis,dplat(is),obstype,jiter,nchanl,npred,idate,ireal,ipchan,iextra,jextra
        write(6,*)'SETUPRAD:  write header record for ',&
             isis,ireal,iextra,' to file ',trim(diag_rad_file),' ',iadate
        do i=1,nchanl
           n=ich(i)
           if( n < 1 )cycle
           freq4=sc%frequency(n)
           pol4=sc%polarization(n)
           wave4=sc%wavenumber(n)
           varch4=varch(n)
           tlap4=tlapmean(n)
           write(4)freq4,pol4,wave4,varch4,tlap4,iuse_rad(n),&
                nuchan(n),ich(i)
        end do
     endif
  endif
     

! Load data array for current satellite
  read(lunin) data_s,luse


! PROCESSING OF SATELLITE DATA

! Loop over data in this block
  do n = 1,nobs

     id_qc = izero
     if(luse(n))aivals(1,is) = aivals(1,is) + one

!   Extract analysis relative observation time.
     dtime = data_s(itime,n)

!    Extract lon and lat.
     slons  = data_s(ilon,n)    ! grid relative longitude
     slats  = data_s(ilat,n)    ! grid relative latitude                     
     cenlon = data_s(ilone,n)   ! earth relative longitude (degrees)
     cenlat = data_s(ilate,n)   ! earth relative latitude (degrees)                       
!    Extract angular information         
     zasat  = data_s(ilzen_ang,n)
     cosza  = cos(zasat)
     secant_term  = one/cosza          ! view angle path factor
         
     if(goes_img)then
        panglr = zero
        cld = data_s(iclr_sky,n)
     else
        panglr = data_s(iscan_ang,n)
     end if

     if(amsre)then
       sgagl = data_s(isgnt_ang,n)
     end if

!    Extract nadir (scan step position)
     nadir = nint(data_s(iscan_pos,n))
     pangs  = data_s(iszen_ang,n)

!    Load geometry structure
     geometryinfo%sensor_zenith_angle = zasat*rad2deg  ! local zenith angle
     geometryinfo%source_zenith_angle = pangs          ! solar zenith angle
     geometryinfo%sensor_scan_angle   = panglr*rad2deg ! scan angle
     if(abs(geometryinfo%sensor_zenith_angle) > one ) then
        geometryinfo%distance_ratio = &
             abs( sin(geometryinfo%sensor_scan_angle*deg2rad)/ &
             sin(geometryinfo%sensor_zenith_angle*deg2rad) )
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
         
!    Set relative weight value
     val_obs=one
     ixx=nint(data_s(nreal,n))
     if (ixx > izero .and. super_val1(ixx) >= one) then
        val_obs=data_s(nreal-1,n)/super_val1(ixx)
     endif

!    Load channel data into work array.  At same time, perturb
!    observation.  Note:  if perturb_obs=.false., the array
!    perturb=0.0 and observation is unchanged.

     do i = 1,nchanl
        obserr = min(tnoise(i),errmax(i))
        tb_obs(i) = data_s(i+nreal,n) + perturb(i,n)*obserr
     end do

     if(goes_img)then
        do i = 1,nchanl
           tb_obs_sdv(i) = data_s(i+15,n)
        end do
     end if

!    Extract Navy and NCEP SST
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


!    Interpolate model fields to observation location
     call intrppx(dtime,&
           tvp,qvp,poz,prsltmp,prsitmp,zsges,ts5,tsavg5,vegtype5,vegf5, &
           soiltype5,soilt5,soilm5,snow5,trop5,&
           uwind,vwind,f10,slats,slons,mype)

!  check for microwave and thin snow - if thin then reset to land 
!               but type == mixed
!    if(microwave .and. snow .and. snow5 < r0_1)then         !thin snow
!       sfcpct(2)=sfcpct(2)+sfcpct(4)
!       sfcpct(4)=zero
!       snow = .false.
!       mixed = .true.
!    end if
         
!    Count data of different surface types
     if(luse(n))then
        if (mixed) then
           aivals(5,is) = aivals(5,is) + one
        else if (ice .or. snow) then
           aivals(4,is) = aivals(4,is) + one
        else if (land) then
           aivals(3,is) = aivals(3,is) + one
        end if
     end if

!    For SST retrieval, use interpolated NCEP SST analysis
     if (retrieval) ts5=sstfg

!    Load surface structure

!    Define land characteristics

!    **NOTE:  The model surface type --> CRTM surface type
!             mapping below is specific to the versions NCEP
!             GFS and NNM as of September 2005
     itype = int(vegtype5)
     if (regional) then
        itype = min(max(1,itype),24)
        surface%land_type = nmm_to_crtm(itype)
     else
        itype = min(max(0,itype),13)
        surface%land_type = gfs_to_crtm(itype)
     end if

     surface%wind_speed            = f10*sqrt(uwind*uwind+vwind*vwind)
     surface%water_coverage        = sfcpct(1)
     surface%land_coverage         = sfcpct(2)
     surface%ice_coverage          = sfcpct(3)
     surface%snow_coverage         = sfcpct(4)
     surface%water_temperature     = ts5(1)
     surface%land_temperature      = ts5(2)
     surface%ice_temperature       = ts5(3)
     surface%snow_temperature      = ts5(4)
     surface%soil_moisture_content = soilm5
     surface%vegetation_fraction   = vegf5
     surface%soil_temperature      = soilt5
     surface%snow_depth            = snow5

!    Load surface sensor data structure
     do i=1,channelinfo%n_channels
        surface%sensordata%tb(i) = tb_obs(i)
     end do

!       Load profiles into model layers

!    Prepare for accumulating information for statistics and passing
!    needed information on to minimization
     do k=1,nsig
        qvp(k) = max(qsmall,qvp(k))
        c1(k)=(constoz*ozcon)/(prsitmp(k)-prsitmp(k+1))
        c2(k)=one/(one+fv*qvp(k))
        c3(k)=one/(one-qvp(k))
        c4(k)=fv*tvp(k)*c2(k)
        c5(k)=r1000*c3(k)*c3(k)
        pin5(k)  = r10*prsitmp(k)
        prsr5(k) = r10*prsltmp(k)
        temp5(k) = tvp(k)
     end do

     pin5(nsig+1) = r10*prsitmp(nsig+1)


!    Interpolate guess pressure at interfaces as well as 
!    log pressure at mid-layers to obs locations/times

     call add_rtm_layers(prsitmp,prsltmp,prsitmp_rtm,prsltmp_rtm,klevel)

!       Load profiles into extended RTM model layers
     do k = 1,msig
        kk = msig - k + 1
        atmosphere%level_pressure(k) = r10*prsitmp_rtm(kk)
        atmosphere%pressure(k)       = r10*prsltmp_rtm(kk)

        kk2 = klevel(kk)
        atmosphere%temperature(k)    = tvp(kk2)
        atmosphere%absorber(k,1)     = r1000*qvp(kk2)*c3(kk2)
        atmosphere%absorber(k,2)     = max(ozsmall,poz(kk2)*constoz)
     end do

!    Set up to return Tb jacobians.  Zero atmosphere
!    and surface jacobian structures
     do i=1,nchanl
       rtsolution_k(i)%radiance = zero
       rtsolution_k(i)%brightness_temperature = one
     end do
     call crtm_zero_atmosphere(atmosphere_k(:))
     call crtm_zero_surface(surface_k(:))


!    Call CRTM K Matrix model
     error_status = crtm_k_matrix(atmosphere,surface,&
         rtsolution_k,geometryinfo,channelinfo,atmosphere_k,&
         surface_k,rtsolution)

     if (error_status /=0) &
         write(6,*)'RAD_TRAN_K:  ***ERROR*** during crtm_k_matrix call ',&
         error_status


!    Compute transmittance from layer optical depths
     do i=1,nchanl
        emissivity(i)   = rtsolution(i)%surface_emissivity
        emissivity_k(i) = rtsolution_k(i)%surface_emissivity
        ts(i)   = surface_k(i)%water_temperature + &
                  surface_k(i)%land_temperature + &
                  surface_k(i)%ice_temperature + &
                  surface_k(i)%snow_temperature
        if (surface%wind_speed>small_wind) then
          term = surface_k(i)%wind_speed * f10*f10 / surface%wind_speed
          uwind_k(i) = term * uwind
          vwind_k(i) = term * vwind
        else
          uwind_k(i)    = zero
          vwind_k(i)    = zero
        endif


!       Zero jacobian and transmittance arrays
        do k=1,nsig
           temp(k,i)   = zero
           wmix(k,i)   = zero
           omix(k,i)   = zero
           ptau5(k,i)  = zero
        end do

        total_od = zero
!       Accumulate values from extended into model layers
        do k=1,msig
           kk = klevel(msig-k+1)
           temp(kk,i) = temp(kk,i) + atmosphere_k(i)%temperature(k)
           wmix(kk,i) = wmix(kk,i) + atmosphere_k(i)%absorber(k,1)
           omix(kk,i) = omix(kk,i) + atmosphere_k(i)%absorber(k,2)
           total_od   = total_od + rtsolution(i)%layer_optical_depth(k)
!          if (total_od*secant < limit_exp)ptau5(kk,i) = exp(-total_od*secant_term)
           ptau5(kk,i) = exp(-min(limit_exp,total_od*secant_term))
        end do
     end do

      
!*****
!     COMPUTE AND APPLY BIAS CORRECTION TO SIMULATED VALUES
!*****

!    Construct predictors for 1B radiance bias correction.
     pred(1) = r0_01
     pred(2) = one_tenth*(secant_term-one)**2-.015_r_kind
!    pred(2) = zero
     if(ssmi .or. ssmis .or. amsre)pred(2)=zero
     pred(3) = zero

!    Compute predictor for microwave cloud liquid water bias correction.
     clw=zero
     ierrret=0
     tpwc=zero
     kraintype=izero
     if(microwave .and. sea)then

       if (amsua) then

         if(tsavg5>t0c)then
           tbcx1=rtsolution(1)%brightness_temperature+cbias(nadir,ich(1))
           tbcx2=rtsolution(2)%brightness_temperature+cbias(nadir,ich(2))
           if (tbcx1 <=r284 .and. tbcx2<=r284 .and. tb_obs(1) > zero &
                .and. tb_obs(2) > zero) &
              clw=amsua_clw_d1*(tbcx1-tb_obs(1))/(r285-tbcx1)+ &
                  amsua_clw_d2*(tbcx2-tb_obs(2))/(r285-tbcx2)
           clw = max(zero,clw)
           pred(3) = clw*cosza*cosza
         end if
         
       else if (ssmis_las .or. ssmis_env) then

!       Compute guess total precipitable water
           tpw5 = zero
           do k=1,nsig
              tpw5 = tpw5 + qvp(k) * &
                   tpwcon*r10*(prsitmp(k)-prsitmp(k+1))
           end do

           call ret_ssmis( tb_obs(1),nchanl, n,              &
                ssmis_las,ssmis_uas,ssmis_img,ssmis_env,     &
                tpw5,tsavg5,cosza,emissivity,                &
                clw, ierrret)

       else if (amsre) then

           call retrieval_amsre(                                 &   
                tb_obs(1),nchanl,amsre_low,amsre_mid,amsre_hig,  &
                uwind,vwind,f10,tsavg5,                          &
                tpwc,clw,si85,kraintype,ierrret ) 

       else if(ssmi .or. ssmis_uas) then

           call retrieval_mi(tb_obs(1),nchanl, ssmi,ssmis,     &
                tpwc,clw,si85,kraintype,ierrret ) 

       endif
       clw = max(zero,clw)

       if (amsre) then
          pred(3) = clw
       else
          pred(3) = clw*cosza*cosza
       end if
     end if

!    Apply bias correction
     do i=1,nchanl
        mm=ich(i)

        tlapchn(i)= (ptau5(2,i)-ptau5(1,i))*(tsavg5-temp5(2))
        do k=2,nsig-1
           tlapchn(i)=tlapchn(i)+&
                (ptau5(k+1,i)-ptau5(k,i))*(temp5(k-1)-temp5(k+1))
        end do
        tlapchn(i) = r0_01*tlapchn(i)
        tlap = tlapchn(i)-tlapmean(mm)

        predterms(1,i) = cbias(nadir,mm)             !global_satangbias
        predterms(2,i) = tlap*predx(mm,npred)        !tlap
        predterms(3,i) = tlap*tlap*predx(mm,npred-1) !tlap*tlap
        do j = 1,npred-2
           predterms(j+3,i) = predx(mm,j)*pred(j)
        end do

!       Apply SST dependent bias correction with cubic spline
!       ksat = 17 : NOAA-17; ksat = 18: NOAA-18
        if (avhrr_navy .or. avhrr) then
           if(dplat(is) == 'n17')ksat=17
           if(dplat(is) == 'n18')ksat=18
           call spline_cub(fbias(:,mm,ksat-16),tsavg5,ys_bias_sst)
           predterms(6,m) = ys_bias_sst
        endif

!       tbc    = obs - guess after bias correction
!       tbcnob = obs - guess before bias correction

        tbcnob(i)    = tb_obs(i) - rtsolution(i)%brightness_temperature  
        tbc(i)       = tbcnob(i)                     

        do j=1,npred+1
           tbc(i)=tbc(i) - predterms(j,i) !obs-ges with bias correction
        end do

        error0(i)     = tnoise(i)
        if(tnoise(i) < 1.e4 .or. (iuse_rad(ich(i))==-1  &
             .and. rad_diagsave))then
           varinv(i)     = val_obs/tnoise(i)**2
           errf(i)       = tnoise(i)
        else
           if(id_qc(i) == izero)id_qc(i)=1
           varinv(i)     = zero
           errf(i)       = zero
        endif
!    End of loop over channels         
     end do

!******
!    QC OBSERVATIONS BASED ON VARIOUS CRITERIA
!            Separate blocks for various instruments.
!******
     
!  ---------- IR -------------------
!    QC HIRS/2, GOES, HIRS/3 and AIRS sounder data
!
     if (hirs .or. goessndr .or. airs) then

!     
!       Reduce weight given to obs for shortwave ir if 
!       solar zenith angle tiny_r_kind
        if (pangs <= 89.0_r_kind .and. (sfcpct(1) > zero)) then
!          QC2 in statsrad
           if(luse(n))aivals(9,is) = aivals(9,is) + one
           do i=1,nchanl
              ll = ich(i)
              if(sc%wavenumber(ll) > r2000)then
                 if(sc%wavenumber(ll) > r2400)then
                    varinv(i)=zero
                    if(id_qc(i) == izero)id_qc(i)=2
                 else
                    tmp=one-(sc%wavenumber(ll)-r2000)*ptau5(1,i)&
                         *max(zero,cos(pangs*deg2rad))*oneover400
                    varinv(i)=tmp*varinv(i)
                    if(id_qc(i) == izero)id_qc(i)=3
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

!      If GOES and lza > 60. do not use
       if( goessndr .and. zasat*rad2deg > 60._r_kind) then
!        QC5 in statsrad
         if(luse(n))aivals(12,is) = aivals(12,is) + one
         do i=1,nchanl
           varinv(i) = zero
           if(id_qc(i) == izero)id_qc(i)=4
         end do
       end if

!      Reduce weight for obs over higher topography
        sfchgtfact=one
        if (zsges > r2000) then
!        QC1 in statsrad
         if(luse(n))aivals(8,is) = aivals(8,is) + one
         sfchgtfact    = (r2000/zsges)**4
!        if(id_qc(i) == izero)id_qc(i)=5
        endif

!       Generate q.c. bounds and modified variances.
        do i=1,nchanl
           m=ich(i)
           if (tb_obs(i) > r1000 .or. tb_obs(i) <= zero) varinv(i)=zero
           varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))

!          Modify error based on transmittance at top of model
           varinv(i)=varinv(i)*ptau5(nsig,i)
           errf(i)=errf(i)*ptau5(nsig,i)

!       NOTE:  The qc below uses the inverse squared obs error.
!              If a particular channel is not being assimilated
!              (iuse_rad==-1), we should not use this channel
!              in the qc.  The loop below loads array varinv_use
!              such that this condition is satisfied.  Array
!              varinv_use is then used in the qc calculations.

           varinv_use(i) = varinv(i)
           if (iuse_rad(m)<izero) varinv_use(i) = zero
        end do

!       QC based on presence/absence of cloud

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
!       cldp=ten*prsr5(1)
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
       
        do i=1,nchanl
           delta=max(r0_05*tnoise(i),r0_02)
           if ( airs) delta=.0002_r_kind
           do k=1,lcloud
              if(abs(cld*dtb(i,k)) > delta)then
!                QC4 in statsrad
                 if(luse(n))aivals(11,is)   = aivals(11,is) + one
                 varinv(i) = zero
                 varinv_use(i) = zero
                 if(id_qc(i) == izero)id_qc(i)=6
                 exit
              end if
           end do
        end do

!       If no clouds check surface temperature/emissivity

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
             dts=min(3.0_r_kind,dts)
           end if
           do i=1,nchanl
              delta=max(r0_05*tnoise(i),r0_02)
              if ( airs) delta=.0002_r_kind
              if(abs(dts*ts(i)) > delta)then
!                QC3 in statsrad
                 if(luse(n) .and. varinv(i) > zero) &
                      aivals(10,is)   = aivals(10,is) + one
                 varinv(i) = zero
                 if(id_qc(i) == izero)id_qc(i)=7
              end if
           end do
        end if
        cenlatx=abs(cenlat)*oneover25
        if (cenlatx < one) then
           if(luse(n))aivals(6,is) = aivals(6,is) + one
           efact   = half*(cenlatx+one)
        else
           efact = one
        endif

!       Generate q.c. bounds and modified variances.
        do i=1,nchanl
           if(varinv(i) > tiny_r_kind)then
              errf(i)=efact*errf(i)
              dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
              term = dtbf*dtbf
              if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
           end if
        end do

!   End of HIRS and GOES QC blocks


!  --------- MSU -------------------
!    QC MSU data
     else if (msu) then

        vfact = one

!       Reduce qc bounds in tropics
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

!       Apply window test to channel 2 using channel 1
        if (abs(tbc(1)) > 5.0_r_kind) then
           errf(2) = zero
           varinv(2) = zero
           if(id_qc(2) == izero)id_qc(2)=2
!          QC1 in statsrad
           if(luse(n))aivals(8,is)   = aivals(8,is) + one
        endif

!       Reduce q.c. bounds over higher topography
        if (zsges > r2000) then
!          QC2 in statsrad
           if(luse(n))aivals(9,is)   = aivals(9,is) + one
           fact = r2000/zsges
           errf(1) = fact*errf(1)
           errf(2) = fact*errf(2)
           errf(3) = fact*errf(3)
!          if(id_qc(1) == izero)id_qc(1)=3
!          if(id_qc(2) == izero)id_qc(2)=3
!          if(id_qc(3) == izero)id_qc(3)=3
           vfact = fact
        end if

!       Generate q.c. bounds and modified variances.
        errf(3) = two*errf(3)
        errf(4) = two*errf(4)
        do i=1,nchanl

!          Modify error based on transmittance at top of model
           varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
           errf(i)=efact*errf(i)*ptau5(nsig,i)

           if(varinv(i) > tiny_r_kind)then
              dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
              term = dtbf*dtbf
              if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
           end if
        end do

!
!    End of MSU QC block


!  ---------- AMSU-A -------------------
!     QC AMSU-A data
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
         
!        Reduce qc bounds in tropics
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
!         sval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*tb_obsbc1)*tb_obsbc1 +  &
!              0.454_r_kind*tb_obsbc2-tb_obsbc15
         dsval=zero
         if(sea)then
            dsval=((2.41_r_kind-0.0098_r_kind*tb_obsbc1)*tbc(1) + &
                 0.454_r_kind*tbc(2)-tbc(15))*w1f6
            dsval=max(zero,dsval)
         end if
         
         clwx=cosza*clw*w1f4
!        QC6 in statsrad
         if(clwx >= one)aivals(13,is) = aivals(13,is) + one
         factch4=clwx**2+(tbc(4)*w2f4)**2
!              factch6x=((sval-five)/r10)**2+(tbc(6)/0.8_r_kind)**2
!        QC7 in statsrad
         if(dsval >= one)aivals(14,is) = aivals(14,is) + one
         factch6=dsval**2+(tbc(6)*w2f6)**2
         
         tpwc=factch4

         if(factch6 >= one .or. mixed)then
            efactmc=zero
            vfactmc=zero
            errf(6)=zero
            varinv(6)=zero
            do i=1,6
              if(id_qc(i) == izero)id_qc(i)=2
            end do
            if(id_qc(15) == izero)id_qc(15)=2
!           QC3 in statsrad
            if(.not. mixed.and. luse(n))aivals(10,is) = aivals(10,is) + one

         else if(factch4 > half)then
            efactmc=zero
            vfactmc=zero
            do i=1,5
              if(id_qc(i) == izero)id_qc(i)=3
            end do
            if(id_qc(15) == izero)id_qc(15)=3
!           QC1 in statsrad
            if(luse(n)) aivals(8,is) = aivals(8,is) + one

        else if(sea)then
!           QC based on ratio of obs-ges increment versus the sensitivity of
!           the simulated brightness temperature to the surface emissivity
!           Y2K hurricane season runs by QingFu Liu found the hurricane
!           forecast tracks to be degraded without this QC.
!           (Is this still true?)

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
!              QC2 in statsrad
               if(luse(n))aivals(9,is) = aivals(9,is) + one
               efactmc=zero
               vfactmc=zero
               do i=1,5
                 if(id_qc(i) == izero)id_qc(i)=4
               end do
               if(id_qc(15) == izero)id_qc(15)=4
            end if
         end if

!        Reduce q.c. bounds over higher topography
         if (zsges > r2000) then
!           QC4 in statsrad
            aivals(11,is) = aivals(11,is) + one
            fact    = r2000/zsges
            efactmc = fact*efactmc
            errf(6) = fact*errf(6)
            vfactmc = fact*vfactmc
            varinv(6)  = fact*varinv(6)
            if (zsges > r4000) then
!              QC5 in statsrad
               if(luse(n))aivals(12,is) = aivals(12,is) + one
               fact   = r4000/zsges
               errf(7)= fact*errf(7)
               varinv(7)= fact*varinv(7)
            end if
         end if

!        Generate q.c. bounds and modified variances.
         do i=1,nchanl

!           Modify error based on transmittance at top of model
            varinv(i)=varinv(i)*ptau5(nsig,i)
            errf(i)=errf(i)*ptau5(nsig,i)

            if(varinv(i) > tiny_r_kind)then
              dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
              term=dtbf*dtbf
              if(i <= 5 .or. i == 15)then
               
!                Adjust observation error based on magnitude of liquid
!                water correction.  0.2 is empirical factor

                 term=term+0.2_r_kind*(predx(ich(i),3)*pred(3))**2

                 errf(i)   = efactmc*errf(i)
                 varinv(i) = vfactmc*varinv(i)
              end if
              errf(i)   = efact*errf(i)
              if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
            end if
            
            if ( (i <= 5 .or. i == 15) .and. &
                 (varinv(i)<1.e-9_r_kind) ) pred(3) = zero
            
         end do

!    End of AMSU-A QC block
!
!  ---------- AMSU-B -------------------
!    QC AMSU-B and MHS data

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
            if(tb_obs(2) < 300.0_r_kind)then
               dsi=0.13_r_kind*(tbc(1)-33.58_r_kind*tbc(2)/(300.0_r_kind-tb_obs(2)))
!              QC3 in statsrad
               if(luse(n) .and. dsi >= one)aivals(10,is) = aivals(10,is) + one
            end if
!           si=42.72_r_kind+0.85_r_kind*tbc(1)-tbc(2)
         else
            dsi=0.85_r_kind*tbc(1)-tbc(2)
!           si=42.72_r_kind+0.85_r_kind*tb_obs(1)-tb_obs(2)
!           QC4 in statsrad
            if(luse(n) .and. dsi >= one)aivals(11,is) = aivals(11,is) + one
         end if
         dsi=max(zero,dsi)
         fact1=((tbc(1)-7.5_r_kind*dsi)/ten)**2+(dsi)**2

!  Allow saving of qc factors
         clw = dsi
         tpwc = fact1


         if(fact1 > one)then
            vfact=zero
!           QC1 in statsrad
            if(luse(n))aivals(8,is) = aivals(8,is) + one
            do i=1,nchanl
              if(id_qc(i) == izero)id_qc(i)=2
            end do
         else
            efact = (one-fact1*fact1)*efact
            vfact = (one-fact1*fact1)*vfact
!           Reduce q.c. bounds over higher topography
            if (zsges > r2000) then
!              QC2 in statsrad
               if(luse(n))aivals(9,is) = aivals(9,is) + one
               fact = r2000/zsges
               efact = fact*efact
               vfact = fact*vfact
            end if
         end if

!        Generate q.c. bounds and modified variances.
         do i=1,nchanl

!           Modify error based on transmittance at top of model
            varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
            errf(i)=efact*errf(i)*ptau5(nsig,i)

            if(varinv(i)>tiny_r_kind)then
               dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
               term=dtbf*dtbf
               if(term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
            end if
            
         end do

!    End of AMSU-B QC block
!
!  ---------- GOES imager --------------
!    GOES imager Q C
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
            
!           Filter out data according to clear sky fraction
            if(dplat(is) == 'g10' .and. cld <r40 ) then
               fact2=zero
               fact3=zero
               fact4=zero
               fact5=zero
!           QC7 in statsrad
               if(luse(n))aivals(14,is)= aivals(14,is) + one
            else if(dplat(is) == 'g12' .and. cld <r70 ) then
               fact2=zero
               fact3=zero
               fact4=zero
               fact5=zero
!           QC7 in statsrad
               if(luse(n))aivals(14,is)= aivals(14,is) + one
            end if
            
!           Quality control according to brightness temperature 
!           standard deviation from data
            if(tb_obs_sdv(1) >one ) then
               fact2=zero
!           QC3 in statsrad
               if(luse(n))aivals(10,is)= aivals(10,is) + one
            end if
            
            if(tb_obs_sdv(2) >1.5_r_kind ) then
               fact3=zero
!           QC4 in statsrad
               if(luse(n))aivals(11,is)= aivals(11,is) + one
            end if
            
            if(tb_obs_sdv(3) >one ) then
               fact4=zero
!           QC5 in statsrad
               if(luse(n))aivals(12,is)= aivals(12,is) + one
            end if
            
            if(tb_obs_sdv(4) >one ) then
               fact5=zero
!           QC6 in statsrad
               if(luse(n))aivals(13,is)= aivals(13,is) + one
            end if
            
!           Reduce weight for obs over higher topography
            if (zsges > r2000) then
               fact    = r2000/zsges
               efact   = fact*efact
               vfact   = fact*vfact
!           QC2 in statsrad
               if(luse(n))aivals(9,is)= aivals(9,is) + one
            end if
         else
            vfact=zero
         end if
            
!        Generate q.c. bounds and modified variances.
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
               if (tb_obs_sdv(2) >r0_4 .and. tb_obs_sdv(2) <=r0_5) &
                    varinv(i)=varinv(i)/1.05_r_kind
               if (tb_obs_sdv(2) >r0_5 .and. tb_obs_sdv(2) <=r0_6) &
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

!    End of GOES imager QC block
!
!  ---------- AVRHRR --------------
!    NAVY AVRHRR Q C

     else if (avhrr_navy .or. avhrr) then

         if(tb_obs(2) > zero .and. tb_obs(2) < 400.0_r_kind)then
            fact=one
            efact=one
            if (zsges > r100 .or. .not. sea)then
               fact    = zero
!           QC1 in statsrad
               if(luse(n)) aivals(8,is)= aivals(8,is) + one
            end if
         else
            vfact = zero
         end if

!        Generate q.c. bounds and modified variances.
         do i=1,nchanl
            errf(i)   = efact*errf(i)
            varinv(i) = fact*varinv(i)
!           Reject day time AVHRR Ch-3A observation
            if ( i == 1 .and. dtp_avh /= 152.0_r_kind ) then          ! day time
               varinv(i) = zero
!           QC3 in statsrad
               if(luse(n))aivals(10,is)= aivals(10,is) + one
            endif
         end do

!    End of AVHRR QC block
!
!  ---------- SSM/I , SSMIS, AMSRE  -------------------
!    SSM/I, SSMIS, & AMSRE Q C

     else if( ssmi .or. amsre .or. ssmis )then   

         call qcssmi(nchanl, &
              zsges,luse(n),sea,land,ice,snow,mixed, &
              ts,emissivity_k,ierrret,kraintype,tpwc,clw,sgagl, &
              tbcnob,ssmi,amsre_low,amsre_mid,amsre_hig,ssmis, &
              varinv,errf,aivals(1,is),id_qc)

!  ---------- SSU  -------------------
!     SSU Q C

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
         
!        Reduce weight for obs over higher topography
         sfchgtfact=one
         if (zsges > r2000) then
            sfchgtfact    = (r2000/zsges)**4
            if(luse(n)) aivals(11,is)= aivals(11,is) + one
         endif
         
!       Generate q.c. bounds and modified variances.
         do i=1,nchanl
            m=ich(i)
            if (tb_obs(i) > r400 .or. tb_obs(i) <= r100) then
               varinv(i)=zero
               if(luse(n)) aivals(12,is)= aivals(12,is) + one
            endif
            varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))
                                                                                                
!           Modify error based on transmittance at top of model
            varinv(i)=varinv(i)*ptau5(nsig,i)
            errf(i)=errf(i)*ptau5(nsig,i)
         end do
            
!        Reduce qc bounds in tropics
         if (cenlatx < one) then
            if(luse(n))aivals(7,is) = aivals(7,is) + one
            efact = half*(cenlatx+one)
         else
            efact = one
         endif
                                                                                                
!        Generate q.c. bounds and modified variances.
         do i=1,nchanl
            if(varinv(i) > tiny_r_kind)then
               errf(i)=efact*errf(i)
               dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
               term = dtbf*dtbf
               if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
            end if
         end do
 
!     End of SSU qc block
      end if

!    Done with sensor qc blocks.  Now make final qc decisions.


!    Apply gross check to observations.  Toss obs failing test.
     do i = 1,nchanl
       if (varinv(i) > tiny_r_kind ) then
         m=ich(i)
         errf(i) = min(three*errf(i),ermax_rad(m))
         if (abs(tbc(i)) > errf(i)) then

!          If mean obs-ges difference around observations
!          location is too large and difference at the 
!          observation location is similarly large, then
!          toss the observation.

           if(id_qc(i) == izero)id_qc(i)=8
           varinv(i) = zero
           if(luse(n))stats(2,m) = stats(2,m) + one
           if(luse(n))aivals(7,is) = aivals(7,is) + one
         end if
       end if
     end do

     if(amsua .or. amsub .or. mhs .or. msu .or. hsb)then
        if(amsua)nlev=6
        if(amsub .or. mhs)nlev=5
        if(hsb)nlev=4
        if(msu)nlev=4
        kval=izero
        do i=2,nlev
!       do i=1,nlev
           if (varinv(i)<tiny_r_kind .and. iuse_rad(ich(i))==1) then
              kval=i-1
              if(amsub .or. hsb .or. mhs)kval=nlev
              if(amsua .and. i <= 3)kval = zero
           end if
        end do
        if(kval > izero)then
           do i=1,kval
              varinv(i)=zero
              if(id_qc(i) == izero)id_qc(i)=9
           end do
           if(amsua)then
              varinv(15)=zero
              if(id_qc(15) == izero)id_qc(15)=9
           end if
        end if
     end if

!    If requested, generate SST retrieval
     if(retrieval) then
        if(avhrr_navy .or. avhrr) then
           call avhrr_sst_retrieval(obstype,dplat(is),nchanl,tnoise,&
                varinv,tsavg5,sstnv,sstcu,temp,wmix,ts,tbc,cenlat,cenlon,zasat,&
                dtime,dtp_avh,tlapchn,predterms,emissivity,pangs,tbcnob,tb_obs,&
                rad_diagsave,nadir,mype,ireal,ipchan,luse(n))
           go to 100
        endif
     endif

     icc = 0
     do i = 1,nchanl

!       Only process observations to be assimilated

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
           
!          Only "good" obs are included in J calculation.
           if (iuse_rad(m) == 1)then
              if(luse(n))then
                 aivals(40,is) = aivals(40,is) + tbc(i)*varrad
                 aivals(39,is) = aivals(39,is) -two*(error0(i)**2)*varinv(i)*term
                 aivals(38,is) = aivals(38,is) +one
                 if(wgt < wgtlim) aivals(2,is)=aivals(2,is)+one
              end if


!******
!    CONSTRUCT SENSITIVITY VECTORS.  WRITE TO OUTPUT FILE.
!******

!             Load arrays used in minimization

              icc      = icc+1                ! total channel count
              radinv(icc) = tbc(i)            ! obs-ges innovation
              icxx(icc) = m                   ! channel index
              var(icc) = one/error0(i)**2     ! 1/(obs error)**2  (original uninflated error)
              ratio_raderr(icc)=error0(i)**2*varinv(i) ! (original error)/(inflated error)
              pred_tlap(icc)=tlapchn(i)-tlapmean(m)   ! lapse rate predictor

!             Load jacobian for temperature (dTb/dTv).  The factor c2
!             converts the temperature from sensible to virtual
              do k = 1,nsig
                 htlto(k,icc) = temp(k,i)*c2(k)
              end do

!             Load jacobian for moisture (dTb/dq)
              do k = 1,nsig
                 htlto(nsig+k,icc)=c5(k)*wmix(k,i)-c4(k)*temp(k,i)

!                Deflate moisture jacobian above the tropopause.
                 if (pin5(k) < trop5) then
                    ifactq(m)=15
                    term = (pin5(k)-trop5)/(trop5-pin5(nsig))
                    htlto(nsig+k,icc) = exp(ifactq(m)*term)*htlto(nsig+k,icc)
                 endif
              end do

!             Load jacobian for ozone (dTb/doz).  For hirs and goes channel 9
!             (ozone channel) we do not let the observations change the ozone.
              if ((hirs .or. goessndr) .and. (varinv(ich9) < tiny_r_kind))then
                 do k = 1,nsig
                    htlto(nsig2+k,icc) = zero
                 end do
              else
                 do k = 1,nsig
                    htlto(nsig2+k,icc) = omix(k,i)*c1(k)
                 end do
              endif

!             Load Jacobian for wind speed (dTb/du, dTb/dv)
              if( dtbduv_on .and. microwave) then
                 htlto(nsig3p1,icc) = uwind_k(i)
                 htlto(nsig3p2,icc) = vwind_k(i)
              else
                 htlto(nsig3p1,icc) = zero
                 htlto(nsig3p2,icc) = zero
              end if

!             Load jacobian for skin temperature (dTb/dTskin)
              htlto(nsig3p3,icc) = ts(i)

!          End of use data block
           end if

!       End of varinv>tiny_r_kind block
        endif

!    End loop over channels.
     end do

!    Load data into output arrays
     if(.not. retrieval)then
        if(icc > 0)then
          ncnt =ncnt+1
          if(.not. associated(radhead))then
             allocate(radhead,stat=istat)
             if(istat /= 0)write(6,*)' failure to write radhead '
             radtail => radhead
          else
             allocate(radtail%llpoint,stat=istat)
             radtail => radtail%llpoint
             if(istat /= 0)write(6,*)' failure to write radtail%llpoint '
          end if
          allocate(radtail%res(icc),radtail%err2(icc), &
                   radtail%raterr2(icc),radtail%pred1(npred-2), &
                   radtail%pred2(icc),radtail%dtb_dvar(nsig3p3,icc), &
                   radtail%icx(icc))
             
          radtail%nchan  = icc         ! profile observation count
          call get_ij(mm1,slats,slons,radtail%ij(1),radtail%wij(1))
          radtail%time=dtime
          radtail%luse=luse(n)
          do m=1,npred-2
            radtail%pred1(m)=pred(m)
          end do
          do m=1,icc
            radtail%res(m)=radinv(m)
            radtail%err2(m)=var(m)
            radtail%raterr2(m)=ratio_raderr(m)
            radtail%pred2(m)=pred_tlap(m)
            radtail%icx(m)=icxx(m)
            do k=1,nsig3p3
              radtail%dtb_dvar(k,m)=htlto(k,m)
            end do
          end do
          
            
        
!         Write data to output file
          nchan_total=nchan_total+icc
        end if

     endif

!    Write diagnostics to output file.
     if (rad_diagsave .and. luse(n)) then
        diagbuf(1)  = cenlat                         ! observation latitude (degrees)
        diagbuf(2)  = cenlon                         ! observation longitude (degrees)
        diagbuf(3)  = zsges                          ! model (guess) elevation at observation location

        diagbuf(4)  = dtime                          ! observation time (hours relative to analysis time)

        diagbuf(5)  = data_s(iscan_pos,n)            ! sensor scan position
        diagbuf(6)  = zasat*rad2deg                  ! satellite zenith angle (degrees)
        diagbuf(7)  = data_s(ilazi_ang,n)            ! satellite azimuth angle (degrees)
        diagbuf(8)  = pangs                          ! solar zenith angle (degrees)
        diagbuf(9)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
        diagbuf(10) = sgagl                          ! sun glint angle (degrees) (sgagl)

        diagbuf(11) = surface%water_coverage         ! fractional coverage by water
        diagbuf(12) = surface%land_coverage          ! fractional coverage by land
        diagbuf(13) = surface%ice_coverage           ! fractional coverage by ice
        diagbuf(14) = surface%snow_coverage          ! fractional coverage by snow
        diagbuf(15) = surface%water_temperature      ! surface temperature over water (K)
        diagbuf(16) = surface%land_temperature       ! surface temperature over land (K)
        diagbuf(17) = surface%ice_temperature        ! surface temperature over ice (K)
        diagbuf(18) = surface%snow_temperature       ! surface temperature over snow (K)
        diagbuf(19) = surface%soil_temperature       ! soil temperature (K)
        diagbuf(20) = surface%soil_moisture_content  ! soil moisture
        diagbuf(21) = surface%land_type              ! surface land type
        diagbuf(22) = surface%vegetation_fraction    ! vegetation fraction
        diagbuf(23) = surface%snow_depth             ! snow depth
        diagbuf(24) = surface%wind_speed             ! surface wind speed (m/s)

!       Note:  The following quantities are not computed for all sensors
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
           if (iuse_rad(ich(i))/=1) useflag=-one
           diagbufchan(5,i)= id_qc(i)*useflag! quality control mark or event indicator

           diagbufchan(6,i)=emissivity(i)   ! surface emissivity
           diagbufchan(7,i)=tlapchn(i)      ! stability index
           do j=1,npred+1
              diagbufchan(7+j,i)=predterms(j,i) ! Tb bias correction terms (K)
           end do
        end do

        if (.not.lextra) then
           write(4) diagbuf,diagbufchan
        else
           write(4) diagbuf,diagbufchan,diagbufex
        endif

     end if


100  continue

! End of n-loop over obs
  end do

! If retrieval, close open bufr sst file  
  if (retrieval) call finish_sst_retrieval


! Jump here when there is no data to process for current satellite
135 continue
! Deallocate arrays
  err1=0; err2=0; err3=0; err4=0
  err1 = crtm_destroy_atmosphere(atmosphere)
  err2 = crtm_destroy_surface(surface)
  err3 = crtm_destroy_rtsolution(rtsolution)
  err4 = crtm_destroy_rtsolution(rtsolution_k)
  if (err1/=success) write(6,*)'***ERROR** destroy atmosphere.    err=',err1
  if (err2/=success) write(6,*)'***ERROR** destroy surface.       err=',err2
  if (err3/=success) write(6,*)'***ERROR** destroy rtsolution.    err=',err3
  if (err4/=success) write(6,*)'***ERROR** destroy rtsolution_k.  err=',err4
  deallocate(rtsolution,rtsolution_k,atmosphere_k,surface_k)


  if (rad_diagsave) then
     close(4)
     if (lextra) deallocate(diagbufex)
  endif


! End of routine
  return
 end subroutine setuprad
