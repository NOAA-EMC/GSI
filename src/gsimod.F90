!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: gsimod  ---

!
! !INTERFACE:
!
  module gsimod

! !USES:

  use kinds, only: i_kind
  use obsmod, only: dmesh,dval,dthin,dtype,dfile,dplat,dsfcalc,ndat,&
     init_obsmod_dflts,create_obsmod_vars,write_diag,oberrflg,&
     time_window,perturb_obs,perturb_fact,sfcmodel,destroy_obsmod_vars,dsis,ndatmax,&
     dtbduv_on,time_window_max,offtime_data,init_directories,oberror_tune, &
     blacklst,init_obsmod_vars,lobsdiagsave,lobskeep,lobserver,hilbert_curve,&
     lread_obs_save,lread_obs_skip,create_passive_obsmod_vars,lwrite_predterms,lwrite_peakwt
  use obs_sensitivity, only: lobsensfc,lobsensincr,lobsensjb,lsensrecompute, &
                             lobsensadj,lobsensmin,iobsconv,llancdone,init_obsens
  use gsi_4dvar, only: setup_4dvar,init_4dvar,nhr_assimilation,min_offset, &
                       l4dvar,nhr_obsbin,nhr_subwin,nwrvecs,&
                       lsqrtb,lcongrad,lbfgsmin,ltlint,ladtest,lgrtest,&
                       idmodel,clean_4dvar,lwrtinc,lanczosave,jsiga
  use obs_ferrscale, only: lferrscale
  use mpimod, only: npe,mpi_comm_world,ierror,init_mpi_vars,destroy_mpi_vars,mype
  use radinfo, only: retrieval,diag_rad,init_rad,init_rad_vars,adp_anglebc,angord,&
                       use_edges,passive_bc
  use radinfo, only: crtm_coeffs_path
  use ozinfo, only: diag_ozone,init_oz
  use aeroinfo, only: diag_aero, init_aero
  use coinfo, only: diag_co,init_co
  use convinfo, only: init_convinfo,npred_conv_max, &
                      id_bias_ps,id_bias_t,id_bias_spd, &
                      conv_bias_ps,conv_bias_t,conv_bias_spd, &
                      stndev_conv_ps,stndev_conv_t,stndev_conv_spd,diag_conv
  use oneobmod, only: oblon,oblat,obpres,obhourset,obdattim,oneob_type,&
     oneobtest,magoberr,maginnov,init_oneobmod,pctswitch
  use balmod, only: fstat
  use turblmod, only: use_pbl,init_turbl
  use qcmod, only: dfact,dfact1,&
      erradar_inflate,use_poq7,&
      repe_dw,init_qcvars,vadfile,noiqc,c_varqc
  use pcpinfo, only: npredp,diag_pcp,dtphys,deltim,init_pcp
  use jfunc, only: iout_iter,iguess,miter,factqmin,factqmax,niter,niter_no_qc,biascor,&
     init_jfunc,qoption,switch_on_derivatives,tendsflag,l_foto,jiterstart,jiterend,&
     bcoption,diurnalbc,print_diag_pcg,tsensible,lgschmidt
  use state_vectors, only: init_anasv,final_anasv
  use control_vectors, only: init_anacv,final_anacv,nrf,nvars,nrf_3d,cvars3d,cvars2d,nrf_var
  use berror, only: norh,ndeg,vs,bw,init_berror,hzscl,hswgt,pert_berr,pert_berr_fct,&
     bkgv_flowdep,bkgv_rewgtfct,bkgv_write,fpsproj,newpc4pred
  use anberror, only: anisotropic,ancovmdl,init_anberror,npass,ifilt_ord,triad4, &
     binom,normal,ngauss,rgauss,anhswgt,an_vs,&
     grid_ratio,grid_ratio_p,an_flen_u,an_flen_t,an_flen_z, &
     rtma_subdomain_option,nsmooth,nsmooth_shapiro,&
     pf2aP1,pf2aP2,pf2aP3,afact0,covmap,lreadnorm
  use compact_diffs, only: noq,init_compact_diffs
  use jcmod, only: init_jcvars,ljcdfi,alphajc,ljcpdry,bamp_jcpdry
  use tendsmod, only: ctph0,stph0,tlm0
  use mod_vtrans, only: nvmodes_keep,init_vtrans
  use mod_strong, only: jcstrong,jcstrong_option,nstrong,&
       period_max,period_width,init_strongvars,baldiag_full,baldiag_inc
  use gridmod, only: nlat,nlon,nsig,hybrid,wrf_nmm_regional,nems_nmmb_regional,&
     nmmb_reference_grid,grid_ratio_nmmb,&
     filled_grid,half_grid,wrf_mass_regional,nsig1o,nnnn1o,update_regsfc,&
     diagnostic_reg,gencode,nlon_regional,nlat_regional,nvege_type,&
     twodvar_regional,regional,init_grid,init_reg_glob_ll,init_grid_vars,netcdf,&
     nlayers,use_gfs_ozone,check_gfs_ozone_date,regional_ozone,jcap,jcap_b,vlevs
  use guess_grids, only: ifact10,sfcmod_gfs,sfcmod_mm5,use_compress
  use gsi_io, only: init_io,lendian_in
  use regional_io, only: convert_regional_guess,update_pint,preserve_restart_date
  use constants, only: zero,one,init_constants,gps_constants,init_constants_derived,three
  use fgrid2agrid_mod, only: nord_f2a,init_fgrid2agrid
  use smooth_polcarf, only: norsp,init_smooth_polcas
  use read_l2bufr_mod, only: minnum,del_azimuth,del_elev,del_range,del_time,&
     range_max,elev_angle_max,initialize_superob_radar,l2superob_only
  use m_berror_stats,only : berror_stats ! filename if other than "berror_stats"
  use lag_fields,only : infile_lag,lag_nmax_bal,&
                        &lag_vorcore_stderr_a,lag_vorcore_stderr_b,lag_modini
  use lag_interp,only : lag_accur
  use lag_traj,only   : lag_stepduration
  use hybrid_ensemble_parameters,only : l_hyb_ens,uv_hyb_ens,aniso_a_en,generate_ens,&
                         n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,&
                         beta1_inv,s_ens_h,s_ens_v,init_hybrid_ensemble_parameters
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis,init_rapidrefresh_cldsurf, &
                            dfi_radar_latent_heat_time_period,metar_impact_radius,&
                            metar_impact_radius_lowCloud,l_gsd_terrain_match_surfTobs
  use gsi_chemtracer_mod, only: gsi_chemtracer_init,gsi_chemtracer_final
  use gsi_4dcouplermod, only: gsi_4dcoupler_parallel_init
  use tcv_mod, only: init_tcps_errvals,tcp_oberr,tcp_innmax,tcp_oedelt
  implicit none

  private

! !PUBLIC ROUTINES:

   public gsimain_initialize
   public gsimain_run
   public gsimain_finalize

!
! !DESCRIPTION: This module contains code originally in the GSI main program.
! The main
!               program has been split in initialize/run/finalize segments, and
!               subroutines
!  created for these steps: gsimain_initialize(), gsimain_run() and
!  gsimain_finalize().
!  In non-ESMF mode (see below) a main program is assembled by calling these 3
!  routines in
!  sequence.
                                                                                                                         
                    
!  This file can be compiled in 2 different modes: an ESMF and a non-ESMF mode.
!  When HAVE_ESMF
!  is defined (ESMF mode), a few I/O related statements are skipped during
!  initialize() and
!  a main program is not provided. These is no dependency on the ESMF in this
!  file and in the
!  routines called from here. The ESMF interface is implemented in
!  GSI_GridCompMod which in
!  turn calls the initialize/run/finalize routines defined here.
!
! !REVISION HISTORY:
!
!  01Jul2006  Cruz      Initial code.
!  19Oct2006  da Silva  Updated prologue.
!  10Apr2007  Todling   Created from gsimain
!  13Jan2007  Tremolet  Updated interface to setup_4dvar
!  03Oct2007  Todling   Add lobserver
!  03Oct2007  Tremolet  Add DFI and lanczos-save
!  04Jan2008  Tremolet  Add forecast sensitivity to observations options
!  10Sep2008  Guo       Add CRTM files directory path
!  02Dec2008  Todling   Remove reference to old TLM of analysis  
!  20Nov2008  Todling   Add lferrscale to scale OMF w/ Rinv (actual fcst not guess)
!  08Dec2008  Todling   Placed switch_on_derivatives,tendsflag in jcopts namelist
!  28Jan2009  Todling   Remove original GMAO interface
!  06Mar2009  Meunier   Add initialisation for lagrangian data
!  04-21-2009 Derber    Ensure that ithin is positive if neg. set to zero
!  07-08-2009 Sato      Update for anisotropic mode (global/ensemble based)
!  08-31-2009 Parrish   Add changes for version 3 regional tangent linear normal mode constraint
!  09-22-2009 Parrish   Add read of namelist/hybrid_ensemble/.  contains parameters used for hybrid
!                        ensemble option.
!  10-09-2009 Wu        replace nhr_offset with min_offset since it's 1.5 hr for regional
!  02-17-2010 Parrish   add nlon_ens, nlat_ens, jcap_ens to namelist/hybrid_ensemble/, in preparation for 
!                         dual resolution capability when running gsi in hybrid ensemble mode.
!  02-20-2010 Zhu       Add init_anacv,nrf,nvars,nrf_3d for control variables;
!  02-21-2010 Parrish   add jcap_ens_test to namelist/hybrid_ensemble/ so can simulate lower resolution
!                         ensemble compared to analysis for case when ensemble and analysis resolution are
!                         the same.  used for preliminary testing of dual resolution hybrid ensemble option.
!  02-25-2010 Zhu       Remove berror_nvars
!  03-06-2010 Parrish   add flag use_gfs_ozone to namelist SETUP--allows read of gfs ozone for regional runs
!  03-09-2010 Parrish   add flag check_gfs_ozone_date to namelist SETUP--if true, date check gfs ozone
!  03-15-2010 Parrish   add flag regional_ozone to namelist SETUP--if true, then turn on ozone in 
!                         regional analysis
!  03-17-2010 todling   add knob for analysis error estimate (jsiga)
!  03-17-2010 Zhu       Add nc3d and nvars in init_grid_vars interface
!  03-29-2010 hu        add namelist variables for controling rapid refesh options
!                                 including cloud analysis and surface enhancement
!                       add and read namelist for RR
!  03-31-2010 Treadon   replace init_spec, init_spec_vars, destroy_spec_vars with general_* routines
!  04-07-2010 Treadon   write rapidrefresh_cldsurf settings to stdout
!  04-10-2010 Parrish   add vlevs from gridmod, so can pass as argument to init_mpi_vars, which must
!                        be called after init_grid_vars, as it currently is.  This must be done to
!                        avoid "use gridmod" in mpimod.F90, which causes compiler conflict, since
!                        "use mpimod" appears in gridmod.f90.
!  04-22-2010 Tangborn  add carbon monoxide settings
!  04-25-2010 Zhu       Add option newpc4pred for new pre-conditioning of predictors
!  05-05-2010 Todling   replace parallel_init w/ corresponding from gsi_4dcoupler
!  05-06-2010 Zhu       Add option adp_anglebc for radiance variational angle bias correction;
!                       npred was removed from setup namelist
!  05-12-2010 Zhu       Add option passive_bc for radiance bias correction for monitored channels
!  05-30-2010 Todling   reposition init of control and state vectors; add init_anasv; update chem
!  06-04-2010 Todling   update interface to init_grid_vars
!  06-05-2010 Todling   remove as,tsfc_sdv,an_amp0 from bkgerr namelist (now in anavinfo table)
!  08-10-2010 Wu        add nvege_type to gridopts namelist 
!  08-24-2010 hcHuang   add diag_aero and init_aero for aerosol observations
!  08-26-2010 Cucurull  add use_compress to setup namelist, add a call to gps_constants
!  09-02-2010 Zhu       Add option use_edges for the usage of radiance data on scan edges
!                         
!EOP
!-------------------------------------------------------------------------

! Declare variables.
  logical:: limit,writediag
  integer(i_kind) i,ngroup


! Declare namelists with run-time gsi options.
!
! Namelists:  setup,gridopts,jcopts,bkgerr,anbkgerr,obsqc,obs_input,
!             singleob_test,superob_radar,emissjac
!
! SETUP (general control namelist) :
!
!     gencode  - source generation code
!     factqmin - weighting factor for negative moisture constraint
!     factqmax - weighting factor for supersaturated moisture constraint
!     deltim   - model timestep
!     dtphys   - physics timestep
!     biascor  - background error bias correction coefficient
!     bcoption - 0=ibc; 1=sbc
!     diurnalbc- 1= diurnal bias; 0= persistent bias
!     ndat     - number of observations datasets
!     niter()  - number of inner interations for each outer iteration
!     niter_no_qc() - number of inner interations without nonlinear qc for each outer iteration
!     miter    - number of outer iterations
!     qoption  - option of analysis variable; 1:q/qsatg 2:norm RH
!     fstat    - logical to seperate f from balance projection
!     nhr_assimilation - assimilation time interval (currently 6hrs for global, 3hrs for reg)
!     min_offset       - time in minutes of analysis in assimilation window (default 3 hours)
!     l4dvar           - turn 4D-Var on/off (default=off=3D-Var)
!     jsiga            - calculate approximate analysis errors from lanczos for jiter=jsiga
!     idmodel          - uses identity model when running 4D-Var (test purposes)
!     lwrtinc          - when .t., writes out increments instead of analysis
!     nhr_obsbin       - length of observation bins
!     nhr_subwin       - length of weak constraint 4d-Var sub-window intervals
!     iout_iter- output file number for iteration information
!     npredp   - number of predictors for precipitation bias correction
!     retrieval- logical to turn off or on the SST physical retrieval
!     diag_rad - logical to turn off or on the diagnostic radiance file true=on
!     diag_conv-logical to turn off or on the diagnostic conventional file (true=on)
!     diag_ozone - logical to turn off or on the diagnostic ozone file (true=on)
!     diag_aero  - logical to turn off or on the diagnostic aerosol file (true=on)
!     diag_co - logical to turn off or on the diagnostic carbon monoxide file (true=on)
!     write_diag - logical to write out diagnostic files on outer iteration
!     lobsdiagsave - write out additional observation diagnostics
!     lobskeep     - keep obs from first outer loop for subsequent OL
!     lobsensfc    - compute forecast sensitivity to observations
!     lobsensjb    - compute Jb sensitivity to observations
!     lobsensincr  - compute increment sensitivity to observations
!     lobsensadj   - use adjoint of approx. Hessian to compute obs sensitivity
!     llancdone    - use to tell adjoint that Lanczos vecs have been pre-computed
!     lsensrecompute - does adjoint by recomputing forward solution
!     lobsensmin   - use minimisation to compute obs sensitivity
!     iobsconv     - compute convergence test in observation space
!                     =1 at final point, =2 at every iteration
!     lobserver    - when .t., calculate departure vectors only
!     lanczosave   - save lanczos vectors for forecast sensitivity computation
!     lferrscale   - apply H'R^{-1}H to a forecast error vector read on the fly
!     iguess   - flag for guess solution (currently not working)
!                iguess = -1  do not use guess file
!                iguess =  0  write only guess file
!                iguess =  1  read and write guess file
!                iguess =  2  read only guess file
!     oneobtest- one ob test flag true=on
!     switch_on_derivatives - if true, then compute horizontal derivatives of all state variables
!                           (to be used eventually for time derivatives, dynamic constraints,
!                            and observation forward models that need horizontal derivatives)
!     tendsflag - if true, compute time tendencies
!     l_foto   - option for First-Order Time extrapolation to Observation
!     sfcmodel - if true, then use boundary layer forward model for surface temperature data.
!     dtbduv_on - if true, use d(microwave brightness temperature)/d(uv wind) in inner loop
!     ifact10 - flag for recomputing 10m wind factor
!               ifact10 = 1 compute using GFS surface physics
!               ifact10 = 2 compute using MM5 surface physics
!               ifact10 = 0 or any other value - DO NOT recompute - use value from guess file
!     offtime_data - if true, then allow use of obs files with ref time different
!                        from analysis time.  default value = .false., in which case
!                        analysis fails if obs file ref time is different from analysis time.
!
!     perturb_obs - logical flag to perutrb observation (true=on)
!     oberror_tune - logical flag to tune oberror table  (true=on)
!     perturb_fact -  magnitude factor for observation perturbation
!     crtm_coeffs_path - path of directory w/ CRTM coeffs files
!     print_diag_pcg - logical turn on of printing of GMAO diagnostics in pcgsoi.f90
!     preserve_restart_date - if true, then do not update regional restart file date.
!     tsensible - option to use sensible temperature as the analysis variable. works
!                 only for twodvar_regional=.true.
!     lgschmidt - option for re-biorthogonalization of the {gradx} and {grady} sets
!                 from pcgsoi when twodvar_regional=.true.
!     hilbert_curve - option for hilbert-curve based cross-validation. works only
!                     with twodvar_regional=.true.
!     lread_obs_save - option to write out collective obs selection info
!     lread_obs_skip - option to read in collective obs selection info
!     use_gfs_ozone  - option to read in gfs ozone and interpolate to regional model domain
!     check_gfs_ozone_date  - option to date check gfs ozone before interpolating to regional model domain
!     regional_ozone  - option to turn on ozone in regional analysis
!     lwrite_predterms - option to write out actual predictor terms instead of predicted bias to the
!                        radiance diagnostic files
!     lwrite_peakwt    - option to writ out the approximate pressure of the peak of the weighting function
!                        for satellite data to the radiance diagnostic files
!     adp_anglebc - option to perform variational angle bias correction
!     angord      - order of polynomial for variational angle bias correction
!     newpc4pred  - option for additional preconditioning for pred coeff.
!     passive_bc  - option to turn on bias correction for passive (monitored) channels
!     use_edges   - option to exclude radiance data on scan edges
!     use_compress - option to turn on the use of compressibility factors in geopotential heights

!     NOTE:  for now, if in regional mode, then iguess=-1 is forced internally.
!            add use of guess file later for regional mode.

  namelist/setup/gencode,factqmin,factqmax,deltim,dtphys,&
       biascor,bcoption,diurnalbc,&
       ndat,niter,niter_no_qc,miter,qoption,nhr_assimilation,&
       min_offset, &
       iout_iter,npredp,retrieval,&
       diag_rad,diag_pcp,diag_conv,diag_ozone,diag_aero,diag_co,iguess,write_diag,&
       oneobtest,sfcmodel,dtbduv_on,ifact10,l_foto,offtime_data,&
       npred_conv_max,&
       id_bias_ps,id_bias_t,id_bias_spd, &
       conv_bias_ps,conv_bias_t,conv_bias_spd, &
       stndev_conv_ps,stndev_conv_t,stndev_conv_spd,use_pbl,use_compress,&
       perturb_obs,perturb_fact,oberror_tune,preserve_restart_date, &
       crtm_coeffs_path, &
       berror_stats,newpc4pred,adp_anglebc,angord,passive_bc,use_edges, &
       lobsdiagsave, &
       l4dvar,lsqrtb,lcongrad,lbfgsmin,ltlint,nhr_obsbin,nhr_subwin,&
       nwrvecs,ladtest,lgrtest,lobskeep,lsensrecompute,jsiga, &
       lobsensfc,lobsensjb,lobsensincr,lobsensadj,lobsensmin,iobsconv, &
       idmodel,lwrtinc,jiterstart,jiterend,lobserver,lanczosave,llancdone, &
       lferrscale,print_diag_pcg,tsensible,lgschmidt,lread_obs_save,lread_obs_skip, &
       use_gfs_ozone,check_gfs_ozone_date,regional_ozone,lwrite_predterms,&
       lwrite_peakwt

! GRIDOPTS (grid setup variables,including regional specific variables):
!     jcap     - spectral resolution
!     nsig     - number of sigma levels
!     nlat     - number of latitudes
!     nlon     - number of longitudes
!     hybrid   - logical hybrid data file flag true=hybrid
!     nlon_regional - 
!     nlat_regional
!     diagnostic_reg - logical for regional debugging
!     update_regsfc - logical to write out updated surface fields to the
!                     regional analysis file (default = false)
!     netcdf            - if true, then wrf files are in netcdf format,
!                       -   otherwise wrf files are in binary format.
!     regional          - logical for regional GSI run
!     wrf_nmm_regional  - logical for input from WRF NMM
!     wrf_mass_regional - logical for input from WRF MASS-CORE
!     nems_nmmb_regional- logical for input from NEMS NMMB
!     nmmb_reference_grid= 'H', then analysis grid covers H grid domain
!                                = 'V', then analysis grid covers V grid domain
!     grid_ratio_nmmb   - ratio of analysis grid to nmmb model grid in nmmb model grid units.
!     twodvar_regional  - logical for regional 2d-var analysis
!     filled_grid       - logical to fill in puts on WRF-NMM E-grid
!     half_grid         - logical to use every other row of WRF-NMM E-Grid
!     nvege_type - number of types of vegetation; old=24, IGBP=20
!     nlayers    - number of sub-layers to break indicated model layer into
!                  prior to calling radiative transfer model


  namelist/gridopts/jcap,jcap_b,nsig,nlat,nlon,hybrid,nlat_regional,nlon_regional,&
       diagnostic_reg,update_regsfc,netcdf,regional,wrf_nmm_regional,nems_nmmb_regional,&
       wrf_mass_regional,twodvar_regional,filled_grid,half_grid,nvege_type,nlayers,&
       nmmb_reference_grid,grid_ratio_nmmb

! BKGERR (background error related variables):
!     vs       - scale factor for vertical correlation lengths for background error
!     hzscl(n) - scale factor for horizontal smoothing, n=1,number of scales (3 for now)
!                specifies factor by which to reduce horizontal scales (i.e. 2 would
!                then apply 1/2 of the horizontal scale
!     hswgt(n) - empirical weights to apply to each horizontal scale
!     norh     - order of interpolation in smoothing
!     ndeg     - degree of smoothing in recursive filters
!     noq      - 1/4 of accuracy in compact finite differencing
!     bw       - factor in background error calculation
!     norsp    - order of interpolation for smooth polar cascade routine
!                 default is norsp=0, in which case norh is used with original
!                 polar cascade interpolation.
!     pert_berror - logical to turn on random inflation/deflation of background error
!                   tuning parameters
!     pert_berr_fct - factor for increasing/decreasing berror parameters, this is multiplied
!                     by random number
!     bkgv_flowdep  - flag to turn on flow dependence to background error variances
!     bkgv_rewgtfct - factor used to perform flow dependent reweighting of error variances
!     bkgv_write - flag to turn on=.true. /off=.false. generation of binary file with reweighted variances
!     fpsproj  - controls full nsig projection to surface pressure

  namelist/bkgerr/vs,hzscl,hswgt,norh,ndeg,noq,bw,norsp,fstat,pert_berr,pert_berr_fct, &
	bkgv_flowdep,bkgv_rewgtfct,bkgv_write,fpsproj

! ANBKGERR (anisotropic background error related variables):
!     anisotropic - if true, then use anisotropic background error
!     ancovmdl    - covariance model settings - 0: pt-based, 1: ensemble based
!     triad4      - for 2d variables, if true, use blended triad algorithm
!     ifilt_ord   - filter order for anisotropic filters
!     npass       - 2*npass = number of factors in background error
!     normal      - number of random vectors to use for filter normalization
!                     ( if < 0 then slightly slower, but results independent of
!                       number of processors)
!     binom       - if true, weight correlation lengths of factors using binomial
!                      distribution, with shortest scales on outside, longest scales
!                      on inside.  This can help to produce smoother correlations in the
!                      presence of strong anisotrophy
!     grid_ratio  - ratio of coarse to fine grid in fine grid units
!     grid_ratio_p- ratio of coarse to fine grid in fine grid units for polar patches
!     nord_f2a    - order of interpolation for transfer operators between filter grid and analysis grid
!     ngauss      - number of gaussians to add together in each factor
!     rgauss      - multipliers on reference aspect tensor for each gaussian factor
!     anhswgt     - empirical weights to apply to each gaussian
!     an_vs       - scale factor for background error vertical scales (temporary carry over from
!                    isotropic inhomogeneous option)
!     an_flen_u   -  coupling parameter for connecting horizontal wind to background error
!     an_flen_t   -  coupling parameter for connecting grad(pot temp) to background error
!     an_flen_z   -  coupling parameter for connecting grad(terrain) to background error
!     afact0      - anistropy effect parameter, the range must be in 0.0-1.0.
!     covmap      - if true, covariance map would be drawn
!     rtma_subdomain_option - if true, then call alternative code which calls recursive filter
!                              directly from subdomain mode, bypassing transition to/from
!                              horizontal slabs.  This is mainly to improve efficiency for
!                              2d rtma analysis.  at the moment, this only works for
!                              twodvar_regional=.true.  rtma_subdomain_option will be forced
!                              to false when twodvar_regional=.false.
!     lreadnorm   -  if true, then read normalization from fixed files
!     nsmooth     -  number of 1-2-1 smoothing passes before and after background error application
!     nsmooth_shapiro - number of 2nd moment preserving (shapiro) smoothing passes before and after
!                       background error application.
!                        NOTE:  default for nsmooth and nsmooth_shapiro is 0.
!                               if both are > 0, then nsmooth will be forced to zero.

  namelist/anbkgerr/anisotropic,ancovmdl,triad4,ifilt_ord,npass,normal,binom,&
       ngauss,rgauss,anhswgt,an_vs, &
       grid_ratio,grid_ratio_p,nord_f2a,an_flen_u,an_flen_t,an_flen_z, &
       rtma_subdomain_option,lreadnorm,nsmooth,nsmooth_shapiro, &
       afact0,covmap

! JCOPTS (Jc term)
!                 if .false., uses original formulation based on wind, temp, and ps tends
!     ljcdfi      - when .t. uses digital filter initialization of increments (4dvar)
!     alphajc     - parameter for digital filter
!     ljpdry      - when .t. uses dry pressure constraint on increment
!     bamp_jcpdry - parameter for pdry_jc
!

  namelist/jcopts/ljcdfi,alphajc,switch_on_derivatives,tendsflag,ljcpdry,bamp_jcpdry

! STRONGOPTS (strong dynamic constraint)
!     jcstrong - if .true., strong contraint on
!     jcstrong_option - =1 for slow global strong constraint
!                       =2 for fast global strong constraint
!                       =3 for regional strong constraint
!                       =4 version 3 of regional strong constraint
!     nstrong  - if > 0, then number of iterations of implicit normal mode initialization
!                   to apply for each inner loop iteration
!     period_max     - cutoff period for gravity waves included in implicit normal mode
!                    initialization (units = hours)
!     period_width   - defines width of transition zone from included to excluded gravity waves
!     period_max - cutoff period for gravity waves included in implicit normal mode
!                   initialization (units = hours)
!     period_width - defines width of transition zone from included to excluded gravity waves
!     nvmodes_keep - number of vertical modes to use in implicit normal mode initialization
!     baldiag_full 
!     baldiag_inc

  namelist/strongopts/jcstrong,jcstrong_option,nstrong, &
                      period_max,period_width,nvmodes_keep, &
		      baldiag_full,baldiag_inc

! OBSQC (observation quality control variables):
!
!     Parameters used for gross error checks
!        obserrx = max(ermin,min(ermax,obs error)
!        if(abs(simulated)-observation)/obserrx > gross observation rejected
!
!
!     Parameters below use for nonlinear (variational) quality control
!     repe_dw  - factor for representativeness error in radar doppler winds
!     dfact    - factor for duplicate obs at same location for conv. data
!     dfact1   - time factor for duplicate obs at same location for conv. data
!     erradar_inflate - radar error inflation factor
!     oberrflg - logical for reading in new obs error table (if set to true)
!     vadfile  - character(10) variable holding name of vadwnd bufr file
!     noiqc    - logical flag to bypass OIQC (if set to true)
!     c_varqc - constant number to control var. qc turnning on speed
!     blacklst - logical for reading in raob blacklist (if set to true)
!     use_poq7 - logical flag to accept (.true.) sbuv profile quality flag 7
!     tcp_oberr  - observation error (without inflation) for tcps obs in mb
!     tcp_innmax - parameter for tcps oberr inflation (max innovation for inflation) in mb
!     tcp_oedelt - parameter for tcps oberr inflation (delta oberr over tcp_oberr) in mb

  namelist/obsqc/ repe_dw,dfact,dfact1,erradar_inflate,oberrflg,vadfile,noiqc,&
       c_varqc,blacklst,use_poq7,hilbert_curve,tcp_oberr,tcp_innmax,tcp_oedelt

! OBS_INPUT (controls input data):
!      dfile(ndat)      - input observation file name
!      dtype(ndat)      - observation type
!      dplat(ndat)      - satellite (platform) id (for satellite data)
!      dsis(ndat)       - sensor/instrument/satellite flag from satinfo files
!      dthin(ndat)      - satellite group
!      dval(ndat)       - relative value of each profile within group
!                         relative weight for observation = dval/sum(dval)
!                         within grid box
!      dsfcalc(ndat)    - specifies method to determine surface fields
!                         within a FOV. when equal to one, integrate
!                         model fields over FOV. when not one, bilinearly
!                         interpolate model fields to FOV center.
!      time_window(ndat)- time window for each input data file
!      dmesh(max(dthin))- thinning mesh for each group
!      time_window_max  - upper limit on time window for all input data

  namelist/obs_input/dfile,dtype,dplat,dsis,dthin,dval,dmesh,dsfcalc,time_window,time_window_max

! SINGLEOB_TEST (one observation test case setup):
!      maginnov   - magnitude of innovation for one ob
!      magoberr   - magnitude of observational error
!      oneob_type - observation type
!      oblat      - observation latitude
!      oblon      - observation longitude
!      obpres     - observation pressure
!      obdattim   - observation date
!      obhourset  - observation delta time from analysis time
!      pctswitch  - if .true. innovation & oberr are relative (%) of background value
!                      (level ozone only)

  namelist/singleob_test/maginnov,magoberr,oneob_type,&
       oblat,oblon,obpres,obdattim,obhourset,pctswitch

! SUPEROB_RADAR (level 2 bufr file to radar wind superobs):
!      del_azimuth     - azimuth range for superob box  (default 5 degrees)
!      del_elev        - elevation angle range for superob box  (default .05 degrees)
!      del_range       - radial range for superob box  (default 5 km)
!      del_time        - 1/2 time range for superob box  (default .5 hours)
!      elev_angle_max  - max elevation angle (default of 5 deg recommended by S. Liu)
!      minnum                  - minimum number of samples needed to make a superob
!      range_max       - max radial range to use in constructing superobs  (default 100km)
!      l2superob_only  - if true, then process level 2 data creating superobs, then quit.
!                          (added for easier retrospective testing, since level 2 bufr
!                             files are very large and hard to work with)

  namelist/superob_radar/del_azimuth,del_elev,del_range,del_time,&
       elev_angle_max,minnum,range_max,l2superob_only

! LAG_DATA (lagrangian data assimilation related variables):
!     lag_accur - Accuracy used to decide whether or not a balloon is on the grid
!     infile_lag- File containing the initial position of the balloon
!     lag_stepduration- Duration of one time step for the propagation model
!     lag_nmax_bal- Maximum number of balloons at starting time
!     lag_vorcore_stderr_a - Observation error for vorcore balloon
!     lag_vorcore_stderr_b -   error = b + a*timestep(in hours)
  namelist/lag_data/lag_accur,infile_lag,lag_stepduration,lag_nmax_bal,&
      lag_vorcore_stderr_a,lag_vorcore_stderr_b

! HYBRID_ENSEMBLE (parameters for use with hybrid ensemble option)
!     l_hyb_ens     - if true, then turn on hybrid ensemble option
!     uv_hyb_ens    - if true, then ensemble perturbation wind variables are u,v,
!                       otherwise, ensemble perturbation wind variables are stream, pot. functions.
!     aniso_a_en - if true, then use anisotropic localization of hybrid ensemble control variable a_en.
!     generate_ens - if true, then generate internal ensemble based on existing background error
!     n_ens        - number of ensemble members.
!     nlon_ens     - number of longitudes on ensemble grid (may be different from analysis grid nlon)
!     nlat_ens     - number of latitudes on ensemble grid (may be different from analysis grid nlat)
!     jcap_ens     - for global spectral model, spectral truncation
!     jcap_ens_test- for global spectral model, test spectral truncation (to test dual resolution)
!     beta1_inv           - 1/beta1, the weight given to static background error covariance
!                              0 <= beta1_inv <= 1,  tuned for optimal performance
!                             =1, then ensemble information turned off
!                             =0, then static background turned off
!                            beta2_inv = 1 - beta1_inv is weight given to ensemble derived covariance
!     s_ens_h             - homogeneous isotropic horizontal ensemble localization scale (km)
!     s_ens_v             - vertical localization scale (grid units for now)
!                              s_ens_h, s_ens_v, and beta1_inv are tunable parameters.
  namelist/hybrid_ensemble/l_hyb_ens,uv_hyb_ens,aniso_a_en,generate_ens,n_ens,nlon_ens,nlat_ens,jcap_ens,&
                jcap_ens_test,beta1_inv,s_ens_h,s_ens_v

! rapidrefresh_cldsurf (options for cloud analysis and surface 
!                             enhancement for RR appilcation  ):
!      l_cloud_analysis     -   if .true., turn cloud analysis on
!      dfi_radar_latent_heat_time_period     -   DFI forward integration window in minutes
!      metar_impact_radius  - metar low cloud observation impact radius in grid number
!      l_gsd_terrain_match_surfTobs - if .true., GSD terrain match for surface temperature observation
  namelist/rapidrefresh_cldsurf/l_cloud_analysis,dfi_radar_latent_heat_time_period, &
                                metar_impact_radius,metar_impact_radius_lowCloud, &
                                l_gsd_terrain_match_surfTobs

!EOC

!---------------------------------------------------------------------------

   CONTAINS

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IROUTINE: gsimain_initialize

! ! INTERFACE:

  subroutine gsimain_initialize

!*************************************************************
! Begin gsi code
!
  use mpeu_util,only: die
  implicit none
  character(len=*),parameter :: myname_='gsimod.gsimain_initialize'
  integer:: ios

  call gsi_4dcoupler_parallel_init

  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  if (mype==0) call w3tagb('GSI_ANL',1999,0232,0055,'NP23')


! Initialize defaults of vars in modules
  call init_4dvar

! Read in user specification of state and control variables
  call init_anasv
  call init_anacv
  call gsi_chemtracer_init

  call init_constants_derived
  call init_oneobmod
  call init_qcvars
  call init_obsmod_dflts
  call init_directories(mype)
  call init_pcp
  call init_rad
  call init_oz
  call init_aero
  call init_co
  call init_convinfo
  call init_jfunc
  call init_berror
  call init_anberror
  call init_fgrid2agrid(pf2aP1)
  call init_fgrid2agrid(pf2aP2)
  call init_fgrid2agrid(pf2aP3)
  call init_grid
  call init_turbl
  call init_compact_diffs
  call init_smooth_polcas  
  call init_jcvars
  call init_strongvars
  call initialize_superob_radar
  call init_io(mype)
  call init_vtrans
  call init_obsens
  call init_hybrid_ensemble_parameters
  call init_rapidrefresh_cldsurf
  call init_tcps_errvals
  preserve_restart_date=.false.


! Read user input from namelists.  All processor elements 
! read the namelist input.  SGI MPI FORTRAN does not allow
! all tasks to read from standard in (unit 5).  Hence, open
! namelist to different unit number and have each task read 
! namelist file.
#ifdef ibm_sp
  read(5,setup)
  read(5,gridopts)
  read(5,bkgerr)
  read(5,anbkgerr)
  read(5,jcopts)
  read(5,strongopts)
  read(5,obsqc)
  read(5,obs_input)
  read(5,superob_radar)
  read(5,lag_data)
  read(5,hybrid_ensemble)
  read(5,rapidrefresh_cldsurf)
#else
  open(11,file='gsiparm.anl')
  read(11,setup,iostat=ios)
        if(ios/=0) call die(myname_,'read(setup)',ios)
  read(11,gridopts,iostat=ios)
        if(ios/=0) call die(myname_,'read(gridopts)',ios)
  read(11,bkgerr,iostat=ios)
        if(ios/=0) call die(myname_,'read(bkgerr)',ios)
  read(11,anbkgerr,iostat=ios)
        if(ios/=0) call die(myname_,'read(anbkgerr)',ios)
  read(11,jcopts,iostat=ios)
        if(ios/=0) call die(myname_,'read(jcopts)',ios)
  read(11,strongopts,iostat=ios)
        if(ios/=0) call die(myname_,'read(strongopts)',ios)
  read(11,obsqc,iostat=ios)
        if(ios/=0) call die(myname_,'read(obsqc)',ios)
  read(11,obs_input,iostat=ios)
        if(ios/=0) call die(myname_,'read(obs_input)',ios)
  read(11,superob_radar,iostat=ios)
        if(ios/=0) call die(myname_,'read(superob_radar)',ios)
  read(11,lag_data,iostat=ios)
        if(ios/=0) call die(myname_,'read(lag_data)',ios)
  read(11,hybrid_ensemble,iostat=ios)
        if(ios/=0) call die(myname_,'read(hybrid_ensemble)',ios)
  read(11,rapidrefresh_cldsurf,iostat=ios)
        if(ios/=0) call die(myname_,'read(rapidrefresh_cldsurf)',ios)
  close(11)
#endif


! 4D-Var setup
  call setup_4dvar(miter,mype)
  if (.not. l4dvar) then
     ljcdfi=.false.
  endif
  if (l4dvar.and.lsensrecompute) then
     lobsensfc  =lobsensfc  .and.(jiterstart==jiterend)
     lobsensjb  =lobsensjb  .and.(jiterstart==jiterend)
     lobsensincr=lobsensincr.and.(jiterstart==jiterend)
  endif
  lobsensfc=lobsensfc.or.lobsensjb.or.lobsensincr
  lsensrecompute=lsensrecompute.and.lobsensfc
  if (lobsensadj .and. .not.lcongrad) then
     write(6,*)'gsimod: adjoint computation requires congrad',lobsensadj,lcongrad
     call stop2(137)
  end if
  if (jsiga>0 .and. .not.lcongrad) then
     write(6,*)'gsimod: analysis error estimate requires congrad',jsiga,lcongrad
     call stop2(137)
  endif


! Check user input for consistency among parameters for given setups.


! Set regional parameters
  if(filled_grid.and.half_grid) filled_grid=.false.
  regional=wrf_nmm_regional.or.wrf_mass_regional.or.twodvar_regional.or.nems_nmmb_regional


! Check that regional=.true. if jcstrong_option > 2
  if(jcstrong_option>2.and..not.regional) then
     if(mype==0) then
        write(6,*) ' jcstrong_option>2 not allowed except for regional=.true.'
        write(6,*) ' ERROR EXIT FROM GSI'
     end if
     call stop2(328)
  end if


!  jcstrong_option=4 currently requires that 2*nvmodes_keep <= npe
  if(jcstrong_option==4) then
     if(2*nvmodes_keep>npe) then
        if(mype==0) write(6,*)' jcstrong_option=4 and nvmodes_keep > npe'
        if(mype==0) write(6,*)' npe, old value of nvmodes_keep=',npe,nvmodes_keep
        nvmodes_keep=npe/2
        if(mype==0) write(6,*)'    new nvmodes_keep, npe=',nvmodes_keep,npe
     end if
  end if


! Ensure time window specified in obs_input does not exceed 
! specified maximum value
  limit=.false.
  do i=1,ndat
     if (time_window(i)>time_window_max) then
        time_window(i) = time_window_max
        limit = .true.
     endif
  end do
  writediag=.false.
  do i=1,miter+1
     if(write_diag(i))writediag=.true.
  end do
  if(.not. writediag)then
     diag_rad=.false.
     diag_conv=.false.
     diag_ozone=.false.
     diag_aero=.false.
     diag_co=.false.
     diag_pcp=.false.
  end if


  if (mype==0 .and. limit) &
       write(6,*)'GSIMOD:  reset time window for one or ',&
       'more OBS_INPUT entries to ',time_window_max


! Force use of perturb_obs for oberror_tune
  if (oberror_tune ) then
     perturb_obs=.true.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** reset perturb_obs=',perturb_obs
  endif


! Finish initialization of observation setup
  call init_obsmod_vars(mype)


! Force use of external observation error table for regional runs
  if (regional .and. .not.oberrflg) then
     oberrflg=.true.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** reset oberrflg=',oberrflg
  endif


! Set 10m wind factor logicals based on ifact10
  if (ifact10==1 .or. ifact10==2) then
     if (ifact10==1) sfcmod_gfs = .true.
     if (ifact10==2) sfcmod_mm5 = .true.
  endif


! If strong constraint is turned off (jcstrong=.false.), 
! force other strong constraint variables to zero
  if ((.not.jcstrong) .and. nstrong/=0 ) then
     nstrong=0
     if (mype==0) write(6,*)'GSIMOD:  reset nstrong=',nstrong,&
          ' because jcstrong=',jcstrong
  endif
  if (.not.jcstrong) then
     baldiag_full=.false.
     baldiag_inc =.false.
  end if

! Turn off uv option if hybrid/ensemble options is false for purposes 
! of TLNMC 
  if (.not.l_hyb_ens) uv_hyb_ens=.false.

! Turn on derivatives if using dynamic constraint
! For now if wrf mass or 2dvar no dynamic constraint
  if (jcstrong.or.l_foto) tendsflag=.true.
  if (tendsflag) switch_on_derivatives=.true.


! Turn off Jc-pdry weak constraint if regional application
  if (regional) ljcpdry=.false.


! Initialize lagrangian data assimilation - must be called after gsi_4dvar
  call lag_modini()


! Stop if TOO MANY observation input files
  if (ndat>ndatmax) then
     write(6,*)'GSIMOD:  ***ERROR*** ndat=',ndat,' > ndatmax'
     call stop2(89)
  endif


! Ensure tendency flag is on if preciptation data listed as observation type.
! NOTE: only applicable for global run when not using observer
  if (.not.tendsflag .and. .not.regional) then
     check_pcp: do i=1,ndat
        if ( .not.tendsflag .and. index(dtype(i),'pcp') /=0 ) then
           tendsflag = .true.
           switch_on_derivatives=.true.
           if (mype==0) write(6,*)'GSIMOD:  set tendsflag,switch_on_derivatives=',&
                tendsflag,switch_on_derivatives,' for pcp data'
           exit check_pcp
        endif
     end do check_pcp
  endif

! Ensure no conflict between flag lread_obs_save and lread_obs_skip
  if (lread_obs_save .and. lread_obs_skip) then
     if (mype==0) write(6,*)'GSIMOD:  ***ERROR*** lread_obs_save=',lread_obs_save,&
          ' and lread_obs_skip=',lread_obs_skip,' can not both be TRUE'
     call stop2(329)
  endif


! Optionally read in namelist for single observation run
  if (oneobtest) then
     miter=1
     ndat=1
     dfile(1)='prepqc'
     time_window(1)=three
     dplat='oneob'
     dthin=1
     dval=one
     dmesh=one
     factqmin=zero
     factqmax=zero
#ifdef ibm_sp
     read(5,singleob_test)
#else
     open(11,file='gsiparm.anl')
     read(11,singleob_test,iostat=ios)
        if(ios/=0) call die(myname_,'read(singleob_test)',ios)
     close(11)
#endif
     dtype(1)=oneob_type
     if(dtype(1)=='u' .or. dtype(1)=='v')dtype(1)='uv'
     dsis(1)=dtype(1)
  endif

! Write namelist output to standard out
  if(mype==0) then
     write(6,200)
200  format(' calling gsisub with following input parameters:',//)
     write(6,setup)
     write(6,gridopts)
     write(6,bkgerr)
     write(6,anbkgerr)
     write(6,jcopts)
     write(6,strongopts)
     write(6,obsqc)
     ngroup=0
     do i=1,ndat
        dthin(i) = max(dthin(i),0)
        if(dthin(i) > ngroup)ngroup=dthin(i)
     end do
     if(ngroup>0)write(6,*)' ngroup = ',ngroup,' dmesh = ',(dmesh(i),i=1,ngroup)
     do i=1,ndat
        write(6,*)dfile(i),dtype(i),dplat(i),dsis(i),dval(i),dthin(i),dsfcalc(i),time_window(i)
     end do
     write(6,superob_radar)
     write(6,lag_data)
     write(6,hybrid_ensemble)
     write(6,rapidrefresh_cldsurf)	
     if (oneobtest) write(6,singleob_test)
  endif


! If this is a wrf regional run, then run interface with wrf
  update_pint=.false.
  if (regional) call convert_regional_guess(mype,ctph0,stph0,tlm0)


! Initialize variables, create/initialize arrays
  call init_constants(regional)
  call gps_constants(use_compress)
  call init_reg_glob_ll(mype,lendian_in)
  call init_grid_vars(jcap,npe,cvars3d,cvars2d,nrf_var,mype)
  call init_mpi_vars(nsig,mype,nsig1o,nnnn1o,nrf,nvars,nrf_3d,vlevs)
  call create_obsmod_vars
  if (passive_bc) call create_passive_obsmod_vars

  
! Initialize values in radinfo
  call init_rad_vars


  end subroutine gsimain_initialize

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IROUTINE: gsimain_run

! ! INTERFACE:

  subroutine gsimain_run(init_pass,last_pass)
    use mpeu_util, only: tell
    implicit none
    logical, optional, intent(in):: init_pass  ! initial pass through multiple background bins
    logical, optional, intent(in):: last_pass  ! last pass through multiple background bins

!EOC

!---------------------------------------------------------------------------
  logical :: init_pass_
  logical :: last_pass_
 
  init_pass_ =.false.
  if(present(init_pass)) init_pass_=init_pass
  last_pass_  =.false.
  if(present(last_pass)) last_pass_ =last_pass

! Call the main gsi driver routine
  call gsisub(mype, init_pass_,last_pass_)

  end subroutine gsimain_run

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IROUTINE: gsimain_finalize

 subroutine gsimain_finalize

! !REVISION HISTORY:
!
!  30May2010 Todling add final_anasv and final_anacv
!EOC

!---------------------------------------------------------------------------

  implicit none
! Deallocate arrays
  call clean_4dvar
  call destroy_obsmod_vars
  call destroy_mpi_vars
  call gsi_chemtracer_final
  call final_anacv
  call final_anasv

! Done with GSI.
  if (mype==0)  call w3tage('GSI_ANL')
  
  call mpi_finalize(ierror)
 
 end subroutine gsimain_finalize

 end module gsimod

