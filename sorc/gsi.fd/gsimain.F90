program gsi
!$$$  main program documentation block
!                .      .    .                                       .
! main program: GSI_ANL
!   PRGMMR: DERBER           ORG: NP23                DATE: 1999-08-20
!
! abstract: The gridpoint statistical interpolation (GSI) analysis code
!   performs an atmospheric analysis over a specified domain (global
!   or regional) in which guess fields from a forecast model are combined
!   with available observations using a 3D-VAR approach.
!
! program history log:
!   1991-xx-xx  parrish/derber   initial SSI code
!   1991-12-10  parrish/derber   fixed coding error in near sfc anal
!   1992-09-14  derber           improved version of global ssi analysis
!   1998-05-15  weiyu yang       mpp version of global ssi
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-09-02  wu,treadon,kleist,parrish,yang adapt for global/regional 
!                                unified version (see note below)
!   2003-12-24  derber, j. add documentation
!   2004-07-19  parrish - add mass core to regional gsi 
!   2004-07-23  derber - modify to include conventional sst
!   2004-08-04  treadon - add only on use declarations; add intent in/out;
!                         remove eta_regional from namelist (not used)
!   2004-10-12  parrish - namelist modifications for nonlinear qc
!   2004-11-03  treadon - add horizontal scale weighting factors to namelist
!   2004-11-10  treadon - add comments for error codes; initialize variables
!                         wrf_anl_filename and wrf_ges_filename
!   2004-11-29  parrish - remove code to handle regional binary update
!   2004-12-08  xu li   - add logical variable retrieval for SST physical retrieval
!                         algorithm
!   2004-12-15  treadon - update documentation; simplify regional ges & anl i/o
!   2004-12-22  treadon - rename diagnostic output logical flags; add logical
!                         array write_diag to control computation/output of 
!                         diagnostic files on each outer iter
!   2004-12-29  treadon - replace code to handle regional i/o conversions/updates
!                         with a single call to an interface module
!   2005-01-22  parrish - add compact_diffs
!   2005-01-24  kleist  - reorganize namelists
!   2005-02-23  wu      - add namelist variable qoption
!   2005-03-01  parrish - add namelist group for anisotropic background error
!   2005-03-07  dee     - add logical gmao_intfc for gmao model interface!
!   2005-04-08  treadon - add call set_nlnqc_flags
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-05-27  yanqiu  - added obs_sen to control GSI's TLM
!   2005-05-27  kleist/derber - add option to read in new ob error table
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!                                if (norsp==0) will default to polar cascade
!   2005-06-01  treadon - add nlayers to namelist gridopts
!   2005-06-06  wu      - add namelist variable fstat, logical to seperate f
!                         from balance projection
!   2005-07-06  parrish - add/initialize logical variable update_pint
!   2005-07-10  kleist  - add options for Jc term
!   2005-08-01  parrish,lee - add changes to include new surface temperature 
!                             forward model
!   2005-08-03  derber - remove gross and variational qc conventional 
!                        variables from namelists
!   2005-03-24  derber - remove call set_nlnqc_flags
!   2005-09-08  derber - modify to use input group time window clean up unused variables
!   2005-09-28  parrish - modify namelist parameters for radar wind superobs
!   2005-09-29  kleist - expanded namelist for Jc option
!   2005-10-17  parrish - add ctph0,stph0,tlm0 to call convert_regional_guess
!   2005-10-18  treadon - remove dload from OBS_INPUT namelist
!   2005-11-09  wu - turn off limq when using qoption=2
!   2005-11-21  kleist - use tendency module, force flags to true if necessary
!   2005-11-22  wu - add perturb_conv and pfact to SETUP namelist
!   2005-12-01  cucurull - update information to include GPS bending angle code
!   2005-12-20  parrish - add parameter sfcmodel for option to select boundary layer
!                         forward model for surface temperature observations
!   2006-01-10  treadon - move deallocate array calls from gsisub to gsimain
!   2006-01-27  guo     - add namelist to handle gmao grid
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-27  todling - introduced ndatmax (set in obsmod)
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-06  middlecoff - added three exit states
!   2006-04-19  treadon - add logical switch dtbduv_on to namelist setup
!   2006-04-20  wu - check OBS_INPUT time_window against obsmod default
!   2006-04-21  parrish - modifications for new treatment of level 2 radar winds
!   2006-04-21  kleist  - Jc namelist generalized
!   2006-05-22  su - add noiqc flag
!   2006-07-28  derber  - add dfact1 namelist parameter, remove jppf
!   2006-08-15  parrish - add namelist STRONGOPTS for new strong constraint option
!   2006-09-29  treadon - add ifact10 logic, allow limq with qoption=2
!   2006-10-12  treadon - set tendsflag and switch_on_derivatives for pcp data
!   2007-03-01  treadon - set nstrong=nstrong_end=0 when jcstrong=.false.
!   2007-06-08  kleist/treadon - add init_directories
!
! usage:
!   input files:
! **************************
!    input observation data file names are specified in namelist obs_input
! **************************
!     berror_stats  - background error statistics
!     emissivity_coefficients     - IR surface emissivity coefficient file
!     ozinfo        - ozone observation information file
!     pcpinfo       - precipitation rate observation info file
!     satbias_angle - satellite angle dependent file
!     satbias_in    - satellite bias correction coefficient file
!     satinfo       - satellite channel info file
!     sfcf**        - background surface files (typically sfcf03,sfcf06 and sfcf09)
!     sigf**        - background forecast files (typically sigf03,sigf06 and sigf09)
!     spectral_coefficients       - radiative transfer spectral coefficient file
!     transmittance_coefficients  - radiative transfer transmittance coefficient file
!
!     NOTE:  input observation data file names are specified in namelist obs_input
!
!   output files:  (including scratch files)
!     fort.2*      - diagnostic output from setup routines (fort.220 contains
!                    output from the inner loop minimization --> pcgsoi.f90)
!     fort.6       - runtime output
!     pcpbias_out  - output precipitation bias correction file
!     satbias_out  - output satellite bias correction file
!     sfcanl.gsi   - output surface file
!     siganl       - output atmospheric analysis file
!
!     conv."processor"       - conventional observation diagnostic file written
!                              by mpi task "processor"
!     pcp_"type"."processor" - precipitation rate diagnostic file for
!                              satellite/sensor "type" written by mpi task 
!                              "processor"
!     "sattype"*".processor" - brightness temperature diagnostic file for
!                              satellite/sensor "sattype" written by mpi task 
!                              "processor"
!     sbuv2."id"."processor" - sbuv2 ozone diagnostic file for satellite
!                              "id" written by mpi task "processor"
!     
!
!   scratch files
!     obs_setup1***,obs_inner1***,obs_input***** 
!
!   subprograms called:
!     source code files
!         ajmpcp, balance, berror, bkerror, bkgcov, bkgvar, 
!         constants, deter_subdomain, dprodx, dtast, dvast, emiss, emiss_ssmi,
!         fill_mass_grid2, fill_nmm_grid2, fpvsx_ad, gengrid_vars, genqsat,
!         glbsoi, grdcrd, grdsphdp, grid2sub, gridmod, gscond_ad,
!         gsimain, gsisub, guess_grids, half_nmm_grid2, hopers, iceem_amsu,
!         inguesfc, inisph, init_commvars, intall, intall_qc, intdw, intlimq,
!         intoz, intpcp, intps, intpw, intq, intrad, intref, intbend, intrp2a, intrp3,
!         intrp3oz, intrppx, intrw, intspd, intsrw, intsst, intt, intw, jfunc,
!         kinds, landem, locatelat_reg, mpimod, nlmsas_ad, obs_para, obsmod,
!         omegas_ad, oneobmod, ozinfo, pcgsoi, pcpinfo, polcarf, precpd_ad,
!         prewgt, prewgt_reg, psichi2uv_reg, psichi2uvt_reg,
!         qcmod, rad_tran_k, radinfo, rdgesig, rdgstat_reg, rdsfull,
!         read_airs, read_avhrr_navy, read_bufrtovs, read_files, read_goesimg,
!         read_goesndr, read_gps_ref, read_guess, read_ieeetovs, read_lidar,
!         read_obs, read_ozone, read_pcp, read_prepbufr, read_radar, 
!         read_superwinds, read_wrf_mass_files, read_wrf_mass_guess, 
!         read_wrf_nmm_files, read_wrf_nmm_guess, rfdpar, rsearch, satthin,
!         setupdw, setupoz, setuppcp, setupps, setuppw, setupq, setuprad,
!         setupref, setupbend, setuprhsall, setuprw, setupspd, setupsrw, setupsst,
!         setupt, setupw, simpin1, simpin1_init, smooth121, smoothrf,
!         smoothwwrf, smoothzrf, snwem_amsu, specmod, 
!         sst_retrieval, statsconv, statsoz, statspcp, statsrad, stop1, stpbend,
!         stpcalc, stpcalc_qc, stpdw, stplimq, stpoz, stppcp, stpps, stppw,
!         stpq, stprad, stpref, stprw, stpspd, stpsrw, stpsst, stpt, stpw,
!         stvp2uv, stvp2uv_reg, sub2grid, tbalance, tintrp2a, tintrp3,
!         tpause, tpause_t, transform, tstvp2uv, tstvp2uv_reg, unfill_mass_grid2,
!         unfill_nmm_grid2, unhalf_nmm_grid2, update_ggrid, wrf_binary_interface,
!         wrf_netcdf_interface, write_all, wrsfca, wrsiga, wrwrfmassa, wrwrfnmma,
!
!     modules:
!       From GSI:
!         berror, constants, gridmod, guess_grids, jfunc, kinds, mpimod, obsmod, 
!         oneobmod, ozinfo, pcpinfo, qcmod, radinfo, satthin, specmod 
!
!       From Community Radiative Transfer Model (CRTM)):
!         error_handler, initialize, k_matrix_model, spectral_coefficients
!
!       From InfraRed Sea-Surface Emissivity (IRSSE) model:
!         irsse_model
!
!
!      
!     libraries (for NCEP ibm):
!       w3_d      - NCEP W3 library
!       bufr_d_64 - 64 bit NCEP BUFR library
!       sp_d      - NCEP spectral-grid transform library
!       bacio_4   - byte addressable I/O library
!       sigio     - NCEP GFS sigma file I/O library
!       sfcio     - NCEP GFS surface file I/O library
!       CRTM      - Community Radiative Transfer Model
!       ESSL      - fast scientific calculation subroutines
!       MASS      - fast intrinsic function replacements
!       NETCDF    - the netcdf library
!       WRF       - the WRF library
!
!   exit states:
!     cond =   0 - successful run
!          =  24 - problem in update_start_date   
!          =  31 - extrapolation error (interp_a2e)
!          =  32 - failure in sort routine (indexx, in satthin)
!          =  33 - error in coarse --> fine grid interolation operator (get_3ops)
!          =  35 - model top pressure above RTM top pressure (add_layers_rtm)
!          =  36 - total number of model layers > RTM layers 
!          =  41 - illegal min,max horizontal scale (prewgt)
!          =  44 - illegal surface emissivity type(emiss)
!          =  45 - IR surface emissivity failure (emiss)
!          =  54 - data handling mix up(setuprhsall)
!          =  55 - NOBS > NSDATA (setuprhsall-tran)
!          =  59 - problems reading sst analysis (rdgrbsst)
!          =  60 - inconsistent dimensions (rdgrbsst)
!          =  61 - odd number of longitudes (inisph)
!          =  62 - latitude not between +/- pi (inisph)
!          =  63 - error in LUD matrix dcomposition (inisph)
!          =  64 - singular matrix (inisph)
!          =  65 - vanishing row in L-D-U decomposition (ldum)
!          =  66 - singular matrix in L-D-U decomposition (ldum)
!          =  67 - matrix too large in L-D-U decomposition (ldum)
!          =  68 - raflib error (raflib, raflib_8)
!          =  69 - imaginary root to large (rfdpar1)
!          =  71 - channel number mix up (setuprad)
!          =  73 - incompatable horizontal or vertical resolution for statistics (prewgt)
!          =  74 - error reading regional or global guess file
!          =  75 - error writing regional or global analysis file
!          =  76 - error reading guess solution(glbsoi)
!          =  77 - error reading pcpinfo file(pcpinfo)
!          =  78 - incorrect number of records in pcpinfo file(pcpinfo)
!          =  79 - problem reading satellite information file (radinfo)
!          =  80 - problem reading global surface guess file (inguesfc,rdsfull,wrsfca)
!          =  81 - surface guess field tag not yet supported (inguesfc,rdsfull
!          =  82 - problem writing global surface analysis file (wrsfca)
!          =  83 - problem in gps statistics generating code (genstats*)
!          =  84 - buffer size too small for radar data (read_radar)
!          =  85 - guess resolution incompatable with namelist (gsisub)
!          =  86 - too many profile levels (read_gps_ref)
!          =  87 - too many input observation files(assumed max is 55)(gsisub)
!          =  88 - failure in radiative transfer code (rad_tran_k)
!          =  89 - problem reading namelist input (gsimain.F90)
!          =  91 - incompatable observation and analysis dates (read_lidar)
!          =  92 - incompatable observation and analysis dates (read_radar)
!          =  93 - incompatable observation and analysis dates (read_pcp)
!          =  94 - incompatable observation and analysis dates (read_prepbufr)
!          =  95 - incompatable observation and analysis dates (read_ozone)
!          =  96 - incompatable observation and analysis dates (read_gps_ref)
!          =  97 - error in radar wind superob specifications (read_superwinds)
!          =  99 - problem with numerical precision of ges_* and/or bias_* arrays
!
! remarks: resolution, unit numbers and several constants are
!          in the input data cards
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!    NOTE:  PARAMETERS RELATED TO GLOBAL/REGIONAL ANALYSIS:

!     This program has been adapted for use for global or regional analysis.
!     Originally, only one regional model was allowed for, the WRF version of
!     the NCEP non-hydrostatic mesoscale model (WRF NMM).  This model uses a
!     rotated lat-lon coordinate so it was relatively easy to adapt the global
!     code to this rotated grid.  However, to run with other regional models
!     with different map definitions, it would be necessary to make special
!     rules for every model grid.  An alternative has been introduced here, 
!     which requires only input of the earth latitudes and longitudes of the
!     model grid points.  An inverse interpolation scheme is then used to 
!     convert from earth coordinates to model coordinates.  This is a universal
!     technique which works for any regional model input grid, with the exception 
!     that the regional grid cannot have a polar singularity or periodicity.
!     The interpolation introduces small errors, but these errors are proportional
!     to the model resolution.  For 10km models, the maximum coordinate transformation
!     error is < .5km.
!
!     The analysis does not currently work with staggered grids, so some
!     interpolation in the horizontal is required for regional models with
!     grid staggering.  The WRF NMM uses an E-grid, and there are 2 interpolation
!     options, one which fills the holes in the E-grid for more accurate, but
!     much more expensive option, and the other which takes every other row of
!     the E grid, which has no interpolation for mass variables, but winds must
!     be interpolated.  To minimize the impact of interpolation errors, only
!     the analysis increment on the analysis grid is interpolated back to the
!     model grid and added onto the guess.  This is a well-known technique for
!     reducing interpolation error in data assimilation when multiple grids are
!     used.
!
!     There are currently two regional models accepted by the analysis:

!             wrf_nmm_regional = .true.   input is from WRF NMM  (NCEP model)
!             wrf_mass_regional = .true.  input is from WRF MASS-CORE (NCAR model)
!
!     For a regional run, several additional namelist parameters must be specified:
!
!           diagnostic_reg   -- if .true., then run diagnostic tests for debugging
!           update_regsfc    -- if .true., then write updated surface fields to analysis file
!           nhr_assimilation -- assimilation interval in hours, =3 for current NMM assimilation
!                    (following only needed for wrf_nmm_regional =.true.
!           filled_grid      -- if .true. fill in points on WRF NMM E-grid (expensive, but most accurate)
!           half_grid        -- if .true. use every other row of WRF NMM E-grid
!
!     Additional notes:
!
!
!        1.  For regional runs, there are specialized routines at the beginning and end of the
!               analysis for I/O.  Currently the options are for the WRF NMM and the WRF mass core.
!
!        2.  WRF restart files can now be directly read into GSI. There are currently 4 options,
!                a)  WRF NMM binary format
!                b)  WRF NMM netcdf format
!                c)  WRF MC  binary format
!                d)  WRF MC  netcdf format
!            To simplify the initial introduction of direct connection to WRF files, interface
!               routines are called at the beginning and end of gsimain, creating an intermediate
!               binary file which the code currently expects.  However this is now invisible
!               to the user.
!
!
!==================================================================================================
  use kinds, only: i_kind
  use obsmod, only: dmesh,dval,dthin,dtype,dfile,dplat,ndat,diag_conv,&
     init_obsmod_dflts,create_obsmod_vars,write_diag,obs_sen,oberrflg,&
     time_window,perturb_obs,perturb_fact,sfcmodel,destroy_obsmod_vars,dsis,ndatmax,&
     dtbduv_on,time_window_max,init_directories
  use mpimod, only: npe,mpi_comm_world,ierror,init_mpi_vars,destroy_mpi_vars
  use radinfo, only: retrieval,diag_rad,jpch,npred,jpchread,init_rad,init_rad_vars
  use ozinfo, only: jpch_oz,diag_ozone,init_oz
  use oneobmod, only: oblon,oblat,obpres,obhourset,obdattim,oneob_type,&
     oneobtest,magoberr,maginnov	
  use balmod, only: fstat
  use qcmod, only: dfact,dfact1,repe_gps,&
      erradar_inflate,&
      repe_dw,init_qcvars,vadfile,noiqc
  use oneobmod, only: init_oneobmod,oneobmakebufr
  use pcpinfo, only: npredp,diag_pcp,jtype,dtphys,deltim,init_pcp
  use jfunc, only: iout_iter,iguess,miter,factqmin,factqmax,niter,niter_no_qc,biascor,&
     init_jfunc,qoption,switch_on_derivatives,tendsflag
  use berror, only: norh,ndeg,vs,as,bw,init_berror,hzscl,hswgt,pert_berr,pert_berr_fct
  use anberror, only: anisotropic,init_anberror,npass,ifilt_ord,triad4, &
     binom,normal,ngauss,rgauss,an_amp,an_vs,&
     grid_ratio,an_flen_u,an_flen_t,an_flen_z
  use compact_diffs, only: noq,init_compact_diffs
  use jcmod, only: jcterm,jcdivt,bamp_ext1,bamp_ext2,bamp_int1,bamp_int2,init_jcvars
  use tendsmod, only: ctph0,stph0,tlm0
  use mod_vtrans, only: nvmodes_keep,init_vtrans
  use mod_inmi, only: jcstrong,nstrong,nstrong_end,update_end,period_max,period_width,&
       init_strongvars
  use specmod, only: jcap,init_spec,init_spec_vars,destroy_spec_vars
  use gridmod, only: nlat,nlon,nsig,hybrid,wrf_nmm_regional,&
     filled_grid,half_grid,wrf_mass_regional,nsig1o,update_regsfc,&
     diagnostic_reg,gencode,nhr_assimilation,nlon_regional,nlat_regional,&
     twodvar_regional,regional,init_grid,init_reg_glob_ll,init_grid_vars,netcdf,&
     gmao_intfc,nlayers
  use guess_grids, only: ifact10,sfcmod_gfs,sfcmod_mm5
  use gsi_io, only: init_io,lendian_in
  use regional_io, only: convert_regional_guess,update_pint
  use constants, only: zero,one,init_constants,init_constants_derived,three
  use fgrid2agrid_mod, only: nord_f2a,init_fgrid2agrid
  use smooth_polcarf, only: norsp,init_smooth_polcas
  use read_l2bufr_mod, only: minnum,del_azimuth,del_elev,del_range,del_time,&
     range_max,elev_angle_max,initialize_superob_radar
#ifdef _GMAO_FVGSI_
  use m_fvAnaGrid,only : fvAnaGrid_NMLread
#endif
  implicit none


! Declare variables.
  logical:: limit
  integer(i_kind) mype,i,ngroup


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
!     ndat     - number of observations datasets
!     npred    - number of radiance biases predictors
!     jpchread - total number of channels read for radiances bias correction
!     niter()  - number of inner interations for each outer iteration
!     niter_no_qc() - number of inner interations without nonlinear qc for each outer iteration
!     miter    - number of outer iterations
!     qoption  - option of analysis variable; 1:q/qsatg 2:norm RH
!     fstat    - logical to seperate f from balance projection
!     nhr_assimilation - assimilation time interval (currently 6hrs for global, 3hrs for reg)
!     iout_iter- output file number for iteration information
!     npredp   - number of predictors for precipitation bias correction
!     jpch     - number of radiance channels/satellites
!     jtype    - number of precipitation types
!     jpch_oz  - number of ozone levels/satellites
!     retrieval- logical to turn off or on the SST physical retrieval
!     diag_rad - logical to turn off or on the diagnostic radiance file true=on
!     diag_conv-logical to turn off or on the diagnostic conventional file (true=on)
!     perturb_obs  - logical to perturb observations (true=on)
!     perturb_fact - scaling factor for observation perturbations
!     diag_ozone - logical to turn off or on the diagnostic ozone file (true=on)
!     write_diag - logical to write out diagnostic files on outer iteration
!     iguess   - flag for guess solution (currently not working)
!                iguess = -1  do not use guess file
!                iguess =  0  write only guess file
!                iguess =  1  read and write guess file
!                iguess =  2  read only guess file
!     oneobtest- one ob test flag true=on
!     obs_sen - if .true., execute TLM code.  default is .false. --> otherwise skip TLM code
!     switch_on_derivatives - if true, then compute horizontal derivatives of all state variables
!                           (to be used eventually for time derivatives, dynamic constraints,
!                            and observation forward models that need horizontal derivatives)
!     tendsflag - if true, compute time tendencies
!     sfcmodel - if true, then use boundary layer forward model for surface temperature data.
!     dtbduv_on - if true, use d(microwave brightness temperature)/d(uv wind) in inner loop
!     ifact10 - flag for recomputing 10m wind factor
!               ifact10 = 1 compute using GFS surface physics
!               ifact10 = 2 compute using MM5 surface physics
!               ifact10 = 0 or any other value - DO NOT recompute - use value from guess file
!

!     NOTE:  for now, if in regional mode, then iguess=-1 is forced internally.
!            add use of guess file later for regional mode.

  namelist/setup/gencode,factqmin,factqmax,deltim,dtphys,biascor, &
       ndat,npred,jpchread,niter,niter_no_qc,miter,qoption,nhr_assimilation,&
       iout_iter,npredp,retrieval,jpch,jtype,jpch_oz,&
       diag_rad,diag_pcp,diag_conv,diag_ozone,iguess,write_diag,&
       oneobtest,obs_sen,switch_on_derivatives,tendsflag,perturb_obs,&
       perturb_fact,sfcmodel,dtbduv_on,ifact10

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
!     twodvar_regional  - logical for regional 2d-var analysis
!     filled_grid       - logical to fill in puts on WRF-NMM E-grid
!     half_grid         - logical to use every other row of WRF-NMM E-Grid
!     gmao_intfc - logical for GMAO agcm interface
!     nlayers    - number of sub-layers to break indicated model layer into
!                  prior to calling radiative transfer model


  namelist/gridopts/jcap,nsig,nlat,nlon,hybrid,nlat_regional,nlon_regional,&
       diagnostic_reg,update_regsfc,netcdf,regional,wrf_nmm_regional,&
       wrf_mass_regional,twodvar_regional,filled_grid,half_grid,gmao_intfc,nlayers

! BKGERR (background error related variables):
!     as()     - normalized scale factor for background error
!                as(1) = streamfunction
!                as(2) = velocity potential
!                as(3) = log(ps)
!                as(4) = temperature
!                as(5) = water vapor mixing ratio
!                as(7) = skin temperature
!                as(8) = cloud condensate mixing ratio
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

  namelist/bkgerr/as,vs,hzscl,hswgt,norh,ndeg,noq,bw,norsp,fstat,pert_berr,pert_berr_fct

! ANBKGERR (anisotropic background error related variables):
!     anisotropic - if true, then use anisotropic background error
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
!     nord_f2a    - order of interpolation for transfer operators between filter grid and analysis grid
!     ngauss      - number of gaussians to add together in each factor
!     rgauss      - multipliers on reference aspect tensor for each gaussian factor
!     an_amp      - multiplying factors on reference background error variances
!                    an_amp(k, 1) - streamfunction          (k=1,ngauss)
!                    an_amp(k, 2) - velocity potential
!                    an_amp(k, 3) - log(ps)
!                    an_amp(k, 4) - temperature
!                    an_amp(k, 5) - water vapor mixing ratio
!                    an_amp(k, 6) - ozone
!                    an_amp(k, 7) - sea surface temperature
!                    an_amp(k, 8) - cloud condensate mixing ratio
!                    an_amp(k, 9) - land surface temperature
!                    an_amp(k,10) - ice surface temperature
!     an_vs       - scale factor for background error vertical scales (temporary carry over from
!                    isotropic inhomogeneous option)
!     an_flen_u   -  coupling parameter for connecting horizontal wind to background error
!     an_flen_t   -  coupling parameter for connecting grad(pot temp) to background error
!     an_flen_z   -  coupling parameter for connecting grad(terrain) to background error

  namelist/anbkgerr/anisotropic,triad4,ifilt_ord,npass,normal,binom,&
       ngauss,rgauss,an_amp,an_vs, &
       grid_ratio,nord_f2a,an_flen_u,an_flen_t,an_flen_z

! JCOPTS (Jc term)
!     jcterm   - if .true., jc term turned on (linearized about outer loop for now_
!     jcdivt    - if .true., uses divergence tendency formulation
!                 if .false., uses original formulation based on wind, temp, and ps tends
!     bamp_ext1 - multiplying factor for first external component to Jc
!     bamp_ext2 - multiplying factor for second external component to Jc
!     bamp_int1 - multiplying factor for first internal component to Jc
!     bamp_int2 - multiplying factor for second internal component to Jc
!
!     NOTE: magnitudes of the terms differ greatly between the two formulations

  namelist/jcopts/jcterm,jcdivt,bamp_ext1,bamp_ext2,bamp_int1,bamp_int2

! STRONGOPTS (strong dynamic constraint)
!     jcstrong - if .true., strong contraint on
!     nstrong  - if > 0, then number of iterations of implicit normal mode initialization
!                   to apply for each inner loop iteration
!     nstrong_end - if > 0, then number of iterations of implicit normal mode initialization
!                    to apply to final increment at end of each outer loop
!     update_end  - if .true., then actually update analysis increment with nstrong_end iterations
!                              of inmi
!                   if .false., then use inmi at end of inner loop only to diagnose balance of analysis inc
!     period_max - cutoff period for gravity waves included in implicit normal mode
!                   initialization (units = hours)
!     period_width - defines width of transition zone from included to excluded gravity waves
!     nvmodes_keep - number of vertical modes to use in implicit normal mode initialization

  namelist/strongopts/jcstrong,nstrong,nstrong_end,update_end,period_max,period_width,nvmodes_keep

! OBSQC (observation quality control variables):
!
!     Parameters used for gross error checks
!        obserrx = max(ermin,min(ermax,obs error)
!        if(abs(simulated)-observation)/obserrx > gross observation rejected
!
!
!     Parameters below use for nonlinear (variational) quality control
!     repe_dw  - factor for representativeness error in radar doppler winds
!     repe_gps - factor for representativeness error in gps local observations
!     dfact    - factor for duplicate obs at same location for conv. data
!     dfact1   - time factor for duplicate obs at same location for conv. data
!     erradar_inflate - radar error inflation factor
!     oberrflg - logical for reading in new obs error table (if set to true)
!     vadfile  - character(10) variable holding name of vadwnd bufr file
!     noiqc    - logical flag to bypass OIQC (if set to true)

  namelist/obsqc/ repe_dw,repe_gps,dfact,dfact1,erradar_inflate,oberrflg,vadfile,noiqc

! OBS_INPUT (controls input data):
!      dfile(ndat)      - input observation file name
!      dtype(ndat)      - observation type
!      dplat(ndat)      - satellite (platform) id (for satellite data)
!      dsis(ndat)       - sensor/instrument/satellite flag from satinfo files
!      dthin(ndat)      - satellite group
!      dval(ndat)       - relative value of each profile within group
!                         relative weight for observation = dval/sum(dval)
!                         within grid box
!      time_window(ndat)- time window for each input data file
!      dmesh(max(dthin))- thinning mesh for each group
!      time_window_max  - upper limit on time window for all input data

  namelist/obs_input/dfile,dtype,dplat,dsis,dthin,dval,dmesh,time_window,time_window_max

! SINGLEOB_TEST (one observation test case setup):
!      maginnov   - magnitude of innovation for one ob
!      magoberr   - magnitude of observational error
!      oneob_type - observation type
!      oblat      - observation latitude
!      oblon      - observation longitude
!      obpres     - observation pressure
!      obdattim   - observation date
!      obhourset  - observation delta time from analysis time

  namelist/singleob_test/maginnov,magoberr,oneob_type,&
       oblat,oblon,obpres,obdattim,obhourset

! SUPEROB_RADAR (level 2 bufr file to radar wind superobs):
!      del_azimuth     - azimuth range for superob box  (default 5 degrees)
!      del_elev        - elevation angle range for superob box  (default .05 degrees)
!      del_range       - radial range for superob box  (default 5 km)
!      del_time        - 1/2 time range for superob box  (default .5 hours)
!      elev_angle_max  - max elevation angle (default of 5 deg recommended by S. Liu)
!      minnum                  - minimum number of samples needed to make a superob
!      range_max       - max radial range to use in constructing superobs  (default 100km)

  namelist/superob_radar/del_azimuth,del_elev,del_range,del_time,&
       elev_angle_max,minnum,range_max

!*************************************************************
! Begin gsi code
!
! MPI setup
  call mpi_init(ierror)
  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  if (mype==0) call w3tagb('GSI_ANL',1999,0232,0055,'NP23')


! Initialize defaults of vars in modules
  call init_constants_derived
  call init_oneobmod
  call init_qcvars
  call init_obsmod_dflts
  call init_directories(mype)
  call init_pcp
  call init_rad
  call init_oz
  call init_jfunc
  call init_berror
  call init_anberror
  call init_fgrid2agrid
  call init_grid
  call init_spec
  call init_compact_diffs
  call init_smooth_polcas  
  call init_jcvars
  call init_strongvars
  call initialize_superob_radar
  call init_io(mype)
  call init_vtrans


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
#ifdef _GMAO_FVGSI_
    if(gmao_intfc) call fvAnaGrid_NMLread(5)
#endif
#else
  open(11,file='gsiparm.anl')
  read(11,setup)
  read(11,gridopts)
  read(11,bkgerr)
  read(11,anbkgerr)
  read(11,jcopts)
  read(11,strongopts)
  read(11,obsqc)
  read(11,obs_input)
  read(11,superob_radar)
#ifdef _GMAO_FVGSI_
    if(gmao_intfc) call fvAnaGrid_NMLread(11)
#endif
  close(11)
#endif



! Check user input for consistency among parameters for given setups.

! Set regional parameters
  if(filled_grid.and.half_grid) filled_grid=.false.
  regional=wrf_nmm_regional.or.wrf_mass_regional.or.twodvar_regional


! Ensure time window specified in obs_input does not exceed 
! specified maximum value
  limit=.false.
  do i=1,ndat
     if (time_window(i)>time_window_max) then
        time_window(i) = time_window_max
        limit = .true.
     endif
  end do
  if (mype==0 .and. limit) &
       write(6,*)'GSIMAIN:  reset time window for one or ',&
       'more OBS_INPUT entries to ',time_window_max

! Force use of external observation error table for regional runs
  if (regional .and. .not.oberrflg) then
     oberrflg=.true.
     if (mype==0) write(6,*)'GSIMAIN:  ***WARNING*** reset oberrflg=',oberrflg
  endif

! Set 10m wind factor logicals based on ifact10
  if (ifact10==1 .or. ifact10==2) then
     if (ifact10==1) sfcmod_gfs = .true.
     if (ifact10==2) sfcmod_mm5 = .true.
  endif


! Set parameters for GMAO interface
  if (gmao_intfc) then
     hybrid = .true.
     regional = .false.
  end if

! If strong constraint is turned off (jcstrong=.false.), 
! force other strong constraint variables to zero
  if ((.not.jcstrong) .and. (nstrong/=0 .or. nstrong_end/=0)) then
     nstrong=0
     nstrong_end=0
     if (mype==0) write(6,*)'GSIMAIN:  reset nstrong=',nstrong,&
          ' and nstrong_end=',nstrong_end,&
          ' because jcstrong=',jcstrong
  endif


! Turn on derivatives if using dynamic constraint
! For now if wrf mass or 2dvar no dynamic constraint
  if (wrf_mass_regional .or. twodvar_regional) jcterm = .false.
  if (jcterm.or.nstrong.gt.0.or.nstrong_end.gt.0) tendsflag=.true.
  if (tendsflag) switch_on_derivatives=.true.

! Strong constraint only implemented for global--regional to come later
  if(regional.and.(nstrong.gt.0.or.nstrong_end.gt.0)) then
      write(6,*)'GSIMAIN:  ***ERROR*** nstrong and/or nstrong_end > 0 ',&
           'in regional mode--not implemented yet'
      call stop2(89)
  end if

! Stop if TOO MANY observation input files
  if (ndat>ndatmax) then
     write(6,*)'GSIMAIN:  ***ERROR*** ndat=',ndat,' > ndatmax'
     call stop2(89)
  endif


! Ensure tendency flag is on if preciptation data listed as observation type.
! NOTE: only applicable for global run
  if (.not.tendsflag .and. .not.regional) then
     check_pcp: do i=1,ndat
        if ( .not.tendsflag .and. index(dtype(i),'pcp') /=0 ) then
           tendsflag = .true.
           switch_on_derivatives=.true.
           if (mype==0) write(6,*)'GSIMAIN:  set tendsflag,switch_on_derivatives=',&
                tendsflag,switch_on_derivatives,' for pcp data'
           exit check_pcp
        endif
     end do check_pcp
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
     read(11,singleob_test)
     close(11)
#endif
     dtype(1)=oneob_type
     if(dtype(1)=='u' .or. dtype(1)=='v')dtype(1)='uv'
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
       if(dthin(i) > ngroup)ngroup=dthin(i)
     end do
     if(ngroup>0)write(6,*)' ngroup = ',ngroup,' dmesh = ',(dmesh(i),i=1,ngroup)
     do i=1,ndat
        write(6,*)dfile(i),dtype(i),dplat(i),dsis(i),dval(i),dthin(i),time_window(i)
     end do
     write(6,superob_radar)
     if (oneobtest) write(6,singleob_test)
  endif


! If this is a wrf regional run, then run interface with wrf
  update_pint=.false.
  if (regional) call convert_regional_guess(mype,ctph0,stph0,tlm0)


! Initialize variables, create/initialize arrays
  call init_constants(regional)
  call init_reg_glob_ll(mype,lendian_in)
  call init_grid_vars(jcap,npe)
  if (.not.regional) call init_spec_vars(nlat,nlon,nsig)
  call init_mpi_vars(nsig,mype,nsig1o)
  call create_obsmod_vars

  
! Initialize values in radinfo
  call init_rad_vars


! If single ob test, create prep.bufr file with single ob in it
  if (oneobtest) then
     if(mype==0)call oneobmakebufr
     call mpi_barrier(mpi_comm_world,ierror)
  end if
 
! Call the main gsi driver routine
  call gsisub(mype)


! Deallocate arrays
  if (.not.regional) call destroy_spec_vars
  call destroy_obsmod_vars
  call destroy_mpi_vars


! Done with GSI.
  if (mype==0)  call w3tage('GSI_ANL')
  
  call mpi_finalize(ierror)
 
 stop
 end
