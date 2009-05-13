!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IPROGRAM: gsimain -- runs NCEP gsi

! ! INTERFACE:

   program gsi

   use gsimod
   use timermod, only: timer_pri

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
!   2006-03-01  eliu    - add logical variable gmao_rtm to the namelist to handle gmao rtm
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-06  middlecoff - added three exit states
!   2006-04-19  treadon - add logical switch dtbduv_on to namelist setup
!   2006-04-20  wu - check OBS_INPUT time_window against obsmod default
!   2006-04-20  kistler - added init_conv for conv ob bias correction
!   2006-04-21  parrish - modifications for new treatment of level 2 radar winds
!   2006-04-21  kleist  - Jc namelist generalized
!   2006-05-22  su - add noiqc flag
!   2006-07-28  derber  - add dfact1 namelist parameter, remove jppf
!   2006-08-15  parrish - add namelist STRONGOPTS for new strong constraint option
!   2006-08-30  zhang,b - add diurnal cycle bias correction capability
!   2006-09-29  treadon - add ifact10 logic, allow limq with qoption=2
!   2006-10-12  treadon - set tendsflag and switch_on_derivatives for pcp data
!   2006-10-25  sienkiewicz - add blacklist flag to namelist
!   2006-11-30  todling - add fpsproj parameter to bkgerr namelist
!   2007-03-12       su - add perturb_obs,perturb_fact,c_varqc
!   2007-03-20  rancic  - incorporate foto
!   2007-04-10  todling - split following C.Cruz and da Silva's modification to ESMF
!   2007-04-13  tremolet - add error code 100
!   2007-06-08  kleist/treadon - add init_directories
!   2007-06-20  rancic/derber - add pbl option
!   2007-07-03  kleist - add variance reweighting option
!   2007-09-30  todling  - add timer
!   2007-10-24  parrish - add l2superob_only option
!   2008-03-24      wu - add oberror tuning option 
!   2008-05-20  Guo, J. - removed obsolete gmao_rtm control
!			- removed diurnal bias correction implmented by Zhang, B.
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
!          =  48 - allocation or i/o error (convinfo)
!          =  49 - error in call rdmemm (read_prepbufr)
!          =  50 - ndata > maxobs  (read_prepbufr)
!          =  51 - invalid pflag (convthin:make3grids)
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
!          =  70 - error setting up assimilation time window
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
!          = 100 - abor1 called
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
!           nhr_offset       -- time of analysis in assimilation window (hours)
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

   call gsimain_initialize

   call gsimain_run

   call gsimain_finalize

   call timer_pri(6)

   end program gsi

