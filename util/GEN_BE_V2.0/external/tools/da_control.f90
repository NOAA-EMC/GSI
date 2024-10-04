module da_control

   
   implicit none

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_defines.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
logical :: use_background_errors
logical :: write_increments
logical :: var4d
integer :: var4d_bin
logical :: var4d_lbc
integer :: multi_inc
integer :: var4d_coupling
logical :: print_detail_radar
logical :: print_detail_rad
logical :: print_detail_xa
logical :: print_detail_xb
logical :: print_detail_obs
logical :: print_detail_f_obs
logical :: print_detail_map
logical :: print_detail_grad
logical :: print_detail_regression
logical :: print_detail_spectral
logical :: print_detail_testing
logical :: print_detail_parallel
logical :: print_detail_be
logical :: print_detail_outerloop
logical :: check_max_iv_print
logical :: check_buddy_print
integer :: analysis_accu
logical :: calc_w_increment
logical :: dt_cloud_model
logical :: write_mod_filtered_obs
integer :: fg_format
integer :: ob_format
logical :: thin_conv
logical :: use_synopobs
logical :: use_shipsobs
logical :: use_metarobs
logical :: use_soundobs
logical :: use_mtgirsobs
logical :: use_tamdarobs
logical :: use_pilotobs
logical :: use_airepobs
logical :: use_geoamvobs
logical :: use_polaramvobs
logical :: use_bogusobs
logical :: use_buoyobs
logical :: use_profilerobs
logical :: use_satemobs
logical :: use_gpsztdobs
logical :: use_gpspwobs
logical :: use_gpsrefobs
logical :: use_ssmiretrievalobs
logical :: use_ssmitbobs
logical :: use_ssmt1obs
logical :: use_ssmt2obs
logical :: use_qscatobs
logical :: use_radarobs
logical :: use_radar_rv
logical :: use_radar_rf
logical :: use_radar_rle
logical :: use_radar_rr
logical :: use_hirs2obs
logical :: use_hirs3obs
logical :: use_hirs4obs
logical :: use_mhsobs
logical :: use_msuobs
logical :: use_amsuaobs
logical :: use_amsubobs
logical :: use_airsobs
logical :: use_airsretobs
logical :: use_eos_amsuaobs
logical :: use_hsbobs
logical :: use_ssmisobs
logical :: use_kma1dvar
logical :: use_filtered_rad
logical :: use_obs_errfac
logical :: check_max_iv
real :: max_error_t
real :: max_error_uv
real :: max_error_pw
real :: max_error_ref
real :: max_error_rh
real :: max_error_q
real :: max_error_p
real :: max_error_tb
real :: max_error_thickness
real :: max_error_rv
real :: max_error_rf
real :: max_error_buv
real :: max_error_bt
real :: max_error_bq
real :: max_error_slp
logical :: check_buddy
logical :: put_rand_seed
logical :: omb_set_rand
logical :: omb_add_noise
logical :: position_lev_dependant
integer :: obs_qc_pointer
integer :: qmarker_retain
integer :: max_sound_input
integer :: max_mtgirs_input
integer :: max_tamdar_input
integer :: max_synop_input
integer :: max_geoamv_input
integer :: max_polaramv_input
integer :: max_airep_input
integer :: max_satem_input
integer :: max_pilot_input
integer :: max_radar_input
integer :: max_metar_input
integer :: max_gpspw_input
integer :: max_ships_input
integer :: max_profiler_input
integer :: max_bogus_input
integer :: max_buoy_input
integer :: max_ssmi_rv_input
integer :: max_ssmi_tb_input
integer :: max_ssmt1_input
integer :: max_ssmt2_input
integer :: max_qscat_input
integer :: max_gpsref_input
integer :: max_airsr_input
integer :: max_tovs_input
integer :: max_ssmis_input
integer :: report_start
integer :: report_end
integer :: tovs_start
integer :: tovs_end
logical :: gpsref_thinning
integer :: max_ext_its
integer :: ntmax
integer :: nsave
integer :: write_interval
logical :: precondition_cg
real :: precondition_factor
logical :: use_lanczos
logical :: orthonorm_gradient
integer :: cv_options
integer :: cloud_cv_options
real , DIMENSION(3) :: as1
real , DIMENSION(3) :: as2
real , DIMENSION(3) :: as3
real , DIMENSION(3) :: as4
real , DIMENSION(3) :: as5
integer :: rf_passes
real :: je_factor
real :: power_truncation
logical :: def_sub_domain
real :: x_start_sub_domain
real :: y_start_sub_domain
real :: x_end_sub_domain
real :: y_end_sub_domain
integer :: stdout
integer :: stderr
integer :: trace_unit
integer :: trace_pe
integer :: trace_repeat_head
integer :: trace_repeat_body
integer :: trace_max_depth
logical :: trace_use
logical :: trace_use_frequent
logical :: trace_use_dull
logical :: trace_memory
logical :: trace_all_pes
logical :: trace_csv
logical :: use_html
logical :: warnings_are_fatal
logical :: test_transforms
logical :: test_statistics
logical :: interpolate_stats
real , DIMENSION(99) :: be_eta
logical :: test_dm_exact
integer :: cv_options_hum
integer :: check_rh
real :: set_omb_rand_fac
integer :: seed_array1
integer :: seed_array2
integer :: sfc_assi_options
logical :: calculate_cg_cost_fn
logical :: lat_stats_option
integer :: interp_option
integer :: balance_type
integer :: vert_corr
integer :: vertical_ip
integer :: vert_evalue
real :: max_vert_var1
real :: max_vert_var2
real :: max_vert_var3
real :: max_vert_var4
real :: max_vert_var5
real :: max_vert_var6
real :: max_vert_var7
real :: max_vert_var8
real :: max_vert_var_alpha
real :: psi_chi_factor
real :: psi_t_factor
real :: psi_ps_factor
real :: psi_rh_factor
real :: chi_u_t_factor
real :: chi_u_ps_factor
real :: chi_u_rh_factor
real :: t_u_rh_factor
real :: ps_u_rh_factor
integer :: rtminit_print
integer :: rtminit_nsensor
logical :: thinning
logical :: read_biascoef
logical :: biascorr
logical :: biasprep
logical :: rttov_scatt
logical :: write_profile
logical :: write_jacobian
logical :: qc_rad
logical :: write_iv_rad_ascii
logical :: write_oa_rad_ascii
logical :: write_filtered_rad
logical :: use_error_factor_rad
logical :: use_landem
integer :: mw_emis_sea
integer :: tovs_min_transfer
logical :: tovs_batch
integer :: rtm_option
logical :: use_crtm_kmatrix
logical :: use_rttov_kmatrix
logical :: crtm_cloud
logical :: only_sea_rad
logical :: use_pseudo_rad
integer :: pseudo_rad_platid
integer :: pseudo_rad_satid
integer :: pseudo_rad_senid
integer :: pseudo_rad_ichan
real :: pseudo_rad_lat
real :: pseudo_rad_lon
real :: pseudo_rad_inv
real :: pseudo_rad_err
logical :: use_simulated_rad
logical :: simulated_rad_io
integer :: simulated_rad_ngrid
logical :: use_varbc
logical :: freeze_varbc
real :: varbc_factor
integer :: varbc_nbgerr
integer :: varbc_nobsmin
logical :: use_airs_mmr
logical :: airs_warmest_fov
integer :: crtm_atmosphere
logical , DIMENSION(2) :: use_satcv
integer :: num_pseudo
real :: pseudo_x
real :: pseudo_y
real :: pseudo_z
real :: pseudo_val
real :: pseudo_err
integer :: alphacv_method
integer :: ensdim_alpha
integer :: alpha_truncation
integer :: alpha_corr_type
real :: alpha_corr_scale
real :: alpha_std_dev
logical :: alpha_vertloc
logical :: alpha_hydrometeors
character*256 :: analysis_type
integer :: sensitivity_option
logical :: adj_sens
character*256 :: analysis_date
character*256 :: pseudo_var
character*256 :: documentation_url
character*256 :: time_window_min
character*256 :: time_window_max
logical :: jcdfi_use
integer :: jcdfi_diag
real :: jcdfi_error_wind
real :: jcdfi_error_t
real :: jcdfi_error_mu
logical :: enable_identity
logical :: trajectory_io
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer :: interval_seconds
character*256 :: rsmas_data_path
logical :: all_ic_times
character*256 :: auxinput1_inname
integer :: io_form_auxinput1
logical :: override_restart_timers
character*256 :: auxhist1_inname
character*256 :: auxhist1_outname
integer :: io_form_auxhist1
character*256 :: auxhist2_inname
character*256 :: auxhist2_outname
integer :: io_form_auxhist2
character*256 :: auxhist3_inname
character*256 :: auxhist3_outname
integer :: io_form_auxhist3
character*256 :: auxhist4_inname
character*256 :: auxhist4_outname
integer :: io_form_auxhist4
integer :: debug_level
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
logical :: write_input
logical :: write_restart_at_0h
logical :: adjust_output_times
logical :: adjust_input_times
integer :: diag_print
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_radar
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: time_step_dfi
logical :: step_to_output_time
integer :: adaptation_domain
logical :: use_adaptive_time_step
integer :: max_dom
integer :: num_metgrid_levels
integer :: num_metgrid_soil_levels
real :: p_top_requested
integer :: interp_type
integer :: vert_refine_fact
integer :: extrap_type
integer :: t_extrap_type
logical :: lowest_lev_from_sfc
logical :: use_levels_below_ground
logical :: use_tavg_for_tsk
logical :: use_surface
integer :: lagrange_order
integer :: force_sfc_in_vinterp
real :: zap_close_levels
logical :: sfcp_to_sfcp
logical :: adjust_heights
logical :: smooth_cg_topo
logical :: rh2qv_wrt_liquid
real :: qv_max_p_safe
real :: qv_max_flag
real :: qv_max_value
real :: qv_min_p_safe
real :: qv_min_flag
real :: qv_min_value
integer :: feedback
integer :: smooth_option
integer :: blend_width
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: nproc_x
integer :: nproc_y
integer :: irand
integer :: num_moves
integer :: ts_buf_size
integer :: max_ts_locs
integer :: track_level
logical :: perturb_input
real :: max_dz
logical :: insert_bogus_storm
logical :: remove_storm
integer :: num_storm
integer :: num_land_cat
integer :: num_soil_cat
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: sst_update
integer :: sst_skin
integer :: tmn_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
integer :: lagday
integer :: omlcall
real :: oml_hml0
real :: oml_gamma
integer :: isftcflx
integer :: iz0tlnd
real :: shadlen
integer :: no_mp_heating
integer :: fractional_seaice
logical :: tice2tsk_if2cold
real :: bucket_mm
real :: bucket_j
real :: mp_tend_lim
integer :: prec_acc_opt
integer :: bucketr_opt
real :: obs_nudgezfullr1_uv
real :: obs_nudgezrampr1_uv
real :: obs_nudgezfullr2_uv
real :: obs_nudgezrampr2_uv
real :: obs_nudgezfullr4_uv
real :: obs_nudgezrampr4_uv
real :: obs_nudgezfullr1_t
real :: obs_nudgezrampr1_t
real :: obs_nudgezfullr2_t
real :: obs_nudgezrampr2_t
real :: obs_nudgezfullr4_t
real :: obs_nudgezrampr4_t
real :: obs_nudgezfullr1_q
real :: obs_nudgezrampr1_q
real :: obs_nudgezfullr2_q
real :: obs_nudgezrampr2_q
real :: obs_nudgezfullr4_q
real :: obs_nudgezrampr4_q
real :: obs_nudgezfullmin
real :: obs_nudgezrampmin
real :: obs_nudgezmax
real :: obs_sfcfact
real :: obs_sfcfacr
real :: obs_dpsmx
logical :: obs_ipf_nudob
logical :: obs_ipf_init
integer :: scm_force
real :: scm_force_dx
integer :: num_force_layers
integer :: scm_lu_index
integer :: scm_isltyp
real :: scm_vegfra
integer :: scm_canwat
real :: scm_lat
real :: scm_lon
logical :: scm_th_adv
logical :: scm_wind_adv
logical :: scm_qv_adv
logical :: scm_vert_adv
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: km_opt_dfi
integer :: damp_opt
integer :: gwd_opt
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
logical :: use_baseparam_fr_nml
real :: fft_filter_lat
logical :: rotated_pole
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical :: constant_bc
real :: spec_exp
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
integer :: nobs_ndg_vars
integer :: nobs_err_flds
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
integer :: use_wps_input
integer :: dfi_stage
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE

   
   logical :: use_obsgts
   logical :: use_rad

   
   
   

   
   real, parameter    :: pi = 3.1415926           
   real, parameter    :: radian = pi / 180.0
   real, parameter    :: gas_constant = 287.0     
   real, parameter    :: gas_constant_v = 461.6   
   real, parameter    :: cp = 7.0*gas_constant/2.0 
   real, parameter    :: t_kelvin = 273.15
   real, parameter    :: t_triple = 273.16 
   
   
   
   real, parameter    :: t_roughem = 273.0
   real, parameter    :: t_landem = 273.0

   real, parameter    :: kappa = gas_constant / cp
   real, parameter    :: rd_over_rv = gas_constant / gas_constant_v
   real, parameter    :: rd_over_rv1 = 1.0 - rd_over_rv
   real, parameter    :: L_over_Rv = 5418.12

   real, parameter    :: gamma = 1.4

   
   real, parameter    :: gravity = 9.81        
   
   real, parameter    :: earth_radius = 6370.0          
   
   real, parameter    :: earth_omega  = 0.000072921     

   
   real, parameter    :: es_alpha = 611.2
   real, parameter    :: es_beta = 17.67
   real, parameter    :: es_gamma = 243.5
   real, parameter    :: es_gammabeta = es_gamma * es_beta
   real, parameter    :: es_gammakelvin = es_gamma - t_kelvin

   
   real, parameter    :: SVP1=0.6112, SVP2=17.67, SVP3=29.65
   real, parameter    :: SVPT0=t_kelvin, TO=t_kelvin
   real, parameter    :: N0R=8.0E6, N0S=2.0E7, RHOS=0.1
   real, parameter    :: AVT=841.99667, BVT=0.8, BVT2=2.5+0.5*BVT, BVT3=3.0+BVT
   real, parameter    :: PPI=1.0/(pi*N0R), PPIS=1.0/(pi*N0S*RHOS)
   real, parameter    :: XLV1=2370.0, XLF0=0.3337E6, XLV0=3.15E6
   real, parameter    :: XLS=XLV0-XLV1*t_triple+XLF0

   
   real, parameter         :: k_kar = 0.4    

   
   
   real, parameter    :: zdk1 = 2.2768e-5 
   real, parameter    :: zdk2 = 2.66e-3 
   real, parameter    :: zdk3 = 2.8e-7 
   
   real, parameter    :: wdk1 = 2.21e-7 
   real, parameter    :: wdk2 = 3.73e-3 
 
   
   real, parameter    :: a_ew = 0.622 
   real, parameter    :: b_ew = 0.378  

   
   real, parameter    :: coeff = (wdk2*1.e8) / 77.6

   real, parameter :: da_zero = 0D0

   complex, parameter :: da_zero_complex = (da_zero,da_zero)
   
   
   
   

   

   integer, parameter ::  missing       = -888888
   real   , parameter ::  missing_r     = -888888.0
   real   , parameter ::  Max_StHeight_Diff = 100.0

   integer, parameter :: cv_options_hum_specific_humidity = 1
   integer, parameter :: cv_options_hum_relative_humidity = 2

   
   integer, parameter :: vert_corr_1 = 1
   integer, parameter :: vert_corr_2 = 2

   integer, parameter :: vertical_ip_0            = 0
   integer, parameter :: vertical_ip_sqrt_delta_p = 1
   integer, parameter :: vertical_ip_delta_p      = 2

   integer, parameter :: vert_evalue_global = 1
   integer, parameter :: vert_evalue_local  = 2

   integer, parameter :: alphacv_method_vp = 1
   integer, parameter :: alphacv_method_xa = 2

   integer, parameter :: sfc_assi_options_1 = 1
   integer, parameter :: sfc_assi_options_2 = 2

   integer, parameter :: check_rh_simple = 1
   integer, parameter :: check_rh_tpw    = 2

   logical :: anal_type_verify=.false.
   logical :: anal_type_randomcv=.false.
   logical :: anal_type_qcobs=.false.

   integer,parameter :: monitor_on  = 1
   integer,parameter :: monitor_off = 0

   integer,parameter :: qc_good       =  1
   integer,parameter :: qc_bad        = -1
   integer,parameter :: qc_varbc_bad  = -1

   integer, parameter :: bufr_satellite_id   = 1
   integer, parameter :: bufr_ifov           = 2
   integer, parameter :: bufr_year           = 3
   integer, parameter :: bufr_month          = 4
   integer, parameter :: bufr_day            = 5
   integer, parameter :: bufr_hour           = 6
   integer, parameter :: bufr_minute         = 7
   integer, parameter :: bufr_second         = 8
   integer, parameter :: bufr_lat            = 9
   integer, parameter :: bufr_lon            = 10
   integer, parameter :: bufr_satzen         = 11
   integer, parameter :: bufr_solzen         = 12
   integer, parameter :: bufr_station_height = 13
   integer, parameter :: bufr_landsea_mask   = 14
   integer, parameter :: bufr_solazi         = 15     

   integer, parameter :: nchan_amsua = 15
   integer, parameter :: nchan_amsub = 5
   integer, parameter :: nchan_mhs   = 5
   integer, parameter :: nchan_msu   = 4
   integer, parameter :: nchan_hirs2 = 19
   integer, parameter :: nchan_hirs3 = 19
   integer, parameter :: nchan_hirs4 = 19
   integer, parameter :: nchan_ssmis = 24
   integer, parameter :: nchan_airs  = 281

   

   integer            :: iter
   integer            :: cv_size
   integer, parameter :: MP = 6
   integer, parameter :: LP = 6
   integer, parameter :: MAXFEV = 10
   real, parameter    :: FTOL = 1.0E-4
   real, parameter    :: GTOL = 0.9
   real, parameter    :: XTOL = 1.0E-17
   real, parameter    :: STPMIN = 1.0E-20
   real, parameter    :: STPMAX = 1.0E+20
   
   
   real, parameter    :: pplow = 1.0e-8       
   real, parameter    :: pp_umin = 1.0e-2     
   real, parameter    :: pp_vmin = 1.0e-2     
   real, parameter    :: pp_tmin = 1.0e-2     
   real, parameter    :: pp_qmin = 1.0e-6     
   real, parameter    :: pp_pmin= 1.0e+1      

   
   integer, parameter :: Forward_FFT     = -1 
   integer, parameter :: Inverse_FFT     =  1 
   integer, parameter :: num_fft_factors = 10 
 
   
   integer, parameter :: balance_geo = 1      
   integer, parameter :: balance_cyc = 2      
   integer, parameter :: balance_geocyc = 3   

   
   real, parameter    :: typical_u_rms = 2.0     
   real, parameter    :: typical_v_rms = 2.0     
   real, parameter    :: typical_speed_rms = 2.0 
   real, parameter    :: typical_tb19v_rms = 1.0 
   real, parameter    :: typical_tb19h_rms = 1.0 
   real, parameter    :: typical_tb22v_rms = 1.0 
   real, parameter    :: typical_tb37v_rms = 1.0 
   real, parameter    :: typical_tb37h_rms = 1.0 
   real, parameter    :: typical_tb85v_rms = 1.0 
   real, parameter    :: typical_tb85h_rms = 1.0 
   real, parameter    :: typical_t_rms = 1.0     
   real, parameter    :: typical_p_rms = 100.0   
   real, parameter    :: typical_q_rms = 0.00001 
   real, parameter    :: typical_rho_rms = 0.01  
   real, parameter    :: typical_tpw_rms = 0.2   
   real, parameter    :: typical_ref_rms = 5.0   
   real, parameter    :: typical_rh_rms = 20.0   
   real, parameter    :: typical_thickness_rms = 50.0   
   real, parameter    :: typical_qrn_rms = 0.00001 
   real, parameter    :: typical_qcw_rms = 0.00001 
   real, parameter    :: typical_w_rms = 0.1     
   real, parameter    :: typical_rv_rms = 1.0    
   real, parameter    :: typical_rf_rms = 1.0    

   
   
   

   real, parameter    :: inv_typ_vp1_sumsq = 0.00001 
   real, parameter    :: inv_typ_vp2_sumsq = 0.00001 
   real, parameter    :: inv_typ_vp3_sumsq = 0.00001 
   real, parameter    :: inv_typ_vp4_sumsq = 10000.0 
   real, parameter    :: inv_typ_vp5_sumsq = 0.00001 
   real, parameter    :: inv_typ_vpalpha_sumsq = 1.0 

   CHARACTER (LEN=10) :: release_version = 'V3.2.1    '

   integer, parameter :: fg_format_wrf_arw_regional = 1
   integer, parameter :: fg_format_wrf_nmm_regional = 2
   integer, parameter :: fg_format_wrf_arw_global   = 3
   integer, parameter :: fg_format_kma_global = 4

   integer, parameter :: ob_format_bufr = 1
   integer, parameter :: ob_format_ascii = 2
   integer, parameter :: ob_format_madis = 3

   integer, parameter :: convert_fd2uv = 1
   integer, parameter :: convert_uv2fd = -1

   

   

   

   
   integer, parameter :: trace_csv_unit = 8

   integer :: y_unit, yp_unit, cost_unit, grad_unit, stats_unit, jo_unit
   integer :: check_max_iv_unit, check_buddy_unit, rand_unit, omb_unit, &
              filtered_obs_unit
   integer :: biasprep_unit, qcstat_conv_unit

   integer,parameter :: filename_len = 200

   integer, parameter :: num_alpha_corr_types = 3

   integer, parameter :: alpha_corr_type_exp      = 1
   integer, parameter :: alpha_corr_type_soar     = 2
   integer, parameter :: alpha_corr_type_gaussian = 3

   integer :: alpha_corr_unit1(num_alpha_corr_types)
   integer :: alpha_corr_unit2(num_alpha_corr_types)

   integer, parameter :: max_num_of_var = 200 

   integer, parameter :: unit_start = 20
   integer, parameter :: unit_end = 500
   logical :: unit_used(unit_start:unit_end) = .false.

   

   character(len=3), parameter :: grid_ordering = "xyz"
   character(len=3), parameter :: grid_stagger  = "xyz"

   
   
   

   integer            :: map_projection       
   real               :: ycntr
   integer            :: coarse_ix            
   integer            :: coarse_jy            
   real               :: coarse_ds            
   real               :: start_x              
   real               :: start_y              
   real               :: start_lat            
   real               :: start_lon            
   real               :: delt_lat             
   real               :: delt_lon             

   real               :: phic                 
   real               :: xlonc                
   real               :: cone_factor          
   real               :: truelat1_3dv         
   real               :: truelat2_3dv         
   real               :: pole                 
   real               :: dsm                  
   real               :: psi1                 
   real               :: c2                   

   real               :: ptop
   real, parameter    :: t0 = 300.0

   
   
   

   integer, parameter :: v_interp_not_specified = missing, &
                         v_interp_p             = 1, &
                         v_interp_h             = 2

   
   
   

   integer                :: Anal_Space  
                                         
                                         
                                         

   integer                :: mix         
   integer                :: mjy         
   integer                :: mkz         

   

   real, allocatable      :: rf_turnconds(:) 

   integer, parameter     :: max_ob_levels = 1001 
   integer, parameter     :: max_fgat_time = 100  

   integer                :: time

   logical       :: gaussian_lats  


   integer       :: cv_size_domain_jb    
   integer       :: cv_size_domain_je    
   integer       :: cv_size_domain_jp    
   integer       :: cv_size_domain_js    
   integer       :: cv_size_domain_jl    
   integer       :: cv_size_domain       

   
   real          :: sigma_alpha          
   real          :: jb_factor            

   
   real, parameter :: maximum_rh = 100.0
   real, parameter :: minimum_rh =  10.0

   

   integer, parameter :: jperr = 6

   
   

   real, parameter :: err_k(0:jperr+1) = &
                      (/200000.0, 100100.0,70000.0,50000.0,30000.0,10000.0,5000.0, 1.0/)
   real, parameter :: err_u(0:jperr+1) = &
                      (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7,  2.7/)
   real, parameter :: err_v(0:jperr+1) = &
                      (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7 , 2.7 /)
   real, parameter :: err_t(0:jperr+1) = &
                      (/ 1.8, 1.8,   1.3,   1.3,   2.0,   3.1,  4.0 , 4.0 /)
   real, parameter :: err_rh(0:jperr+1) = &
                      (/ 10.0, 10.0,  10.0,  10.0,  10.0,  10.0, 10.0,  10.0/)
   real, parameter :: err_p(0:jperr+1) = &
                      (/ 100.0,100.0, 100.0, 100.0, 100.0, 100.0,100.0,100.0 /)

   

   real, parameter :: max_buddy_t             =     8.0, &
                      max_buddy_uv            =     8.0, &
                      max_buddy_z             =     8.0, &
                      max_buddy_rh            =    40.0, &
                      max_buddy_p             =   350.0, &
                      buddy_weight            =     1.0, &
                      bin_p_width             =  5000.0, &
                      bin_z_width             =   500.0 

   

   integer, parameter ::  &
      missing_data            = -88, &     
                                           
      outside_of_domain       = -77, &     
                                           
      wrong_direction         = -15, &     
                                           
      negative_spd            = -14, &     
                                           
      zero_spd                = -13, &     
                                           
      wrong_wind_data         = -12, &     
                                           
      zero_t_td               = -11, &     
                                           
      t_fail_supa_inver       = -10, &     
                                           
      wrong_t_sign            = - 9, &     
                                           
      above_model_lid         = - 8, &     
                                           
      far_below_model_surface = - 7, &     
                                           
      below_model_surface     = - 6, &     
                                           
      standard_atmosphere     = - 5, &     
                                           
      from_background         = - 4, &     
                                           
      fails_error_max         = - 3, &     
                                           
      fails_buddy_check       = - 2, &     
                                           
      no_buddies              = - 1, &     
                                           
      good_quality            =   0, &     
                                           
      convective_adjustment   =   1, &     
                                           
      surface_correction      =   2, &     
                                           
      Hydrostatic_recover     =   3, &     
                                           
      Reference_OBS_recover   =   4, &     
                                           
      Other_check             =  88        

   

   integer                :: num_procs            
   integer                :: myproc               
   integer, parameter     :: root = 0             
   logical                :: rootproc             

   integer, parameter :: var4d_coupling_disk_linear = 1
   integer, parameter :: var4d_coupling_disk_simul  = 2

   integer, parameter :: rtm_option_rttov = 1
   integer, parameter :: rtm_option_crtm = 2

   

   integer, parameter            :: maxsensor = 30

   integer, parameter :: num_ob_indexes = 27
   integer, parameter :: npres_print = 12


   

   integer :: trace_start_points=0   

   integer, parameter :: sound     = 1
   integer, parameter :: synop     = 2
   integer, parameter :: pilot     = 3
   integer, parameter :: satem     = 4
   integer, parameter :: geoamv    = 5
   integer, parameter :: polaramv  = 6
   integer, parameter :: airep     = 7
   integer, parameter :: gpspw     = 8
   integer, parameter :: gpsref    = 9
   integer, parameter :: metar     = 10
   integer, parameter :: ships     = 11
   integer, parameter :: ssmi_rv   = 12
   integer, parameter :: ssmi_tb   = 13
   integer, parameter :: ssmt1     = 14
   integer, parameter :: ssmt2     = 15
   integer, parameter :: qscat     = 16
   integer, parameter :: profiler  = 17
   integer, parameter :: buoy      = 18
   integer, parameter :: bogus     = 19
   integer, parameter :: pseudo    = 20
   integer, parameter :: radar     = 21
   integer, parameter :: radiance  = 22
   integer, parameter :: airsr     = 23
   integer, parameter :: sonde_sfc = 24
   integer, parameter :: mtgirs    = 25
   integer, parameter :: tamdar    = 26
   integer, parameter :: tamdar_sfc = 27

   character(len=14), parameter :: obs_names(num_ob_indexes) = (/ &
      "sound         ", &
      "synop         ", &
      "pilot         ", &
      "satem         ", &
      "geoamv        ", &
      "polaramv      ", &
      "airep         ", &
      "gpspw         ", &
      "gpsrf         ", &
      "metar         ", &
      "ships         ", &
      "ssmi_rv       ", &
      "ssmi_tb       ", &
      "ssmt1         ", &
      "ssmt2         ", &
      "qscat         ", &
      "profiler      ", &
      "buoy          ", &
      "bogus         ", &
      "pseudo        ", &
      "radar         ", &
      "radiance      ", &
      "airs retrieval", &
      "sonde_sfc     ", &
      "mtgirs        ", &
      "tamdar        ", &
      "tamdar_sfc    " &

   /)

   integer, parameter :: max_no_fm = 290

   integer, parameter :: num_ob_vars=9

   logical, parameter :: in_report(num_ob_vars,2) = reshape((/&
     .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false., & 
     .true.,.true.,.true.,.true.,.true.,.true.,.false.,.false.,.false./), &
     (/num_ob_vars,2/))

   integer, parameter :: report_h   = 1
   integer, parameter :: report_u   = 2
   integer, parameter :: report_v   = 3
   integer, parameter :: report_t   = 4
   integer, parameter :: report_q   = 5
   integer, parameter :: report_p   = 6
   integer, parameter :: report_rh  = 7
   integer, parameter :: report_slp = 8
   integer, parameter :: report_zk  = 9

   logical :: obs_use(num_ob_indexes) = .false.

   

   integer, parameter :: fm_satem = 86
   integer, parameter :: fm_amv   = 88

   character(len=120)  :: fmt_info ='(a12,1x,a19,1x,a40,1x,i6,3(f12.3,11x),6x,a5)'
   character(len=120)  :: fmt_srfc = '(7(:,f12.3,i4,f7.2))'

   character(len=120)  :: fmt_each = &
      '(3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2))'

   

   real, parameter :: deg_to_rad = pi/180.0
   real, parameter :: rad_to_deg = 1.0/deg_to_rad
  
   real, allocatable :: cos_xls(:)
   real, allocatable :: sin_xls(:)
   real, allocatable :: cos_xle(:)
   real, allocatable :: sin_xle(:)

   integer :: ierr 
   integer :: comm 

   integer :: ids,ide,jds,jde,kds,kde
   integer :: ims,ime,jms,jme,kms,kme
   integer :: its,ite,jts,jte,kts,kte
   integer :: ips,ipe,jps,jpe,kps,kpe
   integer :: itsy,itey,jtsy,jtey,ktsy,ktey
   integer :: itsx,itex,jtsx,jtex,ktsx,ktex

   integer :: num_qcstat_conv(2,num_ob_indexes,num_ob_vars,npres_print)
   character*4, parameter :: ob_vars(num_ob_vars) = (/'U   ','V   ','T   ',&
                                                      'Q   ','Ps  ','Spd ',&
                                                      'Tpw ','GpsR','Thic'/)
   real, parameter :: pptop(1:npres_print) = (/ 1000.0, 900.0, 800.0, 600.0, 400.0, 300.0,  &
                      250.0,  200.0, 150.0, 100.0, 50.0, 0./)

   real, parameter :: ppbot(npres_print) = (/ 1200.0, 999.9, 899.9, 799.0, 599.9, 399.9,  &
                      299.9,  249.9, 199.9, 149.9, 99.9, 2000./)

   real*8, allocatable :: time_slots(:)

   logical :: global

   integer :: num_fgat_time

end module da_control
