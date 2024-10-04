module data
   implicit none

   private
   public :: data_t
   public :: data_ptr

   integer, parameter,public :: max_rdiag_reals = 25

   !--------------------------------------------------
   !  generic index numbers into the rdiagbuf array
   !        for _all_ data types
   !
   integer, parameter, public :: idx_obs_type    = 1     ! obs type
   integer, parameter, public :: idx_obs_subtype = 2     ! obs subtype
   integer, parameter, public :: idx_obs_lat     = 3     ! obs latitude
   integer, parameter, public :: idx_obs_lon     = 4     ! obs longitude
   integer, parameter, public :: idx_stn_elev    = 5     ! stn elevation
   integer, parameter, public :: idx_pres        = 6     ! obs pressure 
   integer, parameter, public :: idx_time        = 8     ! obs time (hrs relative to analysis time)
   integer, parameter, public :: idx_rwgt        = 13    ! non-linear qc relative weight

  

   !--------------------------------------------------
   !  index numbers into the rdiagbuf array
   !        for type   ps   obs
   !
   !  (see setupps.f90 for more info)
   !
   integer, parameter, public :: idx_obs_type_ps    = 1     ! obs type
   integer, parameter, public :: idx_obs_subtype_ps = 2     ! obs subtype
   integer, parameter, public :: idx_obs_lat_ps     = 3     ! obs latitude
   integer, parameter, public :: idx_obs_lon_ps     = 4     ! obs longitude
   integer, parameter, public :: idx_stn_elev_ps    = 5     ! stn elevation
   integer, parameter, public :: idx_pres_ps        = 6     ! obs pressure (hPa)
   integer, parameter, public :: idx_hgt_ps         = 7     ! obs height (meters)
   integer, parameter, public :: idx_time_ps        = 8     ! obs time (hrs relative to analysis time)
   integer, parameter, public :: idx_iqc_ps         = 9     ! prepbufr qc or event mark
   integer, parameter, public :: idx_var_jb_ps      = 10    ! non-linear qc param
   integer, parameter, public :: idx_iuse_ps        = 11    ! read_prepbufr data usage flag
   integer, parameter, public :: idx_anl_use_ps     = 12    ! analysis usage flag
   integer, parameter, public :: idx_rwgt_ps        = 13    ! non-linear qc relative weight
   integer, parameter, public :: idx_err_input_ps   = 14    ! prepbufr invers obs error (hPa**-1)
   integer, parameter, public :: idx_errinv_ps      = 15    ! read_prepbufr invers obs error (hPa**-1)
   integer, parameter, public :: idx_errinv_fnl_ps  = 16    ! final invers obs error (HPa**-1)
   integer, parameter, public :: idx_obs_ps         = 17    ! surface pressure observation
   integer, parameter, public :: idx_obsmg_adj_ps   = 18    ! obs-ges used in analysis (coverted to hPa)
   integer, parameter, public :: idx_obsmg_nadj_ps  = 19    ! obs-ges w/o adjustment to guess surface pressure (hPa)
   integer, parameter, public :: idx_spread_ps      = 20    ! spread (filled in by EnKF)


   !--------------------------------------------------
   !  index numbers into the rdiagbuf array
   !        for type   q   obs
   !
   !  (see setupq.f90 for more info)
   !
   integer, parameter, public :: idx_obs_type_q     = 1     ! obs type
   integer, parameter, public :: idx_obs_subtype_q  = 2     ! obs subtype
   integer, parameter, public :: idx_obs_lat_q      = 3     ! obs latitude
   integer, parameter, public :: idx_obs_lon_q      = 4     ! obs longitude
   integer, parameter, public :: idx_stn_elev_q     = 5     ! stn elevation
   integer, parameter, public :: idx_pres_q         = 6     ! obs pressure (hPa)
   integer, parameter, public :: idx_hgt_q          = 7     ! obs height (meters)
   integer, parameter, public :: idx_time_q         = 8     ! obs time (hrs relative to analysis time)
   integer, parameter, public :: idx_iqc_q          = 9     ! prepbufr qc or event mark
   integer, parameter, public :: idx_var_jb_q       = 10    ! non-linear qc param
   integer, parameter, public :: idx_iuse_q         = 11    ! read_prepbufr data usage flag
   integer, parameter, public :: idx_anl_use_q      = 12    ! analysis usage flag
   integer, parameter, public :: idx_rwgt_q         = 13    ! non-linear qc relative weight
   integer, parameter, public :: idx_err_input_q    = 14    ! prepbufr invers obs error
   integer, parameter, public :: idx_errinv_q       = 15    ! read_prepbufr invers obs error 
   integer, parameter, public :: idx_errinv_fnl_q   = 16    ! final invers obs error 
   integer, parameter, public :: idx_obs_q          = 17    ! surface pressure observation
   integer, parameter, public :: idx_obsmg_adj_q    = 18    ! obs-ges used in analysis 
   integer, parameter, public :: idx_obsmg_nadj_q   = 19    ! obs-ges w/o adjustment to guess surface pressure 
   integer, parameter, public :: idx_ges_sat_q      = 20    ! guess saturation specific
   integer, parameter, public :: idx_spread_q       = 21    ! spread (filled in by EnKF)



  !--------------------------------------------------
  !  index numbers into the rdiagbuf array
  !        for type   t   obs
  !
  !  (see setupt.f90 for more info)
  !
  integer, parameter, public :: idx_obs_type_t    = 1     ! obs type
  integer, parameter, public :: idx_obs_subtype_t = 2     ! obs subtype
  integer, parameter, public :: idx_obs_lat_t     = 3     ! obs latitude
  integer, parameter, public :: idx_obs_lon_t     = 4     ! obs longitude
  integer, parameter, public :: idx_stn_elev_t    = 5     ! stn elevation
  integer, parameter, public :: idx_pres_t        = 6     ! obs pressure (hPa)
  integer, parameter, public :: idx_hgt_t         = 7     ! obs height (meters)
  integer, parameter, public :: idx_time_t        = 8     ! obs time (hrs relative to analysis time)
  integer, parameter, public :: idx_iqc_t         = 9     ! prepbufr qc or event mark
  integer, parameter, public :: idx_setup_qc_t    = 10    ! setup qc or event mark
  integer, parameter, public :: idx_iuse_t        = 11    ! read_prepbufr data usage flag
  integer, parameter, public :: idx_anl_use_t     = 12    ! analysis usage flag
  integer, parameter, public :: idx_rwgt_t        = 13    ! combination of var_jb and rwgt
  integer, parameter, public :: idx_err_input_t   = 14    ! prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_t      = 15    ! read_prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_fnl_t  = 16    ! final inverse obs error 
  integer, parameter, public :: idx_obs_t         = 17    ! temperature obs (K)
  integer, parameter, public :: idx_omgbc_t       = 18    ! obs-ges used in analysis (K)
  integer, parameter, public :: idx_omgnbc_t      = 19    ! obs-ges w/o bias correction (K)
  integer, parameter, public :: idx_pof_t         = 20    ! data pof
  integer, parameter, public :: idx_vv_t          = 21    ! data vertical velocity
  integer, parameter, public :: idx_pbias_one     = 22    ! pred bias term 1
  integer, parameter, public :: idx_pbias_two     = 23    ! pred bias term 2
  integer, parameter, public :: idx_pbias_three   = 24    ! pred bias term 3
  integer, parameter, public :: idx_pbias_four    = 25    ! pred bias term 4


  !--------------------------------------------------
  !  index numbers into the rdiagbuf array
  !        for type   uv   obs
  !
  !  (see setupw.f90 for more info)
  !
  integer, parameter, public :: idx_obs_type_uv    = 1     ! obs type
  integer, parameter, public :: idx_obs_subtype_uv = 2     ! obs subtype
  integer, parameter, public :: idx_obs_lat_uv     = 3     ! obs latitude
  integer, parameter, public :: idx_obs_lon_uv     = 4     ! obs longitude
  integer, parameter, public :: idx_stn_elev_uv    = 5     ! stn elevation
  integer, parameter, public :: idx_pres_uv        = 6     ! obs pressure (hPa)
  integer, parameter, public :: idx_hgt_uv         = 7     ! obs height (meters)
  integer, parameter, public :: idx_time_uv        = 8     ! obs time (hrs relative to analysis time)
  integer, parameter, public :: idx_iqc_uv         = 9     ! prepbufr qc or event mark
  integer, parameter, public :: idx_setup_qc_uv    = 10    ! setup qc or event mark
  integer, parameter, public :: idx_iuse_uv        = 11    ! read_prepbufr data usage flag
  integer, parameter, public :: idx_anl_use_uv     = 12    ! analysis usage flag
  integer, parameter, public :: idx_rwgt_uv        = 13    ! combination of var_jb and rwgt
  integer, parameter, public :: idx_err_input_uv   = 14    ! prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_uv      = 15    ! read_prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_fnl_uv  = 16    ! final inverse obs error 
  integer, parameter, public :: idx_u_obs_uv       = 17    ! u wind component observation
  integer, parameter, public :: idx_u_omgbc_uv     = 18    ! u obs-ges used in analysis (m/s)
  integer, parameter, public :: idx_u_omgnbc_uv    = 19    ! u obs-ges w/o bias correction (m/s)
  integer, parameter, public :: idx_v_obs_uv       = 20    ! v wind component observation
  integer, parameter, public :: idx_v_omgbc_uv     = 21    ! v obs-ges used in analysis (m/s)
  integer, parameter, public :: idx_v_omgnbc_uv    = 22    ! v obs-ges w/o bias correction (m/s)


  !--------------------------------------------------
  !  index numbers into the rdiagbuf array
  !        for type   sst   obs
  !
  !  (see setupsst.f90 for more info)
  !
  integer, parameter, public :: idx_obs_type_sst     = 1     ! obs type
  integer, parameter, public :: idx_obs_subtype_sst  = 2     ! obs subtype
  integer, parameter, public :: idx_obs_lat_sst      = 3     ! obs latitude
  integer, parameter, public :: idx_obs_lon_sst      = 4     ! obs longitude
  integer, parameter, public :: idx_stn_elev_sst     = 5     ! stn elevation
  integer, parameter, public :: idx_opn_wtr_tmp_sst  = 6     ! background open water temperature
  integer, parameter, public :: idx_depth_sst        = 7     ! observation depth (meters)
  integer, parameter, public :: idx_time_sst         = 8     ! obs time (hrs relative to analysis time)
  integer, parameter, public :: idx_opn_wtr_pct_sst  = 9     ! open water percentage
  integer, parameter, public :: idx_setup_qc_sst     = 10    ! setup qc or event mark
  integer, parameter, public :: idx_iuse_sst         = 11    ! read_prepbufr data usage flag
  integer, parameter, public :: idx_anl_use_sst      = 12    ! analysis usage flag
  integer, parameter, public :: idx_rwgt_sst         = 13    ! combination of var_jb and rwgt
  integer, parameter, public :: idx_err_input_sst    = 14    ! prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_sst       = 15    ! read_prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_fnl_sst   = 16    ! final inverse obs error 
  integer, parameter, public :: idx_obs_sst          = 17    ! SST observation (K)
  integer, parameter, public :: idx_omgbc_sst        = 18    ! obs-ges used in analysis (K)
  integer, parameter, public :: idx_omgnbc_sst       = 19    ! obs-ges w/o bias correction (K)
  integer, parameter, public :: idx_type_sst         = 20    ! type of measurment
  integer, parameter, public :: idx_tr_sst           = 21    ! Tr
  integer, parameter, public :: idx_dt_warm_sst      = 22    ! dt_warm at zob
  integer, parameter, public :: idx_dt_cool_sst      = 23    ! dt_cool at zob
  integer, parameter, public :: idx_dtz_dtr_sst      = 23    ! d(tz)/d(Tr) at zob 



  !--------------------------------------------------
  !  index numbers into the rdiagbuf array
  !        for type   gps   obs
  !
  !  (see setupbend.f90 for more info)
  !
  integer, parameter, public :: idx_obs_type_gps     = 1     ! obs type
  integer, parameter, public :: idx_obs_subtype_gps  = 2     ! profile identifier
  integer, parameter, public :: idx_obs_lat_gps      = 3     ! obs latitude
  integer, parameter, public :: idx_obs_lon_gps      = 4     ! obs longitude
  integer, parameter, public :: idx_bend_ang_gps     = 5     ! incremental bending angle (x100 %)
  integer, parameter, public :: idx_pres_gps         = 6     ! pressure at obs location (hPa)
  integer, parameter, public :: idx_height_gps       = 7     ! impact height in meters
  integer, parameter, public :: idx_time_gps         = 8     ! obs time (hrs relative to analysis time)
  integer, parameter, public :: idx_zsges_gps        = 9     ! model terrain (m)    
  integer, parameter, public :: idx_setup_qc_gps     = 10    ! setup qc or event mark
  integer, parameter, public :: idx_iuse_gps         = 11    ! read_prepbufr data usage flag
  integer, parameter, public :: idx_anl_use_gps      = 12    ! analysis usage flag
  integer, parameter, public :: idx_rwgt_gps         = 13    ! combination of var_jb and rwgt
  integer, parameter, public :: idx_err_input_gps    = 14    ! prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_gps       = 15    ! read_prepbufr inverse obs error 
  integer, parameter, public :: idx_errinv_fnl_gps   = 16    ! final inverse obs error 
  integer, parameter, public :: idx_obs_gps          = 17    ! bending angle observation (radians)
  integer, parameter, public :: idx_tref_gps         = 18    ! temperature at obs location (K)
  integer, parameter, public :: idx_hob_gps          = 19    ! model vertival grid
  integer, parameter, public :: idx_uses_gps         = 20    ! uses gps_ref      
  integer, parameter, public :: idx_qref_gps         = 21    ! specific humidity at obs location (kg/kg)
  integer, parameter, public :: idx_spread_gps       = 22    ! spread


  ! Data is stored in data_t
  type :: data_t
     character(8)       :: stn_id
     real,dimension(25) :: rdiag
  end type data_t

  ! A container for storing data_t pointers
  type :: data_ptr
     type(data_t), pointer :: p
  end type data_ptr

end module data
