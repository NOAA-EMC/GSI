/*************************************************************
 * File name   : AMSU_B_1BS.h
 * Function    : header and data structures for AMSU-B 1B*
 * Update      : 02/07/2005 (H. Meng) 
 *************************************************************/
#pragma  pack(1)

/*---------------------------------*
 *         header record           *
 *---------------------------------*/

/***  .1.   General information   (bytes: 305) ***/
typedef struct
{
  char        component_id[32];
  short int   version_number;
  short int   creation_year_day[2];
  char        letter_q;
  float       pi;
  long  int   hex_afffffff;
  char        local_data_set_name[80];
  char        translator_id[20];
  char        processor_id[20];
  short int   morning_afternoon_indicator;
  short int   first_scan_record;
  short int   last_scan_record;
  long  int   record_length;
  short int   number_of_header_recs;
  char        original_data_set_name[80];
  char        processing_block_id[8];
  short int   spacecraft_id;
  short int   instrument_id;
  short int   data_type_code;
  short int   tip_source_code;
  long  int   start_julian_day;
  short int   start_year;
  short int   start_day_of_year;
  long  int   start_milliseconds_of_day;
  long  int   end_julian_day;
  short int   end_year;
  short int   end_day_of_year;
  long  int   end_milliseconds_of_day;
  short int   fov1_center_delta;
  short int   cpids_year;
  short int   cpids_doy;
}HBLOCK1;
#define L_HBLOCK1  sizeof(HBLOCK1)


/***  .2.   Data set quality indicators (bytes: 86) ***/
typedef struct
{
  long  int   first_instrument_status;
  char        first_processor_check_flag;
  char        first_scan_control_status;
  char        first_pixel_data_invalid_flag;
  char        first_scan_sync;
  char        first_mode_transition_flag;
  char        first_module_id;
  char        first_ram_check_flag; 
  char        first_rom_check_flag; 
  char        first_memory_checks_status;
  char        first_space_view_msb;
  char        first_space_view_lsb;
  char        first_chan_18_19_20;
  char        first_chan_17;
  char        first_chan_16; 
  char        first_stepped_mode; 
  char        first_investigation_mode;  
  char        first_parked_in_space_view_mode;
  char        first_parked_in_nadir_view_mode; 
  char        first_parked_in_target_view_mode;  
  char        first_scan_normal_mode; 
  char        first_survival_heater; 
  char        first_power;  
  short int   status_change_scan_record_num; 
  long  int   second_instrument_status; 
  char        second_processor_check_flag;  
  char        second_scan_control_status;  
  char        second_pixel_data_invalid_flag;  
  char        second_scan_sync;  
  char        second_mode_transition_flag;  
  char        second_module_id;  
  char        second_ram_check_flag; 
  char        second_rom_check_flag; 
  char        second_memory_checks_status;  
  char        second_space_view_msb;  
  char        second_space_view_lsb;  
  char        second_chan_18_19_20;  
  char        second_chan_17;  
  char        second_chan_16; 
  char        second_stepped_mode;  
  char        second_investigation_mode;  
  char        second_parked_in_space_view_mode;  
  char        second_parked_in_nadir_view_mode;  
  char        second_parked_in_target_view_mde;  
  char        second_scan_normal_mode;  
  char        second_survival_heater;  
  char        second_power;  
  short int   scan_count;   
  short int   complete_scan_count;  
  short int   missing_scan_count;  
  short int   data_gaps_count;  
  short int   lunar_contaminated_scan_count;
  long  int   data_frame_count;  
  short int   tip_parity_error_count;  
  short int   sync_error_count;  
  short int   time_sequence_error_record;  
  short int   time_sequence_error_code;  
  short int   clock_update_record;  
  short int   earth_loc_error_record;  
  short int   earth_loc_error_code;   
  char        pseudo_noise_flag;  
  char        tape_direction;  
  char        data_mode;  
  char        data_source;
} HBLOCK2;
#define L_HBLOCK2 sizeof(HBLOCK2)

/***  .3.   Calibration and Navigation (bytes: 365) ***/
typedef struct
{
  short int   instrument_temp_sensor_id;
  float       rf_shelf_ref_temp[2][3];
  float       warm_tar_fix_bias_corr[5][3];
  float       cold_space_fixed_bias_corr[5];
  float       non_linearity_coeff[5][3];
  float       t_r_central_wave_number[5];
  float       t_r_conversion_constant_1[5];
  float       t_r_conversion_constant_2[5];
  char        ref_ellipsoid_model_id[8];
  float       nadir_tolerance;
  char        reasonableness_test_active;
  char        attitude_error_correction[2];
  float       roll_error_constant;
  float       pitch_error_constant;
  float       yaw_error_constant;
  short int   orbit_vector_epoch_year;
  short int   orbit_vector_day_of_year;
  long  int   orbit_vector_utc_time_of_day;
  double      semimajor_axis;
  double      eccentricity;
  double      inclination;
  double      argument_of_perigee;
  double      right_ascension;
  double      mean_anomaly;
  double      position_vector_x_component;
  double      position_vector_y_component;
  double      position_vector_z_component;
  double      velocity_vector_x_dot_component;
  double      velocity_vector_y_dot_component;
  double      velocity_vector_z_dot_component;
  double      earth_sun_distance_ratio;
}HBLOCK3;
#define L_HBLOCK3 sizeof(HBLOCK3)

/***  .4.  AMSU/B digital and analog telemetery conversion (bytes: 2522)  ***/
typedef struct
{
  float     mixer_ch16_temp_coef[4];
  float     mixer_ch17_temp_coef[4];
  float     mixer_ch18_20_temp_coef[4];
  float     fet_amp_temp_coef[5][4];
  float     cal_target_temp_coef[7][4];
  float     sub_reflector_temp_coef[4];
  float     lo_monitor_curr_ch16_coef[4];
  float     lo_monitor_curr_ch17_coe[4];
  float     lo_monitor_curr_ch18_20_coe[4];
  float     lo_ch16_temp_coef[4];
  float     lo_ch17_temp_coef[4];
  float     lo_ch18_20_temp_coef[4];
  float     prt_bridge_voltage_coef[4];
  float     prt_board_temp_coef[4];
  float     analog_telem_second_conv_coef[8][4];
  float     analog_telem_ref_2nd_conv_coef[4];
  float     analog_telem_ice_temp_conv_coef[4];
  float     analog_telem_mde_temp_conv_coef[4];
  float     analog_telem_peu_temp_conv_coef[4];
  float     analog_telem_psu_temp_conv_coef[4];
  float     analog_telem_scan_mtr_temp_coef[4];
  float     analog_telem_scan_mtr_curr_coef[4];
  float     analog_telem_lo_ch16_tmp_coef[4];
  float     analog_telem_lo_ch17_tmp_coef[4];
  float     analog_telem_lo_ch18_20_tmp_coe[4];
  float     lunar_angle_threshold;
  short int tx_count_corrections[4][21][5];
  float     tx_power[4];
  short int tx_newbias_corrections[3][33][5];
}HBLOCK4;
#define L_HBLOCK4 sizeof(HBLOCK4)



/*---------------------------------*
 *         scanline data           *
 *---------------------------------*/

/***  .1.   Scanline Data  (bytes: 83) ***/
typedef struct
{
  short int  scan_line_number;
  short int  year_of_scan;
  short int  day_of_year_of_scan;
  short int  clock_drift_delta;
  long  int  time_of_day_of_scan;
  char       orbit_node;
  char       clock_drift_correction;
  short int  major_frame_count;
  long  int  quality_indicator_bit_field;
  char       do_not_use_scan;
  char       time_error;
  char       data_gap_indicator;
  char       calibration_error;
  char       earth_location_error;
  char       first_good_time_after_update;
  char       instrument_status_change;
  char       amsu_sync_error_detected;
  char       amsu_minor_frame_error_detected;
  char       amsu_major_frame_error_detected;
  char       amsu_parity_error_detected;
  char       time_problem_indicator[4];
  char       calib_scan_problem_indicator[10];
  char       earth_location_problem_indicator[5];
  char       calib_chan_problem_indicator[5][6];
  char       tx_switch_during_cal_interval;
  char       tx_newbias_flag;
  char       tx_newbias_change;
}SCANLINE1;
#define L_SCANLINE1  sizeof(SCANLINE1)

/***  .2.   calibration coefficients  (bytes: 120) ***/
typedef struct
  {
  float      pri_cal_coeffs[5][3];
  float      sec_cal_coeffs[5][3];
}SCANLINE2;
#define L_SCANLINE2  sizeof(SCANLINE2)


/***  .3.   Earth location  (bytes: 3126) ***/
typedef struct
{
  char       earth_loc_attitude_corrected;
  char       earth_loc_indicator;
  char       attitude_control;
  char       attitude_smode;
  char       attitude_pwtipDac;
  char       earth_loc_accuracy_flag;
  long  int  gdtime;
  float      aroll;
  float      apitch;
  float      ayaw;
  float      total_applied_attitude_corr[3];
  short int  s_c_altitude;
  float      solar_zenith_angle[90];
  float      satellite_zenith_angle[90];
  float      relative_azimuth_angle[90];
  float      lat_lon_degrees[90][2];
  short int  observations[90][5];
  short int  position_information[90];
  char       position_flags[90];
  short int  cold_cal_counts[5][4];
  short int  cold_cal_position_info[4];
  char       cold_cal_position_flags[4];
  float      space_view_lunar_angle[4];
  short int  warm_cal_counts[5][4];
  short int  warm_cal_position_info[4];
  char       warm_cal_position_flags[4];
}SCANLINE3;
#define L_SCANLINE3  sizeof(SCANLINE3)

/***  .4.   AMSU/B Digital telemetry (bytes: 82) ***/
typedef struct
{
  long int  dig_invalid_data_bit_flags[2];
  short int  digital_data_word_a01;
  char       proc_check_flag;
  char       scan_control_status;
  char       pixel_data_invalid_flag;
  char       scan_sync;
  char       mode_transition_flag;
  char       dig_module_id;
  short int  digital_b_telemetry;
  char       ram_check_flag;
  char       rom_check_flag;
  char       memory_check_status;
  char       space_view_lsb;
  char       space_view_msb;
  char       chan_18_19_20_relay;
  char       chan_17_relay;
  char       chan_16_relay;
  char       stepped_mode;
  char       investigation_mode;
  char       parked_in_space_view_mode;
  char       parked_in_nadir_view_mode;
  char       parked_in_target_view_mode;
  char       scan_normal_mode;
  char       survival_heater;
  char       relay_power;
  short int  mixer_temp_ch_16;
  short int  mixer_temp_ch_17;
  short int  mixer_temp_ch_18_20;
  short int  fet_amp_temp_ch16;
  short int  fet_amp_temp_ch17;
  short int  fet_amp_temp_ch18;
  short int  fet_amp_temp_ch19;
  short int  fet_amp_temp_ch20;
  short int  cal_target_temp_1;
  short int  cal_target_temp_2;
  short int  cal_target_temp_3;
  short int  cal_target_temp_4;
  short int  cal_target_temp_5;
  short int  cal_target_temp_6;
  short int  cal_target_temp_7;
  short int  subreflector_temp;
  short int  local_oscil_mon_curr_ch16;
  short int  local_oscil_mon_curr_ch17;
  short int  local_oscil_mon_curr_ch18_20;
  short int  local_oscil_temp_ch16;
  short int  local_oscil_temp_ch17;
  short int  local_oscil_temp_ch18_20;
  short int  prt_bridge_voltage;
  short int  prt_board_temperature;
}SCANLINE4;
#define L_SCANLINE4 sizeof(SCANLINE4)

/***  .5.   AMSU-B Analog telemetry (bytes: 85) ***/
typedef struct
{
  long int   analog_invalid_data_bit_flags;
  char       sarr_b_status_flag;                 
  char       sarr_a_status_flag; 
  char       stx_3_power_flag;
  char       stx_2_power_flag;
  char       stx_1_power_flag;
  char       stx_4_status_flag; 
  char       stx_3_status_flag; 
  char       stx_2_status_flag;
  char       stx_1_status_flag;    
  char       lo_temp_ch18_20_flag;
  char       lo_temp_ch17_flag;
  char       lo_temp_ch16_flag;
  char       scanner_motor_curr_flag;
  char       scanner_motor_temp_flag;
  char       psu_temp_flag;
  char       peu_temp_flag;
  char       mde_temp_flag;
  char       ice_temp_flag;
  char       secondary_ref_temp_flag;
  char       secondary_voltage_flags[8];
  short int  secondary_voltage[8];
  short int  secondary_ref_temp;
  short int  ice_temp;
  short int  mde_temp;
  short int  peu_temp;
  short int  psu_temp;
  short int  scanner_motor_temp;
  short int  scanner_motor_curr;
  short int  lo_temp_ch16;
  short int  lo_temp_ch17;
  short int  lo_temp_ch18_20;
  short int  stx_1_status;
  short int  stx_2_status;
  short int  stx_3_status;
  short int  stx_4_status;
  short int  stx_1_power;
  short int  stx_2_power;
  short int  stx_3_power;
  short int  sarr_a_power;
  short int  sarr_b_power;
}SCANLINE5;
#define L_SCANLINE5 sizeof(SCANLINE5)
