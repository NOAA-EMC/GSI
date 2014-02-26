/*************************************************************
 * File name   : AMSU_A_1BS.h
 * Function    : header and data structures for AMSU-A 1B*    
 * Update      : 04/15/97
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
  char        instrument_id[2];
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


/***  .2.   General information   (bytes: 90) ***/
typedef struct
{
  short int   instrument_status_a2;
  char        a2_inst_stat_cold_cal_msb;
  char        a2_inst_stat_cold_cal_lsb;
  char        a2_inst_stat_nadir_mode;
  char        a2_inst_stat_cold_cal_mode;
  char        a2_inst_stat_warm_cal_mode;
  char        a2_inst_stat_full_scan__mode;
  char        a2_inst_stat_survival_pwr;
  char        a2_inst_stat_module_pwr;
  char        a2_inst_stat_scan_comp_pwr;
  char        a2_inst_stat_scanner_pwr;
  short int   status_change_scan_a2;
  short int   second_instrument_status_a2;
  char        a2_2nd_stat_cold_cal_msb;
  char        a2_2nd_stat_cold_cal_lsb;
  char        a2_2nd_stat_nadir_mode;
  char        a2_2nd_stat_cold_cal_mode;
  char        a2_2nd_stat_warm_cal_mode;
  char        a2_2nd_stat_full_scan__mode;
  char        a2_2nd_stat_survival_pwr;
  char        a2_2nd_stat_module_pwr;
  char        a2_2nd_stat_scan_comp_pwr;
  char        a2_2nd_stat_scanner_pwr;
  short int   instrument_status_a1;
  char        a1_inst_stat_cold_cal_msb;
  char        a1_inst_stat_cold_cal_lsb;
  char        a1_inst_stat_nadir_mode;
  char        a1_inst_stat_cold_cal_mode;
  char        a1_inst_stat_warm_cal_mode;
  char        a1_inst_stat_full_scan__mode;
  char        a1_inst_stat_module_pwr;
  char        a1_inst_stat_survival_pwr;
  char        a1_inst_stat_pllo_pwr;
  char        a1_inst_stat_scanner_a1_2_pwr;
  char        a1_inst_stat_scanner_a1_1_pwr;
  short int   status_change_scan_a1;
  short int   second_instrument_status_a1;
  char        a1_2nd_stat_cold_cal_msb;
  char        a1_2nd_stat_cold_cal_lsb;
  char        a1_2nd_stat_nadir_mode;
  char        a1_2nd_stat_cold_cal_mode;
  char        a1_2nd_stat_warm_cal_mode;
  char        a1_2nd_stat_full_scan__mode;
  char        a1_2nd_stat_module_pwr;
  char        a1_2nd_stat_survival_pwr;
  char        a1_2nd_stat_pllo_pwr;
  char        a1_2nd_stat_scanner_a1_2_pwr;
  char        a1_2nd_stat_scanner_a1_1_pwr;
  short int   scan_count;
  short int   complete_scan_count;
  short int   missing_scan_count;
  short int   data_gaps_count;
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
  short int   instrument_temp_sensor_id[3];
}HBLOCK2;
#define L_HBLOCK2  sizeof(HBLOCK2)


/***  .3.   Temperature-Radiance conversion (chs 1-15)  (bytes: 875) ***/
typedef struct
{
  float     rf_shelf_ref_temp[3][3];  
  float     rf_shelf_ref_temp_pllo_2[3];  
  float     rf_mux_diplexer_ref_temp[3][3];  
  float     rf_mux_ref_temp_pllo_2[3];  
  float     warm_tar_fix_bias_corr[15][3];  
  float     cold_space_fixed_bias_corr[15];  
  float     warm_tar_bias_corr_ch9_14[6][3];  
  float     non_linearity_coeff[15][3];
  float     non_linearity_coeff_pllo_2[6][3];
  float     t_r_central_wave_number[15];
  float     t_r_conversion_constant_1[15];
  float     t_r_conversion_constant_2[15];
  char      ref_ellipsoid_model_id[8];
  float     nadir_tolerance;
  char      reasonableness_test_active;
  char      attitude_error_correction[2];
  float     roll_error_constant;
  float     pitch_error_constant;
  float     yaw_error_constant;
  short int orbit_vector_epoch_year;
  short int orbit_vector_day_of_year;
  long  int orbit_vector_utc_time_of_day;
}HBLOCK3;
#define L_HBLOCK3  sizeof(HBLOCK3)


/***  .4.  (bytes: 148)  ***/
typedef struct
{
  double     semimajor_axis;
  double     eccentricity;
  double     inclination;
  double     argument_of_perigee;
  double     right_ascension;
  double     mean_anomaly;
  double     position_vector_x_component;
  double     position_vector_y_component; 
  double     position_vector_z_component; 
  double     velocity_vector_x_dot_component;
  double     velocity_vector_y_dot_component; 
  double     velocity_vector_z_dot_component;
  double     earth_sun_distance_ratio;
  long int   maneuver_start_time[3];
  long int   maneuver_end_time[3];
  float      delta_v[3];
  float      spacecraft_mass[2];
}HBLOCK4;
#define L_HBLOCK4  sizeof(HBLOCK4)


/***  .5.   AMSU A-1 Digital A Conversion  (bytes: 936) ***/
typedef struct
{
  float     a1_scan_motor_temp_coef[2][4];
  float     a1_feed_horn_temp_coef[2][4];
  float     a1_rf_mux_temp_coef[2][4];
  float     a1_local_osc_temp_coef_ch3_8[6][4];
  float     a1_local_osc_temp_coef_ch15[4];
  float     a1_pllo_2_temp_coef[4];
  float     a1_pllo_1_temp_coef[4];
  float     a1_pllo_ref_osc_temp_coef[4];
  float     a1_mixer_if_amp_ch3_8_temp_coef[6][4]; 
  float     a1_mixer_if_amp_ch9_14_temp_coef[4]; 
  float     a1_mixer_if_amp_ch15_temp_coef[4];
  float     a1_if_amp_ch11_14_temp_coef[4];
  float     a1_if_amp_temp_coef_ch9_14[6][4];
  float     a1_dc_dc_converter_temp_coef[4];
  float     a1_rf_shelf_temp_coef[2][4];
  float     a1_detector_preamp_temp_coef[4];
  float     a1_warm_load_temp_coef[2][5][4];
  float     a1_analog_telem_scan_motor_temp[2][2];  
  float     a1_analog_telem_rf_shelf_temp[2][2];  
  float     a1_analog_telem_warm_load_temp[2][2];  
  float     a1_analog_telem_ant_motor_cur[2][2];  
  float     a1_analog_telem_p15v_signal_proc[2];  
  float     a1_analog_telem_p15v_antenna_drv[2];  
  float     a1_analog_telem_n15v_signal_proc[2];  
  float     a1_analog_telem_n15v_antenna_drv[2];  
  float     a1_analog_telem_p8v_receiver_amp[2];  
  float     a1_analog_telem_p5v_signal_proc[2];
  float     a1_analog_telem_p5v_antenna_drv[2];  
  float     a1_analog_telem_p8_5_p10vdc[2];  
  float     a1_analog_telem_p15v_pll_ch9_14[2];  
  float     a1_analog_telem_n15v_pll_ch9_14[2];  
  float     a1_analog_telem_lo_voltage_ch3_8[6][2];  
  float     a1_analog_telem_pllo_pri_lock[2];  
  float     a1_analog_telem_pllo_sec_lock[2];  
  float     a1_analog_telem_gdo_voltage_ch15[2];  
}HBLOCK5;
#define L_HBLOCK5  sizeof(HBLOCK5)


/***  .6.   AMSU-A2 Digital A Conversion  (bytes: 434) ***/
typedef struct
{
  float     a2_scan_motor_temp_coeff[4];
  float     a2_feed_horn_temp_coef[4];
  float     a2_rf_mux_diplexer_temp_coef[4];
  float     a2_mixer_if_amp_temp_coef[2][4];
  float     a2_local_osc_temp_coef[2][4];
  float     a2_compensator_motor_temp_coef[4];
  float     a2_sub_reflector_temp_coef[4];
  float     a2_dc_dc_converter_temp_coef[4];
  float     a2_rf_shelf_temp_coef[4];
  float     a2_detector_preamp_temp_coef[4];
  float     a2_warm_load_temp_coef[7][4];
  float     a2_analog_telem_scan_motor_temp[2];  
  float     a2_analog_telem_comp_motor_temp[2];  
  float     a2_analog_telem_rf_shelf_temp[2];  
  float     a2_analog_telem_warm_load_temp[2];  
  float     a2_analog_telem_comp_motor_cur[2];  
  float     a2_analog_telem_ant_motor_cur[2];  
  float     a2_analog_telem_p15v_signal_proc[2];  
  float     a2_analog_telem_p15v_antenna_drv[2];  
  float     a2_analog_telem_n15v_signal_proc[2];  
  float     a2_analog_telem_n15v_antenna_drv[2];  
  float     a2_analog_telem_p8_p10vdc[2];  
  float     a2_analog_telem_p5v_signal_proc[2];
  float     a2_analog_telem_p5v_antenna_drv[2];  
  float     a2_analog_telem_lo_voltage_ch1[2];  
  float     a2_analog_telem_lo_voltage_ch2[2];  
  short int lcc_scans;
  float     avg_earth_moon_distance;
  float     avg_moon_sun_angle;
}HBLOCK6;
#define L_HBLOCK6  sizeof(HBLOCK6)


/*---------------------------------*
 *         scanline data           *
 *---------------------------------*/

/***  .1.   Scanline Data  (bytes: 186) ***/
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
  char       calib_scan_problem_indicator[9];
  char       earth_location_problem_indicator[7];
  char       calib_chan_problem_indicator[15][9];
}SCANLINE1;
#define L_SCANLINE1  sizeof(SCANLINE1)


/***  .2.   calibration coefficients  (bytes: 361) ***/
typedef struct
{
  float      pri_cal_coeffs[15][3];
  float      sec_cal_coeffs[15][3];
  char       eartch_loc_accuracy_flag;
}SCANLINE2;
#define L_SCANLINE2  sizeof(SCANLINE2)


/***  .3.   Earth location  (bytes: 2097) ***/
typedef struct
{
  char       earth_loc_attitude_corrected;
  char       earth_loc_indicator;
  char       attitude_control;
  char       attitude_smode;
  char       attitude_pwtipDac;
  long  int  gdtime;
  float      aroll;
  float      apitch;
  float      ayaw; 
  float      total_applied_attitude_corr[3]; 
  short int  s_c_altitude;
  float      solar_zenith_angle[30];
  float      satellite_zenith_angle[30]; 
  float      relative_azimuth_angle[30];
  float      lat_lon_degrees[30][2];
  char       spacecraft_maneuver_flag;
  float      computed_yaw_steering[3];
  char       yaw_steering_flag;
  char       sync_sequence_a1[3];
  char       unit_id_sn_a1;
  char       dig_housekeeping_a1[4];
  short int  position_information[3][30][2];
  char       position_flags[3][30][2];
  short int  observations[30][15];
}SCANLINE3;
#define L_SCANLINE3  sizeof(SCANLINE3)


/***  .4.   Calibration coefficients and data  (bytes: 248) ***/
typedef struct
{
  short int  cold_cal[15][2];
  short int  cold_cal_position_info[3][2];
  char       cold_cal_position_flags[3][2];
  short int  scan_motor_prt_a1[2];
  short int  feed_horn_prt_a1[2];
  short int  rf_mux_prt_a1[2];
  short int  local_oscil_ch3_to_8_prt_a1[6];
  short int  local_oscil_ch15_prt_a1;
  short int  pll_lo2_ch9_to_14_prt_a1;
  short int  pll_lo1_ch9_to_14_prt_a1;
  short int  pllo_prt_a1;
  short int  mix_if_amp_ch_3_8_a1[6];
  short int  mix_if_amp_ch_9_14_a1;
  short int  mix_if_amp_ch_15_a1;
  short int  if_amp_ch_11_14_a1;
  short int  if_amp_chans_9_14_a1[6];
  short int  dc_dc_converter_a1;
  short int  rf_shelf_a1_1;
  short int  rf_shelf_a1_2;
  short int  detector_preamp_assmebly_a1;
  short int  warm_load_prt_a1_1[5];
  short int  warm_load_prt_a1_2[5];
  short int  temp_sensor_ref_volt_a1;
  short int  warm_cal[15][2];
  short int  warm_cal_position_info[3][2];
  char       warm_cal_position_flags[3][2];
}SCANLINE4;
#define L_SCANLINE4  sizeof(SCANLINE4)


/***  .4.   AMSU-A1 Telemetry Digital Housekeeping Data  (bytes: 84) ***/
typedef struct
{
  short int  a1_dig_b_invalid_bit_flags;
  char       a1_cold_cal_pos_msb_flag;
  char       a1_cold_cal_pos_lsb_flag;
  char       a1_ant_in_nadir_pos_flag;
  char       a1_ant_in_cold_pos_flag;
  char       a1_ant_in_warm_pos_flag;
  char       a1_full_scan_flag;
  char       a1_module_pwr_flag; 
  char       a1_survival_htr_flag;
  char       a1_phase_lock_loop_flag;
  char       a1_2_scanner_pwr_flag;
  char       a1_1_scanner_pwr_flag;
  short int  a1_dig_b_telemetry_bit_flag;
  char       a1_cold_cal_pos_msb;
  char       a1_cold_cal_pos_lsb;
  char       a1_ant_in_nadir_pos;
  char       a1_ant_in_cold_pos;
  char       a1_ant_in_warm_pos;
  char       a1_full_scan;
  char       a1_module_pwr; 
  char       a1_survival_htr;
  char       a1_phase_lock_loop;
  char       a1_2_scanner_pwr;
  char       a1_1_scanner_pwr;
  long  int  a1_analog_invalid_bit_flags;
  char       scanner_motor_temps_a1_flag[2];
  char       rf_shelf_temps_a1_flag[2];
  char       warm_load_temps_a1_flag[2];
  char       drive_motor_cur_a1_flag[2];
  char       p15vdc_signal_a1_flag;
  char       p15vdc_antenna_a1_flag;
  char       n15vdc_signal_a1_flag;
  char       n15vdc_antenna_a1_flag;
  char       p8vdc_receiver_a1_flag;
  char       p5vdc_signal_a1_flag;
  char       p5vdc_antenna_a1_flag;
  char       p8_5_p10_vdc_a1_flag;
  char       p15vdc_pll_a1_flag;
  char       n15vdc_pll_a1_flag;
  char       lo_voltage_chans3_8_flag[6];
  char       pllo_primary_detect_a1_flag;
  char       pllo_redundant_detect_a1_flag;
  char       gdo_voltage_ch15_a1_flag;
  char       scanner_motor_temps_a1[2];
  char       rf_shelf_temps_a1[2];
  char       warm_load_temps_a1[2];
  char       drive_motor_cur_a1[2];
  char       p15vdc_signal_a1;
  char       p15vdc_antenna_a1;
  char       n15vdc_signal_a1;
  char       n15vdc_antenna_a1;
  char       p8vdc_receiver_a1;
  char       p5vdc_signal_a1;
  char       p5vdc_antenna_a1;
  char       p8_5vdc_pll_a1;
  char       p15vdc_pll_a1;
  char       n15vdc_pll_a1;
  char       lo_voltage_chans3_8[6];
  char       pllo_primary_detect_a1;
  char       pllo_redundant_detect_a1;
  char       gdo_voltage_ch15_a1;
}SCANLINE5;
#define L_SCANLINE5  sizeof(SCANLINE5)



/***  .5.   AMSU-A2 Analog Housekeeping (TIP)  (bytes: 48) ***/
typedef struct
{
  char       sync_sequence_a2[3];
  char       unit_id_sn_a2;
  char       dig_housekeeping_a2[4];
  short int  scan_motor_a2;
  short int  feed_horn_a2;
  short int  rf_mux_diplexer_a2;
  short int  mixer_if_amp_ch_1_a2; 
  short int  mixer_if_amp_ch_2_a2;
  short int  lo_ch_1_a2;
  short int  lo_ch_2_a2; 
  short int  compensation_motor_a2;
  short int  subreflector_a2;
  short int  dc_dc_converter_a2; 
  short int  rf_shelf_a2;
  short int  detector_preamp_asmbly_a2; 
  short int  warm_load_prt_a2[7];
  short int  temp_sensor_ref_volt_a2; 
}SCANLINE6;
#define L_SCANLINE6  sizeof(SCANLINE6)


/***  .6.   AMSU-A2 Telemetry Digital Housekeeping Data (bytes: 655) ***/
typedef struct
{
  short int  a2_dig_b_invalid_bit_flags;
  char       a2_cold_cal_pos_msb_flag;
  char       a2_cold_cal_pos_lsb_flag;
  char       a2_ant_in_nadir_pos_flag;
  char       a2_ant_in_cold_pos_flag;
  char       a2_ant_in_warm_pos_flag;
  char       a2_full_scan_flag;
  char       a2_survival_htr_flag;
  char       a2_module_pwr_flag; 
  char       a2_compensator_motor_flag;
  char       a2_scanner_pwr_flag;
  short int  a2_dig_b_telemetry_bit_flag;
  char       a2_cold_cal_pos_msb;
  char       a2_cold_cal_pos_lsb;
  char       a2_ant_in_nadir_pos;
  char       a2_ant_in_cold_pos;
  char       a2_ant_in_warm_pos;
  char       a2_full_scan;
  char       a2_survival_htr;
  char       a2_module_pwr; 
  char       a2_compensator_motor;
  char       a2_scanner_pwr;
  short int  a2_analog_invalid_bit_flags;
  char       scanner_motor_temps_a2_flag;
  char       compensator_temp_a2_flag;
  char       rf_shelf_temp_a2_flag;
  char       warm_load_temp_a2_flag;
  char       compensator_motor_cur_a2_flag;
  char       drive_motor_cur_a2_flag; 
  char       p15vdc_signal_a2_flag;
  char       p15vdc_antenna_a2_flag;
  char       n15vdc_signal_a2_flag;
  char       n15vdc_antenna_a2_flag;
  char       p8_p10vdc_a2_flag;
  char       p5vdc_signal_a2_flag;
  char       p5vdc_antenna_a2_flag;
  char       lo_voltage_ch1_flag;
  char       lo_voltage_ch2_flag;
  char       scanner_motor_temps_a2;
  char       compensator_temp_a2;
  char       rf_shelf_temp_a2;
  char       warm_load_temp_a2;
  char       compensator_motor_cur_a2;
  char       drive_motor_cur_a2; 
  char       p15vdc_signal_a2;
  char       p15vdc_antenna_a2;
  char       n15vdc_signal_a2;
  char       n15vdc_antenna_a2;
  char       p8_p10vdc_a2;
  char       p5vdc_signal_a2;
  char       p5vdc_antenna_a2;
  char       lo_voltage_ch1;
  char       lo_voltage_ch2;
  long  int  aip_minor_quality_s_c[80];
  char       mf_missing[80];
  char       mf_questionable[80];
  char       mf_cpu_filler[80];
  char       delta_counts_lcc[15];
  float      lunar_azimuth_angle[3];
  float      lunar_elevation_angle[3];
}SCANLINE7;
#define L_SCANLINE7  sizeof(SCANLINE7)
