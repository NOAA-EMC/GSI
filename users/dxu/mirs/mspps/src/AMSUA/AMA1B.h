/*************************************************************
 * File name   : AMA1B.h
 * Function    : Header and data structures for AMSU-A 1B
 * Update      : 05/08/2006
 *************************************************************/

/*---------------------------------*
 *         header record           *
 *---------------------------------*/

typedef struct
{
  char                  dummy0[22];
  char                  original_data_set_name[42];
  char                  dummy1[20];
  unsigned short        start_year;
  unsigned short        start_day_of_year;
  unsigned long         start_milliseconds_of_day;
  char                  dummy2[4];
  unsigned short        end_year;
  unsigned short        end_day_of_year;
  unsigned long         end_milliseconds_of_day;
  char                  dummy3[40];
  unsigned short        last_scan_record;
  char                  dummy4[542];
  long                  t_r_central_wave_number_1;
  char                  dummy5[8];
  long                  t_r_central_wave_number_2;
  char                  dummy6[8];
  long                  t_r_central_wave_number_3;
  char                  dummy7[8];
  long                  t_r_central_wave_number_4;
  char                  dummy8[8];
  long                  t_r_central_wave_number_5;
  char                  dummy9[8];
  long                  t_r_central_wave_number_6;
  char                  dummy10[8];
  long                  t_r_central_wave_number_7; 
  char                  dummy11[8];
  long                  t_r_central_wave_number_8;
  char                  dummy12[8];
  long                  t_r_central_wave_number_9; 
  char                  dummy13[8];
  long                  t_r_central_wave_number_10; 
  char                  dummy14[8];
  long                  t_r_central_wave_number_11;
  char                  dummy15[8];
  long                  t_r_central_wave_number_12;
  char                  dummy16[8];
  long                  t_r_central_wave_number_13;
  char                  dummy17[8];
  long                  t_r_central_wave_number_14;
  char                  dummy18[8];
  long                  t_r_central_wave_number_15;
  char                  dummy19[40];
  unsigned short        orbit_vector_epoch_year;
  unsigned short        orbit_vector_day_of_year;
  unsigned long         orbit_vector_utc_time_of_day;
  long                  semimajor_axis;
  long                  eccentricity;
  long                  inclination;
  long                  argument_of_perigee;
  long                  right_ascension;
  long                  mean_anomaly;
  char                  dummy20[1628];

}HBLOCK;
#define L_HBLOCK  sizeof(HBLOCK)

/*---------------------------------*
 *         scanline data           *
 *---------------------------------*/

typedef struct
{
  char                  dummy1[8];
  unsigned long         time_of_day_of_scan;
  unsigned short        orbit_node; 
  char                  dummy2[10];
  unsigned long         quality_indicator_bit_field;
  char                  dummy3[52];
  long                  pri_cal_coeffs[15][3];
  char                  dummy4[212];
  short                 angular_relationships[90];
  long                  earth_location[60];
  char                  dummy5[12];
  unsigned short        scene_telemetry_a1[510];
  char                  dummy6[268];
  unsigned short        scene_telemetry_a2[120];
  char                  dummy7[128];

}SCANLINE;
#define L_SCANLINE  sizeof(SCANLINE)
