/*************************************************************	
 * File name   : MHS1B.h	
 * Function    : Header and data structures for MHS 1B	
 * Update      : 05/12/2006	
 *************************************************************/	
	
/*---------------------------------*	
 *         header record           *	
 *---------------------------------*/	
	
typedef struct	
{	
  char             	dummy0[22];
  char                  original_data_set_name[42];
  char                  dummy1[20];
  unsigned short   	start_year;
  unsigned short  	start_day_of_year;
  unsigned long  	start_milliseconds_of_day;
  char             	dummy2[4];
  unsigned short  	end_year;
  unsigned short  	end_day_of_year;
  unsigned long  	end_milliseconds_of_day;
  char             	dummy3[28];
  unsigned short  	last_scan_record;
  char             	dummy4[282];
  unsigned long  	t_r_central_wave_number_1;
  char             	dummy5[8];
  unsigned long  	t_r_central_wave_number_2;
  char             	dummy6[8];
  unsigned long  	t_r_central_wave_number_3;
  char             	dummy7[8];
  unsigned long  	t_r_central_wave_number_4;
  char             	dummy8[8];
  unsigned long  	t_r_central_wave_number_5;
  char             	dummy9[44];
  unsigned short  	orbit_vector_epoch_year;
  unsigned short  	orbit_vector_day_of_year;
  unsigned long  	orbit_vector_utc_time_of_day;
  long  		semimajor_axis;
  long  		eccentricity;
  long  		inclination;
  long  		argument_of_perigee;
  long  		right_ascension;
  long  		mean_anomaly;
  char             	dummy10[2528];
}HBLOCK;	
#define L_HBLOCK  sizeof(HBLOCK)	
	
/*---------------------------------*	
 *         scanline data           *	
 *---------------------------------*/	
	
typedef struct	
{	
  char             	dummy1[8];
  unsigned long  	time_of_day_of_scan;
  unsigned short  	orbit_node;
  char             	dummy2[10];
  unsigned long  	quality_indicator_bit_field;
  char             	dummy3[32];
  long  		pri_cal_coeffs[5][3];
  char             	dummy4[92];
  short  		angular_relationships[270];
  long  		earth_location[180];
  char             	dummy5[8];
  unsigned short  	earth_view_data[540];
  char             	dummy6[512];
	
}SCANLINE;	
#define L_SCANLINE  sizeof(SCANLINE)	
