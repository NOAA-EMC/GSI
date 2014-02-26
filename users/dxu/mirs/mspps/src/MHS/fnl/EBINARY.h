/***********************************************************************
 *  File name          :  EBINARY.h
 *  Function           :  Define binary swath related data
 *  Called by          :  ramb_whdfeos.c, bin_time.c wrt_bin.c 
 ***************************************************************/
extern float         b_rr[MAXSCANLINE_M][NUMSPOT_M];
extern float         b_snow[MAXSCANLINE_M][NUMSPOT_M];
extern float         b_iwp[MAXSCANLINE_M][NUMSPOT_M];

extern long int      b_time[MAXSCANLINE_M][NUMSPOT_M];
extern long int      orbit_start_time;
extern long int      orbit_end_time;

extern long int      header[NUM_HEADER];
extern float         out_array[NUM_PROD];
