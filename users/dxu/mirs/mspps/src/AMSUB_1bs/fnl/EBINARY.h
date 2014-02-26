/****************************************************************
 *  File name          :  EBINARY.h
 *  Function           :  Define binary swath related data
 *  Called by          :  rama_whdfeos.c, bin_time.c, wrt_bin.c 
 ***************************************************************/
extern float         b_rr[MAXSCANLINE_B][NUMSPOT_B];
extern float         b_snow[MAXSCANLINE_B][NUMSPOT_B];
extern float         b_iwp[MAXSCANLINE_B][NUMSPOT_B];

extern long int      b_time[MAXSCANLINE_B][NUMSPOT_B];
extern long int      orbit_start_time;
extern long int      orbit_end_time;

extern long int      header[NUM_HEADER];
extern float         out_array[NUM_PROD];
