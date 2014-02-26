/********************************************************************
 *  File name          :  EBINARY.h
 *  Function           :  Define binary swath related data
 *  Called by          :  rama_whdfeos.c, gnrt_stime.c, wrt_bin.c 
 *******************************************************************/
extern float         b_tpw[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_clw[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_sice[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_ts[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_em1[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_em2[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_em3[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_rr[MAXSCANLINE_A][NUMSPOT_A];
extern float         b_snow[MAXSCANLINE_A][NUMSPOT_A];

extern long int      b_time[MAXSCANLINE_A][NUMSPOT_A];
extern long int      orbit_start_time;
extern long int      orbit_end_time;

extern long int      header[NUM_HEADER];
extern float         out_array[NUM_PROD];
