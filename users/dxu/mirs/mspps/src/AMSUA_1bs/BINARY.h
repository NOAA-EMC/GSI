/********************************************************************
 *  File name          :  BINARY.h
 *  Function           :  Define binary swath related data
 *  Called by          :  AMA2HDF.c
 *******************************************************************/
float         b_tpw[MAXSCANLINE_A][NUMSPOT_A];
float         b_clw[MAXSCANLINE_A][NUMSPOT_A];
float         b_sice[MAXSCANLINE_A][NUMSPOT_A];
float         b_ts[MAXSCANLINE_A][NUMSPOT_A];
float         b_em1[MAXSCANLINE_A][NUMSPOT_A];
float         b_em2[MAXSCANLINE_A][NUMSPOT_A];
float         b_em3[MAXSCANLINE_A][NUMSPOT_A];
float         b_rr[MAXSCANLINE_A][NUMSPOT_A];
float         b_snow[MAXSCANLINE_A][NUMSPOT_A];

long int      b_time[MAXSCANLINE_A][NUMSPOT_A];
long int      orbit_start_time;
long int      orbit_end_time;

long int      header[NUM_HEADER];
float         out_array[NUM_PROD];
