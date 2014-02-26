/***********************************************************************
 *  File name          :  BINARY.h
 *  Function           :  Define binary swath related data
 *  Called by          :  AMA2HDF.c
 ***************************************************************/
float         b_rr[MAXSCANLINE_M][NUMSPOT_M];
float         b_snow[MAXSCANLINE_M][NUMSPOT_M];
float         b_iwp[MAXSCANLINE_M][NUMSPOT_M];

long int      b_time[MAXSCANLINE_M][NUMSPOT_M];
long int      orbit_start_time;
long int      orbit_end_time;

long int      header[NUM_HEADER];
float         out_array[NUM_PROD];
