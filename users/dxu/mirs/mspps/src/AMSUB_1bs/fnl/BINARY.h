/****************************************************************
 *  File name          :  BINARY.h
 *  Function           :  Define binary swath related data
 *  Called by          :  AMA2HDF.c
 ***************************************************************/
float         b_rr[MAXSCANLINE_B][NUMSPOT_B];
float         b_snow[MAXSCANLINE_B][NUMSPOT_B];
float         b_iwp[MAXSCANLINE_B][NUMSPOT_B];

long int      b_time[MAXSCANLINE_B][NUMSPOT_B];
long int      orbit_start_time;
long int      orbit_end_time;

long int      header[NUM_HEADER];
float         out_array[NUM_PROD];
