/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the data arrays
 *  Called by          :  AMB2HDF.c
 ***************************************************************/

/* Time */

short int   	year[MAXSCANLINE_B];
short int   	doy[MAXSCANLINE_B];
char        	month[MAXSCANLINE_B];
char        	dom[MAXSCANLINE_B];
char        	hour[MAXSCANLINE_B];
char        	minute[MAXSCANLINE_B];
char        	second[MAXSCANLINE_B];
float64    	time_tai93[MAXSCANLINE_B];

/* Ancillary Data */

char 	    	mask[MAP_ROWS][MAP_COLS];
char		qc_flag[MAXSCANLINE_B];
char        	orb_mode[MAXSCANLINE_B];
char		stype[MAXSCANLINE_B][NUMSPOT_B];

float		lat[MAXSCANLINE_B][NUMSPOT_B];
float		lon[MAXSCANLINE_B][NUMSPOT_B];
float		lza[MAXSCANLINE_B][NUMSPOT_B];
float		sza[MAXSCANLINE_B][NUMSPOT_B];

/* Antenna Temperature etc. */

long int   	count[NUMCHAN_B][NUMSPOT_B];
short int	corr_org[NUMCHAN_B][NUMSPOT_B][4];
short int	corr_mys[NUMCHAN_B][NUMSPOT_B][3];

float 		at[MAXSCANLINE_B][NUMSPOT_B][NUMCHAN_B];
float          	rat[NUMSPOT_B];
float          	rad[NUMSPOT_B];
float       	clcoef[NUMCHAN_B][3];
float		calrad[NUMCHAN_B][NUMSPOT_B];

/* Mis. Data */

short int	FN_length_AMB;
short int	bad_at;
int       	latbox_up[NUMSPOT_B];
int      	latbox_down[NUMSPOT_B];

/* Structures */

SCANLINE1   	scanline1[MAXSCANLINE_B];
SCANLINE2   	scanline2[MAXSCANLINE_B];
SCANLINE3   	scanline3[MAXSCANLINE_B];
SCANLINE5   	scanline5[MAXSCANLINE_B];

Sys_Time    	Sys_times;
Limit_B     	limit_B;

HBLOCK1     	hblock1;
HBLOCK3     	hblock3;
HBLOCK4     	hblock4;

GEO_STIME   	geo_scantime[MAXSCANLINE_B];
