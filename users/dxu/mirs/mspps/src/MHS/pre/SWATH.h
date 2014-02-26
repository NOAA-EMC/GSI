/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the relevant data 
 *  Called by          :  MHS2HDF.c
 ***************************************************************/
/* Time */

short int   	year[MAXSCANLINE_M];
short int   	doy[MAXSCANLINE_M];
char        	month[MAXSCANLINE_M];
char        	dom[MAXSCANLINE_M];
char        	hour[MAXSCANLINE_M];
char        	minute[MAXSCANLINE_M];
char        	second[MAXSCANLINE_M];
double    	time_tai93[MAXSCANLINE_M];

/* Ancillary Data */

char 	    	mask[MAP_ROWS][MAP_COLS];
char		qc_flag[MAXSCANLINE_M];
char        	orb_mode[MAXSCANLINE_M];
char		stype[MAXSCANLINE_M][NUMSPOT_M];

float		lat[MAXSCANLINE_M][NUMSPOT_M];
float		lon[MAXSCANLINE_M][NUMSPOT_M];
float		lza[MAXSCANLINE_M][NUMSPOT_M];
float		sza[MAXSCANLINE_M][NUMSPOT_M];

float           t_r_central_wave_number[NUMCHAN_M];

/* Antenna Temperature etc. */

long int   	count[NUMCHAN_M][NUMSPOT_M];
short int	corr_org[NUMCHAN_M][NUMSPOT_M][4];
short int	corr_mys[NUMCHAN_M][NUMSPOT_M][3];

float 		at[MAXSCANLINE_M][NUMSPOT_M][NUMCHAN_M];
double		rat[NUMSPOT_M];
double		rad[NUMSPOT_M];
double		clcoef[NUMCHAN_M][3];
double		calrad[NUMCHAN_M][NUMSPOT_M];

/* Mis. Data */

short int	FN_length_MHS;
short int	bad_at;
int       	latbox_up[NUMSPOT_M];
int      	latbox_down[NUMSPOT_M];

/* Structures */

SCANLINE   	scanline[MAXSCANLINE_M];

Sys_Time    	Sys_times;
Limit_M     	limit_M;

HBLOCK     	hblock;

GEO_STIME   	geo_scantime[MAXSCANLINE_M];
