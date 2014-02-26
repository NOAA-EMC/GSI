/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the global data 
 *  Called by          :  BSWATH.c
 ***************************************************************/

/* Time */

short int       year[MAXSCANLINE_B];
short int       doy[MAXSCANLINE_B];
char            month[MAXSCANLINE_B];
char            dom[MAXSCANLINE_B];
char            hour[MAXSCANLINE_B];
char            minute[MAXSCANLINE_B];
char            second[MAXSCANLINE_B];
double          time_tai93[MAXSCANLINE_B];

/* Navigation Data */

short int  	epoch_year;
short int  	epoch_day;
long  int  	epoch_time;

float     	semimajor_axis;
float     	eccentricity;
float     	inclination;
float     	argument_of_perigee;
float     	right_ascension;
float     	mean_anomaly;

/* Ancillary Data */

char            mask[MAP_ROWS_SNOW][MAP_COLS_SNOW];

char        	orb_mode[MAXSCANLINE_B];
char		stype[MAXSCANLINE_B][NUMSPOT_B];
char		stype_snow[MAXSCANLINE_B][NUMSPOT_B];
char		stype_AB[MAXSCANLINE_B][NUMSPOT_B];
char            stype_A[MAXSCANLINE_A][NUMSPOT_A];

float		lat[MAXSCANLINE_B][NUMSPOT_B];
float		lon[MAXSCANLINE_B][NUMSPOT_B];
float		lza[MAXSCANLINE_B][NUMSPOT_B];
float		sza[MAXSCANLINE_B][NUMSPOT_B];

short int       iwp_flag[MAXSCANLINE_B][NUMSPOT_B];
int             latbox_up[NUMSPOT_B];
int             latbox_down[NUMSPOT_B];

/* Antenna Temperature */

float 		at[MAXSCANLINE_B][NUMSPOT_B][NUMCHAN_B];

/* Products */

short int     	rr[MAXSCANLINE_B][NUMSPOT_B];
short int     	snow[MAXSCANLINE_B][NUMSPOT_B];
short int     	snowr[MAXSCANLINE_B][NUMSPOT_B];
short int     	iwp[MAXSCANLINE_B][NUMSPOT_B];
short int       ctype[MAXSCANLINE_B][NUMSPOT_B];
short int	de[MAXSCANLINE_B][NUMSPOT_B];
short int	swe[MAXSCANLINE_B][NUMSPOT_B];

/* LZA parameters for rain rate correction */

double          coe_rr[5][6];

/* AMSU-A data */

float		at_A[MAXSCANLINE_A][NUMSPOT_A][NUMCHAN_A];
float		at_AB[MAXSCANLINE_B][NUMSPOT_B][NUMCHAN_A];
float		lza_A[MAXSCANLINE_A][NUMSPOT_A];
float		lza_AB[MAXSCANLINE_B][NUMSPOT_B];

short int     	clw_A[MAXSCANLINE_A][NUMSPOT_A];
short int      	tpw_A[MAXSCANLINE_A][NUMSPOT_A];
short int     	sice_A[MAXSCANLINE_A][NUMSPOT_A];
short int     	clw_AB[MAXSCANLINE_B][NUMSPOT_B];
short int      	tpw_AB[MAXSCANLINE_B][NUMSPOT_B];
short int     	sice_AB[MAXSCANLINE_B][NUMSPOT_B];

/* AVN data */

short int       num_avn;
short int       avnhr[3];
short int       avnset[3];
float	    	windu_avn[3][NUMROW_GDAS][NUMCOL_GDAS];
float	    	windv_avn[3][NUMROW_GDAS][NUMCOL_GDAS];
float	    	ts_avn[3][NUMROW_GDAS][NUMCOL_GDAS];
float	    	tpw_avn[3][NUMROW_GDAS][NUMCOL_GDAS];

/* SNOWFALL RATE */
short int       load_indx;
float           iwp_tb[MAX_TB];
float           snowr_tb[MAX_TB];

/* ELEVATION */

short int   	elev_large[NUMY_ELEV][NUMX_ELEV];
float       	elev_mean[MAXSCANLINE_B][NUMSPOT_B];
short int   	lonleft, lonright, lattop, latbot;

/* Misc. Data */

short int	FN_length_AMB;
float		fovsize[NUMSPOT_B];
float       	pred89, pred150;
float		at_limits[5][2];

/* Structures */

Sys_Time    	Sys_times;
Limit_Prod  	limit_Prod;
