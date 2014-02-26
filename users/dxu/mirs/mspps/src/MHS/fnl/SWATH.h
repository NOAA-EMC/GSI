/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the global data 
 *  Called by          :  BSWATH.c
 ***************************************************************/
/* Time */
short int       year[MAXSCANLINE_M];
short int       doy[MAXSCANLINE_M];
char            month[MAXSCANLINE_M];
char            dom[MAXSCANLINE_M];
char            hour[MAXSCANLINE_M];
char            minute[MAXSCANLINE_M];
char            second[MAXSCANLINE_M];
double          time_tai93[MAXSCANLINE_M];

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

char        	orb_mode[MAXSCANLINE_M];
char		stype[MAXSCANLINE_M][NUMSPOT_M];
char		stype_snow[MAXSCANLINE_M][NUMSPOT_M];
char            stype_AB[MAXSCANLINE_M][NUMSPOT_M];
char            stype_A[MAXSCANLINE_A][NUMSPOT_A];

float		lat[MAXSCANLINE_M][NUMSPOT_M];
float		lon[MAXSCANLINE_M][NUMSPOT_M];
float		lza[MAXSCANLINE_M][NUMSPOT_M];
float		sza[MAXSCANLINE_M][NUMSPOT_M];

short int       iwp_flag[MAXSCANLINE_M][NUMSPOT_M];

int             latbox_up[NUMSPOT_M];
int             latbox_down[NUMSPOT_M];

/* Antenna Temperature */

float 		at[MAXSCANLINE_M][NUMSPOT_M][NUMCHAN_M];
float 		at150[MAXSCANLINE_M][NUMSPOT_M];
float 		at176[MAXSCANLINE_M][NUMSPOT_M];

/* Products */

short int     	rr[MAXSCANLINE_M][NUMSPOT_M];
short int     	snow[MAXSCANLINE_M][NUMSPOT_M];
short int     	iwp[MAXSCANLINE_M][NUMSPOT_M];
short int       ctype[MAXSCANLINE_M][NUMSPOT_M];
short int	de[MAXSCANLINE_M][NUMSPOT_M];
short int	swe[MAXSCANLINE_M][NUMSPOT_M];
short int       snowr[MAXSCANLINE_M][NUMSPOT_M];

/* LZA parameters for rain rate correction */

double          coe_rr[5][6];


/* AMSU-A data */

double          time_A[MAXSCANLINE_A];
float		at_A[MAXSCANLINE_A][NUMSPOT_A][NUMCHAN_A];
float		at_AB[MAXSCANLINE_M][NUMSPOT_M][NUMCHAN_A];
float		lza_A[MAXSCANLINE_A][NUMSPOT_A];
float		lza_AB[MAXSCANLINE_M][NUMSPOT_M];

short int     	clw_A[MAXSCANLINE_A][NUMSPOT_A];
short int      	tpw_A[MAXSCANLINE_A][NUMSPOT_A];
short int     	sice_A[MAXSCANLINE_A][NUMSPOT_A];
short int     	clw_AB[MAXSCANLINE_M][NUMSPOT_M];
short int      	tpw_AB[MAXSCANLINE_M][NUMSPOT_M];
short int     	sice_AB[MAXSCANLINE_M][NUMSPOT_M];

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
float       	elev_mean[MAXSCANLINE_M][NUMSPOT_M];
short int   	lonleft, lonright, lattop, latbot;

/* Misc. Data */
short int       switch_scnnum;
short int	FN_length_AMB;
float		fovsize[NUMSPOT_M];
float       	pred89, pred150;
float		at_limits[5][2];

/* Structures */

Sys_Time    	Sys_times;
Limit_Prod  	limit_Prod;
