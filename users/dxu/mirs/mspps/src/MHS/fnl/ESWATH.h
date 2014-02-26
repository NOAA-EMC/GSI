/*********************************************************************** 
 *  File name          :  ESWATH.h
 *  Function           :  Definition of all the global  data 
 *  Called by          :  Subroutines 
 ***************************************************************/
/* Time */
extern short int       year[MAXSCANLINE_M];
extern short int       doy[MAXSCANLINE_M];
extern char            month[MAXSCANLINE_M];
extern char            dom[MAXSCANLINE_M];
extern char            hour[MAXSCANLINE_M];
extern char            minute[MAXSCANLINE_M];
extern char            second[MAXSCANLINE_M];
extern double          time_tai93[MAXSCANLINE_M];

/* Navigation Data */

extern short int  	epoch_year;
extern short int  	epoch_day;
extern long  int  	epoch_time;

extern float     	semimajor_axis;
extern float     	eccentricity;
extern float     	inclination;
extern float     	argument_of_perigee;
extern float     	right_ascension;
extern float     	mean_anomaly;

/* Ancillary Data */

extern char            mask[MAP_ROWS_SNOW][MAP_COLS_SNOW];

extern char        	orb_mode[MAXSCANLINE_M];
extern char		stype[MAXSCANLINE_M][NUMSPOT_M];
extern char		stype_snow[MAXSCANLINE_M][NUMSPOT_M];
extern char            stype_AB[MAXSCANLINE_M][NUMSPOT_M];
extern char            stype_A[MAXSCANLINE_A][NUMSPOT_A];

extern float		lat[MAXSCANLINE_M][NUMSPOT_M];
extern float		lon[MAXSCANLINE_M][NUMSPOT_M];
extern float		lza[MAXSCANLINE_M][NUMSPOT_M];
extern float		sza[MAXSCANLINE_M][NUMSPOT_M];

extern short int       iwp_flag[MAXSCANLINE_M][NUMSPOT_M];

extern int             latbox_up[NUMSPOT_M];
extern int             latbox_down[NUMSPOT_M];

/* Antenna Temperature */

extern float 		at[MAXSCANLINE_M][NUMSPOT_M][NUMCHAN_M];
extern float 		at150[MAXSCANLINE_M][NUMSPOT_M];
extern float 		at176[MAXSCANLINE_M][NUMSPOT_M];

/* Products */

extern short int     	rr[MAXSCANLINE_M][NUMSPOT_M];
extern short int     	snow[MAXSCANLINE_M][NUMSPOT_M];
extern short int     	iwp[MAXSCANLINE_M][NUMSPOT_M];
extern short int        ctype[MAXSCANLINE_M][NUMSPOT_M];
extern short int	de[MAXSCANLINE_M][NUMSPOT_M];
extern short int	swe[MAXSCANLINE_M][NUMSPOT_M];
extern short int        snowr[MAXSCANLINE_M][NUMSPOT_M];


/* LZA parameters for rain rate correction */

extern double          coe_rr[5][6];

/* AMSU-A data */

extern double           time_A[MAXSCANLINE_A];
extern float		at_A[MAXSCANLINE_A][NUMSPOT_A][NUMCHAN_A];
extern float		at_AB[MAXSCANLINE_M][NUMSPOT_M][NUMCHAN_A];
extern float		lza_A[MAXSCANLINE_A][NUMSPOT_A];
extern float		lza_AB[MAXSCANLINE_M][NUMSPOT_M];

extern short int     	clw_A[MAXSCANLINE_A][NUMSPOT_A];
extern short int      	tpw_A[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	sice_A[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	clw_AB[MAXSCANLINE_M][NUMSPOT_M];
extern short int      	tpw_AB[MAXSCANLINE_M][NUMSPOT_M];
extern short int     	sice_AB[MAXSCANLINE_M][NUMSPOT_M];

/* AVN data */

extern short int       num_avn;
extern short int       avnhr[3];
extern short int       avnset[3];
extern float	    	windu_avn[3][NUMROW_GDAS][NUMCOL_GDAS];
extern float	    	windv_avn[3][NUMROW_GDAS][NUMCOL_GDAS];
extern float	    	ts_avn[3][NUMROW_GDAS][NUMCOL_GDAS];
extern float	    	tpw_avn[3][NUMROW_GDAS][NUMCOL_GDAS];

/* SNOWFALL RATE */
extern short int       load_indx;
extern float           iwp_tb[MAX_TB];
extern float           snowr_tb[MAX_TB];


/* ELEVATION */

extern short int   	elev_large[NUMY_ELEV][NUMX_ELEV];
extern float       	elev_mean[MAXSCANLINE_M][NUMSPOT_M];
extern short int   	lonleft, lonright, lattop, latbot;

/* Misc. Data */
extern short int        switch_scnnum;
extern short int	FN_length_AMB;
extern float		fovsize[NUMSPOT_M];
extern float       	pred89, pred150;
extern float		at_limits[5][2];

/* Structures */

extern Sys_Time    	Sys_times;
extern Limit_Prod  	limit_Prod;
