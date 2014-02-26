/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the relevant data 
 *  Called by          :  AMA2HDF.c
 ***************************************************************/
/* Time */

short int   	year[MAXSCANLINE_A];
short int   	doy[MAXSCANLINE_A];
char        	month[MAXSCANLINE_A];
char        	dom[MAXSCANLINE_A];
char        	hour[MAXSCANLINE_A];
char        	minute[MAXSCANLINE_A];
char        	second[MAXSCANLINE_A];
float64    	time_tai93[MAXSCANLINE_A];

/* Ancillary Data */

char 	    	mask[MAP_ROWS][MAP_COLS];
char		qc_flag[MAXSCANLINE_A];
char        	orb_mode[MAXSCANLINE_A];
char		stype[MAXSCANLINE_A][NUMSPOT_A];

float		lat[MAXSCANLINE_A][NUMSPOT_A];
float		lon[MAXSCANLINE_A][NUMSPOT_A];
float		lza[MAXSCANLINE_A][NUMSPOT_A];
float		sza[MAXSCANLINE_A][NUMSPOT_A];

/* Antenna Temperature etc. */

short int   	count[NUMCHAN_A][NUMSPOT_A];

float 		at[MAXSCANLINE_A][NUMSPOT_A][NUMCHAN_A];
float		ssmi_r[SSMI_LON][SSMI_LAT];
float          	rat[NUMSPOT_A];
float          	rad[NUMSPOT_A];
float       	clcoef[NUMCHAN_A][3];
float		calrad[NUMCHAN_A][NUMSPOT_A];

/* Products */

short int     	clw[MAXSCANLINE_A][NUMSPOT_A];
short int      	tpw[MAXSCANLINE_A][NUMSPOT_A];
short int     	sice[MAXSCANLINE_A][NUMSPOT_A];
short int     	ts[MAXSCANLINE_A][NUMSPOT_A];
short int     	em1[MAXSCANLINE_A][NUMSPOT_A];
short int     	em2[MAXSCANLINE_A][NUMSPOT_A];
short int     	em3[MAXSCANLINE_A][NUMSPOT_A];
short int     	rr[MAXSCANLINE_A][NUMSPOT_A];
short int     	snow[MAXSCANLINE_A][NUMSPOT_A];

/* AVN data */

short int	num_avn;
short int	avnhr[3];
short int	avnset[3];
float	    	windu_avn[3][NUMROW_AVN][NUMCOL_AVN];
float	    	windv_avn[3][NUMROW_AVN][NUMCOL_AVN];
float	    	ts2m_avn[3][NUMROW_AVN][NUMCOL_AVN];
float	    	ts_avn[3][NUMROW_AVN][NUMCOL_AVN];
float	    	tpw_avn[3][NUMROW_AVN][NUMCOL_AVN];

/* Misc. Data */

short int	FN_length_AMA;
short int	bad_at;
short int 	i_ssmi,j_ssmi;
int       	latbox_up[NUMSPOT_A];
int      	latbox_down[NUMSPOT_A];

float		A0[4];
float		A1[4];
float		A2[4];
float		A3[4];
float		A4[4];
float		A5[4];

float		D0[4];
float		D1[4];
float		D2[4];
float		D3[4];
float		D4[4];
float		D5[4];

/* Structures */

SCANLINE1   scanline1[MAXSCANLINE_A];
SCANLINE2   scanline2[MAXSCANLINE_A];
SCANLINE3   scanline3[MAXSCANLINE_A];

Sys_Time    Sys_times;
Limit_A     limit_A;

HBLOCK1     hblock1;
HBLOCK3     hblock3;
HBLOCK4     hblock4;

GEO_STIME   geo_scantime[MAXSCANLINE_A];
