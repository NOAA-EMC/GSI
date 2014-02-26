/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the data arrays
 *  Called by          :  AMB2HDF.c
 ***************************************************************/
/* Time */

extern short int   	year[MAXSCANLINE_B];
extern short int   	doy[MAXSCANLINE_B];
extern char        	month[MAXSCANLINE_B];
extern char        	dom[MAXSCANLINE_B];
extern char        	hour[MAXSCANLINE_B];
extern char        	minute[MAXSCANLINE_B];
extern char        	second[MAXSCANLINE_B];
extern float64    	time_tai93[MAXSCANLINE_B];

/* Ancillary Data */

extern char 	    	mask[MAP_ROWS][MAP_COLS];
extern char		qc_flag[MAXSCANLINE_B];
extern char        	orb_mode[MAXSCANLINE_B];
extern char		stype[MAXSCANLINE_B][NUMSPOT_B];

extern float		lat[MAXSCANLINE_B][NUMSPOT_B];
extern float		lon[MAXSCANLINE_B][NUMSPOT_B];
extern float		lza[MAXSCANLINE_B][NUMSPOT_B];
extern float		sza[MAXSCANLINE_B][NUMSPOT_B];

/* Antenna Temperature etc. */

extern long int   	count[NUMCHAN_B][NUMSPOT_B];
extern short int	corr_org[NUMCHAN_B][NUMSPOT_B][4];
extern short int	corr_mys[NUMCHAN_B][NUMSPOT_B][3];

extern float 		at[MAXSCANLINE_B][NUMSPOT_B][NUMCHAN_B];
extern float          	rat[NUMSPOT_B];
extern float          	rad[NUMSPOT_B];
extern float       	clcoef[NUMCHAN_B][3];
extern float		calrad[NUMCHAN_B][NUMSPOT_B];

/* Mis. Data */

extern short int	FN_length_AMB;
extern short int	bad_at;
extern int       	latbox_up[NUMSPOT_B];
extern int      	latbox_down[NUMSPOT_B];

/* Structures */

extern SCANLINE1   	scanline1[MAXSCANLINE_B];
extern SCANLINE2   	scanline2[MAXSCANLINE_B];
extern SCANLINE3   	scanline3[MAXSCANLINE_B];
extern SCANLINE5   	scanline5[MAXSCANLINE_B];

extern Sys_Time    	Sys_times;
extern Limit_B     	limit_B;

extern HBLOCK1     	hblock1;
extern HBLOCK3     	hblock3;
extern HBLOCK4     	hblock4;

extern GEO_STIME   	geo_scantime[MAXSCANLINE_B];
