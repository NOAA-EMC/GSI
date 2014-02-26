/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the relevant data 
 *  Called by          :  MHS2HDF.c
 ***************************************************************/
/* Time */

extern short int   	year[MAXSCANLINE_M];
extern short int   	doy[MAXSCANLINE_M];
extern char        	month[MAXSCANLINE_M];
extern char        	dom[MAXSCANLINE_M];
extern char        	hour[MAXSCANLINE_M];
extern char        	minute[MAXSCANLINE_M];
extern char        	second[MAXSCANLINE_M];
extern double    	time_tai93[MAXSCANLINE_M];

/* Ancillary Data */

extern char 	    	mask[MAP_ROWS][MAP_COLS];
extern char		qc_flag[MAXSCANLINE_M];
extern char        	orb_mode[MAXSCANLINE_M];
extern char		stype[MAXSCANLINE_M][NUMSPOT_M];

extern float		lat[MAXSCANLINE_M][NUMSPOT_M];
extern float		lon[MAXSCANLINE_M][NUMSPOT_M];
extern float		lza[MAXSCANLINE_M][NUMSPOT_M];
extern float		sza[MAXSCANLINE_M][NUMSPOT_M];

extern float            t_r_central_wave_number[NUMCHAN_M];

/* Antenna Temperature etc. */

extern long int   	count[NUMCHAN_M][NUMSPOT_M];
extern short int	corr_org[NUMCHAN_M][NUMSPOT_M][4];
extern short int	corr_mys[NUMCHAN_M][NUMSPOT_M][3];

extern float 		at[MAXSCANLINE_M][NUMSPOT_M][NUMCHAN_M];
extern double		rat[NUMSPOT_M];
extern double		rad[NUMSPOT_M];
extern double           clcoef[NUMCHAN_M][3];
extern double		calrad[NUMCHAN_M][NUMSPOT_M];

/* Mis. Data */

extern short int	FN_length_MHS;
extern short int	bad_at;
extern int       	latbox_up[NUMSPOT_M];
extern int      	latbox_down[NUMSPOT_M];

/* Structures */

extern SCANLINE   	scanline[MAXSCANLINE_M];

extern Sys_Time    	Sys_times;
extern Limit_M     	limit_M;

extern HBLOCK     	hblock;

extern GEO_STIME   	geo_scantime[MAXSCANLINE_M];
