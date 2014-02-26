/*********************************************************************** 
 *  File name          :  ESWATH.h
 *  Function           :  Definition of all the relevant data 
 *  Called by          :  Subroutines 
 ***************************************************************/
/* Time */

extern short int   	year[MAXSCANLINE_A];
extern short int   	doy[MAXSCANLINE_A];
extern char        	month[MAXSCANLINE_A];
extern char        	dom[MAXSCANLINE_A];
extern char        	hour[MAXSCANLINE_A];
extern char        	minute[MAXSCANLINE_A];
extern char        	second[MAXSCANLINE_A];
extern double    	time_tai93[MAXSCANLINE_A];

/* Ancillary Data */

extern char 	    	mask[MAP_ROWS][MAP_COLS];
extern char		qc_flag[MAXSCANLINE_A];
extern char        	orb_mode[MAXSCANLINE_A];
extern char		stype[MAXSCANLINE_A][NUMSPOT_A];

extern float		lat[MAXSCANLINE_A][NUMSPOT_A];
extern float		lon[MAXSCANLINE_A][NUMSPOT_A];
extern float		lza[MAXSCANLINE_A][NUMSPOT_A];
extern float		sza[MAXSCANLINE_A][NUMSPOT_A];

extern float            t_r_central_wave_number[NUMCHAN_A];

/* Antenna Temperature etc. */

extern unsigned short   count[NUMCHAN_A][NUMSPOT_A];

extern float 		at[MAXSCANLINE_A][NUMSPOT_A][NUMCHAN_A];
extern float          	rat[NUMSPOT_A];
extern float          	rad[NUMSPOT_A];
extern float       	clcoef[NUMCHAN_A][3];
extern float		calrad[NUMCHAN_A][NUMSPOT_A];

/* Products */

extern short int     	clw[MAXSCANLINE_A][NUMSPOT_A];
extern short int      	tpw[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	sice[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	ts[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	em1[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	em2[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	em3[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	rr[MAXSCANLINE_A][NUMSPOT_A];
extern short int     	snow[MAXSCANLINE_A][NUMSPOT_A];

/* AVN data */

extern short int	num_avn;
extern short int	avnhr[3];
extern short int	avnset[3];
extern float	    	windu_avn[3][NUMROW_AVN][NUMCOL_AVN];
extern float	    	windv_avn[3][NUMROW_AVN][NUMCOL_AVN];
extern float	    	ts2m_avn[3][NUMROW_AVN][NUMCOL_AVN];
extern float	    	ts_avn[3][NUMROW_AVN][NUMCOL_AVN];
extern float	    	tpw_avn[3][NUMROW_AVN][NUMCOL_AVN];

/* Misc. Data */

extern short int	FN_length_AMA;
extern short int	bad_at;
extern int       	latbox_up[NUMSPOT_A];
extern int      	latbox_down[NUMSPOT_A];

extern float            A0[4];
extern float            A1[4];
extern float            A2[4];
extern float            A3[4];
extern float            A4[4];
extern float            A5[4];

extern float            D0[4];
extern float            D1[4];
extern float            D2[4];
extern float            D3[4];
extern float            D4[4];
extern float            D5[4];

/* Structures */

extern SCANLINE    scanline[MAXSCANLINE_A];

extern Sys_Time    Sys_times;
extern Limit_A     limit_A;

extern HBLOCK      hblock;

extern GEO_STIME   geo_scantime[MAXSCANLINE_A];
