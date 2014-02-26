/*********************************************************************** 
 *  File name          :  SWATH.h
 *  Function           :  Definition of all the relevant data 
 *  Called by          : 
 ***************************************************************/

/* Time */

short int   	year[MAXSCAN];
short int   	doy[MAXSCAN];
short int       month[MAXSCAN];
short int       dom[MAXSCAN];
short int       hour[MAXSCAN];
short int       minute[MAXSCAN];
short int       second[MAXSCAN];

double      	time_utc[MAXSCAN];

/* Ancillary Data */

short int       iTypAtm[MAXSCAN][MAXFOV];
short int       iTypSfc[MAXSCAN][MAXFOV];
short int       orb_mode[MAXSCAN];

short int	qc[MAXSCAN][MAXFOV][4];
short int       qc_dep[MAXSCAN][MAXFOV][4];
short int	nattempt[MAXSCAN][MAXFOV];
short int	niter[MAXSCAN][MAXFOV];
float		chisq[MAXSCAN][MAXFOV];

float		lat[MAXSCAN][MAXFOV];
float		lon[MAXSCAN][MAXFOV];
float		angle[MAXSCAN][MAXFOV];
float		rel_azi[MAXSCAN][MAXFOV];
float		sza[MAXSCAN][MAXFOV]; 


/* Products */

float     	ptemp[MAXSCAN][MAXFOV][MAXLAY];
float     	pvapor[MAXSCAN][MAXFOV][MAXLAY];
float     	pozone[MAXSCAN][MAXFOV][MAXLAY];
float           pclw[MAXSCAN][MAXFOV][MAXLAY];
float           prain[MAXSCAN][MAXFOV][MAXLAY];
float           pgraupel[MAXSCAN][MAXFOV][MAXLAY];
float           psnow[MAXSCAN][MAXFOV][MAXLAY];
float           pice[MAXSCAN][MAXFOV][MAXLAY];

/* Surface products */

short int     	tskin[MAXSCAN][MAXFOV];
short int       surfp[MAXSCAN][MAXFOV];
short int       emis[MAXSCAN][MAXFOV][MAXCH];

/* Misc. Data */
short int     	nqc;
char            SWATH_NAME[20];
float           player[MAXLAY];
float           plevel[MAXLAY+1];
float		freq[MAXCH];
short int       polo[MAXCH];
float           fov_size[MAXFOV];
float           lat_size[MAXFOV];

short int       alg_sn;

short int   	bt[MAXSCAN][MAXFOV][MAXCH];
short int   	ym[MAXSCAN][MAXFOV][MAXCH];
short int   	yfwd[MAXSCAN][MAXFOV][MAXCH];
short int   	chanSel[MAXSCAN][MAXFOV][MAXCH];

/*DEP products *******************/
short int      	tpw[MAXSCAN][MAXFOV];
short int     	clw[MAXSCAN][MAXFOV];
short int       rwp[MAXSCAN][MAXFOV];
short int       lwp[MAXSCAN][MAXFOV];
short int       swp[MAXSCAN][MAXFOV];
short int       iwp[MAXSCAN][MAXFOV];
short int       gwp[MAXSCAN][MAXFOV];
short int     	rr[MAXSCAN][MAXFOV];
short int     	swe[MAXSCAN][MAXFOV];
short int       snowgs[MAXSCAN][MAXFOV];
short int     	snow[MAXSCAN][MAXFOV];
short int     	sice[MAXSCAN][MAXFOV];
short int     	sice_my[MAXSCAN][MAXFOV];
short int     	sice_fy[MAXSCAN][MAXFOV];

short int       sfr[MAXSCAN][MAXFOV];
short int       cldtop[MAXSCAN][MAXFOV];
short int       cldbase[MAXSCAN][MAXFOV];
short int       cldthick[MAXSCAN][MAXFOV];
short int       preciptype[MAXSCAN][MAXFOV];
short int     	rflag[MAXSCAN][MAXFOV];

short int     	surfm[MAXSCAN][MAXFOV];
short int     	windsp[MAXSCAN][MAXFOV];
short int     	winddir[MAXSCAN][MAXFOV];
short int     	windu[MAXSCAN][MAXFOV];
short int     	windv[MAXSCAN][MAXFOV];


/**************************************************
 * NPP ATMS Special parameters
 **************************************************/
float   	geospatial_first_scanline_first_fov_lat[1];
float 		geospatial_first_scanline_first_fov_lon[1];
float 		geospatial_first_scanline_last_fov_lat[1];
float 		geospatial_first_scanline_last_fov_lon[1];
float 		geospatial_last_scanline_first_fov_lat[1];
float 		geospatial_last_scanline_first_fov_lon[1];
float 		geospatial_last_scanline_last_fov_lat[1];
float 		geospatial_last_scanline_last_fov_lon[1];

int 		total_number_retrievals[1]; // nprf : total numer of profiles
float 		percentage_optimal_retrievals[1]; // QC=0
float 		percentage_suboptimal_retrievals[1]; // QC=1
float 		percentage_bad_retrievals[1];  // QC=2

char            time_coverage_start[21];
char            time_coverage_end[21];
//char          date_created[21];

char            EDR_name[256];
char            DEP_name[256];
char            RDR_name[256];
