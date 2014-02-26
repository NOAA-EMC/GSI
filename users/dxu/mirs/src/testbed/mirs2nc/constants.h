/***********************************************************************        
 *  File    name  :  constants.h
 *  Date          :  11/15/2006             
 *  Contains the constants used in orbital processing.
 *   
 ***********************************************************************/ 

/**************************************************
 *  Orbital part:
 *-------------------------------------------------
 *  lat/lon limits for SSM/I
 *-------------------------------------------------*/
#define LAT_LOWLIMIT   0
#define LAT_HILIMIT    18000
#define LON_LOWLIMIT   0
#define LON_HILIMIT    36000

/*-------------------------------------------------
 * File name
 *-------------------------------------------------*/
/*#define SWATH_NAME        "MIRS_Swath"*/

/*-------------------------------------------------
 * maximum scanline, view and channel
 *-------------------------------------------------*/
#define MAXSCAN        8000
#define MAXFOV         208
#define MAXCH          24
#define MAX_ORB        30
#define MAXLAY         100

/*-------------------------------------------------
 * Parameters for different files
 *-------------------------------------------------*/
#define PARM           0
#define SDR            1
#define EDR            2
#define SHDF           3
#define EHDF           4

/*-------------------------------------------------
 * flags
 *-------------------------------------------------*/
#define FILLER         -1 
#define BT_SCALE       100
#define SCALE          100

/**************************************************
 * Mapping part constants:
 *-------------------------------------------------
 * grid box size and polar stereographic size
 *-------------------------------------------------*/

#define  GRID_LAT       360
#define  GRID_LON       720
#define  MESH           512
#define  GRID           64.0/MESH
#define  MAXPNUM        3
#define  MISSING        -999
#define  ANCI_MISSING   0 
#define  PSMISSING      -9999
#define  NAVA           -888

/**************************************************
 * New scaling constants that matches AMSU products
 **************************************************/

#define TEMP_SCAL      100
#define TPW_SCAL       10.
#define CLW_SCAL       100.
#define RWP_SCAL       100.
#define LWP_SCAL       100.
#define SWP_SCAL       100.
#define IWP_SCAL       100.
#define GWP_SCAL       100.
#define RR_SCAL        10.

#define SWE_SCAL       100.
#define SNOWGS_SCAL    100.
#define SNOW_SCAL      1.
#define STEMP_SCAL     10.
#define SICE_SCAL      1.

#define TSKIN_SCAL     100.
#define SURFP_SCAL     10.
#define EMIS_SCAL      10000.

#define SFR_SCAL        100.
#define CLDTOP_SCAL     10.
#define CLDBASE_SCAL    10.
#define CLDTHICK_SCAL   10.
#define PRECIPTYPE_SCAL 1.
#define RFLAG_SCAL      1.

#define SURFM_SCAL      10.
#define WINDSP_SCAL     100.
#define WINDDIR_SCAL    100.
#define WINDU_SCAL      100.
#define WINDV_SCAL      100.
