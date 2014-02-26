/***********************************************************************        
 *  Header file name  :  CONSTANTS.h     
 *  Function          :  This is the constants header file 
 ***********************************************************************/ 

typedef short int     TOTNUM_OF_SCAN;
typedef short int     FILE_LENGTH;

/*-------------------------------------------------
 * Orbital part constants for AMSUA:
 *-------------------------------------------------*/
#define  REC_LENGTH_A    3679

#define  NUMCHAN_A         15
#define  NUMSPOT_A         30

#define  MAXSCANLINE_A    950
#define  MAXPCENT          33

#define  PARAMA             0
#define  AMA                1

#define SWATH_NAME_A    "AMSUA_Swath"

/*-------------------------------------------------
 * Orbital part constants for AMSUB:
 *-------------------------------------------------*/
#define  REC_LENGTH_B    3496

#define  NUMCHAN_B          5
#define  NUMSPOT_B         90

#define  MAXSCANLINE_B   3000
#define  MAXPCENT          33

#define  PARAMB             3
#define  AMB                4

#define SWATH_NAME_B    "AMSUB_Swath"

#define  HDF_FNL          120
#define  BUFR_FNL          39

/*-------------------------------------------------
 * Orbital part constants for MHS:
 *-------------------------------------------------*/
#define  REC_LENGTH_M    3630

#define  NUMCHAN_M          5
#define  NUMSPOT_M         90

#define  MAXSCANLINE_M   3000
#define  MAXPCENT          33

#define  PARAMM             3
#define  AMM                4

#define SWATH_NAME_M    "MHS_Swath"

/*-------------------------------------------------
 * Constant from Terrain/Elevation part
 *-------------------------------------------------*/
#define  NUMX_ELEV      4320
#define  NUMY_ELEV      2160    

/*-------------------------------------------------
 * Flag constants for both AMSUA and MHS 
 *-------------------------------------------------*/
#define  FILLER          -1
#define  BAD_FLAG      -1.0
#define  QC_FLAG       -999

#define  INDETERM_SFR_NONCONV  -15
#define  INDETERM_MULTI_YEAR_ICE -14
#define  INDETERM_FROZEN -13
#define  INDETERM_ELEV   -12
#define  INDETERM_DESERT -11
#define  INDETERM        -10
#define  INDETERM_COAST  -9
#define  INDETERM_SICE   -8
#define  INDETERM_SNOW   -7
#define  INDETERM_RAIN   -6
#define  INDETERM_CLW    -5
#define  BELOW_BT        -4
#define  OVER_BT         -3
#define  BELOW_PROD      -2
#define  OVER_PROD       -1

#define LAND_SEA_CRITERIA 50

/*-------------------------------------------------
 * Land-sea mask related constants 
 *-------------------------------------------------*/
#define MAP_ROWS_SNOW   1080
#define MAP_COLS_SNOW   2160
#define MAP_ROWS        2880
#define MAP_COLS        5760
#define MASK_RESOLUTION 16
#define MASK_RESOLUTION_SNOW 6

/*-------------------------------------------------
 * OPF file name length
 *-------------------------------------------------*/
#define  OPF_FNLENGTH    51

/**************************************************/
/* Mapping part constants: */

#define     GRID_NAME1       "AMSUB_NH_Grid"
#define     GRID_NAME2       "AMSUB_SH_Grid"

/*-------------------------------------------------
 * grid box size and polar stereographic size
 *-------------------------------------------------*/
#define HLAT       360
#define HLON       720
#define WLAT       180
#define WLON       360
#define DIST_GRID1  5
#define DIST_GRID2  19

#define MESH           1024 
#define GRID           64.0/MESH

#define  MESH_LAT       720
#define  MESH_LON       1440

#define  NUM_HEADER     7
#define  NUM_PROD       7

#define  ANCI_MISSING       0 
#define  MISSING          -99
#define  MISSING_CHAR     255 
#define  MISSING_GEO     -999 
#define  PSMISSING      -9999

#define TEMP_UPPER      285
#define PECENT_ICE      0.0

#define SFR_SCAL        100.0
#define SWE_SCAL        100.0
#define CLW_SCAL        100.0
#define TPW_SCAL        10.0
#define RR_SCAL         10.0
#define SNOW_SCAL       1.0
#define SICE_SCAL       1.0
#define AT_SCAL         100.0
#define IWP_SCAL	100.0
#define SCALER          100

/* #define  PI             3.141593 */
#define  PI             3.141592653589793238
#define  REARTH         6371.
#define  RSAT           833. 


/*********************************************
* GDAS constants
**********************************************/
#define NUMROW_GDAS     181
#define NUMCOL_GDAS     360

/*********************************************
* For snowfall rate computation
 **********************************************/
 #define  RTMCHAN         6
 #define  MAX_TB         79

/*********************************************
* AVN constants
**********************************************/
#define NUMROW_AVN      181
#define NUMCOL_AVN      360

/*********************************************
* Misc. 
**********************************************/   
#define  SCAN_ANG_M      1.1
#define  SALINITY	35.5
#define  T_CLOUD_LAYER   2.0
#define  SWATH_FLENGTH    43
/* #define  SWATH_FLENGTH_A  48 */
/* #define  SWATH_FLENGTH_M  46 */
#define  LATLIM1_M        15
#define  LATLIM2_M        30
#define  ICE_DEN       920.0
#define  ZERO_DEGREE  273.15
#define  COLD_SNOW_LO  243.0
#define  COLD_SNOW_HI  245.0
#define  SWE_LAT_LIMIT -60.0
