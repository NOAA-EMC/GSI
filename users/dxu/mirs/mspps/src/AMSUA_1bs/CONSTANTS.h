/***********************************************************************        
 *  Header file name  :  CONSTANTS.h     
 *  Function          :  This is the constants header file 
 ***********************************************************************/ 

typedef short int     TOTNUM_OF_SCAN;
typedef short int     FILE_LENGTH;

/**************************************************
 * Orbital part constants for AMSUA:
 **************************************************/
#define  REC_LENGTH_A    3679

#define  NUMCHAN_A         15
#define  NUMSPOT_A         30

#define  MAXSCANLINE_A    950 
#define  MAXPCENT          33

#define  PARAMA             0
#define  AMA                1

#define SWATH_NAME_A    "AMSUA_Swath"

#define SCAN_ANG_A	  3.3

#define PI                3.141592653589793238
/* #define  PI               3.141593 */
#define  REARTH           6371
#define  RSAT             833

/**************************************************
 * Orbital part constants for AMSUB:
 **************************************************/
#define  REC_LENGTH_B    3451

#define  NUMCHAN_B          5
#define  NUMSPOT_B         90

#define  MAXSCANLINE_B   3000

#define  PARAMB             3
#define  AMB                4

#define SWATH_NAME_B    "AMSUB_Swath"

#define  SSMI_LAT	  180
#define  SSMI_LON	  360

/*-------------------------------------------------
 * Flag constants for both AMSUA and AMSUB
 *-------------------------------------------------*/
#define  FILLER          -1
#define  BAD_FLAG      -1.0
#define  QC_FLAG       -999

#define  INDETERM_ELEV   -12
#define  INDETERM_DESERT -11
#define  INDETERM        -10
#define  INDETERM_COAST  -9
#define  INDETERM_SICE   -8
#define  INDETERM_SNOW   -7
#define  INDETERM_RAIN   -6
#define  INDETERM_CLW    -5
#define  BELOW_AT        -4
#define  OVER_AT         -3
#define  BELOW_PROD      -2
#define  OVER_PROD       -1

#define LAND_SEA_CRITERIA 50
/*-------------------------------------------------
 * OPF file name length
 *-------------------------------------------------*/
#define  OPF_FNLENGTH    51

/*-------------------------------------------------
 * swath file name length
 *-------------------------------------------------*/
#define  HDF_FNL 	120

/*-------------------------------------------------
 * binary file name length
 *-------------------------------------------------*/
#define  BUFR_FNL 	120

/**************************************************
 * Mapping part constants:
 *-------------------------------------------------
 * grid box size and polar stereographic size
 *-------------------------------------------------*/
#define HLAT       360
#define HLON       720
#define WLAT       180
#define WLON       360
#define MAP_ROWS   2880
#define MAP_COLS   5760
#define MASK_RESOLUTION  16
#define DIST_GRID1  5
#define DIST_GRID2  19

#define MESH           512
#define GRID           64.0/MESH

#define  MESH_LAT       360
#define  MESH_LON       720

#define  NUM_HEADER    11 
#define  NUM_PROD      11 

#define  GRID_RESOLUTION        0.5

#define  ANCI_MISSING       0 
#define  MISSING          -99
#define  PSMISSING      -9999
#define  MISSING_CHAR     255 

#define TEMP_UPPER      285
#define PECENT_ICE      0.0

#define CLW_SCAL        100.0
#define TPW_SCAL        10.0
#define RR_SCAL         10.0
#define AT_SCAL         100.0
#define TS_SCAL         100.0
#define EM_SCAL         100.0
#define SNOW_SCAL       1.0
#define SICE_SCAL       1.0
#define SCALER         100

/*********************************************
* GDAS constants
**********************************************/
#define NUMROW_GDAS     181
#define NUMCOL_GDAS     360

/*********************************************
* AVN constants
**********************************************/
#define NUMROW_AVN      181
#define NUMCOL_AVN      360
