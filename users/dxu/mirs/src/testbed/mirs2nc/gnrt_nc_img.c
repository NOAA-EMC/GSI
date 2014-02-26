/***************************************************************************
 *  Program Name      : gnrt_nc_img.c
 *  Type              : Subroutine
 *  Function          : Program creates netcdf image file 
 *  Input Files       : 
 *  Output Files      : Netcdf files
 *  Subroutine Called : 
 *  Called by         : set_nc_img.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *  
 *  12/2008         v0              Jiang Zhao
 *  07/2011         v1              Wanchun Chen
 *************************************************************************/
#include "defaults.h"
#include "constants.h"
#include "swath.h"
#include <netcdf.h>
#include "npp_header.h"
#include <time.h>
#include <unistd.h>


extern void check_err(const int stat, const int line, const char *file, int err_id);


int gnrt_nc_img(char * fname_nc_img, short int nscan, short int nspot, short int  nchan, char *satid) {

   int  stat;			/* return status */
   int  ncid;			/* netCDF id */
   int  errid = 17;

   /* dimension ids */
   int Scanline_dim;
   int Field_of_view_dim;
   int P_Layer_dim;
   int Channel_dim;
   int Qc_dim_dim;

   /* dimension lengths */
   size_t Scanline_len = nscan;
   size_t Field_of_view_len = nspot;
   size_t P_Layer_len = 100;
   size_t Channel_len = nchan;
   size_t Qc_dim_len = 4;

   /* variable ids */
   int ScanTime_year_id;
   int ScanTime_doy_id;
   int ScanTime_month_id;
   int ScanTime_dom_id;
   int ScanTime_hour_id;
   int ScanTime_minute_id;
   int ScanTime_second_id;
   int ScanTime_UTC_id;
   int Orb_mode_id;
   int Latitude_id;
   int Longitude_id;
   int Sfc_type_id;
   int Atm_type_id;
   int Qc_id;
   //int NAttempt_id;
   //int NIter_id;
   int ChiSqr_id;
   int LZ_angle_id;
   int RAzi_angle_id;
   int SZ_angle_id;
   int BT_id;
   int YM_id;
   //int YFWD_id;
   int ChanSel_id;
   int TPW_id;
   int CLW_id;
   int RWP_id;
   int LWP_id;
   int SWP_id;
   int IWP_id;
   int GWP_id;
   int RR_id;
   int Snow_id;
   int SWE_id;
   int SnowGS_id;
   int SIce_id;
   int SIce_MY_id;
   int SIce_FY_id;
   int TSkin_id;
   int SurfP_id;
   int Emis_id;
   int Freq_id;
   int Polo_id;

   int SFR_id;
   int CldTop_id;
   int CldBase_id;
   int CldThick_id;
   int PrecipType_id;
   int RFlag_id;

   int SurfM_id;
   int WindSp_id;
   int WindDir_id;
   int WindU_id;
   int WindV_id;

/* rank (number of dimensions) for each variable */
#  define RANK_ScanTime_year 1
#  define RANK_ScanTime_doy 1
#  define RANK_ScanTime_month 1
#  define RANK_ScanTime_dom 1
#  define RANK_ScanTime_hour 1
#  define RANK_ScanTime_minute 1
#  define RANK_ScanTime_second 1
#  define RANK_ScanTime_UTC 1
#  define RANK_Orb_mode 1
#  define RANK_Latitude 2
#  define RANK_Longitude 2
#  define RANK_Sfc_type 2
#  define RANK_Atm_type 2
#  define RANK_Qc 3
// #  define RANK_NAttempt 2
// #  define RANK_NIter 2
#  define RANK_ChiSqr 2
#  define RANK_LZ_angle 2
#  define RANK_RAzi_angle 2
#  define RANK_SZ_angle 2
#  define RANK_BT 3
#  define RANK_YM 3
#  define RANK_YFWD 3
#  define RANK_ChanSel 3
#  define RANK_TPW 2
#  define RANK_CLW 2
#  define RANK_RWP 2
#  define RANK_LWP 2
#  define RANK_SWP 2
#  define RANK_IWP 2
#  define RANK_GWP 2
#  define RANK_RR 2
#  define RANK_Snow 2
#  define RANK_SWE 2
#  define RANK_SnowGS 2
#  define RANK_SIce 2
#  define RANK_SIce_MY 2
#  define RANK_SIce_FY 2
#  define RANK_TSkin 2
#  define RANK_SurfP 2
#  define RANK_Emis 3
#  define RANK_Freq 1
#  define RANK_Polo 1

#  define RANK_SFR 2
#  define RANK_CldTop 2
#  define RANK_CldBase 2
#  define RANK_CldThick 2
#  define RANK_PrecipType 2
#  define RANK_RFlag 2

#  define RANK_SurfM 2
#  define RANK_WindSp 2
#  define RANK_WindDir 2
#  define RANK_WindU 2
#  define RANK_WindV 2


   /* variable shapes */
   int ScanTime_year_dims[RANK_ScanTime_year];
   int ScanTime_doy_dims[RANK_ScanTime_doy];
   int ScanTime_month_dims[RANK_ScanTime_month];
   int ScanTime_dom_dims[RANK_ScanTime_dom];
   int ScanTime_hour_dims[RANK_ScanTime_hour];
   int ScanTime_minute_dims[RANK_ScanTime_minute];
   int ScanTime_second_dims[RANK_ScanTime_second];
   int ScanTime_UTC_dims[RANK_ScanTime_UTC];
   int Orb_mode_dims[RANK_Orb_mode];
   int Latitude_dims[RANK_Latitude];
   int Longitude_dims[RANK_Longitude];
   int Sfc_type_dims[RANK_Sfc_type];
   int Atm_type_dims[RANK_Atm_type];
   int Qc_dims[RANK_Qc];
   //int NAttempt_dims[RANK_NAttempt];
   //int NIter_dims[RANK_NIter];
   int ChiSqr_dims[RANK_ChiSqr];
   int LZ_angle_dims[RANK_LZ_angle];
   int RAzi_angle_dims[RANK_RAzi_angle];
   int SZ_angle_dims[RANK_SZ_angle];
   int BT_dims[RANK_BT];
   int YM_dims[RANK_YM];
   //int YFWD_dims[RANK_YFWD];
   int ChanSel_dims[RANK_ChanSel];
   int TPW_dims[RANK_TPW];
   int CLW_dims[RANK_CLW];
   int RWP_dims[RANK_RWP];
   int LWP_dims[RANK_LWP];
   int SWP_dims[RANK_SWP];
   int IWP_dims[RANK_IWP];
   int GWP_dims[RANK_GWP];
   int RR_dims[RANK_RR];
   int Snow_dims[RANK_Snow];
   int SWE_dims[RANK_SWE];
   int SnowGS_dims[RANK_SnowGS];
   int SIce_dims[RANK_SIce];
   int SIce_MY_dims[RANK_SIce_MY];
   int SIce_FY_dims[RANK_SIce_FY];
   int TSkin_dims[RANK_TSkin];
   int SurfP_dims[RANK_SurfP];
   int Emis_dims[RANK_Emis];
   int Freq_dims[RANK_Freq];
   int Polo_dims[RANK_Polo];

   int SFR_dims[RANK_SFR];
   int CldTop_dims[RANK_CldTop];
   int CldBase_dims[RANK_CldBase];
   int CldThick_dims[RANK_CldThick];
   int PrecipType_dims[RANK_PrecipType];
   int RFlag_dims[RANK_RFlag];

   int SurfM_dims[RANK_SurfM];
   int WindSp_dims[RANK_WindSp];
   int WindDir_dims[RANK_WindDir];
   int WindU_dims[RANK_WindU];
   int WindV_dims[RANK_WindV];


   /* attribute vectors */
   double BT_scale[1];
   double YM_scale[1];
   //double YFWD_scale[1];
   int ChanSel_scale[1];
   double TPW_scale[1];
   double CLW_scale[1];
   double RWP_scale[1];
   double LWP_scale[1];
   double SWP_scale[1];
   double IWP_scale[1];
   double GWP_scale[1];
   double RR_scale[1];
   int Snow_scale[1];
   int SWE_scale[1];
   int SnowGS_scale[1];
   int SIce_scale[1];
   int SIce_MY_scale[1];
   int SIce_FY_scale[1];
   int TSkin_scale[1];
   double SurfP_scale[1];
   double Emis_scale[1];
   int cdf_missing_value[1];
   double cdf_version[1];
   int alg_version[1];

   double SFR_scale[1];
   double CldTop_scale[1];
   double CldBase_scale[1];
   double CldThick_scale[1];
   int PrecipType_scale[1];
   int RFlag_scale[1];

   double SurfM_scale[1];
   double WindSp_scale[1];
   double WindDir_scale[1];
   double WindU_scale[1];
   double WindV_scale[1];

   time_t rawtime;
   struct tm * timeinfo;
   char date_created_img[21];
  
   unsigned int random_number;
   char char_random_number[17]; 
   char hostname[256];
   unsigned int iseed;
  
   char uuid[1024];

   /* enter define mode */
   stat = nc_create(fname_nc_img, NC_CLOBBER|NC_NETCDF4, &ncid);
   check_err(stat,__LINE__,__FILE__,errid);
   
   /* creation time */
   time ( &rawtime );
   //timeinfo = localtime ( &rawtime );
   timeinfo = gmtime ( &rawtime );
   sprintf(date_created_img,"%4d-%02d-%02dT%02d:%02d:%02dZ",
     timeinfo->tm_year+1900,timeinfo->tm_mon+1,timeinfo->tm_mday,timeinfo->tm_hour,timeinfo->tm_min,timeinfo->tm_sec);


   /* define dimensions */
   stat = nc_def_dim(ncid, "Scanline", Scanline_len, &Scanline_dim);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_def_dim(ncid, "Field_of_view", Field_of_view_len, &Field_of_view_dim);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_def_dim(ncid, "P_Layer", P_Layer_len, &P_Layer_dim);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_def_dim(ncid, "Channel", Channel_len, &Channel_dim);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_def_dim(ncid, "Qc_dim", Qc_dim_len, &Qc_dim_dim);
   check_err(stat,__LINE__,__FILE__,errid);

   /* define variables */
   Freq_dims[0] = Channel_dim;
   stat = nc_def_var(ncid, "Freq", NC_FLOAT, RANK_Freq, Freq_dims, &Freq_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Polo_dims[0] = Channel_dim;
   stat = nc_def_var(ncid, "Polo", NC_SHORT, RANK_Polo, Polo_dims, &Polo_id);
   check_err(stat,__LINE__,__FILE__,errid);


   ScanTime_year_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_year", NC_SHORT, RANK_ScanTime_year, ScanTime_year_dims, &ScanTime_year_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_doy_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_doy", NC_SHORT, RANK_ScanTime_doy, ScanTime_doy_dims, &ScanTime_doy_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_month_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_month", NC_SHORT, RANK_ScanTime_month, ScanTime_month_dims, &ScanTime_month_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_dom_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_dom", NC_SHORT, RANK_ScanTime_dom, ScanTime_dom_dims, &ScanTime_dom_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_hour_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_hour", NC_SHORT, RANK_ScanTime_hour, ScanTime_hour_dims, &ScanTime_hour_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_minute_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_minute", NC_SHORT, RANK_ScanTime_minute, ScanTime_minute_dims, &ScanTime_minute_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_second_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_second", NC_SHORT, RANK_ScanTime_second, ScanTime_second_dims, &ScanTime_second_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ScanTime_UTC_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "ScanTime_UTC", NC_DOUBLE, RANK_ScanTime_UTC, ScanTime_UTC_dims, &ScanTime_UTC_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Orb_mode_dims[0] = Scanline_dim;
   stat = nc_def_var(ncid, "Orb_mode", NC_SHORT, RANK_Orb_mode, Orb_mode_dims, &Orb_mode_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Latitude_dims[0] = Scanline_dim;
   Latitude_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "Latitude", NC_FLOAT, RANK_Latitude, Latitude_dims, &Latitude_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Longitude_dims[0] = Scanline_dim;
   Longitude_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "Longitude", NC_FLOAT, RANK_Longitude, Longitude_dims, &Longitude_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Sfc_type_dims[0] = Scanline_dim;
   Sfc_type_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "Sfc_type", NC_SHORT, RANK_Sfc_type, Sfc_type_dims, &Sfc_type_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Atm_type_dims[0] = Scanline_dim;
   Atm_type_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "Atm_type", NC_SHORT, RANK_Atm_type, Atm_type_dims, &Atm_type_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Qc_dims[0] = Scanline_dim;
   Qc_dims[1] = Field_of_view_dim;
   Qc_dims[2] = Qc_dim_dim;
   stat = nc_def_var(ncid, "Qc", NC_SHORT, RANK_Qc, Qc_dims, &Qc_id);
   check_err(stat,__LINE__,__FILE__,errid);

   ChiSqr_dims[0] = Scanline_dim;
   ChiSqr_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "ChiSqr", NC_FLOAT, RANK_ChiSqr, ChiSqr_dims, &ChiSqr_id);
   check_err(stat,__LINE__,__FILE__,errid);

/*
   NAttempt_dims[0] = Scanline_dim;
   NAttempt_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "NAttempt", NC_SHORT, RANK_NAttempt, NAttempt_dims, &NAttempt_id);
   check_err(stat,__LINE__,__FILE__,errid);

   NIter_dims[0] = Scanline_dim;
   NIter_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "NIter", NC_SHORT, RANK_NIter, NIter_dims, &NIter_id);
   check_err(stat,__LINE__,__FILE__,errid);
*/

   LZ_angle_dims[0] = Scanline_dim;
   LZ_angle_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "LZ_angle", NC_FLOAT, RANK_LZ_angle, LZ_angle_dims, &LZ_angle_id);
   check_err(stat,__LINE__,__FILE__,errid);

   RAzi_angle_dims[0] = Scanline_dim;
   RAzi_angle_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "RAzi_angle", NC_FLOAT, RANK_RAzi_angle, RAzi_angle_dims, &RAzi_angle_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SZ_angle_dims[0] = Scanline_dim;
   SZ_angle_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SZ_angle", NC_FLOAT, RANK_SZ_angle, SZ_angle_dims, &SZ_angle_id);
   check_err(stat,__LINE__,__FILE__,errid);

   BT_dims[0] = Scanline_dim;
   BT_dims[1] = Field_of_view_dim;
   BT_dims[2] = Channel_dim;
   stat = nc_def_var(ncid, "BT", NC_SHORT, RANK_BT, BT_dims, &BT_id);
   check_err(stat,__LINE__,__FILE__,errid);

   YM_dims[0] = Scanline_dim;
   YM_dims[1] = Field_of_view_dim;
   YM_dims[2] = Channel_dim;
   stat = nc_def_var(ncid, "YM", NC_SHORT, RANK_YM, YM_dims, &YM_id);
   check_err(stat,__LINE__,__FILE__,errid);

/*
   YFWD_dims[0] = Scanline_dim;
   YFWD_dims[1] = Field_of_view_dim;
   YFWD_dims[2] = Channel_dim;
   stat = nc_def_var(ncid, "YFWD", NC_SHORT, RANK_YFWD, YFWD_dims, &YFWD_id);
   check_err(stat,__LINE__,__FILE__,errid);
*/

   ChanSel_dims[0] = Scanline_dim;
   ChanSel_dims[1] = Field_of_view_dim;
   ChanSel_dims[2] = Channel_dim;
   stat = nc_def_var(ncid, "ChanSel", NC_SHORT, RANK_ChanSel, ChanSel_dims, &ChanSel_id);
   check_err(stat,__LINE__,__FILE__,errid);

   TPW_dims[0] = Scanline_dim;
   TPW_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "TPW", NC_SHORT, RANK_TPW, TPW_dims, &TPW_id);
   check_err(stat,__LINE__,__FILE__,errid);

   CLW_dims[0] = Scanline_dim;
   CLW_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "CLW", NC_SHORT, RANK_CLW, CLW_dims, &CLW_id);
   check_err(stat,__LINE__,__FILE__,errid);

   RWP_dims[0] = Scanline_dim;
   RWP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "RWP", NC_SHORT, RANK_RWP, RWP_dims, &RWP_id);
   check_err(stat,__LINE__,__FILE__,errid);

   LWP_dims[0] = Scanline_dim;
   LWP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "LWP", NC_SHORT, RANK_LWP, LWP_dims, &LWP_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SWP_dims[0] = Scanline_dim;
   SWP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SWP", NC_SHORT, RANK_SWP, SWP_dims, &SWP_id);
   check_err(stat,__LINE__,__FILE__,errid);

   IWP_dims[0] = Scanline_dim;
   IWP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "IWP", NC_SHORT, RANK_IWP, IWP_dims, &IWP_id);
   check_err(stat,__LINE__,__FILE__,errid);

   GWP_dims[0] = Scanline_dim;
   GWP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "GWP", NC_SHORT, RANK_GWP, GWP_dims, &GWP_id);
   check_err(stat,__LINE__,__FILE__,errid);

   RR_dims[0] = Scanline_dim;
   RR_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "RR", NC_SHORT, RANK_RR, RR_dims, &RR_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Snow_dims[0] = Scanline_dim;
   Snow_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "Snow", NC_SHORT, RANK_Snow, Snow_dims, &Snow_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SWE_dims[0] = Scanline_dim;
   SWE_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SWE", NC_SHORT, RANK_SWE, SWE_dims, &SWE_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SnowGS_dims[0] = Scanline_dim;
   SnowGS_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SnowGS", NC_SHORT, RANK_SnowGS, SnowGS_dims, &SnowGS_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SIce_dims[0] = Scanline_dim;
   SIce_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SIce", NC_SHORT, RANK_SIce, SIce_dims, &SIce_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SIce_MY_dims[0] = Scanline_dim;
   SIce_MY_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SIce_MY", NC_SHORT, RANK_SIce_MY, SIce_MY_dims, &SIce_MY_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SIce_FY_dims[0] = Scanline_dim;
   SIce_FY_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SIce_FY", NC_SHORT, RANK_SIce_FY, SIce_FY_dims, &SIce_FY_id);
   check_err(stat,__LINE__,__FILE__,errid);

   TSkin_dims[0] = Scanline_dim;
   TSkin_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "TSkin", NC_SHORT, RANK_TSkin, TSkin_dims, &TSkin_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SurfP_dims[0] = Scanline_dim;
   SurfP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SurfP", NC_SHORT, RANK_SurfP, SurfP_dims, &SurfP_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Emis_dims[0] = Scanline_dim;
   Emis_dims[1] = Field_of_view_dim;
   Emis_dims[2] = Channel_dim;
   stat = nc_def_var(ncid, "Emis", NC_SHORT, RANK_Emis, Emis_dims, &Emis_id);
   check_err(stat,__LINE__,__FILE__,errid);


   SFR_dims[0] = Scanline_dim;
   SFR_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SFR", NC_SHORT, RANK_SFR, SFR_dims, &SFR_id);
   check_err(stat,__LINE__,__FILE__,errid);

   CldTop_dims[0] = Scanline_dim;
   CldTop_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "CldTop", NC_SHORT, RANK_CldTop, CldTop_dims, &CldTop_id);
   check_err(stat,__LINE__,__FILE__,errid);

   CldBase_dims[0] = Scanline_dim;
   CldBase_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "CldBase", NC_SHORT, RANK_CldBase, CldBase_dims, &CldBase_id);
   check_err(stat,__LINE__,__FILE__,errid);

   CldThick_dims[0] = Scanline_dim;
   CldThick_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "CldThick", NC_SHORT, RANK_CldThick, CldThick_dims, &CldThick_id);
   check_err(stat,__LINE__,__FILE__,errid);

   PrecipType_dims[0] = Scanline_dim;
   PrecipType_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "PrecipType", NC_SHORT, RANK_PrecipType, PrecipType_dims, &PrecipType_id);
   check_err(stat,__LINE__,__FILE__,errid);

   RFlag_dims[0] = Scanline_dim;
   RFlag_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "RFlag", NC_SHORT, RANK_RFlag, RFlag_dims, &RFlag_id);
   check_err(stat,__LINE__,__FILE__,errid);


   SurfM_dims[0] = Scanline_dim;
   SurfM_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SurfM", NC_SHORT, RANK_SurfM, SurfM_dims, &SurfM_id);
   check_err(stat,__LINE__,__FILE__,errid);

   WindSp_dims[0] = Scanline_dim;
   WindSp_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "WindSp", NC_SHORT, RANK_WindSp, WindSp_dims, &WindSp_id);
   check_err(stat,__LINE__,__FILE__,errid);

   WindDir_dims[0] = Scanline_dim;
   WindDir_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "WindDir", NC_SHORT, RANK_WindDir, WindDir_dims, &WindDir_id);
   check_err(stat,__LINE__,__FILE__,errid);

   WindU_dims[0] = Scanline_dim;
   WindU_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "WindU", NC_SHORT, RANK_WindU, WindU_dims, &WindU_id);
   check_err(stat,__LINE__,__FILE__,errid);
   
   WindV_dims[0] = Scanline_dim;
   WindV_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "WindV", NC_SHORT, RANK_WindV, WindV_dims, &WindV_id);
   check_err(stat,__LINE__,__FILE__,errid);



   /* assign attributes */
   stat = nc_put_att_text(ncid, ScanTime_year_id, "long_name", 18, "Calendar Year 20XX");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_doy_id, "long_name", 16, "julian day 1-366");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_month_id, "long_name", 19, "Calendar month 1-12");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_dom_id, "long_name", 30, "Calendar day of the month 1-31");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_hour_id, "long_name", 20, "hour of the day 0-23");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_minute_id, "long_name", 23, "minute of the hour 0-59");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_second_id, "long_name", 25, "second of the minute 0-59");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ScanTime_UTC_id, "long_name", 36, "Number of seconds since 00:00:00 UTC");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Orb_mode_id, "description", 24, "0-ascending,1-descending");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Latitude_id, "long_name", 29, "Latitude of the view (-90,90)");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Longitude_id, "long_name", 32, "Longitude of the view (-180,180)");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Sfc_type_id, "description", 47, "type of surface:0-ocean,1-sea ice,2-land,3-snow");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Atm_type_id, "description", 33, "0-simple scene, 1-retrieved scene");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Qc_id, "description", 40, "Qc: 0-good, 1-usable with problem, 2-bad");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, ChiSqr_id, "description", 32, "Convergecy rate: <3-good,>10-bad");
   check_err(stat,__LINE__,__FILE__,errid);

/*   
   stat = nc_put_att_text(ncid, NAttempt_id, "description", 42, "Number of Attempts Performed for Retrieval");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, NIter_id, "description", 20, "Number of Iterations");
   check_err(stat,__LINE__,__FILE__,errid);
*/

   stat = nc_put_att_text(ncid, Freq_id, "description", 25, "Central Frequencies (GHz)");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Polo_id, "description", 13, "Polarizations");
   check_err(stat,__LINE__,__FILE__,errid);
  
   stat = nc_put_att_text(ncid, LZ_angle_id, "long_name", 25, "Local Zenith Angle degree");
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, RAzi_angle_id, "long_name", 35, "Relative Azimuth Angle 0-360 degree");
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, SZ_angle_id, "long_name", 34, "Solar Zenith Angle (-90,90) degree");
   check_err(stat,__LINE__,__FILE__,errid);
   

   stat = nc_put_att_text(ncid, BT_id, "long_name", 23, "Channel Temperature (K)");
   check_err(stat,__LINE__,__FILE__,errid);
   BT_scale[0] = 100;
   stat = nc_put_att_double(ncid, BT_id, "scale", NC_DOUBLE, 1, BT_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, YM_id, "long_name", 36, "Un-Corrected Channel Temperature (K)");
   check_err(stat,__LINE__,__FILE__,errid);
   YM_scale[0] = 100;
   stat = nc_put_att_double(ncid, YM_id, "scale", NC_DOUBLE, 1, YM_scale);
   check_err(stat,__LINE__,__FILE__,errid);


   stat = nc_put_att_text(ncid, ChanSel_id, "long_name", 36, "Channels Selection Used in Retrieval");
   check_err(stat,__LINE__,__FILE__,errid);
   ChanSel_scale[0] = 1;
   stat = nc_put_att_int(ncid, ChanSel_id, "scale", NC_INT, 1, ChanSel_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   
   stat = nc_put_att_text(ncid, TPW_id, "long_name", 29, "Total Precipitable Water (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   TPW_scale[0] = 10;
   stat = nc_put_att_double(ncid, TPW_id, "scale", NC_DOUBLE, 1, TPW_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, CLW_id, "long_name", 23, "Cloud liquid Water (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   CLW_scale[0] = 100;
   stat = nc_put_att_double(ncid, CLW_id, "scale", NC_DOUBLE, 1, CLW_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, RWP_id, "long_name", 20, "Rain Water Path (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   RWP_scale[0] = 100;
   stat = nc_put_att_double(ncid, RWP_id, "scale", NC_DOUBLE, 1, RWP_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, LWP_id, "long_name", 22, "Liquid Water Path (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   LWP_scale[0] = 100;
   stat = nc_put_att_double(ncid, LWP_id, "scale", NC_DOUBLE, 1, LWP_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, SWP_id, "long_name", 15, "Snow Water Path");
   check_err(stat,__LINE__,__FILE__,errid);
   SWP_scale[0] = 100;
   stat = nc_put_att_double(ncid, SWP_id, "scale", NC_DOUBLE, 1, SWP_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, IWP_id, "long_name", 19, "Ice Water Path (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   IWP_scale[0] = 100;
   stat = nc_put_att_double(ncid, IWP_id, "scale", NC_DOUBLE, 1, IWP_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, GWP_id, "long_name", 23, "Graupel Water Path (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   GWP_scale[0] = 100;
   stat = nc_put_att_double(ncid, GWP_id, "scale", NC_DOUBLE, 1, GWP_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, RR_id, "long_name", 17, "Rain Rate (mm/hr)");
   check_err(stat,__LINE__,__FILE__,errid);
   RR_scale[0] = 10;
   stat = nc_put_att_double(ncid, RR_id, "scale", NC_DOUBLE, 1, RR_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, Snow_id, "long_name", 10, "Snow Cover");
   check_err(stat,__LINE__,__FILE__,errid);
   Snow_scale[0] = 1;
   stat = nc_put_att_int(ncid, Snow_id, "scale", NC_INT, 1, Snow_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, SWE_id, "long_name", 26, "Snow Water Equivalent (cm)");
   check_err(stat,__LINE__,__FILE__,errid);
   SWE_scale[0] = 100;
   stat = nc_put_att_int(ncid, SWE_id, "scale", NC_INT, 1, SWE_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, SnowGS_id, "long_name", 20, "Snow Grain Size (mm)");
   check_err(stat,__LINE__,__FILE__,errid);
   SnowGS_scale[0] = 100;
   stat = nc_put_att_int(ncid, SnowGS_id, "scale", NC_INT, 1, SnowGS_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   
   stat = nc_put_att_text(ncid, SIce_id, "long_name", 25, "Sea Ice Concentration (%)");
   check_err(stat,__LINE__,__FILE__,errid);
   SIce_scale[0] = 1;
   stat = nc_put_att_int(ncid, SIce_id, "scale", NC_INT, 1, SIce_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, SIce_MY_id, "long_name", 36, "Multi-Year Sea Ice Concentration (%)");
   check_err(stat,__LINE__,__FILE__,errid);
   SIce_MY_scale[0] = 1;
   stat = nc_put_att_int(ncid, SIce_MY_id, "scale", NC_INT, 1, SIce_MY_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, SIce_FY_id, "long_name", 36, "First-Year Sea Ice Concentration (%)");
   check_err(stat,__LINE__,__FILE__,errid);
   SIce_FY_scale[0] = 1;
   stat = nc_put_att_int(ncid, SIce_FY_id, "scale", NC_INT, 1, SIce_FY_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, TSkin_id, "long_name", 20, "Skin Temperature (K)");
   check_err(stat,__LINE__,__FILE__,errid);
   TSkin_scale[0] = 100;
   stat = nc_put_att_int(ncid, TSkin_id, "scale", NC_INT, 1, TSkin_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, SurfP_id, "long_name", 21, "Surface Pressure (mb)");
   check_err(stat,__LINE__,__FILE__,errid);
   SurfP_scale[0] = 10;
   stat = nc_put_att_double(ncid, SurfP_id, "scale", NC_DOUBLE, 1, SurfP_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, Emis_id, "long_name", 18, "Channel Emmisivity");
   check_err(stat,__LINE__,__FILE__,errid);
   Emis_scale[0] = EMIS_SCAL ;
   stat = nc_put_att_double(ncid, Emis_id, "scale", NC_DOUBLE, 1, Emis_scale);
   check_err(stat,__LINE__,__FILE__,errid);



   stat = nc_put_att_text(ncid, SFR_id, "long_name", 23, "Snow Fall Rate in mm/hr");
   check_err(stat,__LINE__,__FILE__,errid);
   SFR_scale[0] = 100;
   stat = nc_put_att_double(ncid, SFR_id, "scale", NC_DOUBLE, 1, SFR_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, CldTop_id, "long_name", 18, "Cloud Top Pressure");
   check_err(stat,__LINE__,__FILE__,errid);
   CldTop_scale[0] = 10;
   stat = nc_put_att_double(ncid, CldTop_id, "scale", NC_DOUBLE, 1, CldTop_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, CldBase_id, "long_name", 19, "Cloud Base Pressure");
   check_err(stat,__LINE__,__FILE__,errid);
   CldBase_scale[0] = 10;
   stat = nc_put_att_double(ncid, CldBase_id, "scale", NC_DOUBLE, 1, CldBase_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, CldThick_id, "long_name", 15, "Cloud Thickness");
   check_err(stat,__LINE__,__FILE__,errid);
   CldThick_scale[0] = 10;
   stat = nc_put_att_double(ncid, CldThick_id, "scale", NC_DOUBLE, 1, CldThick_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, RFlag_id, "long_name", 9, "Rain Flag");
   check_err(stat,__LINE__,__FILE__,errid);
   RFlag_scale[0] = 1;
   stat = nc_put_att_int(ncid, RFlag_id, "scale", NC_INT, 1, RFlag_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, PrecipType_id, "long_name", 34, "Precipitation Type (Frozen/Liquid)");
   check_err(stat,__LINE__,__FILE__,errid);
   PrecipType_scale[0] = 1;
   stat = nc_put_att_int(ncid, PrecipType_id, "scale", NC_INT, 1, PrecipType_scale);
   check_err(stat,__LINE__,__FILE__,errid);



   stat = nc_put_att_text(ncid, SurfM_id, "long_name", 16, "Surface Moisture");
   check_err(stat,__LINE__,__FILE__,errid);
   SurfM_scale[0] = 10;
   stat = nc_put_att_double(ncid, SurfM_id, "scale", NC_DOUBLE, 1, SurfM_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, WindSp_id, "long_name", 16, "Wind Speed (m/s)");
   check_err(stat,__LINE__,__FILE__,errid);
   WindSp_scale[0] = 100;
   stat = nc_put_att_double(ncid, WindSp_id, "scale", NC_DOUBLE, 1, WindSp_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, WindDir_id, "long_name", 14, "Wind Direction");
   check_err(stat,__LINE__,__FILE__,errid);
   WindDir_scale[0] = 100;
   stat = nc_put_att_double(ncid, WindDir_id, "scale", NC_DOUBLE, 1, WindDir_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, WindU_id, "long_name", 28, "U-direction Wind Speed (m/s)");
   check_err(stat,__LINE__,__FILE__,errid);
   WindU_scale[0] = 100;
   stat = nc_put_att_double(ncid, WindU_id, "scale", NC_DOUBLE, 1, WindU_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, WindV_id, "long_name", 28, "V-direction Wind Speed (m/s)");
   check_err(stat,__LINE__,__FILE__,errid);
   WindV_scale[0] = 100;
   stat = nc_put_att_double(ncid, WindV_id, "scale", NC_DOUBLE, 1, WindV_scale);
   check_err(stat,__LINE__,__FILE__,errid);

   
   
   cdf_missing_value[0] = MISSING;
   stat = nc_put_att_int(ncid, NC_GLOBAL, "missing_value", NC_INT, 1, cdf_missing_value);
   check_err(stat,__LINE__,__FILE__,errid);
   
   cdf_version[0] = 4;
   stat = nc_put_att_double(ncid, NC_GLOBAL, "cdf_version", NC_DOUBLE, 1, cdf_version);
   check_err(stat,__LINE__,__FILE__,errid);
   
   alg_version[0] = alg_sn;
   stat = nc_put_att_int(ncid, NC_GLOBAL, "alg_version", NC_INT, 1, alg_version);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, NC_GLOBAL, "dap_version", 4, "v9r2" );
   check_err(stat,__LINE__,__FILE__,errid);
  
   // NPP ATMS special header information goes here
   if( strcmp( satid, "NPP" ) == 0 ) {
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "Conventions", strlen(NPP_Conventions_str), NPP_Conventions_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "Metadata_Conventions", strlen(NPP_Metadata_Conventions_str), NPP_Metadata_Conventions_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "standard_name_vocabulary", strlen(NPP_standard_name_vocabulary_str), NPP_standard_name_vocabulary_str );
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_text(ncid, NC_GLOBAL, "project", strlen(NPP_project_str), NPP_project_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "title", strlen(NPP_title_img_str), NPP_title_img_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "summary", strlen(NPP_summary_img_str), NPP_summary_img_str );
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_text(ncid, NC_GLOBAL, "date_created", strlen(date_created_img), date_created_img );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "institution", strlen(NPP_institution_str), NPP_institution_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "naming_authority", strlen(NPP_naming_authority_str), NPP_naming_authority_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     //stat = nc_put_att_text(ncid, NC_GLOBAL, "id", strlen(NPP_id_str), NPP_id_str );
     //check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "satellite_name", strlen(NPP_satellite_name_str), NPP_satellite_name_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "instrument_name", strlen(NPP_instrument_name_str), NPP_instrument_name_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "creator_name", strlen(NPP_creator_name_str), NPP_creator_name_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "creator_email", strlen(NPP_creator_email_str), NPP_creator_email_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "creator_url", strlen(NPP_creator_url_str), NPP_creator_url_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "publisher_name", strlen(NPP_publisher_name_str), NPP_publisher_name_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "publisher_email", strlen(NPP_publisher_email_str), NPP_publisher_email_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "publisher_url", strlen(NPP_publisher_url_str), NPP_publisher_url_str );
     check_err(stat,__LINE__,__FILE__,errid);
     
     stat = nc_put_att_text(ncid, NC_GLOBAL, "Metadata_Link", strlen(NPP_Metadata_Link_str), NPP_Metadata_Link_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "references", strlen(NPP_references_str), NPP_references_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "history", strlen(NPP_history_str), NPP_history_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "processing_level", strlen(NPP_processing_level_str), NPP_processing_level_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "source", strlen(RDR_name), RDR_name );
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_text(ncid, NC_GLOBAL, "time_coverage_start", strlen(time_coverage_start), time_coverage_start );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "time_coverage_end", strlen(time_coverage_end), time_coverage_end );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "cdm_data_type", strlen(NPP_cdm_data_type_str), NPP_cdm_data_type_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "geospatial_lat_units", strlen(NPP_geospatial_lat_units_str), NPP_geospatial_lat_units_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "geospatial_lon_units", strlen(NPP_geospatial_lon_units_str), NPP_geospatial_lon_units_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "geospatial_lat_resolution", strlen(NPP_geospatial_lat_resolution_str), NPP_geospatial_lat_resolution_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "geospatial_lon_resolution", strlen(NPP_geospatial_lon_resolution_str), NPP_geospatial_lon_resolution_str );
     check_err(stat,__LINE__,__FILE__,errid);
   

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_first_scanline_first_fov_lat", NC_FLOAT, 1, geospatial_first_scanline_first_fov_lat);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_first_scanline_first_fov_lon", NC_FLOAT, 1, geospatial_first_scanline_first_fov_lon);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_first_scanline_last_fov_lat", NC_FLOAT, 1, geospatial_first_scanline_last_fov_lat);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_first_scanline_last_fov_lon", NC_FLOAT, 1, geospatial_first_scanline_last_fov_lon);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_last_scanline_first_fov_lat", NC_FLOAT, 1, geospatial_last_scanline_first_fov_lat);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_last_scanline_first_fov_lon", NC_FLOAT, 1, geospatial_last_scanline_first_fov_lon);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_last_scanline_last_fov_lat", NC_FLOAT, 1, geospatial_last_scanline_last_fov_lat);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "geospatial_last_scanline_last_fov_lon", NC_FLOAT, 1, geospatial_last_scanline_last_fov_lon);
     check_err(stat,__LINE__,__FILE__,errid);


     stat = nc_put_att_int(ncid, NC_GLOBAL, "total_number_retrievals", NC_INT, 1, total_number_retrievals);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "percentage_optimal_retrievals", NC_FLOAT, 1, percentage_optimal_retrievals);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "percentage_suboptimal_retrievals", NC_FLOAT, 1, percentage_suboptimal_retrievals);
     check_err(stat,__LINE__,__FILE__,errid);

     stat = nc_put_att_float(ncid, NC_GLOBAL, "percentage_bad_retrievals", NC_FLOAT, 1, percentage_bad_retrievals);
     check_err(stat,__LINE__,__FILE__,errid);

     
   
     /*
     stat = nc_put_att_text(ncid, NC_GLOBAL, "EDR_name", strlen(EDR_name), EDR_name );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "DEP_name", strlen(DEP_name), DEP_name );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "RDR_name", strlen(RDR_name), RDR_name );
     check_err(stat,__LINE__,__FILE__,errid);
     */

     /* generate a unique ID for this granule: hostname + date_creation_time + random number + RDR filename */
     
     hostname[256] = '\0';
     gethostname(hostname, 255);
     iseed = (unsigned int)time(NULL);
     srand(iseed);
     random_number = rand();
     sprintf(char_random_number,"%016u",random_number); 
     
     strcpy(uuid,hostname);
     strcat(uuid,"_");
     strcat(uuid,date_created_img);
     strcat(uuid,"_");
     strcat(uuid,char_random_number);
     strcat(uuid,"_");
     strcat(uuid,RDR_name);
     
     stat = nc_put_att_text(ncid, NC_GLOBAL, "id", strlen(uuid), uuid );
     check_err(stat,__LINE__,__FILE__,errid);
           
   }
   
   /* leave define mode */
   stat = nc_enddef (ncid);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__,errid);
   
   return 0;
}
