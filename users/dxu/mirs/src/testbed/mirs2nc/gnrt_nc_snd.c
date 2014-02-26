/***************************************************************************
 *  Program Name      : gnrt_nc_snd.c
 *  Type              : Subroutine
 *  Function          : Program creates netcdf sounding file 
 *  Input Files       : 
 *  Output Files      : Netcdf files
 *  Subroutine Called : 
 *  Called by         : set_nc_snd.c
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

void check_err(const int stat, const int line, const char *file, int err_id) {
    if (stat != NC_NOERR) {
	(void) fprintf(stderr, "line %d of %s: %s\n", line, file, nc_strerror(stat));
        exit(err_id);
    }
}


int gnrt_nc_snd(char *fname_nc_snd, short int nscan, short int nspot, short int nlay, short int nchan, char *satid) {	  
   int  stat;			/* return status */
   int  ncid;			/* netCDF id */
   int  errid = 15;

   /* dimension ids */
   int Scanline_dim;
   int Field_of_view_dim;
   int P_Layer_dim;
   int P_Level_dim;
   int Channel_dim;
   int Qc_dim_dim;

   /* dimension lengths */
   size_t Scanline_len = nscan;
   size_t Field_of_view_len = nspot;
   size_t P_Layer_len = nlay;
   size_t P_Level_len = nlay+1;
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
   int Player_id;
   int Plevel_id;
   int PTemp_id;
   int PVapor_id;
   //int POzone_id;
   int PClw_id;
   int PRain_id;
   int PGraupel_id;
   int PSnow_id;
   int PIce_id;
   int SurfP_id;
   int Freq_id;
   int Polo_id;

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
#  define RANK_NAttempt 2
#  define RANK_NIter 2
#  define RANK_ChiSqr 2
#  define RANK_LZ_angle 2
#  define RANK_RAzi_angle 2
#  define RANK_SZ_angle 2
#  define RANK_Player 1
#  define RANK_Plevel 1
#  define RANK_PTemp 3
#  define RANK_PVapor 3
#  define RANK_POzone 3
#  define RANK_PClw 3
#  define RANK_PRain 3
#  define RANK_PGraupel 3
#  define RANK_PSnow 3
#  define RANK_PIce 3
#  define RANK_SurfP 2
#  define RANK_Freq 1
#  define RANK_Polo 1

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
   int Player_dims[RANK_Player];
   int Plevel_dims[RANK_Plevel];
   int PTemp_dims[RANK_PTemp];
   int PVapor_dims[RANK_PVapor];
   //int POzone_dims[RANK_POzone];
   int PClw_dims[RANK_PClw];
   int PRain_dims[RANK_PRain];
   int PGraupel_dims[RANK_PGraupel];
   int PSnow_dims[RANK_PSnow];
   int PIce_dims[RANK_PIce];
   int SurfP_dims[RANK_SurfP];
   int Freq_dims[RANK_Freq];
   int Polo_dims[RANK_Polo];

   /* attribute vectors */
   double PTemp_scale[1];
   double PVapor_scale[1];
   //double POzone_scale[1];
   double PClw_scale[1];
   double PRain_scale[1];
   double PGraupel_scale[1];
   double PSnow_scale[1];
   double PIce_scale[1];
   double SurfP_scale[1];
   int cdf_missing_value[1];
   double cdf_version[1];
   int alg_version[1];

   time_t rawtime;
   struct tm * timeinfo;
   char date_created_snd[21];
   
   unsigned int random_number;
   char char_random_number[17]; 
   char hostname[256];
   unsigned int iseed;
  
   char uuid[1024];

   /* enter define mode */
   stat = nc_create(fname_nc_snd, NC_CLOBBER|NC_NETCDF4, &ncid);
   check_err(stat,__LINE__,__FILE__,errid);

   /* creation time */
   time ( &rawtime );
   //timeinfo = localtime ( &rawtime );
   timeinfo = gmtime ( &rawtime );
   sprintf(date_created_snd,"%4d-%02d-%02dT%02d:%02d:%02dZ",
      timeinfo->tm_year+1900,timeinfo->tm_mon+1,timeinfo->tm_mday,timeinfo->tm_hour,timeinfo->tm_min,timeinfo->tm_sec);

   /* define dimensions */
   stat = nc_def_dim(ncid, "Scanline", Scanline_len, &Scanline_dim);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_def_dim(ncid, "Field_of_view", Field_of_view_len, &Field_of_view_dim);
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_def_dim(ncid, "P_Layer", P_Layer_len, &P_Layer_dim);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_def_dim(ncid, "P_Level", P_Level_len, &P_Level_dim);
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

   Player_dims[0] = P_Layer_dim;
   stat = nc_def_var(ncid, "Player", NC_FLOAT, RANK_Player, Player_dims, &Player_id);
   check_err(stat,__LINE__,__FILE__,errid);

   Plevel_dims[0] = P_Level_dim;
   stat = nc_def_var(ncid, "Plevel", NC_FLOAT, RANK_Plevel, Plevel_dims, &Plevel_id);
   check_err(stat,__LINE__,__FILE__,errid);


   PTemp_dims[0] = Scanline_dim;
   PTemp_dims[1] = Field_of_view_dim;
   PTemp_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PTemp", NC_FLOAT, RANK_PTemp, PTemp_dims, &PTemp_id);
   check_err(stat,__LINE__,__FILE__,errid);

   PVapor_dims[0] = Scanline_dim;
   PVapor_dims[1] = Field_of_view_dim;
   PVapor_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PVapor", NC_FLOAT, RANK_PVapor, PVapor_dims, &PVapor_id);
   check_err(stat,__LINE__,__FILE__,errid);

/*
   POzone_dims[0] = Scanline_dim;
   POzone_dims[1] = Field_of_view_dim;
   POzone_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "POzone", NC_FLOAT, RANK_POzone, POzone_dims, &POzone_id);
   check_err(stat,__LINE__,__FILE__,errid);
*/

   PClw_dims[0] = Scanline_dim;
   PClw_dims[1] = Field_of_view_dim;
   PClw_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PClw", NC_FLOAT, RANK_PClw, PClw_dims, &PClw_id);
   check_err(stat,__LINE__,__FILE__,errid);

   PRain_dims[0] = Scanline_dim;
   PRain_dims[1] = Field_of_view_dim;
   PRain_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PRain", NC_FLOAT, RANK_PRain, PRain_dims, &PRain_id);
   check_err(stat,__LINE__,__FILE__,errid);

   PGraupel_dims[0] = Scanline_dim;
   PGraupel_dims[1] = Field_of_view_dim;
   PGraupel_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PGraupel", NC_FLOAT, RANK_PGraupel, PGraupel_dims, &PGraupel_id);
   check_err(stat,__LINE__,__FILE__,errid);

   PSnow_dims[0] = Scanline_dim;
   PSnow_dims[1] = Field_of_view_dim;
   PSnow_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PSnow", NC_FLOAT, RANK_PSnow, PSnow_dims, &PSnow_id);
   check_err(stat,__LINE__,__FILE__,errid);

   PIce_dims[0] = Scanline_dim;
   PIce_dims[1] = Field_of_view_dim;
   PIce_dims[2] = P_Layer_dim;
   stat = nc_def_var(ncid, "PIce", NC_FLOAT, RANK_PIce, PIce_dims, &PIce_id);
   check_err(stat,__LINE__,__FILE__,errid);

   SurfP_dims[0] = Scanline_dim;
   SurfP_dims[1] = Field_of_view_dim;
   stat = nc_def_var(ncid, "SurfP", NC_SHORT, RANK_SurfP, SurfP_dims, &SurfP_id);
   check_err(stat,__LINE__,__FILE__,errid);


   /* assign attributes */
   stat = nc_put_att_text(ncid, Freq_id, "long_name", 17, "Central Frequency");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Polo_id, "long_name", 36, "Polarizations(Horizonal or Vertical)");
   check_err(stat,__LINE__,__FILE__,errid);
   
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
   stat = nc_put_att_text(ncid, Sfc_type_id, "description", 47, "type of surface:0-ocean,1-sea-ice,2-land,3-snow");
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, Atm_type_id, "description", 33, "0-simple scene, 1-retrieved scene");
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, Qc_id, "description", 42, "Qc(0): 0-good,1-usable with problem ,2-bad");
   check_err(stat,__LINE__,__FILE__,errid);

   stat = nc_put_att_text(ncid, ChiSqr_id, "description", 32, "convergecy rate: <3-good,>10-bad");
   check_err(stat,__LINE__,__FILE__,errid);

/*   
   stat = nc_put_att_text(ncid, NAttempt_id, "description", 42, "Number of Attempts Performed for Retrieval");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, NIter_id, "description", 20, "Number of Iterations");
   check_err(stat,__LINE__,__FILE__,errid);
*/

   stat = nc_put_att_text(ncid, LZ_angle_id, "long_name", 35, "Local zenith angle: (-59,59) degree");
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, RAzi_angle_id, "long_name", 35, "Relative azimuth angle 0-360 degree");
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, SZ_angle_id, "long_name", 34, "Solar zenith angle (-90,90) degree");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Player_id, "description", 29, "Pressure for each layer in mb");
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, Plevel_id, "description", 29, "Pressure for each level in mb");
   check_err(stat,__LINE__,__FILE__,errid);
  
   stat = nc_put_att_text(ncid, PTemp_id, "long_name", 24, "Temperature profile in K");
   check_err(stat,__LINE__,__FILE__,errid);
   
   PTemp_scale[0] = 1;
   stat = nc_put_att_double(ncid, PTemp_id, "scale", NC_DOUBLE, 1, PTemp_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   stat = nc_put_att_text(ncid, PVapor_id, "long_name", 27, "Water vapor profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   PVapor_scale[0] = 1;
   stat = nc_put_att_double(ncid, PVapor_id, "scale", NC_DOUBLE, 1, PVapor_scale);
   check_err(stat,__LINE__,__FILE__,errid);

/*   
   stat = nc_put_att_text(ncid, POzone_id, "long_name", 21, "Ozone profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   POzone_scale[0] = 1;
   stat = nc_put_att_double(ncid, POzone_id, "scale", NC_DOUBLE, 1, POzone_scale);
   check_err(stat,__LINE__,__FILE__,errid);
*/   
   stat = nc_put_att_text(ncid, PClw_id, "long_name", 34, "Cloud liquid water profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   PClw_scale[0] = 1;
   stat = nc_put_att_double(ncid, PClw_id, "scale", NC_DOUBLE, 1, PClw_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, PRain_id, "long_name", 25, "Rain mass profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   PRain_scale[0] = 1;
   stat = nc_put_att_double(ncid, PRain_id, "scale", NC_DOUBLE, 1, PRain_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, PGraupel_id, "long_name", 28, "Graupel mass profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   PGraupel_scale[0] = 1;
   stat = nc_put_att_double(ncid, PGraupel_id, "scale", NC_DOUBLE, 1, PGraupel_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, PSnow_id, "long_name", 25, "Snow mass profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   PSnow_scale[0] = 1;
   stat = nc_put_att_double(ncid, PSnow_id, "scale", NC_DOUBLE, 1, PSnow_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, PIce_id, "long_name", 24, "Ice mass profile in g/kg");
   check_err(stat,__LINE__,__FILE__,errid);
   PIce_scale[0] = 1;
   stat = nc_put_att_double(ncid, PIce_id, "scale", NC_DOUBLE, 1, PIce_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   stat = nc_put_att_text(ncid, SurfP_id, "long_name", 22, "Surface pressure in mb");
   check_err(stat,__LINE__,__FILE__,errid);
   SurfP_scale[0] = 10;
   stat = nc_put_att_double(ncid, SurfP_id, "scale", NC_DOUBLE, 1, SurfP_scale);
   check_err(stat,__LINE__,__FILE__,errid);
   
   cdf_missing_value[0] = -999;
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
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "title", strlen(NPP_title_snd_str), NPP_title_snd_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "summary", strlen(NPP_summary_snd_str), NPP_summary_snd_str );
     check_err(stat,__LINE__,__FILE__,errid);
   
     stat = nc_put_att_text(ncid, NC_GLOBAL, "date_created", strlen(date_created_snd), date_created_snd );
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
     strcat(uuid,date_created_snd);
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
