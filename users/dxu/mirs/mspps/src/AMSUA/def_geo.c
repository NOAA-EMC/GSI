/**************************************************************************
 *  Program Name      : def_geo.c
 *  Type              : Subroutine
 *  Function          : Program defines geolocation fields for HDF-EOS
 *                      swath file
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : sw_def.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
long int def_geo(long int sw_id)
{ 
   
   long int   status, result=0;

/*---------------------------------------------------*
 * Define the geolocation fields: scan time fields  
 *---------------------------------------------------*/
   status = SWdefgeofield(sw_id, "ScanTime_year", "Position1",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_year %ld\n", status);
   result = result + status;
   
   status = SWdefgeofield(sw_id, "ScanTime_month", "Position1",
            DFNT_INT8, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_month %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "ScanTime_dom", "Position1",
            DFNT_INT8, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_dom %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "ScanTime_hour", "Position1",
            DFNT_INT8, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_hour %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "ScanTime_minute", "Position1",
            DFNT_INT8, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_minute %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "ScanTime_second", "Position1",
            DFNT_INT8, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_second %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "ScanTime_doy", "Position1",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_geo/define geofield ScanTime_doy %ld\n", status);
   result = result + status;

/*---------------------------------------------------*
 * Define the geolocation fields: latitude, longitude 
 * and Time(TAI93) fields
 *---------------------------------------------------*/
   status = SWdefgeofield(sw_id, "Latitude", "Position1,Position2",
            DFNT_FLOAT32, HDFE_NOMERGE);
   printf ("def_geo/define geofield Latitude %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "Longitude", "Position1,Position2",
            DFNT_FLOAT32, HDFE_NOMERGE);
   printf ("def_geo/define geofield Longitude %ld\n", status);
   result = result + status;

   status = SWdefgeofield(sw_id, "Time", "Position1",
            DFNT_FLOAT64, HDFE_NOMERGE);
   printf ("def_geo/define geofield Time %ld\n", status);
   result = result + status;

   return(result);

}/*  end of def_geo.c  */
