/***********************************************************************
 *  Program Name      : def_geo.c
 *  Type              : Subroutine
 *  Function          : Program defines geolocation fields for the
 *                      preliminary AMSU-B swath file (in HDF-EOS format)
 *  Input Files       : None
 *  Output Files      : NPR.ABOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : sw_def.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
long int def_geo(long int sw_id)
{ 
   
   long int   status, result=0;

/*----------------------------------------------*
 * Define scan time fields
 *----------------------------------------------*/
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

/*----------------------------------------------*
 * Define Time(TAI93) and the geolocation fields
 *----------------------------------------------*/
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

} /* end of def_geo.c */
