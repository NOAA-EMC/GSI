/**************************************************************************
 *  Program Name      : wrt_geo.c
 *  Type              : Subroutine
 *  Function          : Program writes geolocation fields to HDF-EOS 
 *			swath file
 *  Input Files       : None
 *  Output Files      : NPR.AAOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : toolkit_td.c 
 *  Called by         : sw_wrt.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************************/
  float64    toolkit_td(char *);     

/******************************************************************/
long int wrt_geo(long int sw_id, short int  numscan) 
{ 

   long int   status, result = 0, i;
   float64    Con_time;
   char       astime[28];
   char       yyyy[5], dd[4], mon[3], day[3], hh[3], mm[3], ss[3];
 
   int32        start[2]  = {0, 0};
   int32        stride[2] = {1, 1};
   int32        edges[2];

/*---------------------------------------------------------*
 * Generate geolocation table/array, edges 
 *---------------------------------------------------------*/
   edges[0] = numscan;
   edges[1] = NUMSPOT_A;
   
   for(i = 0; i < numscan; i++)
   {
     year[i]   = geo_scantime[i].year;
     month[i]  = geo_scantime[i].month;
     dom[i]    = geo_scantime[i].dom;
     doy[i]    = geo_scantime[i].doy;
     hour[i]   = geo_scantime[i].hour;
     minute[i] = geo_scantime[i].minute;
     second[i] = geo_scantime[i].second;

     sprintf(yyyy,"%4d",year[i]);
     yyyy[4] = '\0';

     sprintf(dd,"%3d",doy[i]);
     if(doy[i] < 100)dd[0] = '0';
     if(doy[i] < 10) dd[1] = '0';
     if(doy[i] == 0) dd[2] = '0';
     dd[3] = '\0';

     sprintf(mon, "%2d",month[i]);
     if(month[i] <10) mon[0] = '0';
     if(month[i] == 0) mon[1] = '0';
     mon[2] = '\0';

     sprintf(day,"%2d",dom[i]);
     if(dom[i] < 10) day[0] = '0';
     if(dom[i] == 0) day[1] ='0';
     day[2] = '\0';

     sprintf(hh, "%2d", hour[i]);
     if(hour[i] < 10) hh[0] = '0';
     if(hour[i] == 0) hh[1] = '0';
     hh[2] = '\0';

     sprintf(mm, "%2d", minute[i]);
     if(minute[i] < 10) mm[0] = '0';
     if(minute[i] == 0) mm[1] = '0';
     mm[2] = '\0';

     sprintf(ss, "%2d", second[i]);
     if(second[i] < 10) ss[0] = '0';
     if(second[i] == 0) ss[1] ='0';
     ss[2] = '\0';

     strcpy(astime, yyyy);
     strcat(astime,"-");
     strcat(astime, mon);
     strcat(astime,"-");
     strcat(astime, day);
     strcat(astime, "T");
     strcat(astime, hh);
     strcat(astime, ":");
     strcat(astime, mm);
     strcat(astime,":");
     strcat(astime, ss);
     strcat(astime,".000000Z");

     Con_time = toolkit_td(astime); 
     time_tai93[i] = Con_time;

   }

/*---------------------------------------------------------*
 * Write geolocation fields
 *---------------------------------------------------------*/
   status = SWwritefield(sw_id, "Latitude", start, stride, edges, lat);
   printf("wrt_geo/writefield latitude %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "Longitude", start, stride, edges, lon);
   printf("wrt_geo/writefield longitude %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "Time", start, stride, edges, time_tai93);
   printf("wrt_geo/writefield time %ld\n",status);
   result = result + status;

/*---------------------------------------------------------*
 * Write scan time
 *---------------------------------------------------------*/
   status = SWwritefield(sw_id, "ScanTime_year", start, stride, edges, year);
   printf("wrt_geo/writefield ScanTime_year %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "ScanTime_month", start, stride, edges, month);
   printf("wrt_geo/writefield ScanTime_month %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "ScanTime_dom", start, stride, edges, dom);
   printf("wrt_geo/writefield ScanTime_dom %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "ScanTime_hour", start, stride, edges, hour);
   printf("wrt_geo/writefield ScanTime_hour %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "ScanTime_minute", start,stride,edges, minute);
   printf("wrt_geo/writefield ScanTime_minute %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "ScanTime_second",start,stride,edges, second);
   printf("wrt_geo/writefield ScanTime_second %ld\n",status);
   result = result + status;

   status = SWwritefield(sw_id, "ScanTime_doy", start, stride, edges, doy);
   printf("wrt_geo/writefield ScanTime_doy %ld\n",status);
   result = result + status;

   return(result);

}/*  end of wrt_geo.c */
