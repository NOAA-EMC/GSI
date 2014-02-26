/***********************************************************************
 *  Program Name      : def_dim.c
 *  Type              : Subroutine
 *  Function          : Program defines dimensions for data and 
 *                      geolocation fields
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

/**********************************************************/
long int def_dim(long int  sw_id, TOTNUM_OF_SCAN  numscan)
{ 
   long int   status, results = 0;

/*----------------------------------------------------*
 * Define swath dimensions for the data fields
 *----------------------------------------------------*/
   status = SWdefdim(sw_id, "Scanline", numscan);
   results += status;
   printf ("def_dim/define dim Scanline  %ld\n", status);
   status = SWdefdim(sw_id, "Field_of_view", NUMSPOT_B);
   results += status;
   printf ("def_dim/define dim FielsOfView  %ld\n", status);

/*----------------------------------------------------*
 * Define swath dimensions for the geolocation fields
 *----------------------------------------------------*/
   status = SWdefdim(sw_id, "Position1", numscan);
   results += status;
   printf ("def_dim/define dim Position1  %ld\n", status);
   status = SWdefdim(sw_id, "Position2", NUMSPOT_B);
   results += status;
   printf ("def_dim/define dim Position2  %ld\n", status);

/*----------------------------------------------------*
 * Establish mapping between data and geolocation 
 * fields
 *----------------------------------------------------*/
   status = SWdefdimmap(sw_id, "Position1", "Scanline", 0,1);
   results += status;
   printf ("def_dim/define map1 %ld\n", status);
   status = SWdefdimmap(sw_id, "Position2", "Field_of_view", 0,1);
   results += status;
   printf ("def_dim/define map2 %ld\n", status);

   return(results);
 
} /* end of def_dim.c */
