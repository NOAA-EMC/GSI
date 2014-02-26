/***************************************************************************
 *  Program Name      : scn_data.c
 *  Type              : Subroutine
 *  Function          : Program extracts data from AMSU-A 1B* file
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_whdfeos.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************************/
void scn_data(short int numscan) 
{
   short int        iscan, ifov;

   for(iscan = 0; iscan < numscan; iscan++)
      for(ifov = 0; ifov < NUMSPOT_A; ifov++)
      {
         lat[iscan][ifov] = scanline3[iscan].lat_lon_degrees[ifov][0];
         lon[iscan][ifov] = scanline3[iscan].lat_lon_degrees[ifov][1];

         if(ifov < NUMSPOT_A/2)
           lza[iscan][ifov] = -scanline3[iscan].satellite_zenith_angle[ifov];
         else
           lza[iscan][ifov] = scanline3[iscan].satellite_zenith_angle[ifov];

         sza[iscan][ifov] = scanline3[iscan].solar_zenith_angle[ifov];

      }

}  /* end of scn_data.c */
