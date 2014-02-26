/***********************************************************************
 *  Program Name      : scn_data.c
 *  Type              : Subroutine
 *  Function          : Program extracts data from AMSU-B 1B* scanlines
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/**********************************************************/
void scn_data(TOTNUM_OF_SCAN numscan) 
{
   short int        iscan, ifov;

   for(iscan = 0; iscan < numscan; iscan++)
      for(ifov = 0; ifov < NUMSPOT_B; ifov++)
      {
	 lat[iscan][ifov] = scanline3[iscan].lat_lon_degrees[ifov][0];
         lon[iscan][ifov] = scanline3[iscan].lat_lon_degrees[ifov][1];

         if(ifov < NUMSPOT_B/2)
           lza[iscan][ifov] = -scanline3[iscan].satellite_zenith_angle[ifov];
	 else
	   lza[iscan][ifov] = scanline3[iscan].satellite_zenith_angle[ifov];

         sza[iscan][ifov] = scanline3[iscan].solar_zenith_angle[ifov];

      }

}  /* end of scn_data.c */
