/***********************************************************************
 *  Program Name      : scn_data.c
 *  Type              : Subroutine
 *  Function          : Program extracts data from MHS 1B* scanlines  
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rmhs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/**********************************************************/
void scn_data(TOTNUM_OF_SCAN numscan) 
{
   short int       iscan, ifov;

   for(iscan = 0; iscan < numscan; iscan++)
      for(ifov = 0; ifov < NUMSPOT_M; ifov++)
      {
         lat[iscan][ifov] = scanline[iscan].earth_location[ifov*2]*1.0E-4;
         lon[iscan][ifov] = scanline[iscan].earth_location[ifov*2+1]*1.0E-4;

         if(ifov < NUMSPOT_M/2)
           lza[iscan][ifov] = -scanline[iscan].angular_relationships[ifov*3+1]*1.0E-2;
	 else
	   lza[iscan][ifov] = scanline[iscan].angular_relationships[ifov*3+1]*1.0E-2; 

         sza[iscan][ifov] = scanline[iscan].angular_relationships[ifov*3]*1.0E-2; 

      }

}  /* end of scn_data.c */
