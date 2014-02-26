/***********************************************************************
 *  Program Name      : gnrt_elev.c
 *  Type              : Subroutine
 *  Function          : Program generates swath-size elevation field
 *                      from a large terrain data base
 *  Input Files       : tbase.bin
 *  Output Files      : None
 *  Subroutine Called : mean_elev.c
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/***********************************************/
void mean_elev();

/***********************************************/
void gnrt_elev(TOTNUM_OF_SCAN numscan)
{
   const       float lon0 = 180.; 
   const       float lat0 = 90.;

   short int   iscan, ifov, i, j, indx;
   float       ang[NUMSPOT_B+1], angle = 0, angle1 = 0;
   float       fov_size, loncorr, elev2;
   float       alat, alon;

/*----------------------------------------*
 * Check if the scan line is good
 *----------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
       if(at_AB[iscan][0][0] > 0 && at[iscan][0][0] > 0) 
       {
          for(ifov = 0; ifov < NUMSPOT_B; ifov++)
          {
	     fov_size = fovsize[ifov];
             alat = lat[iscan][ifov];
             alon = lon[iscan][ifov];

/*----------------------------------------*
 * Determine the coordinates of the FOV 
 * boundaries in the "elev_large" data
 * coordinate system
 *----------------------------------------*/
	     loncorr = fabs(1/cos(PI*alat/180));
	     lonleft = (alon + lon0 - 0.5 * fov_size * loncorr) * 12.;
	     lonright = (alon + lon0 + 0.5 * fov_size * loncorr) * 12.;
             
	     if (fabs(ifov-(NUMSPOT_B-1.)/2.) < LATLIM1_B - 0.4)   
	     {
                lattop = NUMY_ELEV - 1 - (alat + lat0) * 12.;
	 	latbot = lattop;
	     }
 	     else if (fabs(ifov-(NUMSPOT_B-1.)/2.) < LATLIM2_B - 0.4)
	     {
		lattop = NUMY_ELEV - 1 - (alat + lat0) * 12. + 0.5;	        
	        latbot = lattop - 1;
	     }
	     else
	     {
		lattop = NUMY_ELEV - 1 - (alat + lat0) * 12.;
		lattop = lattop + 1;
		latbot = lattop - 2;
	     }
	     if(lattop >= NUMY_ELEV)
		lattop = NUMY_ELEV - 1;
             if(latbot < 0)
		latbot = 0;
                
/*----------------------------------------*
 * Compute the mean of all the elevation 
 * values in the FOV
 *----------------------------------------*/
             mean_elev(iscan, ifov); 

          }  /* end of ifov   */

       }   /* end of scanline check */

    }     /* end of iscan loop      */

}  /* end of gnrt_elev.c */
