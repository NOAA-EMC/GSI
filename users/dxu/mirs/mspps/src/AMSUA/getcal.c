/***************************************************************************
 *  Program Name      : getcal.c
 *  Type              : Subroutine
 *  Function          : Program extracts earth view of each spot and 
 *			calibration coefficients from AMSU-A 1B data,
 *			and call subroutines to convert counts to radiances 
 *			and then radiances to antenna temperatures
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : cnt2rd.c, rad2at.c 
 *  Called by         : gnrt_prod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void cnt2rd();
void rad2at();

/*****************************************************************/
void getcal(short int iscan)
{
   short int   ichan, ispot;
   long  int   ifchan, insnum, numchan, numspot;

   numchan = NUMCHAN_A;
   numspot = NUMSPOT_A;
   insnum  = 2;

/*---------------------------------------------------------*
 * Extract earth views (counts) and calibration coeffs
 *---------------------------------------------------------*/
   for(ispot = 0; ispot < NUMSPOT_A; ispot++)
     for(ichan = 0; ichan < NUMCHAN_A; ichan++)
     {
       if(ichan < 2)
         count[ichan][ispot] = scanline[iscan].scene_telemetry_a2[ispot*4+ichan+2];
       else
         count[ichan][ispot] = scanline[iscan].scene_telemetry_a1[ispot*17+ichan+2];
     }

   for(ichan = 0; ichan < NUMCHAN_A; ichan++)
   {
     clcoef[ichan][2] = scanline[iscan].pri_cal_coeffs[ichan][0]*1.0E-19;
     clcoef[ichan][1] = scanline[iscan].pri_cal_coeffs[ichan][1]*1.0E-13;
     clcoef[ichan][0] = scanline[iscan].pri_cal_coeffs[ichan][2]*1.0E-09;
   }

/*---------------------------------------------------------*
 * Calculate radiances from counts
 *---------------------------------------------------------*/
     cnt2rd(numchan, numspot);

/*---------------------------------------------------------*
 * Convert radiances to antenna temperatures
 *---------------------------------------------------------*/
   for(ichan = 0; ichan < NUMCHAN_A; ichan++)
   {
     ifchan = ichan;
     for (ispot=0; ispot< NUMSPOT_A; ispot++)
          rad[ispot] = calrad[ichan][ispot];

     rad2at(ifchan);    /* calculate AT from radiance */
  
/*---------------------------------------------------------*
 * Save the calculated AT into a 3-D array. Bad data are 
 * flagged as -99.
 *---------------------------------------------------------*/
     for (ispot=0; ispot< NUMSPOT_A; ispot++)
        at[iscan][ispot][ichan] = rat[ispot];

   } /* end of ichan for loop*/

}/* end of getcal.c */
