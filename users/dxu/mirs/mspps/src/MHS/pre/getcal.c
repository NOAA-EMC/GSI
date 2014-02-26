/***********************************************************************
 *  Program Name      : getcal.c
 *  Type              : Subroutine
 *  Function          : Program extracts earth view of each spot and
 *                      calibration coefficients from AMSU-B 1B* data,  
 *			then call subroutines to convert count to 
 * 			antenna temperature
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : cnt2rd.c, rad2at.c  
 *  Called by         : gnrt_at.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/***********************************************************/
void cnt2rd();
void rad2at();

/***********************************************************/
void getcal(short int iscan)
{
   short int   ichan, ispot;
   long  int   ifchan, insnum, numchan, numspot;

   numchan = NUMCHAN_M;
   numspot = NUMSPOT_M;
   insnum  = 2;

/*-------------------------------------------------*
 * Extract earth views (counts) 
 *-------------------------------------------------*/
   for(ispot = 0; ispot < NUMSPOT_M; ispot++)
     for(ichan = 0; ichan < NUMCHAN_M; ichan++)
     {
       count[ichan][ispot] = scanline[iscan].earth_view_data[ispot*6+ichan+1];
       if(count[ichan][ispot] < 0)
         count[ichan][ispot] = count[ichan][ispot] + 65536;
     }

/*---------------------------------------------------*
 * Extract calibration coefficients
 *---------------------------------------------------*/
   for(ichan = 0; ichan < NUMCHAN_M; ichan++)
   {
     clcoef[ichan][2] = scanline[iscan].pri_cal_coeffs[ichan][0]*1.0E-16;
     clcoef[ichan][1] = scanline[iscan].pri_cal_coeffs[ichan][1]*1.0E-10;
     clcoef[ichan][0] = scanline[iscan].pri_cal_coeffs[ichan][2]*1.0E-06;
   }

/*---------------------------------------------------*
 * Calculate radiances from counts, returned values
 * are saved in calrad[NUMCHAN_M][NUMSPOT_M]
 *---------------------------------------------------*/
   cnt2rd(numchan, numspot);

/*---------------------------------------------------*
 * Convert radiances to antenna temperatures
 *---------------------------------------------------*/
   for(ichan = 0; ichan < NUMCHAN_M; ichan++)
   {
     ifchan = ichan;
     for (ispot=0; ispot< NUMSPOT_M; ispot++)
       rad[ispot] = calrad[ichan][ispot];

     rad2at(ifchan);    
  
/*---------------------------------------------------*
 * Save the calculated AT (rat[NUMSPOT_M]) into a 3-D
 * array at[]. Bad data is flagged as -99.
 *---------------------------------------------------*/
     for (ispot=0; ispot< NUMSPOT_M; ispot++)
          at[iscan][ispot][ichan] = rat[ispot];

   } /* ichan for loop*/

}/* end of getcal.c */
