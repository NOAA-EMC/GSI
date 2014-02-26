/***********************************************************************
 *  Program Name      : getcal.c
 *  Type              : Subroutine
 *  Function          : Program extracts earth view of each spot and
 *                      calibration coefficients from AMSU-B 1B* data,  
 *			then call subroutines to convert count to 
 * 			antenna temperature
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : bias_corr.c, cnt2rd.c, rad2at.c  
 *  Called by         : gnrt_at.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/***********************************************************/
void bias_corr(short int);
void cnt2rd();
void rad2at();

/***********************************************************/
void getcal(short int iscan)
{
   short int   ichan, ispot, item;
   long  int   ifchan, insnum, numchan, numspot;

   numchan = NUMCHAN_B;
   numspot = NUMSPOT_B;
   insnum  = 2;

/*-------------------------------------------------*
 * Extract earth views (counts) 
 *-------------------------------------------------*/
   for(ispot = 0; ispot < NUMSPOT_B; ispot++)
     for(ichan = 0; ichan < NUMCHAN_B; ichan++)
     {
       count[ichan][ispot] = scanline3[iscan].observations[ispot][ichan];
       if(count[ichan][ispot] < 0)
         count[ichan][ispot] = count[ichan][ispot] + 65536;
     }

/*---------------------------------------------------*
 * Extract calibration coefficients
 *---------------------------------------------------*/
   for(ichan = 0; ichan < NUMCHAN_B; ichan++)
     for(item = 0; item < 3; item++)
       clcoef[ichan][item] = scanline2[iscan].pri_cal_coeffs[ichan][item];

/*---------------------------------------------------*
 * Calculate radiances from counts, returned values
 * are saved in calrad[NUMCHAN_B][NUMSPOT_B]
 *---------------------------------------------------*/
   cnt2rd(numchan, numspot);

/*---------------------------------------------------*
 * Convert radiances to antenna temperatures
 *---------------------------------------------------*/
   for(ichan = 0; ichan < NUMCHAN_B; ichan++)
   {
     ifchan = ichan;
     for (ispot=0; ispot< NUMSPOT_B; ispot++)
       rad[ispot] = calrad[ichan][ispot];

     rad2at(ifchan);    
  
/*---------------------------------------------------*
 * Save the calculated AT (rat[NUMSPOT_B]) into a 3-D
 * array at[]. Bad data is flagged as -99.
 *---------------------------------------------------*/
     for (ispot=0; ispot< NUMSPOT_B; ispot++)
          at[iscan][ispot][ichan] = rat[ispot];

   } /* ichan for loop*/

}/* end of getcal.c */
