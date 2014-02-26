/***********************************************************************
 *  Program Name      : qc_at.c
 *  Type              : Subroutine
 *  Function          : Program performs quality control for MHS
 *			antenna temperatures (ATs)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : gnrt_at.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/**********************************************************/
void qc_at(short int iscan) 
{
  long  int        ifov, ichan;
  short int        fov_tot, ch_flag;

/*---------------------------------------------*
 * Check AT at each FOV of each channel 
 *---------------------------------------------*/
    fov_tot = 0;
    for (ifov = 0; ifov < NUMSPOT_M; ifov++)
    {
       ch_flag = 0;
       for (ichan = 0; ichan < NUMCHAN_M; ichan++)
       {
           if(at[iscan][ifov][ichan] < limit_M.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_M.Temp_upper[ichan])
           { 
              ch_flag = 1;
              break; 
           }

       } /* ichan loop */

       if(ch_flag == 1)
         break;

    } /* ifov loop */
   
/*---------------------------------------------*
 * If any pixel of any channel is out of the 
 * limits, flag the entire scanline as bad 
 * for all channels and set all ATs to be -99.
 *---------------------------------------------*/
    if(ch_flag == 1) 
    {
        bad_at = 1;

        for(ichan = 0; ichan < NUMCHAN_M; ichan++)
          for(ifov = 0; ifov < NUMSPOT_M; ifov++)
            at[iscan][ifov][ichan] = MISSING;
    }

    else
        bad_at = 0;


} /* end of qc_at.c */    
