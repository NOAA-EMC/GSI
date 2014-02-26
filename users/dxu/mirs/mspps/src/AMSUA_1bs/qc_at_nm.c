/***************************************************************************
 *  Program Name      : qc_at.c
 *  Type              : Subroutine
 *  Function          : Program performs quality control for AMSU-A 
 *			antenna temperatures 
 *  Input Files       : None
 *  Output Files      : bad_at (a parameter) 
 *  Subroutine Called : None 
 *  Called by         : gnrt_prod.c 
 *
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
void qc_at(short int iscan) 
{
  short int        ifov, ichan;
  short int        ch_flag;

/*--------------------------------------------------------*
 * Channel 1 and channel 2 limit test
 *--------------------------------------------------------*/
    ch_flag = 0;
    bad_at = 0;

    for (ifov = 0; ifov < NUMSPOT_A; ifov++)
    {
       for (ichan = 0; ichan < 2; ichan++) 
       {
           if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
           { 
		ch_flag = 1;
                break;
           }

        } /* ichan loop */

        if(ch_flag == 1) 
          break;

    } /* ifov loop */

/*--------------------------------------------------------*
 * If any pixel of any channel is out of the limits, then 
 * ATs of the entire scanline of both channel-1 and -2 are 
 * set to -99.
 *--------------------------------------------------------*/

    if(ch_flag == 1)
    {
       bad_at = 1;
 
       for (ifov = 0; ifov < NUMSPOT_A; ifov++)
       {
         for (ichan = 0; ichan < 2; ichan++)
           at[iscan][ifov][ichan] = MISSING; 
       }

    }
 
} /* end of qc_at.c */    
