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
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *  03/05/08     Modified to deal with chan4 problem.   jz (Perot)
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
void qc_at(short int iscan) 
{
  short int        ifov, ichan;
  short int        ch_flag, ch_flag_11, ch_flag_14, ch_flag_4;

/*--------------------------------------------------------*
 * Channel 1 to 10, channel 12, 13 and 15 limit test
 *--------------------------------------------------------*/
    ch_flag = 0;
    for (ifov = 0; ifov < NUMSPOT_A; ifov++)
    {
       for (ichan = 0; ichan < 10; ichan++) /* channel 1 ~ 10 */
       {
	 if(ichan !=3){
           if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
           { 
              ch_flag = 1;
              break;
           }
	 }//channel 4 is treated differently here!
       }

       for (ichan = 11; ichan < 13; ichan++) /* channel 12 ~ 13 */
       {
           if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
           { 
              ch_flag = 1;
              break;
           }

        } /* ichan loop */

        ichan = 14; /* channel 15 */
	if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
	  ch_flag = 1;

        if(ch_flag == 1) 
          break;

    } /* ifov loop */

/*--------------------------------------------------------*
 * If any pixel of any channel is out of the limits, then 
 * ATs of the entire scanline of all channels are set to 
 * -99.
 *--------------------------------------------------------*/

    if(ch_flag == 1)
    {
       bad_at = 1;
 
       for (ifov = 0; ifov < NUMSPOT_A; ifov++)
       {
         for (ichan = 0; ichan < NUMCHAN_A; ichan++)
           at[iscan][ifov][ichan] = MISSING; 

       }
    }

/*--------------------------------------------------------*
 * Channel 11 and 14 AT exceeds limits more often than other
 * channels. So they are checked and flagged separately.  
 *--------------------------------------------------------*/
    else  /* check limits for channel 11 and 14  and channel 4*/
    {
       bad_at = 0;

       ichan = 10;
       ch_flag_11 = 0;
       for (ifov = 0; ifov < NUMSPOT_A; ifov++)
       {
	  if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
          {
            ch_flag_11 = 1;
            break;
          } 
       }
 
       if(ch_flag_11 == 1)
       {
	  for (ifov = 0; ifov < NUMSPOT_A; ifov++)
	    at[iscan][ifov][10] = MISSING; 
       }
  
       ichan = 13;
       ch_flag_14 = 0;
       for (ifov = 0; ifov < NUMSPOT_A; ifov++)
       {
	  if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
          {
            ch_flag_14 = 1;
            break;
          } 
       }
 
       if(ch_flag_14 == 1)
       {
	  for (ifov = 0; ifov < NUMSPOT_A; ifov++)
	    at[iscan][ifov][13] = MISSING; 
       }

       ichan = 3;
       ch_flag_4 = 0;
       for (ifov = 0; ifov < NUMSPOT_A; ifov++)
       {
	  if(at[iscan][ifov][ichan] < limit_A.Temp_lower[ichan] ||
              at[iscan][ifov][ichan] > limit_A.Temp_upper[ichan])
          {
            ch_flag_4 = 1;
            break;
          } 
       }
 
       if(ch_flag_4 == 1)
       {
	  for (ifov = 0; ifov < NUMSPOT_A; ifov++)
	    at[iscan][ifov][3] = MISSING; 
       }
  
    } /* end of if(ch_flag == 1) */
 
} /* end of qc_at.c */    
