/***********************************************************************
 *  Program Name      : bin_time.c
 *  Type              : Subroutine
 *  Function          : Program calculates the output time field for
 *			the binary swath file 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *    1/26/2001      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"
#include "EBINARY.h"

/***************************************************/
void bin_time(TOTNUM_OF_SCAN numscan)
{
   short int   iscan, ifov;

   for(iscan = 0; iscan < numscan; iscan++)
     for(ifov = 0; ifov < NUMSPOT_M; ifov++)
       b_time[iscan][ifov] = hour[iscan] * 3600 + minute[iscan] * 60 + second[iscan];

}  /* End of bin_time.c */
