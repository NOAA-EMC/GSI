/***********************************************************************
 *  Program Name      : gnrt_prod.c
 *  Type              : Subroutine
 *  Function          : Program generates AMSU-B products
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : calprod.c
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/***************************************************/
void calprod();

/***************************************************/
void gnrt_prod(TOTNUM_OF_SCAN numscan)
{
   short int   iscan;

/*----------------------------------*
 * Check if the scan line is good
 *----------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
       if(at_AB[iscan][0][0] > 0 && at[iscan][0][0] > 0) 
          calprod(iscan);

   }  /* end of iscan loop */

}  /* end of gnrt_prod.c */
