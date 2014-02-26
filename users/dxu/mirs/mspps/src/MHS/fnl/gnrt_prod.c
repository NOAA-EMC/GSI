/***********************************************************************
 *  Program Name      : gnrt_prod.c
 *  Type              : Subroutine
 *  Function          : Program generates MHS products 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : calprod.c 
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/10/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************/
void calprod();

/*******************************************************/
void gnrt_prod(TOTNUM_OF_SCAN numscan)
{
   short int   iscan;

/*----------------------------------*
 * Check if the scan line is good
 *----------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
     
     if(at_AB[iscan][0][0] > 0 && at[iscan][0][0] > 0) {
          calprod(iscan);
	  //printf(" Done scan %d\n",iscan);
          }
     //printf("Through scan %d %f %f\n",iscan, at_AB[iscan][0][0], at[iscan][0][0] );
   }  /* end of iscan loop */

}  /* end of gnrt_prod.c */
