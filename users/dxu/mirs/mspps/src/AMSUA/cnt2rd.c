/***************************************************************************
 *  Program Name      : cnt2rd.c
 *  Type              : Subroutine
 *  Function          : Program calculates radiance from count 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : getcal.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
void cnt2rd(long int numchn, long int numspt)
{
      long int i,j;

      for(i = 0; i < numchn; i++)
         for(j = 0; j < numspt; j++)
         {
            calrad[i][j] = clcoef[i][0];
            calrad[i][j] = calrad[i][j] + clcoef[i][1]*count[i][j];
            calrad[i][j] = calrad[i][j] + clcoef[i][2]*count[i][j]*count[i][j];
         }

} /* end of cnt2rd.c */
