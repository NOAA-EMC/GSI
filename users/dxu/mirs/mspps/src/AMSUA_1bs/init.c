/***************************************************************************
 *  Program Name      : init.c
 *  Type              : Subroutine
 *  Function          : Program initializes AT and product fields 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/********************************************************************/
void init()
{
    short int ifov, iscan, ich;
    

    for(iscan = 0; iscan < MAXSCANLINE_A; iscan++){
       for(ifov = 0; ifov < NUMSPOT_A; ifov++)
       {
          stype[iscan][ifov] = MISSING_CHAR;

          tpw[iscan][ifov] = MISSING;
          clw[iscan][ifov] = MISSING;
          sice[iscan][ifov] = MISSING;
   	  ts[iscan][ifov] = MISSING;
 	  em1[iscan][ifov] = MISSING;
 	  em2[iscan][ifov] = MISSING;
 	  em3[iscan][ifov] = MISSING;
          rr[iscan][ifov] = MISSING;
          snow[iscan][ifov] = MISSING;

          for(ich = 0; ich < NUMCHAN_A; ich++)
             at[iscan][ifov][ich] = MISSING;

       } 
    }
    


} /* end of init.c */
