/***********************************************************************
 *  Program Name      : init.c
 *  Type              : Subroutine
 *  Function          : Program initializes output fields
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/************************************************************/
void initat()
{

       short int ifov, snum, ich;

       for(snum = 0; snum < MAXSCANLINE_B; snum++)
           for(ifov = 0; ifov < NUMSPOT_B; ifov++)
           {
	       stype[snum][ifov] = MISSING_CHAR;

               for(ich = 0; ich < NUMCHAN_B; ich++)
                    at[snum][ifov][ich] = MISSING;
           }

} /* end of init.c */ 
