/***********************************************************************
 *  Program Name      : init.c
 *  Type              : Subroutine
 *  Function          : Program initializes output fields 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rmhs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
void initall()
{

       short int ifov, snum, ich;

       for(snum = 0; snum < MAXSCANLINE_M; snum++)
           for(ifov = 0; ifov < NUMSPOT_M; ifov++)
           {
	       stype[snum][ifov] = MISSING_CHAR;

               for(ich = 0; ich < NUMCHAN_M; ich++)
                    at[snum][ifov][ich] = MISSING;
           }

}  /* end of init.c */ 
