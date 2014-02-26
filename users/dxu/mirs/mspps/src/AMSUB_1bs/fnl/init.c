/***********************************************************************
 *  Program Name      : init.c
 *  Type              : Subroutine
 *  Function          : Program initializes product arrays
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"
 
/***************************************************/
void initprod()
{

       short int ifov, snum;

       for(snum = 0; snum < MAXSCANLINE_B; snum++)
           for(ifov = 0; ifov < NUMSPOT_B; ifov++)
           {
               rr[snum][ifov] = MISSING;
               snow[snum][ifov] = MISSING;
		snowr[snum][ifov] = MISSING;
               iwp[snum][ifov] = MISSING;
               de[snum][ifov] = MISSING;
               swe[snum][ifov] = MISSING;
		
           } 

} /* end of init.c */
