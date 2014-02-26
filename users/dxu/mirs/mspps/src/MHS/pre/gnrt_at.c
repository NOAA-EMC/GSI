/***********************************************************************
 *  Program Name      : gnrt_at.c
 *  Type              : Subroutine
 *  Function          : Program calculates antenna temperature from 
 *			earth view (count) and performs quality control
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : getcal.c qc_at.c 
 *  Called by         : rmhs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/************************************************************/
void getcal();
void qc_at();

/************************************************************/
void gnrt_at(TOTNUM_OF_SCAN numscan)
{
   short int   iscan;
   
   long int    new_flag;
  
/*---------------------------------------------------*
 * Check if the scanline is good
 *---------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
       	qc_flag[iscan] = 1;

	new_flag = scanline[iscan].quality_indicator_bit_field;
       
	orb_mode[iscan] = scanline[iscan].orbit_node>>15;
/*       if(iscan < 1000)
         printf("Orbit mode: %d %d\n",iscan,orb_mode[iscan]); */

	orb_mode[iscan] = orb_mode[iscan] + 1;

/*---------------------------------------------------*
 * Toss off the scanlines that are bad and flaged as
 * "do_not_use_scan" or "quality_indicator_bit_field".
 * Also toss off the scanlines that have constant ATs 
 * for the whole scanline. 
 *---------------------------------------------------*/
	if(new_flag == 0) /* good scanline */
	{
           qc_flag[iscan] = 0;

           getcal(iscan);

           qc_at(iscan);

           if(bad_at == 1) 
	     qc_flag[iscan] = 2;

	}   /* end of new_flag statement */

    }     /* end of iscan loop */

}  /* end of gnrt_at.c */
