/***************************************************************************
 *  Program Name      : gnrt_prod.c
 *  Type              : Subroutine
 *  Function          : Program reads in AMSU-A 1B* data, calculates ATs, 
 *			and generates products
 *  Input Files       : jan.dat, feb.dat, mar.dat, apr.dat,
 *                      may.dat, jun.dat, jul.dat, aug.dat,
 *                      sep.dat, oct.dat, nov.dat, dec.dat 
 *			(only use one input file for each swath)
 *  Output Files      : None
 *  Subroutine Called : openfs.c, read_ssmir.c, qc_at.c, getcal.c, calprod.c,
 *			set_month.c 
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void getcal();
void qc_at();
void calprod();

/*****************************************************************/
void gnrt_prod(short int numscan)
{
   short int   	iscan, iflag;
   long int 	new_flag;

/*---------------------------------------------------------------*
 * Check if the scanline is good
 *---------------------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
       qc_flag[iscan] = 1;

       iflag = scanline1[iscan].do_not_use_scan;
       new_flag = scanline1[iscan].quality_indicator_bit_field;

       orb_mode[iscan] = scanline1[iscan].orbit_node;
       orb_mode[iscan] = orb_mode[iscan] + 1;

/*---------------------------------------------------------------*
 * Toss off the scanlines that are bad and flag them as 
 * "do_not_use_scan". No geophysical products are calculated
 * if bad_at = 1. This parameter is set in qc_at.c and a value
 * of 1 signals that at least one FOV of one channel out of 
 * channel 1~13 and channel 15 is bad and that the entire 
 * scanline is tossed for all channels  
 *--------------------------------------------------------------*/
       if (new_flag == 0 && iflag == 0)   /* good scanline */
       {
           qc_flag[iscan] = 0;

           getcal(iscan);

           qc_at(iscan);

	   /*      printf("Check the flag %d %d \n", iscan, bad_at); */

           if(bad_at == 1) 
	     qc_flag[iscan] = 2;
           else
	     calprod(iscan);

       }   /* end of iflag check */

    }   /* end of iscan loop */

}  /* end of gnrt_prod.c  */
