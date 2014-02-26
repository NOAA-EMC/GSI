/***********************************************************************
 *  Program Name      : read_mask_snow.c
 *  Type              : Subroutine
 *  Function          : Program reads in the 1/6 degree land/sea 
 *			mask data which is derived from the original 
 *			University of Bristol mask data by filtering
 *			out all waterbodies occupying less or equal to
 *			1000 pixels
 *  Input Files       : mask1000.bin
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : BSWATH.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   12/03/2001      v2.0
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************/
void read_mask_snow(FILE *fparm)
{
   fread(mask, sizeof(mask), 1, fparm);

   fclose(fparm);

   printf(" Successfully read the 1/6 degree map data in !\n");

} /* end of read_mask_snow.c */
